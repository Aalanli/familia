use crate::ast2::*;
use crate::context::Db;
use crate::lexer::Lexer;
use crate::{ProgramSource};

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{prelude::*};
use std::{collections::HashMap, env, fmt, fs};

pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug, PartialEq)]
enum Token<'src> {
    I32(i32),
    Str(&'src str),
    Ctrl(char),
    Ident(&'src str),
    Op(&'src str),
    Fn,
    Let,
    Return,
    Class,
    Interface,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::I32(i) => write!(f, "{}", i),
            Token::Str(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::Class => write!(f, "class"),
            Token::Interface => write!(f, "interface"),
        }
    }
}


fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::I32);

    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let ctrl = one_of("()[]{}:;,.").map(Token::Ctrl);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "fn" => Token::Fn,
        "let" => Token::Let,
        "return" => Token::Return,
        "class" => Token::Class,
        "interface" => Token::Interface,
        _ => Token::Ident(ident),
    });

    let token = num.or(str_).or(op).or(ctrl).or(ident);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

fn symbol_span<'db, 'tokens: 'db, 'src: 'tokens>(db: &'db Db) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    SymbolSpan,
    extra::Err<Rich<'tokens, Token<'src>, Span>>
    > + Clone + 'db {
    let ident = select! { Token::Ident(i) => i };
    ident.map_with(move |ident, e| {
        let sym = Symbol::new(db, ident.to_string());
        WithSpan::new(sym, e.span())
    })
}


fn type_parser<'db, 'tokens: 'db, 'src: 'tokens>(db: &'db Db) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    TypeId,
    extra::Err<Rich<'tokens, Token<'src>, Span>>
> + Clone + 'db {

    recursive(|type_parser| {
        let void = just(Token::Ctrl('(')).ignore_then(just(Token::Ctrl(')')))
            .map(|_| TypeKind::Void);

        let ident = select! { Token::Ident(i) => i };
        let raw_symbol = ident.map_with(move |ident, e| {
                let sym = Symbol::new(db, ident.to_string());
                sym
            });

        let path = raw_symbol
            .separated_by(just(Token::Ctrl('.')))
            .at_least(1)
            .collect::<Vec<_>>()
            .map(|x| {
                TypeKind::Symbol(Path::new(db, x))
            });
        
        let typed_var = symbol_span(db)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser.clone())
            .map(|(s, t)| {
                TypedVar::new(s, t)
            });
        
        let structure = typed_var.separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(|x| {
                TypeKind::Struct { fields: x }
            })
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))).labelled("struct");
        
        void.or(path).or(structure)
            .map_with(|tykind, e| {
                TypeId::new(db, WithSpan::new(tykind, e.span()))
            })
    }) 
}

// fn expr_parser<'db, 'tokens: 'db, 'src: 'tokens>(db: &'db Db) -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>,
//     Expr,
//     extra::Err<Rich<'tokens, Token<'src>, Span>>
// > + Clone + 'db {
//     recursive(|expr| {

//     })
// }

#[cfg(test)]
mod test_parse {
    use std::io;

    use insta::assert_debug_snapshot;
    use lalrpop_util::ErrorRecovery;

    use crate::Diagnostic;

    use super::*;
    // fn parse_stmt(stmt: &str) -> String {
    //     let db = crate::Database::default();
    //     let stmt = familia2::StmtParser::new().parse(&db, Lexer::new(stmt.chars()));
    //     match stmt {
    //         Ok(s) => format!("{s:?}"),
    //         Err(error) => {
    //             let err = ErrorRecovery { error, vec![] };
    //             Diagnostic::report_parse_err(&db, err);
    //             let errors =
    //         }
    //     }

    // }

    #[test]
    fn test_lex1() {
        let (tokens, errs) = lexer().parse("input a b ++ \"abc\" -+= let a return : { [").into_output_errors();
        assert_debug_snapshot!((tokens, errs));
    }

    fn display_err(mut w: impl std::io::Write, src: String, filename: String, err: &[Rich<Token>]) {
        // .map(|e| e.map_token(|tok| tok.to_string()))
        for e in err.into_iter() {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .write(sources([(filename.clone(), src.clone())]), &mut w)
                .unwrap();
        }
    }

    #[test]
    fn test_parse2() {
        let db = Db::new();
        let src = " {a: i32, b: {a: i32, c: String}} ";
        let (tokens, err) = lexer().parse(src).into_output_errors();
        let tokens = tokens.unwrap();
        let (sym, err) = type_parser(&db).parse(tokens.as_slice().spanned((src.len()..src.len()).into())).into_output_errors();
        display_err(io::stderr(), src.to_string(), "L".to_string(), &err);
        if sym.is_some() {
            println!("{:?}", db[sym.unwrap()]);
        }
    }
}
