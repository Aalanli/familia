use crate::ast2::*;
use crate::context::Db;
use crate::lexer::Lexer;
use crate::ProgramSource;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use either::Either;
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

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
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

fn symbol_span<'db, 'tokens: 'db, 'src: 'tokens>(
    db: &'db Db,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    SymbolSpan,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone
       + 'db {
    let ident = select! { Token::Ident(i) => i };
    ident.map_with(move |ident, e| {
        let sym = Symbol::new(db, ident.to_string());
        WithSpan::new(sym, e.span())
    })
}

fn parse_path<'db, 'tokens: 'db, 'src: 'tokens>(
    db: &'db Db,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Path,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone
       + 'db {
    let ident = select! { Token::Ident(i) => i };
    let raw_symbol = ident.map(|ident| {
        let sym = Symbol::new(db, ident.to_string());
        sym
    });

    let path = raw_symbol
        .separated_by(just(Token::Ctrl('.')))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|x| Path::new(db, x));
    path
}

fn type_parser<'db, 'tokens: 'db, 'src: 'tokens>(
    db: &'db Db,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    TypeId,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone
       + 'db {
    recursive(|type_parser| {
        let void = just(Token::Ctrl('('))
            .ignore_then(just(Token::Ctrl(')')))
            .map(|_| TypeKind::Void);

        let typed_var = symbol_span(db)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser.clone())
            .map(|(s, t)| TypedVar::new(s, t));

        let structure = typed_var
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(|x| TypeKind::Struct { fields: x })
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .labelled("struct");

        void.or(parse_path(db).map(|p| TypeKind::Symbol(p)))
            .or(structure)
            .map_with(|tykind, e| TypeId::new(db, WithSpan::new(tykind, e.span())))
    })
}

fn expr_parser<'db, 'tokens: 'db, 'src: 'tokens>(
    db: &'db Db,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Expr,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone
       + 'db {
    recursive(|expr| {
        let ty_hint = just(Token::Ctrl(':')).ignore_then(type_parser(db)).or_not();

        let var = symbol_span(db)
            .then(ty_hint)
            .map(|(s, ty)| ExprKind::Var(Var { name: s, ty }));

        let void_lit = just(Token::Ctrl('('))
            .ignored()
            .then_ignore(just(Token::Ctrl(')')))
            .map(|_| ExprKind::VoidLit);

        let int_lit = select! {Token::I32(i) => i}.map(|i| ExprKind::IntLit(i));
        let str_lit = select! { Token::Str(s) => s }
            .map(|s| ExprKind::StringLit(Symbol::new(db, s.to_string())));

        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .collect::<Vec<_>>();

        let atom = var
            .or(void_lit)
            .or(int_lit)
            .or(str_lit)
            .map_with(|kind, e1| Expr {
                kind,
                span: e1.span(),
            })
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let call = atom.foldl_with(
            items
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map(Either::Left)
                .or(just(Token::Ctrl('.'))
                    .ignore_then(symbol_span(db))
                    .map(Either::Right))
                .repeated(),
            |f, a, e| {
                let kind = match a {
                    Either::Left(args) => ExprKind::Call {
                        object: Box::new(f),
                        args,
                    },
                    Either::Right(attr) => ExprKind::GetAttr {
                        expr: Box::new(f),
                        sym: attr,
                    },
                };
                Expr {
                    kind,
                    span: e.span(),
                }
            },
        );

        let op = just(Token::Op("+")).ignored();
        let add = call
            .clone()
            .foldl_with(op.then(call).repeated(), |a, (_, b), e| Expr {
                kind: ExprKind::Add {
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                },
                span: e.span(),
            });

        let expr = add;

        expr
    })
}

#[cfg(test)]
mod test_parse {
    use fmt::Display;
    use insta::assert_debug_snapshot;

    use crate::Diagnostic;

    use super::*;

    macro_rules! run_parser {
        ($src:literal, $parser:expr) => {{
            let mut w = Vec::new();
            let (tokens, err) = lexer().parse(&($src)).into_output_errors();
            let errs = err
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string()))
                .collect::<Vec<_>>();
            if tokens.is_none() {
                display_err(&mut w, $src.to_string(), "L".to_string(), &errs);
            } else {
                let (item, err_parse) = ($parser)
                    .parse(
                        tokens
                            .as_ref()
                            .unwrap()
                            .as_slice()
                            .spanned(($src.len()..$src.len()).into()),
                    )
                    .into_output_errors();
                if let Some(item) = item {
                    use std::io::Write;
                    write!(w, "{:?}", item).unwrap();
                } else {
                    display_err(&mut w, $src.to_string(), "L".to_string(), &err_parse);
                }
            }

            String::from_utf8(w).unwrap()
        }};
    }

    #[test]
    fn test_lex1() {
        let (tokens, errs) = lexer()
            .parse("input a b ++ \"abc\" -+= let a return : { [")
            .into_output_errors();
        assert_debug_snapshot!((tokens, errs));
    }

    fn display_err<T: Display>(
        mut w: impl std::io::Write,
        src: String,
        filename: String,
        err: &[Rich<T>],
    ) {
        // .map(|e| e.map_token(|tok| tok.to_string()))
        for e in err.into_iter() {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string()), // .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                    // .with_color(Color::Yellow)
                }))
                .finish()
                .write(sources([(filename.clone(), src.clone())]), &mut w)
                .unwrap();
        }
    }

    #[test]
    fn test_parse_type() {
        let db = Db::new();
        let res = run_parser!("{a: i32}", type_parser(&db));
        assert_debug_snapshot!(res);
    }

    #[test]
    fn test_parse_expr() {
        let db = Db::new();
        let res = run_parser!("((a.a(2, \"3\") + 3)()).c", expr_parser(&db));
        assert_debug_snapshot!(res);
    }

    #[test]
    fn test_parse_getattr() {
        let db = Db::new();
        let res = run_parser!("a.b.c.d.e", expr_parser(&db));
        assert_debug_snapshot!(res);
    }

    #[test]
    fn test_parse_getattr_fail() {
        let db = Db::new();
        let res = run_parser!("a().b", expr_parser(&db));
        assert_debug_snapshot!(res);
    }
}
