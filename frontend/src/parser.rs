use crate::{
    ast::{Decl, Lexer},
    ModSource,
};

use crate::prelude::*;
use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(pub familia);

pub fn parse(program: &ModSource) -> PhaseResult<Decl> {
    let lexer = Lexer::new(program.text.chars());
    let ast = familia::ASTParser::new().parse(lexer);
    match ast {
        Ok(ast) => Ok(ast),
        Err(e) => {
            let (span, msg) = match e {
                ParseError::InvalidToken { location } => (location.span(location), "Invalid token"),
                ParseError::UnrecognizedEof { location, .. } => {
                    (location.span(location), "Unrecognized EOF")
                }
                ParseError::UnrecognizedToken {
                    token: (start, _, end),
                    ..
                } => (start.span(end), "Unrecognized token"),
                ParseError::ExtraToken {
                    token: (start, _, end),
                } => (start.span(end), "Extra token"),
                ParseError::User { error } => {
                    program.add_err(ProgramError {
                        error_message: "Lex error",
                        span: Some(*error.get_span()),
                        highlight_message: Some(error.get_message().into()),
                        ..Default::default()
                    });
                    return Err(program.err());
                }
            };
            program.add_err(ProgramError {
                error_message: "Parse error",
                span: Some(span),
                highlight_message: Some(msg.into()),
                ..Default::default()
            });
            Err(program.err())
        }
    }
}

#[cfg(test)]
mod parse_test {
    use super::parse;

    #[test]
    fn test_parse() {
        // type decl
        parse(
            &"\
            type T = {
                a: i32,
                b: i32
            }
        "
            .into(),
        )
        .unwrap();
        // fn decl
        parse(
            &"\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
            }
        "
            .into(),
        )
        .unwrap();
        // fn call
        parse(
            &"\
            fn main() {
                foo({a: {a: 1, b: 2}, b: {a: 3, b: 4}}, {a: 5, b: 6});
            }
        "
            .into(),
        )
        .unwrap();

        parse(&"fn t(a:i32):i32".into()).unwrap();
    }

    #[test]
    fn test_parse_class() {
        parse(
            &"\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            type S = {a: R, b: R}
            class C {
                type T = {a: i32, b: i32}

                fn foo(a: S, b: R): i32 {
                    return (a.a.a + b.b.b);
                }

                fn bar(a: i32): i32 {
                    return (a + 2);
                }
            }
        "
            .into(),
        )
        .unwrap();
    }

    #[test]
    fn test_fail() {
        parse(
            &"\
            type T = {
                a: i32,
                b: i32
        "
            .into(),
        )
        .unwrap_err();

        parse(
            &"\
            type T = {
                a: i32,
                b: i32
            }
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
        "
            .into(),
        )
        .unwrap_err();

        parse(&"fn t(a::i32):i32".into()).unwrap_err();
    }

    #[test]
    fn test_parse_string() {
        let _t = parse(
            &"\
            fn main() {
                let a = \"hello, world\";
                let b = \"\\\"hello, world\\\"\";
            }
            "
            .into(),
        )
        .unwrap();
        // println!("{:?}", _t);
    }

    #[test]
    fn test_lex_fail() {
        let src = "fn 3main() { let a = 1; let b = 2; let c = a + b; }".into();
        let res = parse(&src);
        match res {
            Err(e) => {
                println!("{}", e);
            }
            _ => panic!("Expected error"),
        }
    }

    #[test]
    fn test_parse_fail1() {
        let srcs = [
            "fn main)() { let a = 1; let b = 2; let c = a + b; }",
            "fn main3() { le a = 1; let b = 2; let c = a + b; }",
            "fn main3() {",
            "fn main3() { let a = \" ; }",
        ];
        for src in srcs.into_iter() {
            let src = src.into();
            let res = parse(&src);
            match res {
                Err(e) => {
                    println!("{}", e);
                }
                _ => panic!("Expected error"),
            }
        }
    }
}
