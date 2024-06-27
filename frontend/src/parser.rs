use crate::{ast::{Decl, LexError, Lexer, Loc, Tok}, ModSource};

use anyhow::Result;
use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(pub familia);

pub type Error = ParseError<Loc, Tok, LexError>;

pub fn parse(program: &ModSource) -> Result<Decl, Error> {
    let lexer = Lexer::new(program.text.chars());
    let ast = familia::ASTParser::new().parse(lexer)?;
    Ok(ast)
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
        ".into()
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
        ".into()
        )
        .unwrap();
        // fn call
        parse(&
            "\
            fn main() {
                foo({a: {a: 1, b: 2}, b: {a: 3, b: 4}}, {a: 5, b: 6});
            }
        ".into()
        )
        .unwrap();

        parse(&"fn t(a:i32):i32".into()).unwrap();
    }

    #[test]
    fn test_parse_class() {
        parse(&
            "\
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
        ".into()
        )
        .unwrap();
    }

    #[test]
    fn test_fail() {
        parse(&
            "\
            type T = {
                a: i32,
                b: i32
        ".into())
        .unwrap_err();

        parse(&
            "\
            type T = {
                a: i32,
                b: i32
            }
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
        ".into())
        .unwrap_err();

        parse(&"fn t(a::i32):i32".into()).unwrap_err();
    }

    #[test]
    fn test_parse_string() {
        let _t = parse(&
            "\
            fn main() {
                let a = \"hello, world\";
                let b = \"\\\"hello, world\\\"\";
            }
            ".into()
        ).unwrap();
        // println!("{:?}", _t);
    }
}
