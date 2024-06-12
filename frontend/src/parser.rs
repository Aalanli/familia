use crate::ast::{LexError, Lexer, Loc, Tok, AST};

use anyhow::Result;
use lalrpop_util::{lalrpop_mod, ParseError};

lalrpop_mod!(pub familia);

pub type Error = ParseError<Loc, Tok, LexError>;

pub fn parse(program: &str) -> Result<AST, Error> {
    let lexer = Lexer::new(program.chars());
    familia::ASTParser::new().parse(lexer)
}

#[cfg(test)]
mod parse_test {
    use super::parse;

    #[test]
    fn test_parse() {
        // type decl
        parse(
            "\
            type T = {
                a: i32,
                b: i32
            }
        ",
        )
        .unwrap();
        // fn decl
        parse(
            "\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
            }
        ",
        )
        .unwrap();
        // fn call
        parse(
            "\
            fn main() {
                foo({a: {a: 1, b: 2}, b: {a: 3, b: 4}}, {a: 5, b: 6});
            }
        ",
        )
        .unwrap();

        parse("fn t(a:i32):i32").unwrap();
    }

    #[test]
    fn test_parse_class() {
        parse(
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
        ",
        )
        .unwrap();
    }

    #[test]
    fn test_fail() {
        parse("\
            type T = {
                a: i32,
                b: i32
        ").unwrap_err();
            
        parse("\
            type T = {
                a: i32,
                b: i32
            }
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
        ").unwrap_err();

        parse("fn t(a::i32):i32").unwrap_err();
    }
}
