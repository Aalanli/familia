use crate::ast::AST;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub familia);

pub fn parse(program: &str) -> AST {
    familia::ASTParser::new().parse(program).unwrap()
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
        );
        // fn decl
        parse(
            "\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
            }
        ",
        );
        // fn call
        parse(
            "\
            fn main() {
                foo({a: {a: 1, b: 2}, b: {a: 3, b: 4}}, {a: 5, b: 6});
            }
        ",
        );
    }
}
