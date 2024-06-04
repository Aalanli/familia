use lalrpop_util::lalrpop_mod;
use crate::ast::AST;

lalrpop_mod!(pub familia);

pub fn parse(program: &str) -> AST {
    familia::ASTParser::new().parse(program).unwrap()
}

#[cfg(test)]
mod parse_test {
    use super::parse;

    #[test]
    fn test_parse() {
        parse("\
            type T = {
                a: i32,
                b: i32
            }
        ");
        parse("\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
            }
        ");
    }
}