use anyhow::Result;
use lalrpop_util::lalrpop_mod;
use crate::ast::AST;

lalrpop_mod!(pub familia);

pub fn parse(program: &str) -> Result<AST> {
    if let Err(_) = familia::ASTParser::new().parse(program) {
        Err(anyhow::anyhow!("parse error"))
    } else {
        Ok(familia::ASTParser::new().parse(program).unwrap())
    
    }
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
        ).unwrap();
        // fn decl
        parse(
            "\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            
            fn foo(a: R, b: T): i32 {
                return (a.a.a + b.b);
            }
        ",
        ).unwrap();
        // fn call
        parse(
            "\
            fn main() {
                foo({a: {a: 1, b: 2}, b: {a: 3, b: 4}}, {a: 5, b: 6});
            }
        ",
        ).unwrap();
    }
}
