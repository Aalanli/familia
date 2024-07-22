use lalrpop_util::{lalrpop_mod, ParseError};

use crate::{Db, ProgramSource};
use crate::lexer::Lexer;
use crate::ast2::Module;

lalrpop_mod!(pub familia2);

#[salsa::tracked]
pub fn parse<'db>(db: &'db dyn Db, src: ProgramSource) {
    // let k = familia2::TestParser::new().parse(db, "tokens".chars());
}


#[cfg(test)]
mod test_parse {
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
}
