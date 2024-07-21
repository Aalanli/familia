use lalrpop_util::{lalrpop_mod, ParseError};

use crate::{Db, ProgramSource};
use crate::ast2::Module;

lalrpop_mod!(pub familia2);

#[salsa::tracked]
pub fn parse<'db>(db: &'db dyn Db, src: ProgramSource) {
    // let k = familia2::TestParser::new().parse(db, "tokens".chars());
}
