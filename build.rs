use std::env;

fn main() {
    lalrpop::process_root().unwrap();
    env::set_var("LLVM_SYS_180_PREFIX", "thirdparty");
}