use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/rts.cpp");
    let stdout = Command::new("clang")
        .arg("-O3")
        .arg("-fPIE")
        .arg("-S")
        .arg("-emit-llvm")
        .arg("-c")
        .arg("src/rts.cpp")
        .arg("-o")
        .arg("src/rts.ll")
        .output()
        .unwrap();
    assert!(stdout.status.success());
}
