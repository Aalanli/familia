use std::process::Command;
use lazy_static::lazy_static;

lazy_static! {
    static ref GPP: String = find_gpp();
}

fn find_gpp() -> String {
    let output = Command::new("which")
        .arg("g++")
        .output()
        .expect("failed to execute process");
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(!stdout.is_empty());
    stdout
}

pub fn object_to_executable(object_file: &str, executable_file: &str) {
    assert!(std::path::Path::new(object_file).exists());

    let output = Command::new("g++")
        .arg(object_file)
        .arg("-o")
        .arg(executable_file)
        .output()
        .expect("failed to execute process");
    assert!(output.status.success());
}

#[test]
fn test_which() {
    let gpp = find_gpp();
    assert!(!gpp.is_empty());
    println!("{}", gpp);
}
