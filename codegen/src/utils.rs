use std::process::Command;

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
