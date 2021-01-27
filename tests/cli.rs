use assert_cmd::prelude::*;
use std::fs;
use std::fs::File;
use std::io::{Error, Write};
use std::process::Command;
use tempfile::tempdir;
use uuid::Uuid;

#[allow(unused_must_use)]
fn status(input: &str) -> Result<i32, Error> {
    // Compile
    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
    cmd.arg(input);
    let output = String::from_utf8(cmd.output().unwrap().stdout).unwrap();

    // Save output
    let dir = tempdir()?;
    let bin_name = Uuid::new_v4().to_string();
    let src_name = bin_name.clone() + ".s";
    let mut src = File::create(dir.path().join(&src_name))?;
    writeln!(src, "{}", output)?;

    // Assemble
    let src_path = dir
        .path()
        .join(&src_name)
        .into_os_string()
        .into_string()
        .unwrap();
    Command::new("cc")
        .args(&["-o", &bin_name, &src_path])
        .output();

    // Execute
    let bin_path = "./".to_string() + &bin_name;
    let actual = Command::new(&bin_path).status().unwrap().code().unwrap();

    // Cleanup
    fs::remove_file(&bin_path)?;
    drop(src);
    dir.close();

    Ok(actual)
}

#[test]
fn number_ok() {
    assert_eq!(status("1").unwrap(), 1);
}

#[test]
fn expr_ok() {
    assert_eq!(status(" 5 + 6 * 7 ").unwrap(), 47);
    assert_eq!(status(" 5 * ( 9 - 6 ) ").unwrap(), 15);
    assert_eq!(status(" ( 3 + 5 ) / 2 ").unwrap(), 4);
}

#[test]
fn unary_op_ok() {
    assert_eq!(status(" -5 + 6 * 7 ").unwrap(), 37);
    assert_eq!(status(" 5 * ( +9 - -6 ) ").unwrap(), 75);
    assert_eq!(status(" -( -3 + -5 ) / 2 ").unwrap(), 4);
}

#[test]
fn relational_op_ok() {
    assert_eq!(status(" 1 == 1 ").unwrap(), 1);
    assert_eq!(status(" 1 == 2 ").unwrap(), 0);
    assert_eq!(status(" 1 != 1 ").unwrap(), 0);
    assert_eq!(status(" 1 != 2 ").unwrap(), 1);
    assert_eq!(status(" 1 < 0 ").unwrap(), 0);
    assert_eq!(status(" 1 < 1 ").unwrap(), 0);
    assert_eq!(status(" 1 < 2 ").unwrap(), 1);
    assert_eq!(status(" 1 <= 0 ").unwrap(), 0);
    assert_eq!(status(" 1 <= 1 ").unwrap(), 1);
    assert_eq!(status(" 1 <= 2 ").unwrap(), 1);
    assert_eq!(status(" 0 > 1 ").unwrap(), 0);
    assert_eq!(status(" 1 > 1 ").unwrap(), 0);
    assert_eq!(status(" 2 > 1 ").unwrap(), 1);
    assert_eq!(status(" 0 >= 1 ").unwrap(), 0);
    assert_eq!(status(" 1 >= 1 ").unwrap(), 1);
    assert_eq!(status(" 2 >= 1 ").unwrap(), 1);
    assert_eq!(status(" -( -3 + -5 ) / 2 == 4 ").unwrap(), 1);
    assert_eq!(status(" -( -3 + -5 ) / 2 == 5 ").unwrap(), 0);
}
