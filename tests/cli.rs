use assert_cmd::prelude::*;
use std::fs;
use std::fs::File;
use std::io::{Error, Write};
use std::process::Command;
use tempfile::tempdir;

#[allow(unused_must_use)]
fn status(input: &str) -> Result<i32, Error> {
    // Compile
    let mut cmd = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();
    cmd.arg(input);
    let output = String::from_utf8(cmd.output().unwrap().stdout).unwrap();

    // Save output
    let dir = tempdir()?;
    let mut src = File::create(dir.path().join("tmp.s"))?;
    writeln!(src, "{}", output)?;

    // Assemble
    let src_path = dir
        .path()
        .join("tmp.s")
        .into_os_string()
        .into_string()
        .unwrap();
    Command::new("cc").args(&["-o", "tmp", &src_path]).output();

    // Execute
    let actual = Command::new("./tmp").status().unwrap().code().unwrap();

    // Cleanup
    fs::remove_file("./tmp")?;
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
