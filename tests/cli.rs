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
    let result = Command::new("cc")
        .args(&["-o", &bin_name, &src_path])
        .output()
        .unwrap();
    if !result.status.success() {
        println!("ERROR: {}", input);
        println!("ERROR: {}", String::from_utf8(result.stderr).unwrap());
    }

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
    assert_eq!(status("return 1;").unwrap(), 1);
}

#[test]
fn expr_ok() {
    assert_eq!(status(" 5 + 6 * 7 ;").unwrap(), 47);
    assert_eq!(status(" 5 * ( 9 - 6 ) ;").unwrap(), 15);
    assert_eq!(status(" ( 3 + 5 ) / 2 ;").unwrap(), 4);
}

#[test]
fn unary_op_ok() {
    assert_eq!(status(" -5 + 6 * 7 ;").unwrap(), 37);
    assert_eq!(status(" 5 * ( +9 - -6 ) ;").unwrap(), 75);
    assert_eq!(status(" -( -3 + -5 ) / 2 ;").unwrap(), 4);
}

#[test]
fn relational_op_ok() {
    assert_eq!(status(" 1 == 1 ;").unwrap(), 1);
    assert_eq!(status(" 1 == 2 ;").unwrap(), 0);
    assert_eq!(status(" 1 != 1 ;").unwrap(), 0);
    assert_eq!(status(" 1 != 2 ;").unwrap(), 1);
    assert_eq!(status(" 1 < 0 ;").unwrap(), 0);
    assert_eq!(status(" 1 < 1 ;").unwrap(), 0);
    assert_eq!(status(" 1 < 2 ;").unwrap(), 1);
    assert_eq!(status(" 1 <= 0 ;").unwrap(), 0);
    assert_eq!(status(" 1 <= 1 ;").unwrap(), 1);
    assert_eq!(status(" 1 <= 2 ;").unwrap(), 1);
    assert_eq!(status(" 0 > 1 ;").unwrap(), 0);
    assert_eq!(status(" 1 > 1 ;").unwrap(), 0);
    assert_eq!(status(" 2 > 1 ;").unwrap(), 1);
    assert_eq!(status(" 0 >= 1 ;").unwrap(), 0);
    assert_eq!(status(" 1 >= 1 ;").unwrap(), 1);
    assert_eq!(status(" 2 >= 1 ;").unwrap(), 1);
    assert_eq!(status(" -( -3 + -5 ) / 2 == 4 ;").unwrap(), 1);
    assert_eq!(status(" -( -3 + -5 ) / 2 == 5 ;").unwrap(), 0);
}

#[test]
fn variable_ok() {
    assert_eq!(
        status(
            r#"
            a = 3;
            b = 5 * 6 - 8;
            a + b / 2;
            "#
        )
        .unwrap(),
        14,
    );
    assert_eq!(
        status(
            r#"
            first = 3;
            second = 5 * 6 - 8;
            3 * first + second / 2;
            "#
        )
        .unwrap(),
        20,
    );
}

#[test]
fn return_ok() {
    assert_eq!(status("return 3;").unwrap(), 3);
    assert_eq!(status("return (3);").unwrap(), 3);
    assert_eq!(status("return(3);").unwrap(), 3);
    assert_eq!(status("return\n3;").unwrap(), 3);
    assert_eq!(status("return\n(3);").unwrap(), 3);
    assert_eq!(status("return 3; return 4;").unwrap(), 3);
    assert_eq!(status("return 3 + 3 * 4;").unwrap(), 15);
    assert_eq!(
        status(
            r#"
            first = 3 * 5;
            second = 20 - first;
            return second * 2;
            "#
        )
        .unwrap(),
        10,
    );
}

#[test]
fn if_ok() {
    assert_eq!(
        status(
            r#"
            first = 2;
            second = 0;
            if (first < 3)
                second = first / 2;
            return second;
            "#
        )
        .unwrap(),
        1,
    );
    assert_eq!(
        status(
            r#"
            first = 4;
            second = 0;
            if (first < 3)
                second = first / 2;
            return second;
            "#
        )
        .unwrap(),
        0,
    );
    assert_eq!(
        status(
            r#"
            first = 4;
            if (first < 3)
                second = first / 2;
            else
                second = first * 2;
            return second;
            "#
        )
        .unwrap(),
        8,
    );
    assert_eq!(
        status(
            r#"
            first = 2;
            if (first < 3)
                second = first / 2;
            else
                second = first * 2;
            return second;
            "#
        )
        .unwrap(),
        1,
    );
}

#[test]
fn while_ok() {
    assert_eq!(
        status(
            r#"
            first = 0;
            while (first < 10)
                first = first + 1;
            return first;
            "#
        )
        .unwrap(),
        10,
    );
}

#[test]
fn for_ok() {
    assert_eq!(
        status(
            r#"
            second = 0;
            for (first = 0; first < 10; first = first + 1)
                second = first;
            return second;
            "#
        )
        .unwrap(),
        9,
    );
}

#[test]
fn block_ok() {
    assert_eq!(
        status(
            r#"
            {
                second = 0;
                for (first = 0; first < 10; first = first + 1)
                    second = first;
                return second;
            }
            "#
        )
        .unwrap(),
        9,
    );
}
