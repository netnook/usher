use crate::parser::{self};
use pretty_assertions::assert_eq;
use std::{fs, path::PathBuf};

#[test]
fn test_progs() -> Result<(), std::io::Error> {
    let mut count = 0;

    let entries = list_program_files()?;
    for entry in entries {
        println!("**** testing: {:?}", entry);

        let src = fs::read_to_string(&entry)?;

        let (input, expected) = src
            .split_once("---")
            .unwrap_or_else(|| panic!("{entry:?}: split failed."));

        let prog = parser::parse(input).unwrap_or_else(|_| panic!("{entry:?}: parse ok"));

        let printed = match prog.run() {
            Ok(ok) => ok.print(),
            Err(err) => err.print(),
        };

        assert_eq!(
            printed.trim(),
            expected.trim(),
            "{entry:?}: output did not match expectation"
        );

        count += 1;
    }

    assert_eq!(
        count, 4,
        "Expected different count for processed program files"
    );

    Ok(())
}

fn list_program_files() -> Result<Vec<PathBuf>, std::io::Error> {
    let dir = {
        let dir = std::env::var("CARGO_MANIFEST_DIR").expect("get CARGO_MANIFEST_DIR");
        let mut dir = PathBuf::from(dir);
        dir.push("src/lang/tests");
        dir
    };

    let entries = fs::read_dir(dir).unwrap();
    let mut results = Vec::new();

    for entry in entries {
        let path = entry?.path();
        if !path.is_file() {
            continue;
        }

        if path.extension().is_none_or(|e| e != "usher") {
            continue;
        }

        results.push(path);
    }

    Ok(results)
}
