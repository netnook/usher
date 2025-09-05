use pretty_assertions::assert_eq;
use std::path::Path;

datatest_stable::harness! {
    { test = compile_script, root = "tests/compile", pattern = ".usher" },
    { test = run_script, root = "tests/run", pattern = ".usher" },
}

fn compile_script(_path: &Path, src: String) -> datatest_stable::Result<()> {
    let (input, expected) = src
        .split_once("---")
        .expect("src expected to have parts separated by ---");

    let prog = usher::parse(input).expect("script to parse ok");

    let actual = format!("{prog:-#?}");
    // let actual = format!("{prog:#?}");

    assert_eq!(
        actual.trim(),
        expected.trim(),
        "output did not match expectation"
    );

    Ok(())
}

fn run_script(_path: &Path, src: String) -> datatest_stable::Result<()> {
    let (input, expected) = src
        .split_once("---")
        .expect("src expected to have parts separated by ---");

    let prog = usher::parse(input).expect("script to parse ok");

    let actual = match prog.run() {
        Ok(ok) => format!("{ok:#?}"),
        Err(err) => format!("{err:#?}"),
    };

    assert_eq!(
        actual.trim(),
        expected.trim(),
        "output did not match expectation"
    );

    Ok(())
}
