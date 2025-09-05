use std::path::Path;

datatest_stable::harness! {
    { test = run_script, root = "tests/scripts", pattern = ".usher" },
}

fn run_script(_path: &Path, src: String) -> datatest_stable::Result<()> {
    let (input, expected) = src
        .split_once("---")
        .expect("src expected to have parts separated by ---");

    let prog = usher::parse(input).expect("script to parse ok");

    let printed = match prog.run() {
        Ok(ok) => format!("{ok:#?}"),
        Err(err) => format!("{err:#?}"),
    };

    assert_eq!(
        printed.trim(),
        expected.trim(),
        "output did not match expectation"
    );

    Ok(())
}
