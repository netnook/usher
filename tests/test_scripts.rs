use pretty_assertions::assert_eq;
use std::path::Path;
use usher::printer;

datatest_stable::harness! {
    { test = compile_error_detailed, root = "tests/compile-error-detailed", pattern = ".usher" },
    { test = compile_script, root = "tests/compile", pattern = ".usher" },
    { test = run_script, root = "tests/run", pattern = ".usher" },
}

fn compile_script(path: &Path, src: String) -> datatest_stable::Result<()> {
    for part in src.split("*****\n") {
        let src = part.trim_end_matches('*').trim();

        let (input, expected) = src
            .split_once("---\n")
            .expect("src expected to have parts separated by ---");

        let input = input.trim_end_matches('-');

        let actual = match usher::parse(path.to_str().unwrap(), input) {
            Ok(prog) => format!("{prog:-#?}"),
            Err(err) => format!("{err:-#?}"),
        };

        assert_eq!(
            actual.trim(),
            expected.trim(),
            "output did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
            src.lines().next().unwrap()
        );
    }

    Ok(())
}

fn compile_error_detailed(path: &Path, src: String) -> datatest_stable::Result<()> {
    for part in src.split("*****\n") {
        let src = part.trim_end_matches('*').trim();

        let (input, expected) = src
            .split_once("---\n")
            .expect("src expected to have parts separated by ---");

        let input = input.trim_end_matches('-');

        let actual = match usher::parse(path.to_str().unwrap(), input) {
            Ok(prog) => format!("{prog:-#?}"),
            Err(err) => printer::print_parse_error_to_string(err),
        };

        assert_eq!(
            actual.trim(),
            expected.trim(),
            "output did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
            src.lines().next().unwrap()
        );
    }

    Ok(())
}

fn run_script(path: &Path, src: String) -> datatest_stable::Result<()> {
    for part in src.split("*****\n") {
        let src = part.trim_end_matches('*').trim();

        let (input, expected) = src
            .split_once("---\n")
            .expect("src expected to have parts separated by ---");

        let input = input.trim_end_matches('-');

        let prog = usher::parse(path.to_str().unwrap(), input).expect("script to parse ok");

        let actual = match prog.eval() {
            Ok(ok) => format!("{ok:#?}"),
            Err(err) => printer::print_eval_error_to_string(err),
        };

        assert_eq!(
            actual.trim(),
            expected.trim(),
            "output did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
            src.lines().next().unwrap()
        );
    }
    Ok(())
}
