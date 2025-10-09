use pretty_assertions::assert_eq;
use std::{cell::RefCell, path::Path, rc::Rc};
use usher::{
    lang::{Context, Output},
    printer,
};

datatest_stable::harness! {
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

fn run_script(path: &Path, src: String) -> datatest_stable::Result<()> {
    for part in src.split("*****\n") {
        let src = part.trim_end_matches('*').trim();

        let end = src
            .find("\n---")
            .expect("expect at least one out/result section");

        let input = &src[..end];

        let mut rest = &src[end + 1..];

        let mut expected_stdout = None;
        let mut expected_stderr = None;
        let mut expected_result = None;

        loop {
            if rest.is_empty() {
                break;
            }

            let (key, body, r) = next_section(rest);

            match key {
                "stdout" => expected_stdout
                    .replace(body)
                    .is_none_or(|_| panic!("stdout already defined")),
                "stderr" => expected_stderr
                    .replace(body)
                    .is_none_or(|_| panic!("stderr already defined")),
                "result" => expected_result
                    .replace(body)
                    .is_none_or(|_| panic!("result already defined")),
                _ => panic!("unknown key '{key}'"),
            };

            rest = r;
        }

        let prog = usher::parse(path.to_str().unwrap(), input); //.expect("script to parse ok");

        let stdout_buf = Rc::new(RefCell::new(Vec::new()));
        let stdout = Output::Capture(stdout_buf.clone());

        let stderr_buf = Rc::new(RefCell::new(Vec::new()));
        let mut stderr = Output::Capture(stderr_buf.clone());

        let actual_result = match prog {
            Ok(prog) => {
                let mut ctxt = Context::default();
                ctxt.set_stdout(stdout.clone());
                ctxt.set_stderr(stderr.clone());

                let result = prog.eval_with_context(&mut ctxt);

                let actual_result = match result {
                    Ok(ok) => format!("{ok:#?}"),
                    Err(e) => {
                        printer::print_eval_error(&mut stderr, e)?;
                        "".to_string()
                    }
                };

                actual_result
            }
            Err(e) => {
                printer::print_parse_error(&mut stderr, e)?;
                "".to_string()
            }
        };

        let actual_stdout = String::from_utf8(stdout_buf.take()).unwrap();
        let actual_stderr = String::from_utf8(stderr_buf.take()).unwrap();
        // println!("actual out: {actual_stdout}");
        // println!("actual err: {actual_stderr}");

        if let Some(expected_result) = expected_result {
            assert_eq!(
                actual_result.trim(),
                expected_result.trim(),
                "result output did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
                src.lines().next().unwrap()
            );
        }
        //

        if let Some(expected_stdout) = expected_stdout {
            assert_eq!(
                actual_stdout.trim(),
                expected_stdout.trim(),
                "stdout did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
                src.lines().next().unwrap()
            );
        }

        if let Some(expected_stderr) = expected_stderr {
            assert_eq!(
                actual_stderr.trim(),
                expected_stderr.trim(),
                "stderr did not match expectation for >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> '{}'",
                src.lines().next().unwrap()
            );
        }
    }
    Ok(())
}

fn next_section<'a>(src: &'a str) -> (&'a str, &'a str, &'a str) {
    if !src.starts_with("---") {
        panic!("expected src to start with '---' but got {}", src);
    }
    let key_start = find_after(src, 3, |c| c != '-').expect("non '-' char should be avaiable");
    let key_end = find_after(src, key_start, |c| c == '-').expect("'-' char should be avaiable");
    let key = &src[key_start..key_end];
    if key.is_empty() {
        panic!("missing key");
    }

    let body_start = find_after(src, key_end, |c| c == '\n').unwrap_or_else(|| src.len());

    let body_end = find_str_after(src, body_start, "\n---")
        .map(|i| i + 1)
        .unwrap_or(src.len());

    let body = if body_start == body_end {
        &src[body_start..body_end]
    } else {
        &src[body_start + 1..body_end]
    };

    (key, body, &src[body_end..])
}

fn find_after<F>(src: &str, start: usize, f: F) -> Option<usize>
where
    F: Fn(char) -> bool,
{
    src[start..].find(f).map(|i| i + start)
}

fn find_str_after(src: &str, start: usize, s: &str) -> Option<usize> {
    src[start..].find(s).map(|i| i + start)
}
