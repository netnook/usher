use super::*;
use pretty_assertions::assert_eq;

#[track_caller]
fn do_test_parser_exact<'a, F, T>(func: F, input: &'static str, expected: T, expected_end: isize)
where
    F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
    T: PartialEq<T> + std::fmt::Debug,
{
    let mut parser = Parser::new(input);
    parser.pos = 1;

    let actual = func(&mut parser)
        .expect("parser should succeed")
        .expect("with some");

    assert_eq!(actual, expected, "assert actual (left) == expected (right)");

    if expected_end > 0 {
        assert_eq!(
            parser.pos, expected_end as usize,
            "assert actual_end ({}) == expected_end ({expected_end})",
            parser.pos
        );
    } else {
        let actual_remain = input.len() - parser.pos;
        let expected_remain = -expected_end as usize;
        assert_eq!(
            actual_remain, expected_remain,
            "assert actual_remain ({actual_remain}) == expected_remaining ({expected_remain})"
        );
    }
}

#[test]
fn test_string_spans() {
    do_test_parser_exact(
        Parser::string,
        r#"_"ab{ foo -45 }cde{ 35 }"_"#,
        _interp![
            s("ab").spanned(2, 2),
            sub(id("foo").spanned(6, 3), i(45).spanned(11, 2)).spanned(10, 1),
            s("cde").spanned(15, 3),
            i(35).spanned(20, 2)
        ],
        -1,
    );
}

#[test]
fn test_stmt_spanneds() {
    do_test_parser_exact(
        Parser::stmt,
        " var a=xx>=2 ",
        var(
            id("a").spanned(5, 1),
            greater_equal(id("xx").spanned(7, 2), i(2).spanned(11, 1)).spanned(9, 2),
        )
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        " var # comment \n a = # comment \n xyz + 23 ",
        var(
            id("a").spanned(17, 1),
            add(id("xyz").spanned(33, 3), i(23).spanned(39, 2)).spanned(37, 1),
        )
        .into(),
        -1,
    );
}

#[test]
fn test_literal_spanneds() {
    do_test_parser_exact(
        Parser::expression,
        " 1234 ",
        i(1234).spanned(1, 4).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " -1234 ",
        i(-1234).spanned(1, 5).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " 12.34 ",
        f(12.34).spanned(1, 5).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " -12.34 ",
        f(-12.34).spanned(1, 6).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " 12. ",
        f(12.0).spanned(1, 3).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " -12. ",
        f(-12.0).spanned(1, 4).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " \"xy\\\"z\" ",
        s("xy\"z").spanned(1, 7).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " true ",
        b(true).spanned(1, 4).into(),
        -1,
    );
    do_test_parser_exact(
        Parser::expression,
        " false ",
        b(false).spanned(1, 5).into(),
        -1,
    );
    do_test_parser_exact(Parser::expression, " true", b(true).spanned(1, 4).into(), 0);

    do_test_parser_exact(
        Parser::expression,
        " nil2 ",
        id("nil2").spanned(1, 4).into(),
        -1,
    );
}

#[test]
fn test_nil_spans() {
    do_test_parser_exact(Parser::expression, " nil ", nil().spanned(1, 3).into(), -1);
    do_test_parser_exact(
        Parser::expression,
        " nil2 ",
        id("nil2").spanned(1, 4).into(),
        -1,
    );
}
