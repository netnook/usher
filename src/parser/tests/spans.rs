use super::*;
use pretty_assertions::assert_eq;

#[track_caller]
fn do_test_parser_exact<'a, F, T>(func: F, input: &'static str, expected: T, expected_end: isize)
where
    F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
    T: PartialEq<T> + std::fmt::Debug,
{
    let mut parser = Parser::new("dummy", input);
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
            sub(var(id("foo").spanned(6, 3)), i(45).spanned(11, 2)).spanned(10, 1),
            s("cde").spanned(15, 3),
            i(35).spanned(20, 2)
        ],
        -1,
    );
}

#[test]
fn test_list_spans() {
    do_test_parser_exact(
        Parser::expression,
        r#" [111, "xxx"] "#,
        list!(i(111).spanned(2, 3), s("xxx").spanned(7, 5))
            .spanned(1, 12)
            .into(),
        -1,
    );
}

#[test]
fn test_dict_spans() {
    do_test_parser_exact(
        Parser::expression,
        r#" dict(a:1,b : 22 ) "#,
        dict_builder(vec![
            kv(id("a").spanned(6, 1), i(1).spanned(8, 1)),
            kv(id("b").spanned(10, 1), i(22).spanned(14, 2)),
        ])
        .spanned(1, 17)
        .into(),
        -1,
    );
}

#[test]
fn test_stmt_spanneds() {
    do_test_parser_exact(
        Parser::stmt,
        " var a=xx>=2 ",
        decl(
            var(id("a").spanned(5, 1)),
            greater_equal(var(id("xx").spanned(7, 2)), i(2).spanned(11, 1)).spanned(9, 2),
        )
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        " var # comment \n a = # comment \n xyz + 23 ",
        decl(
            var(id("a").spanned(17, 1)),
            add(var(id("xyz").spanned(33, 3)), i(23).spanned(39, 2)).spanned(37, 1),
        )
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        " var a = !(-b==c) ",
        decl(
            var(id("a").spanned(5, 1)),
            not(equal(
                neg(var(id("b").spanned(12, 1))).spanned(11, 1),
                var(id("c").spanned(15, 1)),
            )
            .spanned(13, 2))
            .spanned(9, 1),
        )
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        " abc.def.ghi ",
        prop_of(
            prop_of(var(id("abc").spanned(1, 3)), id("def").spanned(5, 3)).spanned(4, 4),
            id("ghi").spanned(9, 3),
        )
        .spanned(8, 4)
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        r#" abc[a][123] "#,
        index_of(
            index_of(var(id("abc").spanned(1, 3)), var(id("a").spanned(5, 1))).spanned(4, 3),
            i(123).spanned(8, 3),
        )
        .spanned(7, 5)
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        r#" abc.def?[ghi]? "#,
        chain_catch(
            index_of(
                prop_of(var(id("abc").spanned(1, 3)), id("def").spanned(5, 3))
                    .with_throw_on_missing_prop(true)
                    .spanned(4, 5),
                var(id("ghi").spanned(10, 3)),
            )
            .with_throw_on_missing_prop(true)
            .spanned(9, 6),
        ),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        r#" aaa(123).bbb(456) "#,
        _call!(
            _call!(var(id("aaa").spanned(1, 3)), arg(i(123).spanned(5, 3)),).spanned(1, 8),
            method(id("bbb").spanned(10, 3)),
            arg(i(456).spanned(14, 3)),
        )
        .spanned(9, 9)
        .into(),
        -1,
    );
    do_test_parser_exact(
        Parser::stmt,
        " { \n abc = 2 \n 42 \n } ",
        _block!(
            assign(var(id("abc").spanned(5, 3)), i(2).spanned(11, 1)),
            i(42).spanned(15, 2)
        )
        .spanned(1, 20)
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
        var(id("nil2").spanned(1, 4)).into(),
        -1,
    );
}

#[test]
fn test_nil_spans() {
    do_test_parser_exact(Parser::expression, " nil ", nil().spanned(1, 3).into(), -1);
    do_test_parser_exact(
        Parser::expression,
        " nil2 ",
        var(id("nil2").spanned(1, 4)).into(),
        -1,
    );
}

#[test]
fn test_break_spans() {
    do_test_parser_exact(
        Parser::stmt,
        r#" break "#,
        _break().spanned(1, 5).into(),
        -1,
    );
}

#[test]
fn test_continue_spans() {
    do_test_parser_exact(
        Parser::stmt,
        r#" continue "#,
        _continue().spanned(1, 8).into(),
        -1,
    );
}

#[test]
fn test_end_spans() {
    do_test_parser_exact(Parser::stmt, r#" end "#, _end().spanned(1, 3).into(), -1);
}
