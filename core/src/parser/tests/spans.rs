use super::*;
use pretty_assertions::assert_eq;

#[track_caller]
fn do_test_parser_exact(input: &'static str, expected: AstNode, expected_end: isize) {
    let mut parser = Parser::new("dummy", input);
    parser.pos = 1;

    let actual = parser
        .stmt()
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
        r#"_"ab{ foo -45 }cde{ 35 }"_"#,
        _interp![
            s("ab").spanned(2, 2),
            sub(var(id("foo").spanned(6, 3)), i(45).spanned(11, 2)),
            s("cde").spanned(15, 3),
            i(35).spanned(20, 2)
        ]
        .spanned(1, 24)
        .into(),
        -1,
    );
}

#[test]
fn test_list_spans() {
    do_test_parser_exact(
        r#" [111, "xxx"] "#,
        list_builder!(i(111).spanned(2, 3), s("xxx").spanned(7, 5))
            .spanned(1, 12)
            .into(),
        -1,
    );
}

#[test]
fn test_dict_spans() {
    do_test_parser_exact(
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
        " var a=xx>=2 ",
        decl(
            var(id("a").spanned(5, 1)),
            greater_equal(var(id("xx").spanned(7, 2)), i(2).spanned(11, 1)),
        )
        .spanned(1, 11)
        .into(),
        -1,
    );
    do_test_parser_exact(
        " var # comment \n a = # comment \n xyz + 23 ",
        decl(
            var(id("a").spanned(17, 1)),
            add(var(id("xyz").spanned(33, 3)), i(23).spanned(39, 2)),
        )
        .spanned(1, 40)
        .into(),
        -1,
    );
    do_test_parser_exact(
        " var a = !(-b==c) ",
        decl(
            var(id("a").spanned(5, 1)),
            not(equal(
                neg(var(id("b").spanned(12, 1))).spanned(11, 1),
                var(id("c").spanned(15, 1)),
            ))
            .spanned(9, 1),
        )
        .spanned(1, 16)
        .into(),
        -1,
    );
    do_test_parser_exact(
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
        r#" abc.def?[ghi]? "#,
        catch_missing_optional_property(
            index_of(
                prop_of(var(id("abc").spanned(1, 3)), id("def").spanned(5, 3))
                    .with_optional_property(true)
                    .spanned(4, 5),
                var(id("ghi").spanned(10, 3)),
            )
            .with_optional_property(true)
            .spanned(9, 6),
        ),
        -1,
    );
    do_test_parser_exact(
        r#" aaa(123).bbb(456) "#,
        _method_call!(
            _function_call!(id("aaa").spanned(1, 3), arg!(i(123).spanned(5, 3))).spanned(1, 8),
            id("bbb").spanned(10, 3),
            arg!(i(456).spanned(14, 3))
        )
        .spanned(9, 9)
        .into(),
        -1,
    );
    do_test_parser_exact(
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
    do_test_parser_exact(" 1234 ", i(1234).spanned(1, 4).into(), -1);
    do_test_parser_exact(" -1234 ", i(-1234).spanned(1, 5).into(), -1);
    do_test_parser_exact(" 12.34 ", f(12.34).spanned(1, 5).into(), -1);
    do_test_parser_exact(" -12.34 ", f(-12.34).spanned(1, 6).into(), -1);
    do_test_parser_exact(" 12. ", f(12.0).spanned(1, 3).into(), -1);
    do_test_parser_exact(" -12. ", f(-12.0).spanned(1, 4).into(), -1);
    do_test_parser_exact(" \"xy\\\"z\" ", s("xy\"z").spanned(1, 7).into(), -1);
    do_test_parser_exact(" true ", b(true).spanned(1, 4).into(), -1);
    do_test_parser_exact(" false ", b(false).spanned(1, 5).into(), -1);
    do_test_parser_exact(" true", b(true).spanned(1, 4).into(), 0);

    do_test_parser_exact(" nil2 ", var(id("nil2").spanned(1, 4)).into(), -1);
}

#[test]
fn test_nil_spans() {
    do_test_parser_exact(" nil ", nil().spanned(1, 3).into(), -1);
    do_test_parser_exact(" nil2 ", var(id("nil2").spanned(1, 4)).into(), -1);
}

#[test]
fn test_break_spans() {
    do_test_parser_exact(r#" break "#, brk!().spanned(1, 5).into(), -1);
    do_test_parser_exact(
        r#" break 42 "#,
        brk!(i(42).spanned(7, 2)).spanned(1, 8).into(),
        -1,
    );
}

#[test]
fn test_continue_spans() {
    do_test_parser_exact(r#" continue "#, _continue().spanned(1, 8).into(), -1);
}

#[test]
fn test_declaration_spans() {
    do_test_parser_exact(
        r#" var a = 1 "#,
        decl(var(id("a").spanned(5, 1)), i(1).spanned(9, 1))
            .spanned(1, 9)
            .into(),
        -1,
    );
}

#[test]
fn test_this_spans() {
    do_test_parser_exact(r#" this "#, this().spanned(1, 4).into(), -1);
}

#[test]
fn test_end_spans() {
    do_test_parser_exact(r#" end "#, end().spanned(1, 3).into(), -1);
}

#[test]
fn test_ifelse_spans() {
    do_test_parser_exact(
        r#" if a { 1 } else if b { 2 } else { 3 } "#,
        _if!(
            cond(var(id("a").spanned(4, 1)) => _block![i(1).spanned(8, 1)].spanned(6, 5)),
            cond(var(id("b").spanned(20, 1)) => _block![i(2).spanned(24, 1)].spanned(22, 5)),
            else(_block![i(3).spanned(35, 1)].spanned(33, 5))
        )
        .spanned(1, 37)
        .into(),
        -1,
    );
    do_test_parser_exact(
        r#" if a { 1 } "#,
        _if!(
            cond(var(id("a").spanned(4, 1)) => _block![i(1).spanned(8, 1)].spanned(6, 5))
        )
        .spanned(1, 10)
        .into(),
        -1,
    );
}

#[test]
fn test_for_spans() {
    do_test_parser_exact(
        r#" for a in b { 1 } "#,
        _for!(
            var(id("a").spanned(5, 1));
            var(id("b").spanned(10, 1));
            _block![i(1).spanned(14, 1)].spanned(12, 5)
        )
        .spanned(1, 16)
        .into(),
        -1,
    );
}

#[test]
fn test_return_spans() {
    do_test_parser_exact(" return \n a ", ret!().spanned(1, 6).into(), -5);
    do_test_parser_exact(
        " return 2+22",
        ret!(add(i(2).spanned(8, 1), i(22).spanned(10, 2)))
            .spanned(1, 11)
            .into(),
        0,
    );
}

#[test]
fn test_functiondef_spans() {
    do_test_parser_exact(
        " function foo() { 1 } ",
        _func!(
            name(id("foo").spanned(10, 3)),
            _block![i(1).spanned(18, 1)].spanned(16, 5)
        )
        .spanned(1, 20)
        .into(),
        -1,
    );
}
