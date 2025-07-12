use super::{b, i, ident, nil, s};
use crate::{
    lang::{AstNode, ListBuilder, ObjectBuilder},
    parser::Parser,
};
use pretty_assertions::assert_eq;

#[track_caller]
fn do_test_nested_ok(input: &str, expected: AstNode, len_remaining: usize) {
    let mut p = Parser::new(input);
    p.pos = 1;
    let actual = p.expression().expect("parse ok").expect("result found");
    assert_eq!(expected, actual);
    assert_eq!(p.pos, input.len() - len_remaining);
}
#[test]
fn test_nested() {
    do_test_nested_ok(
        r#"-{a:{a:false b:"xxx"} b:nil c:[1 2 3] the_d:"bar"}-"#,
        ObjectBuilder::new(vec![
            (
                ident("a").into(),
                ObjectBuilder::new(vec![
                    (ident("a").into(), b(false).into()),
                    (ident("b").into(), s("xxx").into()),
                ])
                .into(),
            ),
            (ident("b").into(), nil().into()),
            (
                ident("c").into(),
                ListBuilder::new(vec![i(1).into(), i(2).into(), i(3).into()]).into(),
            ),
            (ident("the_d").into(), s("bar").into()),
        ])
        .into(),
        1,
    );
    do_test_nested_ok(
        r#"-[{a:"one"}]-"#,
        ListBuilder::new(vec![
            ObjectBuilder::new(vec![(ident("a").into(), s("one").into())]).into(),
        ])
        .into(),
        1,
    );
}
