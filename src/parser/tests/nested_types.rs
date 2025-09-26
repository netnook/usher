use super::{b, do_test_parser_some, i, id, kv, nil, s};
use crate::parser::{
    Parser,
    tests::{dict_builder, list},
};

#[test]
fn test_nested() {
    do_test_parser_some(
        Parser::expression,
        r#" dict(a:dict(a:false, b:"xxx",) , b:nil, c:[1, 2, 3,], the_d:"bar") "#,
        dict_builder(vec![
            kv(
                id("a"),
                dict_builder(vec![kv(id("a"), b(false)), kv(id("b"), s("xxx"))]),
            ),
            kv(id("b"), nil()),
            kv(id("c"), list!(i(1), i(2), i(3))),
            kv(id("the_d"), s("bar")),
        ])
        .into(),
        -1,
    );
    do_test_parser_some(
        Parser::expression,
        r#" [dict(a:"one")] "#,
        list!(dict_builder(vec![kv(id("a"), s("one"))])).into(),
        -1,
    );
}
