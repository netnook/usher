use super::{b, i, id, kv, nil, s};
use crate::parser::{
    expression::tests::do_test_expr_ok,
    tests::{dict_builder, list_builder},
};

#[test]
fn test_nested() {
    do_test_expr_ok(
        r#" dict(a:dict(a:false, b:"xxx",) , b:nil, c:[1, 2, 3,], the_d:"bar") "#,
        dict_builder(vec![
            kv(
                id("a"),
                dict_builder(vec![kv(id("a"), b(false)), kv(id("b"), s("xxx"))]),
            ),
            kv(id("b"), nil()),
            kv(id("c"), list_builder!(i(1), i(2), i(3))),
            kv(id("the_d"), s("bar")),
        ]),
        -1,
    );
    do_test_expr_ok(
        r#" [dict(a:"one")] "#,
        list_builder!(dict_builder(vec![kv(id("a"), s("one"))])),
        -1,
    );
}
