use super::{b, do_test_parser_some, i, id, kv, nil, s};
use crate::{
    lang::{DictBuilder, ListBuilder},
    parser::Parser,
};

#[test]
fn test_nested() {
    do_test_parser_some(
        Parser::expression,
        r#" dict(a:dict(a:false, b:"xxx",) , b:nil, c:[1, 2, 3,], the_d:"bar") "#,
        DictBuilder::new(vec![
            kv(
                id!("a"),
                DictBuilder::new(vec![kv(id!("a"), b!(false)), kv(id!("b"), s!("xxx"))]),
            ),
            kv(id!("b"), nil!()),
            kv(
                id!("c"),
                ListBuilder::new(vec![i!(1).into(), i!(2).into(), i!(3).into()]),
            ),
            kv(id!("the_d"), s!("bar")),
        ])
        .into(),
        -1,
    );
    do_test_parser_some(
        Parser::expression,
        r#" [dict(a:"one")] "#,
        ListBuilder::new(vec![DictBuilder::new(vec![kv(id!("a"), s!("one"))]).into()]).into(),
        -1,
    );
}
