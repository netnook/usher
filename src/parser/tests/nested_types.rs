use super::{b, do_test_parser_some, i, id, nil, s};
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
            (
                id("a").into(),
                DictBuilder::new(vec![
                    (id("a").into(), b(false).into()),
                    (id("b").into(), s("xxx").into()),
                ])
                .into(),
            ),
            (id("b").into(), nil().into()),
            (
                id("c").into(),
                ListBuilder::new(vec![i(1).into(), i(2).into(), i(3).into()]).into(),
            ),
            (id("the_d").into(), s("bar").into()),
        ])
        .into(),
        -1,
    );
    do_test_parser_some(
        Parser::expression,
        r#" [dict(a:"one")] "#,
        ListBuilder::new(vec![
            DictBuilder::new(vec![(id("a").into(), s("one").into())]).into(),
        ])
        .into(),
        -1,
    );
}
