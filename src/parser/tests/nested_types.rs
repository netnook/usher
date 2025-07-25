use super::{b, do_test_parser_some, i, id, nil, s};
use crate::{
    lang::{ListBuilder, MapBuilder},
    parser::Parser,
};

#[test]
fn test_nested() {
    do_test_parser_some(
        Parser::expression,
        r#" map(a:map(a:false, b:"xxx",) , b:nil, c:list(1, 2, 3,), the_d:"bar") "#,
        MapBuilder::new(vec![
            (
                id("a").into(),
                MapBuilder::new(vec![
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
        r#" list(map(a:"one")) "#,
        ListBuilder::new(vec![
            MapBuilder::new(vec![(id("a").into(), s("one").into())]).into(),
        ])
        .into(),
        -1,
    );
}
