use super::{b, do_test_parser_some, i, ident, nil, s};
use crate::{
    lang::{ListBuilder, ObjectBuilder},
    parser::Parser,
};

#[test]
fn test_nested() {
    do_test_parser_some(
        Parser::expression,
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
    do_test_parser_some(
        Parser::expression,
        r#"-[{a:"one"}]-"#,
        ListBuilder::new(vec![
            ObjectBuilder::new(vec![(ident("a").into(), s("one").into())]).into(),
        ])
        .into(),
        1,
    );
}
