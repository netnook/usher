use crate::lang::{Dict, EvalStop, InternalProgramError, List, Span, StringCell, Value, ValueType};

#[allow(dead_code)] // FIXME remove allow once in use
pub struct MacroUtils {}

#[allow(dead_code)] // FIXME remove allow once in use
impl MacroUtils {
    pub(crate) fn to_string(
        val: Value,
        span: Span,
        param_name: &str,
    ) -> Result<StringCell, EvalStop> {
        match val {
            Value::Str(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::String, span, param_name)),
        }
    }
    pub(crate) fn to_int(val: Value, span: Span, param_name: &str) -> Result<isize, EvalStop> {
        match val {
            Value::Integer(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Integer, span, param_name)),
        }
    }
    pub(crate) fn to_float(val: Value, span: Span, param_name: &str) -> Result<f64, EvalStop> {
        match val {
            Value::Float(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Float, span, param_name)),
        }
    }
    pub(crate) fn to_bool(val: Value, span: Span, param_name: &str) -> Result<bool, EvalStop> {
        match val {
            Value::Bool(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Boolean, span, param_name)),
        }
    }
    pub(crate) fn to_list(val: Value, span: Span, param_name: &str) -> Result<List, EvalStop> {
        match val {
            Value::List(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::List, span, param_name)),
        }
    }
    pub(crate) fn to_dict(val: Value, span: Span, param_name: &str) -> Result<Dict, EvalStop> {
        match val {
            Value::Dict(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Dict, span, param_name)),
        }
    }
    fn type_error(val: Value, expected: ValueType, span: Span, param_name: &str) -> EvalStop {
        InternalProgramError::FunctionCallBadArgType {
            name: param_name.to_string(),
            expected,
            actual: val.value_type(),
            span,
        }
        .into()
    }
    pub(crate) fn param_already_set_error(param_name: &str, span: Span) -> EvalStop {
        InternalProgramError::FunctionCallParamAlreadySet {
            name: param_name.to_string(),
            span,
        }
        .into()
    }
    pub(crate) fn missing_argument_error(param_name: &str, span: Span) -> EvalStop {
        InternalProgramError::FunctionCallMissingRequiredArgument {
            name: param_name.to_string(),
            span,
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::{Context, Dict, List, Nillable, Span},
        parser::tests::{_function_call, arg, b, dict, f, i, id, l, list, nil, s},
    };
    use usher_macros::UsherArgs;

    #[test]
    fn test_arg_structs_types() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: StringCell,
            bbb: isize,
            ccc: f64,
            ddd: bool,
            eee: List,
            fff: Dict,
            ggg: Value,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(i(42)),
            arg!(f(42.3)),
            arg!(b(true)),
            arg!(l(list!().into())),
            arg!(l(dict!().into())),
            arg!(s("foo"))
        );
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, "a-string".into());
        assert_eq!(args.bbb, 42);
        assert_eq!(args.ccc, 42.3);
        assert!(args.ddd);
        assert_eq!(args.eee, List::new());
        assert_eq!(args.fff, Dict::new());
        assert_eq!(args.ggg, Value::Str("foo".into()));
    }

    #[test]
    fn test_arg_structs_required_present() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: StringCell,
            bbb: isize,
        }

        let call = _function_call!("foo", arg!(s("a-string")), arg!(i(42)));
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, "a-string".into());
        assert_eq!(args.bbb, 42);
    }

    #[test]
    fn test_arg_structs_required_missing() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
            _bbb: isize,
        }

        let call = _function_call!("foo",).spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallMissingRequiredArgument {
                name: "_aaa".to_string(),
                span: Span::new(10, 5)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_optional_present() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Option<StringCell>,
            bbb: Option<isize>,
        }

        let call = _function_call!("foo", arg!(s("a-string")), arg!(i(42)));
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, Some("a-string".into()));
        assert_eq!(args.bbb, Some(42));
    }

    #[test]
    fn test_arg_structs_optional_missing() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Option<StringCell>,
            bbb: Option<isize>,
        }

        let call = _function_call!("foo",);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, None);
        assert_eq!(args.bbb, None);
    }

    #[test]
    fn test_arg_structs_nillable() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Nillable<StringCell>,
            bbb: Nillable<isize>,
        }

        let call = _function_call!("foo", arg!(nil()), arg!(i(42)));
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, Nillable::Nil);
        assert_eq!(args.bbb, Nillable::Some(42));
    }

    #[test]
    fn test_arg_structs_with_arg_index() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: (usize, StringCell),
            bbb: (usize, StringCell),
            ccc: (usize, StringCell),
        }

        let call = _function_call!(
            "foo",
            arg!("aaa", s("foo-1")),
            arg!("ccc", s("foo-2")),
            arg!("bbb", s("foo-3"))
        );
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, (0, "foo-1".into()));
        assert_eq!(args.bbb, (2, "foo-3".into()));
        assert_eq!(args.ccc, (1, "foo-2".into()));
    }

    // FIXME test more combinations of nillbale/option/...

    #[test]
    fn test_arg_structs_named_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: StringCell,
            bbb: StringCell,
            ccc: StringCell,
            ddd: StringCell,
            eee: StringCell,
            fff: StringCell,
        }

        let call = _function_call!(
            "foo",
            arg!("aaa", s("val-aaa")),
            arg!("bbb", s("val-bbb")),
            arg!("ccc", s("val-ccc")),
            arg!("ddd", s("val-ddd")),
            arg!("eee", s("val-eee")),
            arg!("fff", s("val-fff"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, "val-aaa".into());
        assert_eq!(args.bbb, "val-bbb".into());
        assert_eq!(args.ccc, "val-ccc".into());
        assert_eq!(args.ddd, "val-ddd".into());
        assert_eq!(args.eee, "val-eee".into());
        assert_eq!(args.fff, "val-fff".into());
    }

    #[test]
    fn test_arg_structs_star_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Vec<StringCell>,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(s("b-string")),
            arg!(s("c-string"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(
            args.aaa,
            vec!["a-string".into(), "b-string".into(), "c-string".into(),]
        );
    }

    #[test]
    fn test_arg_structs_mixed_positional_and_named_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: isize,
            bbb: isize,
            ccc: isize,
            ggg: Vec<isize>,
            ddd: isize,
            eee: isize,
            fff: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(i(1)),
            arg!(i(2)),
            arg!(i(3)),
            arg!(i(7)),
            arg!(i(8)),
            arg!("ddd", i(4)),
            arg!("fff", i(5)),
            arg!("eee", i(6))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, 1);
        assert_eq!(args.bbb, 2);
        assert_eq!(args.ccc, 3);
        assert_eq!(args.ddd, 4);
        assert_eq!(args.eee, 6);
        assert_eq!(args.fff, 5);
        assert_eq!(args.ggg, vec![7, 8]);
    }

    #[test]
    fn test_arg_structs_too_many_args() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
            _bbb: StringCell,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(s("b-string")),
            arg!(s("c-string").spanned(42, 24))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallUnexpectedArgument {
                span: Span::new(42, 24)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_named_arg() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
            _bbb: StringCell,
        }

        let call = _function_call!(
            "foo",
            arg!("_aaa", s("a-string")),
            arg!(id("bad").spanned(42, 24), s("bad-string")),
            arg!("_bbb", s("b-string"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallNoSuchParameter {
                name: "bad".to_string(),
                span: Span::new(42, 24)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_positional() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
        }

        let call = _function_call!("foo", arg!(i(22).spanned(12, 13)));
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::String,
                actual: ValueType::Integer,
                span: Span::new(12, 13)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_named() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(id("_aaa").spanned(3, 4), s("bad").spanned(5, 6))
        );
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::Integer,
                actual: ValueType::String,
                span: Span::new(3, 8)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_star() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: Vec<f64>,
        }

        let call = _function_call!("foo", arg!(f(1.1)), arg!(b(true).spanned(5, 6)));
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::Float,
                actual: ValueType::Boolean,
                span: Span::new(5, 6)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_eval_order() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs2 {
            aaa: isize,
            bbb: isize,
            star: Vec<isize>,
            ccc: isize,
            ddd: isize,
            eee: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!("ddd", _function_call!(id("test_next_id"),)),
            arg!("ccc", _function_call!(id("test_next_id"),)),
            arg!("eee", _function_call!(id("test_next_id"),))
        );
        let mut ctxt = Context::default();

        let args = TestArgs2::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, 0);
        assert_eq!(args.bbb, 1);
        assert_eq!(args.star, vec![2, 3]);
        assert_eq!(args.ccc, 5);
        assert_eq!(args.ddd, 4);
        assert_eq!(args.eee, 6);
    }
}
