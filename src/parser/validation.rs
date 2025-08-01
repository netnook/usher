use enumset::{EnumSet, EnumSetType};

#[allow(dead_code)]
type FlagSet = EnumSet<Flags>;

#[derive(EnumSetType, Debug)]
pub(crate) enum Flags {
    InterpolatedStr,
    ReturnVal,
    AssignLhs,
    AssignRhs,
    KvValue,
    FuncParamDefault,
    FuncBody,
    BinaryOp,
    UnaryOp,
    PropOf,
    IndexOf,
    IndexVal,
    IfCond,
}
