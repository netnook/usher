use enumset::{EnumSet, EnumSetType};

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
