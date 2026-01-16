use crate::lang::{
    Arg, Assignment, AstNode, BinaryOp, Block, Break, CatchMissingOptionalProperty,
    ConditionalBlock, Continue, Declaration, DictBuilder, For, FunctionCall, FunctionDef, IfElse,
    IndexOf, InterpolatedStr, KeyValueBuilder, ListBuilder, Literal, Param, PropertyOf, ReturnStmt,
    This, UnaryOp, Var,
};
use std::rc::Rc;

pub(crate) enum VisitorResult<T> {
    Stop(T),
    Continue,
}

pub(crate) trait Accept<T> {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T>;
}

#[allow(unused_variables)]
pub(crate) trait Visitor<T>: Sized {
    fn go(&mut self, n: &AstNode) -> Option<T> {
        match self.visit_node(n) {
            VisitorResult::Stop(v) => Some(v),
            VisitorResult::Continue => None,
        }
    }

    fn visit_node(&mut self, n: &AstNode) -> VisitorResult<T> {
        self.do_visit_node(n)
    }

    fn do_visit_node(&mut self, n: &AstNode) -> VisitorResult<T> {
        match n {
            AstNode::This(v) => self.visit_this(v),
            AstNode::Literal(v) => self.visit_literal(v),
            AstNode::InterpolatedStr(v) => self.visit_interpolated_str(v),
            AstNode::ListBuilder(v) => self.visit_list_builder(v),
            AstNode::DictBuilder(v) => self.visit_dict_builder(v),
            AstNode::PropertyOf(v) => self.visit_property_of(v),
            AstNode::IndexOf(v) => self.visit_index_of(v),
            AstNode::UnaryOp(v) => self.visit_unary_op(v),
            AstNode::BinaryOp(v) => self.visit_binary_op(v),
            AstNode::CatchMissingOptionalProperty(v) => {
                self.visit_catch_missing_optional_property(v)
            }
            AstNode::Block(v) => self.visit_block(v),
            AstNode::IfElse(v) => self.visit_if_else(v),
            AstNode::For(v) => self.visit_for(v),
            AstNode::Var(v) => self.visit_var(v),
            AstNode::Declaration(v) => self.visit_declaration(v),
            AstNode::FunctionDef(v) => self.visit_function_def(v),
            AstNode::FunctionCall(v) => self.visit_function_call(v),
            AstNode::ReturnStmt(v) => self.visit_return_stmt(v),
            AstNode::Assignment(v) => self.visit_assignment(v),
            AstNode::KeyValue(v) => self.visit_key_value(v),
            AstNode::Break(v) => self.visit_break(v),
            AstNode::Continue(v) => self.visit_continue(v),
        }
    }

    fn visit_this(&mut self, v: &This) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_literal(&mut self, v: &Literal) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_interpolated_str(&mut self, v: &InterpolatedStr) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_list_builder(&mut self, v: &ListBuilder) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_dict_builder(&mut self, v: &DictBuilder) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_property_of(&mut self, v: &PropertyOf) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_index_of(&mut self, v: &IndexOf) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_unary_op(&mut self, v: &UnaryOp) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_binary_op(&mut self, v: &BinaryOp) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_catch_missing_optional_property(
        &mut self,
        v: &CatchMissingOptionalProperty,
    ) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_block(&mut self, v: &Block) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_if_else(&mut self, v: &IfElse) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_conditional_block(&mut self, v: &ConditionalBlock) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_for(&mut self, v: &For) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_var(&mut self, v: &Var) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_declaration(&mut self, v: &Declaration) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_function_def(&mut self, v: &Rc<FunctionDef>) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_param(&mut self, v: &Param) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_function_call(&mut self, v: &FunctionCall) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_arg(&mut self, v: &Arg) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_return_stmt(&mut self, v: &ReturnStmt) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_assignment(&mut self, v: &Assignment) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_key_value(&mut self, v: &KeyValueBuilder) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_break(&mut self, v: &Break) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_continue(&mut self, v: &Continue) -> VisitorResult<T> {
        v.accept(self)
    }
}

macro_rules! accept_default {
    ($name:ident) => {
        impl<T> Accept<T> for $name {
            fn accept(&self, _: &mut impl Visitor<T>) -> VisitorResult<T> {
                VisitorResult::Continue
            }
        }
    };
    ($name:ident, $($rest:tt)*) => {
        impl<T> Accept<T> for $name {
            fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
                accept_default!(@inner self, visitor, $($rest)*);
                VisitorResult::Continue
            }
        }
    };
    (@inner $self:expr, $visitor:expr, $field:ident:$handler:ident, $($rest:tt)*) => {
        accept_default!(@handler $handler, $self, $visitor, &$self.$field);
        accept_default!(@inner $self, $visitor, $($rest)*);
    };
    (@inner $self:expr, $visitor:expr, $field:ident:opt:$handler:ident, $($rest:tt)*) => {
        if let Some(f) = &$self.$field {
            accept_default!(@handler $handler, $self, $visitor, f)
        };
        accept_default!(@inner $self, $visitor, $($rest)*);
    };
    (@inner $self:expr, $visitor:expr, $field:ident:vec:$handler:ident, $($rest:tt)*) => {
        for e in &$self.$field {
            accept_default!(@handler $handler, $self, $visitor, e)
        }
        accept_default!(@inner $self, $visitor, $($rest)*);
    };
    (@handler node, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_node($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler var, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_var($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler arg, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_arg($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler param, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_param($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler kv, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_key_value($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler block, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_block($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@handler conditional_block, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_conditional_block($val) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
    };
    (@inner $self:expr, $visitor:expr, ) => {};
}
pub(crate) use accept_default;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Visitor;
    use crate::parser::parse;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_visitor() {
        #[derive(Debug, Default)]
        struct TestVisitor {
            data: Vec<String>,
        }
        impl TestVisitor {
            fn push(&mut self, s: impl Into<String>) {
                self.data.push(s.into());
            }
        }
        impl Visitor<()> for TestVisitor {
            fn visit_this(&mut self, v: &This) -> VisitorResult<()> {
                self.push("This");
                v.accept(self)
            }
            fn visit_var(&mut self, v: &Var) -> VisitorResult<()> {
                self.push(format!("Var {}", v.ident.key));
                v.accept(self)
            }
            fn visit_literal(&mut self, v: &Literal) -> VisitorResult<()> {
                self.push(format!("Literal {}", v.val));
                v.accept(self)
            }
            fn visit_interpolated_str(&mut self, v: &InterpolatedStr) -> VisitorResult<()> {
                self.push("InterpolatedStr");
                v.accept(self)
            }
            fn visit_list_builder(&mut self, v: &ListBuilder) -> VisitorResult<()> {
                self.push("ListBuilder");
                v.accept(self)
            }
            fn visit_dict_builder(&mut self, v: &DictBuilder) -> VisitorResult<()> {
                self.push("DictBuilder");
                v.accept(self)
            }
            fn visit_property_of(&mut self, v: &PropertyOf) -> VisitorResult<()> {
                self.push("PropertyOf");
                v.accept(self)
            }
            fn visit_index_of(&mut self, v: &IndexOf) -> VisitorResult<()> {
                self.push("IndexOf");
                v.accept(self)
            }
            fn visit_unary_op(&mut self, v: &UnaryOp) -> VisitorResult<()> {
                self.push("UnaryOp");
                v.accept(self)
            }
            fn visit_binary_op(&mut self, v: &BinaryOp) -> VisitorResult<()> {
                self.push("BinaryOp");
                v.accept(self)
            }
            fn visit_catch_missing_optional_property(
                &mut self,
                v: &CatchMissingOptionalProperty,
            ) -> VisitorResult<()> {
                self.push("ChainCatch");
                v.accept(self)
            }
            fn visit_block(&mut self, v: &Block) -> VisitorResult<()> {
                self.push("Block");
                v.accept(self)
            }
            fn visit_if_else(&mut self, v: &IfElse) -> VisitorResult<()> {
                self.push("IfElse");
                v.accept(self)
            }
            fn visit_conditional_block(&mut self, v: &ConditionalBlock) -> VisitorResult<()> {
                self.push("ConditionalBlock");
                v.accept(self)
            }
            fn visit_for(&mut self, v: &For) -> VisitorResult<()> {
                self.push("For");
                v.accept(self)
            }
            fn visit_declaration(&mut self, v: &Declaration) -> VisitorResult<()> {
                self.push("Declaration");
                v.accept(self)
            }
            fn visit_function_def(&mut self, v: &Rc<FunctionDef>) -> VisitorResult<()> {
                self.push("FunctionDef");
                v.accept(self)
            }
            fn visit_param(&mut self, v: &Param) -> VisitorResult<()> {
                self.push("Param");
                v.accept(self)
            }
            fn visit_function_call(&mut self, v: &FunctionCall) -> VisitorResult<()> {
                self.push("FunctionCall");
                v.accept(self)
            }
            fn visit_arg(&mut self, v: &Arg) -> VisitorResult<()> {
                self.push("Arg");
                v.accept(self)
            }
            fn visit_return_stmt(&mut self, v: &ReturnStmt) -> VisitorResult<()> {
                self.push("ReturnStmt");
                v.accept(self)
            }
            fn visit_assignment(&mut self, v: &Assignment) -> VisitorResult<()> {
                self.push("Assignment");
                v.accept(self)
            }
            fn visit_key_value(&mut self, v: &KeyValueBuilder) -> VisitorResult<()> {
                self.push("KeyValueBuilder");
                v.accept(self)
            }
            fn visit_break(&mut self, v: &Break) -> VisitorResult<()> {
                self.push("Break");
                v.accept(self)
            }
            fn visit_continue(&mut self, v: &Continue) -> VisitorResult<()> {
                self.push("Continue");
                v.accept(self)
            }
        }

        let mut test_visitor = TestVisitor::default();

        let p = parse(
            "dummy",
            r#"
                this
                end
                42
                "start{x}end"
                [1,2]
                dict(a:1,b:2)
                a.b[2]?
                !x
                y+z
                {
                    this
                    this
                }
                if 1 {
                    "a"
                } else if 2 {
                    "b"
                } else {
                    "c"
                }

                for a, b, c in d {
                    continue
                    break 45
                }

                var a = 11
                b = 22

                function f(a, b, c:1) {
                    return 22
                }
                foo.bar(1,2,c:3)
            "#,
        )
        .unwrap();

        for s in p.stmts {
            test_visitor.visit_node(&s);
        }

        assert_eq!(
            test_visitor.data,
            [
                "This",
                "Literal end",
                "Literal 42",
                "InterpolatedStr",
                "Literal \"start\"",
                "Var x",
                "Literal \"end\"",
                "ListBuilder",
                "Literal 1",
                "Literal 2",
                "DictBuilder",
                "KeyValueBuilder",
                "Literal 1",
                "KeyValueBuilder",
                "Literal 2",
                "ChainCatch",
                "IndexOf",
                "PropertyOf",
                "Var a",
                "Literal 2",
                "UnaryOp",
                "Var x",
                "BinaryOp",
                "Var y",
                "Var z",
                "Block",
                "This",
                "This",
                "IfElse",
                "ConditionalBlock",
                "Literal 1",
                "Block",
                "Literal \"a\"",
                "ConditionalBlock",
                "Literal 2",
                "Block",
                "Literal \"b\"",
                "Block",
                "Literal \"c\"",
                "For",
                "Var d",
                "Var a",
                "Var b",
                "Var c",
                "Block",
                "Continue",
                "Break",
                "Literal 45",
                "Declaration",
                "Var a",
                "Literal 11",
                "Assignment",
                "Var b",
                "Literal 22",
                "FunctionDef",
                "Var f",
                "Param",
                "Var a",
                "Param",
                "Var b",
                "Param",
                "Var c",
                "Block",
                "ReturnStmt",
                "Literal 22",
                "FunctionCall",
                "Var foo",
                "Arg",
                "Literal 1",
                "Arg",
                "Literal 2",
                "Arg",
                "Literal 3",
            ]
        );
    }
}
