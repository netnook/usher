use crate::lang::{
    Arg, Assignment, AstNode, BinaryOp, Block, Break, ChainCatch, ConditionalBlock, Continue,
    Declaration, DictBuilder, End, For, FunctionCall, FunctionDef, Identifier, IfElse, IndexOf,
    InterpolatedStr, KeyValueBuilder, ListBuilder, Literal, Param, PropertyOf, ReturnStmt, This,
    UnaryOp,
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
        match n {
            AstNode::This(v) => self.visit_this(v),
            AstNode::Identifier(v) => self.visit_identifier(v),
            AstNode::Literal(v) => self.visit_literal(v),
            AstNode::InterpolatedStr(v) => self.visit_interpolated_str(v),
            AstNode::ListBuilder(v) => self.visit_list_builder(v),
            AstNode::DictBuilder(v) => self.visit_dict_builder(v),
            AstNode::PropertyOf(v) => self.visit_property_of(v),
            AstNode::IndexOf(v) => self.visit_index_of(v),
            AstNode::UnaryOp(v) => self.visit_unary_op(v),
            AstNode::BinaryOp(v) => self.visit_binary_op(v),
            AstNode::ChainCatch(v) => self.visit_chain_catch(v),
            AstNode::Block(v) => self.visit_block(v),
            AstNode::IfElse(v) => self.visit_if_else(v),
            AstNode::For(v) => self.visit_for(v),
            AstNode::Declaration(v) => self.visit_declaration(v),
            AstNode::FunctionDef(v) => self.visit_function_def(v),
            AstNode::FunctionCall(v) => self.visit_function_call(v),
            AstNode::ReturnStmt(v) => self.visit_return_stmt(v),
            AstNode::Assignment(v) => self.visit_assignment(v),
            AstNode::KeyValue(v) => self.visit_key_value(v),
            AstNode::Break(v) => self.visit_break(v),
            AstNode::Continue(v) => self.visit_continue(v),
            AstNode::End(v) => self.visit_end(v),
        }
    }

    fn visit_this(&mut self, v: &This) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_identifier(&mut self, v: &Identifier) -> VisitorResult<T> {
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
    fn visit_chain_catch(&mut self, v: &ChainCatch) -> VisitorResult<T> {
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
    fn visit_end(&mut self, v: &End) -> VisitorResult<T> {
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
    (@handler identifier, $self:expr, $visitor:expr,  $val:expr) => {
        match $visitor.visit_identifier($val) {
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
