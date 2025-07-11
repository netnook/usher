use super::{ParseResult, Parser};
use crate::lang::AstNode;

impl<'a> Parser<'a> {
    /// Consume an expression or nothing.
    pub(super) fn expression(&mut self) -> ParseResult<Option<AstNode>> {
        // FIXME: this is a dummy implementation to enable other parsers

        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::List(v)));
        }
        if let Some(v) = self.string()? {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.float() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.integer() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.boolean() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.nil() {
            return Ok(Some(AstNode::Value(v)));
        }

        Ok(None)
    }
}
