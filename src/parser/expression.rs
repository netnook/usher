use super::{ParseResult, Parser};
use crate::lang::AstNode;

impl<'a> Parser<'a> {
    /// Consume an expression or nothing.
    pub(super) fn expression(&mut self) -> ParseResult<Option<AstNode>> {
        // FIXME: this is a dummy implementation to enable other parsers

        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::ListBuilder(v)));
        }
        if let Some(v) = self.object()? {
            return Ok(Some(AstNode::ObjectBuilder(v)));
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
        if let Some(v) = self.identifier()? {
            return Ok(Some(AstNode::Identifier(v)));
        }

        Ok(None)
    }

    // FIXME: do we need this later
    pub(super) fn object_or_list(&mut self) -> ParseResult<Option<AstNode>> {
        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::ListBuilder(v)));
        }
        if let Some(v) = self.object()? {
            return Ok(Some(AstNode::ObjectBuilder(v)));
        }
        Ok(None)
    }
}
