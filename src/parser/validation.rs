use crate::{
    lang::{Accept, Break, Continue, For, FunctionDef, Program, Visitor, VisitorResult},
    parser::error::SemanticError,
};
use std::{collections::VecDeque, rc::Rc};

#[derive(Default)]
pub(crate) struct Validator {
    errors: VecDeque<SemanticError>,
    stack: Vec<ValidationContext>,
    context: ValidationContext,
}

#[derive(Default)]
struct ValidationContext {
    for_loop_depth: usize,
}

type ValidationResult = Result<(), SemanticError>;

impl Validator {
    pub(crate) fn validate(prog: &Program) -> ValidationResult {
        let mut validator = Validator::default();

        for s in &prog.stmts {
            match validator.visit_node(s) {
                VisitorResult::Stop(_) => break,
                VisitorResult::Continue => {}
            }
        }

        validator.check_error()?;

        Ok(())
    }

    fn check_error(&mut self) -> ValidationResult {
        if let Some(e) = self.errors.pop_front() {
            return Err(e);
        };
        Ok(())
    }
}

impl Visitor<()> for Validator {
    fn visit_function_def(&mut self, v: &Rc<FunctionDef>) -> VisitorResult<()> {
        self.stack.push(std::mem::take(&mut self.context));
        let res = v.accept(self);
        self.context = self
            .stack
            .pop()
            .expect("stack to have at least one element");
        res
    }

    fn visit_for(&mut self, v: &For) -> VisitorResult<()> {
        self.context.for_loop_depth += 1;
        let res = v.accept(self);
        self.context.for_loop_depth -= 1;
        res
    }

    fn visit_break(&mut self, v: &Break) -> VisitorResult<()> {
        if self.context.for_loop_depth == 0 {
            self.errors
                .push_back(SemanticError::BreakWithoutFor { span: v.span });
        }
        v.accept(self)
    }

    fn visit_continue(&mut self, v: &Continue) -> VisitorResult<()> {
        if self.context.for_loop_depth == 0 {
            self.errors
                .push_back(SemanticError::ContinueWithoutFor { span: v.span });
        }
        v.accept(self)
    }
}
