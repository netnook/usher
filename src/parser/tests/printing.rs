use crate::lang::{
    Arg, Assignment, AstNode, BinaryOp, BinaryOpCode, Block, ChainCatch, ConditionalBlock,
    Declaration, DictBuilder, ForStmt, FunctionCall, FunctionDef, Identifier, IfElseStmt, IndexOf,
    InterpolatedStr, KeyValue, ListBuilder, Literal, Param, Program, ProgramError, PropertyOf,
    ReturnStmt, UnaryOp, UnaryOpCode, Value,
};
use std::io::{BufWriter, Write};

impl<'a> Program<'a> {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        for s in &self.stmts {
            s.print_write(&mut buf, 0);
        }

        let bytes = buf.into_inner().expect("ok");

        String::from_utf8(bytes).expect("utf-8 ok")
    }
}

impl AstNode {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        self.print_write(&mut buf, 0);

        let bytes = buf.into_inner().expect("ok");

        String::from_utf8(bytes).expect("utf-8 ok")
    }

    fn print_write(&self, w: &mut impl Write, indent: usize) {
        match self {
            AstNode::This => write_indented(w, indent, "this"),
            AstNode::Identifier(v) => v.print_write(w, indent),
            AstNode::Literal(v) => v.print_write(w, indent),
            AstNode::InterpolatedStr(v) => v.print_write(w, indent),
            AstNode::ListBuilder(v) => v.print_write(w, indent),
            AstNode::DictBuilder(v) => v.print_write(w, indent),
            AstNode::PropertyOf(v) => v.print_write(w, indent),
            AstNode::IndexOf(v) => v.print_write(w, indent),
            AstNode::UnaryOp(v) => v.print_write(w, indent),
            AstNode::BinaryOp(v) => v.print_write(w, indent),
            AstNode::ChainCatch(v) => v.print_write(w, indent),
            AstNode::Block(v) => v.print_write(w, indent),
            AstNode::IfElseStmt(v) => v.print_write(w, indent),
            AstNode::ForStmt(v) => v.print_write(w, indent),
            AstNode::FunctionDef(v) => v.print_write(w, indent),
            AstNode::FunctionCall(v) => v.print_write(w, indent),
            AstNode::ReturnStmt(v) => v.print_write(w, indent),
            AstNode::Declaration(v) => v.print_write(w, indent),
            AstNode::Assignment(v) => v.print_write(w, indent),
            AstNode::KeyValue(v) => v.print_write(w, indent),
            AstNode::Break => write_indented(w, indent, "break"),
            AstNode::Continue => write_indented(w, indent, "continue"),
            AstNode::End => write_indented(w, indent, "end"),
        }
    }
}

impl Identifier {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_indent(w, indent);
        w.write_all(b"(id ").unwrap();
        w.write_all(self.name.as_bytes()).unwrap();
        w.write_all(b")\n").unwrap();
    }
}

impl Declaration {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "decl");
        self.ident.print_write(w, indent + 1);
        self.value.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl Assignment {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "assign");
        self.lhs.print_write(w, indent + 1);
        self.rhs.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl KeyValue {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "kv");
        self.key.print_write(w, indent + 1);
        self.value.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl IfElseStmt {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "if");
        for cb in &self.conditional_blocks {
            cb.print_write(w, indent + 1);
        }
        if let Some(else_block) = &self.else_block {
            else_block.print_write(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl ConditionalBlock {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "conditional");
        self.condition.print_write(w, indent + 1);
        self.block.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl ForStmt {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "for");
        self.loop_var_1.print_write(w, indent + 1);
        match &self.loop_var_2 {
            Some(v) => v.print_write(w, indent + 1),
            None => {
                write_indent(w, indent + 1);
                w.write_all(b"-\n").unwrap();
            }
        }
        self.loop_expr.print_write(w, indent + 1);
        self.block.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl FunctionDef {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "fn");
        if let Some(name) = &self.name {
            write_indented(w, indent + 1, &format!("(name {})", name.name));
        }
        if !self.params.is_empty() {
            write_open(w, indent + 1, "params");
            for p in &self.params {
                p.print_write(w, indent + 2);
            }
            write_close(w, indent + 1);
        }
        self.body.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl Param {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        match &self.value {
            Some(value) => {
                write_open(w, indent, &self.name.name);
                value.print_write(w, indent + 1);
                write_close(w, indent);
            }
            None => write_indented(w, indent, &self.name.name),
        }
    }
}

impl FunctionCall {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "call");
        write_open(w, indent + 1, "on");
        self.on.print_write(w, indent + 2);
        write_close(w, indent + 1);

        if let Some(method) = &self.method {
            write_open(w, indent + 1, "method");
            method.print_write(w, indent + 2);
            write_close(w, indent + 1);
        }

        if !self.args.is_empty() {
            write_open(w, indent + 1, "args");
            for a in &self.args {
                a.print_write(w, indent + 2);
            }
            write_close(w, indent + 1);
        }

        write_close(w, indent);
    }
}

impl Arg {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        match &self.name {
            Some(name) => {
                write_open(w, indent, &name.name);
                self.value.print_write(w, indent + 1);
                write_close(w, indent);
            }
            None => self.value.print_write(w, indent),
        }
    }
}
impl ReturnStmt {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        if let Some(value) = &self.value {
            write_open(w, indent, "return");
            value.print_write(w, indent + 1);
            write_close(w, indent);
        } else {
            write_indented(w, indent, "return");
        }
    }
}

impl Block {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "block");
        for cb in &self.stmts {
            cb.print_write(w, indent + 1);
        }
        write_close(w, indent);
    }
}
impl ChainCatch {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "?");
        self.inner.print_write(w, indent + 1);
        write_close(w, indent);
    }
}
impl UnaryOp {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        let s = match self.op {
            UnaryOpCode::Not => "not",
            UnaryOpCode::Negative => "neg",
        };
        write_open(w, indent, s);
        self.on.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl BinaryOp {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        let s = match self.op {
            BinaryOpCode::Add => "+",
            BinaryOpCode::Sub => "-",
            BinaryOpCode::Mul => "*",
            BinaryOpCode::Div => "/",
            BinaryOpCode::Mod => "%",
            BinaryOpCode::Equal => "==",
            BinaryOpCode::NotEqual => "!=",
            BinaryOpCode::Greater => ">",
            BinaryOpCode::GreaterOrEqual => ">=",
            BinaryOpCode::LessOrEqual => "<=",
            BinaryOpCode::Less => "<",
            BinaryOpCode::And => "&&",
            BinaryOpCode::Or => "||",
        };
        write_open(w, indent, s);
        self.lhs.print_write(w, indent + 1);
        self.rhs.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl PropertyOf {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "property");
        self.of.print_write(w, indent + 1);
        self.property.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl IndexOf {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "index");
        self.of.print_write(w, indent + 1);
        self.index.print_write(w, indent + 1);
        write_close(w, indent);
    }
}

impl ListBuilder {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "list");
        for e in &self.entries {
            e.print_write(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl DictBuilder {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "dict");
        for kv in &self.entries {
            kv.print_write(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl InterpolatedStr {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "interpolate");
        for cb in &self.parts {
            cb.print_write(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl Literal {
    fn print_write(&self, w: &mut impl Write, indent: usize) {
        self.val.print_write(w, indent);
    }
}

impl Value {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        self.print_write(&mut buf, 0);

        let bytes = buf.into_inner().expect("ok");

        String::from_utf8(bytes).expect("utf-8 ok")
    }

    fn print_write(&self, w: &mut impl Write, indent: usize) {
        write_indent(w, indent);
        match self {
            Value::Func(_) => panic!("func value should not occur in testing"),
            Value::BuiltInFunc(_) => panic!("funcbuiltin value should not occur in testing"),
            Value::Str(s) => {
                w.write_all(b"\"").unwrap();
                w.write_all(s.as_bytes()).unwrap();
                w.write_all(b"\"").unwrap();
            }
            Value::Integer(v) => {
                w.write_all(format!("{v}").as_bytes()).unwrap();
            }
            Value::Float(v) => {
                w.write_all(format!("{v}").as_bytes()).unwrap();
                w.write_all(b"f").unwrap();
            }
            Value::Bool(v) => {
                w.write_all(format!("{v}").as_bytes()).unwrap();
            }
            Value::List(v) => {
                w.write_all("list[\n".as_bytes()).unwrap();
                for v in &v.borrow().content {
                    v.print_write(w, indent + 1);
                }
                write_indented(w, indent, "]");
            }
            Value::Dict(v) => {
                w.write_all("dict(".as_bytes()).unwrap();
                for (k, v) in &v.borrow().content {
                    write_indented(w, indent + 1, &format!("{k}:"));
                    v.print_write(w, indent + 1);
                }
                write_indented(w, indent, ")");
            }
            Value::Nil => {
                w.write_all("nil".as_bytes()).unwrap();
            }
        }
        w.write_all(b"\n").unwrap();
    }
}

impl ProgramError {
    pub(crate) fn print(&self) -> String {
        format!("{self:#?}")
    }
}

fn write_indent(w: &mut impl Write, indent: usize) {
    for _ in 0..indent {
        w.write_all(b"  ").unwrap();
    }
}

fn write_indented(w: &mut impl Write, indent: usize, text: &str) {
    write_indent(w, indent);
    w.write_all(text.as_bytes()).unwrap();
    w.write_all(b"\n").unwrap();
}

fn write_open(w: &mut impl Write, indent: usize, name: &str) {
    write_indent(w, indent);
    w.write_all(b"(").unwrap();
    w.write_all(name.as_bytes()).unwrap();
    w.write_all(b"\n").unwrap();
}

fn write_close(w: &mut impl Write, indent: usize) {
    write_indent(w, indent);
    w.write_all(b")").unwrap();
    w.write_all(b"\n").unwrap();
}
