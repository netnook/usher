use crate::lang::{
    Arg, Assignment, AstNode, BinaryOp, BinaryOpCode, Block, ChainCatch, ConditionalBlock,
    Declaration, DictBuilder, ForStmt, FunctionCall, FunctionDef, Identifier, IfElseStmt, IndexOf,
    InterpolatedStr, KeyValue, ListBuilder, Literal, Param, Program, PropertyOf, ReturnStmt,
    UnaryOp, UnaryOpCode, Value,
};
use std::io::{BufWriter, Write};

impl<'a> Program<'a> {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        for s in &self.stmts {
            s.do_print(&mut buf, 0);
        }

        let bytes = buf.into_inner().expect("ok");

        String::from_utf8(bytes).expect("utf-8 ok")
    }
}

impl AstNode {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        self.do_print(&mut buf, 0);

        let bytes = buf.into_inner().expect("ok");

        String::from_utf8(bytes).expect("utf-8 ok")
    }

    fn do_print(&self, w: &mut impl Write, indent: usize) {
        match self {
            AstNode::This => write_indented(w, indent, "this"),
            AstNode::Identifier(v) => v.print(w, indent),
            AstNode::Literal(v) => v.print(w, indent),
            AstNode::InterpolatedStr(v) => v.print(w, indent),
            AstNode::ListBuilder(v) => v.print(w, indent),
            AstNode::DictBuilder(v) => v.print(w, indent),
            AstNode::PropertyOf(v) => v.print(w, indent),
            AstNode::IndexOf(v) => v.print(w, indent),
            AstNode::UnaryOp(v) => v.print(w, indent),
            AstNode::BinaryOp(v) => v.print(w, indent),
            AstNode::ChainCatch(v) => v.print(w, indent),
            AstNode::Block(v) => v.print(w, indent),
            AstNode::IfElseStmt(v) => v.print(w, indent),
            AstNode::ForStmt(v) => v.print(w, indent),
            AstNode::FunctionDef(v) => v.print(w, indent),
            AstNode::FunctionCall(v) => v.print(w, indent),
            AstNode::ReturnStmt(v) => v.print(w, indent),
            AstNode::Declaration(v) => v.print(w, indent),
            AstNode::Assignment(v) => v.print(w, indent),
            AstNode::KeyValue(v) => v.print(w, indent),
            AstNode::Break => write_indented(w, indent, "break"),
            AstNode::Continue => write_indented(w, indent, "continue"),
            AstNode::End => write_indented(w, indent, "end"),
        }
    }
}

impl Identifier {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_indent(w, indent);
        w.write_all(b"(id ").unwrap();
        w.write_all(self.name.as_bytes()).unwrap();
        w.write_all(b")\n").unwrap();
    }
}

impl Declaration {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "decl");
        self.ident.print(w, indent + 1);
        self.value.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl Assignment {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "assign");
        self.lhs.do_print(w, indent + 1);
        self.rhs.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl KeyValue {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "kv");
        self.key.print(w, indent + 1);
        self.value.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl IfElseStmt {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "if");
        for cb in &self.conditional_blocks {
            cb.print(w, indent + 1);
        }
        if let Some(else_block) = &self.else_block {
            else_block.print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl ConditionalBlock {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "conditional");
        self.condition.do_print(w, indent + 1);
        self.block.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl ForStmt {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "for");
        self.loop_var_1.print(w, indent + 1);
        match &self.loop_var_2 {
            Some(v) => v.print(w, indent + 1),
            None => {
                write_indent(w, indent + 1);
                w.write_all(b"-\n").unwrap();
            }
        }
        self.loop_expr.do_print(w, indent + 1);
        self.block.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl FunctionDef {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "fn");
        if let Some(name) = &self.name {
            write_indented(w, indent + 1, &format!("(name {})", name.name));
        }
        if !self.params.is_empty() {
            write_open(w, indent + 1, "params");
            for p in &self.params {
                p.print(w, indent + 2);
            }
            write_close(w, indent + 1);
        }
        self.body.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl Param {
    fn print(&self, w: &mut impl Write, indent: usize) {
        match &self.value {
            Some(value) => {
                write_open(w, indent, &self.name.name);
                value.do_print(w, indent + 1);
                write_close(w, indent);
            }
            None => write_indented(w, indent, &self.name.name),
        }
    }
}

impl FunctionCall {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "call");
        write_open(w, indent + 1, "on");
        self.on.do_print(w, indent + 2);
        write_close(w, indent + 1);

        if !self.args.is_empty() {
            write_open(w, indent + 1, "args");
            for a in &self.args {
                a.print(w, indent + 2);
            }
            write_close(w, indent + 1);
        }

        write_close(w, indent);
    }
}

impl Arg {
    fn print(&self, w: &mut impl Write, indent: usize) {
        match &self.name {
            Some(name) => {
                write_open(w, indent, &name.name);
                self.value.do_print(w, indent + 1);
                write_close(w, indent);
            }
            None => self.value.do_print(w, indent),
        }
    }
}
impl ReturnStmt {
    fn print(&self, w: &mut impl Write, indent: usize) {
        if let Some(value) = &self.value {
            write_open(w, indent, "return");
            value.do_print(w, indent + 1);
            write_close(w, indent);
        } else {
            write_indented(w, indent, "return");
        }
    }
}

impl Block {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "block");
        for cb in &self.stmts {
            cb.do_print(w, indent + 1);
        }
        write_close(w, indent);
    }
}
impl ChainCatch {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "?");
        self.inner.do_print(w, indent + 1);
        write_close(w, indent);
    }
}
impl UnaryOp {
    fn print(&self, w: &mut impl Write, indent: usize) {
        let s = match self.op {
            UnaryOpCode::Not => "not",
            UnaryOpCode::Negative => "neg",
        };
        write_open(w, indent, s);
        self.on.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl BinaryOp {
    fn print(&self, w: &mut impl Write, indent: usize) {
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
        self.lhs.do_print(w, indent + 1);
        self.rhs.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl PropertyOf {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "property");
        self.from.do_print(w, indent + 1);
        self.property.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl IndexOf {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "index");
        self.from.do_print(w, indent + 1);
        self.index.do_print(w, indent + 1);
        write_close(w, indent);
    }
}

impl ListBuilder {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "list");
        for e in &self.entries {
            e.do_print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl DictBuilder {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "dict");
        for kv in &self.entries {
            kv.print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl InterpolatedStr {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "interpolate");
        for cb in &self.parts {
            cb.do_print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl Literal {
    fn print(&self, w: &mut impl Write, indent: usize) {
        self.val.print(w, indent);
    }
}

impl Value {
    fn print(&self, w: &mut impl Write, indent: usize) {
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
            Value::Dict(v) => {
                w.write_all("dict(".as_bytes()).unwrap();
                for (k, v) in &v.content {
                    write_indented(w, indent + 1, &format!("{k}:"));
                    v.print(w, indent + 1);
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
