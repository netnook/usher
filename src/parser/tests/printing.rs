use crate::lang::{
    Assignment, AstNode, BinaryOp, BinaryOpCode, Block, ChainCatch, ConditionalBlock, Declaration,
    ForStmt, Identifier, IfElseStmt, IndexOf, InterpolatedStr, ListBuilder, ObjectBuilder, Program,
    PropertyOf, UnaryOp, UnaryOpCode, Value,
};
use std::io::{BufWriter, Write};

impl Program {
    pub(crate) fn print(&self) -> String {
        let mut buf = BufWriter::new(Vec::new());

        for s in &self.stmts {
            s.print(&mut buf, 0);
        }

        let bytes = buf.into_inner().expect("ok");
        let printed = String::from_utf8(bytes).expect("utf-8 ok");

        printed
    }
}

impl AstNode {
    fn print(&self, w: &mut impl Write, indent: usize) {
        match self {
            AstNode::This => write_indented(w, indent, "this"),
            AstNode::Identifier(v) => v.print(w, indent),
            AstNode::Value(v) => v.print(w, indent),
            AstNode::InterpolatedStr(v) => v.print(w, indent),
            AstNode::ListBuilder(v) => v.print(w, indent),
            AstNode::ObjectBuilder(v) => v.print(w, indent),
            AstNode::PropertyOf(v) => v.print(w, indent),
            AstNode::IndexOf(v) => v.print(w, indent),
            AstNode::UnaryOp(v) => v.print(w, indent),
            AstNode::BinaryOp(v) => v.print(w, indent),
            AstNode::ChainCatch(v) => v.print(w, indent),
            AstNode::IfElseStmt(v) => v.print(w, indent),
            AstNode::ForStmt(v) => v.print(w, indent),
            AstNode::Declaration(v) => v.print(w, indent),
            AstNode::Assignment(v) => v.print(w, indent),
            AstNode::Break => write_indented(w, indent, "break"),
            AstNode::Continue => write_indented(w, indent, "continue"),
        }
    }
}

impl Identifier {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_indent(w, indent);
        w.write(b"(id ").unwrap();
        w.write(self.name.as_bytes()).unwrap();
        w.write(b")\n").unwrap();
    }
}

impl Declaration {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "decl");
        self.ident.print(w, indent + 1);
        self.value.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl Assignment {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "assign");
        self.lhs.print(w, indent + 1);
        self.rhs.print(w, indent + 1);
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
        self.condition.print(w, indent + 1);
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
                w.write(b"-\n").unwrap();
            }
        }
        self.loop_expr.print(w, indent + 1);
        self.block.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl Block {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "block");
        for cb in &self.stmts {
            cb.print(w, indent + 1);
        }
        write_close(w, indent);
    }
}
impl ChainCatch {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "?");
        self.inner.print(w, indent + 1);
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
        self.on.print(w, indent + 1);
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
        self.lhs.print(w, indent + 1);
        self.rhs.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl PropertyOf {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "property");
        self.from.print(w, indent + 1);
        self.property.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl IndexOf {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "index");
        self.from.print(w, indent + 1);
        self.index.print(w, indent + 1);
        write_close(w, indent);
    }
}

impl ListBuilder {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "list");
        for e in &self.entries {
            e.print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl ObjectBuilder {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "object");
        for (k, v) in &self.entries {
            write_open(w, indent + 1, "kv");
            k.print(w, indent + 2);
            v.print(w, indent + 2);
            write_close(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl InterpolatedStr {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_open(w, indent, "interpolate");
        for cb in &self.parts {
            cb.print(w, indent + 1);
        }
        write_close(w, indent);
    }
}

impl Value {
    fn print(&self, w: &mut impl Write, indent: usize) {
        write_indent(w, indent);
        match self {
            Value::Str(s) => {
                w.write(b"\"").unwrap();
                w.write(s.as_bytes()).unwrap();
                w.write(b"\"").unwrap();
            }
            Value::Integer(v) => {
                w.write(format!("{v}").as_bytes()).unwrap();
            }
            Value::Float(v) => {
                w.write(format!("{v}").as_bytes()).unwrap();
                w.write(b"f").unwrap();
            }
            Value::Bool(v) => {
                w.write(format!("{v}").as_bytes()).unwrap();
            }
            Value::Nil => {
                w.write("nil".as_bytes()).unwrap();
            }
        }
        w.write(b"\n").unwrap();
    }
}

fn write_indent(w: &mut impl Write, indent: usize) {
    for _ in 0..indent {
        w.write(b"  ").unwrap();
    }
}

fn write_indented(w: &mut impl Write, indent: usize, name: &str) {
    write_indent(w, indent);
    w.write(name.as_bytes()).unwrap();
    w.write(b"\n").unwrap();
}

fn write_open(w: &mut impl Write, indent: usize, name: &str) {
    write_indent(w, indent);
    w.write(b"(").unwrap();
    w.write(name.as_bytes()).unwrap();
    w.write(b"\n").unwrap();
}

fn write_close(w: &mut impl Write, indent: usize) {
    write_indent(w, indent);
    w.write(b")").unwrap();
    w.write(b"\n").unwrap();
}
