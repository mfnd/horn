use std::{rc::Rc, fmt, ops::Index, cell::RefCell};

use crate::{prelude::BuiltIn, ir_gen::{PredicateDef, QueryDef}};

use super::{Value, RuntimeError};


#[derive(Debug, Clone, Copy)]
pub enum ArithComparisonOp {
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug)]
pub enum Instruction {
    Allocate(u32),
    Return,
    LoadRegister { register: u32, variable: u32 },
    StoreRegister{ register: u32, variable: u32 },
    StoreRegisterConstant{ register: u32, constant: Value },
    ConsListRegister {register: u32, value: Value},
    UnifyRegisterConstant{ register: u32, constant: Value },
    PopUnifyRegister { register: u32 },
    UnifyVariables { variable1: u32, variable2: u32 },
    UnifyVariableConstant { variable: u32, constant: Value },
    UnifyVariableRegister { variable: u32, register: u32},
    CompareArithRegisters { op: ArithComparisonOp, register1: u32, register2: u32 },
    Call(Rc<Rule>),
    NativeCall(Rc<NativePredicate>),
    NamedCall(usize, u32),
    PushConstant(Value),
    PushVariable(u32),
    CreateStructure(usize, u32),
    CreateList { head_count: u32, with_tail: bool},
    Pop(u32),
    Cut
}


pub struct PreludeAtoms {}

impl PreludeAtoms {
    pub const Add: usize = 0;
    pub const Sub: usize = 1;
    pub const Mul: usize = 2;
    pub const Div: usize = 3;
}

pub const PRELUDE_ATOMS : &[(usize, &str)] = &[
    (PreludeAtoms::Add, "+"),
    (PreludeAtoms::Sub, "-"),
    (PreludeAtoms::Mul, "*"),
    (PreludeAtoms::Div, "/")
];


#[derive(Clone)]
pub struct CodeBlock {
    pub head: Value,
    pub body: Vec<Value>,
    pub code: Rc<Vec<Instruction>>
}

impl From<PredicateDef> for CodeBlock {
    fn from(pred: PredicateDef) -> Self {
        CodeBlock { 
            head: pred.head, 
            body: pred.body, 
            code: Rc::from(pred.code)
        }
    }
}

impl From<QueryDef> for CodeBlock {
    fn from(query: QueryDef) -> Self {
        CodeBlock { 
            head: Value::Nil, 
            body: Vec::new(), 
            code: Rc::from(query.code)
        }
    }
}

impl Index<usize> for CodeBlock {
    type Output = Instruction;

    fn index(&self, index: usize) -> &Self::Output {
        &self.code[index]
    }
}

pub struct Rule {
    pub functor: usize,
    pub arity: u32,
    pub code: RefCell<Vec<CodeBlock>>
}

impl fmt::Debug for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rule")
        .field("functor", &self.functor)
        .field("arity", &self.arity)
        .finish()
    }
}

pub struct NativePredicate {
    pub functor: String,
    pub arity: u32,
    pub function_ptr: BuiltIn
}

impl fmt::Debug for NativePredicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativePredicate")
        .field("functor", &self.functor)
        .field("arity", &self.arity)
        .finish()
    }
}


#[derive(Debug)]
pub struct RuleInfo {
    functor: String,
    arity: u32
}

impl RuleInfo {
    pub fn boxed(functor: &str, arity: u32) -> Box<Self> {
        Box::from(RuleInfo { functor: functor.to_string(), arity: arity })
    }
}
