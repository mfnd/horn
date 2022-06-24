use std::{rc::Rc, fmt, ops::Index, cell::RefCell};

use crate::builtins::BuiltIn;

use super::Value;

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
    Call(Rc<Rule>),
    NativeCall(Rc<NativePredicate>),
    NamedCall(usize, u32),
    PushConstant(Value),
    PushVariable(u32),
    CreateStructure(usize, u32),
    CreateList { head_count: u32, with_tail: bool},
    Pop(u32)
}


#[derive(Clone, Copy)]
pub enum Operator {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3
}

impl From<usize> for Operator {
    fn from(op: usize) -> Self {
        match op {
            0 => Operator::Add,
            1 => Operator::Sub,
            2 => Operator::Mul,
            3 => Operator::Div,
            _ => panic!("Invalid operator")
        }
    }
}

pub const OPERATOR_ATOMS : &[(Operator, &str)] = &[
    (Operator::Add, "+"),
    (Operator::Sub, "-"),
    (Operator::Mul, "*"),
    (Operator::Div, "/")
];


#[derive(Clone)]
pub struct CodeBlock {
    pub code: Rc<Vec<Instruction>>
}

impl CodeBlock {

    pub fn from(code: Vec<Instruction>) -> Self {
        CodeBlock { 
            code: Rc::from(code)
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