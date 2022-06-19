use std::cell::RefCell;
use std::{fmt, mem, vec};
use std::ops::Index;
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::builtins::{BuiltIn, BUILTINS};
use crate::debugln;
use crate::ir_gen::{QueryDef, IRGen, Module};
use crate::parser::CFGNode;


#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    All,
    Atom(usize),
    Int(i64),
    Str(Rc<str>),
    Struct(Rc<Struct>),
    Ref(ValueCell),
    List(List)
}

#[derive(Clone, Debug)]
pub enum List {
    Nil,
    Cons(Rc<Node>),
    Ref(ValueCell)
}

impl List {

    fn cons(head: Value, tail: List) -> Self {
        List::Cons(
            Rc::from(
                Node {
                    head,
                    tail
                }
            )
        )
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    head: Value,
    tail: List
}

#[derive(Debug)]
pub struct Struct {
    functor: usize,
    terms: Vec<Value>
}

#[derive(Clone)]
pub struct ValueCell {
    value_ref: Rc<RefCell<Value>>
}

impl ValueCell {

    pub fn get_value(&self) -> Value {
        self.value_ref.as_ref().borrow().clone()
    }

    pub fn get_value_deref(&self) -> Value {
        let value = self.value_ref.as_ref().borrow().clone();
        match value {
            Value::Ref(value_cell) => value_cell.get_value_deref(),
            _ => value
        }
    }

    pub fn put_ref(&self, other: ValueCell) {
        self.value_ref.as_ref().replace(Value::Ref(other));
    }

    pub fn put(&self, other: Value) {
        self.value_ref.as_ref().replace(other);
    }

    pub fn new() -> Self {
        ValueCell {
            value_ref: Rc::from(RefCell::from(Value::Nil))
        }
    }

}

impl fmt::Debug for ValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueCell")
         .field("value", &self.get_value())
         .finish()
    }
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
    Call(Rc<Rule>),
    NativeCall(Rc<NativePredicate>),
    NamedCall(usize, u32),
    PushConstant(Value),
    PushVariable(u32),
    CreateStructure(usize, u32),
    CreateList { head_count: u32, with_tail: bool},
    Pop(u32)
}

struct CallFrame {
    stack: Vec<ValueCell>,
    code: CodeBlock,
    prev_pc: usize,
    prev_frame: usize
}

impl CallFrame {
    fn create(code: CodeBlock, stack_size: usize, prev_frame: usize, prev_pc: usize) -> Self {
        CallFrame {
            stack: (0..stack_size).map(|_| ValueCell::new()).collect(),
            code,
            prev_pc,
            prev_frame
        }
    }
}

struct ChoiceFrame {
    args: Vec<Value>,
    rule: Rc<Rule>,
    choice_idx: usize,
    trail_top: usize,
    call_top: usize,
    prev_pc: usize,
}

impl ChoiceFrame {
    fn create(rule: Rc<Rule>, args: Vec<Value>, trail_top: usize, call_top: usize, prev_pc: usize) -> Self {
        ChoiceFrame {
            args,
            rule,
            choice_idx: 0,
            trail_top,
            call_top,
            prev_pc
        }
    }
}


#[derive(Clone)]
pub struct CodeBlock {
    code: Rc<Vec<Instruction>>
}

impl CodeBlock {

    fn from(code: Vec<Instruction>) -> Self {
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
    functor: usize,
    arity: u32,
    code: RefCell<Vec<CodeBlock>>
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
    functor: String,
    arity: u32,
    function_ptr: BuiltIn
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

#[derive(Clone, Copy)]
enum Operator {
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

const OPERATOR_ATOMS : &[(Operator, &str)] = &[
    (Operator::Add, "+"),
    (Operator::Sub, "-"),
    (Operator::Mul, "*"),
    (Operator::Div, "/")
];


pub enum QueryError {
    NoResult,
    QueryNotSet
}

pub type QueryResult = Result<Vec<Value>, QueryError>;

pub struct PrologVM {
    rules: HashMap<(usize, u32), Rc<Rule>>,
    native_predicates: HashMap<(usize, u32), Rc<NativePredicate>>,
    atoms: HashMap<String, usize>,
    registers: Vec<Value>,
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    choice_stack: Vec<ChoiceFrame>,
    trail: Vec<ValueCell>,
    curr_frame: usize,
    freeze_idx: usize,
    code: Option<CodeBlock>,
    pc: usize,
    prev_pc: usize,
}

impl PrologVM {

    pub fn new() -> Self {
        let mut atoms = HashMap::new();
        for (op, ident) in OPERATOR_ATOMS {
            atoms.insert(String::from(*ident), *op as usize);
        }

        let mut native_predicates = HashMap::new();
        BUILTINS.iter().for_each(|(name, builtin)| {
            let idx = atoms.len();
            atoms.insert(String::from(name.0), idx);
            native_predicates.insert((idx, name.1), Rc::from(
                NativePredicate {
                    functor: String::from(name.0),
                    arity: name.1,               
                    function_ptr: *builtin
                }
            ));
        });

        PrologVM { 
            rules: HashMap::new(),
            native_predicates: native_predicates,
            atoms: atoms,
            registers: vec![Value::Nil; 16],
            value_stack: Vec::new(),
            call_stack: Vec::new(),
            choice_stack: Vec::new(),
            trail: Vec::new(),
            curr_frame: 0,
            freeze_idx: 0,
            code: None,
            pc: 0,
            prev_pc: 0,
        }
    }

    pub fn unify(&mut self, val1: Value, val2: Value) -> bool {
        match (val1, val2) {
            (Value::All, _) | (_, Value::All) => true,
            (Value::Atom(id1), Value::Atom(id2)) => id1 == id2,
            (Value::Int(v1), Value::Int(v2)) => v1 == v2,
            (Value::Str(v1), Value::Str(v2)) => v1 == v2,
            (Value::Struct(struct1), Value::Struct(struct2)) => {
                if Rc::ptr_eq(&struct1, &struct2) {
                    true
                } else if (*struct1).functor == (*struct2).functor && (*struct1).terms.len() == (*struct2).terms.len() {
                    let mut res = true;
                    for (term1, term2) in (*struct1).terms.iter().zip((*struct2).terms.iter()) {
                        if !self.unify(term1.clone(), term2.clone()) {
                            res = false;
                            break;
                        }
                    }
                    res
                } else {
                    false
                }
            }
            (Value::List(list1), Value::List(list2)) => {
                self.unify_lists(&list1, &list2)
            }
            (Value::Ref(loc1), Value::Ref(loc2)) => {
                let v1 = loc1.get_value();
                let v2 = loc2.get_value();
                match (&v1, &v2) {
                    (Value::Nil, _) => {
                        self.trail.push(loc1.clone());
                        loc1.put_ref(loc2.clone());
                        true
                    },
                    (_, Value::Nil) => {
                        self.trail.push(loc2.clone());
                        loc2.put_ref(loc1.clone());
                        true
                    }
                    _ => {
                        self.unify(v1, v2)
                    }
                }
            },
            (Value::Ref(loc), other) | (other, Value::Ref(loc)) => {
                let ref_val = loc.get_value();
                
                match ref_val {
                    Value::Nil => {
                        self.trail.push(loc.clone());
                        loc.put(other);
                        true
                    }
                    _ => self.unify(ref_val, other)
                }
            }
            _ => false
        }
    }

    pub fn unify_lists(&mut self, first: &List, second: &List) -> bool {
        let mut list1 = first;
        let mut list2 = second;

        
        
        loop {
            match (list1, list2) {
                (List::Nil, List::Nil) => break,
                (List::Nil, List::Cons(_)) => return false,
                (List::Cons(_), List::Nil) => return false,
                (List::Cons(node1), List::Cons(node2)) => {
                    if Rc::ptr_eq(&node1, &node2) {
                        return true;
                    }
                    if !self.unify(node1.head.clone(), node2.head.clone()) {
                        return false;
                    }
                    list1 = &node1.tail;
                    list2 = &node2.tail;
                }
                (List::Nil, List::Ref(loc)) | (List::Ref(loc), List::Nil)  => {
                    self.trail.push(loc.clone());
                    loc.put(Value::List(List::Nil));
                    return true;
                },
                (cons_list @ List::Cons(node), List::Ref(loc)) | (List::Ref(loc), cons_list @ List::Cons(node)) => {
                    let ref_val = loc.get_value_deref();
                    match ref_val {
                        Value::Nil => {
                            self.trail.push(loc.clone());
                            loc.put(Value::List(List::Cons(node.clone())));
                        }
                        Value::List(list) => return self.unify_lists(&list, cons_list),
                        _ => return false
                    }
                }
                (List::Ref(loc1), List::Ref(loc2)) => {
                    let list1 = loc1.get_value_deref();
                    let list2 = loc2.get_value_deref();
                    match (&list1, &list2) {
                        (Value::Nil, Value::Nil) => {
                            self.trail.push(loc1.clone());
                            loc1.put_ref(loc2.clone());
                            return true;
                        }
                        (Value::Nil, Value::List(_)) => {
                            self.trail.push(loc1.clone());
                            loc1.put(list2.clone());
                            return true;
                        },
                        (Value::List(_), Value::Nil) => {
                            self.trail.push(loc2.clone());
                            loc2.put(list1.clone());
                            return true;
                        },
                        (Value::List(_), Value::List(_)) => {
                            self.unify(list1, list2);
                        }
                        _ => {
                            panic!("List reference values are expected to be lists - Unify {:?} and {:?}", list1, list2);
                        }
                    }
                }
                _ => unreachable!()
            }
        }

        true
    }

    pub fn execute_query_str(&mut self, query_str: &str) -> Result<Vec<Value>, QueryError>{
        if let Some(CFGNode::Query(terms)) = CFGNode::parse_query(query_str) {
            let mut ir_gen = IRGen::new();
            let query_rule = ir_gen.generate_query(terms);
            
            self.execute_query(query_rule)
        } else {
            panic!("Could not query");
        }
    }


    pub fn execute_query(&mut self, mut query: QueryDef) -> Result<Vec<Value>, QueryError> {
        self.call_stack.clear();
        self.choice_stack.clear();
        self.curr_frame = 0;

        let atom_mapping: Vec<usize> = query.atoms.iter().map(|s| self.get_or_create_atom(s)).collect();
        self.link_code(&mut query.code, &atom_mapping);

        

        self.pc = 0;
        self.code = Some(CodeBlock::from(query.code));

        self.next()

    }

    pub fn next(&mut self) -> Result<Vec<Value>, QueryError> {
        let satisfied = self.execute()?;
        if !satisfied {
            return Err(QueryError::NoResult);
        }

        let mut results = Vec::new();
        let curr_frame = &self.call_stack[0];
        for i in &curr_frame.stack {
            results.push(i.get_value_deref())
        }

        Ok(results)
    }

    pub fn execute(&mut self) -> Result<bool, QueryError> {
        let mut pc : usize = self.pc;
        let mut code = match &self.code {
            Some(code) => code.clone(),
            None => return Err(QueryError::QueryNotSet)
        };
        let mut backtrack = self.choice_stack.len() > 0;
        let mut error: Option<QueryError> = None;
        let mut satisfied = true;

        'main: loop {
            if backtrack {
                loop {
                    let last_choice = self.choice_stack.last_mut();
                    if let Some(choice_frame) = last_choice {
                        let alternates = choice_frame.rule.code.borrow();
                        if choice_frame.choice_idx + 1 < alternates.len() {
                            
                            while self.trail.len() > choice_frame.trail_top {
                                if let Some(change) = self.trail.pop() {
                                    
                                    change.put(Value::Nil);
                                }
                            }
    
                            while self.call_stack.len() > choice_frame.call_top {
                                self.call_stack.pop();
                            }

                            choice_frame.choice_idx += 1;
                            code = alternates[choice_frame.choice_idx].clone();
                            for (idx, val) in choice_frame.args.iter().enumerate() {
                                
                                self.registers[idx] = val.clone();
                            }
                            pc = 0;
                            self.freeze_idx = choice_frame.call_top;
                            self.curr_frame = choice_frame.call_top - 1;
                            self.prev_pc = choice_frame.prev_pc;
                            
                            backtrack = false;
                            break;
                        }
                    } else {
                        satisfied = false;
                        break 'main;
                    }
                    self.choice_stack.pop();
                    
                }
            }


            let instruction = &code[pc];
            pc += 1;

            

            match instruction {
                Instruction::Allocate(frame_size) => {
                    let prev_frame = self.curr_frame;
                    self.curr_frame = self.call_stack.len();
                    
                    self.call_stack.push(CallFrame::create(code.clone(), *frame_size as usize, prev_frame, self.prev_pc));
                }
                Instruction::Return => {
                    if self.call_stack.len() <= 1 || self.curr_frame == 0 {
                        
                        break
                    }        

                    
                    let (prev_frame, prev_pc) = if self.freeze_idx <= self.curr_frame {
                        assert_eq!(self.curr_frame, self.call_stack.len() - 1);
                        let frame = self.call_stack.pop().unwrap();
                        (frame.prev_frame, frame.prev_pc)
                    } else {
                        let frame = &self.call_stack[self.curr_frame];
                        (frame.prev_frame, frame.prev_pc)
                    };

                    self.curr_frame = prev_frame;

                    
                            
                    let call_frame = &mut self.call_stack[self.curr_frame];
                    pc = prev_pc;
                    code = call_frame.code.clone();

                    
                }
                Instruction::Pop(register) => self.registers[*register as usize] = self.value_stack.pop().unwrap(),
                Instruction::LoadRegister { register, variable } => {
                    let value = self.read_register(*register);
                    self.write_local_variable(*variable, value);
                }
                Instruction::StoreRegister { register, variable } => {
                    self.registers[*register as usize] = Value::Ref(self.read_local_variable(*variable));
                }
                Instruction::StoreRegisterConstant { register, constant } => {
                    self.registers[*register as usize] = constant.clone();
                },
                Instruction::ConsListRegister { register, value } => {
                    let register_idx = *register as usize;
                    let register_value = mem::replace(&mut self.registers[register_idx], Value::Nil);
                    if let Value::List(list) = register_value {
                        self.registers[register_idx] = Value::List(List::cons(value.clone(), list));
                    }
                }
                Instruction::UnifyRegisterConstant { register, constant } => {
                    let val = self.read_register(*register);
                    let res = self.unify(
                        val,
                        constant.clone()
                    );
                    if !res {
                        backtrack = true;
                    }
                }
                Instruction::PopUnifyRegister { register } => {
                    let register_val = self.read_register(*register);
                    let popped_val = self.value_stack.pop().unwrap();
                    
                    let res = self.unify(
                        register_val,
                        popped_val
                    );

                    let register_val = self.read_register(*register);
                    
                    if !res {
                        backtrack = true;
                    }
                }
                Instruction::UnifyVariables { variable1, variable2 } => {
                    let res = self.unify(
                        Value::Ref(self.read_local_variable(*variable1)),
                        Value::Ref(self.read_local_variable(*variable2))
                    );
                    if !res {
                        backtrack = true;
                    }
                }
                Instruction::UnifyVariableConstant { variable, constant } => {
                    let res = self.unify(Value::Ref(self.read_local_variable(*variable)), constant.clone());
                    if !res {
                        let value = self.read_local_variable(*variable);
                        
                        backtrack = true;
                        
                    }
                }
                Instruction::UnifyVariableRegister { variable, register } => {
                    let val = self.read_register(*register);
                    let res = self.unify(
                        Value::Ref(self.read_local_variable(*variable)),
                        val
                    );
                    if !res {
                        backtrack = true;
                    }
                }
                Instruction::Call(rule) => {
                    
                    //self.save_pc(pc);
                    self.prev_pc = pc;
                    pc = 0;
                    let rule = rule.clone();
                    let alternates = rule.code.borrow();
                    if alternates.len() == 0 {
                        panic!("Predicate not defined");
                    } else if alternates.len() > 1 {
                        let arg_count = rule.arity as usize;
                        let mut args = Vec::with_capacity(arg_count);
                        for i in 0..arg_count {
                            args.push(self.registers[i].clone());
                        } 
                        self.choice_stack.push(
                            ChoiceFrame::create(rule.clone(), args, self.trail.len(), self.call_stack.len(), self.prev_pc)
                        );
                        self.freeze_idx = self.call_stack.len();
                        
                    }
                    code = alternates[0].clone();
                }
                Instruction::NativeCall(native_pred) => {
                    (native_pred.function_ptr)(self);
                }
                Instruction::NamedCall(functor, arity) => unreachable!(),
                Instruction::PushConstant(val) => self.value_stack.push(val.clone()),
                Instruction::PushVariable(local_var) => self.value_stack.push(Value::Ref(self.read_local_variable(*local_var))),
                Instruction::CreateStructure(functor, arity) => {
                    let stack_len = self.value_stack.len();
                    let arity = *arity as usize;
                    if stack_len >= arity as usize {
                        let mut params = vec![Value::Nil; arity];
                        for i in (0..arity).rev() {
                            params[i] = self.value_stack.pop().unwrap();
                        }
                        self.value_stack.push(
                            Value::Struct(
                                Rc::from(
                                    Struct {
                                        functor: *functor,
                                        terms: params,
                                    }
                                )
                            )
                        )
                    } else {
                        panic!("Not enough values in stack")
                    }
                }
                Instruction::CreateList { head_count, with_tail } => {
                    let stack_len = self.value_stack.len();
                    let required_count = if *with_tail {
                        *head_count as usize + 1
                    } else {
                        *head_count as usize
                    };
                    if stack_len >= required_count {
                        let mut list = if *with_tail {
                            let stack_top = self.value_stack.pop().unwrap();
                            match stack_top {
                                Value::List(tail) => tail,
                                Value::Ref(value_cell) => {
                                    let value = value_cell.get_value();
                                    match value {
                                        Value::Nil => List::Ref(value_cell),
                                        Value::List(list) => list,
                                        _ => {
                                            panic!("Expected unassigned ref or ref with list");
                                        }
                                    }
                                }
                                _ => {
                                    println!("Value: {:#?}", stack_top);
                                    panic!("Expected list");
                                }
                            }
                        } else {
                            List::Nil
                        };
                        
                        for _ in 0..*head_count {
                            let head = self.value_stack.pop().unwrap();
                            list = List::cons(head, list);
                        }
                        self.value_stack.push(Value::List(list));
                    } else {
                        panic!("Not enough values in stack")
                    }
                }

            }
        }
        
        if self.choice_stack.len() == 0 {
            self.pc = 0;
            self.code = None;
        }

        

        match error {
            Some(e) => Err(e),
            None => Ok(satisfied)
        }
    }

    /*pub fn save_pc(&mut self, pc: usize) {
        let curr_frame = &mut self.call_stack[self.curr_frame];
        curr_frame.pc = pc;
    }*/

    pub fn read_register(&self, register: u32) -> Value {
        self.registers[register as usize].clone()
    }

    pub fn read_local_variable(&self, variable: u32) -> ValueCell {
        let curr_frame = &self.call_stack[self.curr_frame];
        curr_frame.stack[variable as usize].clone()
    }

    pub fn write_local_variable(&mut self, variable: u32, value: Value) {
        let curr_frame = &mut self.call_stack[self.curr_frame];
        curr_frame.stack[variable as usize].put(value);
    }

    pub fn load_module(&mut self, module: Module) {
        
        let atom_mapping: Vec<usize> = module.atoms.iter().map(|s| self.get_or_create_atom(s)).collect();
        for mut pred in module.predicates {
            let mapped_functor = atom_mapping[pred.functor];
            let rule_name = (mapped_functor, pred.arity as u32);

            self.link_code(&mut pred.code, &atom_mapping);

            match self.rules.entry(rule_name) {
                Entry::Occupied(e) => {
                    let mut alternates = e.get().code.borrow_mut();
                    alternates.push(CodeBlock::from(pred.code))
                }
                Entry::Vacant(e) => {
                    let rule = Rc::from(
                        Rule {
                            functor: pred.functor,
                            arity: pred.arity as u32,
                            code: RefCell::from(vec![CodeBlock::from(pred.code)]),
                        }
                    );
                    e.insert(rule);
                }
            }
        }
    }

    pub fn link_code(&mut self, code: &mut [Instruction], atom_mapping: &[usize]) {
        for inst in code.iter_mut() {
            match inst {
                Instruction::NamedCall(functor, arity) => {
                    let f = atom_mapping[*functor];
                    let a = *arity;
                    if let Some(native_pred) = self.get_native(f, a) {
                        *inst = Instruction::NativeCall(native_pred);
                    } else {
                        *inst = Instruction::Call(self.get_or_create_predicate(f, a));
                    }
                }
                Instruction::CreateStructure(functor, _) => {
                    *functor = atom_mapping[*functor];
                    
                }
                _ => ()
            }
        }
    }

    pub fn get_or_create_atom(&mut self, symbol: &str) -> usize {
        let next_idx = self.atoms.len();
        *self.atoms.entry(symbol.to_string()).or_insert(next_idx)
    }

    pub fn get_native(&mut self, functor: usize, arity: u32) -> Option<Rc<NativePredicate>> {
        self.native_predicates.get(&(functor, arity)).cloned()
    }

    pub fn get_or_create_predicate(&mut self, functor: usize, arity: u32) -> Rc<Rule> {
        let name = (functor, arity);
        self.rules.entry(name).or_insert_with(|| 
            Rc::from(
                Rule {
                    functor,
                    arity,
                    code: RefCell::from(vec![]),
                }
            )
        ).clone()
    }

    pub fn eval_arithmetic(&self, expr: &Value) -> Value {
        match expr {
            Value::Int(i) => Value::Int(*i),
            Value::Struct(s) => {
                let arity = s.terms.len();
                if arity == 2 {
                    let arg1 = self.eval_arithmetic(&s.terms[0]);
                    let arg2 = self.eval_arithmetic(&s.terms[1]);
                    match (arg1, arg2) {
                        (Value::Int(v1), Value::Int(v2)) => {
                            match s.functor.into() {
                                Operator::Add => Value::Int(v1 + v2),
                                Operator::Sub => Value::Int(v1 - v2),
                                _ => panic!("Invalid operands")
                            }
                        }
                        (a1, a2) => {
                            
                            panic!("Invalid operands")
                        }
                    }
                } else {
                    panic!()
                }
            }
            Value::Ref(value_cell) => value_cell.get_value_deref(),
            _ => todo!()
        }
    }

}