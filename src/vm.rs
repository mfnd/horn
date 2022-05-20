use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt;
use std::hash::Hash;
use std::iter::Inspect;
use std::ops::Index;
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;


#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    All,
    Atom(usize),
    Int(u64),
    Str(String),
    Struct(Rc<Struct>),
    Ref(ValueCell)
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

pub enum Intrinsic {
    PrintRegister(u32),
    PrintVariable(u32)
}

pub enum Instruction {
    Allocate(u32),
    Return,
    LoadReg(u32, u32),
    StoreRegVariable(u32, u32),
    StoreRegConstant(u32, Value),
    UnifyVariable(u32, u32),
    UnifyConstant(u32, Value),
    UnifyRegister(u32, u32),
    Call(Rc<Rule>),
    NamedCall(Box<RuleInfo>),
    IntrinsicCall(Intrinsic)
}

struct CallFrame {
    stack: Vec<ValueCell>,
    code: CodeBlock,
    pc: usize
}

impl CallFrame {
    fn create(code: CodeBlock, stack_size: usize) -> Self {
        CallFrame {
            stack: (0..stack_size).map(|_| ValueCell::new()).collect(),
            code,
            pc: 0
        }
    }
}

struct ChoiceFrame {
    args: Vec<Value>,
    rule: Rc<Rule>,
    choice_idx: usize,
    trail_top: usize,
    call_top: usize,
}

impl ChoiceFrame {
    fn create(rule: Rc<Rule>, args: Vec<Value>, trail_top: usize, call_top: usize) -> Self {
        ChoiceFrame {
            args,
            rule,
            choice_idx: 0,
            trail_top,
            call_top
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

pub struct RuleInfo {
    functor: String,
    arity: u32
}

impl RuleInfo {
    pub fn boxed(functor: &str, arity: u32) -> Box<Self> {
        Box::from(RuleInfo { functor: functor.to_string(), arity: arity })
    }
}

pub struct PrologVM {
    rules: HashMap<(usize, u32), Rc<Rule>>,
    atoms: HashMap<String, usize>,
    registers: Vec<Value>,
    call_stack: Vec<CallFrame>,
    choice_stack: Vec<ChoiceFrame>,
    trail: Vec<ValueCell>,
    curr_frame: usize,
    freeze_idx: usize,
}

impl PrologVM {

    pub fn new() -> Self {
        PrologVM { 
            rules: HashMap::new(),
            atoms: HashMap::new(),
            registers: vec![Value::Nil; 16],
            call_stack: Vec::new(),
            choice_stack: Vec::new(),
            trail: Vec::new(),
            curr_frame: 0,
            freeze_idx: 0,
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

    pub fn execute_code(&mut self, code: Vec<Instruction>) {
        let query_rule = Rule {
            functor: 0,
            arity: 0,
            code: RefCell::from(vec![CodeBlock::from(code)])
        };
        self.execute(Rc::from(query_rule));

    }

    pub fn execute(&mut self, rule: Rc<Rule>) {
        let mut pc : usize = 0;
        let mut code = rule.code.borrow()[0].clone();
        let mut backtrack = false;

        loop {
            if backtrack {
                loop {
                    let last_choice = self.choice_stack.last_mut();
                    if let Some(choice_frame) = last_choice {
                        let alternates = choice_frame.rule.code.borrow();
                        if choice_frame.choice_idx + 1 < alternates.len() {
                            println!("Trying alternative: {}", choice_frame.choice_idx + 1);
                            while self.trail.len() > choice_frame.trail_top {
                                if let Some(change) = self.trail.pop() {
                                    println!("Reverting {:?}", change);
                                    change.put(Value::Nil);
                                }
                            }
    
                            while self.call_stack.len() > choice_frame.call_top {
                                self.call_stack.pop();
                            }

                            choice_frame.choice_idx += 1;
                            code = alternates[choice_frame.choice_idx].clone();
                            for (idx, val) in choice_frame.args.iter().enumerate() {
                                println!("Reverting register {} to {:?}", idx, val);
                                self.registers[idx] = val.clone();
                            }
                            pc = 0;
                            self.freeze_idx = choice_frame.call_top;
                            backtrack = false;
                            break;
                        }
                    } else {
                        panic!("Can not backtrack")
                    }
                    self.choice_stack.pop();
                    print!("Popped choice frame: {}", self.choice_stack.len());
                }
            }


            let instruction = &code[pc];
            pc += 1;

            match instruction {
                Instruction::Allocate(frame_size) => {
                    self.curr_frame = self.call_stack.len();
                    self.call_stack.push(CallFrame::create(code.clone(), *frame_size as usize));
                }
                Instruction::Return => {
                    println!("Returning");
                    if self.freeze_idx < self.curr_frame {
                        self.call_stack.pop();
                    } 

                    if self.call_stack.is_empty() || self.curr_frame == 0 {
                        break
                    }
                    println!("sizes {} {}", self.call_stack.len(), self.curr_frame);
                    self.curr_frame -= 1;                    
                    let call_frame = &mut self.call_stack[self.curr_frame];
                    pc = call_frame.pc;
                    code = call_frame.code.clone();
                }
                Instruction::LoadReg(register, local_var) => {
                    let value = self.read_register(*register);
                    self.write_local_variable(*local_var, value);
                }
                Instruction::StoreRegVariable(register, local_var) => {
                    self.registers[*register as usize] = Value::Ref(self.read_local_variable(*local_var));
                }
                Instruction::StoreRegConstant(register, value) => {
                    self.registers[*register as usize] = value.clone();
                }
                Instruction::UnifyRegister(var, reg) => {
                    let val = self.read_register(*reg);
                    let res = self.unify(
                        Value::Ref(self.read_local_variable(*var)),
                        val
                    );
                    if !res {
                        panic!("Can not unify");
                    }
                }
                Instruction::UnifyVariable(var1, var2) => {
                    let res = self.unify(
                        Value::Ref(self.read_local_variable(*var1)),
                        Value::Ref(self.read_local_variable(*var2))
                    );
                    if !res {
                        panic!("Can not unify");
                    }
                }
                Instruction::UnifyConstant(var, constant) => {
                    let res = self.unify(Value::Ref(self.read_local_variable(*var)), constant.clone());
                    if !res {
                        let variable = self.read_local_variable(*var);
                        backtrack = true;
                        println!("Can not unify constant - Var {}: Val: {:?} Const: {:?}", var, variable, constant);
                    }
                },
                Instruction::Call(_) => todo!(),
                Instruction::NamedCall(rule_info) => {
                    println!("Calling {}/{}", &rule_info.functor, &rule_info.arity);
                    self.save_pc(pc);
                    pc = 0;
                    let functor = *self.atoms.get(&rule_info.functor)
                        .expect(format!("Functor not found {}/{}", &rule_info.functor, &rule_info.arity).as_str());
                    let rule = self.rules.get(&(functor, rule_info.arity))
                        .expect(format!("Rule not found {}/{}", &rule_info.functor, &rule_info.arity).as_str());
                    let alternates = rule.code.borrow();
                    if alternates.len() > 1 {
                        let arg_count = rule.arity as usize;
                        let mut args = Vec::with_capacity(arg_count);
                        for i in 0..arg_count {
                            args.push(self.registers[i].clone());
                        } 
                        self.choice_stack.push(
                            ChoiceFrame::create(rule.clone(), args, self.trail.len(), self.call_stack.len())
                        );
                        self.freeze_idx = self.call_stack.len();
                        println!("Pushed choice frame {}", self.choice_stack.len());
                    }
                    code = alternates[0].clone();
                }
                Instruction::IntrinsicCall(intrinsic) => self.execute_intrinsic(intrinsic),
            }
        }
    }

    pub fn execute_intrinsic(&self, intrinsic: &Intrinsic) {
        match intrinsic {
            Intrinsic::PrintRegister(register) => {
                let val1 = self.read_register(*register);
                println!("Register {}: {:?}", *register, val1);
            }
            Intrinsic::PrintVariable(var) => {
                let val = self.read_local_variable(*var);
                println!("Variable {}: {:?}", var, val);
            }
        }
    }

    pub fn save_pc(&mut self, pc: usize) {
        let curr_frame = &mut self.call_stack[self.curr_frame];
        curr_frame.pc = pc;
    }

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

    pub fn load_rule(&mut self, functor: &str, arity: u32, code: Vec<Instruction>) {
        let next_atom_idx = self.atoms.len();
        let functor = *self.atoms.entry(functor.to_string()).or_insert(next_atom_idx);
        let rule_name = (functor, arity);


        match self.rules.entry(rule_name) {
            Entry::Occupied(e) => {
                let mut alternates = e.get().code.borrow_mut();
                alternates.push(CodeBlock::from(code))
            }
            Entry::Vacant(e) => {
                let rule = Rc::from(
                    Rule {
                        functor,
                        arity,
                        code: RefCell::from(vec![CodeBlock::from(code)]),
                    }
                );
                e.insert(rule);
            }
        }
    }

}