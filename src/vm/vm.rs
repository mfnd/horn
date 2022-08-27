use std::cell::RefCell;
use std::{mem, vec};
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::prelude::BUILTINS;
use crate::debugln;
use crate::ir_gen::{QueryDef, IRGen, Module, PredicateDef};
use crate::parser::{CFGNode, PrologParser};
use crate::vm::{List, Instruction, Struct};

use super::error::{RuntimeError, RuntimeErrorType};
use super::{ValueCell, CodeBlock, Rule, NativePredicate, PRELUDE_ATOMS, PreludeAtoms, ArithComparisonOp, unify, Trail, Namespace};
use super::value::Value;

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


pub type RuntimeResult<T> = Result<T, Box<RuntimeError>>;
pub type QueryResult = RuntimeResult<Vec<Value>>;

pub struct PrologVM {
    namespace: Namespace,
    registers: Vec<Value>,
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    choice_stack: Vec<ChoiceFrame>,
    trail: Trail,
    curr_frame: usize,
    freeze_idx: usize,
    code: Option<CodeBlock>,
    pc: usize,
    prev_pc: usize,
}

impl PrologVM {

    pub fn new() -> Self {
        let mut namespace = Namespace::new();
        for (op, ident) in PRELUDE_ATOMS {
            assert!(*op as usize == namespace.atom_count());
            namespace.get_or_create_atom(ident);
        }

        BUILTINS.iter().for_each(|(name, builtin)| {
            let idx = namespace.get_or_create_atom(name.0);
            namespace.create_native(idx, name.1, *builtin);
        });

        PrologVM { 
            namespace,
            registers: vec![Value::Nil; 16],
            value_stack: Vec::new(),
            call_stack: Vec::new(),
            choice_stack: Vec::new(),
            trail: Trail::new(),
            curr_frame: 0,
            freeze_idx: 0,
            code: None,
            pc: 0,
            prev_pc: 0,
        }
    }

    #[inline(always)]
    pub fn unify(&mut self, val1: Value, val2: Value) -> bool {
        unify(val1, val2, &mut self.trail)
    }

    pub fn set_query_from_str(&mut self, query_str: &str) -> RuntimeResult<()> {
        if let Some(CFGNode::Query(terms)) = PrologParser::new().parse_query(query_str) {
            let mut ir_gen = IRGen::new();
            let query_rule = ir_gen.generate_query(terms);

            self.set_query(query_rule);
            Ok(())
        } else {
            Err(Box::from(
                RuntimeError {
                    error_type: RuntimeErrorType::ParseError,
                    cause: None,
                    message: String::from("Parse error"),
                }
            ))
        }
    }

    pub fn set_query(&mut self, mut query: QueryDef) {
        self.call_stack.clear();
        self.choice_stack.clear();
        self.curr_frame = 0;

        let atom_mapping: Vec<usize> = query.namespace.atoms.iter().map(|s| self.get_or_create_atom(s)).collect();
        self.namespace.link_code(&mut query.code, &atom_mapping);

        self.pc = 0;
        self.code = Some(CodeBlock::from(query));
    }

    pub fn execute(&mut self) -> Result<bool, Box<RuntimeError>> {
        let mut pc : usize = self.pc;
        let mut code = match &self.code {
            Some(code) => code.clone(),
            None => return Err(RuntimeError::query_not_set())
        };
        let mut backtrack = self.choice_stack.len() > 0;
        let mut satisfied = true;

        'main: loop {
            if backtrack {
                loop {
                    let last_choice = self.choice_stack.last_mut();
                    if let Some(choice_frame) = last_choice {
                        let alternates = choice_frame.rule.code.borrow();
                        if choice_frame.choice_idx + 1 < alternates.len() {
                            debugln!("Trying alternative: {}", choice_frame.choice_idx + 1);

                            self.trail.rollback_until(choice_frame.trail_top);
    
                            while self.call_stack.len() > choice_frame.call_top {
                                self.call_stack.pop();
                            }

                            choice_frame.choice_idx += 1;
                            code = alternates[choice_frame.choice_idx].clone();
                            for (idx, val) in choice_frame.args.iter().enumerate() {
                                debugln!("Reverting register {} to {:?}", idx, val);
                                self.registers[idx] = val.clone();
                            }
                            pc = 0;
                            self.freeze_idx = choice_frame.call_top;
                            self.curr_frame = choice_frame.call_top - 1;
                            self.prev_pc = choice_frame.prev_pc;
                            debugln!("Reverting freeze idx {} curr frame {}", self.freeze_idx, self.curr_frame);
                            backtrack = false;
                            break;
                        }
                    } else {
                        satisfied = false;
                        break 'main;
                    }
                    self.choice_stack.pop();
                    debugln!("Popped choice frame: {}", self.choice_stack.len());
                }
            }


            let instruction = &code[pc];
            pc += 1;

            match instruction {
                Instruction::Allocate(frame_size) => {
                    let prev_frame = self.curr_frame;
                    self.curr_frame = self.call_stack.len();
                    debugln!("Allocating {:?} {:?}", prev_frame, self.curr_frame);
                    self.call_stack.push(CallFrame::create(code.clone(), *frame_size as usize, prev_frame, self.prev_pc));
                }
                Instruction::Return => {
                    if self.call_stack.len() <= 1 || self.curr_frame == 0 {
                        debugln!("Call stack empty");
                        break
                    }        

                    debugln!("Freeze {:?} Curr {:?}", self.freeze_idx, self.curr_frame);
                    let (prev_frame, prev_pc) = if self.freeze_idx <= self.curr_frame {
                        assert_eq!(self.curr_frame, self.call_stack.len() - 1);
                        let frame = self.call_stack.pop().unwrap();
                        (frame.prev_frame, frame.prev_pc)
                    } else {
                        let frame = &self.call_stack[self.curr_frame];
                        (frame.prev_frame, frame.prev_pc)
                    };

                    self.curr_frame = prev_frame;

                    debugln!("Prev frame {}", self.curr_frame);
                            
                    let call_frame = &mut self.call_stack[self.curr_frame];
                    pc = prev_pc;
                    code = call_frame.code.clone();

                    debugln!("Curr frame {:?} Freeze idx {:?} Pc {:?}", self.curr_frame, self.freeze_idx, pc); 
                }
                &Instruction::Cut => {
                    self.choice_stack.clear();
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
                        debugln!("Can not unify constant - Var {}: Val: {:?} Const: {:?}", variable, value, constant);
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
                Instruction::CompareArithRegisters { op, register1, register2 } => {
                    let res = self.compare_arithmetic(
                        *op,
                        &self.read_register(*register1),
                        &self.read_register(*register2)
                    )?;
                    if !res {
                        backtrack = true;
                    }
                }
                Instruction::Call(rule) => {
                    self.prev_pc = pc;
                    pc = 0;
                    let rule = rule.clone();
                    let alternates = rule.code.borrow();
                    if alternates.len() == 0 {
                        panic!("Predicate not defined: {}/{}", self.namespace.get_atom_symbol(rule.functor).unwrap_or("UNKNOWN"), rule.arity);
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
                        debugln!("Pushed choice frame {}", self.choice_stack.len());
                    }
                    code = alternates[0].clone();
                }
                Instruction::NativeCall(native_pred) => {
                    (native_pred.function_ptr)(self)?;
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
                                        params,
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
                                    debugln!("Value: {:#?}", stack_top);
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

        Ok(satisfied)
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
        self.namespace.load_module(module)
    }

    pub fn load_predicate(&mut self, predicate: PredicateDef) {
        self.namespace.load_predicate(predicate)
    }

    pub fn get_or_create_atom(&mut self, symbol: &str) -> usize {
        self.namespace.get_or_create_atom(symbol)
    }

    pub fn get_predicate(&mut self, functor: usize, arity: u32) -> Option<Rc<Rule>> {
        self.namespace.get_predicate(functor, arity)
    }

    pub fn eval_arithmetic(&self, expr: &Value) -> RuntimeResult<Value> {
        let result = match expr {
            Value::Int(i) => Value::Int(*i),
            Value::Struct(s) => {
                let arity = s.params.len();
                if arity == 2 {
                    let arg1 = self.eval_arithmetic(&s.params[0])?;
                    let arg2 = self.eval_arithmetic(&s.params[1])?;
                    match (arg1, arg2) {
                        (Value::Int(v1), Value::Int(v2)) => {
                            match s.functor {
                                PreludeAtoms::Add => Value::Int(v1 + v2),
                                PreludeAtoms::Sub => Value::Int(v1 - v2),
                                PreludeAtoms::Mul => Value::Int(v1 * v2),
                                // TODO: Handle division by zero
                                PreludeAtoms::Div => Value::Int(v1 / v2),
                                _ => return Err(Box::from(
                                    RuntimeError {
                                        error_type: RuntimeErrorType::ArithmeticError,
                                        cause: None,
                                        message: format!(
                                            "Unsupported arithmetic operation {:?}", self.namespace.get_atom_symbol(s.functor).unwrap()
                                        ),
                                    }
                                ))
                            }
                        }
                        (a1, a2) => {
                            return Err(Box::from(
                                RuntimeError {
                                    error_type: RuntimeErrorType::ValueError,
                                    cause: None,
                                    message: format!(
                                        "Invalid operands {:?} and {:?} for {:?}", a1.type_str(), a2.type_str(), self.namespace.get_atom_symbol(s.functor).unwrap()
                                    ),
                                }
                            ))
                        }
                    }
                } else {
                    return Err(Box::from(
                        RuntimeError {
                            error_type: RuntimeErrorType::ArithmeticError,
                            cause: None,
                            message: format!(
                                "Invalid arithmetic function {}/{}", self.namespace.get_atom_symbol(s.functor).unwrap(), arity
                            ),
                        }
                    ))
                }
            }
            Value::List(list) => Value::List(list.clone()),
            Value::Ref(value_cell) => self.eval_arithmetic(&value_cell.get_value_deref())?,
            _ => todo!()
        };
        Ok(result)
    }

    pub fn compare_arithmetic(&self, op: ArithComparisonOp, left: &Value, right: &Value) -> RuntimeResult<bool> {
        let left_val = self.eval_arithmetic(left)?;
        let right_val = self.eval_arithmetic(right)?;
        let result = match op {
            ArithComparisonOp::Eq  => left_val == right_val,
            ArithComparisonOp::Neq => left_val != right_val,
            ArithComparisonOp::Lt  => left_val <  right_val,
            ArithComparisonOp::Lte => left_val <= right_val,
            ArithComparisonOp::Gt  => left_val >  right_val,
            ArithComparisonOp::Gte => left_val >= right_val,
        };
        Ok(result)
    }

}

impl Iterator for PrologVM {
    type Item = QueryResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.execute() {
            Ok(true) => {
                let mut results = Vec::new();
                let curr_frame = &self.call_stack[0];
                for i in &curr_frame.stack {
                    // Deep copy unified variables since backtracking can revert bindings
                    // TODO: Is there a way to solve this without resorting to deep copy?
                    results.push(i.get_value_deref().deep_copy())
                }
        
                Some(Ok(results))
            }
            Ok(false) => None,
            Err(err) => {
                if let RuntimeErrorType::QueryNotSetError = err.error_type {
                    None
                } else {
                    Some(Err(err))
                }
            }
        }
    }
}