use std::{ops::Add, collections::{HashSet, HashMap}, hash::Hash, mem, rc::Rc};

use crate::{parser::{CFGNode, Structure, Term, ListExpr, OperatorType, PrecedenceMap, DEFAULT_PRECEDENCES}, vm::{Instruction, Value, RuleInfo, ArithComparisonOp, Struct, ValueCell, List, Namespace, ValueCellContent}};


#[derive(Debug)]
pub struct PredicateDef {
    pub functor: usize,
    pub arity: usize,
    pub code: Vec<Instruction>,
    pub variables: Vec<String>,
    pub head: Value,
    pub body: Vec<Value>
}

#[derive(Debug)]
pub struct QueryDef {
    pub variables: Vec<String>,
    pub namespace: Namespace,
    pub code: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Module {
    pub namespace: Namespace,
    pub predicates: Vec<PredicateDef>
}

impl Module {

    fn new() -> Self {
        Module {
            namespace: Namespace::new(),
            predicates: Vec::new()
        }
    }

}

pub struct IRGen {
    curr_module: Module,
    code: Vec<Instruction>,
    variables: Vec<String>,
}

impl IRGen {

    pub fn new() -> Self {
        IRGen {
            curr_module: Module::new(),
            code: Vec::new(),
            variables: Vec::new(),
        }
    }

    pub fn generate(&mut self, cfg: CFGNode) -> Module {

        match cfg {
            CFGNode::Fact(fact) => {
                let pred = self.generate_fact(fact);
                self.curr_module.predicates.push(pred);
            }
            CFGNode::Rule(head, body) => {
                let pred = self.generate_rule(head, body);
                self.curr_module.predicates.push(pred);
            }
            CFGNode::File(nodes) => {
                let pred_vec = self.generate_file(nodes);
                self.curr_module.predicates.extend(pred_vec);
            }
            _ => unreachable!()
        }

        mem::replace(&mut self.curr_module, Module::new())
    }

    fn generate_file(&mut self, nodes: Vec<CFGNode>) -> Vec<PredicateDef> {
        let mut predicates : Vec<PredicateDef> = Vec::new();
        for node in nodes {
            match node {
                CFGNode::Fact(fact) => predicates.push(self.generate_fact(fact)),
                CFGNode::Rule(head, body) => predicates.push(self.generate_rule(head, body)),
                _ => unreachable!()
            }
        }
        predicates
    }

    fn generate_fact(&mut self, fact: Structure) -> PredicateDef {
        let functor = self.get_or_create_atom(&fact.functor);
        let arity = fact.params.len();

        for (idx, param) in fact.params.iter().enumerate() {
            match param {
                &Term::Number(num) => self.code.push(
                    Instruction::UnifyRegisterConstant {
                        register: idx as u32, 
                        constant: Value::Int(num)
                    }
                ),
                Term::String(s) => self.code.push(
                    Instruction::UnifyRegisterConstant {
                        register: idx as u32, 
                        constant: Value::Str(Rc::from(s.as_ref()))
                    }
                ),
                Term::Atom(atom) => todo!(),
                Term::Variable(variable) => {
                    if let Some(var_idx) = self.variables.iter().position(|v| v == variable) {
                        self.code.push(
                            Instruction::UnifyVariableRegister {
                                variable: var_idx as u32,
                                register: idx as u32
                            }
                        )
                    } else {
                        let var_idx = self.variables.len();
                        self.variables.push(String::from(variable));
                        self.code.push(
                            Instruction::LoadRegister{
                                register: idx as u32, 
                                variable: var_idx as u32
                            }
                        )
                    }
                },
                Term::Structure(_) => todo!(),
                Term::Number(_) => todo!(),
                Term::List(list_expr) => { 
                    self.create_list(list_expr);
                    self.code.push(Instruction::PopUnifyRegister { register: idx as u32 });
                }
            }
        }

        self.code.insert(0, Instruction::Allocate(self.variables.len() as u32));
        self.code.push(Instruction::Return);

        PredicateDef { 
            functor, 
            arity,
            code: mem::replace(&mut self.code, Vec::new()), 
            variables: mem::replace(&mut self.variables, Vec::new()),
            head: self.convert_to_meta_value(Term::Structure(fact)),
            body: Vec::new()
        }
    }


    fn generate_rule(&mut self, head: Structure, body: Vec<Term>) -> PredicateDef {
        let functor = self.get_or_create_atom(&head.functor);
        let arity = head.params.len();

        for (idx, param) in head.params.iter().enumerate() {
            match param {
                &Term::Number(num) => self.code.push(
                    Instruction::UnifyRegisterConstant {
                        register: idx as u32, 
                        constant: Value::Int(num)
                    }
                ),
                Term::String(s) => self.code.push(
                    Instruction::UnifyRegisterConstant {
                        register: idx as u32, 
                        constant: Value::Str(Rc::from(s.as_ref()))
                    }
                ),
                Term::Atom(atom) => todo!(),
                Term::Variable(variable) => {
                    if let Some(var_idx) = self.variables.iter().position(|v| v == variable) {
                        self.code.push(
                            Instruction::UnifyVariableRegister {
                                variable: var_idx as u32, 
                                register: idx as u32
                            }
                        )
                    } else {
                        let var_idx = self.variables.len();
                        self.variables.push(variable.clone());
                        self.code.push(
                            Instruction::LoadRegister {
                                register: idx as u32,
                                variable: var_idx as u32
                            }
                        )
                    }
                },
                Term::Structure(_) => todo!(),
                Term::Number(_) => todo!(),
                Term::List(list_expr) => { 
                    self.create_list(list_expr);
                    self.code.push(Instruction::PopUnifyRegister { register: idx as u32 });
                }
            }
        }

        for body_term in body.clone() {
            match body_term {
                Term::Structure(s) => {
                    self.create_body(s)
                },
                Term::Atom(atom) => {
                    if atom == "!" {
                        self.code.push(Instruction::Cut);
                    } else {
                        todo!();
                    }
                }
                _ => todo!()
            }
        }

        self.code.insert(0, Instruction::Allocate(self.variables.len() as u32));
        self.code.push(Instruction::Return);

        PredicateDef { 
            functor, 
            arity,
            code: mem::replace(&mut self.code, Vec::new()), 
            variables: mem::replace(&mut self.variables, Vec::new()),
            head: self.convert_to_meta_value(Term::Structure(head)),
            body: body.into_iter().map(|term| self.convert_to_meta_value(term)).collect()
        }
    }

    pub fn generate_query(&mut self, terms: Vec<Term>) -> QueryDef {
        for term in terms {
            match term {
                Term::Structure(s) => {
                    self.create_body(s)
                },
                Term::Number(_) => todo!(),
                Term::Atom(_) => todo!(),
                Term::Variable(_) => todo!(),
                _ => todo!()
            }
        }

        for idx in 0..self.variables.len() {
            self.code.push(Instruction::StoreRegister {
                register: idx as u32, 
                variable: idx as u32
            });
        }

        self.code.insert(0, Instruction::Allocate(self.variables.len() as u32));
        self.code.push(Instruction::Return);

        let query_module = mem::replace(&mut self.curr_module, Module::new());

        QueryDef { 
            variables: mem::replace(&mut self.variables, Vec::new()), 
            code: mem::replace(&mut self.code, Vec::new()),
            namespace: query_module.namespace,
        }
    }

    pub fn create_body(&mut self, s: Structure) {
        let functor = self.get_or_create_atom(&s.functor);
        let arity = s.params.len();

        for (idx, param) in s.params.into_iter().enumerate() {
            match param {
                Term::Number(num) => self.code.push(
                    Instruction::StoreRegisterConstant {
                        register: idx as u32, 
                        constant: Value::Int(num)
                    }
                ),
                Term::String(s) => self.code.push(
                    Instruction::StoreRegisterConstant {
                        register: idx as u32,
                        constant: Value::Str(Rc::from(s.as_ref()))
                    }
                ),
                Term::Atom(atom) => todo!(),
                Term::Variable(variable) => {
                    let var_idx = self.get_or_create_variable(&variable);
                    self.code.push(
                        Instruction::StoreRegister {
                            register: idx as u32, 
                            variable: var_idx as u32
                        }
                    );   
                },
                Term::Structure(structure) => {
                    self.create_structure(&structure);
                    self.code.push(Instruction::Pop(idx as u32))
                }
                Term::Number(_) => todo!(),
                Term::List(list_expr) => { 
                    self.create_list(&list_expr);
                    self.code.push(Instruction::Pop(idx as u32))
                }
            }
        }
        self.handle_functor(functor, arity as u32);
    }

    pub fn create_structure(&mut self, s: &Structure) {
        let functor = self.get_or_create_atom(&s.functor);
        let arity = s.params.len() as u32;

        for param in &s.params {
            self.push_term(param);
        }

        self.code.push(Instruction::CreateStructure(functor, arity));
    }

    pub fn create_list(&mut self, list: &ListExpr) {
        for head in &list.heads {
            self.push_term(head);
        }
        if let Some(tail) = &list.tail {
            self.push_term(tail)
        }

        self.code.push(Instruction::CreateList {
            head_count: list.heads.len() as u32,
            with_tail: list.tail.is_some()
        });
    }

    pub fn push_term(&mut self, term: &Term) {
        match term {
            Term::Number(num) => self.code.push(Instruction::PushConstant(Value::Int(*num))),
            Term::Atom(_) => todo!(),
            Term::String(_) => todo!(),
            Term::Variable(name) => {
                let var_idx = self.get_or_create_variable(name);
                self.code.push(Instruction::PushVariable(var_idx))
            }
            Term::Structure(s) => self.create_structure(s),
            Term::List(l) => self.create_list(l),
        }
    }

    pub fn handle_functor(&mut self, functor: usize, arity: u32) -> bool {
        let functor_str = self.curr_module.namespace.get_atom_symbol(functor).expect("Invalid functor id");
        match (functor_str, arity) {
            ("=:=", 2) => self.create_comparison_op(ArithComparisonOp::Eq),
            ("=/=", 2) => self.create_comparison_op(ArithComparisonOp::Neq),
            ("<",   2) => self.create_comparison_op(ArithComparisonOp::Lt),
            ("=<",  2) => self.create_comparison_op(ArithComparisonOp::Lte),
            (">",   2) => self.create_comparison_op(ArithComparisonOp::Gt),
            (">=",  2) => self.create_comparison_op(ArithComparisonOp::Gte),
            _          => self.code.push(Instruction::NamedCall(functor, arity as u32))
        }
        true
    }

    pub fn create_comparison_op(&mut self, op: ArithComparisonOp) {
        self.code.push(Instruction::CompareArithRegisters {
            op,
            register1: 0,
            register2: 1,
        });
    }

    pub fn get_or_create_atom(&mut self, val: &str) -> usize {
        self.curr_module.namespace.get_or_create_atom(val)
    }

    pub fn get_or_create_variable(&mut self, val: &str) -> u32 {
        if let Some(var_idx) = self.variables.iter().position(|v| v == val) {
            var_idx as u32
        } else {
            let var_idx = self.variables.len();
            self.variables.push(String::from(val));
            var_idx as u32
        }
    }

    fn convert_to_meta_value(&mut self, term: Term) -> Value {
        match term {
            Term::Number(num) => Value::Int(num),
            Term::Atom(atom) => Value::Atom(self.get_or_create_atom(&atom)),
            Term::String(string) => Value::Str(Rc::from(string.as_ref())),
            Term::Variable(variable) => Value::Ref(ValueCell::new()),
            Term::Structure(structure) => {
                let functor = self.get_or_create_atom(&structure.functor);
                let params: Vec<Value> = structure.params
                    .into_iter()
                    .map(|p| self.convert_to_meta_value(p))
                    .collect(); 
                Value::create_struct(functor, params)
            }
            Term::List(list) => {
                let converted_tail = list.tail.map_or(Value::List(List::Nil), |lst| self.convert_to_meta_value(*lst));
                println!("{:?}", converted_tail);
                let mut curr = match converted_tail {
                    Value::List(tail) => tail,
                    Value::Ref(value_cell) => List::Ref(value_cell),
                    _ => unreachable!()
                };
                
                for el in list.heads.into_iter().rev() {
                    let value = self.convert_to_meta_value(el);
                    curr = List::cons(value, curr);
                }
                Value::List(curr)
            }
        }
    }


    pub fn generate_fact_from_value(&mut self, head: Value) -> PredicateDef {
        if let Value::Struct(structure) = head.clone() {
            for (idx, param) in structure.params.iter().enumerate() {
                match param {
                    Value::Ref(value_cell) => {
                        match value_cell.get_content() {
                            ValueCellContent::Bound(val) => self.code.push(
                                Instruction::UnifyRegisterConstant {
                                    register: idx as u32, 
                                    constant: val
                                }
                            ),
                            ValueCellContent::Unbound(id) => {
                                let variable = format!("_V{}", id);
                                if let Some(var_idx) = self.variables.iter().position(|v| v == &variable) {
                                    self.code.push(
                                        Instruction::UnifyVariableRegister {
                                            variable: var_idx as u32,
                                            register: idx as u32
                                        }
                                    )
                                } else {
                                    let var_idx = self.variables.len();
                                    self.variables.push(variable);
                                    self.code.push(
                                        Instruction::LoadRegister{
                                            register: idx as u32, 
                                            variable: var_idx as u32
                                        }
                                    )
                                }
                            }
                        }
                    },
                    Value::Struct(_) => todo!(),
                    Value::List(_) => todo!(),
                    _ => self.code.push(
                        Instruction::UnifyRegisterConstant {
                            register: idx as u32, 
                            constant: param.clone()
                        }
                    )
                }
            }        

            self.code.insert(0, Instruction::Allocate(self.variables.len() as u32));
            self.code.push(Instruction::Return);
            
            PredicateDef { 
                functor: structure.functor,
                arity: structure.params.len(),
                code: mem::replace(&mut self.code, Vec::new()), 
                variables: mem::replace(&mut self.variables, Vec::new()),
                head: head,
                body: Vec::new()
            }
        } else {
            todo!()
        }
    }
}