use std::{ops::Add, collections::{HashSet, HashMap}, hash::Hash, mem, rc::Rc};

use crate::{parser::{CFGNode, Structure, Term, ListExpr, OperatorType, PrecedenceMap, DEFAULT_PRECEDENCES, InfixExpr}, vm::{Instruction, Value, RuleInfo}};


#[derive(Debug)]
pub struct PredicateDef {
    pub functor: usize,
    pub arity: usize,
    pub code: Vec<Instruction>,
    pub variables: Vec<String>
}

#[derive(Debug)]
pub struct QueryDef {
    pub variables: Vec<String>,
    pub atoms: Vec<String>,
    pub atoms_by_symbol: HashMap<String, usize>,
    pub code: Vec<Instruction>
}

#[derive(Debug)]
pub struct Module {
    pub atoms: Vec<String>,
    pub atoms_by_symbol: HashMap<String, usize>,
    pub predicates: Vec<PredicateDef>
}

impl Module {

    fn new() -> Self {
        Module {
            atoms: Vec::new(),
            atoms_by_symbol: HashMap::new(),
            predicates: Vec::new()
        }
    }

}

pub struct IRGen<'a> {
    curr_module: Module,
    code: Vec<Instruction>,
    variables: Vec<String>,
    precedences: &'a PrecedenceMap<'a>
}

impl<'a> IRGen<'a> {

    pub fn new() -> Self {
        Self::with_precedences(&DEFAULT_PRECEDENCES)
    }

    pub fn with_precedences(precedences: &'a PrecedenceMap) -> Self {
        IRGen {
            curr_module: Module::new(),
            code: Vec::new(),
            variables: Vec::new(),
            precedences
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
        let functor = self.get_or_create_atom(fact.functor);
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
                Term::InfixExpr(terms) => todo!(),
            }
        }

        self.code.insert(0, Instruction::Allocate(self.variables.len() as u32));
        self.code.push(Instruction::Return);

        PredicateDef { 
            functor, 
            arity,
            code: mem::replace(&mut self.code, Vec::new()), 
            variables: mem::replace(&mut self.variables, Vec::new())
        }
    }

    fn generate_rule(&mut self, head: Structure, body: Vec<Term>) -> PredicateDef {
        let functor = self.get_or_create_atom(head.functor);
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
                Term::InfixExpr(terms) => todo!(),
            }
        }

        for body_term in body {
            match body_term {
                Term::Structure(s) => {
                    self.create_body(s)
                },
                Term::InfixExpr(expr) => {
                    if let Term::Structure(s) = expr.parse(self.precedences){
                        self.create_body(s)
                    } else {
                        unreachable!();
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
            variables: mem::replace(&mut self.variables, Vec::new())
        }
    }

    pub fn generate_query(&mut self, terms: Vec<Term>) -> QueryDef {
        for term in terms {
            match term {
                Term::Structure(s) => {
                    let functor = self.get_or_create_atom(s.functor);
                    let arity = s.params.len();
                    for (idx, param) in s.params.iter().enumerate() {
                        match param {
                            &Term::Number(num) => self.code.push(
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
                                let var_idx = self.get_or_create_variable(variable);
                                self.code.push(
                                    Instruction::StoreRegister {
                                        register: idx as u32, 
                                        variable: var_idx as u32
                                    }
                                );            
                            },
                            Term::Structure(structure) => {
                                self.create_structure(structure);
                                self.code.push(Instruction::Pop(idx as u32))
                            }
                            Term::Number(_) => todo!(),
                            Term::List(list_expr) => {
                                self.create_list(list_expr);
                                self.code.push(Instruction::Pop(idx as u32))
                            },
                            Term::InfixExpr(_) => todo!(),
                        }
                    }
                    self.code.push(Instruction::NamedCall(functor, arity as u32));
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
            atoms: query_module.atoms,
            atoms_by_symbol: query_module.atoms_by_symbol 
        }
    }

    pub fn create_body(&mut self, s: Structure) {
        let functor = self.get_or_create_atom(s.functor);
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
                    if let Some(var_idx) = self.variables.iter().position(|v| v == &variable) {
                        self.code.push(
                            Instruction::StoreRegister {
                                register: idx as u32,
                                variable: var_idx as u32
                            }
                        )
                    } else {
                        let var_idx = self.variables.len();
                        self.variables.push(variable.clone());
                        self.code.push(
                            Instruction::StoreRegister {
                                register: idx as u32,
                                variable: var_idx as u32
                            }
                        )
                    }

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
                Term::InfixExpr(_) => todo!()
            }
        }
        self.code.push(Instruction::NamedCall(functor, arity as u32));
    }

    pub fn create_structure(&mut self, s: &Structure) {
        let functor = self.get_or_create_atom(s.functor.clone());
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
            Term::InfixExpr(_) => todo!(),
        }
    }

    pub fn get_or_create_atom(&mut self, val: String) -> usize {
        let next_idx = self.curr_module.atoms.len();
        match self.curr_module.atoms_by_symbol.get(&val) {
            Some(atom) => *atom,
            None => {
                self.curr_module.atoms.push(val.clone());
                self.curr_module.atoms_by_symbol.insert(val, next_idx);
                next_idx
            }
        }
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

} 