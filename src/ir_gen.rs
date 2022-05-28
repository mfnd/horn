use std::{ops::Add, collections::HashSet};

use crate::{parser::{CFGNode, Structure, Term}, vm::{Instruction, Value, RuleInfo}};


#[derive(Debug)]
pub struct RuleDef {
    pub functor: String,
    pub arity: usize,
    pub code: Vec<Instruction>
}

#[derive(Debug)]
pub struct QueryDef {
    pub variables: Vec<String>,
    pub code: Vec<Instruction>
}

pub struct IRGen {
    
}

impl IRGen {

    pub fn generate(&self, cfg: CFGNode) -> Vec<RuleDef> {
        let mut rule_defs = Vec::new();

        match cfg {
            CFGNode::Fact(fact) => rule_defs.push(self.generate_fact(fact)),
            CFGNode::Rule(_, _) => todo!(),
            CFGNode::File(nodes) => {
                for n in nodes {
                    rule_defs.extend(self.generate(n));
                }
            }
            _ => unreachable!()
        }

        rule_defs
    }

    fn generate_fact(&self, fact: Structure) -> RuleDef {
        let functor = fact.functor;
        let arity = fact.params.len();

        let mut code = Vec::new();
        let mut variables : Vec<&str> = Vec::new();
        for (idx, param) in fact.params.iter().enumerate() {
            match param {
                Term::Number(num) => code.push(
                    Instruction::UnifyRegisterWithConstant(idx as u32, Value::Int(*num))
                ),
                Term::Atom(atom) => todo!(),
                Term::Variable(variable) => {
                    if let Some(var_idx) = variables.iter().position(|&v| v == variable) {
                        code.push(
                            Instruction::UnifyRegister(var_idx as u32, idx as u32)
                        )
                    } else {
                        let var_idx = variables.len();
                        variables.push(variable);
                        code.push(
                            Instruction::LoadReg(idx as u32, var_idx as u32)
                        )
                    }
                },
                Term::Structure(_) => todo!(),
            }
        }

        code.insert(0, Instruction::Allocate(variables.len() as u32));
        code.push(Instruction::Return);

        RuleDef { functor, arity, code }
    }

    pub fn generate_query(&self, terms: Vec<Term>) -> QueryDef {
        let mut code = Vec::new();
        let mut variables : Vec<String> = Vec::new();
        for term in terms {
            match term {
                Term::Structure(s) => {
                    let functor = s.functor;
                    let arity = s.params.len();
                    for (idx, param) in s.params.iter().enumerate() {
                        match param {
                            Term::Number(num) => code.push(
                                Instruction::StoreRegConstant(idx as u32, Value::Int(*num))
                            ),
                            Term::Atom(atom) => todo!(),
                            Term::Variable(variable) => {
                                if let Some(var_idx) = variables.iter().position(|v| v == variable) {
                                    code.push(
                                        Instruction::StoreRegVariable(idx as u32, var_idx as u32)
                                    )
                                } else {
                                    let var_idx = variables.len();
                                    variables.push(variable.clone());
                                    code.push(
                                        Instruction::StoreRegVariable(idx as u32, var_idx as u32)
                                    )
                                }
            
                            },
                            Term::Structure(_) => todo!(),
                        }
                    }
                    code.push(Instruction::NamedCall(RuleInfo::boxed(&functor, arity as u32)));
                },
                Term::Number(_) => todo!(),
                Term::Atom(_) => todo!(),
                Term::Variable(_) => todo!()
            }
        }

        for idx in 0..variables.len() {
            code.push(Instruction::StoreRegVariable(idx as u32, idx as u32));
        }

        code.insert(0, Instruction::Allocate(variables.len() as u32));
        code.push(Instruction::Return);

        QueryDef { variables, code }
    }

} 