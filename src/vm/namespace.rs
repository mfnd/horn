use std::{collections::{HashMap, hash_map::Entry}, rc::Rc, cell::RefCell};

use crate::{ir_gen::{PredicateDef, Module}, prelude::BuiltIn, debugln, vm::CodeBlock};

use super::{Instruction, Value, Rule, NativePredicate, List, Struct};



#[derive(Debug)]
pub struct Namespace {
    pub atoms: Vec<String>,
    pub atoms_by_symbol: HashMap<String, usize>,
    pub predicates: HashMap<(usize, u32), Rc<Rule>>,
    pub native_predicates: HashMap<(usize, u32), Rc<NativePredicate>>,
}

impl Namespace {

    pub fn new() -> Self {
        Namespace {
            atoms: Vec::new(),
            atoms_by_symbol: HashMap::new(),
            predicates: HashMap::new(),
            native_predicates: HashMap::new()
        }
    }

    pub fn get_or_create_atom(&mut self, val: &str) -> usize {
        let next_idx = self.atoms.len();
        match self.atoms_by_symbol.get(val) {
            Some(atom) => *atom,
            None => {
                self.atoms.push(String::from(val));
                self.atoms_by_symbol.insert(String::from(val), next_idx);
                next_idx
            }
        }
    }

    pub fn get_atom_symbol(&self, atom: usize) -> Option<&str> {
        self.atoms.get(atom).map(|v| v.as_str())
    }

    pub fn atom_count(&self) -> usize {
        self.atoms.len()
    }

    pub fn load_module(&mut self, module: Module) {
        debugln!("{:#?}", module);
        let atom_mapping: Vec<usize> = module.namespace.atoms.iter().map(|s| self.get_or_create_atom(s)).collect();
        for mut pred in module.predicates {
            let mapped_functor = atom_mapping[pred.functor];
            let rule_name = (mapped_functor, pred.arity as u32);

            //self.link_code(&mut pred.code, &atom_mapping);
            self.link_predicate(&mut pred, &atom_mapping);

            match self.predicates.entry(rule_name) {
                Entry::Occupied(e) => {
                    let mut alternates = e.get().code.borrow_mut();
                    alternates.push(CodeBlock::from(pred))
                }
                Entry::Vacant(e) => {
                    let rule = Rc::from(
                        Rule {
                            functor: pred.functor,
                            arity: pred.arity as u32,
                            code: RefCell::from(vec![CodeBlock::from(pred)]),
                        }
                    );
                    e.insert(rule);
                }
            }
        }
    }

    pub fn link_predicate(&mut self, pred: &mut PredicateDef, atom_mapping: &[usize]) {
        self.link_code(&mut pred.code, atom_mapping);
        pred.head = self.link_value(&mut pred.head, atom_mapping);
        pred.body = pred.body.iter().map(|v| self.link_value(v, atom_mapping)).collect();
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

    pub fn link_value(&mut self, value: &Value, atom_mapping: &[usize]) -> Value {
        match value {
            Value::Atom(atom) => {
                let mapped_atom = atom_mapping[*atom];
                Value::Atom(mapped_atom)
            }
            Value::Struct(structure) => {
                let mapped_functor = atom_mapping[structure.functor];
                let params: Vec<Value> = structure.params
                    .iter()
                    .map(|v| self.link_value(v, atom_mapping))
                    .collect();
                Value::Struct(Rc::from(Struct {
                    functor: mapped_functor,
                    params
                }))
            }
            Value::List(list) => Value::List(self.link_list_value(list, atom_mapping)),
            _ => value.clone()
        }
    }

    fn link_list_value(&mut self, list: &List, atom_mapping: &[usize]) -> List {
        match list {
            List::Nil => List::Nil,
            List::Cons(node) => {
                List::cons(
                    self.link_value(&node.head, atom_mapping), 
                    self.link_list_value(&node.tail, atom_mapping)
                )
            }
            List::Ref(value_cell) => List::Ref(value_cell.clone())
        }
    }

    pub fn get_native(&mut self, functor: usize, arity: u32) -> Option<Rc<NativePredicate>> {
        self.native_predicates.get(&(functor, arity)).cloned()
    }

    pub fn create_native(&mut self, functor: usize, arity: u32, function_ptr: BuiltIn) {
        self.native_predicates.insert((functor, arity), Rc::from(
            NativePredicate {
                functor: String::from(self.get_atom_symbol(functor).unwrap()),
                arity: arity,               
                function_ptr: function_ptr
            }
        ));
    }

    pub fn get_or_create_predicate(&mut self, functor: usize, arity: u32) -> Rc<Rule> {
        let name = (functor, arity);
        self.predicates.entry(name).or_insert_with(|| 
            Rc::from(
                Rule {
                    functor,
                    arity,
                    code: RefCell::from(vec![]),
                }
            )
        ).clone()
    }

    pub fn get_predicate(&mut self, functor: usize, arity: u32) -> Option<Rc<Rule>> {
        let name = (functor, arity);
        self.predicates.get(&name).cloned()
    }

}