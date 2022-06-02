use std::collections::HashMap;
use std::{fs, fmt};

use crate::ir_gen::IRGen;
use crate::vm::{Value, PrologVM};
use crate::parser::{PrologParser, PrologRule, CFGNode};

pub type BuiltIn = fn(&mut PrologVM) -> bool;

pub fn consult(vm: &mut PrologVM) -> bool {
    if let Value::Str(file_path) = vm.read_register(0) {
        println!("Consulting {}...", file_path);
        let file = fs::read_to_string(&file_path).unwrap();
        let file_node = CFGNode::parse_file(&file).expect("Couldn't parse");
        let ir_gen = IRGen {};
        let ir = ir_gen.generate(file_node);
        let rule_count = ir.len();
        for rule in ir {
            vm.load_rule(&rule.functor, rule.arity as u32, rule.code);
        }
        println!("{} rule(s) are loaded", rule_count);
        return true;
    }
    false
}

lazy_static! {
    pub static ref BUILTINS: HashMap<(&'static str, u32), BuiltIn> = {
        let mut builtins : HashMap<(&'static str, u32), BuiltIn> = HashMap::new();
        builtins.insert(("consult", 1), consult);
        builtins
    };
}

pub fn get_builtin(functor: &str, arity: u32) -> Option<BuiltIn> {
    BUILTINS.get(&(functor, arity)).copied()
}