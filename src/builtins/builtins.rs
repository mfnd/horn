use std::collections::HashMap;
use std::{fs, fmt};

use crate::debugln;
use crate::ir_gen::IRGen;
use crate::vm::{Value, PrologVM};
use crate::parser::{PrologParser, PrologRule, CFGNode};

pub type BuiltIn = fn(&mut PrologVM) -> bool;

pub fn consult(vm: &mut PrologVM) -> bool {
    if let Value::Str(file_path) = vm.read_register(0) {
        let file = fs::read_to_string(&*file_path).unwrap();
        let file_node = CFGNode::parse_file(&file).expect("Couldn't parse");
        let mut ir_gen = IRGen::new();
        let module = ir_gen.generate(file_node);
        vm.load_module(module);
        return true;
    }
    false
}

pub fn is(vm: &mut PrologVM) -> bool {
    let value = vm.read_register(0);
    let expr = vm.read_register(1);
    let result = vm.eval_arithmetic(&expr);
    
    debugln!("Evaluating {:?} {:?}", value, expr);

    vm.unify(value, result)
}

lazy_static! {
    pub static ref BUILTINS: HashMap<(&'static str, u32), BuiltIn> = {
        let mut builtins : HashMap<(&'static str, u32), BuiltIn> = HashMap::new();
        builtins.insert(("consult", 1), consult);
        builtins.insert(("is", 2), is);
        builtins
    };
}