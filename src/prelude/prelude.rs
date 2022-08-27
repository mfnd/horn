use std::collections::HashMap;
use std::{fs, fmt};

use crate::debugln;
use crate::ir_gen::IRGen;
use crate::vm::{Value, PrologVM, RuntimeResult, unify, Trail, unify_ref};
use crate::parser::{PestPrologParser, PrologRule, CFGNode, PrologParser};

pub type BuiltIn = fn(&mut PrologVM) -> RuntimeResult<bool>;

pub fn consult(vm: &mut PrologVM) -> RuntimeResult<bool> {
    if let Value::Str(file_path) = vm.read_register(0) {
        let file = fs::read_to_string(&*file_path).unwrap();
        let file_node = PrologParser::new().parse_file(&file).expect("Couldn't parse");
        let mut ir_gen = IRGen::new();
        let module = ir_gen.generate(file_node);
        vm.load_module(module);
        return Ok(true);
    }
    Ok(false)
}

pub fn is(vm: &mut PrologVM) -> RuntimeResult<bool> {
    let value = vm.read_register(0);
    let expr = vm.read_register(1);
    let result = vm.eval_arithmetic(&expr)?;
    
    debugln!("Evaluating {:?} {:?}", value, expr);

    Ok(vm.unify(value, result))
}

fn assertz(vm: &mut PrologVM) -> RuntimeResult<bool> {
    let predicate_struct = vm.read_register(0);
    let (functor, arity) = match &predicate_struct {
        Value::Struct(s) => (s.functor, s.params.len() as u32),
        _ => todo!()
    };

    let mut ir_gen = IRGen::new();
    let predicate_def = ir_gen.generate_fact_from_value(predicate_struct);
    vm.load_predicate(predicate_def);

    Ok(true)
}


#[derive(PartialEq)]
enum RetractBehavior {
    First,
    All
}

fn retract_with_behavior(vm: &mut PrologVM, behavior: RetractBehavior) -> RuntimeResult<bool> {
    let predicate_struct = vm.read_register(0);
    let (functor, arity) = match &predicate_struct {
        Value::Struct(s) => (s.functor, s.params.len() as u32),
        _ => todo!()
    };

    let mut removed_any = false;

    if let Some(predicate) = vm.get_predicate(functor, arity) {
        predicate.code.borrow_mut().retain(|alternate| {
            if behavior == RetractBehavior::First && removed_any {
                return true
            }
            let mut trail = Trail::new();
            let res = unify_ref(&alternate.head, &predicate_struct, &mut trail);
            trail.clear();
            removed_any |= res;
            !res
        });
    };

    Ok(removed_any)
}

pub fn retract(vm: &mut PrologVM) -> RuntimeResult<bool> {
    retract_with_behavior(vm, RetractBehavior::First)
}

pub fn retractall(vm: &mut PrologVM) -> RuntimeResult<bool> {
    retract_with_behavior(vm, RetractBehavior::All)
}

lazy_static! {
    pub static ref BUILTINS: HashMap<(&'static str, u32), BuiltIn> = {
        let mut builtins : HashMap<(&'static str, u32), BuiltIn> = HashMap::new();
        builtins.insert(("consult", 1), consult);
        builtins.insert(("is", 2), is);
        builtins.insert(("retractall", 1), retractall);
        builtins.insert(("retract", 1), retract);
        builtins.insert(("assertz", 1), assertz);
        builtins
    };
}