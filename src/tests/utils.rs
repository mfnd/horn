use crate::{vm::{PrologVM, Value, QueryError}, parser::{CFGNode, PrologParser}, ir_gen::IRGen};



pub fn consult_str(vm: &mut PrologVM, source: &str) {
    let file_node = PrologParser::new().parse_file(&source).expect("Couldn't parse");
    let mut ir_gen = IRGen::new();
    let module = ir_gen.generate(file_node);
    vm.load_module(module);
}

pub fn assert_results(vm: &mut PrologVM, query: &str, expected: &[&[Value]]) {
    vm.set_query_from_str(query).expect("Could not set query");
    let results: Result<Vec<Vec<Value>>, QueryError> = vm.collect();
    match results {
        Ok(values) => {
            assert_eq!(values, expected);
        }
        Err(err) => panic!("Error while collecting results: {:?}", err)
    }
}

pub fn assert_single_result(vm: &mut PrologVM, query: &str, expected: &[Value]) {
    assert_results(vm, query, &[expected]);
}


#[macro_export]
macro_rules! run_collect_assert_single {
    ($($test_name:ident: ($kb:expr, $query:expr, $expected: expr),)*) => {
    $(
        #[test]
        fn $test_name() {
            let mut runtime = PrologVM::new();
            consult_str(&mut runtime, $kb);
            assert_single_result(
                &mut runtime, 
                $query, 
                $expected
            );
        }
    )*
    }
}

#[macro_export]
macro_rules! run_collect_assert {
    ($($test_name:ident: ($kb:expr, $query:expr, $expected: expr),)*) => {
    $(
        #[test]
        fn $test_name() {
            let mut runtime = PrologVM::new();
            consult_str(&mut runtime, $kb);
            assert_results(
                &mut runtime, 
                $query, 
                $expected
            );
        }
    )*
    }
}