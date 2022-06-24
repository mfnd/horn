use crate::{vm::{PrologVM, Value, List}, run_collect_assert, run_collect_assert_single};

use super::utils::{consult_str, assert_results, assert_single_result};

run_collect_assert_single! {
    first_two: (
        "first_two([X, Y | Z], X, Y, Z).", 
        "first_two([3, 1, 2, 4, 6], A, B, C).", 
        &[
            Value::Int(3),
            Value::Int(1),
            Value::List(List::from(vec![Value::Int(2), Value::Int(4), Value::Int(6)]))
        ]
    ),
    last_element: (
        r#"
        last([E], E).
        last([H | T], E) :- last(T, E).
        "#,
        "last([1, 2, 3, 4, 5], E).",
        &[Value::Int(5)]
    ),
    map_plus_one: (
        r#"
        map_plus_one([], []).
        map_plus_one([H | T], [OutH | OutT]) :- is(OutH, +(H, 1)), map_plus_one(T, OutT).
        "#,
        "map_plus_one([3, 1, 10, 2, 0], Mapped).",
        &[Value::List(List::from(vec![Value::Int(4), Value::Int(2), Value::Int(11), Value::Int(3), Value::Int(1)]))]
    ),
}

run_collect_assert! {
    map_multiple: (
        r#"
        map_plus_one([], []).
        map_plus_one([H | T], [OutH | OutT]) :- is(OutH, +(H, 1)), map_plus_one(T, OutT).

        map_plus_ten([], []).
        map_plus_ten([H | T], [OutH | OutT]) :- is(OutH, +(H, 10)), map_plus_ten(T, OutT).

        map_minus_one([], []).
        map_minus_one([H | T], [OutH | OutT]) :- is(OutH, -(H, 1)), map_minus_one(T, OutT).

        map_multiple(I, O) :- map_plus_one(I, O).
        map_multiple(I, O) :- map_plus_ten(I, O).
        map_multiple(I, O) :- map_minus_one(I, O).
        "#,
        "map_multiple([3, 1, 10, 2, 0], Mapped).",
        &[
            &[Value::List(List::from(vec![Value::Int(4), Value::Int(2), Value::Int(11), Value::Int(3), Value::Int(1)]))],
            &[Value::List(List::from(vec![Value::Int(13), Value::Int(11), Value::Int(20), Value::Int(12), Value::Int(10)]))],
            &[Value::List(List::from(vec![Value::Int(2), Value::Int(0), Value::Int(9), Value::Int(1), Value::Int(-1)]))]
        ]
    ),
}