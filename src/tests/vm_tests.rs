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
        map_plus_one([H | T], [OutH | OutT]) :- OutH is H + 1, map_plus_one(T, OutT).

        map_plus_ten([], []).
        map_plus_ten([H | T], [OutH | OutT]) :- OutH is H + 10, map_plus_ten(T, OutT).

        map_minus_one([], []).
        map_minus_one([H | T], [OutH | OutT]) :- OutH is H - 1, map_minus_one(T, OutT).

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

run_collect_assert! {
    cut_test_1: (
        r#"
        alt(1, 3).
        alt(1, 5).
        alt(2, 4).
        alt(5, 9).
        alt(1, 7).

        select_one(X, Y) :- alt(X, Y), !.

        "#,
        "select_one(1, Res).",
        &[
            &[Value::Int(3)],
        ]
    ),

    cut_test_2: (
        r#"
        a(1).
        a(2).
        a(3).

        b(1, 10).
        b(1, 11).
        b(2, 3).
        b(2, 4).
        b(3, 5).

        compound(X, Y) :- a(X), !, b(X, Y).

        "#,
        "compound(X, Y).",
        &[
            &[Value::Int(1), Value::Int(10)], 
            &[Value::Int(1), Value::Int(11)],
        ]
    ),

    cut_test_max_1: (
        r#"
        max1(X, Y, X) :- X > Y, !.
        max1(X, Y, Y).
        "#,
        "max1(5, 4, Res).",
        &[
            &[Value::Int(5)], 
        ]
    ),

    cut_test_max_2: (
        r#"
        max1(X, Y, X) :- X > Y, !.
        max1(X, Y, Y).
        "#,
        "max1(4, 5, Res).",
        &[
            &[Value::Int(5)], 
        ]
    ),
}


run_collect_assert! {
    retract_test_1: (
        r#"
        fact(1, 3).
        fact(2, 4).
        fact(1, 5).
        fact(2, 6).

        retract_test(X, Y) :- retract(fact(1, I)), fact(X, Y).

        "#,
        "retract_test(X, Y).",
        &[
            &[Value::Int(2), Value::Int(4)],
            &[Value::Int(1), Value::Int(5)],
            &[Value::Int(2), Value::Int(6)]
        ]
    ),
    retract_test_2: (
        r#"
        fact(1, 3).
        fact(2, 4).
        fact(1, 5).
        fact(2, 6).

        retract_test(X, Y) :- 
            retract(fact(1, I)), 
            retract(fact(1, I)),
            fact(X, Y).

        "#,
        "retract_test(X, Y).",
        &[
            &[Value::Int(2), Value::Int(4)],
            &[Value::Int(2), Value::Int(6)]
        ]
    ),
    retractall_test_1: (
        r#"
        fact(1, 3).
        fact(2, 4).
        fact(1, 5).
        fact(2, 6).

        retract_test(X, Y) :- retractall(fact(1, I)), fact(X, Y).

        "#,
        "retract_test(X, Y).",
        &[
            &[Value::Int(2), Value::Int(4)],
            &[Value::Int(2), Value::Int(6)]
        ]
    ),

    assertz_test_1: (
        r#"
        fact(1, 3).

        assertz_test(X, Y) :- assertz(fact(2, 4)), fact(X, Y).

        "#,
        "assertz_test(X, Y).",
        &[
            &[Value::Int(1), Value::Int(3)],
            &[Value::Int(2), Value::Int(4)]
        ]
    ),

    assertz_test_2: (
        r#"
        fact(1, 3).

        assertz_test(Added, X, Y) :- assertz(fact(2, Added)), fact(X, Y).

        "#,
        "assertz_test(6, X, Y).",
        &[
            &[Value::Int(1), Value::Int(3)],
            &[Value::Int(2), Value::Int(6)]
        ]
    ),

    assertz_test_3: (
        r#"
        fact(1, 3).

        assertz_test(X, Y) :- assertz(fact(Z, Z)), fact(X, Y).

        "#,
        "assertz_test(1, Y).",
        &[
            &[Value::Int(3)],
            &[Value::Int(1)]
        ]
    ),
}