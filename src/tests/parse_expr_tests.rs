use crate::parser::{CFGNode, Term};



#[macro_export]
macro_rules! parse_and_check_canonical_equivalence {
    ($($test_name:ident: ($infix:expr, $canonical:expr),)*) => {
    $(
        #[test]
        fn $test_name() {
            let expr = if let CFGNode::Term(Term::InfixExpr(expr)) = CFGNode::parse_expression($infix).unwrap() {
                expr
            } else {
                panic!("Could not parse expression into InfixExpr terms")
            };
            let canonical = if let CFGNode::Term(term) = CFGNode::parse_basic_term($canonical).unwrap() {
                term
            } else {
                panic!("Could not parse expression into a term")
            };
        
            let parsed_expr = expr.parse_with_defaults();
            assert_eq!(parsed_expr, canonical);
        }
    )*
    }
}

parse_and_check_canonical_equivalence! {
    simple_1: ("2 + 3 * 4", "+(2, *(3, 4))"),
    simple_2: ("6 + 3 * 2 + 5 - 7 * 4", "-(+(+(6,*(3, 2)), 5),*(7,4))))"),
    left_and_right_assoc_mixed_1: ("2 * 3 ^ 2 * 2 ^ 2", "*(*(2, ^(3, 2)), ^(2, 2))"),
}