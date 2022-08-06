use crate::parser::{PrologParser, Term, PrecedenceMap, Operator, PrefixType, InfixType, OperatorInfo, PostfixType, DEFAULT_PRECEDENCES};



#[macro_export]
macro_rules! parse_and_check_canonical_equivalence {
    ($($test_name:ident: ($infix:expr, $canonical:expr),)*) => {
    $(
        #[test]
        fn $test_name() {
            let parser = PrologParser::new();
            let expr = parser.parse_expression($infix).unwrap();
            let canonical = parser.parse_expression($canonical).unwrap();
            assert_eq!(expr, canonical);
        }
    )*
    }
}

#[macro_export]
macro_rules! parse_and_check_canonical_equivalence_custom_operators {
    ($($test_name:ident: ($infix:expr, $canonical:expr, $operators:expr), )*) => {
    $(
        #[test]
        fn $test_name() {
            let parser = PrologParser::with_operators(&TEST_PRECEDENCES);
            let expr = parser.parse_expression($infix).unwrap();
            let canonical = parser.parse_expression($canonical).unwrap();
            assert_eq!(expr, canonical);
        }
    )*
    }
}

lazy_static! {
    pub static ref TEST_PRECEDENCES: PrecedenceMap = {
        let mut test_precedences = DEFAULT_PRECEDENCES.clone();
        test_precedences.insert(String::from("++"), Operator::new_postfix("++", 100, PostfixType::YF));
        test_precedences
    };
}

parse_and_check_canonical_equivalence! {
    simple_1: ("2 + 3 * 4", "+(2, *(3, 4))"),
    simple_2: ("6 + 3 * 2 + 5 - 7 * 4", "-(+(+(6,*(3, 2)), 5),*(7,4))"),
    left_and_right_assoc_mixed_1: ("2 * 3 ^ 2 * 2 ^ 2", "*(*(2, ^(3, 2)), ^(2, 2))"),
    negate_1: ("\\+ X is 3 + 4", "\\+(is(X,+(3,4)))"),
    negate_2: ("\\+ X is (3 + 4)", "\\+(is(X,+(3,4)))"),
    unary_functor_1: ("X is +(2)", "is(X,+(2))"),
    comparison_1: ("2*(X+3) == 3-5*4+Y", "==(*(2, +(X,3)), +(-(3, *(5,4)), Y))"),
}


parse_and_check_canonical_equivalence_custom_operators! {
    postfix_1: ("2 - 4++", "-(2,++(4))", TEST_PRECEDENCES),
    postfix_2: ("+(2- 3)++ * 4", "*(++(+(-(2,3))), 4)", TEST_PRECEDENCES),
    postfix_3: ("\\+ X is Y++", "\\+(is(X,++(Y)))", TEST_PRECEDENCES),
}