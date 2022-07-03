use std::collections::HashMap;

use pest::{iterators::Pair, Parser};

use crate::vm::List;

#[derive(Parser)]
#[grammar = "prolog.pest"]
pub struct PrologParser;

pub type PrologRule = Rule;

#[derive(Debug)]
pub enum CFGNode {
    ListTail(String),
    Term(Term),
    Fact(Structure),
    Rule(Structure, Vec<Term>),
    Query(Vec<Term>),
    File(Vec<CFGNode>),
    EOF
}

impl CFGNode {

    pub fn parse_file(file_content: &str) -> Option<Self> {
        let file = PrologParser::parse(PrologRule::file, file_content).unwrap().next().unwrap();
        Self::from(file)
    }

    pub fn parse_expression(expr: &str) -> Option<Self> {
        let expr_pair = PrologParser::parse(PrologRule::infix_expression, expr).unwrap().next().unwrap();
        Self::from(expr_pair)        
    }

    pub fn parse_basic_term(expr: &str) -> Option<Self> {
        let expr_pair = PrologParser::parse(PrologRule::basic_term, expr).unwrap().next().unwrap();
        Self::from(expr_pair)        
    }

    pub fn parse_debug(file_content: &str) -> Option<Self> {
        let file = PrologParser::parse(PrologRule::file, file_content).unwrap().next().unwrap();
        println!("{:?}", file);
        Self::from(file)
    }

    pub fn parse_query(query_str: &str) -> Option<Self> {
        let query = PrologParser::parse(PrologRule::query, query_str).unwrap().next().unwrap();
        Self::from(query)
    }

    pub fn from(pair: Pair<Rule>) -> Option<Self> {
        let node= match pair.as_rule() {
            Rule::file => {
                let mut results: Vec<CFGNode> = Vec::new();
                for p in pair.into_inner() {
                    let node = CFGNode::from(p).unwrap();
                    if let CFGNode::EOF = node {
                        break;
                    } else {
                        results.push(node);
                    }
                }
                CFGNode::File(results)
            }
            Rule::number => {
                let val: i64 = pair.as_str().parse().unwrap();
                CFGNode::Term(Term::Number(val))
            },
            Rule::string => {
                let val: String = String::from(pair.into_inner().next()?.as_str());
                CFGNode::Term(Term::String(val))
            }
            Rule::atom => {
                let name = String::from(pair.as_str());
                CFGNode::Term(Term::Atom(name))
            },
            Rule::variable => {
                let variable = String::from(pair.as_str());
                CFGNode::Term(Term::Variable(variable))
            },
            Rule::structure => {
                let mut pairs = pair.into_inner();
                let functor_pair = pairs.next()?;
                let functor = match functor_pair.as_rule() {
                    Rule::atom => String::from(functor_pair.as_str()),
                    _ => return None,
                };
                let mut params: Vec<Term> = Vec::new();
                for p in pairs {
                    if let CFGNode::Term(term) = CFGNode::from(p)? {
                        params.push(term);
                    } else {
                        return None
                    }
                }
                CFGNode::Term(Term::Structure(
                    Structure {
                        functor,
                        params
                    }
                ))
                
            },
            Rule::list => {
                let mut members: Vec<Term> = Vec::new();
                let mut tail = None;
                for p in pair.into_inner() {
                    match CFGNode::from(p)? {
                        CFGNode::Term(term) => members.push(term),
                        CFGNode::ListTail(term) => tail = Some(Box::from(Term::Variable(term))),
                        _ => unreachable!()
                    }
                }
                CFGNode::Term(Term::List(
                    ListExpr {
                        heads: members, 
                        tail: tail
                    }
                ))
            }
            Rule::list_tail => {
                let variable = CFGNode::from(pair.into_inner().next()?)?;
                if let CFGNode::Term(Term::Variable(var)) = variable {
                    CFGNode::ListTail(var)
                } else{
                    return None
                }
            }
            Rule::infix_expression => {
                let pairs = pair.into_inner();
                let mut terms: Vec<Term> = Vec::new();
                for pair in pairs {
                    let inner = CFGNode::from(pair)?;
                    if let CFGNode::Term(term) = inner {
                        terms.push(term);
                    } else {
                        return None
                    }
                }
                CFGNode::Term(Term::InfixExpr(InfixExpr { terms }))
            }
            Rule::basic_term => CFGNode::from(pair.into_inner().next()?)?,
            Rule::fact => {
                let inner = CFGNode::from(pair.into_inner().next()?)?;
                if let CFGNode::Term(Term::Structure(term)) = inner {
                    CFGNode::Fact(term)
                } else {
                    return None
                }
            }
            Rule::rule => {
                let mut pairs = pair.into_inner();
                let head_node = CFGNode::from(pairs.next()?)?;
                let head = match head_node {
                    CFGNode::Term(Term::Structure(h)) => h,
                    _ => return None
                };

                let mut body: Vec<Term> = Vec::new();
                for p in pairs {
                    if let CFGNode::Term(term) = CFGNode::from(p)? {
                        body.push(term);
                    } else {
                        return None
                    }
                }
                CFGNode::Rule(head, body)
            },
            Rule::query => {
                let mut terms: Vec<Term> = Vec::new();
                for p in pair.into_inner() {
                    if let CFGNode::Term(term) = CFGNode::from(p)? {
                        terms.push(term);
                    } else {
                        return None
                    }
                }
                CFGNode::Query(terms)
            },
            Rule::EOI => CFGNode::EOF,
            r => panic!("Not implemented {:?}", r)
        };
        Some(node)
    }
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Number(i64),
    Atom(String),
    String(String),
    Variable(String),
    Structure(Structure),
    List(ListExpr),
    InfixExpr(InfixExpr)
}

#[derive(Debug, PartialEq)]
pub struct Structure {
    pub functor: String,
    pub params: Vec<Term>
}

#[derive(Debug, PartialEq)]
pub struct ListExpr {
    pub heads: Vec<Term>,
    pub tail: Option<Box<Term>>
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OperatorType {
    XFX,
    XFY,
    YFX,
    FX,
    FY,
    XF,
    YF
}

#[derive(Debug, Clone)]
pub struct Operator {
    atom: String,
    precedence: u32,
    op_type: OperatorType
}

impl Operator {

    fn new(atom: &str, precedence: u32, op_type: OperatorType) -> Self {
        Operator { atom: String::from(atom), precedence, op_type }
    }

    fn arity(&self) -> usize {
        match self.op_type {
            OperatorType::YFX | OperatorType::XFY | OperatorType::XFX => 2,
            _ => 1
        }
    }

}

pub type PrecedenceMap<'a> = HashMap<&'a str, Operator>;

lazy_static! {

    pub static ref DEFAULT_PRECEDENCES: PrecedenceMap<'static> = PrecedenceMap::from(
        [
            ("\\+", Operator::new("\\+", 900, OperatorType::FY)),
            ("is", Operator::new("is", 700, OperatorType::XFX)),
            ("+", Operator::new("+", 500, OperatorType::YFX)),
            ("-", Operator::new("-", 500, OperatorType::YFX)),
            ("*", Operator::new("*", 400, OperatorType::YFX)),
            ("/", Operator::new("/", 400, OperatorType::YFX)),
            ("^", Operator::new("^", 200, OperatorType::XFY)),
            ("^", Operator::new("^", 200, OperatorType::XFY)),
        ]
    );
}

#[derive(Debug, PartialEq)]
pub struct InfixExpr {
    terms: Vec<Term>
}

impl InfixExpr {

    pub fn parse_with_defaults(self) -> Term {
        self.parse(&DEFAULT_PRECEDENCES)
    }

    pub fn parse(self, precedences: &PrecedenceMap) -> Term {
        let mut stack: Vec<Term> = Vec::new();
        let mut operator_stack: Vec<Operator> = Vec::new();
        for term in self.terms {
            match term {
                Term::Atom(atom) => {
                    println!("Atom: {}", atom);
                    if let Some(new_op) = precedences.get(atom.as_str()) {
                        while let Some(top_op) = operator_stack.last() {
                            if new_op.precedence > top_op.precedence 
                                || (new_op.op_type == OperatorType::YFX && new_op.precedence == top_op.precedence) {
                                let operator = operator_stack.pop().unwrap();
                                let mut params: Vec<Term> = Vec::new();
                                for _ in 0..operator.arity() {
                                    let operand = stack.pop().unwrap();
                                    params.insert(0, operand);
                                }
                                stack.push(Term::Structure(Structure {
                                    functor: operator.atom,
                                    params: params
                                }));
                            } else {
                                break;
                            }
                        }
                        operator_stack.push(new_op.clone());
                    }
                    else {
                        panic!("Undefined operator");
                    }
                }
                other => {
                    stack.push(other);
                }
            }
        }

        while let Some(operator) = operator_stack.pop() {
            println!("Top atom: {:?}", operator);
            let mut params: Vec<Term> = Vec::new();
            for _ in 0..operator.arity() {
                let operand = stack.pop().unwrap();
                params.insert(0, operand);
            }
            stack.push(Term::Structure(Structure {
                functor: operator.atom,
                params: params
            }));
        }

        assert!(stack.len() == 1);
        stack.pop().unwrap()
    }

}