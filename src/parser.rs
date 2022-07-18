use std::{collections::HashMap};

use pest::{iterators::Pair, Parser};

use crate::vm::List;

#[derive(Parser)]
#[grammar = "prolog.pest"]
pub struct PrologParser;

pub type PrologRule = Rule;

#[derive(Debug, PartialEq)]
pub enum CFGNode {
    ListTail(String),
    Term(Term),
    Expr(Term),
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
        let expr_pair = PrologParser::parse(PrologRule::expression, expr).unwrap().next().unwrap();
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

    pub fn parse_debug_terms(file_content: &str) {
        let file = PrologParser::parse(PrologRule::terms, file_content).unwrap().next().unwrap();
        println!("Parsed {:#?}", file);
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
            Rule::expression => {
                let mut shuntyard_parser = ShuntyardParser::new();
                CFGNode::Term(shuntyard_parser.parse(pair)?)
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

    fn apply_shuntyard(pair: Pair<Rule>) -> Option<Term> {
        let pairs = pair.into_inner();

        let mut operand_stack: Vec<Term> = Vec::new();
        let mut operator_stack: Vec<ShuntyardOperator> = Vec::new();
        let mut args_stack: Vec<Term> = Vec::new();
        let mut args_mode = false;

        for (idx, pair) in pairs.enumerate() {
            if pair.as_str() == "(" {
                operator_stack.push(ShuntyardOperator::LeftPar);
                println!("LPAR found");
            }
            else if pair.as_str() == "," {
                operator_stack.push(ShuntyardOperator::Comma);
                println!("COMMA found");
            } 
            else if pair.as_str() == ")" {
                println!("RPAR found");
                let mut left_par_found = false;
                while let Some(top_op) = operator_stack.pop() {
                    match top_op {
                        ShuntyardOperator::LeftPar => {
                            left_par_found = true;
                            break
                        }
                        ShuntyardOperator::Comma => {
                            if !args_mode {
                                args_stack.push(operand_stack.pop().unwrap());
                                args_mode = true;
                            }
                            args_stack.push(operand_stack.pop().unwrap());
                        }
                        ShuntyardOperator::Operator(next_op) => {
                            let mut params: Vec<Term> = Vec::new();
                            for _ in 0..next_op.arity() {
                                let operand = operand_stack.pop().unwrap();
                                params.insert(0, operand);
                            }
                            operand_stack.push(Term::Structure(Structure {
                                functor: next_op.atom,
                                params: params
                            }));
                        }
                    }
                }
                assert!(left_par_found);
                if args_mode {
                    if let Term::Atom(functor) = operand_stack.pop().unwrap() {
                        operand_stack.push(Term::Structure(Structure {
                            functor: functor,
                            params: std::mem::replace(&mut args_stack, Vec::new()).into_iter().rev().collect()
                        }));
                    }
                    println!("Args: {:?}", args_stack);
                    args_mode = false;
                }
            } else {
                println!("{:?}", pair);
                let inner = CFGNode::from(pair)?;
                if let CFGNode::Term(term) = inner {
                    match term {
                        Term::Atom(atom) => {
                            println!("Atom: {}", atom);
                            if let Some(new_op) = DEFAULT_PRECEDENCES.get(atom.as_str()) {
                                while let Some(ShuntyardOperator::Operator(top_op)) = operator_stack.last() {
                                    if new_op.precedence > top_op.precedence 
                                        || (new_op.op_type == OperatorType::YFX && new_op.precedence == top_op.precedence) {
                                        let operator = match operator_stack.pop()? {
                                            ShuntyardOperator::Operator(op) => op,
                                            _ => return None
                                        };
                                        let mut params: Vec<Term> = Vec::new();
                                        for _ in 0..operator.arity() {
                                            let operand = operand_stack.pop().unwrap();
                                            params.insert(0, operand);
                                        }
                                        operand_stack.push(Term::Structure(Structure {
                                            functor: operator.atom,
                                            params: params
                                        }));
                                    } else {
                                        break;
                                    }
                                }
                                operator_stack.push(ShuntyardOperator::Operator(new_op.clone()));
                            }
                            else {
                                operand_stack.push(Term::Atom(atom));
                            }
                        }
                        other => {
                            operand_stack.push(other);
                        }
                    }
                } else {
                    return None
                }
            }
        }

        while let Some(ShuntyardOperator::Operator(operator)) = operator_stack.pop() {
            println!("Top atom: {:?}", operator);
            let mut params: Vec<Term> = Vec::new();
            for _ in 0..operator.arity() {
                let operand = operand_stack.pop().unwrap();
                params.insert(0, operand);
            }
            operand_stack.push(Term::Structure(Structure {
                functor: operator.atom,
                params: params
            }));
        }

        assert!(operand_stack.len() == 1);
        assert!(operator_stack.len() == 0);
        operand_stack.pop()
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


#[derive(Debug)]
enum ShuntyardOperator {
    LeftPar,
    Comma,
    Operator(Operator),
}


struct ShuntyardParser {
    operand_stack : Vec<(Term, usize)>,
    operator_stack: Vec<(ShuntyardOperator, usize)>,
    pos: usize
}

impl ShuntyardParser {

    pub fn new() -> Self {
        ShuntyardParser { 
            operand_stack: Vec::new(), 
            operator_stack: Vec::new(), 
            pos: 0
        }
    }

    fn push_operator(&mut self, operator: ShuntyardOperator) {
        self.operator_stack.push((operator, self.pos));
    }

    fn push_operand(&mut self, operand: Term) {
        self.operand_stack.push((operand, self.pos));
    }

    fn pop_operand(&mut self) -> Option<Term> {
        match self.operand_stack.pop() {
            Some((term, _)) => Some(term),
            None => None,
        }
    }

    fn pop_unary_functor(&mut self) -> Option<Term> {
        let latest_operand_pos: usize = match &self.operand_stack.last() {
            Some((Term::Atom(_), pos)) => *pos,
            _ => 0,
        };
        let latest_operator_pos: usize = match &self.operator_stack.last() {
            Some((_, pos)) => *pos,
            None => 0
        };
        if latest_operand_pos > latest_operator_pos {
            self.pop_operand()
        } else {
            None
        }
    }

    fn pop_argument(&mut self) -> Term {
        let latest_operand_pos: usize = match &self.operand_stack.last() {
            Some((_, pos)) => *pos,
            None => 0,
        };
        let latest_operator_pos: usize = match &self.operator_stack.last() {
            Some((_, pos)) => *pos,
            None => 0
        };
        if latest_operator_pos > latest_operand_pos {
            match self.operator_stack.pop().unwrap().0 {
                ShuntyardOperator::LeftPar => todo!(),
                ShuntyardOperator::Comma => todo!(),
                ShuntyardOperator::Operator(op) => Term::Atom(op.atom),
            }
        } else {
            self.operand_stack.pop().unwrap().0
        }
    } 

    pub fn parse(&mut self, pair: Pair<Rule>) -> Option<Term> {
        let pairs = pair.into_inner();

        let mut args_stack: Vec<Term> = Vec::new();
        let mut args_mode = false;

        for (idx, pair) in pairs.enumerate() {
            self.pos = idx + 1;
            if pair.as_str() == "(" {
                self.push_operator(ShuntyardOperator::LeftPar);
                //println!("LPAR found");
            }
            else if pair.as_str() == "," {
                self.push_operator(ShuntyardOperator::Comma);
                //println!("COMMA found");
            } 
            else if pair.as_str() == ")" {
                //println!("RPAR found");
                let mut left_par_found = false;
                while let Some((top_op, _)) = self.operator_stack.pop() {
                    match top_op {
                        ShuntyardOperator::LeftPar => {
                            left_par_found = true;
                            break
                        }
                        ShuntyardOperator::Comma => {
                            if !args_mode {
                                let arg = self.pop_argument();
                                args_stack.push(arg);
                                args_mode = true;
                            }
                            args_stack.push(self.pop_argument());
                        }
                        ShuntyardOperator::Operator(next_op) => {
                            let mut params: Vec<Term> = Vec::new();
                            for _ in 0..next_op.arity() {
                                let operand = self.pop_operand().unwrap();
                                params.insert(0, operand);
                            }
                            self.push_operand(Term::Structure(Structure {
                                functor: next_op.atom,
                                params: params
                            }));
                        }
                    }
                }
                assert!(left_par_found);
                if args_mode {
                    if let Term::Atom(functor) = self.pop_argument() {
                        self.push_operand(Term::Structure(Structure {
                            functor: functor,
                            params: std::mem::replace(&mut args_stack, Vec::new()).into_iter().rev().collect()
                        }));
                    } else {
                        panic!("Expected functor");
                    }
                    println!("Args: {:?}", args_stack);
                    args_mode = false;
                } else {
                    println!("Operators {:?} Operands {:?}", self.operator_stack, self.operand_stack);
                    let operand = self.pop_operand().unwrap();
                    match self.pop_unary_functor() {
                        Some(Term::Atom(functor)) => {
                            self.push_operand(Term::Structure(Structure {
                                functor: functor,
                                params: vec![operand]
                            }));
                        }
                        _ => self.push_operand(operand)
                    }
                }
            } else {
                println!("{:?}", pair);
                let inner = CFGNode::from(pair)?;
                if let CFGNode::Term(term) = inner {
                    match term {
                        Term::Atom(atom) => {
                            println!("Atom: {}", atom);
                            if let Some(new_op) = DEFAULT_PRECEDENCES.get(atom.as_str()) {
                                while let Some((ShuntyardOperator::Operator(top_op), _)) = self.operator_stack.last() {
                                    if new_op.precedence > top_op.precedence 
                                        || (new_op.op_type == OperatorType::YFX && new_op.precedence == top_op.precedence) {
                                        let operator = match self.operator_stack.pop()? {
                                            (ShuntyardOperator::Operator(op), _) => op,
                                            _ => return None
                                        };
                                        let mut params: Vec<Term> = Vec::new();
                                        for _ in 0..operator.arity() {
                                            let operand = self.pop_operand().unwrap();
                                            params.insert(0, operand);
                                        }
                                        self.push_operand(Term::Structure(Structure {
                                            functor: operator.atom,
                                            params: params
                                        }));
                                    } else {
                                        break;
                                    }
                                }
                                self.push_operator(ShuntyardOperator::Operator(new_op.clone()));
                            }
                            else {
                                self.push_operand(Term::Atom(atom));
                            }
                        }
                        other => {
                            self.push_operand(other);
                        }
                    }
                } else {
                    return None
                }
            }
        }

        while let Some((ShuntyardOperator::Operator(operator), _)) = self.operator_stack.pop() {
            println!("Top atom: {:?}", operator);
            let mut params: Vec<Term> = Vec::new();
            for _ in 0..operator.arity() {
                let operand = self.pop_operand().unwrap();
                params.insert(0, operand);
            }
            self.push_operand(Term::Structure(Structure {
                functor: operator.atom,
                params: params
            }));
        }

        println!("Operand Stack: {:?}", self.operand_stack);
        println!("Operator Stack: {:?}", self.operator_stack);

        assert!(self.operand_stack.len() == 1);
        assert!(self.operator_stack.len() == 0);

        self.pop_operand()
    }

}