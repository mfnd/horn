use std::{collections::HashMap, fs::OpenOptions, fmt::Write};

use pest::{iterators::Pair, Parser};

use crate::vm::List;

#[derive(Parser)]
#[grammar = "prolog.pest"]
pub struct PestPrologParser;

pub type PrologRule = Rule;


pub struct PrologParser {
    operators: PrecedenceMap
}

impl PrologParser {
    pub fn new() -> Self {
        PrologParser { 
            operators: DEFAULT_PRECEDENCES.clone() 
        }
    }

    pub fn with_operators(operators: &PrecedenceMap) -> Self {
        PrologParser { operators: operators.clone() }

    }

    pub fn add_operator(&mut self, operator: Operator) {
        self.operators.insert(operator.atom.clone(), operator);
    }

    pub fn parse_file(&self, file_content: &str) -> Option<CFGNode> {
        let file = PestPrologParser::parse(PrologRule::file, file_content).unwrap().next().unwrap();
        self.from(file)
    }

    pub fn parse_expression(&self, expr: &str) -> Option<CFGNode> {
        let expr_pair = PestPrologParser::parse(PrologRule::term_only, expr).unwrap().next().unwrap();
        self.from(expr_pair)        
    }

    
    pub fn parse_basic_term(&self, expr: &str) -> Option<CFGNode> {
        let expr_pair = PestPrologParser::parse(PrologRule::basic_term, expr).unwrap().next().unwrap();
        self.from(expr_pair)        
    }

    pub fn parse_debug(&self, file_content: &str) -> Option<CFGNode> {
        let file = PestPrologParser::parse(PrologRule::file, file_content).unwrap().next().unwrap();
        println!("{:?}", file);
        self.from(file)
    }

    pub fn parse_debug_terms(&self, file_content: &str) {
        let file = PestPrologParser::parse(PrologRule::terms, file_content).unwrap().next().unwrap();
        println!("Parsed {:#?}", file);
    }

    pub fn parse_query(&self, query_str: &str) -> Option<CFGNode> {
        let query = PestPrologParser::parse(PrologRule::query, query_str).unwrap().next().unwrap();
        self.from(query)
    }

    pub fn from(&self, pair: Pair<Rule>) -> Option<CFGNode> {
        let node= match pair.as_rule() {
            Rule::file => {
                let mut results: Vec<CFGNode> = Vec::new();
                for p in pair.into_inner() {
                    let node = self.from(p).unwrap();
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
                let struct_head = pairs.next()?;
                let functor = match struct_head.as_rule() {
                    Rule::struct_head => {
                        let atom_pair = struct_head.into_inner().next()?;  
                        match atom_pair.as_rule() {
                            Rule::atom => String::from(atom_pair.as_str()),
                            _ => return None
                        }
                    },
                    _ => return None,
                };
                let mut params: Vec<Term> = Vec::new();
                for p in pairs {
                    if let CFGNode::Term(term) = self.from(p)? {
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
                    match self.from(p)? {
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
                let variable = self.from(pair.into_inner().next()?)?;
                if let CFGNode::Term(Term::Variable(var)) = variable {
                    CFGNode::ListTail(var)
                } else{
                    return None
                }
            }
            Rule::paren_expr => {
                self.from(pair.into_inner().next()?)?
            }
            Rule::expression => {
                let mut shuntyard_parser = ShuntingyardParser::new();
                CFGNode::Term(shuntyard_parser.parse(pair, &self)?)
            }
            Rule::basic_term => self.from(pair.into_inner().next()?)?,
            Rule::fact => {
                let inner = self.from(pair.into_inner().next()?)?;
                if let CFGNode::Term(Term::Structure(term)) = inner {
                    CFGNode::Fact(term)
                } else {
                    return None
                }
            }
            Rule::rule => {
                let mut pairs = pair.into_inner();
                let head_node = self.from(pairs.next()?)?;
                let head = match head_node {
                    CFGNode::Term(Term::Structure(h)) => h,
                    _ => return None
                };

                let mut body: Vec<Term> = Vec::new();
                for p in pairs {
                    if let CFGNode::Term(term) = self.from(p)? {
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
                    if let CFGNode::Term(term) = self.from(p)? {
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
pub enum CFGNode {
    ListTail(String),
    Term(Term),
    Fact(Structure),
    Rule(Structure, Vec<Term>),
    Query(Vec<Term>),
    File(Vec<CFGNode>),
    EOF
}

#[derive(PartialEq, Clone)]
pub enum Term {
    Number(i64),
    Atom(String),
    String(String),
    Variable(String),
    Structure(Structure),
    List(ListExpr),
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Number(num) => num.fmt(f),
            Term::Atom(atom) => atom.fmt(f),
            Term::String(string) => string.fmt(f),
            Term::Variable(var) => var.fmt(f),
            Term::Structure(structure) => structure.fmt(f),
            Term::List(list_expr) => list_expr.fmt(f),
        }
    }
}


#[derive(PartialEq, Clone)]
pub struct Structure {
    pub functor: String,
    pub params: Vec<Term>
}

impl std::fmt::Debug for Structure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.functor)?;
        f.write_char('(')?;
        let mut started = false;
        for param in &self.params {
            if started {
                f.write_str(" ,")?;
            } else {
                started = true;
            }
            param.fmt(f)?;
        }
        f.write_char(')')?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Clone, Copy)]
pub enum InfixType {
    XFX,
    XFY,
    YFX
}


#[derive(Debug, Clone, Copy)]
pub enum PrefixType {
    FX,
    FY
}

#[derive(Debug, Clone, Copy)]
pub enum PostfixType {
    XF,
    YF
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
    None
}


#[derive(Debug, Clone, Copy)]
pub struct OperatorInfo<T> {
    pub precedence: u32,
    pub op_type: T
}

impl OperatorInfo<InfixType> {
    pub fn associativity(&self) -> Associativity {
        match self.op_type {
            InfixType::XFX => Associativity::None,
            InfixType::XFY => Associativity::Right,
            InfixType::YFX => Associativity::Left,
        }
    }
}

impl OperatorInfo<PostfixType> {
    pub fn associativity(&self) -> Associativity {
        match self.op_type {
            PostfixType::XF => Associativity::None,
            PostfixType::YF => Associativity::Left,
        }
    }
}

impl OperatorInfo<PrefixType> {
    pub fn associativity(&self) -> Associativity {
        match self.op_type {
            PrefixType::FX => Associativity::None,
            PrefixType::FY => Associativity::Right,
        }
    }
}


#[derive(Debug, Clone)]
pub struct Operator {
    pub atom: String,
    pub prefix: Option<OperatorInfo<PrefixType>>,
    pub infix: Option<OperatorInfo<InfixType>>,
    pub postfix: Option<OperatorInfo<PostfixType>>
}

impl Operator {

    pub fn new_prefix(atom: &str, precedence: u32, op_type: PrefixType) -> Self {
        Operator { 
            atom: String::from(atom), 
            prefix: Some(OperatorInfo { precedence, op_type }),
            infix: None,
            postfix: None
        }
    }

    pub fn new_postfix(atom: &str, precedence: u32, op_type: PostfixType) -> Self {
        Operator { 
            atom: String::from(atom), 
            prefix: None,
            infix: None,
            postfix: Some(OperatorInfo { precedence, op_type })
        }
    }

    pub fn new_infix(atom: &str, precedence: u32, op_type: InfixType) -> Self {
        Operator { 
            atom: String::from(atom), 
            prefix: None,
            infix: Some(OperatorInfo { precedence, op_type }),
            postfix: None
        }
    }
}


pub type PrecedenceMap = HashMap<String, Operator>;

lazy_static! {

    pub static ref DEFAULT_PRECEDENCES: PrecedenceMap = {
        let mut precedences = PrecedenceMap::new();
        let operators = [
            ("\\+", Operator::new_prefix("\\+", 900, PrefixType::FY)),
            ("is", Operator::new_infix("is", 700, InfixType::XFX)),
            ("==", Operator::new_infix("==", 700, InfixType::XFX)),
            ("=:=", Operator::new_infix("=:=", 700, InfixType::XFX)),
            ("=/=", Operator::new_infix("=/=", 700, InfixType::XFX)),
            ("<", Operator::new_infix("<", 700, InfixType::XFX)),
            ("=<", Operator::new_infix("=<", 700, InfixType::XFX)),
            (">", Operator::new_infix(">", 700, InfixType::XFX)),
            (">=", Operator::new_infix(">=", 700, InfixType::XFX)),
            ("+", Operator {
                atom: String::from("+"),
                infix: Some(OperatorInfo { precedence: 500, op_type: InfixType::YFX }),
                prefix: Some(OperatorInfo { precedence: 200, op_type: PrefixType::FY}),
                postfix: None
            }),
            ("-", Operator::new_infix("-", 500, InfixType::YFX)),
            ("*", Operator::new_infix("*", 400, InfixType::YFX)),
            ("/", Operator::new_infix("/", 400, InfixType::YFX)),
            ("^", Operator::new_infix("^", 200, InfixType::XFY)),
            ("^", Operator::new_infix("^", 200, InfixType::XFY)),
        ];
        for (atom, op) in operators {
            precedences.insert(String::from(atom), op);
        }
        precedences
    };
}

#[derive(Debug)]
enum ShuntingyardOperator {
    LeftPar,
    Comma,
    Operator(Operator),
}


struct ShuntingyardParser {
    operand_stack : Vec<(Term, usize)>,
    operator_stack: Vec<(ShuntingyardOperator, usize)>,
    pos: usize
}

impl ShuntingyardParser {

    pub fn new() -> Self {
        ShuntingyardParser { 
            operand_stack: Vec::new(),
            operator_stack: Vec::new(),
            pos: 0
        }
    }

    fn push_operator(&mut self, operator: ShuntingyardOperator) {
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

    fn is_last_token_operator(&self) -> bool{
        let latest_operand_pos: usize = match &self.operand_stack.last() {
            Some((_, pos)) => *pos,
            None => 0,
        };
        let latest_operator_pos: usize = match &self.operator_stack.last() {
            Some((_, pos)) => *pos,
            None => 0
        };
        return latest_operator_pos > latest_operand_pos
    }

    fn apply_top(&mut self) {
        let should_be_postfix = self.is_last_token_operator();
        if let Some((ShuntingyardOperator::Operator(operator), pos)) = self.operator_stack.pop() {
            if should_be_postfix {
                match operator.postfix {
                    Some(_) => {
                        let operand = self.pop_operand().unwrap();
                        self.push_operand(Term::Structure(
                            Structure { 
                                functor: operator.atom,
                                params: vec![operand]
                            }
                        ));
                    }
                    None => unreachable!("Postfix for {:?}", operator.atom),
                }
            } else {
                let right_operand = self.pop_operand().unwrap();
                let should_be_prefix = self.is_last_token_operator() || self.operand_stack.is_empty();
                if should_be_prefix {
                    match operator.prefix {
                        Some(_) => {
                            self.push_operand(Term::Structure(
                                Structure { 
                                    functor: operator.atom,
                                    params: vec![right_operand]
                                }
                            ));
                        }
                        None => unreachable!("Prefix for {:?}", operator.atom),
                    }
                } else {
                    match operator.infix {
                        Some(_) => {
                            let left_operand = self.pop_operand().unwrap();
                            self.push_operand(Term::Structure(
                                Structure { 
                                    functor: operator.atom,
                                    params: vec![left_operand, right_operand]
                                }
                            ));
                            
                        }
                        None => unreachable!("Infix for {:?}", operator.atom)
                    }
                }
            }
        }
    }

    fn handle_operator(&mut self, operator: Operator) -> bool {
        if let Some((ShuntingyardOperator::Operator(last_op), pos)) = &self.operator_stack.last() {
            if let Some(op_prefix) = operator.prefix {
                if self.is_last_token_operator() {
                    if let Some(last_op_infix) = last_op.infix {
                        match last_op_infix.op_type {
                            InfixType::XFX | InfixType::YFX => {
                                if op_prefix.precedence >= last_op_infix.precedence {
                                    panic!("Priority clash");
                                }
                            }
                            InfixType::XFY => {
                                if op_prefix.precedence > last_op_infix.precedence {
                                    panic!("Priority clash");
                                }
                            }
                        }
                    }
                    else if let Some(last_op_prefix) = last_op.prefix {
                        match last_op_prefix.op_type {
                            PrefixType::FX => if last_op_prefix.precedence == op_prefix.precedence {
                                panic!("Priority clash");
                            } else {
                                self.apply_top();
                                return true;
                            },
                            PrefixType::FY => if last_op_prefix.precedence <= op_prefix.precedence {
                                self.apply_top();
                                return true;
                            }
                        }
                    }
                    else {
                        panic!("Operator Error");
                    }
                }
            } 
            if let Some(op_infix) = operator.infix {
                if let Some(last_op_infix) = last_op.infix {
                    // TODO: Check whether (InfixType::XFY, InfixType::YFX) | (InfixType::YFX, InfixType::YFX) case is correct
                    match (last_op_infix.op_type, op_infix.op_type) {
                        (InfixType::XFX, InfixType::XFX) 
                        | (InfixType::XFX, InfixType::XFY) 
                        | (InfixType::YFX, InfixType::XFX) 
                        | (InfixType::YFX, InfixType::XFY)
                        | (InfixType::XFY, InfixType::YFX) => {
                            if last_op_infix.precedence == op_infix.precedence {
                                panic!("Operator clash {} vs {}", last_op.atom, operator.atom);
                            } else if last_op_infix.precedence < op_infix.precedence {
                                self.apply_top();
                                return true;
                            }
                        }                        
                        (InfixType::XFX, InfixType::YFX) | (InfixType::YFX, InfixType::YFX) => {
                            if last_op_infix.precedence <= op_infix.precedence {
                                self.apply_top();
                                return true;
                            }
                        }
                        (InfixType::XFY, InfixType::XFX) | (InfixType::XFY, InfixType::XFY) => {
                            if last_op_infix.precedence < op_infix.precedence {
                                self.apply_top();
                                return true;
                            }
                        }
                    }
                }
                else if let Some(last_op_prefix) = last_op.prefix {
                    assert!(!self.is_last_token_operator());
                    match (last_op_prefix.op_type, op_infix.associativity()) {
                        (PrefixType::FX, Associativity::Left) | (PrefixType::FY, Associativity::Left) => 
                            if last_op_prefix.precedence <= op_infix.precedence {
                                self.apply_top();
                                return true;
                            },
                        (PrefixType::FX, Associativity::Right) | (PrefixType::FX, Associativity::None) => 
                            if last_op_prefix.precedence < op_infix.precedence {
                                self.apply_top();
                                return true;
                            } else if last_op_prefix.precedence == op_infix.precedence {
                                panic!("operator clash")
                            },
                        (PrefixType::FY, Associativity::Right) | (PrefixType::FY, Associativity::None) =>           
                            if last_op_prefix.precedence < op_infix.precedence {
                                self.apply_top();
                                return true;
                            },
                        
                    }
                }
                else if let Some(last_op_postfix) = last_op.postfix {
                    assert!(self.is_last_token_operator());
                    match op_infix.associativity() {
                        Associativity::Left => if last_op_postfix.precedence <= op_infix.precedence {
                            self.apply_top();
                            return true;
                        }
                        Associativity::Right | Associativity::None => if last_op_postfix.precedence == op_infix.precedence {
                            panic!("Operator clash {} vs {}", last_op.atom, operator.atom);
                        } else if last_op_postfix.precedence < op_infix.precedence {
                            self.apply_top();
                            return true;
                        }
                    }                    
                }
            }
            if let Some(op_postfix) = operator.postfix {
                if !self.is_last_token_operator() {
                    if let Some(last_op_infix) = last_op.infix {
                        match last_op_infix.op_type {
                            InfixType::XFX | InfixType::YFX => {
                                if op_postfix.precedence == last_op_infix.precedence {
                                    panic!("Priority clash");
                                } else if op_postfix.precedence > last_op_infix.precedence {
                                    self.apply_top();
                                    return true;
                                }
                            }
                            InfixType::XFY => {
                                if op_postfix.precedence > last_op_infix.precedence {
                                    self.apply_top();
                                    return true;
                                }
                            }
                        }
                    }
                }
                else if let Some(last_op_postfix) = last_op.postfix {
                    match last_op_postfix.op_type {
                        PostfixType::XF => if last_op_postfix.precedence == op_postfix.precedence {
                            panic!("Priority clash");
                        } else {
                            self.apply_top();
                            return true;
                        },
                        PostfixType::YF => if last_op_postfix.precedence <= op_postfix.precedence {
                            self.apply_top();
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    pub fn parse_with_defaults(&mut self, pair: Pair<Rule>) -> Option<Term> {
        self.parse(pair, &PrologParser::new())
    }

    pub fn parse(&mut self, pair: Pair<Rule>, prolog_parser: &PrologParser) -> Option<Term> {
        let pairs = pair.into_inner();

        for (idx, pair) in pairs.enumerate() {
            self.pos = idx + 1;
            let inner = prolog_parser.from(pair)?;
            if let CFGNode::Term(term) = inner {
                match term {
                    Term::Atom(atom) => {
                        if let Some(new_op) = prolog_parser.operators.get(atom.as_str()) {
                            while self.handle_operator(new_op.clone()) {}
                            self.push_operator(ShuntingyardOperator::Operator(new_op.clone()));
                        }
                        else {
                            self.push_operand(Term::Atom(atom));
                        }
                    }
                    Term::Structure(structure) => {
                        // TODO: Clean up followin if-else statements
                        if structure.params.len() != 1 {
                            self.push_operand(Term::Structure(structure));
                        } else if let Some(new_op) = prolog_parser.operators.get(structure.functor.as_str()) {
                            if new_op.prefix.is_none() && new_op.postfix.is_none() {
                                while self.handle_operator(new_op.clone()) {}
                                self.push_operator(ShuntingyardOperator::Operator(new_op.clone()));
                                self.push_operand(structure.params[0].clone());
                            } else {
                                self.push_operand(Term::Structure(structure));
                            }
                        }
                        else {
                            self.push_operand(Term::Structure(structure));
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

        while !self.operator_stack.is_empty() {
            self.apply_top();
        }

        assert!(self.operand_stack.len() == 1);
        assert!(self.operator_stack.len() == 0);

        self.pop_operand()
    }

}