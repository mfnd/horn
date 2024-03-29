use std::{rc::Rc, cell::RefCell, fmt};

use crate::debugln;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    All,
    Atom(usize),
    Int(i64),
    Str(Rc<str>),
    Struct(Rc<Struct>),
    Ref(ValueCell),
    List(List)
}

impl Value {

    pub fn create_struct(functor: usize, terms: Vec<Value>) -> Self {
        Value::Struct(
            Rc::from(
                Struct {
                    functor,
                    params: terms
                }
            )
        )
    }

    pub fn deep_copy(&self) -> Self {
        match self {
            Value::Struct(s) => {
                Value::Struct(
                    Rc::from(
                        Struct {
                            functor: s.functor,
                            params: s.params.iter().map(|v| v.deep_copy()).collect(),
                        }
                    )
                )
            }
            Value::Ref(value_cell) => {
                value_cell.get_value_deref().deep_copy()
            }
            Value::List(list) => {
                Value::List(list.deep_copy())
            }
            _ => self.clone()
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::All => "all",
            Value::Atom(_) => "atom",
            Value::Int(_) => "integer",
            Value::Str(_) => "string",
            Value::Struct(_) => "struct",
            Value::Ref(value_cell) => value_cell.get_value_deref().type_str(),
            Value::List(_) => "list",
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => false,
            (Self::Atom(left), Self::Atom(right)) => left == right,
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Str(left), Self::Str(right)) => left == right,
            (Self::Struct(left), Self::Struct(right)) => left == right,
            (Self::Ref(left), Self::Ref(right)) => {
                left.get_value_deref() == right.get_value_deref()
            },
            (Self::Ref(ref_value), other) | (other, Self::Ref(ref_value)) => {
                dbg!(&ref_value.get_value_deref() == other)
            }
            (Self::List(left), Self::List(right)) => left == right,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left.partial_cmp(right),
            (Self::Str(left), Self::Str(right)) => left.partial_cmp(right),
            (Self::Ref(left), Self::Ref(right)) => {
                left.get_value_deref().partial_cmp(&right.get_value_deref())
            },
            (Self::Ref(left), right) => left.get_value_deref().partial_cmp(right),
            (left, Self::Ref(right)) => left.partial_cmp(&right.get_value_deref()),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub enum List {
    Nil,
    Cons(Rc<Node>),
    Ref(ValueCell)
}

impl List {

    pub fn cons(head: Value, tail: List) -> Self {
        List::Cons(
            Rc::from(
                Node {
                    head,
                    tail
                }
            )
        )
    }

    pub fn deep_copy(&self) -> Self {
        match self {
            List::Nil => List::Nil,
            List::Cons(node) => {
                List::cons(node.head.deep_copy(), node.tail.deep_copy())
            }
            List::Ref(value_cell) => {
                let deref_value = value_cell.get_value_deref();
                match deref_value {
                    Value::List(list) => list.deep_copy(),
                    _ => unreachable!()
                }
            }
        }
    }
}

impl From<Vec<Value>> for List {
    fn from(values: Vec<Value>) -> Self {
        let mut curr_node = List::Nil;
        for value in values.into_iter().rev() {
            curr_node = List::cons(value, curr_node);
        }
        curr_node
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Cons(left), Self::Cons(right)) => left == right,
            (Self::Ref(left), Self::Ref(right)) => {
                left.get_value_deref() == right.get_value_deref()
            }
            (Self::Ref(ref_list), other) | (other, Self::Ref(ref_list)) => {
                match ref_list.get_value_deref() {
                    Value::List(list) => &list == other,
                    o => {
                        println!("Ref is not list {:?}", o);
                        false
                    }
                }
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub head: Value,
    pub tail: List
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub functor: usize,
    pub params: Vec<Value>
}

#[derive(Clone)]
pub struct ValueCell {
    value_ref: Rc<RefCell<Value>>
}

pub enum ValueCellContent {
    Bound(Value),
    Unbound(usize)
}

impl ValueCell {

    pub fn get_value(&self) -> Value {
        self.value_ref.as_ref().borrow().clone()
    }

    pub fn get_value_deref(&self) -> Value {
        let value = self.value_ref.as_ref().borrow().clone();
        match value {
            Value::Ref(value_cell) => value_cell.get_value_deref(),
            _ => value
        }
    }

    pub fn get_content(&self) -> ValueCellContent {
        let value = self.value_ref.as_ref().borrow().clone();
        match value {
            Value::Ref(value_cell) => value_cell.get_content(),
            Value::Nil => ValueCellContent::Unbound(self.value_ref.as_ptr() as usize),
            _ => ValueCellContent::Bound(value)
        } 
    }

    pub fn put_ref(&self, other: ValueCell) {
        self.value_ref.as_ref().replace(Value::Ref(other));
    }

    pub fn put(&self, other: Value) {
        self.value_ref.as_ref().replace(other);
    }

    pub fn new() -> Self {
        ValueCell {
            value_ref: Rc::from(RefCell::from(Value::Nil))
        }
    }

}

impl fmt::Debug for ValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueCell")
         .field("value", &self.get_value())
         .finish()
    }
}


pub struct Trail {
    trail: Vec<ValueCell>
}

impl Trail {

    pub fn new() -> Self {
        Trail { 
            trail: Vec::new() 
        }
    }

    pub fn push(&mut self, value_cell: ValueCell) {
        self.trail.push(value_cell);
    }

    pub fn len(&self) -> usize {
        self.trail.len()
    }

    pub fn rollback_until(&mut self, until: usize) {
        while self.trail.len() > until {
            if let Some(change) = self.trail.pop() {
                debugln!("Reverting {:?}", change);
                change.put(Value::Nil);
            }
        }
    }

    pub fn clear(&mut self) {
        self.rollback_until(0);
    }
    
}