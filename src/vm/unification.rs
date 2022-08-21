use std::rc::Rc;

use super::{Value, ValueCell, List, Trail};


pub fn unify_ref(val1: &Value, val2: &Value, trail: &mut Trail) -> bool {
    match (val1, val2) {
        (Value::All, _) | (_, Value::All) => true,
        (Value::Atom(id1), Value::Atom(id2)) => id1 == id2,
        (Value::Int(v1), Value::Int(v2)) => v1 == v2,
        (Value::Str(v1), Value::Str(v2)) => v1 == v2,
        (Value::Struct(struct1), Value::Struct(struct2)) => {
            if Rc::ptr_eq(&struct1, &struct2) {
                true
            } else if (*struct1).functor == (*struct2).functor && (*struct1).terms.len() == (*struct2).terms.len() {
                let mut res = true;
                for (term1, term2) in (*struct1).terms.iter().zip((*struct2).terms.iter()) {
                    if !unify(term1.clone(), term2.clone(), trail) {
                        res = false;
                        break;
                    }
                }
                res
            } else {
                false
            }
        }
        (Value::List(list1), Value::List(list2)) => {
            unify_lists(&list1, &list2, trail)
        }
        (Value::Ref(loc1), Value::Ref(loc2)) => {
            let v1 = loc1.get_value();
            let v2 = loc2.get_value();
            match (&v1, &v2) {
                (Value::Nil, _) => {
                    trail.push(loc1.clone());
                    loc1.put_ref(loc2.clone());
                    true
                },
                (_, Value::Nil) => {
                    trail.push(loc2.clone());
                    loc2.put_ref(loc1.clone());
                    true
                }
                _ => {
                    unify(v1, v2, trail)
                }
            }
        },
        (Value::Ref(loc), other) | (other, Value::Ref(loc)) => {
            let ref_val = loc.get_value();
            match ref_val {
                Value::Nil => {
                    trail.push(loc.clone());
                    loc.put(other.clone());
                    true
                }
                _ => unify(ref_val, other.clone(), trail)
            }
        }
        _ => false
    }
}

pub fn unify(val1: Value, val2: Value, trail: &mut Trail) -> bool {
    match (val1, val2) {
        (Value::All, _) | (_, Value::All) => true,
        (Value::Atom(id1), Value::Atom(id2)) => id1 == id2,
        (Value::Int(v1), Value::Int(v2)) => v1 == v2,
        (Value::Str(v1), Value::Str(v2)) => v1 == v2,
        (Value::Struct(struct1), Value::Struct(struct2)) => {
            if Rc::ptr_eq(&struct1, &struct2) {
                true
            } else if (*struct1).functor == (*struct2).functor && (*struct1).terms.len() == (*struct2).terms.len() {
                let mut res = true;
                for (term1, term2) in (*struct1).terms.iter().zip((*struct2).terms.iter()) {
                    if !unify(term1.clone(), term2.clone(), trail) {
                        res = false;
                        break;
                    }
                }
                res
            } else {
                false
            }
        }
        (Value::List(list1), Value::List(list2)) => {
            unify_lists(&list1, &list2, trail)
        }
        (Value::Ref(loc1), Value::Ref(loc2)) => {
            let v1 = loc1.get_value();
            let v2 = loc2.get_value();
            match (&v1, &v2) {
                (Value::Nil, _) => {
                    trail.push(loc1.clone());
                    loc1.put_ref(loc2.clone());
                    true
                },
                (_, Value::Nil) => {
                    trail.push(loc2.clone());
                    loc2.put_ref(loc1.clone());
                    true
                }
                _ => {
                    unify(v1, v2, trail)
                }
            }
        },
        (Value::Ref(loc), other) | (other, Value::Ref(loc)) => {
            let ref_val = loc.get_value();
            match ref_val {
                Value::Nil => {
                    trail.push(loc.clone());
                    loc.put(other);
                    true
                }
                _ => unify(ref_val, other, trail)
            }
        }
        _ => false
    }
}

pub fn unify_lists(first: &List, second: &List, trail: &mut Trail) -> bool {
    let mut list1 = first;
    let mut list2 = second;        
    loop {
        match (list1, list2) {
            (List::Nil, List::Nil) => break,
            (List::Nil, List::Cons(_)) => return false,
            (List::Cons(_), List::Nil) => return false,
            (List::Cons(node1), List::Cons(node2)) => {
                if Rc::ptr_eq(&node1, &node2) {
                    return true;
                }
                if !unify(node1.head.clone(), node2.head.clone(), trail) {
                    return false;
                }
                list1 = &node1.tail;
                list2 = &node2.tail;
            }
            (List::Nil, List::Ref(loc)) | (List::Ref(loc), List::Nil)  => {
                trail.push(loc.clone());
                loc.put(Value::List(List::Nil));
                return true;
            },
            (cons_list @ List::Cons(node), List::Ref(loc)) | (List::Ref(loc), cons_list @ List::Cons(node)) => {
                let ref_val = loc.get_value_deref();
                match ref_val {
                    Value::Nil => {
                        trail.push(loc.clone());
                        loc.put(Value::List(List::Cons(node.clone())));
                    }
                    Value::List(list) => return unify_lists(&list, cons_list, trail),
                    _ => return false
                }
            }
            (List::Ref(loc1), List::Ref(loc2)) => {
                let list1 = loc1.get_value_deref();
                let list2 = loc2.get_value_deref();
                match (&list1, &list2) {
                    (Value::Nil, Value::Nil) => {
                        trail.push(loc1.clone());
                        loc1.put_ref(loc2.clone());
                        return true;
                    }
                    (Value::Nil, Value::List(_)) => {
                        trail.push(loc1.clone());
                        loc1.put(list2.clone());
                        return true;
                    },
                    (Value::List(_), Value::Nil) => {
                        trail.push(loc2.clone());
                        loc2.put(list1.clone());
                        return true;
                    },
                    (Value::List(_), Value::List(_)) => {
                        unify(list1, list2, trail);
                    }
                    _ => {
                        panic!("List reference values are expected to be lists - Unify {:?} and {:?}", list1, list2);
                    }
                }
            }
            _ => unreachable!()
        }
    }

    true
}