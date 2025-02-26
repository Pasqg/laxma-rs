use std::{fmt::Display, mem, rc::Rc};

use nohash_hasher::IntMap;

use super::{identifier_map::IdentifierId, internal_repr::FunctionDefinition};

//todo: eventually crate feature
pub(super) type RcValue = Rc<Value>;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Typed(IdentifierId, IdentifierId, Vec<RcValue>),
    Integer(i64),
    Float(f32),
    Bool(bool),
    String(Rc<String>),
    // (definition, captures) effectively a closure (model as such)
    Function(Rc<FunctionDefinition>, Rc<IntMap<IdentifierId, RcValue>>),
    Void,
    Unknown,
}

// Like for linked lists, we need to avoid recursive drop().
// Here it's enough for now to only handle Typed variant as Function chains of million elements are highly unlikely
// see https://rust-unofficial.github.io/too-many-lists/first-drop.html
impl Drop for Value {
    fn drop(&mut self) {
        let mut stack = Vec::new();
        let mut current: *mut Value = self;

        while let Some(node) = unsafe { current.as_mut() } {
            if let Value::Typed(_, _, values) = node {
                for rc in mem::take(values) {
                    // Only executes when there reference count is 0
                    if let Ok(inner) = Rc::try_unwrap(rc) {
                        stack.push(Box::new(inner));
                    }
                }
            }

            current = match stack.pop() {
                Some(boxed) => Box::leak(boxed),
                None => std::ptr::null_mut(),
            };
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Typed(name, variant, values) => write!(
                f,
                "{name}::{variant}({})",
                values
                    .iter()
                    .map(|val| format!("{val}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Function(_, _) => write!(f, "Function"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{}", &s.to_string()[1..s.len() - 1]),
            Value::Void => write!(f, "Void"),
            Value::Unknown => write!(f, "Unknown"),
        }
    }
}

impl Value {
    pub fn value_to_str(
        &self,
        id_to_name: &impl Fn(&IdentifierId) -> Rc<String>,
    ) -> Result<String, String> {
        match self {
            Value::Typed(id, variant, values) => {
                let mut args = Vec::new();
                for value in values {
                    args.push(value.value_to_str(id_to_name)?);
                }
                Ok(format!(
                    "{}::{}({})",
                    id_to_name(id),
                    id_to_name(variant),
                    args.join(", ")
                ))
            }
            Value::Function(def, _) => Ok(format!("Function {}", id_to_name(&def.id))),
            Value::Integer(x) => Ok(format!("{x}")),
            Value::Float(x) => Ok(format!("{x}")),
            Value::Bool(x) => Ok(format!("{x}")),
            Value::String(x) => Ok(format!("{}", &x.to_string()[1..x.len() - 1])),
            Value::Void => Ok("Void".to_string()),
            Value::Unknown => Ok("Unknown".to_string()),
        }
    }

    pub(super) fn as_boolean(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Not a boolean"),
        }
    }

    pub(super) fn as_int(&self) -> i64 {
        match self {
            Value::Integer(x) => *x,
            _ => panic!("Not an Int"),
        }
    }
}