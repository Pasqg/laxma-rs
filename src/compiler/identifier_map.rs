use std::{collections::HashMap, rc::Rc};

use nohash_hasher::IntMap;

pub(super) type IdentifierId = i32;

pub(super) const UNDEFINED_ID: IdentifierId = -1;
pub(super) const ADD_ID: IdentifierId = 0;
pub(super) const SUB_ID: IdentifierId = 1;
pub(super) const MUL_ID: IdentifierId = 2;
pub(super) const DIV_ID: IdentifierId = 3;
pub(super) const GT_ID: IdentifierId = 4;
pub(super) const LT_ID: IdentifierId = 5;
pub(super) const EQ_ID: IdentifierId = 6;
pub(super) const GE_ID: IdentifierId = 7;
pub(super) const LE_ID: IdentifierId = 8;
pub(super) const PRINT_ID: IdentifierId = 9;
pub(super) const INT_ID: IdentifierId = 10;
pub(super) const VOID_ID: IdentifierId = 11;
pub(super) const BOOL_ID: IdentifierId = 12;
pub(super) const STRING_ID: IdentifierId = 13;
pub(super) const TRUE_ID: IdentifierId = 14;
pub(super) const FALSE_ID: IdentifierId = 15;
pub(super) const WILDCARD_ID: IdentifierId = 16;
pub(super) const UNKNOWN_ID: IdentifierId = 17;
pub(super) const FLOAT_ID: IdentifierId = 18;
pub(super) const ERROR_ID: IdentifierId = 19;
pub(super) const REPL_ID: IdentifierId = 20;
pub(super) const PRINTLN_ID: IdentifierId = 21;
pub(super) const WHILE_ID: IdentifierId = 22;
pub(super) const RANGE_ID: IdentifierId = 23;
pub(super) const LIST_ID: IdentifierId = 24;
pub(super) const EMPTY_LIST_ID: IdentifierId = 25;
pub(super) const FOLDL_ID: IdentifierId = 26;

#[derive(Debug, Clone)]
pub(super) struct IdentifierIdMap {
    id_map: HashMap<Rc<String>, IdentifierId>,
    identifier_map: IntMap<IdentifierId, Rc<String>>,
}

impl IdentifierIdMap {

    pub fn new() -> Self {
        let id_map = HashMap::from([
            (Rc::new("Undefined".to_string()), UNDEFINED_ID),
            (Rc::new("REPL".to_string()), REPL_ID),
            (Rc::new("+".to_string()), ADD_ID),
            (Rc::new("-".to_string()), SUB_ID),
            (Rc::new("*".to_string()), MUL_ID),
            (Rc::new("/".to_string()), DIV_ID),
            (Rc::new(">".to_string()), GT_ID),
            (Rc::new("<".to_string()), LT_ID),
            (Rc::new("==".to_string()), EQ_ID),
            (Rc::new(">=".to_string()), GE_ID),
            (Rc::new("<=".to_string()), LE_ID),
            (Rc::new("print".to_string()), PRINT_ID),
            (Rc::new("println".to_string()), PRINTLN_ID),
            (Rc::new("error".to_string()), ERROR_ID),
            (Rc::new("while".to_string()), WHILE_ID),
            (Rc::new("range".to_string()), RANGE_ID),
            (Rc::new("foldl".to_string()), FOLDL_ID),

            (Rc::new("Int".to_string()), INT_ID),
            (Rc::new("Float".to_string()), FLOAT_ID),
            (Rc::new("String".to_string()), STRING_ID),
            (Rc::new("Bool".to_string()), BOOL_ID),
            (Rc::new("Void".to_string()), VOID_ID),
            (Rc::new("Unknown".to_string()), UNKNOWN_ID),
            (Rc::new("true".to_string()), TRUE_ID),
            (Rc::new("false".to_string()), FALSE_ID),
            (Rc::new("List".to_string()), LIST_ID),
            (Rc::new("Empty".to_string()), EMPTY_LIST_ID),

            (Rc::new("_".to_string()), WILDCARD_ID),
        ]);

        let mut identifier_map = IntMap::default();
        for (k, v) in &id_map {
            identifier_map.insert(v.to_owned(), Rc::clone(k));
        }

        Self {
            id_map,
            identifier_map,
        }
    }

    pub(super) fn get_id(&mut self, identifier: &Rc<String>) -> IdentifierId {
        let id = self.id_map.get(identifier);
        if id.is_some() {
            return id.unwrap().to_owned();
        }

        let id = self.id_map.len() as IdentifierId;
        self.id_map.insert(Rc::clone(identifier), id);
        self.identifier_map.insert(id, Rc::clone(identifier));

        id
    }

    pub(super) fn get_identifier(&self, id: &IdentifierId) -> Option<&Rc<String>> {
        self.identifier_map.get(id)
    }
}
