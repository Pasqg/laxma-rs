use std::{collections::HashSet, hash::Hash};

use super::token_stream::Token;

#[derive(Eq, PartialEq, Debug)]
pub struct AST<RuleId> {
    pub(super) id: Option<RuleId>,
    pub(super) matched: Vec<Token>,
    pub(super) children: Vec<AST<RuleId>>,
}

impl<RuleId> AST<RuleId> {
    pub fn new(
        id: Option<RuleId>,
        matched: Vec<Token>,
        children: Vec<AST<RuleId>>,
    ) -> Self {
        Self {
            id,
            matched,
            children,
        }
    }

    pub fn empty() -> Self {
        Self::new(None, Vec::new(), Vec::new())
    }

    pub fn merge(mut self, other: Self) -> Self  {
        let other_matched = other.matched.clone();
        self.matched.extend(other_matched);
        self.children.push(other);
        self
    }

    /*
        Prunes the tree depth-first by:
            - replacing nodes that have only one child with the child itself (i.e. degenerate subtree)
            - removing nodes with None id and no children (structural tokens that provide no additional information)

        Example:
            Unpruned: variable -> identifier -> name ["myvar"]
            Pruned: variable ["myvar"]

        Use the excluded set to exclude rules from being pruned. Note this does not apply recursively.
        Example:
            excluded = {"variable"}
            Unpruned: variable -> identifier -> name ["myvar"]
            Pruned: variable -> identifier ["myvar"]

        The parent rule is used for the child's matched tokens unless use_child_rule contains the parent rule.
        Note this doesn't apply recursively.
        Example: use_child_rule
            excluded = {"variable"} use_child_rule = {"Identifier"}
            Unpruned: variable -> identifier -> name ["myvar"]
            Pruned: variable -> name ["myvar"]
    */
    pub fn prune(&self, excluded: &HashSet<RuleId>, use_child_rule: &HashSet<RuleId>) -> Self
    where
        RuleId: Eq + Hash + Copy
    {
        let children = &self.children;
        if children.len() == 1 && self.id.is_some() && excluded.contains(&self.id.unwrap()) {
            let child = &children[0];
            if child.id.is_none() {
                return Self::new(self.id, child.matched.clone(), Vec::new());
            }

            let child = child.prune(excluded, use_child_rule);

            let mut rule_id = self.id;
            if rule_id.is_none() || use_child_rule.contains(&rule_id.unwrap()) {
                rule_id = child.id
            }
            return Self::new(rule_id, child.matched, child.children);
        }

        Self::new(
            self.id,
            self.matched.clone(),
            self.children
                .iter()
                .map(|child| child.prune(excluded, use_child_rule))
                .collect(),
        )
    }
}
