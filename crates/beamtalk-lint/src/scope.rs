// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use std::collections::HashSet;

/// Lightweight scope stack used by lint passes to track variable bindings.
///
/// Each level holds the set of variable names defined at that depth.
pub(crate) struct LintScope {
    levels: Vec<HashSet<String>>,
}

impl LintScope {
    pub(crate) fn new() -> Self {
        Self {
            levels: vec![HashSet::new()],
        }
    }

    pub(crate) fn push(&mut self) {
        self.levels.push(HashSet::new());
    }

    pub(crate) fn pop(&mut self) {
        debug_assert!(
            self.levels.len() > 1,
            "LintScope::pop called with only root scope"
        );
        if self.levels.len() > 1 {
            self.levels.pop();
        }
    }

    /// Define `name` in the current (innermost) scope level.
    pub(crate) fn define(&mut self, name: &str) {
        if let Some(top) = self.levels.last_mut() {
            top.insert(name.to_string());
        }
    }

    /// Returns `true` if `name` is defined in any currently visible scope level.
    pub(crate) fn is_bound(&self, name: &str) -> bool {
        self.levels.iter().any(|level| level.contains(name))
    }

    #[cfg(test)]
    pub(crate) fn depth(&self) -> usize {
        self.levels.len()
    }

    #[cfg(test)]
    pub(crate) fn root_is_empty(&self) -> bool {
        self.levels[0].is_empty()
    }
}
