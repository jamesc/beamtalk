// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor state field initialization and inheritance code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates state field initializers for actor `init/1` callbacks. Inherited
//! state from a stateful parent is supplied by the super-init chain (BT-1417,
//! see `gen_server/callbacks.rs`), not by this module.

use super::super::document::{Document, leaf, line};
use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, Module};
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates only the current class's own state fields (not inherited).
    ///
    /// Returns a list of `Document` fragments, each representing one field
    /// initializer line (with a leading `line()` for indentation).
    /// The caller wraps these in a `nest()` at the appropriate level.
    ///
    /// This is used when calling parent init — we only add fields defined in this class,
    /// not fields from parent classes (those come from parent's init).
    pub(super) fn generate_own_state_fields(
        &mut self,
        module: &Module,
    ) -> Result<Vec<Document<'static>>> {
        let mut fields = Vec::new();

        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });

        if let Some(class) = current_class {
            for state in &class.state {
                let value_code = if let Some(ref default_value) = state.default_value {
                    self.expression_doc(default_value)?
                } else {
                    Document::Str("'nil'")
                };
                fields.push(docvec![
                    line(),
                    docvec![", ", leaf::atom(state.name.name.to_string()), " => "],
                    value_code,
                ]);
            }
        }

        Ok(fields)
    }

    /// Generates the state fields for a class whose parent is a base class
    /// (`Actor` / `Object` / none) — i.e. a class with no stateful ancestor.
    ///
    /// Returns a list of `Document` fragments, each representing one field
    /// initializer line (with a leading `line()` for indentation).
    /// The caller wraps these in a `nest()` at the appropriate level.
    ///
    /// Inherited state from a *non-base* parent is NOT collected here. A subclass
    /// of a stateful actor instead has its `init/1` call the parent's compiled
    /// `init/1` and merge its own fields on top — the super-init chain (BT-1417,
    /// see `gen_server/callbacks.rs`). That `has_parent_init` path resolves the
    /// parent's compiled module and so handles cross-file, stdlib, and package
    /// parents uniformly; this function is only reached with a base-class parent.
    /// It also emits fields from module-level `x := literal` assignments for
    /// script/workspace modules.
    pub(in crate::codegen::core_erlang) fn generate_initial_state_fields(
        &mut self,
        module: &Module,
    ) -> Result<Vec<Document<'static>>> {
        let mut fields = Vec::new();

        // Initialize fields from module expressions (assignments at top level)
        // Only include literal values - blocks are methods handled by dispatch/3
        for stmt in &module.expressions {
            if let Expression::Assignment { target, value, .. } = &stmt.expression {
                if let Expression::Identifier(id) = target.as_ref() {
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        let value_code = self.expression_doc(value)?;
                        fields.push(docvec![
                            line(),
                            docvec![", ", leaf::atom(id.name.to_string()), " => "],
                            value_code,
                        ]);
                    }
                }
            }
        }

        // Find the current class being compiled (matches module name)
        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });

        if let Some(class) = current_class {
            // Emit this class's own fields. Inherited state from a non-base parent
            // is supplied at runtime by the super-init chain (BT-1417), not collected
            // here — this branch is only reached when the parent is a base class.
            for state in &class.state {
                let value_code = if let Some(ref default_value) = state.default_value {
                    self.expression_doc(default_value)?
                } else {
                    Document::Str("'nil'")
                };
                fields.push(docvec![
                    line(),
                    docvec![", ", leaf::atom(state.name.name.to_string()), " => "],
                    value_code,
                ]);
            }
        } else {
            // Fallback: no class matched the module name. Reached by hand-constructed test
            // fixtures that build a `Module` with an unprefixed/bare class name (e.g.
            // "counter") instead of the `bt@…` scheme real compilation uses — see
            // `util::module_matches_class`. Load-bearing for those tests, not dead code.
            for class in &module.classes {
                for state in &class.state {
                    let value_code = if let Some(ref default_value) = state.default_value {
                        self.expression_doc(default_value)?
                    } else {
                        Document::Str("'nil'")
                    };
                    fields.push(docvec![
                        line(),
                        docvec![", ", leaf::atom(state.name.name.to_string()), " => "],
                        value_code,
                    ]);
                }
            }
        }

        Ok(fields)
    }
}
