// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor state field initialization and inheritance code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates state field initializers for actor `init/1` callbacks. Inherited
//! state from a stateful parent is supplied by the super-init chain
//! (see `gen_server/callbacks.rs`), not by this module.

use super::super::util::omit_late_defaultless_slot;
use super::super::{CoreErlangGenerator, Result};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, leaf, line};
use beamtalk_core::ast::{Expression, Module, StateDeclaration};

/// The state fields of an `init/1` default map: a prelude binding the defaults
/// that run code, in source order, and one map-entry line per field.
pub(in crate::core_erlang) struct StateFieldInits {
    /// `let FieldDefaultN = <default> in ` for the defaults hoisted ahead of
    /// the map (empty when at most one default can run code).
    pub prelude: Document<'static>,
    /// One `, 'name' => value` line per field, each with a leading `line()`.
    pub fields: Vec<Document<'static>>,
}

impl CoreErlangGenerator {
    /// BT-4591: sequences the default-value expressions of one map literal.
    /// A map literal's pairs evaluate in unspecified order (erlc runs the last
    /// first), and a default is an arbitrary expression, so two defaults that
    /// send messages are bound to temps in source order through
    /// `thread_subexprs` and the map holds only their variables.
    ///
    /// Returns the prelude (`let FieldDefaultN = <default> in `, empty when at
    /// most one default can run code), which the caller places directly before
    /// the map, and one value document per declaration, in order (`'nil'` for a
    /// declaration without a default). The one helper every default-map
    /// builder uses: an actor's `init/1`, a Value class's `new/0`, and the
    /// `classState:` initial values.
    pub(in crate::core_erlang) fn thread_default_values(
        &mut self,
        states: &[&StateDeclaration],
    ) -> Result<(Document<'static>, Vec<Document<'static>>)> {
        let exprs: Vec<&Expression> = states
            .iter()
            .filter_map(|state| state.default_value.as_ref())
            .collect();
        let (prelude, docs) = self.thread_subexprs(&exprs, "FieldDefault")?;
        let mut docs = docs.into_iter();
        let values = states
            .iter()
            .map(|state| {
                if state.default_value.is_some() {
                    docs.next().expect("one threaded document per default")
                } else {
                    leaf::atom("nil")
                }
            })
            .collect();
        Ok((self.threaded_prelude_doc(&prelude), values))
    }

    /// The map entries for `states`' defaults — see [`Self::thread_default_values`].
    fn state_field_inits(
        &mut self,
        mut leading: Vec<Document<'static>>,
        states: &[&StateDeclaration],
    ) -> Result<StateFieldInits> {
        let (prelude, values) = self.thread_default_values(states)?;
        for (state, value_code) in states.iter().zip(values) {
            leading.push(docvec![
                line(),
                docvec![", ", leaf::atom(state.name.name.to_string()), " => "],
                value_code,
            ]);
        }
        Ok(StateFieldInits {
            prelude,
            fields: leading,
        })
    }

    /// Generates only the current class's own state fields (not inherited).
    ///
    /// Each field is one `Document` fragment (with a leading `line()` for
    /// indentation); the caller wraps them in a `nest()` at the appropriate
    /// level, after the returned prelude.
    ///
    /// This is used when calling parent init — we only add fields defined in this class,
    /// not fields from parent classes (those come from parent's init).
    pub(super) fn generate_own_state_fields(&mut self, module: &Module) -> Result<StateFieldInits> {
        let states: Vec<&StateDeclaration> = self
            .current_class(module)
            .map(|class| {
                class
                    .state
                    .iter()
                    .filter(|state| !omit_late_defaultless_slot(state))
                    .collect()
            })
            .unwrap_or_default();
        self.state_field_inits(Vec::new(), &states)
    }

    /// Generates the state fields for a class whose parent is a base class
    /// (`Actor` / `Object` / none) — i.e. a class with no stateful ancestor.
    ///
    /// Returns the fields like [`Self::generate_own_state_fields`].
    /// Inherited state from a *non-base* parent is NOT collected here. A subclass
    /// of a stateful actor instead has its `init/1` call the parent's compiled
    /// `init/1` and merge its own fields on top — the super-init chain
    /// (see `gen_server/callbacks.rs`). That `has_parent_init` path resolves the
    /// parent's compiled module and so handles cross-file, stdlib, and package
    /// parents uniformly; this function is only reached with a base-class parent.
    /// It also emits fields from module-level `x := literal` assignments for
    /// script/workspace modules.
    pub(in crate::core_erlang) fn generate_initial_state_fields(
        &mut self,
        module: &Module,
    ) -> Result<StateFieldInits> {
        let mut leading = Vec::new();

        // Initialize fields from module expressions (assignments at top level)
        // Only include literal values - blocks are methods handled by dispatch/3
        for stmt in &module.expressions {
            if let Expression::Assignment { target, value, .. } = &stmt.expression {
                if let Expression::Identifier(id) = target.as_ref() {
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        let value_code = self.expression_doc(value)?;
                        leading.push(docvec![
                            line(),
                            docvec![", ", leaf::atom(id.name.to_string()), " => "],
                            value_code,
                        ]);
                    }
                }
            }
        }

        // Find the current class being compiled (identity check — ADR 0119).
        // Its own fields are emitted here; inherited state from a non-base
        // parent is supplied at runtime by the super-init chain. Fallback: no
        // class identity was set (e.g. a hand-constructed test fixture that
        // calls this method directly without going through
        // `setup_class_identity`) — load-bearing for those tests, not dead code.
        let classes: Vec<&beamtalk_core::ast::ClassDefinition> = match self.current_class(module) {
            Some(class) => vec![class],
            None => module.classes.iter().collect(),
        };
        let states: Vec<&StateDeclaration> = classes
            .iter()
            .flat_map(|class| class.state.iter())
            .filter(|state| !omit_late_defaultless_slot(state))
            .collect();
        self.state_field_inits(leading, &states)
    }
}
