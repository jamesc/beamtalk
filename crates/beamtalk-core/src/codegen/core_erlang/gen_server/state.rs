// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor state field initialization and inheritance code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates state field initializers for actor `init/1` callbacks,
//! including inherited fields from parent classes.

use super::super::{CoreErlangGenerator, Result};
use crate::ast::{Expression, Identifier, Module};
use std::fmt::Write;

impl CoreErlangGenerator {
    /// Generates only the current class's own state fields (not inherited).
    ///
    /// This is used when calling parent init — we only add fields defined in this class,
    /// not fields from parent classes (those come from parent's init).
    pub(super) fn generate_own_state_fields(&mut self, module: &Module) -> Result<()> {
        // Find the current class being compiled
        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });

        if let Some(class) = current_class {
            // Only emit this class's own fields
            for state in &class.state {
                self.write_indent()?;
                write!(self.output, ", '{}' => ", state.name.name)?;
                if let Some(ref default_value) = state.default_value {
                    self.generate_expression(default_value)?;
                } else {
                    // No default value - initialize to nil
                    write!(self.output, "'nil'")?;
                }
                writeln!(self.output)?;
            }
        }

        Ok(())
    }

    /// Generates all state fields including inherited ones (for base classes).
    ///
    /// This version includes fields from module-level assignments and recursively
    /// collects inherited fields from parent classes when they're in the same module.
    pub(in crate::codegen::core_erlang) fn generate_initial_state_fields(
        &mut self,
        module: &Module,
    ) -> Result<()> {
        // Initialize fields from module expressions (assignments at top level)
        // Only include literal values - blocks are methods handled by dispatch/3
        for expr in &module.expressions {
            if let Expression::Assignment { target, value, .. } = expr {
                if let Expression::Identifier(id) = target.as_ref() {
                    // Only generate field if it's a simple literal (not a block/method)
                    if matches!(value.as_ref(), Expression::Literal(..)) {
                        self.write_indent()?;
                        write!(self.output, ", '{}' => ", id.name)?;
                        self.generate_expression(value)?;
                        writeln!(self.output)?;
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
            // Collect inherited fields from parent classes (recursively)
            let inherited_fields = Self::collect_inherited_fields(class.superclass_name(), module)?;

            // Emit inherited fields first
            for (field_name, default_value) in inherited_fields {
                self.write_indent()?;
                write!(self.output, ", '{field_name}' => ")?;
                self.generate_expression(&default_value)?;
                writeln!(self.output)?;
            }

            // Then emit this class's own fields (can override parent defaults)
            for state in &class.state {
                self.write_indent()?;
                write!(self.output, ", '{}' => ", state.name.name)?;
                if let Some(ref default_value) = state.default_value {
                    self.generate_expression(default_value)?;
                } else {
                    // No default value - initialize to nil
                    write!(self.output, "'nil'")?;
                }
                writeln!(self.output)?;
            }
        } else {
            // Fallback: if no matching class found (legacy modules), emit all class fields
            for class in &module.classes {
                for state in &class.state {
                    self.write_indent()?;
                    write!(self.output, ", '{}' => ", state.name.name)?;
                    if let Some(ref default_value) = state.default_value {
                        self.generate_expression(default_value)?;
                    } else {
                        // No default value - initialize to nil
                        write!(self.output, "'nil'")?;
                    }
                    writeln!(self.output)?;
                }
            }
        }

        Ok(())
    }

    /// Recursively collects all inherited state fields from parent classes.
    ///
    /// Returns a vector of `(field_name, default_value)` pairs in inheritance order
    /// (most distant ancestor first). This ensures parent fields are initialized
    /// before child fields, allowing children to override parent defaults.
    ///
    /// Only works when parent classes are defined in the same Module AST.
    /// For cross-file inheritance (e.g., from standard library classes), the
    /// parent's fields are not included - they must be provided via `InitArgs` or
    /// handled by a future import mechanism.
    fn collect_inherited_fields(
        parent_name: &str,
        module: &Module,
    ) -> Result<Vec<(String, Expression)>> {
        let mut fields = Vec::new();

        // Base case: Actor, Object, and root classes (none) have no state fields
        if parent_name == "Actor" || parent_name == "Object" || parent_name == "none" {
            return Ok(fields);
        }

        // Find parent class in the same module
        let parent_class = module
            .classes
            .iter()
            .find(|c| c.name.name.eq_ignore_ascii_case(parent_name));

        if let Some(parent) = parent_class {
            // Recursively collect grandparent fields first
            let grandparent_fields =
                Self::collect_inherited_fields(parent.superclass_name(), module)?;
            fields.extend(grandparent_fields);

            // Add this parent's fields
            for state in &parent.state {
                let default_value = if let Some(ref val) = state.default_value {
                    val.clone()
                } else {
                    // No default - use nil
                    Expression::Identifier(Identifier {
                        name: "nil".into(),
                        span: state.span,
                    })
                };
                fields.push((state.name.name.to_string(), default_value));
            }
        }
        // If parent not found in module, it's a cross-file reference - skip for now

        Ok(fields)
    }
}
