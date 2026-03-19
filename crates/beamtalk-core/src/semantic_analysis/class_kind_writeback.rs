// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class kind writeback pass (BT-1534).
//!
//! **DDD Context:** Semantic Analysis
//!
//! After the class hierarchy is built, this pass corrects `class_kind` on
//! class definitions whose *indirect* superclass is `Value` or `Actor`.
//!
//! The parser sets `class_kind` based on the direct superclass name
//! (`from_superclass_name`), so `TestCase subclass: MyTest` gets
//! `ClassKind::Object` even though `TestCase` inherits from Value.
//! This pass walks the hierarchy and fixes those.
//!
//! Consumed by codegen: `compute_auto_slot_methods` checks `class_kind`
//! to decide whether to generate `withX:` setters and keyword constructors.

use crate::ast::Module;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;

/// Corrects `class_kind` for classes whose ancestry includes Value or Actor
/// but whose direct superclass name is neither.
///
/// Must run **after** the class hierarchy is built and **before** codegen.
pub fn apply_class_kind_writeback(module: &mut Module, hierarchy: &ClassHierarchy) {
    for class in &mut module.classes {
        let name = class.name.name.as_str();
        let resolved = hierarchy.resolve_class_kind(name);
        if class.class_kind != resolved {
            class.class_kind = resolved;
        }
    }
}
