// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Supervisor kind writeback pass (BT-1218, ADR 0059 Phase 1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! After the class hierarchy is built, this pass walks class definitions
//! and sets `supervisor_kind` to `Static` or `Dynamic` for classes that
//! inherit from `Supervisor` or `DynamicSupervisor` respectively.
//!
//! This information is consumed by:
//! - **Codegen** (Phase 2): routing Supervisor/DynamicSupervisor subclasses
//!   away from `gen_server` code generation to `supervisor_codegen`.

use crate::ast::{Module, SupervisorKind};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;

/// Writes `supervisor_kind` into each `ClassDefinition` that inherits from
/// `Supervisor` or `DynamicSupervisor`.
///
/// Must run **after** the class hierarchy is built (so ancestry lookups work)
/// and **before** codegen (so the `supervisor_kind` field is populated).
pub fn apply_supervisor_kind_writeback(module: &mut Module, hierarchy: &ClassHierarchy) {
    for class in &mut module.classes {
        let name = class.name.name.as_str();
        if hierarchy.is_supervisor_subclass(name) {
            class.supervisor_kind = Some(SupervisorKind::Static);
        } else if hierarchy.is_dynamic_supervisor_subclass(name) {
            class.supervisor_kind = Some(SupervisorKind::Dynamic);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{lex_with_eof, parse};

    #[test]
    fn test_actor_subclass_has_no_supervisor_kind() {
        let source = "Actor subclass: Counter";
        let tokens = lex_with_eof(source);
        let (mut module, _) = parse(tokens);
        let (hierarchy_result, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy_result.unwrap();
        apply_supervisor_kind_writeback(&mut module, &hierarchy);
        assert_eq!(module.classes[0].supervisor_kind, None);
    }

    #[test]
    fn test_supervisor_subclass_gets_static_kind() {
        let source = "Supervisor subclass: WebApp";
        let tokens = lex_with_eof(source);
        let (mut module, _) = parse(tokens);
        let (hierarchy_result, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy_result.unwrap();
        apply_supervisor_kind_writeback(&mut module, &hierarchy);
        assert_eq!(
            module.classes[0].supervisor_kind,
            Some(SupervisorKind::Static)
        );
    }

    #[test]
    fn test_dynamic_supervisor_subclass_gets_dynamic_kind() {
        let source = "DynamicSupervisor subclass: WorkerPool";
        let tokens = lex_with_eof(source);
        let (mut module, _) = parse(tokens);
        let (hierarchy_result, _) = ClassHierarchy::build_with_options(&module, true);
        let hierarchy = hierarchy_result.unwrap();
        apply_supervisor_kind_writeback(&mut module, &hierarchy);
        assert_eq!(
            module.classes[0].supervisor_kind,
            Some(SupervisorKind::Dynamic)
        );
    }
}
