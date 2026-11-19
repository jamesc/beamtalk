// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis tests, split by concern (see
//! `docs/development/rust-guidelines.md` § Testing). `test_helpers` and
//! `property_tests`, declared as siblings in `semantic_analysis/mod.rs`,
//! are unaffected by this split.
//!
//! - [`analyser_core_and_block_context`] — core `Analyser` fundamentals
//!   and block-context classification
//! - [`bt4573_setup_cascade_single_diagnostic`] — a `setUp` multi-`with*:`
//!   cascade draws only `cascade-drops-update` (BT-4573)
//! - [`class_state_abroad`] — ADR 0130 §5 `class-state-abroad` lint (BT-3712)
//! - [`local_threading`] — ADR 0131 §6 and Phase 0 outer-local threading
//!   errors (BT-3745)
//! - [`match_arm_analysis`] — `match:` arm pattern binding and scoping
//! - [`match_arm_block`] — BT-4028 `match-arm-block` warning wiring
//! - [`class_hierarchy_and_method_validators`] — `ClassHierarchy` and
//!   reflection-method validator integration
//! - [`self_misuse_and_unused_variables`] — `self`-misuse and unused-
//!   variable/parameter diagnostics
//! - [`dead_code_super_and_shadowing`] — dead-code, `super`-misuse, and
//!   shadowing diagnostics
//! - [`abstract_and_native_instantiation`] — abstract-class and
//!   `native:`-class instantiation diagnostics
//! - [`hierarchy_injection_and_singleton_types`] — ADR 0050 hierarchy
//!   injection and singleton-union type annotations
//! - [`adr0103_handle_scope_and_sendability`] — `handleScope:` validity
//!   and block-capture sendability
//! - [`extension_method_integration`] — extension methods vs. the type
//!   checker
//! - [`workspace_bindings_protocols_and_aliases`] — workspace-binding
//!   shadowing, protocol name resolution, and ADR 0108 alias seeding
//! - [`typed_params_knowledge_scope_and_cross_file`] — typed block
//!   parameters, `KnowledgeScope`, and cross-file extension visibility

use super::test_helpers::test_span;
use super::*;
use crate::test_helpers::test_support::parse_bt;

use crate::ast::{
    Block, BlockParameter, ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword,
    Expression, Identifier, Literal, MatchArm, MessageSelector, MethodDefinition, Pattern,
    StateDeclaration, StringSegment, TypeAnnotation,
};

use crate::source_analysis::{Severity, Span};

use crate::test_helpers::test_support::{bare, empty_module, module_with_classes};

/// The classes `src` declares, as `package` exports them: the cross-file
/// declarations a build hands `analyse_full` for a dependency (BT-4283).
fn declared_by(package: &str, src: &str) -> Vec<class_hierarchy::ClassInfo> {
    let mut infos = ClassHierarchy::extract_class_infos(&parse_bt(src));
    ClassHierarchy::stamp_package_on_infos(&mut infos, package);
    infos
}

mod abstract_and_native_instantiation;
mod adr0103_handle_scope_and_sendability;
mod adr0123_shape_version_and_migrations;
mod analyser_core_and_block_context;
mod bt4573_setup_cascade_single_diagnostic;
mod bt5025_cascade_later_messages;
mod bt5026_state_defaults_and_protocol_bodies;
mod class_hierarchy_and_method_validators;
mod class_state_abroad;
mod cross_file_extension_checks;
mod dead_code_super_and_shadowing;
mod extension_method_integration;
mod flattened_provision_diagnostics;
mod hierarchy_injection_and_singleton_types;
mod local_threading;
mod match_arm_analysis;
mod match_arm_block;
mod native_actor_subclass;
mod paren_operand_spans;
mod self_misuse_and_unused_variables;
mod standalone_e0402;
mod typed_params_knowledge_scope_and_cross_file;
mod workspace_bindings_protocols_and_aliases;
