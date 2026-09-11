// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Semantic analysis tests, split by concern (see
//! `docs/development/rust-guidelines.md` § Testing). `test_helpers` and
//! `property_tests`, declared as siblings in `semantic_analysis/mod.rs`,
//! are unaffected by this split.
//!
//! - [`analyser_core_and_block_context`] — core `Analyser` fundamentals
//!   and block-context classification
//! - [`match_arm_analysis`] — `match:` arm pattern binding and scoping
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

use crate::ast::{
    Block, BlockParameter, ClassDefinition, ClassKind, CommentAttachment, DeclaredKeyword,
    Expression, ExpressionStatement, Identifier, Literal, MatchArm, MessageSelector,
    MethodDefinition, Pattern, StateDeclaration, StringSegment, TypeAnnotation,
};

use crate::source_analysis::{Severity, Span};

fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

mod abstract_and_native_instantiation;
mod adr0103_handle_scope_and_sendability;
mod analyser_core_and_block_context;
mod class_hierarchy_and_method_validators;
mod dead_code_super_and_shadowing;
mod extension_method_integration;
mod hierarchy_injection_and_singleton_types;
mod match_arm_analysis;
mod self_misuse_and_unused_variables;
mod typed_params_knowledge_scope_and_cross_file;
mod workspace_bindings_protocols_and_aliases;
