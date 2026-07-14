// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler core.
//!
//! This crate contains the core compiler functionality:
//! - Lexical analysis (tokenization)
//! - Parsing (AST construction)
//! - Semantic analysis (type checking, name resolution)
//! - Code generation (Core Erlang output)
//!
//! The compiler is designed as a language service, prioritizing
//! IDE responsiveness over batch compilation speed.

#![doc = include_str!("../../../README.md")]

pub mod ast;
pub(crate) mod ast_walker;
pub mod codegen;
pub mod compilation;
pub mod erlang;
pub mod ffi_type_specs;
pub mod file_walker;
pub mod language_service;
pub mod lint;
pub mod project;
pub mod queries;
pub mod repl;
pub mod semantic_analysis;
pub mod source_analysis;
pub(crate) mod state_threading_selectors;
pub(crate) mod synthetic_selectors;
pub mod test_helpers;
pub mod unparse;

/// Re-export commonly used types.
pub mod prelude {
    pub use crate::ast::{
        ClassDefinition, DeclaredKeyword, Expression, Identifier, Literal, MethodDefinition,
        Module, StateDeclaration, TypeAnnotation,
    };
    pub use crate::language_service::{
        Completion, CompletionKind, HoverInfo, LanguageService, Location, Position, ProjectIndex,
        SimpleLanguageService,
    };
    pub use crate::source_analysis::Span;
}

/// Compiler options controlling semantic analysis and code generation.
///
/// These flags control how the compiler handles stdlib-specific features
/// like `@primitive` pragmas (ADR 0007) and workspace bindings (ADR 0010).
#[derive(Debug, Clone, Default)]
#[allow(clippy::struct_excessive_bools)] // Config struct — bools are appropriate here
pub struct CompilerOptions {
    /// When true, the module being compiled is part of the standard library.
    /// Enables `@primitive` pragmas without warnings.
    pub stdlib_mode: bool,

    /// When true, allows `@primitive` pragmas in non-stdlib code.
    /// Emits a warning instead of an error.
    pub allow_primitives: bool,

    /// BT-374 / ADR 0010 / ADR 0019: Whether workspace bindings are available.
    /// When true, class references resolve through session bindings or class
    /// registry. When false (batch compile), they go directly to the registry.
    pub workspace_mode: bool,

    /// When true, suppress warning diagnostics during compilation.
    /// Useful for test fixtures that intentionally trigger warnings.
    pub suppress_warnings: bool,

    /// When true, treat warnings and hints as errors — compilation fails if any are emitted.
    pub warnings_as_errors: bool,

    /// BT-979: When true, skip the effect-free lint check on `module.expressions`.
    ///
    /// Set this for bootstrap-test compilation, where top-level expressions are
    /// intentional test assertions (paired with `// =>` comments) rather than
    /// accidentally discarded values. Defaults to false so the REPL and normal
    /// `beamtalk build` / `beamtalk lint` paths all get the check.
    pub skip_module_expression_lint: bool,

    /// The package name of the module being compiled (ADR 0071, BT-1700).
    ///
    /// Used by `analyse_with_packages` to set the `package` field on
    /// `ClassInfo` entries built from AST source. `None` for REPL sessions
    /// and contexts where no package is active.
    pub current_package: Option<String>,

    /// How complete the cross-file knowledge injected into analysis is
    /// (BT-2796, ADR 0100 Rule 2 sequencing guard).
    ///
    /// Defaults to [`semantic_analysis::KnowledgeScope::ModuleOnly`]. Set to
    /// `ProjectComplete` only by orchestrators that walked the entire project
    /// (CLI build Pass 1, lint with a package root, the LSP after workspace
    /// preload) so the receiver-knowledge classifier can distinguish
    /// "parent missing because single-file analysis" from "parent missing
    /// because genuinely unresolved".
    pub knowledge_scope: semantic_analysis::KnowledgeScope,

    /// Whether the package being compiled declares dependencies (BT-2794).
    ///
    /// Pre-WS3 (ADR 0070 amendment), dependency extension contributions are
    /// invisible to the checker, and a dependency can extend any class —
    /// including `Object`. When true and `knowledge_scope` is
    /// `ProjectComplete`, the receiver-knowledge classifier keeps every
    /// receiver `Open` (no unresolved-selector hints) rather than risk
    /// hinting on a dependency-contributed method that genuinely exists
    /// (ADR 0100 Rule 1's third downgrade).
    pub has_package_dependencies: bool,
}
