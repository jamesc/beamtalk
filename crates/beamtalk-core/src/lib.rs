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
pub mod erlang;
pub mod language_service;
pub mod lint;
pub mod project;
pub mod queries;
pub mod semantic_analysis;
pub mod source_analysis;
pub mod test_helpers;
pub mod unparse;

/// Re-export commonly used types.
pub mod prelude {
    pub use crate::ast::{
        ClassDefinition, Expression, Identifier, Literal, MethodDefinition, Module,
        StateDeclaration, TypeAnnotation,
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

    /// BT-979: When true, skip the effect-free lint check on `module.expressions`.
    ///
    /// Set this for bootstrap-test compilation, where top-level expressions are
    /// intentional test assertions (paired with `// =>` comments) rather than
    /// accidentally discarded values. Defaults to false so the REPL and normal
    /// `beamtalk build` / `beamtalk lint` paths all get the check.
    pub skip_module_expression_lint: bool,
}
