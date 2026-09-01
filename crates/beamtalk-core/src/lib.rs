// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk compiler core.
//!
//! This crate contains the core compiler functionality:
//! - Lexical analysis (tokenization)
//! - Parsing (AST construction)
//! - Semantic analysis (type checking, name resolution)
//!
//! Code generation (Core Erlang output) lives in the standalone
//! `beamtalk-codegen` crate (ADR 0117 step 5, BT-3362), which depends on
//! this crate's Compilation context, never the reverse.
//!
//! The compiler is designed as a language service, prioritizing
//! IDE responsiveness over batch compilation speed.

#![doc = include_str!("../../../README.md")]

// BT-3361 (ADR 0117 Decision step 5): widened from `pub(crate)` — the
// Language Service context (now the standalone `beamtalk-language-service`
// crate) reaches `is_announce_selector` as the shared-leaf fact both it and
// this crate's `semantic_analysis` must agree on (see this module's own doc
// comment). It was already a public-shaped leaf beneath two DDD contexts;
// only one of those contexts used to live inside this crate, so a `pub(crate)`
// item it reached had to become genuinely `pub` once the consumer moved out.
pub mod announce_selectors;
pub mod ast;
// BT-3340: widened from `pub(crate)` — `for_each_expr_seq` is used by the
// standalone `beamtalk-lint` crate now that `lint` has moved out of this
// crate (ADR 0117 Decision step 2).
pub mod ast_walker;
pub mod compilation;
// BT-3361 (ADR 0117 Decision step 5): widened from `pub(crate)` — same
// rationale as `announce_selectors` above; `erlang_module_of_receiver` is
// reached from `beamtalk-language-service`'s `queries::ffi_sites_query`.
pub mod ffi_receiver;
pub mod ffi_type_specs;
pub mod file_walker;
// BT-3361 (ADR 0117 Decision step 5): widened from `pub(crate)` — same
// rationale as `announce_selectors` above; `selector_span` is reached from
// `beamtalk-language-service`'s `queries` module.
pub mod method_source_walker;
pub mod near_miss_divider;
pub mod semantic_analysis;
pub mod source_analysis;
pub mod span;
// BT-3362 (ADR 0117 Decision step 5): widened from `pub(crate)` — the
// standalone `beamtalk-codegen` crate's `core_erlang` reaches
// `is_exception_selector`/`is_conditional_selector` to decide state-threading
// codegen for exception handlers and conditionals.
pub mod state_threading_selectors;
pub mod synthetic_selectors;
pub mod test_helpers;
pub mod tool_expr;
pub mod unparse;

/// Re-export commonly used types.
///
/// BT-3361 (ADR 0117 Decision step 5): this used to also re-export
/// `Completion`, `CompletionKind`, `HoverInfo`, `LanguageService`, `Location`,
/// `Position`, `ProjectIndex`, and `SimpleLanguageService` from
/// `language_service` — removed since that module moved into the standalone
/// `beamtalk-language-service` crate, which this crate cannot depend on
/// without creating a cycle (Language Service depends on Compilation, never
/// the reverse). No production code imported these through `prelude` (only
/// direct `beamtalk_core::language_service::...` / `crate::queries::...`
/// paths existed), so callers that want them now import directly from
/// `beamtalk_language_service::{...}`.
pub mod prelude {
    pub use crate::ast::{
        ClassDefinition, DeclaredKeyword, Expression, Identifier, Literal, MethodDefinition,
        Module, StateDeclaration, TypeAnnotation,
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
    /// Threaded into semantic analysis via `AnalysisContext::with_options` to
    /// set the `package` field on `ClassInfo` entries built from AST source.
    /// `None` for REPL sessions and contexts where no package is active.
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
