// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Experimental parser using chumsky parser combinators.
//!
//! This module provides an alternative parser implementation using the `chumsky`
//! library to evaluate parser combinator approaches for Beamtalk.
//!
//! # Feature Gate
//!
//! This module requires the `chumsky-parser` feature:
//! ```toml
//! beamtalk-core = { version = "0.1", features = ["chumsky-parser"] }
//! ```
//!
//! # Evaluation Summary
//!
//! After evaluating chumsky 1.0.0-alpha.8, key observations:
//!
//! ## Chumsky's Approach to Precedence
//!
//! Chumsky uses Pratt parsing internally via its `pratt()` combinator. This means:
//! - **Same algorithm**: Both our Pratt prototype and chumsky use the same underlying technique
//! - **Table-driven**: Operators defined with `infix(left(N), op, fold_fn)` where N is precedence
//! - **Adding operators**: Similar effort to our hand-rolled Pratt parser
//!
//! ## Error Recovery
//!
//! Chumsky provides `recover_with()` combinators for error recovery:
//! - `skip_then_retry_until()` - skip tokens and retry
//! - `skip_until()` - skip to synchronization point
//! - Quality depends on careful combinator placement
//!
//! **Trade-off**: Our hand-rolled parser has purpose-built synchronization points
//! at statement boundaries. Achieving equivalent recovery in chumsky requires
//! careful combinator design and may be less intuitive.
//!
//! ## Trivia Handling
//!
//! Chumsky parses token streams, not raw text. Trivia handling options:
//! 1. Pre-tokenize with our lexer, adapt tokens (preserves trivia)
//! 2. Use chumsky's `text` module (loses fine-grained trivia control)
//!
//! **Trade-off**: We'd need a token adapter layer, adding complexity.
//!
//! ## IDE Integration
//!
//! Chumsky provides spans via `map_with_span()` or `SimpleSpan`. However:
//! - Span granularity depends on token representation
//! - Converting from chumsky spans to our `Span` type adds overhead
//!
//! ## Maintainability
//!
//! Pros:
//! - Declarative grammar definition
//! - Composable parsers
//! - Built-in common patterns
//!
//! Cons:
//! - Complex generic types (error messages can be cryptic)
//! - API instability (1.0.0-alpha has breaking changes)
//! - Learning curve for error recovery patterns
//! - Debugging parser issues is harder than stepping through recursive descent
//!
//! ## Lines of Code Comparison
//!
//! For binary operator precedence alone:
//! - Current parser: ~80 lines (parse_comparison + parse_additive + parse_multiplicative)
//! - Pratt prototype: ~60 lines (binding power table + single parse function)
//! - Chumsky: ~40 lines (pratt combinator + operator definitions)
//!
//! However, total parser complexity would be similar because:
//! - Message send syntax (unary, binary, keyword) is unusual
//! - Cascade handling requires custom logic regardless
//! - Block parameters and trivia need careful handling
//!
//! ## Recommendation
//!
//! **Do not adopt chumsky for Beamtalk.** Reasons:
//!
//! 1. **Same algorithm**: Chumsky uses Pratt parsing, which we can implement directly
//! 2. **Control**: Hand-rolled parser gives full control over error recovery
//! 3. **Stability**: Chumsky 1.0 is alpha; API may change
//! 4. **Debugging**: Recursive descent is easier to debug and understand
//! 5. **Dependency**: One less dependency to maintain
//!
//! The Pratt parsing prototype in `parser_pratt.rs` provides the same benefits
//! (table-driven precedence) without the abstraction overhead.

#![cfg(feature = "chumsky-parser")]

// Placeholder module to document findings.
// Full implementation would require significant work to:
// 1. Adapt our Token type for chumsky's Input trait
// 2. Map spans between chumsky and our Span type
// 3. Implement error recovery that matches our current quality
//
// This is not worth the effort given chumsky uses the same Pratt
// algorithm internally, which we've already prototyped directly.
