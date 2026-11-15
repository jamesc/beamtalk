// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build the Beamtalk standard library.
//!
//! **DDD Context:** CLI / Build System
//!
//! Compiles all `lib/*.bt` files through the normal pipeline with `--stdlib-mode`
//! and outputs `.beam` files to `runtime/apps/beamtalk_stdlib/ebin/`.
//! Uses incremental builds: skips compilation if all outputs are newer than
//! all inputs (source files, compiler binary, and runtime `.beam` files).
//!
//! Part of ADR 0007 (Compilable Stdlib with Primitive Injection).

//! # Layout
//!
//! - [`orchestrate`]: the `build_stdlib` entry point, file discovery, and the
//!   incremental up-to-date check.
//! - [`metadata`]: the `ClassMeta` model plus class/alias/protocol extraction.
//! - [`generators`]: `generated_builtins.rs`, the `.hrl` headers, and the
//!   `.app` / `.app.src` files.

mod generators;
mod metadata;
mod orchestrate;

#[cfg(test)]
mod tests;

pub use orchestrate::build_stdlib;
