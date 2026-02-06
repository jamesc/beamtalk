// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in operation code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! This module handles code generation for built-in operations, organized by
//! receiver type:
//! - [`operators`] — Binary operators (arithmetic, comparison, string)
//! - [`integer`] — Integer-specific methods
//! - [`boolean`] — Boolean-specific methods
//! - [`dictionary`] — Dictionary/map operations
//! - [`string`] — String operations
//! - [`collections`] — Block, list, proto-object, and object methods

mod boolean;
mod collections;
mod dictionary;
mod integer;
mod operators;
mod string;
