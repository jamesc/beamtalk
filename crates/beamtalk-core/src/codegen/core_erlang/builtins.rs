// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Built-in operation code generation.
//!
//! This module handles code generation for built-in operations like:
//! - Block evaluation (`value`, `whileTrue:`, `whileFalse:`, `repeat`)
//! - Dictionary operations (`at:`, `at:put:`, etc.)
//! - Boolean operations (`ifTrue:`, `ifFalse:`, etc.)
//! - Integer arithmetic (`+`, `-`, `*`, `/`, etc.)
//! - String operations (`++`, `size`, etc.)
//! - Binary operators with standard math precedence
//!
//! # Future Work
//!
//! This module is a stub. Built-in operation methods should be extracted from
//! `mod.rs` following the pattern established in `expressions.rs` and `gen_server.rs`:
//!
//! 1. Move `try_generate_block_message` and related block operations
//! 2. Move `try_generate_dictionary_message` and map operations  
//! 3. Move `try_generate_boolean_message` and boolean control flow
//! 4. Move `try_generate_integer_message` and arithmetic operations
//! 5. Move `try_generate_string_message` and string operations
//! 6. Move `try_generate_list_message` and collection operations
//! 7. Move `generate_binary_op` for binary operators
//!
//! Each extraction should be followed by running tests to ensure correctness.
