// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Snapshot tests for the beamtalk compiler.
//!
//! This crate provides integration tests that validate compiler output
//! across all compilation stages: lexing, parsing, and code generation.
//!
//! Tests are organized in the `cases/` directory, with each subdirectory
//! representing a test case. The build script generates test functions
//! from these directories.
