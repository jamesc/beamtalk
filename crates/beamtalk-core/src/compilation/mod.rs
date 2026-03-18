// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Compilation passes for Beamtalk.
//!
//! **DDD Context:** Compilation
//!
//! This module contains project-wide compilation passes that operate across
//! multiple source files. Individual file analysis lives in `semantic_analysis`;
//! these passes coordinate cross-file concerns like extension method indexing
//! and conflict detection.

pub mod extension_index;

pub use extension_index::{
    ExtensionIndex, ExtensionKey, ExtensionLocation, MethodSide, collect_extensions,
};
