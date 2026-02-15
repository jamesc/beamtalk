// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Query modules for language service operations.
//!
//! This module provides domain services (Providers) from the Language Service
//! context in the DDD model. Each provider implements a specific language
//! service capability:
//!
//! - [`completion_provider`] - Suggest completions at cursor position
//! - [`diagnostic_provider`] - Collect errors and warnings
//! - [`document_symbols_provider`] - Return document outline symbols
//! - [`hover_provider`] - Show information on hover
//!
//! **DDD Context:** Language Service
//!
//! These providers follow the DDD ubiquitous language where "Provider" is
//! the standard term for domain services that compute language service
//! responses. This aligns with LSP terminology (`CompletionProvider`,
//! `DiagnosticProvider`, `HoverProvider`) and makes the code self-documenting.
//!
//! ## References
//!
//! - DDD model: `docs/beamtalk-ddd-model.md` (Language Service Context)
//! - LSP specification: Language Server Protocol

pub mod completion_provider;
pub mod diagnostic_provider;
pub mod document_symbols_provider;
pub mod hover_provider;
