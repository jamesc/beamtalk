// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared re-export for `beamtalk-language-service` per-feature tests.
//!
//! Every child test module reaches the crate root's public and
//! crate-visible surface (`SimpleLanguageService`, `Position`,
//! `CompletionKind`, ...) via `use super::common::*;`.

pub(super) use super::super::*;
