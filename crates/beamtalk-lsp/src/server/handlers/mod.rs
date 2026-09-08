// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-capability `LanguageServer` handler bodies, as inherent `impl Backend`
//! blocks. `server::mod`'s `impl LanguageServer for Backend` dispatches each
//! trait method to the matching `handle_*` method defined here.

mod hierarchy;
mod lifecycle;
mod navigation;
mod symbols;
