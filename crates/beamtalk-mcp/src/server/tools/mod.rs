// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! MCP tool implementations, one module per tool family. Each family adds
//! an `impl BeamtalkMcp` block wired up via `#[tool_router(router = ...)]`;
//! `server::BeamtalkMcp::tool_router()` sums all of them into the router the
//! server actually dispatches through.

pub(crate) mod diagnostics;
pub(crate) mod docs;
pub(crate) mod editing;
pub(crate) mod evaluate;
pub(crate) mod flush;
pub(crate) mod traces;
