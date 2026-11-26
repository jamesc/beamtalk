// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-request panic containment for the LSP server (BT-5078).
//!
//! Analysis runs compiler code over half-typed user text, so a parser or
//! type-checker bug can panic inside a handler. Left alone, that unwinds
//! through the service. Every `LanguageServer` method is run through
//! [`guard_request`] / [`guard_notification`], which catches the panic, logs
//! it, and turns it into a JSON-RPC internal error for *that request only*.
//! The caught panic is never converted into an empty or "successful" answer.
//!
//! Locks held when the panic unwound are poisoned; [`super::lock`] recovers
//! them on the next use, so the server stays usable.

use std::any::Any;
use std::future::Future;
use std::panic::AssertUnwindSafe;

use futures_util::FutureExt;
use tower_lsp::jsonrpc::{Error, Result};

pub(in crate::server) fn panic_message(payload: &(dyn Any + Send)) -> String {
    payload
        .downcast_ref::<&str>()
        .map(|s| (*s).to_string())
        .or_else(|| payload.downcast_ref::<String>().cloned())
        .unwrap_or_else(|| "non-string panic payload".to_string())
}

/// Runs a request handler, converting a panic into an LSP internal error.
pub(in crate::server) async fn guard_request<T>(
    method: &'static str,
    handler: impl Future<Output = Result<T>>,
) -> Result<T> {
    match AssertUnwindSafe(handler).catch_unwind().await {
        Ok(result) => result,
        Err(payload) => {
            let msg = panic_message(payload.as_ref());
            tracing::error!(method, panic = %msg, "LSP handler panicked");
            let mut err = Error::internal_error();
            err.message = format!("internal error in `{method}`: {msg}").into();
            Err(err)
        }
    }
}

/// Runs a notification handler, logging and swallowing a panic (notifications
/// have no response to carry an error).
pub(in crate::server) async fn guard_notification(
    method: &'static str,
    handler: impl Future<Output = ()>,
) {
    if let Err(payload) = AssertUnwindSafe(handler).catch_unwind().await {
        let msg = panic_message(payload.as_ref());
        tracing::error!(method, panic = %msg, "LSP notification handler panicked");
    }
}
