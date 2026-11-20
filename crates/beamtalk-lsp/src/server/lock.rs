// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Poison-tolerant locking for the LSP server (BT-5078).
//!
//! A handler that panics while holding one of the server's mutexes poisons
//! it. With a plain `.lock().unwrap()` every later request would then
//! panic too, so one compiler bug reachable from a half-typed buffer would
//! take the whole server down. The state behind these locks is plain data
//! (maps, caches, the analysis service) that is re-derived from the editor's
//! buffers, so recovering the guard and carrying on is correct; the panic
//! itself is reported to the offending request by [`super::guard`].

use std::sync::{Mutex, MutexGuard, PoisonError};

/// `Mutex::lock` that recovers the guard from a poisoned lock.
pub(in crate::server) trait LockRecover<T> {
    fn lock_recover(&self) -> MutexGuard<'_, T>;
}

impl<T> LockRecover<T> for Mutex<T> {
    fn lock_recover(&self) -> MutexGuard<'_, T> {
        self.lock().unwrap_or_else(PoisonError::into_inner)
    }
}
