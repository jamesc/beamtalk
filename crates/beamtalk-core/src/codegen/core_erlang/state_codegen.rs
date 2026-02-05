// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State threading for mutable actor fields.
//!
//! In Beamtalk, actor fields are conceptually mutable, but Core Erlang is
//! purely functional. We simulate mutation by threading state variables
//! through field assignments:
//!
//! ```beamtalk
//! self.counter := 0.
//! self.counter := self.counter + 1.
//! ```
//!
//! Compiles to:
//!
//! ```erlang
//! let State1 = call 'maps':'put'('counter', 0, State) in
//! let State2 = call 'maps':'put'('counter',
//!                  call 'maps':'get'('counter', State1) + 1, State1) in
//! ...
//! ```
//!
//! # State Variable Naming
//!
//! - **State** (version 0): The original state passed to the method
//! - **State1** (version 1): State after first field assignment
//! - **State2** (version 2): State after second assignment
//! - etc.
//!
//! # Version Management
//!
//! The [`StateThreading`] service tracks the current version and provides
//! methods to get current/next state variable names.

/// State threading service for tracking state variable versions.
///
/// This service manages the incrementing counter used to generate unique
/// state variable names (State, State1, State2, ...) through field assignments.
#[derive(Debug)]
pub(super) struct StateThreading {
    /// Current state version counter.
    /// Version 0 = "State", 1 = "State1", 2 = "State2", etc.
    version: usize,
}

impl StateThreading {
    /// Creates a new state threading service starting at version 0.
    pub(super) fn new() -> Self {
        Self { version: 0 }
    }

    /// Returns the current version number.
    pub(super) fn version(&self) -> usize {
        self.version
    }

    /// Returns the current state variable name.
    ///
    /// - Version 0 → `"State"`
    /// - Version 1 → `"State1"`
    /// - Version 2 → `"State2"`
    /// - etc.
    pub(super) fn current_var(&self) -> String {
        if self.version == 0 {
            "State".to_string()
        } else {
            format!("State{}", self.version)
        }
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// Call this when generating a field assignment to get the name for
    /// the state after the update.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let mut state = StateThreading::new();
    /// assert_eq!(state.current_var(), "State");
    /// assert_eq!(state.next_var(), "State1");
    /// assert_eq!(state.current_var(), "State1");
    /// ```
    pub(super) fn next_var(&mut self) -> String {
        self.version += 1;
        self.current_var()
    }

    /// Resets the state version to 0.
    ///
    /// Call this at the start of each method body, or when entering a
    /// nested context that needs independent state tracking.
    pub(super) fn reset(&mut self) {
        self.version = 0;
    }

    /// Sets the version to a specific value.
    ///
    /// This is used when saving/restoring version across nested contexts.
    pub(super) fn set_version(&mut self, version: usize) {
        self.version = version;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_state_is_version_zero() {
        let state = StateThreading::new();
        assert_eq!(state.version(), 0);
        assert_eq!(state.current_var(), "State");
    }

    #[test]
    fn test_next_var_increments_version() {
        let mut state = StateThreading::new();
        assert_eq!(state.current_var(), "State");
        assert_eq!(state.next_var(), "State1");
        assert_eq!(state.version(), 1);
        assert_eq!(state.current_var(), "State1");
        assert_eq!(state.next_var(), "State2");
        assert_eq!(state.version(), 2);
    }

    #[test]
    fn test_reset_returns_to_version_zero() {
        let mut state = StateThreading::new();
        state.next_var();
        state.next_var();
        assert_eq!(state.version(), 2);
        state.reset();
        assert_eq!(state.version(), 0);
        assert_eq!(state.current_var(), "State");
    }

    #[test]
    fn test_set_version() {
        let mut state = StateThreading::new();
        state.set_version(5);
        assert_eq!(state.version(), 5);
        assert_eq!(state.current_var(), "State5");
    }
}
