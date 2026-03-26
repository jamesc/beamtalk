// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency specification types for the Beamtalk package system (ADR 0070).
//!
//! **DDD Context:** Compilation
//!
//! These types represent parsed dependency declarations from `beamtalk.toml`.
//! Phase 1 supports two dependency sources:
//! - **Path dependencies:** local filesystem paths (for monorepo/development)
//! - **Git dependencies:** remote git repositories with tag, branch, or rev pinning

use std::collections::BTreeMap;
use std::fmt;
use std::path::PathBuf;

/// A single dependency declaration from the `[dependencies]` section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DependencySpec {
    /// The dependency's package name (validated against package naming rules).
    pub name: String,
    /// Where to fetch the dependency source from.
    pub source: DependencySource,
}

/// The source location for a dependency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencySource {
    /// A local filesystem path dependency.
    ///
    /// ```toml
    /// utils = { path = "../my-utils" }
    /// ```
    Path {
        /// Path to the dependency's root directory (containing `beamtalk.toml`).
        path: PathBuf,
    },

    /// A git repository dependency.
    ///
    /// ```toml
    /// json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
    /// ```
    Git {
        /// The git repository URL.
        url: String,
        /// The git reference to check out.
        reference: GitReference,
    },
}

/// A git reference for pinning a dependency to a specific point in history.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitReference {
    /// A git tag (e.g. `"v1.0.0"`).
    Tag(String),
    /// A git branch (e.g. `"main"`).
    Branch(String),
    /// An exact commit SHA (e.g. `"abc1234"`).
    Rev(String),
}

impl fmt::Display for DependencySource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Path { path } => write!(f, "path: {}", path.display()),
            Self::Git { url, reference } => {
                write!(f, "git: {url}")?;
                match reference {
                    GitReference::Tag(tag) => write!(f, " (tag: {tag})"),
                    GitReference::Branch(branch) => write!(f, " (branch: {branch})"),
                    GitReference::Rev(rev) => write!(f, " (rev: {rev})"),
                }
            }
        }
    }
}

impl fmt::Display for DependencySpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.source)
    }
}

/// An ordered collection of dependency specifications.
///
/// Uses `BTreeMap` for deterministic iteration order (alphabetical by name).
pub type DependencyMap = BTreeMap<String, DependencySpec>;
