// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dependency specification types for the Beamtalk package system (ADR 0070).
//!
//! **DDD Context:** Compilation
//!
//! These types represent parsed dependency declarations from `beamtalk.toml`.
//! Three dependency sources are supported:
//! - **Path dependencies:** local filesystem paths (for monorepo/development)
//! - **Git dependencies:** remote git repositories with tag, branch, or rev pinning
//! - **Registry dependencies:** an exact version resolved through the package
//!   registry index into a `(git url, tag)` pair

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

    /// A registry dependency, declared as a bare exact version string.
    ///
    /// ```toml
    /// yaml = "0.2.1"
    /// ```
    ///
    /// The version is looked up in the registry index (`packages/<name>.toml`)
    /// to produce a `(git url, tag)` pair, which then flows through the same
    /// machinery as a [`DependencySource::Git`] dependency.
    Registry {
        /// The exact requested version (`major.minor.patch`).
        version: String,
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
            Self::Registry { version } => write!(f, "registry: {version}"),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    // DependencySource::fmt — Path variant
    #[test]
    fn display_path_source() {
        let source = DependencySource::Path {
            path: PathBuf::from("../my-utils"),
        };
        assert_eq!(source.to_string(), "path: ../my-utils");
    }

    // DependencySource::fmt — Git + Tag variant
    #[test]
    fn display_git_tag_source() {
        let source = DependencySource::Git {
            url: "https://github.com/jamesc/beamtalk-json".to_string(),
            reference: GitReference::Tag("v1.0.0".to_string()),
        };
        assert_eq!(
            source.to_string(),
            "git: https://github.com/jamesc/beamtalk-json (tag: v1.0.0)"
        );
    }

    // DependencySource::fmt — Git + Branch variant (was uncovered)
    #[test]
    fn display_git_branch_source() {
        let source = DependencySource::Git {
            url: "https://github.com/jamesc/beamtalk-json".to_string(),
            reference: GitReference::Branch("main".to_string()),
        };
        assert_eq!(
            source.to_string(),
            "git: https://github.com/jamesc/beamtalk-json (branch: main)"
        );
    }

    // DependencySource::fmt — Git + Rev variant (was uncovered)
    #[test]
    fn display_git_rev_source() {
        let source = DependencySource::Git {
            url: "https://github.com/jamesc/beamtalk-json".to_string(),
            reference: GitReference::Rev("abc1234".to_string()),
        };
        assert_eq!(
            source.to_string(),
            "git: https://github.com/jamesc/beamtalk-json (rev: abc1234)"
        );
    }

    // DependencySource::fmt — Registry variant
    #[test]
    fn display_registry_source() {
        let source = DependencySource::Registry {
            version: "0.2.1".to_string(),
        };
        assert_eq!(source.to_string(), "registry: 0.2.1");
    }

    // DependencySpec::fmt (was entirely uncovered)
    #[test]
    fn display_dependency_spec_with_path() {
        let spec = DependencySpec {
            name: "my-utils".to_string(),
            source: DependencySource::Path {
                path: PathBuf::from("../my-utils"),
            },
        };
        assert_eq!(spec.to_string(), "my-utils (path: ../my-utils)");
    }

    #[test]
    fn display_dependency_spec_with_registry() {
        let spec = DependencySpec {
            name: "yaml".to_string(),
            source: DependencySource::Registry {
                version: "1.0.0".to_string(),
            },
        };
        assert_eq!(spec.to_string(), "yaml (registry: 1.0.0)");
    }
}
