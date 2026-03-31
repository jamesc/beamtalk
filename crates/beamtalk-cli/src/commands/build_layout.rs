// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Centralised build path construction.
//!
//! All `_build/dev/*` paths are constructed through [`BuildLayout`] to ensure
//! consistency across commands (build, test, run, repl, deps).

use camino::Utf8PathBuf;

/// Provides all build-output paths relative to a project root.
///
/// # Layout
///
/// ```text
/// <project_root>/
///   _build/
///     dev/
///       ebin/                    — compiled .beam files
///       native/
///         ebin/                  — native Erlang .beam files
///         default/lib/           — rebar3 hex dep libs (ERL_LIBS)
///         rebar.config           — generated rebar3 config
///     deps/
///       <name>/                  — git dep checkout
///         ebin/                  — compiled dep .beam files
/// ```
#[derive(Debug, Clone)]
pub struct BuildLayout {
    project_root: Utf8PathBuf,
}

impl BuildLayout {
    /// Create a new `BuildLayout` rooted at the given project directory.
    pub fn new(project_root: impl Into<Utf8PathBuf>) -> Self {
        Self {
            project_root: project_root.into(),
        }
    }

    // ── Package output ───────────────────────────────────────────────

    /// `_build/dev/ebin/` — compiled Beamtalk `.beam` files.
    pub fn ebin_dir(&self) -> Utf8PathBuf {
        self.project_root.join("_build").join("dev").join("ebin")
    }

    // ── Native Erlang ────────────────────────────────────────────────

    /// `_build/dev/native/` — base directory for native Erlang builds.
    pub fn native_dir(&self) -> Utf8PathBuf {
        self.project_root.join("_build").join("dev").join("native")
    }

    /// `_build/dev/native/ebin/` — compiled native `.erl` → `.beam` files.
    pub fn native_ebin_dir(&self) -> Utf8PathBuf {
        self.native_dir().join("ebin")
    }

    /// `_build/dev/native/default/lib/` — rebar3 hex dep libs (`ERL_LIBS`).
    pub fn rebar_lib_dir(&self) -> Utf8PathBuf {
        self.native_dir().join("default").join("lib")
    }

    // ── Dependencies ─────────────────────────────────────────────────

    /// `_build/deps/` — top-level dependency directory.
    pub fn deps_dir(&self) -> Utf8PathBuf {
        self.project_root.join("_build").join("deps")
    }

    /// `_build/deps/<name>/` — checkout directory for a single dependency.
    pub fn dep_checkout_dir(&self, name: &str) -> Utf8PathBuf {
        self.deps_dir().join(name)
    }

    /// `_build/deps/<name>/ebin/` — compiled `.beam` files for a dependency.
    pub fn dep_ebin_dir(&self, name: &str) -> Utf8PathBuf {
        self.dep_checkout_dir(name).join("ebin")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ebin_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.ebin_dir(), "/home/user/my_app/_build/dev/ebin");
    }

    #[test]
    fn test_native_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.native_dir(), "/home/user/my_app/_build/dev/native");
    }

    #[test]
    fn test_native_ebin_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.native_ebin_dir(),
            "/home/user/my_app/_build/dev/native/ebin"
        );
    }

    #[test]
    fn test_rebar_lib_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.rebar_lib_dir(),
            "/home/user/my_app/_build/dev/native/default/lib"
        );
    }

    #[test]
    fn test_deps_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.deps_dir(), "/home/user/my_app/_build/deps");
    }

    #[test]
    fn test_dep_checkout_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.dep_checkout_dir("utils"),
            "/home/user/my_app/_build/deps/utils"
        );
    }

    #[test]
    fn test_dep_ebin_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.dep_ebin_dir("utils"),
            "/home/user/my_app/_build/deps/utils/ebin"
        );
    }
}
