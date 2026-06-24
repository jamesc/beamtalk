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

    // ── Roots ────────────────────────────────────────────────────────

    /// `_build/` — the top-level build-output root for this project.
    ///
    /// Everything `clean` may remove lives under this directory. Nothing
    /// outside it (source, `native/` source, `beamtalk.toml`, …) is ever a
    /// build artifact.
    pub fn build_root(&self) -> Utf8PathBuf {
        self.project_root.join("_build")
    }

    /// `_build/dev/` — the build output for the `dev` profile.
    pub fn profile_dir(&self) -> Utf8PathBuf {
        self.build_root().join("dev")
    }

    // ── Package output ───────────────────────────────────────────────

    /// `_build/dev/ebin/` — compiled Beamtalk `.beam` files.
    pub fn ebin_dir(&self) -> Utf8PathBuf {
        self.profile_dir().join("ebin")
    }

    /// `_build/dev/.beamtalk-stamp.json` — the project's build provenance stamp
    /// (ADR 0098). Records the toolchain that produced this scope so stale
    /// artifacts from a different toolchain are rebuilt rather than reused.
    pub fn stamp_path(&self) -> Utf8PathBuf {
        self.profile_dir().join(".beamtalk-stamp.json")
    }

    // ── Native Erlang ────────────────────────────────────────────────

    /// `_build/dev/native/` — base directory for native Erlang builds.
    pub fn native_dir(&self) -> Utf8PathBuf {
        self.profile_dir().join("native")
    }

    /// `_build/dev/native/ebin/` — compiled native `.erl` → `.beam` files.
    pub fn native_ebin_dir(&self) -> Utf8PathBuf {
        self.native_dir().join("ebin")
    }

    /// `_build/dev/native/include/` — generated headers (e.g. `beamtalk_classes.hrl`).
    pub fn native_include_dir(&self) -> Utf8PathBuf {
        self.native_dir().join("include")
    }

    /// `_build/dev/native/default/lib/` — rebar3 hex dep libs (`ERL_LIBS`).
    pub fn rebar_lib_dir(&self) -> Utf8PathBuf {
        self.native_dir().join("default").join("lib")
    }

    // ── Type cache (ADR 0075) ─────────────────────────────────────────

    /// `_build/type_cache/` — cached Erlang FFI type specs.
    pub fn type_cache_dir(&self) -> Utf8PathBuf {
        self.build_root().join("type_cache")
    }

    // ── Dependencies ─────────────────────────────────────────────────

    /// `_build/deps/` — top-level dependency directory.
    pub fn deps_dir(&self) -> Utf8PathBuf {
        self.build_root().join("deps")
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
    fn test_build_root() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.build_root(), "/home/user/my_app/_build");
    }

    #[test]
    fn test_profile_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.profile_dir(), "/home/user/my_app/_build/dev");
    }

    #[test]
    fn test_ebin_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(layout.ebin_dir(), "/home/user/my_app/_build/dev/ebin");
    }

    #[test]
    fn test_stamp_path() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.stamp_path(),
            "/home/user/my_app/_build/dev/.beamtalk-stamp.json"
        );
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
    fn test_native_include_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.native_include_dir(),
            "/home/user/my_app/_build/dev/native/include"
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
    fn test_type_cache_dir() {
        let layout = BuildLayout::new("/home/user/my_app");
        assert_eq!(
            layout.type_cache_dir(),
            "/home/user/my_app/_build/type_cache"
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
