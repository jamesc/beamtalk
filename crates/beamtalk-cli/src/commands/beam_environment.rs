// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Unified BEAM environment assembly for all CLI commands.
//!
//! **DDD Context:** Build System
//!
//! Every CLI command that starts a BEAM node (build, test, run, repl, workspace)
//! needs the same set of code paths, OTP application names, and class metadata.
//! Historically each command assembled these independently, leading to a recurring
//! bug class where a new source (dep ebin, native ebin, hex dep, etc.) was wired
//! into one command but missed in others.
//!
//! [`BeamEnvironment`] is the single source of truth for "what does the BEAM need
//! to know about this project?" — assembled once from the project layout and
//! consumed by all commands.

use camino::{Utf8Path, Utf8PathBuf};
use miette::Result;
use tracing::debug;

use super::build_layout::BuildLayout;

/// Everything needed to set up a BEAM node for a Beamtalk project.
///
/// Built once via [`BeamEnvironment::from_layout`] and consumed by all commands.
/// Adding a new code path source (e.g. registry deps) requires updating only
/// this constructor — all commands pick it up automatically.
#[derive(Debug, Clone)]
pub struct BeamEnvironment {
    /// All `-pa` directories beyond the runtime paths: package ebin, dep ebins,
    /// native ebin, rebar3 hex dep ebins.
    ///
    /// Does **not** include the runtime paths (stdlib, workspace, etc.) which are
    /// provided separately by [`beamtalk_cli::repl_startup::beam_pa_args`].
    pub code_paths: Vec<Utf8PathBuf>,

    /// OTP applications to `ensure_all_started` before workspace startup.
    ///
    /// Sourced from the lockfile's resolved hex packages (e.g. gun, cowboy).
    pub otp_apps: Vec<String>,

    /// `ERL_LIBS` directory for rebar3 deps (only set when the directory exists).
    ///
    /// Used by erlc for native Erlang compilation — not needed at BEAM runtime.
    /// Available for `ErlcInvocation` callers that need it.
    #[allow(dead_code)] // Part of the public environment API; consumed by erlc invocations
    pub erl_libs: Option<Utf8PathBuf>,
}

impl BeamEnvironment {
    /// Build the complete BEAM environment from a project's [`BuildLayout`].
    ///
    /// Collects:
    /// 1. Package ebin directory (always included, even if it doesn't exist yet)
    /// 2. Path dependency ebin directories (`_build/deps/*/ebin/`)
    /// 3. Native Erlang ebin (`_build/dev/native/ebin/`, if it exists)
    /// 4. Rebar3 hex dep ebins (`_build/dev/native/default/lib/*/ebin/`)
    /// 5. Hex dep names from the lockfile (for OTP app startup)
    /// 6. `ERL_LIBS` for rebar3 compilation
    ///
    /// # Errors
    ///
    /// Returns an error if the lockfile exists but cannot be parsed.
    pub fn from_layout(layout: &BuildLayout, project_root: &Utf8Path) -> Result<Self> {
        let mut code_paths = Vec::new();

        // 1. Package ebin — always first so the project's own classes take priority
        let ebin_dir = layout.ebin_dir();
        code_paths.push(ebin_dir);

        // 2. Path dependency ebins
        for dep_ebin in super::deps::collect_dep_ebin_paths(layout) {
            debug!(ebin = %dep_ebin, "Adding dep ebin to environment");
            code_paths.push(dep_ebin);
        }

        // 3. Native Erlang ebin (only if it exists — may not have native sources)
        let native_ebin = layout.native_ebin_dir();
        if native_ebin.exists() {
            debug!(ebin = %native_ebin, "Adding native ebin to environment");
            code_paths.push(native_ebin);
        }

        // 4. Rebar3 hex dep ebins
        for ebin in super::build::collect_rebar3_ebin_paths(layout) {
            debug!(ebin = %ebin, "Adding rebar3 ebin to environment");
            code_paths.push(ebin);
        }

        // 5. Hex dep names from lockfile → OTP apps to start
        let otp_apps = super::deps::lockfile::Lockfile::collect_hex_dep_names(project_root)?;

        // 6. ERL_LIBS for rebar3 compilation (only if the lib dir exists)
        let rebar_lib = layout.rebar_lib_dir();
        let erl_libs = if rebar_lib.exists() {
            Some(rebar_lib)
        } else {
            None
        };

        Ok(Self {
            code_paths,
            otp_apps,
            erl_libs,
        })
    }

    /// Convert code paths to `std::path::PathBuf` for APIs that require it.
    pub fn code_paths_std(&self) -> Vec<std::path::PathBuf> {
        self.code_paths
            .iter()
            .map(|p| p.clone().into_std_path_buf())
            .collect()
    }

    /// Build `OsString` `-pa` arguments for all code paths.
    ///
    /// Returns pairs of `["-pa", "<path>"]` suitable for appending to
    /// an `erl` command's argument list.
    pub fn pa_args(&self) -> Vec<std::ffi::OsString> {
        let mut args = Vec::with_capacity(self.code_paths.len() * 2);
        for path in &self.code_paths {
            args.push(std::ffi::OsString::from("-pa"));
            #[cfg(windows)]
            {
                let escaped = path.as_str().replace('\\', "/");
                args.push(std::ffi::OsString::from(escaped));
            }
            #[cfg(not(windows))]
            {
                args.push(std::ffi::OsString::from(path.as_str()));
            }
        }
        args
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_from_layout_empty_project() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        // Should always include the package ebin dir (even if it doesn't exist)
        assert_eq!(env.code_paths.len(), 1);
        assert!(env.code_paths[0].ends_with("_build/dev/ebin"));
        assert!(env.otp_apps.is_empty());
        assert!(env.erl_libs.is_none());
    }

    #[test]
    fn test_from_layout_with_dep_ebin() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create a dep ebin directory
        let dep_ebin = layout.dep_ebin_dir("my_dep");
        fs::create_dir_all(&dep_ebin).unwrap();

        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        // Should include package ebin + dep ebin
        assert_eq!(env.code_paths.len(), 2);
        assert!(env.code_paths[1].ends_with("my_dep/ebin"));
    }

    #[test]
    fn test_from_layout_with_native_ebin() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create native ebin directory
        let native_ebin = layout.native_ebin_dir();
        fs::create_dir_all(&native_ebin).unwrap();

        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        // Should include package ebin + native ebin
        assert_eq!(env.code_paths.len(), 2);
        assert!(env.code_paths[1].ends_with("native/ebin"));
    }

    #[test]
    fn test_from_layout_with_rebar_lib() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create rebar3 lib/gun/ebin/ directory
        let gun_ebin = layout.rebar_lib_dir().join("gun").join("ebin");
        fs::create_dir_all(&gun_ebin).unwrap();

        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        // Should include package ebin + rebar3 gun ebin
        assert_eq!(env.code_paths.len(), 2);
        assert!(env.code_paths[1].ends_with("gun/ebin"));
        // ERL_LIBS should be set since rebar lib dir exists
        assert!(env.erl_libs.is_some());
    }

    #[test]
    fn test_code_paths_std() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        let std_paths = env.code_paths_std();
        assert_eq!(std_paths.len(), env.code_paths.len());
    }

    #[test]
    fn test_pa_args() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create native ebin so we get 2 code paths
        let native_ebin = layout.native_ebin_dir();
        fs::create_dir_all(&native_ebin).unwrap();

        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();
        let args = env.pa_args();

        // Each code path produces 2 args: "-pa" and the path
        assert_eq!(args.len(), env.code_paths.len() * 2);
        assert_eq!(args[0], std::ffi::OsString::from("-pa"));
        assert_eq!(args[2], std::ffi::OsString::from("-pa"));
    }

    #[test]
    fn test_all_paths_collected() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        // Create all possible path sources
        let dep_ebin = layout.dep_ebin_dir("utils");
        fs::create_dir_all(&dep_ebin).unwrap();

        let native_ebin = layout.native_ebin_dir();
        fs::create_dir_all(&native_ebin).unwrap();

        let rebar_ebin = layout.rebar_lib_dir().join("cowboy").join("ebin");
        fs::create_dir_all(&rebar_ebin).unwrap();

        let env = BeamEnvironment::from_layout(&layout, &root).unwrap();

        // package ebin + dep ebin + native ebin + rebar3 ebin = 4
        assert_eq!(env.code_paths.len(), 4);
    }
}
