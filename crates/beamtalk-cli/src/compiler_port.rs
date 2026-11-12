// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Locating the `beamtalk-compiler-port` binary (BT-4674).
//!
//! One rule for every site that hands the binary to a BEAM node (`run`,
//! `repl`, workspace startup, release assembly): a user-provided
//! `BEAMTALK_COMPILER_PORT_BIN` wins, otherwise the binary next to the
//! running `beamtalk` executable, with the platform's executable suffix.

use std::path::{Path, PathBuf};

/// Environment variable naming the compiler port binary.
pub const COMPILER_PORT_BIN_ENV: &str = "BEAMTALK_COMPILER_PORT_BIN";

/// File name of the compiler port binary on this platform.
#[must_use]
pub fn compiler_port_file_name() -> String {
    format!("beamtalk-compiler-port{}", std::env::consts::EXE_SUFFIX)
}

/// The compiler port binary sitting in `bin_dir`, if it is a file.
#[must_use]
pub fn compiler_port_in(bin_dir: &Path) -> Option<PathBuf> {
    let candidate = bin_dir.join(compiler_port_file_name());
    candidate.is_file().then_some(candidate)
}

/// Resolve the compiler port binary: the `BEAMTALK_COMPILER_PORT_BIN`
/// override (taken as given, e.g. a Nix/Homebrew wrapper), else the binary
/// next to the current executable. `None` when neither exists.
#[must_use]
pub fn compiler_port_binary() -> Option<PathBuf> {
    resolve(
        std::env::var_os(COMPILER_PORT_BIN_ENV).map(PathBuf::from),
        std::env::current_exe().ok().as_deref(),
    )
}

fn resolve(override_path: Option<PathBuf>, current_exe: Option<&Path>) -> Option<PathBuf> {
    override_path.or_else(|| compiler_port_in(current_exe?.parent()?))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_name_carries_platform_suffix() {
        let name = compiler_port_file_name();
        assert!(name.starts_with("beamtalk-compiler-port"));
        assert_eq!(
            Path::new(&name)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("exe")),
            cfg!(windows),
            "`.exe` suffix iff Windows: {name}"
        );
    }

    #[test]
    fn finds_sibling_with_platform_suffix() {
        let dir = tempfile::tempdir().unwrap();
        let exe = dir
            .path()
            .join(format!("beamtalk{}", std::env::consts::EXE_SUFFIX));
        assert_eq!(resolve(None, Some(&exe)), None, "absent binary");
        let port = dir.path().join(compiler_port_file_name());
        std::fs::write(&port, b"").unwrap();
        assert_eq!(resolve(None, Some(&exe)), Some(port));
    }

    #[test]
    fn override_wins_over_sibling() {
        let dir = tempfile::tempdir().unwrap();
        let exe = dir.path().join("beamtalk");
        std::fs::write(dir.path().join(compiler_port_file_name()), b"").unwrap();
        let wrapper = PathBuf::from("/nix/store/wrapper/compiler-port");
        assert_eq!(resolve(Some(wrapper.clone()), Some(&exe)), Some(wrapper));
    }
}
