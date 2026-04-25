// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI subprocess tests (BT-2084).
//!
//! These tests use `assert_cmd` to invoke the built `beamtalk` binary
//! against synthesized fixture projects in temporary directories.
//! Every helper is hermetic: nothing is written outside the `TempDir`
//! it returns, so tests are safe to run in parallel.

use assert_cmd::Command;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use tempfile::TempDir;

/// Path to the `beamtalk` binary built by `cargo`.
///
/// `assert_cmd::Command::cargo_bin` works because `beamtalk-cli` declares
/// `beamtalk` as a `[[bin]]` and the test binary lives in the same crate.
#[allow(dead_code)] // some test binaries don't call every helper
pub fn beamtalk() -> Command {
    let mut cmd = Command::cargo_bin("beamtalk").expect("beamtalk binary built by cargo");
    // Pin the runtime/sysroot to this workspace so tests do not depend on a
    // system-installed beamtalk. `repl_startup::find_runtime_dir_with_layout`
    // honours `BEAMTALK_RUNTIME_DIR` first, which keeps `doctor`/`build`/`test`
    // pointing at the in-repo `runtime/` directory.
    cmd.env("BEAMTALK_RUNTIME_DIR", runtime_dir())
        // Disable colored output so assertions on text content are stable.
        .env("NO_COLOR", "1")
        // Quiet tracing — some tests assert on stderr content.
        .env_remove("RUST_LOG");
    cmd
}

/// Locate the workspace `runtime/` directory.
fn runtime_dir() -> &'static Path {
    static DIR: OnceLock<PathBuf> = OnceLock::new();
    DIR.get_or_init(|| {
        let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // crates/beamtalk-cli -> crates -> repo root -> runtime
        manifest
            .parent()
            .and_then(|p| p.parent())
            .map(|root| root.join("runtime"))
            .expect("workspace root has runtime/ directory")
    })
}

/// Create a fresh temp directory holding a minimal Beamtalk library project.
///
/// The project has:
/// * `beamtalk.toml` — package manifest
/// * `src/Greeter.bt` — a trivial Value class
/// * `test/GreeterTest.bt` — one passing `BUnit` test
///
/// All paths are derived from the returned `TempDir`; nothing is written
/// elsewhere, so tests are hermetic and parallel-safe.
#[allow(dead_code)] // some test binaries don't call every helper
pub fn fixture_project() -> TempDir {
    let dir = tempfile::tempdir().expect("create tempdir");
    let root = dir.path();

    std::fs::create_dir_all(root.join("src")).expect("mkdir src");
    std::fs::create_dir_all(root.join("test")).expect("mkdir test");

    std::fs::write(
        root.join("beamtalk.toml"),
        "# Copyright 2026 James Casey\n\
         # SPDX-License-Identifier: Apache-2.0\n\
         \n\
         [package]\n\
         name = \"cli_subprocess_fixture\"\n\
         version = \"0.1.0\"\n\
         \n\
         [dependencies]\n",
    )
    .expect("write beamtalk.toml");

    std::fs::write(
        root.join("src/Greeter.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         /// Trivial greeter used by CLI subprocess tests.\n\
         Value subclass: Greeter\n\
         \n\
         \x20\x20hello => \"hello\"\n",
    )
    .expect("write src/Greeter.bt");

    std::fs::write(
        root.join("test/GreeterTest.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         TestCase subclass: GreeterTest\n\
         \n\
         \x20\x20testHello => self assert: Greeter new hello equals: \"hello\"\n",
    )
    .expect("write test/GreeterTest.bt");

    dir
}
