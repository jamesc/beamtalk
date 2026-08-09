// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for CLI subprocess tests (BT-2084).
//!
//! These tests use `assert_cmd` to invoke the built `beamtalk` binary
//! against synthesized fixture projects in temporary directories.
//! Every helper is hermetic: nothing is written outside the `TempDir`
//! it returns, so tests are safe to run in parallel.

use assert_cmd::Command;
use beamtalk_cli::pid_liveness::is_process_alive;
use std::cell::OnceCell;
use std::path::{Path, PathBuf};
use std::sync::Once;
use std::sync::OnceLock;
use tempfile::TempDir;

/// Resolve the workspace root (repo root) from `CARGO_MANIFEST_DIR`.
///
/// `CARGO_MANIFEST_DIR` points at `crates/beamtalk-cli`, so two `parent()`
/// calls reach the repo root.
#[allow(dead_code)]
pub fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

/// Path to the debug `beamtalk` binary.
///
/// Used by ignored integration tests that spawn the binary via
/// `std::process::Command` rather than `assert_cmd`.
#[allow(dead_code)]
pub fn beamtalk_binary() -> PathBuf {
    project_root().join("target/debug/beamtalk")
}

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
        // BT-3066: Pin the shared, OTP-version-keyed FFI type-spec cache
        // (`beamtalk_core::ffi_type_specs::shared_otp_cache_dir`) to a
        // fresh directory per invocation instead of letting it default to
        // the developer/CI machine's persistent cache dir
        // (`dirs::cache_dir()`, e.g. `%LOCALAPPDATA%\beamtalk\otp-specs\`
        // on Windows). That default is *shared across every checkout and
        // test run on the machine* and outlives `_build/` wipes by design
        // (BT-2470) — exactly the opposite of what a hermetic subprocess
        // test needs. A single prior extraction failure (e.g. a build
        // worker killed mid-batch) permanently poisons it with negative
        // (`specs_line: ""`) cache entries for every module that failed to
        // report a result, which are then replayed as fresh forever (same
        // `.beam` mtime, same mapping stamp) — silently disabling FFI
        // arg-type checking for any later test run on that machine,
        // including in `beamtalk lint`/`build`'s "not stale" `@expect`
        // matching. Long-lived developer machines accumulate this state;
        // ephemeral CI runners mostly don't, which is why this surfaced as
        // a "deterministic on this Windows box" failure rather than a CI
        // one. BT-3077: every `beamtalk()` call made from a given test
        // *thread* shares that thread's isolated directory (see
        // `thread_cache_dir`) rather than littering a fresh one per call.
        // libtest runs each `#[test]` on its own worker thread and never
        // runs two tests concurrently on the same thread, so calls sharing
        // a directory are always sequential — this can't reintroduce the
        // cross-test races/poisoning a fully process-wide shared directory
        // would (verified: that was tried and did reproduce spurious
        // failures under `--test-threads` > 1). The directory is a
        // `TempDir` guard, so it's deleted automatically when its owning
        // thread exits (libtest joins every worker thread before the test
        // binary exits), and a startup sweep mops up any directory a prior
        // *process* left behind (e.g. Ctrl-C/timeout kill skipped the
        // thread-local `Drop`).
        .env("BEAMTALK_CACHE_DIR", thread_cache_dir())
        // Disable colored output so assertions on text content are stable.
        .env("NO_COLOR", "1")
        // Quiet tracing — some tests assert on stderr content.
        .env_remove("RUST_LOG");
    cmd
}

/// Prefix shared by every `BEAMTALK_CACHE_DIR` this helper ever creates, so
/// [`sweep_stale_cache_dirs`] can recognise its own litter under the OS
/// temp dir (and the encoded PID within it) without touching unrelated
/// entries.
const CACHE_DIR_PREFIX: &str = "beamtalk-cli-test-cache-";

thread_local! {
    /// This test thread's `BEAMTALK_CACHE_DIR` (BT-3066), created lazily on
    /// first use and reused by every `beamtalk()` call made from this
    /// thread (BT-3077) instead of littering a fresh directory per call.
    /// Dropping the `TempDir` deletes it — which happens when this worker
    /// thread exits, i.e. after libtest has run every test scheduled on it
    /// and joins the thread, well after any subprocess using the directory
    /// has finished.
    static CACHE_DIR: OnceCell<TempDir> = const { OnceCell::new() };
}

/// Returns this test thread's `BEAMTALK_CACHE_DIR` isolation directory
/// (BT-3066/BT-3077), creating it on first call from this thread.
///
/// Threads (not the whole process) are the sharing unit because libtest
/// runs each `#[test]` on its own worker thread and never runs two tests
/// concurrently on the same thread — so every `beamtalk()` call sharing a
/// directory is guaranteed sequential, which is what keeps this from
/// reintroducing the cross-test cache-poisoning race BT-3066 fixed. A
/// process-wide shared directory does not have that guarantee (many test
/// threads run truly concurrently) and was confirmed to reproduce spurious
/// `beamtalk lint`/`build` FFI-check failures under parallel test
/// execution.
fn thread_cache_dir() -> PathBuf {
    sweep_stale_cache_dirs_once();
    CACHE_DIR.with(|cell| {
        cell.get_or_init(|| {
            tempfile::Builder::new()
                .prefix(&format!("{CACHE_DIR_PREFIX}{}-", std::process::id()))
                .tempdir()
                .expect("create BEAMTALK_CACHE_DIR tempdir")
        })
        .path()
        .to_path_buf()
    })
}

/// Best-effort removal, once per test *process*, of `BEAMTALK_CACHE_DIR`
/// directories left behind by earlier processes that never got to run their
/// [`CACHE_DIR`] thread-local destructors — e.g. a `cargo test` run killed
/// by Ctrl-C or a CI timeout mid-suite. Normal completion doesn't need this:
/// every directory is already cleaned up by `TempDir::drop` as its owning
/// thread exits.
fn sweep_stale_cache_dirs_once() {
    static SWEPT: Once = Once::new();
    SWEPT.call_once(|| {
        let Ok(entries) = std::fs::read_dir(std::env::temp_dir()) else {
            return;
        };
        for entry in entries.flatten() {
            let name = entry.file_name();
            let Some(name) = name.to_str() else { continue };
            let Some(rest) = name.strip_prefix(CACHE_DIR_PREFIX) else {
                continue;
            };
            // rest is "{pid}-{random}"; only the pid field matters here.
            let Some(pid_str) = rest.split('-').next() else {
                continue;
            };
            let Ok(pid) = pid_str.parse::<u32>() else {
                continue;
            };
            if pid == std::process::id() || is_process_alive(pid) {
                continue;
            }
            // Best-effort: the owning process may have raced back to life
            // with a reused PID between the check above and this removal.
            let _ = std::fs::remove_dir_all(entry.path());
        }
    });
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
