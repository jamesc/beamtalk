// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared helpers for the `beamtalk-repl` integration-test binaries.

use std::path::Path;
use std::process::Command;

/// Modules per `erlc` invocation. Windows caps a command line at ~32K
/// characters and these corpora are hundreds of modules, so one `erlc`
/// call with every path overflows it (`Os { code: 206 }`, BT-4224).
/// Batches of 32 keep every invocation to a few kilobytes on every platform.
const BATCH: usize = 32;

/// Compiles `modules` (`(module name, Core Erlang source)`) with
/// `erlc +from_core` into `dir`, in batches of [`BATCH`].
///
/// `Ok(())` when `erlc` accepts every module, or when `erlc` is not
/// installed (`NotFound` only — any other spawn failure, such as the
/// Windows command-line overflow that motivated the batching, is an
/// `Err`, never a silent pass). Every batch runs even after one fails, and
/// the `Err` carries the output of every failing batch (BT-4379).
///
/// # Panics
///
/// Panics if a `.core` file cannot be written into `dir`.
pub fn erlc_batched(dir: &Path, modules: &[(String, String)]) -> Result<(), String> {
    let mut files = Vec::new();
    for (name, code) in modules {
        let file = dir.join(format!("{name}.core"));
        std::fs::write(&file, code).expect("write core");
        files.push(file);
    }
    let mut errors = Vec::new();
    for chunk in files.chunks(BATCH) {
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg("-o")
            .arg(dir)
            .args(chunk)
            .output();
        match output {
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(e) => errors.push(format!("failed to invoke erlc: {e}")),
            Ok(o) if o.status.success() => {}
            Ok(o) => errors.push(format!(
                "{}{}",
                String::from_utf8_lossy(&o.stdout),
                String::from_utf8_lossy(&o.stderr)
            )),
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("\n"))
    }
}

/// [`erlc_batched`] into a fresh temporary directory (named after
/// `prefix`), removed afterwards — for callers that only need the verdict,
/// not the `.beam` files.
///
/// # Panics
///
/// Panics if the temporary directory cannot be created.
#[allow(dead_code)] // some test binaries don't call every helper
pub fn erlc_batched_in_temp_dir(prefix: &str, modules: &[(String, String)]) -> Result<(), String> {
    let dir = beamtalk_core::test_helpers::unique_temp_dir(prefix);
    std::fs::create_dir_all(&dir).expect("temp dir");
    let result = erlc_batched(&dir, modules);
    let _ = std::fs::remove_dir_all(&dir);
    result
}
