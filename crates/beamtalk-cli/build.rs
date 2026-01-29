// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to compile the Erlang runtime using rebar3.
//!
//! This ensures the runtime BEAM files are built alongside the Rust code.

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");
    let runtime_dir = workspace_root.join("runtime");

    // Watch individual Erlang source files for changes
    // (watching a directory only triggers when files are added/removed, not modified)
    let src_dir = runtime_dir.join("src");
    if let Ok(entries) = fs::read_dir(&src_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path
                .extension()
                .is_some_and(|ext| ext == "erl" || ext == "hrl")
            {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
    }
    println!(
        "cargo:rerun-if-changed={}",
        runtime_dir.join("rebar.config").display()
    );

    // Check if rebar3 is available
    let rebar3_check = Command::new("rebar3").arg("--version").output();

    if rebar3_check.is_err() {
        println!("cargo:warning=rebar3 not found, skipping runtime build");
        println!("cargo:warning=Install rebar3 to build the Erlang runtime automatically");
        return;
    }

    // Run rebar3 compile in the runtime directory
    let status = Command::new("rebar3")
        .arg("compile")
        .current_dir(&runtime_dir)
        .status()
        .expect("Failed to run rebar3 compile");

    assert!(
        status.success(),
        "rebar3 compile failed with exit code: {:?}",
        status.code()
    );
}
