// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build script to compile the Erlang runtime using rebar3.
//!
//! This ensures the runtime BEAM files are built alongside the Rust code.

use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let workspace_root = Path::new(&manifest_dir)
        .parent()
        .and_then(Path::parent)
        .expect("Cannot find workspace root");
    let runtime_dir = workspace_root.join("runtime");

    // Watch the runtime source files for changes
    let src_dir = runtime_dir.join("src");
    println!("cargo:rerun-if-changed={}", src_dir.display());
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
    println!("cargo:warning=Building Erlang runtime with rebar3...");

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
