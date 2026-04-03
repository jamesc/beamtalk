// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generate Beamtalk stub definitions from FFI spec files (ADR 0075 Phase 3).
//!
//! **DDD Context:** Compilation
//!
//! This is a placeholder skeleton for the `beamtalk generate stubs` command.
//! Full implementation will read FFI type-definition files and generate `.bt`
//! class stubs with method signatures and type annotations.

use miette::Result;

/// Run the `generate stubs` command (placeholder).
#[expect(
    clippy::unnecessary_wraps,
    reason = "skeleton — will return errors once implemented"
)]
pub fn run(spec_path: &str, output: &str) -> Result<()> {
    println!("beamtalk generate stubs: not yet implemented");
    println!("  spec:   {spec_path}");
    println!("  output: {output}");
    println!();
    println!("This command will generate Beamtalk stub definitions from FFI spec files.");
    println!("See ADR 0075 for the planned design.");
    Ok(())
}
