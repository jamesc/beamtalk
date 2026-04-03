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
pub fn run(spec_path: &str, output: &str) -> Result<()> {
    eprintln!("beamtalk generate stubs: not yet implemented");
    eprintln!("  spec:   {spec_path}");
    eprintln!("  output: {output}");
    eprintln!();
    eprintln!("This command will generate Beamtalk stub definitions from FFI spec files.");
    eprintln!("See ADR 0075 for the planned design.");
    miette::bail!("generate stubs is not yet implemented — see ADR 0075 Phase 3")
}
