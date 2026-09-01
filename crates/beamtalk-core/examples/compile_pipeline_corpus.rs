// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3124 — corpus-through-BEAM Rust half: generate `.core` text for every
//! `.bt`/`.btscript` file in one or more input corpus directories.
//!
//! Pair to `scripts/compile-pipeline-corpus-lint.escript`. This example runs
//! each corpus file through the same pipeline as the `compile_pipeline` fuzz
//! target (lex → parse → analyse → `generate_module`) and writes every
//! successful `Ok` output as a `.core` file to the output directory. The
//! Erlang-side script then batch-compiles those `.core` files with
//! `compile:file/2` (`from_core`, `clint`) — this is where "erlc rejects it"
//! / `core_lint` unbound-var failures surface, without putting BEAM in the
//! libFuzzer hot loop.
//!
//! A `generate_module` `Err` is not reported here: arbitrary corpus growth
//! (fuzzer-discovered inputs especially) is expected to fail analysis or
//! codegen far more often than it succeeds, and that is not this tool's
//! concern — `core_erlang_structural_issues` inside the fuzz target itself
//! already catches structurally-broken `Ok` outputs before they'd reach
//! this step. This tool exists for the *narrower* gap: inputs where
//! `generate_module` returns `Ok` and passes the structural-validity check,
//! but the output still isn't accepted by the real OTP compiler pipeline
//! (`erlc`/`core_lint`) once it round-trips through `core_scan`/`core_parse`.
//!
//! Usage:
//!
//! ```text
//! cargo run --release --example compile_pipeline_corpus -p beamtalk-core -- \
//!     <output_dir> <input_dir> [<input_dir> ...]
//! escript scripts/compile-pipeline-corpus-lint.escript <output_dir>
//! ```

use beamtalk_codegen::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::semantic_analysis::analyse;
use beamtalk_core::source_analysis::{lex_with_eof, parse};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let mut args = std::env::args().skip(1);
    let out_dir = PathBuf::from(
        args.next()
            .unwrap_or_else(|| "target/compile-pipeline-corpus".to_string()),
    );
    let input_dirs: Vec<PathBuf> = args.map(PathBuf::from).collect();
    let input_dirs = if input_dirs.is_empty() {
        vec![
            PathBuf::from("stdlib/test"),
            PathBuf::from("tests/repl-protocol/cases"),
        ]
    } else {
        input_dirs
    };

    fs::create_dir_all(&out_dir).expect("create output dir");

    let mut written = 0usize;
    let mut skipped = 0usize;
    // Disambiguates module names derived from file stems that collide
    // across input dirs (e.g. a fuzzer-grown corpus dir alongside the seed
    // corpus): first occurrence keeps the bare name, later ones get a
    // `_2`, `_3`, ... suffix so no .core file is silently overwritten.
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for input_dir in &input_dirs {
        let entries = match fs::read_dir(input_dir) {
            Ok(entries) => entries,
            Err(e) => {
                eprintln!("skipping unreadable input dir {}: {e}", input_dir.display());
                continue;
            }
        };
        let mut paths: Vec<PathBuf> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
        paths.sort();

        for path in paths {
            if !is_beamtalk_source(&path) {
                continue;
            }
            let Ok(source) = fs::read_to_string(&path) else {
                skipped += 1;
                continue;
            };

            let base_name = module_name_for(&path);
            let count = name_counts.entry(base_name.clone()).or_insert(0);
            *count += 1;
            let module_name = if *count == 1 {
                base_name
            } else {
                format!("{base_name}_{count}")
            };

            let tokens = lex_with_eof(&source);
            let (module, _diagnostics) = parse(tokens);
            let analysis = analyse(&module);
            let options = CodegenOptions::new(&module_name).with_analysis(analysis);

            match generate_module(&module, options) {
                Ok(core_erlang) => {
                    let out_file = out_dir.join(format!("{module_name}.core"));
                    fs::write(&out_file, core_erlang).expect("write .core file");
                    written += 1;
                }
                Err(_) => skipped += 1,
            }
        }
    }

    println!("Wrote {written} .core file(s) to {}", out_dir.display());
    println!("Skipped {skipped} input(s) that did not codegen cleanly (expected — see moduledoc).");
}

fn is_beamtalk_source(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|e| e.to_str()),
        Some("bt" | "btscript")
    )
}

/// A Core Erlang module name derived from the input file's stem — must be a
/// valid Erlang atom-safe identifier. Corpus filenames are already
/// snake_case-ish; lowercase and replace anything non-alphanumeric with `_`
/// defensively so a stray filename never produces unparseable Core Erlang.
fn module_name_for(path: &Path) -> String {
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("corpus");
    let mut name: String = stem
        .to_lowercase()
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    if name.is_empty() || !name.chars().next().unwrap().is_ascii_lowercase() {
        name.insert_str(0, "bt_corpus_");
    }
    name
}
