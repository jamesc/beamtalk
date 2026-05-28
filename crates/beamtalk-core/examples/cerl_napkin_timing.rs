// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0088 Phase 0b — wire-cost timing harness (BT-2315).
//!
//! Produces the Rust half of the napkin's timing data:
//!
//!   1. Construct two cerl fixtures (empty module + ~thousands of nodes).
//!   2. Time ETF encode in Rust (`Iterations` repeats, report best/mean).
//!   3. Write the encoded bytes — plus the equivalent text Core Erlang —
//!      to fixture files in `target/cerl-napkin-timing/`.
//!   4. Print the Rust-side timing table.
//!
//! The Erlang side of the timing measurement runs in
//! `scripts/cerl-napkin-timing.escript`, which reads the fixture files,
//! times `binary_to_term` + `compile:forms` for the cerl path, and
//! `core_scan` + `core_parse` + `compile:forms` for the text path.
//!
//! The aggregate analysis (cerl vs text, what fraction of total time is
//! ETF encode+decode) lives in `docs/ADR/0088-phase-0b-napkin.md`.
//!
//! Usage:
//!
//! ```text
//! cargo run --release --example cerl_napkin_timing -p beamtalk-core
//! escript scripts/cerl-napkin-timing.escript target/cerl-napkin-timing
//! ```

use beamtalk_core::codegen::core_erlang::cerl::CModule;
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

const ITERATIONS: usize = 1000;
const MANY_FUNCTIONS: usize = 250;

fn main() {
    let out_dir =
        PathBuf::from(std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".to_string()))
            .join("cerl-napkin-timing");
    fs::create_dir_all(&out_dir).expect("create fixture dir");

    println!("ADR 0088 Phase 0b — Rust-side timing harness");
    println!("============================================");
    println!("Iterations per measurement: {ITERATIONS}");
    println!("Fixture dir: {}", out_dir.display());
    println!();

    // ── Empty module ─────────────────────────────────────────────────
    measure_and_write_fixture(
        &out_dir,
        "empty",
        &CModule::minimum("bt_napkin_empty"),
        &text_module_minimum("bt_napkin_empty"),
    );

    // ── Many-function module (representative thousands-of-nodes tree) ─
    measure_and_write_fixture(
        &out_dir,
        "many",
        &CModule::multi_function("bt_napkin_many", MANY_FUNCTIONS),
        &text_module_multi_function("bt_napkin_many", MANY_FUNCTIONS),
    );

    println!();
    println!("Wrote fixtures to {}", out_dir.display());
    println!(
        "Next: escript scripts/cerl-napkin-timing.escript {}",
        out_dir.display()
    );
}

fn measure_and_write_fixture(out_dir: &Path, tag: &str, module: &CModule, text: &str) {
    let etf_path = out_dir.join(format!("{tag}.etf"));
    let text_path = out_dir.join(format!("{tag}.core"));

    // Warm-up — JIT/cache effects can skew the first sample.
    for _ in 0..50 {
        let _ = module.encode_etf().unwrap();
    }

    let mut samples_ns: Vec<u128> = Vec::with_capacity(ITERATIONS);
    let mut last_bytes: Vec<u8> = Vec::new();
    for _ in 0..ITERATIONS {
        let t0 = Instant::now();
        let bytes = module.encode_etf().unwrap();
        samples_ns.push(t0.elapsed().as_nanos());
        last_bytes = bytes;
    }

    let bytes_len = last_bytes.len();
    fs::write(&etf_path, &last_bytes).expect("write etf fixture");
    fs::write(&text_path, text).expect("write text fixture");

    let best = samples_ns.iter().min().copied().unwrap_or(0);
    let mean = samples_ns.iter().sum::<u128>() / (samples_ns.len() as u128);
    let p95 = percentile(&mut samples_ns, 95);

    println!("Fixture: {tag}");
    println!("  ETF size       : {bytes_len} bytes");
    println!("  Text size      : {} bytes", text.len());
    println!("  Rust encode    : best {best} ns | mean {mean} ns | p95 {p95} ns");
    println!("  ETF fixture    : {}", etf_path.display());
    println!("  Text fixture   : {}", text_path.display());
    println!();
}

/// `p` is an integer percentile in 0..=100 — integer math here avoids the
/// f64-cast pedantic-clippy complaints in a place that does not need
/// fractional precision.
fn percentile(samples: &mut [u128], p: usize) -> u128 {
    samples.sort_unstable();
    let len = samples.len();
    let idx = (len.saturating_mul(p) / 100).min(len.saturating_sub(1));
    samples[idx]
}

/// Text-Core-Erlang equivalent of `CModule::minimum`.
///
/// The text path is what `core_scan` + `core_parse` need; the resulting
/// `compile:forms/2` output is functionally identical to what the cerl
/// path produces (up to bytecode-irrelevant annotation chunks).
fn text_module_minimum(name: &str) -> String {
    format!(
        "module '{name}' ['module_info'/0, 'module_info'/1]\n\
         attributes []\n\
         'module_info'/0 = fun () -> call 'erlang':'get_module_info' ('{name}')\n\
         'module_info'/1 = fun (X) -> call 'erlang':'get_module_info' ('{name}', X)\n\
         end\n"
    )
}

/// Text-Core-Erlang equivalent of `CModule::multi_function`.
fn text_module_multi_function(name: &str, n: usize) -> String {
    let mut s = String::with_capacity(64 * n + 256);
    let _ = write!(s, "module '{name}' [");
    for i in 0..n {
        if i > 0 {
            s.push_str(", ");
        }
        let _ = write!(s, "'f{i}'/0");
    }
    s.push_str(", 'module_info'/0, 'module_info'/1]\n");
    s.push_str("attributes []\n");
    for i in 0..n {
        let _ = writeln!(s, "'f{i}'/0 = fun () -> 'f{i}_ok'");
    }
    let _ = writeln!(
        s,
        "'module_info'/0 = fun () -> call 'erlang':'get_module_info' ('{name}')"
    );
    let _ = writeln!(
        s,
        "'module_info'/1 = fun (X) -> call 'erlang':'get_module_info' ('{name}', X)"
    );
    s.push_str("end\n");
    s
}
