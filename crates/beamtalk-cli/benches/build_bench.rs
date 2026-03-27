// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Build performance benchmarks for tracking regressions.
//!
//! Run with: `cargo bench -p beamtalk-cli`
//!
//! These benchmarks measure the performance of the build pipeline at different
//! levels: full stdlib builds (cold and warm), incremental rebuilds with file
//! changes, and Pass 1 class scanning (the lex → parse → class extraction step).
//!
//! Build benchmarks use `std::process::Command` to invoke the `beamtalk` binary,
//! measuring the full pipeline including process startup. This matches what users
//! actually experience and avoids needing to expose internal build functions.

use std::path::{Path, PathBuf};
use std::process::Command;

use criterion::{Criterion, criterion_group, criterion_main};

/// Locate the workspace root (repo root) by walking up from `CARGO_MANIFEST_DIR`.
fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // crates/beamtalk-cli -> repo root (two levels up)
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("cannot determine workspace root")
        .to_path_buf()
}

/// Locate the `beamtalk` binary in the target directory.
///
/// Checks release first (used by `cargo bench`), then debug.
/// Falls back to building via `cargo build` if not found.
fn beamtalk_binary() -> PathBuf {
    let root = workspace_root();

    // cargo bench builds in release mode by default
    let release_bin = root.join("target/release/beamtalk");
    if release_bin.exists() {
        return release_bin;
    }

    let debug_bin = root.join("target/debug/beamtalk");
    if debug_bin.exists() {
        return debug_bin;
    }

    // Build the binary if it doesn't exist
    let status = Command::new("cargo")
        .args(["build", "-p", "beamtalk-cli", "--release"])
        .current_dir(&root)
        .status()
        .expect("failed to run cargo build");
    assert!(status.success(), "cargo build failed");

    assert!(
        release_bin.exists(),
        "beamtalk binary not found after build at {}",
        release_bin.display()
    );
    release_bin
}

/// Run `beamtalk build-stdlib --quiet` in the workspace root.
///
/// Returns true if the command succeeded.
fn run_stdlib_build(binary: &Path, root: &Path) -> bool {
    let output = Command::new(binary)
        .args(["build-stdlib", "--quiet"])
        .current_dir(root)
        .output()
        .expect("failed to execute beamtalk build-stdlib");
    output.status.success()
}

/// Touch a single stdlib source file to invalidate its build cache entry.
///
/// This simulates a 1-file change for incremental build benchmarking by
/// rewriting the file contents to update the mtime.
fn touch_file(path: &Path) {
    // Read and rewrite to update mtime
    let contents = std::fs::read(path).expect("failed to read file for touch");
    std::fs::write(path, contents).expect("failed to write file for touch");
}

/// Clean all stdlib `.beam` outputs so the next build is a cold build.
fn clean_stdlib_ebin(root: &Path) {
    let ebin = root.join("runtime/apps/beamtalk_stdlib/ebin");
    if ebin.exists() {
        for entry in std::fs::read_dir(&ebin).expect("failed to read ebin dir") {
            let entry = entry.expect("failed to read ebin entry");
            let path = entry.path();
            if path.extension().is_some_and(|e| e == "beam") {
                std::fs::remove_file(&path).ok();
            }
        }
    }
    // Also remove the metadata cache
    let cache_file = root.join("runtime/apps/beamtalk_stdlib/ebin/.beamtalk-cache.json");
    std::fs::remove_file(cache_file).ok();
}

/// Collect all `.bt` source file paths from the stdlib `src/` directory.
fn collect_stdlib_sources(root: &Path) -> Vec<PathBuf> {
    let src_dir = root.join("stdlib/src");
    let mut files = Vec::new();
    for entry in std::fs::read_dir(&src_dir).expect("failed to read stdlib/src") {
        let entry = entry.expect("failed to read stdlib entry");
        let path = entry.path();
        if path.extension().is_some_and(|e| e == "bt") {
            files.push(path);
        }
    }
    files.sort();
    files
}

// ---------------------------------------------------------------------------
// Benchmark: Full stdlib build (cold — no cached .beam files)
// ---------------------------------------------------------------------------

fn bench_stdlib_cold_build(c: &mut Criterion) {
    let binary = beamtalk_binary();
    let root = workspace_root();

    let mut group = c.benchmark_group("stdlib_build");
    // These are slow benchmarks (seconds each), so use a small sample size
    // and longer measurement time.
    group.sample_size(10);
    group.measurement_time(std::time::Duration::from_secs(120));

    group.bench_function("cold_build", |b| {
        b.iter_with_setup(
            || {
                // Setup: clean all .beam files so it's a true cold build
                clean_stdlib_ebin(&root);
            },
            |()| {
                assert!(run_stdlib_build(&binary, &root), "stdlib cold build failed");
            },
        );
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark: Incremental stdlib build — 0 changes (warm cache hit)
// ---------------------------------------------------------------------------

fn bench_stdlib_warm_noop(c: &mut Criterion) {
    let binary = beamtalk_binary();
    let root = workspace_root();

    // Ensure stdlib is fully built before we start
    assert!(
        run_stdlib_build(&binary, &root),
        "initial stdlib build failed"
    );

    let mut group = c.benchmark_group("stdlib_build");
    group.sample_size(20);
    group.measurement_time(std::time::Duration::from_secs(30));

    group.bench_function("warm_noop", |b| {
        b.iter(|| {
            assert!(
                run_stdlib_build(&binary, &root),
                "stdlib warm noop build failed"
            );
        });
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark: Incremental stdlib build — 1 file changed
// ---------------------------------------------------------------------------

fn bench_stdlib_incremental_one_file(c: &mut Criterion) {
    let binary = beamtalk_binary();
    let root = workspace_root();

    // Ensure stdlib is fully built
    assert!(
        run_stdlib_build(&binary, &root),
        "initial stdlib build failed"
    );

    // Pick a representative file (Object.bt — the base class, moderate size)
    let touch_target = root.join("stdlib/src/Object.bt");
    assert!(
        touch_target.exists(),
        "Object.bt not found — expected at {}",
        touch_target.display()
    );

    let mut group = c.benchmark_group("stdlib_build");
    group.sample_size(10);
    group.measurement_time(std::time::Duration::from_secs(60));

    group.bench_function("incremental_one_file", |b| {
        b.iter_with_setup(
            || {
                // Touch one file to mark it as changed
                touch_file(&touch_target);
            },
            |()| {
                assert!(
                    run_stdlib_build(&binary, &root),
                    "stdlib incremental build failed"
                );
            },
        );
    });

    group.finish();
}

// ---------------------------------------------------------------------------
// Benchmark: Pass 1 class scanning (lex → parse → extract class infos)
// ---------------------------------------------------------------------------

fn bench_pass1_class_scanning(c: &mut Criterion) {
    let root = workspace_root();
    let sources = collect_stdlib_sources(&root);

    // Pre-read all source files into memory so we only benchmark parsing,
    // not filesystem I/O.
    let file_contents: Vec<String> = sources
        .iter()
        .map(|p| std::fs::read_to_string(p).expect("failed to read source file"))
        .collect();

    let mut group = c.benchmark_group("pass1_scanning");
    group.sample_size(50);

    group.bench_function("all_stdlib_files", |b| {
        b.iter(|| {
            let mut class_count = 0usize;
            for source in &file_contents {
                let tokens = beamtalk_core::source_analysis::lex_with_eof(source);
                let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
                let infos =
                    beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module);
                class_count += infos.len();
            }
            class_count
        });
    });

    group.finish();
}

/// Pass 1 scanning of a single representative file, useful for tracking
/// per-file regression independently of file count changes.
fn bench_pass1_single_file(c: &mut Criterion) {
    let root = workspace_root();
    let file_path = root.join("stdlib/src/Collection.bt");
    assert!(
        file_path.exists(),
        "Collection.bt not found for single-file bench"
    );
    let source = std::fs::read_to_string(&file_path).expect("failed to read Collection.bt");

    let mut group = c.benchmark_group("pass1_scanning");
    group.sample_size(200);

    group.bench_function("single_file", |b| {
        b.iter(|| {
            let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
            let (module, _diagnostics) = beamtalk_core::source_analysis::parse(tokens);
            beamtalk_core::semantic_analysis::ClassHierarchy::extract_class_infos(&module)
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_stdlib_cold_build,
    bench_stdlib_warm_noop,
    bench_stdlib_incremental_one_file,
    bench_pass1_class_scanning,
    bench_pass1_single_file,
);
criterion_main!(benches);
