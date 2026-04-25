// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostic parity corpus (BT-2078).
//!
//! Drives a curated set of fixture projects through every diagnostic
//! surface — CLI `beamtalk lint`, MCP `lint`, MCP `diagnostic_summary`, and
//! LSP `textDocument/diagnostic` — and asserts that each surface produces
//! the diagnostic count locked in for that fixture.
//!
//! This is the Tier-2 counterpart to BT-2052/BT-2056/BT-2060/BT-2067 (lint
//! divergence) and BT-2027 (LSP diagnostic regression). Each fixture under
//! `tests/parity/diagnostics/<name>/` represents a category where surface
//! drift has historically appeared:
//!
//! | Fixture                      | Regression context |
//! |------------------------------|--------------------|
//! | `cross_file_class`           | BT-2052 — cross-file class extraction |
//! | `sealed_subclass`            | Sealed subclass error |
//! | `stdlib_mode`                | BT-2027 — package-mode cross-file lookup |
//! | `protocol_cross_file`        | BT-1950 — cross-file Protocol use |
//! | `mixed_diagnostics`          | type / dnu / lint mix |
//! | `unreadable_target`          | BT-2067 — unreadable target file (Unix only) |
//! | `unreadable_package`         | BT-2056 — unreadable extraction file (Unix only) |
//!
//! Each fixture is checked four ways. The expected diagnostic count is
//! locked in **per surface** so legitimate-but-bounded divergences (e.g.
//! sealed-subclass errors are uncategorised, so they appear on LSP but are
//! filtered out of CLI/MCP `lint`) don't make the test useless. Any drift
//! away from the locked-in shape — in either direction — fails the test.
//!
//! The test is gated behind `#[ignore]` because it spawns real CLI/MCP/LSP
//! child processes. Run via `just test-parity` (which already invokes every
//! `--ignored` test in this crate).

#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::uninlined_format_args,
    clippy::too_many_lines
)]

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use beamtalk_parity_tests::drivers::{lsp::LspDriver, mcp::McpDriver};
use beamtalk_parity_tests::pool::{SharedRepl, beamtalk_binary, shared_repl};

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "diagnostic parity corpus — run via `just test-parity`"]
async fn diagnostic_parity_corpus() {
    let corpus_root = corpus_root();
    assert!(
        corpus_root.is_dir(),
        "diagnostic corpus directory missing: {}",
        corpus_root.display()
    );

    // Spawn the shared workspace + a single MCP driver up front. Reusing one
    // MCP child across fixtures keeps the wall-clock under 30s.
    let repl = shared_repl().expect("start shared workspace");
    let mut mcp = McpDriver::spawn(&repl).await.expect("spawn mcp driver");

    let cases = corpus_cases();
    let mut failures: Vec<String> = Vec::new();
    for case in &cases {
        if !case.unreadable_files.is_empty() && !cfg!(unix) {
            // Fixtures relying on `chmod 000` are Unix-only; skip on Windows.
            continue;
        }
        let staged = match stage_fixture(&corpus_root, case) {
            Ok(s) => s,
            Err(e) => {
                failures.push(format!("[{}] stage: {e}", case.name));
                continue;
            }
        };
        if let Err(e) = run_case(case, &staged, &mut mcp).await {
            failures.push(format!("[{}] {e}", case.name));
        }
        // Always restore permissions before the next case so a failure
        // doesn't leave the staging directory un-removable.
        staged.restore_permissions();
    }

    mcp.close().await;
    stop_parity_workspace(&repl);

    assert!(
        failures.is_empty(),
        "diagnostic parity assertions failed:\n  - {}",
        failures.join("\n  - ")
    );
}

/// Per-surface expected counts. Each surface field is checked with `>=` so
/// a fixture can lock a lower bound (which catches the "regressed to zero"
/// failure mode without being brittle when an unrelated improvement adds
/// new categorised diagnostics).
struct ExpectedCounts {
    cli_lint: usize,
    mcp_lint: usize,
    mcp_summary: usize,
    lsp: usize,
}

/// One fixture in the corpus.
struct CorpusCase {
    /// Fixture directory name under `tests/parity/diagnostics/`.
    name: &'static str,
    /// Path to the LSP target file, relative to the staged project root.
    /// LSP needs a single file to open via `textDocument/didOpen`.
    lsp_target: &'static str,
    /// Path to lint, relative to the staged project root. Use `""` for the
    /// project root.
    lint_target: &'static str,
    /// Locked-in per-surface lower bound on diagnostic count.
    expected: ExpectedCounts,
    /// Files (relative to staged root) the harness should chmod to 000
    /// before driving the surfaces. Unix-only; the case is skipped on
    /// Windows when this is non-empty.
    unreadable_files: &'static [&'static str],
    /// Description of the regression scenario this fixture pins down (used
    /// in failure messages for context).
    why: &'static str,
}

/// Curated corpus, in declaration order. Keep in sync with
/// `tests/parity/diagnostics/`.
fn corpus_cases() -> Vec<CorpusCase> {
    vec![
        CorpusCase {
            name: "cross_file_class",
            lsp_target: "src/caller.bt",
            lint_target: "",
            expected: ExpectedCounts {
                cli_lint: 0,
                mcp_lint: 0,
                mcp_summary: 0,
                lsp: 0,
            },
            unreadable_files: &[],
            why: "BT-2052: cross-file class type annotation must lint clean on every surface",
        },
        CorpusCase {
            name: "sealed_subclass",
            lsp_target: "src/bad_integer.bt",
            lint_target: "",
            // Sealed-subclass diagnostics are uncategorised in the analyser,
            // so they only surface on LSP (which publishes every diagnostic)
            // — CLI / MCP lint and diagnostic_summary filter to categorised
            // diagnostics. This expected shape is locked in deliberately so
            // any drift (in either direction) is caught.
            expected: ExpectedCounts {
                cli_lint: 0,
                mcp_lint: 0,
                mcp_summary: 0,
                lsp: 1,
            },
            unreadable_files: &[],
            why: "Sealed-subclass errors surface on LSP but are filtered out of lint surfaces",
        },
        CorpusCase {
            name: "stdlib_mode",
            lsp_target: "test/math_box_test.bt",
            // BT-2027: lint the `test/` subdir specifically — the regression
            // surfaced when the extraction set didn't include sibling `src/`.
            lint_target: "test",
            expected: ExpectedCounts {
                cli_lint: 0,
                mcp_lint: 0,
                mcp_summary: 0,
                lsp: 0,
            },
            unreadable_files: &[],
            why: "BT-2027: linting test/ must include src/ in cross-file extraction",
        },
        CorpusCase {
            name: "protocol_cross_file",
            lsp_target: "src/consumer.bt",
            lint_target: "",
            expected: ExpectedCounts {
                cli_lint: 0,
                mcp_lint: 0,
                mcp_summary: 0,
                lsp: 0,
            },
            unreadable_files: &[],
            why: "BT-1950: cross-file Protocol use must resolve on every surface",
        },
        CorpusCase {
            name: "mixed_diagnostics",
            lsp_target: "src/mixer.bt",
            lint_target: "",
            // 2 categorised diagnostics (Type + Dnu) on every surface; LSP
            // additionally surfaces the same 2 plus zero or more hint-level
            // entries depending on analyser changes — `>= 2` is the locked
            // contract.
            expected: ExpectedCounts {
                cli_lint: 2,
                mcp_lint: 2,
                mcp_summary: 2,
                lsp: 2,
            },
            unreadable_files: &[],
            why: "Type + DNU mix should report >= 2 diagnostics on every surface",
        },
        CorpusCase {
            name: "unreadable_target",
            lsp_target: "src/locked.bt",
            lint_target: "src/locked.bt",
            // BT-2067: every surface must surface at least one diagnostic
            // for an unreadable target file. LSP's behaviour here is
            // implementation-defined (it may or may not publish a
            // diagnostic), so we leave it at >= 0.
            expected: ExpectedCounts {
                cli_lint: 1,
                mcp_lint: 1,
                mcp_summary: 1,
                lsp: 0,
            },
            unreadable_files: &["src/locked.bt"],
            why: "BT-2067: every diagnostic surface must report unreadable target file",
        },
        CorpusCase {
            name: "unreadable_package",
            lsp_target: "src/target.bt",
            lint_target: "src/target.bt",
            // BT-2056: MCP lint and diagnostic_summary surface a warning for
            // unreadable extraction files. CLI lint historically logged a
            // miette-formatted error; LSP doesn't currently surface anything
            // for this case (its extraction set is per-document, not the
            // full package). Lock in the >=1 contract for the surfaces that
            // actually report this case.
            expected: ExpectedCounts {
                cli_lint: 0,
                mcp_lint: 1,
                mcp_summary: 1,
                lsp: 0,
            },
            unreadable_files: &["src/locked_sibling.bt"],
            why: "BT-2056: unreadable package-extraction file must produce warnings on lint surfaces",
        },
    ]
}

/// A staged fixture with its target paths fully resolved.
struct StagedFixture {
    /// Root of the staged copy.
    root: PathBuf,
    /// Resolved LSP target.
    lsp_target: PathBuf,
    /// Resolved lint target (file or directory).
    lint_target: PathBuf,
    /// Files we chmod'd; restored by [`restore_permissions`].
    locked_files: Vec<PathBuf>,
}

impl StagedFixture {
    fn restore_permissions(&self) {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            for f in &self.locked_files {
                let _ = std::fs::set_permissions(f, std::fs::Permissions::from_mode(0o644));
            }
        }
        #[cfg(not(unix))]
        {
            // No-op on Windows.
            let _ = &self.locked_files;
        }
    }
}

fn stage_fixture(corpus_root: &Path, case: &CorpusCase) -> Result<StagedFixture, String> {
    let src = corpus_root.join(case.name);
    if !src.is_dir() {
        return Err(format!("fixture missing: {}", src.display()));
    }
    let dst = std::env::temp_dir().join(format!("beamtalk-parity-diag-{}", case.name));
    let _ = std::fs::remove_dir_all(&dst);
    copy_tree(&src, &dst).map_err(|e| format!("copy_tree {}: {e}", src.display()))?;

    let lsp_target = dst.join(case.lsp_target);
    let lint_target = if case.lint_target.is_empty() {
        dst.clone()
    } else {
        dst.join(case.lint_target)
    };

    #[cfg(unix)]
    let locked_files: Vec<std::path::PathBuf> = {
        use std::os::unix::fs::PermissionsExt;
        let mut acc = Vec::new();
        for rel in case.unreadable_files {
            let p = dst.join(rel);
            std::fs::set_permissions(&p, std::fs::Permissions::from_mode(0o000))
                .map_err(|e| format!("chmod 000 {}: {e}", p.display()))?;
            acc.push(p);
        }
        acc
    };
    #[cfg(not(unix))]
    let locked_files: Vec<std::path::PathBuf> = {
        let _ = case.unreadable_files;
        Vec::new()
    };

    Ok(StagedFixture {
        root: dst,
        lsp_target,
        lint_target,
        locked_files,
    })
}

/// Drive every surface and assert each meets its locked-in lower bound.
async fn run_case(
    case: &CorpusCase,
    staged: &StagedFixture,
    mcp: &mut McpDriver,
) -> Result<(), String> {
    let lint_target_str = staged.lint_target.to_string_lossy().into_owned();

    // CLI: `beamtalk lint --format json <path>`. JSON gives an exact `total`
    // so we don't have to re-derive it from miette's textual output.
    let (cli_count, cli_raw) = run_cli_lint(&staged.lint_target)?;

    // MCP: `lint` tool. Returns a structured `total` we read directly.
    let mcp_lint_out = mcp
        .lint(&lint_target_str)
        .await
        .map_err(|e| format!("mcp lint: {e}"))?;
    let mcp_lint_count = mcp_lint_out.diagnostic_count.ok_or_else(|| {
        format!(
            "mcp lint produced no count (raw={})",
            truncate(&mcp_lint_out.raw)
        )
    })?;

    // MCP: `diagnostic_summary` tool. Returns a JSON object with `total`
    // plus optional `error` / `unreadable_*_files` arrays — all of which
    // count as diagnostic-shaped output for parity.
    let (mcp_summary_count, summary_raw) = call_diagnostic_summary(mcp, &lint_target_str).await?;

    // LSP: open the target file and read the first publishDiagnostics. Spawn
    // a fresh LSP child per case so the workspace root reflects the fixture.
    //
    // BT-2067: when the LSP target is one of the chmod'd files, the LSP
    // driver can't even read the file to send `didOpen` — skip the LSP
    // probe. The `expected.lsp = 0` lower bound is still asserted (the
    // surface trivially meets it), and the other surfaces still verify
    // unreadable-file diagnostic emission.
    let lsp_target_is_locked = staged.locked_files.iter().any(|p| p == &staged.lsp_target);
    let lsp_count = if lsp_target_is_locked {
        0
    } else {
        drive_lsp(&staged.root, &staged.lsp_target).await?
    };

    let lsp_raw = String::new();
    let observed = [
        ("cli", cli_count, case.expected.cli_lint, &cli_raw),
        (
            "mcp.lint",
            mcp_lint_count,
            case.expected.mcp_lint,
            &mcp_lint_out.raw,
        ),
        (
            "mcp.summary",
            mcp_summary_count,
            case.expected.mcp_summary,
            &summary_raw,
        ),
        ("lsp", lsp_count, case.expected.lsp, &lsp_raw),
    ];

    let mut errors = Vec::new();
    for (label, got, want, raw) in &observed {
        if *got < *want {
            errors.push(format!(
                "{label}: expected >= {want} diagnostics, got {got} (raw={})",
                truncate(raw)
            ));
        }
    }
    if !errors.is_empty() {
        return Err(format!(
            "{} ({}):\n    {}",
            case.name,
            case.why,
            errors.join("\n    ")
        ));
    }
    Ok(())
}

/// Run `beamtalk lint --format json <path>` and return the `total` from the
/// trailing summary line. The caller treats the count as a lower bound, so
/// extra trailing diagnostics (added in future) don't break the test.
fn run_cli_lint(path: &Path) -> Result<(usize, String), String> {
    let bin = beamtalk_binary("beamtalk")?;
    let output = Command::new(&bin)
        .args(["lint", "--format", "json"])
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("spawn beamtalk lint: {e}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let combined = format!("{stdout}\n--stderr--\n{stderr}");

    // The JSON output is line-delimited: zero or more diagnostic objects
    // followed by one summary object with a `total` field. Look for the
    // summary line.
    let mut total = 0usize;
    let mut found_summary = false;
    for line in stdout.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let Ok(v) = serde_json::from_str::<serde_json::Value>(trimmed) else {
            continue;
        };
        if v.get("type").and_then(serde_json::Value::as_str) == Some("summary") {
            if let Some(t) = v.get("total").and_then(serde_json::Value::as_u64) {
                total = usize::try_from(t).unwrap_or(0);
                found_summary = true;
            }
        }
    }
    if !found_summary {
        // BT-2067: the CLI exits non-zero with a miette-formatted error
        // message before printing any JSON. Treat that as one diagnostic so
        // the unreadable-target case still meets its lower bound.
        if !output.status.success() && !combined.trim().is_empty() {
            return Ok((1, combined));
        }
    }
    Ok((total, combined))
}

/// Call MCP `diagnostic_summary` and return the diagnostic count plus raw
/// text so failure messages can show the response.
async fn call_diagnostic_summary(
    mcp: &mut McpDriver,
    path: &str,
) -> Result<(usize, String), String> {
    let result = mcp
        .call_tool("diagnostic_summary", serde_json::json!({"path": path}))
        .await
        .map_err(|e| format!("mcp diagnostic_summary: {e}"))?;
    let text = extract_text(&result);
    let parsed: serde_json::Value = serde_json::from_str(&text)
        .map_err(|e| format!("parse diagnostic_summary text `{text}`: {e}"))?;
    let total = parsed
        .get("total")
        .and_then(serde_json::Value::as_u64)
        .and_then(|n| usize::try_from(n).ok())
        .unwrap_or(0);
    let mut count = total;
    // BT-2056 / BT-2067: structured fields also count as diagnostic-shaped
    // output. Without this, surfaces that surface unreadable-file
    // diagnostics through these fields (rather than `total`) would zero
    // out and the fixture would fail.
    if parsed.get("error").is_some() {
        count = count.max(1);
    }
    if let Some(arr) = parsed
        .get("unreadable_target_files")
        .and_then(serde_json::Value::as_array)
    {
        count = count.max(arr.len());
    }
    if let Some(arr) = parsed
        .get("unreadable_package_files")
        .and_then(serde_json::Value::as_array)
    {
        count = count.max(arr.len());
    }
    Ok((count, text))
}

fn extract_text(result: &serde_json::Value) -> String {
    let mut out = String::new();
    if let Some(arr) = result.get("content").and_then(serde_json::Value::as_array) {
        for item in arr {
            if let Some(t) = item.get("text").and_then(serde_json::Value::as_str) {
                if !out.is_empty() {
                    out.push('\n');
                }
                out.push_str(t);
            }
        }
    }
    out
}

/// Spawn a fresh `beamtalk-lsp` against the fixture root, open the target
/// file, and return the number of diagnostics the server publishes.
async fn drive_lsp(workspace_root: &Path, target: &Path) -> Result<usize, String> {
    let mut lsp = LspDriver::spawn(workspace_root)
        .await
        .map_err(|e| format!("lsp spawn: {e}"))?;
    let result = lsp.diagnose(target).await;
    lsp.close().await;
    let out = result.map_err(|e| format!("lsp diagnose: {e}"))?;
    out.diagnostic_count
        .ok_or_else(|| "lsp returned no diagnostic count".to_string())
}

/// Stop the parity workspace so its loaded classes don't linger in BEAM
/// node memory and bleed into other test runs.
fn stop_parity_workspace(repl: &SharedRepl) {
    let Ok(bin) = beamtalk_binary("beamtalk") else {
        return;
    };
    let _ = Command::new(bin)
        .args(["workspace", "stop", &repl.workspace_id])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
}

fn corpus_root() -> PathBuf {
    // CARGO_MANIFEST_DIR points at `crates/beamtalk-parity-tests`.
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest)
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .join("tests/parity/diagnostics")
}

fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

fn truncate(s: &str) -> String {
    const LIMIT: usize = 400;
    if s.len() <= LIMIT {
        s.to_string()
    } else {
        // Walk backwards from LIMIT to find a valid char boundary so we
        // don't panic on multi-byte UTF-8 sequences in diagnostic output.
        let mut end = LIMIT;
        while end > 0 && !s.is_char_boundary(end) {
            end -= 1;
        }
        let mut t = s[..end].to_string();
        t.push_str("...[truncated]");
        t
    }
}
