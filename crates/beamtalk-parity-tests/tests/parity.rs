// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-surface parity integration test (BT-2077).
//!
//! Runs every `tests/parity/cases/*.parity.bt` case through the surfaces
//! declared in its header and asserts that the normalized outputs agree.
//!
//! All `#[test]`s here are gated behind `#[ignore]` because they spawn real
//! workspaces and child binaries. Invoke via `just test-parity`.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use beamtalk_parity_tests::cases::{Case, Expectation, Op, Step, Surface, discover};
use beamtalk_parity_tests::drivers::{
    SurfaceOutput, cli as cli_driver, lsp::LspDriver, mcp::McpDriver, repl::ReplDriver,
};
use beamtalk_parity_tests::pool::{SharedRepl, shared_repl};

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
#[ignore = "parity harness — run via `just test-parity`"]
async fn parity_suite() {
    let cases_dir = parity_root().join("cases");
    let case_paths = discover(&cases_dir).expect("discover parity cases");
    assert!(
        !case_paths.is_empty(),
        "no parity cases found under {}",
        cases_dir.display()
    );

    let repl = shared_repl().expect("start shared workspace");
    // The MCP driver is shared across cases too — re-using a single child
    // process keeps wall-clock down.
    let mut mcp = McpDriver::spawn(&repl).await.expect("spawn mcp driver");
    let mut repl_driver = ReplDriver::connect(&repl)
        .await
        .expect("connect repl driver");

    // Stage the project fixture once for any case that needs it.
    let staged_project = stage_simple_project();
    let staged_bad_file = stage_diagnostic_file();
    let staged_test_runner = stage_test_runner_project();
    let staged_mixed = stage_mixed_project();

    // BT-2089: pre-load every test-fixture project on the REPL and MCP
    // workspaces so all `Op::Test` cases see every class regardless of
    // load order. Workspace loads now accumulate across projects, so this
    // is a one-time setup rather than the per-class workaround that BT-2080
    // used to need.
    if let Some(p) = staged_project.as_deref() {
        let path = p.to_string_lossy();
        repl_driver
            .load_project_with_tests(&path)
            .await
            .map_err(|e| format!("repl preload {}: {e}", p.display()))
            .expect("preload simple_project on repl");
        mcp.load_project_with_tests(&path)
            .await
            .map_err(|e| format!("mcp preload {}: {e}", p.display()))
            .expect("preload simple_project on mcp");
    }
    if let Some(p) = staged_test_runner.as_deref() {
        let path = p.to_string_lossy();
        repl_driver
            .load_project_with_tests(&path)
            .await
            .map_err(|e| format!("repl preload {}: {e}", p.display()))
            .expect("preload test_runner_project on repl");
        mcp.load_project_with_tests(&path)
            .await
            .map_err(|e| format!("mcp preload {}: {e}", p.display()))
            .expect("preload test_runner_project on mcp");
    }

    let mut failures: Vec<String> = Vec::new();
    for path in &case_paths {
        let case = match Case::from_path(path) {
            Ok(c) => c,
            Err(e) => {
                failures.push(format!("[{}] parse: {e}", path.display()));
                continue;
            }
        };
        for step in &case.steps {
            let resolved = resolve_placeholders(
                &step.input,
                staged_project.as_deref(),
                staged_bad_file.as_deref(),
                staged_test_runner.as_deref(),
                staged_mixed.as_deref(),
            );
            let outcome = run_step(step, &resolved, &mut repl_driver, &mut mcp, &repl).await;
            if let Err(msg) = outcome {
                failures.push(format!(
                    "[{}:{}] {} — {}",
                    case.path.display(),
                    step.line,
                    surfaces_label(&step.surfaces),
                    msg
                ));
            }
        }
    }

    mcp.close().await;

    // Stop the parity workspace so its loaded classes don't linger in BEAM
    // node memory and bleed into other test runs (the workspace would
    // otherwise stay alive for the 5-minute idle timeout).
    stop_parity_workspace(&repl);

    assert!(
        failures.is_empty(),
        "parity assertions failed:\n  - {}",
        failures.join("\n  - ")
    );
}

fn stop_parity_workspace(repl: &SharedRepl) {
    use beamtalk_parity_tests::pool::beamtalk_binary;
    use std::process::{Command, Stdio};
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

/// Run a single step against every declared surface and assert agreement.
async fn run_step(
    step: &Step,
    input: &str,
    repl: &mut ReplDriver,
    mcp: &mut McpDriver,
    shared: &SharedRepl,
) -> Result<(), String> {
    let mut outputs: Vec<(Surface, SurfaceOutput)> = Vec::new();
    for &surface in &step.surfaces {
        let out = drive(step, input, surface, repl, mcp, shared).await?;
        outputs.push((surface, out));
    }

    match &step.expect {
        Expectation::Value(expected) => {
            for (surface, out) in &outputs {
                let got = out.value.as_deref().ok_or_else(|| {
                    format!("surface {surface} produced no value (raw={})", out.raw)
                })?;
                if got != expected.as_str() {
                    return Err(format!(
                        "value mismatch on {surface}: expected `{expected}`, got `{got}` (raw={})",
                        out.raw
                    ));
                }
            }
        }
        Expectation::Classes(expected) => {
            for (surface, out) in &outputs {
                let got = out.classes.as_ref().ok_or_else(|| {
                    format!("surface {surface} produced no class set (raw={})", out.raw)
                })?;
                if !expected.is_subset(got) {
                    let missing: BTreeSet<_> = expected.difference(got).collect();
                    return Err(format!(
                        "missing classes on {surface}: {:?} (raw={})",
                        missing, out.raw
                    ));
                }
            }
        }
        Expectation::DiagnosticCount(expected) => {
            for (surface, out) in &outputs {
                let got = out.diagnostic_count.ok_or_else(|| {
                    format!(
                        "surface {surface} produced no diagnostic count (raw={})",
                        out.raw
                    )
                })?;
                let agrees = if *expected == 0 {
                    got == 0
                } else {
                    got >= *expected
                };
                if !agrees {
                    return Err(format!(
                        "diagnostic count mismatch on {surface}: expected {expected}, got {got} (raw={})",
                        out.raw
                    ));
                }
            }
        }
    }
    Ok(())
}

async fn drive(
    step: &Step,
    input: &str,
    surface: Surface,
    repl: &mut ReplDriver,
    mcp: &mut McpDriver,
    shared: &SharedRepl,
) -> Result<SurfaceOutput, String> {
    let _ = shared; // kept in the signature for future use (workspace-id-aware drivers)
    match (surface, step.op) {
        (Surface::Repl, Op::Eval) => repl.eval(input).await,
        (Surface::Mcp, Op::Eval) => mcp.evaluate(input).await,

        (Surface::Repl, Op::Load) => repl.load_project(input).await,
        (Surface::Mcp, Op::Load) => mcp.load_project(input).await,
        (Surface::Cli, Op::Load) => cli_driver::build_project(Path::new(input)),

        (Surface::Mcp, Op::Lint) => mcp.lint(input).await,
        (Surface::Cli, Op::Lint) => cli_driver::lint(Path::new(input)),

        (Surface::Repl, Op::Test) => {
            // BT-2089: workspace project loads now accumulate, so we no
            // longer need the per-class pre-load workaround. Both
            // `simple_project` and `test_runner_project` are loaded once
            // at the start of `parity_suite`; this branch just runs the
            // test against the already-loaded workspace.
            repl.test_class(input).await
        }
        (Surface::Mcp, Op::Test) => {
            // BT-2089: see Repl branch above.
            mcp.test_class(input).await
        }
        (Surface::Cli, Op::Test) => {
            // CLI has no class registry, so map the input class name to its
            // staged test file. Falls back to the default `simple_project`
            // dir when the input isn't a known test-runner class — that
            // preserves the BT-2077 behaviour for `CounterTest`.
            let path = cli_test_path_for_class(input);
            cli_driver::test_project(&path)
        }

        (Surface::Cli, Op::Diagnose) => cli_driver::diagnose(Path::new(input)),
        (Surface::Mcp, Op::Diagnose) => mcp.diagnose_file(input).await,
        (Surface::Lsp, Op::Diagnose) => {
            let parent = Path::new(input).parent().unwrap_or(Path::new("/"));
            let mut lsp = LspDriver::spawn(parent)
                .await
                .map_err(|e| format!("lsp spawn: {e}"))?;
            let result = lsp.diagnose(Path::new(input)).await;
            lsp.close().await;
            result
        }

        (s, op) => Err(format!(
            "Surface `{s}` does not support op `{op:?}` (input=`{input}`)"
        )),
    }
}

/// Stage `tests/parity/fixtures/simple_project/` to a stable temp directory.
///
/// The directory is reused across cases so the workspace pool can keep one
/// `load-project` cache hot. Returns `None` if the fixture is absent (which
/// is treated as a hard test failure later, but we don't panic here so the
/// case loop can still produce a useful error message).
fn stage_simple_project() -> Option<PathBuf> {
    let src = parity_root().join("fixtures/simple_project");
    if !src.exists() {
        return None;
    }
    let dst = std::env::temp_dir().join("beamtalk-parity-simple");
    let _ = std::fs::remove_dir_all(&dst);
    copy_tree(&src, &dst).ok()?;
    Some(dst)
}

/// Stage `tests/parity/fixtures/test_runner_project/` (BT-2080).
///
/// Counterpart to [`stage_simple_project`] for the test-runner parity suite.
/// Each fixture project lives in a stable temp directory so the harness can
/// pre-load it on every `Op::Test` invocation without re-staging.
fn stage_test_runner_project() -> Option<PathBuf> {
    let src = parity_root().join("fixtures/test_runner_project");
    if !src.exists() {
        return None;
    }
    let dst = std::env::temp_dir().join("beamtalk-parity-test-runner");
    let _ = std::fs::remove_dir_all(&dst);
    copy_tree(&src, &dst).ok()?;
    Some(dst)
}

/// Stage `tests/parity/projects/mixed/` to a stable temp directory so the
/// BT-2079 load-project parity case can drive `:sync` / `load_project` /
/// `beamtalk build` against the same on-disk tree across surfaces.
///
/// The fixture lives under `tests/parity/projects/` (sibling of `fixtures/`)
/// to keep the larger BT-2079 project tree separate from the small
/// per-case fixtures used by the original BT-2077 cases.
fn stage_mixed_project() -> Option<PathBuf> {
    let src = parity_root().join("projects/mixed");
    if !src.exists() {
        return None;
    }
    let dst = std::env::temp_dir().join("beamtalk-parity-mixed");
    let _ = std::fs::remove_dir_all(&dst);
    copy_tree(&src, &dst).ok()?;
    Some(dst)
}

/// Class-name → test-runner-project test file mapping.
///
/// Used by [`cli_test_path_for_class`] to map a class name to the
/// staged test file. (BT-2089: REPL/MCP no longer need a class→project
/// lookup because both fixture projects are pre-loaded at the start of
/// the parity suite.)
const TEST_RUNNER_CLASSES: &[(&str, &str)] = &[
    ("PassingRunnerTest", "passing_test.bt"),
    ("AssertFailRunnerTest", "asserting_fail_test.bt"),
    ("SetupErrorRunnerTest", "setup_error_test.bt"),
    ("TeardownErrorRunnerTest", "teardown_error_test.bt"),
    ("ActorStateRunnerTest", "actor_state_test.bt"),
    ("TempDirRunnerTest", "temp_dir_test.bt"),
];

/// Map a `TestCase` class name to the staged test file that defines it.
///
/// CLI `beamtalk test` operates on a path, not a class name; this lookup
/// gives the test-runner parity case (BT-2080) per-class CLI scoping that
/// matches the REPL/MCP `:test ClassName` semantics. Unknown class names
/// fall back to the BT-2077 `simple_project` directory so `CounterTest`
/// keeps working unchanged.
fn cli_test_path_for_class(class: &str) -> PathBuf {
    let runner_root = std::env::temp_dir().join("beamtalk-parity-test-runner");
    if let Some((_, file)) = TEST_RUNNER_CLASSES.iter().find(|(c, _)| *c == class) {
        runner_root.join("test").join(file)
    } else {
        std::env::temp_dir().join("beamtalk-parity-simple")
    }
}

fn stage_diagnostic_file() -> Option<PathBuf> {
    let src = parity_root().join("fixtures/diagnostic/bad_syntax.bt");
    if !src.exists() {
        return None;
    }
    let dst_dir = std::env::temp_dir().join("beamtalk-parity-diagnostic");
    let _ = std::fs::create_dir_all(&dst_dir);
    let dst = dst_dir.join("bad_syntax.bt");
    let _ = std::fs::remove_file(&dst);
    std::fs::copy(&src, &dst).ok()?;
    Some(dst)
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

fn parity_root() -> PathBuf {
    // CARGO_MANIFEST_DIR points at `crates/beamtalk-parity-tests`.
    let manifest = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest)
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .join("tests/parity")
}

fn resolve_placeholders(
    input: &str,
    project: Option<&Path>,
    bad_file: Option<&Path>,
    test_runner_project: Option<&Path>,
    mixed_project: Option<&Path>,
) -> String {
    let mut out = input.to_string();
    if let Some(p) = project {
        out = out.replace("<project>", &p.to_string_lossy());
    }
    if let Some(p) = bad_file {
        out = out.replace("<bad_file>", &p.to_string_lossy());
    }
    if let Some(p) = test_runner_project {
        out = out.replace("<test_runner_project>", &p.to_string_lossy());
    }
    if let Some(p) = mixed_project {
        out = out.replace("<mixed_project>", &p.to_string_lossy());
    }
    out
}

fn surfaces_label(surfaces: &BTreeSet<Surface>) -> String {
    surfaces
        .iter()
        .map(|s| s.label())
        .collect::<Vec<_>>()
        .join(",")
}
