// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk build` (BT-2084).
//!
//! Verifies exit code, the `_build/` artefact set produced by a successful
//! build, and error output when sources fail to parse.

mod cli_common;

use predicates::prelude::*;
use predicates::str::contains;

#[test]
fn build_clean_project_produces_beam_artefacts() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();

    // Successful builds emit at least one .beam under _build/.
    let build_dir = project.path().join("_build");
    let beam = find_first_beam(&build_dir);
    assert!(
        beam.is_some(),
        "expected a .beam under _build/; tree:\n{:#?}",
        list_tree(&build_dir, 4)
    );
}

#[test]
fn build_emits_error_for_unparseable_source() {
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/Bad.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         this is not valid beamtalk syntax\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .failure()
        .stderr(contains("Failed to compile").or(contains("error")));
}

#[test]
fn build_force_recompiles_unchanged_sources() {
    let project = cli_common::fixture_project();
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();

    // Second pass without --force is a no-op…
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        // Incremental builds report "unchanged — nothing to compile" on stderr.
        .stderr(contains("unchanged").or(contains("nothing to compile")));

    // …but --force re-runs the full pipeline.
    cli_common::beamtalk()
        .current_dir(project.path())
        .args(["build", "--force"])
        .assert()
        .success();
}

// ---------------------------------------------------------------------------
// BT-2920: E0401/E0402 visibility checks must fire at build time
// ---------------------------------------------------------------------------

#[test]
fn build_fails_with_e0402_for_cross_file_internal_class_leak() {
    // Regression for BT-2920: `current_package` was never threaded into the
    // CLI build path, so `check_class_visibility` silently emitted zero
    // diagnostics. Mirrors docs/beamtalk-language-features.md's
    // TokenBuffer/Parser example — the internal class lives in a *sibling*
    // file from the public method that leaks it, exercising the cross-file
    // package-stamping fix (`ClassHierarchy::stamp_package_on_infos`), not
    // just same-file visibility.
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/TokenBuffer.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         internal Object subclass: TokenBuffer\n\
         \x20\x20data => nil\n",
    )
    .unwrap();
    std::fs::write(
        project.path().join("src/Parser.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         Object subclass: Parser\n\
         \x20\x20tokenize: input :: String -> TokenBuffer => nil\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .failure()
        // miette word-wraps the message to terminal width, so match the two
        // halves separately rather than the exact single-line string.
        .stderr(
            contains("Internal class 'TokenBuffer' appears in public signature of")
                .and(contains("tokenize:'")),
        );
}

#[test]
fn build_fails_with_e0402_for_internal_type_alias_leak() {
    // Regression for BT-2920 (ADR 0108 Semantics, BT-2898): a public type
    // alias whose expansion transitively reaches an internal alias must fail
    // the build with E0402, not just an LSP squiggle.
    let project = cli_common::fixture_project();
    std::fs::write(
        project.path().join("src/Types.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         internal type Priv = Integer\n\
         type Pub = Priv | String\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .failure()
        // See the comment in the class-leak test above re: miette wrapping.
        .stderr(
            contains("Internal type alias 'Priv' appears in the expansion of public type alias")
                .and(contains("'Pub'")),
        );
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn find_first_beam(root: &std::path::Path) -> Option<std::path::PathBuf> {
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|e| e == "beam") {
                return Some(path);
            }
        }
    }
    None
}

fn list_tree(root: &std::path::Path, depth: usize) -> Vec<std::path::PathBuf> {
    fn walk(dir: &std::path::Path, depth: usize, out: &mut Vec<std::path::PathBuf>) {
        if depth == 0 {
            return;
        }
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            out.push(path.clone());
            if path.is_dir() {
                walk(&path, depth - 1, out);
            }
        }
    }
    let mut out = Vec::new();
    walk(root, depth, &mut out);
    out
}
