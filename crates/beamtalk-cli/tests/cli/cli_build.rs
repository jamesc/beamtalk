// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk build` (BT-2084).
//!
//! Verifies exit code, the `_build/` artefact set produced by a successful
//! build, and error output when sources fail to parse.

use crate::cli_common;

use predicates::prelude::*;
use predicates::str::contains;

/// ADR 0075 / BT-1846 / BT-1847 end-to-end regression: a project with a
/// `stubs/` directory must build successfully — `stubs/*.bt` is scanned
/// separately by `load_project_stub_registry` and must never reach the
/// ordinary compile pipeline, where `declare native:` is a hard error
/// ("only valid in stubs/ directory"). A prior version of this feature
/// only exercised `load_project_stub_registry` in isolation and missed
/// that `find_source_files` swept `stubs/` into `env.source_files` too.
#[test]
fn build_succeeds_with_project_local_stubs_directory() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("stubs")).expect("mkdir stubs");
    std::fs::write(
        project.path().join("stubs/lists.bt"),
        "declare native: lists\n  reverse: list :: List -> List\n",
    )
    .expect("write stubs/lists.bt");

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();
}

/// Companion to the above: the stub isn't just tolerated, it's actually
/// consumed — a stubbed function's argument-type mismatch is caught by the
/// type checker (BT-1847's function/arity-level registry override), proving
/// `stubs/` is both excluded from ordinary compilation and loaded into the
/// FFI type registry in the same build.
#[test]
fn build_enforces_stub_declared_argument_types() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("stubs")).expect("mkdir stubs");
    std::fs::write(
        project.path().join("stubs/lists.bt"),
        "declare native: lists\n  reverse: list :: List -> List\n",
    )
    .expect("write stubs/lists.bt");
    std::fs::write(
        project.path().join("src/StubUser.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         result := Erlang lists reverse: 42.\n",
    )
    .expect("write src/StubUser.bt");

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .stderr(contains("expects List").and(contains("got Integer")));
}

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

/// BT-3120 acceptance criterion, exercised through the real `beamtalk build`
/// subprocess rather than the internal `detect_changes`/`partition_files`
/// unit tests: rewriting a source file with byte-for-byte identical content
/// (e.g. `touch`, or an editor save-without-edit) bumps its mtime but must
/// not trigger recompilation — only a genuine content change should. An
/// mtime-keyed cache would recompile on the touch alone; a content-hash-keyed
/// one must not.
#[test]
fn build_touch_without_content_change_is_not_recompiled() {
    let project = cli_common::fixture_project();
    let greeter = project.path().join("src/Greeter.bt");
    let original = std::fs::read_to_string(&greeter).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success();

    // Rewrite the exact same bytes — this changes the file's mtime (most
    // filesystems have at best millisecond, often only second, resolution,
    // so sleep past that) but not its content.
    std::thread::sleep(std::time::Duration::from_millis(1100));
    std::fs::write(&greeter, &original).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("unchanged").or(contains("nothing to compile")));

    // A genuine content change on the same file must still be recompiled.
    std::fs::write(&greeter, format!("{original}\n  goodbye => \"goodbye\"\n")).unwrap();

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        // Only `src/Greeter.bt` is a build source in this fixture (the test
        // file under `test/` isn't part of `beamtalk build`'s source set),
        // so a genuine change to it recompiles the project's one file —
        // i.e. NOT the "all unchanged" no-op message from the touch above.
        .stderr(contains("unchanged").not());
}

/// BT-3410 acceptance criterion: an unchanged file's previously-known
/// diagnostics must still be reported on every `build` invocation, not just
/// the one that first recompiled it. Before the fix, `beamtalk build`'s
/// incremental change-detection skipped diagnostic computation entirely for
/// an unchanged file — a real diagnostic (even a non-fatal `Hint`, as here)
/// was shown once and then silently vanished from every subsequent plain
/// `build` until something forced a full recompile.
#[test]
fn build_replays_diagnostics_for_unchanged_file_on_every_run() {
    let project = cli_common::fixture_project();
    let greeter = project.path().join("src/Greeter.bt");
    let original = std::fs::read_to_string(&greeter).unwrap();
    // A DNU is only a `Hint` (non-fatal) — the build must still succeed both
    // times, exercising the exact "shown once, then vanishes" trap from the
    // issue rather than an error that would fail the build outright.
    std::fs::write(
        &greeter,
        format!("{original}\n  typo => 3 fooBarBaz: 1 quux: 2\n"),
    )
    .unwrap();

    // First build actually compiles the file and reports the diagnostic.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("does not understand"));

    // Second build sees no content change and skips recompilation — the
    // diagnostic must still be reported, replayed from the incremental cache.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(
            contains("unchanged")
                .or(contains("nothing to compile"))
                .and(contains("does not understand")),
        );

    // A third build confirms this isn't a one-time carry-over from the first
    // (changed) build's own in-process state.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("does not understand"));
}

/// BT-3410 review follow-up: a diagnostics-cache miss for an otherwise
/// unchanged file (simulating a fresh sidecar right after upgrading to this
/// feature, corruption, or a version bump) must self-heal within the *same*
/// build — recompiling just that file — rather than silently reporting
/// nothing for it until its content changes or `--force` is used.
#[test]
fn build_recompiles_unchanged_file_on_diagnostics_cache_miss() {
    let project = cli_common::fixture_project();
    let greeter = project.path().join("src/Greeter.bt");
    let original = std::fs::read_to_string(&greeter).unwrap();
    std::fs::write(
        &greeter,
        format!("{original}\n  typo => 3 fooBarBaz: 1 quux: 2\n"),
    )
    .unwrap();

    // First build populates both the beam-hash and diagnostics sidecars.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("does not understand"));

    let diagnostics_cache = project
        .path()
        .join("_build/dev/ebin/.beamtalk-diagnostics-cache.json");
    assert!(
        diagnostics_cache.exists(),
        "expected a diagnostics cache sidecar at {diagnostics_cache:?} after the first build"
    );
    std::fs::remove_file(&diagnostics_cache).unwrap();

    // Second build: the beam-hash cache still says Greeter.bt is unchanged,
    // but the diagnostics cache has no entry for it — this must trigger a
    // real recompile of just that file (not a silent "nothing to compile"
    // that drops the diagnostic).
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("does not understand").and(contains("nothing to compile").not()));

    // Third build: the diagnostics cache is repopulated, so this returns to
    // the normal unchanged/replay path.
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(
            contains("does not understand")
                .and(contains("unchanged").or(contains("nothing to compile"))),
        );
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
// BT-3044: genuine cross-package alias collision must be reported once
// ---------------------------------------------------------------------------

#[test]
fn build_reports_genuine_cross_package_alias_collision_exactly_once() {
    // BT-3044 (follow-up from BT-3043): `beamtalk lint` was fixed to dedupe
    // repeat sightings of a genuine cross-package alias collision across its
    // whole per-file loop. This test proves `beamtalk build` never needed the
    // equivalent fix: `execute_build_passes`'s Pass 2 loop (`build.rs`)
    // propagates each file's compile error with `?`, so it stops at the
    // *first* file whose `AliasRegistry::add_pre_loaded` seeding rediscovers
    // the collision — the diagnostic is inherently printed once, then the
    // whole build aborts, rather than surviving to re-diagnose it on every
    // subsequent file the way an error-tolerant pass (like lint) would.
    //
    // `--force` bypasses incremental change detection so every file is a
    // "changed" file, matching the acceptance criterion's "full/clean build,
    // no incremental skip".
    let root = tempfile::tempdir().expect("create tempdir");

    for (pkg, rhs) in [("producer_a", "String"), ("producer_b", "Integer")] {
        let dir = root.path().join(pkg);
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::write(
            dir.join("beamtalk.toml"),
            format!("[package]\nname = \"{pkg}\"\nversion = \"0.1.0\"\n"),
        )
        .unwrap();
        std::fs::write(dir.join("src/types.bt"), format!("type Shared = {rhs}\n")).unwrap();
    }

    let consumer_dir = root.path().join("consumer");
    std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
    std::fs::write(
        consumer_dir.join("beamtalk.toml"),
        "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\n\
         producer_a = { path = \"../producer_a\" }\n\
         producer_b = { path = \"../producer_b\" }\n",
    )
    .unwrap();
    for i in 1..=5 {
        std::fs::write(
            consumer_dir.join(format!("src/File{i}.bt")),
            format!("Object subclass: File{i}\n  foo => 1\n"),
        )
        .unwrap();
    }

    cli_common::beamtalk()
        .current_dir(&consumer_dir)
        .args(["build", "--force"])
        .assert()
        .failure()
        .stderr(contains("Pre-loaded type alias `Shared` collides"))
        // Count on a short, unwrapped prefix rather than the full sentence —
        // miette word-wraps the message to terminal width, so "collides
        // with another type alias" and "of the same name" can land on
        // separate lines and defeat a substring match spanning the wrap.
        .stderr(predicate::function(|out: &str| {
            out.matches("Pre-loaded type alias `Shared` collides")
                .count()
                == 1
        }));
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
