// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk build`.
//!
//! Verifies exit code, the `_build/` artefact set produced by a successful
//! build, and error output when sources fail to parse.

use crate::cli_common;

use predicates::prelude::*;
use predicates::str::contains;

/// ADR 0075 end-to-end regression: a project with a
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
/// type checker (via the function/arity-level registry override), proving
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

/// BT-4378 AC: the package that types its native FFI only through
/// `stubs/*.bt`, under `[diagnostics] type = "error"`, gets the same verdict
/// from `beamtalk build` as from the workspace `load_project` path — the
/// compiler port's
/// `load_project_parity::compile_file_types_ffi_through_project_stubs_like_build`
/// compiles the same checked-in fixture: accepted with the stub, rejected
/// for the untyped FFI call once the stub is gone.
#[test]
fn build_types_ffi_through_project_stubs_like_load_project() {
    let project = cli_common::copied_fixture_project("tests/parity/projects/stubbed_ffi");
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("inferred as Dynamic").not());

    // A plain (non-`--force`) rebuild: the incremental cache keys on the stub
    // tiers, so removing `stubs/` recompiles the unchanged source (BT-4393).
    std::fs::remove_dir_all(project.path().join("stubs")).expect("remove stubs/");
    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .failure()
        .stderr(contains("inferred as Dynamic").and(contains("untyped")));
}

#[test]
fn plain_rebuild_follows_stub_edits() {
    // BT-4393: editing a stub between two plain `beamtalk build` runs must
    // recompile the (unchanged) source against the new stub, not replay the
    // previous build's verdict.
    let project = cli_common::copied_fixture_project("tests/parity/projects/stubbed_ffi");
    let stub = project.path().join("stubs/stubbed_ffi_native.bt");
    let original = std::fs::read_to_string(&stub).expect("read stub");
    let build = || {
        cli_common::beamtalk()
            .current_dir(project.path())
            .arg("build")
            .assert()
    };

    build()
        .success()
        .stderr(contains("inferred as Dynamic").not());

    // The stub no longer declares `answer/0`, so the call is untyped again.
    std::fs::write(&stub, original.replace("answer ->", "renamed ->")).expect("edit stub");
    build()
        .failure()
        .stderr(contains("inferred as Dynamic").and(contains("untyped")));

    // Restoring the stub brings the clean verdict back.
    std::fs::write(&stub, &original).expect("restore stub");
    build()
        .success()
        .stderr(contains("inferred as Dynamic").not());
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

/// Acceptance criterion, exercised through the real `beamtalk build`
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

/// Acceptance criterion: an unchanged file's previously-known
/// diagnostics must still be reported on every `build` invocation, not just
/// the one that first recompiled it. Skipping diagnostic computation
/// entirely for an unchanged file would let a real diagnostic (even a
/// non-fatal `Hint`, as here) show once and then silently vanish from
/// every subsequent plain `build` until something forced a full recompile.
#[test]
fn build_replays_diagnostics_for_unchanged_file_on_every_run() {
    let project = cli_common::fixture_project();
    let greeter = project.path().join("src/Greeter.bt");
    let original = std::fs::read_to_string(&greeter).unwrap();
    // A DNU is only a `Hint` (non-fatal) — the build must still succeed both
    // times, exercising the exact "shown once, then vanishes" trap this test
    // guards against, rather than an error that would fail the build outright.
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

/// A diagnostics-cache miss for an otherwise
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

/// BT-4336 (buccaneer-style cache staleness): a byte-identical file must be
/// re-checked when a signature it type-checks against changes in another
/// file. `App.bt` never changes; only `Lib label`'s declared return type
/// does, which makes `App shout -> String` a type error. Before, the second
/// build replayed `App.bt`'s clean cached diagnostics and passed code a
/// REPL `load-project` rejects; only `--force` or `lint
/// --warnings-as-errors` saw the error. A doc-comment-only edit keeps the
/// signature equal and must not rebuild `App.bt`.
#[test]
fn build_rechecks_unchanged_dependent_when_dependency_signature_changes() {
    let project = tempfile::tempdir().expect("create tempdir");
    let root = project.path();
    std::fs::create_dir_all(root.join("src")).expect("mkdir src");
    std::fs::write(
        root.join("beamtalk.toml"),
        "[package]\nname = \"surface_probe\"\nversion = \"0.1.0\"\n\n\
         [diagnostics]\ntype = \"error\"\n",
    )
    .expect("write beamtalk.toml");
    let lib = root.join("src/Lib.bt");
    let write_lib = |doc: &str, return_type: &str, body: &str| {
        std::fs::write(
            &lib,
            format!("Value subclass: Lib\n\n  /// {doc}\n  label -> {return_type} => {body}\n"),
        )
        .expect("write src/Lib.bt");
    };
    write_lib("The label.", "String", "\"x\"");
    std::fs::write(
        root.join("src/App.bt"),
        "Value subclass: App\n\n  shout -> String => Lib new label\n",
    )
    .expect("write src/App.bt");

    let build = || {
        cli_common::beamtalk()
            .current_dir(root)
            .arg("build")
            .assert()
    };
    build().success();

    // Doc-only edit: the signature is unchanged, so `App.bt` is not rebuilt.
    write_lib("The label, documented.", "String", "\"x\"");
    build().success().stderr(contains("Compiling 1 of 2 files"));

    // Signature change with `App.bt` byte-identical: the build must fail.
    write_lib("The label, documented.", "Integer", "1");
    build()
        .failure()
        .stderr(contains("declares return type String, but body returns"));
}

// ---------------------------------------------------------------------------
// E0401/E0402 visibility checks must fire at build time
// ---------------------------------------------------------------------------

#[test]
fn build_fails_with_e0402_for_cross_file_internal_class_leak() {
    // Regression: `current_package` must be threaded into the
    // CLI build path, or `check_class_visibility` silently emits zero
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
    // Regression (ADR 0108 Semantics): a public type
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
// Genuine cross-package alias collision must be reported once
// ---------------------------------------------------------------------------

#[test]
fn build_reports_genuine_cross_package_alias_collision_exactly_once() {
    // `beamtalk lint` dedupes repeat sightings of a genuine cross-package
    // alias collision across its whole per-file loop. This test proves
    // `beamtalk build` needs no equivalent fix: `execute_build_passes`'s
    // Pass 2 loop (`build.rs`)
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

/// BT-4022: an FFI call to the package's own `native/*.erl` module must
/// not warn `Unknown Erlang module`, while a genuinely unknown module
/// still does. The fixture has never been built (`_build/` does not exist
/// when the command starts), so this also pins the collection ordering:
/// project-known modules are gathered *after* native compilation, once
/// rebar3/erlc have produced the ebin dirs Pass 2 analysis reads.
#[test]
fn build_knows_own_native_module_but_warns_on_unknown() {
    let project = cli_common::fixture_project();
    std::fs::create_dir_all(project.path().join("native")).expect("mkdir native");
    std::fs::write(
        project.path().join("native/my_nif.erl"),
        "%% Copyright 2026 James Casey\n\
         %% SPDX-License-Identifier: Apache-2.0\n\
         -module(my_nif).\n\
         -moduledoc \"Fixture native module for the BT-4022 build test.\".\n\
         -export([ping/0]).\n\
         -doc \"Returns `pong`.\".\n\
         ping() -> pong.\n",
    )
    .expect("write native/my_nif.erl");
    std::fs::write(
        project.path().join("src/FfiUser.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         native_result := Erlang my_nif ping.\n\
         unknown_result := Erlang definitely_unknown_xyz ping.\n",
    )
    .expect("write src/FfiUser.bt");

    cli_common::beamtalk()
        .current_dir(project.path())
        .arg("build")
        .assert()
        .success()
        .stderr(contains("Unknown Erlang module `definitely_unknown_xyz`"))
        .stderr(contains("Unknown Erlang module `my_nif`").not());
}

/// BT-4225: a path dependency's warnings stay its own (cargo shows none for
/// a dependency) — the `untyped FFI` flood a downstream build printed, and an
/// unknown-module warning, never reach the downstream package's output. That
/// the dependency's FFI calls are typed at all is pinned separately by
/// `dependency_type_registry_loads_dep_stubs_and_otp_specs`.
#[test]
fn build_does_not_show_a_dependencys_warnings() {
    let root = tempfile::tempdir().expect("create tempdir");

    let dep_dir = root.path().join("ffidep");
    std::fs::create_dir_all(dep_dir.join("src")).unwrap();
    std::fs::write(
        dep_dir.join("beamtalk.toml"),
        "[package]\nname = \"ffidep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dep_dir.join("src/options.bt"),
        "// Copyright 2026 James Casey\n\
         // SPDX-License-Identifier: Apache-2.0\n\
         \n\
         sealed typed Value subclass: Options\n  \
           field: items :: List = #()\n\
         \n  \
           named -> Object =>\n    \
             Erlang proplists get_value: #name with: self.items\n\
         \n  \
           probe -> Object =>\n    \
             Erlang definitely_unknown_xyz ping\n",
    )
    .unwrap();

    let consumer_dir = root.path().join("consumer");
    std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
    std::fs::write(
        consumer_dir.join("beamtalk.toml"),
        "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\n\
         ffidep = { path = \"../ffidep\" }\n",
    )
    .unwrap();
    std::fs::write(
        consumer_dir.join("src/main.bt"),
        "Object subclass: Main\n  run => 1\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(&consumer_dir)
        .arg("build")
        .assert()
        .success()
        .stderr(contains("untyped FFI").not())
        .stderr(contains("definitely_unknown_xyz").not());
}

/// BT-4387: the root package's `class_corpus.json` lists only the root's own
/// classes. MCP loads it alongside every `_build/deps/*/class_corpus.json`,
/// so a dependency class listed by the root too would appear twice.
#[test]
fn build_root_class_corpus_lists_only_its_own_classes() {
    let root = tempfile::tempdir().expect("create tempdir");

    let dep_dir = root.path().join("helperdep");
    std::fs::create_dir_all(dep_dir.join("src")).unwrap();
    std::fs::write(
        dep_dir.join("beamtalk.toml"),
        "[package]\nname = \"helperdep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    std::fs::write(
        dep_dir.join("src/helper.bt"),
        "Object subclass: Helper\n  assist => 1\n",
    )
    .unwrap();

    let consumer_dir = root.path().join("consumer");
    std::fs::create_dir_all(consumer_dir.join("src")).unwrap();
    std::fs::write(
        consumer_dir.join("beamtalk.toml"),
        "[package]\nname = \"consumer\"\nversion = \"0.1.0\"\n\n\
         [dependencies]\n\
         helperdep = { path = \"../helperdep\" }\n",
    )
    .unwrap();
    std::fs::write(
        consumer_dir.join("src/main.bt"),
        "Helper subclass: Main\n  run => self assist\n",
    )
    .unwrap();

    cli_common::beamtalk()
        .current_dir(&consumer_dir)
        .arg("build")
        .assert()
        .success();

    let corpus_classes = |path: std::path::PathBuf| -> Vec<String> {
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
        let json: serde_json::Value = serde_json::from_str(&text).unwrap();
        json.as_array()
            .unwrap()
            .iter()
            .map(|c| c["name"].as_str().unwrap().to_string())
            .collect()
    };
    let build = consumer_dir.join("_build");
    let root_classes = corpus_classes(build.join("dev/class_corpus.json"));
    let dep_classes = corpus_classes(build.join("deps/helperdep/class_corpus.json"));
    assert_eq!(root_classes, ["Main"], "root corpus: {root_classes:?}");
    assert_eq!(
        dep_classes,
        ["Helper"],
        "dependency corpus: {dep_classes:?}"
    );

    // What MCP loads: the root corpus, then every dependency's.
    let all: Vec<&String> = root_classes.iter().chain(&dep_classes).collect();
    let unique: std::collections::BTreeSet<&&String> = all.iter().collect();
    assert_eq!(unique.len(), all.len(), "duplicate corpus entries: {all:?}");
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
