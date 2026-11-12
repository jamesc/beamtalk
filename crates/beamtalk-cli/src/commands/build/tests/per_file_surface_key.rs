// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Per-file incremental keys over the consulted surface (BT-4690): a signature
//! change rebuilds the files that consulted it, never fewer than a forced full
//! rebuild would change.

use super::*;
use crate::commands::build::class_index::ClassIndexResult;
use std::collections::{BTreeMap, BTreeSet};

/// A package on disk, built the way `execute_build_passes` builds it.
struct Pkg {
    _temp: TempDir,
    root: Utf8PathBuf,
}

/// What compiling one file produced: its diagnostics and generated Core Erlang.
type Outputs = BTreeMap<String, String>;

impl Pkg {
    fn new(files: &[(&str, &str)]) -> Self {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::create_dir_all(root.join("src")).unwrap();
        write_test_file(
            &root.join("beamtalk.toml"),
            "[package]\nname = \"test_pkg\"\nversion = \"0.1.0\"\n",
        );
        let pkg = Self { _temp: temp, root };
        for (name, content) in files {
            pkg.write(name, Some(content));
        }
        pkg
    }

    fn write(&self, name: &str, content: Option<&str>) {
        let path = self.root.join("src").join(name);
        match content {
            Some(content) => write_test_file(&path, content),
            None => fs::remove_file(&path).unwrap(),
        }
    }

    fn env(&self) -> BuildEnvironment {
        setup_build_environment(self.root.as_str()).unwrap()
    }

    fn index(env: &BuildEnvironment, force: bool) -> ClassIndexResult {
        let dep_ctx = DependencyContext {
            resolved_deps: Vec::new(),
            has_native_deps: false,
        };
        build_class_index(env, &dep_ctx, &default_options(), force).unwrap()
    }
}

fn edges_of(index: &ClassIndexResult) -> BuildGraphEdges {
    BuildGraphEdges {
        file_protocol_uses: index.file_protocol_uses.clone(),
        protocol_hashes: crate::commands::util::protocol_hashes(
            &index.all_protocol_defs,
            index.file_protocol_uses.values().flatten(),
            None,
        ),
        analysis_surface_hash: index.analysis_surface_hash.clone(),
        stub_inputs_hash: String::new(),
        surface_keys: index.surface_keys.clone(),
    }
}

/// Compile `files` as Pass 2 does and return each file's output and consulted surface.
fn compile_files(
    env: &BuildEnvironment,
    index: &ClassIndexResult,
    pairs: &[(Utf8PathBuf, String, Utf8PathBuf)],
    files: &[Utf8PathBuf],
) -> Vec<(String, String, ConsultedSurface)> {
    let ctx = CompileContext {
        hierarchy: ClassHierarchyContext {
            class_module_index: index.class_module_index.clone(),
            class_superclass_index: index.class_superclass_index.clone(),
            pre_loaded_classes: index.all_class_infos.clone(),
            pre_loaded_protocols: index.all_protocol_infos.clone(),
            pre_loaded_protocol_defs: index.all_protocol_defs.clone(),
            pre_loaded_protocol_sources: index.all_protocol_sources.clone(),
            pre_loaded_aliases: index.all_alias_infos.clone(),
            extension_index: index.extension_index.clone(),
            dependency_extensions_complete: index.dependency_extensions_complete,
        },
        ..CompileContext::default()
    };
    fs::create_dir_all(&env.build_dir).unwrap();
    let mut out = Vec::new();
    for (file, module, core_file) in pairs {
        if !files.contains(file) {
            continue;
        }
        let (diags, surface) =
            compile_file_with_surface(file, module, core_file, &default_options(), &ctx, None)
                .unwrap_or_else(|e| panic!("{file}: {e:?}"));
        let mut text = diags
            .iter()
            .map(|d| format!("{:?}: {}", d.severity, d.message))
            .collect::<Vec<_>>()
            .join("\n");
        text.push_str("\n--core--\n");
        text.push_str(&fs::read_to_string(core_file).unwrap());
        out.push((file.file_name().unwrap().to_string(), text, surface));
    }
    out
}

fn names(files: &[Utf8PathBuf]) -> Vec<String> {
    let mut v: Vec<String> = files
        .iter()
        .map(|f| f.file_name().unwrap().to_string())
        .collect();
    v.sort();
    v
}

/// One incremental build, recorded as a real successful build records it:
/// detect, compile the changed files, re-key them on what they consulted, and
/// save the sidecar. Returns the (changed, unchanged) file names and what the
/// changed files produced.
fn build(pkg: &Pkg) -> (Vec<String>, Vec<String>, Outputs) {
    let env = pkg.env();
    let index = Pkg::index(&env, false);
    let edges = edges_of(&index);
    let pairs = compute_file_module_pairs(&env).unwrap();
    fs::create_dir_all(&env.build_dir).unwrap();
    let mut changes = detect_changes(
        &env.source_files,
        &env.build_dir,
        &pairs,
        false,
        &index.source_hashes,
        &edges,
    );
    let mut outputs = Outputs::new();
    for (name, text, surface) in compile_files(&env, &index, &pairs, &changes.changed_files) {
        let file = env
            .source_files
            .iter()
            .find(|f| f.file_name() == Some(&name));
        super::super::changes::record_compiled_surface(
            &mut changes.source_hashes,
            &edges,
            file.unwrap(),
            &surface,
        );
        outputs.insert(name, text);
    }
    for (_, module, _) in &pairs {
        write_test_file(&env.build_dir.join(format!("{module}.beam")), "BEAM");
    }
    super::super::super::build_cache::save_beam_hash_cache(&env.build_dir, &changes.source_hashes);
    (
        names(&changes.changed_files),
        names(&changes.unchanged_files),
        outputs,
    )
}

/// The files the next build would rebuild, without building anything.
fn plan(pkg: &Pkg) -> BTreeSet<String> {
    let env = pkg.env();
    let index = Pkg::index(&env, false);
    let pairs = compute_file_module_pairs(&env).unwrap();
    let changes = detect_changes(
        &env.source_files,
        &env.build_dir,
        &pairs,
        false,
        &index.source_hashes,
        &edges_of(&index),
    );
    names(&changes.changed_files).into_iter().collect()
}

/// Every file compiled from scratch against the package as it is now.
fn full_outputs(pkg: &Pkg) -> Outputs {
    let env = pkg.env();
    let index = Pkg::index(&env, true);
    let pairs = compute_file_module_pairs(&env).unwrap();
    compile_files(&env, &index, &pairs, &env.source_files)
        .into_iter()
        .map(|(name, text, _)| (name, text))
        .collect()
}

const FIXTURE: &[(&str, &str)] = &[
    (
        "base.bt",
        "Value subclass: Base\n  one -> Integer => 1\n  class limit -> Integer => 10\n",
    ),
    (
        "sub.bt",
        "Base subclass: Sub\n  two -> Integer => self one + 1\n",
    ),
    (
        "user_base.bt",
        "Value subclass: UserBase\n  run -> Integer => Base new one\n",
    ),
    (
        "user_sub.bt",
        "Value subclass: UserSub\n  run -> Integer => Sub new two\n",
    ),
    (
        "user_limit.bt",
        "Value subclass: UserLimit\n  run -> Integer => Base limit\n",
    ),
    (
        "leaf.bt",
        "Value subclass: Leaf\n  /// The value.\n  value -> Integer => 7\n",
    ),
    (
        "user_leaf.bt",
        "Value subclass: UserLeaf\n  run -> Integer => Leaf new value\n",
    ),
    (
        "unrelated.bt",
        "Value subclass: Unrelated\n  run -> Integer => 3\n",
    ),
    (
        "tagged.bt",
        "Protocol define: Tagged\n  name -> String\n\n  tag -> String => self name\n",
    ),
    (
        "widget.bt",
        "Object subclass: Widget\n  uses: Tagged\n  name -> String => \"w\"\n",
    ),
    (
        "user_widget.bt",
        "Object subclass: UserWidget\n  run: w :: Widget -> String => w tag\n",
    ),
    ("ext.bt", "String >> shout -> Integer => 1\n"),
    (
        "user_ext.bt",
        "Value subclass: UserExt\n  run -> Integer => \"x\" shout\n",
    ),
    (
        "compass.bt",
        "type Direction = #north | #south\nValue subclass: Compass\n  heading -> Direction => #north\n",
    ),
    (
        "user_alias.bt",
        "Value subclass: UserAlias\n  run -> Direction => Compass new heading\n",
    ),
    (
        "counter.bt",
        "Actor subclass: Counter\n  state: n :: Integer = 0\n  initialize => self.n := 0\n  bump => self.n := self.n + 1\n",
    ),
    (
        "user_counter.bt",
        "Value subclass: UserCounter\n  run => Counter spawn bump\n",
    ),
];

/// An edit: file name → new content (`None` deletes it).
type Edit = &'static [(&'static str, Option<&'static str>)];

const EDITS: &[(&str, Edit)] = &[
    (
        "add a method",
        &[(
            "base.bt",
            Some(
                "Value subclass: Base\n  one -> Integer => 1\n  extra -> Integer => 2\n  class limit -> Integer => 10\n",
            ),
        )],
    ),
    (
        "remove a method",
        &[(
            "base.bt",
            Some("Value subclass: Base\n  class limit -> Integer => 10\n"),
        )],
    ),
    (
        "rename a method",
        &[(
            "base.bt",
            Some("Value subclass: Base\n  uno -> Integer => 1\n  class limit -> Integer => 10\n"),
        )],
    ),
    (
        "change a return type",
        &[(
            "base.bt",
            Some(
                "Value subclass: Base\n  one -> String => \"1\"\n  class limit -> Integer => 10\n",
            ),
        )],
    ),
    (
        "add a class",
        &[(
            "fresh.bt",
            Some("Value subclass: Fresh\n  value -> Integer => 1\n"),
        )],
    ),
    ("remove a class", &[("leaf.bt", None)]),
    (
        "rename a class",
        &[
            ("leaf.bt", None),
            (
                "leaf2.bt",
                Some("Value subclass: Leaf2\n  value -> Integer => 7\n"),
            ),
        ],
    ),
    (
        "change the superclass",
        &[(
            "sub.bt",
            Some("Value subclass: Sub\n  two -> Integer => 2\n"),
        )],
    ),
    (
        "change a protocol provision",
        &[(
            "tagged.bt",
            Some("Protocol define: Tagged\n  name -> String\n\n  label -> String => self name\n"),
        )],
    ),
    (
        "add an extension",
        &[("ext2.bt", Some("String >> whisper -> Integer => 1\n"))],
    ),
    ("remove the extension", &[("ext.bt", Some("// none\n"))]),
    (
        "change an extension signature",
        &[("ext.bt", Some("String >> shout -> String => \"x\"\n"))],
    ),
    (
        "edit an initialize body",
        &[(
            "counter.bt",
            Some(
                "Actor subclass: Counter\n  state: n :: Integer = 0\n  initialize => self.n := 5\n  bump => self.n := self.n + 1\n",
            ),
        )],
    ),
    (
        "change a class-side constant",
        &[(
            "base.bt",
            Some("Value subclass: Base\n  one -> Integer => 1\n  class limit -> Integer => 11\n"),
        )],
    ),
    (
        "change an alias",
        &[(
            "compass.bt",
            Some(
                "type Direction = #north | #south | #east\nValue subclass: Compass\n  heading -> Direction => #north\n",
            ),
        )],
    ),
    (
        "edit a doc comment",
        &[(
            "leaf.bt",
            Some("Value subclass: Leaf\n  /// A different doc.\n  value -> Integer => 7\n"),
        )],
    ),
    (
        "edit a method body",
        &[(
            "leaf.bt",
            Some("Value subclass: Leaf\n  /// The value.\n  value -> Integer => 8\n"),
        )],
    ),
];

fn apply(pkg: &Pkg, edit: Edit) {
    for (file, content) in edit {
        pkg.write(file, *content);
    }
}

fn revert(pkg: &Pkg, edit: Edit) {
    for (file, _) in edit {
        match FIXTURE.iter().find(|(name, _)| name == file) {
            Some((_, content)) => pkg.write(file, Some(content)),
            None => {
                let _ = fs::remove_file(pkg.root.join("src").join(file));
            }
        }
    }
}

/// The soundness oracle: for every edit, the files the per-file key rebuilds
/// include every file whose diagnostics or generated code differ under a
/// forced full rebuild.
#[test]
fn per_file_key_rebuilds_a_superset_of_the_files_a_full_rebuild_changes() {
    let pkg = Pkg::new(FIXTURE);
    let (changed, _, before) = build(&pkg);
    assert_eq!(
        changed.len(),
        FIXTURE.len(),
        "cold build compiles everything"
    );
    let (changed, _, _) = build(&pkg);
    assert!(changed.is_empty(), "a no-op rebuild is clean: {changed:?}");

    for (label, edit) in EDITS {
        apply(&pkg, edit);
        let rebuilt = plan(&pkg);
        let after = full_outputs(&pkg);
        let differing: BTreeSet<String> = after
            .iter()
            .filter(|(name, text)| before.get(*name) != Some(*text))
            .map(|(name, _)| name.clone())
            .collect();
        let missed: Vec<_> = differing.difference(&rebuilt).collect();
        assert!(
            missed.is_empty(),
            "{label}: per-file key skipped {missed:?}, which a full rebuild changes \
             (rebuilt {rebuilt:?})"
        );
        // Settle on the edited state, then return to the fixture.
        build(&pkg);
        revert(&pkg, edit);
        build(&pkg);
    }
}

fn rebuilt_by(pkg: &Pkg, edit: Edit) -> BTreeSet<String> {
    apply(pkg, edit);
    plan(pkg)
}

/// Adding a method to a class rebuilds the class's file and the files that
/// consulted it, and nothing that never looked at it.
#[test]
fn adding_a_method_rebuilds_only_the_files_that_consulted_the_class() {
    let pkg = Pkg::new(FIXTURE);
    build(&pkg);
    let rebuilt = rebuilt_by(&pkg, EDITS[0].1);
    assert!(rebuilt.contains("base.bt"), "{rebuilt:?}");
    assert!(rebuilt.contains("user_base.bt"), "{rebuilt:?}");
    assert!(
        rebuilt.contains("sub.bt"),
        "a subclass inherits it: {rebuilt:?}"
    );
    for untouched in [
        "unrelated.bt",
        "leaf.bt",
        "user_leaf.bt",
        "user_widget.bt",
        "user_ext.bt",
    ] {
        assert!(
            !rebuilt.contains(untouched),
            "{untouched} never consulted Base: {rebuilt:?}"
        );
    }
}

/// Adding or removing a class rebuilds the files that consulted that name,
/// including by absence, and leaves the rest cached.
#[test]
fn adding_or_removing_a_class_rebuilds_the_files_that_consulted_the_name() {
    let pkg = Pkg::new(FIXTURE);
    build(&pkg);
    let rebuilt = rebuilt_by(
        &pkg,
        &[(
            "fresh.bt",
            Some("Value subclass: Fresh\n  value -> Integer => 1\n"),
        )],
    );
    assert_eq!(
        rebuilt,
        BTreeSet::from(["fresh.bt".to_string()]),
        "a new class nobody consulted rebuilds only itself"
    );
    build(&pkg);

    // A file that looked up a name that did not exist depends on its absence.
    pkg.write(
        "ghost.bt",
        Some("Value subclass: Ghost\n  run => Phantom new\n"),
    );
    build(&pkg);
    let rebuilt = rebuilt_by(
        &pkg,
        &[(
            "phantom.bt",
            Some("Value subclass: Phantom\n  value -> Integer => 1\n"),
        )],
    );
    assert!(rebuilt.contains("ghost.bt"), "{rebuilt:?}");
    assert!(!rebuilt.contains("unrelated.bt"), "{rebuilt:?}");
    build(&pkg);

    let rebuilt = rebuilt_by(&pkg, &[("leaf.bt", None)]);
    assert!(rebuilt.contains("user_leaf.bt"), "{rebuilt:?}");
    assert!(!rebuilt.contains("unrelated.bt"), "{rebuilt:?}");
}

/// A doc-only or body-only edit rebuilds just the edited file.
#[test]
fn doc_and_body_edits_rebuild_only_their_own_file() {
    let pkg = Pkg::new(FIXTURE);
    build(&pkg);
    for label in ["edit a doc comment", "edit a method body"] {
        let edit = EDITS.iter().find(|(l, _)| *l == label).unwrap().1;
        let rebuilt = rebuilt_by(&pkg, edit);
        assert_eq!(rebuilt, BTreeSet::from(["leaf.bt".to_string()]), "{label}");
        build(&pkg);
        revert(&pkg, edit);
        build(&pkg);
    }
}

/// Without a stored consulted surface (an old or unreadable sidecar entry) a
/// file keys on the full package-wide hash, as before.
#[test]
fn a_file_without_a_stored_surface_keys_on_the_full_hash() {
    let pkg = Pkg::new(FIXTURE);
    build(&pkg);
    let env = pkg.env();
    let mut hashes = super::super::super::build_cache::load_beam_hash_cache(&env.build_dir);
    let before = hashes.len();
    hashes.retain(|key, _| !key.ends_with("\u{0}consulted"));
    assert!(
        hashes.len() < before,
        "the sidecar stores consulted surfaces"
    );
    super::super::super::build_cache::save_beam_hash_cache(&env.build_dir, &hashes);
    // Nothing stored: the first build re-keys every file, then it is clean.
    let rebuilt = plan(&pkg);
    assert!(
        rebuilt.len() == FIXTURE.len() || rebuilt.is_empty(),
        "{rebuilt:?}"
    );

    build(&pkg);
    let hashes = super::super::super::build_cache::load_beam_hash_cache(&env.build_dir);
    let key = super::super::surface_key::consulted_key(&env.source_files[0]);
    assert!(
        hashes.contains_key(&key),
        "re-keyed files store their surface"
    );
    assert!(plan(&pkg).is_empty());
}
