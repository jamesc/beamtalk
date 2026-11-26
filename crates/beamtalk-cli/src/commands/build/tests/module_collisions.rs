// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5074: two sources deriving one module are a build error naming both,
//! the module index and class infos agree on precedence, and the `.app`
//! alias `source_file` does not depend on how the path was spelled.

use super::*;
use crate::commands::deps::path::{OtpTypeSpecs, ResolvedDependency, compile_dependency_at};

fn project_with_files(temp: &TempDir, name: &str, files: &[(&str, &str)]) -> Utf8PathBuf {
    let project = create_test_project(temp);
    write_test_file(
        &project.join("beamtalk.toml"),
        &format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
    );
    for (file, body) in files {
        write_test_file(&project.join("src").join(file), body);
    }
    project
}

#[test]
fn test_package_build_rejects_two_files_with_one_module() {
    let temp = TempDir::new().unwrap();
    let project = project_with_files(
        &temp,
        "collide",
        &[
            ("FooBar.bt", "Object subclass: FooBar\n"),
            ("foo_bar.bt", "Object subclass: FooBarTwo\n"),
        ],
    );
    let err = build(project.as_str(), &default_options(), false).unwrap_err();
    let msg = format!("{err:?}");
    assert!(
        msg.contains("FooBar.bt")
            && msg.contains("foo_bar.bt")
            && msg.contains("bt@collide@foo_bar"),
        "error must name both files and the module: {msg}"
    );
}

#[test]
fn test_dependency_build_rejects_two_files_with_one_module() {
    let temp = TempDir::new().unwrap();
    let project_root = Utf8PathBuf::from_path_buf(temp.path().join("app")).unwrap();
    let dep_root = Utf8PathBuf::from_path_buf(temp.path().join("dep")).unwrap();
    fs::create_dir_all(dep_root.join("src")).unwrap();
    fs::create_dir_all(&project_root).unwrap();
    write_test_file(
        &dep_root.join("beamtalk.toml"),
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    );
    write_test_file(&dep_root.join("src/FooBar.bt"), "Object subclass: FooBar\n");
    write_test_file(
        &dep_root.join("src/foo_bar.bt"),
        "Object subclass: FooBarTwo\n",
    );

    let err = compile_dependency_at(
        &project_root,
        &dep_root,
        "dep",
        &default_options(),
        &[],
        &OtpTypeSpecs::default(),
    )
    .unwrap_err();
    let msg = format!("{err:?}");
    assert!(
        msg.contains("FooBar.bt") && msg.contains("foo_bar.bt") && msg.contains("bt@dep@foo_bar"),
        "error must name both files and the module: {msg}"
    );
}

#[test]
fn test_project_class_wins_module_index_over_dependency_class() {
    let temp = TempDir::new().unwrap();
    let project = project_with_files(&temp, "mine", &[("thing.bt", "Object subclass: Thing\n")]);
    let env = setup_build_environment(project.as_str()).unwrap();
    let dep = ResolvedDependency {
        name: "other".to_string(),
        root: Utf8PathBuf::from_path_buf(temp.path().join("other")).unwrap(),
        ebin_path: Utf8PathBuf::from_path_buf(temp.path().join("other/ebin")).unwrap(),
        class_module_index: HashMap::from([("Thing".to_string(), "bt@other@thing".to_string())]),
        class_infos: Vec::new(),
        protocol_infos: Vec::new(),
        protocol_defs: Vec::new(),
        protocol_sources: HashMap::default(),
        alias_infos: Vec::new(),
        is_direct: true,
        via_chain: Vec::new(),
        stubs_dir: None,
    };
    let dep_ctx = DependencyContext {
        resolved_deps: vec![dep],
        has_native_deps: false,
    };
    let index = build_class_index(&env, &dep_ctx, &default_options(), true).unwrap();
    assert_eq!(
        index.class_module_index.get("Thing").map(String::as_str),
        Some("bt@mine@thing"),
        "the project's class must win, as it does in collect_all_class_infos"
    );
}

#[test]
fn test_alias_source_file_is_package_relative_however_the_root_is_spelled() {
    let temp = TempDir::new().unwrap();
    let project = project_with_files(
        &temp,
        "aliases",
        &[("modes.bt", "type Mode = #fast | #slow\n")],
    );
    let file = project.join("src/modes.bt");
    let rel = build_alias_metadata(std::slice::from_ref(&file), Some(&project));
    assert_eq!(rel.len(), 1);
    assert_eq!(rel[0].source_file, "src/modes.bt");

    // The same project reached through a different (non-normalised) spelling
    // of its root must give the same `.app` content.
    let dotted = Utf8PathBuf::from(format!("{project}/./"));
    let via_dotted = build_alias_metadata(&[dotted.join("src/modes.bt")], Some(&dotted));
    assert_eq!(via_dotted[0].source_file, rel[0].source_file);
}
