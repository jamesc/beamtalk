// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5074: cache-key coverage - native `-spec`s, build-mode flags and a failed
//! erlc batch must not let a later build treat stale output as current.

use super::*;
use beamtalk_core::build_layout::BuildLayout;

#[test]
#[ignore = "requires erlc"]
fn test_changed_native_spec_rechecks_unchanged_caller() {
    let temp = TempDir::new().unwrap();
    let project = create_test_project(&temp);
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"spec_key\"\nversion = \"0.1.0\"\n",
    );
    write_test_file(&project.join("src/main.bt"), "main := [Erlang foo bar].");
    let native = project.join("native");
    fs::create_dir(&native).unwrap();
    let erl = |ret: &str, body: &str| {
        write_test_file(
            &native.join("foo.erl"),
            &format!("-module(foo).\n-export([bar/0]).\n-spec bar() -> {ret}.\nbar() -> {body}.\n"),
        );
    };

    erl("integer()", "1");
    build(project.as_str(), &default_options(), false).unwrap();
    let layout = BuildLayout::new(&project);
    let core = layout.ebin_dir().join("bt@spec_key@main.core");
    assert!(core.exists(), "first build writes {core}");

    // A no-change rebuild must not recompile main.bt.
    fs::remove_file(&core).unwrap();
    build(project.as_str(), &default_options(), false).unwrap();
    assert!(
        !core.exists(),
        "an unchanged project must not recompile main.bt"
    );

    // Only the native -spec changes; main.bt is byte-identical.
    erl("binary()", "<<\"x\">>");
    build(project.as_str(), &default_options(), false).unwrap();
    assert!(
        core.exists(),
        "a changed native -spec must re-check the .bt caller"
    );
}

#[test]
fn test_registry_fingerprint_tracks_spec_changes_but_not_line_numbers() {
    use beamtalk_core::semantic_analysis::type_checker::{
        FunctionSignature, InferredType, NativeTypeRegistry, TypeProvenance,
    };
    let sig = |ret: &str, line: Option<u32>| FunctionSignature {
        name: "bar".into(),
        arity: 0,
        params: Vec::new(),
        return_type: InferredType::known(ret),
        provenance: TypeProvenance::Extracted,
        line,
    };
    let reg = |ret: &str, line: Option<u32>| {
        let mut r = NativeTypeRegistry::new();
        r.register_module("foo", vec![sig(ret, line)]);
        r.fingerprint()
    };
    assert_ne!(reg("Integer", Some(3)), reg("String", Some(3)));
    assert_eq!(reg("Integer", Some(3)), reg("Integer", Some(40)));
}

#[test]
fn test_build_flag_change_forces_provenance_rebuild() {
    let temp = TempDir::new().unwrap();
    let project = create_test_project(&temp);
    write_test_file(
        &project.join("beamtalk.toml"),
        "[package]\nname = \"flags\"\nversion = \"0.1.0\"\n",
    );
    write_test_file(&project.join("src/main.bt"), "main := [1].");
    let env = setup_build_environment(project.as_str()).unwrap();
    let plain = default_options();
    let prims = beamtalk_core::CompilerOptions {
        allow_primitives: true,
        ..default_options()
    };
    let stdlib = beamtalk_core::CompilerOptions {
        stdlib_mode: true,
        ..default_options()
    };

    fs::create_dir_all(env.layout.stamp_path().parent().unwrap()).unwrap();
    crate::commands::build_stamp::write_stamp_with_flags(
        &env.layout.stamp_path(),
        crate::commands::build_stamp::current_otp_version(),
        &env_build_flags(&plain),
    );
    assert!(!provenance_requires_rebuild(&env, &plain));
    assert!(
        provenance_requires_rebuild(&env, &prims),
        "--allow-primitives"
    );
    assert!(provenance_requires_rebuild(&env, &stdlib), "--stdlib-mode");

    crate::commands::build_stamp::write_stamp_with_flags(
        &env.layout.stamp_path(),
        crate::commands::build_stamp::current_otp_version(),
        &env_build_flags(&prims),
    );
    assert!(!provenance_requires_rebuild(&env, &prims));
    assert!(
        provenance_requires_rebuild(&env, &plain),
        "flag dropped again"
    );
}

#[test]
fn test_failed_batch_forgets_hashes_so_a_reverted_edit_is_recompiled() {
    let temp = TempDir::new().unwrap();
    let build_dir = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let mut hashes = std::collections::HashMap::new();
    hashes.insert("src/a.bt".to_string(), "hash-v1".to_string());
    hashes.insert("src/b.bt".to_string(), "hash-b".to_string());
    crate::commands::build_cache::save_beam_hash_cache(&build_dir, &hashes);

    crate::commands::build_cache::invalidate_beam_hashes(&build_dir, ["src/a.bt"]);

    let after = crate::commands::build_cache::load_beam_hash_cache(&build_dir);
    assert!(!after.contains_key("src/a.bt"), "batch member forgotten");
    assert_eq!(after.get("src/b.bt").map(String::as_str), Some("hash-b"));
}
