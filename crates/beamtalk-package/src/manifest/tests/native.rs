// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: native.

use super::*;

// --- Native dependency parsing tests ---

#[test]
fn test_parse_native_deps_pessimistic_major_minor() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
gun = "~> 2.1"
cowboy = "~> 2.12"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.native_dependencies.len(), 2);
    let gun = &manifest.native_dependencies["gun"];
    assert_eq!(gun.name, "gun");
    assert_eq!(gun.hex_constraint(), Some("~> 2.1"));
    let cowboy = &manifest.native_dependencies["cowboy"];
    assert_eq!(cowboy.name, "cowboy");
    assert_eq!(cowboy.hex_constraint(), Some("~> 2.12"));
}

#[test]
fn test_parse_native_deps_pessimistic_patch() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
jason = "~> 1.4.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(
        manifest.native_dependencies["jason"].hex_constraint(),
        Some("~> 1.4.0")
    );
}

#[test]
fn test_parse_native_deps_exact_version() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
cowboy = "2.12.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(
        manifest.native_dependencies["cowboy"].hex_constraint(),
        Some("2.12.0")
    );
}

#[test]
fn test_parse_native_deps_compound_constraint() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
ranch = ">= 1.0.0 and < 2.0.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(
        manifest.native_dependencies["ranch"].hex_constraint(),
        Some(">= 1.0.0 and < 2.0.0")
    );
}

#[test]
fn test_parse_native_deps_comparison_operators() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
dep_ge = ">= 1.0"
dep_gt = "> 1.0"
dep_le = "<= 3.0"
dep_lt = "< 2.0.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.native_dependencies.len(), 4);
    assert_eq!(
        manifest.native_dependencies["dep_ge"].hex_constraint(),
        Some(">= 1.0")
    );
    assert_eq!(
        manifest.native_dependencies["dep_gt"].hex_constraint(),
        Some("> 1.0")
    );
    assert_eq!(
        manifest.native_dependencies["dep_le"].hex_constraint(),
        Some("<= 3.0")
    );
    assert_eq!(
        manifest.native_dependencies["dep_lt"].hex_constraint(),
        Some("< 2.0.0")
    );
}

#[test]
fn test_parse_no_native_deps_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.native_dependencies.is_empty());
}

#[test]
fn test_parse_empty_native_deps_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[native.dependencies]
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.native_dependencies.is_empty());
}

#[test]
fn test_native_deps_do_not_affect_regular_deps() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
utils = { path = "../utils" }

[native.dependencies]
gun = "~> 2.1"
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 1);
    assert!(manifest.dependencies.contains_key("utils"));
    assert_eq!(manifest.native_dependencies.len(), 1);
    assert!(manifest.native_dependencies.contains_key("gun"));
}

// --- BT-4555: native git sources and [native.overrides] ---

fn parse_native_toml(native: &str) -> Result<ParsedManifest> {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        &format!("[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n\n{native}"),
    );
    parse_manifest_full(&path.join("beamtalk.toml"))
}

fn native_err(native: &str) -> String {
    format!("{:?}", parse_native_toml(native).unwrap_err())
}

/// Native-dependency manifests that must be rejected: one row per case,
/// `(case, native TOML, expected error substring)`. The case name is in the
/// failure message.
const NATIVE_REJECTIONS: &[(&str, &str, &str)] = &[
    (
        "non_string_value",
        "[native.dependencies]\ngun = 42\n",
        "version constraint string or a git",
    ),
    (
        "empty_constraint",
        "[native.dependencies]\ngun = \"\"\n",
        "must not be empty",
    ),
    (
        "invalid_version",
        "[native.dependencies]\ngun = \"abc\"\n",
        "non-numeric",
    ),
    (
        "too_many_segments",
        "[native.dependencies]\ngun = \"1.2.3.4\"\n",
        "too many segments",
    ),
    (
        "tilde_without_minor",
        "[native.dependencies]\ngun = \"~> 2\"\n",
        "major.minor",
    ),
    (
        "invalid_name_uppercase",
        "[native.dependencies]\nGun = \"~> 2.1\"\n",
        "lowercase",
    ),
    (
        "invalid_name_dash",
        "[native.dependencies]\nmy-dep = \"~> 1.0\"\n",
        "invalid character",
    ),
    (
        "tilde_in_compound",
        "[native.dependencies]\ngun = \"~> 2.1 and < 3.0\"\n",
        "pessimistic",
    ),
    (
        "git_dep_without_ref",
        "[native.dependencies]\noidcc = { git = \"https://github.com/jamesc/oidcc\" }\n",
        "Native dependency 'oidcc' is a git dependency but has no ref",
    ),
    (
        "git_dep_multiple_refs",
        "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", branch = \"main\", tag = \"v1\" }\n",
        "specifies multiple git references",
    ),
    (
        "git_dep_ref_key_points_at_rev",
        "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", ref = \"abc\" }\n",
        "pin a commit with 'rev' instead",
    ),
    (
        "table_without_git",
        "[native.dependencies]\noidcc = { version = \"3.0.0\" }\n",
        "has no 'git' URL",
    ),
    (
        "git_dep_unknown_key",
        "[native.dependencies]\noidcc = { git = \"https://x/oidcc\", branch = \"main\", path = \"../x\" }\n",
        "invalid git source",
    ),
    (
        "git_dep_empty_url",
        "[native.dependencies]\noidcc = { git = \" \", branch = \"main\" }\n",
        "empty 'git' URL",
    ),
    (
        "override_non_atom_erl_opt",
        "[native.overrides]\njose = { erl_opts = [\"{d, 'X'}\"] }\n",
        "invalid erl_opts entry",
    ),
    (
        "override_unknown_key",
        "[native.overrides]\njose = { deps = [] }\n",
        "Native override 'jose' has an invalid format",
    ),
    (
        "override_without_erl_opts",
        "[native.overrides]\njose = {}\n",
        "Native override 'jose' has an invalid format",
    ),
    (
        "override_invalid_name",
        "[native.overrides]\nJose = { erl_opts = [] }\n",
        "must start with a lowercase letter",
    ),
    (
        "git_ext_transport",
        "[native.dependencies]\nevil = { git = \"ext::sh -c id\", tag = \"1\" }\n",
        "unsupported git URL",
    ),
    (
        "git_option_lookalike",
        "[native.dependencies]\nevil = { git = \"--upload-pack=x\", tag = \"1\" }\n",
        "may not begin with",
    ),
    (
        "git_unknown_scheme",
        "[native.dependencies]\nevil = { git = \"badscheme://h/r\", tag = \"1\" }\n",
        "unsupported git URL",
    ),
];

#[test]
fn test_reject_native_manifests() {
    for &(case, native, expected) in NATIVE_REJECTIONS {
        let err = native_err(native);
        assert!(
            err.contains(expected),
            "case `{case}`: expected the error to contain {expected:?}, got: {err}"
        );
    }
}

#[test]
fn test_parse_native_git_dep_branch_tag_rev() {
    let manifest = parse_native_toml(
        r#"
[native.dependencies]
oidcc = { git = "https://github.com/jamesc/oidcc", branch = "rfc7009-revocation" }
jose = { git = "https://github.com/potatosalad/erlang-jose", tag = "1.11.10" }
yamerl = { git = "https://github.com/yakaz/yamerl", rev = "abc1234" }
gun = "~> 2.1"
"#,
    )
    .unwrap();

    let deps = &manifest.native_dependencies;
    assert_eq!(
        deps["oidcc"].source,
        NativeSource::Git {
            url: "https://github.com/jamesc/oidcc".to_string(),
            reference: GitReference::Branch("rfc7009-revocation".to_string()),
        }
    );
    assert_eq!(
        deps["jose"].source,
        NativeSource::Git {
            url: "https://github.com/potatosalad/erlang-jose".to_string(),
            reference: GitReference::Tag("1.11.10".to_string()),
        }
    );
    assert_eq!(
        deps["yamerl"].source,
        NativeSource::Git {
            url: "https://github.com/yakaz/yamerl".to_string(),
            reference: GitReference::Rev("abc1234".to_string()),
        }
    );
    assert_eq!(deps["oidcc"].hex_constraint(), None);
    assert_eq!(deps["gun"].hex_constraint(), Some("~> 2.1"));
}

#[test]
fn test_parse_native_overrides() {
    let manifest = parse_native_toml(
        r#"
[native.dependencies]
oidcc = "~> 3.0"

[native.overrides]
jose = { erl_opts = [] }
cowlib = { erl_opts = ["debug_info", "nowarn_export_all"] }
"#,
    )
    .unwrap();

    assert_eq!(manifest.native_overrides.len(), 2);
    assert_eq!(
        manifest.native_overrides["jose"].erl_opts,
        Vec::<String>::new()
    );
    assert_eq!(
        manifest.native_overrides["cowlib"].erl_opts,
        vec!["debug_info".to_string(), "nowarn_export_all".to_string()]
    );
}

#[test]
fn test_no_native_overrides_section() {
    let manifest = parse_native_toml("[native.dependencies]\ngun = \"~> 2.1\"\n").unwrap();
    assert!(manifest.native_overrides.is_empty());
}

#[test]
fn test_parse_native_git_dep_trims_url() {
    let manifest = parse_native_toml(
        "[native.dependencies]\noidcc = { git = \" https://x/oidcc \", branch = \"main\" }\n",
    )
    .unwrap();
    assert_eq!(
        manifest.native_dependencies["oidcc"].source,
        NativeSource::Git {
            url: "https://x/oidcc".to_string(),
            reference: GitReference::Branch("main".to_string()),
        }
    );
}

#[test]
fn test_packages_without_native_deps_unaffected() {
    // A manifest with only regular dependencies should still parse correctly
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
description = "Regular app without native deps"

[dependencies]
json = { git = "https://github.com/jamesc/beamtalk-json", tag = "v1.0.0" }
utils = { path = "../utils" }
"#,
    );

    let manifest = parse_manifest_full(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.dependencies.len(), 2);
    assert!(manifest.native_dependencies.is_empty());
}
