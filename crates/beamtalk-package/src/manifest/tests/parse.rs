// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `manifest` tests: parse.

use super::*;

/// BT-4382: one checker-scope rule for every surface, including the
/// malformed-manifest case the four former copies disagreed on.
#[test]
fn package_checker_scope_covers_every_manifest_state() {
    use beamtalk_core::semantic_analysis::KnowledgeScope;

    assert_eq!(
        package_checker_scope(None),
        PackageCheckerScope::module_only()
    );

    let dir = TempDir::new().unwrap();
    let root = write_manifest(&dir, "[package]\nname = \"my_pkg\"\nversion = \"0.1.0\"\n");
    let scope = package_checker_scope(Some(&root));
    assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
    assert_eq!(scope.package_name.as_deref(), Some("my_pkg"));

    let dir = TempDir::new().unwrap();
    let root = write_manifest(&dir, "[package\nnot toml");
    let scope = package_checker_scope(Some(&root));
    assert_eq!(scope.knowledge_scope, KnowledgeScope::ProjectComplete);
    assert_eq!(scope.package_name, None);

    let dir = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
    assert_eq!(
        package_checker_scope(Some(&root)),
        PackageCheckerScope::module_only(),
        "a root with no manifest is not a package"
    );
}

#[test]
fn test_parse_valid_manifest() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[dependencies]
"#,
    );

    let manifest = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.name, "my_app");
    assert_eq!(manifest.version, "0.1.0");
    assert!(manifest.description.is_none());
    assert!(manifest.licenses.is_none());
}

#[test]
fn test_parse_manifest_with_optional_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "1.2.3"
description = "A cool app"
licenses = ["Apache-2.0", "MIT"]

[dependencies]
"#,
    );

    let manifest = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(manifest.name, "my_app");
    assert_eq!(manifest.version, "1.2.3");
    assert_eq!(manifest.description.as_deref(), Some("A cool app"));
    assert_eq!(
        manifest.licenses.as_deref(),
        Some(["Apache-2.0".to_string(), "MIT".to_string()].as_slice())
    );
}

#[test]
fn test_parse_manifest_missing_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("beamtalk.toml"),
        "error should mention file: {err}"
    );
}

#[test]
fn test_parse_manifest_missing_version() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_missing_package_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r"
[dependencies]
",
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_invalid_toml() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "this is not valid toml {{{{");

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
}

#[test]
fn test_parse_manifest_nonexistent_file() {
    let path = Utf8PathBuf::from("/nonexistent/beamtalk.toml");
    let result = parse_manifest(&path);
    assert!(result.is_err());
}

#[test]
fn test_find_manifest_exists() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "found_app"
version = "0.1.0"
"#,
    );

    let result = find_manifest(&path).unwrap();
    assert!(result.is_some());
    assert_eq!(result.unwrap().name, "found_app");
}

#[test]
fn test_find_manifest_not_found() {
    let temp = TempDir::new().unwrap();
    let path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let result = find_manifest(&path).unwrap();
    assert!(result.is_none());
}

#[test]
fn test_package_name_for_project() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "named_app"
version = "0.1.0"
"#,
    );
    assert_eq!(
        package_name_for_project(path.as_str()),
        Some("named_app".to_string())
    );
}

#[test]
fn test_package_name_for_project_missing_or_malformed_is_none() {
    let temp = TempDir::new().unwrap();
    let missing = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    assert_eq!(package_name_for_project(missing.as_str()), None);

    let bad = TempDir::new().unwrap();
    let bad_path = write_manifest(&bad, "not valid toml {{{{");
    assert_eq!(package_name_for_project(bad_path.as_str()), None);
}

#[test]
fn test_find_manifest_malformed() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, "not valid toml {{{{");

    let result = find_manifest(&path);
    assert!(result.is_err());
}

/// BT-5076: an unknown key used to be silently dropped, so a typo changed
/// nothing and told nobody. It is now an error naming the key.
#[test]
fn test_parse_manifest_rejects_unknown_fields() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
unknown_field = "typo"

[dependencies]
some_dep = "1.0"
"#,
    );

    let err = format!(
        "{:?}",
        parse_manifest(&path.join("beamtalk.toml")).unwrap_err()
    );
    assert!(err.contains("unknown field `unknown_field`"), "{err}");
}

#[test]
fn test_parse_manifest_rejects_invalid_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "MyApp"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("my_app"), "should suggest fix: {err}");
}

#[test]
fn test_parse_manifest_rejects_reserved_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "stdlib"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(err.contains("reserved"), "should mention reserved: {err}");
}

#[test]
fn test_parse_manifest_rejects_erlang_app_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "crypto"
version = "0.1.0"
"#,
    );

    let result = parse_manifest(&path.join("beamtalk.toml"));
    assert!(result.is_err());
    let err = format!("{:?}", result.unwrap_err());
    assert!(
        err.contains("Erlang"),
        "should mention Erlang conflict: {err}"
    );
}

#[test]
fn test_find_application_config_present() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
supervisor = "AppSup"
"#,
    );

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_some());
    assert_eq!(app_config.unwrap().supervisor, "AppSup");
}

#[test]
fn test_find_application_config_absent() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"
"#,
    );

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_none());
}

#[test]
fn test_find_application_config_no_manifest() {
    let temp = TempDir::new().unwrap();
    let path = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

    let app_config = find_application_config(&path).unwrap();
    assert!(app_config.is_none());
}

#[test]
fn test_find_application_config_rejects_invalid_package_name() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "BadName"
version = "0.1.0"

[application]
supervisor = "AppSup"
"#,
    );

    let result = find_application_config(&path);
    assert!(
        result.is_err(),
        "should reject manifest with invalid package name"
    );
}

#[test]
fn test_parse_manifest_with_application_section() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        r#"
[package]
name = "my_app"
version = "0.1.0"

[application]
supervisor = "RootSup"
"#,
    );

    let manifest = parse_manifest_raw(&path.join("beamtalk.toml")).unwrap();
    assert!(manifest.application.is_some());
    assert_eq!(manifest.application.unwrap().supervisor, "RootSup");
}

// ---- BT-5076: version validation, unknown keys, reserved names ----

fn parse_err(content: &str) -> String {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, content);
    format!(
        "{:?}",
        parse_manifest_full(&path.join("beamtalk.toml")).unwrap_err()
    )
}

#[test]
fn package_version_accepts_semver_forms() {
    for v in [
        "0.1.0",
        "1.2.3",
        "10.20.30",
        "1.0.0-rc.1",
        "1.0.0+build.5",
        "1.0.0-a-b+c-d",
        "1.0.0-0",
        "1.0.0-alpha.0",
        "1.0.0+001",
    ] {
        assert!(validate_package_version(v).is_ok(), "should accept {v}");
    }
}

#[test]
fn package_version_rejects_malformed() {
    for v in [
        "1.0.0-01",
        "1.0.0-alpha.01",
        "",
        "1",
        "1.0",
        "1.0.0.0",
        "v1.0.0",
        "01.0.0",
        "1.0.x",
        "1.0.0-",
        "1.0.0+",
        "1.0.0-a..b",
        "1/../2.0.0",
        "1.0.0 ",
        "1.0.0\n",
        "../1.0.0",
        "1.0.0-rc 1",
    ] {
        let err = validate_package_version(v).unwrap_err();
        assert!(err.contains("MAJOR.MINOR.PATCH"), "{v}: {err}");
    }
}

#[test]
fn manifest_with_invalid_version_is_an_error_naming_the_form() {
    let content = "[package]\nname = \"p\"\nversion = \"1.0\"\n";
    let err = parse_err(content);
    assert!(err.contains("Invalid [package] version"), "{err}");
    assert!(err.contains("MAJOR.MINOR.PATCH"), "{err}");

    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, content);
    assert!(parse_manifest(&path.join("beamtalk.toml")).is_err());
    assert!(find_application_config(&path).is_err());
}

#[test]
fn misspelled_top_level_section_is_an_error() {
    let err = parse_err(
        "[package]\nname = \"p\"\nversion = \"0.1.0\"\n\n[dependancies]\nfoo = { path = \"../foo\" }\n",
    );
    assert!(err.contains("unknown field `dependancies`"), "{err}");
}

#[test]
fn misspelled_package_key_is_an_error() {
    // `strict_deps` for the real `strict-deps` must not silently leave strict mode off.
    let err = parse_err("[package]\nname = \"p\"\nversion = \"0.1.0\"\nstrict_deps = true\n");
    assert!(err.contains("unknown field `strict_deps`"), "{err}");
    assert!(
        err.contains("strict-deps"),
        "should list the real key: {err}"
    );
}

#[test]
fn misspelled_application_key_is_an_error() {
    let err = parse_err(
        "[package]\nname = \"p\"\nversion = \"0.1.0\"\n\n[application]\nsuper = \"AppSup\"\n",
    );
    assert!(err.contains("unknown field `super`"), "{err}");
}

#[test]
fn runtime_apps_and_full_otp_set_are_reserved_package_names() {
    for name in [
        "beamtalk_runtime",
        "beamtalk_workspace",
        "beamtalk_stdlib",
        "beamtalk_compiler",
        "erts",
        "asn1",
        "debugger",
        "diameter",
        "edoc",
        "eldap",
        "ftp",
        "megaco",
        "os_mon",
        "parsetools",
        "reltool",
        "runtime_tools",
        "snmp",
        "syntax_tools",
        "tftp",
        "wx",
    ] {
        assert!(
            validate_package_name(name).is_err(),
            "'{name}' must be rejected as a package name"
        );
    }
}

#[test]
fn native_dependency_may_not_take_an_otp_or_runtime_name() {
    for name in ["ssl", "erts", "kernel", "beamtalk_runtime", "wx"] {
        let err = parse_err(&format!(
            "[package]\nname = \"p\"\nversion = \"0.1.0\"\n\n[native.dependencies]\n{name} = \"~> 1.0\"\n"
        ));
        assert!(err.contains("reserved"), "{name}: {err}");
    }
}

#[test]
fn native_dependency_may_not_share_the_package_name() {
    let err = parse_err(
        "[package]\nname = \"jose\"\nversion = \"0.1.0\"\n\n[native.dependencies]\njose = \"~> 1.0\"\n",
    );
    assert!(err.contains("same name as this package"), "{err}");
}

// ---- BT-5076: the shapes real external packages use must still load ----
//
// One manifest per shape the sibling beamtalk-* packages publish. If one of
// these fails, strict validation broke a real package.

/// The package names the real external packages use. None may be reserved.
const REAL_PACKAGE_NAMES: &[&str] = &[
    "buccaneer",
    "catalina",
    "hpack",
    "http",
    "agents",
    "announcements",
    "oauth",
    "mint",
    "yaml",
    "linear",
];

#[test]
fn real_package_names_are_not_reserved() {
    for name in REAL_PACKAGE_NAMES {
        assert!(
            validate_package_name(name).is_ok(),
            "real package '{name}' must not be a reserved name"
        );
    }
}

fn parses(content: &str) -> ParsedManifest {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(&temp, content);
    parse_manifest_full(&path.join("beamtalk.toml")).unwrap()
}

#[test]
fn shape_package_dependencies_and_diagnostics_loads() {
    let m = parses(
        "[package]\nname = \"buccaneer\"\nversion = \"0.1.7\"\ndescription = \"d\"\n\n\
         [dependencies]\nhttp = \"0.3.6\"\n\n[diagnostics]\nunused = \"warn\"\ndnu = \"hint\"\n",
    );
    assert_eq!(m.package.name, "buccaneer");
    assert_eq!(m.dependencies.len(), 1);
    assert!(!m.diagnostics.is_empty());
}

#[test]
fn shape_package_dependencies_and_native_dependencies_loads() {
    let m = parses(
        "[package]\nname = \"yaml\"\nversion = \"0.2.0\"\ndescription = \"d\"\n\n\
         [dependencies]\n\n[native.dependencies]\nyamerl = \"~> 0.10\"\n\
         jose = { git = \"https://github.com/potatosalad/erlang-jose\", tag = \"1.11.10\" }\n",
    );
    assert_eq!(m.native_dependencies.len(), 2);
}

#[test]
fn shape_package_with_licenses_loads() {
    let m = parses(
        "[package]\nname = \"agents\"\nversion = \"0.1.1\"\ndescription = \"d\"\n\
         licenses = [\"Apache-2.0\"]\n\n[dependencies]\n",
    );
    assert_eq!(
        m.package.licenses.as_deref(),
        Some(&["Apache-2.0".to_string()][..])
    );
}

#[test]
fn shape_package_without_description_loads() {
    let m = parses("[package]\nname = \"oauth\"\nversion = \"0.2.1\"\n\n[dependencies]\n");
    assert_eq!(m.package.description, None);
}

#[test]
fn every_real_package_name_and_version_loads() {
    for (name, version) in [
        ("buccaneer", "0.1.7"),
        ("catalina", "0.2.7"),
        ("hpack", "0.1.1"),
        ("http", "0.3.6"),
        ("agents", "0.1.1"),
        ("announcements", "0.1.1"),
        ("oauth", "0.2.1"),
        ("mint", "0.2.1"),
        ("yaml", "0.2.0"),
        ("linear", "0.1.3"),
    ] {
        let m = parses(&format!(
            "[package]\nname = \"{name}\"\nversion = \"{version}\"\n"
        ));
        assert_eq!(m.package.name, name);
    }
}

/// The exact `oauth` and `yaml` manifests: a string-version `[dependencies]`
/// entry and string-version `[native.dependencies]` entries.
#[test]
fn shape_oauth_and_yaml_exact_manifests_load() {
    let oauth = parses(
        "[package]\nname = \"oauth\"\nversion = \"0.2.1\"\n\n\
         [dependencies]\nhttp = \"0.3.6\"\n\n[native.dependencies]\noidcc = \"3.9.2\"\n",
    );
    assert_eq!(oauth.dependencies.len(), 1);
    assert!(oauth.native_dependencies.contains_key("oidcc"));

    let yaml = parses(
        "[package]\nname = \"yaml\"\nversion = \"0.2.0\"\n\n\
         [dependencies]\n\n[native.dependencies]\nyamerl = \"0.10.0\"\n",
    );
    assert!(yaml.dependencies.is_empty());
    assert!(yaml.native_dependencies.contains_key("yamerl"));
}

#[test]
fn dependency_manifest_tolerates_unknown_keys_but_package_manifest_does_not() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    std::fs::write(
        &path,
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\nfuture_key = \"x\"\n\n[future_section]\na = 1\n",
    )
    .unwrap();
    assert!(parse_manifest_full(&path).is_err());
    let parsed = parse_dependency_manifest_full(&path).unwrap();
    assert_eq!(parsed.package.name, "dep");
}

#[test]
fn dependency_manifest_still_reports_real_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    std::fs::write(&path, "[package]\nname = \"dep\"\nversion = \n").unwrap();
    assert!(parse_dependency_manifest_full(&path).is_err());
    std::fs::write(
        &path,
        "[package]\nname = \"Bad Name\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    assert!(parse_dependency_manifest_full(&path).is_err());
}

/// BT-5186: leniency stops at dependency specs — an unknown key inside one
/// `[dependencies]` / `[native.dependencies]` / `[native.overrides]` entry
/// decides what is fetched or how it compiles, so it stays an error.
#[test]
fn dependency_manifest_rejects_unknown_keys_inside_dependency_specs() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    for spec in [
        "[dependencies]\nother = { path = \"../other\", future_spec_key = 1 }\n",
        "[native.dependencies]\ngun = { git = \"https://example.com/gun.git\", tag = \"v1\", future_spec_key = 1 }\n",
        "[native.overrides]\ngun = { erl_opts = [\"debug_info\"], future_spec_key = 1 }\n",
    ] {
        std::fs::write(
            &path,
            format!("[package]\nname = \"dep\"\nversion = \"0.1.0\"\n\n{spec}"),
        )
        .unwrap();
        let err = parse_dependency_manifest_full(&path).expect_err(spec);
        assert!(
            format!("{err:?}").contains("future_spec_key"),
            "error must name the key for {spec}: {err:?}"
        );
    }
}

/// BT-5186: `[native]` itself is a closed table: strict in the project's own
/// manifest (a typo such as `[native.dependancies]` used to be silently
/// ignored), dropped with a warning in a dependency's.
#[test]
fn unknown_native_subtable_is_strict_for_the_project_and_lenient_for_a_dependency() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    std::fs::write(
        &path,
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n\n[native.dependancies]\ngun = \"2.0.0\"\n",
    )
    .unwrap();
    let err = parse_manifest_full(&path).unwrap_err();
    assert!(format!("{err:?}").contains("dependancies"), "{err:?}");
    let parsed = parse_dependency_manifest_full(&path).unwrap();
    assert!(parsed.native_dependencies.is_empty());
}

#[test]
fn unknown_key_warning_is_first_only_per_file_and_key() {
    let dir = tempfile::tempdir().unwrap();
    let root = camino::Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
    let path = root.join("beamtalk.toml");
    std::fs::write(&path, "").unwrap();
    assert!(first_unknown_key_warning(&path, "k"));
    assert!(!first_unknown_key_warning(&path, "k"));
    // Another spelling of the same file is the same dependency.
    assert!(!first_unknown_key_warning(
        &root.join(".").join("beamtalk.toml"),
        "k"
    ));
    assert!(first_unknown_key_warning(&path, "other"));
}

/// An unknown key inside an inline table cannot be removed alone; dropping
/// its line would silently drop the table's known keys too (here, `gun`).
#[test]
fn dependency_manifest_rejects_unknown_key_inside_an_inline_table() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    std::fs::write(
        &path,
        "native = { dependencies = { gun = \"2.0.0\" }, future_opt = true }\n\n\
         [package]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    )
    .unwrap();
    let err = parse_dependency_manifest_full(&path).unwrap_err();
    assert!(format!("{err:?}").contains("future_opt"), "{err:?}");
}

/// `[release]` and `[diagnostics]` never affect a dependent, so a dependency
/// whose sections use keys this toolchain does not know still parses.
#[test]
fn dependency_manifest_tolerates_unknown_release_and_diagnostics_entries() {
    let dir = tempfile::tempdir().unwrap();
    let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
    std::fs::write(
        &path,
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\n\n\
         [release]\nfuture-release-key = true\n\n\
         [diagnostics]\nfuture-category = \"warning\"\n",
    )
    .unwrap();
    assert!(parse_manifest_full(&path).is_err());
    let parsed = parse_dependency_manifest_full(&path).unwrap();
    assert!(parsed.diagnostics.is_empty());
}

/// A dependency's own compile reads its manifest leniently too, so it keeps
/// its `pkg@Class` qualifier checks.
#[test]
fn dependency_checker_scope_reads_the_manifest_leniently() {
    let dir = TempDir::new().unwrap();
    let root = write_manifest(
        &dir,
        "[package]\nname = \"dep\"\nversion = \"0.1.0\"\nfuture_package_key = 1\n",
    );
    assert_eq!(package_checker_scope(Some(&root)).known_packages, None);
    let scope = dependency_checker_scope(&root);
    assert_eq!(scope.package_name.as_deref(), Some("dep"));
    assert!(scope.known_packages.is_some());
}

#[test]
fn manifest_min_beamtalk_is_optional_and_parsed() {
    let temp = TempDir::new().unwrap();
    let path = write_manifest(
        &temp,
        "[package]\nname = \"p\"\nversion = \"1.0.0\"\nmin-beamtalk = \"0.7.0\"\n",
    );
    let pkg = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(pkg.min_beamtalk.as_deref(), Some("0.7.0"));

    let path = write_manifest(&temp, "[package]\nname = \"p\"\nversion = \"1.0.0\"\n");
    let pkg = parse_manifest(&path.join("beamtalk.toml")).unwrap();
    assert_eq!(pkg.min_beamtalk, None);
}

#[test]
fn manifest_with_malformed_min_beamtalk_is_a_diagnostic_not_a_panic() {
    for bad in ["", ">=0.7.0", "0.7", "0.7.0-rc1", "latest", "0.7.x"] {
        let content =
            format!("[package]\nname = \"p\"\nversion = \"1.0.0\"\nmin-beamtalk = \"{bad}\"\n");
        let err = parse_err(&content);
        assert!(err.contains("min-beamtalk"), "{bad}: {err}");
    }
}
