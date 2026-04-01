// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `.app` file generation for beamtalk packages.
//!
//! **DDD Context:** Build System
//!
//! Generates OTP application resource files (`.app`) from compiled
//! beamtalk packages. See ADR 0026 §3 for the package ↔ OTP mapping.

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

use super::manifest::PackageManifest;
use crate::beam_compiler::escape_erlang_string;

/// Metadata for a class discovered during compilation.
///
/// ADR 0070 Phase 4: Extended with `package`, `kind`, and `type_params`
/// for cross-package compiler resolution.
#[derive(Debug, Clone)]
pub struct ClassMetadata {
    /// The BEAM module name (e.g., `bt@my_counter@counter`).
    pub module: String,
    /// The beamtalk class name (e.g., `Counter`).
    pub class_name: String,
    /// The parent class name (e.g., `Actor`, `Object`).
    pub parent_class: String,
    /// The package that owns this class (from `beamtalk.toml`).
    pub package: String,
    /// The class kind: `"object"`, `"value"`, or `"actor"` (ADR 0067).
    pub kind: String,
    /// Generic type parameter names (ADR 0068), e.g., `["T", "E"]`.
    pub type_params: Vec<String>,
}

/// Generate an OTP `.app` file for a compiled package.
///
/// The `.app` file is written to `{build_dir}/{package_name}.app` and contains:
/// - `{description, ...}` from manifest
/// - `{vsn, ...}` from manifest
/// - `{modules, [...]}` auto-discovered from compiled modules
/// - `{registered, []}`
/// - `{applications, [kernel, stdlib, ...bt_deps..., ...hex_deps..., beamtalk_runtime]}`
/// - `{mod, {beamtalk_{appname}_app, []}}` when `app_callback_module` is `Some`
/// - `{env, [{classes, [...]}]}` class→module mapping
#[allow(clippy::too_many_arguments)]
pub fn generate_app_file(
    build_dir: &Utf8Path,
    manifest: &PackageManifest,
    module_names: &[String],
    class_metadata: &[ClassMetadata],
    app_callback_module: Option<&str>,
    native_module_names: &[String],
    bt_dep_names: &[String],
    hex_dep_names: &[String],
) -> Result<()> {
    let app_content = format_app_file(
        manifest,
        module_names,
        class_metadata,
        app_callback_module,
        native_module_names,
        bt_dep_names,
        hex_dep_names,
    );
    let app_path = build_dir.join(format!("{}.app", manifest.name));

    fs::write(&app_path, app_content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write .app file '{app_path}'"))?;

    Ok(())
}

/// Format the OTP `.app` file content as an Erlang term.
fn format_app_file(
    manifest: &PackageManifest,
    module_names: &[String],
    class_metadata: &[ClassMetadata],
    app_callback_module: Option<&str>,
    native_module_names: &[String],
    bt_dep_names: &[String],
    hex_dep_names: &[String],
) -> String {
    let description = escape_erlang_string(
        manifest
            .description
            .as_deref()
            .unwrap_or("A beamtalk package"),
    );
    let version = escape_erlang_string(&manifest.version);

    let modules_list = format_modules_list(module_names);
    let classes_list = format_classes_list(class_metadata);
    let native_modules_entry = format_native_modules_entry(native_module_names);
    let applications_list = format_applications_list(bt_dep_names, hex_dep_names);

    let mod_entry = match app_callback_module {
        Some(cb_module) => format!("\n    {{mod, {{{cb_module}, []}}}},"),
        None => String::new(),
    };

    format!(
        r#"{{application, {name}, [
    {{description, "{description}"}},
    {{vsn, "{version}"}},
    {{modules, [{modules}]}},
    {{registered, []}},
    {{applications, [{applications}]}},{mod_entry}
    {{env, [
        {{classes, [{classes}]}}{native_modules}
    ]}}
]}}.
"#,
        name = manifest.name,
        description = description,
        version = version,
        modules = modules_list,
        applications = applications_list,
        classes = classes_list,
        mod_entry = mod_entry,
        native_modules = native_modules_entry,
    )
}

/// Format the module list for the `.app` file.
///
/// Module names are quoted atoms since they contain `@` characters.
fn format_modules_list(module_names: &[String]) -> String {
    let mut sorted = module_names.to_vec();
    sorted.sort();
    sorted
        .iter()
        .map(|m| format!("'{m}'"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format the `{applications, [...]}` list for the `.app` file.
///
/// ADR 0072 §7: Direct hex deps are listed between `stdlib` and
/// `beamtalk_runtime` so that `application:ensure_all_started` walks
/// the dependency tree correctly. Transitive deps (e.g., `ranch` via
/// `cowboy`) are covered by each dependency's own `.app` file.
fn format_applications_list(bt_dep_names: &[String], hex_dep_names: &[String]) -> String {
    let mut apps = vec!["kernel".to_string(), "stdlib".to_string()];
    if !bt_dep_names.is_empty() {
        let mut sorted = bt_dep_names.to_vec();
        sorted.sort();
        apps.extend(sorted);
    }
    if !hex_dep_names.is_empty() {
        let mut sorted = hex_dep_names.to_vec();
        sorted.sort();
        apps.extend(sorted);
    }
    apps.push("beamtalk_runtime".to_string());
    apps.join(", ")
}

/// Format the `{native_modules, [...]}` entry for the `.app` `env` section.
///
/// ADR 0072 Phase 1: When a package has `native/*.erl` files, their module
/// names are listed here so OTP tooling and runtime code can discover them.
/// Returns an empty string when there are no native modules (packages without
/// `native/` are unaffected).
fn format_native_modules_entry(native_module_names: &[String]) -> String {
    if native_module_names.is_empty() {
        return String::new();
    }
    let mut sorted = native_module_names.to_vec();
    sorted.sort();
    let modules = sorted.join(", ");
    format!(",\n        {{native_modules, [{modules}]}}")
}

/// Format the class metadata list for the `.app` file.
///
/// ADR 0070 Phase 4: Each entry is a map with module, class, parent, package,
/// kind, and `type_params` fields:
/// ```erlang
/// #{name => 'Counter', module => 'bt@app@counter', parent => 'Actor',
///   package => 'my_app', kind => actor, type_params => []}
/// ```
fn format_classes_list(class_metadata: &[ClassMetadata]) -> String {
    if class_metadata.is_empty() {
        return String::new();
    }
    let mut sorted = class_metadata.to_vec();
    sorted.sort_by(|a, b| a.module.cmp(&b.module));
    sorted
        .iter()
        .map(|c| {
            let type_params = if c.type_params.is_empty() {
                "[]".to_string()
            } else {
                format!(
                    "[{}]",
                    c.type_params
                        .iter()
                        .map(|tp| format!("'{tp}'"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            format!(
                "#{{name => '{class}', module => '{module}', parent => '{parent}', \
                 package => '{package}', kind => {kind}, type_params => {type_params}}}",
                module = c.module,
                class = c.class_name,
                parent = c.parent_class,
                package = c.package,
                kind = c.kind,
                type_params = type_params
            )
        })
        .collect::<Vec<_>>()
        .join(",\n            ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn test_manifest(name: &str, version: &str, description: Option<&str>) -> PackageManifest {
        PackageManifest {
            name: name.to_string(),
            version: version.to_string(),
            description: description.map(String::from),
            licenses: None,
            strict_deps: false,
        }
    }

    #[test]
    fn test_format_app_file_basic() {
        let manifest = test_manifest("my_app", "0.1.0", Some("A test app"));
        let modules = vec![
            "bt@my_app@main".to_string(),
            "bt@my_app@counter".to_string(),
        ];
        let classes = vec![];

        let result = format_app_file(&manifest, &modules, &classes, None, &[], &[], &[]);

        assert!(result.contains("{application, my_app, ["));
        assert!(result.contains("{description, \"A test app\"}"));
        assert!(result.contains("{vsn, \"0.1.0\"}"));
        assert!(result.contains("'bt@my_app@counter'"));
        assert!(result.contains("'bt@my_app@main'"));
        assert!(result.contains("{applications, [kernel, stdlib, beamtalk_runtime]}"));
        assert!(result.contains("{classes, []}"));
        assert!(result.ends_with(".\n"));
        assert!(!result.contains("{mod,"));
    }

    #[test]
    fn test_format_app_file_default_description() {
        let manifest = test_manifest("my_app", "0.1.0", None);
        let result = format_app_file(&manifest, &[], &[], None, &[], &[], &[]);

        assert!(result.contains("{description, \"A beamtalk package\"}"));
    }

    #[test]
    fn test_format_app_file_with_classes() {
        let manifest = test_manifest("my_counter", "0.1.0", Some("A counter example"));
        let modules = vec!["bt@my_counter@counter".to_string()];
        let classes = vec![ClassMetadata {
            module: "bt@my_counter@counter".to_string(),
            class_name: "Counter".to_string(),
            parent_class: "Actor".to_string(),
            package: "my_counter".to_string(),
            kind: "actor".to_string(),
            type_params: vec![],
        }];

        let result = format_app_file(&manifest, &modules, &classes, None, &[], &[], &[]);

        assert!(
            result.contains("name => 'Counter'"),
            "should contain class name in map. Got: {result}"
        );
        assert!(
            result.contains("package => 'my_counter'"),
            "should contain package. Got: {result}"
        );
        assert!(
            result.contains("kind => actor"),
            "should contain kind. Got: {result}"
        );
        assert!(
            result.contains("type_params => []"),
            "should contain empty type_params. Got: {result}"
        );
    }

    #[test]
    fn test_format_modules_list_sorted() {
        let modules = vec!["bt@app@zebra".to_string(), "bt@app@alpha".to_string()];
        let result = format_modules_list(&modules);
        assert_eq!(result, "'bt@app@alpha', 'bt@app@zebra'");
    }

    #[test]
    fn test_format_modules_list_empty() {
        let result = format_modules_list(&[]);
        assert_eq!(result, "");
    }

    #[test]
    fn test_generate_app_file_writes_file() {
        let temp = TempDir::new().unwrap();
        let build_dir = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let manifest = test_manifest("my_app", "0.1.0", Some("Test"));
        let modules = vec!["bt@my_app@main".to_string()];
        let classes = vec![];

        generate_app_file(
            &build_dir,
            &manifest,
            &modules,
            &classes,
            None,
            &[],
            &[],
            &[],
        )
        .unwrap();

        let app_path = build_dir.join("my_app.app");
        assert!(app_path.exists());
        let content = std::fs::read_to_string(&app_path).unwrap();
        assert!(content.contains("{application, my_app, ["));
    }

    #[test]
    fn test_format_classes_list_multiple() {
        let classes = vec![
            ClassMetadata {
                module: "bt@app@b".to_string(),
                class_name: "Bravo".to_string(),
                parent_class: "Object".to_string(),
                package: "app".to_string(),
                kind: "object".to_string(),
                type_params: vec![],
            },
            ClassMetadata {
                module: "bt@app@a".to_string(),
                class_name: "Alpha".to_string(),
                parent_class: "Actor".to_string(),
                package: "app".to_string(),
                kind: "actor".to_string(),
                type_params: vec![],
            },
        ];
        let result = format_classes_list(&classes);
        // Should be sorted by module name — Alpha (bt@app@a) before Bravo (bt@app@b)
        assert!(
            result.find("'Alpha'").unwrap() < result.find("'Bravo'").unwrap(),
            "classes should be sorted by module name. Got: {result}"
        );
    }

    #[test]
    fn test_format_classes_list_with_type_params() {
        let classes = vec![ClassMetadata {
            module: "bt@app@container".to_string(),
            class_name: "Container".to_string(),
            parent_class: "Object".to_string(),
            package: "app".to_string(),
            kind: "object".to_string(),
            type_params: vec!["T".to_string(), "E".to_string()],
        }];
        let result = format_classes_list(&classes);
        assert!(
            result.contains("type_params => ['T', 'E']"),
            "should contain type_params. Got: {result}"
        );
    }

    #[test]
    fn test_format_app_file_escapes_description() {
        let manifest = test_manifest("my_app", "0.1.0", Some(r#"A "quoted" app"#));
        let result = format_app_file(&manifest, &[], &[], None, &[], &[], &[]);
        assert!(result.contains(r#"{description, "A \"quoted\" app"}"#));
    }

    #[test]
    fn test_format_app_file_with_mod_entry() {
        let manifest = test_manifest("my_app", "0.1.0", Some("An OTP app"));
        let result = format_app_file(
            &manifest,
            &[],
            &[],
            Some("beamtalk_my_app_app"),
            &[],
            &[],
            &[],
        );
        assert!(result.contains("{mod, {beamtalk_my_app_app, []}}"));
    }

    #[test]
    fn test_format_app_file_without_mod_entry() {
        let manifest = test_manifest("my_app", "0.1.0", None);
        let result = format_app_file(&manifest, &[], &[], None, &[], &[], &[]);
        assert!(!result.contains("{mod,"));
    }

    #[test]
    fn test_generate_app_file_with_supervisor_writes_mod() {
        let temp = TempDir::new().unwrap();
        let build_dir = camino::Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();

        let manifest = test_manifest("my_app", "0.1.0", Some("Test"));
        let modules = vec!["bt@my_app@app_sup".to_string()];
        let classes = vec![];

        generate_app_file(
            &build_dir,
            &manifest,
            &modules,
            &classes,
            Some("beamtalk_my_app_app"),
            &[],
            &[],
            &[],
        )
        .unwrap();

        let content = std::fs::read_to_string(build_dir.join("my_app.app")).unwrap();
        assert!(content.contains("{mod, {beamtalk_my_app_app, []}}"));
    }

    #[test]
    fn test_format_native_modules_entry_empty() {
        let result = format_native_modules_entry(&[]);
        assert_eq!(result, "");
    }

    #[test]
    fn test_format_native_modules_entry_sorted() {
        let modules = vec!["zebra_mod".to_string(), "alpha_mod".to_string()];
        let result = format_native_modules_entry(&modules);
        assert!(
            result.contains("{native_modules, [alpha_mod, zebra_mod]}"),
            "native_modules should be sorted. Got: {result}"
        );
    }

    #[test]
    fn test_format_native_modules_entry_single() {
        let modules = vec!["hello_native".to_string()];
        let result = format_native_modules_entry(&modules);
        assert!(
            result.contains("{native_modules, [hello_native]}"),
            "Got: {result}"
        );
    }

    #[test]
    fn test_format_app_file_with_native_modules() {
        let manifest = test_manifest("http", "0.1.0", Some("HTTP package"));
        let modules = vec!["bt@http@client".to_string()];
        let native_modules = vec![
            "beamtalk_http_server".to_string(),
            "beamtalk_http".to_string(),
        ];

        let result = format_app_file(&manifest, &modules, &[], None, &native_modules, &[], &[]);

        assert!(
            result.contains("{native_modules, [beamtalk_http, beamtalk_http_server]}"),
            "Should contain sorted native_modules in env. Got: {result}"
        );
        assert!(
            result.contains("{classes, []}"),
            "Should still contain classes. Got: {result}"
        );
    }

    #[test]
    fn test_format_app_file_without_native_modules_unchanged() {
        let manifest = test_manifest("my_app", "0.1.0", Some("No native"));
        let result = format_app_file(&manifest, &[], &[], None, &[], &[], &[]);

        assert!(
            !result.contains("native_modules"),
            "Should not contain native_modules when empty. Got: {result}"
        );
    }

    #[test]
    fn test_format_applications_list_no_hex_deps() {
        let result = format_applications_list(&[], &[]);
        assert_eq!(result, "kernel, stdlib, beamtalk_runtime");
    }

    #[test]
    fn test_format_applications_list_with_hex_deps_sorted() {
        let hex_deps = vec!["gun".to_string(), "cowboy".to_string()];
        let result = format_applications_list(&[], &hex_deps);
        assert_eq!(result, "kernel, stdlib, cowboy, gun, beamtalk_runtime");
    }

    #[test]
    fn test_format_applications_list_with_bt_deps_sorted() {
        let bt_deps = vec!["yaml".to_string(), "http".to_string()];
        let result = format_applications_list(&bt_deps, &[]);
        assert_eq!(result, "kernel, stdlib, http, yaml, beamtalk_runtime");
    }

    #[test]
    fn test_format_applications_list_with_bt_and_hex_deps() {
        let bt_deps = vec!["http".to_string()];
        let hex_deps = vec!["cowboy".to_string()];
        let result = format_applications_list(&bt_deps, &hex_deps);
        assert_eq!(result, "kernel, stdlib, http, cowboy, beamtalk_runtime");
    }

    #[test]
    fn test_format_app_file_with_hex_deps() {
        let manifest = test_manifest("http", "0.1.0", Some("HTTP package"));
        let hex_deps = vec!["gun".to_string(), "cowboy".to_string()];

        let result = format_app_file(&manifest, &[], &[], None, &[], &[], &hex_deps);

        assert!(
            result.contains("{applications, [kernel, stdlib, cowboy, gun, beamtalk_runtime]}"),
            "Should list hex deps between stdlib and beamtalk_runtime. Got: {result}"
        );
    }

    #[test]
    fn test_format_app_file_with_hex_deps_and_native_modules() {
        let manifest = test_manifest("http", "0.1.0", Some("HTTP package"));
        let native_modules = vec!["beamtalk_http".to_string()];
        let hex_deps = vec!["gun".to_string()];

        let result = format_app_file(&manifest, &[], &[], None, &native_modules, &[], &hex_deps);

        assert!(
            result.contains("{applications, [kernel, stdlib, gun, beamtalk_runtime]}"),
            "Should include hex deps. Got: {result}"
        );
        assert!(
            result.contains("{native_modules, [beamtalk_http]}"),
            "Should include native modules. Got: {result}"
        );
    }
}
