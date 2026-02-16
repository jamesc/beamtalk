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
#[derive(Debug, Clone)]
pub struct ClassMetadata {
    /// The BEAM module name (e.g., `bt@my_counter@counter`).
    pub module: String,
    /// The beamtalk class name (e.g., `Counter`).
    pub class_name: String,
    /// The parent class name (e.g., `Actor`, `Object`).
    pub parent_class: String,
}

/// Generate an OTP `.app` file for a compiled package.
///
/// The `.app` file is written to `{build_dir}/{package_name}.app` and contains:
/// - `{description, ...}` from manifest
/// - `{vsn, ...}` from manifest
/// - `{modules, [...]}` auto-discovered from compiled modules
/// - `{registered, []}`
/// - `{applications, [kernel, stdlib, beamtalk_runtime]}`
/// - `{env, [{classes, [...]}]}` class→module mapping
pub fn generate_app_file(
    build_dir: &Utf8Path,
    manifest: &PackageManifest,
    module_names: &[String],
    class_metadata: &[ClassMetadata],
) -> Result<()> {
    let app_content = format_app_file(manifest, module_names, class_metadata);
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

    format!(
        r#"{{application, {name}, [
    {{description, "{description}"}},
    {{vsn, "{version}"}},
    {{modules, [{modules}]}},
    {{registered, []}},
    {{applications, [kernel, stdlib, beamtalk_runtime]}},
    {{env, [
        {{classes, [{classes}]}}
    ]}}
]}}.
"#,
        name = manifest.name,
        description = description,
        version = version,
        modules = modules_list,
        classes = classes_list,
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

/// Format the class metadata list for the `.app` file.
///
/// Each entry is `{'module', 'ClassName', 'ParentClass'}`.
fn format_classes_list(class_metadata: &[ClassMetadata]) -> String {
    if class_metadata.is_empty() {
        return String::new();
    }
    let mut sorted = class_metadata.to_vec();
    sorted.sort_by(|a, b| a.module.cmp(&b.module));
    sorted
        .iter()
        .map(|c| {
            format!(
                "{{'{module}', '{class}', '{parent}'}}",
                module = c.module,
                class = c.class_name,
                parent = c.parent_class
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
            start: None,
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

        let result = format_app_file(&manifest, &modules, &classes);

        assert!(result.contains("{application, my_app, ["));
        assert!(result.contains("{description, \"A test app\"}"));
        assert!(result.contains("{vsn, \"0.1.0\"}"));
        assert!(result.contains("'bt@my_app@counter'"));
        assert!(result.contains("'bt@my_app@main'"));
        assert!(result.contains("{applications, [kernel, stdlib, beamtalk_runtime]}"));
        assert!(result.contains("{classes, []}"));
        assert!(result.ends_with(".\n"));
    }

    #[test]
    fn test_format_app_file_default_description() {
        let manifest = test_manifest("my_app", "0.1.0", None);
        let result = format_app_file(&manifest, &[], &[]);

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
        }];

        let result = format_app_file(&manifest, &modules, &classes);

        assert!(result.contains("{'bt@my_counter@counter', 'Counter', 'Actor'}"));
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

        generate_app_file(&build_dir, &manifest, &modules, &classes).unwrap();

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
            },
            ClassMetadata {
                module: "bt@app@a".to_string(),
                class_name: "Alpha".to_string(),
                parent_class: "Actor".to_string(),
            },
        ];
        let result = format_classes_list(&classes);
        // Should be sorted by module name
        assert!(result.starts_with("{'bt@app@a'"));
        assert!(result.contains("{'bt@app@b'"));
    }

    #[test]
    fn test_format_app_file_escapes_description() {
        let manifest = test_manifest("my_app", "0.1.0", Some(r#"A "quoted" app"#));
        let result = format_app_file(&manifest, &[], &[]);
        assert!(result.contains(r#"{description, "A \"quoted\" app"}"#));
    }
}
