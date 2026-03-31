// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Create new beamtalk projects.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

use super::manifest::{format_name_error, validate_package_name};

/// Whether the project is a library or an application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectKind {
    Library,
    Application,
}

/// Create a new beamtalk project.
pub fn new_project(name: &str, app: bool) -> Result<()> {
    // Validate package name before creating anything
    if let Err(e) = validate_package_name(name) {
        miette::bail!("{}", format_name_error(name, &e));
    }

    let project_path = Utf8PathBuf::from(name);

    // Check if directory already exists
    if project_path.exists() {
        miette::bail!("Directory '{}' already exists", name);
    }

    let kind = if app {
        ProjectKind::Application
    } else {
        ProjectKind::Library
    };

    // Create project directory structure
    create_project_structure(&project_path, name, kind)
        .wrap_err_with(|| format!("Failed to create project '{name}'"))?;

    let kind_label = match kind {
        ProjectKind::Library => "library",
        ProjectKind::Application => "application",
    };
    println!("Created {kind_label} package '{name}'");
    println!();
    println!("Next steps:");
    println!("  cd {name}");
    println!("  just build");
    println!("  just test");
    if kind == ProjectKind::Application {
        println!("  just run");
    }

    Ok(())
}

/// Create the project directory structure with template files.
fn create_project_structure(path: &Utf8Path, name: &str, kind: ProjectKind) -> Result<()> {
    // Create directories
    fs::create_dir_all(path.join("src"))
        .into_diagnostic()
        .wrap_err("Failed to create src directory")?;

    fs::create_dir_all(path.join("test"))
        .into_diagnostic()
        .wrap_err("Failed to create test directory")?;

    fs::create_dir_all(path.join(".github").join("workflows"))
        .into_diagnostic()
        .wrap_err("Failed to create .github/workflows directory")?;

    // Create beamtalk.toml
    write_beamtalk_toml(path, name, kind)?;

    // Create source files
    match kind {
        ProjectKind::Library => write_library_sources(path, name)?,
        ProjectKind::Application => write_application_sources(path, name)?,
    }

    // Create test file
    write_sample_test(path, name, kind)?;

    // Create README.md
    write_readme(path, name, kind)?;

    // Create .gitignore
    write_gitignore(path)?;

    // Create Justfile
    write_justfile(path, kind)?;

    // Create CI workflow
    write_ci_workflow(path)?;

    // Create AGENTS.md
    write_agents_md(path, name)?;

    // Create .github/copilot-instructions.md
    write_copilot_instructions(path, name)?;

    // Create .mcp.json
    write_mcp_json(path)?;

    Ok(())
}

fn write_beamtalk_toml(path: &Utf8Path, name: &str, kind: ProjectKind) -> Result<()> {
    let mut content = format!(
        r#"# Copyright 2026 {name} authors
# SPDX-License-Identifier: Apache-2.0

[package]
name = "{name}"
version = "0.1.0"
"#
    );

    if kind == ProjectKind::Application {
        use std::fmt::Write;
        let supervisor = to_class_name(name, "AppSup");
        write!(content, "\n[application]\nsupervisor = \"{supervisor}\"\n").unwrap();
    }

    content.push_str("\n[dependencies]\n");

    fs::write(path.join("beamtalk.toml"), content)
        .into_diagnostic()
        .wrap_err("Failed to create beamtalk.toml")
}

fn write_library_sources(path: &Utf8Path, name: &str) -> Result<()> {
    let class_name = to_class_name(name, "");
    let content = format!(
        r#"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

/// {class_name} — main class for the {name} library.
Object subclass: {class_name}

  /// Return a greeting string.
  class greet => "Hello from {name}!"
"#
    );
    fs::write(path.join("src").join(format!("{class_name}.bt")), content)
        .into_diagnostic()
        .wrap_err("Failed to create library source file")
}

fn write_application_sources(path: &Utf8Path, name: &str) -> Result<()> {
    let sup_name = to_class_name(name, "AppSup");
    let main_name = "Main";

    // Write supervisor
    let sup_content = format!(
        r"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

/// {sup_name} — root supervisor for {name}.
///
/// Referenced in beamtalk.toml [application] supervisor.
/// `beamtalk run` starts this automatically as an OTP application.
Supervisor subclass: {sup_name}

  class strategy -> Symbol => #oneForOne

  class children => #()
"
    );
    fs::write(path.join("src").join(format!("{sup_name}.bt")), sup_content)
        .into_diagnostic()
        .wrap_err("Failed to create supervisor")?;

    // Write Main entry point
    let main_content = format!(
        r#"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

/// Main entry point for {name}.
/// Run with: beamtalk run Main run
Object subclass: {main_name}

  class run =>
    self new run

  run =>
    TranscriptStream current show: "Hello from {name}!"; cr.
    self
"#
    );
    fs::write(path.join("src").join("Main.bt"), main_content)
        .into_diagnostic()
        .wrap_err("Failed to create Main.bt")
}

fn write_sample_test(path: &Utf8Path, name: &str, kind: ProjectKind) -> Result<()> {
    let (class_name, assertion) = match kind {
        ProjectKind::Library => {
            let cn = to_class_name(name, "");
            let assertion = format!("self assert: ({cn} greet) equals: \"Hello from {name}!\"");
            (format!("{cn}Test"), assertion)
        }
        ProjectKind::Application => {
            // Main is a value object, so `new` returns an instance
            let assertion = "self assert: (Main new class name) equals: #Main".to_string();
            ("MainTest".to_string(), assertion)
        }
    };

    let content = format!(
        r"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

TestCase subclass: {class_name}

  testBasic =>
    {assertion}
"
    );
    fs::write(path.join("test").join(format!("{class_name}.bt")), content)
        .into_diagnostic()
        .wrap_err("Failed to create test file")
}

fn write_readme(path: &Utf8Path, name: &str, kind: ProjectKind) -> Result<()> {
    let kind_label = match kind {
        ProjectKind::Library => "library",
        ProjectKind::Application => "application",
    };

    let mut content = format!(
        r"# {name}

A Beamtalk {kind_label}.

## Building

```bash
just build
```

## Testing

```bash
just test
```
"
    );

    if kind == ProjectKind::Application {
        content.push_str(
            r"
## Running

```bash
just run
```
",
        );
    }

    fs::write(path.join("README.md"), content)
        .into_diagnostic()
        .wrap_err("Failed to create README.md")
}

fn write_gitignore(path: &Utf8Path) -> Result<()> {
    let content = r"# Build outputs
/_build/
*.beam
*.core

# IDE
.vscode/
.idea/
*.swp
*.swo
";
    fs::write(path.join(".gitignore"), content)
        .into_diagnostic()
        .wrap_err("Failed to create .gitignore")
}

fn write_justfile(path: &Utf8Path, kind: ProjectKind) -> Result<()> {
    let mut content = include_str!("../../templates/Justfile").to_string();

    if kind == ProjectKind::Application {
        content.push_str(
            "\n# Run the application\nrun:\n    beamtalk run\n",
        );
    }

    fs::write(path.join("Justfile"), content)
        .into_diagnostic()
        .wrap_err("Failed to create Justfile")
}

fn write_ci_workflow(path: &Utf8Path) -> Result<()> {
    let content = include_str!("../../templates/ci.yml");
    fs::write(
        path.join(".github").join("workflows").join("ci.yml"),
        content,
    )
    .into_diagnostic()
    .wrap_err("Failed to create CI workflow")
}

fn write_agents_md(path: &Utf8Path, name: &str) -> Result<()> {
    let content = include_str!("../../templates/agents.md").replace("{{project_name}}", name);
    fs::write(path.join("AGENTS.md"), content)
        .into_diagnostic()
        .wrap_err("Failed to create AGENTS.md")
}

fn write_copilot_instructions(path: &Utf8Path, name: &str) -> Result<()> {
    let content =
        include_str!("../../templates/copilot-instructions.md").replace("{{project_name}}", name);
    fs::write(
        path.join(".github").join("copilot-instructions.md"),
        content,
    )
    .into_diagnostic()
    .wrap_err("Failed to create .github/copilot-instructions.md")
}

fn write_mcp_json(path: &Utf8Path) -> Result<()> {
    let content = r#"{
  "mcpServers": {
    "beamtalk": {
      "command": "beamtalk-mcp",
      "args": ["--start"]
    }
  }
}
"#;
    fs::write(path.join(".mcp.json"), content)
        .into_diagnostic()
        .wrap_err("Failed to create .mcp.json")
}

/// Convert a `snake_case` package name to a `PascalCase` class name.
/// If `suffix` is non-empty, it is appended.
fn to_class_name(name: &str, suffix: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;
    for ch in name.chars() {
        if ch == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(ch.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    result.push_str(suffix);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use tempfile::TempDir;

    /// Helper: create a project in a temp dir and return the temp dir.
    fn create_test_project(project_name: &str, app: bool) -> (TempDir, std::path::PathBuf) {
        let temp = TempDir::new().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name, app).unwrap();

        std::env::set_current_dir(&original_dir).unwrap();

        let project_path = temp.path().join(project_name);
        (temp, project_path)
    }

    #[test]
    fn test_to_class_name() {
        assert_eq!(to_class_name("foo", ""), "Foo");
        assert_eq!(to_class_name("my_cool_app", ""), "MyCoolApp");
        assert_eq!(to_class_name("my_app", "AppSup"), "MyAppAppSup");
        assert_eq!(to_class_name("http", "AppSup"), "HttpAppSup");
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_directory() {
        let (_temp, project_path) = create_test_project("test_lib_dir", false);
        assert!(project_path.exists());
        assert!(project_path.is_dir());
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_src_directory() {
        let (_temp, project_path) = create_test_project("test_lib_src", false);
        assert!(project_path.join("src").exists());
        assert!(project_path.join("src").is_dir());
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_test_directory() {
        let (_temp, project_path) = create_test_project("test_lib_testdir", false);
        assert!(project_path.join("test").exists());
        assert!(project_path.join("test").is_dir());
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_beamtalk_toml() {
        let (_temp, project_path) = create_test_project("test_lib_toml", false);
        let content = fs::read_to_string(project_path.join("beamtalk.toml")).unwrap();
        assert!(content.contains("[package]"));
        assert!(content.contains("name = \"test_lib_toml\""));
        assert!(content.contains("version = \"0.1.0\""));
        assert!(
            !content.contains("[application]"),
            "library should not contain [application] section"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_class_file() {
        let (_temp, project_path) = create_test_project("my_lib", false);
        let class_path = project_path.join("src").join("MyLib.bt");
        assert!(class_path.exists(), "library class file should exist");

        let content = fs::read_to_string(class_path).unwrap();
        assert!(content.contains("Object subclass: MyLib"));
        assert!(content.contains("class greet"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_test_file() {
        let (_temp, project_path) = create_test_project("my_lib", false);
        let test_path = project_path.join("test").join("MyLibTest.bt");
        assert!(test_path.exists(), "library test file should exist");

        let content = fs::read_to_string(test_path).unwrap();
        assert!(content.contains("TestCase subclass: MyLibTest"));
        assert!(content.contains("MyLib greet"));
        assert!(
            content.contains("assert:"),
            "test should use assert: method"
        );
        assert!(
            content.contains("equals:"),
            "test should use equals: assertion"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_has_no_main() {
        let (_temp, project_path) = create_test_project("test_lib_nomain", false);
        assert!(
            !project_path.join("src").join("main.bt").exists(),
            "library should not have main.bt"
        );
        assert!(
            !project_path.join("src").join("Main.bt").exists(),
            "library should not have Main.bt"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_app_creates_beamtalk_toml_with_application() {
        let (_temp, project_path) = create_test_project("test_app_toml", true);
        let content = fs::read_to_string(project_path.join("beamtalk.toml")).unwrap();
        assert!(content.contains("[package]"));
        assert!(content.contains("name = \"test_app_toml\""));
        assert!(
            content.contains("[application]"),
            "app should contain [application] section"
        );
        assert!(
            content.contains("supervisor = \"TestAppTomlAppSup\""),
            "app should reference supervisor class: {content}"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_app_creates_supervisor() {
        let (_temp, project_path) = create_test_project("my_app", true);
        let sup_path = project_path.join("src").join("MyAppAppSup.bt");
        assert!(sup_path.exists(), "supervisor file should exist");

        let content = fs::read_to_string(sup_path).unwrap();
        assert!(content.contains("Supervisor subclass: MyAppAppSup"));
        assert!(content.contains("class children"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_app_creates_main() {
        let (_temp, project_path) = create_test_project("my_app", true);
        let main_path = project_path.join("src").join("Main.bt");
        assert!(main_path.exists(), "Main.bt should exist for app");

        let content = fs::read_to_string(main_path).unwrap();
        assert!(content.contains("Object subclass: Main"));
        assert!(content.contains("TranscriptStream current show:"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_library_creates_justfile() {
        let (_temp, project_path) = create_test_project("test_lib_just", false);
        let content = fs::read_to_string(project_path.join("Justfile")).unwrap();
        assert!(content.contains("build:"));
        assert!(content.contains("test:"));
        assert!(content.contains("fmt:"));
        assert!(content.contains("lint:"));
        assert!(content.contains("clean:"));
        assert!(content.contains("ci: fmt lint build test"));
        assert!(content.contains("release:"));
        assert!(content.contains("publish:"));
        assert!(
            !content.contains("run:"),
            "library Justfile should not have run target"
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_app_creates_justfile_with_run() {
        let (_temp, project_path) = create_test_project("test_app_just", true);
        let content = fs::read_to_string(project_path.join("Justfile")).unwrap();
        assert!(content.contains("build:"));
        assert!(content.contains("test:"));
        assert!(
            content.contains("run:"),
            "app Justfile should have run target"
        );
        assert!(content.contains("beamtalk run"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_ci_workflow() {
        let (_temp, project_path) = create_test_project("test_ci_wf", false);
        let ci_path = project_path
            .join(".github")
            .join("workflows")
            .join("ci.yml");
        assert!(ci_path.exists(), "CI workflow should exist");

        let content = fs::read_to_string(ci_path).unwrap();
        assert!(content.contains("erlef/setup-beam"));
        assert!(content.contains("beamtalk.dev/install.sh"));
        assert!(content.contains("setup-just"));
        assert!(content.contains("just fmt"));
        assert!(content.contains("just lint"));
        assert!(content.contains("just build"));
        assert!(content.contains("just test"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_readme() {
        let (_temp, project_path) = create_test_project("test_readme", false);
        let content = fs::read_to_string(project_path.join("README.md")).unwrap();
        assert!(content.contains("# test_readme"));
        assert!(content.contains("library"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_app_creates_readme_with_run() {
        let (_temp, project_path) = create_test_project("test_readme_app", true);
        let content = fs::read_to_string(project_path.join("README.md")).unwrap();
        assert!(content.contains("# test_readme_app"));
        assert!(content.contains("application"));
        assert!(content.contains("just run"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_gitignore() {
        let (_temp, project_path) = create_test_project("test_gitignore", false);
        let content = fs::read_to_string(project_path.join(".gitignore")).unwrap();
        assert!(content.contains("/_build/"));
        assert!(
            !content.contains("/build/"),
            ".gitignore should not contain /build/"
        );
        assert!(content.contains("*.beam"));
        assert!(content.contains("*.core"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_fails_if_directory_exists() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_exists";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name, false).unwrap();

        // Try to create again - should fail
        let result = new_project(project_name, false);
        assert!(result.is_err());

        std::env::set_current_dir(original_dir).unwrap();
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_with_underscores_and_digits() {
        let (_temp, project_path) = create_test_project("my_cool_project123", false);
        assert!(project_path.exists());
        // Library class should be named MyCoolProject123.bt
        assert!(
            project_path
                .join("src")
                .join("MyCoolProject123.bt")
                .exists()
        );
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_rejects_invalid_name() {
        let temp = TempDir::new().unwrap();

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        // Dashes are invalid
        let result = new_project("my-cool-app", false);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid"),
            "error should mention invalid: {err}"
        );

        // CamelCase is invalid
        let result = new_project("MyApp", false);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("my_app"), "error should suggest my_app: {err}");

        // Reserved name
        let result = new_project("stdlib", false);
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("reserved"),
            "error should mention reserved: {err}"
        );

        std::env::set_current_dir(original_dir).unwrap();
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_agents_md() {
        let (_temp, project_path) = create_test_project("test_agents", false);
        let content = fs::read_to_string(project_path.join("AGENTS.md")).unwrap();
        assert!(content.contains("test_agents"));
        assert!(content.contains("beamtalk build"));
        assert!(content.contains("beamtalk repl"));
        assert!(content.contains("Syntax"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_copilot_instructions() {
        let (_temp, project_path) = create_test_project("test_copilot", false);
        let copilot_path = project_path.join(".github").join("copilot-instructions.md");
        assert!(copilot_path.exists());

        let content = fs::read_to_string(copilot_path).unwrap();
        assert!(content.contains("test_copilot"));
        assert!(content.contains("Beamtalk"));
        assert!(content.contains("beamtalk build"));
    }

    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_mcp_json() {
        let (_temp, project_path) = create_test_project("test_mcp", false);
        let content = fs::read_to_string(project_path.join(".mcp.json")).unwrap();
        assert!(content.contains("mcpServers"));
        assert!(content.contains("beamtalk-mcp"));
        assert!(content.contains("--start"));
    }
}
