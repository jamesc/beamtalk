// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Create new beamtalk projects.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

use super::manifest::{format_name_error, validate_package_name};

/// Create a new beamtalk project.
pub fn new_project(name: &str) -> Result<()> {
    // Validate package name before creating anything
    if let Err(e) = validate_package_name(name) {
        miette::bail!("{}", format_name_error(name, &e));
    }

    let project_path = Utf8PathBuf::from(name);

    // Check if directory already exists
    if project_path.exists() {
        miette::bail!("Directory '{}' already exists", name);
    }

    // Create project directory structure
    create_project_structure(&project_path, name)
        .wrap_err_with(|| format!("Failed to create project '{name}'"))?;

    println!("Created package '{name}'");
    println!();
    println!("Next steps:");
    println!("  cd {name}");
    println!("  beamtalk build");
    println!("  beamtalk repl");

    Ok(())
}

/// Create the project directory structure with template files.
fn create_project_structure(path: &Utf8Path, name: &str) -> Result<()> {
    // Create directories
    fs::create_dir_all(path.join("src"))
        .into_diagnostic()
        .wrap_err("Failed to create src directory")?;

    fs::create_dir_all(path.join("test"))
        .into_diagnostic()
        .wrap_err("Failed to create test directory")?;

    fs::create_dir_all(path.join(".github"))
        .into_diagnostic()
        .wrap_err("Failed to create .github directory")?;

    // Create beamtalk.toml
    let toml_content = format!(
        r#"# Copyright 2026 {name} authors
# SPDX-License-Identifier: Apache-2.0

[package]
name = "{name}"
version = "0.1.0"

[dependencies]
"#
    );
    fs::write(path.join("beamtalk.toml"), toml_content)
        .into_diagnostic()
        .wrap_err("Failed to create beamtalk.toml")?;

    // Create main.bt
    let main_content = format!(
        r#"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

// Main entry point for {name}.
// Run with: beamtalk run Main run
// Or load interactively: beamtalk repl

Object subclass: Main

  class run =>
    self new run

  run =>
    TranscriptStream current show: "Hello from {name}!"; cr.
    self
"#
    );
    fs::write(path.join("src").join("main.bt"), main_content)
        .into_diagnostic()
        .wrap_err("Failed to create main.bt")?;

    // Create README.md
    let readme_content = format!(
        r"# {name}

A beamtalk project.

## Building

```bash
beamtalk build
```

## Running

```bash
beamtalk repl
```
"
    );
    fs::write(path.join("README.md"), readme_content)
        .into_diagnostic()
        .wrap_err("Failed to create README.md")?;

    // Create .gitignore
    let gitignore_content = r"# Build outputs
/_build/
*.beam
*.core

# IDE
.vscode/
.idea/
*.swp
*.swo
";
    fs::write(path.join(".gitignore"), gitignore_content)
        .into_diagnostic()
        .wrap_err("Failed to create .gitignore")?;

    // Create AGENTS.md
    write_agents_md(path, name)?;

    // Create .github/copilot-instructions.md
    write_copilot_instructions(path, name)?;

    // Create .mcp.json — points at beamtalk-mcp with --start so Claude auto-boots
    // the workspace on first open without requiring `beamtalk repl` to be running.
    let mcp_content = r#"{
  "mcpServers": {
    "beamtalk": {
      "command": "beamtalk-mcp",
      "args": ["--start"]
    }
  }
}
"#;
    fs::write(path.join(".mcp.json"), mcp_content)
        .into_diagnostic()
        .wrap_err("Failed to create .mcp.json")?;

    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    use tempfile::TempDir;

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_directory() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project";

        // Save current dir and change to temp
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let project_path = temp.path().join(project_name);
        assert!(project_path.exists());
        assert!(project_path.is_dir());
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_src_directory() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_src";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let src_path = temp.path().join(project_name).join("src");
        assert!(src_path.exists());
        assert!(src_path.is_dir());
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_beamtalk_toml() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_toml";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let toml_path = temp.path().join(project_name).join("beamtalk.toml");
        assert!(toml_path.exists());

        let content = fs::read_to_string(toml_path).unwrap();
        assert!(content.contains("[package]"));
        assert!(content.contains(&format!("name = \"{project_name}\"")));
        assert!(content.contains("version = \"0.1.0\""));
        assert!(
            !content.contains("[run]"),
            "should not contain [run] section"
        );
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_main_bt() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_main";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let main_path = temp.path().join(project_name).join("src").join("main.bt");
        assert!(main_path.exists());

        let content = fs::read_to_string(main_path).unwrap();
        assert!(content.contains("Object subclass: Main"));
        assert!(content.contains("TranscriptStream current show:"));
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_readme() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_readme";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let readme_path = temp.path().join(project_name).join("README.md");
        assert!(readme_path.exists());

        let content = fs::read_to_string(readme_path).unwrap();
        assert!(content.contains(&format!("# {project_name}")));
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_gitignore() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_gitignore";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files
        std::env::set_current_dir(&original_dir).unwrap();

        let gitignore_path = temp.path().join(project_name).join(".gitignore");
        assert!(gitignore_path.exists());

        let content = fs::read_to_string(gitignore_path).unwrap();
        assert!(content.contains("/_build/"));
        assert!(
            !content.contains("/build/"),
            ".gitignore should not contain /build/"
        );
        assert!(content.contains("*.beam"));
        assert!(content.contains("*.core"));
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_fails_if_directory_exists() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_exists";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        // Create project first time
        new_project(project_name).unwrap();

        // Try to create again - should fail
        let result = new_project(project_name);
        assert!(result.is_err());

        // Restore directory before test ends
        std::env::set_current_dir(original_dir).unwrap();
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_with_underscores_and_digits() {
        let temp = TempDir::new().unwrap();
        let project_name = "my_cool_project123";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files to avoid tempdir cleanup issues
        std::env::set_current_dir(&original_dir).unwrap();

        let project_path = temp.path().join(project_name);
        assert!(project_path.exists());
        assert!(project_path.join("src").join("main.bt").exists());
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_rejects_invalid_name() {
        let temp = TempDir::new().unwrap();

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        // Dashes are invalid
        let result = new_project("my-cool-app");
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("Invalid"),
            "error should mention invalid: {err}"
        );

        // CamelCase is invalid
        let result = new_project("MyApp");
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(err.contains("my_app"), "error should suggest my_app: {err}");

        // Reserved name
        let result = new_project("stdlib");
        assert!(result.is_err());
        let err = format!("{:?}", result.unwrap_err());
        assert!(
            err.contains("reserved"),
            "error should mention reserved: {err}"
        );

        // Restore directory
        std::env::set_current_dir(original_dir).unwrap();
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_test_directory() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_testdir";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        std::env::set_current_dir(&original_dir).unwrap();

        let test_path = temp.path().join(project_name).join("test");
        assert!(test_path.exists());
        assert!(test_path.is_dir());
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_agents_md() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_agents";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        std::env::set_current_dir(&original_dir).unwrap();

        let agents_path = temp.path().join(project_name).join("AGENTS.md");
        assert!(agents_path.exists());

        let content = fs::read_to_string(agents_path).unwrap();
        assert!(content.contains(project_name));
        assert!(content.contains("beamtalk build"));
        assert!(content.contains("beamtalk repl"));
        assert!(content.contains("Syntax"));
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_copilot_instructions() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_copilot";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        std::env::set_current_dir(&original_dir).unwrap();

        let copilot_path = temp
            .path()
            .join(project_name)
            .join(".github")
            .join("copilot-instructions.md");
        assert!(copilot_path.exists());

        let content = fs::read_to_string(copilot_path).unwrap();
        assert!(content.contains(project_name));
        assert!(content.contains("Beamtalk"));
        assert!(content.contains("beamtalk build"));
    }

    /// Uses `#[serial(cwd)]` because it changes the current working directory
    /// (process-global state) using `std::env::set_current_dir`.
    #[test]
    #[serial(cwd)]
    fn test_new_project_creates_mcp_json() {
        let temp = TempDir::new().unwrap();
        let project_name = "test_project_mcp";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        std::env::set_current_dir(&original_dir).unwrap();

        let mcp_path = temp.path().join(project_name).join(".mcp.json");
        assert!(mcp_path.exists());

        let content = fs::read_to_string(mcp_path).unwrap();
        assert!(content.contains("mcpServers"));
        assert!(content.contains("beamtalk-mcp"));
        assert!(content.contains("--start"));
    }
}
