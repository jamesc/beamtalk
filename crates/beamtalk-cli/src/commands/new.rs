// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Create new beamtalk projects.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::fs;

/// Create a new beamtalk project.
pub fn new_project(name: &str) -> Result<()> {
    let project_path = Utf8PathBuf::from(name);

    // Check if directory already exists
    if project_path.exists() {
        miette::bail!("Directory '{}' already exists", name);
    }

    // Create project directory structure
    create_project_structure(&project_path, name)
        .wrap_err_with(|| format!("Failed to create project '{name}'"))?;

    println!("Created beamtalk project '{name}'");
    println!();
    println!("Next steps:");
    println!("  cd {name}");
    println!("  beamtalk build");
    println!("  beamtalk run");

    Ok(())
}

/// Create the project directory structure with template files.
fn create_project_structure(path: &Utf8Path, name: &str) -> Result<()> {
    // Create directories
    fs::create_dir_all(path.join("src"))
        .into_diagnostic()
        .wrap_err("Failed to create src directory")?;

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
        r"// Copyright 2026 {name} authors
// SPDX-License-Identifier: Apache-2.0

// Main entry point for {name}
main := [
    Transcript show: 'Hello from {name}!'; cr.
]
"
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
beamtalk run
```
"
    );
    fs::write(path.join("README.md"), readme_content)
        .into_diagnostic()
        .wrap_err("Failed to create README.md")?;

    // Create .gitignore
    let gitignore_content = r"# Build outputs
/build/
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

    Ok(())
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
        assert!(content.contains("main :="));
        assert!(content.contains("Transcript show:"));
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
        assert!(content.contains("/build/"));
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
    fn test_new_project_with_special_characters() {
        let temp = TempDir::new().unwrap();
        let project_name = "my-cool_project123";

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        new_project(project_name).unwrap();

        // Restore directory before checking files to avoid tempdir cleanup issues
        std::env::set_current_dir(&original_dir).unwrap();

        let project_path = temp.path().join(project_name);
        assert!(project_path.exists());
        assert!(project_path.join("src").join("main.bt").exists());
    }
}
