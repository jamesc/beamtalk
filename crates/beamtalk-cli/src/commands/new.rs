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

/// Static MCP, essential patterns, and pitfalls sections appended to every
/// generated AGENTS.md. Kept as a constant to keep `write_agents_md` under
/// the line-count limit.
const AGENTS_MD_MCP_AND_PITFALLS: &str = r#"## Development Workflow

The `.mcp.json` MCP server provides a persistent REPL session. Use it as
your primary development environment — not CLI commands.

**Session startup:**

1. Call `describe` to discover available operations
2. Call `load_project` with `include_tests: true` to load all source + tests
3. On a new codebase, read the language guide at https://www.beamtalk.dev/docs/language-features

**Edit → Reload → Test → Debug loop:**

1. Edit a `.bt` source file
2. `evaluate: 'Workspace load: "path"'` or `evaluate: "ClassName reload"`
   — or `load_project` again after multi-file edits
3. `test` with class name or file path — fast, no recompile
4. `evaluate` to debug failures — bindings preserved from prior calls
5. Only use CLI `beamtalk test` as a final full-suite check before committing

**Useful eval commands:**
- `Beamtalk help: ClassName` — class docs
- `Workspace load: "path"` — load a file
- `ClassName reload` — reload a changed class
- `Workspace classes` — list loaded classes

**Why MCP over CLI:**
- Classes stay loaded — no fresh compile each time
- Local bindings preserved — debug state carries across tool calls
- Faster iteration — reload one class, not rebuild everything

## Live Workspace (MCP)

The `.mcp.json` in this project configures the `beamtalk` MCP server, which gives
you live access to a running REPL. Claude Code starts it automatically via
`beamtalk-mcp --start` — no manual `beamtalk repl` required.

**Prefer MCP tools over guessing.** If you're uncertain what a method returns or
whether code is correct, evaluate it directly rather than inferring from source.

| Tool | When to use |
|------|-------------|
| `describe` | First call — discover operations and protocol version |
| `load_project` | Session startup — load all source + test files |
| `evaluate` | Test expressions, debug, call Workspace/Beamtalk APIs |
| `test` | Run tests by class name or file path |
| `complete` | Autocompletion suggestions |
| `search_examples` | Find patterns and working code (offline) |
| `show_codegen` | Inspect generated Core Erlang |
| `inspect` | Examine a live actor's state |

## Essential Patterns

### Class Hierarchy

```beamtalk
// Immutable data — auto-generates getters, withX: setters, keyword constructor, equality
Value subclass: Point
  state: x = 0
  state: y = 0

// Mutable state — manual getters/setters, self.field := works
Object subclass: Config
  state: raw = nil

// Concurrent process — gen_server backed, async casts with !
Actor subclass: Server
  state: count = 0

// OTP supervision tree — for long-running services
Supervisor subclass: MyApp
  class strategy => #oneForOne
  class children => #(DatabasePool, HttpServer, Worker)
```

Rules:
- Pure data → `Value`
- Mutable but not concurrent → `Object`
- Concurrent process → `Actor`
- Long-running service with child processes → `Supervisor` with `beamtalk run`

### String Escaping

| Syntax | Result |
|--------|--------|
| `"hello {name}"` | String interpolation |
| `"literal \{ brace \}"` | Escaped braces |
| `"She said ""hello"""` | Escaped double-quote |

### Destructuring and match:

```beamtalk
// Tuple destructuring (critical for Erlang FFI)
{#ok, content} := Erlang file read_file: "path"

// Array destructuring
#[a, b] := #[10, 20]

// Map destructuring
#{#x => x, #y => y} := someDict

// match: with clauses
value match: [
  #ok -> "success";
  #error -> "failure";
  _ -> "unknown"
]
```

### Key Stdlib Classes

| Class | Purpose |
|-------|---------|
| `System` | `getEnv:`, `osPlatform`, `pid` |
| `Subprocess` | Sync subprocess with stdin/stdout |
| `ReactiveSubprocess` | Push-mode subprocess with delegate callbacks |
| `Supervisor` | OTP supervision trees for service applications |
| `HTTPClient` / `HTTPServer` | HTTP client and server |
| `File` | Filesystem operations |
| `Json` / `Yaml` | Serialization |

### Critical Gotcha — Block Mutations

```beamtalk
// WRONG on Value/Object — assignment inside block doesn't propagate
count := 0
items do: [:x | count := count + 1]  // count is still 0!

// CORRECT — use inject:into:
count := items inject: 0 into: [:acc :x | acc + 1]
```

## Not Smalltalk — Common Pitfalls

Beamtalk looks like Smalltalk but has important differences. The compiler will
catch most of these, but they waste time:

| Smalltalk habit | Beamtalk equivalent | Notes |
|---|---|---|
| `\| temp \|` temp var declarations | Just use `:=` directly | No declaration syntax |
| Trailing `.` on every statement | Newline is the separator | `.` is optional; use it only to disambiguate cascades |
| `"this is a comment"` | `// this is a comment` | Double-quoted strings are data, not comments |
| `^value` on last expression | Just write `value` | `^` is early-return only; last expr is implicitly returned |
| Left-to-right binary (`2+3*4=20`) | Standard math precedence (`2+3*4=14`) | `*` binds tighter than `+` |
| `'hello', name` concatenation | `"hello {name}"` interpolation | `++` also works: `"hello" ++ name` |
| `[:x \| \|temp\| temp := x]` block locals | `[:x \| temp := x]` | No block-local declarations |
| `:` for type annotations | `::` (double-colon) | `state: x :: Integer = 0`, `param :: Type -> ReturnType =>` |
| Unknown message raises error | Same — DNU raises `does_not_understand` error | Use `respondsTo:` to check before sending |

**`^` in blocks is a non-local return (exits the enclosing method):**

```beamtalk
// ^ inside a block exits the METHOD, not just the block:
firstPositive: items =>
  items do: [:x | x > 0 ifTrue: [^x]].   // ^ returns from firstPositive:
  nil   // reached only if no positive element found
```

**DNU raises a `does_not_understand` error.** Sending a message a class
doesn't implement raises a structured error — not a silent `false`. Use
`respondsTo:` or `evaluate` in the live workspace to confirm a method exists
before calling it.

**Implicit return rule:** the last expression of a method body is always its
return value. Never write `^` on the last line — only use it for early exits
inside the method:

```beamtalk
// Wrong — redundant ^
max: other =>
  ^(self > other ifTrue: [self] ifFalse: [other])

// Correct
max: other =>
  self > other ifTrue: [self] ifFalse: [other]

// Correct use of ^ for early return
safeDiv: other =>
  other = 0 ifTrue: [^0].
  self / other
```

## Language Documentation

- **Full language reference:** https://www.beamtalk.dev/docs/language-features — read this when starting work on a new Beamtalk codebase
- Syntax rationale: https://www.beamtalk.dev/docs/syntax-rationale
- Examples: see `src/` directory
"#;

fn write_agents_md(path: &Utf8Path, name: &str) -> Result<()> {
    let content = format!(
        r#"# {name} — Agent Guide

## Project Structure

```
{name}/
├── beamtalk.toml    # Package manifest
├── src/             # Source files (.bt)
│   └── main.bt      # Entry point
├── test/            # BUnit test files
├── _build/          # Build output (generated)
├── AGENTS.md        # This file
├── .github/
│   └── copilot-instructions.md
├── .mcp.json        # MCP server config
├── README.md
└── .gitignore
```

## Build & Run

```bash
beamtalk build       # Compile to BEAM bytecode
beamtalk repl        # Interactive development (auto-loads package)
beamtalk test        # Run BUnit tests
```

## Beamtalk Syntax Basics

```beamtalk
// Variables
x := 42
name := "hello"

// Message sends
x factorial              // unary
3 + 4                    // binary
list at: 1 put: "value"  // keyword

// Blocks (closures)
square := [:x | x * x]
square value: 5          // => 25

// Classes
Object subclass: Counter
  state: count = 0

  increment => self.count := self.count + 1
  count => self.count
```

{AGENTS_MD_MCP_AND_PITFALLS}"#
    );
    fs::write(path.join("AGENTS.md"), content)
        .into_diagnostic()
        .wrap_err("Failed to create AGENTS.md")
}

fn write_copilot_instructions(path: &Utf8Path, name: &str) -> Result<()> {
    let content = format!(
        r"# Copilot Instructions for {name}

This is a [Beamtalk](https://www.beamtalk.dev) project that compiles to the BEAM virtual machine.

## Key Conventions

- Source files use `.bt` extension and live in `src/`
- Tests use BUnit (TestCase subclasses) and live in `test/`
- Build output goes to `_build/` (gitignored)
- Package manifest is `beamtalk.toml`

## Beamtalk Syntax

- Smalltalk-inspired message passing: `object message`, `object message: arg`
- Blocks are closures: `[:x | x + 1]`
- Use `//` for line comments
- Implicit returns (last expression is the return value)
- Use `^` only for early returns, never on the last expression
- Newlines separate statements (no periods)

## Build Commands

```bash
beamtalk build    # Compile the project
beamtalk repl     # Start interactive REPL
beamtalk test     # Run tests
```

## MCP / Live Workspace

`.mcp.json` configures the `beamtalk` MCP server. In Claude Code it starts
automatically. Call `describe` first, then `load_project` to load all source.
Use `evaluate`, `test`, `inspect`, and `show_codegen` to interact with a live
REPL rather than inferring behaviour from source. See `AGENTS.md` for the
full tool list and development workflow.
"
    );
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
