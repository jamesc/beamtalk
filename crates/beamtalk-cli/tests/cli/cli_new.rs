// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Subprocess tests for `beamtalk new`.

use crate::cli_common;

use predicates::str::contains;

#[test]
fn new_library_generates_project_structure() {
    let dir = tempfile::tempdir().unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "my_lib"])
        .assert()
        .success()
        .stdout(contains("Created library package"));

    let proj = dir.path().join("my_lib");
    assert!(proj.join("beamtalk.toml").exists(), "beamtalk.toml missing");
    assert!(proj.join("src").is_dir(), "src/ missing");
    assert!(proj.join("test").is_dir(), "test/ missing");
    assert!(proj.join(".gitignore").exists(), ".gitignore missing");
    assert!(proj.join("Justfile").exists(), "Justfile missing");
    assert!(proj.join("README.md").exists(), "README.md missing");

    let toml = std::fs::read_to_string(proj.join("beamtalk.toml")).unwrap();
    assert!(
        toml.contains("name = \"my_lib\""),
        "beamtalk.toml should contain the project name; got:\n{toml}"
    );
}

#[test]
fn new_app_emits_application_supervisor() {
    let dir = tempfile::tempdir().unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "my_app", "--app"])
        .assert()
        .success()
        .stdout(contains("Created application package"));

    let proj = dir.path().join("my_app");
    let toml = std::fs::read_to_string(proj.join("beamtalk.toml")).unwrap();
    assert!(
        toml.contains("[application]"),
        "application package should have [application] section; got:\n{toml}"
    );
    assert!(proj.join("src/main.bt").exists(), "src/main.bt missing");
}

/// BT-3648 (ADR 0129): a freshly generated application prints its greeting
/// under `beamtalk run`. The generated `Main` uses the class-side `Console`
/// facade, so this proves a name resolved as an ordinary class works in a
/// batch-compiled project with no REPL/workspace injection.
#[test]
fn new_app_run_prints_greeting() {
    let dir = tempfile::tempdir().unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "my_app", "--app"])
        .assert()
        .success();

    cli_common::beamtalk_run()
        .current_dir(dir.path().join("my_app"))
        .args(["Main", "run"])
        .timeout(std::time::Duration::from_mins(2))
        .assert()
        .success()
        .stdout(contains("Hello from my_app!"));
}

#[test]
fn new_into_existing_directory_fails() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("existing")).unwrap();

    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "existing"])
        .assert()
        .failure()
        .stderr(contains("already exists"));
}

#[test]
fn new_rejects_invalid_package_name() {
    let dir = tempfile::tempdir().unwrap();
    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "1bad-name"])
        .assert()
        .failure();
}

/// `PascalCase` class name to the `snake_case` file stem the compiler requires.
fn snake_case(class_name: &str) -> String {
    let mut out = String::new();
    for (i, c) in class_name.chars().enumerate() {
        if c.is_uppercase() && i > 0 {
            out.push('_');
        }
        out.extend(c.to_lowercase());
    }
    out
}

/// The name a fenced block declares, if it is a class or protocol definition.
fn declared_name(block: &str) -> Option<String> {
    block.lines().find_map(|line| {
        let rest = line.strip_prefix("Protocol define: ").or_else(|| {
            let (head, rest) = line.split_once(" subclass: ")?;
            // A definition starts at column 0 with a modifier or a base class.
            head.chars().next().filter(|c| c.is_alphabetic())?;
            Some(rest)
        })?;
        let name: String = rest
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        (!name.is_empty()).then_some(name)
    })
}

/// BT-5228: every fenced `beamtalk` block in the generated `AGENTS.md` must compile.
/// Class and protocol blocks become their own source file; statement blocks
/// are wrapped in a class method. `beamtalk check` fails on any error, so a
/// template example that stops compiling fails CI.
#[test]
#[allow(clippy::single_match_else)] // the two arms are the two block kinds; reads better as match
fn agents_md_template_code_blocks_compile() {
    let dir = tempfile::tempdir().unwrap();
    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "tpl_check"])
        .assert()
        .success();
    let proj = dir.path().join("tpl_check");
    for sub in ["src", "test"] {
        for entry in std::fs::read_dir(proj.join(sub)).unwrap() {
            std::fs::remove_file(entry.unwrap().path()).unwrap();
        }
    }

    let agents = std::fs::read_to_string(proj.join("AGENTS.md")).unwrap();
    let mut blocks = Vec::new();
    let mut current: Option<String> = None;
    for line in agents.lines() {
        match (&mut current, line) {
            (None, "```beamtalk") => current = Some(String::new()),
            (Some(_), "```") => blocks.push(current.take().unwrap()),
            (Some(b), l) => {
                b.push_str(l);
                b.push('\n');
            }
            _ => {}
        }
    }
    assert!(
        blocks.len() >= 15,
        "expected the template to carry many beamtalk blocks, found {}",
        blocks.len()
    );

    for (i, block) in blocks.iter().enumerate() {
        let (sub, stem, source) = match declared_name(block) {
            Some(name) => {
                let sub = if block.contains("TestCase subclass:") {
                    "test"
                } else {
                    "src"
                };
                (sub, snake_case(&name), block.clone())
            }
            None => {
                let body: String = block
                    .lines()
                    .map(|l| {
                        if l.is_empty() {
                            "\n".to_string()
                        } else {
                            format!("    {l}\n")
                        }
                    })
                    .collect();
                (
                    "src",
                    format!("snippet{i}"),
                    format!("Object subclass: Snippet{i}\n  class run =>\n{body}"),
                )
            }
        };
        std::fs::write(proj.join(sub).join(format!("{stem}.bt")), source).unwrap();
    }

    cli_common::beamtalk()
        .current_dir(&proj)
        .arg("check")
        .assert()
        .success();
}

/// BT-5228: `beamtalk agents` detects and refreshes a stale generated region
/// and leaves project-specific text outside the markers alone.
#[test]
fn agents_check_and_refresh_keep_local_sections() {
    let dir = tempfile::tempdir().unwrap();
    cli_common::beamtalk()
        .current_dir(dir.path())
        .args(["new", "agents_demo"])
        .assert()
        .success();
    let proj = dir.path().join("agents_demo");

    cli_common::beamtalk()
        .current_dir(&proj)
        .args(["agents", "check"])
        .assert()
        .success();

    let path = proj.join("AGENTS.md");
    let fresh = std::fs::read_to_string(&path).unwrap();
    let stale = fresh.replace("## Build & Run", "## Old Heading");
    let stale = format!("{stale}\n## Local rules\nkeep me\n");
    std::fs::write(&path, &stale).unwrap();

    cli_common::beamtalk()
        .current_dir(&proj)
        .args(["agents", "check"])
        .assert()
        .failure()
        .stdout(contains("AGENTS.md: stale"));

    cli_common::beamtalk()
        .current_dir(&proj)
        .args(["agents", "refresh"])
        .assert()
        .success();
    let refreshed = std::fs::read_to_string(&path).unwrap();
    assert!(refreshed.contains("## Build & Run"));
    assert!(!refreshed.contains("## Old Heading"));
    assert!(refreshed.ends_with("\n## Local rules\nkeep me\n"));

    cli_common::beamtalk()
        .current_dir(&proj)
        .args(["agents", "check"])
        .assert()
        .success();
}

/// BT-5228: the in-repo example packages must track the installed template, so
/// `examples/*/AGENTS.md` cannot drift the way the downstream copies did.
#[test]
fn example_agent_guides_match_the_template() {
    let examples = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../examples");
    let mut checked = 0;
    for entry in std::fs::read_dir(&examples).unwrap() {
        let dir = entry.unwrap().path();
        if !dir.join("AGENTS.md").exists() {
            continue;
        }
        cli_common::beamtalk()
            .current_dir(&dir)
            .args(["agents", "check"])
            .assert()
            .success();
        checked += 1;
    }
    assert!(
        checked >= 7,
        "expected the example packages, found {checked}"
    );
}
