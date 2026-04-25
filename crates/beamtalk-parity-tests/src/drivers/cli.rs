// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! CLI surface driver.
//!
//! Runs the real `beamtalk` binary against per-case temp projects. Avoids the
//! `assert_cmd` crate to keep the dependency footprint small — `Command` plus
//! a few helpers is enough for the operations we exercise (`build`, `test`,
//! `lint`, `check`).

use std::path::Path;
use std::process::{Command, Stdio};

use crate::drivers::SurfaceOutput;
use crate::pool::beamtalk_binary;

/// Run `beamtalk build <path>` and report the diagnostic count.
///
/// `beamtalk build` is intentionally terse on success ("Compiling N files"),
/// so the class set is recovered by scanning the source tree for top-level
/// `Foo subclass: Bar` headers. This matches what the REPL and MCP surfaces
/// would observe — both ultimately call into the same compiler, so any class
/// present in the source has gone through the build path.
pub fn build_project(path: &Path) -> Result<SurfaceOutput, String> {
    let bin = beamtalk_binary("beamtalk")?;
    let output = Command::new(&bin)
        .args(["build"])
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("spawn beamtalk build: {e}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let combined = format!("{stdout}\n{stderr}");
    let classes = if output.status.success() {
        scan_classes(path)
    } else {
        std::collections::BTreeSet::new()
    };
    let diagnostic_count = if output.status.success() {
        0
    } else {
        count_diagnostic_lines(&combined).max(1)
    };
    Ok(SurfaceOutput {
        classes: Some(classes),
        diagnostic_count: Some(diagnostic_count),
        raw: combined,
        ..SurfaceOutput::default()
    })
}

/// Scan a project tree for class declarations.
///
/// The expected forms are:
///
/// ```text
/// Value subclass: Foo
/// Actor subclass: Bar
/// TestCase subclass: BazTest
/// Protocol define: Bazable
/// ```
///
/// `Protocol define:` is included so the CLI scanner agrees with REPL
/// and MCP load-project responses, which list registered protocols
/// alongside concrete classes (BT-1950).
fn scan_classes(root: &Path) -> std::collections::BTreeSet<String> {
    let mut out = std::collections::BTreeSet::new();
    visit_bt_files(root, &mut |file| {
        let Ok(text) = std::fs::read_to_string(file) else {
            return;
        };
        for line in text.lines() {
            let t = line.trim();
            // Match `<Anything> subclass: <Name>` at the start of a line.
            if let Some(idx) = t.find(" subclass: ") {
                let name = &t[idx + " subclass: ".len()..];
                if let Some(first) = name.split_whitespace().next() {
                    if first.chars().next().is_some_and(char::is_uppercase) {
                        out.insert(first.to_string());
                    }
                }
            }
            // Match `Protocol define: <Name>` so cross-file protocols
            // (BT-1950) are visible in the CLI's class set.
            if let Some(rest) = t.strip_prefix("Protocol define: ") {
                if let Some(first) = rest.split_whitespace().next() {
                    let bare = first.split('(').next().unwrap_or(first);
                    if bare.chars().next().is_some_and(char::is_uppercase) {
                        out.insert(bare.to_string());
                    }
                }
            }
        }
    });
    out
}

fn visit_bt_files(root: &Path, visit: &mut impl FnMut(&Path)) {
    if root.is_file() {
        if root.extension().is_some_and(|e| e == "bt") {
            visit(root);
        }
        return;
    }
    let Ok(entries) = std::fs::read_dir(root) else {
        return;
    };
    for entry in entries.flatten() {
        let p = entry.path();
        // Skip generated `_build` and any `.git` dirs.
        if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
            if name == "_build" || name == ".git" || name == "build" {
                continue;
            }
        }
        if p.is_dir() {
            visit_bt_files(&p, visit);
        } else if p.extension().is_some_and(|e| e == "bt") {
            visit(&p);
        }
    }
}

/// Run `beamtalk lint <path>` and count the resulting diagnostics.
pub fn lint(path: &Path) -> Result<SurfaceOutput, String> {
    let bin = beamtalk_binary("beamtalk")?;
    let output = Command::new(&bin)
        .args(["lint"])
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("spawn beamtalk lint: {e}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let combined = format!("{stdout}\n{stderr}");
    let count = count_diagnostic_lines(&combined);
    Ok(SurfaceOutput {
        diagnostic_count: Some(count),
        raw: combined,
        ..SurfaceOutput::default()
    })
}

/// Run `beamtalk build <path>` and count compile diagnostics. Used for the
/// parity diagnostic case because plain `lint` misses parse errors.
pub fn diagnose(path: &Path) -> Result<SurfaceOutput, String> {
    let bin = beamtalk_binary("beamtalk")?;
    let output = Command::new(&bin)
        .args(["build"])
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("spawn beamtalk build: {e}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let combined = format!("{stdout}\n{stderr}");
    // miette renders the leading line `× <message>` for each diagnostic.
    let count = combined
        .lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("× ") || t.starts_with("error:") || t.starts_with("Error:")
        })
        .count();
    let final_count = if output.status.success() {
        0
    } else {
        count.max(1)
    };
    Ok(SurfaceOutput {
        diagnostic_count: Some(final_count),
        raw: combined,
        ..SurfaceOutput::default()
    })
}

/// Run `beamtalk test <path>` against a project.
pub fn test_project(path: &Path) -> Result<SurfaceOutput, String> {
    let bin = beamtalk_binary("beamtalk")?;
    let output = Command::new(&bin)
        .args(["test", "--quiet"])
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("spawn beamtalk test: {e}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    let combined = format!("{stdout}\n{stderr}");
    let value = if output.status.success() {
        "pass".to_string()
    } else {
        "fail".to_string()
    };
    Ok(SurfaceOutput {
        value: Some(value),
        raw: combined,
        ..SurfaceOutput::default()
    })
}

fn count_diagnostic_lines(text: &str) -> usize {
    text.lines()
        .filter(|l| {
            let t = l.trim_start();
            t.starts_with("warning")
                || t.starts_with("error")
                || t.contains(": warning:")
                || t.contains(": error:")
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_lines_counts_classic_formats() {
        let text = "warning: redundant\nerror: bad\nfoo.bt:1:1: warning: x\nok\n";
        assert_eq!(count_diagnostic_lines(text), 3);
    }

    #[test]
    fn scan_classes_picks_up_subclasses_and_protocols() {
        let dir = std::env::temp_dir().join("beamtalk-parity-cli-scan-test");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("a.bt"),
            "Value subclass: Foo\nProtocol define: Barable\n",
        )
        .unwrap();
        std::fs::write(dir.join("b.bt"), "Actor subclass: Baz\n").unwrap();
        let classes = scan_classes(&dir);
        assert!(classes.contains("Foo"));
        assert!(classes.contains("Baz"));
        assert!(classes.contains("Barable"));
        let _ = std::fs::remove_dir_all(&dir);
    }
}
