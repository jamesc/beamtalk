// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `beamtalk agents` — keep the generated part of a project's `AGENTS.md` and
//! `.github/copilot-instructions.md` in step with the installed toolchain.
//!
//! `beamtalk new` writes both files from templates. A project copy taken at one
//! toolchain version otherwise drifts as the template learns new features
//! (BT-5228), so each template wraps its generated text in
//! `<!-- beamtalk:template begin -->` / `<!-- beamtalk:template end -->`
//! markers. Text outside the markers belongs to the project and is never
//! touched:
//!
//! - `beamtalk agents refresh` rewrites the region between the markers.
//! - `beamtalk agents check` exits non-zero when a region is stale, for CI.
//!
//! **DDD Context:** CLI (project scaffolding).

use std::fs;

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result, bail};

use crate::commands::new::{render_agents_md, render_copilot_instructions};
use crate::commands::util::find_project_root;

/// Opens the generated region of a templated file.
pub(crate) const BEGIN_MARKER: &str = "<!-- beamtalk:template begin -->";
/// Closes the generated region of a templated file.
pub(crate) const END_MARKER: &str = "<!-- beamtalk:template end -->";

/// `beamtalk agents` subcommands.
#[derive(Debug, clap::Subcommand)]
pub enum AgentsCommand {
    /// Rewrite the generated region of AGENTS.md and
    /// .github/copilot-instructions.md from the installed template
    Refresh,
    /// Exit non-zero if either generated region differs from the installed template
    Check,
}

/// Byte range of the text between the markers, markers included.
fn region(content: &str) -> Option<(usize, usize)> {
    let start = content.find(BEGIN_MARKER)?;
    let end = start + content[start..].find(END_MARKER)? + END_MARKER.len();
    Some((start, end))
}

/// Result of comparing one file with the template.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Status {
    /// The generated region matches the template.
    Current,
    /// The generated region differs; carries the file with the region replaced.
    Stale(String),
    /// The file has no (or half a) marker pair, so there is nothing to replace.
    NoMarkers,
}

/// Compare `existing` with `rendered` (a freshly rendered template) and, when the
/// generated regions differ, produce the refreshed file. Text outside the markers
/// is carried over unchanged.
pub(crate) fn compare(existing: &str, rendered: &str) -> Status {
    let (Some((s, e)), Some((rs, re))) = (region(existing), region(rendered)) else {
        return Status::NoMarkers;
    };
    // A CRLF checkout (e.g. Windows `autocrlf`) must not read as stale.
    if existing[s..e].replace("\r\n", "\n") == rendered[rs..re] {
        Status::Current
    } else {
        Status::Stale(format!(
            "{}{}{}",
            &existing[..s],
            &rendered[rs..re],
            &existing[e..]
        ))
    }
}

struct Target {
    rel: &'static str,
    render: fn(&str) -> String,
}

const TARGETS: [Target; 2] = [
    Target {
        rel: "AGENTS.md",
        render: render_agents_md,
    },
    Target {
        rel: ".github/copilot-instructions.md",
        render: render_copilot_instructions,
    },
];

/// Run `beamtalk agents <action>` for the package containing the current directory.
pub fn run(action: &AgentsCommand) -> Result<()> {
    let root = find_project_root()?;
    let manifest = beamtalk_package::manifest::parse_manifest_full(&root.join("beamtalk.toml"))?;
    let refresh = matches!(*action, AgentsCommand::Refresh);
    let stale = process(&root, &manifest.package.name, refresh)?;
    if stale > 0 && !refresh {
        bail!(
            "{stale} file(s) are out of date with this toolchain's template; run `beamtalk agents refresh`"
        );
    }
    Ok(())
}

/// Returns the number of stale files (refreshed ones count when `refresh`).
fn process(root: &Utf8Path, name: &str, refresh: bool) -> Result<usize> {
    // Validate every file before writing any, so a refresh is all or nothing.
    let mut pending = Vec::new();
    for target in &TARGETS {
        let path = root.join(target.rel);
        if !path.exists() {
            println!("{}: missing (skipped)", target.rel);
            continue;
        }
        let existing = fs::read_to_string(&path)
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read {path}"))?;
        match compare(&existing, &(target.render)(name)) {
            Status::Current => println!("{}: current", target.rel),
            Status::NoMarkers => bail!(
                "{} has no `{BEGIN_MARKER}` / `{END_MARKER}` markers. Move any project-specific \
                 notes aside, delete the file and run `beamtalk new` in a scratch directory to \
                 copy the marked template, then add your notes back outside the markers.",
                target.rel
            ),
            Status::Stale(updated) => pending.push((target.rel, path, updated)),
        }
    }
    for (rel, path, updated) in &pending {
        if refresh {
            fs::write(path, updated)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to write {path}"))?;
            println!("{rel}: refreshed");
        } else {
            println!("{rel}: stale");
        }
    }
    Ok(pending.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fresh_render_is_current() {
        let r = render_agents_md("demo");
        assert_eq!(compare(&r, &r), Status::Current);
    }

    #[test]
    fn stale_region_is_replaced_and_local_text_kept() {
        let rendered = render_agents_md("demo");
        let (s, e) = region(&rendered).unwrap();
        let old = format!(
            "# demo\n\nLocal intro.\n\n{BEGIN_MARKER}\nold text\n{END_MARKER}\n\n## Local rules\nkeep me\n"
        );
        let Status::Stale(new) = compare(&old, &rendered) else {
            panic!("expected stale");
        };
        assert!(new.contains(&rendered[s..e]));
        assert!(new.starts_with("# demo\n\nLocal intro.\n\n"));
        assert!(new.ends_with("\n\n## Local rules\nkeep me\n"));
        assert!(!new.contains("old text"));
        assert_eq!(compare(&new, &rendered), Status::Current);
    }

    #[test]
    fn crlf_checkout_is_current() {
        let r = render_agents_md("demo");
        assert_eq!(compare(&r.replace('\n', "\r\n"), &r), Status::Current);
    }

    #[test]
    fn unmarked_file_is_reported() {
        let rendered = render_agents_md("demo");
        assert_eq!(compare("# demo\n", &rendered), Status::NoMarkers);
        let half = format!("{BEGIN_MARKER}\nno end");
        assert_eq!(compare(&half, &rendered), Status::NoMarkers);
    }

    #[test]
    fn both_templates_carry_one_marker_pair() {
        for r in [
            render_agents_md("demo"),
            render_copilot_instructions("demo"),
        ] {
            assert_eq!(r.matches(BEGIN_MARKER).count(), 1);
            assert_eq!(r.matches(END_MARKER).count(), 1);
        }
    }
}
