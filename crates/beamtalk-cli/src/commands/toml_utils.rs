// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared TOML-formatting utilities used by multiple CLI commands.

use std::fmt::Write as _;

use camino::Utf8Path;
use miette::{Context, IntoDiagnostic, Result};

/// Check that `content` parses as TOML, then write it to `manifest_path`
/// atomically (stage, fsync, rename).
///
/// Used for programmatic `beamtalk.toml` edits: a generated manifest that does
/// not parse is a bug in the edit, so it is rejected before it can clobber the
/// user's file.
pub(crate) fn write_validated_manifest(manifest_path: &Utf8Path, content: &str) -> Result<()> {
    let _: toml::Value = toml::from_str(content)
        .into_diagnostic()
        .wrap_err("Generated beamtalk.toml is invalid TOML — this is a bug")?;

    // Atomic (BT-5017): a crash mid-write must not leave a truncated manifest.
    crate::commands::util::write_atomic(manifest_path, content, ".beamtalk-manifest.")
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{manifest_path}'"))
}

/// The line ending a document uses: `\r\n` if it contains one, else `\n`.
///
/// Surgical manifest edits keep the file's own convention instead of
/// normalising every line. The rule is shared with `beamtalk fmt`.
pub(crate) use beamtalk_core::source_analysis::line_ending;

/// The table name of a `[name]` header line, or `None` if `line` is not a
/// (single-bracket) table header. A trailing `# comment` is allowed, and
/// whitespace around the name is ignored, matching what TOML accepts.
pub(crate) fn table_header_name(line: &str) -> Option<&str> {
    let rest = line.trim().strip_prefix('[')?;
    if rest.starts_with('[') {
        return None; // array-of-tables
    }
    let (name, after) = rest.split_once(']')?;
    let after = after.trim();
    (after.is_empty() || after.starts_with('#')).then(|| name.trim())
}

/// Whether `line` starts any table or array-of-tables header.
pub(crate) fn is_any_header(line: &str) -> bool {
    line.trim_start().starts_with('[')
}

/// Escape a value for embedding in a TOML basic string.
///
/// TOML basic strings prohibit unescaped control characters (U+0000–U+001F,
/// U+007F) as well as unescaped backslashes and double-quotes. Leaving any
/// of those characters bare either produces invalid TOML or, in the worst
/// case, injects extra key-value pairs into the surrounding document.
pub(crate) fn escape_toml_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 || c == '\x7f' => {
                let _ = write!(out, "\\u{:04X}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// BT-5017: a manifest edit is staged and renamed. An in-place write kept
    /// the file's inode, so a crash mid-write left a truncated `beamtalk.toml`.
    #[cfg(unix)]
    #[test]
    fn write_validated_manifest_replaces_by_rename() {
        use std::os::unix::fs::MetadataExt;
        let dir = tempfile::tempdir().unwrap();
        let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
        std::fs::write(&path, "[package]\nname = \"old\"\n").unwrap();
        let before = std::fs::metadata(&path).unwrap().ino();
        write_validated_manifest(&path, "[package]\nname = \"new\"\n").unwrap();
        assert_ne!(std::fs::metadata(&path).unwrap().ino(), before);
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "[package]\nname = \"new\"\n"
        );
        assert_eq!(std::fs::read_dir(dir.path()).unwrap().count(), 1);
    }

    #[test]
    fn write_validated_manifest_rejects_invalid_toml_and_keeps_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = camino::Utf8PathBuf::from_path_buf(dir.path().join("beamtalk.toml")).unwrap();
        std::fs::write(&path, "[package]\nname = \"old\"\n").unwrap();
        assert!(write_validated_manifest(&path, "not = [valid").is_err());
        assert_eq!(
            std::fs::read_to_string(&path).unwrap(),
            "[package]\nname = \"old\"\n"
        );
    }

    #[test]
    fn test_escape_toml_string_escapes_quotes_and_backslashes() {
        assert_eq!(escape_toml_string(r#"say "hi""#), r#"say \"hi\""#);
        assert_eq!(escape_toml_string(r"a\b"), r"a\\b");
    }

    /// Covers the full TOML-prohibited control-character range (U+0000–U+001F,
    /// U+007F), not just the named short escapes.
    #[test]
    fn test_escape_toml_string_escapes_full_control_character_range() {
        assert_eq!(escape_toml_string("a\nb"), r"a\nb");
        assert_eq!(escape_toml_string("a\rb"), r"a\rb");
        assert_eq!(escape_toml_string("a\tb"), r"a\tb");
        // NUL, VT (U+000B, no short escape), a mid-range control (U+001F),
        // and DEL (U+007F) — all fall back to the `\uXXXX` form.
        assert_eq!(
            escape_toml_string("a\u{0}\u{b}\u{1f}\u{7f}b"),
            "a\\u0000\\u000B\\u001F\\u007Fb"
        );
    }
}
