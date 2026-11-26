// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared leaf: absolutizing a filesystem path without resolving symlinks.
//!
//! **DDD Context:** Infrastructure (shared leaf, no domain logic)
//!
//! Both [`crate::ffi_type_specs::find_runtime_dir_with_layout`] (this crate)
//! and `beamtalk-cli`'s release assembly (`release::assembly::absolutize`)
//! need to turn a possibly-relative path into an absolute one *without*
//! canonicalizing it — canonicalizing resolves symlinks, which breaks a
//! consumer that compares the result against another string built the same
//! way (BT-3619's `RELEASE_DIR` literal-prefix match) or that spawns a
//! program whose path contains a `/` from a different working directory
//! (BT-3624: `Command::current_dir` plus a relative program path resolves
//! the program *after* the chdir, not before).

use camino::{Utf8Path, Utf8PathBuf};
use std::borrow::Cow;
use std::path::{Path, PathBuf};

/// Make `path` absolute by joining it onto the current directory when it
/// isn't already, then lexically normalize the result the way Erlang's own
/// `filename:join/1` normalizes paths: `.` segments, repeated separators and
/// a trailing separator are dropped, `..` segments are kept verbatim
/// (resolving them lexically would be wrong across a symlink).
/// `Path::components()` already implements exactly these rules.
///
/// Deliberately does **not** canonicalize — see the module doc comment for
/// why that distinction matters to callers.
///
/// # Errors
///
/// Returns an error if `path` is relative and the current directory cannot
/// be read.
pub fn absolutize(path: &Path) -> std::io::Result<PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    Ok(absolute.components().collect())
}

/// Windows `MAX_PATH`, including the terminating NUL. A verbatim path at or
/// beyond this length may only be addressable in its `\\?\` form.
const MAX_PATH: usize = 260;

/// Device names Windows reserves in every directory, with or without an
/// extension (`NUL.txt` is still the NUL device). Compared case-insensitively
/// against the part of a component before its first `.`.
fn is_reserved_device_name(component: &str) -> bool {
    let stem = component
        .split('.')
        .next()
        .unwrap_or("")
        .trim_end_matches(' ');
    let upper = stem.to_ascii_uppercase();
    if matches!(
        upper.as_str(),
        "CON" | "PRN" | "AUX" | "NUL" | "CONIN$" | "CONOUT$"
    ) {
        return true;
    }
    for prefix in ["COM", "LPT"] {
        if let Some(suffix) = upper.strip_prefix(prefix) {
            let mut chars = suffix.chars();
            if let (Some(c), None) = (chars.next(), chars.next())
                && (c.is_ascii_digit() || matches!(c, '\u{b9}' | '\u{b2}' | '\u{b3}'))
            {
                return true;
            }
        }
    }
    false
}

/// Whether `component` is safe to present without the `\\?\` prefix, i.e.
/// the Win32 path parser would read it back as the same name.
fn is_plain_component(component: &str) -> bool {
    !component.is_empty()
        && component != "."
        && component != ".."
        && !component.ends_with('.')
        && !component.ends_with(' ')
        && !component
            .chars()
            .any(|c| c.is_control() || matches!(c, '<' | '>' | ':' | '"' | '/' | '|' | '?' | '*'))
        && !is_reserved_device_name(component)
}

/// Strip the Windows verbatim (`\\?\`) prefix from `path` when — and only
/// when — the plain form names the same file.
///
/// This is the same rule set as the `dunce` crate (which the workspace does
/// not depend on): `std::fs::canonicalize` on Windows always returns the
/// `\\?\` form, which is correct for the OS but leaks into diagnostics,
/// recorded module source files and printed paths.
///
/// * `\\?\C:\x` becomes `C:\x`.
/// * `\\?\UNC\server\share\x` becomes `\\server\share\x`.
/// * Anything else is returned unchanged: already-plain paths, other verbatim
///   forms (`\\?\Volume{…}\`, `\\?\pipe\`), and paths that only work
///   verbatim — `MAX_PATH` or longer, a reserved device name (`CON`, `NUL.txt`,
///   `COM1`…), a component with a trailing dot or space, a `.`/`..` or empty
///   component, a `/`, or a character Win32 rejects (`<>:"|?*`, controls).
///
/// Pure string function with no platform dependence, so it is unit-tested on
/// every platform. Use [`simplified`] / [`canonicalize`] for `Path` values.
#[must_use]
pub fn simplify_verbatim(path: &str) -> Cow<'_, str> {
    if path.encode_utf16().count() >= MAX_PATH {
        return Cow::Borrowed(path);
    }
    if let Some(rest) = path.strip_prefix(r"\\?\UNC\") {
        // `server\share[\…]`: both must be present and non-empty.
        let mut parts = rest.split('\\');
        let (Some(server), Some(share)) = (parts.next(), parts.next()) else {
            return Cow::Borrowed(path);
        };
        let ok = is_plain_component(server)
            && is_plain_component(share)
            && trailing_components_plain(parts);
        return if ok {
            Cow::Owned(format!(r"\\{rest}"))
        } else {
            Cow::Borrowed(path)
        };
    }
    if let Some(rest) = path.strip_prefix(r"\\?\") {
        let bytes = rest.as_bytes();
        let drive_ok = bytes.len() >= 3 && bytes[0].is_ascii_alphabetic() && &bytes[1..3] == b":\\";
        if drive_ok && trailing_components_plain(rest[3..].split('\\')) {
            return Cow::Borrowed(rest);
        }
    }
    Cow::Borrowed(path)
}

/// Check the components after the drive/share. A single trailing separator
/// yields one empty final component, which is allowed.
fn trailing_components_plain<'a>(components: impl Iterator<Item = &'a str>) -> bool {
    let comps: Vec<&str> = components.collect();
    let last = comps.len().saturating_sub(1);
    comps
        .iter()
        .enumerate()
        .all(|(i, c)| (i == last && c.is_empty()) || is_plain_component(c))
}

/// [`simplify_verbatim`] for a [`Path`]. A no-op off Windows, where `\\?\` is
/// an ordinary (if odd) file name and must never be rewritten. Non-UTF-8
/// Windows paths are returned unchanged.
#[must_use]
pub fn simplified(path: &Path) -> PathBuf {
    #[cfg(windows)]
    if let Some(s) = path.to_str() {
        let plain = simplify_verbatim(s);
        if plain != s {
            return PathBuf::from(plain.into_owned());
        }
    }
    path.to_path_buf()
}

/// `std::fs::canonicalize` that never returns a `\\?\` path where a plain
/// one is equivalent. Behaves exactly like `std::fs::canonicalize` off Windows.
///
/// Every site that canonicalises a path feeding a diagnostic, a recorded
/// source file, a cache/dedup key or printed output must use this, **and so
/// must every path it is later compared with** — mixing forms makes
/// `starts_with`/`==` silently miss on Windows.
///
/// # Errors
///
/// Whatever `std::fs::canonicalize` returns.
pub fn canonicalize(path: impl AsRef<Path>) -> std::io::Result<PathBuf> {
    std::fs::canonicalize(path).map(|p| simplified(&p))
}

/// [`canonicalize`] for UTF-8 paths.
///
/// # Errors
///
/// An I/O error, or `InvalidData` when the canonical path is not UTF-8.
pub fn canonicalize_utf8(path: impl AsRef<Utf8Path>) -> std::io::Result<Utf8PathBuf> {
    let canonical = canonicalize(path.as_ref().as_std_path())?;
    Utf8PathBuf::from_path_buf(canonical).map_err(|p| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("canonical path is not valid UTF-8: {}", p.display()),
        )
    })
}

/// `canonicalize`, extended to paths that do not exist yet: the longest
/// existing prefix is canonicalized and the missing tail applied to it one
/// component at a time, re-resolving every component that exists (a `..` can
/// lead back into existing directories and their symlinks). Paths come back in
/// the [`canonicalize`] spelling (no `\\?\` prefix). `None` when no prefix
/// resolves.
#[must_use]
pub fn canonicalize_lenient(path: &Path) -> Option<PathBuf> {
    use std::path::Component;
    let components: Vec<Component<'_>> = path.components().collect();
    for split in (1..=components.len()).rev() {
        let prefix: PathBuf = components[..split].iter().collect();
        let Ok(mut resolved) = canonicalize(&prefix) else {
            continue;
        };
        for component in &components[split..] {
            match component {
                Component::Normal(name) => {
                    resolved.push(name);
                    // A `..` can lead the tail back into existing directories,
                    // whose symlinks must be resolved too.
                    if let Ok(real) = canonicalize(&resolved) {
                        resolved = real;
                    }
                }
                Component::ParentDir => {
                    resolved.pop();
                }
                Component::CurDir => {}
                Component::Prefix(_) | Component::RootDir => return None,
            }
        }
        return Some(resolved);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- simplify_verbatim: pure, platform-independent, Windows-form inputs ----

    fn simp(s: &str) -> String {
        simplify_verbatim(s).into_owned()
    }

    #[test]
    fn simplify_strips_drive_prefix() {
        assert_eq!(simp(r"\\?\C:\x"), r"C:\x");
        assert_eq!(
            simp(r"\\?\C:\Users\runneradmin\AppData\Local\Temp\.tmpNvhjtD\src\DnuActor.bt"),
            r"C:\Users\runneradmin\AppData\Local\Temp\.tmpNvhjtD\src\DnuActor.bt"
        );
        assert_eq!(simp(r"\\?\d:\"), r"d:\");
        assert_eq!(simp(r"\\?\C:\dir\"), r"C:\dir\");
    }

    #[test]
    fn simplify_rewrites_unc_prefix() {
        assert_eq!(simp(r"\\?\UNC\server\share"), r"\\server\share");
        assert_eq!(
            simp(r"\\?\UNC\server\share\a\b.bt"),
            r"\\server\share\a\b.bt"
        );
    }

    #[test]
    fn simplify_leaves_plain_paths_unchanged() {
        for p in [
            r"C:\x\y.bt",
            r"\\server\share\x",
            "/usr/local/x.bt",
            "src/a.bt",
            "",
        ] {
            assert_eq!(simp(p), p);
            assert!(matches!(simplify_verbatim(p), Cow::Borrowed(_)));
        }
    }

    #[test]
    fn simplify_is_idempotent() {
        for p in [r"\\?\C:\x", r"\\?\UNC\s\h\x", r"C:\x", r"\\?\C:\a\NUL"] {
            let once = simp(p);
            assert_eq!(simp(&once), once);
        }
    }

    #[test]
    fn simplify_keeps_paths_that_only_work_verbatim() {
        let long = format!(r"\\?\C:\{}", "a".repeat(260));
        let just_short = format!(r"\\?\C:\{}", "a".repeat(250));
        assert_eq!(simp(&long), long, "too long for MAX_PATH");
        assert_eq!(simp(&just_short), format!(r"C:\{}", "a".repeat(250)));
        for p in [
            r"\\?\C:\dir\NUL",
            r"\\?\C:\dir\nul.txt",
            r"\\?\C:\dir\COM1",
            r"\\?\C:\dir\lpt9.bt",
            r"\\?\C:\dir\CON",
            r"\\?\C:\dir\aux.tar.gz",
            r"\\?\C:\dir\file.",
            r"\\?\C:\dir\file ",
            r"\\?\C:\dir\..\x",
            r"\\?\C:\dir\.\x",
            r"\\?\C:\dir\\x",
            r"\\?\C:\dir/x",
            r"\\?\C:\dir\a:b",
            r"\\?\C:\dir\a*b",
            r"\\?\C:\dir\a?b",
            r"\\?\C:\dir\a<b",
            "\\\\?\\C:\\dir\\a\tb",
            r"\\?\UNC\server\share\NUL",
            r"\\?\UNC\server",
            r"\\?\UNC\\share",
            r"\\?\C:",
            r"\\?\C",
            r"\\?\Volume{01234567-89ab-cdef-0123-456789abcdef}\x",
            r"\\?\pipe\name",
            r"\\?\1:\x",
        ] {
            assert_eq!(simp(p), p, "must stay verbatim: {p}");
        }
    }

    #[test]
    fn simplify_allows_look_alike_names_that_are_not_reserved() {
        assert_eq!(
            simp(r"\\?\C:\console\comma\com10\nullable.bt"),
            r"C:\console\comma\com10\nullable.bt"
        );
    }

    #[test]
    fn simplify_counts_length_in_utf16_units() {
        // 130 astral chars = 260 UTF-16 units, but only 130 chars / 520 bytes.
        let long = format!(r"\\?\C:\{}", "\u{1F600}".repeat(130));
        assert_eq!(simp(&long), long);
    }

    #[test]
    fn simplified_is_identity_off_windows() {
        // `\\?\` is a legal (if bizarre) file name on Unix: never rewrite it.
        #[cfg(not(windows))]
        assert_eq!(
            simplified(Path::new(r"\\?\C:\x")),
            PathBuf::from(r"\\?\C:\x")
        );
        assert_eq!(simplified(Path::new("a/b")), PathBuf::from("a/b"));
    }

    #[test]
    fn canonicalize_matches_std_off_windows_and_resolves_dots() {
        let tmp = tempfile::TempDir::new().unwrap();
        let sub = tmp.path().join("a");
        std::fs::create_dir(&sub).unwrap();
        let via_dots = sub.join("..").join("a");
        let got = canonicalize(&via_dots).unwrap();
        #[cfg(not(windows))]
        assert_eq!(got, std::fs::canonicalize(&sub).unwrap());
        assert!(got.is_absolute());
        assert!(canonicalize(tmp.path().join("missing")).is_err());
    }

    #[test]
    fn canonicalize_utf8_round_trips() {
        let tmp = tempfile::TempDir::new().unwrap();
        let p = Utf8PathBuf::from_path_buf(tmp.path().to_path_buf()).unwrap();
        let got = canonicalize_utf8(&p).unwrap();
        assert_eq!(got.as_std_path(), canonicalize(tmp.path()).unwrap());
    }

    /// Real `std::fs::canonicalize` output on Windows is `\\?\`-prefixed; the
    /// helper must hand back the plain form, and plain-form comparisons must
    /// hold between the two entry points.
    #[cfg(windows)]
    #[test]
    fn windows_canonicalize_has_no_verbatim_prefix() {
        let tmp = tempfile::TempDir::new().unwrap();
        let raw = std::fs::canonicalize(tmp.path()).unwrap();
        assert!(
            raw.to_string_lossy().starts_with(r"\\?\"),
            "std precondition: {raw:?}"
        );
        let got = canonicalize(tmp.path()).unwrap();
        assert!(!got.to_string_lossy().starts_with(r"\\?\"), "got {got:?}");
        assert_eq!(got, simplified(&raw));
        // Plain form still names the same directory.
        assert_eq!(std::fs::canonicalize(&got).unwrap(), raw);
        // A file inside stays under the root in the plain form.
        let file = tmp.path().join("DnuActor.bt");
        std::fs::write(&file, "x").unwrap();
        let f = canonicalize(&file).unwrap();
        assert!(f.starts_with(&got));
        assert!(!f.to_string_lossy().contains(r"\\?\"));
    }

    #[cfg(windows)]
    #[test]
    fn windows_canonicalize_keeps_reserved_name_verbatim() {
        // A file literally called `NUL.txt` can only be created via the
        // verbatim form; the helper must not strip it into the NUL device.
        let tmp = tempfile::TempDir::new().unwrap();
        let verbatim = std::fs::canonicalize(tmp.path()).unwrap().join("NUL.txt");
        if std::fs::write(&verbatim, "x").is_ok() {
            let got = canonicalize(&verbatim).unwrap();
            assert!(got.to_string_lossy().starts_with(r"\\?\"), "got {got:?}");
        }
    }

    #[test]
    fn absolutize_drops_cur_dir_segments_from_a_relative_path() {
        let cwd = std::env::current_dir().unwrap();
        let abs = absolutize(Path::new("./_build/release/app-0.1.0")).unwrap();
        assert_eq!(abs, cwd.join("_build").join("release").join("app-0.1.0"));
    }

    #[test]
    fn absolutize_normalizes_an_absolute_path_like_erlang_filename_join() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path();
        let messy = PathBuf::from(format!("{}/./a//b/./", root.display()));
        assert_eq!(absolutize(&messy).unwrap(), root.join("a").join("b"));
    }

    #[test]
    fn absolutize_keeps_parent_dir_segments() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path();
        let with_parent = root.join("a").join("..").join("b");
        assert_eq!(absolutize(&with_parent).unwrap(), with_parent);
    }

    #[test]
    fn absolutize_leaves_an_already_clean_absolute_path_untouched() {
        let tmp = tempfile::TempDir::new().unwrap();
        let root = tmp.path().join("a").join("b");
        assert_eq!(absolutize(&root).unwrap(), root);
    }
}
