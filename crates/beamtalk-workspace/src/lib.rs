// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared workspace utilities for beamtalk tools.
//!
//! Provides workspace ID generation and file I/O used by both
//! `beamtalk-cli` and `beamtalk-mcp`.
//!
//! **DDD Context:** CLI / Language Service

use std::fs;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

use std::fmt::Write;

use miette::{IntoDiagnostic, Result, miette};
use sha2::{Digest, Sha256};

/// epmd (Erlang Port Mapper Daemon) client: `NAMES_REQ` TCP protocol,
/// deregistration polling, registration lookup.
pub mod epmd;

/// Lowercase-hex-encode a byte slice.
///
/// The shared leaf under every hash-to-hex-string need in these tools —
/// [`hash_workspace_path_string`] here and the source-content hashing in
/// `beamtalk-cli`'s `commands::util::content_hash_of` both hex-encode a
/// SHA-256 digest and must not each carry their own copy of this formatting
/// loop (see `docs/development/architecture-principles.md` § Duplication &
/// the Shared-Leaf-Module Pattern).
#[must_use]
pub fn hex_encode(bytes: &[u8]) -> String {
    bytes
        .iter()
        .fold(String::with_capacity(bytes.len() * 2), |mut s, b| {
            let _ = write!(s, "{b:02x}");
            s
        })
}

/// SHA256-hash a path string down to a 12-hex-char workspace ID.
///
/// The pure half of [`generate_workspace_id`] — no filesystem access, no
/// `canonicalize()`. Split out so a golden test can pin a fixed
/// input/output pair without depending on a platform-specific canonicalized
/// path (e.g. `std::env::temp_dir()` resolves differently per OS) and
/// without re-implementing the hash in the test itself — see
/// `docs/development/architecture-principles.md` §7.
#[must_use]
pub fn hash_workspace_path_string(path_str: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(path_str.as_bytes());
    let result = hasher.finalize();

    // Use first 12 hex chars (6 bytes) for readability — take first 6 bytes
    hex_encode(&result[..6])
}

/// Generate a workspace ID from a project path.
///
/// Uses SHA256 hash of the canonicalized absolute path (first 12 hex chars).
///
/// # Errors
///
/// Returns an error if:
/// - The path cannot be canonicalized (e.g. does not exist).
/// - The canonicalized path contains non-UTF-8 bytes.
pub fn generate_workspace_id(project_path: &Path) -> Result<String> {
    let absolute = project_path.canonicalize().into_diagnostic()?;
    let path_str = absolute
        .to_str()
        .ok_or_else(|| miette!("Project path contains invalid UTF-8: {:?}", absolute))?;
    Ok(hash_workspace_path_string(path_str))
}

/// Get the root Beamtalk state directory (`~/.beamtalk/`) — the shared leaf
/// under every tool that needs it (workspace storage here, the desktop
/// launcher's log file, ...), so none of them re-derive `dirs::home_dir()`
/// on their own.
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn beamtalk_root_dir() -> Result<PathBuf> {
    beamtalk_home::beamtalk_root_dir().ok_or_else(|| miette!("Could not determine home directory"))
}

/// Resolve the address epmd should bind/contact: `ERL_EPMD_ADDRESS` if the
/// operator has set one (e.g. a trusted private network's interface), else
/// loopback (`127.0.0.1`) — the default posture for every Beamtalk-spawned
/// BEAM node (ADR 0091 Decision 5), so a node that *starts* epmd (there is
/// only one epmd per machine; whoever gets there first sets its bind
/// address for as long as it keeps running) never exposes the port mapper
/// on `0.0.0.0`. Shared by `beamtalk-cli`'s workspace startup
/// (`commands::workspace::startup_command`) and
/// `beamtalk-desktop-broker`'s front spawn (`spawn::build_env`) — both set
/// `ERL_EPMD_ADDRESS` in the child process env from this same resolution,
/// rather than each re-deriving the "does the operator want to override
/// loopback" policy independently. Never returns `"0.0.0.0"` on its own —
/// an operator who explicitly exports that anyway is making their own
/// informed choice, not something this function should second-guess.
#[must_use]
pub fn resolve_epmd_address() -> String {
    std::env::var("ERL_EPMD_ADDRESS").unwrap_or_else(|_| "127.0.0.1".to_string())
}

/// Get the base directory for all workspaces (`~/.beamtalk/workspaces/`).
///
/// # Errors
///
/// Returns an error if the home directory cannot be determined.
pub fn workspaces_base_dir() -> Result<PathBuf> {
    Ok(beamtalk_root_dir()?.join("workspaces"))
}

/// Get the workspace directory for a given workspace ID.
///
/// Validates the workspace ID to prevent path traversal attacks.
///
/// # Errors
///
/// Returns an error if:
/// - The home directory cannot be determined.
/// - The workspace ID contains path traversal components (`..`, `/`, `\`).
/// - The workspace ID is empty.
pub fn workspace_dir(workspace_id: &str) -> Result<PathBuf> {
    if workspace_id.is_empty() {
        return Err(miette!("Workspace ID cannot be empty"));
    }
    if workspace_id.contains("..") || workspace_id.contains('/') || workspace_id.contains('\\') {
        return Err(miette!(
            "Workspace ID contains invalid path components: {:?}",
            workspace_id
        ));
    }
    Ok(workspaces_base_dir()?.join(workspace_id))
}

/// Read the port (and optional nonce) from the port file written by `beamtalk_repl_server`.
///
/// Port file format (BT-611): `PORT\nNONCE` (two lines).
/// Returns `Ok(None)` if the file does not exist.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_port_file(workspace_id: &str) -> Result<Option<(u16, Option<String>)>> {
    let port_file_path = workspace_dir(workspace_id)?.join("port");

    let content = match fs::read_to_string(&port_file_path) {
        Ok(content) => content,
        Err(err) if err.kind() == ErrorKind::NotFound => return Ok(None),
        Err(err) => return Err(err).into_diagnostic(),
    };

    let mut lines = content.lines();
    if let Some(port_line) = lines.next() {
        if let Ok(port) = port_line.trim().parse::<u16>() {
            let nonce = lines
                .next()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty());
            Ok(Some((port, nonce)))
        } else {
            tracing::warn!(
                path = %port_file_path.display(),
                "Invalid port file content in workspace port file"
            );
            Ok(None)
        }
    } else {
        Ok(None)
    }
}

/// Read the cookie file for a workspace.
///
/// Returns `Ok(None)` if the file does not exist or is empty.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_cookie_file(workspace_id: &str) -> Result<Option<String>> {
    let cookie_path = workspace_dir(workspace_id)?.join("cookie");

    let content = match fs::read_to_string(&cookie_path) {
        Ok(content) => content,
        Err(err) if err.kind() == ErrorKind::NotFound => return Ok(None),
        Err(err) => return Err(err).into_diagnostic(),
    };
    let trimmed = content.trim();
    if trimmed.is_empty() {
        Ok(None)
    } else {
        Ok(Some(trimmed.to_string()))
    }
}

/// Parse the REPL port from `beamtalk repl` stdout.
///
/// Expects a line like: `Connected to REPL backend on port 12345.`
pub fn parse_repl_port(stdout: &str) -> Option<u16> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("Connected to REPL backend on port ")
            .and_then(|rest| rest.trim_end_matches('.').trim().parse().ok())
    })
}

/// Parse the workspace ID from `beamtalk repl` stdout.
///
/// Expects a line like: `  Workspace: abc123def456 (new)`
pub fn parse_repl_workspace_id(stdout: &str) -> Option<String> {
    stdout.lines().find_map(|line| {
        line.strip_prefix("  Workspace: ")
            .and_then(|rest| rest.split_whitespace().next())
            .map(std::string::ToString::to_string)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // Guards every test below that reads/mutates `ERL_EPMD_ADDRESS` — Rust
    // runs tests in this binary concurrently by default, and
    // `std::env::set_var`/`remove_var` racing a concurrent `env::var` read
    // in another thread is a real hazard, not a theoretical one (mirrors
    // `beamtalk-desktop-broker::test_support::ENV_LOCK`'s same discipline).
    static EPMD_ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[test]
    fn resolve_epmd_address_defaults_to_loopback() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        assert_eq!(resolve_epmd_address(), "127.0.0.1");
    }

    #[test]
    fn resolve_epmd_address_respects_an_operator_override() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::set_var("ERL_EPMD_ADDRESS", "10.0.0.5") };
        let result = resolve_epmd_address();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_EPMD_ADDRESS") };
        assert_eq!(result, "10.0.0.5");
    }

    #[test]
    fn workspaces_base_dir_is_workspaces_under_beamtalk_root_dir() {
        assert_eq!(
            workspaces_base_dir().unwrap(),
            beamtalk_root_dir().unwrap().join("workspaces")
        );
    }

    #[test]
    fn test_generate_workspace_id_length() {
        // Use the current directory (which must exist)
        let cwd = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&cwd).unwrap();
        assert_eq!(
            id.len(),
            12,
            "Workspace ID should be 12 hex chars (6 bytes)"
        );
    }

    #[test]
    fn test_generate_workspace_id_hex_format() {
        let cwd = std::env::current_dir().unwrap();
        let id = generate_workspace_id(&cwd).unwrap();
        assert!(
            id.chars().all(|c| c.is_ascii_hexdigit()),
            "Workspace ID should be all hex digits, got: {id}"
        );
    }

    #[test]
    fn test_generate_workspace_id_deterministic() {
        let cwd = std::env::current_dir().unwrap();
        let id1 = generate_workspace_id(&cwd).unwrap();
        let id2 = generate_workspace_id(&cwd).unwrap();
        assert_eq!(id1, id2, "Same path should produce the same workspace ID");
    }

    #[test]
    fn test_generate_workspace_id_different_paths() {
        let temp = std::env::temp_dir();
        let cwd = std::env::current_dir().unwrap();
        // Only run if temp and cwd differ (they almost always do)
        if temp != cwd {
            let id1 = generate_workspace_id(&temp).unwrap();
            let id2 = generate_workspace_id(&cwd).unwrap();
            assert_ne!(id1, id2, "Different paths should produce different IDs");
        }
    }

    #[test]
    fn test_generate_workspace_id_rejects_nonexistent_path() {
        let result = generate_workspace_id(Path::new("/nonexistent/path/that/does/not/exist"));
        assert!(result.is_err(), "Non-existent path should produce an error");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn test_generate_workspace_id_rejects_non_utf8_path() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        let tmp = std::env::temp_dir();
        let mut invalid_bytes = b"beamtalk-workspace-test-\xff\xfe-".to_vec();
        invalid_bytes.extend_from_slice(std::process::id().to_string().as_bytes());
        let invalid_name = OsStr::from_bytes(&invalid_bytes);
        let non_utf8_path = tmp.join(invalid_name);

        if let Err(e) = std::fs::create_dir(&non_utf8_path) {
            if e.kind() != std::io::ErrorKind::AlreadyExists {
                eprintln!("skipping test: failed to create non-UTF8 dir {non_utf8_path:?}: {e}");
                return;
            }
        }

        let result = generate_workspace_id(&non_utf8_path);
        let _ = std::fs::remove_dir(&non_utf8_path);

        assert!(result.is_err(), "Non-UTF-8 path should produce an error");
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("invalid UTF-8"),
            "Error should mention invalid UTF-8, got: {err}"
        );
    }

    #[test]
    fn test_workspace_dir_rejects_path_traversal() {
        assert!(workspace_dir("../etc").is_err());
        assert!(workspace_dir("..").is_err());
        assert!(workspace_dir("foo/bar").is_err());
        assert!(workspace_dir("foo\\bar").is_err());
        assert!(workspace_dir("").is_err());
    }

    #[test]
    fn test_workspace_dir_accepts_valid_ids() {
        let dir = workspace_dir("abc123def456");
        assert!(dir.is_ok());
        assert!(dir.unwrap().ends_with("abc123def456"));
    }

    #[test]
    fn test_read_port_file_missing_workspace() {
        let result = read_port_file("nonexistent_workspace_common_abc123").unwrap();
        assert_eq!(result, None, "Missing workspace should return None");
    }

    #[test]
    fn test_read_port_file_valid() {
        let workspace_id = format!("test_wsc_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "9876\nnonce123").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, Some((9876, Some("nonce123".to_string()))));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_no_nonce() {
        let workspace_id = format!("test_wsc_nononce_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "8765\n").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, Some((8765, None)));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_port_file_invalid_content() {
        let workspace_id = format!("test_wsc_invalid_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("port"), "not_a_number\n").unwrap();

        let result = read_port_file(&workspace_id).unwrap();
        assert_eq!(result, None, "Invalid port file should return None");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_cookie_file_missing() {
        let result = read_cookie_file("nonexistent_workspace_common_xyz").unwrap();
        assert_eq!(result, None, "Missing cookie file should return None");
    }

    #[test]
    fn test_read_cookie_file_valid() {
        let workspace_id = format!("test_wsc_cookie_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("cookie"), "mysecretcookie\n").unwrap();

        let result = read_cookie_file(&workspace_id).unwrap();
        assert_eq!(result, Some("mysecretcookie".to_string()));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_read_cookie_file_empty() {
        let workspace_id = format!("test_wsc_empty_cookie_{}", std::process::id());
        let dir = workspaces_base_dir().unwrap().join(&workspace_id);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("cookie"), "  \n  ").unwrap();

        let result = read_cookie_file(&workspace_id).unwrap();
        assert_eq!(result, None, "Empty cookie file should return None");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_parse_repl_port_typical() {
        let stdout = "Welcome to beamtalk REPL\n  Workspace: abc123def456 (new)\nConnected to REPL backend on port 9876.\n";
        assert_eq!(parse_repl_port(stdout), Some(9876));
    }

    #[test]
    fn test_parse_repl_port_missing() {
        assert_eq!(parse_repl_port("some other output\n"), None);
        assert_eq!(parse_repl_port(""), None);
    }

    #[test]
    fn test_parse_repl_port_malformed() {
        assert_eq!(
            parse_repl_port("Connected to REPL backend on port notanumber.\n"),
            None
        );
    }

    #[test]
    fn test_parse_repl_workspace_id_typical() {
        let stdout = "  Workspace: abc123def456 (new)\nConnected to REPL backend on port 9876.\n";
        assert_eq!(
            parse_repl_workspace_id(stdout),
            Some("abc123def456".to_string())
        );
    }

    #[test]
    fn test_parse_repl_workspace_id_missing() {
        assert_eq!(parse_repl_workspace_id("no workspace line\n"), None);
        assert_eq!(parse_repl_workspace_id(""), None);
    }

    #[test]
    fn test_parse_repl_workspace_id_bare() {
        assert_eq!(
            parse_repl_workspace_id("  Workspace: deadbeef1234\n"),
            Some("deadbeef1234".to_string())
        );
    }

    #[test]
    fn test_parse_repl_workspace_id_empty_prefix() {
        assert_eq!(parse_repl_workspace_id("  Workspace: \n"), None);
        assert_eq!(parse_repl_workspace_id("  Workspace: "), None);
    }
}
