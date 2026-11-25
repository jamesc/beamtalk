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

/// Cross-platform process-liveness primitives: `is_process_alive` and
/// `proc_start_time`. Shared leaf for `beamtalk-cli` and
/// `beamtalk-desktop-broker` (see module doc).
pub mod pid_liveness;

/// The one process-cwd guard for tests (`with_cwd`, `CwdGuard`), shared by
/// `beamtalk-cli`, `beamtalk-core` and `beamtalk-package`. Test-only: it
/// mutates the process cwd and panics on failure, so production code must not
/// use it (hidden from the docs for that reason; a cargo feature cannot gate it
/// without every consumer enabling it, and `cargo_workspace_root!` sets the
/// precedent for test helpers living in this leaf crate).
#[doc(hidden)]
pub mod test_cwd;

/// Test helper asserting a secret is absent from a process's argv (BT-5126).
/// Test-only for the same reason as [`test_cwd`].
#[doc(hidden)]
pub mod test_argv;

/// Returns the Cargo workspace root for the calling crate: two levels up from
/// [`CARGO_MANIFEST_DIR`](std::env::var), which cargo sets to the manifest
/// directory of the crate being compiled or tested.
///
/// This is a `macro_rules!` macro (not a function) because
/// `env!("CARGO_MANIFEST_DIR")` is a compile-time expansion that resolves to
/// the **calling crate**'s manifest directory — a library function would
/// capture `beamtalk-workspace`'s own directory instead.
///
/// The shared leaf under every "locate the workspace root in a test/bench
/// helper" need in these tools — `beamtalk-cli`, `beamtalk-core`,
/// `beamtalk-language-service`, and `beamtalk-parity-tests` all need the
/// identical `CARGO_MANIFEST_DIR/../..` walk and must not each carry their own
/// copy (see `docs/development/architecture-principles.md` § Duplication &
/// the Shared-Leaf-Module Pattern).
///
/// # Panics
///
/// Panics if the path does not have at least two ancestor directories (i.e.,
/// the crate is not nested two levels below the workspace root).
#[macro_export]
macro_rules! cargo_workspace_root {
    () => {
        ::std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .expect("workspace root is two levels above CARGO_MANIFEST_DIR")
            .to_path_buf()
    };
}

/// Default workspace idle timeout in seconds (4 hours).
pub const DEFAULT_IDLE_TIMEOUT_SECONDS: u64 = 3600 * 4;

/// Environment variable that overrides [`DEFAULT_IDLE_TIMEOUT_SECONDS`].
pub const IDLE_TIMEOUT_ENV_VAR: &str = "BEAMTALK_WORKSPACE_TIMEOUT";

/// The workspace idle timeout in seconds: `BEAMTALK_WORKSPACE_TIMEOUT` when it
/// parses as a `u64`, else [`DEFAULT_IDLE_TIMEOUT_SECONDS`].
///
/// The one reader of the variable, shared by the CLI and the MCP server so the
/// default and the parse rule cannot drift apart.
#[must_use]
pub fn idle_timeout_seconds_from_env() -> u64 {
    std::env::var(IDLE_TIMEOUT_ENV_VAR)
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(DEFAULT_IDLE_TIMEOUT_SECONDS)
}

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

/// Recursively copy the directory tree at `src` into `dst` (created if
/// absent; existing files are overwritten).
///
/// The shared leaf under every "copy a checked-in fixture project into a
/// tempdir" need in the test helpers of `beamtalk-cli`,
/// `beamtalk-compiler-port` and `beamtalk-parity-tests`, which must not each
/// carry their own copy (see `docs/development/architecture-principles.md`
/// § Duplication & the Shared-Leaf-Module Pattern). Symlinks are followed,
/// which suits source fixtures; a tree whose symlinks must survive (an ERTS
/// copy) needs `beamtalk-cli`'s release-assembly copy instead.
///
/// # Errors
///
/// Returns the first I/O error from reading `src` or writing `dst`.
pub fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Resolve `<name>` (or `<name>.exe` on Windows) as a sibling `[[bin]]`
/// artifact next to the calling test binary.
///
/// The calling test binary's own `current_exe()` lives at
/// `<profile-dir>/deps/<test-binary>`, so its grandparent is the same
/// `<profile-dir>` cargo places sibling `[[bin]]` outputs into — including
/// under `cargo llvm-cov`, which builds everything into
/// `target/llvm-cov-target/<profile>/` rather than plain
/// `target/<profile>/`. A hardcoded `target/debug/<name>` path resolves to
/// a stale, uninstrumented binary in that case: a subprocess-spawning e2e
/// test runs against it happily while contributing zero coverage signal
/// for that binary's own crate.
///
/// Falls back to walking up from the current directory looking for
/// `target/debug/<name>`, for callers whose `current_exe()` doesn't follow
/// the usual `<profile-dir>/deps/<test-binary>` shape.
///
/// The shared leaf under every "find a sibling workspace binary from a test
/// binary" need in these tools: `beamtalk-parity-tests`' CLI/LSP/MCP e2e
/// drivers, `beamtalk-mcp`'s REPL-spawning integration tests, and
/// `beamtalk-cli`'s own subprocess-spawning integration tests must not each
/// carry their own copy of this resolution logic (see
/// `docs/development/architecture-principles.md` § Duplication & the
/// Shared-Leaf-Module Pattern).
///
/// # Errors
///
/// Returns an error message if `<name>` isn't found next to the test
/// binary or anywhere in `target/debug` walking up from the current
/// directory.
pub fn resolve_sibling_binary(name: &str) -> std::result::Result<PathBuf, String> {
    let suffix = std::env::consts::EXE_SUFFIX;
    let file = format!("{name}{suffix}");

    if let Ok(exe) = std::env::current_exe()
        && let Some(profile_dir) = exe.ancestors().nth(2)
    {
        let candidate = profile_dir.join(&file);
        if candidate.exists() {
            return Ok(candidate);
        }
    }

    if let Ok(mut cwd) = std::env::current_dir() {
        loop {
            let candidate = cwd.join("target/debug").join(&file);
            if candidate.exists() {
                return Ok(candidate);
            }
            if !cwd.pop() {
                break;
            }
        }
    }

    Err(format!(
        "binary `{name}` not found next to the test binary or in target/debug; run `just build` first"
    ))
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
    // Deliberately the raw `std` canonicalize, NOT `beamtalk_core::paths::canonicalize`
    // (BT-4914): the result is hashed into the workspace ID, so on Windows the
    // `\\?\`-prefixed form is part of every existing ID. Simplifying it here would
    // orphan all existing workspaces. This crate also sits below `beamtalk-core`.
    // Path *comparisons* elsewhere re-canonicalize both sides through the shared
    // helper, so they never depend on this form.
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
    beamtalk_env::beamtalk_root_dir().ok_or_else(|| miette!("Could not determine home directory"))
}

/// Resolve the address epmd should bind/contact: `ERL_EPMD_ADDRESS` if the
/// operator has set one (e.g. a trusted private network's interface), else
/// loopback (`127.0.0.1`) — the default posture for every Beamtalk-spawned
/// BEAM node (ADR 0091 Decision 5), so a node that *starts* epmd (there is
/// only one epmd per machine; whoever gets there first sets its bind
/// address for as long as it keeps running) never exposes the port mapper
/// on `0.0.0.0`. Shared by every `erl` spawn in `beamtalk-cli` (via
/// [`pin_epmd_loopback`] / [`erl_command`]) and `beamtalk-desktop-broker`'s
/// front spawn (`spawn::build_env`) — both set `ERL_EPMD_ADDRESS` in the
/// child process env from this same resolution, rather than each
/// re-deriving the "does the operator want to override loopback" policy
/// independently.
///
/// An empty or whitespace-only value counts as unset: epmd reads an empty
/// address list as "every interface", so passing it through would produce
/// exactly the wildcard epmd this rule exists to prevent (BT-3987). Never
/// returns `"0.0.0.0"` on its own — an operator who explicitly exports that
/// anyway is making their own informed choice, not something this function
/// should second-guess.
#[must_use]
pub fn resolve_epmd_address() -> String {
    std::env::var(EPMD_ADDRESS_VAR)
        .ok()
        .filter(|v| !v.trim().is_empty())
        .unwrap_or_else(|| DEFAULT_EPMD_ADDRESS.to_string())
}

/// The environment variable `erl`/`epmd` read for epmd's bind address.
pub const EPMD_ADDRESS_VAR: &str = "ERL_EPMD_ADDRESS";

/// Loopback: the address epmd binds when the operator hasn't chosen one.
pub const DEFAULT_EPMD_ADDRESS: &str = "127.0.0.1";

/// Apply the [`resolve_epmd_address`] rule to an `erl` command (BT-3987).
///
/// A distributed node (`-sname`/`-name`, which can also arrive via
/// `ERL_FLAGS`/`ERL_AFLAGS`/`ERL_ZFLAGS` or an `-args_file`) runs
/// `epmd -daemon` if none is running, and that daemon binds every interface
/// unless `ERL_EPMD_ADDRESS` is set, keeping that posture until killed. A
/// node that never starts distribution ignores the variable, so
/// `beamtalk-cli` pins it on **every** `erl` it spawns rather than trying to
/// tell named spawns from unnamed ones; its `erl_spawns_pin_epmd_loopback`
/// lint fails on a spawn that skips this helper. Call it *after* any
/// `cmd.env_clear()`, which would otherwise wipe the pin.
///
/// The value the child sees is decided in this order:
///
/// 1. The command already sets a non-empty `ERL_EPMD_ADDRESS` (`cmd.env(..)`):
///    left untouched, that caller's value wins.
/// 2. The command sets it empty, or removes it (`cmd.env_remove(..)`): the
///    caller has said "no operator value here", so loopback.
/// 3. Otherwise [`resolve_epmd_address`]: the operator's `ERL_EPMD_ADDRESS`
///    from this process's environment, else loopback. This also holds after
///    `cmd.env_clear()`, so an env-allowlisted spawn still passes an
///    operator-set value through.
pub fn pin_epmd_loopback(cmd: &mut std::process::Command) {
    let explicit = cmd
        .get_envs()
        .find(|(k, _)| *k == std::ffi::OsStr::new(EPMD_ADDRESS_VAR))
        .map(|(_, v)| v.is_some_and(|v| !v.to_string_lossy().trim().is_empty()));
    match explicit {
        Some(true) => {}
        Some(false) => {
            cmd.env(EPMD_ADDRESS_VAR, DEFAULT_EPMD_ADDRESS);
        }
        None => {
            cmd.env(EPMD_ADDRESS_VAR, resolve_epmd_address());
        }
    }
}

/// `erl` flags that bind this node's Erlang distribution listener to
/// loopback (BT-5067; ADR 0125 / ADR 0126 state loopback as the posture).
///
/// [`pin_epmd_loopback`] only governs epmd; without this the node's own
/// distribution port listens on every interface, protected by the cookie
/// alone. Mirrors the release `vm.args` line. Returns no flags only when the
/// operator's `ERL_EPMD_ADDRESS` names a non-loopback address
/// ([`loopback_dist_for`]): pinning the listener to loopback would silently
/// defeat that private-network choice.
///
/// The pinned address is IPv4 loopback, or IPv6 loopback when the node's
/// distribution runs over IPv6 (`-proto_dist inet6_tcp`/`inet6_tls` in
/// `ERL_FLAGS`, `ERL_AFLAGS` or `ERL_ZFLAGS`), since an IPv4 interface cannot
/// bind an IPv6 listener.
#[must_use]
pub fn loopback_dist_args() -> Vec<&'static str> {
    if loopback_dist_for(std::env::var(EPMD_ADDRESS_VAR).ok().as_deref()) {
        let ipv6 = ["ERL_FLAGS", "ERL_AFLAGS", "ERL_ZFLAGS"]
            .iter()
            .filter_map(|var| std::env::var(var).ok())
            .any(|flags| flags.contains("inet6_"));
        let interface = if ipv6 {
            "{0,0,0,0,0,0,0,1}"
        } else {
            "{127,0,0,1}"
        };
        vec!["-kernel", "inet_dist_use_interface", interface]
    } else {
        Vec::new()
    }
}

/// Whether the distribution listener stays pinned to loopback, given the
/// operator's `ERL_EPMD_ADDRESS` (BT-5127, BT-5185).
///
/// The value is epmd's comma-separated address list. Each entry is trimmed
/// and blank entries are skipped. The pin is dropped only when some entry
/// parses as an IP address that is not loopback (strictly: an IPv6 `%zone`
/// suffix is ignored and an IPv4-mapped address counts as its IPv4 address);
/// unset, blank, loopback
/// (`127.0.0.0/8`, `::1`, `localhost`) and unparseable values all pin, so a
/// typo fails closed rather than opening every interface. Pinned to
/// `tests/conformance/loopback_dist_conformance.json`, which the Elixir front's
/// `BtAttach.Workspace.loopback_dist?/1` reads too.
#[must_use]
pub fn loopback_dist_for(epmd_address: Option<&str>) -> bool {
    !epmd_address.unwrap_or("").split(',').any(|entry| {
        let entry = entry.trim();
        let address = entry
            .split_once('%')
            .map_or(entry, |(address, _zone)| address);
        address
            .parse::<std::net::IpAddr>()
            .is_ok_and(|ip| !ip.to_canonical().is_loopback())
    })
}

/// `Command::new("erl")` with [`pin_epmd_loopback`] already applied: the
/// constructor for an `erl` spawn that doesn't clear its environment.
#[must_use]
pub fn erl_command() -> std::process::Command {
    let mut cmd = std::process::Command::new("erl");
    pin_epmd_loopback(&mut cmd);
    cmd
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
    workspace_dir_in(&workspaces_base_dir()?, workspace_id)
}

/// [`workspace_dir`] under an explicit `workspaces_root` (the real
/// `~/.beamtalk/workspaces` or a temporary directory in tests).
///
/// # Errors
///
/// Returns an error if the workspace ID is empty or contains path traversal
/// components (`..`, `/`, `\`).
pub fn workspace_dir_in(workspaces_root: &Path, workspace_id: &str) -> Result<PathBuf> {
    if workspace_id.is_empty() {
        return Err(miette!("Workspace ID cannot be empty"));
    }
    if workspace_id.contains("..") || workspace_id.contains('/') || workspace_id.contains('\\') {
        return Err(miette!(
            "Workspace ID contains invalid path components: {:?}",
            workspace_id
        ));
    }
    Ok(workspaces_root.join(workspace_id))
}

/// Read the port (and optional nonce) from the port file written by `beamtalk_repl_server`.
///
/// Port file format: `PORT\nNONCE` (two lines).
/// Returns `Ok(None)` if the file does not exist.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_port_file(workspace_id: &str) -> Result<Option<(u16, Option<String>)>> {
    read_port_file_under(&workspaces_base_dir()?, workspace_id)
}

/// [`read_port_file`] under an explicit `workspaces_root`.
///
/// # Errors
///
/// Returns an error if the ID is invalid or the file exists but cannot be read.
pub fn read_port_file_under(
    workspaces_root: &Path,
    workspace_id: &str,
) -> Result<Option<(u16, Option<String>)>> {
    let port_file_path = workspace_dir_in(workspaces_root, workspace_id)?.join("port");

    let content = match fs::read_to_string(&port_file_path) {
        Ok(content) => content,
        Err(err) if err.kind() == ErrorKind::NotFound => return Ok(None),
        Err(err) => return Err(err).into_diagnostic(),
    };

    if let Some(parsed) = parse_port_file(&content) {
        Ok(Some(parsed))
    } else {
        // An empty file is "not written yet", not corruption: stay quiet.
        if !content.trim().is_empty() {
            tracing::warn!(
                path = %port_file_path.display(),
                "Invalid port file content in workspace port file"
            );
        }
        Ok(None)
    }
}

/// Parse the contents of a port file: `PORT\nNONCE` (the nonce line is optional).
///
/// The single parser for the port-file format; [`read_port_file`] and
/// [`read_port_file_nonce_in`] both use it. Strict (BT-5112): the port must be
/// on the first line, as ASCII digits only (no sign, no whitespace), in
/// 1..=65535. Returns `None` otherwise. The nonce line is trimmed; an empty one
/// is no nonce. Pinned to `tests/conformance/port_file_conformance.json`.
fn parse_port_file(content: &str) -> Option<(u16, Option<String>)> {
    let mut lines = content.lines();
    let first = lines.next()?;
    if first.is_empty() || !first.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let port = first.parse::<u16>().ok().filter(|p| *p != 0)?;
    let nonce = lines
        .next()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    Some((port, nonce))
}

/// Find the nonce of the workspace whose port file under `workspaces_root`
/// names `port`.
///
/// Scans every immediate subdirectory of `workspaces_root` (O(N)); callers
/// that know the workspace ID should use [`read_port_file`] instead.
/// `workspaces_root` is a parameter so callers can pass the real
/// `~/.beamtalk/workspaces` directory or a temporary directory in tests.
///
/// Returns `None` if no matching port file is found, it has no nonce line,
/// or on any I/O error.
#[must_use]
pub fn read_port_file_nonce_in(workspaces_root: &Path, port: u16) -> Option<String> {
    fs::read_dir(workspaces_root)
        .ok()?
        .flatten()
        .filter(|entry| entry.path().is_dir())
        .filter_map(|entry| fs::read_to_string(entry.path().join("port")).ok())
        .filter_map(|content| parse_port_file(&content))
        .find(|(file_port, _)| *file_port == port)
        .and_then(|(_, nonce)| nonce)
}

/// Read the cookie file for a workspace.
///
/// Returns `Ok(None)` if the file does not exist or is empty.
///
/// # Errors
///
/// Returns an error if the file exists but cannot be read.
pub fn read_cookie_file(workspace_id: &str) -> Result<Option<String>> {
    read_cookie_file_under(&workspaces_base_dir()?, workspace_id)
}

/// [`read_cookie_file`] under an explicit `workspaces_root`.
///
/// # Errors
///
/// Returns an error if the ID is invalid or the file exists but cannot be read.
pub fn read_cookie_file_under(
    workspaces_root: &Path,
    workspace_id: &str,
) -> Result<Option<String>> {
    let cookie_path = workspace_dir_in(workspaces_root, workspace_id)?.join("cookie");

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

    /// Write a port file (`PORT\nNONCE\n`) into `workspaces_root/ws_id/port`.
    fn write_port_file(workspaces_root: &Path, ws_id: &str, port: u16, nonce: Option<&str>) {
        let ws_dir = workspaces_root.join(ws_id);
        fs::create_dir_all(&ws_dir).unwrap();
        let content = match nonce {
            Some(n) => format!("{port}\n{n}\n"),
            None => format!("{port}\n"),
        };
        fs::write(ws_dir.join("port"), content).unwrap();
    }

    #[test]
    fn parse_port_file_reads_port_and_optional_nonce() {
        assert_eq!(
            parse_port_file("4040\nabc\n"),
            Some((4040, Some("abc".to_string())))
        );
        assert_eq!(parse_port_file("4040\n"), Some((4040, None)));
        assert_eq!(parse_port_file("4040\n\n"), Some((4040, None)));
        assert_eq!(
            parse_port_file("4040\n  abc \n"),
            Some((4040, Some("abc".to_string())))
        );
        assert_eq!(parse_port_file("  4040  \n  abc \n"), None);
        assert_eq!(parse_port_file("not-a-port\nabc\n"), None);
        assert_eq!(parse_port_file(""), None);
    }

    /// BT-5185: an IPv6 distribution is pinned to IPv6 loopback; an IPv4
    /// interface could not bind its listener.
    #[test]
    fn loopback_dist_args_pin_ipv6_distribution_to_ipv6_loopback() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe {
            std::env::set_var(EPMD_ADDRESS_VAR, "::1");
            std::env::set_var("ERL_AFLAGS", "-proto_dist inet6_tcp");
        }
        let ipv6 = loopback_dist_args();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var("ERL_AFLAGS") };
        let ipv4 = loopback_dist_args();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var(EPMD_ADDRESS_VAR) };
        assert_eq!(ipv6.last(), Some(&"{0,0,0,0,0,0,0,1}"));
        assert_eq!(ipv4.last(), Some(&"{127,0,0,1}"));
    }

    /// BT-5185: the loopback decision is the shared fixture's table, read
    /// through the environment exactly as a spawn reads it. Shared with the
    /// Elixir front (`BtAttach.Workspace.loopback_dist?/1`).
    #[test]
    fn loopback_dist_args_match_the_shared_conformance_fixture() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../tests/conformance/loopback_dist_conformance.json");
        let raw = fs::read_to_string(path).unwrap();
        let fixture: serde_json::Value = serde_json::from_str(&raw).unwrap();
        let cases = fixture["cases"].as_array().unwrap();
        assert!(!cases.is_empty());
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        let mut wrong = Vec::new();
        for case in cases {
            let name = case["name"].as_str().unwrap();
            let pin = case["pin"].as_bool().unwrap();
            // SAFETY: guarded by EPMD_ENV_LOCK above.
            unsafe {
                match case["value"].as_str() {
                    Some(v) => std::env::set_var(EPMD_ADDRESS_VAR, v),
                    None => std::env::remove_var(EPMD_ADDRESS_VAR),
                }
            }
            if loopback_dist_args().is_empty() == pin {
                wrong.push(name.to_string());
            }
        }
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var(EPMD_ADDRESS_VAR) };
        assert!(wrong.is_empty(), "wrong decision for: {wrong:?}");
    }

    /// Shared with the VS Code extension (`parsePortFileContent` in
    /// `editors/vscode/src/workspaceClient.ts`): both read the same fixture.
    #[test]
    fn parse_port_file_matches_the_shared_conformance_fixture() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../tests/conformance/port_file_conformance.json");
        let raw = fs::read_to_string(path).unwrap();
        let fixture: serde_json::Value = serde_json::from_str(&raw).unwrap();
        let cases = fixture["cases"].as_array().unwrap();
        assert!(!cases.is_empty());
        for case in cases {
            let name = case["name"].as_str().unwrap();
            let expected = case["port"].as_u64().map(|port| {
                (
                    u16::try_from(port).unwrap(),
                    case["nonce"].as_str().map(str::to_string),
                )
            });
            assert_eq!(
                parse_port_file(case["content"].as_str().unwrap()),
                expected,
                "{name}"
            );
        }
    }

    #[test]
    fn read_port_file_nonce_in_finds_nonce_for_port() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 12345, Some("abc123def456"));
        assert_eq!(
            read_port_file_nonce_in(tmp.path(), 12345),
            Some("abc123def456".to_string())
        );
    }

    #[test]
    fn read_port_file_nonce_in_wrong_port_is_none() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 23456, Some("nonce_value"));
        assert_eq!(read_port_file_nonce_in(tmp.path(), 54321), None);
    }

    #[test]
    fn read_port_file_nonce_in_missing_nonce_line_is_none() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 11111, None);
        assert_eq!(read_port_file_nonce_in(tmp.path(), 11111), None);
    }

    #[test]
    fn read_port_file_nonce_in_picks_matching_workspace() {
        let tmp = tempfile::TempDir::new().unwrap();
        write_port_file(tmp.path(), "ws1", 10001, Some("nonce_a"));
        write_port_file(tmp.path(), "ws2", 10002, Some("nonce_b"));
        write_port_file(tmp.path(), "ws3", 10003, Some("nonce_c"));
        assert_eq!(
            read_port_file_nonce_in(tmp.path(), 10002),
            Some("nonce_b".to_string())
        );
        assert_eq!(read_port_file_nonce_in(tmp.path(), 10099), None);
    }

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

    /// The `ERL_EPMD_ADDRESS` value `cmd` will hand its child, if it sets one.
    fn epmd_env(cmd: &std::process::Command) -> Option<String> {
        cmd.get_envs()
            .find(|(k, _)| *k == std::ffi::OsStr::new(EPMD_ADDRESS_VAR))
            .and_then(|(_, v)| v)
            .map(|v| v.to_string_lossy().into_owned())
    }

    #[test]
    fn pin_epmd_loopback_defaults_to_loopback_when_unset() {
        // Remove the variable in the *command's* env, not this process's, so
        // the default path runs even on a host whose shell exports
        // ERL_EPMD_ADDRESS globally.
        let mut cmd = std::process::Command::new("erl");
        cmd.env_remove(EPMD_ADDRESS_VAR);
        pin_epmd_loopback(&mut cmd);
        assert_eq!(epmd_env(&cmd).as_deref(), Some("127.0.0.1"));
    }

    #[test]
    fn pin_epmd_loopback_passes_through_an_operator_set_value() {
        let mut cmd = std::process::Command::new("erl");
        cmd.env(EPMD_ADDRESS_VAR, "10.0.0.5");
        pin_epmd_loopback(&mut cmd);
        assert_eq!(epmd_env(&cmd).as_deref(), Some("10.0.0.5"));
    }

    #[test]
    fn pin_epmd_loopback_treats_an_empty_command_value_as_unset() {
        let mut cmd = std::process::Command::new("erl");
        cmd.env(EPMD_ADDRESS_VAR, " ");
        pin_epmd_loopback(&mut cmd);
        assert_eq!(epmd_env(&cmd).as_deref(), Some("127.0.0.1"));
    }

    #[test]
    fn pin_epmd_loopback_passes_the_operator_value_through_env_clear() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::set_var(EPMD_ADDRESS_VAR, "10.0.0.5") };
        let mut operator_set = std::process::Command::new("erl");
        operator_set.env_clear();
        pin_epmd_loopback(&mut operator_set);
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var(EPMD_ADDRESS_VAR) };
        let mut unset = std::process::Command::new("erl");
        unset.env_clear();
        pin_epmd_loopback(&mut unset);
        assert_eq!(epmd_env(&operator_set).as_deref(), Some("10.0.0.5"));
        assert_eq!(epmd_env(&unset).as_deref(), Some("127.0.0.1"));
    }

    #[test]
    fn resolve_epmd_address_treats_an_empty_value_as_unset() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::set_var(EPMD_ADDRESS_VAR, "") };
        let result = resolve_epmd_address();
        // SAFETY: guarded by EPMD_ENV_LOCK above.
        unsafe { std::env::remove_var(EPMD_ADDRESS_VAR) };
        assert_eq!(result, "127.0.0.1");
    }

    #[test]
    fn erl_command_is_pinned() {
        let _guard = EPMD_ENV_LOCK.lock().unwrap();
        let cmd = erl_command();
        assert_eq!(cmd.get_program(), "erl");
        assert!(epmd_env(&cmd).is_some());
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

        if let Err(e) = std::fs::create_dir(&non_utf8_path)
            && e.kind() != std::io::ErrorKind::AlreadyExists
        {
            eprintln!("skipping test: failed to create non-UTF8 dir {non_utf8_path:?}: {e}");
            return;
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
