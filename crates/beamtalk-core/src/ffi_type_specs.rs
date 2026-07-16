// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Beamtalk runtime discovery and Erlang FFI type-spec extraction.
//!
//! **DDD Context:** Compilation — Runtime Discovery & Native Type Extraction
//!
//! Locates the compiled `beamtalk_runtime` OTP release (dev checkout or
//! installed layout) and, from it, extracts `-spec` attributes from OTP and
//! dependency `.beam` files into a [`NativeTypeRegistry`](crate::semantic_analysis::type_checker::NativeTypeRegistry)
//! via a `beamtalk_build_worker` BEAM node (ADR 0075).
//!
//! BT-2859: This lives in `beamtalk-core` — not `beamtalk-cli` — specifically
//! so `beamtalk-lsp` can call the same single source of truth `beamtalk
//! build`/`beamtalk lint` (via `beamtalk-cli`) and the MCP `lint`/
//! `diagnostic_summary` tools (BT-2858, via `beamtalk-cli`'s
//! `native_type_specs` module, now a thin wrapper around this one) already
//! share, without a `beamtalk-lsp -> beamtalk-cli` dependency (forbidden —
//! see `docs/development/architecture-principles.md`, and the identical
//! rationale for `beamtalk.toml` `[diagnostics]` table parsing living here,
//! ADR 0100 Rule 3 / BT-2800).
//!
//! [`extract_type_specs`] is deliberately layout-agnostic — it takes an
//! explicit cache directory and dependency ebin directories rather than a
//! `beamtalk-cli`-specific `BuildLayout`, so each caller (the CLI via
//! `BuildLayout`, the LSP via its own `_build/` path joins) supplies its own
//! notion of where those directories live.

use crate::semantic_analysis::type_checker::{
    NativeTypeRegistry, is_specs_line, is_specs_result_error, is_specs_result_ok, parse_specs_line,
};
use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result, miette};
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::SystemTime;
use tracing::{debug, info, instrument, warn};

/// OTP kernel logger configuration that redirects the default handler to stderr.
///
/// Used as `-kernel logger <this>` VM arg across REPL, workspace, and compilation
/// nodes (BT-1431). Ensures early boot OTP logger events go to stderr instead of
/// stdout, preventing them from being lost (detached nodes) or mixed into protocol
/// output (REPL/compilation nodes).
pub const KERNEL_LOGGER_STDERR: &str =
    "[{handler, default, logger_std_h, #{config => #{type => standard_error}}}]";

/// Whether the runtime was found in a development checkout or an installed layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeLayout {
    /// Development: `runtime/_build/default/lib/<app>/ebin/`
    Dev,
    /// Installed: `PREFIX/lib/beamtalk/lib/<app>/ebin/`
    Installed,
}

/// Directories that must be on the BEAM code path (`-pa`) for the REPL to work.
#[derive(Debug, Clone)]
pub struct BeamPaths {
    /// Path to the `beamtalk_runtime` application's `ebin/` directory.
    pub runtime_ebin: PathBuf,
    /// Path to the `beamtalk_workspace` application's `ebin/` directory.
    pub workspace_ebin: PathBuf,
    /// Path to the `beamtalk_compiler` application's `ebin/` directory.
    pub compiler_ebin: PathBuf,
    /// Path to the `beamtalk_stdlib` application's `ebin/` directory.
    /// In Dev layout this is `apps/beamtalk_stdlib/ebin/` (Beamtalk-compiled class beams).
    pub stdlib_ebin: PathBuf,
    /// Path to the rebar3-compiled Erlang modules for `beamtalk_stdlib`.
    /// In Dev layout this is `_build/default/lib/beamtalk_stdlib/ebin/`
    /// (contains `beamtalk_json.beam`, `beamtalk_regex.beam`, etc.).
    /// In Installed layout this equals `stdlib_ebin`.
    pub stdlib_erlang_ebin: PathBuf,
    /// Path to the `cowboy` HTTP server's `ebin/` directory (ADR 0020).
    pub cowboy_ebin: PathBuf,
    /// Path to the `cowlib` library's `ebin/` directory (cowboy dependency).
    pub cowlib_ebin: PathBuf,
    /// Path to the `ranch` acceptor pool's `ebin/` directory (cowboy dependency).
    pub ranch_ebin: PathBuf,
    /// Path to the `telemetry` event bus's `ebin/` directory (ADR 0069).
    pub telemetry_ebin: PathBuf,
    /// Path to the `telemetry_poller` periodic VM stats `ebin/` directory (ADR 0069).
    pub telemetry_poller_ebin: PathBuf,
}

/// Compute the standard `-pa` directories from a runtime root.
///
/// For development layout (`runtime/` dir), paths are under `_build/default/lib/`.
/// For installed layout (`PREFIX/lib/beamtalk/`), paths are under `lib/`.
pub fn beam_paths(runtime_dir: &Path) -> BeamPaths {
    beam_paths_for_layout(runtime_dir, RuntimeLayout::Dev)
}

/// Compute `-pa` directories for a specific layout.
pub fn beam_paths_for_layout(runtime_dir: &Path, layout: RuntimeLayout) -> BeamPaths {
    match layout {
        RuntimeLayout::Dev => {
            let build_lib_dir = runtime_dir.join("_build/default/lib");
            BeamPaths {
                runtime_ebin: build_lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: build_lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: build_lib_dir.join("beamtalk_compiler/ebin"),
                cowboy_ebin: build_lib_dir.join("cowboy/ebin"),
                cowlib_ebin: build_lib_dir.join("cowlib/ebin"),
                ranch_ebin: build_lib_dir.join("ranch/ebin"),
                telemetry_ebin: build_lib_dir.join("telemetry/ebin"),
                telemetry_poller_ebin: build_lib_dir.join("telemetry_poller/ebin"),
                // Stdlib class beams are produced by `beamtalk build-stdlib` under apps/, not _build/
                stdlib_ebin: runtime_dir.join("apps/beamtalk_stdlib/ebin"),
                // Stdlib Erlang module beams are rebar3-compiled to _build/
                stdlib_erlang_ebin: build_lib_dir.join("beamtalk_stdlib/ebin"),
            }
        }
        RuntimeLayout::Installed => {
            let lib_dir = runtime_dir.join("lib");
            BeamPaths {
                runtime_ebin: lib_dir.join("beamtalk_runtime/ebin"),
                workspace_ebin: lib_dir.join("beamtalk_workspace/ebin"),
                compiler_ebin: lib_dir.join("beamtalk_compiler/ebin"),
                cowboy_ebin: lib_dir.join("cowboy/ebin"),
                cowlib_ebin: lib_dir.join("cowlib/ebin"),
                ranch_ebin: lib_dir.join("ranch/ebin"),
                telemetry_ebin: lib_dir.join("telemetry/ebin"),
                telemetry_poller_ebin: lib_dir.join("telemetry_poller/ebin"),
                stdlib_ebin: lib_dir.join("beamtalk_stdlib/ebin"),
                // In installed layout, Erlang modules and class beams are in the same dir
                stdlib_erlang_ebin: lib_dir.join("beamtalk_stdlib/ebin"),
            }
        }
    }
}
// ── Runtime Discovery ──────────────────────────────────────────────

/// Find the runtime directory by checking multiple locations.
///
/// Delegates to [`find_runtime_dir_with_layout`]; see that function for
/// layout detection and resolution order.
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir() -> Result<PathBuf> {
    let (path, _layout) = find_runtime_dir_with_layout()?;
    Ok(path)
}

/// Like [`find_runtime_dir`] but also returns the detected [`RuntimeLayout`].
///
/// # Errors
///
/// Returns an error if no valid runtime directory is found, or if
/// `BEAMTALK_RUNTIME_DIR` is set but doesn't contain a valid runtime.
pub fn find_runtime_dir_with_layout() -> Result<(PathBuf, RuntimeLayout)> {
    // Check explicit env var first
    if let Ok(dir) = std::env::var("BEAMTALK_RUNTIME_DIR") {
        let path = PathBuf::from(dir);
        if path.join("rebar.config").exists() {
            return Ok((path, RuntimeLayout::Dev));
        }
        return Err(miette!(
            "BEAMTALK_RUNTIME_DIR is set but does not contain a valid runtime (no rebar.config)"
        ));
    }

    // Dev-mode candidates (checked via rebar.config)
    let dev_candidates = [
        // 1. CARGO_MANIFEST_DIR (when running via cargo run)
        std::env::var("CARGO_MANIFEST_DIR")
            .ok()
            .map(|d| PathBuf::from(d).join("../../runtime")),
        // 2. Current working directory (running from repo root)
        Some(PathBuf::from("runtime")),
        // 3. Executable's grandparent (target/debug/beamtalk -> repo root)
        std::env::current_exe().ok().and_then(|exe| {
            exe.parent()
                .and_then(|p| p.parent())
                .and_then(|p| p.parent())
                .map(|p| p.join("runtime"))
        }),
    ];

    for candidate in dev_candidates.into_iter().flatten() {
        if candidate.join("rebar.config").exists() {
            return Ok((candidate, RuntimeLayout::Dev));
        }
    }

    // Installed-mode candidate: {exe_dir}/../lib/beamtalk/
    // Validated by checking for .beam files in the runtime ebin dir
    if let Some(installed_root) = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(|p| p.join("../lib/beamtalk")))
        .and_then(|p| p.canonicalize().ok())
    {
        if has_beam_files(&installed_root.join("lib/beamtalk_runtime/ebin")) {
            return Ok((installed_root, RuntimeLayout::Installed));
        }
    }

    Err(miette!(
        "Could not find Beamtalk runtime directory.\n\
        Please run from the repository root or set BEAMTALK_RUNTIME_DIR."
    ))
}

/// Check whether a directory contains compiled `.beam` files.
pub fn has_beam_files(dir: &Path) -> bool {
    dir.is_dir()
        && std::fs::read_dir(dir)
            .map(|entries| {
                entries
                    .flatten()
                    .any(|e| e.path().extension().is_some_and(|ext| ext == "beam"))
            })
            .unwrap_or(false)
}
/// Escapes a string for safe embedding in an Erlang string literal, used
/// when sending `.beam` file paths to the `beamtalk_build_worker` node.
///
/// Duplicates `beam_compiler::escape_erlang_string` (a pure, dependency-free
/// utility unlikely to change) rather than sharing it, so this module doesn't
/// need to depend on the CLI binary's `beam_compiler` module.
fn escape_erlang_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result
}

// ---------------------------------------------------------------------------
// Type cache: persists spec extraction results per Erlang module (ADR 0075)
// ---------------------------------------------------------------------------

/// Cache entry for a single Erlang module's spec extraction result.
///
/// Stores the raw protocol line (as emitted by `beamtalk_build_worker`) alongside
/// the `.beam` file's modification time. On subsequent builds, the cache is hit
/// when the `.beam` mtime matches — avoiding a round-trip to the BEAM node.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct TypeCacheEntry {
    /// Unix timestamp (seconds since epoch) of the `.beam` file when specs were extracted.
    beam_mtime_secs: u64,
    /// Sub-second nanoseconds of the `.beam` file's modification time.
    /// Combined with `beam_mtime_secs` to avoid cache collisions on rapid rewrites.
    #[serde(default)]
    beam_mtime_nanos: u32,
    /// Absolute path to the `.beam` file at the time specs were extracted —
    /// canonicalised when possible, otherwise resolved against the build's
    /// cwd. Used by [`load_type_cache_registry`] to re-stat the file and
    /// skip the entry if the live mtime no longer matches what was cached.
    /// Persisting an absolute path means `beamtalk lint` can validate the
    /// cache regardless of which cwd it is invoked from. Empty only for
    /// legacy entries written before BT-2139 — those are tolerated as fresh
    /// until the next build rewrites them with a path.
    #[serde(default)]
    beam_path: String,
    /// The producing compiler's Erlang→Beamtalk type-mapping stamp — a
    /// content hash of `beamtalk_spec_reader.erl` baked in at compile time
    /// via `BEAMTALK_SPEC_MAPPING_STAMP` (BT-2852). Compared against
    /// [`current_spec_mapping_stamp`] on read; a mismatch (including the
    /// empty default for entries written before BT-2852) is a cache miss, so
    /// a compiler upgrade that changes how Erlang types map to Beamtalk types
    /// invalidates stale FFI signatures instead of waiting for an unrelated
    /// `.beam` mtime or OTP-version change that may never happen.
    #[serde(default)]
    mapping_stamp: String,
    /// The raw `beamtalk-specs-module:...` protocol line (without newline).
    /// Empty string if the module had no specs or an error occurred.
    specs_line: String,
}

/// The current compiler's Erlang→Beamtalk type-mapping stamp (BT-2852) — see
/// [`TypeCacheEntry::mapping_stamp`]. Baked in at compile time by
/// `beamtalk-cli/build.rs` via `beamtalk_build::emit_spec_mapping_stamp`, so
/// comparing it is a cheap `&str` comparison, not a per-build filesystem hash.
///
/// `pub` so the CLI binary's `commands::lint` tests (and `beamtalk-mcp`) can
/// stamp/compare fixture cache entries against the running compiler's value.
pub fn current_spec_mapping_stamp() -> &'static str {
    env!("BEAMTALK_SPEC_MAPPING_STAMP")
}

/// Manages the type-spec cache for incremental spec extraction.
///
/// Each Erlang module gets a JSON file `<module>_<pathhash>.json` containing
/// the cached protocol line and the `.beam` file's mtime. On cache hit
/// (matching mtime), the protocol line is replayed into the
/// `NativeTypeRegistry` without spawning a BEAM node.
///
/// # Tiers (BT-2470)
///
/// The cache has two tiers:
///
/// * `local_dir` — the project-local `_build/type_cache/`. Always written, so
///   on-disk consumers ([`load_type_cache_registry`] for `beamtalk lint`, and
///   the LSP's `load_type_cache`) keep finding every module's specs unchanged.
/// * `shared_dir` — an optional shared, OTP-version-keyed tier (see
///   [`shared_otp_cache_dir`]). It survives `_build/` wipes and is reused
///   across projects and sessions, so a freshly cloned workspace does not pay
///   the cost of re-extracting hundreds of OTP modules. Only used for OTP
///   extraction; dependency extraction passes `None`.
///
/// On a `local` miss the `shared` tier is consulted; a shared hit is mirrored
/// back into the `local` tier so the on-disk consumers above see it. Writes go
/// to both tiers.
#[derive(Debug)]
struct TypeCache {
    local_dir: Utf8PathBuf,
    shared_dir: Option<Utf8PathBuf>,
}

impl TypeCache {
    /// Creates a single-tier type cache rooted at the given (project-local)
    /// directory. The directory is created lazily on first write.
    pub fn new(local_dir: Utf8PathBuf) -> Self {
        Self {
            local_dir,
            shared_dir: None,
        }
    }

    /// Creates a two-tier cache: a project-local tier plus a shared,
    /// OTP-version-keyed tier (BT-2470).
    pub fn with_shared(local_dir: Utf8PathBuf, shared_dir: Utf8PathBuf) -> Self {
        Self {
            local_dir,
            shared_dir: Some(shared_dir),
        }
    }

    /// Returns the cache file path for a given Erlang module name and beam path
    /// within `base`.
    ///
    /// The cache key incorporates a hash of the beam file's absolute path to
    /// prevent collisions when different projects have same-named `.beam` files
    /// (e.g., two projects both containing `my_app.beam` in the global stub cache).
    fn entry_path(base: &Utf8Path, module_name: &str, beam_path: &Utf8Path) -> Utf8PathBuf {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        beam_path.as_str().hash(&mut hasher);
        let path_hash = hasher.finish();
        base.join(format!("{module_name}_{path_hash:016x}.json"))
    }

    /// Checks if the cache entry for `module_name` at `beam_path` is still valid.
    ///
    /// Returns `Some(specs_line)` if the cache is fresh (`.beam` mtime matches),
    /// or `None` if the cache is stale or missing. The local tier is checked
    /// first; on a miss the shared tier is consulted and any shared hit is
    /// mirrored back into the local tier.
    fn lookup(
        &self,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
    ) -> Option<String> {
        if let Some(line) = Self::read_fresh(
            &self.local_dir,
            module_name,
            beam_path,
            beam_mtime_secs,
            beam_mtime_nanos,
        ) {
            return Some(line);
        }
        if let Some(shared) = &self.shared_dir {
            if let Some(line) = Self::read_fresh(
                shared,
                module_name,
                beam_path,
                beam_mtime_secs,
                beam_mtime_nanos,
            ) {
                // Mirror into the local tier so on-disk consumers (LSP, lint)
                // find OTP specs in `_build/type_cache/` exactly as before.
                Self::write_entry(
                    &self.local_dir,
                    module_name,
                    beam_path,
                    beam_mtime_secs,
                    beam_mtime_nanos,
                    &line,
                );
                return Some(line);
            }
        }
        None
    }

    /// Writes a cache entry for the given module to both tiers.
    fn store(
        &self,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
        specs_line: &str,
    ) {
        Self::write_entry(
            &self.local_dir,
            module_name,
            beam_path,
            beam_mtime_secs,
            beam_mtime_nanos,
            specs_line,
        );
        if let Some(shared) = &self.shared_dir {
            Self::write_entry(
                shared,
                module_name,
                beam_path,
                beam_mtime_secs,
                beam_mtime_nanos,
                specs_line,
            );
        }
    }

    /// Reads a fresh cache entry from `base`, or `None` if missing/stale.
    ///
    /// Freshness requires both the `.beam` mtime to match *and* the entry's
    /// mapping stamp to match the running compiler's (BT-2852) — an entry
    /// written before BT-2852 carries the empty default stamp, which never
    /// matches a real hash, so it is a miss rather than a crash.
    fn read_fresh(
        base: &Utf8Path,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
    ) -> Option<String> {
        let path = Self::entry_path(base, module_name, beam_path);
        let content = std::fs::read_to_string(path.as_std_path()).ok()?;
        let entry: TypeCacheEntry = serde_json::from_str(&content).ok()?;
        if entry.beam_mtime_secs == beam_mtime_secs
            && entry.beam_mtime_nanos == beam_mtime_nanos
            && entry.mapping_stamp == current_spec_mapping_stamp()
        {
            Some(entry.specs_line)
        } else {
            None
        }
    }

    /// Writes a single cache entry into `base`, creating the directory if
    /// needed. The write is atomic (temp file + rename) so concurrent builds
    /// sharing the OTP tier never observe a half-written entry (BT-2470).
    fn write_entry(
        base: &Utf8Path,
        module_name: &str,
        beam_path: &Utf8Path,
        beam_mtime_secs: u64,
        beam_mtime_nanos: u32,
        specs_line: &str,
    ) {
        if let Err(e) = std::fs::create_dir_all(base.as_std_path()) {
            debug!("Failed to create type cache dir {base}: {e}");
            return;
        }
        // BT-2139: persist an absolute (canonicalised) path so freshness
        // validation in `load_type_cache_registry` works when `beamtalk lint`
        // runs from a different working directory than the build that wrote
        // the cache. If `canonicalize_utf8` fails (typically because the
        // `.beam` vanished or moved mid-build), fall back to a manually-built
        // absolute path: if `beam_path` is already absolute, use it as-is;
        // otherwise resolve it against the current working directory. We
        // intentionally do *not* leave `beam_path` empty here — that would
        // make `is_cache_entry_fresh` treat the entry as legacy/fresh, so
        // lint would keep replaying stale specs instead of detecting that
        // the underlying `.beam` is gone.
        let canonical_beam_path = beam_path.canonicalize_utf8().map_or_else(
            |_| {
                if beam_path.is_absolute() {
                    beam_path.as_str().to_string()
                } else {
                    std::env::current_dir()
                        .ok()
                        .and_then(|cwd| Utf8PathBuf::from_path_buf(cwd).ok())
                        .map(|cwd| cwd.join(beam_path).into_string())
                        .unwrap_or_default()
                }
            },
            Utf8PathBuf::into_string,
        );
        let entry = TypeCacheEntry {
            beam_mtime_secs,
            beam_mtime_nanos,
            beam_path: canonical_beam_path,
            mapping_stamp: current_spec_mapping_stamp().to_string(),
            specs_line: specs_line.to_string(),
        };
        let path = Self::entry_path(base, module_name, beam_path);
        let json = match serde_json::to_string(&entry) {
            Ok(json) => json,
            Err(e) => {
                debug!("Failed to serialize type cache for {module_name}: {e}");
                return;
            }
        };
        // Atomic publish: write to a unique temp file then rename into place.
        // The temp name includes the pid so concurrent writers don't clash.
        let tmp = base.join(format!(
            "{module_name}_{:016x}.{}.tmp",
            {
                use std::hash::{Hash, Hasher};
                let mut h = std::collections::hash_map::DefaultHasher::new();
                beam_path.as_str().hash(&mut h);
                h.finish()
            },
            std::process::id()
        ));
        if let Err(e) = std::fs::write(tmp.as_std_path(), &json) {
            debug!("Failed to write type cache temp for {module_name}: {e}");
            return;
        }
        if let Err(e) = std::fs::rename(tmp.as_std_path(), path.as_std_path()) {
            // On Windows, rename can fail with "Access denied" if the destination
            // is open without delete-sharing (a concurrent reader). Fall back to a
            // copy so the cache update isn't silently dropped, then always clean up
            // the temp file regardless of which path succeeded.
            debug!(
                "Failed to publish type cache for {module_name} via rename: {e}; falling back to copy"
            );
            if let Err(e2) = std::fs::copy(tmp.as_std_path(), path.as_std_path()) {
                debug!("Failed to publish type cache for {module_name} via copy: {e2}");
            }
            let _ = std::fs::remove_file(tmp.as_std_path());
        }
    }
}

/// Returns the shared, OTP-version-keyed type-spec cache directory for the
/// given OTP version string, or `None` if no suitable base directory can be
/// determined (BT-2470).
///
/// The OTP portion of the FFI type cache (stdlib, kernel, erts, crypto, …)
/// only changes when the OTP/ERTS version changes — it is not project-specific.
/// Caching it outside `_build/` lets a freshly cloned workspace (where
/// `_build/type_cache/` does not yet exist) reuse a previous extraction instead
/// of re-reading hundreds of `.beam` files on startup.
///
/// Base directory resolution, in priority order:
/// 1. `BEAMTALK_CACHE_DIR` environment variable (explicit override, used by
///    tests and CI cache mounts).
/// 2. The platform cache directory ([`dirs::cache_dir`], which honours
///    `XDG_CACHE_HOME` on Linux).
///
/// The version string is sanitised for filesystem safety, so an OTP upgrade
/// (new version key) lands in a fresh sub-directory and never reuses
/// stale-version entries.
pub fn shared_otp_cache_dir(otp_version: &str) -> Option<Utf8PathBuf> {
    let base = std::env::var_os("BEAMTALK_CACHE_DIR")
        .map(std::path::PathBuf::from)
        .or_else(dirs::cache_dir)?;
    let base = Utf8PathBuf::from_path_buf(base).ok()?;
    let version_key: String = otp_version
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '.' || c == '-' || c == '_' {
                c
            } else {
                '-'
            }
        })
        .collect();
    if version_key.is_empty() {
        return None;
    }
    Some(base.join("beamtalk").join("otp-specs").join(version_key))
}

/// Extracts type specs from `.beam` files and populates a `NativeTypeRegistry`.
///
/// Uses the `beamtalk_build_worker` `{read_specs, ...}` protocol to extract
/// `-spec` attributes from `.beam` files in batch. Results are cached in
/// `cache_dir` (typically `_build/type_cache/`) keyed by module name and
/// `.beam` mtime — incremental builds read zero `.beam` files on cache hit.
///
/// # Protocol
///
/// Sends `{read_specs, [BeamFile1, BeamFile2, ...]}.` to the build worker.
/// Reads `beamtalk-specs-module:<module>:<erlang_term>` lines and a final
/// `beamtalk-specs-result-ok` or `beamtalk-specs-result-error`.
///
/// # Arguments
///
/// * `beam_files` - List of `.beam` file paths to extract specs from
/// * `cache_dir` - Directory for caching results (e.g., `_build/type_cache/`)
///
/// # Returns
///
/// A populated `NativeTypeRegistry` on success.
///
/// # Errors
///
/// Returns an error if the build worker cannot be started (runtime not compiled).
pub fn extract_beam_specs(
    beam_files: &[Utf8PathBuf],
    cache_dir: &Utf8Path,
) -> Result<NativeTypeRegistry> {
    extract_beam_specs_with_cache(beam_files, &TypeCache::new(cache_dir.to_path_buf()))
}

/// Like [`extract_beam_specs`], but adds a shared, OTP-version-keyed cache tier
/// (BT-2470) in front of the project-local `local_cache_dir`.
///
/// On a fresh `_build/` (e.g. a newly cloned workspace) the local tier misses
/// for every OTP module, but a warm shared tier — populated by a previous build
/// in any project — supplies the specs without re-reading hundreds of `.beam`
/// files. Shared hits are mirrored into the local tier so the LSP and
/// `beamtalk lint` keep loading OTP specs from `_build/type_cache/` unchanged.
///
/// Passing `shared_cache_dir = None` is equivalent to [`extract_beam_specs`].
///
/// # Errors
///
/// Returns an error if the build worker cannot be started (runtime not compiled).
pub fn extract_beam_specs_tiered(
    beam_files: &[Utf8PathBuf],
    local_cache_dir: &Utf8Path,
    shared_cache_dir: Option<&Utf8Path>,
) -> Result<NativeTypeRegistry> {
    let cache = match shared_cache_dir {
        Some(shared) => TypeCache::with_shared(local_cache_dir.to_path_buf(), shared.to_path_buf()),
        None => TypeCache::new(local_cache_dir.to_path_buf()),
    };
    extract_beam_specs_with_cache(beam_files, &cache)
}

#[instrument(skip_all, fields(beam_count = beam_files.len()))]
fn extract_beam_specs_with_cache(
    beam_files: &[Utf8PathBuf],
    cache: &TypeCache,
) -> Result<NativeTypeRegistry> {
    if beam_files.is_empty() {
        return Ok(NativeTypeRegistry::new());
    }

    let mut registry = NativeTypeRegistry::new();

    // Phase 1: Check cache, partition into hits and misses.
    let mut cache_misses = Vec::new();
    let mut cache_hit_count = 0;

    for beam_file in beam_files {
        let module_name = sanitize_module_name(beam_file.file_stem().unwrap_or(beam_file.as_str()));
        let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);

        if let Some(specs_line) = cache.lookup(module_name, beam_file, mtime_secs, mtime_nanos) {
            if !specs_line.is_empty() {
                parse_specs_line(&specs_line, &mut registry);
            }
            cache_hit_count += 1;
        } else {
            cache_misses.push(beam_file.clone());
        }
    }

    if cache_hit_count > 0 {
        debug!(
            cache_hits = cache_hit_count,
            cache_misses = cache_misses.len(),
            "Type cache results"
        );
    }

    if cache_misses.is_empty() {
        info!(
            modules = registry.module_count(),
            functions = registry.function_count(),
            "Type specs loaded from cache (zero .beam files read)"
        );
        return Ok(registry);
    }

    // Phase 2: Extract specs from cache misses via build worker.
    let new_lines = extract_specs_via_build_worker(&cache_misses)?;

    // Phase 3: Parse results and update cache.
    // Build a set of modules that produced output to identify negative-cache candidates.
    let mut seen_modules: std::collections::HashSet<String> = std::collections::HashSet::new();
    for (module_name, specs_line) in &new_lines {
        seen_modules.insert(module_name.clone());
        // Find the beam file for caching (need path + mtime).
        let beam_file = cache_misses.iter().find(|f| {
            f.file_stem()
                .is_some_and(|stem| stem == module_name.as_str())
        });
        if !specs_line.is_empty() {
            parse_specs_line(specs_line, &mut registry);
        }
        if let Some(beam_file) = beam_file {
            let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);
            cache.store(module_name, beam_file, mtime_secs, mtime_nanos, specs_line);
        }
    }

    // Negative cache: modules that were sent for extraction but produced no output
    // (e.g., no debug_info, no specs). Cache them with empty specs_line to avoid
    // re-extracting on every build.
    for beam_file in &cache_misses {
        let module_name = sanitize_module_name(beam_file.file_stem().unwrap_or(beam_file.as_str()));
        if !seen_modules.contains(module_name) {
            let (mtime_secs, mtime_nanos) = beam_mtime(beam_file);
            cache.store(module_name, beam_file, mtime_secs, mtime_nanos, "");
        }
    }

    info!(
        modules = registry.module_count(),
        functions = registry.function_count(),
        cache_hits = cache_hit_count,
        extracted = new_lines.len(),
        "Type spec extraction complete"
    );

    Ok(registry)
}

/// Reads `<module>_<hash>.json` files from `cache_dir` and replays the
/// freshest cached `specs_line` per module into a new [`NativeTypeRegistry`].
///
/// Used by `beamtalk lint` (BT-2134) to populate the same FFI type registry
/// `beamtalk build` uses, so the type checker's "Dynamic in typed class"
/// warning agrees with build on whether an FFI call is typed. Without this,
/// lint sees every `(Erlang m) f:` call as `Dynamic(UntypedFfi)` even when
/// the build cache has typed signatures.
///
/// `TypeCache::cache_path` keys filenames by the BEAM path hash, so multiple
/// `<module>_<hash>.json` entries can accumulate after dependency upgrades or
/// BEAM path changes. Replaying every file would let `read_dir` order pick a
/// stale signature, reintroducing the lint/build disagreement BT-2134 fixed.
/// Instead, group by module name and pick the entry with the latest file
/// mtime — that's the one the most recent build wrote, and it matches what
/// build's `extract_beam_specs` resolved for the current BEAM set.
///
/// Each entry's `beam_path` is re-stat'd against the live filesystem (BT-2139).
/// If the underlying `.beam` has changed since the build wrote the cache —
/// e.g. `cargo build` rebuilt a NIF module, or an OTP upgrade replaced
/// `gen_tcp.beam` — the entry is skipped so lint does not warn off stale FFI
/// signatures. Entries written before BT-2139 carry an empty `beam_path`;
/// those are pessimistically accepted as fresh until the next build rewrites
/// them with a path.
///
/// Returns `None` if `cache_dir` is not a directory or contains no entries.
pub fn load_type_cache_registry(cache_dir: &Utf8Path) -> Option<NativeTypeRegistry> {
    if !cache_dir.is_dir() {
        return None;
    }

    // Group cache files by module name, keeping the latest-mtime entry per
    // module so a stale `<module>_<old_hash>.json` doesn't shadow the fresh
    // one when `read_dir` happens to yield it second.
    let entries = std::fs::read_dir(cache_dir.as_std_path()).ok()?;
    let mut latest_by_module: std::collections::HashMap<String, (SystemTime, std::path::PathBuf)> =
        std::collections::HashMap::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(filename) = path.file_name().and_then(|s| s.to_str()) else {
            continue;
        };
        let Some(stem) = filename.strip_suffix(".json") else {
            continue;
        };
        // Cache filenames are exactly `<module>_<16-hex>`. Anything else is
        // foreign and must be ignored, including `<module>_socket_<hash>`
        // (where the module name itself ends in an underscore segment).
        let Some((module, hash)) = stem.rsplit_once('_') else {
            continue;
        };
        if hash.len() != 16 || !hash.chars().all(|c| c.is_ascii_hexdigit()) {
            continue;
        }
        let mtime = entry
            .metadata()
            .and_then(|m| m.modified())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        latest_by_module
            .entry(module.to_string())
            .and_modify(|(prev_mtime, prev_path)| {
                if mtime > *prev_mtime {
                    *prev_mtime = mtime;
                    prev_path.clone_from(&path);
                }
            })
            .or_insert((mtime, path));
    }

    let mut registry = NativeTypeRegistry::new();
    for (_, path) in latest_by_module.values() {
        let Ok(content) = std::fs::read_to_string(path) else {
            continue;
        };
        let Ok(entry) = serde_json::from_str::<TypeCacheEntry>(&content) else {
            continue;
        };
        if !is_cache_entry_fresh(&entry) {
            continue;
        }
        if !entry.specs_line.is_empty() {
            parse_specs_line(&entry.specs_line, &mut registry);
        }
    }

    if registry.module_count() == 0 {
        None
    } else {
        Some(registry)
    }
}

/// Sanitizes a module name derived from a beam file path by stripping any
/// directory components (path separators). This prevents path traversal when
/// the module name is used in cache filenames.
fn sanitize_module_name(name: &str) -> &str {
    // Take only the final component after any path separator.
    let after_slash = name.rfind('/').map_or(name, |i| &name[i + 1..]);
    after_slash
        .rfind('\\')
        .map_or(after_slash, |i| &after_slash[i + 1..])
}

/// Returns `true` if the cache entry still describes the live `.beam` file —
/// i.e. the file at `beam_path` exists and its mtime matches what was cached
/// — *and* the entry's type-mapping stamp matches the running compiler's
/// (BT-2852).
///
/// The mapping-stamp check is evaluated first: an entry written before
/// BT-2852 carries the empty default stamp, which never matches a real hash,
/// so every pre-BT-2852 entry is a miss (cache rebuild), never a crash —
/// including legacy entries with an empty `beam_path` that the check below
/// would otherwise pessimistically accept.
///
/// Legacy entries written before BT-2139 carry an empty `beam_path`; we have
/// no way to validate them, so — once the mapping stamp matches — they are
/// pessimistically accepted as fresh. The next `beamtalk build` rewrites them
/// with a path, which then enables validation on subsequent lint runs.
fn is_cache_entry_fresh(entry: &TypeCacheEntry) -> bool {
    if entry.mapping_stamp != current_spec_mapping_stamp() {
        return false;
    }
    if entry.beam_path.is_empty() {
        return true;
    }
    let path = Utf8Path::new(&entry.beam_path);
    if !path.exists() {
        return false;
    }
    let (secs, nanos) = beam_mtime(path);
    secs == entry.beam_mtime_secs && nanos == entry.beam_mtime_nanos
}

/// Returns the modification time of a `.beam` file as `(seconds, nanoseconds)`
/// since Unix epoch. Sub-second precision avoids cache collisions on rapid rewrites.
fn beam_mtime(path: &Utf8Path) -> (u64, u32) {
    std::fs::metadata(path.as_std_path())
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
        .map_or((0, 0), |d| (d.as_secs(), d.subsec_nanos()))
}

/// Extracts specs from `.beam` files by spawning a `beamtalk_build_worker` BEAM
/// node and sending the `{read_specs, [...]}` command.
///
/// Returns a list of `(module_name, specs_line)` pairs. The `specs_line` is the
/// raw protocol line for successful modules, or an empty string for modules that
/// had errors (`no_debug_info`, etc.).
fn extract_specs_via_build_worker(beam_files: &[Utf8PathBuf]) -> Result<Vec<(String, String)>> {
    let mut child = spawn_build_worker_for_specs(beam_files)?;

    let result = extract_specs_from_child(&mut child, beam_files);

    // Always wait/kill the child process to prevent zombies, even on error.
    if result.is_err() {
        let _ = child.kill();
    }
    let _ = child.wait();

    result
}

/// Inner helper for `extract_specs_via_build_worker`. Separated so that the
/// caller can guarantee `child.wait()` / `child.kill()` on all exit paths,
/// including early `?` returns from stdin/stdout/stderr capture or write errors.
fn extract_specs_from_child(
    child: &mut std::process::Child,
    beam_files: &[Utf8PathBuf],
) -> Result<Vec<(String, String)>> {
    let mut stdin = child
        .stdin
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stdin"))?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| miette::miette!("Failed to capture spec extraction stderr"))?;

    // Format and send the {read_specs, [BeamFile1, ...]}. input
    let file_list: Vec<String> = beam_files
        .iter()
        .map(|p| {
            let abs = std::fs::canonicalize(p.as_std_path())
                .unwrap_or_else(|_| p.as_std_path().to_path_buf());
            format!("\"{}\"", escape_erlang_string(&abs.to_string_lossy()))
        })
        .collect();
    let input = format!("{{read_specs,[{}]}}.\n", file_list.join(","));

    stdin
        .write_all(input.as_bytes())
        .into_diagnostic()
        .wrap_err("Failed to write to spec extraction stdin")?;
    drop(stdin);

    read_specs_protocol(stdout, stderr)
}

/// Spawns a `beamtalk_build_worker` BEAM node with the given `-pa` path arguments.
///
/// Applies the standard boot flags (`-noshell -mode minimal -boot no_dot_erlang`) and
/// the kernel logger redirect (BT-1431).  Callers supply the variable `-pa` paths and
/// add a context-specific `wrap_err` message on the returned `Result`.
///
/// # Errors
///
/// Returns an error if the `erl` process fails to spawn.
pub fn spawn_build_worker_node(pa_args: &[String]) -> Result<std::process::Child> {
    Command::new("erl")
        .arg("-noshell")
        .arg("-mode")
        .arg("minimal")
        .arg("-boot")
        .arg("no_dot_erlang")
        // Redirect OTP default logger to stderr (BT-1431). Without this, logger
        // output goes to stdout and mixes into the compilation protocol.
        .arg("-kernel")
        .arg("logger")
        .arg(KERNEL_LOGGER_STDERR)
        .args(pa_args)
        .arg("-s")
        .arg("beamtalk_build_worker")
        .arg("main")
        .current_dir(std::env::temp_dir())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .into_diagnostic()
}

/// Spawns a `beamtalk_build_worker` BEAM node for spec extraction.
///
/// `beam_files` are the `.beam` files about to be read for specs; their parent
/// directories are added to the `-pa` list alongside the runtime's own ebin
/// dirs so that sibling modules (e.g. a package's own `native/` ebin dir, per
/// ADR 0075 "package-bundled native code") can be resolved by `code:which/1`
/// when a `-spec` references one of their remote types (BT-2861).
fn spawn_build_worker_for_specs(beam_files: &[Utf8PathBuf]) -> Result<std::process::Child> {
    let (runtime_dir, layout) = find_runtime_dir_with_layout().map_err(|_| {
        miette::miette!(
            "Spec extraction requires the Beamtalk runtime.\n\
             Build the runtime first: cd runtime && rebar3 compile"
        )
    })?;

    let runtime_dir = runtime_dir
        .canonicalize()
        .into_diagnostic()
        .wrap_err("Failed to resolve runtime directory for spec extraction")?;

    let paths = beam_paths_for_layout(&runtime_dir, layout);

    if !paths
        .compiler_ebin
        .join("beamtalk_build_worker.beam")
        .exists()
    {
        return Err(miette::miette!(
            "Spec extraction requires beamtalk_build_worker module.\n\
             Rebuild: cd runtime && rebar3 compile"
        ));
    }

    // Add all runtime ebin directories to the code path so the spec reader
    // can resolve remote types (e.g., `beamtalk_result:t()` needs the
    // beamtalk_stdlib ebin on the path for `code:which/1` to find it).
    let mut ebin_dirs: Vec<std::path::PathBuf> = vec![
        paths.compiler_ebin.clone(),
        paths.runtime_ebin.clone(),
        paths.stdlib_erlang_ebin.clone(),
        paths.workspace_ebin.clone(),
    ];

    // Also add the ebin directories the beam files themselves live in (BT-2861).
    // A package's own native/ ebin dir isn't one of the runtime dirs above, so
    // without this a module's `-spec` referencing a sibling native module's
    // type (e.g. `beamtalk_http_response:t()`) can't be resolved by
    // `code:which/1`, and falls back to `Dynamic`. Canonicalize first: the
    // worker node runs with its cwd set to the system temp dir
    // (`spawn_build_worker_node`), so a relative `beam_file` path (e.g. from
    // a CLI arg given as a relative path) would resolve against the wrong
    // directory if passed to `-pa` as-is.
    let mut seen_dirs: std::collections::HashSet<std::path::PathBuf> =
        ebin_dirs.iter().cloned().collect();
    for beam_file in beam_files {
        let absolute = match std::fs::canonicalize(beam_file.as_std_path()) {
            Ok(abs) => abs,
            Err(e) => {
                warn!("Failed to canonicalize '{beam_file}': {e}. Using path as-is.");
                beam_file.as_std_path().to_path_buf()
            }
        };
        if let Some(parent) = absolute.parent() {
            let parent = parent.to_path_buf();
            if seen_dirs.insert(parent.clone()) {
                ebin_dirs.push(parent);
            }
        }
    }

    let mut pa_args = Vec::new();
    for ebin in &ebin_dirs {
        if ebin.exists() {
            pa_args.push("-pa".to_string());
            #[cfg(windows)]
            pa_args.push(ebin.to_string_lossy().replace('\\', "/"));
            #[cfg(not(windows))]
            pa_args.push(ebin.display().to_string());
        }
    }
    spawn_build_worker_node(&pa_args).wrap_err("Failed to start BEAM node for spec extraction")
}

/// Reads the `beamtalk-specs-module:` protocol lines from the build worker's stdout.
///
/// Returns `(module_name, raw_protocol_line)` pairs for each module.
fn read_specs_protocol(
    stdout: std::process::ChildStdout,
    stderr: std::process::ChildStderr,
) -> Result<Vec<(String, String)>> {
    // Read stderr in background to avoid deadlock
    let stderr_thread = thread::spawn(move || {
        let reader = BufReader::new(stderr);
        for line in reader.lines().map_while(Result::ok) {
            if !line.is_empty() {
                debug!(target: "spec_reader_stderr", "{}", line);
            }
        }
    });

    let reader = BufReader::new(stdout);
    let mut results = Vec::new();
    let mut success = false;

    for line in reader.lines() {
        let line = line.into_diagnostic()?;
        if is_specs_line(&line) {
            if let Some(rest) = line.strip_prefix("beamtalk-specs-module:") {
                if let Some(colon_pos) = rest.find(':') {
                    let module_name = rest[..colon_pos].to_string();
                    results.push((module_name, line.clone()));
                }
            }
        } else if is_specs_result_ok(&line) {
            success = true;
        } else if is_specs_result_error(&line) {
            warn!("Spec extraction reported errors (some modules may lack type info)");
            success = true;
        }
    }

    let _ = stderr_thread.join();

    if !success {
        warn!("Spec extraction did not receive a result line — partial results may be used");
    }

    Ok(results)
}

/// Result of probing the OTP installation for spec extraction (BT-2470).
#[derive(Debug, Default, Clone)]
pub struct OtpDiscovery {
    /// OTP version key (`<otp_release>-<erts_version>`, e.g. `27-15.0.1`) used
    /// to key the shared type-spec cache. `None` if the probe could not report
    /// it, in which case the shared cache tier is skipped.
    pub version: Option<String>,
    /// Absolute paths to all `.beam` files in the common OTP library ebins.
    pub beam_files: Vec<Utf8PathBuf>,
}

/// Probes the running OTP installation for its compound version key
/// (`<otp_release>-<erts>`, e.g. `27-15.0.1`) without enumerating any `.beam`
/// files.
///
/// This is the **same** key [`discover_otp_beam_files`] reports and the same one
/// that keys the shared type-spec cache (BT-2470). ADR 0098 provenance stamps
/// must use this compound — not bare `erlang:system_info(otp_release)`, which
/// returns only `"27"` — so a minor OTP/ERTS bump still invalidates artifacts.
///
/// Returns `None` if `erl` cannot be invoked or did not report a version; the
/// caller then compares provenance on `beamtalk_version` alone.
pub fn discover_otp_version() -> Option<String> {
    // Prefix the value with a sentinel (mirroring `discover_otp_beam_files`) and
    // scan for it, rather than trusting the whole of stdout: some OTP/platform
    // combinations emit ERTS startup lines before `io:format`, and a value
    // polluted by that noise would never match a recorded stamp — turning every
    // build into a full rebuild.
    let probe = "io:format(\"otp-version:~s-~s~n\", [erlang:system_info(otp_release), erlang:system_info(version)]), halt().";
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg(probe)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .find_map(|line| line.trim().strip_prefix("otp-version:"))
        // A bare "-" means both probes returned empty; treat as unknown.
        .filter(|version| !version.is_empty() && *version != "-")
        .map(str::to_string)
}

/// Discovers `.beam` files on the OTP code path and the OTP version.
///
/// Returns absolute paths to all `.beam` files in common OTP library ebin
/// directories (`stdlib`, `kernel`, etc.) plus an OTP/ERTS version key. Used to
/// find modules available for spec extraction and to key the shared type-spec
/// cache.
///
/// # Errors
///
/// Returns an error if `erl` cannot be invoked to discover the OTP lib directory.
pub fn discover_otp_beam_files() -> Result<OtpDiscovery> {
    // Apps we want type specs from. `erts` is included so `erlang.beam`
    // (BIFs like `whereis/1`, `spawn/3`, `self/0`) gets covered — its specs
    // are on disk even though `code:which(erlang)` returns `preloaded`. BT-2159.
    //
    // We probe `code:lib_dir(App)` per app rather than globbing `<lib_dir>/<app>-*`
    // because OTP layouts differ: upstream/kerl/brew put `erts-<vsn>` directly
    // under the OTP root, while Debian also mirrors it under `lib/`. `code:lib_dir/1`
    // is Erlang's canonical resolution and handles both.
    const COMMON_APPS: &[&str] = &[
        "stdlib", "kernel", "erts", "crypto", "ssl", "inets", "mnesia", "os_mon",
    ];

    let apps_atom_list = COMMON_APPS.join(",");
    // The probe prints one `otp-version:<release>-<erts>` line (the shared
    // cache key, BT-2470) followed by one ebin directory per discovered app.
    let probe = format!(
        "io:format(\"otp-version:~s-~s~n\", [erlang:system_info(otp_release), erlang:system_info(version)]), \
         lists:foreach(fun(App) -> case code:lib_dir(App) of {{error,_}} -> ok; Dir -> io:format(\"~s~n\", [filename:join(Dir, \"ebin\")]) end end, [{apps_atom_list}]), halt()."
    );

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-boot")
        .arg("no_dot_erlang")
        .arg("-eval")
        .arg(&probe)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to discover OTP ebin directories")?;

    if !output.status.success() {
        let stderr_msg = String::from_utf8_lossy(&output.stderr);
        warn!(
            "erl probe failed (exit {}): {}",
            output.status,
            stderr_msg.trim()
        );
        return Ok(OtpDiscovery::default());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut version = None;
    let mut beam_files = Vec::new();
    for line in stdout.lines() {
        let line = line.trim();
        if let Some(v) = line.strip_prefix("otp-version:") {
            if !v.is_empty() {
                version = Some(v.to_string());
            }
            continue;
        }
        let ebin_dir = std::path::Path::new(line);
        if !ebin_dir.is_dir() {
            continue;
        }
        if let Ok(entries) = std::fs::read_dir(ebin_dir) {
            for file in entries.flatten() {
                let path = file.path();
                if path.extension().is_some_and(|e| e == "beam") {
                    if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                        beam_files.push(utf8);
                    }
                }
            }
        }
    }

    debug!(
        count = beam_files.len(),
        version = ?version,
        "Discovered OTP .beam files"
    );
    Ok(OtpDiscovery {
        version,
        beam_files,
    })
}

/// Discover `.beam` files from project dependency directories.
///
/// Collects beams from:
/// - Path dependency ebin directories (`_build/deps/*/ebin/`)
/// - Native Erlang ebin (`_build/dev/native/ebin/`)
/// - Rebar3 hex dep ebins (`_build/dev/native/default/lib/*/ebin/`)
///
/// The caller combines these with OTP beams before passing the full set to
/// [`extract_beam_specs`], so the [`NativeTypeRegistry`] covers both OTP and
/// project dependencies.
pub fn discover_dependency_beam_files(ebin_dirs: &[Utf8PathBuf]) -> Vec<Utf8PathBuf> {
    let mut beam_files = Vec::new();

    for ebin_dir in ebin_dirs {
        if !ebin_dir.exists() {
            continue;
        }
        let Ok(entries) = std::fs::read_dir(ebin_dir) else {
            warn!(ebin = %ebin_dir, "Failed to read dependency ebin directory");
            continue;
        };
        for file in entries.flatten() {
            let path = file.path();
            if path.extension().is_some_and(|e| e == "beam") {
                if let Ok(utf8) = Utf8PathBuf::from_path_buf(path) {
                    beam_files.push(utf8);
                }
            }
        }
    }

    debug!(
        count = beam_files.len(),
        "Discovered dependency .beam files"
    );
    beam_files
}

/// ADR 0075 Phase 1 / BT-2851 / BT-2859: Extract type specs from OTP and
/// dependency `.beam` files and cache them.
///
/// This is the single source of truth for populating a [`NativeTypeRegistry`]
/// from `.beam` files, shared by `beamtalk build`/`beamtalk lint` (via
/// `beamtalk-cli`'s `native_type_specs::extract_project_type_specs`, which
/// resolves `cache_dir`/`dependency_ebin_dirs` from a `BuildLayout`),
/// `beamtalk-mcp`'s `lint`/`diagnostic_summary` tools (BT-2858, same path),
/// and `beamtalk-lsp` (BT-2859, which resolves the same `_build/` paths
/// itself rather than depending on `beamtalk-cli`'s `BuildLayout`).
///
/// `dependency_ebin_dirs` should include the project's path-dependency ebin
/// dirs (`_build/deps/*/ebin/`), its own compiled `native/` ebin dir, and any
/// rebar3 hex-dependency ebin dirs — see `native_type_specs::
/// collect_dependency_ebin_dirs` in `beamtalk-cli` for the canonical CLI-side
/// construction.
///
/// Non-fatal: if spec extraction fails (e.g., runtime not compiled), returns
/// `None` rather than erroring — callers fall back to untyped FFI checking.
pub fn extract_type_specs(
    cache_dir: &Utf8Path,
    dependency_ebin_dirs: &[Utf8PathBuf],
) -> Option<NativeTypeRegistry> {
    // Discover OTP .beam files and the OTP version (for the shared cache key).
    let otp = match discover_otp_beam_files() {
        Ok(discovery) => discovery,
        Err(e) => {
            debug!("Skipping OTP type spec extraction: {e}");
            OtpDiscovery::default()
        }
    };

    // De-duplicate OTP beams by module name (file stem); first occurrence wins.
    let mut seen_modules = std::collections::HashSet::new();
    let mut otp_beams = otp.beam_files;
    otp_beams.retain(|beam_file| match beam_file.file_stem() {
        Some(stem) => seen_modules.insert(stem.to_owned()),
        None => true,
    });

    // De-duplicate dep beams, and drop any module already covered by OTP so the
    // OTP-first precedence (and a single .beam per module) is preserved.
    let mut dep_beams = discover_dependency_beam_files(dependency_ebin_dirs);
    dep_beams.retain(|beam_file| match beam_file.file_stem() {
        Some(stem) => seen_modules.insert(stem.to_owned()),
        None => true,
    });

    if otp_beams.is_empty() && dep_beams.is_empty() {
        debug!("No .beam files found for type spec extraction");
        return None;
    }

    // BT-2470: OTP specs go through the shared, version-keyed cache tier (which
    // survives a wiped `_build/`); dependency/native specs stay project-local.
    let shared_dir = otp.version.as_deref().and_then(shared_otp_cache_dir);
    if let Some(shared) = &shared_dir {
        debug!(dir = %shared, "Using shared OTP type-spec cache");
    }

    let otp_registry = match extract_beam_specs_tiered(&otp_beams, cache_dir, shared_dir.as_deref())
    {
        Ok(registry) => registry,
        Err(e) => {
            debug!("OTP type spec extraction failed (non-fatal): {e}");
            NativeTypeRegistry::new()
        }
    };

    let dep_registry = match extract_beam_specs(&dep_beams, cache_dir) {
        Ok(registry) => registry,
        Err(e) => {
            debug!("Dependency type spec extraction failed (non-fatal): {e}");
            NativeTypeRegistry::new()
        }
    };

    // Merge dependency specs into the OTP registry; OTP wins on any collision.
    let mut registry = otp_registry;
    registry.merge(dep_registry);

    if registry.module_count() > 0 {
        info!(
            modules = registry.module_count(),
            functions = registry.function_count(),
            "Extracted Erlang FFI type specs"
        );
        Some(registry)
    } else {
        None
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    // -----------------------------------------------------------------------
    // Runtime discovery (moved from beamtalk-cli's repl_startup, BT-2859)
    // -----------------------------------------------------------------------

    #[test]
    fn beam_paths_uses_correct_layout() {
        let paths = beam_paths(Path::new("/rt"));
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.cowboy_ebin,
            PathBuf::from("/rt/_build/default/lib/cowboy/ebin")
        );
        assert_eq!(
            paths.cowlib_ebin,
            PathBuf::from("/rt/_build/default/lib/cowlib/ebin")
        );
        assert_eq!(
            paths.ranch_ebin,
            PathBuf::from("/rt/_build/default/lib/ranch/ebin")
        );
        assert_eq!(
            paths.telemetry_ebin,
            PathBuf::from("/rt/_build/default/lib/telemetry/ebin")
        );
        assert_eq!(
            paths.telemetry_poller_ebin,
            PathBuf::from("/rt/_build/default/lib/telemetry_poller/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_paths_installed_layout() {
        let paths = beam_paths_for_layout(
            Path::new("/usr/local/lib/beamtalk"),
            RuntimeLayout::Installed,
        );
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_runtime/ebin")
        );
        assert_eq!(
            paths.workspace_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_workspace/ebin")
        );
        assert_eq!(
            paths.compiler_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_compiler/ebin")
        );
        assert_eq!(
            paths.cowboy_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/cowboy/ebin")
        );
        assert_eq!(
            paths.cowlib_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/cowlib/ebin")
        );
        assert_eq!(
            paths.ranch_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/ranch/ebin")
        );
        assert_eq!(
            paths.telemetry_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/telemetry/ebin")
        );
        assert_eq!(
            paths.telemetry_poller_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/telemetry_poller/ebin")
        );
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_stdlib/ebin")
        );
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/usr/local/lib/beamtalk/lib/beamtalk_stdlib/ebin")
        );
    }

    #[test]
    fn beam_paths_dev_uses_build_dir() {
        let paths = beam_paths_for_layout(Path::new("/rt"), RuntimeLayout::Dev);
        // Dev layout uses _build/default/lib/ for runtime/workspace/etc.
        assert_eq!(
            paths.runtime_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_runtime/ebin")
        );
        // Class beams are under apps/
        assert_eq!(
            paths.stdlib_ebin,
            PathBuf::from("/rt/apps/beamtalk_stdlib/ebin")
        );
        // Erlang module beams are under _build/
        assert_eq!(
            paths.stdlib_erlang_ebin,
            PathBuf::from("/rt/_build/default/lib/beamtalk_stdlib/ebin")
        );
    }

    // -----------------------------------------------------------------------
    // extract_type_specs (BT-2859)
    // -----------------------------------------------------------------------

    #[test]
    fn extract_type_specs_extracts_live_on_cold_cache() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        assert!(!cache_dir.exists());

        let Some(registry) = extract_type_specs(&cache_dir, &[]) else {
            eprintln!(
                "skipping extract_type_specs_extracts_live_on_cold_cache: \
                 no OTP .beam files discovered in this environment"
            );
            return;
        };
        assert!(
            registry.lookup("lists", "reverse", 1).is_some(),
            "live extraction with no prior build must still find lists:reverse/1"
        );
        assert!(cache_dir.exists());
    }

    #[test]
    fn extract_type_specs_no_ebin_dirs_and_no_otp_returns_none_gracefully() {
        // Not a behavioural assertion about OTP discovery (environment-dependent) —
        // just confirms passing an empty dependency-ebin-dirs slice doesn't panic
        // and produces a result consistent with whatever OTP discovery finds.
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let _ = extract_type_specs(&cache_dir, &[]);
    }

    // -----------------------------------------------------------------------
    // TypeCache tests (ADR 0075)
    // -----------------------------------------------------------------------

    #[test]
    fn type_cache_store_and_lookup() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);
        let beam_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        let specs_line = "beamtalk-specs-module:lists:[#{arity => 1,name => <<\"reverse\">>,params => [#{name => <<\"list\">>,type => <<\"List\">>}],return_type => <<\"List\">>}]";
        cache.store("lists", beam_path, 12345, 0, specs_line);

        // Cache hit with matching mtime
        let result = cache.lookup("lists", beam_path, 12345, 0);
        assert_eq!(result, Some(specs_line.to_string()));

        // Cache miss with different mtime (seconds)
        let result = cache.lookup("lists", beam_path, 99999, 0);
        assert!(result.is_none(), "Different mtime should be a cache miss");

        // Cache miss with different mtime (nanos only)
        let result = cache.lookup("lists", beam_path, 12345, 1);
        assert!(result.is_none(), "Different nanos should be a cache miss");

        // Cache miss for unknown module
        let maps_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/maps.beam");
        let result = cache.lookup("maps", maps_path, 12345, 0);
        assert!(result.is_none(), "Unknown module should be a cache miss");
    }

    #[test]
    fn type_cache_invalidates_on_mtime_change() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);
        let beam_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        cache.store("lists", beam_path, 100, 0, "line1");
        assert_eq!(
            cache.lookup("lists", beam_path, 100, 0),
            Some("line1".to_string())
        );

        // Overwrite with new mtime
        cache.store("lists", beam_path, 200, 0, "line2");
        assert!(cache.lookup("lists", beam_path, 100, 0).is_none());
        assert_eq!(
            cache.lookup("lists", beam_path, 200, 0),
            Some("line2".to_string())
        );
    }

    /// BT-2852: A cache entry written by a *different* compiler build — same
    /// module, same `.beam` mtime, same path, but a different
    /// `mapping_stamp` — must be treated as a miss. This is the exact warm-cache
    /// scenario the issue describes: a `beamtalk_spec_reader.erl` mapping-logic
    /// change (e.g. BT-2817 widening `string()` to `String | List`) must
    /// invalidate previously-cached specs even though nothing about the
    /// `.beam` file itself changed.
    #[test]
    fn type_cache_invalidates_on_mapping_stamp_change() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir.clone());
        let beam_path = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        cache.store("lists", beam_path, 100, 0, "specs_line_v1");
        assert_eq!(
            cache.lookup("lists", beam_path, 100, 0),
            Some("specs_line_v1".to_string()),
            "freshly stored entry should be a hit"
        );

        // Simulate an older-build entry: rewrite the on-disk JSON with a
        // different `mapping_stamp`, keeping module/mtime/path identical —
        // i.e. everything the *old* cache key compared stays the same.
        let entry_path = TypeCache::entry_path(&cache_dir, "lists", beam_path);
        let content = std::fs::read_to_string(entry_path.as_std_path()).unwrap();
        let mut entry: serde_json::Value = serde_json::from_str(&content).unwrap();
        entry["mapping_stamp"] =
            serde_json::Value::String("an-older-compiler-builds-stamp".to_string());
        std::fs::write(entry_path.as_std_path(), entry.to_string()).unwrap();

        assert!(
            cache.lookup("lists", beam_path, 100, 0).is_none(),
            "a stale mapping_stamp must invalidate an otherwise-fresh (matching mtime) entry"
        );
    }

    #[test]
    fn type_cache_handles_empty_specs_line() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);
        let beam_path = Utf8Path::new("/some/path/no_specs.beam");

        cache.store("no_specs", beam_path, 100, 0, "");
        assert_eq!(
            cache.lookup("no_specs", beam_path, 100, 0),
            Some(String::new())
        );
    }

    #[test]
    fn type_cache_different_paths_do_not_collide() {
        let temp = TempDir::new().unwrap();
        let cache_dir = Utf8PathBuf::from_path_buf(temp.path().join("type_cache")).unwrap();
        let cache = TypeCache::new(cache_dir);

        let path_a = Utf8Path::new("/project_a/_build/my_app.beam");
        let path_b = Utf8Path::new("/project_b/_build/my_app.beam");

        cache.store("my_app", path_a, 100, 0, "specs_from_a");
        cache.store("my_app", path_b, 100, 0, "specs_from_b");

        // Each path returns its own cached specs
        assert_eq!(
            cache.lookup("my_app", path_a, 100, 0),
            Some("specs_from_a".to_string()),
            "Path A should return its own cached specs"
        );
        assert_eq!(
            cache.lookup("my_app", path_b, 100, 0),
            Some("specs_from_b".to_string()),
            "Path B should return its own cached specs"
        );
    }

    /// BT-2470: a fresh project (empty local tier) resolves OTP specs via the
    /// shared tier, and the shared hit is mirrored into the local tier so the
    /// LSP and `beamtalk lint` keep finding specs in `_build/type_cache/`.
    #[test]
    fn type_cache_shared_tier_serves_and_mirrors_to_local() {
        let temp = TempDir::new().unwrap();
        let shared = Utf8PathBuf::from_path_buf(temp.path().join("shared")).unwrap();
        let beam = Utf8Path::new("/usr/lib/erlang/lib/stdlib/ebin/lists.beam");

        // A prior build in some project populates both its local tier and the
        // shared tier.
        let producer_local = Utf8PathBuf::from_path_buf(temp.path().join("producer")).unwrap();
        let producer = TypeCache::with_shared(producer_local, shared.clone());
        producer.store("lists", beam, 200, 5, "specs_for_lists");

        // A freshly cloned workspace has an empty local tier but the same shared
        // tier — the lookup still succeeds.
        let fresh_local = Utf8PathBuf::from_path_buf(temp.path().join("fresh")).unwrap();
        let consumer = TypeCache::with_shared(fresh_local.clone(), shared);
        assert_eq!(
            consumer.lookup("lists", beam, 200, 5),
            Some("specs_for_lists".to_string()),
            "shared tier should serve a fresh project's local miss"
        );

        // The shared hit was mirrored into the fresh local tier: a local-only
        // cache (no shared tier) now finds it too.
        let local_only = TypeCache::new(fresh_local);
        assert_eq!(
            local_only.lookup("lists", beam, 200, 5),
            Some("specs_for_lists".to_string()),
            "shared hit should be mirrored into the local tier for the LSP/lint"
        );
    }

    #[test]
    #[serial_test::serial(beamtalk_cache_env)]
    fn shared_otp_cache_dir_uses_env_override_and_sanitises_version() {
        let temp = TempDir::new().unwrap();
        let base = temp.path().to_string_lossy().to_string();
        // SAFETY: serialised via #[serial]; the var is removed before returning.
        unsafe {
            std::env::set_var("BEAMTALK_CACHE_DIR", &base);
        }
        let dir = shared_otp_cache_dir("27/15.0:weird");
        // SAFETY: serialised via #[serial]; restores the unset state.
        unsafe {
            std::env::remove_var("BEAMTALK_CACHE_DIR");
        }
        let dir = dir.expect("shared cache dir should resolve under the override");
        assert!(
            dir.starts_with(&base),
            "shared cache dir should honour BEAMTALK_CACHE_DIR: {dir}"
        );
        // Compare on path components, not a slash-joined string, so the
        // assertion holds on Windows (where `join` uses `\`) as well as Unix.
        let tail: Vec<&str> = dir.components().rev().take(3).map(|c| c.as_str()).collect();
        assert_eq!(
            tail,
            vec!["27-15.0-weird", "otp-specs", "beamtalk"],
            "version key should be filesystem-sanitised: {dir}"
        );
    }

    #[test]
    #[serial_test::serial(beamtalk_cache_env)]
    fn shared_otp_cache_dir_rejects_empty_version() {
        let temp = TempDir::new().unwrap();
        // SAFETY: serialised via #[serial]; the var is removed before returning.
        unsafe {
            std::env::set_var("BEAMTALK_CACHE_DIR", temp.path());
        }
        let dir = shared_otp_cache_dir("");
        // SAFETY: serialised via #[serial]; restores the unset state.
        unsafe {
            std::env::remove_var("BEAMTALK_CACHE_DIR");
        }
        assert!(dir.is_none(), "empty version must not yield a cache dir");
    }

    /// BT-2159: `erts` must be in the OTP discovery set so `erlang.beam`
    /// (BIFs like `whereis/1`, `spawn/3`, `self/0`) gets spec extraction.
    /// `code:which(erlang)` returns `preloaded`, but the `.beam` exists in
    /// `<erts-app>/ebin/erlang.beam` with full abstract code.
    #[test]
    fn discover_otp_beam_files_includes_erts() {
        let Ok(discovery) = discover_otp_beam_files() else {
            // Skip only when `erl` cannot be spawned (test env without Erlang).
            // A successful probe that returns zero beams is a real failure and
            // is caught by the assert below.
            return;
        };
        let beams = &discovery.beam_files;
        let has_erlang = beams
            .iter()
            .any(|p| p.file_stem().is_some_and(|s| s == "erlang"));
        assert!(
            has_erlang,
            "discover_otp_beam_files must include erts/ebin/erlang.beam so BIF specs reach NativeTypeRegistry (BT-2159). Got {} beams: {:?}",
            beams.len(),
            beams
        );
        // BT-2470: a successful probe must also report the OTP version key
        // used to scope the shared type-spec cache.
        assert!(
            discovery.version.is_some(),
            "discover_otp_beam_files must report an OTP version key for the shared cache"
        );
    }

    #[test]
    fn extract_beam_specs_empty_input() {
        let result = extract_beam_specs(&[], Utf8Path::new("/tmp/nonexistent"));
        assert!(result.is_ok());
        let registry = result.unwrap();
        assert_eq!(registry.module_count(), 0);
    }

    #[test]
    fn beam_mtime_nonexistent() {
        let (secs, nanos) = beam_mtime(Utf8Path::new("/nonexistent/foo.beam"));
        assert_eq!(secs, 0, "Nonexistent file should return 0 seconds");
        assert_eq!(nanos, 0, "Nonexistent file should return 0 nanos");
    }

    #[test]
    fn sanitize_module_name_plain() {
        assert_eq!(sanitize_module_name("lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_unix_path() {
        assert_eq!(sanitize_module_name("/usr/lib/erlang/lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_windows_path() {
        assert_eq!(sanitize_module_name("C:\\otp\\lib\\lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_strips_mixed_separators() {
        assert_eq!(sanitize_module_name("/usr/lib\\erlang/lists"), "lists");
    }

    #[test]
    fn sanitize_module_name_empty_string() {
        assert_eq!(sanitize_module_name(""), "");
    }

    #[test]
    fn discover_dependency_beam_files_empty_dirs() {
        let result = discover_dependency_beam_files(&[]);
        assert!(result.is_empty());
    }

    #[test]
    fn discover_dependency_beam_files_finds_beams() {
        let temp = TempDir::new().unwrap();
        let ebin = temp.path().join("ebin");
        fs::create_dir_all(&ebin).unwrap();
        fs::write(ebin.join("my_mod.beam"), b"fake beam").unwrap();
        fs::write(ebin.join("other.erl"), b"not a beam").unwrap();

        let ebin_path = Utf8PathBuf::from_path_buf(ebin).unwrap();
        let result = discover_dependency_beam_files(&[ebin_path]);
        assert_eq!(result.len(), 1);
        assert!(result[0].as_str().ends_with("my_mod.beam"));
    }

    #[test]
    fn discover_dependency_beam_files_skips_missing_dirs() {
        let temp = TempDir::new().unwrap();
        let nonexistent = Utf8PathBuf::from_path_buf(temp.path().join("missing")).unwrap();
        let result = discover_dependency_beam_files(&[nonexistent]);
        assert!(result.is_empty());
    }
}
