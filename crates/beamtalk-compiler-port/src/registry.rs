// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Process-wide registry loading.
//!
//! Caches the project's `[diagnostics]` severity-override table (ADR
//! 0100 Rule 3) and Erlang FFI native type registry (ADR 0075, BT-2891)
//! once per compiler-port process, loaded lazily from the working
//! directory the process was spawned with.

/// Process-wide cache for the package's `beamtalk.toml` `[diagnostics]`
/// severity-override table (ADR 0100 Rule 3, BT-2839).
///
/// The compiler port is a long-lived process spawned once per BEAM node
/// session (interactive REPL, `beamtalk run`, or a connected/LiveView
/// session attached to either) with its working directory set to the
/// project root — every node-startup path pins its own cwd there (e.g.
/// `crates/beamtalk-cli/src/commands/repl/process.rs`,
/// `crates/beamtalk-cli/src/commands/run.rs`) — and this port process
/// inherits it at spawn time, unaffected by any later `file:set_cwd/1` on
/// the Erlang side. Reading and parsing `beamtalk.toml`
/// on every `compile_expression`/`compile`/`diagnostics` request would repeat
/// disk I/O on a hot path (`diagnostics` in particular fires on a ~150ms
/// idle-debounce as the user types); caching once per process — mirroring the
/// LSP's "load once at startup" (`Backend::load_diagnostics_table`, BT-2800)
/// — avoids that while keeping the same lenient, no-manifest-is-a-no-op
/// semantics.
static DIAGNOSTICS_OVERRIDES: std::sync::OnceLock<beamtalk_core::compilation::DiagnosticsTable> =
    std::sync::OnceLock::new();

/// Returns the process-wide `[diagnostics]` table, loading it from the
/// current working directory's `beamtalk.toml` on first use.
pub(crate) fn diagnostics_overrides() -> &'static beamtalk_core::compilation::DiagnosticsTable {
    DIAGNOSTICS_OVERRIDES.get_or_init(|| {
        let cwd = std::env::current_dir().unwrap_or_default();
        load_diagnostics_overrides_from(&cwd)
    })
}

/// Load the `[diagnostics]` severity-override table from `<root>/beamtalk.toml`
/// (ADR 0100 Rule 3).
///
/// Delegates to [`beamtalk_core::compilation::load_diagnostics_table_for_root`]
/// for the lenient read-parse-or-empty semantics: missing manifest → empty
/// table (silent), non-`NotFound` I/O errors (permissions, EISDIR, etc.) →
/// `WARN` log + empty table, parse failure → `WARN` log + empty table / Rule 1
/// defaults. The `debug!` log on a non-empty result is compiler-port-specific
/// telemetry kept here in the caller.
///
/// Pure function of `root` (no global state) so it is directly unit-testable
/// without touching the process's real working directory.
pub(crate) fn load_diagnostics_overrides_from(
    root: &std::path::Path,
) -> beamtalk_core::compilation::DiagnosticsTable {
    let table = beamtalk_core::compilation::load_diagnostics_table_for_root(root);
    if !table.is_empty() {
        tracing::debug!(
            count = table.len(),
            "Loaded [diagnostics] severity override(s) from beamtalk.toml"
        );
    }
    table
}

/// Process-wide cache of the project's Erlang FFI type signatures (ADR 0075,
/// BT-2891), loaded once from `<root>/_build/type_cache/` — the same on-disk
/// cache `beamtalk build`/`beamtalk lint` write and read (see
/// `beamtalk_core::ffi_type_specs`).
///
/// Mirrors [`DIAGNOSTICS_OVERRIDES`]: loaded once per process rather than per
/// request, using the project-root cwd every node-startup path already pins
/// (see that static's doc comment). Reads the on-disk cache only — never
/// live-extracts from `.beam` files (unlike the LSP's `load_type_cache`,
/// which may spawn a `beamtalk_build_worker` BEAM node) — because
/// `resolve_completion_type` sits on the REPL's tight completion-latency
/// budget (ADR 0045) and a project that has never run `beamtalk build` should
/// stay registry-blind rather than block a keystroke on a build-worker spawn.
static NATIVE_TYPE_REGISTRY: std::sync::OnceLock<
    beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry,
> = std::sync::OnceLock::new();

/// Returns the process-wide native type registry, loading it from the
/// current working directory's `_build/type_cache/` on first use.
pub(crate) fn native_type_registry()
-> &'static beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry {
    NATIVE_TYPE_REGISTRY.get_or_init(|| {
        let cwd = std::env::current_dir().unwrap_or_default();
        load_native_type_registry_from(&cwd)
    })
}

/// Load the Erlang FFI type registry from `<root>/_build/type_cache/`
/// (ADR 0075, BT-2891).
///
/// Lenient by design, mirroring [`load_diagnostics_overrides_from`]: a root
/// with no `_build/type_cache/` (project never built) yields an empty
/// registry rather than an error — `resolve_expression_type` already treats
/// `None`/empty identically (falls back to `Dynamic`), so this degrades to
/// exactly the pre-BT-2891 registry-blind behaviour. Pure function of `root`
/// so it is directly unit-testable without touching the process's real
/// working directory.
pub(crate) fn load_native_type_registry_from(
    root: &std::path::Path,
) -> beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry {
    use beamtalk_core::semantic_analysis::type_checker::NativeTypeRegistry;

    let Some(root) = camino::Utf8Path::from_path(root) else {
        tracing::debug!(
            root = %root.display(),
            "Project root is not valid UTF-8; native type registry stays empty"
        );
        return NativeTypeRegistry::new();
    };
    let cache_dir = root.join("_build").join("type_cache");
    if let Some(registry) = beamtalk_core::ffi_type_specs::load_type_cache_registry(&cache_dir) {
        tracing::debug!(
            modules = registry.module_count(),
            functions = registry.function_count(),
            "Loaded native type registry from _build/type_cache/"
        );
        registry
    } else {
        tracing::debug!(
            cache_dir = %cache_dir,
            "No _build/type_cache/ found; native type registry stays empty \
             (FFI expressions fall back to Dynamic until `beamtalk build` runs)"
        );
        NativeTypeRegistry::new()
    }
}
