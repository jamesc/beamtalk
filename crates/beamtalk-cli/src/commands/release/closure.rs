// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! The `beamtalk release` app closure (ADR 0125 §1.2, §1.3).
//!
//! The included application set is *computed*, not declared: the project's
//! own app, the ADR 0070 dependency closure (from `_build/deps/` — the same
//! graph `beamtalk build` already resolves, not re-derived), the runtime
//! closure (`beamtalk_runtime`/`beamtalk_stdlib`/`beamtalk_workspace` and
//! their declared deps `cowboy`/`cowlib`/`ranch`/`telemetry`/
//! `telemetry_poller`), `[release] apps` extras, any further rebar3
//! native/hex dep a staged app's `.app` declares and `beamtalk build` has
//! already resolved (e.g. `gun`, declared by `http`), and `kernel`/
//! `stdlib`/`sasl`/`crypto` (host-provided, never staged). `beamtalk_compiler`
//! (+ OTP's own `compiler` app) is included only when `[release]
//! include-compiler` is set.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};

use beamtalk_cli::repl_startup;

use super::super::build_layout::BuildLayout;
use super::super::deps::collect_dep_ebin_paths;
use super::super::manifest::{PackageManifest, ReleaseConfig};

/// One OTP application staged into the release's `lib/` tree.
#[derive(Debug, Clone)]
pub struct StagedApp {
    /// The OTP application name (e.g. `"beamtalk_runtime"`).
    pub name: String,
    /// The version read from the app's **generated** `.app` file (ADR 0125
    /// §1.3 — never from `.app.src`, whose `{vsn, {cmd, …}}` `systools`
    /// cannot resolve).
    pub vsn: String,
    /// The ebin director(y/ies) to copy files from into
    /// `lib/<name>-<vsn>/ebin/`. More than one only for `beamtalk_stdlib`
    /// in a dev checkout, where the `bt@stdlib@*` class beams and the
    /// rebar3-built `.app` live in two separate directories.
    pub source_ebins: Vec<Utf8PathBuf>,
    /// This app's own declared `{applications, […]}` list, read from the
    /// same `.app` file `vsn` came from. Used to discover host-only
    /// transitive dependencies a staged app declares but nothing in the
    /// closure stages directly — e.g. `ranch` (staged) declares `ssl`
    /// (host-provided, never staged). `systools:make_script/2` hard-errors
    /// on a `.rel` that omits any declared dependency
    /// (`{undefined_applications, […]}`), so every one of these must end up
    /// either staged or in the release's host-app set.
    pub declared_deps: Vec<String>,
}

/// The computed application closure for a release build.
#[derive(Debug, Clone)]
pub struct AppClosure {
    /// Host-provided OTP applications (`kernel`, `stdlib`, `sasl`,
    /// `crypto`, and `compiler` when `include-compiler` is set) — resolved
    /// against the *building* machine's own Erlang/OTP install, never
    /// staged into `lib/`.
    pub host_apps: Vec<String>,
    /// Applications staged into `lib/<app>-<vsn>/ebin/`. Order carries no
    /// meaning to `systools:make_script/2` (it derives boot order itself
    /// from each `.app`'s declared dependencies) — a native/hex dep
    /// auto-staged to satisfy another staged app's declared dependency (see
    /// [`compute_app_closure`]) is appended after the project's own app.
    pub staged_apps: Vec<StagedApp>,
}

/// The runtime closure's own application names, in the order
/// `compute_app_closure` stages them (leaves first) — `beamtalk_compiler`
/// only joins it when `include_compiler` is set. The single declared list
/// [`is_runtime_app`] checks against, so a caller that needs to tell a
/// runtime-closure app from a project/dependency one (the shape extractor's
/// `RuntimeLibDirs`/`EmitLibDirs` split, ADR 0125 §2.2 — `assembly.rs`'s
/// `write_shapes_json` caller) never re-derives this set by hand.
pub const RUNTIME_APP_NAMES: &[&str] = &[
    "cowlib",
    "ranch",
    "cowboy",
    "telemetry",
    "telemetry_poller",
    "beamtalk_runtime",
    "beamtalk_stdlib",
    "beamtalk_workspace",
    "beamtalk_compiler",
];

/// Whether `app_name` is one of the runtime closure's own applications
/// (never a project app, an ADR 0070 dependency, or a `[release] apps`
/// extra) — see [`RUNTIME_APP_NAMES`].
pub fn is_runtime_app(app_name: &str) -> bool {
    RUNTIME_APP_NAMES.contains(&app_name)
}

/// Compute the release's app closure (ADR 0125 §1.2).
///
/// `project_root` must already be built (`beamtalk build`) so the project's
/// own `.app` and every `_build/deps/*/ebin/*.app` exist.
// Each `staged.push(…)` below is fallible (`?`) and one is conditional
// (`include_compiler`), so a `vec![…]` literal can't replace the `Vec::new()`
// + `push` sequence `vec_init_then_push` would otherwise suggest.
#[allow(clippy::vec_init_then_push)]
pub fn compute_app_closure(
    layout: &BuildLayout,
    pkg: &PackageManifest,
    release_cfg: &ReleaseConfig,
) -> Result<AppClosure> {
    let (runtime_dir, rt_layout) = repl_startup::find_runtime_dir_with_layout()?;
    let paths = repl_startup::beam_paths_for_layout(&runtime_dir, rt_layout);

    let mut staged = Vec::new();

    // Runtime closure, in dependency order (leaves first) — staged directly
    // from [`RUNTIME_APP_NAMES`], the single declared list, so the two can
    // never drift: `beamtalk_compiler` is the one entry skipped unless
    // `include_compiler` is set.
    for &name in RUNTIME_APP_NAMES {
        if name == "beamtalk_compiler" && !release_cfg.include_compiler {
            continue;
        }
        staged.push(read_staged_app(
            name,
            &runtime_app_source_ebins(name, &paths)?,
        )?);
    }

    // ADR 0070 dependency closure — reuse the graph `beamtalk build`
    // already resolved into `_build/deps/<name>/ebin/`.
    for dep_ebin in collect_dep_ebin_paths(layout) {
        let name = dep_app_name(&dep_ebin)?;
        staged.push(read_staged_app(&name, &[dep_ebin])?);
    }

    // `[release] apps` extras — resolved from the rebar3 hex-dep lib dir
    // `beamtalk build` already populated (`_build/dev/native/default/lib/`).
    // Reserved for a dependency genuinely invisible to this closure (used
    // only via raw FFI, never named in any staged app's `{applications, …}`)
    // — a *declared* native/hex dep like `gun` doesn't need this: the
    // fixed-point loop below finds it on its own.
    for extra in &release_cfg.apps {
        let ebin = native_dep_ebin(layout, extra).ok_or_else(|| {
            miette::miette!(
                "[release] apps names '{extra}', but no compiled ebin was found at '{}'.\n\
                 Add it as a [native.dependencies] entry (or a path/git dependency) so \
                 `beamtalk build` resolves and compiles it first.",
                layout.rebar_lib_dir().join(extra).join("ebin")
            )
        })?;
        staged.push(read_staged_app(extra, &[ebin])?);
    }

    // The project's own app: the one `.rel` entry whose dependencies (via
    // `{applications, …}`) include everything above.
    staged.push(read_staged_app(&pkg.name, &[layout.ebin_dir()])?);

    let mut host_apps = vec![
        "kernel".to_string(),
        "stdlib".to_string(),
        "sasl".to_string(),
        "crypto".to_string(),
    ];
    if release_cfg.include_compiler {
        host_apps.push("compiler".to_string());
    }

    resolve_declared_deps(layout, &mut staged, &mut host_apps)?;

    Ok(AppClosure {
        host_apps,
        staged_apps: staged,
    })
}

/// Resolve every declared dependency a `staged` app names but this closure
/// hasn't accounted for yet, into either a further staged app or a
/// `host_apps` entry. Two cases:
///  - `beamtalk build` already resolved it as a rebar3 native/hex dep into
///    `_build/dev/native/default/lib/` (e.g. `gun`, declared in `http`'s own
///    generated `.app`) — stage it exactly like a `[release] apps` extra,
///    rather than assuming every unstaged declared dep is host-provided OTP.
///    A newly staged dep can itself declare further such deps (e.g. `gun` →
///    `cowlib`), so this walks `staged` to a fixed point rather than a
///    single pass.
///  - Otherwise it's genuinely host-provided OTP — e.g. `ranch` (staged)
///    declares `ssl`. `systools:make_script/2` hard-errors on any declared
///    dependency missing from the `.rel` (`{undefined_applications, …}`), so
///    seed `host_apps` with every such name; `write_rel_and_boot_script`
///    transitively resolves each seed's *own* deps live (e.g. `ssl` pulls in
///    `public_key`/`asn1`), so only the first hop needs finding here.
fn resolve_declared_deps(
    layout: &BuildLayout,
    staged: &mut Vec<StagedApp>,
    host_apps: &mut Vec<String>,
) -> Result<()> {
    let mut staged_names: std::collections::HashSet<String> =
        staged.iter().map(|a| a.name.clone()).collect();
    let mut i = 0;
    while i < staged.len() {
        let declared_deps = staged[i].declared_deps.clone();
        i += 1;
        for dep in declared_deps {
            if staged_names.contains(&dep) || host_apps.contains(&dep) {
                continue;
            }
            match native_dep_ebin(layout, &dep) {
                Some(ebin) => {
                    let app = read_staged_app(&dep, &[ebin])?;
                    staged_names.insert(app.name.clone());
                    staged.push(app);
                }
                None => host_apps.push(dep),
            }
        }
    }
    Ok(())
}

/// The rebar3-populated native/hex-dep lib dir for `name`
/// (`_build/dev/native/default/lib/<name>/ebin`), if `beamtalk build` has
/// already resolved and compiled it there — `None` if `name` isn't a native
/// dependency at all (most likely a genuine host-provided OTP app).
fn native_dep_ebin(layout: &BuildLayout, name: &str) -> Option<Utf8PathBuf> {
    let ebin = layout.rebar_lib_dir().join(name).join("ebin");
    ebin.is_dir().then_some(ebin)
}

/// The source ebin director(y/ies) for one [`RUNTIME_APP_NAMES`] entry, from
/// the runtime's own `-pa` paths (`repl_startup::beam_paths_for_layout`).
/// The only multi-source case is `beamtalk_stdlib` (see [`StagedApp::source_ebins`]).
fn runtime_app_source_ebins(
    name: &str,
    paths: &repl_startup::BeamPaths,
) -> Result<Vec<Utf8PathBuf>> {
    Ok(match name {
        "cowlib" => vec![to_utf8(&paths.cowlib_ebin)?],
        "ranch" => vec![to_utf8(&paths.ranch_ebin)?],
        "cowboy" => vec![to_utf8(&paths.cowboy_ebin)?],
        "telemetry" => vec![to_utf8(&paths.telemetry_ebin)?],
        "telemetry_poller" => vec![to_utf8(&paths.telemetry_poller_ebin)?],
        "beamtalk_runtime" => vec![to_utf8(&paths.runtime_ebin)?],
        "beamtalk_stdlib" => vec![
            to_utf8(&paths.stdlib_ebin)?,
            to_utf8(&paths.stdlib_erlang_ebin)?,
        ],
        "beamtalk_workspace" => vec![to_utf8(&paths.workspace_ebin)?],
        "beamtalk_compiler" => vec![to_utf8(&paths.compiler_ebin)?],
        other => {
            miette::bail!(
                "'{other}' is in RUNTIME_APP_NAMES but has no known source ebin — \
                 add it to `runtime_app_source_ebins`."
            )
        }
    })
}

fn to_utf8(path: &std::path::Path) -> Result<Utf8PathBuf> {
    Utf8PathBuf::from_path_buf(path.to_path_buf())
        .map_err(|p| miette::miette!("Runtime path '{}' is not valid UTF-8", p.display()))
}

/// Derive an app name from a `_build/deps/<name>/ebin` path (the directory
/// two levels up from `ebin/`).
fn dep_app_name(dep_ebin: &Utf8Path) -> Result<String> {
    dep_ebin
        .parent()
        .and_then(|p| p.file_name())
        .map(str::to_string)
        .ok_or_else(|| miette::miette!("Could not derive an app name from dep ebin '{dep_ebin}'"))
}

/// Build a [`StagedApp`] by locating `<name>.app` in one of `source_ebins`
/// (checked in order) and reading its `{vsn, …}` and `{applications, …}`
/// fields.
///
/// Deliberately not a general Erlang term parser — `.app` files are a
/// narrow, well-known shape (rebar3's own output, or `app_file.rs`'s), and
/// ADR 0125 §1.3 requires reading `vsn` from exactly this **generated**
/// file, never `.app.src`'s `{vsn, {cmd, …}}` (which `systools` cannot
/// resolve — `app_file.rs`'s output and rebar3's resolved `.app` are the
/// only valid staging sources).
///
/// Also verifies every module declared in `{modules, […]}` across *every*
/// source ebin that has an `<name>.app` file — not just the first one found
/// — has a matching `<Module>.beam` somewhere in `source_ebins`. The
/// multi-source case (e.g. `beamtalk_stdlib`) can have a *different* `.app`
/// file in each source ebin (the class-beam dir's own generated `.app`, and
/// the rebar3-built one for its Erlang FFI modules), each declaring a
/// disjoint module list — checking only the first-found `.app` would leave
/// the second one's modules (and any build defect in them) completely
/// unchecked, silently passing the exact partial-build scenario this check
/// exists to catch. `stage_one_app` copies files from every source ebin,
/// so `.beam` presence is searched across all of them too. Catches a
/// partial/stale build — a module declared but never actually compiled, or
/// compiled into a directory `stage_one_app` won't see — at the earliest
/// point that can name the exact missing module, instead of a `undef`
/// crash at boot with no indication of which staged app or module was
/// actually incomplete. See `docs/development/debugging.md` for why an
/// `undef` at boot is expensive to trace back to a staging defect
/// otherwise.
fn read_staged_app(name: &str, source_ebins: &[Utf8PathBuf]) -> Result<StagedApp> {
    let mut found = None;
    let mut all_modules: Vec<String> = Vec::new();
    for ebin in source_ebins {
        let app_path = ebin.join(format!("{name}.app"));
        if !app_path.is_file() {
            continue;
        }
        let content = std::fs::read_to_string(app_path.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read '{app_path}'"))?;
        // vsn/applications come from the *first* `.app` found only —
        // `stage_one_app`'s own precedent for which one "wins" when more
        // than one source ebin has a same-named `.app` file.
        if found.is_none() {
            let vsn = extract_erlang_string_field(&content, "vsn").ok_or_else(|| {
                miette::miette!(
                    "'{app_path}' has no `{{vsn, \"…\"}}` field — is it a valid .app file?"
                )
            })?;
            let declared_deps =
                extract_erlang_atom_list_field(&content, "applications").unwrap_or_default();
            found = Some((vsn, declared_deps, app_path));
        }
        // `{modules, […]}`, in contrast, is merged from *every* `.app`
        // found — each source ebin's `.app` only declares the modules
        // that live alongside it, so skipping any of them after the first
        // would leave that ebin's modules (and any build defect in them)
        // unchecked. Duplicates across ebins are harmless — the presence
        // check below just re-verifies the same module twice.
        all_modules.extend(extract_erlang_atom_list_field(&content, "modules").unwrap_or_default());
    }
    let (vsn, declared_deps, app_path) = found.ok_or_else(|| {
        miette::miette!(
            "Could not find '{name}.app' in any of: {}\n\
             Run `beamtalk build` first (for the project app or its \
             dependencies) or `just build` (for the runtime).",
            source_ebins
                .iter()
                .map(|p| p.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    })?;

    let missing: Vec<&String> = all_modules
        .iter()
        .filter(|module| {
            !source_ebins
                .iter()
                .any(|ebin| ebin.join(format!("{module}.beam")).is_file())
        })
        .collect();
    if !missing.is_empty() {
        miette::bail!(
            "'{name}' declares {} module(s) (across {}) with no matching `.beam` file in: {}\n\
             \x20 missing: {}\n\n\
             \x20 This is a partial or stale build, not a beamtalk-release bug — one of the \
             \x20 app's `.app` files was generated (or cached) from a build that never finished \
             \x20 compiling every module it declares. Run `just clean && just build` (or, in CI, \
             \x20 clear the cached runtime build) and retry.",
            missing.len(),
            app_path,
            source_ebins
                .iter()
                .map(|p| p.as_str())
                .collect::<Vec<_>>()
                .join(", "),
            missing
                .iter()
                .map(|m| m.as_str())
                .collect::<Vec<_>>()
                .join(", "),
        );
    }

    Ok(StagedApp {
        name: name.to_string(),
        vsn,
        source_ebins: source_ebins.to_vec(),
        declared_deps,
    })
}

/// Extract the atom names from a `{key, [App1, App2, …]}` list field in an
/// `.app` file's Erlang term text (e.g. `{applications, [kernel, stdlib,
/// ssl]}`). Each entry may be a bare atom (`kernel`) or quoted (`'ranch'`);
/// both are returned as plain strings, in list order.
fn extract_erlang_atom_list_field(content: &str, key: &str) -> Option<Vec<String>> {
    let pattern = format!("{{{key},");
    let after_key = &content[content.find(&pattern)? + pattern.len()..];
    let list_start = after_key.find('[')?;
    let list_end = after_key[list_start..].find(']')? + list_start;
    let inner = &after_key[list_start + 1..list_end];
    Some(
        inner
            .split(',')
            .map(|s| s.trim().trim_matches('\'').to_string())
            .filter(|s| !s.is_empty())
            .collect(),
    )
}

/// Extract the string value of `{key, "…"}` from an Erlang term's source
/// text. Handles `\"`/`\\` escapes; anything else is taken literally.
fn extract_erlang_string_field(content: &str, key: &str) -> Option<String> {
    let pattern = format!("{{{key},");
    let after_key = &content[content.find(&pattern)? + pattern.len()..];
    let quote_start = after_key.find('"')?;
    let mut value = String::new();
    let mut chars = after_key[quote_start + 1..].chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                if let Some(escaped) = chars.next() {
                    value.push(escaped);
                }
            }
            '"' => return Some(value),
            _ => value.push(c),
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn extract_erlang_string_field_finds_vsn() {
        let content = r#"{application, foo, [
    {description, "A package"},
    {vsn, "0.4.0-dev+38a688d"},
    {modules, []}
]}."#;
        assert_eq!(
            extract_erlang_string_field(content, "vsn").as_deref(),
            Some("0.4.0-dev+38a688d")
        );
    }

    #[test]
    fn extract_erlang_string_field_handles_escaped_quote() {
        let content = r#"{vsn, "1.0.0\"beta"},"#;
        assert_eq!(
            extract_erlang_string_field(content, "vsn").as_deref(),
            Some("1.0.0\"beta")
        );
    }

    #[test]
    fn extract_erlang_atom_list_field_reads_bare_and_quoted_atoms() {
        let content = r"{applications, [kernel, stdlib, 'ranch', ssl]},";
        assert_eq!(
            extract_erlang_atom_list_field(content, "applications"),
            Some(vec![
                "kernel".to_string(),
                "stdlib".to_string(),
                "ranch".to_string(),
                "ssl".to_string(),
            ])
        );
    }

    #[test]
    fn extract_erlang_atom_list_field_empty_list() {
        let content = r"{applications, []},";
        assert_eq!(
            extract_erlang_atom_list_field(content, "applications"),
            Some(vec![])
        );
    }

    #[test]
    fn extract_erlang_atom_list_field_missing_key_returns_none() {
        let content = r#"{vsn, "1.0.0"},"#;
        assert_eq!(
            extract_erlang_atom_list_field(content, "applications"),
            None
        );
    }

    #[test]
    fn extract_erlang_string_field_missing_key_returns_none() {
        let content = r#"{application, foo, [{description, "x"}]}."#;
        assert_eq!(extract_erlang_string_field(content, "vsn"), None);
    }

    #[test]
    fn extract_erlang_string_field_does_not_match_description_prefix() {
        // "vsn" appearing inside another field's *value* must not match —
        // only `{vsn,` (the key position) counts.
        let content = r#"{description, "the vsn field is next"}, {vsn, "9.9.9"},"#;
        assert_eq!(
            extract_erlang_string_field(content, "vsn").as_deref(),
            Some("9.9.9")
        );
    }

    #[test]
    fn read_staged_app_reads_generated_app_file_vsn_and_deps() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(
            ebin.join("orders.app").as_std_path(),
            r#"{application, orders, [{description, "d"}, {vsn, "1.4.0"}, {applications, [kernel, stdlib]}, {modules, []}]}."#,
        )
        .unwrap();
        let app = read_staged_app("orders", &[ebin]).unwrap();
        assert_eq!(app.vsn, "1.4.0");
        assert_eq!(
            app.declared_deps,
            vec!["kernel".to_string(), "stdlib".to_string()]
        );
    }

    #[test]
    fn read_staged_app_missing_applications_field_defaults_to_empty() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(
            ebin.join("orders.app").as_std_path(),
            r#"{application, orders, [{description, "d"}, {vsn, "1.4.0"}]}."#,
        )
        .unwrap();
        let app = read_staged_app("orders", &[ebin]).unwrap();
        assert!(app.declared_deps.is_empty());
    }

    #[test]
    fn read_staged_app_missing_vsn_field_errors() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(
            ebin.join("bad.app").as_std_path(),
            r#"{application, bad, [{description, "d"}]}."#,
        )
        .unwrap();
        let err = read_staged_app("bad", &[ebin]).unwrap_err();
        assert!(err.to_string().contains("no `{vsn"), "got: {err}");
    }

    #[test]
    fn is_runtime_app_recognises_the_runtime_closure() {
        for name in [
            "cowlib",
            "ranch",
            "cowboy",
            "telemetry",
            "telemetry_poller",
            "beamtalk_runtime",
            "beamtalk_stdlib",
            "beamtalk_workspace",
            "beamtalk_compiler",
        ] {
            assert!(is_runtime_app(name), "{name} should be a runtime app");
        }
    }

    #[test]
    fn is_runtime_app_rejects_project_and_dependency_apps() {
        for name in ["orders", "some_hex_dep", "beamtalk_test_support"] {
            assert!(!is_runtime_app(name), "{name} should not be a runtime app");
        }
    }

    #[test]
    fn dep_app_name_derives_from_ebin_parent() {
        let ebin = Utf8PathBuf::from("/proj/_build/deps/utils/ebin");
        assert_eq!(dep_app_name(&ebin).unwrap(), "utils");
    }

    #[test]
    fn read_staged_app_checks_source_ebins_in_order() {
        let dir = TempDir::new().unwrap();
        let first = Utf8PathBuf::from_path_buf(dir.path().join("first")).unwrap();
        let second = Utf8PathBuf::from_path_buf(dir.path().join("second")).unwrap();
        fs::create_dir_all(first.as_std_path()).unwrap();
        fs::create_dir_all(second.as_std_path()).unwrap();
        // Only the second directory actually has the .app file.
        fs::write(
            second.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}]}."#,
        )
        .unwrap();

        let app = read_staged_app("beamtalk_stdlib", &[first.clone(), second.clone()]).unwrap();
        assert_eq!(app.name, "beamtalk_stdlib");
        assert_eq!(app.vsn, "0.4.0");
        assert_eq!(app.source_ebins, vec![first, second]);
    }

    #[test]
    fn read_staged_app_accepts_when_every_declared_module_has_a_beam() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(
            ebin.join("ranch.app").as_std_path(),
            r#"{application, ranch, [{vsn, "1.8.0"}, {modules, [ranch_app, ranch_sup]}]}."#,
        )
        .unwrap();
        fs::write(ebin.join("ranch_app.beam").as_std_path(), b"beam").unwrap();
        fs::write(ebin.join("ranch_sup.beam").as_std_path(), b"beam").unwrap();

        let app = read_staged_app("ranch", &[ebin]).unwrap();
        assert_eq!(app.vsn, "1.8.0");
    }

    /// The exact failure class this check exists for: a `.app` file
    /// declaring a module that was never actually compiled/staged (a
    /// partial or stale build) — must be caught here, at build time, with
    /// the specific missing module named, rather than surfacing three
    /// layers away as a bare `undef` when the release tries to boot.
    #[test]
    fn read_staged_app_rejects_declared_module_with_no_beam_file() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        fs::write(
            ebin.join("ranch.app").as_std_path(),
            r#"{application, ranch, [{vsn, "1.8.0"}, {modules, [ranch_app, ranch_sup]}]}."#,
        )
        .unwrap();
        // Only ranch_sup.beam actually exists — ranch_app.beam is missing,
        // simulating a partial/stale build.
        fs::write(ebin.join("ranch_sup.beam").as_std_path(), b"beam").unwrap();

        let err = read_staged_app("ranch", &[ebin]).unwrap_err();
        assert!(err.to_string().contains("ranch_app"), "got: {err}");
        assert!(
            err.to_string().contains("partial or stale build"),
            "got: {err}"
        );
    }

    /// The multi-source case (`beamtalk_stdlib`): a module declared in the
    /// `.app` found in the first source ebin, but only physically present
    /// in the *second* source ebin, must still count as present — modules
    /// are searched across every source ebin, matching what
    /// `stage_one_app` actually copies from.
    #[test]
    fn read_staged_app_finds_module_beam_in_a_later_source_ebin() {
        let dir = TempDir::new().unwrap();
        let first = Utf8PathBuf::from_path_buf(dir.path().join("first")).unwrap();
        let second = Utf8PathBuf::from_path_buf(dir.path().join("second")).unwrap();
        fs::create_dir_all(first.as_std_path()).unwrap();
        fs::create_dir_all(second.as_std_path()).unwrap();
        fs::write(
            first.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}, {modules, [bt_class, beamtalk_json]}]}."#,
        )
        .unwrap();
        fs::write(first.join("bt_class.beam").as_std_path(), b"beam").unwrap();
        // beamtalk_json.beam only exists in the second source ebin.
        fs::write(second.join("beamtalk_json.beam").as_std_path(), b"beam").unwrap();

        let app = read_staged_app("beamtalk_stdlib", &[first, second]).unwrap();
        assert_eq!(app.vsn, "0.4.0");
    }

    /// The realistic multi-source shape: `beamtalk_stdlib` in a dev
    /// checkout has *two separate* `.app` files, one per source ebin — the
    /// class-beam dir's own generated `.app` (declaring only `bt@…`
    /// modules) and the rebar3-built `.app` (declaring the Erlang FFI
    /// modules, e.g. `beamtalk_json`) — each with its own disjoint
    /// `{modules, …}` list, unlike the single-`.app`-declares-both-lists
    /// synthetic fixture above. Both `.app` files' declared modules must be
    /// checked, not just the first one found.
    #[test]
    fn read_staged_app_checks_modules_from_every_apps_file_not_just_the_first() {
        let dir = TempDir::new().unwrap();
        let classes = Utf8PathBuf::from_path_buf(dir.path().join("classes")).unwrap();
        let erlang = Utf8PathBuf::from_path_buf(dir.path().join("erlang")).unwrap();
        fs::create_dir_all(classes.as_std_path()).unwrap();
        fs::create_dir_all(erlang.as_std_path()).unwrap();
        // The class-beam dir's own `.app` — found first — declares only
        // the compiled `.bt` class module.
        fs::write(
            classes.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}, {modules, [bt_stdlib_object]}]}."#,
        )
        .unwrap();
        fs::write(classes.join("bt_stdlib_object.beam").as_std_path(), b"b").unwrap();
        // The rebar3-built `.app` — a *second*, different `.app` file —
        // declares the Erlang FFI modules and is compiled/present here.
        fs::write(
            erlang.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}, {modules, [beamtalk_json, beamtalk_regex]}]}."#,
        )
        .unwrap();
        fs::write(erlang.join("beamtalk_json.beam").as_std_path(), b"b").unwrap();
        fs::write(erlang.join("beamtalk_regex.beam").as_std_path(), b"b").unwrap();

        let app = read_staged_app("beamtalk_stdlib", &[classes, erlang]).unwrap();
        assert_eq!(app.vsn, "0.4.0");
    }

    /// The exact bug the review flagged: a module missing its `.beam` but
    /// declared only in the *second* source ebin's `.app` file (not the
    /// first-found one) must still be caught — this is precisely the shape
    /// a partial/stale `beamtalk_stdlib` FFI build would take, and a check
    /// that only reads the first-found `.app` would pass it vacuously.
    #[test]
    fn read_staged_app_rejects_missing_module_declared_only_in_a_later_apps_file() {
        let dir = TempDir::new().unwrap();
        let classes = Utf8PathBuf::from_path_buf(dir.path().join("classes")).unwrap();
        let erlang = Utf8PathBuf::from_path_buf(dir.path().join("erlang")).unwrap();
        fs::create_dir_all(classes.as_std_path()).unwrap();
        fs::create_dir_all(erlang.as_std_path()).unwrap();
        fs::write(
            classes.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}, {modules, [bt_stdlib_object]}]}."#,
        )
        .unwrap();
        fs::write(classes.join("bt_stdlib_object.beam").as_std_path(), b"b").unwrap();
        // The second `.app` declares beamtalk_json, but its .beam is
        // missing — a partial rebar3 build of the FFI modules.
        fs::write(
            erlang.join("beamtalk_stdlib.app").as_std_path(),
            r#"{application, beamtalk_stdlib, [{vsn, "0.4.0"}, {modules, [beamtalk_json, beamtalk_regex]}]}."#,
        )
        .unwrap();
        fs::write(erlang.join("beamtalk_regex.beam").as_std_path(), b"b").unwrap();
        // beamtalk_json.beam intentionally not written.

        let err = read_staged_app("beamtalk_stdlib", &[classes, erlang]).unwrap_err();
        assert!(err.to_string().contains("beamtalk_json"), "got: {err}");
        assert!(
            err.to_string().contains("partial or stale build"),
            "got: {err}"
        );
    }

    #[test]
    fn read_staged_app_missing_everywhere_errors() {
        let dir = TempDir::new().unwrap();
        let ebin = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let err = read_staged_app("nope", &[ebin]).unwrap_err();
        assert!(err.to_string().contains("Could not find 'nope.app'"));
    }

    /// The exact bug this fix addresses: `http` declares `gun` in its own
    /// generated `.app` `{applications, …}` list, `beamtalk build` has
    /// already compiled `gun` into the rebar3 native lib dir, and no
    /// `[release] apps` entry names it — `gun` must still end up staged,
    /// not swept into `host_apps` where a bare `erl` can never find it.
    #[test]
    fn resolve_declared_deps_stages_a_declared_native_dep_found_in_the_rebar_lib_dir() {
        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        let gun_ebin = layout.rebar_lib_dir().join("gun").join("ebin");
        fs::create_dir_all(gun_ebin.as_std_path()).unwrap();
        fs::write(
            gun_ebin.join("gun.app").as_std_path(),
            r#"{application, gun, [{vsn, "2.1.0"}, {applications, [kernel, stdlib, ssl]}]}."#,
        )
        .unwrap();

        let mut staged = vec![StagedApp {
            name: "http".to_string(),
            vsn: "0.1.5".to_string(),
            source_ebins: vec![],
            declared_deps: vec!["kernel".to_string(), "gun".to_string()],
        }];
        let mut host_apps = vec!["kernel".to_string(), "stdlib".to_string()];

        resolve_declared_deps(&layout, &mut staged, &mut host_apps).unwrap();

        assert!(
            staged.iter().any(|a| a.name == "gun"),
            "gun should be staged, not left for host_apps: {staged:?}"
        );
        assert!(
            !host_apps.contains(&"gun".to_string()),
            "gun must not end up in host_apps: {host_apps:?}"
        );
        // gun's own declared `ssl` has no rebar lib dir entry — it's a
        // genuine host-provided OTP app, so it goes to host_apps.
        assert!(host_apps.contains(&"ssl".to_string()));
    }

    #[test]
    fn resolve_declared_deps_treats_undiscoverable_deps_as_host_apps() {
        let dir = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(dir.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);

        let mut staged = vec![StagedApp {
            name: "ranch".to_string(),
            vsn: "1.8.0".to_string(),
            source_ebins: vec![],
            declared_deps: vec!["ssl".to_string()],
        }];
        let mut host_apps = vec!["kernel".to_string()];

        resolve_declared_deps(&layout, &mut staged, &mut host_apps).unwrap();

        assert_eq!(
            staged.len(),
            1,
            "no rebar lib dir for ssl — nothing to stage"
        );
        assert!(host_apps.contains(&"ssl".to_string()));
    }

    #[test]
    fn compute_app_closure_excludes_compiler_by_default() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let layout = BuildLayout::new(&root);
        let pkg = PackageManifest {
            name: "orders".to_string(),
            version: "1.0.0".to_string(),
            description: None,
            licenses: None,
            strict_deps: false,
        };
        let cfg = ReleaseConfig::default();

        // Without a built runtime on this machine the closure computation
        // itself fails fast (no BEAMTALK_RUNTIME_DIR) — this test only
        // checks the pure `include_compiler` gating logic, so it exercises
        // `compute_app_closure` only when a runtime is discoverable; skip
        // otherwise (Rust unit tests run outside `just build`'s runtime
        // build in some contexts).
        if let Ok(closure) = compute_app_closure(&layout, &pkg, &cfg) {
            assert!(
                !closure
                    .staged_apps
                    .iter()
                    .any(|a| a.name == "beamtalk_compiler"),
                "compiler must be excluded by default"
            );
            assert!(!closure.host_apps.contains(&"compiler".to_string()));
        }
    }
}
