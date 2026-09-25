// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Release-tree staging, `.rel`/`start.boot`/`RELEASES` assembly, and
//! `sys.config`/`vm.args` generation (ADR 0125 §1.3).
//!
//! Staging copies every [`StagedApp`](super::closure::StagedApp) into a
//! fresh `lib/<app>-<vsn>/ebin/`; `.rel`, `start.boot`
//! (`systools:make_script/2`) and `releases/RELEASES`
//! (`release_handler:create_RELEASES/4`) are produced by a single `erl
//! -noshell` invocation (no second ad-hoc spawner — CLAUDE.md) — the app
//! closure itself is entirely Rust-computed (`closure.rs`); the live node
//! is asked only for what a build machine cannot know statically: the host
//! OTP's `kernel`/`stdlib`/`sasl`/`crypto` (`+compiler`) versions and the
//! building ERTS version.

use camino::{Utf8Path, Utf8PathBuf};
use miette::{Context, IntoDiagnostic, Result};
use std::process::Command;

use beamtalk_codegen::core_erlang::escape_atom_chars;

use crate::beam_compiler::escape_erlang_string;
use crate::commands::util::to_forward_slash;

use super::closure::{AppClosure, StagedApp};

/// Copy every staged app's compiled `ebin/` contents into
/// `<release_dir>/lib/<app>-<vsn>/ebin/`.
///
/// Returns the `lib/<app>-<vsn>/ebin` directories, in closure order — the
/// `{path, …}` list `systools:make_script/2` needs. `release_dir` should
/// already be absolute (see [`absolutize`]): the returned paths are a plain
/// join under it, not `canonicalize`d, so they stay a literal string prefix
/// match for the `RELEASE_DIR` variable `write_rel_and_boot_script` derives
/// from that same `release_dir` via the same, non-canonicalizing join.
pub fn stage_apps(release_dir: &Utf8Path, closure: &AppClosure) -> Result<Vec<Utf8PathBuf>> {
    let lib_dir = release_dir.join("lib");
    let mut staged_ebins = Vec::with_capacity(closure.staged_apps.len());
    for app in &closure.staged_apps {
        staged_ebins.push(stage_one_app(&lib_dir, app)?);
    }
    Ok(staged_ebins)
}

fn stage_one_app(lib_dir: &Utf8Path, app: &StagedApp) -> Result<Utf8PathBuf> {
    let dest = lib_dir
        .join(format!("{}-{}", app.name, app.vsn))
        .join("ebin");
    std::fs::create_dir_all(dest.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create staged ebin dir '{dest}'"))?;

    // `<app.name>.app` itself is special: when an app has more than one
    // source ebin (only `beamtalk_stdlib` in a dev checkout, whose
    // rebar3-built `.app` and hand-built class-beam `.app` are two
    // different files with the same name), the *later* source's copy must
    // not silently clobber the one `closure.rs` actually read `vsn`/
    // `declared_deps` from — that would ship a `.app` whose `{modules, …}`
    // doesn't match what was staged. Stage it only from the first source
    // ebin that has it (the same one `read_staged_app` picked); every
    // other file, from every source ebin, is copied normally.
    let app_file_name = format!("{}.app", app.name);
    let mut app_file_staged = false;

    for src in &app.source_ebins {
        let entries = std::fs::read_dir(src.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read source ebin '{src}'"))?;
        for entry in entries {
            let entry = entry.into_diagnostic()?;
            if !entry.file_type().into_diagnostic()?.is_file() {
                continue;
            }
            let file_name = entry.file_name().to_string_lossy().into_owned();
            // Only `.beam` and the app's own `.app` file are runtime
            // artifacts (ADR 0125's own staged-tree shape: "the project's
            // `bt@orders@*.beam` + `orders.app`"). A project's own
            // `layout.ebin_dir()` doubles as its `.core`-compile build dir
            // (`BuildEnvironment::build_dir`), so a source ebin can also
            // hold `.core` intermediates and the generated `.erl` app
            // callback module's own source (`outputs.rs`'s "write the .erl
            // source next to the .core files") — neither belongs in a
            // release, and shipping the `.core` alongside the `.beam` it
            // compiled from previously left two files matching any
            // `*fixture_sup*` name-based lookup (`fn is_runtime_app`'s
            // sibling problem, one level down).
            let is_beam = std::path::Path::new(&file_name)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("beam"));
            if file_name != app_file_name && !is_beam {
                continue;
            }
            if file_name == app_file_name {
                if app_file_staged {
                    continue;
                }
                app_file_staged = true;
            }
            let dest_file = dest.join(&file_name);
            std::fs::copy(entry.path(), dest_file.as_std_path())
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!("Failed to stage '{}' into '{dest}'", entry.path().display())
                })?;
        }
    }

    Ok(dest)
}

/// Generate `releases/<vsn>/sys.config` — the `beamtalk_workspace`
/// application env `beamtalk_workspace_app:start/2` reads (BT-3569):
/// `mode => release`, `console`, `bind`, and (BT-3571, ADR 0125 §1.5)
/// `include_compiler` — `beamtalk_workspace_sup:starts_compiler/2` reads
/// this key to decide whether to start `beamtalk_compiler` and log the
/// boot warning naming its three risks. A user `[release] sys-config`
/// file, if present at `<project_root>/<sys_config_path>`, is merged in:
/// any top-level app key it declares is deep-appended after ours (a later
/// duplicate key wins under `file:consult/1`'s "last one wins" reading —
/// same convention as an OTP `sys.config` overlay).
///
/// `workspace_id` is set to `release_name` rather than left at
/// `beamtalk_workspace_app:env_workspace_config/1`'s fixed `<<"release">>`
/// fallback: the REPL port file
/// (`beamtalk_repl_server:write_port_file/3`) lives at
/// `~/.beamtalk/workspaces/<workspace_id>/port`, so two *different*
/// releases (distinct `-sname`s already, via `generate_vm_args`) running
/// on the same host at once would otherwise both write
/// `~/.beamtalk/workspaces/release/port`, each clobbering the other's —
/// `beamtalk workspace attach` would then discover whichever booted last,
/// not necessarily the one asked for.
pub fn generate_sys_config(
    project_root: &Utf8Path,
    release_config_dir: &Utf8Path,
    sys_config_rel_path: &str,
    release_name: &str,
    console: bool,
    bind: &str,
    include_compiler: bool,
) -> Result<Utf8PathBuf> {
    let escaped_bind = escape_erlang_string(bind);
    let escaped_name = escape_erlang_string(release_name);
    let mut content = format!(
        "[\n\
         \x20 {{beamtalk_workspace, [\n\
         \x20   {{mode, release}},\n\
         \x20   {{console, {console}}},\n\
         \x20   {{bind, \"{escaped_bind}\"}},\n\
         \x20   {{include_compiler, {include_compiler}}},\n\
         \x20   {{auto_cleanup, false}},\n\
         \x20   {{tcp_port, 0}},\n\
         \x20   {{workspace_id, <<\"{escaped_name}\">>}}\n\
         \x20 ]}}"
    );

    let user_sys_config = project_root.join(sys_config_rel_path);
    if user_sys_config.is_file() {
        let user_content = std::fs::read_to_string(user_sys_config.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read user sys-config '{user_sys_config}'"))?;
        let trimmed = user_content.trim().trim_end_matches('.').trim();
        // A well-formed sys.config is `[ {App, [...]}, ... ].` — splice the
        // inner terms in after our own entry rather than nesting two lists.
        let inner = trimmed
            .strip_prefix('[')
            .and_then(|s| s.strip_suffix(']'))
            .unwrap_or(trimmed);
        let inner = inner.trim();
        if !inner.is_empty() {
            content.push_str(",\n ");
            content.push_str(inner);
        }
    }
    content.push_str("\n].\n");

    let path = release_config_dir.join("sys.config");
    std::fs::write(path.as_std_path(), content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{path}'"))?;
    Ok(path)
}

/// Generate `releases/<vsn>/vm.args` — interactive mode, loopback-bound
/// Erlang distribution, no baked-in cookie (ADR 0125 §1.2/§1.6). A user
/// `[release] vm-args` file, if present, is appended after the generated
/// base so its flags (the last of a repeated flag wins, per `erl`'s own
/// arg-file reading) take precedence.
///
/// **No `-sname` here** — deliberately, unlike an ordinary repeated flag.
/// `erl` special-cases `-sname`/`-name`: given twice, it logs "Multiple
/// -sname given to erl, using the first" and keeps the *first* one, so a
/// value baked in here could never be overridden by anything the launcher
/// passes afterwards (verified empirically — this is not the general
/// last-wins rule the rest of this doc comment describes). The launcher
/// script computes `-sname` itself instead (`${RELEASE_NODE:-
/// $RELEASE_NAME@localhost}`, mirroring `RELEASE_COOKIE` and `mix
/// release`'s own `RELEASE_NODE`), which is what makes running more than
/// one instance of the same release on one host possible at all — same-host
/// clustering "for development, testing" is explicitly called out in ADR
/// 0126 §9.
pub fn generate_vm_args(
    project_root: &Utf8Path,
    release_config_dir: &Utf8Path,
    vm_args_rel_path: &str,
) -> Result<Utf8PathBuf> {
    let mut content = "## Generated by `beamtalk release` — ADR 0125 §1.2/§1.6.\n\
         -mode interactive\n\
         -kernel inet_dist_use_interface '{127,0,0,1}'\n\
         -start_epmd true\n"
        .to_string();

    let user_vm_args = project_root.join(vm_args_rel_path);
    if user_vm_args.is_file() {
        let user_content = std::fs::read_to_string(user_vm_args.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to read user vm-args '{user_vm_args}'"))?;
        content.push_str("\n## From ");
        content.push_str(vm_args_rel_path);
        content.push('\n');
        content.push_str(&user_content);
    }

    let path = release_config_dir.join("vm.args");
    std::fs::write(path.as_std_path(), content)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write '{path}'"))?;
    Ok(path)
}

/// Write `<name>.rel`, `start.boot` (`systools:make_script/2`) and
/// `releases/RELEASES` (`release_handler:create_RELEASES/4`) via a single
/// `erl -noshell` invocation.
///
/// `release_dir` is the release's root (`_build/release/<name>-<vsn>/`,
/// ADR 0125 §1.1); `staged_ebins` are the absolute `lib/<app>-<vsn>/ebin`
/// directories [`stage_apps`] returned, in closure order.
///
/// # Errors
///
/// Returns an error if `erl` cannot be spawned, a host app
/// (`kernel`/`stdlib`/`sasl`/`crypto`[+`compiler`]) cannot be loaded,
/// `systools:make_script/2` fails (e.g. a `.rel` omitting a declared
/// dependency), or `release_handler:create_RELEASES/4` fails.
pub fn write_rel_and_boot_script(
    release_dir: &Utf8Path,
    release_name: &str,
    release_vsn: &str,
    closure: &AppClosure,
    staged_ebins: &[Utf8PathBuf],
    include_erts: bool,
) -> Result<Utf8PathBuf> {
    // Absolutize once and derive every other path from *this* value — see
    // `absolutize`'s doc comment for why every path below has to agree,
    // string-for-string, on how it got to absolute.
    let release_dir_abs = absolutize(release_dir)?;
    let rel_dir = release_dir_abs.join("releases").join(release_vsn);
    std::fs::create_dir_all(rel_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{rel_dir}'"))?;

    let rel_file = rel_dir.join(format!("{release_name}.rel"));
    let releases_root = release_dir_abs.join("releases");

    let eval = build_assembly_eval(
        release_name,
        release_vsn,
        &closure.host_apps,
        &closure.staged_apps,
        &rel_file,
        &release_dir_abs,
        &releases_root,
        staged_ebins,
    );

    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-eval")
        .arg(&eval)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to assemble the release (is Erlang/OTP installed?)")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        miette::bail!(
            "Release assembly (.rel / start.boot / RELEASES) failed:\n{}",
            format!("{stdout}{stderr}").trim_end()
        );
    }

    // `include-erts = true` bundles the ERTS *runtime* (`copy_erts`), but a
    // release also needs the host apps `build_assembly_eval`'s `.rel`
    // declares (`kernel`/`stdlib`/`sasl`/`crypto`[+ their own transitive
    // deps]) actually present under `lib/` for the promise "the host needs
    // no Erlang at all" (ADR 0125 §3.2) to be true — `erts-<vsn>/` alone is
    // just the emulator binaries, not the OTP application libraries that
    // ship *beside* it in every real OTP install. `build_assembly_eval`
    // printed one `HOSTAPP <name> <vsn> <lib_dir>` line per resolved host
    // app (from the *live* build-machine install, the only place that
    // closure and those paths can be read) — parse it back out and stage
    // each into `lib/<app>-<vsn>/`, the same shape [`stage_apps`] gives
    // every other app.
    if include_erts {
        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout.lines() {
            let Some(rest) = line.strip_prefix("HOSTAPP ") else {
                continue;
            };
            let mut parts = rest.splitn(3, ' ');
            let (Some(name), Some(vsn), Some(lib_dir)) = (parts.next(), parts.next(), parts.next())
            else {
                continue;
            };
            let src = Utf8Path::new(lib_dir);
            if !src.is_dir() {
                // `code:lib_dir/1` was only just queried for an app that
                // `application:load`ed successfully a few lines above, so
                // this should be unreachable in practice — but if it ever
                // fires, `.rel`/`start.boot` still declares `name-vsn` (it
                // was already written above) while the release tree ends up
                // missing it under `lib/`: a build that reports success but
                // won't boot, with no `--no-include-erts` fallback to catch
                // it later. Surface it now rather than staying silent.
                eprintln!(
                    "warning: host app '{name}-{vsn}' resolved to '{lib_dir}', which is not a directory — it will be missing from the staged release tree"
                );
                continue;
            }
            let dest = release_dir_abs.join("lib").join(format!("{name}-{vsn}"));
            // Only `ebin/` (the code) and `priv/` (NIF `.so`s, certs, etc. —
            // `crypto`/`asn1`/`public_key` need theirs at runtime) — never
            // `src/`/`doc/`/`include/`/`examples/`, which a source-built OTP
            // install (kerl/asdf, common in CI/dev) carries alongside `ebin/`
            // and a release has no use for. Matches `stage_one_app`'s own
            // ebin-only staging for every other app a few functions above.
            for subdir in ["ebin", "priv"] {
                let sub_src = src.join(subdir);
                if !sub_src.is_dir() {
                    continue;
                }
                let sub_dest = dest.join(subdir);
                copy_dir_recursive(&sub_src, &sub_dest).wrap_err_with(|| {
                    format!("Failed to stage host app '{name}' ('{sub_src}') into '{sub_dest}'")
                })?;
            }
        }
    }

    Ok(rel_dir.join("start"))
}

/// Build the `erl -eval` expression that resolves the host app versions and
/// the building ERTS version, writes the `.rel`, and calls
/// `systools:make_script/2` + `release_handler:create_RELEASES/4`.
#[allow(clippy::too_many_arguments)]
fn build_assembly_eval(
    release_name: &str,
    release_vsn: &str,
    host_apps: &[String],
    staged_apps: &[StagedApp],
    rel_file: &Utf8Path,
    release_dir_abs: &Utf8Path,
    releases_root: &Utf8Path,
    staged_ebins: &[Utf8PathBuf],
) -> String {
    // Every app name below is spliced into an expression `erl -eval`
    // *evaluates* (not a string it merely reads), so each one goes through
    // `escape_atom_chars` and stays inside a quoted-atom literal (`'…'`) —
    // never a bare, unescaped identifier. `host_apps`/`staged_apps` names
    // ultimately trace back to `extract_erlang_atom_list_field`
    // (`closure.rs`), which is explicitly *not* a real Erlang term parser
    // and just splits `{applications, […]}` text on commas — a crafted
    // `.app` file from any staged dependency could otherwise smuggle
    // arbitrary Erlang code (e.g. a name like `os:cmd("...")`) into a bare
    // list literal and have it execute at build time. A quoted atom is a
    // single term no matter what's inside the quotes, so this closes that
    // off regardless of what the extractor's loose parsing lets through.
    let host_apps_list = host_apps
        .iter()
        .map(|a| format!("'{}'", escape_atom_chars(a)))
        .collect::<Vec<_>>()
        .join(", ");
    let staged_names_list = staged_apps
        .iter()
        .map(|a| format!("'{}'", escape_atom_chars(&a.name)))
        .collect::<Vec<_>>()
        .join(", ");
    let staged_apps_term = staged_apps
        .iter()
        .map(|a| {
            format!(
                "{{'{}', \"{}\"}}",
                escape_atom_chars(&a.name),
                escape_erlang_string(&a.vsn)
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    // Forward-slash every path before it goes anywhere near the eval
    // string: `Utf8Path::as_str()` carries native separators, and a native
    // Windows `\` run inside an already-quote-escaped Erlang string
    // literal is exactly the shape that miscounts under command-line
    // re-quoting between this process and `erl`'s own argv parsing
    // (verified against a real Windows CI failure — every staged `.app`
    // reported `not_found` by `systools:make_script/2`, all at once, which
    // a single shared mis-parsed path prefix explains and nothing else
    // does). OTP accepts `/`-separated paths natively on Windows, so this
    // sidesteps the whole class of backslash-requoting hazards rather than
    // trying to get the escaping exactly right through every layer — the
    // same reasoning (and the same shared `to_forward_slash` leaf) as
    // `repl_startup.rs`'s `beam_pa_args` and `run.rs`'s eval-string path
    // splicing use for the identical problem.
    let ebin_path_list = erlang_string_list(staged_ebins);

    // Host apps are resolved with a live transitive closure, not a flat
    // list: a *staged* app (e.g. `ranch`) can declare a host-only
    // dependency (`ssl`) that itself has further host-only dependencies
    // (`public_key`, `asn1`) — `closure.rs` seeds this walk with every
    // first-hop name it can see statically from staged `.app` files;
    // everything past that hop only exists on the live host OTP install,
    // so only `application:get_key/2` here can find it.
    //
    // systools names the boot script after `RelFileNoExt` (the `.rel`'s own
    // basename, e.g. `orders.boot`); ADR 0125's own release layout names
    // the file `start.boot`, so the script copies it to that fixed name —
    // what `-boot` is conventionally pointed at.
    //
    // The whole expression below is passed to `erl -eval` as a single
    // command-line argument, so it carries no real newlines — an Erlang
    // `%%` comment here would swallow every token after it (there is no
    // line end to stop at) rather than just the intended line, which is
    // why none of the comments above are duplicated inline in the string.
    format!(
        "StagedNames = [{staged_names_list}], \
         Seeds = [{host_apps_list}], \
         ResolveHost = fun ResolveHost([], _Visited, Acc) -> \
                 Acc; \
             ResolveHost([App | Rest], Visited, Acc) -> \
                 case sets:is_element(App, Visited) orelse lists:member(App, StagedNames) of \
                     true -> ResolveHost(Rest, sets:add_element(App, Visited), Acc); \
                     false -> \
                         case application:load(App) of \
                             ok -> ok; \
                             {{error, {{already_loaded, App}}}} -> ok; \
                             {{error, LoadReason}} -> \
                                 io:format(standard_error, \"error: cannot load ~p: ~p~n\", \
                                     [App, LoadReason]), \
                                 halt(1) \
                         end, \
                         {{ok, Vsn}} = application:get_key(App, vsn), \
                         Deps = case application:get_key(App, applications) of \
                             {{ok, D}} -> D; \
                             undefined -> [] \
                         end, \
                         ResolveHost(Deps ++ Rest, sets:add_element(App, Visited), \
                             [{{App, Vsn}} | Acc]) \
                 end \
         end, \
         HostAppVsns = ResolveHost(Seeds, sets:new(), []), \
         lists:foreach(fun({{HostApp, HostVsn}}) -> \
             LibDir = case code:lib_dir(HostApp) of \
                 {{error, _}} -> \"\"; \
                 Dir -> Dir \
             end, \
             io:format(\"HOSTAPP ~s ~s ~s~n\", [HostApp, HostVsn, LibDir]) \
         end, HostAppVsns), \
         StagedAppVsns = [{staged_apps_term}], \
         AllAppVsns = HostAppVsns ++ StagedAppVsns, \
         ErtsVsn = erlang:system_info(version), \
         RelTerm = {{release, {{\"{release_name}\", \"{release_vsn}\"}}, {{erts, ErtsVsn}}, AllAppVsns}}, \
         RelFileNoExt = \"{rel_file_no_ext}\", \
         ok = file:write_file(RelFileNoExt ++ \".rel\", io_lib:format(\"~p.~n\", [RelTerm])), \
         MakeResult = systools:make_script(RelFileNoExt, [{{path, [{ebin_path_list}]}}, \
             {{variables, [{{\"RELEASE_DIR\", \"{release_dir_abs}\"}}]}}, silent]), \
         case MakeResult of \
             {{ok, systools_make, _}} -> ok; \
             MakeOther -> \
                 io:format(standard_error, \"systools:make_script failed: ~p~n\", [MakeOther]), \
                 halt(1) \
         end, \
         {staged_root_path_check} \
         BootSrc = RelFileNoExt ++ \".boot\", \
         BootDst = filename:join(filename:dirname(RelFileNoExt), \"start.boot\"), \
         case file:copy(BootSrc, BootDst) of \
             {{ok, _}} -> ok; \
             CopyErr -> \
                 io:format(standard_error, \"failed to copy ~s to ~s: ~p~n\", \
                     [BootSrc, BootDst, CopyErr]), \
                 halt(1) \
         end, \
         case release_handler:create_RELEASES(\"{release_dir_abs}\", \"{releases_root}\", \
                 RelFileNoExt ++ \".rel\", []) of \
             ok -> ok; \
             RelOther -> \
                 io:format(standard_error, \"create_RELEASES failed: ~p~n\", [RelOther]), \
                 halt(1) \
         end, \
         halt(0).",
        rel_file_no_ext =
            escape_erlang_string(to_forward_slash(rel_file.as_str()).trim_end_matches(".rel")),
        release_dir_abs = escape_erlang_string(&to_forward_slash(release_dir_abs.as_str())),
        releases_root = escape_erlang_string(&to_forward_slash(releases_root.as_str())),
        release_name = escape_erlang_string(release_name),
        release_vsn = escape_erlang_string(release_vsn),
        staged_root_path_check = staged_root_path_check_eval(release_dir_abs),
    )
}

/// The `build_assembly_eval` step run right after `systools:make_script/2`:
/// fail the build if any staged app's path in the generated `.script` still
/// reads `$ROOT/lib/<app>-…`. systools silently falls back to `$ROOT`
/// whenever `RELEASE_DIR` isn't a literal string prefix of the app dir it
/// read back (see [`absolutize`]), and such a release boots under a bundled
/// ERTS (where `$ROOT` is the release dir) but crashes with an `undef` under
/// a host one. That has shipped three times — macOS's `/tmp` symlink,
/// Windows 8.3 short names, and BT-3619's `/./` segment — so it now fails
/// at build time instead. Relies on the eval's `StagedNames` and
/// `RelFileNoExt` bindings; ends in a `,` so it splices in as one step.
fn staged_root_path_check_eval(release_dir_abs: &Utf8Path) -> String {
    format!(
        "{{ok, [{{script, _, ScriptInstrs}}]}} = file:consult(RelFileNoExt ++ \".script\"), \
         RootPaths = [P || {{path, Ps}} <- ScriptInstrs, P <- Ps, \
             lists:any(fun(N) -> lists:prefix(\"$ROOT/lib/\" ++ atom_to_list(N) ++ \"-\", P) end, \
                 StagedNames)], \
         case lists:usort(RootPaths) of \
             [] -> ok; \
             BadPaths -> \
                 io:format(standard_error, \
                     \"staged apps resolved under $ROOT, not $RELEASE_DIR (~s): ~p~n\", \
                     [\"{release_dir_abs}\", BadPaths]), \
                 halt(1) \
         end,",
        release_dir_abs = escape_erlang_string(&to_forward_slash(release_dir_abs.as_str())),
    )
}

/// Make `path` absolute by joining it onto the current directory when it
/// isn't already — deliberately **not** `canonicalize`/resolving symlinks.
/// `write_rel_and_boot_script` passes the result as `{variables,
/// [{"RELEASE_DIR", …}]}`, which `systools:make_script/2` uses to rewrite
/// any staged ebin path that has it as a *literal string prefix* to
/// `$RELEASE_DIR/…`; every staged ebin path this module produces is a plain
/// join under the same `release_dir`, so both sides of that prefix match
/// have to go through this identical, non-canonicalizing join or the
/// prefix relationship silently breaks wherever the platform's temp
/// directory is itself a symlink (macOS's `/tmp` → `/private/tmp`) — caught
/// via a macOS-only CI failure (`ranch_app:start/2` undef at boot) that a
/// prior version's `Utf8Path::canonicalize_utf8()` on the staged-ebin side
/// alone, with no matching canonicalization on this side, produced.
///
/// The result is also lexically normalized the way Erlang's own
/// `filename:join/1` normalizes the application directories `systools`
/// reads back (BT-3619): `.` segments, repeated separators and a trailing
/// separator are dropped, `..` segments are kept verbatim (Erlang keeps
/// them too, and resolving them lexically would be wrong across a
/// symlink). Without this, the default `beamtalk release` output dir —
/// `./_build/release/<name>-<vsn>` joined onto the current directory —
/// yields a `RELEASE_DIR` of `/proj/./_build/…` that never string-prefixes
/// systools' `/proj/_build/…`, so every staged app falls back to
/// `$ROOT/lib/…`. That is invisible under a bundled ERTS (where `$ROOT` *is*
/// the release dir) and a `ranch_app:start/2` undef at boot under
/// `--no-include-erts` (where `$ROOT` is the host OTP install).
pub(crate) fn absolutize(path: &Utf8Path) -> Result<Utf8PathBuf> {
    let absolute = if path.is_absolute() {
        path.to_owned()
    } else {
        let cwd = std::env::current_dir()
            .into_diagnostic()
            .wrap_err("Failed to read the current directory")?;
        Utf8PathBuf::from_path_buf(cwd)
            .map(|cwd| cwd.join(path))
            .map_err(|p| {
                miette::miette!("Current directory '{}' is not valid UTF-8", p.display())
            })?
    };
    // `components()` already drops `.` segments, repeated separators and a
    // trailing separator, and keeps `..` — exactly `filename:join/1`'s rules.
    Ok(absolute.components().collect())
}

/// Probe the building machine's ERTS root directory and version via a
/// throwaway `erl -noshell` (ADR 0125 §1.3: "the ERTS tree reported by
/// `code:root_dir/0` + `erlang:system_info(version)`") — the same two calls
/// `build_assembly_eval` already makes for the `.rel`'s `{erts, ErtsVsn}`
/// tuple, factored out here because the ERTS-copy step
/// ([`copy_erts`]) needs the *root* directory too, and needs both values
/// **before** staging (the destination directory name is `erts-<vsn>/`).
///
/// # Errors
///
/// Returns an error if `erl` cannot be spawned or exits non-zero.
pub fn discover_erts_info() -> Result<(Utf8PathBuf, String)> {
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-eval")
        .arg("io:format(\"~s~n~s~n\", [code:root_dir(), erlang:system_info(version)]), halt(0).")
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to discover the ERTS root/version")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        miette::bail!(
            "Failed to discover the ERTS root/version:\n{}",
            stderr.trim_end()
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut lines = stdout.lines();
    let root_dir = lines
        .next()
        .ok_or_else(|| miette::miette!("erl produced no output discovering the ERTS root"))?;
    let erts_version = lines
        .next()
        .ok_or_else(|| miette::miette!("erl produced no output discovering the ERTS version"))?;

    Ok((Utf8PathBuf::from(root_dir), erts_version.to_string()))
}

/// Copy `<erts_root>/erts-<erts_version>` into `<release_dir>/erts-<erts_version>/`
/// (ADR 0125 §1.3, `[release] include-erts = true`, the default) — a plain
/// recursive directory copy, since this is copying a *built* ERTS tree
/// (binaries, not something to compile or stage-and-validate the way
/// [`stage_apps`] handles an application's `.app`/`.beam` set).
///
/// # Errors
///
/// Returns an error if `<erts_root>/erts-<erts_version>` does not exist, or
/// on any I/O failure while copying.
pub fn copy_erts(
    release_dir: &Utf8Path,
    erts_root: &Utf8Path,
    erts_version: &str,
) -> Result<Utf8PathBuf> {
    let src = erts_root.join(format!("erts-{erts_version}"));
    if !src.is_dir() {
        miette::bail!(
            "ERTS directory '{src}' not found — expected `code:root_dir/0` ('{erts_root}') \
             to contain an 'erts-{erts_version}' subdirectory."
        );
    }
    let dest = release_dir.join(format!("erts-{erts_version}"));
    copy_dir_recursive(&src, &dest)?;

    // `bin/<name>`'s distribution-only client verbs (`stop`/`ping`/`rpc`,
    // BT-3573) and `eval`'s separate VM need *some* boot script — `erl`
    // with no `-boot` flag falls back to `$ROOTDIR/bin/start.boot`, and
    // with the bundled ERTS this binary's own root-directory autodetection
    // resolves `$ROOTDIR` to `release_dir` (the `erts-<vsn>/bin/erl` layout
    // relx and every OTP install share), which has no such file — only
    // `releases/<vsn>/start.boot`, the *full* release boot script those
    // lightweight verbs must not run (it would start the project's own
    // application a second time). A plain kernel+stdlib install always
    // ships `start_clean.boot` at `<erts_root>/bin/start_clean.boot`
    // (verified: every OTP release, including this build machine's,
    // carries one) — copy it alongside the bundled ERTS so the launcher can
    // point `-boot` at a fixed, verb-independent path
    // (`erts-<vsn>/bin/start_clean`) without booting anything beyond
    // kernel/stdlib. Best-effort: an OTP install that somehow lacks this
    // standard file is not fatal to the release itself, only to those
    // launcher verbs, which is diagnosed there rather than failing the
    // whole build over a launcher convenience file.
    let start_clean_src = erts_root.join("bin").join("start_clean.boot");
    if start_clean_src.is_file() {
        let start_clean_dest = dest.join("bin").join("start_clean.boot");
        std::fs::copy(
            start_clean_src.as_std_path(),
            start_clean_dest.as_std_path(),
        )
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to copy '{start_clean_src}' to '{start_clean_dest}'"))?;
    }

    Ok(dest)
}

/// Recursively copy every file and subdirectory under `src` into `dest`
/// (created if absent), preserving Unix executable permissions (ERTS's
/// `bin/` binaries must stay executable) — the one recursive-copy leaf
/// [`copy_erts`] uses; nothing else in `beamtalk-cli` currently needs one, so
/// this stays private rather than moving to `path_util.rs`'s shared-leaf
/// surface pre-emptively.
fn copy_dir_recursive(src: &Utf8Path, dest: &Utf8Path) -> Result<()> {
    std::fs::create_dir_all(dest.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{dest}'"))?;
    for entry in std::fs::read_dir(src.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read '{src}'"))?
    {
        let entry = entry.into_diagnostic()?;
        let file_name = entry.file_name();
        let file_name = file_name.to_string_lossy();
        let src_path = src.join(file_name.as_ref());
        let dest_path = dest.join(file_name.as_ref());
        let file_type = entry.file_type().into_diagnostic()?;
        if file_type.is_dir() {
            copy_dir_recursive(&src_path, &dest_path)?;
        } else if file_type.is_symlink() {
            // Preserve symlinks as symlinks (ERTS trees carry a few, e.g.
            // versioned .so aliases) rather than following and duplicating
            // their target's contents.
            #[cfg(unix)]
            {
                let target = std::fs::read_link(src_path.as_std_path()).into_diagnostic()?;
                std::os::unix::fs::symlink(&target, dest_path.as_std_path())
                    .into_diagnostic()
                    .wrap_err_with(|| {
                        format!("Failed to symlink '{dest_path}' -> {}", target.display())
                    })?;
            }
            // Non-Unix targets have no symlink to preserve — `fs::copy` reads
            // through the symlink and copies its target's actual bytes.
            #[cfg(not(unix))]
            std::fs::copy(src_path.as_std_path(), dest_path.as_std_path())
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to copy '{src_path}' to '{dest_path}'"))
                .map(|_| ())?;
        } else {
            std::fs::copy(src_path.as_std_path(), dest_path.as_std_path())
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to copy '{src_path}' to '{dest_path}'"))?;
            #[cfg(unix)]
            {
                let perms = std::fs::metadata(src_path.as_std_path())
                    .into_diagnostic()?
                    .permissions();
                std::fs::set_permissions(dest_path.as_std_path(), perms).into_diagnostic()?;
            }
        }
    }
    Ok(())
}

/// Strip `debug_info` chunks from every staged application's beams
/// (`[release] strip-beams = true`, ADR 0125 §1.3/§3.3) via
/// `beam_lib:strip_release/1`, run against the already-assembled release
/// directory (it reads `releases/<vsn>/<name>.rel` to find every
/// application to strip). `__beamtalk_meta/0` survives untouched — it is a
/// compiled function, not a `debug_info` chunk (ADR 0125 §2.2's own
/// reasoning for why `beam_lib` can't *read* it, but the flip side is that
/// stripping can't touch it either).
///
/// # Errors
///
/// Returns an error if `erl` cannot be spawned or `beam_lib:strip_release/1`
/// reports a failure.
pub fn strip_release_beams(release_dir: &Utf8Path) -> Result<()> {
    let release_dir_abs = absolutize(release_dir)?;
    let eval = format!(
        "case beam_lib:strip_release(\"{dir}\") of \
             {{ok, _Modules}} -> ok; \
             StripErr -> \
                 io:format(standard_error, \"beam_lib:strip_release failed: ~p~n\", [StripErr]), \
                 halt(1) \
         end, \
         halt(0).",
        dir = escape_erlang_string(&to_forward_slash(release_dir_abs.as_str())),
    );
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-eval")
        .arg(&eval)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to strip release beams")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        miette::bail!(
            "strip-beams failed:\n{}",
            format!("{stdout}{stderr}").trim_end()
        );
    }
    Ok(())
}

/// Package the assembled release directory into `<name>-<vsn>.tar.gz` via
/// `systools:make_tar/2` (ADR 0125 §1.3). `include_erts`, when set, passes
/// `{erts, ErtsRootDir}` so `make_tar` embeds `erts-<vsn>/` in the tarball
/// itself, copying it fresh from the building host's own OTP install —
/// independent of, and not reused from, [`copy_erts`]'s separate on-disk
/// copy into the release directory (ADR 0125 §1.1's acceptance criterion
/// wants the *unpacked* directory to be bootable on its own, so that copy
/// has to exist regardless of what ships inside the tarball).
///
/// Returns the tarball's path and size in bytes.
///
/// # Errors
///
/// Returns an error if `erl` cannot be spawned, `systools:make_tar/2`
/// fails, or the resulting file cannot be stat'd.
pub fn make_tarball(
    release_dir: &Utf8Path,
    release_name: &str,
    release_vsn: &str,
    staged_ebins: &[Utf8PathBuf],
    tar_out_dir: &Utf8Path,
    include_erts: bool,
    erts_root: &Utf8Path,
) -> Result<(Utf8PathBuf, u64)> {
    let release_dir_abs = absolutize(release_dir)?;
    let rel_file_no_ext = release_dir_abs
        .join("releases")
        .join(release_vsn)
        .join(release_name);
    let ebin_path_list = erlang_string_list(staged_ebins);
    let erts_opt = if include_erts {
        format!(
            ", {{erts, \"{}\"}}",
            escape_erlang_string(&to_forward_slash(erts_root.as_str()))
        )
    } else {
        String::new()
    };
    std::fs::create_dir_all(tar_out_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{tar_out_dir}'"))?;
    let eval = format!(
        "MakeTarResult = systools:make_tar(\"{rel_file_no_ext}\", \
             [{{path, [{ebin_path_list}]}}, {{outdir, \"{tar_out_dir}\"}}{erts_opt}]), \
         case MakeTarResult of \
             ok -> ok; \
             {{ok, _Mod, _Warnings}} -> ok; \
             TarOther -> \
                 io:format(standard_error, \"systools:make_tar failed: ~p~n\", [TarOther]), \
                 halt(1) \
         end, \
         halt(0).",
        rel_file_no_ext = escape_erlang_string(&to_forward_slash(rel_file_no_ext.as_str())),
        tar_out_dir = escape_erlang_string(&to_forward_slash(tar_out_dir.as_str())),
    );
    let output = Command::new("erl")
        .arg("-noshell")
        .arg("-noinput")
        .arg("-eval")
        .arg(&eval)
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to build the release tarball")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        miette::bail!(
            "Release tarball assembly failed:\n{}",
            format!("{stdout}{stderr}").trim_end()
        );
    }

    // `systools:make_tar/2` names the tarball after `RelFileNoExt`'s
    // basename (the .rel's own name, e.g. `orders.tar.gz`) inside
    // `{outdir, tar_out_dir}` — matches ADR §1.1's
    // `_build/release/<name>-<vsn>.tar.gz` only when `release_name` already
    // *is* `<name>-<vsn>` at the `.rel`'s basename, which it is not
    // (`rel_file_no_ext`'s basename is plain `release_name`). Rename to the
    // ADR-specified name.
    let produced = tar_out_dir.join(format!("{release_name}.tar.gz"));
    let wanted = tar_out_dir.join(format!("{release_name}-{release_vsn}.tar.gz"));
    if produced != wanted {
        std::fs::rename(produced.as_std_path(), wanted.as_std_path())
            .into_diagnostic()
            .wrap_err_with(|| format!("Failed to rename '{produced}' to '{wanted}'"))?;
    }

    let size = std::fs::metadata(wanted.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat '{wanted}'"))?
        .len();

    Ok((wanted, size))
}

/// Append `releases/<vsn>/shapes.json` and `releases/<vsn>/beamtalk-
/// provenance.json` into an already-built tarball (ADR 0125 §2.3, BT-3574).
///
/// `systools:make_tar/2` archives only the modules each staged app's
/// generated `.app` declares (`{modules, […]}`) plus the `.rel`/`.boot` —
/// **not** arbitrary extra files this command writes alongside them
/// (verified: `shapes.json`/`beamtalk-provenance.json` are absent from a
/// freshly built tarball even though both exist on disk next to the `.rel`
/// they sit beside). Without this step, `beamtalk release --upgrade-from
/// <tarball>` (`upgrade.rs`) would *always* take the "previous release has
/// no shapes.json" on-the-fly-extraction fallback for a tarball input —
/// and that fallback itself depends on the tarball's `lib/*/ebin` carrying
/// every module the extractor's `beamtalk_stdlib` boot needs, which the
/// same `{modules, […]}` pruning is not guaranteed to satisfy (a module a
/// staged app never lists — e.g. one it only reaches indirectly — is
/// silently absent from the tar the same way the manifests are). Shipping
/// both manifests inside the tarball itself sidesteps that gap entirely: a
/// tarball built by *this* version of the command always carries a
/// `shapes.json` `--upgrade-from` can read directly, no fallback needed.
///
/// Implemented via the `tar`/`gzip` binaries (already a build-time
/// dependency of this command, `beamtalk-cli`'s tarball-inspection tests)
/// rather than a new Rust tar-writing crate dependency: decompress to a
/// **scratch copy** (never the original), `tar -rf` the scratch copy to
/// append the two files at their `releases/<vsn>/` path (relative to
/// `release_dir`, matching every other entry's path inside the archive),
/// recompress the scratch copy to a second scratch file, then
/// `fs::rename` that over `tar_path` only once both steps have already
/// succeeded.
///
/// **Atomic with respect to `tar_path`:** the original, already-valid
/// tarball `make_tarball` produced is read but never written to until the
/// very last step — if `tar`/`gzip` fails partway through, `tar_path`
/// still holds the original, complete tarball rather than a
/// partially-decompressed or truncated one. An in-place `gzip -d -f
/// tar_path` (deleting the original before the append/recompress steps
/// that can still fail) was the original, non-atomic shape of this
/// function; this replaces it.
///
/// # Errors
///
/// Returns an error if `gzip`/`tar` cannot be spawned, either step fails,
/// or the final rename fails — in every failure case, `tar_path` is left
/// exactly as `make_tarball` produced it.
pub fn append_shape_manifests_to_tarball(
    tar_path: &Utf8Path,
    release_dir: &Utf8Path,
    release_vsn: &str,
) -> Result<()> {
    // Both scratch files live next to `tar_path` (not a separate temp
    // dir) so the final `fs::rename` is same-filesystem and therefore
    // atomic; `.tar.gz` -> `.tar.gz.scratch`/`.tar.gz.scratch.gz` keeps
    // them visually paired with the archive they're building, and neither
    // name collides with `tar_path` itself.
    let scratch_plain = Utf8PathBuf::from(format!("{tar_path}.scratch"));
    let scratch_gz = Utf8PathBuf::from(format!("{tar_path}.scratch.gz"));
    let cleanup = |path: &Utf8Path| {
        let _ = std::fs::remove_file(path.as_std_path());
    };

    let plain_file = std::fs::File::create(scratch_plain.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create scratch file '{scratch_plain}'"))?;
    let status = Command::new("gzip")
        .arg("-d")
        .arg("-c")
        .arg(tar_path.as_std_path())
        .stdout(plain_file)
        .status()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to spawn gzip to decompress '{tar_path}'"))?;
    if !status.success() {
        cleanup(&scratch_plain);
        miette::bail!("Failed to decompress '{tar_path}' (gzip exited with {status})");
    }

    let shapes_rel = Utf8PathBuf::from("releases")
        .join(release_vsn)
        .join("shapes.json");
    let provenance_rel = Utf8PathBuf::from("releases")
        .join(release_vsn)
        .join("beamtalk-provenance.json");
    let status = Command::new("tar")
        .arg("-rf")
        .arg(scratch_plain.as_std_path())
        .arg("-C")
        .arg(release_dir.as_std_path())
        .arg(shapes_rel.as_std_path())
        .arg(provenance_rel.as_std_path())
        .status()
        .into_diagnostic()
        .wrap_err_with(|| {
            format!("Failed to spawn tar to append the manifests to '{scratch_plain}'")
        })?;
    if !status.success() {
        cleanup(&scratch_plain);
        miette::bail!(
            "Failed to append shape manifests to '{scratch_plain}' (tar exited with {status})"
        );
    }

    let gz_file = std::fs::File::create(scratch_gz.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create scratch file '{scratch_gz}'"))?;
    let status = Command::new("gzip")
        .arg("-c")
        .arg(scratch_plain.as_std_path())
        .stdout(gz_file)
        .status()
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to spawn gzip to recompress '{scratch_plain}'"))?;
    cleanup(&scratch_plain);
    if !status.success() {
        cleanup(&scratch_gz);
        miette::bail!("Failed to recompress '{scratch_plain}' (gzip exited with {status})");
    }

    std::fs::rename(scratch_gz.as_std_path(), tar_path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to move '{scratch_gz}' into place at '{tar_path}'"))?;
    Ok(())
}

/// Stage the compiler port binary (`beamtalk-compiler-port`[`.exe`]) into
/// `<release_dir>/bin/` (`[release] include-compiler = true`, ADR 0125
/// §1.3/§1.5) — `closure.rs` already stages the `beamtalk_compiler` OTP
/// application's `.beam`s; this is the other half, the native port
/// executable `beamtalk_compiler_port_server` spawns.
///
/// Discovery mirrors `startup_command.rs`'s: `BEAMTALK_COMPILER_PORT_BIN`
/// when set (a Nix/Homebrew wrapper's override), else the file next to this
/// process's own executable — the single place both look, so a build
/// running under either wrapper finds the same binary the CLI itself would
/// launch. `<release_dir>/bin/` is deliberately the same directory a future
/// launcher (`bin/<name>`, BT-3573) will occupy: `startup_command.rs`'s own
/// discovery convention (`bin_dir.join(compiler_name)`, `bin_dir` = the
/// running executable's parent) finds it there for free once that launcher
/// exists, with no new discovery code needed.
///
/// Returns `Ok(None)` (not an error) if no compiler port binary can be
/// found — the message names exactly what `startup_command.rs`'s own
/// discovery already accepts as "missing" (a dev checkout that never built
/// the port binary), so a release built with `include-compiler` in that
/// situation gets a clear staging error naming the fix, rather than an
/// opaque one only surfacing much later at boot.
///
/// # Errors
///
/// Returns an error if the binary is found but cannot be copied.
pub fn stage_compiler_port_binary(release_dir: &Utf8Path) -> Result<Utf8PathBuf> {
    let found = if let Ok(user_path) = std::env::var("BEAMTALK_COMPILER_PORT_BIN") {
        Some(Utf8PathBuf::from(user_path))
    } else {
        std::env::current_exe()
            .ok()
            .and_then(|exe| {
                let bin_dir = exe.parent()?;
                let compiler_name = if cfg!(windows) {
                    "beamtalk-compiler-port.exe"
                } else {
                    "beamtalk-compiler-port"
                };
                let candidate = bin_dir.join(compiler_name);
                candidate.is_file().then_some(candidate)
            })
            .and_then(|p| Utf8PathBuf::from_path_buf(p).ok())
    };

    let Some(src) = found else {
        miette::bail!(
            "[release] include-compiler = true, but no compiler port binary was found.\n\n\
             \x20 Set BEAMTALK_COMPILER_PORT_BIN to its path, or build it alongside the CLI \
             \x20 (cargo build --bin beamtalk-compiler-port) so it sits next to the beamtalk \
             \x20 executable."
        );
    };

    let bin_dir = release_dir.join("bin");
    std::fs::create_dir_all(bin_dir.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to create '{bin_dir}'"))?;
    let dest = bin_dir.join(src.file_name().unwrap_or("beamtalk-compiler-port"));
    std::fs::copy(src.as_std_path(), dest.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stage compiler port binary '{src}' to '{dest}'"))?;
    #[cfg(unix)]
    {
        let perms = std::fs::metadata(src.as_std_path())
            .into_diagnostic()?
            .permissions();
        std::fs::set_permissions(dest.as_std_path(), perms).into_diagnostic()?;
    }
    Ok(dest)
}

/// Write `releases/<vsn>/shapes.json` (ADR 0125 §2.2/§3.4) via
/// `beamtalk_release_shapes:write_shapes_json/4` (`beamtalk_runtime`),
/// through a single `erl -noshell` invocation — the build-time shape
/// extractor described in that module's own moduledoc.
///
/// `runtime_lib_dirs` are staged ebin dirs the extractor needs *loaded* for
/// ancestor resolution (the runtime closure: `beamtalk_runtime`,
/// `beamtalk_stdlib`, `beamtalk_workspace`, `cowboy`/`cowlib`/`ranch`/
/// `telemetry`/`telemetry_poller`, and `beamtalk_compiler` when staged) but
/// never emits a shapes.json entry for; `emit_lib_dirs` are the project's
/// own app, its ADR 0070 dependencies, and any `[release] apps` extras —
/// every class here gets an entry.
///
/// # Errors
///
/// Returns an error if `erl` cannot be spawned or the extractor reports a
/// failure (`beamtalk_stdlib` — and so `beamtalk_runtime` — could not be
/// started; nothing can be extracted without it).
pub fn write_shapes_json(
    runtime_lib_dirs: &[Utf8PathBuf],
    emit_lib_dirs: &[Utf8PathBuf],
    out_path: &Utf8Path,
    release_vsn: &str,
) -> Result<()> {
    let runtime_dirs_term = erlang_string_list(runtime_lib_dirs);
    let emit_dirs_term = erlang_string_list(emit_lib_dirs);
    let eval = format!(
        "case beamtalk_release_shapes:write_shapes_json([{runtime_dirs}], [{emit_dirs}], \
             \"{out_path}\", <<\"{vsn}\">>) of \
             ok -> ok; \
             ShapesErr -> \
                 io:format(standard_error, \"shapes.json extraction failed: ~p~n\", [ShapesErr]), \
                 halt(1) \
         end, \
         halt(0).",
        runtime_dirs = runtime_dirs_term,
        emit_dirs = emit_dirs_term,
        out_path = escape_erlang_string(&to_forward_slash(out_path.as_str())),
        vsn = escape_erlang_string(release_vsn),
    );
    // `beamtalk_release_shapes` itself (and the modules it calls —
    // `beamtalk_module_activation`, `beamtalk_shape_migration`,
    // `beamtalk_class_metadata`) live in the staged `beamtalk_runtime` ebin,
    // one of `runtime_lib_dirs` — but that directory is only *known to the
    // eval string*, not yet on this fresh `erl` process's own code path, so
    // the call inside the eval would itself be `undef` without these `-pa`
    // flags. The eval's own `code:add_pathz` calls (inside
    // `extract_shapes/2`) are for the modules that get *loaded and
    // registered* during extraction; this `-pa` is for the extractor module
    // itself, needed before the eval string can even start running.
    let mut cmd = Command::new("erl");
    cmd.arg("-noshell").arg("-noinput");
    for dir in runtime_lib_dirs {
        cmd.arg("-pa").arg(dir.as_std_path());
    }
    cmd.arg("-eval").arg(&eval);
    let output = cmd
        .output()
        .into_diagnostic()
        .wrap_err("Failed to run erl to extract shapes.json (is the runtime built?)")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        miette::bail!(
            "shapes.json extraction failed:\n{}",
            format!("{stdout}{stderr}").trim_end()
        );
    }
    Ok(())
}

/// Render `dirs` as a comma-separated Erlang string-literal list body (no
/// enclosing brackets — callers splice it directly into a `[…]` list
/// literal), forward-slashed and escaped exactly as `ebin_path_list` above.
fn erlang_string_list(dirs: &[Utf8PathBuf]) -> String {
    dirs.iter()
        .map(|p| {
            format!(
                "\"{}\"",
                escape_erlang_string(&to_forward_slash(p.as_str()))
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Resolve `path` (which must already exist — call this only after
/// `create_dir_all`, never in place of [`absolutize`], which deliberately
/// runs *before* the release directory exists) to the OS's own long-path
/// form, undoing any Windows 8.3 short-name component such as `RUNNER~1`
/// for `runneradmin`.
///
/// This is **not** the same mistake `absolutize`'s doc comment warns
/// about — a *second*, independently-derived absolute form of the same
/// directory breaking the literal string-prefix match
/// `systools:make_script/2`'s `RELEASE_DIR` substitution depends on. That
/// bug came from canonicalizing on only *one* side of the prefix
/// relationship (the staged-ebin side) while leaving the other (the
/// `RELEASE_DIR` variable itself) un-canonicalized, so a symlinked temp
/// dir (macOS's `/tmp` → `/private/tmp`) matched on one side and not the
/// other. Here there is only one call site: `release_dir` is rebound to
/// this resolved form immediately, in `run`, before *anything* downstream
/// (staging, `.rel`/`start.boot` writing) derives a path from it — so
/// every path this module produces already carries the resolved form, the
/// same discipline `absolutize` itself uses, just with a resolution step
/// that has to wait until the directory exists.
///
/// Why it's needed at all: `systools:make_script/2`'s `path` option is
/// searched via `filelib:wildcard/1` and `file:path_open/3`, which read
/// the actual directory entries on disk — on Windows CI runners, whose
/// `%TEMP%` resolves to a short-name path (`C:\Users\RUNNER~1\...`) by
/// default, that filesystem read silently returns the *long*-name form
/// (`C:\Users\runneradmin\...`) for `App#application.dir`. Our
/// `RELEASE_DIR` variable, passed straight through as the short-name
/// string, then fails the boot-script generator's literal
/// `lists:prefix/2` check against that long-name directory, and every
/// staged (non-host) application silently falls back to the default
/// `$ROOT/lib/App-Vsn/ebin` — a directory that doesn't exist, since these
/// apps were never part of the host OTP install — producing an `undef` at
/// boot for whichever staged `permanent`-type application starts first
/// (`ranch_app:start/2` in the fixture release, since `ranch` is early in
/// `cowboy`'s dependency chain). Resolving to the long-path form up front
/// makes both sides of that prefix check agree on the short-vs-long-name
/// component, since the filesystem-read path already converges there — a
/// second, independent mismatch on the drive letter's *case*
/// (`std::fs::canonicalize` uppercases it; Erlang's own path resolution
/// lowercases it) is handled separately, just below.
#[cfg(windows)]
pub(crate) fn resolve_long_path(path: &Utf8Path) -> Result<Utf8PathBuf> {
    let canonical = std::fs::canonicalize(path.as_std_path())
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to resolve the long-path form of '{path}'"))?;
    let canonical = canonical.to_string_lossy();
    // `std::fs::canonicalize` on Windows returns the `\\?\`-prefixed
    // "verbatim" form; strip it back to an ordinary drive-letter path (or
    // UNC path) so it still looks like — and string-prefixes the same way
    // as — every other path this module produces.
    let stripped = canonical
        .strip_prefix(r"\\?\UNC\")
        .map(|rest| format!(r"\\{rest}"))
        .or_else(|| canonical.strip_prefix(r"\\?\").map(str::to_owned))
        .unwrap_or_else(|| canonical.clone().into_owned());
    // `std::fs::canonicalize` normalizes the drive letter to *uppercase*
    // (`C:\...`); Erlang's own `file:path_open/3` (what
    // `systools_make.erl` uses to resolve `App#application.dir` — see the
    // doc comment above) normalizes it to *lowercase* (`c:/...`) instead,
    // confirmed against a real Windows CI failure's `DEBUG systools
    // path-open probe` output. `lists:prefix/2` is a plain
    // case-sensitive character comparison, so a single-character case
    // mismatch on the drive letter alone is enough to fail the match and
    // trigger the same `$ROOT` fallback this whole function exists to
    // avoid. Lowercase it here so this side agrees with what Erlang's
    // path resolution reports, matching the convention Erlang itself
    // uses rather than the one `std::fs` uses.
    let lowercased_drive = match stripped.as_bytes() {
        [drive @ (b'A'..=b'Z' | b'a'..=b'z'), b':', ..] => {
            format!(
                "{}{}",
                (*drive as char).to_ascii_lowercase(),
                &stripped[1..]
            )
        }
        _ => stripped,
    };
    Utf8PathBuf::from_path_buf(std::path::PathBuf::from(lowercased_drive))
        .map_err(|p| miette::miette!("Resolved path '{}' is not valid UTF-8", p.display()))
}

/// Every other platform: a no-op. Only Windows's default-short-name
/// `%TEMP%` behaviour (see the `#[cfg(windows)]` doc comment above) needs
/// this resolved at all — `absolutize`'s plain join is already sufficient
/// everywhere else, matching `absolutize`'s own non-canonicalizing
/// discipline.
#[cfg(not(windows))]
#[allow(clippy::unnecessary_wraps)] // signature must match the `#[cfg(windows)]` version above
pub(crate) fn resolve_long_path(path: &Utf8Path) -> Result<Utf8PathBuf> {
    Ok(path.to_owned())
}

#[cfg(test)]
mod tests {
    use super::super::closure::StagedApp;
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn app(name: &str, vsn: &str, ebin: &Utf8Path) -> StagedApp {
        StagedApp {
            name: name.to_string(),
            vsn: vsn.to_string(),
            source_ebins: vec![ebin.to_owned()],
            declared_deps: Vec::new(),
        }
    }

    /// BT-3619: the default output dir (`./_build/release/<name>-<vsn>`)
    /// must not keep its `.` segment once absolutized — systools compares
    /// `RELEASE_DIR` against `filename:join/1`-normalized app dirs, which
    /// never contain one. Compared via `as_str`: `Utf8Path`'s own `==`
    /// normalizes `.` away and would hide exactly this bug.
    #[test]
    fn absolutize_drops_cur_dir_segments_from_a_relative_path() {
        let cwd = Utf8PathBuf::from_path_buf(std::env::current_dir().unwrap()).unwrap();
        let abs = absolutize(Utf8Path::new("./_build/release/app-0.1.0")).unwrap();
        let expected = cwd.join("_build").join("release").join("app-0.1.0");
        assert_eq!(abs.as_str(), expected.as_str());
    }

    #[test]
    fn absolutize_normalizes_an_absolute_path_like_erlang_filename_join() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        let messy = Utf8PathBuf::from(format!("{root}/./a//b/./"));
        let expected = root.join("a").join("b");
        assert_eq!(absolutize(&messy).unwrap().as_str(), expected.as_str());
    }

    #[test]
    fn absolutize_keeps_parent_dir_segments() {
        let tmp = TempDir::new().unwrap();
        let root = Utf8Path::from_path(tmp.path()).unwrap();
        let with_parent = root.join("a").join("..").join("b");
        assert_eq!(
            absolutize(&with_parent).unwrap().as_str(),
            with_parent.as_str()
        );
    }

    #[test]
    fn stage_apps_copies_files_into_lib_app_vsn_ebin() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src_ebin");
        fs::create_dir_all(src.as_std_path()).unwrap();
        fs::write(src.join("orders.app").as_std_path(), "app content").unwrap();
        fs::write(src.join("bt@orders@main.beam").as_std_path(), b"beam").unwrap();

        let release_dir = root.join("release");
        let closure = AppClosure {
            host_apps: vec!["kernel".to_string()],
            staged_apps: vec![app("orders", "1.0.0", &src)],
        };
        let ebins = stage_apps(&release_dir, &closure).unwrap();

        assert_eq!(ebins.len(), 1);
        let dest = release_dir.join("lib").join("orders-1.0.0").join("ebin");
        assert!(dest.join("orders.app").is_file());
        assert!(dest.join("bt@orders@main.beam").is_file());
        assert_eq!(
            fs::read_to_string(dest.join("orders.app").as_std_path()).unwrap(),
            "app content"
        );
    }

    /// The exact shape a project's own build produces: `layout.ebin_dir()`
    /// doubles as the `.core` compile output dir (`BuildEnvironment::build_dir`)
    /// and holds the generated `.app` callback module's `.erl` source
    /// alongside the `.beam`/`.app` a release actually needs — neither
    /// intermediate belongs in the staged tree (ADR 0125's staged-tree shape
    /// is "the project's `bt@orders@*.beam` + `orders.app`" only).
    #[test]
    fn stage_apps_excludes_core_and_erl_intermediates() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src_ebin");
        fs::create_dir_all(src.as_std_path()).unwrap();
        fs::write(src.join("orders.app").as_std_path(), "app content").unwrap();
        fs::write(src.join("bt@orders@main.beam").as_std_path(), b"beam").unwrap();
        fs::write(
            src.join("bt@orders@main.core").as_std_path(),
            b"core source",
        )
        .unwrap();
        fs::write(
            src.join("beamtalk_orders_app.erl").as_std_path(),
            b"erl source",
        )
        .unwrap();

        let release_dir = root.join("release");
        let closure = AppClosure {
            host_apps: vec!["kernel".to_string()],
            staged_apps: vec![app("orders", "1.0.0", &src)],
        };
        stage_apps(&release_dir, &closure).unwrap();

        let dest = release_dir.join("lib").join("orders-1.0.0").join("ebin");
        assert!(dest.join("orders.app").is_file());
        assert!(dest.join("bt@orders@main.beam").is_file());
        assert!(
            !dest.join("bt@orders@main.core").exists(),
            ".core intermediate must not be staged into a release"
        );
        assert!(
            !dest.join("beamtalk_orders_app.erl").exists(),
            ".erl source must not be staged into a release"
        );
    }

    #[test]
    fn stage_apps_merges_multiple_source_ebins() {
        // Mirrors beamtalk_stdlib in a dev checkout: class beams in one
        // directory, the rebar3-built .app in another.
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let classes = root.join("classes_ebin");
        let erlang = root.join("erlang_ebin");
        fs::create_dir_all(classes.as_std_path()).unwrap();
        fs::create_dir_all(erlang.as_std_path()).unwrap();
        fs::write(classes.join("bt@stdlib@object.beam").as_std_path(), b"c").unwrap();
        // Both source ebins carry a `beamtalk_stdlib.app` — the class-beam
        // dir's copy (with `bt@stdlib@*` in {modules, …}) must win, since
        // that is the one `read_staged_app` reads `vsn`/`declared_deps`
        // from when it is listed first, as it is in real closures.
        fs::write(
            classes.join("beamtalk_stdlib.app").as_std_path(),
            "class-beam app with bt@stdlib@* modules",
        )
        .unwrap();
        fs::write(
            erlang.join("beamtalk_stdlib.app").as_std_path(),
            "rebar3-built app without bt@stdlib@* modules",
        )
        .unwrap();
        fs::write(erlang.join("beamtalk_stdlib_json.beam").as_std_path(), b"n").unwrap();

        let release_dir = root.join("release");
        let closure = AppClosure {
            host_apps: vec![],
            staged_apps: vec![StagedApp {
                name: "beamtalk_stdlib".to_string(),
                vsn: "0.4.0".to_string(),
                source_ebins: vec![classes, erlang],
                declared_deps: Vec::new(),
            }],
        };
        stage_apps(&release_dir, &closure).unwrap();

        let dest = release_dir
            .join("lib")
            .join("beamtalk_stdlib-0.4.0")
            .join("ebin");
        assert!(dest.join("bt@stdlib@object.beam").is_file());
        assert!(dest.join("beamtalk_stdlib_json.beam").is_file());
        assert!(dest.join("beamtalk_stdlib.app").is_file());
        // The first source ebin's .app wins — the one carrying the
        // `bt@stdlib@*` modules `read_staged_app` actually read from.
        assert_eq!(
            fs::read_to_string(dest.join("beamtalk_stdlib.app").as_std_path()).unwrap(),
            "class-beam app with bt@stdlib@* modules"
        );
    }

    #[test]
    fn stage_apps_dev_version_directory_name_has_plus_and_hyphens() {
        // ADR 0125 §1.3: `lib/beamtalk_runtime-0.4.0-dev+38a688d/ebin` must
        // be a valid directory name — verified, not assumed.
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let src = root.join("src_ebin");
        fs::create_dir_all(src.as_std_path()).unwrap();
        fs::write(src.join("beamtalk_runtime.app").as_std_path(), "x").unwrap();

        let release_dir = root.join("release");
        let closure = AppClosure {
            host_apps: vec![],
            staged_apps: vec![app("beamtalk_runtime", "0.4.0-dev+38a688d", &src)],
        };
        stage_apps(&release_dir, &closure).unwrap();

        assert!(
            release_dir
                .join("lib")
                .join("beamtalk_runtime-0.4.0-dev+38a688d")
                .join("ebin")
                .join("beamtalk_runtime.app")
                .is_file()
        );
    }

    #[test]
    fn generate_sys_config_sets_release_mode() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            false,
            "127.0.0.1",
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{mode, release}"), "{content}");
        assert!(content.contains("{console, false}"), "{content}");
        assert!(content.contains("{bind, \"127.0.0.1\"}"), "{content}");
        assert!(content.contains("{include_compiler, false}"), "{content}");
        assert!(content.trim_end().ends_with('.'), "{content}");
    }

    /// The exact bug this guards against: `beamtalk_workspace_sup:init/1`
    /// hard-crashes at boot with `{bad_config, missing_tcp_port_for_repl}`
    /// whenever `console => true` and `tcp_port` is unset — verified by
    /// actually booting a `console = true` release, not just by reading the
    /// supervisor's init clause. `0` (OS-assigned ephemeral port,
    /// discovered via the REPL server's port file) matches dev/workspace
    /// mode's own default.
    #[test]
    fn generate_sys_config_sets_tcp_port_so_console_mode_can_boot() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            true,
            "127.0.0.1",
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{tcp_port, 0}"), "{content}");
    }

    /// The exact bug this guards against: `workspace_id` defaulted to the
    /// fixed string `"release"` for every release-mode node
    /// (`beamtalk_workspace_app:env_workspace_config/1`), so two
    /// *different* releases (`orders`, `symphony`) running on the same
    /// host at once would both write their REPL port to
    /// `~/.beamtalk/workspaces/release/port`, each clobbering the other's.
    #[test]
    fn generate_sys_config_sets_workspace_id_to_the_release_name() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "symphony",
            true,
            "127.0.0.1",
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(
            content.contains("{workspace_id, <<\"symphony\">>}"),
            "{content}"
        );
    }

    #[test]
    fn generate_sys_config_sets_include_compiler() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            false,
            "127.0.0.1",
            true,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{include_compiler, true}"), "{content}");
    }

    #[test]
    fn generate_sys_config_merges_user_file() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let user_dir = root.join("config");
        fs::create_dir_all(user_dir.as_std_path()).unwrap();
        fs::write(
            user_dir.join("sys.config").as_std_path(),
            "[{orders, [{pool_size, 10}]}].",
        )
        .unwrap();

        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            true,
            "0.0.0.0",
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{mode, release}"), "{content}");
        assert!(content.contains("{console, true}"), "{content}");
        assert!(content.contains("{orders, [{pool_size, 10}]}"), "{content}");
    }

    #[test]
    fn generate_sys_config_absent_user_file_is_not_an_error() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let result = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            false,
            "127.0.0.1",
            false,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn generate_vm_args_sets_interactive_mode_and_loopback() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_vm_args(&root, &root, "config/vm.args").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("-mode interactive"), "{content}");
        assert!(content.contains("127,0,0,1"), "{content}");
        // No cookie baked in.
        assert!(!content.contains("-setcookie"), "{content}");
    }

    /// The exact bug this guards against: baking `-sname` in here made it
    /// impossible to override — `erl` uses the *first* `-sname` it sees and
    /// warns on a duplicate, so any value the launcher passed afterwards
    /// (to support running two instances of the same release, via
    /// `RELEASE_NODE`) would have been silently ignored. `-sname` must be
    /// computed by the launcher script instead, never written here.
    #[test]
    fn generate_vm_args_never_bakes_in_sname() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_vm_args(&root, &root, "config/vm.args").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(!content.contains("-sname"), "{content}");
        assert!(!content.contains("-name "), "{content}");
    }

    #[test]
    fn generate_vm_args_appends_user_file() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let user_dir = root.join("config");
        fs::create_dir_all(user_dir.as_std_path()).unwrap();
        fs::write(
            user_dir.join("vm.args").as_std_path(),
            "+K true\n-env ERL_MAX_PORTS 8192\n",
        )
        .unwrap();

        let path = generate_vm_args(&root, &root, "config/vm.args").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("-mode interactive"), "{content}");
        assert!(content.contains("+K true"), "{content}");
        assert!(content.contains("ERL_MAX_PORTS"), "{content}");
    }

    #[test]
    fn build_assembly_eval_contains_expected_calls() {
        let closure_apps = vec![StagedApp {
            name: "orders".to_string(),
            vsn: "1.0.0".to_string(),
            source_ebins: vec![],
            declared_deps: Vec::new(),
        }];
        let eval = build_assembly_eval(
            "orders",
            "1.0.0",
            &["kernel".to_string(), "stdlib".to_string()],
            &closure_apps,
            &Utf8PathBuf::from("/rel/releases/1.0.0/orders.rel"),
            &Utf8PathBuf::from("/rel"),
            &Utf8PathBuf::from("/rel/releases"),
            &[Utf8PathBuf::from("/rel/lib/orders-1.0.0/ebin")],
        );
        assert!(eval.contains("systools:make_script"), "{eval}");
        assert!(eval.contains("release_handler:create_RELEASES"), "{eval}");
        assert!(eval.contains("\"start.boot\""), "{eval}");
        assert!(eval.contains("RELEASE_DIR"), "{eval}");
        // BT-3619: the post-make_script `$ROOT` fallback guard runs, and
        // runs before `start.boot` is copied from the checked script.
        let guard = eval.find("\"$ROOT/lib/\"").expect(&eval);
        assert!(guard > eval.find("systools:make_script").unwrap(), "{eval}");
        assert!(guard < eval.find("\"start.boot\"").unwrap(), "{eval}");
        assert!(eval.contains("'orders'"), "{eval}");
        assert!(eval.contains("Seeds = ['kernel', 'stdlib']"), "{eval}");
        assert!(eval.contains("StagedNames = ['orders']"), "{eval}");
    }

    /// A host app name is never allowed to end up as a *bare* identifier in
    /// the generated `Seeds = […]` list literal `erl -eval` evaluates. Every
    /// entry `extract_erlang_atom_list_field` (`closure.rs`) can hand back
    /// — which is explicitly not a real Erlang term parser and just splits
    /// text on commas — must land inside a quoted-atom literal, with any
    /// embedded quote/backslash escaped, so a crafted `.app` file
    /// (`{applications, [kernel, 'os:cmd("pwned")']}`) can never smuggle a
    /// second, executable expression into that list.
    #[test]
    fn build_assembly_eval_quotes_and_escapes_malicious_host_app_name() {
        // The dangerous character for a *quoted atom* is `'` (the atom
        // delimiter) — an unescaped one lets a crafted name close the atom
        // early and splice a second, executable term into the `Seeds =
        // […]` list literal `erl -eval` evaluates. `kernel'], os:cmd(...` is
        // exactly that shape: naively spliced in bare, `Seeds = [kernel'],
        // os:cmd("pwned"), ['stdlib]` would parse as two list elements, the
        // second one a live function call.
        let malicious = "kernel'], os:cmd(\"pwned\"), ['stdlib".to_string();
        let eval = build_assembly_eval(
            "orders",
            "1.0.0",
            std::slice::from_ref(&malicious),
            &[],
            &Utf8PathBuf::from("/rel/releases/1.0.0/orders.rel"),
            &Utf8PathBuf::from("/rel"),
            &Utf8PathBuf::from("/rel/releases"),
            &[],
        );
        // Every `'` the malicious name contributes must come out escaped
        // (`\'`), so none of them terminates the enclosing quoted atom —
        // i.e. the *exact* escaped form is the only way this name appears.
        assert!(
            eval.contains(&format!("'{}'", escape_atom_chars(&malicious))),
            "malicious host app name must be a single escaped quoted atom: {eval}"
        );
        // The raw name (with its live, un-escaped `'` characters) must not
        // appear anywhere — that would mean it broke out of the atom.
        assert!(!eval.contains(&malicious), "{eval}");
    }

    /// The staged-app equivalent of the above: a crafted `.app` naming a
    /// dependency with an embedded `'` must stay inside a quoted-atom
    /// literal rather than breaking out of it.
    #[test]
    fn build_assembly_eval_quotes_and_escapes_malicious_staged_app_name() {
        let malicious_name = "orders', os:cmd(\"pwned\"), 'x".to_string();
        let closure_apps = vec![StagedApp {
            name: malicious_name.clone(),
            vsn: "1.0.0".to_string(),
            source_ebins: vec![],
            declared_deps: Vec::new(),
        }];
        let eval = build_assembly_eval(
            "orders",
            "1.0.0",
            &[],
            &closure_apps,
            &Utf8PathBuf::from("/rel/releases/1.0.0/orders.rel"),
            &Utf8PathBuf::from("/rel"),
            &Utf8PathBuf::from("/rel/releases"),
            &[],
        );
        assert!(
            eval.contains(&format!("'{}'", escape_atom_chars(&malicious_name))),
            "malicious staged app name must be a single escaped quoted atom: {eval}"
        );
        assert!(!eval.contains(&malicious_name), "{eval}");
    }

    /// Defense-in-depth: even though `build_release` (`mod.rs`) now refuses
    /// a `[release] name`/`[package] version` containing `"` before this
    /// function ever runs (`validate_release_path_component`), the splice
    /// site itself must also escape them — the same belt-and-suspenders
    /// treatment `host_apps`/`staged_apps`/`bind` already got, in case a
    /// caller ever reaches this function with an unvalidated value.
    #[test]
    fn build_assembly_eval_escapes_release_name_and_vsn_with_embedded_quote() {
        let malicious_name = "orders\", {evil, true}, {x, \"";
        let malicious_vsn = "1.0.0\", {evil, true}, {y, \"";
        let eval = build_assembly_eval(
            malicious_name,
            malicious_vsn,
            &[],
            &[],
            &Utf8PathBuf::from("/rel/releases/1.0.0/orders.rel"),
            &Utf8PathBuf::from("/rel"),
            &Utf8PathBuf::from("/rel/releases"),
            &[],
        );
        assert!(
            eval.contains(&format!(
                "{{release, {{\"{}\", \"{}\"}}",
                escape_erlang_string(malicious_name),
                escape_erlang_string(malicious_vsn)
            )),
            "release name/vsn must be escaped inside the .rel term: {eval}"
        );
        assert!(!eval.contains(malicious_name), "{eval}");
        assert!(!eval.contains(malicious_vsn), "{eval}");
    }

    #[test]
    fn generate_sys_config_escapes_bind_with_embedded_quote() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let malicious_bind = "127.0.0.1\"}, {evil, true}, {bind, \"0.0.0.0";
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            "orders",
            false,
            malicious_bind,
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        // The embedded `"` must come out escaped (`\"`), so the whole
        // malicious value stays inert text inside one string literal
        // instead of closing it early and splicing a live `{evil, true}`
        // tuple into the `beamtalk_workspace` env list. The two `\"`
        // occurrences either side of `{evil, true}` are exactly what keep
        // it textual: a real Erlang reader sees one unbroken string, not a
        // string that ends before `{evil, true}` and a new term starting
        // after it.
        assert!(
            content.contains("127.0.0.1\\\"}, {evil, true}, {bind, \\\"0.0.0.0"),
            "bind value must be escaped as a single string literal: {content}"
        );
    }

    #[test]
    fn generate_sys_config_escapes_release_name_with_embedded_quote() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let malicious_name = "orders\">>}, {evil, true}, {workspace_id, <<\"orders";
        let path = generate_sys_config(
            &root,
            &root,
            "config/sys.config",
            malicious_name,
            false,
            "127.0.0.1",
            false,
        )
        .unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(
            content.contains("orders\\\">>}, {evil, true}, {workspace_id, <<\\\"orders"),
            "release name must be escaped as a single binary literal: {content}"
        );
    }
}
