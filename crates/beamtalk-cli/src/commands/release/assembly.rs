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
/// `mode => release`, `console`, `bind`. A user `[release] sys-config` file,
/// if present at `<project_root>/<sys_config_path>`, is merged in: any
/// top-level app key it declares is deep-appended after ours (a later
/// duplicate key wins under `file:consult/1`'s "last one wins" reading —
/// same convention as an OTP `sys.config` overlay).
pub fn generate_sys_config(
    project_root: &Utf8Path,
    release_config_dir: &Utf8Path,
    sys_config_rel_path: &str,
    console: bool,
    bind: &str,
) -> Result<Utf8PathBuf> {
    let escaped_bind = escape_erlang_string(bind);
    let mut content = format!(
        "[\n\
         \x20 {{beamtalk_workspace, [\n\
         \x20   {{mode, release}},\n\
         \x20   {{console, {console}}},\n\
         \x20   {{bind, \"{escaped_bind}\"}},\n\
         \x20   {{auto_cleanup, false}}\n\
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
pub fn generate_vm_args(
    project_root: &Utf8Path,
    release_config_dir: &Utf8Path,
    vm_args_rel_path: &str,
    release_name: &str,
) -> Result<Utf8PathBuf> {
    let mut content = format!(
        "## Generated by `beamtalk release` — ADR 0125 §1.2/§1.6.\n\
         -mode interactive\n\
         -sname {release_name}\n\
         -kernel inet_dist_use_interface '{{127,0,0,1}}'\n\
         -start_epmd true\n"
    );

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
    let ebin_path_list = staged_ebins
        .iter()
        .map(|p| {
            format!(
                "\"{}\"",
                escape_erlang_string(&to_forward_slash(p.as_str()))
            )
        })
        .collect::<Vec<_>>()
        .join(", ");

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
pub(crate) fn absolutize(path: &Utf8Path) -> Result<Utf8PathBuf> {
    if path.is_absolute() {
        return Ok(path.to_owned());
    }
    let cwd = std::env::current_dir()
        .into_diagnostic()
        .wrap_err("Failed to read the current directory")?;
    Utf8PathBuf::from_path_buf(cwd)
        .map(|cwd| cwd.join(path))
        .map_err(|p| miette::miette!("Current directory '{}' is not valid UTF-8", p.display()))
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
        let path =
            generate_sys_config(&root, &root, "config/sys.config", false, "127.0.0.1").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{mode, release}"), "{content}");
        assert!(content.contains("{console, false}"), "{content}");
        assert!(content.contains("{bind, \"127.0.0.1\"}"), "{content}");
        assert!(content.trim_end().ends_with('.'), "{content}");
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

        let path = generate_sys_config(&root, &root, "config/sys.config", true, "0.0.0.0").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("{mode, release}"), "{content}");
        assert!(content.contains("{console, true}"), "{content}");
        assert!(content.contains("{orders, [{pool_size, 10}]}"), "{content}");
    }

    #[test]
    fn generate_sys_config_absent_user_file_is_not_an_error() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let result = generate_sys_config(&root, &root, "config/sys.config", false, "127.0.0.1");
        assert!(result.is_ok());
    }

    #[test]
    fn generate_vm_args_sets_interactive_mode_and_loopback() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let path = generate_vm_args(&root, &root, "config/vm.args", "orders").unwrap();
        let content = fs::read_to_string(path.as_std_path()).unwrap();
        assert!(content.contains("-mode interactive"), "{content}");
        assert!(content.contains("127,0,0,1"), "{content}");
        assert!(content.contains("-sname orders"), "{content}");
        // No cookie baked in.
        assert!(!content.contains("-setcookie"), "{content}");
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

        let path = generate_vm_args(&root, &root, "config/vm.args", "orders").unwrap();
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

    #[test]
    fn generate_sys_config_escapes_bind_with_embedded_quote() {
        let temp = TempDir::new().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let malicious_bind = "127.0.0.1\"}, {evil, true}, {bind, \"0.0.0.0";
        let path =
            generate_sys_config(&root, &root, "config/sys.config", false, malicious_bind).unwrap();
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
}
