%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_load).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Op handlers for load-source, load-project, and unload operations, plus the
section and native-source authoring writes (`save-section`,
`save-native-source`).

Project sync, native `.erl` recompilation and build-stamp validation live in
`beamtalk_workspace_project_sync`; these handlers call into it.

Extracted from beamtalk_repl_server.
The deprecated ops `load-file`, `reload`, and `modules` were removed —
use `Workspace load:`, `ClassName reload`, and `Workspace classes`
respectively.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, resolve_class_to_module/1, resolve_module_atoms/2]).

%% Export internals for white-box testing of the authoring helpers.
-ifdef(TEST).
-export([
    finish_section_write/4,
    atomic_write_file/2,
    atomic_write_file/3
]).
-endif.

-doc """
Handle load-source/load-project/unload ops for the WebSocket transport —
encodes the term result to JSON at the edge.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for load-source/load-project/unload (ADR 0082
write-surface). Returns `{loaded, Classes, Warnings}`,
`{load_project, Classes, Errors, Summary, Warnings}`, `{ok, Value, Output,
Warnings}` (unload), or `{error, #beamtalk_error{}}` — no JSON in this path.
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"load-project">>, Params, _Msg, SessionPid) ->
    PathBin = maps:get(<<"path">>, Params, <<".">>),
    Path = binary_to_list(PathBin),
    IncludeTests = maps:get(<<"include_tests">>, Params, false),
    Force = maps:get(<<"force">>, Params, false),
    Options = #{
        include_tests => IncludeTests,
        force => Force,
        session_pid => SessionPid,
        package_name => maps:get(<<"package_name">>, Params, undefined)
    },
    case beamtalk_workspace_project_sync:sync_project(Path, Options) of
        {error, Err} ->
            {error, Err};
        {ok, Result} ->
            DepErrors = maps:get(dep_errors, Result, []),
            %% Surface collision warnings to the load-project caller
            %% so that cross-project class collisions produce a clear
            %% diagnostic instead of silent eviction.
            Warnings = maps:get(warnings, Result, []),
            {load_project, maps:get(classes, Result), maps:get(errors, Result) ++ DepErrors,
                maps:get(summary, Result), Warnings}
    end;
handle_term(<<"load-source">>, Params, _Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            {error,
                beamtalk_repl_errors:make(
                    empty_expression,
                    'REPL',
                    <<"Empty source">>,
                    <<"Enter Beamtalk source code to compile.">>
                )};
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    Warnings = beamtalk_workspace_project_sync:collect_load_warnings(Classes),
                    {loaded, Classes, Warnings};
                {error, Reason} ->
                    {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
            end
    end;
handle_term(<<"unload">>, Params, _Msg, SessionPid) ->
    %% Restore unload op — fully removes class from system (actors, gen_server,
    %% BEAM module, workspace_meta, session tracker).
    ClassNameBin = maps:get(<<"module">>, Params, <<>>),
    case beamtalk_repl_errors:safe_to_existing_atom(ClassNameBin) of
        {error, badarg} ->
            {error,
                beamtalk_repl_errors:make(
                    class_not_found,
                    'REPL',
                    iolist_to_binary([<<"Class not found: '">>, ClassNameBin, <<"'">>])
                )};
        {ok, ClassName} ->
            case beamtalk_runtime_api:remove_class_from_system(ClassName) of
                {ok, ModuleName} ->
                    %% Clean up workspace-level and session-level tracking.
                    beamtalk_workspace_meta:unregister_module(ModuleName),
                    beamtalk_repl_shell:remove_from_tracker(SessionPid, ModuleName),
                    {ok,
                        iolist_to_binary([
                            <<"Class '">>, ClassNameBin, <<"' removed from system">>
                        ]), <<>>, []};
                {error, Err} ->
                    {error, Err}
            end
    end;
handle_term(<<"save-native-source">>, Params, _Msg, _SessionPid) ->
    %% Edit → compile → reload → write-back for a *project-owned* native
    %% (`.erl`) module. The write target is re-derived server-side from the
    %% module's own compile info (never a client path), and only a project-origin
    %% native is writable — deps/stdlib are rejected read-only.
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    Source = maps:get(<<"source">>, Params, <<>>),
    save_native_source(ModuleBin, Source);
handle_term(<<"save-section">>, Params, _Msg, _SessionPid) ->
    %% Add/rename a `// === Name ===` section-divider comment at the
    %% file/class level. Exactly one of `old_name` (rename) / `before_selector`
    %% (insert) selects the mode — see save_section/5's doc.
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    NewName = maps:get(<<"new_name">>, Params, <<>>),
    OldName = maps:get(<<"old_name">>, Params, <<>>),
    BeforeSelector = maps:get(<<"before_selector">>, Params, <<>>),
    BeforeSide = maps:get(<<"before_side">>, Params, <<"instance">>),
    save_section(ClassBin, NewName, OldName, BeforeSelector, BeforeSide).

%%% Internal helpers

-doc """
Save (edit → compile → reload → write-back) a project-owned native `.erl` module.

The flow, in order, is fail-safe — the on-disk `.erl` is only overwritten after a
clean compile + load, so a compile error leaves the source untouched:

  1. **Authorize** server-side via `native_module_editable_target/1`: resolve the
     module to its on-disk `.erl` path from its *own* compile info and confirm it
     is a project-owned native. A deps/stdlib native, an unknown module, or a
     `.beam`-only module is rejected with a structured `#beamtalk_error{}` — the
     client's `editable` flag is never trusted.
  2. **Compile** the edited buffer to a temp `.erl` alongside the real one (so the
     erlc include path — `native/include` + the generated header — resolves) via
     `compile_native_erl_files/2`. On error, return the structured
     `format_erl_compile_errors/2` maps for inline display; the real `.erl` is
     left untouched.
  3. **Reload** is done by `compile_native_erl_files/2` itself (`code:load_binary`
     + mtime stamp), so the live VM runs the new code.
  4. **Write-back**: overwrite the real `.erl` with the edited source, atomically
     (temp file + rename), and refresh its compile-mtime so the incremental
     native build does not see it as stale (freshness).

Returns `{value, Map}` — `Map` carries `module`, `source_file`, and either
`ok => true` (clean) or `errors => [ErrMap]` (compile errors, same shape as the
load path). A bad request (missing/blank module or source) returns
`{error, #beamtalk_error{}}`.
""".
-spec save_native_source(binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
save_native_source(<<>>, _Source) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"save-native-source requires a module name">>,
            <<"Pass the native module to save.">>
        )};
save_native_source(_ModuleBin, <<>>) ->
    {error,
        beamtalk_repl_errors:make(
            empty_expression,
            'Workspace',
            <<"save-native-source requires non-empty source">>,
            <<"Enter Erlang source to compile and save.">>
        )};
save_native_source(ModuleBin, Source) when is_binary(ModuleBin), is_binary(Source) ->
    %% A module the workspace never loaded has no atom; `to_existing_atom` keeps
    %% the atom table bounded and turns an unknown module into a clean
    %% "not editable" rejection rather than an atom-table leak.
    case beamtalk_repl_errors:safe_to_existing_atom(ModuleBin) of
        {error, badarg} ->
            {error, native_not_editable_error(ModuleBin)};
        {ok, Module} ->
            case beamtalk_repl_ops_browse:native_module_editable_target(Module) of
                {error, not_editable} ->
                    {error, native_not_editable_error(ModuleBin)};
                {ok, ErlPath} ->
                    compile_and_write_native(Module, ModuleBin, ErlPath, Source)
            end
    end.

-doc """
Structured rejection for a non-editable native (deps/stdlib/unknown/.beam-only).
The same message whatever the reason — never leak whether a path exists.
""".
-spec native_not_editable_error(binary()) -> #beamtalk_error{}.
native_not_editable_error(ModuleBin) ->
    beamtalk_repl_errors:make(
        permission_denied,
        'Workspace',
        iolist_to_binary([
            <<"Native module '">>,
            ModuleBin,
            <<"' is not an editable project source.">>
        ]),
        <<
            "Only project-owned native (.erl) modules can be edited; "
            "dependency and stdlib modules are read-only."
        >>
    ).

-doc """
Compile the edited buffer (fail-safe: against a temp `.erl`, real source
untouched on error), reload on success, then write the source to disk and refresh
the compile-mtime. The project root for the erlc include path is derived from the
`.erl` path (it lives under `<ProjectRoot>/native/...`).
""".
-spec compile_and_write_native(atom(), binary(), string(), binary()) -> {value, map()}.
compile_and_write_native(Module, ModuleBin, ErlPath, Source) ->
    SourceFileBin = list_to_binary(ErlPath),
    ProjectRoot = beamtalk_workspace_project_sync:find_project_root(ErlPath),
    %% Refresh the generated beamtalk_classes.hrl before compiling so a
    %% module that `-include("beamtalk_classes.hrl")` resolves against a fresh
    %% header (the same invariant the incremental native build relies on). A
    %% missing project root (no beamtalk.toml — should not happen for a resolved
    %% project native) degrades to compiling without the regen step.
    _ =
        case ProjectRoot of
            undefined ->
                ok;
            Root ->
                beamtalk_workspace_project_sync:regenerate_native_class_header(
                    Root, beamtalk_workspace_project_sync:resolve_package_name(Root, undefined)
                )
        end,
    %% Compile the EDITED buffer against a temp `.erl` in the same directory (so
    %% relative includes resolve identically) — never the real file — so a compile
    %% error leaves the on-disk source untouched.
    TempPath = ErlPath ++ ".bt_native_save_tmp.erl",
    CompileRoot =
        case ProjectRoot of
            undefined -> filename:dirname(filename:dirname(ErlPath));
            Root2 -> Root2
        end,
    case file:write_file(TempPath, Source) of
        {error, WriteReason} ->
            _ = file:delete(TempPath),
            {value, #{
                <<"module">> => ModuleBin,
                <<"source_file">> => SourceFileBin,
                <<"errors">> => [
                    #{
                        <<"path">> => SourceFileBin,
                        <<"kind">> => <<"write_error">>,
                        <<"message">> => beamtalk_error:chars_to_binary(
                            io_lib:format("Could not stage source for compile: ~tp", [WriteReason])
                        )
                    }
                ]
            }};
        ok ->
            {Errors, _Count} = beamtalk_workspace_project_sync:compile_native_erl_files(
                [TempPath], CompileRoot
            ),
            _ = file:delete(TempPath),
            case Errors of
                [] ->
                    finish_native_write(
                        Module, ModuleBin, ErlPath, SourceFileBin, Source, CompileRoot
                    );
                _ ->
                    %% Re-point the temp path's errors at the real file so the IDE
                    %% renders them against the module the author is editing.
                    RetargetedErrors = [
                        E#{<<"path">> => SourceFileBin}
                     || E <- Errors
                    ],
                    {value, #{
                        <<"module">> => ModuleBin,
                        <<"source_file">> => SourceFileBin,
                        <<"errors">> => RetargetedErrors
                    }}
            end
    end.

-doc """
The clean-compile tail: atomically write the edited source to the real `.erl`,
then recompile + reload the module FROM that real path so its compile-info
`source` points at the real file (and the compile-mtime is stamped fresh), and
return the success result.

Recompiling from the real path is essential: the validation compile above loaded
the binary from the temp `.erl` (now deleted), which would otherwise leave the
module's `module_info(compile)` `source` pointing at a deleted temp file. A later
save re-derives the editable target from that compile-info source
(`native_module_editable_target/1`), so a stale temp path there would make the
next save write to the wrong (deleted) file and leave the real `.erl` stale.

Uses `atomic_write_file/2` (no pre-rename recheck): unlike
`save-section`, `Source` here is the client's freshly edited buffer, not a
splice computed from a prior read of `ErlPath` — there is no "expected prior
content" to recheck against, only the already-validated (compiled + loaded)
new source, which should win regardless of what currently sits on disk. The
concurrent-writer *corruption* bug (a shared fixed temp path) is still fixed
here: `atomic_write_file/2` now always uses a unique-per-call temp path.
""".
-spec finish_native_write(atom(), binary(), string(), binary(), binary(), string()) ->
    {value, map()}.
finish_native_write(_Module, ModuleBin, ErlPath, SourceFileBin, Source, CompileRoot) ->
    case atomic_write_file(ErlPath, Source) of
        ok ->
            %% Recompile + reload from the REAL `.erl` (not the validation temp)
            %% so compile-info `source` = ErlPath and the compile-mtime is
            %% stamped against the real file (image == disk).
            {ReErrors, _Count} = beamtalk_workspace_project_sync:compile_native_erl_files(
                [ErlPath], CompileRoot
            ),
            case ReErrors of
                [] ->
                    ?LOG_INFO(
                        "save-native-source: wrote + reloaded native module ~s",
                        [ModuleBin],
                        #{domain => [beamtalk, runtime]}
                    ),
                    {value, #{
                        <<"module">> => ModuleBin,
                        <<"source_file">> => SourceFileBin,
                        <<"ok">> => true
                    }};
                _ ->
                    %% Defensive: the identical source just compiled cleanly
                    %% against the temp, so a failure here is an environment
                    %% transient (I/O error, a stale include regenerated mid-run,
                    %% etc.). Disk and VM are coherent (both carry the new code),
                    %% but module_info(compile) source may still point at the
                    %% deleted temp — so the next save could re-derive the wrong
                    %% target until a reload corrects it. Log prominently so an
                    %% operator can recover (ClassName reload / workspace restart).
                    ?LOG_WARNING(
                        "save-native-source: wrote ~s but recompiling from the real "
                        "path failed (module_info source may point at a deleted temp; "
                        "a reload may be needed before the next save) — ~p",
                        [ModuleBin, ReErrors],
                        #{domain => [beamtalk, runtime]}
                    ),
                    RetargetedErrors = [E#{<<"path">> => SourceFileBin} || E <- ReErrors],
                    {value, #{
                        <<"module">> => ModuleBin,
                        <<"source_file">> => SourceFileBin,
                        <<"errors">> => RetargetedErrors
                    }}
            end;
        {error, WriteReason} ->
            %% The module is already compiled + loaded (the live VM has the new
            %% code), but the on-disk write failed — surface it so the IDE does
            %% not show a false "saved" with a stale file behind it.
            {value, #{
                <<"module">> => ModuleBin,
                <<"source_file">> => SourceFileBin,
                <<"errors">> => [
                    #{
                        <<"path">> => SourceFileBin,
                        <<"kind">> => <<"write_error">>,
                        <<"message">> => beamtalk_error:chars_to_binary(
                            io_lib:format(
                                "Compiled and reloaded, but writing the source to disk failed: ~tp",
                                [WriteReason]
                            )
                        )
                    }
                ]
            }}
    end.

-doc """
Atomic file write: write to a temp file in the same directory, then rename over
the target (rename is atomic on POSIX), so a concurrent reader never sees a
half-written file. On any failure the temp file is cleaned up.

Equivalent to `atomic_write_file/3` with no pre-rename recheck.
""".
-spec atomic_write_file(string(), binary()) -> ok | {error, term()}.
atomic_write_file(Path, Content) ->
    atomic_write_file(Path, Content, undefined).

-doc """
Atomic file write with an optional pre-rename "changed on disk" guard.

Delegates to `beamtalk_workspace_file:atomic_write/3`: the temp path is unique
per call, never a fixed name, so two concurrent writers targeting the same
`Path` each get their own temp file and one writer's `file:write_file/2` can
never interleave with or clobber another's in-flight temp write. On any
failure the temp file is deleted.

`ExpectedCurrent`, when not `undefined`, is byte-compared against `Path`'s
*current* on-disk contents immediately before the rename. This closes the
race window a caller-side check (e.g. a read at the top of the op) cannot: a
concurrent writer's rename can land in the gap between that earlier check and
this call's own rename. A mismatch aborts with `{error, changed_on_disk}` —
nothing is written — rather than silently overwriting a concurrent edit,
mirroring the ADR 0082 flush pipeline's `prev_source` span check
(`beamtalk_workspace_flush.erl`). Other failures return the bare file error
`Reason` (untagged).

After a successful write it sweeps stale staging files a crashed earlier write
to `Path` left beside it (`beamtalk_workspace_file:sweep_stale_temps/1`), as
flush does after its own commits, since both write into the user's source tree.
""".
-spec atomic_write_file(string(), binary(), binary() | undefined) -> ok | {error, term()}.
atomic_write_file(Path, Content, ExpectedCurrent) ->
    case beamtalk_workspace_file:atomic_write(Path, Content, ExpectedCurrent) of
        ok ->
            _ = beamtalk_workspace_file:sweep_stale_temps(Path),
            ok;
        {error, {_Step, Reason}} ->
            {error, Reason}
    end.

-doc """
Add or rename a `// === Name ===` section-divider comment in a class's `.bt`
source file — the System Browser's file/class-level section-
authoring affordance.

Per this module's design ("the divider comment is the storage, no sidecar
metadata"), this writes the divider text directly into the source file. It is
**not** a method-body edit and deliberately does not go through the ADR 0082
patch/ChangeLog/flush pipeline — a comment-only change has no method body to
reconcile against a pending patch, so the simpler direct-write path
`save-native-source` already established for the same reason
applies here too.

Exactly one of two params selects the mode:

  * `old_name` (non-empty): **rename** — locate the existing category whose
    divider name is `old_name` via a *fresh* `categorize_methods/2` read of
    the current on-disk source (never a client-supplied byte offset — the
    file may have changed since the client last read it), then replace that
    divider line's name text with `new_name` in place, preserving
    indentation and the surrounding `=`-run markers.
  * `before_selector` (+ optional `before_side`, default `instance`):
    **insert** — locate that method's own definition span via
    `beamtalk_compiler:resolve_method_span/4` (the same ADR 0082 resolver the
    live-patch install hook uses) and insert a brand-new
    `// === NewName ===` divider line directly above it, matching its
    indentation.

Authorizes and resolves the write target server-side via
`beamtalk_repl_loader:class_source_file/1` + `classify_source_file/1` — never
a client-supplied path — and rejects a class with no editable, in-project
`.bt` file (deps/stdlib/file-less classes) the same way
`native_module_editable_target/1` gates `save-native-source`. The write is
atomic (temp file + rename, `atomic_write_file/3`). A comment-only change
never affects the compiled class, so nothing is recompiled or reloaded.

Guards against clobbering a concurrent write to the same file: before
writing, `finish_section_write/4` re-reads `Path` and requires it to still
byte-match the source this edit was computed from, and `atomic_write_file/3`
repeats that same check immediately before the final rename — closing
the narrower race where a concurrent write's rename lands in the gap between
this first read and that rename. Either check mirrors the ADR 0082 flush
pipeline's `prev_source` check. A mismatch (e.g. another session's method-body
flush landed on this file in between) is rejected as a conflict with nothing
written, rather than silently overwritten.

Returns `{value, #{<<"class">> => _, <<"source_file">> => _, <<"ok">> =>
true}}` on success. Failure (class not found/not editable, missing/blank
params, `old_name` not found, `before_selector` not found, concurrent
external edit, disk I/O error) returns `{error, #beamtalk_error{}}`.
""".
-spec save_section(binary(), binary(), binary(), binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
save_section(<<>>, _NewName, _OldName, _BeforeSelector, _BeforeSide) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"save-section requires a class name">>,
            <<"Pass the class whose source you want to edit.">>
        )};
save_section(_ClassBin, <<>>, _OldName, _BeforeSelector, _BeforeSide) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"save-section requires a non-empty new_name">>,
            <<"Pass the section name to write.">>
        )};
save_section(_ClassBin, _NewName, <<>>, <<>>, _BeforeSide) ->
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"save-section requires old_name (rename) or before_selector (insert)">>,
            <<
                "Pass old_name to rename an existing section, or before_selector "
                "to add a new one above that method."
            >>
        )};
save_section(_ClassBin, _NewName, OldName, BeforeSelector, _BeforeSide) when
    OldName =/= <<>>, BeforeSelector =/= <<>>
->
    %% Review finding: the mode dispatch (`write_section/7`) picks rename
    %% whenever `old_name` is non-empty, silently ignoring `before_selector`
    %% if both are somehow sent together. Reject the ambiguous combination
    %% outright rather than silently picking one.
    {error,
        beamtalk_repl_errors:make(
            invalid_argument,
            'Workspace',
            <<"save-section requires exactly one of old_name or before_selector, not both">>,
            <<"Pass old_name to rename, or before_selector to insert; never both.">>
        )};
save_section(ClassBin0, NewName, OldName, BeforeSelector, BeforeSide0) when
    is_binary(ClassBin0),
    is_binary(NewName),
    is_binary(OldName),
    is_binary(BeforeSelector),
    is_binary(BeforeSide0)
->
    %% ADR 0127 §11: a divider inserted above a FLATTENED (`uses:`-provided)
    %% method belongs in the protocol's file — writing it into the class file
    %% would target a method that file does not define.
    {ClassBin, BeforeSide} = section_target(ClassBin0, BeforeSelector, BeforeSide0),
    case valid_section_name(NewName) of
        false ->
            {error, invalid_section_name_error()};
        true ->
            case beamtalk_repl_errors:safe_to_existing_atom(ClassBin) of
                {error, badarg} ->
                    {error, section_class_not_editable_error(ClassBin)};
                {ok, ClassName} ->
                    case beamtalk_repl_loader:class_source_file(ClassBin) of
                        nil ->
                            {error, section_class_not_editable_error(ClassBin)};
                        SourceFile ->
                            case beamtalk_repl_loader:classify_source_file(SourceFile) of
                                {not_flushable, _Reason} ->
                                    {error, section_class_not_editable_error(ClassBin)};
                                {flushable, Path} ->
                                    write_section(
                                        Path,
                                        ClassBin,
                                        ClassName,
                                        NewName,
                                        OldName,
                                        BeforeSelector,
                                        BeforeSide
                                    )
                            end
                    end
            end
    end.

%% Resolve the class whose file a `save-section` insert writes to: the
%% `before_selector`'s protocol (its `origin`, ADR 0127 §12) when that
%% selector was flattened into `ClassBin` by `uses:`, else `ClassBin` itself.
%% Provisions are instance-side only, so a class-side anchor never reroutes.
-spec section_target(binary(), binary(), binary()) -> {binary(), binary()}.
section_target(ClassBin, <<>>, BeforeSide) ->
    {ClassBin, BeforeSide};
section_target(ClassBin, _BeforeSelector, <<"class">> = BeforeSide) ->
    {ClassBin, BeforeSide};
section_target(ClassBin, BeforeSelector, BeforeSide) ->
    case
        {
            beamtalk_repl_errors:safe_to_existing_atom(ClassBin),
            beamtalk_repl_errors:safe_to_existing_atom(BeforeSelector)
        }
    of
        {{ok, ClassName}, {ok, Selector}} ->
            case beamtalk_xref:method_origin(ClassName, false, Selector) of
                nil -> {ClassBin, BeforeSide};
                Protocol -> {atom_to_binary(Protocol, utf8), <<"instance">>}
            end;
        _ ->
            {ClassBin, BeforeSide}
    end.

%% Without this check, `new_name` could be spliced verbatim into the
%% divider line — a name containing `\n`/`\r`
%% would inject arbitrary extra source lines (corrupting the file, defeating
%% the whole "comment-only edit" premise this op's direct-write bypass of
%% ADR 0082 rests on), and a whitespace-only name composes into a line
%% `parse_divider_name` itself rejects (`method_category.rs`), so the section
%% would silently vanish (its methods merging into whatever category preceded
%% it) with no error. Reject both up front.
-spec valid_section_name(binary()) -> boolean().
valid_section_name(NewName) ->
    case binary:match(NewName, [<<"\n">>, <<"\r">>]) of
        nomatch -> string:trim(NewName) =/= <<>>;
        _ -> false
    end.

-spec invalid_section_name_error() -> #beamtalk_error{}.
invalid_section_name_error() ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'Workspace',
        <<"save-section requires a single-line, non-blank new_name">>,
        <<
            "Section names can't contain a newline (it would corrupt the "
            "source file) or be blank/whitespace-only."
        >>
    ).

-spec section_class_not_editable_error(binary()) -> #beamtalk_error{}.
section_class_not_editable_error(ClassBin) ->
    beamtalk_repl_errors:make(
        permission_denied,
        'Workspace',
        iolist_to_binary([
            <<"Class '">>, ClassBin, <<"' has no editable in-project .bt source.">>
        ]),
        <<
            "Only classes backed by a project-owned .bt file can have "
            "sections added or renamed."
        >>
    ).

-spec write_section(string(), binary(), atom(), binary(), binary(), binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
write_section(Path, ClassBin, ClassName, NewName, <<>>, BeforeSelector, BeforeSide) ->
    case file:read_file(Path) of
        {ok, Source} ->
            insert_section(Path, Source, ClassBin, ClassName, NewName, BeforeSelector, BeforeSide);
        {error, ReadReason} ->
            {error, section_io_error(ClassBin, <<"reading">>, ReadReason)}
    end;
write_section(Path, ClassBin, ClassName, NewName, OldName, _BeforeSelector, _BeforeSide) ->
    case file:read_file(Path) of
        {ok, Source} ->
            rename_section(Path, Source, ClassBin, ClassName, NewName, OldName);
        {error, ReadReason} ->
            {error, section_io_error(ClassBin, <<"reading">>, ReadReason)}
    end.

-spec rename_section(string(), binary(), binary(), atom(), binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
rename_section(Path, Source, ClassBin, ClassName, NewName, OldName) ->
    case beamtalk_compiler:categorize_methods(Source, ClassName) of
        {ok, Categories} ->
            case find_category_divider_span(Categories, OldName) of
                undefined ->
                    {error, section_not_found_error(ClassBin, OldName)};
                ambiguous ->
                    {error, section_ambiguous_error(ClassBin, OldName)};
                {Start, End} ->
                    %% Review finding: rebuild the line from its own
                    %% indentation rather than `binary:replace(OldLine,
                    %% OldName, NewName)` — a divider name is free text (per
                    %% `parse_divider_name`, `method_category.rs`) and can
                    %% legally contain `/`, `//`, or `=` characters, any of
                    %% which `binary:replace`'s first-match substring search
                    %% could match INSIDE the `// ===`/`===` markers instead
                    %% of the actual name, corrupting the divider. The
                    %% located span's own leading whitespace is the only
                    %% part worth preserving; the marker style is normalized
                    %% to the same 3-`=` convention `insert_section/7` uses
                    %% for a new divider, so a rename never needs to parse
                    %% the old line's `=`-run length back out.
                    OldLine = binary:part(Source, Start, End - Start),
                    Indent = leading_indent(OldLine),
                    NewLine = <<Indent/binary, "// === ", NewName/binary, " ===\n">>,
                    Before = binary:part(Source, 0, Start),
                    After = binary:part(Source, End, byte_size(Source) - End),
                    NewSource = <<Before/binary, NewLine/binary, After/binary>>,
                    finish_section_write(Path, ClassBin, Source, NewSource)
            end;
        {error, Reason, Message} ->
            {error, section_locate_error(ClassBin, Reason, Message)}
    end.

%% The category whose divider name matches `TargetName`, as its
%% `divider_span`'s `{Start, End}` byte offsets — `undefined` when no
%% category has that name (or the located one's divider line couldn't be
%% re-found in `source`, defensive; see `find_divider_span`'s own doc), and
%% `ambiguous` when MORE THAN ONE category shares that name (the divider
%% grammar has no uniqueness rule, so this can legitimately happen) — never
%% silently pick one, since renaming the wrong one is a silent data-loss bug
%% for whichever divider was not renamed.
-spec find_category_divider_span([map()], binary()) ->
    {non_neg_integer(), non_neg_integer()} | undefined | ambiguous.
find_category_divider_span(Categories, TargetName) ->
    case
        [
            Span
         || #{name := Name, divider_span := Span} <- Categories,
            Name =:= TargetName,
            Span =/= undefined
        ]
    of
        [#{start := S, 'end' := E}] -> {S, E};
        [] -> undefined;
        [_ | _] -> ambiguous
    end.

-spec insert_section(string(), binary(), binary(), atom(), binary(), binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
insert_section(Path, Source, ClassBin, ClassName, NewName, BeforeSelector, BeforeSide) ->
    Side =
        case BeforeSide of
            <<"class">> -> class;
            _ -> instance
        end,
    %% Server-side backstop (the LiveView UI's `insertable_methods_for_side/2`
    %% already filters these out of the "add section" dropdown, but a stale
    %% or crafted request could still name one): reject inserting directly
    %% above a method that already starts a NAMED category. Two divider
    %% lines back-to-back would silently orphan the existing one —
    %% `find_divider_span`'s own doc (`method_category.rs`) says only the
    %% nearer divider survives — rather than cleanly split the category.
    case beamtalk_compiler:categorize_methods(Source, ClassName) of
        {ok, Categories} ->
            case starts_named_category(Categories, BeforeSelector, Side) of
                true ->
                    {error, section_collision_error(ClassBin, BeforeSelector)};
                false ->
                    do_insert_section(
                        Path, Source, ClassBin, ClassName, NewName, BeforeSelector, Side
                    )
            end;
        {error, Reason, Message} ->
            {error, section_locate_error(ClassBin, Reason, Message)}
    end.

%% True when `Selector`/`Side` is the first method of a category that
%% already has a divider name — see `insert_section/7`'s doc for why that
%% makes it an unsafe insertion point.
-spec starts_named_category([map()], binary(), instance | class) -> boolean().
starts_named_category(Categories, Selector, Side) ->
    lists:any(
        fun
            (#{name := Name, methods := [#{selector := Sel, side := Sd} | _]}) ->
                Name =/= undefined andalso Sel =:= Selector andalso Sd =:= Side;
            (_) ->
                false
        end,
        Categories
    ).

-spec do_insert_section(
    string(), binary(), binary(), atom(), binary(), binary(), instance | class
) -> {value, map()} | {error, #beamtalk_error{}}.
do_insert_section(Path, Source, ClassBin, ClassName, NewName, BeforeSelector, Side) ->
    case beamtalk_compiler:resolve_method_span(Source, ClassName, BeforeSelector, Side) of
        {ok, #{start := Start}, PrevSource} ->
            Indent = leading_indent(PrevSource),
            DividerLine = <<Indent/binary, "// === ", NewName/binary, " ===\n">>,
            Before = binary:part(Source, 0, Start),
            After = binary:part(Source, Start, byte_size(Source) - Start),
            NewSource = <<Before/binary, DividerLine/binary, After/binary>>,
            finish_section_write(Path, ClassBin, Source, NewSource);
        {error, Reason, Message} ->
            {error, section_locate_error(ClassBin, Reason, Message)}
    end.

-spec section_collision_error(binary(), binary()) -> #beamtalk_error{}.
section_collision_error(ClassBin, BeforeSelector) ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'Workspace',
        iolist_to_binary([
            <<"Class '">>,
            ClassBin,
            <<"' already has a section starting at '">>,
            BeforeSelector,
            <<"'.">>
        ]),
        <<"Pick a method further down, or rename the existing section instead.">>
    ).

%% The leading run of spaces/tabs at the start of `Bin` (a method definition's
%% own first line, from `resolve_method_span`'s `prev_source`) — the
%% indentation the new divider line is written at, so it lines up with the
%% method it precedes.
-spec leading_indent(binary()) -> binary().
leading_indent(Bin) ->
    leading_indent(Bin, 0).

leading_indent(Bin, N) when N < byte_size(Bin) ->
    case binary:at(Bin, N) of
        C when C =:= $\s; C =:= $\t -> leading_indent(Bin, N + 1);
        _ -> binary:part(Bin, 0, N)
    end;
leading_indent(Bin, N) ->
    binary:part(Bin, 0, N).

%% Re-read `Path` immediately before writing and
%% require it to still byte-match `Source` (the content this write's
%% `NewSource` was spliced from, read at the start of the op) — mirrors the
%% ADR 0082 flush pipeline's `prev_source` check
%% (`beamtalk_workspace_flush.erl`). Without this, a concurrent flush that
%% lands on `Path` between this op's read and its write is silently
%% overwritten: `NewSource` was built from stale bytes, so the write reverts
%% the other write with no error to either side. A section edit is
%% comment-only, but it splices into and rewrites the *entire* file, so a
%% stale write clobbers unrelated method bodies elsewhere in the same file,
%% not just the divider line.
-spec finish_section_write(string(), binary(), binary(), binary()) ->
    {value, map()} | {error, #beamtalk_error{}}.
finish_section_write(Path, ClassBin, Source, NewSource) ->
    case file:read_file(Path) of
        {ok, Source} ->
            %% Pass `Source` through as the expected-current content
            %% so atomic_write_file/3 repeats this same byte-match check
            %% immediately before its rename, closing the gap between this
            %% read and that rename that a check made only here cannot.
            case atomic_write_file(Path, NewSource, Source) of
                ok ->
                    {value, #{
                        <<"class">> => ClassBin,
                        <<"source_file">> => list_to_binary(Path),
                        <<"ok">> => true
                    }};
                {error, changed_on_disk} ->
                    {error, section_conflict_error(ClassBin)};
                {error, WriteReason} ->
                    {error, section_io_error(ClassBin, <<"writing">>, WriteReason)}
            end;
        {ok, _Changed} ->
            {error, section_conflict_error(ClassBin)};
        {error, ReadReason} ->
            {error, section_io_error(ClassBin, <<"reading">>, ReadReason)}
    end.

-spec section_not_found_error(binary(), binary()) -> #beamtalk_error{}.
section_not_found_error(ClassBin, OldName) ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'Workspace',
        iolist_to_binary([
            <<"Class '">>, ClassBin, <<"' has no section named '">>, OldName, <<"'.">>
        ]),
        <<"The section may have been renamed or removed by another edit; refresh and retry.">>
    ).

-spec section_ambiguous_error(binary(), binary()) -> #beamtalk_error{}.
section_ambiguous_error(ClassBin, OldName) ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'Workspace',
        iolist_to_binary([
            <<"Class '">>,
            ClassBin,
            <<"' has more than one section named '">>,
            OldName,
            <<"'.">>
        ]),
        <<"Rename one of the duplicate dividers directly in the source file to disambiguate.">>
    ).

-spec section_conflict_error(binary()) -> #beamtalk_error{}.
section_conflict_error(ClassBin) ->
    beamtalk_repl_errors:make(
        external_edit,
        'Workspace',
        iolist_to_binary([
            <<"Class '">>,
            ClassBin,
            <<"' was modified on disk while this section edit was in progress.">>
        ]),
        <<"Refresh the class source and retry; nothing was written.">>
    ).

-spec section_locate_error(binary(), atom(), binary()) -> #beamtalk_error{}.
section_locate_error(ClassBin, Reason, Message) ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'Workspace',
        iolist_to_binary([
            <<"Could not locate the section target in '">>,
            ClassBin,
            <<"' (">>,
            atom_to_binary(Reason, utf8),
            <<"): ">>,
            Message
        ]),
        <<"Check that the class and method/section names are current, then retry.">>
    ).

-spec section_io_error(binary(), binary(), term()) -> #beamtalk_error{}.
section_io_error(ClassBin, Verb, Reason) ->
    beamtalk_repl_errors:make(
        io_error,
        'Workspace',
        iolist_to_binary([
            <<"Error ">>,
            Verb,
            <<" source for '">>,
            ClassBin,
            <<"': ">>,
            beamtalk_error:chars_to_binary(io_lib:format("~tp", [Reason]))
        ]),
        <<"Check file permissions and try again.">>
    ).

-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(
        fun(ClassMap) ->
            case maps:get(name, ClassMap, "") of
                "" ->
                    false;
                Name when is_list(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(list_to_binary(Name)) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                Name when is_binary(Name) ->
                    case beamtalk_repl_errors:safe_to_existing_atom(Name) of
                        {ok, Atom} -> {true, Atom};
                        {error, badarg} -> false
                    end;
                _ ->
                    false
            end
        end,
        Classes
    ).

-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    resolve_class_to_module(ClassName, ClassPids).

resolve_class_to_module(ClassName, []) ->
    ClassName;
resolve_class_to_module(ClassName, [Pid | Rest]) ->
    try
        case beamtalk_runtime_api:class_name(Pid) of
            ClassName ->
                beamtalk_runtime_api:module_name(Pid);
            _ ->
                resolve_class_to_module(ClassName, Rest)
        end
    catch
        _:_ ->
            resolve_class_to_module(ClassName, Rest)
    end.
