%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_newclass).

%%% **DDD Context:** REPL Session Context

-moduledoc """
`Workspace newClass:at:` creation for the Beamtalk REPL (ADR 0082 Phase 1).

Owns compiling a brand-new class from a source string, validating its target
path and declared name, installing it in memory and logging the `new-class`
ChangeLog entry (`new_class/2`), plus `revert_remove_class/2`, which reinstalls
a removed class through the same compile+install chokepoint (ADR 0113).

Extracted from `beamtalk_repl_loader` (BT-4702). It is a client of the loader's
load/activate primitives (`load_class_binary/4`, `activate_module/4`,
`maybe_autoflush/1`, …), which stay in `beamtalk_repl_loader` because the
single-site install paths share them; the loader makes no calls into this
module.
""".

-export([
    new_class/2,
    %% ADR 0113 — `Workspace changes revert:` extension for a pending
    %% `'remove-class'` entry: reinstalls the class from its recorded prior
    %% source, reusing new_class/2's own compile+install chokepoint minus its
    %% target-must-not-exist check (see revert_remove_class/2's doc for why).
    revert_remove_class/2
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    %% ADR 0082 Phase 1: pure validation helpers for new_class/2.
    declared_class_name/1,
    validate_new_class/3,
    validate_target_path/1
]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).

%%% ----------------------------------------------------------------------------
%%% New-class creation (ADR 0082 Phase 1)
%%% ----------------------------------------------------------------------------

-doc """
Create a brand-new class from a source String at `TargetPath` (ADR 0082 Phase 1).

Compiles and installs the class in memory, then logs a `kind: "new-class"`
ChangeEntry (`intent: durable`, `flushable: true`, `prev_source = nil`,
`span = nil`, full source). Phase 1 does NOT write `TargetPath` to disk — the
file is written later by `Workspace flush` (Phase 2), which replays the
new-class entry. The entry records `sourceFile = TargetPath` so the flush knows
where to write.

Validation is loud and specific — every failure is an `#beamtalk_error{}` with
no silent fallback (ADR 0082, *`Workspace newClass:` validation*). The op raises
when, in order:

  (a) `TargetPath` already exists on disk;
  (b) `TargetPath` lies outside the project source tree;
  (c) the declared class name does not match the basename of `TargetPath`
      (one-class-per-file convention, ADR 0040);
  (d) a class with that name is already loaded in memory.

On success returns `{ok, [ClassObject]}` (the loaded class object(s), matching
`load:`); on any validation/compile/install failure returns
`{error, #beamtalk_error{}}` so the FFI boundary can raise it.
""".
-spec new_class(binary() | string(), binary() | string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class(Source, TargetPath) when is_binary(Source) ->
    new_class(binary_to_list(Source), TargetPath);
new_class(Source, TargetPath) when is_binary(TargetPath) ->
    new_class(Source, binary_to_list(TargetPath));
new_class(Source, TargetPath) when is_list(Source), is_list(TargetPath) ->
    %% (a) TargetPath must not already exist on disk; (b) must be in-project.
    %% These checks run before compiling so a bad path fails fast and cheaply.
    case validate_target_path(TargetPath) of
        {ok, AbsPath} ->
            new_class_compile(Source, TargetPath, AbsPath);
        {error, _} = PathErr ->
            PathErr
    end;
new_class(_Source, _TargetPath) ->
    {error, new_class_type_error(<<"newClass:at: expects String source and path arguments">>)}.

-doc """
Recompile and reinstall a class from a recorded prior source, reusing the same
compile+install chokepoint `newClass:at:` uses (ADR 0082) rather than
a second whole-class-install mechanism (ADR 0113 — `Workspace changes
revert:` extended to a pending `'remove-class'` entry).

`TargetPath` is the removed class's own recorded `sourceFile` (already an
absolute path — `class_source_file/1`'s value at removal time), and `Source`
is the entry's `prev_source_ref` body: the exact whole-file text that was on
disk immediately before the removal.

Deliberately skips `new_class/2`'s `validate_target_path/1` step: that check
exists to stop a *fresh* `newClass:at:` from silently overwriting an unrelated
file, but a revert of a still-*pending* `'remove-class'` entry is restoring a
file that was never actually deleted — Tier 2 (`flushIncludingDestructive`,
ADR 0113) is the only thing that unlinks it, and a pending entry never reached
that step. Requiring the target's *absence* here would reject the exact case
this function exists to handle. Every other `newClass:at:` validation still
runs via `new_class_compile/3`'s existing chain (declared name matches
`TargetPath`'s basename, no class of that name is already loaded).

Installs in `no_log` mode (see `new_class_install/8`): the reinstalled file was
never actually deleted (revert is pre-flush-only), so its content already
matches disk — emitting a fresh `'new-class'`-kind ChangeEntry here would
permanently misrepresent it as a pending brand-new-file addition (that kind's
`clean` check is hardcoded "always pending", and any later flush would treat
the never-deleted file as an unresolvable `target_exists` conflict). The
caller is responsible for retiring the original `'remove-class'` entry (e.g.
`mark_flushed/1`) once this returns successfully — undoing a removal, like
undoing an add (`revert_removal/3`'s `'new-class'` case), does not itself
emit a new ChangeEntry.

The "its content already matches disk" assumption above holds only absent a
concurrent out-of-band edit (another session, git, an editor touching the
file while the removal sat pending) — this function does not itself guard
against that. The caller,
`beamtalk_workspace_interface_primitives:reinstall_reverted_class/3`, runs
`check_no_external_drift/3` first and never calls this function at all if the
on-disk file has drifted from the recorded `prev_source_ref` snapshot —
see that function's doc.
""".
-spec revert_remove_class(binary() | string(), binary() | string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
revert_remove_class(Source, TargetPath) when is_binary(Source) ->
    revert_remove_class(binary_to_list(Source), TargetPath);
revert_remove_class(Source, TargetPath) when is_binary(TargetPath) ->
    revert_remove_class(Source, binary_to_list(TargetPath));
revert_remove_class(Source, TargetPath) when is_list(Source), is_list(TargetPath) ->
    new_class_compile(Source, TargetPath, TargetPath, no_log);
revert_remove_class(_Source, _TargetPath) ->
    {error,
        new_class_type_error(<<"revert: remove-class reinstall expects String source and path">>)}.

%% Compile (without installing) to discover the declared class name, validate
%% (c) name == basename and (d) not already loaded, then install (+ log unless
%% `Mode` is `no_log` — see `new_class_install/8`).
-spec new_class_compile(string(), string(), string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_compile(Source, TargetPath, AbsPath) ->
    new_class_compile(Source, TargetPath, AbsPath, log).

-spec new_class_compile(string(), string(), string(), log | no_log) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_compile(Source, TargetPath, AbsPath, Mode) ->
    ModuleNameOverride = beamtalk_repl_loader:compute_package_module_name(TargetPath),
    StdlibMode = beamtalk_repl_loader:is_stdlib_path(TargetPath),
    %% `compile_file/4`'s success-typing return here is a class binary or an
    %% error (the protocol-definition variant declared in its spec is produced by
    %% a different compiler entry, not this one — dialyzer confirms it can never
    %% arrive). A protocol-only source therefore surfaces as a compile error or
    %% as a class-less result that `declared_class_name([])` rejects loudly.
    case beamtalk_repl_compiler:compile_file(Source, TargetPath, StdlibMode, ModuleNameOverride) of
        {ok, Binary, ClassNames, ModuleName} ->
            new_class_validate_and_install(
                Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, Mode
            );
        {error, Reason} ->
            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
    end.

%% With a successful compile, finish validation against the declared class name,
%% then install the already-compiled binary (+ log unless `Mode` is `no_log`).
-spec new_class_validate_and_install(
    string(), string(), string(), binary(), [map()], atom(), log | no_log
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_validate_and_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, Mode) ->
    case declared_class_name(ClassNames) of
        {error, _} = NameErr ->
            NameErr;
        {ok, DeclaredName} ->
            case validate_new_class(DeclaredName, TargetPath, class_loaded(DeclaredName)) of
                ok ->
                    new_class_install(
                        Source,
                        TargetPath,
                        AbsPath,
                        Binary,
                        ClassNames,
                        ModuleName,
                        DeclaredName,
                        Mode
                    );
                {error, _} = ValErr ->
                    ValErr
            end
    end.

%% Install the compiled binary in memory (mirrors load_compiled_module/6's
%% activation path, but stateless). In `log` mode (ordinary `newClass:at:`,
%% ADR 0082) also emits the durable new-class ChangeEntry and autoflushes; a
%% ChangeLog failure does not undo the install — the class is already live.
%% In `no_log` mode (a `'remove-class'` revert, ADR 0113 — see
%% `revert_remove_class/2`'s doc) does neither: the file being reinstalled was
%% never deleted, so there is nothing new to log or flush.
-spec new_class_install(
    string(), string(), string(), binary(), [map()], atom(), binary(), log | no_log
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, DeclaredName, Mode) ->
    %% ADR 0107 Phase A: see load_class_binary/4's doc.
    case beamtalk_repl_loader:load_class_binary(ModuleName, AbsPath, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            beamtalk_repl_loader:activate_module(
                ModuleName, ClassNames, AbsPath, NewlyNonLeafSuperclasses
            ),
            %% Record class source so subsequent `>>` / compile:source: patches
            %% against the new class resolve their span (mirrors the file-load path).
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        beamtalk_repl_loader:normalize_class_source_key(Name), Source
                    )
                end,
                ClassNames
            ),
            case Mode of
                log ->
                    ?ENTRIES:emit_new_class_entry(
                        DeclaredName, list_to_binary(Source), list_to_binary(AbsPath)
                    ),
                    %% ADR 0082 Phase 4: autoflush also covers new-class entries
                    %% (they are durable + flushable by construction). See the
                    %% analogous comment in load_recompiled_method/7 for the
                    %% failure semantics.
                    beamtalk_repl_loader:maybe_autoflush(durable);
                no_log ->
                    ok
            end,
            {ok, loaded_class_objects(ClassNames)};
        {error, LoadReason} ->
            ClassAtoms = beamtalk_repl_loader:class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error,
                        new_class_error(
                            new_class_load_failed,
                            beamtalk_error:chars_to_binary(
                                io_lib:format("Could not load new class: ~tp", [LoadReason])
                            ),
                            TargetPath
                        )}
            end
    end.

%% Validate (a) the path does not already exist and (b) it is inside the project
%% source tree. Returns the absolute path on success. `classify_source_file/1`
%% already encodes the in-project containment rule (and treats a workspace with
%% no project context as "outside", which is the correct conservative answer —
%% there is no tree to create the file in).
-spec validate_target_path(string()) -> {ok, string()} | {error, #beamtalk_error{}}.
validate_target_path(TargetPath) ->
    case file:read_file_info(TargetPath) of
        {error, enoent} ->
            case beamtalk_repl_loader:classify_source_file(list_to_binary(TargetPath)) of
                {flushable, AbsPath} ->
                    {ok, AbsPath};
                {not_flushable, _Reason} ->
                    {error,
                        new_class_error(
                            target_outside_project,
                            iolist_to_binary([
                                <<"newClass:at: target is outside the project source tree: ">>,
                                list_to_binary(TargetPath),
                                <<"; new classes must be created inside the current project">>
                            ]),
                            TargetPath
                        )}
            end;
        _Other ->
            %% Any existing filesystem entry (regular file, directory, symlink)
            %% blocks new-class; also treat unreadable paths (eaccess, etc.) as
            %% existing rather than silently overwriting.
            {error,
                new_class_error(
                    target_exists,
                    iolist_to_binary([
                        <<"newClass:at: target already exists on disk: ">>,
                        list_to_binary(TargetPath),
                        <<"; use compile:source: against the existing class, or choose a new path">>
                    ]),
                    TargetPath
                )}
    end.

-doc """
Extract the single declared class name from a compile result's class list.

Enforces the one-class-per-file convention (ADR 0040): `newClass:at:` accepts
exactly one class. An empty list (no class declared) or more than one class is a
loud error. Pure — exported for tests.
""".
-spec declared_class_name([map()]) -> {ok, binary()} | {error, #beamtalk_error{}}.
declared_class_name([#{name := Name}]) ->
    {ok, beamtalk_repl_loader:normalize_class_source_key(Name)};
declared_class_name([]) ->
    {error,
        new_class_error(
            no_class_declared,
            <<"newClass:at: source does not declare a class">>,
            undefined
        )};
declared_class_name(ClassNames) when length(ClassNames) > 1 ->
    Names = [beamtalk_repl_loader:normalize_class_source_key(N) || #{name := N} <- ClassNames],
    {error,
        new_class_error(
            multiple_classes_declared,
            iolist_to_binary([
                <<"newClass:at: source declares multiple classes (">>,
                lists:join(<<", ">>, Names),
                <<"); one class per file (ADR 0040)">>
            ]),
            undefined
        )}.

-doc """
Validate the declared class name against the target path (ADR 0082 Phase 1).

Checks (c) the declared name matches the basename of `TargetPath` (one class per
file, ADR 0040) and (d) no class of that name is already loaded (`Loaded` is the
caller-supplied result of `class_loaded/1`, threaded in so this helper stays
pure and unit-testable). Returns `ok` or `{error, #beamtalk_error{}}`.

The basename match is *snake_case-normalised* so both established file-naming
conventions are accepted for a class `Greeter`: `Greeter.bt` (PascalCase, the
stdlib convention) and `greeter.bt` (snake_case, the examples/fixtures
convention). Both resolve to the same module name in the compiler, so both are
"matching" here. The class name must still be the same word as the file stem —
`Welcomer` at `greeter.bt` is rejected.
""".
-spec validate_new_class(binary(), string(), boolean()) -> ok | {error, #beamtalk_error{}}.
validate_new_class(DeclaredName, TargetPath, Loaded) ->
    BaseName = filename:basename(TargetPath, ".bt"),
    Expected = list_to_binary(BaseName),
    %% Accept either an exact match (Greeter.bt) or a snake_case match
    %% (greeter.bt) — both map to the same module name as the class.
    DeclaredSnake = beamtalk_repl_loader:to_snake_case(binary_to_list(DeclaredName)),
    BaseSnake = beamtalk_repl_loader:to_snake_case(BaseName),
    Matches = (DeclaredName =:= Expected) orelse (DeclaredSnake =:= BaseSnake),
    case Matches of
        false ->
            {error,
                new_class_error(
                    class_name_mismatch,
                    iolist_to_binary([
                        <<"newClass:at: declared class ">>,
                        DeclaredName,
                        <<" does not match basename '">>,
                        Expected,
                        <<"' of ">>,
                        list_to_binary(TargetPath),
                        <<"; either rename the class to match the basename, or use a path with basename ">>,
                        DeclaredName,
                        <<".bt or ">>,
                        list_to_binary(DeclaredSnake),
                        <<".bt. One class per file (ADR 0040)">>
                    ]),
                    TargetPath
                )};
        true when Loaded ->
            {error,
                new_class_error(
                    class_already_loaded,
                    iolist_to_binary([
                        <<"newClass:at: class ">>,
                        DeclaredName,
                        <<" is already loaded; use compile:source: against it, or remove it first">>
                    ]),
                    TargetPath
                )};
        true ->
            ok
    end.

%% True iff a class of this name is currently registered in the runtime.
-spec class_loaded(binary()) -> boolean().
class_loaded(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            is_pid(beamtalk_class_registry:whereis_class(ClassName));
        {error, _} ->
            %% Name has never been interned as an atom, so it cannot be a loaded
            %% class — safe to treat as not loaded.
            false
    end.

%% Resolve loaded class info maps to Beamtalk class objects (same shape `load:`
%% returns). Reuses the workspace primitives' helper so the FFI surfaces the
%% created class to the REPL identically to a file load.
-spec loaded_class_objects([map()]) -> [#beamtalk_object{}].
loaded_class_objects(ClassNames) ->
    beamtalk_workspace_interface_primitives:loaded_class_objects(ClassNames).

-spec new_class_error(atom(), binary(), string() | undefined) -> #beamtalk_error{}.
new_class_error(Kind, Message, TargetPath) ->
    Err0 = beamtalk_error:new(Kind, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    Err2 = beamtalk_error:with_message(Err1, Message),
    case TargetPath of
        undefined ->
            Err2;
        _ ->
            beamtalk_error:with_details(Err2, #{
                target => beamtalk_primitive:string_from_binary(
                    unicode:characters_to_binary(TargetPath)
                )
            })
    end.

-spec new_class_type_error(binary()) -> #beamtalk_error{}.
new_class_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'Workspace'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    beamtalk_error:with_message(Err1, Message).
