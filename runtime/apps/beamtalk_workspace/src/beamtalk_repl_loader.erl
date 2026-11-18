%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Module loading and class activation for the Beamtalk REPL

Handles loading compiled Beamtalk modules into the BEAM runtime,
registering classes, triggering hot reload, and managing file paths.
Extracted from beamtalk_repl_eval.
""".

-export([
    handle_load/2,
    handle_load/3,
    handle_load_source/3,
    load_class_module/3,
    load_protocol_module/3,
    reload_method_definition/4
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    load_compiled_module/6,
    extract_trailing_info/1,
    store_file_class_sources/3,
    store_class_sources/4,
    %% ADR 0082 extension: class-redefinition ChangeLog entry helpers.
    snapshot_class_def_prev_sources/1,
    emit_class_def_entries/3
]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_repl_loader.hrl").

%%% Public API

-doc "Load a Beamtalk file and register its classes.".
-spec handle_load(string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    StdlibMode = beamtalk_repl_loader_support:is_stdlib_path(Path),
                    %% ADR 0127 §10a / BT-3593: ambient `protocol_sources`
                    %% context — see `with_ambient_protocol_sources/1`'s doc
                    %% for why a same-file-only `uses:` isn't enough here.
                    PrebuiltIndexes = beamtalk_repl_loader_support:with_ambient_protocol_sources(
                        beamtalk_repl_compiler:build_class_indexes()
                    ),
                    case compile_file_named(Source, Path, StdlibMode, PrebuiltIndexes) of
                        %% Protocol definition from file compilation.
                        %% Must be matched before the generic 4-tuple to avoid
                        %% {ok, protocol_definition, Info, Warnings} binding to
                        %% {ok, Binary, ClassNames, ModuleName}.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            beamtalk_repl_loader_support:track_protocol_source(
                                ProtocolInfo, Source, Path
                            ),
                            load_protocol_module(ProtocolInfo, Path, State);
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary, ClassNames, ModuleName, Source, Path, State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

-doc """
Load a Beamtalk file with pre-built class indexes.

Like `handle_load/2' but accepts pre-built class indexes to avoid
redundant class registry scans during batch loads (e.g. :load dir).
""".
-spec handle_load(string(), beamtalk_repl_state:state(), map()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State, PrebuiltIndexes) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    StdlibMode = beamtalk_repl_loader_support:is_stdlib_path(Path),
                    FullIndexes = beamtalk_repl_loader_support:with_ambient_protocol_sources(
                        PrebuiltIndexes
                    ),
                    case compile_file_named(Source, Path, StdlibMode, FullIndexes) of
                        %% Protocol definition — must match before generic 4-tuple.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
                            beamtalk_repl_loader_support:track_protocol_source(
                                ProtocolInfo, Source, Path
                            ),
                            load_protocol_module(ProtocolInfo, Path, State);
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary, ClassNames, ModuleName, Source, Path, State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

-doc "Load Beamtalk source from an inline binary string (no file path).".
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    Source = binary_to_list(SourceBin),
    PrebuiltIndexes = beamtalk_repl_loader_support:with_ambient_protocol_sources(
        beamtalk_repl_compiler:build_class_indexes()
    ),
    case beamtalk_repl_compiler:compile_file(Source, Label, false, undefined, PrebuiltIndexes) of
        %% Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
            beamtalk_repl_loader_support:track_protocol_source(ProtocolInfo, Source, undefined),
            load_protocol_module(ProtocolInfo, undefined, State);
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(Binary, ClassNames, ModuleName, Source, undefined, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

-doc """
Load a compiled class module, activate it, and update REPL state.

Returns:
  {ok, ClassName, no_trailing, NewState}    - class loaded, no trailing expressions
  {ok, ClassName, {trailing, ModName, Bin}, NewState} - has trailing expressions to eval
  {error, Reason, NewState}
""".
-spec load_class_module(map(), string(), beamtalk_repl_state:state()) ->
    {ok, term(), no_trailing | {trailing, atom(), binary()}, beamtalk_repl_state:state()}
    | {error, term(), beamtalk_repl_state:state()}.
load_class_module(ClassInfo, Expression, State) ->
    #{binary := Binary, module_name := ClassModName, classes := Classes} = ClassInfo,
    %% ADR 0105 Phase 2: seed the shape-generation store from the
    %% about-to-be-replaced module's CURRENT __beamtalk_meta/0 before this
    %% class-body reload installs — see
    %% beamtalk_workspace_shape_store's moduledoc "Two-phase capture" for why
    %% this must run before code:load_binary, not after.
    beamtalk_repl_loader_recheck:prime_shape_capture(Classes),
    %% Snapshot each class's currently-tracked source BEFORE
    %% store_class_sources below overwrites it — see emit_class_def_entries/3's
    %% doc for why a redefinition of an already-loaded class needs the OLD
    %% source captured ahead of the install, not after.
    PrevSources = snapshot_class_def_prev_sources(Classes),
    %% ADR 0107 Phase A: load_class_binary/4
    %% bakes the "superclasses_losing_leaf_status/1 before code:load_binary/3"
    %% ordering requirement into one call — see its own doc and
    %% activate_module/4's doc for why.
    case beamtalk_repl_loader_support:load_class_binary(ClassModName, "", Binary, Classes) of
        {ok, NewlyNonLeafSuperclasses} ->
            beamtalk_repl_loader_activate:activate_module(
                ClassModName, Classes, undefined, NewlyNonLeafSuperclasses
            ),
            NewState1 = beamtalk_repl_loader_support:maybe_add_loaded_module(ClassModName, State),
            {ClassName, NewState2} = store_class_sources(
                Classes, ClassModName, Expression, NewState1
            ),
            %% Log a pending 'class-def' ChangeLog entry for each
            %% class in this eval that redefined an already-loaded class (the
            %% cockpit `:def` tab's "Compile" action against an *existing*
            %% class). Best-effort: a ChangeLog write must never fail or undo
            %% the in-memory install, which already succeeded above.
            case emit_class_def_entries(Classes, Expression, PrevSources) of
                true -> beamtalk_repl_loader_methods:maybe_autoflush(durable);
                false -> ok
            end,
            TrailingInfo = extract_trailing_info(ClassInfo),
            {ok, ClassName, TrailingInfo, NewState2};
        {error, Reason} ->
            ClassAtoms = beamtalk_repl_loader_support:class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

-doc "Recompile and reload a class after a standalone method definition.".
-spec reload_method_definition(
    map(), [beamtalk_repl_protocol:warning()], string(), beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
reload_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin} = MethodInfo,
    case beamtalk_repl_loader_protocol:is_protocol_name(ClassNameBin) of
        true ->
            %% ADR 0127 §11: `P >> sel => ...` at the REPL edits the protocol's
            %% own source and re-expands every user (two-stage fan-out).
            beamtalk_repl_loader_protocol:patch_protocol_provision(
                ClassNameBin,
                maps:get(selector, MethodInfo),
                ?ENTRIES:method_source_binary(MethodInfo),
                MethodInfo#{intent => maps:get(intent, MethodInfo, durable)},
                Warnings,
                State
            );
        false ->
            reload_class_method_definition(MethodInfo, Warnings, Expression, State)
    end.

-spec reload_class_method_definition(
    map(), [beamtalk_repl_protocol:warning()], string(), beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [beamtalk_repl_protocol:warning()], beamtalk_repl_state:state()}.
reload_class_method_definition(MethodInfo, Warnings, _Expression, State) ->
    #{class_name := ClassNameBin} = MethodInfo,
    ExistingSource = beamtalk_workspace_meta:get_class_source(ClassNameBin),
    case ExistingSource of
        undefined ->
            ErrorMsg =
                <<"Class source not available for ", ClassNameBin/binary,
                    " (source not recorded or workspace metadata unavailable)">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            %% BT-4721: a typed `Class >> sel => ...` is an explicit
            %% redefinition. It goes through the same structured install as an
            %% IDE/MCP save (`compile_method` replaces the existing selector and
            %% returns the merged class source), so the stored class source is the
            %% canonical merged text with no duplicate definition left in it.
            %%
            %% That path parses the method standalone, so it needs the BARE
            %% `selector => body` the compiler port recovered (`method_source`),
            %% never the typed `Class >> selector => body` line. Every caller in
            %% the REPL flow carries it; a MethodInfo without one is a bug in the
            %% caller, not user error, so say so instead of feeding the installer
            %% a `Class >>` line it cannot parse.
            case maps:get(method_source, MethodInfo, undefined) of
                undefined ->
                    {error,
                        {compile_error,
                            <<"internal error: method definition for ", ClassNameBin/binary,
                                " carries no method_source">>},
                        <<>>, Warnings, State};
                _ ->
                    beamtalk_repl_loader_methods:install_method_with_source(
                        ClassNameBin,
                        maps:get(selector, MethodInfo),
                        ?ENTRIES:method_source_binary(MethodInfo),
                        ClassSource,
                        maps:get(intent, MethodInfo, durable),
                        maps:get(author, MethodInfo, <<"repl">>),
                        maps:get(author_kind, MethodInfo, human),
                        Warnings,
                        State,
                        maps:get(is_class_method, MethodInfo, false)
                    )
            end
    end.

%%% Internal functions

%% Load a compiled module into BEAM, register its classes, and update REPL state.
%%
%% Deliberately does NOT emit a `'class-def'` ChangeLog entry, unlike
%% `load_class_module/3`. Every caller of this function (`handle_load/2,3`,
%% `handle_load_source/3`) compiles `Source` from `SourcePath` itself — a
%% `:load <file>` (or the initial project load) installs a class from the
%% SAME file `Workspace flush` would otherwise write it back to, so the live
%% image is, by construction, already in sync with disk: there is nothing
%% pending to log or flush. Logging here would produce a ChangeEntry whose
%% `source`/`prev_source` are byte-identical (a no-op diff) on every ordinary
%% project load — pure log noise, not a fix for anything.
-spec load_compiled_module(
    binary(),
    [map()],
    atom(),
    string(),
    string() | undefined,
    beamtalk_repl_state:state()
) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
load_compiled_module(Binary, ClassNames, ModuleName, Source, SourcePath, State) ->
    LoadPath =
        case SourcePath of
            undefined -> "";
            _ -> SourcePath
        end,
    %% ADR 0105 Phase 2: see load_class_module/3's identical comment.
    beamtalk_repl_loader_recheck:prime_shape_capture(ClassNames),
    %% ADR 0107 Phase A: see load_class_binary/4's doc.
    case beamtalk_repl_loader_support:load_class_binary(ModuleName, LoadPath, Binary, ClassNames) of
        {ok, NewlyNonLeafSuperclasses} ->
            beamtalk_repl_loader_activate:activate_module(
                ModuleName, ClassNames, SourcePath, NewlyNonLeafSuperclasses
            ),
            NewState1 = beamtalk_repl_loader_support:maybe_add_loaded_module(ModuleName, State),
            NewState2 = beamtalk_repl_loader_support:track_module_source(
                ModuleName, SourcePath, NewState1
            ),
            store_file_class_sources(ClassNames, Source, NewState2),
            {ok, ClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = beamtalk_repl_loader_support:class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% Load a protocol module into BEAM, register it, and update REPL state.
%% Used by handle_load/2, handle_load/3, and handle_load_source.
-spec load_protocol_module(map(), string() | undefined, beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
load_protocol_module(ProtocolInfo, Path, State) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    LoadPath =
        case Path of
            undefined -> "";
            _ -> Path
        end,
    case
        beamtalk_repl_loader_support:load_binary_scoped(
            ModuleName, LoadPath, Binary, ProtocolClassNames
        )
    of
        {module, ModuleName} ->
            %% activate_module calls register_class/0 which registers the protocol
            beamtalk_repl_loader_activate:activate_module(ModuleName, ProtocolClassNames, Path),
            NewState1 = beamtalk_repl_loader_support:maybe_add_loaded_module(ModuleName, State),
            NewState2 = beamtalk_repl_loader_support:track_module_source(
                ModuleName, Path, NewState1
            ),
            {ok, ProtocolClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = beamtalk_repl_loader_support:class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% Store class source for later method patching (file load case).
%% Writes to workspace_meta; State is returned unchanged.
-spec store_file_class_sources([map()], string(), beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
store_file_class_sources(ClassNames, Source, State) ->
    lists:foreach(
        fun(#{name := Name}) ->
            NameBin = beamtalk_repl_loader_support:normalize_class_source_key(Name),
            beamtalk_workspace_meta:set_class_source(NameBin, Source)
        end,
        ClassNames
    ),
    State.

%% Store class source for later method patching (inline class definition case).
%% Writes to workspace_meta; State is returned unchanged.
-spec store_class_sources([map()], atom(), string(), beamtalk_repl_state:state()) ->
    {term(), beamtalk_repl_state:state()}.
store_class_sources([], ClassModName, Expression, State) ->
    FallbackName = atom_to_binary(ClassModName, utf8),
    beamtalk_workspace_meta:set_class_source(FallbackName, Expression),
    {FallbackName, State};
store_class_sources(Classes, _ClassModName, Expression, State) ->
    lists:foreach(
        fun(#{name := Name}) ->
            NameBin = beamtalk_repl_loader_support:normalize_class_source_key(Name),
            beamtalk_workspace_meta:set_class_source(NameBin, Expression)
        end,
        Classes
    ),
    [#{name := FirstName} | _] = Classes,
    {beamtalk_repl_loader_support:normalize_class_source_key(FirstName), State}.

%%% ----------------------------------------------------------------------------
%%% Class-redefinition ChangeLog entry (ADR 0082 extension)
%%% ----------------------------------------------------------------------------

%% Snapshot each class's currently-tracked source (workspace_meta's
%% `class_sources`) before an inline class-body install overwrites it via
%% `store_class_sources/4`. `undefined` for a class with no prior tracked
%% source — see `emit_class_def_entries/3`'s doc for why that case is
%% excluded from logging.
-spec snapshot_class_def_prev_sources([map()]) -> #{binary() => string() | undefined}.
snapshot_class_def_prev_sources(Classes) ->
    maps:from_list([
        {NameBin, beamtalk_workspace_meta:get_class_source(NameBin)}
     || #{name := Name} <- Classes,
        NameBin <- [beamtalk_repl_loader_support:normalize_class_source_key(Name)]
    ]).

-doc """
Log a pending `'class-def'` ChangeLog entry for each class in `Classes` that
*redefined* an already-loaded class (ADR 0082 extension).

Called from `load_class_module/3` after a successful inline class-body
install — the cockpit `:def` tab's "Compile" action against an *existing*
class routes through here
(`beamtalk_repl_eval:handle_class_definition/6` → `load_class_module/3`), and
previously installed the new class body with no ChangeLog entry at all: the
CHANGES dock stayed at "No pending changes" and `Workspace flush` silently
discarded the edit.

Only classes with a PRIOR tracked source (`PrevSources`, snapshotted by
`snapshot_class_def_prev_sources/1` before this install overwrote it) are
logged here — a class with no prior source is a genuinely brand-new class
defined inline (`Foo subclass: Bar [...]` typed directly, never installed
before), which is out of scope here: this feature's acceptance criteria is
scoped to redefining an "*existing*" class, and `newClass:at:`'s own
`beamtalk_repl_loader_changelog_entries:emit_new_class_entry/3` already covers the brand-new-class case for the one
FFI chokepoint that creates a file for it. A REPL-typed brand-new inline class
recording no ChangeLog entry is the same pre-existing (unrelated) gap it
always was, not a regression introduced here.

Also skips logging entirely when `Classes` has more than one entry. The `:def`
tab always edits exactly one class; a multi-class inline eval (typed directly
at the REPL, not through the tab) would otherwise log one entry per class,
each carrying the *same* whole `Expression` text as its `source` — there is
no per-class slice of a raw multi-class eval to attribute correctly, and
logging the same shared text under N different classes would be misleading
audit history, not a fix. This is the same "out of scope" carve-out as the
brand-new-class case above, not a data-loss concern (see
`add_class_def_flushability/2`'s doc — no `'class-def'` entry is ever
flushed to disk yet regardless).

Returns `true` iff at least one entry was appended, so the caller knows
whether an autoflush pass is worth triggering.
""".
-spec emit_class_def_entries([map()], string(), #{binary() => string() | undefined}) -> boolean().
emit_class_def_entries([#{name := Name}], Expression, PrevSources) ->
    NameBin = beamtalk_repl_loader_support:normalize_class_source_key(Name),
    case maps:get(NameBin, PrevSources, undefined) of
        undefined ->
            false;
        PrevSource ->
            ?ENTRIES:emit_class_def_entry(NameBin, Expression, PrevSource),
            true
    end;
emit_class_def_entries(_MultipleOrNoClasses, _Expression, _PrevSources) ->
    false.

%% Extract trailing expression info from a class definition result.
-spec extract_trailing_info(map()) ->
    no_trailing | {trailing, atom(), binary()}.
extract_trailing_info(ClassInfo) ->
    case maps:find(trailing_binary, ClassInfo) of
        {ok, TrailingBinary} ->
            TrailingModName = maps:get(trailing_module_name, ClassInfo),
            {trailing, TrailingModName, TrailingBinary};
        error ->
            no_trailing
    end.

%% `beamtalk_repl_compiler:compile_file/5` under the file's module name
%% override; a path segment the naming rule rejects is a compile error before
%% anything is compiled.
-spec compile_file_named(string(), string(), boolean(), map()) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()}
    | {ok, protocol_definition, map(), [beamtalk_repl_protocol:warning()]}
    | {error, term()}.
compile_file_named(Source, Path, StdlibMode, PrebuiltIndexes) ->
    case beamtalk_repl_loader_reload:compute_package_module_name(Path) of
        {ok, ModuleNameOverride} ->
            beamtalk_repl_compiler:compile_file(
                Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes
            );
        {error, _} = Error ->
            Error
    end.
