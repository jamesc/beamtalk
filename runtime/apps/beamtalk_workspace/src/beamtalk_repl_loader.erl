%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Module loading and class activation for the Beamtalk REPL

Handles loading compiled Beamtalk modules into the BEAM runtime,
registering classes, triggering hot reload, and managing file paths.
Extracted from beamtalk_repl_eval (BT-863).
""".

-export([
    handle_load/2,
    handle_load/3,
    handle_load_source/3,
    load_class_module/3,
    reload_method_definition/4,
    activate_module/2,
    activate_module/3,
    register_classes/2,
    trigger_hot_reload/2,
    reload_class_file/1,
    reload_class_file/2,
    is_stdlib_path/1,
    to_snake_case/1,
    verify_class_present/3,
    compute_package_module_name/1,
    new_class/2
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    load_compiled_module/6,
    normalize_class_source_key/1,
    extract_trailing_info/1,
    resolve_class_name/1,
    safe_binary_to_atom/1,
    safe_list_to_atom/1,
    safe_atom_result/1,
    resolve_package_module/4,
    try_package_relative/3,
    maybe_add_loaded_module/2,
    store_file_class_sources/3,
    store_class_sources/4,
    %% ADR 0082 Phase 1 (BT-2283): pure helpers behind the install hook.
    is_path_inside/2,
    method_source_binary/1,
    patch_side/1,
    classify_source_file/1,
    span_error_entry/3,
    %% ADR 0082 Phase 1 (BT-2285): pure validation helpers for new_class/2.
    declared_class_name/1,
    validate_new_class/3,
    validate_target_path/1
]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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
                    StdlibMode = is_stdlib_path(Path),
                    ModuleNameOverride = compute_package_module_name(Path),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride
                        )
                    of
                        %% BT-1950: Protocol definition from file compilation.
                        %% Must be matched before the generic 4-tuple to avoid
                        %% {ok, protocol_definition, Info, Warnings} binding to
                        %% {ok, Binary, ClassNames, ModuleName}.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
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
Load a Beamtalk file with pre-built class indexes (BT-1543).

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
                    StdlibMode = is_stdlib_path(Path),
                    ModuleNameOverride = compute_package_module_name(Path),
                    case
                        beamtalk_repl_compiler:compile_file(
                            Source, Path, StdlibMode, ModuleNameOverride, PrebuiltIndexes
                        )
                    of
                        %% BT-1950: Protocol definition — must match before generic 4-tuple.
                        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
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
    case beamtalk_repl_compiler:compile_file(Source, Label, false, undefined) of
        %% BT-1950: Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
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
    case code:load_binary(ClassModName, "", Binary) of
        {module, ClassModName} ->
            activate_module(ClassModName, Classes),
            NewState1 = maybe_add_loaded_module(ClassModName, State),
            {ClassName, NewState2} = store_class_sources(
                Classes, ClassModName, Expression, NewState1
            ),
            TrailingInfo = extract_trailing_info(ClassInfo),
            {ok, ClassName, TrailingInfo, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

-doc "Recompile and reload a class after a standalone method definition (BT-571).".
-spec reload_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
reload_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin} = MethodInfo,
    ExistingSource = beamtalk_workspace_meta:get_class_source(ClassNameBin),
    case ExistingSource of
        undefined ->
            ErrorMsg =
                <<"Class source not available for ", ClassNameBin/binary,
                    " (source not recorded or workspace metadata unavailable)">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            recompile_with_method(
                ClassSource, MethodInfo, Expression, Warnings, State
            )
    end.

-doc """
Activate a loaded module: register classes, trigger hot reload,
and update workspace metadata.
""".
-spec activate_module(atom(), [map()]) -> ok.
activate_module(ModuleName, Classes) ->
    activate_module(ModuleName, Classes, undefined).

-doc """
Activate a loaded module with an optional source path for workspace metadata.
Passing SourcePath ensures the source file is recorded in workspace_meta so that
new VS Code sessions (which have an empty session tracker) can still navigate to source.
""".
-spec activate_module(atom(), [map()], string() | undefined) -> ok.
activate_module(ModuleName, Classes, SourcePath) ->
    register_classes(Classes, ModuleName),
    trigger_hot_reload(ModuleName, Classes),
    beamtalk_workspace_meta:register_module(ModuleName, SourcePath),
    beamtalk_workspace_meta:update_activity().

-doc "Register loaded classes by calling the module's register_class/0 function.".
-spec register_classes([map()], atom()) -> ok.
register_classes(_ClassInfoList, ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            try
                ModuleName:register_class()
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end.

-doc "Trigger hot reload for existing actors after module reload (BT-572).".
-spec trigger_hot_reload(atom(), [map()]) -> ok.
trigger_hot_reload(ModuleName, Classes) ->
    lists:foreach(
        fun(ClassMap) ->
            hot_reload_class(ModuleName, ClassMap)
        end,
        Classes
    ),
    ok.

-doc """
Compile and load a source file without REPL session state (BT-845).

Called from beamtalk_behaviour_intrinsics:classReload/1 via erlang:apply/3
to avoid a compile-time dependency from beamtalk_runtime to beamtalk_workspace.
""".
-spec reload_class_file(string()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path) ->
    reload_class_file_impl(Path, undefined).

%% BT-868: ExpectedClassName (atom) is verified against the compiled class list.
-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    reload_class_file_impl(Path, ExpectedClassName).

-doc "Check if a file path refers to a stdlib file (under stdlib/src/ directory).".
-spec is_stdlib_path(string()) -> boolean().
is_stdlib_path("stdlib/src/" ++ _) ->
    true;
is_stdlib_path(Path) ->
    case string:find(Path, "/stdlib/src/") of
        nomatch -> false;
        _ -> true
    end.

-doc """
Convert a string to snake_case (e.g., "SchemeSymbol" -> "scheme_symbol").
Matches the Rust to_module_name() convention: inserts underscore before
uppercase only when the previous character was lowercase.
""".
-spec to_snake_case(string()) -> string().
to_snake_case([]) ->
    [];
to_snake_case([H | T]) ->
    to_snake_case(T, [string:to_lower(H)], false).

to_snake_case([], Acc, _PrevWasLower) ->
    lists:reverse(Acc);
to_snake_case([C | Rest], Acc, PrevWasLower) when C >= $A, C =< $Z ->
    case PrevWasLower of
        true ->
            to_snake_case(Rest, [string:to_lower(C), $_ | Acc], false);
        false ->
            to_snake_case(Rest, [string:to_lower(C) | Acc], false)
    end;
to_snake_case([C | Rest], Acc, _PrevWasLower) ->
    to_snake_case(Rest, [C | Acc], C >= $a andalso C =< $z).

-doc """
Verify that the expected class name appears in the compiled class list (BT-868).
""".
-spec verify_class_present(atom() | undefined, [#{name := string()}], string()) ->
    ok | {error, term()}.
verify_class_present(undefined, _ClassNames, _Path) ->
    ok;
verify_class_present(ExpectedClassName, ClassNames, Path) ->
    ExpectedName = atom_to_list(ExpectedClassName),
    DefinedNames = [N || #{name := N} <- ClassNames],
    case lists:member(ExpectedName, DefinedNames) of
        true -> ok;
        false -> {error, {class_not_found, ExpectedClassName, Path, DefinedNames}}
    end.

%%% Internal functions

%% Load a compiled module into BEAM, register its classes, and update REPL state.
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
    case code:load_binary(ModuleName, LoadPath, Binary) of
        {module, ModuleName} ->
            activate_module(ModuleName, ClassNames, SourcePath),
            NewState1 = maybe_add_loaded_module(ModuleName, State),
            NewState2 = track_module_source(ModuleName, SourcePath, NewState1),
            store_file_class_sources(ClassNames, Source, NewState2),
            {ok, ClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% BT-1950: Load a protocol module into BEAM, register it, and update REPL state.
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
    case code:load_binary(ModuleName, LoadPath, Binary) of
        {module, ModuleName} ->
            %% activate_module calls register_class/0 which registers the protocol
            activate_module(ModuleName, ProtocolClassNames, Path),
            NewState1 = maybe_add_loaded_module(ModuleName, State),
            NewState2 = track_module_source(ModuleName, Path, NewState1),
            {ok, ProtocolClassNames, NewState2};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% BT-1950: Load a protocol module without session state (stateless path).
%% Used by reload_compile_and_load for load_files_stateless.
-spec load_protocol_module_stateless(map(), string()) -> {ok, [map()]} | {error, term()}.
load_protocol_module_stateless(ProtocolInfo, Path) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    ProtocolClassNames = [
        #{name => binary_to_list(P), superclass => "Object"}
     || P <- Protocols
    ],
    case code:load_binary(ModuleName, Path, Binary) of
        {module, ModuleName} ->
            activate_module(ModuleName, ProtocolClassNames, Path),
            {ok, ProtocolClassNames};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ProtocolClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error, {load_error, Reason}}
            end
    end.

%% Add a module to the loaded modules list if not already present.
-spec maybe_add_loaded_module(atom(), beamtalk_repl_state:state()) -> beamtalk_repl_state:state().
maybe_add_loaded_module(ModuleName, State) ->
    LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
    case lists:member(ModuleName, LoadedModules) of
        true -> State;
        false -> beamtalk_repl_state:add_loaded_module(ModuleName, State)
    end.

%% Track loaded module in the module tracker.
-spec track_module_source(atom(), string() | undefined, beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
track_module_source(ModuleName, SourcePath, State) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    NewTracker = beamtalk_repl_modules:add_module(ModuleName, SourcePath, Tracker),
    beamtalk_repl_state:set_module_tracker(NewTracker, State).

%% Store class source for later method patching (file load case).
%% Writes to workspace_meta; State is returned unchanged.
-spec store_file_class_sources([map()], string(), beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
store_file_class_sources(ClassNames, Source, State) ->
    lists:foreach(
        fun(#{name := Name}) ->
            NameBin = normalize_class_source_key(Name),
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
            NameBin = normalize_class_source_key(Name),
            beamtalk_workspace_meta:set_class_source(NameBin, Expression)
        end,
        Classes
    ),
    [#{name := FirstName} | _] = Classes,
    {normalize_class_source_key(FirstName), State}.

-spec normalize_class_source_key(atom() | binary() | list()) -> binary().
normalize_class_source_key(Name) when is_binary(Name) -> Name;
normalize_class_source_key(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
normalize_class_source_key(Name) when is_list(Name) -> list_to_binary(Name).

%% Extract trailing expression info from a class definition result (BT-885).
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

%% Trigger hot reload for a single class.
-spec hot_reload_class(atom(), map()) -> ok.
hot_reload_class(ModuleName, ClassMap) ->
    ClassName = resolve_class_name(ClassMap),
    case ClassName of
        undefined ->
            ok;
        _ ->
            Pids =
                try
                    beamtalk_runtime_api:all_instances(ClassName)
                catch
                    error:badarg -> []
                end,
            case Pids of
                [] ->
                    ok;
                _ ->
                    IVars = fetch_instance_vars(ClassName),
                    Extra = {IVars, ModuleName},
                    beamtalk_runtime_api:trigger_code_change(ModuleName, Pids, Extra)
            end
    end.

%% Resolve a class name atom from a class map entry.
-spec resolve_class_name(map()) -> atom() | undefined.
resolve_class_name(ClassMap) ->
    case maps:get(name, ClassMap, undefined) of
        N when is_binary(N) ->
            safe_binary_to_atom(N);
        N when is_atom(N) ->
            N;
        N when is_list(N) ->
            safe_list_to_atom(N);
        _ ->
            undefined
    end.

-spec safe_binary_to_atom(binary()) -> atom() | undefined.
safe_binary_to_atom(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> undefined
    end.

-spec safe_list_to_atom(string()) -> atom() | undefined.
safe_list_to_atom(List) ->
    try
        list_to_existing_atom(List)
    catch
        error:badarg -> undefined
    end.

%% Fetch instance variables from the class registry.
-spec fetch_instance_vars(atom()) -> list().
fetch_instance_vars(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            [];
        ClassPid ->
            try
                beamtalk_runtime_api:instance_variables(ClassPid)
            catch
                _:_ -> []
            end
    end.

%% Reload a class file without REPL session state.
-spec reload_class_file_impl(string(), atom() | undefined) -> {ok, [map()]} | {error, term()}.
reload_class_file_impl(Path, ExpectedClassName) ->
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}};
        true ->
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    ModuleNameOverride = compute_package_module_name(Path),
                    reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName)
            end
    end.

%% Compile and load a file for stateless reload.
-spec reload_compile_and_load(
    string(), string(), binary() | undefined, atom() | undefined
) -> {ok, [map()]} | {error, term()}.
reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName) ->
    StdlibMode = is_stdlib_path(Path),
    case beamtalk_repl_compiler:compile_file(Source, Path, StdlibMode, ModuleNameOverride) of
        %% BT-1950: Protocol definition — must match before generic 4-tuple.
        {ok, protocol_definition, ProtocolInfo, _Warnings} ->
            load_protocol_module_stateless(ProtocolInfo, Path);
        {ok, Binary, ClassNames, ModuleName} ->
            case verify_class_present(ExpectedClassName, ClassNames, Path) of
                ok ->
                    case code:load_binary(ModuleName, Path, Binary) of
                        {module, ModuleName} ->
                            activate_module(ModuleName, ClassNames, Path),
                            {ok, ClassNames};
                        {error, Reason} ->
                            {error, {load_error, Reason}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Recompile a class with a new method definition.
%%
%% BT-911: Delegates to beamtalk_repl_compiler:compile_for_method_reload/2 which
%% wraps all compiler calls in wrap_compiler_errors, preventing compiler crashes
%% from propagating as exits that would kill the REPL process.
-spec recompile_with_method(
    string(), map(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
recompile_with_method(ClassSource, MethodInfo, Expression, Warnings, State) ->
    CombinedSource = ClassSource ++ "\n" ++ Expression,
    %% Source may carry non-Latin1 characters (em dash, arrows, smart quotes in
    %% doc comments) — `unicode:characters_to_binary/1' produces the UTF-8 the
    %% compiler expects, where `list_to_binary/1' crashes with `badarg' on any
    %% codepoint > 255. Both inputs are already-validated source (the stored class
    %% source and a just-compiled expression), so the conversion always yields a
    %% binary here — never the `{error,_,_}' tuple of malformed input.
    SourceBin = unicode:characters_to_binary(CombinedSource),
    %% BT-907: Include superclass index so cross-file inheritance resolves correctly.
    SuperclassIndex = beamtalk_repl_compiler:build_class_superclass_index(),
    Options0 = #{stdlib_mode => false, workspace_mode => true},
    Options1 =
        case map_size(SuperclassIndex) of
            0 -> Options0;
            _ -> Options0#{class_superclass_index => SuperclassIndex}
        end,
    %% Include module index for correct cross-directory class references.
    ModuleIndex = beamtalk_repl_compiler:build_class_module_index(),
    Options =
        case map_size(ModuleIndex) of
            0 -> Options1;
            _ -> Options1#{class_module_index => ModuleIndex}
        end,
    case beamtalk_repl_compiler:compile_for_method_reload(SourceBin, Options) of
        {ok, Binary, ModName, Classes, RecompileWarnings} ->
            AllWarnings = Warnings ++ RecompileWarnings,
            load_recompiled_method(
                Binary,
                ModName,
                Classes,
                MethodInfo,
                CombinedSource,
                AllWarnings,
                State
            );
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

%% Load a recompiled method-patched class binary into BEAM.
-spec load_recompiled_method(
    binary(), atom(), list(), map(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
load_recompiled_method(
    Binary,
    ModName,
    Classes,
    MethodInfo,
    CombinedSource,
    AllWarnings,
    State
) ->
    #{class_name := ClassNameBin, selector := SelectorBin} = MethodInfo,
    case code:load_binary(ModName, "", Binary) of
        {module, ModName} ->
            %% (2) Install in memory. The memory install is the visible effect;
            %% the ChangeEntry below is step (3) — emitted only after install
            %% succeeds (all-or-nothing between install and log, ADR 0082).
            activate_module(ModName, Classes),
            %% Update all classes compiled in this module so sibling class entries
            %% reflect the latest combined source and stay consistent for future >> calls.
            lists:foreach(
                fun(#{name := Name}) ->
                    NameBin = normalize_class_source_key(Name),
                    beamtalk_workspace_meta:set_class_source(NameBin, CombinedSource)
                end,
                Classes
            ),
            %% (3) Emit a ChangeEntry for the live patch (ADR 0082 Phase 1).
            %% Best-effort: a ChangeLog failure must never fail the install — the
            %% method is already live in memory. emit_change_entry/1 logs and
            %% swallows its own errors.
            emit_change_entry(MethodInfo),
            %% (4) ADR 0082 Phase 4: when the workspace is in `autoflush: true'
            %% mode, every successful durable in-memory patch is immediately
            %% flushed to disk. Best-effort and synchronous; a flush failure does
            %% NOT roll back the BEAM module install (prior binary may already be
            %% unloaded and live actors may hold references to the new closures)
            %% — the entry simply stays pending in the log for manual flush
            %% reconciliation. Ephemeral patches are not autoflushed because
            %% only durable+flushable entries are written by `flush/0'.
            maybe_autoflush(maps:get(intent, MethodInfo, durable)),
            Result = <<ClassNameBin/binary, ">>", SelectorBin/binary>>,
            {ok, Result, <<>>, AllWarnings, State};
        {error, LoadReason} ->
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, AllWarnings, State};
                [] ->
                    {error, {load_error, LoadReason}, <<>>, AllWarnings, State}
            end
    end.

%%% ----------------------------------------------------------------------------
%%% New-class creation (ADR 0082 Phase 1, BT-2285)
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

%% Compile (without installing) to discover the declared class name, validate
%% (c) name == basename and (d) not already loaded, then install + log.
-spec new_class_compile(string(), string(), string()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_compile(Source, TargetPath, AbsPath) ->
    ModuleNameOverride = compute_package_module_name(TargetPath),
    StdlibMode = is_stdlib_path(TargetPath),
    %% `compile_file/4`'s success-typing return here is a class binary or an
    %% error (the protocol-definition variant declared in its spec is produced by
    %% a different compiler entry, not this one — dialyzer confirms it can never
    %% arrive). A protocol-only source therefore surfaces as a compile error or
    %% as a class-less result that `declared_class_name([])` rejects loudly.
    case beamtalk_repl_compiler:compile_file(Source, TargetPath, StdlibMode, ModuleNameOverride) of
        {ok, Binary, ClassNames, ModuleName} ->
            new_class_validate_and_install(
                Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName
            );
        {error, Reason} ->
            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
    end.

%% With a successful compile, finish validation against the declared class name,
%% then install the already-compiled binary and emit the new-class ChangeEntry.
-spec new_class_validate_and_install(
    string(), string(), string(), binary(), [map()], atom()
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_validate_and_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName) ->
    case declared_class_name(ClassNames) of
        {error, _} = NameErr ->
            NameErr;
        {ok, DeclaredName} ->
            case validate_new_class(DeclaredName, TargetPath, class_loaded(DeclaredName)) of
                ok ->
                    new_class_install(
                        Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, DeclaredName
                    );
                {error, _} = ValErr ->
                    ValErr
            end
    end.

%% Install the compiled binary in memory (mirrors load_compiled_module/6's
%% activation path, but stateless) and emit the durable new-class ChangeEntry.
%% A ChangeLog failure does not undo the install — the class is already live.
-spec new_class_install(
    string(), string(), string(), binary(), [map()], atom(), binary()
) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
new_class_install(Source, TargetPath, AbsPath, Binary, ClassNames, ModuleName, DeclaredName) ->
    case code:load_binary(ModuleName, AbsPath, Binary) of
        {module, ModuleName} ->
            activate_module(ModuleName, ClassNames, AbsPath),
            %% Record class source so subsequent `>>` / compile:source: patches
            %% against the new class resolve their span (mirrors the file-load path).
            lists:foreach(
                fun(#{name := Name}) ->
                    beamtalk_workspace_meta:set_class_source(
                        normalize_class_source_key(Name), Source
                    )
                end,
                ClassNames
            ),
            emit_new_class_entry(DeclaredName, list_to_binary(Source), list_to_binary(AbsPath)),
            %% ADR 0082 Phase 4: autoflush also covers new-class entries (they
            %% are durable + flushable by construction). See the analogous
            %% comment in load_recompiled_method/7 for the failure semantics.
            maybe_autoflush(durable),
            {ok, loaded_class_objects(ClassNames)};
        {error, LoadReason} ->
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_runtime_api:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError};
                [] ->
                    {error,
                        new_class_error(
                            new_class_load_failed,
                            iolist_to_binary(
                                io_lib:format("Could not load new class: ~p", [LoadReason])
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
            case classify_source_file(list_to_binary(TargetPath)) of
                {flushable, AbsPath} ->
                    {ok, AbsPath};
                {not_flushable, _Reason} ->
                    {error,
                        new_class_error(
                            target_outside_project,
                            iolist_to_binary([
                                <<"newClass:at: target is outside the project source tree: ">>,
                                list_to_binary(TargetPath),
                                <<" — new classes must be created inside the current project">>
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
                        <<" — use compile:source: against the existing class, or choose a new path">>
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
    {ok, normalize_class_source_key(Name)};
declared_class_name([]) ->
    {error,
        new_class_error(
            no_class_declared,
            <<"newClass:at: source does not declare a class">>,
            undefined
        )};
declared_class_name(ClassNames) when length(ClassNames) > 1 ->
    Names = [normalize_class_source_key(N) || #{name := N} <- ClassNames],
    {error,
        new_class_error(
            multiple_classes_declared,
            iolist_to_binary([
                <<"newClass:at: source declares multiple classes (">>,
                lists:join(<<", ">>, Names),
                <<") — one class per file (ADR 0040)">>
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
    DeclaredSnake = to_snake_case(binary_to_list(DeclaredName)),
    BaseSnake = to_snake_case(BaseName),
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
                        <<" — either rename the class to match the basename, or use a path with basename ">>,
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
                        <<" is already loaded — use compile:source: against it, or remove it first">>
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

%% Trigger `Workspace flush' when `autoflush: true' is set on the workspace
%% (ADR 0082 Phase 4, BT-2290). Best-effort and synchronous:
%%
%%   - Ephemeral patches are never autoflushed (they are not flushable by
%%     definition — only `durable AND flushable' entries are written).
%%   - The flush call itself is wrapped in try/catch so a flush failure (e.g.
%%     external-edit conflict surfaces a conflict-summary, not an exception,
%%     but the ChangeLog server being unreachable would exit the gen_server
%%     call) cannot bubble up and undo the BEAM module install — the patch is
%%     already live in memory.
%%   - The flush is best-effort. A conflict / I/O failure leaves the entry
%%     pending in the log; the user can re-flush manually after reconciling.
%%
%% A successful flush returns a `FlushResult' summary; we log a warning when
%% the summary reports conflicts so an autoflush failure is observable in the
%% workspace log even though the install path returns successfully.
-spec maybe_autoflush(durable | ephemeral) -> ok.
maybe_autoflush(ephemeral) ->
    ok;
maybe_autoflush(durable) ->
    case beamtalk_workspace_meta:get_setting(autoflush, false) of
        true -> do_autoflush();
        _ -> ok
    end.

-spec do_autoflush() -> ok.
do_autoflush() ->
    try beamtalk_workspace_flush:flush() of
        {ok, #{conflicts := Conflicts} = Summary} when Conflicts =/= [] ->
            ?LOG_WARNING(
                "Autoflush reported conflicts — pending entries remain in the log",
                #{conflicts => Conflicts, summary => Summary, domain => [beamtalk, runtime]}
            ),
            ok;
        {ok, _Summary} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Autoflush returned a structured error (entries remain pending)",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Autoflush crashed (entries remain pending; patch still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Emit the durable `new-class` ChangeEntry for a freshly created class. Best
%% effort: a ChangeLog write must never fail or undo the in-memory install (the
%% class is already live), mirroring emit_change_entry/1's contract.
-spec emit_new_class_entry(binary(), binary(), binary()) -> ok.
emit_new_class_entry(ClassNameBin, Source, SourceFile) ->
    try
        Entry = #{
            class => ClassNameBin,
            kind => 'new-class',
            source => Source,
            %% Explicit per ADR 0082 contract: new-class entries carry no prior
            %% disk body and no byte span (the file does not yet exist).
            prev_source => undefined,
            span => undefined,
            intent => durable,
            flushable => true,
            source_file => SourceFile,
            author => new_class_author(),
            author_kind => new_class_author_kind()
        },
        _ = beamtalk_workspace_changelog:append(Entry),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for new class (class still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class_name => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

%% Audit author for a new-class entry. MCP `save_class` stamps `agent` into the
%% process dictionary at the submission boundary (same mechanism `compile:source:`
%% uses); a direct REPL call defaults to `human`/`repl`.
-spec new_class_author() -> binary().
new_class_author() ->
    case erlang:get('$beamtalk_author') of
        A when is_binary(A) -> A;
        _ -> new_class_default_author()
    end.

-spec new_class_default_author() -> binary().
new_class_default_author() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> <<"agent">>;
        _ -> <<"repl">>
    end.

-spec new_class_author_kind() -> human | agent.
new_class_author_kind() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> agent;
        _ -> human
    end.

-spec new_class_error(atom(), binary(), string() | undefined) -> #beamtalk_error{}.
new_class_error(Kind, Message, TargetPath) ->
    Err0 = beamtalk_error:new(Kind, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    Err2 = beamtalk_error:with_message(Err1, Message),
    case TargetPath of
        undefined -> Err2;
        _ -> beamtalk_error:with_details(Err2, #{target => list_to_binary(TargetPath)})
    end.

-spec new_class_type_error(binary()) -> #beamtalk_error{}.
new_class_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    beamtalk_error:with_message(Err1, Message).

%% Emit a ChangeLog entry for a live in-memory method patch (ADR 0082 Phase 1).
%%
%% Called from load_recompiled_method/7 *after* the patched class is installed in
%% memory. Captures (per the ADR's "Method patch flow"):
%%   - `kind' (`instance' / `class') from the patch side,
%%   - `intent' (`durable' for `>>' / `compile:source:'; `ephemeral' for
%%     `tryCompile:source:'),
%%   - `author_kind' (`human' / `agent') from the eval submission metadata,
%%   - `flushable' + `sourceFile' + `span' + `prev_source' when the class is
%%     backed by an in-project `.bt' file whose method span resolves cleanly.
%%
%% Non-flushable classes (stdlib / dynamic / dependency — `sourceFile' nil or
%% out-of-project) still log an entry, with `flushable: false' and a reason, so
%% the audit trail stays exhaustive ("every in-memory method mutation produces a
%% ChangeEntry"). A disk-read or span-resolution failure downgrades the entry to
%% non-flushable rather than failing the install.
%%
%% Best-effort: every failure path is logged and swallowed. The method is already
%% live; a ChangeLog write must not undo or block that (the all-or-nothing rule
%% only requires that the entry is emitted *after* a successful install — if
%% emission itself fails, the patch still stands).
-spec emit_change_entry(map()) -> ok.
emit_change_entry(MethodInfo) ->
    try
        do_emit_change_entry(MethodInfo)
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Failed to emit ChangeLog entry for live patch (patch still installed)",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    method_info => maps:with([class_name, selector], MethodInfo),
                    domain => [beamtalk, runtime]
                }
            ),
            ok
    end.

-spec do_emit_change_entry(map()) -> ok.
do_emit_change_entry(MethodInfo) ->
    ClassNameBin = maps:get(class_name, MethodInfo),
    SelectorBin = maps:get(selector, MethodInfo),
    IsClassMethod = maps:get(is_class_method, MethodInfo, false),
    %% `>>' and `compile:source:' are durable; `tryCompile:source:' is ephemeral.
    Intent = maps:get(intent, MethodInfo, durable),
    %% MCP-issued patches tag `agent'; REPL / `>>' default to `human'.
    AuthorKind = maps:get(author_kind, MethodInfo, human),
    Author = maps:get(author, MethodInfo, <<"repl">>),
    Source = method_source_binary(MethodInfo),
    Kind = patch_side(IsClassMethod),
    Side = patch_side(IsClassMethod),
    Base = #{
        class => ClassNameBin,
        selector => SelectorBin,
        kind => Kind,
        source => Source,
        intent => Intent,
        author => Author,
        author_kind => AuthorKind
    },
    Entry = add_flushability(Base, ClassNameBin, SelectorBin, Side),
    _ = beamtalk_workspace_changelog:append(Entry),
    ok.

-spec patch_side(boolean()) -> instance | class.
patch_side(true) -> class;
patch_side(false) -> instance.

%% Best source for the patched method body, preferring the compiler-extracted
%% `method_source' (the `selector => body' fragment) and falling back to the raw
%% `expression' (the full `Class >> selector => body' line).
-spec method_source_binary(map()) -> binary().
method_source_binary(MethodInfo) ->
    case maps:get(method_source, MethodInfo, undefined) of
        Bin when is_binary(Bin) ->
            Bin;
        Str when is_list(Str) ->
            list_to_binary(Str);
        undefined ->
            case maps:get(expression, MethodInfo, <<>>) of
                B when is_binary(B) -> B;
                L when is_list(L) -> list_to_binary(L);
                _ -> <<>>
            end
    end.

%% Derive flushability and, when flushable, the on-disk span + prev_source.
%%
%% A class is flushable iff its `sourceFile' is non-nil AND lies inside the active
%% project tree. For flushable classes we resolve the method's byte span against
%% the *current on-disk* file (not the in-memory combined source) so a later
%% flush splices into exactly what is on disk. If the file cannot be read or the
%% span cannot be resolved, the patch downgrades to non-flushable.
-spec add_flushability(map(), binary(), binary(), instance | class) -> map().
add_flushability(Base, ClassNameBin, SelectorBin, Side) ->
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, AbsPath} ->
                    add_span_or_downgrade(
                        Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath
                    );
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% Reason for a class with no backing `.bt' sourceFile (`class_source_file/1'
%% returned nil). Distinguishes a stdlib class — whose module is compiled to a
%% `.beam' on disk (`code:which/1' returns a path) — from a dynamically-created
%% / live-only class whose module exists only in memory (`code:which/1' returns
%% a non-path atom such as `non_existing', `cover_compiled', or `preloaded').
%% Matches the ChangeLog schema's documented "stdlib" / "dynamic" reasons so flush
%% summaries can tell the two apart.
-spec no_source_reason(binary()) -> binary().
no_source_reason(ClassNameBin) ->
    case class_module(ClassNameBin) of
        {ok, Module} ->
            case code:which(Module) of
                Path when is_list(Path) -> <<"stdlib">>;
                _NonPath -> <<"dynamic">>
            end;
        error ->
            <<"dynamic">>
    end.

%% Resolve a class name binary to its loaded BEAM module name, if the class is
%% registered. Returns `error' when the class is unknown / not loaded.
-spec class_module(binary()) -> {ok, atom()} | error.
class_module(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    {ok, beamtalk_object_class:module_name(Pid)};
                _ ->
                    error
            end;
        {error, _} ->
            error
    end.

%% Resolve the disk span/prev_source for a flushable class; downgrade on failure.
-spec add_span_or_downgrade(map(), binary(), binary(), instance | class, binary(), string()) ->
    map().
add_span_or_downgrade(Base, ClassNameBin, SelectorBin, Side, SourceFile, AbsPath) ->
    case file:read_file(AbsPath) of
        {ok, DiskSource} ->
            resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource);
        {error, ReadReason} ->
            ?LOG_WARNING(
                "ChangeLog: could not read sourceFile for live patch; recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReadReason,
                    domain => [beamtalk, runtime]
                }
            ),
            Base#{flushable => false, not_flushable_reason => <<"disk_read_failed">>}
    end.

-spec resolve_span_entry(map(), binary(), binary(), instance | class, binary(), binary()) -> map().
resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, ClassNameBin, SelectorBin, Side) of
        {ok, Span, PrevSource} ->
            Base#{
                flushable => true,
                source_file => SourceFile,
                span => Span,
                prev_source => PrevSource
            };
        {error, Reason, _Message} ->
            %% Selector absent on disk (a brand-new method added live) is normal:
            %% record a flushable entry with no prev span — a later flush appends
            %% the method. Other resolution errors downgrade to memory-only.
            span_error_entry(Base, SourceFile, Reason)
    end.

-spec span_error_entry(map(), binary(), atom()) -> map().
span_error_entry(Base, SourceFile, selector_not_found) ->
    Base#{flushable => true, source_file => SourceFile};
span_error_entry(Base, _SourceFile, Reason) ->
    Base#{
        flushable => false,
        not_flushable_reason =>
            iolist_to_binary([<<"span_unresolved:">>, atom_to_binary(Reason, utf8)])
    }.

%% Resolve the class's source file via the BEAM module attribute (the same
%% source-of-truth `Behaviour>>sourceFile' reads). Returns nil for classes with
%% no backing file (stdlib / dynamic / not loaded).
-spec class_source_file(binary()) -> binary() | nil.
class_source_file(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    ModuleName = beamtalk_object_class:module_name(Pid),
                    beamtalk_reflection:source_file_from_module(ModuleName);
                _ ->
                    nil
            end;
        {error, _} ->
            nil
    end.

%% Classify a class's source file as flushable (in-project) or not.
%%
%% `flushable' requires the file to resolve to an absolute path inside the active
%% project source tree (per workspace metadata `project_path'). Files outside it
%% are a dependency; a workspace with no project path treats all paths as
%% non-flushable (nothing to flush into).
-spec classify_source_file(binary()) ->
    {flushable, string()} | {not_flushable, binary()}.
classify_source_file(SourceFile) ->
    SourceStr = binary_to_list(SourceFile),
    AbsPath = filename:absname(SourceStr),
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
            ProjectRoot = filename:absname(binary_to_list(ProjectPath)),
            case is_path_inside(ProjectRoot, AbsPath) of
                true -> {flushable, AbsPath};
                false -> {not_flushable, dependency_reason(SourceFile)}
            end;
        _ ->
            %% No project context — cannot flush into a tree we do not know.
            {not_flushable, dependency_reason(SourceFile)}
    end.

-spec dependency_reason(binary()) -> binary().
dependency_reason(SourceFile) ->
    iolist_to_binary([<<"dependency:">>, SourceFile]).

%% True iff `Path' is `Root' itself or lives beneath it (component-wise prefix,
%% so "/a/bc" is not considered inside "/a/b").
-spec is_path_inside(string(), string()) -> boolean().
is_path_inside(Root, Path) ->
    RootParts = filename:split(Root),
    PathParts = filename:split(Path),
    lists:prefix(RootParts, PathParts).

%% Convert a list of class info maps to a list of existing atoms (BT-738).
-spec class_name_atoms([map()]) -> [atom()].
class_name_atoms(Classes) ->
    lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                safe_atom_result(beamtalk_repl_server:safe_to_existing_atom(list_to_binary(Name)));
            (#{name := Name}) when is_binary(Name) ->
                safe_atom_result(beamtalk_repl_server:safe_to_existing_atom(Name));
            (_) ->
                false
        end,
        Classes
    ).

-spec safe_atom_result({ok, atom()} | {error, badarg}) -> {true, atom()} | false.
safe_atom_result({ok, Atom}) -> {true, Atom};
safe_atom_result({error, badarg}) -> false.

%% Compute a package-qualified module name for a file (BT-775 / BT-1670).
%%
%% With package context: derives `bt@{package}@{relative_path_segments}`
%% for files under src/ or test/.  Files outside those directories (e.g.
%% examples/, fixtures/) fall back to `bt@{stem_snake_case}` so the same
%% class always gets the same module name regardless of load path.
%% Without package context: returns `undefined` so the compiler port
%% derives the name from the class name instead.  This is intentional —
%% class-name-based naming in the REPL enables hot reload across file
%% renames (e.g., hot_counter.bt and hot_counter_v2.bt both define
%% HotCounter → same module bt@hot_counter).
-spec compute_package_module_name(string()) -> binary() | undefined.
compute_package_module_name(Path) ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{package_name := PackageName, project_path := ProjectPath}} when
            is_binary(PackageName), is_binary(ProjectPath)
        ->
            AbsPath = filename:absname(Path),
            ProjectRoot = binary_to_list(ProjectPath),
            resolve_package_module(AbsPath, ProjectRoot, PackageName, Path);
        _ ->
            undefined
    end.

%% Try src/ then test/ to resolve the package module name.
%% Falls back to bt@{stem} for files outside src/ and test/ (e.g. examples/).
-spec resolve_package_module(string(), string(), binary(), string()) -> binary().
resolve_package_module(AbsPath, ProjectRoot, PackageName, OrigPath) ->
    case try_package_relative(AbsPath, ProjectRoot, "src") of
        {ok, ModuleName} ->
            iolist_to_binary(["bt@", PackageName, "@", ModuleName]);
        undefined ->
            case try_package_relative(AbsPath, ProjectRoot, "test") of
                {ok, ModuleName} ->
                    iolist_to_binary(["bt@", PackageName, "@test@", ModuleName]);
                undefined ->
                    stem_module_name(OrigPath)
            end
    end.

%% Derive module name from the file stem: bt@{snake_case_stem}.
-spec stem_module_name(string()) -> binary().
stem_module_name(Path) ->
    Basename = filename:basename(Path, ".bt"),
    Snake = to_snake_case(Basename),
    iolist_to_binary(["bt@", Snake]).

%% Check if AbsPath is under ProjectRoot/SubDir and return the relative module path.
-spec try_package_relative(string(), string(), string()) ->
    {ok, iodata()} | undefined.
try_package_relative(AbsPath, ProjectRoot, SubDir) ->
    Dir = filename:join(ProjectRoot, SubDir),
    AbsDir = filename:absname(Dir),
    DirParts = filename:split(AbsDir),
    PathParts = filename:split(AbsPath),
    DirLen = length(DirParts),
    case length(PathParts) > DirLen andalso lists:prefix(DirParts, PathParts) of
        true ->
            RelParts = lists:nthtail(DirLen, PathParts),
            Last = lists:last(RelParts),
            RelPartsNoExt = lists:droplast(RelParts) ++ [filename:rootname(Last)],
            SnakeSegments = [to_snake_case(S) || S <- RelPartsNoExt],
            {ok, lists:join("@", SnakeSegments)};
        false ->
            undefined
    end.
