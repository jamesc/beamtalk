%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Module loading and class activation for the Beamtalk REPL
%%%
%%% **DDD Context:** REPL
%%%
%%% Handles loading compiled Beamtalk modules into the BEAM runtime,
%%% registering classes, triggering hot reload, and managing file paths.
%%% Extracted from beamtalk_repl_eval (BT-863).

-module(beamtalk_repl_loader).

-include_lib("kernel/include/logger.hrl").

-export([
    handle_load/2,
    handle_load_source/3,
    load_class_module/3,
    reload_method_definition/4,
    activate_module/2,
    register_classes/2,
    trigger_hot_reload/2,
    reload_class_file/1,
    reload_class_file/2,
    is_stdlib_path/1,
    to_snake_case/1,
    verify_class_present/3,
    compute_package_module_name/1
]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    load_compiled_module/6
]).
-endif.

%%% Public API

%% @doc Load a Beamtalk file and register its classes.
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
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary, ClassNames, ModuleName, Source, Path, State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

%% @doc Load Beamtalk source from an inline binary string (no file path).
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    Source = binary_to_list(SourceBin),
    case beamtalk_repl_compiler:compile_file(Source, Label, false, undefined) of
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(Binary, ClassNames, ModuleName, Source, undefined, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

%% @doc Load a compiled class module, activate it, and update REPL state.
%%
%% Returns:
%%   {ok, ClassName, no_trailing, NewState}    - class loaded, no trailing expressions
%%   {ok, ClassName, {trailing, ModName, Bin}, NewState} - has trailing expressions to eval
%%   {error, Reason, NewState}
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
            case beamtalk_class_registry:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% @doc Recompile and reload a class after a standalone method definition (BT-571).
-spec reload_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
reload_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin, selector := SelectorBin} = MethodInfo,
    ExistingSource = beamtalk_repl_state:get_class_source(ClassNameBin, State),
    case ExistingSource of
        undefined ->
            ErrorMsg =
                <<"Class not found: ", ClassNameBin/binary,
                    ". Define the class first before adding methods.">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            recompile_with_method(
                ClassSource, ClassNameBin, SelectorBin, Expression, Warnings, State
            )
    end.

%% @doc Activate a loaded module: register classes, trigger hot reload,
%% and update workspace metadata.
-spec activate_module(atom(), [map()]) -> ok.
activate_module(ModuleName, Classes) ->
    register_classes(Classes, ModuleName),
    trigger_hot_reload(ModuleName, Classes),
    beamtalk_workspace_meta:register_module(ModuleName),
    beamtalk_workspace_meta:update_activity().

%% @doc Register loaded classes by calling the module's register_class/0 function.
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

%% @doc Trigger hot reload for existing actors after module reload (BT-572).
-spec trigger_hot_reload(atom(), [map()]) -> ok.
trigger_hot_reload(ModuleName, Classes) ->
    lists:foreach(
        fun(ClassMap) ->
            hot_reload_class(ModuleName, ClassMap)
        end,
        Classes
    ),
    ok.

%% @doc Compile and load a source file without REPL session state (BT-845).
%%
%% Called from beamtalk_behaviour_intrinsics:classReload/1 via erlang:apply/3
%% to avoid a compile-time dependency from beamtalk_runtime to beamtalk_workspace.
-spec reload_class_file(string()) -> ok | {error, term()}.
reload_class_file(Path) ->
    reload_class_file_impl(Path, undefined).

%% BT-868: ExpectedClassName (atom) is verified against the compiled class list.
-spec reload_class_file(string(), atom()) -> ok | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    reload_class_file_impl(Path, ExpectedClassName).

%% @doc Check if a file path refers to a stdlib file (under stdlib/src/ directory).
-spec is_stdlib_path(string()) -> boolean().
is_stdlib_path("stdlib/src/" ++ _) ->
    true;
is_stdlib_path(Path) ->
    case string:find(Path, "/stdlib/src/") of
        nomatch -> false;
        _ -> true
    end.

%% @doc Convert a string to snake_case (e.g., "SchemeSymbol" -> "scheme_symbol").
%% Matches the Rust to_module_name() convention: inserts underscore before
%% uppercase only when the previous character was lowercase.
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

%% @doc Verify that the expected class name appears in the compiled class list (BT-868).
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
            activate_module(ModuleName, ClassNames),
            NewState1 = maybe_add_loaded_module(ModuleName, State),
            NewState2 = track_module_source(ModuleName, SourcePath, NewState1),
            NewState3 = store_file_class_sources(ClassNames, Source, NewState2),
            {ok, ClassNames, NewState3};
        {error, Reason} ->
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_class_registry:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
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
-spec store_file_class_sources([map()], string(), beamtalk_repl_state:state()) ->
    beamtalk_repl_state:state().
store_file_class_sources(ClassNames, Source, State) ->
    lists:foldl(
        fun(#{name := Name}, AccState) ->
            NameBin = list_to_binary(Name),
            beamtalk_repl_state:set_class_source(NameBin, Source, AccState)
        end,
        State,
        ClassNames
    ).

%% Store class source for later method patching (inline class definition case).
-spec store_class_sources([map()], atom(), string(), beamtalk_repl_state:state()) ->
    {term(), beamtalk_repl_state:state()}.
store_class_sources([], ClassModName, Expression, State) ->
    FallbackName = atom_to_binary(ClassModName, utf8),
    NewState = beamtalk_repl_state:set_class_source(FallbackName, Expression, State),
    {FallbackName, NewState};
store_class_sources(Classes, _ClassModName, Expression, State) ->
    StoreFun = fun(#{name := Name}, AccState) ->
        NameBin = normalize_class_source_key(Name),
        beamtalk_repl_state:set_class_source(NameBin, Expression, AccState)
    end,
    [#{name := FirstName} | _] = Classes,
    NewState = lists:foldl(StoreFun, State, Classes),
    {normalize_class_source_key(FirstName), NewState}.

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
            Pids = beamtalk_object_instances:all(ClassName),
            case Pids of
                [] ->
                    ok;
                _ ->
                    IVars = fetch_instance_vars(ClassName),
                    Extra = {IVars, ModuleName},
                    beamtalk_hot_reload:trigger_code_change(ModuleName, Pids, Extra)
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
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            [];
        ClassPid ->
            try
                gen_server:call(ClassPid, instance_variables)
            catch
                _:_ -> []
            end
    end.

%% Reload a class file without REPL session state.
-spec reload_class_file_impl(string(), atom() | undefined) -> ok | {error, term()}.
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
) -> ok | {error, term()}.
reload_compile_and_load(Source, Path, ModuleNameOverride, ExpectedClassName) ->
    StdlibMode = is_stdlib_path(Path),
    case beamtalk_repl_compiler:compile_file(Source, Path, StdlibMode, ModuleNameOverride) of
        {ok, Binary, ClassNames, ModuleName} ->
            case verify_class_present(ExpectedClassName, ClassNames, Path) of
                ok ->
                    case code:load_binary(ModuleName, Path, Binary) of
                        {module, ModuleName} ->
                            activate_module(ModuleName, ClassNames),
                            ok;
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
    string(), binary(), binary(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
recompile_with_method(ClassSource, ClassNameBin, SelectorBin, Expression, Warnings, State) ->
    CombinedSource = ClassSource ++ "\n" ++ Expression,
    SourceBin = list_to_binary(CombinedSource),
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
                ClassNameBin,
                SelectorBin,
                CombinedSource,
                AllWarnings,
                State
            );
        {error, Reason} ->
            {error, Reason, <<>>, Warnings, State}
    end.

%% Load a recompiled method-patched class binary into BEAM.
-spec load_recompiled_method(
    binary(), atom(), list(), binary(), binary(), string(), [binary()], beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
load_recompiled_method(
    Binary,
    ModName,
    Classes,
    ClassNameBin,
    SelectorBin,
    CombinedSource,
    AllWarnings,
    State
) ->
    case code:load_binary(ModName, "", Binary) of
        {module, ModName} ->
            activate_module(ModName, Classes),
            NewState = beamtalk_repl_state:set_class_source(
                ClassNameBin, CombinedSource, State
            ),
            Result = <<ClassNameBin/binary, ">>", SelectorBin/binary>>,
            {ok, Result, <<>>, AllWarnings, NewState};
        {error, LoadReason} ->
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_class_registry:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, AllWarnings, State};
                [] ->
                    {error, {load_error, LoadReason}, <<>>, AllWarnings, State}
            end
    end.

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

%% Compute a package-qualified module name for a file (BT-775).
-spec compute_package_module_name(string()) -> binary() | undefined.
compute_package_module_name(Path) ->
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{package_name := PackageName, project_path := ProjectPath}} when
            is_binary(PackageName), is_binary(ProjectPath)
        ->
            AbsPath = filename:absname(Path),
            ProjectRoot = binary_to_list(ProjectPath),
            resolve_package_module(AbsPath, ProjectRoot, PackageName);
        _ ->
            undefined
    end.

%% Try src/ then test/ to resolve the package module name.
-spec resolve_package_module(string(), string(), binary()) -> binary() | undefined.
resolve_package_module(AbsPath, ProjectRoot, PackageName) ->
    case try_package_relative(AbsPath, ProjectRoot, "src") of
        {ok, ModuleName} ->
            iolist_to_binary(["bt@", PackageName, "@", ModuleName]);
        undefined ->
            case try_package_relative(AbsPath, ProjectRoot, "test") of
                {ok, ModuleName} ->
                    iolist_to_binary(["bt@", PackageName, "@test@", ModuleName]);
                undefined ->
                    undefined
            end
    end.

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
