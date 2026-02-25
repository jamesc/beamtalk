%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Expression evaluation for Beamtalk REPL
%%%
%%% **DDD Context:** REPL
%%%
%%% This module handles compilation, bytecode generation, and evaluation
%%% of Beamtalk expressions via the beamtalk_compiler OTP application
%%% (ADR 0022). Uses the OTP Port backend exclusively.

-module(beamtalk_repl_eval).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([do_eval/2, do_eval/3, do_show_codegen/2, handle_load/2, handle_load_source/3]).
%% BT-845: ADR 0040 Phase 2 — stateless class reload (called via erlang:apply from beamtalk_runtime)
-export([reload_class_file/1, reload_class_file/2]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    extract_assignment/1,
    format_formatted_diagnostics/1,
    maybe_await_future/1,
    should_purge_module/2,
    strip_internal_bindings/1,
    is_stdlib_path/1,
    compute_package_module_name/1,
    to_snake_case/1,
    inject_output/3,
    is_internal_key/1,
    activate_module/2,
    register_classes/2,
    trigger_hot_reload/2,
    handle_class_definition/6,
    handle_method_definition/4,
    compile_expression_via_port/3,
    compile_file_via_port/4,
    verify_class_present/3
]).
-endif.

-define(INTERNAL_REGISTRY_KEY, '__repl_actor_registry__').

%%% Public API

%% @doc Evaluate a Beamtalk expression.
%% This is the core of the REPL - compile, load, and execute.
%% If the result is a Future PID, automatically awaits it before returning.
%% Returns captured stdout as a binary (empty when no output was produced).
%% Returns warnings as a list of formatted diagnostic strings.
-spec do_eval(string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State) ->
    do_eval(Expression, State, undefined).

%% @doc Evaluate with optional streaming subscriber (BT-696).
%% When Subscriber is a pid, IO chunks are forwarded as {eval_out, Chunk}.
-spec do_eval(string(), beamtalk_repl_state:state(), pid() | undefined) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State, Subscriber) ->
    %% Generate unique module name for this evaluation
    Counter = beamtalk_repl_state:get_eval_counter(State),
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),

    Bindings = beamtalk_repl_state:get_bindings(State),

    %% Get actor registry PID to pass to eval context
    RegistryPid = beamtalk_repl_state:get_actor_registry(State),

    %% Compile expression
    case compile_expression(Expression, ModuleName, Bindings) of
        %% BT-571: Inline class definition — load module, register class
        %% BT-885: trailing expressions in ClassInfo are evaluated after class load
        {ok, class_definition, ClassInfo, Warnings} ->
            handle_class_definition(
                ClassInfo, Warnings, Expression, NewState, RegistryPid, Subscriber
            );
        %% BT-571: Standalone method definition — add/replace method on existing class
        {ok, method_definition, MethodInfo, Warnings} ->
            handle_method_definition(MethodInfo, Warnings, Expression, NewState);
        {ok, Binary, _ResultExpr, Warnings} ->
            %% Load the compiled module
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    eval_loaded_module(
                        ModuleName,
                        Expression,
                        Bindings,
                        RegistryPid,
                        Subscriber,
                        Warnings,
                        NewState
                    );
                {error, Reason} ->
                    {error, {load_error, Reason}, <<>>, [], NewState}
            end;
        {error, Reason} ->
            {error, {compile_error, Reason}, <<>>, [], NewState}
    end.

%% @doc Evaluate a loaded module: capture IO, execute, process result, cleanup.
-spec eval_loaded_module(
    atom(),
    string(),
    map(),
    pid() | undefined,
    pid() | undefined,
    [binary()],
    beamtalk_repl_state:state()
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
eval_loaded_module(
    ModuleName,
    Expression,
    Bindings,
    RegistryPid,
    Subscriber,
    Warnings,
    State
) ->
    CaptureRef = beamtalk_io_capture:start(Subscriber),
    EvalResult =
        try
            execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State)
        catch
            Class:Reason:_Stacktrace ->
                %% ADR 0015: Wrap error as Exception and bind to _error
                CaughtExObj = beamtalk_exception_handler:ensure_wrapped(Reason),
                CaughtBindings = maps:put('_error', CaughtExObj, Bindings),
                CaughtState = beamtalk_repl_state:set_bindings(CaughtBindings, State),
                {error, {eval_error, Class, CaughtExObj}, CaughtState}
        after
            cleanup_module(ModuleName, RegistryPid)
        end,
    Output = beamtalk_io_capture:stop(CaptureRef),
    inject_output(EvalResult, Output, Warnings).

%% @doc Execute module and process the result (assignment/future handling).
-spec execute_and_process(
    atom(),
    string(),
    map(),
    pid() | undefined,
    beamtalk_repl_state:state()
) ->
    {ok, term(), beamtalk_repl_state:state()}
    | {error, term(), beamtalk_repl_state:state()}.
execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State) ->
    BindingsWithRegistry =
        case RegistryPid of
            undefined -> Bindings;
            _ -> maps:put(?INTERNAL_REGISTRY_KEY, RegistryPid, Bindings)
        end,
    {RawResult, UpdatedBindings} = apply(ModuleName, eval, [BindingsWithRegistry]),
    CleanBindings = strip_internal_bindings(UpdatedBindings),
    Result = maybe_await_future(RawResult),
    process_eval_result(Result, Expression, CleanBindings, State).

%% @doc Process evaluation result: handle rejected futures and assignments.
-spec process_eval_result(
    term(),
    string(),
    map(),
    beamtalk_repl_state:state()
) ->
    {ok, term(), beamtalk_repl_state:state()}
    | {error, term(), beamtalk_repl_state:state()}.
process_eval_result({future_rejected, ErrorReason}, _Expression, CleanBindings, State) ->
    %% ADR 0015: Wrap rejected future error and bind to _error
    FutExObj = beamtalk_exception_handler:ensure_wrapped(ErrorReason),
    FutBindings = maps:put('_error', FutExObj, CleanBindings),
    FinalState = beamtalk_repl_state:set_bindings(FutBindings, State),
    {error, FutExObj, FinalState};
process_eval_result(Result, Expression, CleanBindings, State) ->
    case extract_assignment(Expression) of
        {ok, VarName} ->
            NewBindings = maps:put(VarName, Result, CleanBindings),
            FinalState = beamtalk_repl_state:set_bindings(NewBindings, State),
            {ok, Result, FinalState};
        none ->
            FinalState = beamtalk_repl_state:set_bindings(CleanBindings, State),
            {ok, Result, FinalState}
    end.

%% @doc Purge eval module if no living actors reference it.
-spec cleanup_module(atom(), pid() | undefined) -> ok.
cleanup_module(ModuleName, RegistryPid) ->
    case should_purge_module(ModuleName, RegistryPid) of
        true ->
            case code:purge(ModuleName) of
                true -> code:delete(ModuleName);
                false -> ok
            end;
        false ->
            ok
    end,
    ok.

%% @doc Compile a Beamtalk expression and return Core Erlang source (BT-700).
%% Does NOT evaluate — only compiles and returns the generated Core Erlang.
-spec do_show_codegen(string(), beamtalk_repl_state:state()) ->
    {ok, binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), [binary()], beamtalk_repl_state:state()}.
do_show_codegen(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    Bindings = beamtalk_repl_state:get_bindings(State),
    SourceBin = list_to_binary(Expression),
    ModNameBin = iolist_to_binary(["beamtalk_repl_codegen_", integer_to_list(Counter)]),
    KnownVars = [
        atom_to_binary(K, utf8)
     || K <- maps:keys(Bindings),
        is_atom(K),
        not is_internal_key(K)
    ],
    try beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars) of
        {ok, class_definition, ClassInfo} ->
            #{core_erlang := CoreErlang, warnings := Warnings} = ClassInfo,
            {ok, CoreErlang, Warnings, NewState};
        {ok, method_definition, _MethodInfo} ->
            {error,
                {compile_error, <<"show-codegen does not support standalone method definitions">>},
                [], NewState};
        {ok, CoreErlang, Warnings} ->
            {ok, CoreErlang, Warnings, NewState};
        {error, Diagnostics} ->
            {error, {compile_error, format_formatted_diagnostics(Diagnostics)}, [], NewState}
    catch
        exit:{noproc, _} ->
            {error,
                {internal_error,
                    <<"Compiler not available. Ensure beamtalk_compiler application is started.">>},
                [], NewState};
        exit:{timeout, _} ->
            {error, {timeout, <<"Compilation timed out.">>}, [], NewState};
        exit:{Reason, _} ->
            {error,
                {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))},
                [], NewState};
        error:Reason ->
            {error,
                {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))},
                [], NewState};
        throw:Reason ->
            {error,
                {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))},
                [], NewState}
    end.
%% @doc Load a Beamtalk file and register its classes.
-spec handle_load(string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    %% Check if file exists
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            %% Read file source
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    %% Detect if file is from the stdlib (lib/ directory)
                    StdlibMode = is_stdlib_path(Path),
                    %% BT-775: Detect package context and compute module name override
                    ModuleNameOverride = compute_package_module_name(Path),
                    %% Compile and load
                    case compile_file(Source, Path, StdlibMode, ModuleNameOverride) of
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(
                                Binary,
                                ClassNames,
                                ModuleName,
                                Source,
                                Path,
                                State
                            );
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

%% @doc Load Beamtalk source from an inline binary string (no file path).
%% Used by the browser workspace Editor pane to compile and load classes.
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    Source = binary_to_list(SourceBin),
    case compile_file(Source, Label, false, undefined) of
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(
                Binary,
                ClassNames,
                ModuleName,
                Source,
                undefined,
                State
            );
        {error, Reason} ->
            {error, Reason, State}
    end.

%%% Internal functions

%% @doc Load a compiled module, register classes, and update state.
%% SourcePath is a file path (for file loads) or undefined (for inline loads).
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
            %% Track loaded module (avoid duplicates on reload)
            LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
            NewState1 =
                case lists:member(ModuleName, LoadedModules) of
                    true -> State;
                    false -> beamtalk_repl_state:add_loaded_module(ModuleName, State)
                end,
            %% Track module in module tracker (SourcePath may be undefined for inline loads)
            Tracker = beamtalk_repl_state:get_module_tracker(NewState1),
            NewTracker = beamtalk_repl_modules:add_module(ModuleName, SourcePath, Tracker),
            NewState2 = beamtalk_repl_state:set_module_tracker(NewTracker, NewState1),
            %% BT-571: Store class source for method patching
            NewState3 = lists:foldl(
                fun(#{name := Name}, AccState) ->
                    NameBin = list_to_binary(Name),
                    beamtalk_repl_state:set_class_source(
                        NameBin, Source, AccState
                    )
                end,
                NewState2,
                ClassNames
            ),
            {ok, ClassNames, NewState3};
        {error, Reason} ->
            %% BT-738: Check for a structured stdlib_shadowing error stored in the
            %% pending errors table by update_class before on_load failed.
            ClassAtoms = class_name_atoms(ClassNames),
            case beamtalk_class_registry:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, State};
                [] ->
                    {error, {load_error, Reason}, State}
            end
    end.

%% BT-571: Handle inline class definition result.
%% Loads the compiled class module, registers its classes, and stores source.
%% BT-885: Evaluates any trailing expressions from ClassInfo after loading the class.
handle_class_definition(ClassInfo, Warnings, Expression, State, RegistryPid, Subscriber) ->
    #{binary := Binary, module_name := ClassModName, classes := Classes} = ClassInfo,
    case code:load_binary(ClassModName, "", Binary) of
        {module, ClassModName} ->
            %% Activate module (register classes, hot reload, metadata)
            activate_module(ClassModName, Classes),
            %% Track loaded module
            LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
            NewState1 =
                case lists:member(ClassModName, LoadedModules) of
                    true -> State;
                    false -> beamtalk_repl_state:add_loaded_module(ClassModName, State)
                end,
            %% Store class source for later method patching (all classes)
            {ClassName, NewState2} =
                case Classes of
                    [#{name := FirstName} | _] ->
                        StoreFun = fun(#{name := Name}, AccState) ->
                            beamtalk_repl_state:set_class_source(
                                Name, Expression, AccState
                            )
                        end,
                        {FirstName, lists:foldl(StoreFun, NewState1, Classes)};
                    _ ->
                        FallbackName = atom_to_binary(ClassModName, utf8),
                        {FallbackName,
                            beamtalk_repl_state:set_class_source(
                                FallbackName, Expression, NewState1
                            )}
                end,
            %% BT-885: Evaluate trailing expressions if present
            case maps:find(trailing_binary, ClassInfo) of
                {ok, TrailingBinary} ->
                    TrailingModName = maps:get(trailing_module_name, ClassInfo),
                    Bindings = beamtalk_repl_state:get_bindings(NewState2),
                    case code:load_binary(TrailingModName, "", TrailingBinary) of
                        {module, TrailingModName} ->
                            eval_loaded_module(
                                TrailingModName,
                                Expression,
                                Bindings,
                                RegistryPid,
                                Subscriber,
                                Warnings,
                                NewState2
                            );
                        {error, LoadReason} ->
                            {error, {load_error, LoadReason}, <<>>, Warnings, NewState2}
                    end;
                error ->
                    {ok, ClassName, <<>>, Warnings, NewState2}
            end;
        {error, Reason} ->
            %% BT-738: Check for a structured stdlib_shadowing error stored in the
            %% pending errors table by update_class before on_load failed.
            ClassAtoms = class_name_atoms(Classes),
            case beamtalk_class_registry:drain_pending_load_errors_by_names(ClassAtoms) of
                [{_ClassName, StructuredError} | _] ->
                    {error, StructuredError, <<>>, Warnings, State};
                [] ->
                    {error, {load_error, Reason}, <<>>, Warnings, State}
            end
    end.

%% BT-571: Handle standalone method definition result.
%% Recompiles the target class with the new/updated method.
handle_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin, selector := SelectorBin} = MethodInfo,
    %% Get existing class source from state
    ExistingSource = beamtalk_repl_state:get_class_source(ClassNameBin, State),
    case ExistingSource of
        undefined ->
            ErrorMsg =
                <<"Class not found: ", ClassNameBin/binary,
                    ". Define the class first before adding methods.">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            %% Combine class source with new method definition
            CombinedSource = ClassSource ++ "\n" ++ Expression,
            SourceBin = list_to_binary(CombinedSource),
            Options = #{stdlib_mode => false, workspace_mode => true},
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok,
                    #{
                        core_erlang := CoreErlang,
                        module_name := ModNameBin,
                        classes := Classes
                    } = CompileResult} ->
                    %% Combine warnings from initial parse and recompile
                    RecompileWarnings = maps:get(warnings, CompileResult, []),
                    AllWarnings = Warnings ++ RecompileWarnings,
                    ModName = binary_to_atom(ModNameBin, utf8),
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            case code:load_binary(ModName, "", Binary) of
                                {module, ModName} ->
                                    %% Activate module (register classes, hot reload, metadata)
                                    activate_module(ModName, Classes),
                                    %% Update stored class source with the combined version
                                    NewState = beamtalk_repl_state:set_class_source(
                                        ClassNameBin, CombinedSource, State
                                    ),
                                    Result = <<ClassNameBin/binary, ">>", SelectorBin/binary>>,
                                    {ok, Result, <<>>, AllWarnings, NewState};
                                {error, LoadReason} ->
                                    %% BT-738: Check for a structured stdlib_shadowing error stored
                                    %% in the pending errors table by update_class before on_load failed.
                                    ClassAtoms = class_name_atoms(Classes),
                                    case
                                        beamtalk_class_registry:drain_pending_load_errors_by_names(
                                            ClassAtoms
                                        )
                                    of
                                        [{_ClassName, StructuredError} | _] ->
                                            {error, StructuredError, <<>>, AllWarnings, State};
                                        [] ->
                                            {error, {load_error, LoadReason}, <<>>, AllWarnings,
                                                State}
                                    end
                            end;
                        {error, Reason} ->
                            ErrorMsg = iolist_to_binary(
                                io_lib:format("Core compile error: ~p", [Reason])
                            ),
                            {error, {compile_error, ErrorMsg}, <<>>, AllWarnings, State}
                    end;
                {error, Diagnostics} ->
                    ErrorMsg = iolist_to_binary(
                        io_lib:format("Compile error: ~p", [Diagnostics])
                    ),
                    {error, {compile_error, ErrorMsg}, <<>>, Warnings, State}
            end
    end.

%% @doc Compute a package-qualified module name for a file being loaded (BT-775).
%% When the file belongs to the current package (i.e., is under {project_path}/src/
%% or {project_path}/test/), returns the module name matching the build system
%% convention: bt@{package}@{relative_path} (or bt@{package}@test@{relative_path}).
%% Returns `undefined` for non-package files or when no package is configured.
-spec compute_package_module_name(string()) -> binary() | undefined.
compute_package_module_name(Path) ->
    %% Single gen_server call to get both package_name and project_path
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{package_name := PackageName, project_path := ProjectPath}} when
            is_binary(PackageName), is_binary(ProjectPath)
        ->
            AbsPath = filename:absname(Path),
            ProjectRoot = binary_to_list(ProjectPath),
            %% Try src/ first, then test/ (BT-775)
            case try_package_relative(AbsPath, ProjectRoot, "src") of
                {ok, ModuleName} ->
                    iolist_to_binary(["bt@", PackageName, "@", ModuleName]);
                undefined ->
                    case try_package_relative(AbsPath, ProjectRoot, "test") of
                        {ok, ModuleName} ->
                            iolist_to_binary(
                                ["bt@", PackageName, "@test@", ModuleName]
                            );
                        undefined ->
                            undefined
                    end
            end;
        _ ->
            undefined
    end.

%% @private
%% Check if AbsPath is under ProjectRoot/SubDir and return the relative module path.
-spec try_package_relative(string(), string(), string()) ->
    {ok, iodata()} | undefined.
try_package_relative(AbsPath, ProjectRoot, SubDir) ->
    Dir = filename:join(ProjectRoot, SubDir),
    AbsDir = filename:absname(Dir),
    %% Use filename:split for platform-safe prefix matching (BT-775)
    DirParts = filename:split(AbsDir),
    PathParts = filename:split(AbsPath),
    DirLen = length(DirParts),
    case
        length(PathParts) > DirLen andalso
            lists:prefix(DirParts, PathParts)
    of
        true ->
            RelParts = lists:nthtail(DirLen, PathParts),
            %% Strip extension from the last segment
            Last = lists:last(RelParts),
            RelPartsNoExt = lists:droplast(RelParts) ++ [filename:rootname(Last)],
            SnakeSegments = [to_snake_case(S) || S <- RelPartsNoExt],
            {ok, lists:join("@", SnakeSegments)};
        false ->
            undefined
    end.

%% @private
%% Convert a string to snake_case (e.g., "SchemeSymbol" -> "scheme_symbol").
%% Matches the Rust to_module_name() convention: inserts underscore before
%% uppercase only when the previous character was lowercase.
%% This means "HTTPRouter" -> "httprouter" (no underscores within acronyms).
-spec to_snake_case(string()) -> string().
to_snake_case([]) ->
    [];
to_snake_case([H | T]) ->
    %% PrevWasLower tracks if prev char was lowercase (like Rust impl)
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

%% @doc Check if a file path refers to a stdlib file (under stdlib/src/ directory).
%% Matches both relative paths (stdlib/src/Integer.bt) and absolute paths
%% containing /stdlib/src/ as a path component.
-spec is_stdlib_path(string()) -> boolean().
is_stdlib_path("stdlib/src/" ++ _) ->
    true;
is_stdlib_path(Path) ->
    case string:find(Path, "/stdlib/src/") of
        nomatch -> false;
        _ -> true
    end.

%% @doc Compile a Beamtalk expression to bytecode via OTP Port backend.
-spec compile_expression(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]}
    | {ok, class_definition, map(), [binary()]}
    | {ok, method_definition, map(), [binary()]}
    | {error, term()}.
compile_expression(Expression, ModuleName, Bindings) ->
    compile_expression_via_port(Expression, ModuleName, Bindings).

%% Compile expression via beamtalk_compiler OTP app (port backend).
compile_expression_via_port(Expression, ModuleName, Bindings) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = [
        atom_to_binary(K, utf8)
     || K <- maps:keys(Bindings),
        is_atom(K),
        not is_internal_key(K)
    ],
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars) of
                %% BT-571: Inline class definition
                {ok, class_definition, ClassInfo} ->
                    #{
                        core_erlang := CoreErlang,
                        module_name := ClassModNameBin,
                        classes := Classes,
                        warnings := Warnings
                    } = ClassInfo,
                    ClassModName = binary_to_atom(ClassModNameBin, utf8),
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            %% BT-885: Compile trailing expressions if present
                            TrailingResult =
                                case maps:find(trailing_core_erlang, ClassInfo) of
                                    {ok, TrailingCoreErlang} ->
                                        case
                                            beamtalk_compiler:compile_core_erlang(
                                                TrailingCoreErlang
                                            )
                                        of
                                            {ok, _TCompiledMod, TBinary} ->
                                                {ok, TBinary, ModuleName};
                                            {error, Reason} ->
                                                {error, Reason}
                                        end;
                                    error ->
                                        none
                                end,
                            case TrailingResult of
                                {error, TrailingErr} ->
                                    {error,
                                        iolist_to_binary(
                                            io_lib:format(
                                                "Trailing expression compile error: ~p",
                                                [TrailingErr]
                                            )
                                        )};
                                _ ->
                                    BaseInfo = #{
                                        binary => Binary,
                                        module_name => ClassModName,
                                        classes => Classes
                                    },
                                    ClassInfo2 =
                                        case TrailingResult of
                                            {ok, TrailingBinary, TrailingModName} ->
                                                BaseInfo#{
                                                    trailing_binary => TrailingBinary,
                                                    trailing_module_name => TrailingModName
                                                };
                                            none ->
                                                BaseInfo
                                        end,
                                    {ok, class_definition, ClassInfo2, Warnings}
                            end;
                        {error, Reason} ->
                            {error,
                                iolist_to_binary(
                                    io_lib:format("Core Erlang compile error: ~p", [Reason])
                                )}
                    end;
                %% BT-571: Standalone method definition
                {ok, method_definition, MethodInfo} ->
                    #{
                        class_name := ClassName,
                        selector := Selector,
                        is_class_method := IsClassMethod,
                        method_source := MethodSource
                    } = MethodInfo,
                    Warnings = maps:get(warnings, MethodInfo, []),
                    {ok, method_definition,
                        #{
                            class_name => ClassName,
                            selector => Selector,
                            is_class_method => IsClassMethod,
                            method_source => MethodSource
                        },
                        Warnings};
                %% Standard expression
                {ok, CoreErlang, Warnings} ->
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            {ok, Binary, {port_compiled}, Warnings};
                        {error, Reason} ->
                            {error,
                                iolist_to_binary(
                                    io_lib:format("Core Erlang compile error: ~p", [Reason])
                                )}
                    end;
                {error, Diagnostics} ->
                    {error, format_formatted_diagnostics(Diagnostics)}
            end
        end,
        direct
    ).

%% @doc Compile a file and extract class metadata via OTP Port backend.
-spec compile_file(string(), string(), boolean(), binary() | undefined) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()} | {error, term()}.
compile_file(Source, Path, StdlibMode, ModuleNameOverride) ->
    compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride).

%% Compile file via beamtalk_compiler OTP app (port backend).
compile_file_via_port(Source, Path, StdlibMode, ModuleNameOverride) ->
    SourceBin = list_to_binary(Source),
    Options0 = #{stdlib_mode => StdlibMode},
    %% BT-775: Pass module_name override when loading a package file
    Options1 =
        case ModuleNameOverride of
            undefined -> Options0;
            _ -> Options0#{module_name => ModuleNameOverride}
        end,
    %% BT-845/BT-860: Pass source file path so compiler embeds beamtalk_source attribute.
    %% Only set source_path when the path looks like a real filesystem path (contains
    %% a directory separator or has a .bt extension). This prevents bogus sourceFile
    %% metadata when handle_load_source/3 is called with a non-file label.
    Options =
        case Path of
            undefined ->
                Options1;
            L when is_list(L) ->
                IsFilePath =
                    lists:member($/, L) orelse
                        lists:member($\\, L) orelse
                        filename:extension(L) =:= ".bt",
                case IsFilePath of
                    true -> Options1#{source_path => list_to_binary(L)};
                    false -> Options1
                end;
            _ ->
                Options1
        end,
    wrap_compiler_errors(
        fun() ->
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok, #{
                    core_erlang := CoreErlang,
                    module_name := ModNameBin,
                    classes := Classes
                }} ->
                    ModuleName = binary_to_atom(ModNameBin, utf8),
                    case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                        {ok, _CompiledMod, Binary} ->
                            ClassNames = [
                                #{
                                    name => binary_to_list(maps:get(name, C, <<"">>)),
                                    superclass => binary_to_list(
                                        maps:get(superclass, C, <<"Object">>)
                                    )
                                }
                             || C <- Classes
                            ],
                            {ok, Binary, ClassNames, ModuleName};
                        {error, Reason} ->
                            {error, {core_compile_error, Reason}}
                    end;
                {error, Diagnostics} ->
                    {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
            end
        end,
        wrapped
    ).

%% @doc Wrap compiler calls with shared error handling for exit/error/throw.
%% ErrorStyle controls error wrapping:
%%   direct  - returns {error, Message} directly (for expression compilation)
%%   wrapped - wraps as {error, {compile_error, Message}} (for file compilation)
-spec wrap_compiler_errors(fun(() -> term()), direct | wrapped) -> term().
wrap_compiler_errors(Fun, ErrorStyle) ->
    try
        Fun()
    catch
        exit:{noproc, _} ->
            wrap_error(
                <<"Compiler not available. Ensure beamtalk_compiler application is started.">>,
                ErrorStyle
            );
        exit:{timeout, _} ->
            wrap_error(<<"Compilation timed out.">>, ErrorStyle);
        exit:{Reason, _} ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle);
        error:Reason ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle);
        throw:Reason ->
            wrap_error(iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason])), ErrorStyle)
    end.

-spec wrap_error(binary(), direct | wrapped) -> {error, term()}.
wrap_error(Msg, direct) -> {error, Msg};
wrap_error(Msg, wrapped) -> {error, {compile_error, Msg}}.

%% @doc Check if a binding key is internal (not a user variable).
-spec is_internal_key(atom()) -> boolean().
is_internal_key(Key) when is_atom(Key) ->
    %% Internal keys start with double underscore
    case atom_to_list(Key) of
        "__" ++ _ -> true;
        _ -> false
    end.

%% @doc Format pre-formatted diagnostics (miette output).
-spec format_formatted_diagnostics(list()) -> binary().
format_formatted_diagnostics([]) ->
    <<"Compilation failed">>;
format_formatted_diagnostics(FormattedList) ->
    %% Join the formatted diagnostics with double newlines for separation
    iolist_to_binary(lists:join(<<"\n\n">>, FormattedList)).

%% @doc Extract variable name from assignment expression.
%%
%% For multi-statement input (e.g. "x := 1. y := 2."), the codegen (BT-780)
%% handles all binding updates via State threading and returns updated bindings
%% directly. Returning `none` for multi-statement input ensures these
%% codegen-managed bindings are not overwritten with the wrong value.
%%
%% Multi-statement detection uses ". " (period + whitespace) rather than "."
%% alone to avoid false positives from float literals (1.5) or string contents
%% with embedded periods followed directly by non-whitespace (e.g., "file.txt").
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    %% Detect multi-statement: a period followed by one-or-more whitespace
    %% characters and then any non-whitespace character (covers identifiers,
    %% digits, parens, quotes, etc. as the start of the next statement).
    %% Requires whitespace to distinguish from floats (1.5) and dotted
    %% names inside strings ("stream_test.txt").
    case re:run(Expression, "\\.\\s+\\S", []) of
        {match, _} ->
            %% Multi-statement: codegen already updated all bindings in State
            none;
        nomatch ->
            case
                re:run(Expression, "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=", [{capture, [1], list}])
            of
                {match, [VarName]} ->
                    {ok, list_to_atom(VarName)};
                nomatch ->
                    none
            end
    end.

%% @doc Activate a loaded module: register classes, trigger hot reload,
%% and update workspace metadata. Called after code:load_binary succeeds.
-spec activate_module(atom(), [map()]) -> ok.
activate_module(ModuleName, Classes) ->
    register_classes(Classes, ModuleName),
    trigger_hot_reload(ModuleName, Classes),
    beamtalk_workspace_meta:register_module(ModuleName),
    beamtalk_workspace_meta:update_activity().

%% @doc Register loaded classes by calling the module's register_class/0 function.
%% This function (generated by codegen) has full metadata including
%% instance_methods and method_source for CompiledMethod introspection.
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

%% @doc Trigger hot reload for existing actors after module reload.
%% Finds actors using the module via the instance registry and triggers
%% code_change with field migration data (BT-572).
-spec trigger_hot_reload(atom(), [map()]) -> ok.
trigger_hot_reload(ModuleName, Classes) ->
    lists:foreach(
        fun(ClassMap) ->
            ClassName =
                case maps:get(name, ClassMap, undefined) of
                    N when is_binary(N) ->
                        try
                            binary_to_existing_atom(N, utf8)
                        catch
                            error:badarg -> undefined
                        end;
                    N when is_atom(N) -> N;
                    N when is_list(N) ->
                        try
                            list_to_existing_atom(N)
                        catch
                            error:badarg -> undefined
                        end;
                    _ ->
                        undefined
                end,
            case ClassName of
                undefined ->
                    ok;
                _ ->
                    Pids = beamtalk_object_instances:all(ClassName),
                    case Pids of
                        [] ->
                            ok;
                        _ ->
                            %% Get new instance vars from class process for migration
                            IVars =
                                case beamtalk_class_registry:whereis_class(ClassName) of
                                    undefined ->
                                        [];
                                    ClassPid ->
                                        try
                                            gen_server:call(ClassPid, instance_variables)
                                        catch
                                            _:_ -> []
                                        end
                                end,
                            Extra = {IVars, ModuleName},
                            beamtalk_hot_reload:trigger_code_change(ModuleName, Pids, Extra)
                    end
            end
        end,
        Classes
    ),
    ok.

%% @doc Auto-await a Future if the result is a tagged future tuple (BT-840).
%% This provides a synchronous REPL experience for async message sends.
%% Returns the awaited value if resolved, `{future_rejected, Reason}' on
%% rejection or timeout, or the original value if not a future.
%%
%% BT-886: Both timeout and rejection are now returned as {future_rejected, Error}
%% so that process_eval_result surfaces them as structured errors to the user,
%% rather than displaying raw tuples.
-spec maybe_await_future(term()) -> term().
maybe_await_future({beamtalk_future, _} = Future) ->
    %% Tagged future — await with a generous timeout for REPL use.
    try beamtalk_future:await(Future, 30000) of
        AwaitedValue ->
            AwaitedValue
    catch
        throw:#beamtalk_error{kind = timeout} = TimeoutError ->
            %% BT-886: Surface timeout as a structured error, not a raw tuple.
            {future_rejected, TimeoutError};
        throw:{future_rejected, #beamtalk_error{} = Reason} ->
            %% Structured error from rejected future (e.g., actor_dead from watcher).
            {future_rejected, Reason};
        throw:{future_rejected, Reason} ->
            %% Return error tuple for REPL inspection instead of throwing.
            %% REPL should be forgiving and allow users to inspect errors.
            {future_rejected, Reason}
    end;
maybe_await_future(Value) ->
    %% Not a future, return as-is
    Value.

%% @doc Check if a module should be purged (no living actors reference it).
-spec should_purge_module(atom(), pid() | undefined) -> boolean().
should_purge_module(_ModuleName, undefined) ->
    %% No registry, purge as normal
    true;
should_purge_module(ModuleName, RegistryPid) ->
    %% Get all actors from registry
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    %% Check if any actor belongs to this module
    HasActors = lists:any(
        fun(#{module := ActorModule}) ->
            ActorModule =:= ModuleName
        end,
        Actors
    ),
    %% Purge only if no actors from this module exist
    not HasActors.

%% @doc Strip internal plumbing keys from bindings map (BT-153).
%% The __repl_actor_registry__ key is injected for codegen to access the registry,
%% but should not be visible to users or persisted in REPL state.
-spec strip_internal_bindings(map()) -> map().
strip_internal_bindings(Bindings) ->
    maps:remove(?INTERNAL_REGISTRY_KEY, Bindings).

%%% IO Capture delegation (BT-706: extracted to beamtalk_io_capture.erl)
%%% See beamtalk_io_capture module for start/stop/capture logic.

%% @doc Inject captured output and warnings into an eval result tuple.
-spec inject_output(tuple(), binary(), [binary()]) -> tuple().
inject_output({ok, Result, State}, Output, Warnings) ->
    {ok, Result, Output, Warnings, State};
inject_output({error, Reason, State}, Output, Warnings) ->
    {error, Reason, Output, Warnings, State}.

%% @doc Compile and load a Beamtalk source file without REPL session state.
%%
%% BT-845: ADR 0040 Phase 2 — stateless reload for the `Counter reload` primitive.
%% Called from `beamtalk_behaviour_intrinsics:classReload/1` via `erlang:apply/3`
%% to avoid a compile-time dependency from beamtalk_runtime to beamtalk_workspace.
%%
-spec reload_class_file(string()) -> ok | {error, term()}.
reload_class_file(Path) ->
    reload_class_file_impl(Path, undefined).

%% BT-868: ExpectedClassName (atom) is verified against the compiled class list.
%% Returns `{error, {class_not_found, ...}}` if the file no longer defines that class.
-spec reload_class_file(string(), atom()) -> ok | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    reload_class_file_impl(Path, ExpectedClassName).

-spec reload_class_file_impl(string(), atom() | undefined) ->
    ok | {error, term()}.
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
                    %% BT-897: Compute package module name for subdirectory files,
                    %% matching handle_load/2 behavior.
                    ModuleNameOverride = compute_package_module_name(Path),
                    case compile_file(Source, Path, false, ModuleNameOverride) of
                        {ok, Binary, ClassNames, ModuleName} ->
                            %% BT-868: Verify the expected class is still defined
                            case
                                verify_class_present(
                                    ExpectedClassName, ClassNames, Path
                                )
                            of
                                ok ->
                                    case
                                        code:load_binary(
                                            ModuleName, Path, Binary
                                        )
                                    of
                                        {module, ModuleName} ->
                                            activate_module(
                                                ModuleName, ClassNames
                                            ),
                                            ok;
                                        {error, Reason} ->
                                            {error, {load_error, Reason}}
                                    end;
                                {error, _} = Err ->
                                    Err
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @private
%% BT-868: Check that the expected class name appears in the compiled class list.
-spec verify_class_present(
    atom() | undefined, [#{name := string()}], string()
) -> ok | {error, term()}.
verify_class_present(undefined, _ClassNames, _Path) ->
    ok;
verify_class_present(ExpectedClassName, ClassNames, Path) ->
    ExpectedName = atom_to_list(ExpectedClassName),
    DefinedNames = [N || #{name := N} <- ClassNames],
    case lists:member(ExpectedName, DefinedNames) of
        true -> ok;
        false -> {error, {class_not_found, ExpectedClassName, Path, DefinedNames}}
    end.

%% @private
%% @doc Convert a list of class info maps to a list of existing atoms.
%% BT-738: Used to look up pending load errors after a failed on_load.
%% Uses safe_to_existing_atom to avoid atom table pollution.
-spec class_name_atoms([map()]) -> [atom()].
class_name_atoms(Classes) ->
    lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                case beamtalk_repl_server:safe_to_existing_atom(list_to_binary(Name)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            (#{name := Name}) when is_binary(Name) ->
                case beamtalk_repl_server:safe_to_existing_atom(Name) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            (_) ->
                false
        end,
        Classes
    ).
