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

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([extract_assignment/1,
         format_formatted_diagnostics/1,
         maybe_await_future/1, should_purge_module/2,
         strip_internal_bindings/1,
         start_io_capture/0, start_io_capture/1,
         stop_io_capture/1, io_capture_loop/2,
         is_stdlib_path/1, reset_captured_group_leaders/2,
         inject_output/3, handle_io_request/2,
         is_internal_key/1, activate_module/2,
         register_classes/2, trigger_hot_reload/2,
         io_passthrough_loop/1,
         is_stdin_request/1, handle_stdin_request/2, prompt_to_binary/1,
         handle_class_definition/4, handle_method_definition/4,
         compile_expression_via_port/3, compile_file_via_port/3]).
-endif.

-define(INTERNAL_REGISTRY_KEY, '__repl_actor_registry__').
-define(IO_CAPTURE_TIMEOUT, 5000).
%% BT-698: Timeout for stdin input during eval (30 seconds)
-define(STDIN_TIMEOUT, 30000).

%%% Public API

%% @doc Evaluate a Beamtalk expression.
%% This is the core of the REPL - compile, load, and execute.
%% If the result is a Future PID, automatically awaits it before returning.
%% Returns captured stdout as a binary (empty when no output was produced).
%% Returns warnings as a list of formatted diagnostic strings.
-spec do_eval(string(), beamtalk_repl_state:state()) -> 
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()} | 
    {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State) ->
    do_eval(Expression, State, undefined).

%% @doc Evaluate with optional streaming subscriber (BT-696).
%% When Subscriber is a pid, IO chunks are forwarded as {eval_out, Chunk}.
-spec do_eval(string(), beamtalk_repl_state:state(), pid() | undefined) -> 
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()} | 
    {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
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
        {ok, class_definition, ClassInfo, Warnings} ->
            handle_class_definition(ClassInfo, Warnings, Expression, NewState);
        %% BT-571: Standalone method definition — add/replace method on existing class
        {ok, method_definition, MethodInfo, Warnings} ->
            handle_method_definition(MethodInfo, Warnings, Expression, NewState);
        {ok, Binary, _ResultExpr, Warnings} ->
            %% Load the compiled module
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    %% Capture stdout during evaluation (BT-696: with streaming)
                    CaptureRef = start_io_capture(Subscriber),
                    EvalResult = try
                        %% Add registry PID to bindings so generated code can access it
                        BindingsWithRegistry = case RegistryPid of
                            undefined -> Bindings;
                            _ -> maps:put(?INTERNAL_REGISTRY_KEY, RegistryPid, Bindings)
                        end,
                        {RawResult, UpdatedBindings} = apply(ModuleName, eval, [BindingsWithRegistry]),
                        %% Strip internal keys before persisting bindings
                        CleanBindings = strip_internal_bindings(UpdatedBindings),
                        %% Auto-await futures for synchronous REPL experience
                        Result = maybe_await_future(RawResult),
                        %% Check if result is a rejected future - treat as error
                        case Result of
                            {future_rejected, ErrorReason} ->
                                %% ADR 0015: Wrap rejected future error and bind to _error
                                FutExObj = beamtalk_exception_handler:ensure_wrapped(ErrorReason),
                                FutBindings = maps:put('_error', FutExObj, CleanBindings),
                                FinalState = beamtalk_repl_state:set_bindings(FutBindings, NewState),
                                {error, FutExObj, FinalState};
                            _ ->
                                %% Normal result - check if this was an assignment
                                case extract_assignment(Expression) of
                                    {ok, VarName} ->
                                        %% Assignment: merge updated bindings with new assignment
                                        NewBindings = maps:put(VarName, Result, CleanBindings),
                                        FinalState = beamtalk_repl_state:set_bindings(NewBindings, NewState),
                                        {ok, Result, FinalState};
                                    none ->
                                        %% No assignment: use updated bindings from mutations
                                        FinalState = beamtalk_repl_state:set_bindings(CleanBindings, NewState),
                                        {ok, Result, FinalState}
                                end
                        end
                    catch
                        Class:Reason:_Stacktrace ->
                            %% ADR 0015: Wrap error as Exception and bind to _error
                            CaughtExObj = beamtalk_exception_handler:ensure_wrapped(Reason),
                            CaughtBindings = maps:put('_error', CaughtExObj, Bindings),
                            CaughtState = beamtalk_repl_state:set_bindings(CaughtBindings, NewState),
                            {error, {eval_error, Class, CaughtExObj}, CaughtState}
                    after
                        %% Only purge module if it has no living actors
                        case should_purge_module(ModuleName, RegistryPid) of
                            true ->
                                %% code:purge/1 returns true if successful, false if in use
                                %% Must purge before delete to avoid "must be purged" error
                                case code:purge(ModuleName) of
                                    true -> code:delete(ModuleName);
                                    false -> ok  %% Module in use, skip deletion
                                end;
                            false ->
                                %% Module still has actors, keep it loaded
                                ok
                        end
                    end,
                    %% Retrieve captured output (always, even on error)
                    Output = stop_io_capture(CaptureRef),
                    inject_output(EvalResult, Output, Warnings);
                {error, Reason} ->
                    {error, {load_error, Reason}, <<>>, [], NewState}
            end;
        {error, Reason} ->
            {error, {compile_error, Reason}, <<>>, [], NewState}
    end.

%% @doc Compile a Beamtalk expression and return Core Erlang source (BT-700).
%% Does NOT evaluate — only compiles and returns the generated Core Erlang.
-spec do_show_codegen(string(), beamtalk_repl_state:state()) ->
    {ok, binary(), [binary()], beamtalk_repl_state:state()} |
    {error, term(), [binary()], beamtalk_repl_state:state()}.
do_show_codegen(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    Bindings = beamtalk_repl_state:get_bindings(State),
    SourceBin = list_to_binary(Expression),
    ModNameBin = iolist_to_binary(["beamtalk_repl_codegen_", integer_to_list(Counter)]),
    KnownVars = [atom_to_binary(K, utf8)
                 || K <- maps:keys(Bindings),
                    is_atom(K),
                    not is_internal_key(K)],
    try beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars) of
        {ok, class_definition, ClassInfo} ->
            #{core_erlang := CoreErlang, warnings := Warnings} = ClassInfo,
            {ok, CoreErlang, Warnings, NewState};
        {ok, method_definition, _MethodInfo} ->
            {error, {compile_error, <<"show-codegen does not support standalone method definitions">>}, [], NewState};
        {ok, CoreErlang, Warnings} ->
            {ok, CoreErlang, Warnings, NewState};
        {error, Diagnostics} ->
            {error, {compile_error, format_formatted_diagnostics(Diagnostics)}, [], NewState}
    catch
        exit:{noproc, _} ->
            {error, {internal_error, <<"Compiler not available. Ensure beamtalk_compiler application is started.">>}, [], NewState};
        exit:{timeout, _} ->
            {error, {timeout, <<"Compilation timed out.">>}, [], NewState};
        exit:{Reason, _} ->
            {error, {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}, [], NewState};
        error:Reason ->
            {error, {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}, [], NewState};
        throw:Reason ->
            {error, {internal_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}, [], NewState}
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
                    %% Compile and load
                    case compile_file(Source, Path, StdlibMode) of
                        {ok, Binary, ClassNames, ModuleName} ->
                            load_compiled_module(Binary, ClassNames, ModuleName,
                                                 Source, Path, State);
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
    case compile_file(Source, Label, false) of
        {ok, Binary, ClassNames, ModuleName} ->
            load_compiled_module(Binary, ClassNames, ModuleName,
                                 Source, undefined, State);
        {error, Reason} ->
            {error, Reason, State}
    end.

%%% Internal functions

%% @doc Load a compiled module, register classes, and update state.
%% SourcePath is a file path (for file loads) or undefined (for inline loads).
-spec load_compiled_module(binary(), [map()], atom(), string(),
                           string() | undefined, beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
load_compiled_module(Binary, ClassNames, ModuleName, Source, SourcePath, State) ->
    LoadPath = case SourcePath of undefined -> ""; _ -> SourcePath end,
    case code:load_binary(ModuleName, LoadPath, Binary) of
        {module, ModuleName} ->
            activate_module(ModuleName, ClassNames),
            %% Track loaded module (avoid duplicates on reload)
            LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
            NewState1 = case lists:member(ModuleName, LoadedModules) of
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
                        NameBin, Source, AccState)
                end,
                NewState2, ClassNames),
            {ok, ClassNames, NewState3};
        {error, Reason} ->
            {error, {load_error, Reason}, State}
    end.

%% BT-571: Handle inline class definition result.
%% Loads the compiled class module, registers its classes, and stores source.
handle_class_definition(ClassInfo, Warnings, Expression, State) ->
    #{binary := Binary, module_name := ClassModName, classes := Classes} = ClassInfo,
    case code:load_binary(ClassModName, "", Binary) of
        {module, ClassModName} ->
            %% Activate module (register classes, hot reload, metadata)
            activate_module(ClassModName, Classes),
            %% Track loaded module
            LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
            NewState1 = case lists:member(ClassModName, LoadedModules) of
                true -> State;
                false -> beamtalk_repl_state:add_loaded_module(ClassModName, State)
            end,
            %% Store class source for later method patching (all classes)
            {ClassName, NewState2} = case Classes of
                [#{name := FirstName} | _] ->
                    StoreFun = fun(#{name := Name}, AccState) ->
                        beamtalk_repl_state:set_class_source(
                            Name, Expression, AccState)
                    end,
                    {FirstName, lists:foldl(StoreFun, NewState1, Classes)};
                _ ->
                    FallbackName = atom_to_binary(ClassModName, utf8),
                    {FallbackName,
                     beamtalk_repl_state:set_class_source(
                         FallbackName, Expression, NewState1)}
            end,
            {ok, ClassName, <<>>, Warnings, NewState2};
        {error, Reason} ->
            {error, {load_error, Reason}, <<>>, Warnings, State}
    end.

%% BT-571: Handle standalone method definition result.
%% Recompiles the target class with the new/updated method.
handle_method_definition(MethodInfo, Warnings, Expression, State) ->
    #{class_name := ClassNameBin, selector := SelectorBin} = MethodInfo,
    %% Get existing class source from state
    ExistingSource = beamtalk_repl_state:get_class_source(ClassNameBin, State),
    case ExistingSource of
        undefined ->
            ErrorMsg = <<"Class not found: ", ClassNameBin/binary,
                         ". Define the class first before adding methods.">>,
            {error, {compile_error, ErrorMsg}, <<>>, Warnings, State};
        ClassSource ->
            %% Combine class source with new method definition
            CombinedSource = ClassSource ++ "\n" ++ Expression,
            SourceBin = list_to_binary(CombinedSource),
            Options = #{stdlib_mode => false, workspace_mode => true},
            case beamtalk_compiler:compile(SourceBin, Options) of
                {ok, #{core_erlang := CoreErlang, module_name := ModNameBin,
                       classes := Classes} = CompileResult} ->
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
                                        ClassNameBin, CombinedSource, State),
                                    Result = <<ClassNameBin/binary, ">>",
                                               SelectorBin/binary>>,
                                    {ok, Result, <<>>, AllWarnings, NewState};
                                {error, Reason} ->
                                    {error, {load_error, Reason}, <<>>, AllWarnings, State}
                            end;
                        {error, Reason} ->
                            ErrorMsg = iolist_to_binary(
                                io_lib:format("Core compile error: ~p", [Reason])),
                            {error, {compile_error, ErrorMsg}, <<>>, AllWarnings, State}
                    end;
                {error, Diagnostics} ->
                    ErrorMsg = iolist_to_binary(
                        io_lib:format("Compile error: ~p", [Diagnostics])),
                    {error, {compile_error, ErrorMsg}, <<>>, Warnings, State}
            end
    end.

%% @doc Check if a file path refers to a stdlib file (under lib/ directory).
%% Matches both relative paths (lib/Integer.bt) and absolute paths
%% containing /lib/ as a path component.
-spec is_stdlib_path(string()) -> boolean().
is_stdlib_path("lib/" ++ _) -> true;
is_stdlib_path(Path) ->
    case string:find(Path, "/lib/") of
        nomatch -> false;
        _ -> true
    end.

%% @doc Compile a Beamtalk expression to bytecode via OTP Port backend.
-spec compile_expression(string(), atom(), map()) ->
    {ok, binary(), term(), [binary()]} |
    {ok, class_definition, map(), [binary()]} |
    {ok, method_definition, map(), [binary()]} |
    {error, term()}.
compile_expression(Expression, ModuleName, Bindings) ->
    compile_expression_via_port(Expression, ModuleName, Bindings).

%% Compile expression via beamtalk_compiler OTP app (port backend).
compile_expression_via_port(Expression, ModuleName, Bindings) ->
    SourceBin = list_to_binary(Expression),
    ModNameBin = atom_to_binary(ModuleName, utf8),
    KnownVars = [atom_to_binary(K, utf8)
                 || K <- maps:keys(Bindings),
                    is_atom(K),
                    not is_internal_key(K)],
    try beamtalk_compiler:compile_expression(SourceBin, ModNameBin, KnownVars) of
        %% BT-571: Inline class definition
        {ok, class_definition, ClassInfo} ->
            #{core_erlang := CoreErlang, module_name := ClassModNameBin,
              classes := Classes, warnings := Warnings} = ClassInfo,
            ClassModName = binary_to_atom(ClassModNameBin, utf8),
            case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                {ok, _CompiledMod, Binary} ->
                    {ok, class_definition, #{binary => Binary, module_name => ClassModName,
                                             classes => Classes}, Warnings};
                {error, Reason} ->
                    {error, iolist_to_binary(io_lib:format("Core Erlang compile error: ~p", [Reason]))}
            end;
        %% BT-571: Standalone method definition
        {ok, method_definition, MethodInfo} ->
            #{class_name := ClassName, selector := Selector,
              is_class_method := IsClassMethod,
              method_source := MethodSource} = MethodInfo,
            Warnings = maps:get(warnings, MethodInfo, []),
            {ok, method_definition, #{class_name => ClassName, selector => Selector,
                                      is_class_method => IsClassMethod,
                                      method_source => MethodSource}, Warnings};
        %% Standard expression
        {ok, CoreErlang, Warnings} ->
            case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                {ok, _CompiledMod, Binary} ->
                    {ok, Binary, {port_compiled}, Warnings};
                {error, Reason} ->
                    {error, iolist_to_binary(io_lib:format("Core Erlang compile error: ~p", [Reason]))}
            end;
        {error, Diagnostics} ->
            {error, format_formatted_diagnostics(Diagnostics)}
    catch
        exit:{noproc, _} ->
            {error, <<"Compiler not available. Ensure beamtalk_compiler application is started.">>};
        exit:{timeout, _} ->
            {error, <<"Compilation timed out.">>};
        exit:{Reason, _} ->
            {error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))};
        error:Reason ->
            {error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))};
        throw:Reason ->
            {error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}
    end.

%% @doc Compile a file and extract class metadata via OTP Port backend.
-spec compile_file(string(), string(), boolean()) ->
    {ok, binary(), [#{name := string(), superclass := string()}], atom()} | {error, term()}.
compile_file(Source, Path, StdlibMode) ->
    compile_file_via_port(Source, Path, StdlibMode).

%% Compile file via beamtalk_compiler OTP app (port backend).
compile_file_via_port(Source, _Path, StdlibMode) ->
    SourceBin = list_to_binary(Source),
    Options = #{stdlib_mode => StdlibMode},
    try beamtalk_compiler:compile(SourceBin, Options) of
        {ok, #{core_erlang := CoreErlang, module_name := ModNameBin,
               classes := Classes}} ->
            ModuleName = binary_to_atom(ModNameBin, utf8),
            case beamtalk_compiler:compile_core_erlang(CoreErlang) of
                {ok, _CompiledMod, Binary} ->
                    ClassNames = [#{
                        name => binary_to_list(maps:get(name, C, <<"">>)),
                        superclass => binary_to_list(maps:get(superclass, C, <<"Object">>))
                    } || C <- Classes],
                    {ok, Binary, ClassNames, ModuleName};
                {error, Reason} ->
                    {error, {core_compile_error, Reason}}
            end;
        {error, Diagnostics} ->
            {error, {compile_error, format_formatted_diagnostics(Diagnostics)}}
    catch
        exit:{noproc, _} ->
            {error, {compile_error, <<"Compiler not available. Ensure beamtalk_compiler application is started.">>}};
        exit:{timeout, _} ->
            {error, {compile_error, <<"Compilation timed out.">>}};
        exit:{Reason, _} ->
            {error, {compile_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}};
        error:Reason ->
            {error, {compile_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}};
        throw:Reason ->
            {error, {compile_error, iolist_to_binary(io_lib:format("Compiler error: ~p", [Reason]))}}
    end.

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
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    case re:run(Expression, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=", [{capture, [1], list}]) of
        {match, [VarName]} ->
            {ok, list_to_atom(VarName)};
        nomatch ->
            none
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
            try ModuleName:register_class()
            catch _:_ -> ok
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
            ClassName = case maps:get(name, ClassMap, undefined) of
                N when is_binary(N) ->
                    try binary_to_existing_atom(N, utf8)
                    catch error:badarg -> undefined
                    end;
                N when is_atom(N) -> N;
                N when is_list(N) ->
                    try list_to_existing_atom(N)
                    catch error:badarg -> undefined
                    end;
                _ -> undefined
            end,
            case ClassName of
                undefined -> ok;
                _ ->
                    Pids = beamtalk_object_instances:all(ClassName),
                    case Pids of
                        [] -> ok;
                        _ ->
                            %% Get new instance vars from class process for migration
                            IVars = case beamtalk_class_registry:whereis_class(ClassName) of
                                undefined -> [];
                                ClassPid ->
                                    try gen_server:call(ClassPid, instance_variables)
                                    catch _:_ -> []
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

%% @doc Auto-await a Future if the result is a Future PID.
%% This provides a synchronous REPL experience for async message sends.
%% Returns the awaited value, or the original value if not a Future.
-spec maybe_await_future(term()) -> term().
maybe_await_future(Value) when is_pid(Value) ->
    %% For REPL purposes, we attempt to await any PID result.
    %% If it's a Future, it will respond with the future protocol.
    %% If it's an actor or other process, it will ignore the message and
    %% we'll timeout waiting for a response.
    %% We use a short timeout (100ms) to detect non-futures quickly.
    TestTimeout = 100,
    Value ! {await, self(), TestTimeout},
    receive
        {future_resolved, Value, AwaitedValue} ->
            %% It was a resolved future
            AwaitedValue;
        {future_rejected, Value, Reason} ->
            %% It was a rejected future - return error tuple for REPL inspection.
            %% We use tuples instead of throwing (like beamtalk_future:await/1 does)
            %% because REPL should be forgiving and allow users to inspect errors.
            {future_rejected, Reason};
        {future_timeout, Value} ->
            %% Future explicitly timed out waiting for resolution.
            %% This confirms it IS a future. Try a longer await.
            try beamtalk_future:await(Value, 5000) of
                AwaitedValue ->
                    AwaitedValue
            catch
                throw:#beamtalk_error{kind = timeout} ->
                    {future_timeout, Value};
                throw:{future_rejected, Reason} ->
                    {future_rejected, Reason}
            end
    after TestTimeout + 50 ->
        %% Not a future process - it didn't respond to the protocol.
        %% Use TestTimeout + 50 to give the future time to send {future_timeout, Value}.
        %% Flush any late responses to avoid mailbox pollution.
        receive
            {future_resolved, Value, _} -> ok;
            {future_rejected, Value, _} -> ok;
            {future_timeout, Value} -> ok
        after 20 ->
            ok
        end,
        %% Return the PID as-is (likely an actor or other process)
        Value
    end;
maybe_await_future({beamtalk_object, _, _, _} = Object) ->
    %% Beamtalk objects (actors) are not futures themselves
    %% Return as-is
    Object;
maybe_await_future(Value) ->
    %% Not a PID or special type, return as-is
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

%%% IO Capture (BT-355)
%%% Captures stdout during eval by temporarily replacing the group_leader
%%% with a custom IO server process.
%%% BT-696: Optionally forwards IO chunks to a subscriber for streaming.

%% @doc Start capturing IO output for the current process.
%% Returns an opaque reference to pass to stop_io_capture/1.
%% Convenience wrapper for start_io_capture(undefined).
-dialyzer({no_unused, start_io_capture/0}).
-spec start_io_capture() -> {pid(), pid()}.
start_io_capture() ->
    start_io_capture(undefined).

%% @doc Start capturing IO output with optional streaming subscriber (BT-696).
%% When Subscriber is a pid, each IO chunk is forwarded as {eval_out, Chunk}.
-spec start_io_capture(pid() | undefined) -> {pid(), pid()}.
start_io_capture(Subscriber) ->
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> io_capture_loop(<<>>, Subscriber) end),
    group_leader(CapturePid, self()),
    {CapturePid, OldGL}.

%% @doc Stop capturing IO and return all captured output as a binary.
%% Restores the original group_leader. Returns <<>> on timeout or if
%% the capture process has already exited.
%%
%% BT-358: After restoring the eval process's group_leader, also resets
%% the group_leader of any processes that inherited the capture process
%% as their group_leader during eval (e.g., spawned actors).
-spec stop_io_capture({pid(), pid()}) -> binary().
stop_io_capture({CapturePid, OldGL}) ->
    group_leader(OldGL, self()),
    %% BT-358: Reset group_leader for any processes spawned during eval
    %% that inherited the capture process as their group_leader.
    reset_captured_group_leaders(CapturePid, OldGL),
    case is_process_alive(CapturePid) of
        true ->
            CapturePid ! {get_captured, self(), OldGL},
            receive
                {captured_output, Output} -> Output
            after ?IO_CAPTURE_TIMEOUT ->
                ?LOG_WARNING("IO capture output retrieval timed out", #{}),
                <<>>
            end;
        false ->
            <<>>
    end.

%% @doc IO server loop that captures put_chars output.
%% Handles both {put_chars, Enc, Chars} and {put_chars, Enc, Mod, Func, Args}
%% (the latter is used by io:format).
%% BT-696: When Subscriber is a pid, forwards each chunk as {eval_out, Chunk}.
%% BT-698: When Subscriber is a pid, handles get_line/get_chars/get_until by
%% sending {need_input, CapturePid, Prompt} to Subscriber and waiting for
%% {stdin_input, Data} response.
%% After capture stops, proxies IO to the original group_leader so that
%% processes spawned during eval still have a working IO path.
-spec io_capture_loop(binary(), pid() | undefined) -> ok.
io_capture_loop(Buffer, Subscriber) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case is_stdin_request(Request) of
                {true, Prompt} ->
                    %% BT-698: Handle stdin request
                    Reply = handle_stdin_request(Subscriber, Prompt),
                    From ! {io_reply, ReplyAs, Reply},
                    io_capture_loop(Buffer, Subscriber);
                false ->
                    {Reply, NewBuffer} = handle_io_request(Request, Buffer),
                    From ! {io_reply, ReplyAs, Reply},
                    %% BT-696: Forward new chunk to subscriber if present
                    case is_pid(Subscriber) andalso byte_size(NewBuffer) > byte_size(Buffer) of
                        true ->
                            Chunk = binary:part(NewBuffer, byte_size(Buffer),
                                                byte_size(NewBuffer) - byte_size(Buffer)),
                            Subscriber ! {eval_out, Chunk};
                        false ->
                            ok
                    end,
                    io_capture_loop(NewBuffer, Subscriber)
            end;
        {get_captured, Pid, OldGL} ->
            Pid ! {captured_output, Buffer},
            io_passthrough_loop(OldGL)
    end.

%% @doc After capture, forward all IO to the original group_leader.
%% Keeps this process alive so spawned children don't get a dead group_leader.
%% Exits after 60 seconds of inactivity to avoid leaking processes.
-spec io_passthrough_loop(pid()) -> ok.
io_passthrough_loop(OldGL) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            OldGL ! {io_request, From, ReplyAs, Request},
            io_passthrough_loop(OldGL)
    after 60000 ->
        ok
    end.

%% @doc Reset group_leader for processes that inherited the capture process.
%% Scans all processes to find those whose group_leader is the capture
%% process and resets them to the original group_leader (BT-358).
-spec reset_captured_group_leaders(pid(), pid()) -> ok.
reset_captured_group_leaders(CapturePid, OldGL) ->
    lists:foreach(
        fun(Pid) ->
            case Pid =/= self() andalso is_process_alive(Pid) of
                true ->
                    case erlang:process_info(Pid, group_leader) of
                        {group_leader, CapturePid} ->
                            group_leader(OldGL, Pid);
                        {group_leader, _} ->
                            ok;  % Different GL, leave it alone
                        undefined ->
                            ok   % Process died between checks, nothing to do
                    end;
                false ->
                    ok
            end
        end,
        erlang:processes()
    ).

%% @doc Check if an IO request is a stdin (input) request.
%% Returns {true, Prompt} for get_line/get_chars/get_until, false otherwise.
%% BT-698: Supports the Erlang IO protocol input requests.
-spec is_stdin_request(term()) -> {true, binary()} | false.
is_stdin_request({get_line, _Encoding, Prompt}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_line, Prompt}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_chars, _Encoding, Prompt, _Count}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_chars, Prompt, _Count}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_until, _Encoding, Prompt, _Mod, _Func, _Args}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_until, Prompt, _Mod, _Func, _Args}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request(_) ->
    false.

%% @doc Convert an IO prompt to a binary string.
-spec prompt_to_binary(term()) -> binary().
prompt_to_binary(Prompt) when is_binary(Prompt) -> Prompt;
prompt_to_binary(Prompt) when is_list(Prompt) ->
    try unicode:characters_to_binary(Prompt, utf8) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<"? ">>
    catch _:_ -> <<"? ">>
    end;
prompt_to_binary(Prompt) when is_atom(Prompt) ->
    atom_to_binary(Prompt, utf8);
prompt_to_binary(_) -> <<"? ">>.

%% @doc Handle a stdin IO request by notifying the subscriber and waiting for input.
%% When no subscriber is present (sync eval), returns {error, enotsup}.
%% BT-698: Sends {need_input, CapturePid, Ref, Prompt} to subscriber, waits for
%% {stdin_input, Ref, Data} response with timeout. The Ref prevents late replies
%% from a timed-out prompt being consumed by a subsequent prompt.
-spec handle_stdin_request(pid() | undefined, binary()) -> term().
handle_stdin_request(undefined, _Prompt) ->
    {error, enotsup};
handle_stdin_request(Subscriber, Prompt) when is_pid(Subscriber) ->
    Ref = make_ref(),
    Subscriber ! {need_input, self(), Ref, Prompt},
    receive
        {stdin_input, Ref, Data} when is_binary(Data) ->
            Data;
        {stdin_input, Ref, eof} ->
            eof
    after ?STDIN_TIMEOUT ->
        ?LOG_WARNING("Stdin input timed out after ~pms", [?STDIN_TIMEOUT]),
        {error, timeout}
    end.

%% @doc Handle a single IO protocol request.
%% Always replies ok for output requests to avoid changing program behavior.
-spec handle_io_request(term(), binary()) -> {term(), binary()}.
handle_io_request({put_chars, Encoding, Chars}, Buffer) ->
    try unicode:characters_to_binary(Chars, Encoding, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request({put_chars, Chars}, Buffer) ->
    %% Legacy IO protocol form without encoding
    try unicode:characters_to_binary(Chars, latin1, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request({put_chars, Encoding, Mod, Func, Args}, Buffer) ->
    try unicode:characters_to_binary(apply(Mod, Func, Args), Encoding, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request(_Other, Buffer) ->
    {{error, enotsup}, Buffer}.

%% @doc Inject captured output and warnings into an eval result tuple.
-spec inject_output(tuple(), binary(), [binary()]) -> tuple().
inject_output({ok, Result, State}, Output, Warnings) ->
    {ok, Result, Output, Warnings, State};
inject_output({error, Reason, State}, Output, Warnings) ->
    {error, Reason, Output, Warnings, State}.
