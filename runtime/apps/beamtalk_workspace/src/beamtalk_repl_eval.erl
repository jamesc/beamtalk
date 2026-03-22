%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Expression evaluation orchestration for the Beamtalk REPL
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Orchestrates compilation, module loading, execution, and result processing
%%% for REPL expressions. Delegates compilation to beamtalk_repl_compiler and
%%% module loading to beamtalk_repl_loader (BT-863).

-module(beamtalk_repl_eval).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    do_eval/2, do_eval/3,
    do_eval_trace/2,
    do_show_codegen/2,
    handle_load/2, handle_load/3,
    handle_load_source/3
]).
%% BT-845: ADR 0040 Phase 2 — stateless class reload (called via erlang:apply from beamtalk_runtime)
-export([reload_class_file/1, reload_class_file/2]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([
    extract_assignment/1,
    maybe_await_future/1,
    rebuild_bindings_from_steps/2,
    should_purge_module/2,
    strip_internal_bindings/1,
    inject_output/3,
    handle_class_definition/7,
    handle_method_definition/4,
    handle_protocol_definition/3,
    wrap_load_err/3
]).
-endif.

-define(INTERNAL_REGISTRY_KEY, '__repl_actor_registry__').
-define(WORKSPACE_BINDINGS_KEY, '__workspace_user_bindings__').

%%% Public API

%% @doc Evaluate a Beamtalk expression.
-spec do_eval(string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State) ->
    do_eval(Expression, State, undefined).

%% @doc Evaluate with optional streaming subscriber (BT-696).
-spec do_eval(string(), beamtalk_repl_state:state(), pid() | undefined) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval(Expression, State, Subscriber) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),

    SessionBindings = beamtalk_repl_state:get_bindings(State),
    %% BT-881: Merge workspace user bindings into session bindings.
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    WorkspaceOnlyBindings = maps:without(maps:keys(SessionBindings), WorkspaceUserBindings),
    Bindings0 = maps:merge(WorkspaceUserBindings, SessionBindings),
    Bindings = maps:put(?WORKSPACE_BINDINGS_KEY, WorkspaceOnlyBindings, Bindings0),

    RegistryPid = beamtalk_repl_state:get_actor_registry(State),

    case beamtalk_repl_compiler:compile_expression(Expression, ModuleName, Bindings) of
        %% BT-571: Inline class definition
        {ok, class_definition, ClassInfo, Warnings} ->
            handle_class_definition(
                ClassInfo, Warnings, Expression, Bindings, NewState, RegistryPid, Subscriber
            );
        %% BT-571: Standalone method definition
        {ok, method_definition, MethodInfo, Warnings} ->
            handle_method_definition(MethodInfo, Warnings, Expression, NewState);
        %% BT-1612: Protocol definition
        {ok, protocol_definition, ProtocolInfo, Warnings} ->
            handle_protocol_definition(ProtocolInfo, Warnings, NewState);
        {ok, Binary, _ResultExpr, Warnings} ->
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
                    wrap_load_err(Reason, Warnings, NewState)
            end;
        {error, Reason} ->
            wrap_compile_err(Reason, NewState)
    end.

%% @doc Evaluate a Beamtalk expression in trace mode (BT-1238).
%%
%% Returns `{ok, Steps, Output, Warnings, State}' where
%% `Steps = [{SourceBin, Value}]' — one entry per top-level statement —
%% or `{error, Reason, Output, Warnings, State}' on failure.
-spec do_eval_trace(string(), beamtalk_repl_state:state()) ->
    {ok, [{binary(), term()}], binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
do_eval_trace(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),

    SessionBindings = beamtalk_repl_state:get_bindings(State),
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    WorkspaceOnlyBindings = maps:without(maps:keys(SessionBindings), WorkspaceUserBindings),
    Bindings0 = maps:merge(WorkspaceUserBindings, SessionBindings),
    Bindings = maps:put(?WORKSPACE_BINDINGS_KEY, WorkspaceOnlyBindings, Bindings0),

    RegistryPid = beamtalk_repl_state:get_actor_registry(State),

    case beamtalk_repl_compiler:compile_expression_trace(Expression, ModuleName, Bindings) of
        {ok, Binary, _ResultExpr, Warnings} ->
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    BindingsWithRegistry =
                        case RegistryPid of
                            undefined -> Bindings;
                            _ -> maps:put(?INTERNAL_REGISTRY_KEY, RegistryPid, Bindings)
                        end,
                    CaptureRef = beamtalk_io_capture:start(undefined),
                    EvalResult =
                        try
                            {RawSteps, UpdatedBindings} = apply(ModuleName, eval, [
                                BindingsWithRegistry
                            ]),
                            CleanBindings = strip_internal_bindings(UpdatedBindings),
                            %% BT-1238: Await each step's value. If any future rejects, propagate
                            %% as a top-level error (consistent with the non-trace eval path).
                            AwaitedSteps = [
                                {Src, maybe_await_future(Val)}
                             || {Src, Val} <- RawSteps
                            ],
                            case
                                lists:search(
                                    fun
                                        ({_Src, {future_rejected, _}}) -> true;
                                        (_) -> false
                                    end,
                                    AwaitedSteps
                                )
                            of
                                {value, {_Src, {future_rejected, FutureReason}}} ->
                                    %% Mirror process_eval_result/4: wrap and store in '_error'.
                                    FutExObj = beamtalk_exception_handler:ensure_wrapped(
                                        FutureReason
                                    ),
                                    FutBindings = maps:put('_error', FutExObj, CleanBindings),
                                    ErrState = beamtalk_repl_state:set_bindings(
                                        FutBindings, NewState
                                    ),
                                    {error, FutExObj, ErrState};
                                false ->
                                    %% BT-1261: Rebuild bindings from awaited step values so that
                                    %% variable assignments whose RHS was a future are stored with
                                    %% the resolved value rather than the raw future handle.
                                    FinalBindings = rebuild_bindings_from_steps(
                                        AwaitedSteps, CleanBindings
                                    ),
                                    FinalState = beamtalk_repl_state:set_bindings(
                                        FinalBindings, NewState
                                    ),
                                    {ok, AwaitedSteps, FinalState}
                            end
                        catch
                            Class:Reason:Stacktrace ->
                                CaughtExObj = beamtalk_exception_handler:ensure_wrapped(
                                    Class, Reason, Stacktrace
                                ),
                                CaughtBindings = maps:put('_error', CaughtExObj, Bindings),
                                CaughtState = beamtalk_repl_state:set_bindings(
                                    CaughtBindings, NewState
                                ),
                                {error, {eval_error, Class, CaughtExObj}, CaughtState}
                        after
                            cleanup_module(ModuleName, RegistryPid)
                        end,
                    Output = beamtalk_io_capture:stop(CaptureRef),
                    case EvalResult of
                        {ok, Steps2, FinalState2} ->
                            {ok, Steps2, Output, Warnings, FinalState2};
                        {error, ErrorReason, ErrorState} ->
                            WrappedReason = beamtalk_repl_errors:ensure_structured_error(
                                ErrorReason
                            ),
                            {error, WrappedReason, Output, Warnings, ErrorState}
                    end;
                {error, Reason} ->
                    wrap_load_err(Reason, Warnings, NewState)
            end;
        {error, Reason} ->
            wrap_compile_err(Reason, NewState)
    end.

%% @doc Compile a Beamtalk expression and return Core Erlang source (BT-700).
-spec do_show_codegen(string(), beamtalk_repl_state:state()) ->
    {ok, binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), [binary()], beamtalk_repl_state:state()}.
do_show_codegen(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    SessionBindings = beamtalk_repl_state:get_bindings(State),
    WorkspaceUserBindings = beamtalk_workspace_interface_primitives:get_user_bindings(),
    Bindings = maps:merge(WorkspaceUserBindings, SessionBindings),
    SourceBin = list_to_binary(Expression),
    ModNameBin = iolist_to_binary(["beamtalk_repl_codegen_", integer_to_list(Counter)]),
    KnownVars = [
        atom_to_binary(K, utf8)
     || K <- maps:keys(Bindings),
        is_atom(K),
        not beamtalk_repl_compiler:is_internal_key(K)
    ],
    case beamtalk_repl_compiler:compile_for_codegen(SourceBin, ModNameBin, KnownVars) of
        {ok, CoreErlang, Warnings} ->
            {ok, CoreErlang, Warnings, NewState};
        {error, Reason} ->
            {error, Reason, [], NewState}
    end.

%% @doc Load a Beamtalk file and register its classes.
-spec handle_load(string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    beamtalk_repl_loader:handle_load(Path, State).

%% @doc Load a Beamtalk file with pre-built class indexes (BT-1543).
-spec handle_load(string(), beamtalk_repl_state:state(), map()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State, PrebuiltIndexes) ->
    beamtalk_repl_loader:handle_load(Path, State, PrebuiltIndexes).

%% @doc Load Beamtalk source from an inline binary string (no file path).
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    beamtalk_repl_loader:handle_load_source(SourceBin, Label, State).

%% @doc Compile and load a source file without REPL session state (BT-845).
-spec reload_class_file(string()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path) ->
    beamtalk_repl_loader:reload_class_file(Path).

-spec reload_class_file(string(), atom()) -> {ok, [map()]} | {error, term()}.
reload_class_file(Path, ExpectedClassName) ->
    beamtalk_repl_loader:reload_class_file(Path, ExpectedClassName).

%%% Internal functions

%% @doc Handle inline class definition: load class module, eval trailing expressions.
-spec handle_class_definition(
    map(),
    [binary()],
    string(),
    map(),
    beamtalk_repl_state:state(),
    pid() | undefined,
    pid() | undefined
) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_class_definition(
    ClassInfo, Warnings, Expression, MergedBindings, State, RegistryPid, Subscriber
) ->
    case beamtalk_repl_loader:load_class_module(ClassInfo, Expression, State) of
        {ok, ClassName, no_trailing, NewState2} ->
            {ok, ClassName, <<>>, Warnings, NewState2};
        {ok, _ClassName, {trailing, TrailingModName, TrailingBinary}, NewState2} ->
            %% BT-885: eval trailing expressions after class load
            case code:load_binary(TrailingModName, "", TrailingBinary) of
                {module, TrailingModName} ->
                    eval_loaded_module(
                        TrailingModName,
                        Expression,
                        MergedBindings,
                        RegistryPid,
                        Subscriber,
                        Warnings,
                        NewState2
                    );
                {error, LoadReason} ->
                    wrap_load_err(LoadReason, Warnings, NewState2)
            end;
        {error, Reason, NewState2} ->
            Err = beamtalk_repl_errors:ensure_structured_error(Reason),
            {error, Err, <<>>, Warnings, NewState2}
    end.

%% @doc Handle standalone method definition: recompile and reload target class.
-spec handle_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_method_definition(MethodInfo, Warnings, Expression, State) ->
    beamtalk_repl_loader:reload_method_definition(MethodInfo, Warnings, Expression, State).

%% @doc Handle protocol definition: load module and register protocol (BT-1612).
-spec handle_protocol_definition(map(), [binary()], beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_protocol_definition(ProtocolInfo, Warnings, State) ->
    #{binary := Binary, module_name := ModuleName, protocols := Protocols} = ProtocolInfo,
    case code:load_binary(ModuleName, "", Binary) of
        {module, ModuleName} ->
            %% Call register_class/0 to register the protocol with the runtime
            case maybe_register_protocol_class(ModuleName) of
                ok ->
                    NewState = beamtalk_repl_state:add_loaded_module(ModuleName, State),
                    %% Build display string: "Protocol Foo defined" or "Protocols Foo, Bar defined"
                    ProtocolNames = [binary_to_list(P) || P <- Protocols],
                    DisplayStr =
                        case ProtocolNames of
                            [Single] ->
                                list_to_binary("Protocol " ++ Single ++ " defined");
                            Multiple ->
                                list_to_binary(
                                    "Protocols " ++ string:join(Multiple, ", ") ++ " defined"
                                )
                        end,
                    {ok, DisplayStr, <<>>, Warnings, NewState};
                {error, RegError} ->
                    {error, RegError, <<>>, Warnings, State}
            end;
        {error, Reason} ->
            wrap_load_err(Reason, Warnings, State)
    end.

%% @private Attempt to call register_class/0 on a protocol module.
%% Returns ok if successful or not exported, or {error, #beamtalk_error{}} on failure.
-spec maybe_register_protocol_class(atom()) -> ok | {error, #beamtalk_error{}}.
maybe_register_protocol_class(ModuleName) ->
    case erlang:function_exported(ModuleName, register_class, 0) of
        true ->
            try
                ModuleName:register_class(),
                ok
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR(
                        "Protocol register_class/0 failed for ~p: ~p:~p~n~p",
                        [ModuleName, Class, Reason, Stacktrace],
                        #{domain => [beamtalk, runtime]}
                    ),
                    Err = beamtalk_repl_errors:ensure_structured_error(
                        {registration_error, {ModuleName, Reason}}
                    ),
                    {error, Err}
            end;
        false ->
            ok
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
eval_loaded_module(ModuleName, Expression, Bindings, RegistryPid, Subscriber, Warnings, State) ->
    CaptureRef = beamtalk_io_capture:start(Subscriber),
    EvalResult =
        try
            execute_and_process(ModuleName, Expression, Bindings, RegistryPid, State)
        catch
            Class:Reason:Stacktrace ->
                CaughtExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stacktrace),
                CaughtBindings = maps:put('_error', CaughtExObj, Bindings),
                CaughtState = beamtalk_repl_state:set_bindings(CaughtBindings, State),
                {error, {eval_error, Class, CaughtExObj}, CaughtState}
        after
            cleanup_module(ModuleName, RegistryPid)
        end,
    Output = beamtalk_io_capture:stop(CaptureRef),
    inject_output(EvalResult, Output, Warnings).

%% @doc Execute module and process the result (assignment/future handling).
-spec execute_and_process(atom(), string(), map(), pid() | undefined, beamtalk_repl_state:state()) ->
    {ok, term(), beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
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
-spec process_eval_result(term(), string(), map(), beamtalk_repl_state:state()) ->
    {ok, term(), beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
process_eval_result({future_rejected, ErrorReason}, _Expression, CleanBindings, State) ->
    FutExObj = beamtalk_exception_handler:ensure_wrapped(ErrorReason),
    FutBindings = maps:put('_error', FutExObj, CleanBindings),
    FinalState = beamtalk_repl_state:set_bindings(FutBindings, State),
    {error, FutExObj, FinalState};
process_eval_result(Result, Expression, CleanBindings, State) ->
    case extract_assignment(Expression) of
        {ok, VarName} ->
            NewBindings = maps:put(VarName, Result, CleanBindings),
            {ok, Result, beamtalk_repl_state:set_bindings(NewBindings, State)};
        none ->
            {ok, Result, beamtalk_repl_state:set_bindings(CleanBindings, State)}
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

%% @doc Rebuild bindings by extracting variable assignments from awaited trace steps (BT-1261).
%%
%% For each step whose source text is a simple assignment (`VarName := Expr`), the
%% binding is updated with the awaited (resolved) value from the step.  This ensures
%% that if the RHS was a future the session binding holds the final value, not the
%% raw `{beamtalk_future, Pid}` handle stored by the evaluator before awaiting.
-spec rebuild_bindings_from_steps([{binary(), term()}], map()) -> map().
rebuild_bindings_from_steps(Steps, Bindings) ->
    lists:foldl(
        fun({Src, AwaitedVal}, Acc) ->
            SrcStr = binary_to_list(Src),
            case extract_assignment(SrcStr) of
                {ok, VarName} -> maps:put(VarName, AwaitedVal, Acc);
                none -> Acc
            end
        end,
        Bindings,
        Steps
    ).

%% @doc Extract variable name from assignment expression.
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    case re:run(Expression, "\\.\\s+\\S", []) of
        {match, _} ->
            none;
        nomatch ->
            case
                re:run(
                    Expression,
                    "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=",
                    [{capture, [1], list}]
                )
            of
                {match, [VarName]} -> {ok, list_to_atom(VarName)};
                nomatch -> none
            end
    end.

%% @doc Auto-await a Future if the result is a tagged future tuple (BT-840).
-spec maybe_await_future(term()) -> term().
maybe_await_future({beamtalk_future, _} = Future) ->
    try beamtalk_runtime_api:future_await(Future, 30000) of
        AwaitedValue -> AwaitedValue
    catch
        throw:#beamtalk_error{kind = timeout} = TimeoutError ->
            {future_rejected, TimeoutError};
        throw:{future_rejected, #beamtalk_error{} = Reason} ->
            {future_rejected, Reason};
        throw:{future_rejected, Reason} ->
            {future_rejected, Reason}
    end;
maybe_await_future(Value) ->
    Value.

%% @doc Check if a module should be purged (no living actors reference it).
-spec should_purge_module(atom(), pid() | undefined) -> boolean().
should_purge_module(_ModuleName, undefined) ->
    true;
should_purge_module(ModuleName, RegistryPid) ->
    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
    not lists:any(fun(#{module := ActorModule}) -> ActorModule =:= ModuleName end, Actors).

%% @doc Strip internal plumbing keys from bindings map (BT-153).
-spec strip_internal_bindings(map()) -> map().
strip_internal_bindings(Bindings) ->
    %% BT-881: Strip workspace-only binding keys injected by do_eval.
    Stripped0 =
        case maps:find(?WORKSPACE_BINDINGS_KEY, Bindings) of
            {ok, WorkspaceOnlyBindings} when is_map(WorkspaceOnlyBindings) ->
                WithoutMeta = maps:remove(?WORKSPACE_BINDINGS_KEY, Bindings),
                maps:fold(
                    fun(Key, OriginalValue, Acc) ->
                        case maps:find(Key, Acc) of
                            {ok, OriginalValue} -> maps:remove(Key, Acc);
                            _ -> Acc
                        end
                    end,
                    WithoutMeta,
                    WorkspaceOnlyBindings
                );
            _ ->
                Bindings
        end,
    maps:remove(?INTERNAL_REGISTRY_KEY, Stripped0).

%% @doc Inject captured output and warnings into an eval result tuple.
-spec inject_output(tuple(), binary(), [binary()]) -> tuple().
inject_output({ok, Result, State}, Output, Warnings) ->
    {ok, Result, Output, Warnings, State};
inject_output({error, Reason, State}, Output, Warnings) ->
    {error, Reason, Output, Warnings, State}.

%% @private Wrap a compile error as a structured #beamtalk_error{} result tuple.
-spec wrap_compile_err(term(), beamtalk_repl_state:state()) ->
    {error, #beamtalk_error{}, binary(), [binary()], beamtalk_repl_state:state()}.
wrap_compile_err(Reason, State) ->
    Err = beamtalk_repl_errors:ensure_structured_error({compile_error, Reason}),
    {error, Err, <<>>, [], State}.

%% @private Wrap a load error as a structured #beamtalk_error{} result tuple.
-spec wrap_load_err(term(), [binary()], beamtalk_repl_state:state()) ->
    {error, #beamtalk_error{}, binary(), [binary()], beamtalk_repl_state:state()}.
wrap_load_err(Reason, Warnings, State) ->
    Err = beamtalk_repl_errors:ensure_structured_error({load_error, Reason}),
    {error, Err, <<>>, Warnings, State}.
