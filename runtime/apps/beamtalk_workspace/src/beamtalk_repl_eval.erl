%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Expression evaluation orchestration for the Beamtalk REPL
%%%
%%% **DDD Context:** REPL
%%%
%%% Orchestrates compilation, module loading, execution, and result processing
%%% for REPL expressions. Delegates compilation to beamtalk_repl_compiler and
%%% module loading to beamtalk_repl_loader (BT-863).

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
    maybe_await_future/1,
    should_purge_module/2,
    strip_internal_bindings/1,
    inject_output/3,
    handle_class_definition/7,
    handle_method_definition/4
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
    WorkspaceUserBindings = beamtalk_workspace_interface:get_user_bindings(),
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
                    {error, {load_error, Reason}, <<>>, [], NewState}
            end;
        {error, Reason} ->
            {error, {compile_error, Reason}, <<>>, [], NewState}
    end.

%% @doc Compile a Beamtalk expression and return Core Erlang source (BT-700).
-spec do_show_codegen(string(), beamtalk_repl_state:state()) ->
    {ok, binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), [binary()], beamtalk_repl_state:state()}.
do_show_codegen(Expression, State) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    SessionBindings = beamtalk_repl_state:get_bindings(State),
    WorkspaceUserBindings = beamtalk_workspace_interface:get_user_bindings(),
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

%% @doc Load Beamtalk source from an inline binary string (no file path).
-spec handle_load_source(binary(), string(), beamtalk_repl_state:state()) ->
    {ok, [map()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load_source(SourceBin, Label, State) ->
    beamtalk_repl_loader:handle_load_source(SourceBin, Label, State).

%% @doc Compile and load a source file without REPL session state (BT-845).
-spec reload_class_file(string()) -> ok | {error, term()}.
reload_class_file(Path) ->
    beamtalk_repl_loader:reload_class_file(Path).

-spec reload_class_file(string(), atom()) -> ok | {error, term()}.
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
                    {error, {load_error, LoadReason}, <<>>, Warnings, NewState2}
            end;
        {error, Reason, NewState2} ->
            {error, Reason, <<>>, Warnings, NewState2}
    end.

%% @doc Handle standalone method definition: recompile and reload target class.
-spec handle_method_definition(map(), [binary()], string(), beamtalk_repl_state:state()) ->
    {ok, term(), binary(), [binary()], beamtalk_repl_state:state()}
    | {error, term(), binary(), [binary()], beamtalk_repl_state:state()}.
handle_method_definition(MethodInfo, Warnings, Expression, State) ->
    beamtalk_repl_loader:reload_method_definition(MethodInfo, Warnings, Expression, State).

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
            Class:Reason:_Stacktrace ->
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
    try beamtalk_future:await(Future, 30000) of
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
