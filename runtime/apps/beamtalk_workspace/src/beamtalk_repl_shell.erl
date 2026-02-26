%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL session shell for workspace
%%%
%%% **DDD Context:** REPL
%%%
%%% Maintains per-session state including:
%%% - Variable bindings (counter := Counter spawn)
%%% - Session ID
%%% - Evaluation state and counter
%%%
%%% Each REPL connection gets its own session process, allowing
%%% multiple users to work in the same workspace with independent
%%% bindings while sharing access to actors and loaded modules.

-module(beamtalk_repl_shell).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Public API
-export([
    start_link/1,
    stop/1,
    eval/2,
    eval_async/3,
    interrupt/1,
    get_bindings/1,
    clear_bindings/1,
    load_file/2,
    load_source/2,
    unload_module/2,
    get_module_tracker/1,
    show_codegen/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%% Public API

%% @doc Start a REPL shell session.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    gen_server:start_link(?MODULE, SessionId, []).

%% @doc Stop a REPL shell session.
-spec stop(pid()) -> ok.
stop(SessionPid) ->
    gen_server:stop(SessionPid, normal, 5000).

%% @doc Evaluate an expression in this session.
-spec eval(pid(), string()) ->
    {ok, term(), binary(), [binary()]} | {error, term(), binary(), [binary()]}.
eval(SessionPid, Expression) ->
    gen_server:call(SessionPid, {eval, Expression}, 30000).

%% @doc Evaluate an expression with streaming subscriber (BT-696).
%% Subscriber receives {eval_out, Chunk} messages during eval.
-spec eval_async(pid(), string(), pid()) -> ok.
eval_async(SessionPid, Expression, Subscriber) ->
    gen_server:cast(SessionPid, {eval_async, Expression, Subscriber}).

%% @doc Compile expression and return Core Erlang source (BT-700).
%% Does NOT evaluate the code.
-spec show_codegen(pid(), string()) -> {ok, binary(), [binary()]} | {error, term(), [binary()]}.
show_codegen(SessionPid, Expression) ->
    gen_server:call(SessionPid, {show_codegen, Expression}, 30000).

%% @doc Interrupt a running evaluation in this session.
%% If no evaluation is in progress, returns ok immediately.
-spec interrupt(pid()) -> ok.
interrupt(SessionPid) ->
    gen_server:call(SessionPid, interrupt, 5000).

%% @doc Get current variable bindings for this session.
-spec get_bindings(pid()) -> {ok, map()}.
get_bindings(SessionPid) ->
    gen_server:call(SessionPid, get_bindings).

%% @doc Clear all variable bindings for this session.
-spec clear_bindings(pid()) -> ok.
clear_bindings(SessionPid) ->
    gen_server:call(SessionPid, clear_bindings).

%% @doc Load a Beamtalk source file in this session.
-spec load_file(pid(), string()) -> {ok, [map()]} | {error, term()}.
load_file(SessionPid, Path) ->
    gen_server:call(SessionPid, {load_file, Path}, 30000).

%% @doc Load Beamtalk source from an inline binary string.
-spec load_source(pid(), binary()) -> {ok, [map()]} | {error, term()}.
load_source(SessionPid, Source) ->
    gen_server:call(SessionPid, {load_source, Source}, 30000).

%% @doc Get the module tracker for this session (user-loaded modules only).
-spec get_module_tracker(pid()) -> {ok, beamtalk_repl_modules:module_tracker()}.
get_module_tracker(SessionPid) ->
    gen_server:call(SessionPid, get_module_tracker, 5000).

%% @doc Unload a module from this session, purging its code and removing it from the tracker.
-spec unload_module(pid(), atom()) -> ok | {error, #beamtalk_error{}}.
unload_module(SessionPid, Module) ->
    gen_server:call(SessionPid, {unload_module, Module}, 5000).

%%% gen_server callbacks

%% @private
init(SessionId) ->
    %% Create session-specific REPL state
    %% We use undefined for listen_socket and port since session doesn't own TCP connection
    State0 = beamtalk_repl_state:new(undefined, 0),

    %% BT-883: Inject non-class workspace globals (singletons + bind:as: names)
    %% as session bindings so they resolve from the bindings map.
    Bindings0 = inject_workspace_bindings(#{}),
    State0b = beamtalk_repl_state:set_bindings(Bindings0, State0),

    %% Get workspace-wide actor registry
    %% The registry is registered globally in the workspace
    RegistryPid =
        case whereis(beamtalk_actor_registry) of
            undefined ->
                ?LOG_WARNING("Actor registry not found for session ~p", [SessionId]),
                undefined;
            Pid ->
                Pid
        end,
    State1 = beamtalk_repl_state:set_actor_registry(RegistryPid, State0b),

    {ok, {SessionId, State1, undefined}}.

%% @private
handle_call({eval, Expression}, From, {SessionId, State, undefined}) ->
    %% Spawn eval in a monitored worker process so it can be interrupted (BT-666)
    Self = self(),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        Result = beamtalk_repl_eval:do_eval(Expression, State),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, From}}};
handle_call({eval, _Expression}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState) ->
    %% Already evaluating — reject concurrent eval
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    {reply, {error, Err2, <<>>, []}, FullState};
handle_call(interrupt, _From, {SessionId, State, {WorkerPid, MonRef, EvalFrom}}) ->
    %% Kill the worker process and reply to the waiting eval caller
    erlang:demonitor(MonRef, [flush]),
    exit(WorkerPid, kill),
    %% Flush any eval_result message the worker may have sent before dying
    receive
        {eval_result, WorkerPid, _} -> ok
    after 0 -> ok
    end,
    Err0 = beamtalk_error:new(interrupted, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"Interrupted">>),
    reply_eval(EvalFrom, {eval_error, Err1, <<>>, []}),
    {reply, ok, {SessionId, State, undefined}};
handle_call(interrupt, _From, {_SessionId, _State, undefined} = FullState) ->
    %% No eval in progress — nothing to interrupt
    {reply, ok, FullState};
handle_call(get_bindings, _From, {SessionId, State, Worker}) ->
    Bindings = beamtalk_repl_state:get_bindings(State),
    {reply, {ok, Bindings}, {SessionId, State, Worker}};
handle_call(clear_bindings, _From, {SessionId, State, Worker}) ->
    %% BT-883: Re-inject workspace globals after clearing bindings
    %% so that singletons and bind:as: names remain available.
    Bindings = inject_workspace_bindings(#{}),
    NewState = beamtalk_repl_state:set_bindings(Bindings, State),
    {reply, ok, {SessionId, NewState, Worker}};
handle_call({load_file, Path}, _From, {SessionId, State, Worker}) ->
    case beamtalk_repl_eval:handle_load(Path, State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState, Worker}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState, Worker}}
    end;
handle_call({load_source, Source}, _From, {SessionId, State, Worker}) ->
    case beamtalk_repl_eval:handle_load_source(Source, "<editor>", State) of
        {ok, LoadedModules, NewState} ->
            {reply, {ok, LoadedModules}, {SessionId, NewState, Worker}};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, {SessionId, NewState, Worker}}
    end;
handle_call(get_module_tracker, _From, {SessionId, State, Worker}) ->
    Tracker = beamtalk_repl_state:get_module_tracker(State),
    {reply, {ok, Tracker}, {SessionId, State, Worker}};
handle_call({unload_module, Module}, _From, {SessionId, State, Worker}) ->
    case code:is_loaded(Module) of
        {file, _} ->
            case code:soft_purge(Module) of
                true ->
                    _ = code:delete(Module),
                    Tracker = beamtalk_repl_state:get_module_tracker(State),
                    NewTracker = beamtalk_repl_modules:remove_module(Module, Tracker),
                    NewState = beamtalk_repl_state:set_module_tracker(NewTracker, State),
                    {reply, ok, {SessionId, NewState, Worker}};
                false ->
                    Err0 = beamtalk_error:new(module_in_use, 'Module'),
                    Err1 = beamtalk_error:with_selector(Err0, Module),
                    Err = beamtalk_error:with_hint(
                        Err1,
                        <<"Stop actors using this module first.">>
                    ),
                    {reply, {error, Err}, {SessionId, State, Worker}}
            end;
        false ->
            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
            Err1 = beamtalk_error:with_selector(Err0, Module),
            Err = beamtalk_error:with_hint(
                Err1,
                <<"Use :load <path> to load it first.">>
            ),
            {reply, {error, Err}, {SessionId, State, Worker}}
    end;
handle_call({show_codegen, _Expression}, _From, {_SessionId, _State, {_Pid, _Ref, _}} = FullState) ->
    %% Reject if eval is in progress
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Wait for the current evaluation to complete.">>),
    {reply, {error, Err2, []}, FullState};
handle_call({show_codegen, Expression}, _From, {SessionId, State, undefined}) ->
    case beamtalk_repl_eval:do_show_codegen(Expression, State) of
        {ok, CoreErlang, Warnings, NewState} ->
            {reply, {ok, CoreErlang, Warnings}, {SessionId, NewState, undefined}};
        {error, Reason, Warnings, NewState} ->
            {reply, {error, Reason, Warnings}, {SessionId, NewState, undefined}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
%% BT-696: Async eval with streaming subscriber
handle_cast({eval_async, Expression, Subscriber}, {SessionId, State, undefined}) ->
    Self = self(),
    {WorkerPid, MonRef} = spawn_monitor(fun() ->
        Result = beamtalk_repl_eval:do_eval(Expression, State, Subscriber),
        Self ! {eval_result, self(), Result}
    end),
    {noreply, {SessionId, State, {WorkerPid, MonRef, {async, Subscriber}}}};
handle_cast(
    {eval_async, _Expression, Subscriber}, {_SessionId, _State, {_Pid, _Ref, _}} = FullState
) ->
    %% Already evaluating — reject concurrent eval
    Err0 = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    Subscriber ! {eval_error, Err2, <<>>, []},
    {noreply, FullState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Worker completed eval successfully (BT-666)
handle_info({eval_result, WorkerPid, Result}, {SessionId, _State, {WorkerPid, MonRef, From}}) ->
    erlang:demonitor(MonRef, [flush]),
    case Result of
        {ok, Value, Output, Warnings, NewState} ->
            reply_eval(From, {eval_done, Value, Output, Warnings}),
            {noreply, {SessionId, NewState, undefined}};
        {error, Reason, Output, Warnings, NewState} ->
            reply_eval(From, {eval_error, Reason, Output, Warnings}),
            {noreply, {SessionId, NewState, undefined}}
    end;
%% Worker process crashed (BT-666)
handle_info(
    {'DOWN', MonRef, process, WorkerPid, Reason},
    {SessionId, State, {WorkerPid, MonRef, From}}
) ->
    Err0 = beamtalk_error:new(eval_crashed, 'REPL'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary(io_lib:format("Evaluation crashed: ~p", [Reason]))
    ),
    reply_eval(From, {eval_error, Err1, <<>>, []}),
    {noreply, {SessionId, State, undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, {SessionId, _State, {WorkerPid, MonRef, _From}}) ->
    %% BT-666: Kill any running worker to avoid zombie evaluations
    erlang:demonitor(MonRef, [flush]),
    exit(WorkerPid, kill),
    ?LOG_INFO("REPL session terminated", #{session => SessionId, reason => Reason}),
    ok;
terminate(Reason, {SessionId, _State, _Worker}) ->
    ?LOG_INFO("REPL session terminated", #{session => SessionId, reason => Reason}),
    ok;
terminate(Reason, _State) ->
    ?LOG_INFO("REPL session terminated", #{reason => Reason}),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% @private BT-883: Inject workspace globals (minus class objects) as session bindings.
%% Walks Workspace globals via beamtalk_workspace_interface:get_session_bindings/0
%% instead of the hardcoded singletons list. Returns singletons (Transcript,
%% Beamtalk, Workspace) plus any user-registered bind:as: names.
inject_workspace_bindings(Bindings) ->
    maps:merge(Bindings, beamtalk_workspace_interface:get_session_bindings()).

%% @private BT-696: Dispatch eval result to sync caller or async subscriber.
reply_eval({async, Subscriber}, Msg) ->
    Subscriber ! Msg,
    ok;
reply_eval(From, {eval_done, Value, Output, Warnings}) ->
    gen_server:reply(From, {ok, Value, Output, Warnings});
reply_eval(From, {eval_error, Reason, Output, Warnings}) ->
    gen_server:reply(From, {error, Reason, Output, Warnings}).
