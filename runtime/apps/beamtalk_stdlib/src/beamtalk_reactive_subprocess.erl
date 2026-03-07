%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc OTP gen_server for push-mode subprocess delivery (BT-1187).
%%%
%%% **DDD Context:** Actor System Context
%%%
%%% Each `beamtalk_reactive_subprocess` process owns one port to the
%%% `beamtalk_exec` Rust helper binary and manages exactly one child subprocess
%%% (ChildId = 0).
%%%
%%% Unlike `beamtalk_subprocess` (pull model), this gen_server casts each
%%% stdout/stderr line to a registered `notify` actor as data arrives.
%%% The gen_server never blocks waiting for callers; it has no buffer queues and
%%% no deferred-reply slots.
%%%
%%% == State Map ==
%%%
%%% ```erlang
%%% #{
%%%   port           => port(),
%%%   child_id       => 0,
%%%   notify         => #beamtalk_object{},   % SubprocessDelegate actor
%%%   self_ref       => #beamtalk_object{},   % this gen_server as a Beamtalk object
%%%   stdout_pending => binary(),             % partial line fragment
%%%   stderr_pending => binary(),             % partial line fragment
%%%   exit_code      => nil | non_neg_integer(),
%%%   port_closed    => boolean()
%%% }
%%% ```
%%%
%%% == SubprocessDelegate Protocol ==
%%%
%%% The `notify` actor must implement:
%%% * `subprocessLine:from:` — called for each stdout line
%%% * `subprocessStderrLine:from:` — called for each stderr line
%%% * `subprocessExit:from:` — called when the subprocess exits
%%%
%%% == Selectors ==
%%%
%%% * `{'writeLine:', [Data]}` — append newline and write to subprocess stdin
%%% * `{exitCode, []}` — returns nil while running, integer after exit
%%% * `{close, []}` — send kill command to helper, close port
%%%
%%% `readLine`, `lines`, `stderrLines`, `readStderrLine` are absent by design
%%% (DNU enforced structurally — no methods = no runtime guard needed).
%%%
%%% == References ==
%%%
%%% * BT-1187 — ReactiveSubprocess feature
%%% * ADR 0051 "Subprocess Execution" — push tier design

-module(beamtalk_reactive_subprocess).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Public API
-export([start_link/1, start/1]).
%% Class-side @primitive dispatch (called from compiled bt@stdlib@reactivesubprocess)
-export([dispatch/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type config() :: #{
    executable := binary(),
    args => [binary()],
    env => #{binary() => binary()},
    dir => binary(),
    notify := #beamtalk_object{}
}.

%%% ============================================================================
%%% Class-side @primitive dispatch
%%% ============================================================================

%% @doc Dispatch a class-side @primitive method call for ReactiveSubprocess.
%%
%% Called by the compiled `bt@stdlib@reactivesubprocess:dispatch/3` for
%% class-side `@primitive` annotations: `open:args:notify:` and
%% `open:args:env:dir:notify:`.
-spec dispatch(atom(), list(), term()) -> term().

dispatch('open:args:notify:', [Command, Args, Notify], _ClassSelf) when
    is_binary(Command), is_record(Notify, beamtalk_object)
->
    ArgsList = beamtalk_subprocess_port:bt_array_to_list(
        'ReactiveSubprocess', Args, 'open:args:notify:'
    ),
    beamtalk_subprocess_port:ensure_binary_args(
        'ReactiveSubprocess', ArgsList, 'open:args:notify:'
    ),
    start_subprocess(
        #{executable => Command, args => ArgsList, notify => Notify}, 'open:args:notify:'
    );
dispatch('open:args:notify:', [_Command, _Args, _Notify], _ClassSelf) ->
    Err = beamtalk_error:new(type_error, 'ReactiveSubprocess', 'open:args:notify:'),
    beamtalk_error:raise(Err);
dispatch('open:args:env:dir:notify:', [Command, Args, Env, Dir, Notify], _ClassSelf) when
    is_binary(Command), is_binary(Dir), is_map(Env), is_record(Notify, beamtalk_object)
->
    ArgsList = beamtalk_subprocess_port:bt_array_to_list(
        'ReactiveSubprocess', Args, 'open:args:env:dir:notify:'
    ),
    beamtalk_subprocess_port:ensure_binary_args(
        'ReactiveSubprocess', ArgsList, 'open:args:env:dir:notify:'
    ),
    EnvMap = maps:without(['$beamtalk_class'], Env),
    beamtalk_subprocess_port:ensure_binary_env(
        'ReactiveSubprocess', EnvMap, 'open:args:env:dir:notify:'
    ),
    start_subprocess(
        #{executable => Command, args => ArgsList, env => EnvMap, dir => Dir, notify => Notify},
        'open:args:env:dir:notify:'
    );
dispatch('open:args:env:dir:notify:', [_Command, _Args, _Env, _Dir, _Notify], _ClassSelf) ->
    Err = beamtalk_error:new(type_error, 'ReactiveSubprocess', 'open:args:env:dir:notify:'),
    beamtalk_error:raise(Err);
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'ReactiveSubprocess'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(Err1).

%% @private Start a supervised beamtalk_reactive_subprocess gen_server and return a beamtalk_object.
-spec start_subprocess(map(), atom()) -> #beamtalk_object{}.
start_subprocess(Config, Selector) ->
    case beamtalk_reactive_subprocess_sup:start_child(Config) of
        {ok, Pid} ->
            #beamtalk_object{
                class = 'ReactiveSubprocess',
                class_mod = 'bt@stdlib@reactive_subprocess',
                pid = Pid
            };
        {error, Reason} ->
            Err0 = beamtalk_error:new(runtime_error, 'ReactiveSubprocess', Selector),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(io_lib:format("Failed to start reactive subprocess: ~p", [Reason]))
            ),
            beamtalk_error:raise(Err1)
    end.

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start a linked reactive subprocess gen_server.
%%
%% Config must contain `executable` (binary path or name on PATH) and
%% `notify` (#beamtalk_object{}).
%% Optional keys: `args` ([binary()]), `env` (#{binary() => binary()}), `dir` (binary()).
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Start an unlinked reactive subprocess gen_server (useful in tests).
-spec start(config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start(?MODULE, Config, []).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

%% @doc Open the exec port, spawn the child subprocess, and store self-reference.
-spec init(config()) -> {ok, map()} | {stop, term()}.
init(Config) ->
    Executable = maps:get(executable, Config),
    Args = maps:get(args, Config, []),
    Options = maps:with([env, dir], Config),
    Notify = maps:get(notify, Config),
    Port = beamtalk_subprocess_port:open(),
    ChildId = 0,
    beamtalk_subprocess_port:spawn_child(Port, ChildId, Executable, Args, Options),
    ?LOG_INFO("ReactiveSubprocess spawned", #{executable => Executable, child_id => ChildId}),
    SelfRef = #beamtalk_object{
        class = 'ReactiveSubprocess',
        class_mod = 'bt@stdlib@reactivesubprocess',
        pid = self()
    },
    State = #{
        port => Port,
        child_id => ChildId,
        notify => Notify,
        self_ref => SelfRef,
        stdout_pending => <<>>,
        stderr_pending => <<>>,
        exit_code => nil,
        port_closed => false
    },
    {ok, State}.

%% @doc Dispatch sync calls.
-spec handle_call(term(), term(), map()) -> {reply, term(), map()}.
handle_call({'writeLine:', [Data]}, _From, State) ->
    handle_writeLine(Data, State);
handle_call({exitCode, []}, _From, State) ->
    {reply, maps:get(exit_code, State, nil), State};
handle_call({close, []}, _From, State) ->
    handle_close(State);
handle_call(Msg, _From, State) ->
    ?LOG_WARNING("Unknown call", #{message => Msg}),
    {reply, {error, unknown_call}, State}.

%% @doc Ignore casts.
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle port data and port exit.
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info({Port, {data, Packet}}, #{port := Port} = State) ->
    case erlang:binary_to_term(Packet, [safe]) of
        {stdout, _ChildId, Data} ->
            push_lines(stdout, Data, State);
        {stderr, _ChildId, Data} ->
            push_lines(stderr, Data, State);
        {exit, _ChildId, Code} ->
            ?LOG_INFO("ReactiveSubprocess exited", #{exit_code => Code}),
            %% Flush pending partial lines, then notify the delegate.
            S0 = push_pending(stdout, State),
            S1 = push_pending(stderr, S0),
            catch beamtalk_subprocess_port:close(Port),
            NewState = S1#{exit_code => Code, port_closed => true},
            cast_notify(NewState, 'subprocessExit:from:', [Code]),
            {noreply, NewState};
        _Other ->
            {noreply, State}
    end;
handle_info({Port, {exit_status, _N}}, #{port := Port, port_closed := true} = State) ->
    %% Port was already closed cleanly via the {exit} handler — discard.
    {noreply, State};
handle_info({Port, {exit_status, _N}}, #{port := Port} = State) ->
    %% beamtalk_exec binary itself exited unexpectedly.
    ?LOG_WARNING("ReactiveSubprocess exec port exited unexpectedly"),
    S0 = push_pending(stdout, State),
    S1 = push_pending(stderr, S0),
    NewState = S1#{port_closed => true},
    cast_notify(NewState, 'subprocessExit:from:', [maps:get(exit_code, NewState, nil)]),
    {noreply, NewState};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @doc No-op hot reload.
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Clean up the port and child when the gen_server stops.
-spec terminate(term(), map()) -> ok.
terminate(_Reason, State) ->
    case maps:get(port_closed, State, false) of
        true ->
            ok;
        false ->
            Port = maps:get(port, State),
            ChildId = maps:get(child_id, State),
            catch beamtalk_subprocess_port:kill_child(Port, ChildId),
            catch beamtalk_subprocess_port:close(Port),
            ok
    end.

%%% ============================================================================
%%% Internal handlers
%%% ============================================================================

%% @private Write Data + newline to subprocess stdin.
-spec handle_writeLine(iodata(), map()) -> {reply, nil | {error, #beamtalk_error{}}, map()}.
handle_writeLine(Data, State) ->
    #{port := Port, child_id := ChildId, port_closed := PortClosed} = State,
    case PortClosed of
        true ->
            Err = beamtalk_error:new(port_closed, 'ReactiveSubprocess', 'writeLine:'),
            {reply, {error, Err}, State};
        false ->
            Bin = iolist_to_binary(Data),
            Line = <<Bin/binary, $\n>>,
            beamtalk_subprocess_port:write_stdin(Port, ChildId, Line),
            {reply, nil, State}
    end.

%% @private Kill subprocess and close the exec port.
-spec handle_close(map()) -> {reply, nil, map()}.
handle_close(State) ->
    case maps:get(port_closed, State, false) of
        true ->
            {reply, nil, State};
        false ->
            #{port := Port, child_id := ChildId} = State,
            catch beamtalk_subprocess_port:kill_child(Port, ChildId),
            catch beamtalk_subprocess_port:close(Port),
            {reply, nil, State#{port_closed => true}}
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Push complete lines from Data to the notify actor.
%%
%% Combines the pending fragment with new data, splits on newlines, and casts
%% each complete line to the notify actor. Stores the remainder as new pending.
-spec push_lines(stdout | stderr, binary(), map()) -> {noreply, map()}.
push_lines(stdout, Data, #{stdout_pending := Pending} = State) ->
    {Lines, Remainder} = beamtalk_subprocess_port:flush_and_collect(Pending, Data),
    lists:foreach(
        fun(Line) -> cast_notify(State, 'subprocessLine:from:', [Line]) end,
        Lines
    ),
    {noreply, State#{stdout_pending => Remainder}};
push_lines(stderr, Data, #{stderr_pending := Pending} = State) ->
    {Lines, Remainder} = beamtalk_subprocess_port:flush_and_collect(Pending, Data),
    lists:foreach(
        fun(Line) -> cast_notify(State, 'subprocessStderrLine:from:', [Line]) end,
        Lines
    ),
    {noreply, State#{stderr_pending => Remainder}}.

%% @private Flush any pending partial line and push it to the notify actor.
%%
%% Called on subprocess exit: the final output may not end with a newline.
-spec push_pending(stdout | stderr, map()) -> map().
push_pending(stdout, #{stdout_pending := <<>>} = State) ->
    State;
push_pending(stdout, #{stdout_pending := Partial} = State) ->
    cast_notify(State, 'subprocessLine:from:', [Partial]),
    State#{stdout_pending => <<>>};
push_pending(stderr, #{stderr_pending := <<>>} = State) ->
    State;
push_pending(stderr, #{stderr_pending := Partial} = State) ->
    cast_notify(State, 'subprocessStderrLine:from:', [Partial]),
    State#{stderr_pending => <<>>}.

%% @private Cast a message to the notify actor.
%%
%% Appends self_ref as the `from:` argument so the delegate knows which
%% ReactiveSubprocess sent the event.
-spec cast_notify(map(), atom(), list()) -> ok.
cast_notify(#{notify := Notify, self_ref := SelfRef}, Selector, Args) ->
    NotifyPid = Notify#beamtalk_object.pid,
    beamtalk_actor:cast_send(NotifyPid, Selector, Args ++ [SelfRef]).
