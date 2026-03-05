%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc OTP gen_server for interactive subprocess management (ADR 0051, Phase 4a).
%%%
%%% **DDD Context:** runtime
%%%
%%% Each `beamtalk_subprocess` process owns one port to the `beamtalk_exec`
%%% Rust helper binary and manages exactly one child subprocess (ChildId = 0).
%%% stdout data is buffered internally in an OTP queue; callers block via
%%% deferred `gen_server:reply/2` until a complete line is available.
%%%
%%% == State Map ==
%%%
%%% ```erlang
%%% #{
%%%   port          => port(),      % beamtalk_exec port
%%%   child_id      => 0,           % fixed — one child per gen_server
%%%   {stdout, buffer}  => queue:queue(binary()),  % complete buffered lines
%%%   {stdout, pending} => binary(),              % partial line fragment
%%%   {stdout, waiting} => From | {From, TimerRef} | undefined,
%%%   exit_code     => nil | non_neg_integer(),
%%%   port_closed   => boolean()
%%% }
%%% ```
%%%
%%% == Selectors ==
%%%
%%% * `{'writeLine:', [Data]}` — append newline and write to subprocess stdin
%%% * `{readLine, []}` — deferred reply; blocks until a line or EOF
%%% * `{'readLine:', [Timeout]}` — like readLine but returns nil after Timeout ms
%%% * `{exitCode, []}` — returns nil while running, integer after exit
%%% * `{close, []}` — send kill command to helper, close port
%%%
%%% == References ==
%%%
%%% * ADR 0051 "Subprocess Execution" — Tier 2 design
%%% * `beamtalk_exec_port` — low-level port commands

-module(beamtalk_subprocess).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([start_link/1, start/1]).

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

-type channel() :: stdout.

-type config() :: #{
    executable := binary(),
    args => [binary()],
    env => #{binary() => binary()},
    dir => binary()
}.

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Start a linked subprocess gen_server.
%%
%% Config must contain `executable` (binary path or name on PATH).
%% Optional keys: `args` ([binary()]), `env` (#{binary() => binary()}), `dir` (binary()).
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Start an unlinked subprocess gen_server (useful in tests).
-spec start(config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start(?MODULE, Config, []).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

%% @doc Open the exec port and spawn the child subprocess.
-spec init(config()) -> {ok, map()} | {stop, term()}.
init(Config) ->
    Executable = maps:get(executable, Config),
    Args = maps:get(args, Config, []),
    Options = maps:with([env, dir], Config),
    Port = beamtalk_exec_port:open(),
    ChildId = 0,
    beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args, Options),
    ?LOG_INFO("Subprocess spawned", #{executable => Executable, child_id => ChildId}),
    State = #{
        port => Port,
        child_id => ChildId,
        {stdout, buffer} => queue:new(),
        {stdout, pending} => <<>>,
        {stdout, waiting} => undefined,
        exit_code => nil,
        port_closed => false
    },
    {ok, State}.

%% @doc Dispatch sync calls.
-spec handle_call(term(), term(), map()) ->
    {reply, term(), map()} | {noreply, map()}.
handle_call({'writeLine:', [Data]}, _From, State) ->
    handle_writeLine(Data, State);
handle_call({readLine, []}, From, State) ->
    read_line_for(stdout, From, infinity, State);
handle_call({'readLine:', [Timeout]}, From, State) when is_integer(Timeout), Timeout > 0 ->
    read_line_for(stdout, From, Timeout, State);
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

%% @doc Handle port data, timeouts, and port exit.
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info({Port, {data, Packet}}, #{port := Port} = State) ->
    case erlang:binary_to_term(Packet, [safe]) of
        {stdout, _ChildId, Data} ->
            buffer_and_maybe_reply(stdout, Data, State);
        {exit, _ChildId, Code} ->
            ?LOG_INFO("Subprocess exited", #{exit_code => Code}),
            %% Flush any partial line that lacked a trailing newline
            S0 = flush_pending(stdout, State),
            %% Close the exec port — child is gone, no more I/O possible
            catch beamtalk_exec_port:close(Port),
            NewState = S0#{exit_code => Code, port_closed => true},
            S1 = maybe_reply_eof(stdout, NewState),
            {noreply, S1};
        _Other ->
            {noreply, State}
    end;
handle_info({Port, {exit_status, _N}}, #{port := Port} = State) ->
    %% beamtalk_exec binary itself exited — treat all channels as EOF
    ?LOG_WARNING("Exec port exited unexpectedly"),
    S0 = flush_pending(stdout, State),
    NewState = S0#{port_closed => true},
    S1 = maybe_reply_eof(stdout, NewState),
    {noreply, S1};
handle_info({read_timeout, Channel}, State) ->
    WaitKey = {Channel, waiting},
    case maps:get(WaitKey, State, undefined) of
        undefined ->
            %% Timer fired after data already arrived — ignore
            {noreply, State};
        {From, _TimerRef} ->
            gen_server:reply(From, nil),
            {noreply, State#{WaitKey => undefined}};
        _Other ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

%% @doc No-op hot reload — state schema is stable.
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Clean up the port and child when the gen_server stops.
-spec terminate(term(), map()) -> ok.
terminate(_Reason, State) ->
    cleanup_port(State),
    ok.

%%% ============================================================================
%%% Internal handlers
%%% ============================================================================

%% @private Write Data + newline to subprocess stdin.
-spec handle_writeLine(iodata(), map()) -> {reply, nil | {error, port_closed}, map()}.
handle_writeLine(Data, State) ->
    #{port := Port, child_id := ChildId, port_closed := PortClosed} = State,
    case PortClosed of
        true ->
            {reply, {error, port_closed}, State};
        false ->
            Bin = iolist_to_binary(Data),
            Line = <<Bin/binary, $\n>>,
            beamtalk_exec_port:write_stdin(Port, ChildId, Line),
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
            beamtalk_exec_port:kill_child(Port, ChildId),
            beamtalk_exec_port:close(Port),
            NewState = State#{port_closed => true},
            S1 = maybe_reply_eof(stdout, NewState),
            {reply, nil, S1}
    end.

%% @private Deferred-reply read for Channel (stdout).
%%
%% Returns a buffered line immediately if one is available.
%% If the buffer is empty and the port is closed, returns nil (EOF).
%% Otherwise defers the reply: stashes From (with optional timer) in state;
%% `handle_info` or the timeout message will deliver the final reply.
-spec read_line_for(channel(), term(), infinity | pos_integer(), map()) ->
    {reply, binary() | nil, map()} | {noreply, map()}.
read_line_for(Channel, From, Timeout, State) ->
    BufKey = {Channel, buffer},
    WaitKey = {Channel, waiting},
    Buffer = maps:get(BufKey, State),
    PortClosed = maps:get(port_closed, State, false),
    ExistingWaiter = maps:get(WaitKey, State, undefined),
    case queue:is_empty(Buffer) of
        false ->
            {{value, Line}, Rest} = queue:out(Buffer),
            {reply, Line, State#{BufKey => Rest}};
        true when PortClosed ->
            %% EOF — no more data will arrive
            {reply, nil, State};
        true when ExistingWaiter =/= undefined ->
            %% Single-consumer: reject concurrent readLine callers (ADR 0051)
            {reply, {error, read_in_progress}, State};
        true when Timeout =:= infinity ->
            %% Defer reply until data arrives via handle_info
            {noreply, State#{WaitKey => From}};
        true ->
            %% Defer with a timer; timeout message delivers nil
            TimerRef = erlang:send_after(Timeout, self(), {read_timeout, Channel}),
            {noreply, State#{WaitKey => {From, TimerRef}}}
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Buffer incoming data and reply to any waiting caller.
-spec buffer_and_maybe_reply(channel(), binary(), map()) -> {noreply, map()}.
buffer_and_maybe_reply(Channel, Data, State) ->
    BufKey = {Channel, buffer},
    PendKey = {Channel, pending},
    WaitKey = {Channel, waiting},
    Buffer = maps:get(BufKey, State),
    Pending = maps:get(PendKey, State, <<>>),
    Combined = <<Pending/binary, Data/binary>>,
    {Lines, Remainder} = split_lines(Combined),
    NewBuffer = queue:join(Buffer, queue:from_list(Lines)),
    case maps:get(WaitKey, State, undefined) of
        undefined ->
            {noreply, State#{BufKey => NewBuffer, PendKey => Remainder}};
        Waiter ->
            case queue:is_empty(NewBuffer) of
                false ->
                    {{value, Line}, Rest} = queue:out(NewBuffer),
                    reply_to_waiter(Waiter, Line),
                    {noreply, State#{
                        BufKey => Rest,
                        PendKey => Remainder,
                        WaitKey => undefined
                    }};
                true ->
                    %% Data arrived but no complete lines yet — keep waiting
                    {noreply, State#{BufKey => NewBuffer, PendKey => Remainder}}
            end
    end.

%% @private Split binary data on newlines.
%% Returns {CompleteLines, PartialRemainder}.
-spec split_lines(binary()) -> {[binary()], binary()}.
split_lines(Data) ->
    case binary:split(Data, <<"\n">>, [global]) of
        [Remainder] ->
            %% No newlines — entire data is a partial line
            {[], Remainder};
        Parts ->
            %% Last element is the remainder after the last newline (may be <<>>)
            {Lines, [Remainder]} = lists:split(length(Parts) - 1, Parts),
            {Lines, Remainder}
    end.

%% @private Flush any partial line from the pending buffer into the line queue.
%%
%% Called on subprocess exit: the final output may not end with a newline.
%% Without this, partial data (e.g., `printf "hello"`) would be silently lost.
-spec flush_pending(channel(), map()) -> map().
flush_pending(Channel, State) ->
    PendKey = {Channel, pending},
    BufKey = {Channel, buffer},
    case maps:get(PendKey, State, <<>>) of
        <<>> ->
            State;
        Partial ->
            Buffer = maps:get(BufKey, State),
            NewBuffer = queue:in(Partial, Buffer),
            State#{BufKey => NewBuffer, PendKey => <<>>}
    end.

%% @private Reply to any waiting caller: buffered data if available, nil (EOF) if empty.
%%
%% Called when the subprocess exits or the port closes. If the buffer has data
%% (e.g., a partial line flushed by `flush_pending/2`), reply with the first
%% line instead of nil — otherwise the data would be silently lost.
-spec maybe_reply_eof(channel(), map()) -> map().
maybe_reply_eof(Channel, State) ->
    WaitKey = {Channel, waiting},
    BufKey = {Channel, buffer},
    case maps:get(WaitKey, State, undefined) of
        undefined ->
            State;
        Waiter ->
            Buffer = maps:get(BufKey, State),
            case queue:is_empty(Buffer) of
                true ->
                    reply_to_waiter(Waiter, nil),
                    State#{WaitKey => undefined};
                false ->
                    {{value, Line}, Rest} = queue:out(Buffer),
                    reply_to_waiter(Waiter, Line),
                    State#{WaitKey => undefined, BufKey => Rest}
            end
    end.

%% @private Reply to a waiting caller, cancelling any pending timer.
-spec reply_to_waiter(term(), term()) -> ok.
reply_to_waiter({From, TimerRef}, Value) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    gen_server:reply(From, Value);
reply_to_waiter(From, Value) ->
    gen_server:reply(From, Value).

%% @private Kill child and close the exec port during shutdown.
-spec cleanup_port(map()) -> ok.
cleanup_port(State) ->
    case maps:get(port_closed, State, false) of
        true ->
            ok;
        false ->
            Port = maps:get(port, State),
            ChildId = maps:get(child_id, State),
            catch beamtalk_exec_port:kill_child(Port, ChildId),
            catch beamtalk_exec_port:close(Port),
            ok
    end.
