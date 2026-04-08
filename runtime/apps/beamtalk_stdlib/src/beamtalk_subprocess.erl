%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_subprocess).
-behaviour(gen_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
OTP gen_server for interactive subprocess management (ADR 0051, ADR 0056).

Each `beamtalk_subprocess` process owns one port to the `beamtalk_exec`
Rust helper binary and manages exactly one child subprocess (ChildId = 0).
stdout and stderr data are buffered independently in OTP queues; callers
block via deferred `gen_server:reply/2` until a complete line is available.

Native backing gen_server for the Subprocess class (ADR 0056). The compiled
facade module (`bt@stdlib@subprocess`) handles class/instance dispatch;
this module only exports `start_link/1` and gen_server callbacks.

== State Map ==

```erlang
#{
  port          => port(),      % beamtalk_exec port
  child_id      => 0,           % fixed — one child per gen_server
  {stdout, buffer}  => queue:queue(binary()),  % complete buffered lines
  {stdout, pending} => binary(),              % partial line fragment
  {stdout, waiting} => From | {timer, From, TimerRef} | undefined,
  {stderr, buffer}  => queue:queue(binary()),  % complete buffered lines
  {stderr, pending} => binary(),              % partial line fragment
  {stderr, waiting} => From | {timer, From, TimerRef} | undefined,
  exit_code     => nil | non_neg_integer(),
  port_closed   => boolean(),
  child_exited  => boolean()
}
```

== Selectors ==

* `{'writeLine:', [Data]}` — append newline and write to subprocess stdin
* `{readLine, []}` — deferred reply; blocks until a stdout line or EOF
* `{'readLine:', [Timeout]}` — like readLine but returns nil after Timeout ms
* `{readStderrLine, []}` — deferred reply; blocks until a stderr line or EOF
* `{'readStderrLine:', [Timeout]}` — like readStderrLine but returns nil after Timeout ms
* `{lines, []}` — returns a Stream whose generator calls readLine via gen_server:call
* `{stderrLines, []}` — returns a Stream whose generator calls readStderrLine via gen_server:call
* `{exitCode, []}` — returns nil while running, integer after exit
* `{close, []}` — send kill command to helper, close port

== References ==

* ADR 0051 "Subprocess Execution" — Tier 2 design
* ADR 0056 "Native Erlang-Backed Actors" — native: + self delegate
* ADR 0021 "Streams and I/O" — Stream generator pattern
* `beamtalk_exec_port` — low-level port commands
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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

-type channel() :: stdout | stderr.

-type config() :: #{
    executable := binary(),
    args => [binary()],
    env => #{binary() => binary()},
    dir => binary()
}.

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Start a linked subprocess gen_server.

Config must contain `executable` (binary path or name on PATH).
Optional keys: `args` (Beamtalk Array or Erlang list of binaries),
`env` (#{binary() => binary()}), `dir` (binary()).

Called by the native facade's `spawn/1` via `spawnWith:`.
""".
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

-doc "Start an unlinked subprocess gen_server (useful in tests).".
-spec start(config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start(?MODULE, Config, []).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

-doc """
Validate config, open the exec port and spawn the child subprocess.

Accepts raw Beamtalk values from `spawnWith:`: the `args` field may be a
Beamtalk Array (tagged map) which is coerced to an Erlang list. The `env`
field has its `$beamtalk_class` key stripped if present.
""".
-spec init(config()) -> {ok, map()} | {stop, term()}.
init(Config) ->
    case validate_config(Config) of
        {ok, #{executable := Executable, args := Args} = ValidConfig} ->
            Options = maps:with([env, dir], ValidConfig),
            Port = beamtalk_exec_port:open(),
            ChildId = 0,
            beamtalk_exec_port:spawn_child(Port, ChildId, Executable, Args, Options),
            ?LOG_INFO("Subprocess spawned", #{
                executable => Executable, child_id => ChildId, domain => [beamtalk, stdlib]
            }),
            State = #{
                port => Port,
                child_id => ChildId,
                {stdout, buffer} => queue:new(),
                {stdout, pending} => <<>>,
                {stdout, waiting} => undefined,
                {stderr, buffer} => queue:new(),
                {stderr, pending} => <<>>,
                {stderr, waiting} => undefined,
                exit_code => nil,
                child_exited => false,
                port_closed => false
            },
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

-doc "Dispatch sync calls.".
-spec handle_call(term(), term(), map()) ->
    {reply, term(), map()} | {noreply, map()}.
%% BT-1604: Strip propagated context from 3-tuple messages (ADR 0069 Phase 2b)
handle_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    handle_call({Selector, Args}, From, State);
handle_call({'writeLine:', [Data]}, _From, State) ->
    handle_writeLine(Data, State);
handle_call({readLine, []}, From, State) ->
    read_line_for(stdout, From, infinity, State);
handle_call({'readLine:', [Timeout]}, From, State) when is_integer(Timeout), Timeout > 0 ->
    read_line_for(stdout, From, Timeout, State);
handle_call({readStderrLine, []}, From, State) ->
    read_line_for(stderr, From, infinity, State);
handle_call({'readStderrLine:', [Timeout]}, From, State) when is_integer(Timeout), Timeout > 0 ->
    read_line_for(stderr, From, Timeout, State);
handle_call({lines, []}, _From, State) ->
    Pid = self(),
    Stream = make_readline_stream(Pid, stdout),
    {reply, Stream, State};
handle_call({stderrLines, []}, _From, State) ->
    Pid = self(),
    Stream = make_readline_stream(Pid, stderr),
    {reply, Stream, State};
handle_call({exitCode, []}, _From, State) ->
    {reply, maps:get(exit_code, State, nil), State};
handle_call({close, []}, _From, State) ->
    handle_close(State);
handle_call(Msg, _From, State) ->
    ?LOG_WARNING("Unknown call", #{message => Msg, domain => [beamtalk, stdlib]}),
    {reply, {error, unknown_call}, State}.

-doc "Ignore casts.".
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc "Handle port data, timeouts, and port exit.".
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info({Port, {data, Packet}}, #{port := Port} = State) ->
    case erlang:binary_to_term(Packet, [safe]) of
        {stdout, _ChildId, Data} ->
            buffer_and_maybe_reply(stdout, Data, State);
        {stderr, _ChildId, Data} ->
            buffer_and_maybe_reply(stderr, Data, State);
        {exit, _ChildId, Code} ->
            ?LOG_INFO("Subprocess exited", #{exit_code => Code, domain => [beamtalk, stdlib]}),
            %% The beamtalk-exec binary joins its reader threads before sending
            %% this exit event (BT-1148), so all stdout/stderr data is guaranteed
            %% to have arrived.  Flush any partial line and close the port now.
            S0 = flush_pending(stdout, State),
            S1 = flush_pending(stderr, S0),
            (try
                beamtalk_exec_port:close(Port)
            catch
                _:_ -> ok
            end),
            NewState = S1#{exit_code => Code, child_exited => true, port_closed => true},
            S2 = maybe_reply_eof(stdout, NewState),
            S3 = maybe_reply_eof(stderr, S2),
            {noreply, S3};
        _Other ->
            {noreply, State}
    end;
handle_info({Port, {exit_status, _N}}, #{port := Port, port_closed := true} = State) ->
    %% Port was already closed cleanly via the {exit} handler.  OTP delivers
    %% {exit_status} even for ports closed by port_close/1, so this is normal
    %% on the happy path — discard without logging.
    {noreply, State};
handle_info({Port, {exit_status, _N}}, #{port := Port} = State) ->
    %% beamtalk_exec binary itself exited unexpectedly — treat all channels as EOF.
    ?LOG_WARNING("Exec port exited unexpectedly", #{domain => [beamtalk, stdlib]}),
    S0 = flush_pending(stdout, State),
    S1 = flush_pending(stderr, S0),
    NewState = S1#{port_closed => true},
    S2 = maybe_reply_eof(stdout, NewState),
    S3 = maybe_reply_eof(stderr, S2),
    {noreply, S3};
handle_info({read_timeout, Channel}, State) ->
    WaitKey = {Channel, waiting},
    case maps:get(WaitKey, State, undefined) of
        undefined ->
            %% Timer fired after data already arrived — ignore
            {noreply, State};
        {timer, From, _TimerRef} ->
            gen_server:reply(From, nil),
            {noreply, State#{WaitKey => undefined}};
        _Other ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

-doc "No-op hot reload — state schema is stable.".
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-doc "Clean up the port and child when the gen_server stops.".
-spec terminate(term(), map()) -> ok.
terminate(_Reason, State) ->
    cleanup_port(State),
    ok.

%%% ============================================================================
%%% Config validation (moved from dispatch/3, BT-1211)
%%% ============================================================================

-doc """
Validate and normalize the config map from spawnWith:.

Coerces Beamtalk Arrays to Erlang lists, strips $beamtalk_class from env,
and validates types. Returns {ok, CleanConfig} or {error, Reason}.
""".
-spec validate_config(map()) -> {ok, config()} | {error, term()}.
validate_config(Config) when is_map(Config) ->
    case maps:get(executable, Config, undefined) of
        undefined ->
            {error, {missing_key, executable}};
        Executable when not is_binary(Executable) ->
            {error, {type_error, executable, expected_binary}};
        Executable ->
            validate_config_args(Config, Executable)
    end;
validate_config(_Other) ->
    {error, {type_error, config, expected_map}}.

-doc "Validate and coerce the args field.".
-spec validate_config_args(map(), binary()) -> {ok, config()} | {error, term()}.
validate_config_args(Config, Executable) ->
    RawArgs = maps:get(args, Config, []),
    case coerce_args(RawArgs) of
        {ok, ArgsList} ->
            case lists:all(fun erlang:is_binary/1, ArgsList) of
                true ->
                    validate_config_env(Config, Executable, ArgsList);
                false ->
                    {error, {type_error, args, expected_binary_elements}}
            end;
        {error, _} = Err ->
            Err
    end.

-doc "Validate and clean the env field.".
-spec validate_config_env(map(), binary(), [binary()]) -> {ok, config()} | {error, term()}.
validate_config_env(Config, Executable, ArgsList) ->
    Base = #{executable => Executable, args => ArgsList},
    case maps:get(env, Config, undefined) of
        undefined ->
            maybe_add_dir(Config, Base);
        Env when is_map(Env) ->
            CleanEnv = maps:without(['$beamtalk_class'], Env),
            case validate_binary_env(CleanEnv) of
                ok ->
                    maybe_add_dir(Config, Base#{env => CleanEnv});
                {error, _} = Err ->
                    Err
            end;
        _Other ->
            {error, {type_error, env, expected_map}}
    end.

-doc "Optionally add the dir field.".
-spec maybe_add_dir(map(), config()) -> {ok, config()} | {error, term()}.
maybe_add_dir(Config, Base) ->
    case maps:get(dir, Config, undefined) of
        undefined ->
            {ok, Base};
        Dir when is_binary(Dir) ->
            {ok, Base#{dir => Dir}};
        _Other ->
            {error, {type_error, dir, expected_binary}}
    end.

-doc "Coerce a Beamtalk Array (tagged map) or plain list to an Erlang list.".
-spec coerce_args(term()) -> {ok, list()} | {error, term()}.
coerce_args(#{'$beamtalk_class' := 'Array', 'data' := Arr}) ->
    case array:is_array(Arr) of
        true -> {ok, array:to_list(Arr)};
        false -> {error, {type_error, args, invalid_array}}
    end;
coerce_args(List) when is_list(List) ->
    {ok, List};
coerce_args(_Other) ->
    {error, {type_error, args, expected_list_or_array}}.

-doc "Validate that all keys and values in env are binaries.".
-spec validate_binary_env(map()) -> ok | {error, term()}.
validate_binary_env(Env) ->
    AllBinary = maps:fold(
        fun(K, V, Acc) -> Acc andalso is_binary(K) andalso is_binary(V) end,
        true,
        Env
    ),
    case AllBinary of
        true -> ok;
        false -> {error, {type_error, env, expected_binary_keys_and_values}}
    end.

%%% ============================================================================
%%% Internal handlers
%%% ============================================================================

-doc "Write Data + newline to subprocess stdin.".
-spec handle_writeLine(iodata(), map()) -> {reply, nil | {error, #beamtalk_error{}}, map()}.
handle_writeLine(Data, State) ->
    #{port := Port, child_id := ChildId, port_closed := PortClosed, child_exited := ChildExited} =
        State,
    case PortClosed orelse ChildExited of
        true ->
            Err = beamtalk_error:new(port_closed, 'Subprocess', 'writeLine:'),
            {reply, {error, Err}, State};
        false ->
            Bin = iolist_to_binary(Data),
            Line = <<Bin/binary, $\n>>,
            beamtalk_exec_port:write_stdin(Port, ChildId, Line),
            {reply, nil, State}
    end.

-doc "Kill subprocess and close the exec port.".
-spec handle_close(map()) -> {reply, nil, map()}.
handle_close(State) ->
    case maps:get(port_closed, State, false) of
        true ->
            {reply, nil, State};
        false ->
            #{port := Port, child_id := ChildId} = State,
            S0 = flush_pending(stdout, State),
            S1 = flush_pending(stderr, S0),
            beamtalk_exec_port:kill_child(Port, ChildId),
            beamtalk_exec_port:close(Port),
            NewState = S1#{port_closed => true},
            S2 = maybe_reply_eof(stdout, NewState),
            S3 = maybe_reply_eof(stderr, S2),
            {reply, nil, S3}
    end.

-doc """
Deferred-reply read for Channel (stdout or stderr).

Returns a buffered line immediately if one is available.
If the buffer is empty and the port is closed, returns nil (EOF).
Otherwise defers the reply: stashes From (with optional timer) in state;
`handle_info` or the timeout message will deliver the final reply.
""".
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
            {noreply, State#{WaitKey => {timer, From, TimerRef}}}
    end.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Buffer incoming data and reply to any waiting caller.".
-spec buffer_and_maybe_reply(channel(), binary(), map()) -> {noreply, map()}.
buffer_and_maybe_reply(Channel, Data, State) ->
    BufKey = {Channel, buffer},
    PendKey = {Channel, pending},
    WaitKey = {Channel, waiting},
    Buffer = maps:get(BufKey, State),
    Pending = maps:get(PendKey, State, <<>>),
    {Lines, Remainder} = beamtalk_subprocess_port:flush_and_collect(Pending, Data),
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

-doc """
Flush any partial line from the pending buffer into the line queue.

Called on subprocess exit: the final output may not end with a newline.
Without this, partial data (e.g., `printf "hello"`) would be silently lost.
""".
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

-doc """
Reply to any waiting caller: buffered data if available, nil (EOF) if empty.

Called when the subprocess exits or the port closes. If the buffer has data
(e.g., a partial line flushed by `flush_pending/2`), reply with the first
line instead of nil — otherwise the data would be silently lost.
""".
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

-doc "Reply to a waiting caller, cancelling any pending timer.".
-spec reply_to_waiter(term(), term()) -> ok.
reply_to_waiter({timer, From, TimerRef}, Value) ->
    erlang:cancel_timer(TimerRef),
    gen_server:reply(From, Value);
reply_to_waiter(From, Value) ->
    gen_server:reply(From, Value).

-doc """
Build a Stream whose generator calls readLine or readStderrLine via gen_server:call.

The generator executes in the caller's process (correct for Streams per ADR 0021).
Each step sends a sync message to the actor to get the next line.
Returns done when the subprocess sends nil (EOF).
""".
-spec make_readline_stream(pid(), channel()) -> map().
make_readline_stream(Pid, Channel) ->
    CallKey =
        case Channel of
            stdout -> {readLine, []};
            stderr -> {readStderrLine, []}
        end,
    Desc =
        case Channel of
            stdout -> <<"Subprocess.lines">>;
            stderr -> <<"Subprocess.stderrLines">>
        end,
    Gen = make_readline_gen(Pid, CallKey),
    beamtalk_stream:make_stream(Gen, Desc).

-doc "Recursive generator: calls gen_server, returns done on nil.".
-spec make_readline_gen(pid(), term()) -> fun(() -> {binary(), fun()} | done).
make_readline_gen(Pid, CallKey) ->
    fun() ->
        case gen_server:call(Pid, CallKey, infinity) of
            nil -> done;
            Line -> {Line, make_readline_gen(Pid, CallKey)}
        end
    end.

-doc "Kill child and close the exec port during shutdown.".
-spec cleanup_port(map()) -> ok.
cleanup_port(State) ->
    case maps:get(port_closed, State, false) of
        true ->
            ok;
        false ->
            Port = maps:get(port, State),
            ChildId = maps:get(child_id, State),
            (try
                beamtalk_exec_port:kill_child(Port, ChildId)
            catch
                _:_ -> ok
            end),
            (try
                beamtalk_exec_port:close(Port)
            catch
                _:_ -> ok
            end),
            ok
    end.
