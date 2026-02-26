%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive implementations for the TranscriptStream singleton actor.
%%%
%%% **DDD Context:** Runtime
%%%
%%% Implements the `@primitive` methods for the TranscriptStream class. The
%%% compiled Beamtalk actor (`bt@stdlib@transcript_stream`) delegates all
%%% `@primitive` method calls here via `dispatch/3`.
%%%
%%% ## State management
%%%
%%% Since `dispatch/3` is called from within the gen_server process (via
%%% `dispatch/4` → compiled @primitive body → `dispatch/3`), all mutable
%%% state is stored in the gen_server's process dictionary using
%%% `erlang:put/get`. This avoids any gen_server re-entry which would
%%% deadlock.
%%%
%%% Process dictionary keys:
%%% - `'$ts_buffer'`        :: queue:queue(binary()) — ring buffer
%%% - `'$ts_buffer_size'`   :: non_neg_integer()
%%% - `'$ts_max_buffer'`    :: pos_integer() (default 1000)
%%% - `'$ts_subscribers'`   :: #{pid() => reference()}
%%%
%%% ## External API
%%%
%%% `subscribe/1` and `unsubscribe/1` are called by `beamtalk_ws_handler`
%%% and `beamtalk_repl_json` to register for `{transcript_output, Text}`
%%% push messages. They spawn a lightweight future process with its
%%% group_leader set to the caller, then cast using the actor protocol
%%% so that `caller_from_future/1` correctly identifies the subscriber.
%%%
%%% ## Methods
%%%
%%% | Selector      | Type | Description                                 |
%%% |---------------|------|---------------------------------------------|
%%% | `show:'       | cast | Buffer text + push to subscribers           |
%%% | `cr'          | cast | Buffer newline + push to subscribers        |
%%% | `subscribe'   | cast | Add caller to subscriber list               |
%%% | `unsubscribe' | cast | Remove caller from subscriber list          |
%%% | `recent'      | call | Return buffer contents as list              |
%%% | `clear'       | call | Empty the buffer                            |

-module(beamtalk_transcript_stream_primitives).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).
%% Stable external API (called by ws_handler and repl_json)
-export([subscribe/1, unsubscribe/1, ensure_utf8/1]).

%%% ============================================================================
%%% Process dictionary keys
%%% ============================================================================

-define(TS_BUFFER, '$ts_buffer').
-define(TS_BUFFER_SIZE, '$ts_buffer_size').
-define(TS_MAX_BUFFER, '$ts_max_buffer').
-define(TS_SUBSCRIBERS, '$ts_subscribers').
-define(TS_DEFAULT_MAX_BUFFER, 1000).

%%% ============================================================================
%%% dispatch/3 — called from compiled bt@stdlib@transcript_stream for @primitives
%%% ============================================================================

%% @doc Dispatch a primitive method call for TranscriptStream.
%%
%% Called by the compiled `bt@stdlib@transcript_stream:dispatch/3` when evaluating
%% `@primitive` method bodies. All state is managed in the process dictionary
%% (avoiding gen_server re-entry / deadlock). The `Self` argument is the
%% `#beamtalk_object{}` tuple wrapping the gen_server pid.
%%
%% The compiled handle_cast stores FuturePid in `'$bt_future_pid'` before
%% dispatching, so subscribe/unsubscribe can identify the caller via
%% `caller_from_future/1`.
-spec dispatch(atom(), list(), term()) -> term().
dispatch('show:', [Value], Self) ->
    ensure_initialized(),
    Text = to_string(Value),
    buffer_text(Text),
    push_to_subscribers(Text),
    Self;
dispatch(cr, [], Self) ->
    ensure_initialized(),
    buffer_text(<<"\n">>),
    push_to_subscribers(<<"\n">>),
    Self;
dispatch(subscribe, [], Self) ->
    ensure_initialized(),
    FuturePid = erlang:get('$bt_future_pid'),
    CallerPid = caller_from_future(FuturePid),
    add_subscriber(CallerPid),
    Self;
dispatch(unsubscribe, [], Self) ->
    ensure_initialized(),
    FuturePid = erlang:get('$bt_future_pid'),
    CallerPid = caller_from_future(FuturePid),
    remove_subscriber(CallerPid),
    Self;
dispatch(recent, [], _Self) ->
    ensure_initialized(),
    queue:to_list(erlang:get(?TS_BUFFER));
dispatch(clear, [], Self) ->
    ensure_initialized(),
    erlang:put(?TS_BUFFER, queue:new()),
    erlang:put(?TS_BUFFER_SIZE, 0),
    Self;
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'TranscriptStream'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(Err1).

%%% ============================================================================
%%% Stable external API
%%% ============================================================================

%% @doc Subscribe a process to Transcript push messages.
%%
%% The subscriber will receive `{transcript_output, Text}' messages.
%% Uses the actor cast protocol with the caller's pid as group_leader of a
%% temporary future process so `caller_from_future/1` returns the caller.
-spec subscribe(pid() | atom()) -> ok.
subscribe(TranscriptRef) ->
    CallerPid = self(),
    FuturePid = spawn(fun() ->
        receive
            {resolve, _} -> ok;
            {reject, _} -> ok
        after 5000 -> ok
        end
    end),
    erlang:group_leader(CallerPid, FuturePid),
    gen_server:cast(TranscriptRef, {subscribe, [], FuturePid}).

%% @doc Unsubscribe a process from Transcript push messages.
-spec unsubscribe(pid() | atom()) -> ok.
unsubscribe(TranscriptRef) ->
    CallerPid = self(),
    FuturePid = spawn(fun() ->
        receive
            {resolve, _} -> ok;
            {reject, _} -> ok
        after 5000 -> ok
        end
    end),
    erlang:group_leader(CallerPid, FuturePid),
    gen_server:cast(TranscriptRef, {unsubscribe, [], FuturePid}).

%% @doc Ensure a binary is valid UTF-8.
%% Returns the binary unchanged if valid, or a formatted representation otherwise.
-spec ensure_utf8(binary()) -> binary().
ensure_utf8(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Utf8 when is_binary(Utf8) -> Utf8;
        {error, _, _} -> list_to_binary(io_lib:format("~p", [Bin]));
        {incomplete, _, _} -> list_to_binary(io_lib:format("~p", [Bin]))
    end.

%%% ============================================================================
%%% Internal state helpers (process dictionary)
%%% ============================================================================

%% @private Ensure the process dictionary state keys are initialised.
%% Safe to call repeatedly; no-op if already initialised.
-spec ensure_initialized() -> ok.
ensure_initialized() ->
    case erlang:get(?TS_BUFFER) of
        undefined ->
            erlang:put(?TS_BUFFER, queue:new()),
            erlang:put(?TS_BUFFER_SIZE, 0),
            erlang:put(?TS_MAX_BUFFER, ?TS_DEFAULT_MAX_BUFFER),
            erlang:put(?TS_SUBSCRIBERS, #{});
        _ ->
            ok
    end.

%% @private Add text to the ring buffer, dropping oldest entry if at capacity.
-spec buffer_text(binary()) -> ok.
buffer_text(Text) ->
    Buffer = erlang:get(?TS_BUFFER),
    Size = erlang:get(?TS_BUFFER_SIZE),
    Max = erlang:get(?TS_MAX_BUFFER),
    Buffer1 = queue:in(Text, Buffer),
    case Size >= Max of
        true ->
            {_, Buffer2} = queue:out(Buffer1),
            erlang:put(?TS_BUFFER, Buffer2);
        false ->
            erlang:put(?TS_BUFFER, Buffer1),
            erlang:put(?TS_BUFFER_SIZE, Size + 1)
    end,
    ok.

%% @private Send text to all current subscribers.
-spec push_to_subscribers(binary()) -> ok.
push_to_subscribers(Text) ->
    Subs = erlang:get(?TS_SUBSCRIBERS),
    maps:foreach(
        fun(Pid, _Ref) ->
            Pid ! {transcript_output, Text}
        end,
        Subs
    ),
    ok.

%% @private Add a subscriber and monitor it for automatic cleanup.
-spec add_subscriber(pid()) -> ok.
add_subscriber(Pid) when is_pid(Pid) ->
    Subs = erlang:get(?TS_SUBSCRIBERS),
    case maps:is_key(Pid, Subs) of
        true ->
            ok;
        false ->
            Ref = monitor(process, Pid),
            erlang:put(?TS_SUBSCRIBERS, Subs#{Pid => Ref}),
            ok
    end.

%% @private Remove a subscriber and demonitor it.
-spec remove_subscriber(pid()) -> ok.
remove_subscriber(Pid) ->
    Subs = erlang:get(?TS_SUBSCRIBERS),
    case maps:find(Pid, Subs) of
        {ok, Ref} ->
            demonitor(Ref, [flush]),
            erlang:put(?TS_SUBSCRIBERS, maps:remove(Pid, Subs)),
            ok;
        error ->
            ok
    end.

%% @private Derive the caller's pid from a future process via its group_leader.
%% Falls back to the FuturePid itself if the future has already exited.
-spec caller_from_future(pid() | undefined) -> pid().
caller_from_future(undefined) ->
    self();
caller_from_future(FuturePid) when is_pid(FuturePid) ->
    case erlang:process_info(FuturePid, group_leader) of
        {group_leader, GL} -> GL;
        undefined -> FuturePid
    end;
caller_from_future(_Other) ->
    self().

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Convert a value to its string representation for display.
-spec to_string(term()) -> binary().
to_string(Value) when is_binary(Value) ->
    ensure_utf8(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_string(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
to_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_string(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        {error, _, _} -> list_to_binary(io_lib:format("~p", [Value]));
        {incomplete, _, _} -> list_to_binary(io_lib:format("~p", [Value]))
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(#beamtalk_object{class = Class}) ->
    <<"a ", (atom_to_binary(Class, utf8))/binary>>;
to_string(Value) when is_map(Value) ->
    beamtalk_tagged_map:format_for_display(Value);
to_string(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).
