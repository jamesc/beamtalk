%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_log).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture (BT-4899): capture OTP `logger` events. Replaces the
per-file "module as logger handler" pattern (a `log/2` callback, handler
add/remove, and primary-level save/restore).

The capture handler forwards every event to the process that installed it, as
`{beamtalk_test_log, Event}` messages, read with `await/2` and `drain/0`.
""".

-export([
    with_capture/1,
    with_capture/2,
    start_capture/1,
    stop_capture/1,
    await/2,
    drain/0,
    log/2
]).

-type event() :: logger:log_event().
-type handle() :: {logger:handler_id(), logger:level() | none}.

-doc "Run `Fun/0` with a capture handler at `debug` and above; see `with_capture/2`.".
-spec with_capture(fun(() -> Result)) -> Result.
with_capture(Fun) ->
    with_capture(debug, Fun).

-doc """
Run `Fun/0` with the primary logger level lowered to `Level` and a capture
handler installed, then restore the level and remove the handler (also when
`Fun` raises). Inside `Fun`, read events with `await/2` or `drain/0`. Events
still queued when `Fun` returns are discarded.
""".
-spec with_capture(logger:level(), fun(() -> Result)) -> Result.
with_capture(Level, Fun) ->
    Handle = start_capture(Level),
    try
        Fun()
    after
        stop_capture(Handle)
    end.

-doc "Install a capture handler for the calling process; see `stop_capture/1`.".
-spec start_capture(logger:level()) -> handle().
start_capture(Level) ->
    #{level := OldLevel} = logger:get_primary_config(),
    Id = list_to_atom("beamtalk_test_log_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Id, ?MODULE, #{config => #{owner => self()}, level => all}),
    ok = logger:set_primary_config(level, Level),
    {Id, OldLevel}.

-doc "Remove the handler, restore the primary level, and discard queued events.".
-spec stop_capture(handle()) -> ok.
stop_capture({Id, OldLevel}) ->
    _ = logger:remove_handler(Id),
    ok = logger:set_primary_config(level, OldLevel),
    _ = drain(),
    ok.

-doc """
Wait up to `TimeoutMs` for a captured event satisfying `Pred`. Events that do
not match are skipped (consumed). Returns `{ok, Event}` or `timeout`.
""".
-spec await(fun((event()) -> boolean()), timeout()) -> {ok, event()} | timeout.
await(Pred, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    await_loop(Pred, Deadline).

await_loop(Pred, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {?MODULE, Event} ->
            case Pred(Event) of
                true -> {ok, Event};
                false -> await_loop(Pred, Deadline)
            end
    after Remaining -> timeout
    end.

-doc "Return (and consume) every captured event already queued, oldest first.".
-spec drain() -> [event()].
drain() ->
    drain([]).

drain(Acc) ->
    receive
        {?MODULE, Event} -> drain([Event | Acc])
    after 0 -> lists:reverse(Acc)
    end.

%% logger handler callback
-doc false.
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(Event, #{config := #{owner := Owner}}) ->
    Owner ! {?MODULE, Event},
    ok.
