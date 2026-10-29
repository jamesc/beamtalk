%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_telemetry).

%%% **DDD Context:** Runtime Context

-moduledoc """
The runtime's single home for emitting and attaching to `telemetry` events
(ADR 0069, amended by BT-4025).

Two audiences share it:

- **The stdlib `Telemetry` facade** (`stdlib/src/telemetry.bt`). The class
  is `native: beamtalk_telemetry` and its class-side methods are
  `self delegate` bodies (ADR 0101, BT-4055), lowered to
  `beamtalk_erlang_proxy:native_call/5` into `execute/3`, `span/3`,
  `attach/3`, `attachMany/3` and `detach/1`. The function names follow the
  `native:` naming rule (the selector's first keyword). These validate their
  arguments and raise structured `#beamtalk_error{}`s. The module opts out of
  the proxy's ok/error → `Result` coercion (`beamtalk_ffi_raw_results/0`):
  `span/3` answers the block's own value, which must come back unchanged.
- **The runtime itself.** `dispatch_span/5` instruments actor dispatch and
  `emit/3` emits the actor lifecycle and distribution events. Both moved
  here from `beamtalk_actor` (the old `maybe_span/3`, and the emit half of
  `maybe_execute_telemetry/3`) so the span logic (start/stop/exception, NLR
  handling) and the guarded emit exist exactly once.

## Dispatch events are opt-in (BT-4113)

`[beamtalk, actor, dispatch, start | stop | exception]` are emitted only
while `dispatch_events_enabled/0` is true. Every actor send pays for them
otherwise (a ref, a wall-clock read, two handler-table lookups and the
metadata maps), whether or not anyone listens. The flag is true when any of
these holds:

- tracing is enabled (`Tracing enable`, `beamtalk_trace_store:enable/0`);
- a Beamtalk handler attached through `attach/3` / `attachMany/3` listens
  to an event under `[beamtalk, actor, dispatch]` (read back from
  `telemetry`'s own handler table, so it clears when the last one detaches);
- the app env `{beamtalk_runtime, dispatch_telemetry}` is `true` (read at
  startup and on every refresh), for Erlang/Elixir code that calls
  `telemetry:attach/4` directly, which Beamtalk cannot observe;
- `enable_dispatch_events/0` was called (undone by
  `disable_dispatch_events/0`, which leaves the other sources alone).

The always-on `{Class, Selector}` aggregates (BT-4103) do not depend on the
flag: `dispatch_span/5` records them directly in `beamtalk_trace_store`,
events or not.

The flag is a single `persistent_term` read on the send path. Writing a
`persistent_term` triggers a global GC scan, which is fine here because the
flag flips only on attach/detach/enable/disable: rare, control-plane
operations. Refreshes are serialized with a `global:trans/3` lock local to
the node, so two concurrent flips cannot leave a stale value behind. A
handler `telemetry` detaches on its own (because it raised) keeps the flag
on until the next refresh, which only costs the events.

Every entry point degrades gracefully when the `telemetry` application is not
loaded (a partial test harness): emitting becomes a no-op and spans just run
their body.

## Spans and non-local returns

`telemetry:span/3` catches every `throw`, so a Beamtalk `^` out of a span
block (thrown as the `?IS_NLR` tuple, ADR 0041) would be reported as an
`exception` event. `run_span/3` instead treats an NLR as a normal exit: it
emits `stop` and lets the NLR continue to its method frame. Every other raise
emits `exception` and is re-raised with its original class, reason and
stacktrace.

## Beamtalk handlers

`attach/3` and `attachMany/3` store the Beamtalk block in the handler
**config** and register the exported `handle_event/4` as the handler fun:
`telemetry` warns about (and slows down) local funs used as handlers. The
block runs in the emitting process, and `telemetry` detaches a handler that
raises (and logs it).
""".

-include("beamtalk.hrl").

-type span_exit() :: {stop | exception, map(), integer()}.

-export([
    %% stdlib Telemetry facade (`native:` / `self delegate` targets)
    execute/3,
    span/3,
    attach/3,
    attachMany/3,
    beamtalk_ffi_raw_results/0,
    detach/1,
    detach_handler/1,
    %% telemetry handler callback for Beamtalk blocks
    handle_event/4,
    %% runtime instrumentation (moved from beamtalk_actor, BT-4025)
    dispatch_span/5,
    emit/3,
    %% dispatch-event opt-in (BT-4113)
    dispatch_events_enabled/0,
    enable_dispatch_events/0,
    disable_dispatch_events/0,
    refresh_dispatch_events/0
]).

%% persistent_term keys (BT-4113). PT_DISPATCH_EVENTS is the flag the send
%% path reads; PT_DISPATCH_FORCED is the enable_dispatch_events/0 source.
-define(PT_DISPATCH_EVENTS, beamtalk_dispatch_events).
-define(PT_DISPATCH_FORCED, beamtalk_dispatch_events_forced).
-define(DISPATCH_PREFIX, [beamtalk, actor, dispatch]).

%%====================================================================
%% stdlib facade
%%====================================================================

-doc """
`Telemetry execute: eventName measurements: m metadata: md`.

`EventName` must be a non-empty list of atoms (Symbols), `Measurements` a map
with atom keys, `Metadata` any map. All three reach `telemetry:execute/3`
unchanged. Answers `nil`.
""".
-spec execute(term(), term(), term()) -> nil.
execute(EventName, Measurements, Metadata) ->
    Sel = 'execute:measurements:metadata:',
    check_event_name(EventName, Sel, <<"eventName">>),
    check_measurements(Measurements, Sel),
    check_metadata(Metadata, Sel),
    ok = emit(EventName, Measurements, Metadata),
    nil.

-doc """
`Telemetry span: eventPrefix metadata: md do: aBlock`.

Emits `EventPrefix ++ [start]`, runs `Block`, then emits `EventPrefix ++
[stop]` and answers the block's value. A raise emits `EventPrefix ++
[exception]` and re-raises unchanged; a `^` out of the block emits `stop`
(see the module doc). The stop metadata is the start metadata.
""".
-spec span(term(), term(), term()) -> term().
span(EventPrefix, Metadata, Block) ->
    Sel = 'span:metadata:do:',
    check_event_name(EventPrefix, Sel, <<"eventPrefix">>),
    check_metadata(Metadata, Sel),
    check_block(Block, 0, Sel),
    run_span(EventPrefix, Metadata, fun() -> {Block(), Metadata} end).

-doc """
`Telemetry attach: handlerId to: eventName do: [:event :measurements :metadata | ...]`.

Answers a `Result`: `ok: nil`, or `error:` with kind `handler_exists` when
`HandlerId` is already attached.
""".
-spec attach(term(), term(), term()) -> beamtalk_result:t().
attach(HandlerId, EventName, Block) ->
    Sel = 'attach:to:do:',
    check_event_name(EventName, Sel, <<"eventName">>),
    do_attach(HandlerId, [EventName], Block, Sel).

-doc """
`Telemetry attach: handlerId toAll: eventNames do: [...]` — the
`telemetry:attach_many/4` form. `EventNames` is a non-empty list of event
names. Answers a `Result` like `attach/3`.
""".
-spec attachMany(term(), term(), term()) -> beamtalk_result:t().
attachMany(HandlerId, EventNames, Block) ->
    Sel = 'attach:toAll:do:',
    case is_list(EventNames) andalso EventNames =/= [] of
        true ->
            lists:foreach(
                fun(Name) -> check_event_name(Name, Sel, <<"eventNames">>) end,
                EventNames
            );
        false ->
            type_error(
                Sel,
                <<"eventNames must be a non-empty List of event names (Lists of Symbols)">>,
                EventNames
            )
    end,
    do_attach(HandlerId, EventNames, Block, Sel).

-doc """
`Telemetry detach: handlerId` — idempotent: detaching an unknown id answers
`nil` too. `HandlerId` must be a Symbol or a String, and the runtime's own
handler ids (`beamtalk_trace_store`'s) are refused with `reserved_name` here
and in `attach/3`/`attachMany/3`, so Beamtalk code cannot switch off trace
capture or squat on a runtime id by accident.
""".
-spec detach(term()) -> nil.
detach(HandlerId) ->
    Sel = 'detach:',
    check_handler_id(HandlerId, Sel),
    check_not_reserved(HandlerId, Sel),
    nil = detach_handler(HandlerId),
    %% BT-4113: the last Beamtalk dispatch handler gone switches the
    %% dispatch events back off (unless another source keeps them on).
    ok = refresh_dispatch_events(),
    nil.

-doc """
Opt out of `beamtalk_erlang_proxy`'s ok/error → `Result` coercion (ADR 0076)
for the `native_call/5` path. `span/3` answers whatever the user's block
answers, so a block value of `#ok` or `{error, _}` must come back as is. The
module's own answers are `nil` and already-built `Result`s, so nothing here
relies on the coercion.
""".
-spec beamtalk_ffi_raw_results() -> true.
beamtalk_ffi_raw_results() -> true.

-doc """
Detach any telemetry handler id, unvalidated, for the runtime's own handlers
(`beamtalk_trace_store`). Idempotent, and a no-op when `telemetry` is not
loaded or its handler table is not running.
""".
-spec detach_handler(term()) -> nil.
detach_handler(HandlerId) ->
    try telemetry:detach(HandlerId) of
        _ -> nil
    catch
        error:undef -> nil;
        exit:{noproc, _} -> nil
    end.

-doc """
`telemetry` handler callback for Beamtalk blocks attached with `attach/3` /
`attachMany/3`. Calls the block stored in the handler config with the event
name, measurements and metadata, in the emitting process.

- **Re-entrancy:** while a handler is running, a nested emit that reaches the
  same handler again in this process (a handler on actor dispatch events that
  itself sends to an actor, say) skips it instead of recursing without bound.
- **Class variables (ADR 0130 §4):** `telemetry` catches a raising handler and
  detaches it, so the block runs under `beamtalk_class_vars:protect/1`: if it
  ran at home and raises, its class-variable writes are discarded.
""".
-spec handle_event([atom()], map(), map(), #{
    block := fun((list(), map(), map()) -> term()), id := term()
}) ->
    ok.
handle_event(EventName, Measurements, Metadata, #{block := Block, id := Id}) ->
    Key = {'$bt_telemetry_in_handler', Id},
    case get(Key) of
        undefined ->
            put(Key, true),
            try
                _ = beamtalk_class_vars:protect(fun() ->
                    Block(EventName, Measurements, Metadata)
                end),
                ok
            after
                erase(Key)
            end;
        true ->
            ok
    end.

%%====================================================================
%% Runtime instrumentation
%%====================================================================

-doc """
Wrap one actor send (ADR 0069 Phase 2a): a send to `Pid` (an actor of
`Class`) of `Selector`, in `Mode` (`sync`, `async` or `cast`). `Fun` performs
the send and answers `{Result, Outcome}`, `Outcome` being `ok`, `error` or
`cast`; the span answers `Result`.

Always records the `{Class, Selector}` aggregate directly in
`beamtalk_trace_store`, exactly once per send: a raise counts as an error
and is re-raised with its original class, reason and stacktrace; a `^`
non-local return counts as a normal exit.

With dispatch events off (the default, see the module doc) that is all it
does: two monotonic clock reads around `Fun` and the aggregate, with no
metadata map built at all. With them on it also emits
`[beamtalk, actor, dispatch, start | stop | exception]` (metadata `pid`,
`class`, `selector`, `mode`, plus `outcome` on stop) and, while trace capture
is enabled, sets the causal span id (and the trace id, for a root span) in
the process dictionary and records a trace event.
""".
-spec dispatch_span(pid(), atom(), atom(), sync | async | cast, fun(() -> {term(), atom()})) ->
    term().
dispatch_span(Pid, Class, Selector, Mode, Fun) ->
    case dispatch_events_enabled() andalso erlang:function_exported(telemetry, execute, 3) of
        false ->
            run_recorded(Pid, Class, Selector, Mode, Fun);
        true ->
            run_dispatch_events(
                #{pid => Pid, class => Class, selector => Selector, mode => Mode}, Fun
            )
    end.

-doc """
Whether actor dispatch telemetry events are emitted (BT-4113): one
`persistent_term` read. See the module doc for what turns it on.
""".
-spec dispatch_events_enabled() -> boolean().
dispatch_events_enabled() ->
    persistent_term:get(?PT_DISPATCH_EVENTS, false).

-doc """
Turn dispatch telemetry events on, for Erlang/Elixir code that attaches to
them with `telemetry:attach/4` directly. Stays on until
`disable_dispatch_events/0`.
""".
-spec enable_dispatch_events() -> ok.
enable_dispatch_events() ->
    with_dispatch_lock(fun() ->
        persistent_term:put(?PT_DISPATCH_FORCED, true),
        do_refresh_dispatch_events()
    end).

-doc """
Undo `enable_dispatch_events/0`. The events stay on while another source
(tracing, a Beamtalk dispatch handler, the app env) still asks for them.
""".
-spec disable_dispatch_events() -> ok.
disable_dispatch_events() ->
    with_dispatch_lock(fun() ->
        _ = persistent_term:erase(?PT_DISPATCH_FORCED),
        do_refresh_dispatch_events()
    end).

-doc """
Recompute the dispatch-events flag from its sources (module doc). Called
after anything that can change one: trace store start and `enable/0` /
`disable/0`, and Beamtalk handler attach/detach. Writes the
`persistent_term` only when the value changes.
""".
-spec refresh_dispatch_events() -> ok.
refresh_dispatch_events() ->
    with_dispatch_lock(fun do_refresh_dispatch_events/0).

-doc """
Emit a `telemetry:execute/3` event if telemetry is loaded, else do nothing.
Moved from `beamtalk_actor:maybe_execute_telemetry/3` (BT-4025), which now
adds only the actor-lifecycle announcement on top; `execute/3` is the
validated Beamtalk-facing form.
""".
-spec emit(list(), map(), map()) -> ok.
emit(EventName, Measurements, Metadata) ->
    case erlang:function_exported(telemetry, execute, 3) of
        true ->
            telemetry:execute(EventName, Measurements, Metadata);
        false ->
            ok
    end.

%%====================================================================
%% Internal: span
%%====================================================================

-doc """
The one span implementation: `telemetry:span/3`'s event shapes
(start/stop/exception, `telemetry_span_context`, `duration`) with a `^`
non-local return treated as a normal exit. `Fun` answers `{Result, StopMeta}`.
""".
-spec run_span(list(), map(), fun(() -> {term(), map()})) -> term().
run_span(EventPrefix, StartMeta, Fun) ->
    case erlang:function_exported(telemetry, execute, 3) of
        true ->
            run_traced_span(EventPrefix, StartMeta, Fun, fun(_Exit) -> ok end);
        false ->
            {Result, _StopMeta} = Fun(),
            Result
    end.

-doc """
`run_span/3` with an exit hook: `OnExit` is called once, after the stop or
exception event, with `{stop, StopMeta, Duration}` or
`{exception, ExceptionMeta, Duration}` (native time units).
`dispatch_span/5` records its aggregate and trace event there.
""".
-spec run_traced_span(list(), map(), fun(() -> {term(), map()}), fun((span_exit()) -> ok)) ->
    term().
run_traced_span(EventPrefix, StartMeta0, Fun, OnExit) ->
    StartTime = erlang:monotonic_time(),
    StartMeta = with_span_context(StartMeta0, erlang:make_ref()),
    Ctx = maps:get(telemetry_span_context, StartMeta),
    telemetry:execute(
        EventPrefix ++ [start],
        #{monotonic_time => StartTime, system_time => erlang:system_time()},
        StartMeta
    ),
    %% bt-catcher-audit: not-applicable-reraises - every clause emits an event and
    %% re-raises the original class/reason/stacktrace (an NLR included)
    try Fun() of
        {Result, StopMeta} ->
            emit_stop(EventPrefix, StartTime, with_span_context(StopMeta, Ctx), OnExit),
            Result
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            %% A `^` out of the block is a normal exit, not an exception.
            emit_stop(EventPrefix, StartTime, StartMeta, OnExit),
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            StopTime = erlang:monotonic_time(),
            Duration = StopTime - StartTime,
            ExcMeta = StartMeta#{kind => Class, reason => Reason, stacktrace => Stack},
            telemetry:execute(
                EventPrefix ++ [exception],
                #{duration => Duration, monotonic_time => StopTime},
                ExcMeta
            ),
            OnExit({exception, ExcMeta, Duration}),
            erlang:raise(Class, Reason, Stack)
    end.

emit_stop(EventPrefix, StartTime, StopMeta, OnExit) ->
    StopTime = erlang:monotonic_time(),
    Duration = StopTime - StartTime,
    telemetry:execute(
        EventPrefix ++ [stop],
        #{duration => Duration, monotonic_time => StopTime},
        StopMeta
    ),
    OnExit({stop, StopMeta, Duration}).

-doc """
Dispatch events off: time `Fun` and record the aggregate, nothing else.
The duration is taken in nanoseconds directly, which is what the aggregate
stores.
""".
run_recorded(Pid, Class, Selector, Mode, Fun) ->
    StartTime = erlang:monotonic_time(nanosecond),
    %% bt-catcher-audit: not-applicable-reraises - every clause records the exit and
    %% re-raises the original class/reason/stacktrace (an NLR included)
    try Fun() of
        {Result, Outcome} ->
            Duration = erlang:monotonic_time(nanosecond) - StartTime,
            beamtalk_trace_store:record_dispatch(Pid, Selector, Duration, Outcome, Mode, Class),
            Result
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            %% A `^` out of the send is a normal exit, not an exception.
            Duration = erlang:monotonic_time(nanosecond) - StartTime,
            beamtalk_trace_store:record_dispatch(Pid, Selector, Duration, ok, Mode, Class),
            erlang:raise(throw, Nlr, Stack);
        ErrClass:Reason:Stack ->
            Duration = erlang:monotonic_time(nanosecond) - StartTime,
            beamtalk_trace_store:record_dispatch(Pid, Selector, Duration, error, Mode, Class),
            erlang:raise(ErrClass, Reason, Stack)
    end.

-doc """
Dispatch events on: the causal IDs trace capture links spans with (only
while tracing; an atomics add), then the full span, recording the aggregate
and trace event from its exit hook.
""".
run_dispatch_events(Metadata, Fun) ->
    case beamtalk_trace_store:is_enabled() of
        true ->
            SpanId = beamtalk_trace_store:next_span_id(),
            %% If no trace_id exists, this is a root span
            case get('$beamtalk_trace_id') of
                undefined ->
                    put('$beamtalk_trace_id', SpanId),
                    put('$beamtalk_span_id', SpanId);
                _ExistingTraceId ->
                    put('$beamtalk_span_id', SpanId)
            end;
        false ->
            ok
    end,
    run_traced_span(
        ?DISPATCH_PREFIX,
        Metadata,
        fun() ->
            {Result, Outcome} = Fun(),
            {Result, Metadata#{outcome => Outcome}}
        end,
        fun record_dispatch_exit/1
    ).

-spec record_dispatch_exit(span_exit()) -> ok.
record_dispatch_exit({stop, StopMeta, Duration}) ->
    beamtalk_trace_store:record_dispatch_stop(StopMeta, Duration);
record_dispatch_exit({exception, ExcMeta, Duration}) ->
    beamtalk_trace_store:record_dispatch_exception(ExcMeta, Duration).

%%====================================================================
%% Internal: dispatch-event flag
%%====================================================================

%% A node-local lock (the requester id is this process, the node list just
%% this node), so concurrent refreshes cannot interleave their reads and
%% the write.
with_dispatch_lock(Fun) ->
    global:trans({?MODULE, self()}, Fun, [node()]).

do_refresh_dispatch_events() ->
    New =
        beamtalk_trace_store:is_enabled() orelse
            persistent_term:get(?PT_DISPATCH_FORCED, false) orelse
            application:get_env(beamtalk_runtime, dispatch_telemetry, false) =:= true orelse
            beamtalk_dispatch_handler_attached(),
    case dispatch_events_enabled() of
        New -> ok;
        _ -> persistent_term:put(?PT_DISPATCH_EVENTS, New)
    end,
    ok.

%% A Beamtalk-attached handler (handle_event/4) on any dispatch event.
beamtalk_dispatch_handler_attached() ->
    Fun = fun ?MODULE:handle_event/4,
    try telemetry:list_handlers(?DISPATCH_PREFIX) of
        Handlers -> lists:any(fun(#{function := F}) -> F =:= Fun end, Handlers)
    catch
        error:undef -> false;
        exit:{noproc, _} -> false
    end.

%% Same rule as telemetry:span/3: a caller-supplied context wins.
with_span_context(#{telemetry_span_context := _} = Meta, _Ctx) -> Meta;
with_span_context(Meta, Ctx) -> Meta#{telemetry_span_context => Ctx}.

%%====================================================================
%% Internal: attach
%%====================================================================

do_attach(HandlerId, EventNames, Block, Sel) ->
    check_handler_id(HandlerId, Sel),
    check_not_reserved(HandlerId, Sel),
    check_block(Block, 3, Sel),
    Result =
        try
            telemetry:attach_many(HandlerId, EventNames, fun ?MODULE:handle_event/4, #{
                block => Block, id => HandlerId
            })
        catch
            error:undef -> {error, telemetry_unavailable};
            exit:{noproc, _} -> {error, telemetry_unavailable}
        end,
    case Result of
        ok ->
            %% BT-4113: a handler on a dispatch event switches the opt-in
            %% dispatch events on.
            ok = refresh_dispatch_events(),
            beamtalk_result:from_tagged_tuple({ok, nil});
        {error, already_exists} ->
            beamtalk_result:from_tagged_tuple(
                {error,
                    error_with_hint(
                        handler_exists,
                        Sel,
                        iolist_to_binary(
                            io_lib:format("A telemetry handler with id ~tp is already attached", [
                                HandlerId
                            ])
                        )
                    )}
            );
        {error, telemetry_unavailable} ->
            beamtalk_result:from_tagged_tuple(
                {error,
                    error_with_hint(
                        telemetry_unavailable,
                        Sel,
                        <<"The telemetry application is not running">>
                    )}
            )
    end.

%%====================================================================
%% Internal: argument checks
%%====================================================================

check_event_name([_ | _] = Name, Sel, What) ->
    case all_atoms(Name) of
        true -> ok;
        false -> event_name_error(Sel, What, Name)
    end;
check_event_name(Name, Sel, What) ->
    event_name_error(Sel, What, Name).

all_atoms([A | Rest]) when is_atom(A) -> all_atoms(Rest);
all_atoms([]) -> true;
all_atoms(_) -> false.

-spec event_name_error(atom(), binary(), term()) -> no_return().
event_name_error(Sel, What, Name) ->
    type_error(Sel, <<What/binary, " must be a non-empty List of Symbols">>, Name).

%% Measurements need atom keys; checked once, without copying the map.
check_measurements(M, Sel) when is_map(M) ->
    case all_atom_keys(maps:next(maps:iterator(M))) of
        true -> ok;
        false -> type_error(Sel, <<"measurements must be a Dictionary with Symbol keys">>, M)
    end;
check_measurements(M, Sel) ->
    type_error(Sel, <<"measurements must be a Dictionary with Symbol keys">>, M).

all_atom_keys(none) -> true;
all_atom_keys({K, _V, Next}) when is_atom(K) -> all_atom_keys(maps:next(Next));
all_atom_keys(_) -> false.

check_metadata(M, _Sel) when is_map(M) -> ok;
check_metadata(M, Sel) -> type_error(Sel, <<"metadata must be a Dictionary">>, M).

check_block(Block, Arity, _Sel) when is_function(Block, Arity) -> ok;
check_block(Block, 0, Sel) ->
    type_error(Sel, <<"the do: argument must be a zero-argument Block">>, Block);
check_block(Block, 3, Sel) ->
    type_error(
        Sel,
        <<"the do: argument must be a three-argument Block [:event :measurements :metadata | ...]">>,
        Block
    ).

check_handler_id(Id, _Sel) when is_atom(Id); is_binary(Id) -> ok;
%% BT-4173: a String handler id arrives tagged.
check_handler_id({beamtalk_string, Id}, _Sel) when is_binary(Id) -> ok;
check_handler_id(Id, Sel) -> type_error(Sel, <<"handlerId must be a Symbol or a String">>, Id).

%% The runtime's own handler ids (`beamtalk_trace_store`'s) cannot be attached
%% or detached from Beamtalk, so the reserved set means the same on both sides.
check_not_reserved(HandlerId, Sel) ->
    case lists:member(HandlerId, beamtalk_trace_store:telemetry_handler_ids()) of
        true ->
            beamtalk_error:raise(
                error_with_hint(
                    reserved_name,
                    Sel,
                    iolist_to_binary(
                        io_lib:format("~tp is a runtime telemetry handler id and is reserved", [
                            HandlerId
                        ])
                    )
                )
            );
        false ->
            ok
    end.

-spec type_error(atom(), binary(), term()) -> no_return().
type_error(Sel, Hint, Got) ->
    beamtalk_error:raise(
        error_with_hint(
            type_error, Sel, iolist_to_binary([Hint, io_lib:format(", got: ~tp", [Got])])
        )
    ).

error_with_hint(Kind, Sel, Hint) ->
    beamtalk_error:with_hint(beamtalk_error:new(Kind, 'Telemetry', Sel), Hint).
