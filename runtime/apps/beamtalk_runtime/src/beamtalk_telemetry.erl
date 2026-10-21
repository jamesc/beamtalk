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
  `beamtalk_erlang_proxy:native_call/4` into `execute/3`, `span/3`,
  `attach/3`, `attachMany/3` and `detach/1`. The function names follow the
  `native:` naming rule (the selector's first keyword). These validate their
  arguments and raise structured `#beamtalk_error{}`s. The module opts out of
  the proxy's ok/error → `Result` coercion (`beamtalk_ffi_raw_results/0`):
  `span/3` answers the block's own value, which must come back unchanged.
- **The runtime itself.** `maybe_span/3` instruments actor dispatch and
  `emit/3` emits the actor lifecycle and distribution events. Both moved
  here from `beamtalk_actor` (`maybe_span/3`, and the emit half of
  `maybe_execute_telemetry/3`) so the span logic (start/stop/exception, NLR
  handling) and the guarded emit exist exactly once.

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
    maybe_span/3,
    emit/3
]).

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
    detach_handler(HandlerId).

-doc """
Opt out of `beamtalk_erlang_proxy`'s ok/error → `Result` coercion (ADR 0076)
for the `native_call/4` path. `span/3` answers whatever the user's block
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
Wrap actor dispatch in a telemetry span (ADR 0069 Phase 2a).

`Fun` answers `{Result, StopMetadata}`. When trace capture is enabled this
also generates a span id and sets the trace id (a root span when none exists)
in the process dictionary for causal linking. Moved from `beamtalk_actor`
(BT-4025); the span itself is `run_span/3`, shared with `span/3`.
""".
-spec maybe_span(list(), map(), fun(() -> {term(), map()})) -> term().
maybe_span(EventPrefix, Metadata, Fun) ->
    %% Generate causal trace IDs when tracing is enabled.
    %% Only incurs atomics cost (~10ns) when trace capture is active.
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
    run_span(EventPrefix, Metadata, Fun).

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
            run_traced_span(EventPrefix, StartMeta, Fun);
        false ->
            {Result, _StopMeta} = Fun(),
            Result
    end.

run_traced_span(EventPrefix, StartMeta0, Fun) ->
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
            emit_stop(EventPrefix, StartTime, with_span_context(StopMeta, Ctx)),
            Result
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            %% A `^` out of the block is a normal exit, not an exception.
            emit_stop(EventPrefix, StartTime, StartMeta),
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            StopTime = erlang:monotonic_time(),
            telemetry:execute(
                EventPrefix ++ [exception],
                #{duration => StopTime - StartTime, monotonic_time => StopTime},
                StartMeta#{kind => Class, reason => Reason, stacktrace => Stack}
            ),
            erlang:raise(Class, Reason, Stack)
    end.

emit_stop(EventPrefix, StartTime, StopMeta) ->
    StopTime = erlang:monotonic_time(),
    telemetry:execute(
        EventPrefix ++ [stop],
        #{duration => StopTime - StartTime, monotonic_time => StopTime},
        StopMeta
    ).

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
