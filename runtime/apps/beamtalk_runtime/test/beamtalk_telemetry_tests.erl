%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_telemetry_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for beamtalk_telemetry (BT-4025): the stdlib Telemetry facade's
runtime half (execute/3, span/3 with NLR handling, attach/3, attachMany/3,
detach/1, handle_event/4) and the runtime helpers moved from beamtalk_actor
(emit/3, dispatch_span/5), plus the BT-4113 dispatch-event opt-in flag.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([forward_event/4]).

%%====================================================================
%% Fixture
%%====================================================================

telemetry_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        fun execute_passes_name_and_maps_unchanged/0,
        fun execute_answers_nil/0,
        fun execute_rejects_bad_event_names/0,
        fun execute_rejects_non_atom_measurement_keys/0,
        fun execute_rejects_non_map_metadata/0,
        fun span_emits_start_and_stop_and_answers_value/0,
        fun span_exception_emits_exception_and_reraises_unchanged/0,
        fun span_nlr_emits_stop_not_exception/0,
        fun span_keeps_caller_span_context/0,
        fun span_rejects_bad_arguments/0,
        fun attach_calls_block_through_handle_event/0,
        fun attach_stores_block_in_config_with_external_handler/0,
        fun attach_duplicate_is_handler_exists/0,
        fun attach_many_covers_every_event/0,
        fun attach_rejects_bad_arguments/0,
        fun detach_is_idempotent/0,
        fun detach_rejects_bad_and_reserved_ids/0,
        fun handler_does_not_reenter_itself/0,
        fun dispatch_span_nlr_emits_stop/0,
        fun raising_handler_is_detached/0,
        fun emit_returns_ok/0,
        fun dispatch_span_emits_span_with_outcome_when_enabled/0,
        fun dispatch_span_emits_nothing_by_default/0,
        fun dispatch_span_reraises_unchanged_when_disabled/0,
        fun dispatch_events_follow_erlang_api/0,
        fun dispatch_events_follow_app_env/0,
        fun dispatch_events_follow_beamtalk_dispatch_handlers/0,
        fun non_dispatch_attach_and_detach_skip_refresh/0,
        fun dispatch_span_survives_missing_trace_table/0
    ]}.

setup() ->
    _ = application:ensure_all_started(telemetry),
    %% function_exported/3 is false until the module is loaded.
    {module, telemetry} = code:ensure_loaded(telemetry),
    ok.

cleanup(_) ->
    ok.

%% Attach an Erlang handler forwarding every event to the test process.
forward(Events) ->
    Id = {?MODULE, make_ref()},
    Self = self(),
    ok = telemetry:attach_many(Id, Events, fun ?MODULE:forward_event/4, #{dest => Self}),
    Id.

forward_event(Event, Measurements, Metadata, #{dest := Dest}) ->
    Dest ! {event, Event, Measurements, Metadata}.

recv() ->
    receive
        {event, E, M, Md} -> {E, M, Md}
    after 1000 -> timeout
    end.

no_more() ->
    receive
        {event, _, _, _} = Msg -> {unexpected, Msg}
    after 0 -> ok
    end.

kind_of(Fun) ->
    try
        Fun(),
        no_error
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind
    end.

%%====================================================================
%% execute/3
%%====================================================================

execute_passes_name_and_maps_unchanged() ->
    Id = forward([[bt4025, exec]]),
    Meta = #{key => <<"k">>, <<"str">> => 1, 7 => [x]},
    nil = beamtalk_telemetry:execute([bt4025, exec], #{count => 3}, Meta),
    ?assertEqual({[bt4025, exec], #{count => 3}, Meta}, recv()),
    telemetry:detach(Id).

execute_answers_nil() ->
    ?assertEqual(nil, beamtalk_telemetry:execute([bt4025, nobody], #{}, #{})).

execute_rejects_bad_event_names() ->
    [
        ?assertEqual(
            type_error,
            kind_of(fun() -> beamtalk_telemetry:execute(Bad, #{}, #{}) end)
        )
     || Bad <- [[], [a, <<"b">>], a, <<"a.b">>, nil]
    ].

execute_rejects_non_atom_measurement_keys() ->
    ?assertEqual(
        type_error,
        kind_of(fun() -> beamtalk_telemetry:execute([a], #{<<"count">> => 1}, #{}) end)
    ),
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:execute([a], [], #{}) end)).

execute_rejects_non_map_metadata() ->
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:execute([a], #{}, 3) end)).

%%====================================================================
%% span/3
%%====================================================================

span_emits_start_and_stop_and_answers_value() ->
    Id = forward([[bt4025, sp, start], [bt4025, sp, stop], [bt4025, sp, exception]]),
    ?assertEqual(42, beamtalk_telemetry:span([bt4025, sp], #{job => 1}, fun() -> 42 end)),
    {[bt4025, sp, start], StartM, StartMd} = recv(),
    {[bt4025, sp, stop], StopM, StopMd} = recv(),
    ?assertMatch(#{monotonic_time := _, system_time := _}, StartM),
    ?assertMatch(#{monotonic_time := _, duration := D} when D >= 0, StopM),
    #{telemetry_span_context := Ctx, job := 1} = StartMd,
    ?assert(is_reference(Ctx)),
    ?assertEqual(StartMd, StopMd),
    ?assertEqual(ok, no_more()),
    telemetry:detach(Id).

span_exception_emits_exception_and_reraises_unchanged() ->
    Id = forward([[bt4025, ex, stop], [bt4025, ex, exception]]),
    Raise = fun() -> erlang:raise(exit, {timeout, here}, [{m, f, 0, []}]) end,
    ?assertEqual(
        {exit, {timeout, here}, [{m, f, 0, []}]},
        try
            beamtalk_telemetry:span([bt4025, ex], #{job => 2}, Raise)
        catch
            C:R:S -> {C, R, S}
        end
    ),
    {[bt4025, ex, exception], M, Md} = recv(),
    ?assertMatch(#{duration := _, monotonic_time := _}, M),
    ?assertMatch(
        #{
            kind := exit,
            reason := {timeout, here},
            stacktrace := [{m, f, 0, []}],
            job := 2,
            telemetry_span_context := _
        },
        Md
    ),
    ?assertEqual(ok, no_more()),
    telemetry:detach(Id).

span_nlr_emits_stop_not_exception() ->
    Id = forward([[bt4025, nlr, stop], [bt4025, nlr, exception]]),
    Token = make_ref(),
    Nlr = {'$bt_nlr', Token, 99, #{}},
    ?assertEqual(
        {thrown, Nlr},
        try
            beamtalk_telemetry:span([bt4025, nlr], #{}, fun() -> throw(Nlr) end)
        catch
            throw:T -> {thrown, T}
        end
    ),
    ?assertMatch({[bt4025, nlr, stop], #{duration := _}, _}, recv()),
    ?assertEqual(ok, no_more()),
    telemetry:detach(Id).

span_keeps_caller_span_context() ->
    Id = forward([[bt4025, ctx, start], [bt4025, ctx, stop]]),
    beamtalk_telemetry:span([bt4025, ctx], #{telemetry_span_context => mine}, fun() -> ok end),
    ?assertMatch({_, _, #{telemetry_span_context := mine}}, recv()),
    ?assertMatch({_, _, #{telemetry_span_context := mine}}, recv()),
    telemetry:detach(Id).

span_rejects_bad_arguments() ->
    Ok = fun() -> 1 end,
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:span([], #{}, Ok) end)),
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:span([a], x, Ok) end)),
    ?assertEqual(
        type_error, kind_of(fun() -> beamtalk_telemetry:span([a], #{}, fun(_) -> 1 end) end)
    ).

%%====================================================================
%% attach / detach
%%====================================================================

attach_calls_block_through_handle_event() ->
    Self = self(),
    Block = fun(E, M, Md) -> Self ! {block, E, M, Md} end,
    ?assertMatch(#{isOk := true}, beamtalk_telemetry:attach(bt4025_block, [bt4025, b], Block)),
    beamtalk_telemetry:execute([bt4025, b], #{n => 1}, #{who => me}),
    receive
        Msg -> ?assertEqual({block, [bt4025, b], #{n => 1}, #{who => me}}, Msg)
    after 1000 -> ?assert(false)
    end,
    nil = beamtalk_telemetry:detach(bt4025_block).

attach_stores_block_in_config_with_external_handler() ->
    Block = fun(_, _, _) -> ok end,
    #{isOk := true} = beamtalk_telemetry:attach(<<"bt4025-config">>, [bt4025, cfg], Block),
    [Handler] = [
        H
     || #{id := <<"bt4025-config">>} = H <- telemetry:list_handlers([bt4025, cfg])
    ],
    #{function := Fun, config := Config} = Handler,
    ?assertEqual({type, external}, erlang:fun_info(Fun, type)),
    ?assertEqual(fun beamtalk_telemetry:handle_event/4, Fun),
    ?assertEqual(#{block => Block, id => <<"bt4025-config">>}, Config),
    nil = beamtalk_telemetry:detach(<<"bt4025-config">>).

attach_duplicate_is_handler_exists() ->
    Block = fun(_, _, _) -> ok end,
    #{isOk := true} = beamtalk_telemetry:attach(bt4025_dup, [bt4025, dup], Block),
    Second = beamtalk_telemetry:attach(bt4025_dup, [bt4025, dup], Block),
    ?assertMatch(#{isOk := false}, Second),
    #{errReason := #{error := #beamtalk_error{kind = Kind}}} = Second,
    ?assertEqual(handler_exists, Kind),
    nil = beamtalk_telemetry:detach(bt4025_dup).

attach_many_covers_every_event() ->
    Self = self(),
    Block = fun(E, _, _) -> Self ! {many, E} end,
    #{isOk := true} = beamtalk_telemetry:attachMany(
        bt4025_many, [[bt4025, m1], [bt4025, m2]], Block
    ),
    beamtalk_telemetry:execute([bt4025, m1], #{}, #{}),
    beamtalk_telemetry:execute([bt4025, m2], #{}, #{}),
    ?assertEqual([{many, [bt4025, m1]}, {many, [bt4025, m2]}], [
        receive
            {many, _} = M -> M
        after 1000 -> timeout
        end
     || _ <- [1, 2]
    ]),
    nil = beamtalk_telemetry:detach(bt4025_many).

attach_rejects_bad_arguments() ->
    Block = fun(_, _, _) -> ok end,
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:attach(42, [a], Block) end)),
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:attach(x, [], Block) end)),
    ?assertEqual(
        type_error, kind_of(fun() -> beamtalk_telemetry:attach(x, [a], fun() -> ok end) end)
    ),
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:attachMany(x, [], Block) end)),
    ?assertEqual(
        type_error, kind_of(fun() -> beamtalk_telemetry:attachMany(x, [[a], []], Block) end)
    ).

detach_is_idempotent() ->
    #{isOk := true} = beamtalk_telemetry:attach(bt4025_det, [bt4025, det], fun(_, _, _) -> ok end),
    ?assertEqual(nil, beamtalk_telemetry:detach(bt4025_det)),
    ?assertEqual(nil, beamtalk_telemetry:detach(bt4025_det)),
    ?assertEqual(nil, beamtalk_telemetry:detach(never_attached)).

raising_handler_is_detached() ->
    Self = self(),
    Block = fun(_, _, _) ->
        Self ! raised,
        error(handler_boom)
    end,
    #{isOk := true} = beamtalk_telemetry:attach(bt4025_bad, [bt4025, bad], Block),
    %% telemetry logs the failure; silence it for the test.
    logger:add_primary_filter(bt4025_quiet, {fun(_, _) -> stop end, []}),
    try
        ?assertEqual(nil, beamtalk_telemetry:execute([bt4025, bad], #{}, #{})),
        ?assertEqual(nil, beamtalk_telemetry:execute([bt4025, bad], #{}, #{}))
    after
        logger:remove_primary_filter(bt4025_quiet)
    end,
    receive
        raised -> ok
    after 1000 -> ?assert(false)
    end,
    receive
        raised -> ?assert(false)
    after 0 -> ok
    end,
    ?assertEqual([], telemetry:list_handlers([bt4025, bad])).

%%====================================================================
%% Runtime helpers
%%====================================================================

emit_returns_ok() ->
    ?assertEqual(ok, beamtalk_telemetry:emit([bt4025, emit], #{}, #{})).

-define(DISPATCH_EVENTS, [
    [beamtalk, actor, dispatch, start],
    [beamtalk, actor, dispatch, stop],
    [beamtalk, actor, dispatch, exception]
]).

%% Run Fun with dispatch events forced on through the Erlang API.
with_dispatch_events(Fun) ->
    ok = beamtalk_telemetry:enable_dispatch_events(),
    try
        Fun()
    after
        ok = beamtalk_telemetry:disable_dispatch_events()
    end.

dispatch_span_emits_span_with_outcome_when_enabled() ->
    with_dispatch_events(fun() ->
        Id = forward(?DISPATCH_EVENTS),
        Pid = self(),
        ?assertEqual(
            result,
            beamtalk_telemetry:dispatch_span(Pid, 'Counter', count, sync, fun() -> {result, ok} end)
        ),
        {[beamtalk, actor, dispatch, start], _, StartMd} = recv(),
        ?assertMatch(
            #{pid := Pid, class := 'Counter', selector := count, mode := sync}, StartMd
        ),
        {[beamtalk, actor, dispatch, stop], #{duration := D}, StopMd} = recv(),
        ?assert(D >= 0),
        ?assertMatch(
            #{pid := Pid, class := 'Counter', outcome := ok, telemetry_span_context := _},
            StopMd
        ),
        ?assertEqual(ok, no_more()),
        telemetry:detach(Id)
    end).

dispatch_span_nlr_emits_stop() ->
    %% A `^` crossing an actor send is a normal exit: a stop, not an exception.
    with_dispatch_events(fun() ->
        Id = forward(?DISPATCH_EVENTS),
        Nlr = {'$bt_nlr', make_ref(), 1, #{}},
        ?assertEqual(
            {thrown, Nlr},
            try
                beamtalk_telemetry:dispatch_span(self(), 'C', s, sync, fun() -> throw(Nlr) end)
            catch
                throw:T -> {thrown, T}
            end
        ),
        ?assertMatch({[beamtalk, actor, dispatch, start], _, _}, recv()),
        ?assertMatch({[beamtalk, actor, dispatch, stop], _, _}, recv()),
        ?assertEqual(ok, no_more()),
        telemetry:detach(Id)
    end).

dispatch_span_emits_nothing_by_default() ->
    %% BT-4113: dispatch events are opt-in; with no source enabled a send
    %% builds no metadata and emits nothing.
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    Id = forward(?DISPATCH_EVENTS),
    ?assertEqual(
        value, beamtalk_telemetry:dispatch_span(self(), 'C', s, cast, fun() -> {value, cast} end)
    ),
    ?assertEqual(ok, no_more()),
    telemetry:detach(Id).

dispatch_span_reraises_unchanged_when_disabled() ->
    Raise = fun() -> erlang:raise(exit, {noproc, here}, [{m, f, 0, []}]) end,
    ?assertEqual(
        {exit, {noproc, here}, [{m, f, 0, []}]},
        try
            beamtalk_telemetry:dispatch_span(self(), 'C', s, sync, Raise)
        catch
            C:R:S -> {C, R, S}
        end
    ).

dispatch_events_follow_erlang_api() ->
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    ok = beamtalk_telemetry:enable_dispatch_events(),
    ?assert(beamtalk_telemetry:dispatch_events_enabled()),
    %% A refresh keeps an explicit enable.
    ok = beamtalk_telemetry:refresh_dispatch_events(),
    ?assert(beamtalk_telemetry:dispatch_events_enabled()),
    ok = beamtalk_telemetry:disable_dispatch_events(),
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()).

dispatch_events_follow_app_env() ->
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    application:set_env(beamtalk_runtime, dispatch_telemetry, true),
    try
        ok = beamtalk_telemetry:refresh_dispatch_events(),
        ?assert(beamtalk_telemetry:dispatch_events_enabled())
    after
        application:unset_env(beamtalk_runtime, dispatch_telemetry)
    end,
    ok = beamtalk_telemetry:refresh_dispatch_events(),
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()).

dispatch_events_follow_beamtalk_dispatch_handlers() ->
    Block = fun(_, _, _) -> ok end,
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    %% A handler on some other event leaves them off.
    #{isOk := true} = beamtalk_telemetry:attach(bt4113_other, [bt4113, other], Block),
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    %% One on a dispatch event (attach:to:do:) turns them on; a second
    %% (attach:toAll:do:) keeps them on until the last one detaches.
    #{isOk := true} = beamtalk_telemetry:attach(
        bt4113_stop, [beamtalk, actor, dispatch, stop], Block
    ),
    ?assert(beamtalk_telemetry:dispatch_events_enabled()),
    #{isOk := true} = beamtalk_telemetry:attachMany(
        bt4113_many, [[bt4113, x], [beamtalk, actor, dispatch, exception]], Block
    ),
    nil = beamtalk_telemetry:detach(bt4113_stop),
    ?assert(beamtalk_telemetry:dispatch_events_enabled()),
    nil = beamtalk_telemetry:detach(bt4113_many),
    ?assertNot(beamtalk_telemetry:dispatch_events_enabled()),
    nil = beamtalk_telemetry:detach(bt4113_other).

non_dispatch_attach_and_detach_skip_refresh() ->
    %% Attach/detach refresh the flag only for dispatch events (BT-4113): a
    %% stale flag survives an unrelated attach/detach and is fixed by the
    %% next dispatch-handler change.
    Block = fun(_, _, _) -> ok end,
    persistent_term:put(beamtalk_dispatch_events, true),
    try
        #{isOk := true} = beamtalk_telemetry:attach(bt4113_skip, [bt4113, skip], Block),
        ?assert(beamtalk_telemetry:dispatch_events_enabled()),
        nil = beamtalk_telemetry:detach(bt4113_skip),
        ?assert(beamtalk_telemetry:dispatch_events_enabled()),
        #{isOk := true} = beamtalk_telemetry:attach(
            bt4113_skip, [beamtalk, actor, dispatch, start], Block
        ),
        nil = beamtalk_telemetry:detach(bt4113_skip),
        ?assertNot(beamtalk_telemetry:dispatch_events_enabled())
    after
        ok = beamtalk_telemetry:refresh_dispatch_events()
    end.

dispatch_span_survives_missing_trace_table() ->
    %% Recording runs inline in the sender (BT-4113), so a recording failure
    %% must not change the send's outcome: with events and tracing on and the
    %% trace-events table missing, a send still answers its value and a
    %% raising send still surfaces its own error. A trace store left running
    %% by an earlier suite owns the table: drop it for the test (it is
    %% public) and hand a fresh one back afterwards.
    HadTable = ets:whereis(beamtalk_trace_events) =/= undefined,
    HadTable andalso ets:delete(beamtalk_trace_events),
    ?assertEqual(undefined, ets:whereis(beamtalk_trace_events)),
    OldEnabled = persistent_term:get(beamtalk_tracing_enabled, undefined),
    HadCounter = persistent_term:get(beamtalk_span_id_counter, undefined) =/= undefined,
    HadCounter orelse persistent_term:put(beamtalk_span_id_counter, atomics:new(1, [])),
    persistent_term:put(beamtalk_tracing_enabled, true),
    try
        with_dispatch_events(fun() ->
            ?assertEqual(
                result,
                beamtalk_telemetry:dispatch_span(self(), 'C', s, sync, fun() -> {result, ok} end)
            ),
            Raise = fun() -> erlang:raise(error, {own, error}, [{m, f, 0, []}]) end,
            ?assertEqual(
                {error, {own, error}, [{m, f, 0, []}]},
                try
                    beamtalk_telemetry:dispatch_span(self(), 'C', s, sync, Raise)
                catch
                    C:R:S -> {C, R, S}
                end
            )
        end)
    after
        case OldEnabled of
            undefined -> persistent_term:erase(beamtalk_tracing_enabled);
            _ -> persistent_term:put(beamtalk_tracing_enabled, OldEnabled)
        end,
        HadCounter orelse persistent_term:erase(beamtalk_span_id_counter),
        erase('$beamtalk_trace_id'),
        erase('$beamtalk_span_id'),
        HadTable andalso restore_trace_events_table()
    end.

%% Recreate the trace-events table the way the trace store does and give it
%% back to the running trace store (its 'ETS-TRANSFER' clause accepts it).
restore_trace_events_table() ->
    ok = beamtalk_ets_heir:ensure_table(
        beamtalk_trace_events,
        [ordered_set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]
    ),
    case whereis(beamtalk_trace_store) of
        undefined -> true;
        Pid -> ets:give_away(beamtalk_trace_events, Pid, undefined)
    end.

detach_rejects_bad_and_reserved_ids() ->
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:detach(42) end)),
    [
        begin
            ?assertEqual(reserved_name, kind_of(fun() -> beamtalk_telemetry:detach(Reserved) end)),
            ?assertEqual(
                reserved_name,
                kind_of(fun() ->
                    beamtalk_telemetry:attach(Reserved, [a], fun(_, _, _) -> ok end)
                end)
            ),
            ?assertEqual(
                reserved_name,
                kind_of(fun() ->
                    beamtalk_telemetry:attachMany(Reserved, [[a]], fun(_, _, _) -> ok end)
                end)
            )
        end
     || Reserved <- beamtalk_trace_store:telemetry_handler_ids()
    ],
    ?assertEqual(nil, beamtalk_telemetry:detach_handler({not_a_symbol, 42})).

handler_does_not_reenter_itself() ->
    %% A handler whose body emits the event it listens to runs once per
    %% outer emit instead of recursing.
    Self = self(),
    Block = fun(_, _, _) ->
        Self ! ran,
        beamtalk_telemetry:execute([bt4025, reenter], #{}, #{})
    end,
    #{isOk := true} = beamtalk_telemetry:attach(bt4025_reenter, [bt4025, reenter], Block),
    nil = beamtalk_telemetry:execute([bt4025, reenter], #{}, #{}),
    ?assertEqual(1, count_ran(0)),
    %% still attached, and the guard was cleared
    ?assertMatch([_], telemetry:list_handlers([bt4025, reenter])),
    ?assertEqual(undefined, get({'$bt_telemetry_in_handler', bt4025_reenter})),
    nil = beamtalk_telemetry:detach(bt4025_reenter).

count_ran(N) ->
    receive
        ran -> count_ran(N + 1)
    after 50 -> N
    end.
