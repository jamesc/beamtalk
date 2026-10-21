%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_telemetry_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for beamtalk_telemetry (BT-4025): the stdlib Telemetry facade's
runtime half (execute/3, span/3 with NLR handling, attach/3, attach_many/3,
detach/1, handle_event/4) and the runtime helpers moved from beamtalk_actor
(emit/3, maybe_span/3).
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
        fun raising_handler_is_detached/0,
        fun emit_returns_ok/0,
        fun maybe_span_runs_fun_and_emits_span/0
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
    ?assertEqual(#{block => Block}, Config),
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
    #{isOk := true} = beamtalk_telemetry:attach_many(
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
    ?assertEqual(type_error, kind_of(fun() -> beamtalk_telemetry:attach_many(x, [], Block) end)),
    ?assertEqual(
        type_error, kind_of(fun() -> beamtalk_telemetry:attach_many(x, [[a], []], Block) end)
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

maybe_span_runs_fun_and_emits_span() ->
    Id = forward([[bt4025, ms, stop]]),
    ?assertEqual(
        result,
        beamtalk_telemetry:maybe_span([bt4025, ms], #{a => 1}, fun() -> {result, #{a => 2}} end)
    ),
    {[bt4025, ms, stop], _, StopMd} = recv(),
    ?assertMatch(#{a := 2, telemetry_span_context := _}, StopMd),
    telemetry:detach(Id).
