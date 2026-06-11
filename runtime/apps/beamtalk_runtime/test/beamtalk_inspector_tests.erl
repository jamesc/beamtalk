%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_inspector_tests).

-moduledoc """
Unit tests for `beamtalk_inspector` — the ADR 0095 Phase 1 inspector shim.

Coverage:
- kind classification (`#value` for a tagged Value map, `#actor` for a pid)
- `#value` field derivation in ADR-0094 sort order, with drillability
- `at:` drilling into a child cursor (parent/path wired) and the
  `no_such_field` error Result on a miss
- `refresh` returning a fresh cursor (immutable original)
- the shared `beamtalk_process_navigation:guarded_state/1` guard: a live
  `sys`-compliant process yields `{ok, State}`; a dead pid yields `unavailable`
- an `#actor` over a live gen_server snapshots its state; a dead actor degrades
  to the single `#status => #unavailable` diagnostic field (never a crash)
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% A minimal sys-compliant gen_server whose state is a tagged Value-shaped map,
%% so the inspector's actor snapshot path can read user fields.
-export([init/1, handle_call/3, handle_cast/2]).

%%====================================================================
%% Test gen_server
%%====================================================================

init(State) -> {ok, State}.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

%% Start a sys-compliant process whose state is the given tagged map.
start_actor(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, State, []),
    Pid.

%% A Value-shaped tagged map (Point with x/y).
point(X, Y) ->
    #{'$beamtalk_class' => 'Point', x => X, y => Y}.

%%====================================================================
%% Kind classification
%%====================================================================

value_kind_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    ?assertEqual(value, beamtalk_inspector:kindOf(I)).

actor_kind_test() ->
    Pid = start_actor(point(1, 2)),
    I = beamtalk_inspector:on(Pid),
    ?assertEqual(actor, beamtalk_inspector:kindOf(I)),
    gen_server:stop(Pid).

root_cursor_has_no_parent_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    ?assertEqual(nil, beamtalk_inspector:parentOf(I)),
    ?assertEqual([], beamtalk_inspector:pathOf(I)).

%%====================================================================
%% #value field derivation (ADR 0094 sort order)
%%====================================================================

value_fields_sorted_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    Fields = beamtalk_inspector:fieldsOf(I),
    Names = [maps:get(name, F) || F <- Fields],
    ?assertEqual([x, y], Names).

value_field_values_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF, YF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(3, maps:get(value, XF)),
    ?assertEqual(4, maps:get(value, YF)).

scalar_field_not_drillable_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF | _] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(false, maps:get(drillable, XF)).

nested_value_field_drillable_test() ->
    Nested = #{'$beamtalk_class' => 'Pair', a => point(1, 2), b => 9},
    I = beamtalk_inspector:on(Nested),
    Fields = beamtalk_inspector:fieldsOf(I),
    [AF] = [F || F <- Fields, maps:get(name, F) =:= a],
    ?assertEqual(true, maps:get(drillable, AF)).

field_records_tagged_inspectorfield_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    [XF | _] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual('InspectorField', maps:get('$beamtalk_class', XF)),
    ?assertEqual(slot, maps:get(kind, XF)).

%%====================================================================
%% Navigation: at: / parent / path
%%====================================================================

%% `inspector/2` returns the raw `{ok, Child} | {error, Err}` tuple; the FFI
%% boundary (`coerce_result/1`) lifts it to a Beamtalk `Result` at the call site.
at_drills_into_child_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {ok, Child} = beamtalk_inspector:inspector(I, x),
    ?assertEqual(3, beamtalk_inspector:subjectOf(Child)),
    ?assertEqual([x], beamtalk_inspector:pathOf(Child)).

at_child_parent_is_cursor_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {ok, Child} = beamtalk_inspector:inspector(I, x),
    ?assertEqual(I, beamtalk_inspector:parentOf(Child)).

at_miss_returns_no_such_field_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    {error, Err} = beamtalk_inspector:inspector(I, nonesuch),
    ?assertEqual(no_such_field, Err#beamtalk_error.kind).

%%====================================================================
%% Refresh
%%====================================================================

refresh_value_returns_cursor_test() ->
    I = beamtalk_inspector:on(point(3, 4)),
    R = beamtalk_inspector:refresh(I),
    ?assertEqual(value, beamtalk_inspector:kindOf(R)),
    ?assertEqual(point(3, 4), beamtalk_inspector:subjectOf(R)).

%%====================================================================
%% Shared guarded_state/1 (ADR 0095 §4)
%%====================================================================

guarded_state_live_test() ->
    Pid = start_actor(point(5, 6)),
    ?assertEqual({ok, point(5, 6)}, beamtalk_process_navigation:guarded_state(Pid)),
    gen_server:stop(Pid).

guarded_state_dead_test() ->
    Pid = start_actor(point(5, 6)),
    gen_server:stop(Pid),
    ?assertEqual(unavailable, beamtalk_process_navigation:guarded_state(Pid)).

guarded_state_non_pid_test() ->
    ?assertEqual(unavailable, beamtalk_process_navigation:guarded_state(not_a_pid)).

%%====================================================================
%% #actor snapshot + unavailable degradation
%%====================================================================

actor_fields_from_snapshot_test() ->
    Pid = start_actor(point(7, 8)),
    I = beamtalk_inspector:on(Pid),
    Fields = beamtalk_inspector:fieldsOf(I),
    Names = lists:sort([maps:get(name, F) || F <- Fields]),
    ?assertEqual([x, y], Names),
    gen_server:stop(Pid).

dead_actor_fields_unavailable_test() ->
    Pid = start_actor(point(7, 8)),
    gen_server:stop(Pid),
    %% Inspecting a dead actor must not crash — single status field.
    I = beamtalk_inspector:on(Pid),
    [StatusF] = beamtalk_inspector:fieldsOf(I),
    ?assertEqual(status, maps:get(name, StatusF)),
    ?assertEqual(unavailable, maps:get(value, StatusF)),
    ?assertEqual(false, maps:get(drillable, StatusF)).
