%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_equality_tests).

-moduledoc """
EUnit tests for beamtalk_equality (BT-2997).

Covers the parts that need no compiled stdlib: the raw `=:=` fast path, the
non-object skipping in the fallback scan, and edge cases. End-to-end dispatch
to a real `equals:` override is covered by `overridable_equality_test.bt`,
which needs a loaded stdlib class to override.
""".
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% eq/2 — raw fast path
%%% ============================================================================

eq_identical_primitives_test() ->
    ?assert(beamtalk_equality:eq(42, 42)),
    ?assert(beamtalk_equality:eq(<<"ab">>, <<"ab">>)),
    ?assert(beamtalk_equality:eq(foo, foo)),
    ?assert(beamtalk_equality:eq([1, 2], [1, 2])).

eq_is_strict_not_coercing_test() ->
    %% Matches `=:=`, not `==` — 1 and 1.0 are different values.
    ?assertNot(beamtalk_equality:eq(1, 1.0)),
    ?assertNot(beamtalk_equality:eq(1.0, 1)).

eq_distinct_primitives_test() ->
    ?assertNot(beamtalk_equality:eq(42, 43)),
    ?assertNot(beamtalk_equality:eq(<<"ab">>, <<"ba">>)),
    ?assertNot(beamtalk_equality:eq(foo, bar)).

eq_non_object_receiver_never_dispatches_test() ->
    %% A non-object left operand takes the bare path even on a miss; if it
    %% dispatched, this would raise rather than answer false.
    ?assertNot(beamtalk_equality:eq(42, #{a => 1})),
    ?assertNot(beamtalk_equality:eq([1, 2, 3], 42)).

%%% ============================================================================
%%% member/2
%%% ============================================================================

member_finds_present_element_test() ->
    ?assert(beamtalk_equality:member(2, [1, 2, 3])),
    ?assert(beamtalk_equality:member(1, [1, 2, 3])),
    ?assert(beamtalk_equality:member(3, [1, 2, 3])).

member_absent_element_test() ->
    ?assertNot(beamtalk_equality:member(9, [1, 2, 3])).

member_is_strict_test() ->
    %% 2.0 must not match the integer 2 — same as `lists:member/2`.
    ?assertNot(beamtalk_equality:member(2.0, [1, 2, 3])).

member_empty_list_test() ->
    ?assertNot(beamtalk_equality:member(1, [])).

member_skips_non_objects_without_dispatch_test() ->
    %% A miss over assorted non-dispatchable element kinds must walk the whole
    %% list and answer false, never attempting a dispatch.
    Elements = [1, 2.5, atom, <<"bin">>, [nested], fun() -> ok end, make_ref()],
    ?assertNot(beamtalk_equality:member(missing, Elements)).

member_untagged_map_is_not_an_object_test() ->
    %% Plain maps carry no '$beamtalk_class' key, so they are primitives:
    %% raw equality decides, and a miss must not dispatch.
    ?assert(beamtalk_equality:member(#{a => 1}, [#{a => 1}])),
    ?assertNot(beamtalk_equality:member(#{a => 2}, [#{a => 1}])).

member_tuple_element_is_not_dispatched_test() ->
    %% A plain tuple is not a tagged map, so it is not dispatchable and must be
    %% compared raw.
    ?assert(beamtalk_equality:member({a, 1}, [{a, 1}])),
    ?assertNot(beamtalk_equality:member({a, 2}, [{a, 1}])).

%%% ============================================================================
%%% Actor references answer by identity, never by dispatch
%%% ============================================================================

member_does_not_dispatch_to_pids_test() ->
    %% `beamtalk_primitive:is_object/1` accepts live actor pids, so dispatching
    %% on them would send a synchronous message per element. `dispatchable/1` is
    %% narrower. A bare spawned pid is not a Beamtalk actor, but the point holds
    %% for any pid: a miss must answer promptly rather than send anything.
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assert(beamtalk_equality:member(Pid, [Pid])),
    ?assertNot(beamtalk_equality:member(Other, [Pid])),
    ?assert(beamtalk_equality:eq(Pid, Pid)),
    ?assertNot(beamtalk_equality:eq(Pid, Other)),
    Pid ! stop,
    Other ! stop.

member_does_not_dispatch_to_beamtalk_object_records_test() ->
    %% `#beamtalk_object{}` is an actor reference (class, class_mod, pid), so it
    %% is excluded for the same reason as a bare pid.
    Ref = {beamtalk_object, 'Counter', 'counter', self()},
    ?assert(beamtalk_equality:member(Ref, [Ref])),
    ?assertNot(
        beamtalk_equality:member({beamtalk_object, 'Counter', 'counter', spawn(fun() -> ok end)}, [
            Ref
        ])
    ).

untagged_map_is_not_dispatchable_test() ->
    %% Only maps carrying '$beamtalk_class' are asked.
    ?assertNot(beamtalk_equality:member(#{a => 2}, [#{a => 1}])),
    ?assert(beamtalk_equality:member(#{a => 1}, [#{a => 1}])).
