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
    %% Every element kind the local guard covers. A miss here must walk the
    %% whole list and answer false, never attempting a dispatch.
    Elements = [1, 2.5, atom, <<"bin">>, [nested], fun() -> ok end, make_ref()],
    ?assertNot(beamtalk_equality:member(missing, Elements)).

member_untagged_map_is_not_an_object_test() ->
    %% Plain maps carry no '$beamtalk_class' key, so they are primitives:
    %% raw equality decides, and a miss must not dispatch.
    ?assert(beamtalk_equality:member(#{a => 1}, [#{a => 1}])),
    ?assertNot(beamtalk_equality:member(#{a => 2}, [#{a => 1}])).

member_tuple_element_is_not_dispatched_test() ->
    %% Tuples are excluded from the local guard because #beamtalk_object{} is a
    %% record — but a plain tuple still fails is_object/1 and must not dispatch.
    ?assert(beamtalk_equality:member({a, 1}, [{a, 1}])),
    ?assertNot(beamtalk_equality:member({a, 2}, [{a, 1}])).
