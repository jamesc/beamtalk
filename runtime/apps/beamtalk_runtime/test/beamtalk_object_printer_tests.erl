%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_printer_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_object_printer — the canonical structural renderer.

Covers: flat value, nested value, empty-field value, depth-cap elision,
width-cap elision, total-length-cap elision, and cycle-guard elision.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Flat value tests
%%% ============================================================================

flat_integer_fields_test() ->
    Fields = [{x, 3}, {y, 4}],
    ?assertEqual(
        <<"Point(x: 3, y: 4)">>,
        beamtalk_object_printer:structural('Point', Fields)
    ).

flat_string_fields_test() ->
    %% Strings should be quoted (Debug/printString form).
    Fields = [{sender, <<"Alice">>}, {text, <<"Hi">>}],
    ?assertEqual(
        <<"Message(sender: \"Alice\", text: \"Hi\")">>,
        beamtalk_object_printer:structural('Message', Fields)
    ).

flat_mixed_fields_test() ->
    Fields = [{active, true}, {count, 42}, {name, <<"Bob">>}],
    ?assertEqual(
        <<"Config(active: true, count: 42, name: \"Bob\")">>,
        beamtalk_object_printer:structural('Config', Fields)
    ).

flat_symbol_field_test() ->
    Fields = [{mode, fast}],
    ?assertEqual(
        <<"Setting(mode: #fast)">>,
        beamtalk_object_printer:structural('Setting', Fields)
    ).

%%% ============================================================================
%%% Sorted field order
%%% ============================================================================

fields_are_sorted_test() ->
    %% Provide fields out of order; output must be sorted.
    Fields = [{z, 3}, {a, 1}, {m, 2}],
    ?assertEqual(
        <<"Triple(a: 1, m: 2, z: 3)">>,
        beamtalk_object_printer:structural('Triple', Fields)
    ).

%%% ============================================================================
%%% Empty-field (no fields) test
%%% ============================================================================

empty_fields_test() ->
    ?assertEqual(
        <<"Unit()">>,
        beamtalk_object_printer:structural('Unit', [])
    ).

%%% ============================================================================
%%% Nested value test
%%% ============================================================================

nested_value_test() ->
    %% Simulate Line(from: Point(x: 0, y: 0), to: Point(x: 3, y: 4))
    %% using tagged maps (Value instances).
    Point1 = #{'$beamtalk_class' => 'Point', x => 0, y => 0},
    Point2 = #{'$beamtalk_class' => 'Point', x => 3, y => 4},
    Fields = [{from, Point1}, {to, Point2}],
    ?assertEqual(
        <<"Line(from: Point(x: 0, y: 0), to: Point(x: 3, y: 4))">>,
        beamtalk_object_printer:structural('Line', Fields)
    ).

double_nested_value_test() ->
    %% Inner → middle → outer nesting.
    Inner = #{'$beamtalk_class' => 'Point', x => 1, y => 2},
    Middle = #{'$beamtalk_class' => 'Wrapper', value => Inner},
    Fields = [{wrapped, Middle}],
    ?assertEqual(
        <<"Box(wrapped: Wrapper(value: Point(x: 1, y: 2)))">>,
        beamtalk_object_printer:structural('Box', Fields)
    ).

%%% ============================================================================
%%% Depth-cap elision
%%% ============================================================================

depth_cap_elision_test() ->
    %% With depth=1, nested tagged maps should be elided.
    Inner = #{'$beamtalk_class' => 'Point', x => 1, y => 2},
    Fields = [{value, Inner}],
    %% depth=1 means we can render the outer level but inner is at depth 0 → elided.
    ?assertEqual(
        <<"Wrapper(value: Point(...))">>,
        beamtalk_object_printer:structural('Wrapper', Fields, #{depth => 1})
    ).

depth_cap_at_zero_test() ->
    %% depth=0 should elide even the top-level fields.
    Fields = [{x, 1}],
    ?assertEqual(
        <<"Point(...)">>,
        beamtalk_object_printer:structural('Point', Fields, #{depth => 0})
    ).

%%% ============================================================================
%%% Width-cap elision
%%% ============================================================================

width_cap_elision_test() ->
    %% width=2 with 4 fields: first 2 shown, then elision.
    Fields = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    ?assertEqual(
        <<"Wide(a: 1, b: 2, ...)">>,
        beamtalk_object_printer:structural('Wide', Fields, #{width => 2})
    ).

width_cap_exact_test() ->
    %% width=3 with exactly 3 fields: no elision.
    Fields = [{a, 1}, {b, 2}, {c, 3}],
    ?assertEqual(
        <<"Exact(a: 1, b: 2, c: 3)">>,
        beamtalk_object_printer:structural('Exact', Fields, #{width => 3})
    ).

%%% ============================================================================
%%% Total-length-cap elision
%%% ============================================================================

length_cap_elision_test() ->
    %% Very short length cap forces truncation.
    Fields = [{name, <<"a very long string value that should be truncated">>}],
    Result = beamtalk_object_printer:structural('Big', Fields, #{length => 20}),
    %% Result should be at most 20 bytes + "..." suffix.
    ?assert(byte_size(Result) =< 23),
    %% Should end with "..."
    Len = byte_size(Result),
    Suffix = binary:part(Result, Len - 3, 3),
    ?assertEqual(<<"...">>, Suffix).

%%% ============================================================================
%%% Cycle-guard elision
%%% ============================================================================

cycle_guard_self_referencing_map_test() ->
    %% Simulate a "cycle" by having two tagged maps with the same phash2.
    %% In practice, a Value referencing itself (impossible with immutable Values,
    %% but the guard protects against it). We test by nesting the same map twice.
    Shared = #{'$beamtalk_class' => 'Node', value => 42},
    %% Nesting the same map at two positions — phash2 is identical, so the second
    %% occurrence triggers the cycle guard.
    Fields = [{left, Shared}, {right, Shared}],
    Result = beamtalk_object_printer:structural('Tree', Fields),
    %% One of them will render fully, the other will be elided as "...".
    %% Since fields are sorted (left, right), left renders first.
    ?assertEqual(
        <<"Tree(left: Node(value: 42), right: ...)">>,
        Result
    ).

%%% ============================================================================
%%% Actor/object reference rendering
%%% ============================================================================

actor_reference_in_field_test() ->
    %% Actor references use printString (opaque).
    Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
    Fields = [{target, Obj}],
    Result = beamtalk_object_printer:structural('Holder', Fields),
    %% Should contain "a Counter" (current printString for non-class objects).
    ?assertMatch(<<"Holder(target: a Counter)">>, Result).

%%% ============================================================================
%%% Nil and boolean field values
%%% ============================================================================

nil_field_test() ->
    Fields = [{value, nil}],
    ?assertEqual(
        <<"Box(value: nil)">>,
        beamtalk_object_printer:structural('Box', Fields)
    ).

boolean_fields_test() ->
    Fields = [{enabled, true}, {visible, false}],
    ?assertEqual(
        <<"Flags(enabled: true, visible: false)">>,
        beamtalk_object_printer:structural('Flags', Fields)
    ).

%%% ============================================================================
%%% Float field value
%%% ============================================================================

float_field_test() ->
    Fields = [{x, 3.14}],
    Result = beamtalk_object_printer:structural('Measure', Fields),
    %% Float rendering varies slightly; just check the structure.
    ?assertMatch(<<"Measure(x: ", _/binary>>, Result).

%%% ============================================================================
%%% Options passthrough
%%% ============================================================================

custom_depth_and_width_test() ->
    %% Deep nesting with custom depth=2, width=1.
    Inner = #{'$beamtalk_class' => 'Point', x => 0, y => 0},
    Middle = #{'$beamtalk_class' => 'Wrapper', value => Inner},
    Fields = [{a, Middle}, {b, 42}],
    %% width=1 → only first field (a) shown, b elided.
    %% depth=2 → Middle renders but Inner (at depth 0) is elided.
    Result = beamtalk_object_printer:structural('Outer', Fields, #{depth => 2, width => 1}),
    ?assertEqual(<<"Outer(a: Wrapper(value: Point(...)), ...)">>, Result).
