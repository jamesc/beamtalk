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
    %% Actor references render via printString. A live actor ref renders
    %% kind-headed and positional as `Actor(ClassName, pid)`, so a Value
    %% holding one recurses to that form. The pid is dynamic, so assert the
    %% stable prefix and closing parens around it.
    Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
    Fields = [{target, Obj}],
    Result = beamtalk_object_printer:structural('Holder', Fields),
    ?assertMatch(<<"Holder(target: Actor(Counter, ", _/binary>>, Result),
    ?assertEqual(<<"))">>, binary:part(Result, byte_size(Result) - 2, 2)).

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

width_resets_per_nesting_level_test() ->
    %% Verify that nested objects get a fresh width budget, not the parent's
    %% remaining count. With width=2, the outer has 3 fields (a, b, c) but
    %% the nested object should still be able to render its own 2 fields.
    Inner = #{'$beamtalk_class' => 'Point', x => 1, y => 2},
    Fields = [{a, Inner}, {b, 42}, {c, 99}],
    Result = beamtalk_object_printer:structural('Outer', Fields, #{width => 2}),
    %% Outer shows a + b, then elides c. But Inner gets its own budget of 2,
    %% so Point shows both x and y.
    ?assertEqual(<<"Outer(a: Point(x: 1, y: 2), b: 42, ...)">>, Result).

%%% ============================================================================
%%% structural_from_state/1 — extract class + user fields from a tagged map
%%% ============================================================================

structural_from_state_flat_test() ->
    %% Class name and user fields are extracted from the tagged map; internal
    %% keys (e.g. '$beamtalk_class') are filtered out.
    State = #{'$beamtalk_class' => 'Point', x => 3, y => 4},
    ?assertEqual(
        <<"Point(x: 3, y: 4)">>,
        beamtalk_object_printer:structural_from_state(State)
    ).

structural_from_state_empty_fields_test() ->
    %% A tagged map with no user fields renders ClassName().
    State = #{'$beamtalk_class' => 'Unit'},
    ?assertEqual(
        <<"Unit()">>,
        beamtalk_object_printer:structural_from_state(State)
    ).

structural_from_state_defaults_to_object_test() ->
    %% A map without a class tag defaults to 'Object'.
    ?assertEqual(
        <<"Object()">>,
        beamtalk_object_printer:structural_from_state(#{})
    ).

structural_from_state_nested_test() ->
    %% Nested tagged-map field values recurse structurally.
    Point = #{'$beamtalk_class' => 'Point', x => 0, y => 0},
    State = #{'$beamtalk_class' => 'Line', from => Point, label => <<"a">>},
    ?assertEqual(
        <<"Line(from: Point(x: 0, y: 0), label: \"a\")">>,
        beamtalk_object_printer:structural_from_state(State)
    ).

%%% ============================================================================
%%% structural_from_state/1 — declared-`late`, unassigned slot (ADR 0124 §9/B8)
%%% ============================================================================

%% These need real `__beamtalk_meta/0` field-kind metadata behind a
%% *registered* class process — `classAllFieldKindsByName/1` (B5b) is a
%% hierarchy walk keyed on live class pids, not on the field list handed to
%% `structural/2` directly, so a bare `#{'$beamtalk_class' => ...}` map (as
%% every other test above uses) always degrades to "no late fields" and
%% never exercises `unassigned_late_fields/2`. Mirrors
%% `beamtalk_object_class_tests.erl`'s `get_class_var_declared_late_
%% unassigned_errors_without_crashing_test_` — a real meta-carrying module
%% built via `compile:forms/2`, registered as a live class with
%% `beamtalk_object_class:start_link/1`.

setup() ->
    case whereis(pg) of
        undefined ->
            {ok, Pid} = pg:start_link(),
            Pid;
        Pid ->
            Pid
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table().

teardown(_) ->
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,
    lists:foreach(
        fun(Pid) ->
            catch gen_server:stop(Pid, normal, 1000)
        end,
        Members
    ).

%% Registers `ClassName` as a live class whose `__beamtalk_meta/0` declares
%% `field_kinds => FieldKinds` — the B5b metadata `unassigned_late_fields/2`
%% reads. `ModAtom` must be unique per test (a live, loaded Erlang module).
register_class_with_field_kinds(ClassName, ModAtom, FieldKinds) ->
    Meta = #{field_kinds => FieldKinds},
    MetaAbstract = erl_parse:abstract(Meta, [{line, 3}]),
    Forms = [
        {attribute, 1, module, ModAtom},
        {attribute, 2, export, [{'__beamtalk_meta', 0}]},
        {function, 3, '__beamtalk_meta', 0, [{clause, 3, [], [], [MetaAbstract]}]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, atom_to_list(ModAtom) ++ ".erl", Bin),
    ClassInfo = #{
        name => ClassName,
        module => Mod,
        superclass => 'Object',
        instance_methods => #{},
        class_methods => #{}
    },
    {ok, _Pid} = beamtalk_object_class:start_link(ClassInfo),
    Mod.

declared_late_unassigned_renders_placeholder_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                _ = register_class_with_field_kinds(
                    'BT3557LateUnassigned',
                    bt3557_late_unassigned_mod,
                    #{proc => late, count => eager}
                ),
                %% `proc` is declared `late` and absent — renders `<unassigned>`.
                %% `count` is present — renders its value normally.
                State = #{'$beamtalk_class' => 'BT3557LateUnassigned', count => 5},
                ?assertEqual(
                    <<"BT3557LateUnassigned(count: 5, proc: <unassigned>)">>,
                    beamtalk_object_printer:structural_from_state(State)
                )
            end)
        ]
    end}.

declared_late_assigned_renders_value_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                _ = register_class_with_field_kinds(
                    'BT3557LateAssigned',
                    bt3557_late_assigned_mod,
                    #{proc => late, count => eager}
                ),
                %% Once assigned, `proc` is a present key — renders its real
                %% value, the same as any other field, not `<unassigned>`.
                State = #{'$beamtalk_class' => 'BT3557LateAssigned', count => 5, proc => 42},
                ?assertEqual(
                    <<"BT3557LateAssigned(count: 5, proc: 42)">>,
                    beamtalk_object_printer:structural_from_state(State)
                )
            end)
        ]
    end}.

declared_late_cleared_field_renders_placeholder_again_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                _ = register_class_with_field_kinds(
                    'BT3557LateCleared',
                    bt3557_late_cleared_mod,
                    #{proc => late}
                ),
                Assigned = #{'$beamtalk_class' => 'BT3557LateCleared', proc => 42},
                ?assertEqual(
                    <<"BT3557LateCleared(proc: 42)">>,
                    beamtalk_object_printer:structural_from_state(Assigned)
                ),
                %% `clearField:` is `maps:remove/2` (`beamtalk_reflection:
                %% clear_field/2`) — simulate its effect directly and confirm
                %% the slot renders `<unassigned>` again, exactly as before
                %% the first assignment.
                Cleared = beamtalk_reflection:clear_field(proc, Assigned),
                ?assertEqual(
                    <<"BT3557LateCleared(proc: <unassigned>)">>,
                    beamtalk_object_printer:structural_from_state(Cleared)
                )
            end)
        ]
    end}.

declared_late_unassigned_nested_in_value_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                _ = register_class_with_field_kinds(
                    'BT3557LateNested',
                    bt3557_late_nested_mod,
                    #{proc => late}
                ),
                %% render_value/5's nested-tagged-map branch shares
                %% fields_for_state/2 with structural_from_state/1, so an
                %% unassigned `late` field nested inside another value's
                %% field also renders `<unassigned>`, not just at the top level.
                Inner = #{'$beamtalk_class' => 'BT3557LateNested'},
                ?assertEqual(
                    <<"Wrapper(value: BT3557LateNested(proc: <unassigned>))">>,
                    beamtalk_object_printer:structural('Wrapper', [{value, Inner}])
                )
            end)
        ]
    end}.

no_late_fields_declared_behaves_as_before_test_() ->
    %% A class with no `late` fields at all (the common case) is unaffected —
    %% `unassigned_late_fields/2` contributes nothing, same output as before
    %% this ADR 0124 §9/B8 change.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                _ = register_class_with_field_kinds(
                    'BT3557AllEager',
                    bt3557_all_eager_mod,
                    #{count => eager}
                ),
                State = #{'$beamtalk_class' => 'BT3557AllEager', count => 5},
                ?assertEqual(
                    <<"BT3557AllEager(count: 5)">>,
                    beamtalk_object_printer:structural_from_state(State)
                )
            end)
        ]
    end}.
