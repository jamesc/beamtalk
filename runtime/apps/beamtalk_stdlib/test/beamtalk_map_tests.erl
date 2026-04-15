%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_map_tests).

-moduledoc """
Tests for beamtalk_map helper module and compiled
bt@stdlib@dictionary stdlib dispatch (BT-418).
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% at_if_absent/3
%%% ============================================================================

at_if_absent_found_test() ->
    ?assertEqual(1, beamtalk_map:at_if_absent(#{a => 1}, a, fun() -> 99 end)).

at_if_absent_missing_test() ->
    ?assertEqual(99, beamtalk_map:at_if_absent(#{a => 1}, b, fun() -> 99 end)).

at_if_absent_empty_map_test() ->
    ?assertEqual(42, beamtalk_map:at_if_absent(#{}, x, fun() -> 42 end)).

at_if_absent_block_not_called_when_present_test() ->
    %% Block should NOT be called when key is present
    ?assertEqual(
        1,
        beamtalk_map:at_if_absent(#{a => 1}, a, fun() -> error(should_not_be_called) end)
    ).

%%% ============================================================================
%%% do/2
%%% ============================================================================

do_iterates_values_test() ->
    Self = self(),
    beamtalk_map:do(#{a => 1, b => 2}, fun(V) -> Self ! {value, V} end),
    Values = collect_messages([]),
    ?assertEqual(lists:sort([1, 2]), lists:sort(Values)).

do_empty_map_test() ->
    ?assertEqual(nil, beamtalk_map:do(#{}, fun(_V) -> ok end)).

do_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_map:do(#{a => 1}, fun(_V) -> ok end)).

%%% ============================================================================
%%% do_with_key/2
%%% ============================================================================

do_with_key_iterates_pairs_test() ->
    Self = self(),
    beamtalk_map:do_with_key(#{a => 1, b => 2}, fun(K, V) -> Self ! {pair, K, V} end),
    Pairs = collect_pair_messages([]),
    ?assertEqual(lists:sort([{a, 1}, {b, 2}]), lists:sort(Pairs)).

do_with_key_empty_map_test() ->
    ?assertEqual(nil, beamtalk_map:do_with_key(#{}, fun(_K, _V) -> ok end)).

do_with_key_returns_nil_test() ->
    ?assertEqual(nil, beamtalk_map:do_with_key(#{x => y}, fun(_K, _V) -> ok end)).

%%% ============================================================================
%%% includes/2
%%% ============================================================================

includes_present_value_test() ->
    ?assert(beamtalk_map:includes(#{a => 1, b => 2}, 1)).

includes_absent_value_test() ->
    ?assertNot(beamtalk_map:includes(#{a => 1, b => 2}, 3)).

includes_empty_map_test() ->
    ?assertNot(beamtalk_map:includes(#{}, anything)).

includes_checks_values_not_keys_test() ->
    %% includes/2 checks values, not keys
    ?assertNot(beamtalk_map:includes(#{a => 1}, a)),
    ?assert(beamtalk_map:includes(#{a => 1}, 1)).

%%% ============================================================================
%%% print_string/1
%%% ============================================================================

print_string_empty_map_test() ->
    ?assertEqual(<<"#{}">>, beamtalk_map:print_string(#{})).

print_string_single_entry_test() ->
    Result = beamtalk_map:print_string(#{1 => 2}),
    ?assertEqual(<<"#{1 => 2}">>, Result).

print_string_strips_beamtalk_class_tag_test() ->
    %% $beamtalk_class tag should be stripped from output
    Result = beamtalk_map:print_string(#{'$beamtalk_class' => 'Dictionary', a => 1}),
    %% Should only show the 'a => 1' pair, not $beamtalk_class
    ?assertNotEqual(nomatch, binary:match(Result, <<"a">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"$beamtalk_class">>)).

print_string_sorted_keys_test() ->
    %% Keys should be sorted for deterministic output
    Result = beamtalk_map:print_string(#{b => 2, a => 1}),
    %% 'a' should appear before 'b'
    {PosA, _} = binary:match(Result, <<"a">>),
    {PosB, _} = binary:match(Result, <<"b">>),
    ?assert(PosA < PosB).

%%% ============================================================================
%%% Compiled bt@stdlib@dictionary dispatch/3
%%% ============================================================================

dictionary_class_test() ->
    ?assertEqual('Dictionary', 'bt@stdlib@dictionary':dispatch('class', [], #{a => 1})),
    ?assertEqual('Dictionary', 'bt@stdlib@dictionary':dispatch('class', [], #{})).

dictionary_keys_test() ->
    ?assertEqual([a], 'bt@stdlib@dictionary':dispatch('keys', [], #{a => 1})),
    ?assertEqual([], 'bt@stdlib@dictionary':dispatch('keys', [], #{})).

dictionary_values_test() ->
    ?assertEqual([1], 'bt@stdlib@dictionary':dispatch('values', [], #{a => 1})).

dictionary_size_test() ->
    ?assertEqual(2, 'bt@stdlib@dictionary':dispatch('size', [], #{a => 1, b => 2})),
    ?assertEqual(0, 'bt@stdlib@dictionary':dispatch('size', [], #{})).

dictionary_at_test() ->
    ?assertEqual(1, 'bt@stdlib@dictionary':dispatch('at:', [a], #{a => 1, b => 2})).

dictionary_at_put_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        'bt@stdlib@dictionary':dispatch('at:put:', [b, 2], #{a => 1})
    ).

dictionary_at_if_absent_test() ->
    ?assertEqual(
        1, 'bt@stdlib@dictionary':dispatch('at:ifAbsent:', [a, fun() -> 99 end], #{a => 1})
    ),
    ?assertEqual(
        99, 'bt@stdlib@dictionary':dispatch('at:ifAbsent:', [b, fun() -> 99 end], #{a => 1})
    ).

dictionary_includes_key_test() ->
    ?assertEqual(true, 'bt@stdlib@dictionary':dispatch('includesKey:', [a], #{a => 1})),
    ?assertEqual(false, 'bt@stdlib@dictionary':dispatch('includesKey:', [b], #{a => 1})).

dictionary_remove_key_test() ->
    ?assertEqual(#{}, 'bt@stdlib@dictionary':dispatch('removeKey:', [a], #{a => 1})).

dictionary_merge_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        'bt@stdlib@dictionary':dispatch('merge:', [#{b => 2}], #{a => 1})
    ).

dictionary_keys_and_values_do_test() ->
    ?assertEqual(
        nil,
        'bt@stdlib@dictionary':dispatch('keysAndValuesDo:', [fun(_, _) -> ok end], #{a => 1})
    ).

%%% ============================================================================
%%% Compiled bt@stdlib@dictionary has_method/1
%%% ============================================================================

dictionary_responds_to_test() ->
    beamtalk_extensions:init(),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('class')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('keys')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('values')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('size')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:put:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('at:ifAbsent:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('includesKey:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('removeKey:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('merge:')),
    ?assertEqual(true, 'bt@stdlib@dictionary':has_method('keysAndValuesDo:')),
    ?assertEqual(false, 'bt@stdlib@dictionary':has_method('nonExistent')).

%%% ============================================================================
%%% Does Not Understand
%%% ============================================================================

dictionary_does_not_understand_test() ->
    beamtalk_extensions:init(),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = does_not_understand, class = 'Dictionary'}
        },
        'bt@stdlib@dictionary':dispatch('nonExistent', [], #{a => 1})
    ).

%%% ============================================================================
%%% Error paths
%%% ============================================================================

dictionary_at_missing_key_test() ->
    %% maps:get raises {badkey, Key} for missing keys — same as pre-migration
    ?assertError(
        {badkey, missing},
        'bt@stdlib@dictionary':dispatch('at:', [missing], #{a => 1})
    ).

%%% ============================================================================
%%% beamtalk_primitive:send integration tests
%%% ============================================================================

primitive_send_size_test() ->
    ?assertEqual(2, beamtalk_primitive:send(#{a => 1, b => 2}, 'size', [])),
    ?assertEqual(0, beamtalk_primitive:send(#{}, 'size', [])).

primitive_send_keys_test() ->
    ?assertEqual([a], beamtalk_primitive:send(#{a => 1}, 'keys', [])).

primitive_send_at_test() ->
    ?assertEqual(1, beamtalk_primitive:send(#{a => 1}, 'at:', [a])).

primitive_send_at_put_test() ->
    ?assertEqual(
        #{a => 1, b => 2},
        beamtalk_primitive:send(#{a => 1}, 'at:put:', [b, 2])
    ).

primitive_send_includes_key_test() ->
    ?assertEqual(true, beamtalk_primitive:send(#{a => 1}, 'includesKey:', [a])),
    ?assertEqual(false, beamtalk_primitive:send(#{a => 1}, 'includesKey:', [b])).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

collect_messages(Acc) ->
    receive
        {value, V} -> collect_messages([V | Acc])
    after 100 -> lists:reverse(Acc)
    end.

collect_pair_messages(Acc) ->
    receive
        {pair, K, V} -> collect_pair_messages([{K, V} | Acc])
    after 100 -> lists:reverse(Acc)
    end.
