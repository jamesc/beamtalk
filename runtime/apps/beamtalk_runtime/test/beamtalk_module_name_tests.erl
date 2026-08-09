%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context / Shared Kernel

-module(beamtalk_module_name_tests).

-moduledoc """
EUnit tests for beamtalk_module_name (BT-3081).

Covers the forward CamelCase→snake_case conversion (`camel_to_snake/1`),
the `bt@…` module-atom assembly helpers, `is_stdlib_module/1`, and the lossy
`snake_to_class/1` inverse.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% camel_to_snake/1 — cross-language conformance fixtures (BT-3081)
%%%
%%% Kept byte-identical to the Rust fixture
%%% (MODULE_NAME_CONFORMANCE_FIXTURES in crates/beamtalk-core/src/ast/mod.rs,
%%% asserted against `to_module_name` there) — the single Rust authority this
%%% module mirrors. If either list changes, update both so the two
%%% implementations stay provably in sync on the same inputs, including the
%%% acronym case-fold collision (BEAMError/Beamerror) and the
%%% lowercase-initial + Unicode cases that previously drifted between the
%%% four now-deleted Erlang copies of this conversion (copies A-D in the
%%% BT-3081 issue).
%%% ============================================================================

-define(CONFORMANCE_FIXTURES, [
    {"Counter", "counter"},
    {"MyCounterActor", "my_counter_actor"},
    {"HTTPRouter", "httprouter"},
    {"BEAMError", "beamerror"},
    {"Beamerror", "beamerror"},
    {"myClass", "my_class"},
    {"aB", "a_b"},
    {"already_snake", "already_snake"},
    {"App2", "app2"},
    {"ABC", "abc"},
    {"", ""},
    {"École", "école"},
    {"MonÉcole", "mon_école"}
]).

camel_to_snake_conformance_test_() ->
    [
        {lists:flatten(io_lib:format("~ts -> ~ts", [Input, Expected])), fun() ->
            ?assertEqual(Expected, beamtalk_module_name:camel_to_snake(Input))
        end}
     || {Input, Expected} <- ?CONFORMANCE_FIXTURES
    ].

%% BT-3081 regression: beamtalk_repl_loader:to_snake_case/1 (copy D) used to
%% force-lowercase the first character unconditionally, discarding whether it
%% actually started lowercase — so a lowercase-initial name immediately
%% followed by an uppercase letter (no lowercase letter in between to
%% "correct" the seed) lost its underscore. "aB" is the minimal repro.
camel_to_snake_lowercase_initial_regression_test() ->
    ?assertEqual("a_b", beamtalk_module_name:camel_to_snake("aB")).

%%% ============================================================================
%%% to_module_atom/1, to_stdlib_module_atom/1, to_package_module_atom/2
%%% ============================================================================

to_module_atom_test() ->
    ?assertEqual('bt@counter', beamtalk_module_name:to_module_atom('Counter')).

to_module_atom_multi_word_test() ->
    ?assertEqual(
        'bt@my_counter_actor', beamtalk_module_name:to_module_atom('MyCounterActor')
    ).

to_stdlib_module_atom_test() ->
    ?assertEqual(
        'bt@stdlib@integer', beamtalk_module_name:to_stdlib_module_atom('Integer')
    ).

to_stdlib_module_atom_acronym_test() ->
    %% BT-3081: BEAMError is the documented case-fold collision fixture —
    %% forward conversion is not lossy (only the inverse is).
    ?assertEqual(
        'bt@stdlib@beamerror', beamtalk_module_name:to_stdlib_module_atom('BEAMError')
    ).

to_package_module_atom_string_test() ->
    ?assertEqual(
        'bt@json@parser', beamtalk_module_name:to_package_module_atom('Parser', "json")
    ).

to_package_module_atom_atom_test() ->
    ?assertEqual(
        'bt@utils@my_class', beamtalk_module_name:to_package_module_atom('MyClass', utils)
    ).

to_package_module_atom_binary_test() ->
    ?assertEqual(
        'bt@json@parser', beamtalk_module_name:to_package_module_atom('Parser', <<"json">>)
    ).

%%% ============================================================================
%%% is_stdlib_module/1
%%% ============================================================================

is_stdlib_module_true_test() ->
    ?assert(beamtalk_module_name:is_stdlib_module('bt@stdlib@integer')).

is_stdlib_module_false_test() ->
    ?assertNot(beamtalk_module_name:is_stdlib_module('bt@mypackage@my_class')).

is_stdlib_module_non_atom_test() ->
    ?assertNot(beamtalk_module_name:is_stdlib_module(<<"bt@stdlib@integer">>)).

%%% ============================================================================
%%% snake_to_class/1 — lossy inverse (fallback only)
%%% ============================================================================

snake_to_class_single_word_test() ->
    ?assertEqual('Integer', beamtalk_module_name:snake_to_class("integer")).

snake_to_class_multi_word_test() ->
    ?assertEqual(
        'MyCounterActor', beamtalk_module_name:snake_to_class("my_counter_actor")
    ).

%% The documented lossy case: BEAMError's module is bt@stdlib@beamerror, but
%% the naive inverse can only guess 'Beamerror', never recover the original
%% acronym casing. This is exactly why module_to_class/1 tries the live class
%% registry first (see beamtalk_stack_frame_tests for the registry-first
%% regression test).
snake_to_class_lossy_for_acronyms_test() ->
    ?assertEqual('Beamerror', beamtalk_module_name:snake_to_class("beamerror")).

snake_to_class_unknown_atom_test() ->
    ?assertEqual(nil, beamtalk_module_name:snake_to_class("zzz_q123_unique_never_an_atom")).
