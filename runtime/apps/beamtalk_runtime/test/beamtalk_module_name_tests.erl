%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context / Shared Kernel

-module(beamtalk_module_name_tests).

-moduledoc """
EUnit tests for beamtalk_module_name.

Covers the forward CamelCase→snake_case conversion (`camel_to_snake/1`),
the `bt@…` module-atom assembly helpers, `is_stdlib_module/1`, and the lossy
`snake_to_class/1` inverse.
""".

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% camel_to_snake/1 — cross-language conformance fixtures
%%%
%%% Single source of truth: shared JSON corpus at
%%% runtime/apps/beamtalk_runtime/test/fixtures/camel_to_snake_conformance.json,
%%% also used by the Rust-side test in crates/beamtalk-core/src/ast/mod.rs
%%% (`to_module_name_conformance_fixtures`). Adding or changing a case means
%%% editing the JSON only — both sides pick it up automatically.
%%% ============================================================================

camel_to_snake_conformance_test_() ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_runtime",
        "test",
        "fixtures",
        "camel_to_snake_conformance.json"
    ]),
    [
        {lists:flatten(io_lib:format("~ts -> ~ts", [Input, Expected])), fun() ->
            ?assertEqual(Expected, beamtalk_module_name:camel_to_snake(Input))
        end}
     || #{<<"input">> := InputBin, <<"expected">> := ExpectedBin} <- Cases,
        Input <- [unicode:characters_to_list(InputBin)],
        Expected <- [unicode:characters_to_list(ExpectedBin)]
    ].

%% Regression: beamtalk_repl_loader:to_snake_case/1 (copy D) used to
%% force-lowercase the first character unconditionally, discarding whether it
%% actually started lowercase — so a lowercase-initial name immediately
%% followed by an uppercase letter (no lowercase letter in between to
%% "correct" the seed) lost its underscore. "aB" is the minimal repro.
camel_to_snake_lowercase_initial_regression_test() ->
    ?assertEqual("a_b", beamtalk_module_name:camel_to_snake("aB")).

%%% ============================================================================
%%% to_module_atom/1, to_stdlib_module_atom/1
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
    %% BEAMError is the documented case-fold collision fixture —
    %% forward conversion is not lossy (only the inverse is).
    ?assertEqual(
        'bt@stdlib@beamerror', beamtalk_module_name:to_stdlib_module_atom('BEAMError')
    ).

%%% ============================================================================
%%% to_qualified_module_atom/2
%%% ============================================================================

to_qualified_module_atom_found_test() ->
    %% 'bt@bt3081test@counter' must already exist as an atom for the
    %% existing-atom-only lookup to find it -- create it via to_module_atom-
    %% style construction (never through to_qualified_module_atom itself,
    %% which must not be what creates the atom under test).
    % elp:fixme W0023 intentional atom creation
    _ = list_to_atom("bt@bt3081test@counter"),
    ?assertEqual(
        'bt@bt3081test@counter',
        beamtalk_module_name:to_qualified_module_atom("counter", <<"bt3081test">>)
    ).

to_qualified_module_atom_binary_and_string_package_agree_test() ->
    % elp:fixme W0023 intentional atom creation
    _ = list_to_atom("bt@bt3081agree@widget"),
    ?assertEqual(
        beamtalk_module_name:to_qualified_module_atom("widget", <<"bt3081agree">>),
        beamtalk_module_name:to_qualified_module_atom("widget", "bt3081agree")
    ).

to_qualified_module_atom_never_creates_atom_test() ->
    %% A snake/package combo that has never been turned into an atom in this
    %% VM must resolve to undefined, not silently mint a new atom.
    Unique = erlang:unique_integer([positive]),
    Snake = "bt3108_never_created_" ++ integer_to_list(Unique),
    ?assertEqual(
        undefined,
        beamtalk_module_name:to_qualified_module_atom(Snake, <<"nopkg">>)
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
