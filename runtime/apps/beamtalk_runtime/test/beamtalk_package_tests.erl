%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

%%% @doc EUnit tests for beamtalk_package module.
%%%
%%% Tests package reflection: enumeration, lookup, reverse-lookup,
%%% class listing, and dependency queries.

-module(beamtalk_package_tests).

-include("beamtalk.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% ============================================================================
%%% Test setup — ensure beamtalk_stdlib app metadata is loaded
%%% ============================================================================

setup() ->
    _ = application:load(beamtalk_stdlib),
    _ = application:load(beamtalk_runtime),
    ok.

%%% ============================================================================
%%% all/0 tests
%%% ============================================================================

all_returns_list_test() ->
    setup(),
    Result = beamtalk_package:all(),
    ?assert(is_list(Result)).

all_includes_stdlib_test() ->
    setup(),
    Result = beamtalk_package:all(),
    ?assert(lists:member(<<"stdlib">>, Result)).

all_excludes_non_package_apps_test() ->
    setup(),
    Result = beamtalk_package:all(),
    %% kernel and stdlib (Erlang's) are not Beamtalk packages
    ?assertNot(lists:member(<<"kernel">>, Result)),
    ?assertNot(lists:member(<<"stdlib_erlang">>, Result)).

all_returns_binaries_test() ->
    setup(),
    Result = beamtalk_package:all(),
    lists:foreach(
        fun(Name) -> ?assert(is_binary(Name)) end,
        Result
    ).

%%% ============================================================================
%%% named/1 tests
%%% ============================================================================

named_returns_tagged_package_map_test() ->
    setup(),
    Info = beamtalk_package:named(<<"stdlib">>),
    ?assert(is_map(Info)),
    ?assertEqual('Package', maps:get('$beamtalk_class', Info)),
    ?assertEqual(<<"stdlib">>, maps:get(name, Info)),
    ?assert(is_binary(maps:get(version, Info))),
    ?assert(is_list(maps:get('packageClasses', Info))),
    ?assert(is_list(maps:get('packageDependencies', Info))),
    ?assert(is_binary(maps:get(source, Info))).

named_accepts_atom_test() ->
    setup(),
    Info = beamtalk_package:named(stdlib),
    ?assertEqual(<<"stdlib">>, maps:get(name, Info)).

named_classes_contains_known_classes_test() ->
    setup(),
    Info = beamtalk_package:named(<<"stdlib">>),
    Classes = maps:get('packageClasses', Info),
    ?assert(lists:member('Object', Classes)),
    ?assert(lists:member('Integer', Classes)),
    ?assert(lists:member('String', Classes)),
    ?assert(lists:member('Array', Classes)).

named_raises_error_for_unknown_package_test() ->
    setup(),
    ?assertError(
        #{error := #beamtalk_error{kind = package_not_found}},
        beamtalk_package:named(<<"nonexistent_package_xyz">>)
    ).

named_raises_error_for_unknown_atom_test() ->
    setup(),
    ?assertError(
        #{error := #beamtalk_error{kind = package_not_found}},
        beamtalk_package:named(nonexistent_package_xyz)
    ).

named_error_includes_hint_with_loaded_packages_test() ->
    setup(),
    try
        beamtalk_package:named(<<"nonexistent_package_xyz">>),
        ?assert(false)
    catch
        error:#{error := #beamtalk_error{hint = Hint}} ->
            ?assertNotEqual(undefined, Hint),
            ?assert(is_binary(Hint)),
            %% Hint should mention loaded packages
            ?assertMatch({match, _}, re:run(Hint, <<"stdlib">>))
    end.

%%% ============================================================================
%%% package_name/1 tests
%%% ============================================================================

package_name_returns_stdlib_for_stdlib_class_test() ->
    setup(),
    %% Object is a stdlib class — should return <<"stdlib">>
    Result = beamtalk_package:package_name('Object'),
    ?assertEqual(<<"stdlib">>, Result).

package_name_returns_nil_for_unknown_class_test() ->
    setup(),
    Result = beamtalk_package:package_name('CompletelyFakeClass_BT1656'),
    ?assertEqual(nil, Result).

package_name_returns_stdlib_for_integer_test() ->
    setup(),
    Result = beamtalk_package:package_name('Integer'),
    ?assertEqual(<<"stdlib">>, Result).

%%% ============================================================================
%%% classes/1 tests
%%% ============================================================================

classes_returns_list_for_stdlib_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

classes_accepts_atom_test() ->
    setup(),
    Result = beamtalk_package:classes(stdlib),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

classes_contains_known_classes_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    ?assert(lists:member('Object', Result)),
    ?assert(lists:member('Array', Result)),
    ?assert(lists:member('Dictionary', Result)).

classes_returns_empty_for_unknown_package_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"nonexistent_package_xyz">>),
    ?assertEqual([], Result).

classes_returns_atoms_test() ->
    setup(),
    Result = beamtalk_package:classes(<<"stdlib">>),
    lists:foreach(
        fun(Name) -> ?assert(is_atom(Name)) end,
        Result
    ).

%%% ============================================================================
%%% dependencies/1 tests
%%% ============================================================================

dependencies_returns_list_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"stdlib">>),
    ?assert(is_list(Result)).

dependencies_accepts_atom_test() ->
    setup(),
    Result = beamtalk_package:dependencies(stdlib),
    ?assert(is_list(Result)).

dependencies_returns_empty_for_unknown_package_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"nonexistent_package_xyz">>),
    ?assertEqual([], Result).

dependencies_returns_binaries_test() ->
    setup(),
    Result = beamtalk_package:dependencies(<<"stdlib">>),
    lists:foreach(
        fun(Name) -> ?assert(is_binary(Name)) end,
        Result
    ).
