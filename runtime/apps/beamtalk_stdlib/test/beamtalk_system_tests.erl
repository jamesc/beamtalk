%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_system module.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests getEnv:/1, getEnv:default:/2, osPlatform/0, osFamily/0,
%%% architecture/0, hostname/0, erlangVersion/0, pid/0,
%%% has_method/1, and type error paths.

-module(beamtalk_system_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% getEnv:/1
%%% ============================================================================

get_env_set_variable_test() ->
    os:putenv("BT_TEST_ENV_VAR", "hello"),
    ?assertEqual(<<"hello">>, beamtalk_system:'getEnv:'(<<"BT_TEST_ENV_VAR">>)),
    os:unsetenv("BT_TEST_ENV_VAR").

get_env_unset_variable_returns_nil_test() ->
    os:unsetenv("BT_TEST_UNSET_VAR"),
    ?assertEqual(nil, beamtalk_system:'getEnv:'(<<"BT_TEST_UNSET_VAR">>)).

get_env_non_binary_name_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_system:'getEnv:'(not_a_string)
    ).

%%% ============================================================================
%%% getEnv:default:/2
%%% ============================================================================

get_env_default_set_variable_test() ->
    os:putenv("BT_TEST_ENV_VAR2", "world"),
    ?assertEqual(
        <<"world">>,
        beamtalk_system:'getEnv:default:'(<<"BT_TEST_ENV_VAR2">>, <<"fallback">>)
    ),
    os:unsetenv("BT_TEST_ENV_VAR2").

get_env_default_unset_returns_default_test() ->
    os:unsetenv("BT_TEST_UNSET_VAR2"),
    ?assertEqual(
        <<"fallback">>,
        beamtalk_system:'getEnv:default:'(<<"BT_TEST_UNSET_VAR2">>, <<"fallback">>)
    ).

get_env_default_non_binary_name_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_system:'getEnv:default:'(42, <<"fallback">>)
    ).

get_env_default_non_binary_default_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = type_error}},
        beamtalk_system:'getEnv:default:'(<<"NAME">>, 42)
    ).

%%% ============================================================================
%%% osPlatform/0
%%% ============================================================================

os_platform_returns_binary_test() ->
    V = beamtalk_system:osPlatform(),
    ?assert(is_binary(V)).

os_platform_non_empty_test() ->
    V = beamtalk_system:osPlatform(),
    ?assert(byte_size(V) > 0).

%%% ============================================================================
%%% osFamily/0
%%% ============================================================================

os_family_returns_binary_test() ->
    V = beamtalk_system:osFamily(),
    ?assert(is_binary(V)).

os_family_known_value_test() ->
    V = beamtalk_system:osFamily(),
    ?assert(V =:= <<"unix">> orelse V =:= <<"win32">>).

%%% ============================================================================
%%% architecture/0
%%% ============================================================================

architecture_returns_binary_test() ->
    V = beamtalk_system:architecture(),
    ?assert(is_binary(V)).

architecture_non_empty_test() ->
    V = beamtalk_system:architecture(),
    ?assert(byte_size(V) > 0).

%%% ============================================================================
%%% hostname/0
%%% ============================================================================

hostname_returns_binary_test() ->
    V = beamtalk_system:hostname(),
    ?assert(is_binary(V)).

hostname_non_empty_test() ->
    V = beamtalk_system:hostname(),
    ?assert(byte_size(V) > 0).

%%% ============================================================================
%%% erlangVersion/0
%%% ============================================================================

erlang_version_returns_binary_test() ->
    V = beamtalk_system:erlangVersion(),
    ?assert(is_binary(V)).

erlang_version_non_empty_test() ->
    V = beamtalk_system:erlangVersion(),
    ?assert(byte_size(V) > 0).

%%% ============================================================================
%%% pid/0
%%% ============================================================================

pid_returns_integer_test() ->
    V = beamtalk_system:pid(),
    ?assert(is_integer(V)).

pid_positive_test() ->
    V = beamtalk_system:pid(),
    ?assert(V > 0).

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

has_method_known_test() ->
    ?assert(beamtalk_system:has_method('getEnv:')),
    ?assert(beamtalk_system:has_method('getEnv:default:')),
    ?assert(beamtalk_system:has_method(osPlatform)),
    ?assert(beamtalk_system:has_method(osFamily)),
    ?assert(beamtalk_system:has_method(architecture)),
    ?assert(beamtalk_system:has_method(hostname)),
    ?assert(beamtalk_system:has_method(erlangVersion)),
    ?assert(beamtalk_system:has_method(pid)).

has_method_unknown_test() ->
    ?assertNot(beamtalk_system:has_method(frobnicateWidget)),
    ?assertNot(beamtalk_system:has_method('putEnv:')).
