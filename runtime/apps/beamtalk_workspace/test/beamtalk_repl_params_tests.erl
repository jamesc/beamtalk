%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_params_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for `beamtalk_repl_params`: the shared request-param helpers
(non-empty strings, existing-atom resolution with an explicit unknown-name
policy, and the actor-pid param).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% nonempty_binary/2 and nonempty_or_undefined/1
%%====================================================================

nonempty_binary_present_test() ->
    ?assertEqual(
        {ok, <<"x">>}, beamtalk_repl_params:nonempty_binary(#{<<"k">> => <<"x">>}, <<"k">>)
    ).

nonempty_binary_rejects_absent_empty_and_non_binary_test() ->
    ?assertEqual(error, beamtalk_repl_params:nonempty_binary(#{}, <<"k">>)),
    ?assertEqual(error, beamtalk_repl_params:nonempty_binary(#{<<"k">> => <<>>}, <<"k">>)),
    ?assertEqual(error, beamtalk_repl_params:nonempty_binary(#{<<"k">> => 7}, <<"k">>)),
    ?assertEqual(error, beamtalk_repl_params:nonempty_binary(#{<<"k">> => "str"}, <<"k">>)).

nonempty_or_undefined_test() ->
    ?assertEqual(undefined, beamtalk_repl_params:nonempty_or_undefined(<<>>)),
    ?assertEqual(undefined, beamtalk_repl_params:nonempty_or_undefined(undefined)),
    ?assertEqual(<<"a">>, beamtalk_repl_params:nonempty_or_undefined(<<"a">>)).

%%====================================================================
%% existing_atom_param/2
%%====================================================================

existing_atom_known_name_ignores_policy_test() ->
    ?assertEqual({ok, lists}, beamtalk_repl_params:existing_atom_param(<<"lists">>, sentinel)),
    ?assertEqual(
        {ok, lists},
        beamtalk_repl_params:existing_atom_param(<<"lists">>, {error, fun(_) -> boom end})
    ).

existing_atom_unknown_sentinel_policy_test() ->
    ?assertEqual(
        {ok, beamtalk_repl_params:unknown_name()},
        beamtalk_repl_params:existing_atom_param(<<"xyzzy_params_never_interned_1">>, sentinel)
    ).

existing_atom_unknown_error_policy_test() ->
    Policy = {error, fun(N) -> {not_found, N} end},
    ?assertEqual(
        {error, {not_found, <<"xyzzy_params_never_interned_2">>}},
        beamtalk_repl_params:existing_atom_param(<<"xyzzy_params_never_interned_2">>, Policy)
    ).

existing_atom_empty_name_is_unknown_test() ->
    ?assertEqual(
        {ok, beamtalk_repl_params:unknown_name()},
        beamtalk_repl_params:existing_atom_param(<<>>, sentinel)
    ).

%%====================================================================
%% actor_pid/1, validate_actor_pid/1, invalid_pid_error/2
%%====================================================================

actor_pid_invalid_string_test() ->
    {error, #beamtalk_error{kind = invalid_pid, class = 'Actor', message = Msg}} =
        beamtalk_repl_params:actor_pid(#{<<"actor">> => <<"not-a-pid">>}),
    ?assertEqual({beamtalk_string, <<"Invalid actor PID: not-a-pid">>}, Msg).

actor_pid_missing_param_is_invalid_pid_test() ->
    {error, #beamtalk_error{kind = invalid_pid}} = beamtalk_repl_params:actor_pid(#{}).

actor_pid_wrong_typed_param_is_invalid_pid_test_() ->
    %% A malformed request can send any JSON value; none may crash the helper.
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = invalid_pid, class = 'Actor'}},
            beamtalk_repl_params:actor_pid(#{<<"actor">> => Value})
        )
     || Value <- [42, 1.5, "0.1.0", [<<"a">>], #{<<"k">> => 1}, true, null]
    ].

%% The rendered error must not itself crash on code points above 255.
actor_pid_wrong_typed_unicode_value_test() ->
    {error, #beamtalk_error{kind = invalid_pid, message = Msg}} =
        beamtalk_repl_params:actor_pid(#{<<"actor">> => #{<<"k">> => [8364, 955]}}),
    ?assertEqual({beamtalk_string, <<"Invalid actor PID: <not a string>">>}, Msg).

nonempty_or_undefined_wrong_type_is_absent_test_() ->
    [
        ?_assertEqual(undefined, beamtalk_repl_params:nonempty_or_undefined(Value))
     || Value <- [42, "str", [<<"a">>], #{<<"k">> => 1}, true]
    ].

actor_pid_unknown_actor_test() ->
    PidBin = list_to_binary(pid_to_list(beamtalk_test_proc:spawn_idle())),
    {error, #beamtalk_error{kind = unknown_actor, hint = Hint}} =
        beamtalk_repl_params:actor_pid(#{<<"actor">> => PidBin}),
    ?assertEqual({beamtalk_string, <<"Use :actors to list valid actor PIDs.">>}, Hint).

validate_actor_pid_invalid_test() ->
    ?assertEqual({error, invalid_pid}, beamtalk_repl_params:validate_actor_pid("not-a-pid")),
    ?assertEqual({error, invalid_pid}, beamtalk_repl_params:validate_actor_pid("")).

is_known_actor_non_actor_test() ->
    ?assertEqual(false, beamtalk_repl_params:is_known_actor(beamtalk_test_proc:spawn_idle())).
