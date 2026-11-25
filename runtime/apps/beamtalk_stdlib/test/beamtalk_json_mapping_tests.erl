%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_mapping_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_json_mapping (BT-5191). The JsonMapping behaviour
itself is covered by `stdlib/test/json_mapping_test.bt`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Without the secret metadata, encoding fails closed with a structured error.
secret_slots_unreadable_raises_structured_error_test() ->
    Obj = #beamtalk_object{class = 'Bt5191Ghost', class_mod = bt5191_no_such_module, pid = self()},
    ?assertBtError(runtime_error, 'JsonMapping', 'encode:', beamtalk_json_mapping:secretSlots(Obj)).

secret_slots_of_non_actor_is_empty_test() ->
    ?assertEqual([], beamtalk_json_mapping:secretSlots(#{a => 1})).

redacted_is_a_string_test() ->
    ?assertEqual(
        beamtalk_primitive:string_from_binary(<<"[redacted]">>), beamtalk_json_mapping:redacted()
    ).
