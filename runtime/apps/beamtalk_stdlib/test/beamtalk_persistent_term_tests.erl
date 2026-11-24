%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_persistent_term_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_persistent_term (ADR 0153 §3).

Keys are unique per test (`make_ref/0` based) because `persistent_term` is
node-global.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-define(COUNTER_KEY, '$bt_pt_test_counter').

%% Run Fun(Key) with a key unique to this call, erasing it afterwards.
with_key(Fun) ->
    Key = {pt_test, make_ref()},
    try
        Fun(Key)
    after
        beamtalk_persistent_term:erase(Key)
    end.

getOrPut_builds_once_test() ->
    with_key(fun(Key) ->
        ?assertEqual(42, beamtalk_persistent_term:getOrPut(Key, fun() -> 42 end)),
        ?assertEqual(
            42, beamtalk_persistent_term:getOrPut(Key, fun() -> error(should_not_run) end)
        )
    end).

getOrPut_stores_under_wrapped_key_test() ->
    with_key(fun(Key) ->
        beamtalk_persistent_term:getOrPut(Key, fun() -> v end),
        ?assertEqual(v, persistent_term:get({beamtalk_persistent_term, Key})),
        ?assertEqual(missing, persistent_term:get(Key, missing))
    end).

getOrPut_raising_block_stores_nothing_test() ->
    with_key(fun(Key) ->
        ?assertError(boom, beamtalk_persistent_term:getOrPut(Key, fun() -> error(boom) end)),
        ?assertNot(beamtalk_persistent_term:includesKey(Key)),
        ?assertEqual(7, beamtalk_persistent_term:getOrPut(Key, fun() -> 7 end))
    end).

getOrPut_nil_counts_as_present_test() ->
    with_key(fun(Key) ->
        ?assertEqual(nil, beamtalk_persistent_term:getOrPut(Key, fun() -> nil end)),
        ?assert(beamtalk_persistent_term:includesKey(Key)),
        ?assertEqual(
            nil, beamtalk_persistent_term:getOrPut(Key, fun() -> error(should_not_run) end)
        )
    end).

getOrPut_keeps_first_value_when_block_races_test() ->
    with_key(fun(Key) ->
        %% The block itself stores first, as a racing process would.
        Result = beamtalk_persistent_term:getOrPut(Key, fun() ->
            persistent_term:put({beamtalk_persistent_term, Key}, first),
            second
        end),
        ?assertEqual(first, Result),
        ?assertEqual(first, persistent_term:get({beamtalk_persistent_term, Key}))
    end).

getOrPut_concurrent_processes_agree_test() ->
    with_key(fun(Key) ->
        Parent = self(),
        Pids = [
            spawn_link(fun() ->
                Parent ! {self(), beamtalk_persistent_term:getOrPut(Key, fun() -> built end)}
            end)
         || _ <- lists:seq(1, 8)
        ],
        Results = [
            receive
                {Pid, V} -> V
            after 5000 -> timeout
            end
         || Pid <- Pids
        ],
        ?assertEqual([built], lists:usort(Results))
    end).

getOrPut_refuses_pid_test() ->
    with_key(fun(Key) ->
        ?assertBtError(
            type_error, beamtalk_persistent_term:getOrPut(Key, fun() -> self() end)
        ),
        ?assertNot(beamtalk_persistent_term:includesKey(Key))
    end).

getOrPut_refuses_actor_and_class_test() ->
    with_key(fun(Key) ->
        Obj = #beamtalk_object{class = 'Counter', class_mod = counter, pid = self()},
        ?assertBtError(type_error, beamtalk_persistent_term:getOrPut(Key, fun() -> Obj end)),
        ClassObj = #beamtalk_object{
            class = 'Counter class', class_mod = counter, pid = self()
        },
        ?assertBtError(
            type_error, beamtalk_persistent_term:getOrPut(Key, fun() -> ClassObj end)
        ),
        ?assertNot(beamtalk_persistent_term:includesKey(Key))
    end).

getOrPut_refusal_names_key_test() ->
    with_key(fun(Key) ->
        try beamtalk_persistent_term:getOrPut(Key, fun() -> self() end) of
            _ -> ?assert(false)
        catch
            error:Wrapped ->
                #{error := #beamtalk_error{hint = Hint, details = Details}} = Wrapped,
                ?assertEqual(#{key => Key}, Details),
                ?assertNotEqual(nomatch, binary:match(iolist_to_binary([Hint]), <<"pt_test">>))
        end
    end).

getOrPut_hit_skips_pid_check_test() ->
    %% The check runs only on a miss: a value already stored is answered as is.
    with_key(fun(Key) ->
        Self = self(),
        persistent_term:put({beamtalk_persistent_term, Key}, Self),
        ?assertEqual(Self, beamtalk_persistent_term:getOrPut(Key, fun() -> other end))
    end).

getOrPut_nested_pid_not_detected_test() ->
    with_key(fun(Key) ->
        ?assertEqual(
            {wrapped, self()},
            beamtalk_persistent_term:getOrPut(Key, fun() -> {wrapped, self()} end)
        )
    end).

getOrPut_non_block_test() ->
    with_key(fun(Key) ->
        ?assertBtError(type_error, beamtalk_persistent_term:getOrPut(Key, not_a_block))
    end).

getOrElse_test() ->
    with_key(fun(Key) ->
        ?assertEqual(0, beamtalk_persistent_term:getOrElse(Key, fun() -> 0 end)),
        ?assertNot(beamtalk_persistent_term:includesKey(Key)),
        beamtalk_persistent_term:getOrPut(Key, fun() -> 5 end),
        ?assertEqual(
            5, beamtalk_persistent_term:getOrElse(Key, fun() -> error(should_not_run) end)
        )
    end).

getOrElse_non_block_test() ->
    with_key(fun(Key) ->
        ?assertBtError(type_error, beamtalk_persistent_term:getOrElse(Key, 1))
    end).

includesKey_and_erase_test() ->
    with_key(fun(Key) ->
        ?assertNot(beamtalk_persistent_term:includesKey(Key)),
        beamtalk_persistent_term:getOrPut(Key, fun() -> 1 end),
        ?assert(beamtalk_persistent_term:includesKey(Key)),
        ?assertEqual(true, beamtalk_persistent_term:erase(Key)),
        ?assertEqual(false, beamtalk_persistent_term:erase(Key)),
        ?assertNot(beamtalk_persistent_term:includesKey(Key))
    end).
