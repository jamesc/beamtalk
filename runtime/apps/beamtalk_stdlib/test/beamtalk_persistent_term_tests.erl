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
                ?assertNotEqual(nomatch, binary:match(element(2, Hint), <<"pt_test">>))
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

%% BT-5191: a deterministic interleaving of two racers. A misses and blocks
%% inside its block; B then misses on the same key. B must wait for A (not run
%% its own block), and both must answer the one value A stored. Before the
%% fix B ran its block and stored `b` over the top of A's build.
getOrPut_second_racer_waits_and_answers_first_value_test() ->
    with_key(fun(Key) ->
        Parent = self(),
        A = spawn_link(fun() ->
            V = beamtalk_persistent_term:getOrPut(Key, fun() ->
                Parent ! {a_building, self()},
                receive
                    go -> a
                end
            end),
            Parent ! {result, self(), V}
        end),
        receive
            {a_building, A} -> ok
        after 5000 -> error(a_never_built)
        end,
        B = spawn_link(fun() ->
            V = beamtalk_persistent_term:getOrPut(Key, fun() ->
                Parent ! b_ran,
                b
            end),
            Parent ! {result, self(), V}
        end),
        %% Proceed once B is queued behind A on the key's lock, or (the bug)
        %% has run its own block.
        BState = await_queued_or_ran({beamtalk_persistent_term, Key}, B, 500),
        A ! go,
        ResultA = receive_result(A),
        ResultB = receive_result(B),
        ?assertEqual(queued, BState),
        ?assertEqual(a, ResultA),
        ?assertEqual(a, ResultB),
        ?assertEqual(a, persistent_term:get({beamtalk_persistent_term, Key}))
    end).

%% BT-5191: every racer offers a distinct value; all answers equal the stored one.
getOrPut_racers_with_distinct_values_answer_stored_value_test() ->
    with_key(fun(Key) ->
        Parent = self(),
        Pids = [
            spawn_link(fun() ->
                Parent ! {result, self(), beamtalk_persistent_term:getOrPut(Key, fun() -> N end)}
            end)
         || N <- lists:seq(1, 16)
        ],
        Results = [receive_result(Pid) || Pid <- Pids],
        Stored = persistent_term:get({beamtalk_persistent_term, Key}),
        ?assertEqual([Stored], lists:usort(Results))
    end).

%% BT-5191: `~tp` of a key above code point 255 used to make the refusal
%% itself fail with `badarg` instead of the type error.
getOrPut_refusal_works_for_unicode_key_test() ->
    Key = {pt_test, make_ref(), 'ключ', <<"κλειδί"/utf8>>},
    try
        ?assertBtError(type_error, beamtalk_persistent_term:getOrPut(Key, fun() -> self() end)),
        ?assertNot(beamtalk_persistent_term:includesKey(Key))
    after
        beamtalk_persistent_term:erase(Key)
    end.

%% A block that asks for its own key would wait for itself: it raises instead.
getOrPut_block_asking_for_own_key_raises_test() ->
    with_key(fun(Key) ->
        ?assertBtError(
            runtime_error,
            beamtalk_persistent_term:getOrPut(Key, fun() ->
                beamtalk_persistent_term:getOrPut(Key, fun() -> inner end)
            end)
        ),
        ?assertNot(beamtalk_persistent_term:includesKey(Key)),
        ?assertEqual(7, beamtalk_persistent_term:getOrPut(Key, fun() -> 7 end))
    end).

%% A building process that dies hands the build to the next waiter.
getOrPut_builder_death_lets_waiter_build_test() ->
    with_key(fun(Key) ->
        Parent = self(),
        {A, MRef} = spawn_monitor(fun() ->
            beamtalk_persistent_term:getOrPut(Key, fun() ->
                Parent ! {a_building, self()},
                receive
                    die -> exit(killed_in_block)
                end
            end)
        end),
        receive
            {a_building, A} -> ok
        after 5000 -> error(a_never_built)
        end,
        B = spawn_link(fun() ->
            Parent ! {result, self(), beamtalk_persistent_term:getOrPut(Key, fun() -> b end)}
        end),
        queued = await_queued_or_ran({beamtalk_persistent_term, Key}, B, 500),
        A ! die,
        receive
            {'DOWN', MRef, process, A, _} -> ok
        end,
        ?assertEqual(b, receive_result(B))
    end).

%% Wait until `Pid` is queued behind the holder of `LockKey` (`queued`), or
%% has run its own block / finished (`ran`). Polls `Tries` times, 10 ms apart.
await_queued_or_ran(_LockKey, _Pid, 0) ->
    timeout;
await_queued_or_ran(LockKey, Pid, Tries) ->
    receive
        b_ran -> ran
    after 10 ->
        case lists:member(Pid, beamtalk_fifo_lock:queue(LockKey)) of
            true -> queued;
            false -> await_queued_or_ran(LockKey, Pid, Tries - 1)
        end
    end.

receive_result(Pid) ->
    receive
        {result, Pid, V} -> V
    after 5000 -> {timeout, Pid}
    end.
