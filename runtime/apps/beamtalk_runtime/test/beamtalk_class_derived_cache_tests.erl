%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_derived_cache_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for `beamtalk_class_derived_cache` (BT-4064): memoisation, the
`nocache` opt-out, the generation guard against a write racing a compute, and
invalidation by every `beamtalk_class_metadata` write.
""".

-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok.

cleanup(_) ->
    ok.

derived_cache_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"a cached value is served without recomputing", fun memoises/0},
        {"a nocache answer is not stored", fun nocache_is_not_stored/0},
        {"a write that lands during a compute makes its result stale",
            fun write_during_compute_is_not_served/0},
        {"a hit failing the validity check is recomputed and repaired",
            fun invalid_hit_is_recomputed/0},
        {"insert, merge_identity, set_runtime_class_methods, put_class_variables and delete each invalidate",
            fun metadata_writes_invalidate/0},
        {"a warm fetch reads no ETS entry (BT-4652)", fun memo_hit_reads_no_ets/0},
        {"a nocache answer is not memoised (BT-4652)", fun nocache_is_not_memoised/0},
        {"a generation bump turns the memo into a miss (BT-4652)", fun memo_follows_invalidate/0},
        {"a memo hit still runs the validity check (BT-4652)", fun memo_hit_runs_valid/0},
        {"without a generation nothing is memoised (BT-4652)", fun no_generation_no_memo/0},
        {"a full memo is dropped and refilled (BT-4652)", fun memo_is_bounded/0},
        {"a generation change sweeps stale memo entries (BT-4652)", fun memo_sweeps_stale/0},
        {"a superclass cycle terminates (BT-4065)", fun superclass_cycle_terminates/0}
    ]}.

memoises() ->
    Key = key(),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, a} end)),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)).

nocache_is_not_stored() ->
    Key = key(),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {nocache, a} end)),
    ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, b} end)).

write_during_compute_is_not_served() ->
    %% The compute read state from before a metadata write that lands while it
    %% is running; its result is tagged with the pre-write generation and must
    %% never be served afterwards.
    Key = key(),
    Stale = fun() ->
        ok = beamtalk_class_derived_cache:invalidate(),
        {cache, stale}
    end,
    ?assertEqual(stale, beamtalk_class_derived_cache:fetch(Key, Stale)),
    ?assertEqual(fresh, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, fresh} end)),
    ?assertEqual(fresh, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)).

invalid_hit_is_recomputed() ->
    %% fetch/3's Valid gate (used by tagged_class_dispatch/1's
    %% module-loaded check and class_of_object_by_name/1's pid-liveness
    %% check): a cached value that fails it is recomputed, and the
    %% recomputation overwrites the stale entry.
    Key = key(),
    ?assertEqual(stale, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, stale} end)),
    ?assertEqual(
        fresh,
        beamtalk_class_derived_cache:fetch(
            Key, fun() -> {cache, fresh} end, fun(V) -> V =:= fresh end
        )
    ),
    ?assertEqual(
        fresh, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)
    ).

metadata_writes_invalidate() ->
    Class = 'DerivedCacheTestClass',
    Writes = [
        fun() -> beamtalk_class_metadata:insert(Class, m1, [], none, false) end,
        fun() -> beamtalk_class_metadata:merge_identity(Class, m2, [], none, false) end,
        %% ADR 0142 §2 (BT-4299): a runtime fun at an ancestor changes a
        %% descendant's resolved owner, so it must drop derived facts too.
        fun() -> beamtalk_class_metadata:set_runtime_class_methods(Class, [foo]) end,
        %% BT-4299: so must a class-variable change (it feeds direct_call_ok).
        fun() -> beamtalk_class_metadata:put_class_variables(Class, true) end,
        fun() -> beamtalk_class_metadata:put_class_variables(Class, false) end,
        fun() -> beamtalk_class_metadata:delete(Class) end
    ],
    lists:foreach(
        fun(Write) ->
            Key = key(),
            old = beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, old} end),
            ok = Write(),
            ?assertEqual(new, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, new} end))
        end,
        Writes
    ).

superclass_cycle_terminates() ->
    %% `isKindOf:` walks the hierarchy on a hot path; a corrupt cyclic chain
    %% must answer, not loop.
    A = 'DerivedCacheCycleA',
    B = 'DerivedCacheCycleB',
    ok = beamtalk_class_metadata:insert(A, ma, [], B, false),
    ok = beamtalk_class_metadata:insert(B, mb, [], A, false),
    try
        ?assert(beamtalk_class_derived_cache:inherits_from(A, B)),
        ?assertNot(beamtalk_class_derived_cache:inherits_from(A, 'DerivedCacheCycleMissing'))
    after
        beamtalk_class_metadata:delete(A),
        beamtalk_class_metadata:delete(B)
    end.

key() ->
    {?MODULE, make_ref()}.

%% BT-4416: generation/0 is what per-process memos (beamtalk_equality's
%% `equals:` fast path) tag themselves with, so every invalidate/0 must move it.
generation_moves_on_invalidate_test() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    Gen0 = beamtalk_class_derived_cache:generation(),
    ?assert(is_integer(Gen0)),
    ok = beamtalk_class_derived_cache:invalidate(),
    ?assertNotEqual(Gen0, beamtalk_class_derived_cache:generation()).

%%% BT-4652: the per-process memo inside fetch/2,3.

erase_memo() ->
    [erase(K) || {{beamtalk_class_derived_cache, _} = K, _} <- get()],
    erase('$beamtalk_derived_cache_count'),
    ok.

memo_size() ->
    length([K || {{beamtalk_class_derived_cache, _} = K, _} <- get()]).

memo_hit_reads_no_ets() ->
    %% Remove the ETS row behind a warm key: only the per-process memo can
    %% still answer it.
    Key = key(),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, a} end)),
    true = ets:delete(beamtalk_class_derived_cache, Key),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)).

nocache_is_not_memoised() ->
    Key = key(),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {nocache, a} end)),
    ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, b} end)),
    ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)).

memo_follows_invalidate() ->
    Key = key(),
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, a} end)),
    ok = beamtalk_class_derived_cache:invalidate(),
    ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, b} end)).

memo_hit_runs_valid() ->
    Key = key(),
    ?assertEqual(
        a, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, a} end, fun(_) -> true end)
    ),
    %% Warm in the memo; a failing Valid must still send it back to Compute.
    ?assertEqual(
        b,
        beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, b} end, fun(V) -> V =/= a end)
    ),
    ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> error(recomputed) end)).

no_generation_no_memo() ->
    GenKey = {beamtalk_class_derived_cache, generation},
    Ref = persistent_term:get(GenKey),
    Key = key(),
    try
        erase_memo(),
        persistent_term:erase(GenKey),
        ?assertEqual(undefined, beamtalk_class_derived_cache:generation()),
        ?assertEqual(a, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, a} end)),
        ?assertEqual(b, beamtalk_class_derived_cache:fetch(Key, fun() -> {cache, b} end)),
        ?assertEqual(0, memo_size())
    after
        persistent_term:put(GenKey, Ref),
        beamtalk_class_derived_cache:invalidate()
    end.

memo_is_bounded() ->
    ok = beamtalk_class_derived_cache:invalidate(),
    erase_memo(),
    [
        beamtalk_class_derived_cache:fetch({memo_bound, N}, fun() -> {cache, N} end)
     || N <- lists:seq(1, 5000)
    ],
    ?assert(memo_size() =< 4096),
    ?assert(memo_size() > 0),
    ?assertEqual(
        5000, beamtalk_class_derived_cache:fetch({memo_bound, 5000}, fun() -> error(recomputed) end)
    ).

memo_sweeps_stale() ->
    ok = beamtalk_class_derived_cache:invalidate(),
    erase_memo(),
    [beamtalk_class_derived_cache:fetch({memo_stale, N}, fun() -> {cache, N} end) || N <- [1, 2, 3]],
    ?assertEqual(3, memo_size()),
    ok = beamtalk_class_derived_cache:invalidate(),
    %% Entries from the old generation linger until the next store, which
    %% sweeps them; only the new entry survives.
    ?assertEqual(a, beamtalk_class_derived_cache:fetch(key(), fun() -> {cache, a} end)),
    ?assertEqual(1, memo_size()).
