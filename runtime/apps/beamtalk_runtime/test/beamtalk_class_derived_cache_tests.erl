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
        {"insert, merge_identity and delete each invalidate", fun metadata_writes_invalidate/0}
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

metadata_writes_invalidate() ->
    Class = 'DerivedCacheTestClass',
    Writes = [
        fun() -> beamtalk_class_metadata:insert(Class, m1, [], none, false) end,
        fun() -> beamtalk_class_metadata:merge_identity(Class, m2, [], none, false) end,
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

key() ->
    {?MODULE, make_ref()}.
