%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_threaded_class_entries_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for the class-method threaded entries ADR 0132 Phase 1c (BT-3871)
generates, over the compiled fixture `test_fixtures/threaded_class_entries_fixture.bt`.

Each fixture method compiles to **one body over `Acc`** (`'__t2_class_<sel>'/n+2`)
with the plain `'class_<sel>'/n+1` as a wrapper over it. These tests call the
threaded entry **directly** with a hand-packed `Acc` map and a Tier 2 block
(`fun(Args..., Acc) -> {Result, Acc1}`), and the plain entry with the matching
pure block, and assert that

- the two agree on the **result** (the plain path's behaviour is unchanged);
- the sequence of **block invocations** (their arguments, in order) is the
  same, and the `Acc` the block threaded comes back (every invocation advanced
  it exactly once, in order, whatever the method does around the invocation);
- keys of `Acc` the block never touched survive;
- a `^` (non-local return) carries the `Acc` version at the throw, and a
  foreign token's NLR is re-raised untouched;
- the compiled metadata resolves the entry through `send_threaded/4`.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(MOD, 'bt@threaded_class_entries_fixture').
-define(CLASS, 'ThreadedClassEntriesFixture').

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(threaded_class_entries_fixture, ?CLASS),
    ok.

%%====================================================================
%% Blocks
%%====================================================================

%% A Tier 2 block of user arity N: `fun(Args..., Acc) -> {F(Args...), Acc1}`
%% that records its arguments in `Acc` under `log`.
t2(0, F) -> fun(Acc) -> step(F, [], Acc) end;
t2(1, F) -> fun(A, Acc) -> step(F, [A], Acc) end;
t2(2, F) -> fun(A, B, Acc) -> step(F, [A, B], Acc) end.

step(F, Args, Acc) ->
    {erlang:apply(F, Args), Acc#{log => maps:get(log, Acc, []) ++ [Args]}}.

%% The matching Tier 1 block, which records its arguments in the process
%% dictionary instead.
t1(0, F) -> fun() -> note(F, []) end;
t1(1, F) -> fun(A) -> note(F, [A]) end;
t1(2, F) -> fun(A, B) -> note(F, [A, B]) end.

note(F, Args) ->
    put(log, get(log) ++ [Args]),
    erlang:apply(F, Args).

insert_at(0, X, Rest) -> [X | Rest];
insert_at(N, X, [H | T]) -> [H | insert_at(N - 1, X, T)].

plain_fn(Selector) -> list_to_atom("class_" ++ atom_to_list(Selector)).
entry_fn(Selector) -> list_to_atom("__t2_class_" ++ atom_to_list(Selector)).

%% Runs `Selector` through its plain entry with a pure block and through its
%% threaded entry with a Tier 2 block, asserts they agree, and answers
%% `{Result, BlockLog, Acc1}`. `Others` are the non-block arguments in order,
%% `BlockPos` the block's argument position.
differential(Selector, Others, BlockPos, BlockArity, F) ->
    put(log, []),
    PlainArgs = insert_at(BlockPos, t1(BlockArity, F), Others),
    Plain = erlang:apply(?MOD, plain_fn(Selector), [nil | PlainArgs]),
    PlainLog = erase(log),
    EntryArgs = insert_at(BlockPos, t2(BlockArity, F), Others),
    {Result, Acc1} = erlang:apply(
        ?MOD, entry_fn(Selector), [nil | EntryArgs] ++ [#{log => [], untouched => kept}]
    ),
    ?assertEqual(Plain, Result),
    ?assertEqual(PlainLog, maps:get(log, Acc1)),
    ?assertEqual(kept, maps:get(untouched, Acc1)),
    {Result, PlainLog, Acc1}.

%%====================================================================
%% Each Phase 1 use, plain entry against threaded entry
%%====================================================================

entries_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"twice: invokes the block twice", fun() ->
            {R, Log, _} = differential('twice:', [], 0, 0, fun() -> 7 end),
            ?assertEqual(7, R),
            ?assertEqual([[], []], Log)
        end},
        {"each:do: folds the callable over the items", fun() ->
            {R, Log, _} = differential('each:do:', [[1, 2, 3]], 1, 1, fun(X) -> X * 10 end),
            ?assertEqual(nil, R),
            ?assertEqual([[1], [2], [3]], Log)
        end},
        {"twiceVia: passes the block on to a same-class entry", fun() ->
            {R, Log, _} = differential('twiceVia:', [], 0, 0, fun() -> 4 end),
            ?assertEqual(4, R),
            ?assertEqual([[], []], Log)
        end},
        {"viaOwn: passes the block on through the class's own name", fun() ->
            {R, Log, _} = differential('viaOwn:', [], 0, 0, fun() -> 5 end),
            ?assertEqual(5, R),
            ?assertEqual([[], []], Log)
        end},
        {"apply:to: invokes the block with an argument", fun() ->
            {R, Log, _} = differential('apply:to:', [9], 0, 1, fun(X) -> X + 1 end),
            ?assertEqual(10, R),
            ?assertEqual([[9]], Log)
        end},
        {"cascade: invokes the block once per message and answers the last", fun() ->
            {R, Log, _} = differential('cascade:', [], 0, 1, fun(X) -> X * 100 end),
            ?assertEqual(200, R),
            ?assertEqual([[1], [2]], Log)
        end},
        {"sumOver:with: threads the fold's accumulator and Acc", fun() ->
            {R, Log, _} = differential(
                'sumOver:with:', [[1, 2, 3]], 1, 2, fun(Sum, X) -> Sum + X end
            ),
            ?assertEqual(6, R),
            ?assertEqual([[0, 1], [1, 2], [3, 3]], Log)
        end},
        {"collectIn:with:", fun() ->
            {R, Log, _} = differential('collectIn:with:', [[1, 2, 3]], 1, 1, fun(X) -> X * X end),
            ?assertEqual([1, 4, 9], R),
            ?assertEqual([[1], [2], [3]], Log)
        end},
        {"selectIn:with:", fun() ->
            {R, Log, _} = differential('selectIn:with:', [[1, 2, 3, 4]], 1, 1, fun(X) ->
                X rem 2 =:= 0
            end),
            ?assertEqual([2, 4], R),
            ?assertEqual([[1], [2], [3], [4]], Log)
        end},
        {"detectIn:with: stops at the first match", fun() ->
            {R, Log, _} = differential('detectIn:with:', [[1, 2, 3, 4]], 1, 1, fun(X) -> X > 1 end),
            ?assertEqual(2, R),
            ?assertEqual([[1], [2]], Log)
        end},
        {"detectIn:with: falls back to the ifNone handler", fun() ->
            {R, Log, _} = differential('detectIn:with:', [[1, 2]], 1, 1, fun(X) -> X > 5 end),
            ?assertEqual(0, R),
            ?assertEqual([[1], [2]], Log)
        end},
        {"countIn:with:", fun() ->
            {R, Log, _} = differential('countIn:with:', [[1, 2, 3]], 1, 1, fun(X) -> X > 1 end),
            ?assertEqual(2, R),
            ?assertEqual([[1], [2], [3]], Log)
        end},
        {"bothOf: evaluates two invocations as operands, in order", fun() ->
            {R, Log, _} = differential('bothOf:', [], 0, 1, fun(X) -> X * 10 end),
            ?assertEqual(30, R),
            ?assertEqual([[1], [2]], Log)
        end},
        {"doubled: uses an invocation as an assigned value", fun() ->
            {R, Log, _} = differential('doubled:', [], 0, 1, fun(X) -> X * 10 end),
            ?assertEqual(60, R),
            ?assertEqual([[3]], Log)
        end},
        {"branch: invokes the block as a conditional's receiver (true arm)", fun() ->
            {R, Log, _} = differential('branch:', [], 0, 1, fun(X) -> X end),
            ?assertEqual(10, R),
            ?assertEqual([[1]], Log)
        end},
        {"branch: (false arm)", fun() ->
            {R, Log, _} = differential('branch:', [], 0, 1, fun(X) -> -X end),
            ?assertEqual(20, R),
            ?assertEqual([[1]], Log)
        end},
        {"withLocals: keeps the body's own local out of Acc", fun() ->
            {R, Log, Acc} = differential('withLocals:', [], 0, 1, fun(X) -> X * 2 end),
            ?assertEqual(12, R),
            ?assertEqual([[6]], Log),
            %% Acc holds the caller's keys only: no `'__local__total'` leaked in.
            ?assertEqual([log, untouched], lists:sort(maps:keys(Acc)))
        end},
        {"afterEnsure: lowers a handler construct beside the invocation", fun() ->
            {R, Log, _} = differential('afterEnsure:', [], 0, 1, fun(X) -> X + 1 end),
            ?assertEqual(6, R),
            ?assertEqual([[5]], Log)
        end}
    ]}.

%%====================================================================
%% Non-local return
%%====================================================================

nlr_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a ^ after an invocation answers {Val, AccAtThrow}: the writes before it are kept",
            fun() ->
                {R, Log, Acc} = differential(
                    'callThen:firstPositiveIn:', [[-1, 0, 7, 8]], 0, 0, fun() -> ok end
                ),
                ?assertEqual(7, R),
                ?assertEqual([[]], Log),
                ?assertEqual([[]], maps:get(log, Acc))
            end},
        {"a body that finishes without the ^ answers nil and the final Acc", fun() ->
            {R, Log, _} = differential(
                'callThen:firstPositiveIn:', [[-1, 0]], 0, 0, fun() -> ok end
            ),
            ?assertEqual(nil, R),
            ?assertEqual([[]], Log)
        end},
        {"a ^ before any invocation carries the entry's Acc", fun() ->
            {R, Log, Acc} = differential('guarded:flag:', [true], 0, 0, fun() -> 1 end),
            ?assertEqual(0, R),
            ?assertEqual([], Log),
            ?assertEqual([], maps:get(log, Acc))
        end},
        {"a guarded body without the ^ invokes the block", fun() ->
            {R, Log, _} = differential('guarded:flag:', [false], 0, 0, fun() -> 1 end),
            ?assertEqual(1, R),
            ?assertEqual([[]], Log)
        end},
        {"a foreign token's NLR is re-raised untouched, from both entries", fun() ->
            Foreign = {'$bt_nlr', make_ref(), foreign_value, nil},
            Throwing2 = fun(Acc) ->
                erlang:throw(Foreign),
                {nil, Acc}
            end,
            Throwing1 = fun() -> erlang:throw(Foreign) end,
            ?assertEqual(
                {thrown, Foreign},
                try
                    ?MOD:'__t2_class_callThen:firstPositiveIn:'(nil, Throwing2, [1], #{})
                catch
                    throw:T -> {thrown, T}
                end
            ),
            ?assertEqual(
                {thrown, Foreign},
                try
                    ?MOD:'class_callThen:firstPositiveIn:'(nil, Throwing1, [1])
                catch
                    throw:T -> {thrown, T}
                end
            )
        end}
    ]}.

%%====================================================================
%% The tier gate, as the body uses it
%%====================================================================

tier_gate_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a Tier 1 block through the threaded entry leaves Acc unchanged", fun() ->
            Acc = #{n => 1},
            ?assertEqual(
                {7, Acc}, ?MOD:'__t2_class_twice:'(nil, fun() -> 7 end, Acc)
            )
        end},
        {"a block of the wrong arity raises a structured arity_mismatch", fun() ->
            try
                ?MOD:'__t2_class_twice:'(nil, fun(_A, _B, _C) -> ok end, #{}),
                ?assert(false)
            catch
                error:#{error := #beamtalk_error{kind = Kind}} ->
                    ?assertEqual(arity_mismatch, Kind)
            end
        end}
    ]}.

%%====================================================================
%% The compiled metadata resolves the entry
%%====================================================================

send_threaded_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"send_threaded/4 reaches the compiled entry and the Acc round-trips", fun() ->
            Pid = beamtalk_class_registry:whereis_class(?CLASS),
            Class = beamtalk_class_registry:class_object_from_pid(Pid),
            {R, Acc} = beamtalk_dispatch:send_threaded(
                Class, 'twice:', [t2(0, fun() -> 3 end)], #{log => []}
            ),
            ?assertEqual(3, R),
            ?assertEqual([[], []], maps:get(log, Acc)),
            %% The plain send of the same selector is the unchanged plain path.
            ?assertEqual(3, beamtalk_class_dispatch:class_send(Pid, 'twice:', [fun() -> 3 end]))
        end},
        {"a method without a threading parameter has no entry", fun() ->
            Pid = beamtalk_class_registry:whereis_class(?CLASS),
            ?assertEqual(
                error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'plain:', [1])
            ),
            ?assertEqual(
                error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'storeAway:', [b])
            ),
            ?assertEqual(2, ?MOD:'class_plain:'(nil, 1)),
            ?assertEqual([f, f], ?MOD:'class_storeAway:'(nil, f))
        end},
        {"the registered entries are exactly the exported '__t2_class_' functions", fun() ->
            Pid = beamtalk_class_registry:whereis_class(?CLASS),
            {_Instance, ClassSide} = beamtalk_class_metadata:lookup_tier2_methods(?CLASS),
            {ok, {?MOD, [{exports, Exports}]}} = beam_lib:chunks(code:which(?MOD), [exports]),
            Exported = [
                F
             || {F, _Arity} <- Exports,
                lists:prefix("__t2_class_", atom_to_list(F))
            ],
            Advertised = [Fn || {Fn, _Positions} <- maps:values(ClassSide)],
            ?assertEqual(lists:sort(Exported), lists:sort(Advertised)),
            maps:foreach(
                fun(Sel, {Fn, _}) ->
                    ?assertEqual(
                        {ok, ?MOD, Fn},
                        beamtalk_class_dispatch:lookup_threaded_class_entry(
                            Pid, Sel, lists:duplicate(arity_of(Sel), x)
                        )
                    )
                end,
                ClassSide
            )
        end}
    ]}.

arity_of(Selector) ->
    case atom_to_list(Selector) of
        S -> length([C || C <- S, C =:= $:])
    end.
