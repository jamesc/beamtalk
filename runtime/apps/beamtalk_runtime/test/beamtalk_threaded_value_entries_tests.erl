%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_threaded_value_entries_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for the value-type threaded entries ADR 0132 Phase 1d (BT-3872)
generates, over the compiled fixtures `test_fixtures/threaded_value_entries_fixture.bt`
(a sealed `Object subclass:`), `threaded_value_record_fixture.bt` (a
`Value subclass:`) and `threaded_value_test_case_fixture.bt` (a `TestCase
subclass:` whose threading methods also write a field).

Each threading method compiles to **one body over `Acc`** (`'__t2_<sel>'/n+2`)
with the plain `'<sel>'/n+1` as a wrapper over it, which `'dispatch'/3` keeps
calling. These tests call the entry **directly** with a hand-packed `Acc` map
and a Tier 2 block, and the plain function with the matching pure block, and
assert the two agree on the result and the block invocations
(`beamtalk_threaded_entry_test_helper:differential/6`); that a body writing a
field threads `Self` beside `Acc`; that the plain function refuses a Tier 2 block
before running it, also when it arrives through a dynamic send; that a use
holding another parameter's use threads both, the inner one first (BT-3890);
that the methods the index gives no threading parameter (a `^` block) have no
entry; and that the compiled metadata resolves the entry through
`send_threaded/4`.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(MOD, 'bt@threaded_value_entries_fixture').
-define(CLASS, 'ThreadedValueEntriesFixture').
-define(REC, 'bt@threaded_value_record_fixture').
-define(REC_CLASS, 'ThreadedValueRecordFixture').
-define(TC, 'bt@threaded_value_test_case_fixture').
-define(TC_CLASS, 'ThreadedValueTestCaseFixture').

-import(beamtalk_threaded_entry_test_helper, [t2/2, ran_t2/1, refused/1]).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(threaded_value_entries_fixture, ?CLASS),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(threaded_value_record_fixture, ?REC_CLASS),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(
        threaded_value_test_case_fixture, ?TC_CLASS
    ),
    ok.

entry_fn(Selector) -> list_to_atom("__t2_" ++ atom_to_list(Selector)).

%% `Selector` of `Mod` on the instance `Self`, plain function against entry.
differential(Mod, Self, Selector, Others, BlockPos, BlockArity, F) ->
    beamtalk_threaded_entry_test_helper:differential(
        {Mod, Selector, entry_fn(Selector)}, Self, Others, BlockPos, BlockArity, F
    ).

differential(Selector, Others, BlockPos, BlockArity, F) ->
    differential(?MOD, ?MOD:new(), Selector, Others, BlockPos, BlockArity, F).

%%====================================================================
%% Each Phase 1 use, plain function against threaded entry
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
        {"twiceVia: passes the block on to the sealed class's own entry", fun() ->
            {R, Log, _} = differential('twiceVia:', [], 0, 0, fun() -> 4 end),
            ?assertEqual(4, R),
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
        {"detectIn:with: stops at the first match", fun() ->
            {R, Log, _} = differential('detectIn:with:', [[1, 2, 3, 4]], 1, 1, fun(X) -> X > 1 end),
            ?assertEqual(2, R),
            ?assertEqual([[1], [2]], Log)
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
        {"branch: invokes the block as a conditional's receiver", fun() ->
            {R1, _, _} = differential('branch:', [], 0, 1, fun(X) -> X end),
            ?assertEqual(10, R1),
            {R2, _, _} = differential('branch:', [], 0, 1, fun(X) -> -X end),
            ?assertEqual(20, R2)
        end},
        {"withLocals: keeps the body's own local out of Acc", fun() ->
            {R, Log, Acc} = differential('withLocals:', [], 0, 1, fun(X) -> X * 2 end),
            ?assertEqual(12, R),
            ?assertEqual([[6]], Log),
            ?assertEqual([log, untouched], lists:sort(maps:keys(Acc)))
        end},
        {"both:with: threads both blocks, the inner use first (BT-3890)", fun() ->
            %% `self apply: a to: (b value)`: `b value` steps Acc, then `a`
            %% is passed on to `apply:to:`'s entry with the stepped Acc.
            ?assertEqual(
                11, ?MOD:'both:with:'(?MOD:new(), fun(X) -> X + 1 end, fun() -> 10 end)
            ),
            {R, Acc} = ?MOD:'__t2_both:with:'(
                ?MOD:new(),
                t2(1, fun(X) -> X + 1 end),
                t2(0, fun() -> 10 end),
                #{log => []}
            ),
            ?assertEqual(11, R),
            ?assertEqual([[], [10]], maps:get(log, Acc))
        end}
    ]}.

value_subclass_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a Value subclass's twice:", fun() ->
            {R, Log, _} = differential(?REC, ?REC:new(), 'twice:', [], 0, 0, fun() -> 3 end),
            ?assertEqual(3, R),
            ?assertEqual([[], []], Log)
        end},
        {"each: folds over the field's items", fun() ->
            {_, Log, _} = differential(?REC, ?REC:new(), 'each:', [], 0, 1, fun(X) -> X end),
            ?assertEqual([[1], [2], [3]], Log)
        end},
        {"twiceVia: passes the block on", fun() ->
            {R, Log, _} = differential(?REC, ?REC:new(), 'twiceVia:', [], 0, 0, fun() -> 5 end),
            ?assertEqual(5, R),
            ?assertEqual([[], []], Log)
        end},
        {"detectFirst: answers the first match", fun() ->
            {R, Log, _} = differential(
                ?REC, ?REC:new(), 'detectFirst:', [], 0, 1, fun(X) -> X >= 2 end
            ),
            ?assertEqual(2, R),
            ?assertEqual([[1], [2]], Log)
        end}
    ]}.

%% ADR 0120/0122: a `TestCase` body that writes a field threads `Self` beside
%% `Acc` (`[SelfVt, Acc]`): the field write before the invocation is seen by
%% it, and the one after it by the body's answer.
self_and_acc_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"bump: writes n, then invokes the block with it", fun() ->
            {R, Log, _} = differential(?TC, ?TC:new(), 'bump:', [], 0, 1, fun(N) -> N * 10 end),
            ?assertEqual(10, R),
            ?assertEqual([[1]], Log)
        end},
        {"bumpTwice: writes n around the invocation and answers it", fun() ->
            {R, Log, _} = differential(
                ?TC, ?TC:new(), 'bumpTwice:', [], 0, 1, fun(N) -> N + 4 end
            ),
            ?assertEqual(6, R),
            ?assertEqual([[1]], Log)
        end}
    ]}.

%%====================================================================
%% The plain function refuses a Tier 2 block before it runs
%%====================================================================

plain_refuses_tier2_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a Tier 2 block to a plain function raises stateful_block_dispatch unrun", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> ?MOD:'twice:'(?MOD:new(), ran_t2(0)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"so does a late-bound send, which dispatch/3 routes to the plain function", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() ->
                    beamtalk_message_dispatch:send(?MOD:new(), 'twiceVia:', [ran_t2(0)])
                end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"and the callable of a collection fold", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> ?MOD:'each:do:'(?MOD:new(), [1, 2], ran_t2(1)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"and a field-writing TestCase body, before its block runs", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> ?TC:'bump:'(?TC:new(), ran_t2(1)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"a pure block through dispatch is the unchanged plain path", fun() ->
            ?assertEqual(
                8, beamtalk_message_dispatch:send(?MOD:new(), 'twice:', [fun() -> 8 end])
            )
        end}
    ]}.

%%====================================================================
%% Methods the index gives no threading parameter have no entry
%%====================================================================

no_entry(Mod, Class, Selector, Arity) ->
    {Instance, _ClassSide} = beamtalk_class_metadata:lookup_tier2_methods(Class),
    ?assertNot(maps:is_key(Selector, Instance)),
    ?assertNot(erlang:function_exported(Mod, entry_fn(Selector), Arity + 2)),
    ?assertEqual(
        error,
        beamtalk_class_dispatch:lookup_threaded_entry(Class, Selector, lists:duplicate(Arity, x))
    ).

%% A late-bound send (a value-type method with a `^` block answers
%% `{Result, Self}` to `'dispatch'/3`, which unwraps it).
send(Receiver, Selector, Args) ->
    beamtalk_message_dispatch:send(Receiver, Selector, Args).

no_entry_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a ^ in a block that invokes the parameter: no entry, the plain method answers", fun() ->
            no_entry(?MOD, ?CLASS, 'detectFirst:in:', 2),
            ?assertEqual(
                3, send(?MOD:new(), 'detectFirst:in:', [fun(X) -> X > 2 end, [1, 3, 5]])
            ),
            ?assertEqual(nil, send(?MOD:new(), 'detectFirst:in:', [fun(_) -> false end, [1]]))
        end},
        {"a ^ block beside a root invocation: no entry", fun() ->
            no_entry(?MOD, ?CLASS, 'firstAfter:in:', 2),
            ?assertEqual(7, send(?MOD:new(), 'firstAfter:in:', [fun() -> ok end, [-1, 7]]))
        end},
        {"no block, or a stored block: no entry", fun() ->
            no_entry(?MOD, ?CLASS, 'plain:', 1),
            no_entry(?MOD, ?CLASS, 'storeAway:', 1),
            ?assertEqual(2, ?MOD:'plain:'(?MOD:new(), 1)),
            ?assertEqual([f, f], ?MOD:'storeAway:'(?MOD:new(), f))
        end}
    ]}.

%%====================================================================
%% The compiled metadata resolves the entry
%%====================================================================

send_threaded_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"send_threaded/4 to a value reaches the compiled entry and Acc round-trips", fun() ->
            {R, Acc} = beamtalk_dispatch:send_threaded(
                ?MOD:new(), 'twice:', [t2(0, fun() -> 3 end)], #{log => []}
            ),
            ?assertEqual(3, R),
            ?assertEqual([[], []], maps:get(log, Acc))
        end},
        {"and to a Value subclass instance", fun() ->
            {R, Acc} = beamtalk_dispatch:send_threaded(
                ?REC:new(), 'each:', [t2(1, fun(X) -> X end)], #{log => []}
            ),
            ?assertEqual(nil, R),
            ?assertEqual([[1], [2], [3]], maps:get(log, Acc))
        end},
        {"the registered entries are exactly the exported '__t2_' functions", fun() ->
            lists:foreach(
                fun({Mod, Class}) ->
                    {Instance, _ClassSide} = beamtalk_class_metadata:lookup_tier2_methods(Class),
                    {ok, {Mod, [{exports, Exports}]}} =
                        beam_lib:chunks(code:which(Mod), [exports]),
                    Exported = [
                        F
                     || {F, _Arity} <- Exports,
                        lists:prefix("__t2_", atom_to_list(F)),
                        not lists:prefix("__t2_class_", atom_to_list(F))
                    ],
                    Advertised = [Fn || {Fn, _Positions} <- maps:values(Instance)],
                    ?assertNotEqual([], Advertised),
                    ?assertEqual(lists:sort(Exported), lists:sort(Advertised))
                end,
                [{?MOD, ?CLASS}, {?REC, ?REC_CLASS}, {?TC, ?TC_CLASS}]
            )
        end}
    ]}.
