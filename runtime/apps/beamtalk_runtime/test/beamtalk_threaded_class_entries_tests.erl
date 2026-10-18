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
- a method whose body holds a `^` block has no entry (Phase 1c ships no
  NLR-carrying threaded body; ADR 0132 Phase 3), and a foreign token's NLR is
  re-raised untouched;
- the compiled metadata resolves the entry through `send_threaded/4`.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").
-include("beamtalk_threading_abi.hrl").

-define(MOD, 'bt@threaded_class_entries_fixture').
-define(CLASS, 'ThreadedClassEntriesFixture').
%% The plain-entry sentinel, as the plain wrapper of `twice:` passes it.
-define(PLAIN, ?BT_PLAIN_ENTRY_ACC('ThreadedClassEntriesFixture class', 'twice:')).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(threaded_class_entries_fixture, ?CLASS),
    ok.

%%====================================================================
%% Blocks
%%====================================================================

%% A Tier 2 block of user arity N: `fun(Args..., Acc) -> {F(Args...), Acc1}`
%% that records its arguments in `Acc` under `log`, minted under the Tier 2
%% mark the gate reads (BT-3885).
t2(0, F) -> mark(fun(Acc) -> step(F, [], Acc) end);
t2(1, F) -> mark(fun(A, Acc) -> step(F, [A], Acc) end);
t2(2, F) -> mark(fun(A, B, Acc) -> step(F, [A, B], Acc) end).

mark(F) -> beamtalk_tier2_test_block:mark(F).

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
%% Non-local return: a method whose body holds a `^` block has no entry
%%====================================================================

%% ADR 0132 Phase 1c (BT-3871 review): a `^` throws the `Acc` version current
%% where its block is lowered, which a threaded callee can outrun, so the index
%% gives such a method no threaded entry; it is the plain method it always was.
no_entry(Selector, Arity) ->
    Pid = beamtalk_class_registry:whereis_class(?CLASS),
    {_Instance, ClassSide} = beamtalk_class_metadata:lookup_tier2_methods(?CLASS),
    ?assertNot(maps:is_key(Selector, ClassSide)),
    ?assertNot(erlang:function_exported(?MOD, entry_fn(Selector), Arity + 2)),
    ?assertEqual(
        error,
        beamtalk_class_dispatch:lookup_threaded_class_entry(
            Pid, Selector, lists:duplicate(Arity, x)
        )
    ).

nlr_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a ^ after an invocation: no entry, the plain method answers the ^", fun() ->
            no_entry('callThen:firstPositiveIn:', 2),
            ?assertEqual(
                7, ?MOD:'class_callThen:firstPositiveIn:'(nil, fun() -> ok end, [-1, 0, 7, 8])
            ),
            ?assertEqual(nil, ?MOD:'class_callThen:firstPositiveIn:'(nil, fun() -> ok end, [-1, 0]))
        end},
        {"a ^ in a conditional arm: no entry, the plain method is unchanged", fun() ->
            no_entry('guarded:flag:', 2),
            ?assertEqual(0, ?MOD:'class_guarded:flag:'(nil, fun() -> 1 end, true)),
            ?assertEqual(1, ?MOD:'class_guarded:flag:'(nil, fun() -> 1 end, false))
        end},
        {"a foreign token's NLR is re-raised untouched", fun() ->
            Foreign = {'$bt_nlr', make_ref(), foreign_value, nil},
            Throwing1 = fun() -> erlang:throw(Foreign) end,
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
%% The plain entry refuses a Tier 2 block (BT-3871 review blocker)
%%====================================================================

%% A Tier 2 block records that it ran; the plain entry has no caller map to
%% thread, so it must be refused *before* it runs, as it was before ADR 0132.
ran_t2(Arity) ->
    Ran = fun() -> put(ran, true) end,
    case Arity of
        0 ->
            mark(fun(Acc) ->
                Ran(),
                {nil, Acc}
            end);
        1 ->
            mark(fun(_A, Acc) ->
                Ran(),
                {nil, Acc}
            end)
    end.

refused(Fun) ->
    erase(ran),
    try
        Fun(),
        ok
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind
    end.

plain_refuses_tier2_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a Tier 2 block to a plain entry raises stateful_block_dispatch unrun", fun() ->
            ?assertEqual(
                stateful_block_dispatch, refused(fun() -> ?MOD:'class_twice:'(nil, ran_t2(0)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"so does a forward through the plain entry", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> ?MOD:'class_twiceVia:'(nil, ran_t2(0)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"and the callable of a collection fold", fun() ->
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> ?MOD:'class_each:do:'(nil, [1, 2], ran_t2(1)) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"a side-effecting block of the wrong arity is an arity_mismatch, unrun", fun() ->
            %% One argument too many is not a Tier 2 block unless it carries
            %% the mark (BT-3885), so this is the arity mistake it is.
            Wrong = fun(_A) ->
                put(ran, true),
                1
            end,
            ?assertEqual(
                arity_mismatch, refused(fun() -> ?MOD:'class_twice:'(nil, Wrong) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"the refusal names the class method, not the value send", fun() ->
            try
                ?MOD:'class_twice:'(nil, ran_t2(0)),
                ?assert(false)
            catch
                error:#{error := #beamtalk_error{} = E} ->
                    ?assertMatch(
                        #beamtalk_error{
                            kind = stateful_block_dispatch,
                            class = 'ThreadedClassEntriesFixture class',
                            selector = 'twice:',
                            details = #{reason := plain_entry}
                        },
                        E
                    ),
                    ?assertEqual(
                        <<
                            "'twice:' on ThreadedClassEntriesFixture class cannot thread a "
                            "block that assigns captured variables through this send; "
                            "the block was not run"
                        >>,
                        E#beamtalk_error.message
                    )
            end,
            ?assertEqual(undefined, get(ran))
        end},
        {"a dynamic class send of a Tier 2 block raises and runs nothing", fun() ->
            Pid = beamtalk_class_registry:whereis_class(?CLASS),
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> beamtalk_class_dispatch:class_send(Pid, 'twice:', [ran_t2(0)]) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"a block of an impossible arity is an arity_mismatch, unrun", fun() ->
            Wrong = fun(_A, _B, _C) ->
                put(ran, true),
                1
            end,
            ?assertEqual(arity_mismatch, refused(fun() -> ?MOD:'class_twice:'(nil, Wrong) end)),
            ?assertEqual(undefined, get(ran))
        end},
        {"call_threaded/3 with the sentinel never answers a map", fun() ->
            ?assertEqual({3, ?PLAIN}, beamtalk_dispatch:call_threaded(fun() -> 3 end, [], ?PLAIN)),
            ?assertEqual(
                {3, ?PLAIN}, beamtalk_dispatch:call_threaded(fun(X) -> X end, [3], ?PLAIN)
            ),
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() -> beamtalk_dispatch:call_threaded(ran_t2(0), [], ?PLAIN) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"valueWithArguments: with a list gates arity like value:", fun() ->
            Two = fun(_A, _B) ->
                put(ran, true),
                1
            end,
            Three = fun(_A, _B, _C) ->
                put(ran, true),
                1
            end,
            ?assertEqual(
                {1, ?PLAIN}, beamtalk_dispatch:call_threaded_with_arguments(Two, [a, b], ?PLAIN)
            ),
            ?assertEqual(
                arity_mismatch,
                refused(fun() ->
                    beamtalk_dispatch:call_threaded_with_arguments(Three, [a, b], ?PLAIN)
                end)
            ),
            ?assertEqual(
                stateful_block_dispatch,
                refused(fun() ->
                    beamtalk_dispatch:call_threaded_with_arguments(mark(Three), [a, b], ?PLAIN)
                end)
            ),
            ?assertEqual(undefined, get(ran)),
            ?assertEqual(
                arity_mismatch,
                refused(fun() ->
                    beamtalk_dispatch:call_threaded_with_arguments(Two, [a, b, c, d], ?PLAIN)
                end)
            ),
            ?assertEqual(
                arity_mismatch,
                refused(fun() ->
                    beamtalk_dispatch:call_threaded_with_arguments(Two, [a, b, c, d], #{})
                end)
            )
        end},
        {"a real Acc map still threads a Tier 2 block", fun() ->
            ?assertEqual(
                {nil, #{}}, beamtalk_dispatch:call_threaded(ran_t2(0), [], #{})
            )
        end}
    ]}.

%%====================================================================
%% The threaded entry with a real Acc map: a block that is not Tier 2 is
%% never run as one (BT-3885)
%%====================================================================

%% A plain block that takes one argument more than it is called with has the
%% arity of a Tier 2 block. It must raise `arity_mismatch` *before* it runs
%% (it would see the caller's Acc as its argument), and whatever it would have
%% answered (an Erlang FFI `{ok, Map}` included) must never become the
%% caller's Acc.
map_acc_refuses_wrong_arity_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a side-effecting block one argument too many does not run", fun() ->
            Wrong = fun(_A) ->
                put(ran, true),
                1
            end,
            ?assertEqual(
                arity_mismatch,
                refused(fun() -> ?MOD:'__t2_class_twice:'(nil, Wrong, #{n => 1}) end)
            ),
            ?assertEqual(undefined, get(ran))
        end},
        {"a wrong-arity block answering {ok, Map} is not a Tier 2 reply", fun() ->
            Ffi = fun(_A) -> {ok, #{foreign => map}} end,
            ?assertEqual(
                arity_mismatch,
                refused(fun() -> ?MOD:'__t2_class_twice:'(nil, Ffi, #{n => 1}) end)
            )
        end},
        {"the same through the callable of a collection fold", fun() ->
            Wrong = fun(_X, _Y) ->
                put(ran, true),
                {ok, #{foreign => map}}
            end,
            ?assertEqual(
                arity_mismatch,
                refused(fun() -> ?MOD:'__t2_class_each:do:'(nil, [1, 2], Wrong, #{}) end)
            ),
            ?assertEqual(undefined, get(ran))
        end}
    ]}.

%%====================================================================
%% The mark codegen mints is the mark the gate reads (BT-3885)
%%====================================================================

compiled_mark_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a compiled Tier 2 block carries the mark; a pure block does not", fun() ->
            Writer = ?MOD:class_writerBlock(nil),
            ?assert(is_function(Writer, 1)),
            ?assert(beamtalk_dispatch:is_tier2_block(Writer)),
            Pure = ?MOD:'class_pureBlock:'(nil, 1),
            ?assert(is_function(Pure, 1)),
            ?assertNot(beamtalk_dispatch:is_tier2_block(Pure))
        end},
        {"a compiled Tier 2 block threads through the entry with a real Acc", fun() ->
            Writer = ?MOD:class_writerBlock(nil),
            ?assertEqual(
                {2, #{'__local__t' => 2, kept => yes}},
                ?MOD:'__t2_class_twice:'(nil, Writer, #{'__local__t' => 0, kept => yes})
            )
        end},
        {"a compiled pure block of one argument too many is refused unrun", fun() ->
            Pure = ?MOD:'class_pureBlock:'(nil, 1),
            ?assertEqual(
                arity_mismatch, refused(fun() -> ?MOD:'__t2_class_twice:'(nil, Pure, #{}) end)
            )
        end},
        {"a plain fun created inside a Tier 2 block is not marked", fun() ->
            Outer = ?MOD:class_nestingWriterBlock(nil),
            {Inner, _} = Outer(#{}),
            ?assert(is_function(Inner, 1)),
            ?assert(beamtalk_dispatch:is_tier2_block(Outer)),
            ?assertNot(beamtalk_dispatch:is_tier2_block(Inner))
        end},
        {"non-funs and external funs are not marked", fun() ->
            ?assertNot(beamtalk_dispatch:is_tier2_block(42)),
            ?assertNot(beamtalk_dispatch:is_tier2_block(fun maps:put/3))
        end}
    ]}.

%%====================================================================
%% A `^` block that flows into a use never answers a stale Acc
%% (BT-3871 review blocker 2)
%%====================================================================

bump(Acc) -> Acc#{count => maps:get(count, Acc, 0) + 1}.

%% If `Selector` had a threaded entry, a Tier 2 caller block that writes to its
%% Acc and then runs the method's `^` block would have to get its write back (it
%% did not: the throw carried the Acc the block was created at). With the fix
%% the method has no entry, and its plain entry answers as it always did.
nlr_case(Selector, EntryArgs, Expected, PlainArgs, PlainExpected) ->
    Entry = entry_fn(Selector),
    Arity = length(EntryArgs) + 2,
    case erlang:function_exported(?MOD, Entry, Arity) of
        true ->
            ?assertEqual(
                Expected, erlang:apply(?MOD, Entry, [nil | EntryArgs] ++ [#{count => 0}])
            );
        false ->
            ok
    end,
    no_entry(Selector, length(EntryArgs)),
    ?assertEqual(PlainExpected, erlang:apply(?MOD, plain_fn(Selector), [nil | PlainArgs])).

nlr_into_use_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"the review repro: find: hands the caller block a ^ block", fun() ->
            %% `[:k | count := count + 1. k value: 42]` as a Tier 2 fun.
            T2 = mark(fun(K, Acc) ->
                Acc1 = bump(Acc),
                {K(42), Acc1}
            end),
            nlr_case('find:', [T2], {42, #{count => 1}}, [fun(K) -> K(42) end], 42)
        end},
        {"a ^ block passed on through a forward", fun() ->
            T2 = mark(fun(K, Acc) ->
                Acc1 = bump(Acc),
                {K(6), Acc1}
            end),
            nlr_case('nlrForward:', [T2], {6, #{count => 1}}, [fun(K) -> K(6) end], 6)
        end},
        {"a ^ block as a fold's ifNone: handler", fun() ->
            T2 = mark(fun(_X, Acc) -> {false, bump(Acc)} end),
            nlr_case(
                'nlrFold:with:', [[1, 2], T2], {0, #{count => 2}}, [[1, 2], fun(_) -> false end], 0
            )
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
