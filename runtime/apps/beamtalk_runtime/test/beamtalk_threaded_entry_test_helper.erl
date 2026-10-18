%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_threaded_entry_test_helper).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared helpers for the ADR 0132 threaded-entry EUnit suites
(`beamtalk_threaded_class_entries_tests` for class methods,
`beamtalk_threaded_value_entries_tests` for value-type methods).

- `t2/2` builds a Tier 2 block (`fun(Args..., Acc) -> {Result, Acc1}`) that logs
  its arguments in `Acc` under `log`; `t1/2` the matching Tier 1 block, which
  logs them in the process dictionary.
- `differential/6` runs one method through its plain function with the Tier 1
  block and through its threaded entry with the Tier 2 block and a hand-packed
  `Acc`, and asserts the two agree on the result and the invocation log, and
  that a key of `Acc` the block never touched survives.
- `ran_t2/1` and `refused/1` check that a Tier 2 block handed to a plain entry
  is refused *before* it runs.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([t2/2, t1/2, differential/6, ran_t2/1, refused/1]).

-doc "A Tier 2 block of user arity N that logs its arguments in `Acc`.".
-spec t2(0..2, function()) -> function().
t2(0, F) -> fun(Acc) -> step(F, [], Acc) end;
t2(1, F) -> fun(A, Acc) -> step(F, [A], Acc) end;
t2(2, F) -> fun(A, B, Acc) -> step(F, [A, B], Acc) end.

step(F, Args, Acc) ->
    {erlang:apply(F, Args), Acc#{log => maps:get(log, Acc, []) ++ [Args]}}.

-doc "The matching Tier 1 block, logging its arguments in the process dictionary.".
-spec t1(0..2, function()) -> function().
t1(0, F) -> fun() -> note(F, []) end;
t1(1, F) -> fun(A) -> note(F, [A]) end;
t1(2, F) -> fun(A, B) -> note(F, [A, B]) end.

note(F, Args) ->
    put(log, get(log) ++ [Args]),
    erlang:apply(F, Args).

insert_at(0, X, Rest) -> [X | Rest];
insert_at(N, X, [H | T]) -> [H | insert_at(N - 1, X, T)].

-doc """
Runs `Mod:PlainFn(Receiver, Args...)` with a pure block and
`Mod:EntryFn(Receiver, Args..., Acc)` with a Tier 2 block, the block at
`BlockPos` among `Others` (the non-block arguments, in order), asserts they agree
and answers `{Result, BlockLog, Acc1}`.
""".
-spec differential(
    {module(), atom(), atom()}, term(), list(), non_neg_integer(), 0..2, function()
) -> {term(), list(), map()}.
differential({Mod, PlainFn, EntryFn}, Receiver, Others, BlockPos, BlockArity, F) ->
    put(log, []),
    PlainArgs = insert_at(BlockPos, t1(BlockArity, F), Others),
    Plain = erlang:apply(Mod, PlainFn, [Receiver | PlainArgs]),
    PlainLog = erase(log),
    EntryArgs = insert_at(BlockPos, t2(BlockArity, F), Others),
    {Result, Acc1} = erlang:apply(
        Mod, EntryFn, [Receiver | EntryArgs] ++ [#{log => [], untouched => kept}]
    ),
    ?assertEqual(Plain, Result),
    ?assertEqual(PlainLog, maps:get(log, Acc1)),
    ?assertEqual(kept, maps:get(untouched, Acc1)),
    {Result, PlainLog, Acc1}.

-doc "A Tier 2 block of arity N that records in the process dictionary that it ran.".
-spec ran_t2(0..1) -> function().
ran_t2(0) ->
    fun(Acc) ->
        put(ran, true),
        {nil, Acc}
    end;
ran_t2(1) ->
    fun(_A, Acc) ->
        put(ran, true),
        {nil, Acc}
    end.

-doc "Runs `Fun`, answering `ok` or the kind of the structured error it raised.".
-spec refused(fun(() -> term())) -> ok | atom().
refused(Fun) ->
    erase(ran),
    try
        Fun(),
        ok
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind
    end.
