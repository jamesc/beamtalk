%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_map).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime helper operations for Dictionary (Erlang maps).

Complex Dictionary operations that cannot be inlined as
direct BIF calls in generated Core Erlang. Called from compiled
stdlib module bt@stdlib@dictionary.

Replaces the former beamtalk_map.erl hand-written dispatch module.
""".

-export([
    at_if_absent/3,
    at_if_present/3,
    at_if_present_if_absent/4,
    do/2,
    do_with_key/2,
    includes/2,
    lookup_for_threading/3,
    pairs_for_threading/2,
    print_string/1
]).

-doc "Get value at key, or evaluate block if absent.".
-spec at_if_absent(map(), term(), fun(() -> term())) -> term().
at_if_absent(Map, Key, Block) when is_function(Block, 0) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Block()
    end.

-doc "Evaluate block with the value at key if present, else answer nil.".
-spec at_if_present(map(), term(), fun((term()) -> term())) -> term().
at_if_present(Map, Key, Block) when is_function(Block, 1) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Key, Map) of
        {ok, Value} -> Block(Value);
        error -> nil
    end.

-doc "Evaluate present block with the value at key, or absent block if missing.".
-spec at_if_present_if_absent(map(), term(), fun((term()) -> term()), fun(() -> term())) ->
    term().
at_if_present_if_absent(Map, Key, PresentBlock, AbsentBlock) when
    is_function(PresentBlock, 1), is_function(AbsentBlock, 0)
->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Key, Map) of
        {ok, Value} -> PresentBlock(Value);
        error -> AbsentBlock()
    end.

-doc """
The receiver guard of an inlined `at:ifAbsent:`, `at:ifPresent:` or
`at:ifPresent:ifAbsent:` whose handler writes an outer local (ADR 0131
Phase 2d, BT-3798; BT-3824).

Compiled code runs the handler inline, in the caller's activation, so the
write can be threaded back. That is only valid when the primitive itself would
have run it: a `Dictionary` (an untagged map). Any other receiver would have
dispatched to its own method of that name with a closure, which cannot return the
write, so it raises a structured `type_error` instead of dropping it.

A value-type instance is also a map, but tagged with `'$beamtalk_class'`; it is
not a `Dictionary` and takes the error branch. `Selector` is the lookup
selector, for the error only.
""".
-spec lookup_for_threading(term(), atom(), term()) -> {ok, term()} | error.
lookup_for_threading(Receiver, Selector, Key) when is_map(Receiver) ->
    case beamtalk_primitive:is_object(Receiver) of
        false ->
            maps:find(Key, Receiver);
        true ->
            unsupported_receiver(Receiver, Selector)
    end;
lookup_for_threading(Receiver, Selector, _Key) ->
    unsupported_receiver(Receiver, Selector).

-doc """
The receiver guard of an inlined `keysAndValuesDo:` / `doWithKey:` whose block
touches outer state, a local, a field or a self-send (ADR 0131, BT-3841): the `{Key, Value}` pairs of a
`Dictionary` (an untagged map), for the compiled fold to iterate in the
caller's activation.

Any other receiver would have run its own method of that name with a closure
that cannot return the write (`Ets keysAndValuesDo:`, a user class's
`doWithKey:`), and iterating its tagged map would walk the wrong entries, so
a tagged map (an `Ets` handle, a value-type instance) or a non-map raises the
structured `type_error` that `lookup_for_threading/3` raises. `Selector` is
the iteration selector, for the error only.
""".
-spec pairs_for_threading(term(), atom()) -> [{term(), term()}].
pairs_for_threading(Receiver, Selector) when is_map(Receiver) ->
    case beamtalk_primitive:is_object(Receiver) of
        false -> maps:to_list(Receiver);
        true -> unsupported_receiver(Receiver, Selector)
    end;
pairs_for_threading(Receiver, Selector) ->
    unsupported_receiver(Receiver, Selector).

-spec unsupported_receiver(term(), atom()) -> no_return().
unsupported_receiver(Receiver, Selector) ->
    beamtalk_error:raise_type_error(
        beamtalk_primitive:class_of(Receiver),
        Selector,
        <<
            "The block touches state of the enclosing method (a local, a field or a self-send), and "
            "only a Dictionary receiver can run it inline; compute the new value "
            "without writing it from the block"
        >>
    ).

-doc "Iterate over all values in the dictionary.".
-spec do(map(), fun((term()) -> term())) -> 'nil'.
do(Map, Block) when is_function(Block, 1) ->
    maps:foreach(fun(_K, V) -> Block(V) end, Map),
    nil.

-doc "Iterate over all key-value pairs, calling block with key and value.".
-spec do_with_key(map(), fun((term(), term()) -> term())) -> 'nil'.
do_with_key(Map, Block) when is_function(Block, 2) ->
    maps:foreach(Block, Map),
    nil.

-doc "Test if the dictionary contains the given value.".
-spec includes(map(), term()) -> boolean().
includes(Map, Value) ->
    %% This searches *values*, which is a linear scan, so it honours an
    %% `equals:` override. Dictionary *keys* are Erlang map keys and cannot —
    %% their identity is decided in the VM.
    beamtalk_equality:member(Value, maps:values(Map)).

-doc """
Format a dictionary as Beamtalk syntax: #{key => value, ...}
Used by Dictionary>>printString and REPL display.
""".
-spec print_string(map()) -> binary().
print_string(Map) ->
    %% Strip the $beamtalk_class tag if present (plain dictionaries don't have it)
    PlainMap = maps:remove('$beamtalk_class', Map),
    Pairs = maps:fold(
        fun(K, V, Acc) ->
            KeyStr = beamtalk_primitive:print_string(K),
            ValStr = beamtalk_primitive:print_string(V),
            [iolist_to_binary([KeyStr, <<" => ">>, ValStr]) | Acc]
        end,
        [],
        PlainMap
    ),
    SortedPairs = lists:sort(Pairs),
    iolist_to_binary([<<"#{">>, lists:join(<<", ">>, SortedPairs), <<"}">>]).
