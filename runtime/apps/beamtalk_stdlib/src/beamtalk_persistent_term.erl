%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_persistent_term).

%%% **DDD Context:** Object System Context

-moduledoc """
Native module behind the `PersistentTerm` stdlib class (ADR 0153 §3).

A thin facade over OTP `persistent_term`. Every user key is stored under
`{beamtalk_persistent_term, Key}`, so it cannot collide with the runtime's own
`persistent_term` keys. This module is the only place that builds that tuple.

A read copies nothing. Replacing or erasing a value forces a global GC, so use
it for values built once and read many times.

## Functions

| Function        | Description                                              |
|-----------------|----------------------------------------------------------|
| `getOrPut/2`    | Stored value, or run the block, store and answer it      |
| `getOrElse/2`   | Stored value, or the block's value (stores nothing)      |
| `includesKey/1` | Whether a value is stored                                |
| `erase/1`       | `true` if a value was removed (forces a global GC)       |

## Refused values

On a miss, a value that is itself a pid, an actor or a class is refused with
a `type_error`: a process identity outlives its process. A process identity
nested inside a larger value is not detected.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([getOrPut/2, getOrElse/2, includesKey/1, erase/1]).

-doc """
Answer the value stored under `Key`. On a miss, run `Block`, read again, and
store the result unless another process stored first (the first value wins).

A raising block stores nothing. The pid/actor/class check runs only on a miss.
""".
-spec getOrPut(term(), fun(() -> term())) -> term().
getOrPut(Key, Block) when is_function(Block, 0) ->
    Miss = make_ref(),
    case persistent_term:get(pt_key(Key), Miss) of
        Miss ->
            Value = Block(),
            ok = refuse_process_identity(Key, Value),
            %% Read again: another process may have stored while the block ran.
            %% Keep the first value so a racing second store is not a global GC.
            case persistent_term:get(pt_key(Key), Miss) of
                Miss ->
                    persistent_term:put(pt_key(Key), Value),
                    Value;
                Stored ->
                    Stored
            end;
        Stored ->
            Stored
    end;
getOrPut(_Key, _Block) ->
    beamtalk_error:raise_type_error(
        'PersistentTerm', 'at:ifAbsentPut:', <<"Second argument must be a Block">>
    ).

-doc "Answer the stored value, or the result of `Block`. Stores nothing.".
-spec getOrElse(term(), fun(() -> term())) -> term().
getOrElse(Key, Block) when is_function(Block, 0) ->
    Miss = make_ref(),
    case persistent_term:get(pt_key(Key), Miss) of
        Miss -> Block();
        Stored -> Stored
    end;
getOrElse(_Key, _Block) ->
    beamtalk_error:raise_type_error(
        'PersistentTerm', 'at:ifAbsent:', <<"Second argument must be a Block">>
    ).

-doc "Whether a value is stored under `Key` (a stored `nil` counts).".
-spec includesKey(term()) -> boolean().
includesKey(Key) ->
    Miss = make_ref(),
    persistent_term:get(pt_key(Key), Miss) =/= Miss.

-doc "Remove the value under `Key`. Answers `true` if one was removed. Forces a global GC.".
-spec erase(term()) -> boolean().
erase(Key) ->
    persistent_term:erase(pt_key(Key)).

%%% ============================================================================
%%% Internal
%%% ============================================================================

-spec pt_key(term()) -> {beamtalk_persistent_term, term()}.
pt_key(Key) -> {?MODULE, Key}.

-spec refuse_process_identity(term(), term()) -> ok.
refuse_process_identity(Key, Value) ->
    case Value of
        _ when is_pid(Value) -> refuse(Key, <<"a pid">>);
        #beamtalk_object{} -> refuse(Key, <<"an actor or a class">>);
        _ -> ok
    end.

-spec refuse(term(), binary()) -> no_return().
refuse(Key, What) ->
    Hint = iolist_to_binary([
        <<"Cannot store ">>,
        What,
        <<" under key ">>,
        io_lib:format("~tp", [Key]),
        <<": a process or actor reference (a registered one included) is refused. ">>,
        <<"Store the registered name as a Symbol and look the process up instead.">>
    ]),
    beamtalk_error:raise(
        beamtalk_error:with_details(
            beamtalk_error:new(type_error, 'PersistentTerm', 'at:ifAbsentPut:', Hint),
            #{key => Key}
        )
    ).
