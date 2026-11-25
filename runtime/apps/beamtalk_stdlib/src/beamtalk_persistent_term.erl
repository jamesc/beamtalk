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
-export([lock_key/1]).

%% Default bound on waiting behind another process's build of the same key.
%% Override with the `beamtalk_runtime` env
%% `{fifo_lock_timeout_ms, #{persistent_term => Ms}}` (tests shrink it).
-define(BUILD_WAIT_MS, 60000).

-doc """
Answer the value stored under `Key`. On a miss, take the node-local lock for
`Key`, read again, and only if it is still missing run `Block` and store the
result. Every caller answers the one stored value (the first value wins).

The miss path is serialised per key through `beamtalk_fifo_lock` (BT-5191):
a second process that misses while the first is building waits for it and
then answers the stored value without running its own block, so the block
runs once and there is no replacing `put` (no global GC). Hits take no lock.
A raising block stores nothing and releases the lock; if the building process
dies, the next waiter builds. A block that calls `getOrPut/2` on its own key
raises a structured `runtime_error` (it would wait for itself). A wait is
bounded (60 s by default): a build that needs another process which itself
waits on the same key (a cycle the lock cannot see) raises a `runtime_error`
in the waiter instead of hanging both.

A raising block stores nothing. The pid/actor/class check runs only on a miss.
""".
-spec getOrPut(term(), fun(() -> term())) -> term().
getOrPut(Key, Block) when is_function(Block, 0) ->
    Miss = make_ref(),
    case persistent_term:get(pt_key(Key), Miss) of
        Miss -> build_locked(Key, Block, Miss);
        Stored -> Stored
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

-doc """
The `beamtalk_fifo_lock` key that serialises misses on `Key` (exported for
tests). The key is encoded with `term_to_binary/2` (`deterministic`, so `=:=` keys
holding maps encode alike) because the lock table
orders keys with `==`: `1` and `1.0` are distinct `persistent_term` keys and
must not share one lock.
""".
-spec lock_key(term()) -> {beamtalk_persistent_term, binary()}.
lock_key(Key) -> {?MODULE, term_to_binary(Key, [deterministic])}.

%% The miss path of `getOrPut/2`, run under the per-key lock so the check,
%% the block and the store are one step for every other caller of this key.
-spec build_locked(term(), fun(() -> term()), reference()) -> term().
build_locked(Key, Block, Miss) ->
    PtKey = pt_key(Key),
    LockKey = lock_key(Key),
    Timeout = beamtalk_fifo_lock:configured_timeout(persistent_term, ?BUILD_WAIT_MS),
    %% bt-catcher-audit: not-applicable-reraises - the only clause turns this call's own recursive_lock into a raised beamtalk_error; block errors propagate untouched
    try
        beamtalk_fifo_lock:with_lock_or_raise(
            LockKey, Timeout, {'PersistentTerm', 'at:ifAbsentPut:'}, fun() ->
                %% Read again under the lock: the holder before us may have stored.
                case persistent_term:get(PtKey, Miss) of
                    Miss ->
                        Value = Block(),
                        ok = refuse_process_identity(Key, Value),
                        %% The block itself may have stored (a raw
                        %% `persistent_term:put/2`); keep that value rather
                        %% than replace it, since a replace is a global GC.
                        case persistent_term:get(PtKey, Miss) of
                            Miss ->
                                persistent_term:put(PtKey, Value),
                                Value;
                            Stored ->
                                Stored
                        end;
                    Stored ->
                        Stored
                end
            end
        )
    catch
        error:{recursive_lock, LockKey} ->
            beamtalk_error:raise(
                beamtalk_error:with_details(
                    beamtalk_error:new(
                        runtime_error,
                        'PersistentTerm',
                        'at:ifAbsentPut:',
                        <<"The ifAbsentPut: block asked for its own key, which it is still building.">>
                    ),
                    #{key => Key}
                )
            )
    end.

-spec refuse_process_identity(term(), term()) -> ok.
refuse_process_identity(Key, Value) ->
    case Value of
        _ when is_pid(Value) -> refuse(Key, <<"a pid">>);
        #beamtalk_object{} -> refuse(Key, <<"an actor or a class">>);
        _ -> ok
    end.

-spec refuse(term(), binary()) -> no_return().
refuse(Key, What) ->
    %% chars_to_binary, not iolist_to_binary: `~tp` of a key holding a code
    %% point above 255 is not an iolist (BT-5191).
    Hint = beamtalk_error:chars_to_binary([
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
