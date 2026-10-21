%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_derived_cache).

%%% **DDD Context:** Object System Context

-moduledoc """
Memo table for facts *derived* from `beamtalk_class_metadata` rows: answers
that are a pure function of the class table but cost more than one ETS read
to compute (a superclass-chain walk, a CamelCase → snake_case module-name
conversion), and sit on hot dispatch paths.

Added for BT-4064: a dynamic send to a tagged-map Value instance used to
rebuild the class's module name from strings and walk its superclass chain
(to rule out `Exception`) on **every** send, ~1 µs per getter. With this
cache the steady-state cost is one `persistent_term`/`atomics` read and one
ETS read.

## API

* `fetch/2` — generic memo: `fetch(Key, Compute)` returns the cached value
  for `Key`, or runs `Compute()` and caches its result.
* `inherits_from/2` — cached `beamtalk_class_registry:inherits_from/2`,
  the shared hierarchy helper (e.g. the Exception check on the Value send
  path; class-reference / `isKindOf:` checks can reuse it).

Keys are caller-chosen terms; namespace them with a leading tag
(`{inherits_from, Class, Ancestor}`, `{tagged_class_dispatch, Class}`).

## Invalidation

Every write that can change a class's `module` or `superclass` —
`beamtalk_class_metadata:insert/5` (class `init/1`), `merge_identity/5` (hot
reload, redefinition) and `delete/1` (removal, `terminate/2`), plus the
(re)creation of the metadata table itself — calls `invalidate/0` *after*
the write has landed. Invalidation is global, not per class: a derived fact
about class C can depend on any of C's ancestors' rows, and class writes are
rare (load / reload time), so a global flush is both correct and cheap.

Correctness against concurrent readers comes from a generation counter, not
from the flush. A reader takes the generation *before* it reads the
metadata it derives from and stores the entry tagged with that generation; a
hit requires the stored generation to equal the current one. If a write
lands while a reader is computing, the writer's bump makes the reader's
entry stale on arrival, so a computation that saw pre-write data can never
be served after the write. The `ets:delete_all_objects/1` in `invalidate/0`
only reclaims memory.

The counter is an `atomics` ref held in `persistent_term` (written once, at
first `new/0`; never updated, so no global GC). Its initial value is a
node-unique multiple of 2^32, so in the (start-up only) race where two
processes both create it, entries tagged under the losing counter can never
match the winning one.

Facts that also depend on code-loading state (whether a module is loaded)
are not tracked by this invalidation; callers that cache such facts must
re-validate them on a hit (see `beamtalk_primitive:tagged_class_dispatch/1`).
""".

-export([
    new/0,
    invalidate/0,
    fetch/2,
    inherits_from/2
]).

-define(TABLE, beamtalk_class_derived_cache).
-define(GEN_KEY, {?MODULE, generation}).

-doc """
Ensure the cache table and generation counter exist (idempotent).

Called from `beamtalk_class_metadata:new/0`, so the table is created by the
same process, at the same time, and with the same heir as the metadata table.
""".
-spec new() -> ok.
new() ->
    ensure_generation(),
    case ets:info(?TABLE, owner) of
        undefined ->
            try
                _ = ets:new(
                    ?TABLE,
                    [set, public, named_table, {read_concurrency, true}] ++
                        beamtalk_ets_heir:heir_option()
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            beamtalk_ets_heir:maybe_set_heir(?TABLE),
            ok
    end.

-doc """
Invalidate every cached entry. Call *after* the metadata write that makes
them stale has landed (see the moduledoc).
""".
-spec invalidate() -> ok.
invalidate() ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined ->
            %% No counter yet: nothing can have been cached against it.
            ok;
        Ref ->
            atomics:add(Ref, 1, 1),
            try
                ets:delete_all_objects(?TABLE),
                ok
            catch
                error:badarg -> ok
            end
    end.

-doc """
Return the cached value for `Key`, computing it with `Compute` on a miss.

`Compute` returns `{cache, Value}` to have `Value` stored, or
`{nocache, Value}` for an answer that must not be memoised (e.g. one derived
from a class that has no metadata row yet, whose later registration is the
event that would invalidate it — but whose current answer came from the code
path, not the class table). Without the cache table or counter (before
`new/0`, or mid-teardown) every call computes.
""".
-spec fetch(term(), fun(() -> {cache | nocache, Value})) -> Value.
fetch(Key, Compute) ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined ->
            {_, Value} = Compute(),
            Value;
        Ref ->
            %% Read the generation before computing (see the moduledoc).
            Gen = atomics:get(Ref, 1),
            try ets:lookup(?TABLE, Key) of
                [{_, Gen, Value}] -> Value;
                _ -> compute_and_store(Key, Gen, Compute)
            catch
                error:badarg -> element(2, Compute())
            end
    end.

-doc """
Cached `beamtalk_class_registry:inherits_from/2`: true if `Class` is
`Ancestor` or a (transitive) subclass of it, per `beamtalk_class_metadata`.

An unregistered class answers `false`, and that `false` is cached too: the
class's registration writes its metadata row, which invalidates the cache.
""".
-spec inherits_from(atom(), atom()) -> boolean().
inherits_from(Class, Ancestor) ->
    fetch({inherits_from, Class, Ancestor}, fun() ->
        {cache, beamtalk_class_registry:inherits_from(Class, Ancestor)}
    end).

%%====================================================================
%% Internal
%%====================================================================

-spec compute_and_store(term(), integer(), fun(() -> {cache | nocache, Value})) -> Value.
compute_and_store(Key, Gen, Compute) ->
    case Compute() of
        {cache, Value} ->
            try
                ets:insert(?TABLE, {Key, Gen, Value})
            catch
                error:badarg -> ok
            end,
            Value;
        {nocache, Value} ->
            Value
    end.

-spec ensure_generation() -> ok.
ensure_generation() ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined ->
            Ref = atomics:new(1, [{signed, false}]),
            atomics:put(Ref, 1, erlang:unique_integer([positive]) bsl 32),
            persistent_term:put(?GEN_KEY, Ref);
        _ ->
            ok
    end.
