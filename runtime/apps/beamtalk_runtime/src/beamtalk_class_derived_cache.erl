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
* `fetch/3` — the same, with a `Valid` check applied to a cached value
  before it is served (for facts that also depend on state this cache does
  not track, such as whether a module is loaded). A hit that fails it is
  recomputed and the entry overwritten.
* `inherits_from/2` — cached `beamtalk_class_registry:inherits_from/2`,
  the shared hierarchy helper for standalone hierarchy checks (BT-4065:
  class references, `isKindOf:`, `Result`). The Value send path does not
  call it: it memoises its Exception bit as part of its own
  `{tagged_class_dispatch, Class}` entry, so one hit answers everything.

Keys are caller-chosen terms; namespace them with a leading tag
(`{inherits_from, Class, Ancestor}`, `{tagged_class_dispatch, Class}`,
`{instance_method_info, Class, Selector}`, `{class_object, Class}`,
`{defining_method, Class, Selector}`, `{is_kind_of, Class, Behaviour}`,
`{class_method_owner, ClassPid, Selector}`,
`{inherited_target, Class, Selector}`, `{declared_fields, Class}`,
`{chain_has_user_new1, Class}`, `{chain_has_user_new0, Class}`,
`{direct_constructor, ClassPid, Selector}`).
`chain_has_user_new1` is whether an ancestor of `Class` defines a user
class-side `new:` (`beamtalk_class_metadata:chain_has_user_new1/1`, BT-4409);
`direct_constructor` is the dynamic `new`/`new:` verdict built on it
(`beamtalk_class_dispatch:lookup_direct_constructor/3`): the class, its tag
and the module whose `new/0`/`new/1` may build in the caller, or `error`,
before the per-send `direct_call_ok/2` and export checks.
`declared_fields` is the full declared field set (own, inherited and `late`
names) that `beamtalk_class_instantiation:check_init_args/5` validates a
`new:`/`spawnWith:` map against (BT-4281); only a completed superclass walk
is cached, never its `BaseState`-derived `not_found` fallback.
`instance_method_info` is `beamtalk_method_resolver:instance_method_info/2`,
the dispatch-time method lookup behind
`beamtalk_primitive:runtime_instance_method/2`; `class_object` is the
first-class class object `beamtalk_primitive:class_of_object_by_name/1`
answers, whose cached pid is re-validated on every hit; `defining_method`
is the send-path hierarchy resolution behind `beamtalk_dispatch:lookup/5`;
`is_kind_of` is `Object>>isKindOf:`'s decided answer for an instance of
`Class` (`beamtalk_behaviour_intrinsics:objectIsKindOf/2`, BT-4221), whose
cached pids are re-validated on every hit; `class_method_owner` is the
class-side twin of `defining_method`, the defining class and module a dynamic
class-side send runs in the caller (`beamtalk_class_dispatch:class_send/3`,
ADR 0142 §2), or its `decline` (cached too, flushed by the next class write).
The `instance_method_info` key caches
misses too (a selector no class in the chain defines, e.g. a DNU
or `respondsTo:` probe), so the table holds up to one row per class and
selector atom sent through that path; selectors are existing atoms, and the
next class write flushes them all. The `defining_method` key caches hits
only; a miss re-walks so errors keep their exact shape.
`inherited_target` (BT-4280, BT-4260) is the verdict a compiled class's
default dispatch arm asks first (`beamtalk_dispatch:inherited_target/2`):
`{compiled, Owner, Module}` when an inherited send may call the owner's
compiled `State`-threading method directly (an actor's `dispatch/4` arm),
`{value, Owner, Module}` when it may call the owner's state-less `dispatch/3`
directly (a value-type `dispatch/3` arm), else `slow`. All are cached (an
unregistered class is not), and a `compiled` hit re-checks the module's
`dispatch/4` export (a `value` hit needs no check; its remote call loads it).

## Invalidation

Every write that can change a class's `module`, `superclass` or published
method records — `beamtalk_class_metadata:insert/5` (class `init/1`),
`merge_identity/5` (hot reload, redefinition), instance-side
`put_method_records/3` (every `publish_reflection/1`: `init/1`, `put_method`,
reload), `put_reflection/2` (instance variables, which the `declared_fields`
memo reads; it invalidates on its own rather than relying on
`publish_reflection/1` calling `put_method_records/3` after it) and
`delete/1` (removal, `terminate/2`), plus the
(re)creation of the metadata table itself — calls `invalidate/0` *after* the
write has landed. So do the class-side writes the `class_method_owner` memo
reads: `set_runtime_class_methods/2` (an ADR 0084 fun's selector),
`put_class_variables/2` and `put_process_bound_selectors/2`, each on a change,
and the constructor facts `put_direct_constructors/2` and
`put_defines_user_class_new1/2` (BT-4409) and `put_defines_user_class_new0/2` (BT-4479). Extension install and removal do too: compiled
`has_method_local/1` consults the extension table, so an extension changes
resolution for every chain through its class.
Invalidation is global, not per class: a derived fact about
class C can depend on any of C's ancestors' rows, and class writes are rare
(load / reload time), so a global flush is both correct and cheap.

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

## Per-process memo (BT-4652)

`fetch/2,3` also keep a process-dictionary memo, `{Gen, #{Key => Value}}` under
one key, holding exactly the values the ETS layer holds (`cache` answers; a
`nocache` answer is never memoised). A hit costs the generation read and a
`get/1`. A `fetch/3` `Valid` check runs on memo hits too, so a purge needs no
generation bump: the keys that depend on code loading already carry a `Valid`.
A generation change orphans the whole map, and the next store replaces it
(stale entries never accumulate); a map reaching 4096 keys is dropped and
refilled. Without a generation (before `new/0`) nothing is memoised.

Facts that also depend on code-loading state (whether a module is loaded)
are not tracked by this invalidation; callers that cache such facts must
re-validate them on a hit with `fetch/3` (see
`beamtalk_primitive:tagged_class_dispatch/1`).

**Only the `beamtalk_class_metadata` API may write the metadata table.** A
raw `ets:insert`/`ets:delete` on it bypasses `invalidate/0`; code that must
do one (test fixtures restoring a snapshot) has to call `invalidate/0`
itself. The two tables are created together by `beamtalk_class_metadata:new/0`,
so they share an owner and heir and are lost together.
""".

-export([
    new/0,
    invalidate/0,
    fetch/2,
    fetch/3,
    generation/0,
    inherits_from/2
]).

%% `fetch/2` is a hot dispatch path (BT-4300's in-caller class send): inline
%% its `fetch/3` body and the trivial validity check.
-compile({inline, [fetch_valid/3]}).

-define(TABLE, beamtalk_class_derived_cache).
-define(GEN_KEY, {?MODULE, generation}).
-define(MEMO_KEY, {?MODULE, memo}).
%% Cap on one process's memo (keys are classes x key kinds, so this is far
%% above any real working set; it only bounds a pathological selector sweep).
-define(MEMO_MAX, 4096).

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
    fetch_valid(Key, Compute, always).

-doc """
`fetch/2` with a validity check on hits: a cached value for which
`Valid(Value)` is false is recomputed (and, if `Compute` says `cache`,
overwritten), so a stale entry is repaired once rather than re-derived on
every call. `Valid` is applied to cached values only, never to a value
`Compute` has just produced.
""".
-spec fetch(term(), fun(() -> {cache | nocache, Value}), fun((Value) -> boolean())) -> Value.
fetch(Key, Compute, Valid) ->
    fetch_valid(Key, Compute, Valid).

%% `fetch/3`, where `always` stands for a validity check that holds for every
%% value (`fetch/2`), so a hit calls no fun.
-spec fetch_valid(term(), fun(() -> {cache | nocache, Value}), fun((Value) -> boolean()) | always) ->
    Value.
fetch_valid(Key, Compute, Valid) ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined ->
            {_, Value} = Compute(),
            Value;
        Ref ->
            %% Read the generation before computing (see the moduledoc).
            Gen = atomics:get(Ref, 1),
            case memo_find(Gen, Key) of
                {ok, Value} when Valid =:= always ->
                    Value;
                {ok, Value} ->
                    case Valid(Value) of
                        true -> Value;
                        false -> compute_and_store(Key, Gen, Compute)
                    end;
                error ->
                    fetch_ets(Key, Gen, Compute, Valid)
            end
    end.

%% The per-process memo missed: read the ETS layer, and memoise a hit.
-spec fetch_ets(term(), integer(), fun(() -> {cache | nocache, Value}), fun((Value) -> boolean()) | always) ->
    Value.
fetch_ets(Key, Gen, Compute, Valid) ->
    try ets:lookup(?TABLE, Key) of
        [{_, Gen, Value}] when Valid =:= always ->
            memo_put(Gen, Key, Value);
        [{_, Gen, Value}] ->
            case Valid(Value) of
                true -> memo_put(Gen, Key, Value);
                false -> compute_and_store(Key, Gen, Compute)
            end;
        _ ->
            compute_and_store(Key, Gen, Compute)
    catch
        error:badarg -> element(2, Compute())
    end.

-doc """
The current cache generation, or `undefined` before `new/0`.

Every `invalidate/0` bumps it, so a caller that memoises a derived fact
somewhere cheaper than this module's ETS table (BT-4416: a per-process memo
on the `equals:` send path, where one ETS read is most of the cost) can tag
the memo with the generation read *before* computing it and treat a
mismatch as a miss.
""".
-spec generation() -> integer() | undefined.
generation() ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined -> undefined;
        Ref -> atomics:get(Ref, 1)
    end.

-doc """
Cached `beamtalk_class_registry:inherits_from/2`: true if `Class` is
`Ancestor` or a (transitive) subclass of it, per `beamtalk_class_metadata`.

Only answers for a registered `Class` (one with a superclass in its
metadata row) are cached, so probing arbitrary names cannot grow the table.
""".
-spec inherits_from(atom(), atom()) -> boolean().
inherits_from(Class, Ancestor) ->
    fetch({inherits_from, Class, Ancestor}, fun() ->
        Cacheable =
            case beamtalk_class_metadata:lookup_superclass(Class) of
                {ok, _} -> cache;
                not_found -> nocache
            end,
        {Cacheable, beamtalk_class_registry:inherits_from(Class, Ancestor)}
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
            memo_put(Gen, Key, Value);
        {nocache, Value} ->
            Value
    end.

%% The per-process memo (BT-4652): one process-dictionary entry holding the
%% generation it was filled under and a map of key to value. It holds exactly
%% what the ETS layer holds (values `Compute` marked `cache`), so a hit is a
%% generation read and a `get/1` instead of an ETS read that copies the entry.
%% A generation bump orphans the whole map, which the next `memo_put/3`
%% replaces rather than extends, so stale entries never accumulate; a map that
%% reaches `?MEMO_MAX` entries is dropped and refilled.
-spec memo_find(integer(), term()) -> {ok, term()} | error.
memo_find(Gen, Key) ->
    case get(?MEMO_KEY) of
        {Gen, Map} -> maps:find(Key, Map);
        _ -> error
    end.

-spec memo_put(integer(), term(), Value) -> Value.
memo_put(Gen, Key, Value) ->
    Map =
        case get(?MEMO_KEY) of
            {Gen, M} when map_size(M) < ?MEMO_MAX -> M;
            _ -> #{}
        end,
    put(?MEMO_KEY, {Gen, Map#{Key => Value}}),
    Value.

-spec ensure_generation() -> ok.
ensure_generation() ->
    case persistent_term:get(?GEN_KEY, undefined) of
        undefined ->
            Ref = atomics:new(1, [{signed, false}]),
            %% Masked so the shifted value always fits the unsigned 64-bit cell.
            atomics:put(Ref, 1, (erlang:unique_integer([positive]) band 16#FFFFFFFF) bsl 32),
            persistent_term:put(?GEN_KEY, Ref);
        _ ->
            ok
    end.
