%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_protocol_registry).

%%% **DDD Context:** Object System Context

-moduledoc """
Runtime protocol registry and query engine (ADR 0068 Phase 2c).

Provides an ETS-based registry of protocol definitions and runtime query
functions for protocol introspection. Protocol metadata is registered by
compiled modules during `on_load` via `register_protocol/1`.

## Protocol Registration

Each protocol is stored as a map:
```erlang
#{name => 'Printable',
  module => 'bt@stdlib@printable',
  required_methods => [#{'selector' => 'asString', 'arity' => 0}],
  type_params => [],
  extending => undefined}
```

The `module` key records the BEAM module the protocol was defined in (BT-2615).
A protocol class object is dispatched by the shared `beamtalk_protocol_object`
module, so this is the only place that retains the protocol's true origin —
the System Browser reads it to badge a protocol stdlib vs project.

## Query API

| Function                    | Description                                    |
|-----------------------------|------------------------------------------------|
| conforms_to/2               | Check if class conforms to protocol             |
| protocols_for_class/1       | List protocols a class conforms to              |
| required_methods/1          | Required method selectors for a protocol        |
| conforming_classes/1        | Classes conforming to a protocol                |
| protocol_info/1             | Full protocol metadata                          |
| is_protocol/1               | Check if a name is a registered protocol        |

## Conformance Model

Runtime conformance uses structural checking — a class conforms if it
responds to all required selectors. This mirrors the compile-time
`ProtocolRegistry::check_conformance` but operates on live runtime data
via `beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`.

See also: docs/ADR/0068-parametric-types-and-protocols.md — Stage 2
See also: beamtalk_behaviour_intrinsics — backs the class-side primitives

## Conformance Cache (BT-3222)

`conforms_to/2` is a structural check: for every required selector it walks
the ancestor chain via `classCanUnderstandFromName/2`, which is a
`gen_server:call` per level — measured at 59-135 µs/call on a live 109-class
workspace (`docs/internal/adr-0115-phase1-spike-findings.md` §3), four orders
of magnitude above the ETS lookups around it. Results are cached in a second
ETS table (`beamtalk_protocol_conforms_cache`) keyed by `{ClassName,
ProtocolName}`, so a repeated pair is an ETS lookup with no `gen_server:call`
at all.

Invalidation is a conservative whole-cache flush rather than fine-grained
per-class/per-protocol tracking — conformance depends on the *transitive*
method set, so a `put_method` on an ancestor invalidates every descendant's
conformance, not just the class it was called on; proving a fine-grained
scheme sound is materially harder than proving "flush everything" sound.

The flush is a monotonic **generation counter**, not `ets:delete_all_objects/1`
on the results themselves: each cache entry is stamped `{Result, Gen}` with
the generation sampled *before* `compute_conforms_to/2` runs, and
`invalidate_conforms_cache/0` bumps a single counter rather than clearing
rows. `cache_lookup/1` only treats an entry as a hit if its stamped `Gen`
still equals the *current* counter. This closes a lost-invalidation race a
row-clearing flush cannot: `compute_conforms_to/2` is a multi-`gen_server:call`
walk that can take milliseconds (see the timings below), so a flush that
lands *during* an in-flight compute — e.g. `beamtalk_xref:compute_relatedness/3`
calling `conforms_to/2` from inside hot-reload's own `senders_of/2` recompute,
concurrently with the very mutation that triggers the flush — must not let
that compute's now-stale result land in the cache *after* the flush already
ran. Clearing rows can't prevent that: the write simply happens after the
clear. Stamping the generation *before* the compute and checking it *after*
means a bump anywhere in between is always visible to the reader: the stored
generation is behind, so it's a permanent miss for that entry, not a
resurrected stale value (BT-3222 review round 2). A repeated `{ClassName,
ProtocolName}` pair's entry is overwritten in place, never duplicated, so
the table doesn't grow from re-querying the same pair across generations —
but unlike the old whole-table flush, a bump never removes rows for a pair
that stops being queried (e.g. a class renamed or removed mid-session), so
those linger until that exact name is queried again. Unbounded in principle
over a long, churny hot-reload session with ever-fresh class names; accepted
because real class/protocol names are near-universally reused, not
uniquely generated per reload. Bumped on:

- `register_protocol/1` / `unregister_protocol/1` (this module)
- class registration and hot reload — `beamtalk_object_class:init/1` and its
  `{update_class, _}` handler (a class's method set can change on either)
- live method patches — `beamtalk_object_class`'s `{put_method, ...}` and
  `{put_class_method, ...}` handlers (hot-patch / `classRemoveSelector` on a
  local method routes through `update_class`, already covered above)
- class removal — `beamtalk_class_lifecycle:class_removed/2` calls
  `unregister_protocol/1` unconditionally, which flushes regardless of
  whether the removed class's module defined any protocols itself

Every cache read/write/flush is `ets:info/1`-guarded and try/catch-wrapped
around `badarg`, matching every other table in this module — the cache is
simply skipped (never crashes a caller) if the table is absent or torn down
concurrently (e.g. during test teardown).
""".

-include_lib("kernel/include/logger.hrl").

-export([
    init/0,
    register_protocol/1,
    unregister_protocol/1,
    conforms_to/2,
    protocols_for_class/1,
    required_methods/1,
    conforming_classes/1,
    protocol_info/1,
    is_protocol/1,
    all_protocol_names/0,
    invalidate_conforms_cache/0
]).

-define(PROTOCOL_TABLE, beamtalk_protocol_registry).
-define(CONFORMS_CACHE_TABLE, beamtalk_protocol_conforms_cache).
%% Generation-counter row inside ?CONFORMS_CACHE_TABLE (BT-3222). Not a valid
%% `{atom(), atom()}` cache key shape, so it can never collide with a real
%% `{ClassName, ProtocolName}` entry in the same `set` table.
-define(CONFORMS_CACHE_GEN_KEY, '$conforms_cache_generation').

%%% ============================================================================
%%% Initialization
%%% ============================================================================

-doc """
Initialize the protocol registry ETS table.

Called during application startup (beamtalk_runtime_app:start/2) before
any compiled modules load their protocol definitions.

BT-3105: Carries `beamtalk_class_registry:heir_option/0` (the runtime
supervisor, once it is alive) so an owner crash hands the table off instead
of destroying every registered protocol.

BT-3222: Also creates the `conforms_to/2` result cache table (see this
module's "Conformance Cache" doc), heir-protected the same way.
""".
-spec init() -> ok.
init() ->
    ensure_protocol_table(),
    ensure_conforms_cache_table(),
    ok.

-doc "Idempotently create the protocol definitions ETS table.".
-spec ensure_protocol_table() -> ok.
ensure_protocol_table() ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            ets:new(?PROTOCOL_TABLE, [
                named_table,
                set,
                public,
                {read_concurrency, true}
                | beamtalk_class_registry:heir_option()
            ]),
            ok;
        _ ->
            %% Table already exists (e.g., re-init after hot reload)
            ok
    end.

-doc """
Idempotently create the `conforms_to/2` result cache ETS table (BT-3222).

Keyed by `{ClassName, ProtocolName}` -> `boolean()`. Separate from
`?PROTOCOL_TABLE` so a whole-cache flush (`invalidate_conforms_cache/0`) never
touches registered protocol definitions.
""".
-spec ensure_conforms_cache_table() -> ok.
ensure_conforms_cache_table() ->
    case ets:info(?CONFORMS_CACHE_TABLE) of
        undefined ->
            ets:new(?CONFORMS_CACHE_TABLE, [
                named_table,
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
                | beamtalk_class_registry:heir_option()
            ]),
            ok;
        _ ->
            ok
    end.

%%% ============================================================================
%%% Registration
%%% ============================================================================

-doc """
Register a protocol definition.

Called from compiled module `on_load` callbacks when a module defines
protocols. The `Info` map must contain:
- `name` (atom): Protocol name (e.g., 'Printable')
- `required_methods` (list of maps): Each with `selector` (atom) and `arity` (integer)
- `type_params` (list of atoms): Type parameter names, or `[]`
- `extending` (atom or `undefined`): Parent protocol name

It may also carry (BT-2615):
- `module` (atom): the BEAM module the protocol was defined in (e.g.
  `bt@stdlib@printable`), used to resolve the protocol class object's origin.

Duplicate registrations overwrite the previous entry (idempotent for hot reload).
""".
-spec register_protocol(map()) -> ok.
register_protocol(#{name := Name} = Info) ->
    ets:insert(?PROTOCOL_TABLE, {Name, Info}),
    %% BT-3222: A (re-)registered protocol can change required_methods /
    %% extending for Name, so every cached conforms_to/2 result naming it —
    %% for any class — is potentially stale.
    invalidate_conforms_cache(),
    ?LOG_DEBUG(
        "Registered protocol ~p",
        [Name],
        #{domain => [beamtalk, runtime]}
    ),
    maybe_create_protocol_class(Name, Info),
    ok;
register_protocol(BadInfo) ->
    ?LOG_WARNING(
        "Invalid protocol registration (missing 'name' key): ~p",
        [BadInfo],
        #{domain => [beamtalk, runtime]}
    ),
    ok.

-doc """
Unregister every protocol whose `module` metadata field matches `Module`
(BT-3105).

Called from `beamtalk_class_lifecycle:class_removed/2` when the class
defining a protocol is removed from the system — until now there was no
unregister path at all, so a protocol whose defining module was purged
stayed registered (and conformance-checkable) forever. A no-op when the
table has not been initialised, or when no registered protocol carries a
`module` field matching `Module` (protocols registered before BT-2615 have
no `module` field and are never matched).

Does not tear down the protocol's own sealed class object (created by
`maybe_create_protocol_class/2`) — that is a separate class removal, out of
scope here; this only purges the registry row so `is_protocol/1` and
`conforms_to/2` stop seeing it.
""".
-spec unregister_protocol(atom()) -> ok.
unregister_protocol(Module) when is_atom(Module) ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            ok;
        _ ->
            _ = ets:select_delete(?PROTOCOL_TABLE, [
                {{'_', #{module => '$1'}}, [{'=:=', '$1', {const, Module}}], [true]}
            ]),
            ok
    end,
    %% BT-3222: Unconditional, not just on an actual match — this is also the
    %% single call `beamtalk_class_lifecycle:class_removed/2` makes for every
    %% class removal, and a removed class's conformance results (as well as
    %% every descendant re-walked through it) must not survive as stale
    %% cache entries even when the removed class's own module defined no
    %% protocol.
    invalidate_conforms_cache().

%%% ============================================================================
%%% Query API
%%% ============================================================================

-doc """
Check if a class conforms to a protocol.

Structural conformance: a class conforms if it responds to all required
selectors of the protocol (including inherited requirements from
`extending` protocols), and all required class methods (BT-1611).

Returns `true` if:
- The class responds to all required instance selectors
- The class responds to all required class method selectors

Returns `false` if:
- The protocol is not registered (unknown or non-protocol names)
- The class is missing one or more required selectors (instance or class)

BT-3222: Results are cached in `?CONFORMS_CACHE_TABLE`, keyed by
`{ClassName, ProtocolName}` — a repeated pair is a plain ETS lookup with no
`gen_server:call` to any class process. See this module's "Conformance
Cache" doc for the full invalidation list and why a generation-stamped entry,
not a whole-table flush, is what actually makes a stale entry unreturnable
across a class or protocol mutation.
""".
-spec conforms_to(atom(), atom()) -> boolean().
conforms_to(ClassName, ProtocolName) ->
    CacheKey = {ClassName, ProtocolName},
    case cache_lookup(CacheKey) of
        {ok, Cached} ->
            Cached;
        miss ->
            %% Sampled *before* compute_conforms_to/2 runs — see the
            %% "Conformance Cache" moduledoc for why the store below must
            %% carry this pre-compute generation, not one read afterward.
            Gen = current_generation(),
            Result = compute_conforms_to(ClassName, ProtocolName),
            cache_store(CacheKey, Result, Gen),
            Result
    end.

%% The uncached structural check — exactly what conforms_to/2 did before
%% BT-3222; the caching wrapper above is the only change to its call sites.
-spec compute_conforms_to(atom(), atom()) -> boolean().
compute_conforms_to(ClassName, ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            %% Unknown protocol — cannot conform to something that isn't a protocol
            false;
        Info ->
            AllMethods = all_required_methods(Info),
            AllClassMethods = all_required_class_methods(Info),
            try
                InstanceOk = lists:all(
                    fun(#{selector := Selector}) ->
                        beamtalk_behaviour_intrinsics:classCanUnderstandFromName(
                            ClassName, Selector
                        )
                    end,
                    AllMethods
                ),
                ClassOk = lists:all(
                    fun(#{selector := Selector}) ->
                        class_has_class_method(ClassName, Selector)
                    end,
                    AllClassMethods
                ),
                InstanceOk andalso ClassOk
            catch
                Kind:Reason:ST ->
                    %% If the class process is dead or unreachable, assume non-conformance
                    ?LOG_DEBUG(
                        "Protocol conformance check failed for ~p (protocol ~p): ~p:~p",
                        [ClassName, ProtocolName, Kind, Reason],
                        #{stacktrace => ST, domain => [beamtalk, runtime]}
                    ),
                    false
            end
    end.

%%% ============================================================================
%%% Conformance Cache (BT-3222)
%%% ============================================================================

-doc """
Look up a cached `conforms_to/2` result.

A hit requires the entry's stamped generation to still equal the *current*
generation counter — an entry stamped before a bump is a permanent miss,
never a stale hit, regardless of write timing (see the "Conformance Cache"
moduledoc). `badarg`-safe: a missing table (never initialised, or torn down
concurrently — e.g. mid test-teardown) reports a cache miss rather than
raising, exactly like every other lookup in this module.
""".
-spec cache_lookup({atom(), atom()}) -> {ok, boolean()} | miss.
cache_lookup(Key) ->
    try
        case ets:info(?CONFORMS_CACHE_TABLE) of
            undefined ->
                miss;
            _ ->
                case ets:lookup(?CONFORMS_CACHE_TABLE, Key) of
                    [{_, {Result, Gen}}] ->
                        case Gen =:= current_generation() of
                            true -> {ok, Result};
                            false -> miss
                        end;
                    [] ->
                        miss
                end
        end
    catch
        error:badarg -> miss
    end.

-doc """
Store a `conforms_to/2` result in the cache, stamped with the generation
sampled before it was computed.

No compare-and-swap against the current generation is needed here — that's
the point of stamp-and-validate-on-read (see the moduledoc): a store carrying
a now-stale `Gen` is harmless because `cache_lookup/1` will never treat it as
a hit. `badarg`-safe (see `cache_lookup/1`) — a failed store just means the
next call recomputes, never a crash.
""".
-spec cache_store({atom(), atom()}, boolean(), non_neg_integer()) -> ok.
cache_store(Key, Result, Gen) ->
    try
        case ets:info(?CONFORMS_CACHE_TABLE) of
            undefined ->
                ok;
            _ ->
                ets:insert(?CONFORMS_CACHE_TABLE, {Key, {Result, Gen}}),
                ok
        end
    catch
        error:badarg -> ok
    end.

-doc """
Read the current cache generation, initialising it to `0` if this is the
first read since the table was created.

`badarg`-safe: reports `0` if the table is missing or torn down concurrently,
matching every other accessor in this module — a caller that gets `0` here
either finds no matching row on the immediately-following `cache_lookup/1`
(miss, safe) or is racing table teardown entirely, in which case the result
is discarded anyway.
""".
-spec current_generation() -> non_neg_integer().
current_generation() ->
    try
        ets:update_counter(
            ?CONFORMS_CACHE_TABLE, ?CONFORMS_CACHE_GEN_KEY, 0, {?CONFORMS_CACHE_GEN_KEY, 0}
        )
    catch
        error:badarg -> 0
    end.

-doc """
Invalidate every cached `conforms_to/2` result (BT-3222) by bumping the
generation counter — see the "Conformance Cache" moduledoc for why a
counter bump, not `ets:delete_all_objects/1`, is what makes this race-free.
Call sites are listed there.

`badarg`-safe: a missing or concurrently-torn-down table is a no-op, matching
`cache_lookup/1` / `cache_store/3`.
""".
-spec invalidate_conforms_cache() -> ok.
invalidate_conforms_cache() ->
    try
        ets:update_counter(
            ?CONFORMS_CACHE_TABLE, ?CONFORMS_CACHE_GEN_KEY, 1, {?CONFORMS_CACHE_GEN_KEY, 0}
        ),
        ok
    catch
        error:badarg -> ok
    end.

-doc """
Return the list of protocols a class conforms to.

Checks all registered protocols against the class. Returns a list of
protocol name atoms, sorted alphabetically for deterministic output.
""".
-spec protocols_for_class(atom()) -> [atom()].
protocols_for_class(ClassName) ->
    Names = all_protocol_names(),
    Conforming = [N || N <- Names, conforms_to(ClassName, N)],
    lists:sort(Conforming).

-doc """
Return the required method selectors for a protocol.

Returns a list of selector atoms. Includes methods from extended protocols.
BT-1611: Class method selectors are included with a `class ` prefix atom
(e.g., `'class fromString:'`) to distinguish them from instance methods.
Returns `[]` if the protocol is not registered.
""".
-spec required_methods(atom()) -> [atom()].
required_methods(ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            [];
        Info ->
            AllMethods = all_required_methods(Info),
            InstanceSels = [Sel || #{selector := Sel} <- AllMethods],
            AllClassMethods = all_required_class_methods(Info),
            ClassSels = [
                % elp:fixme W0023 intentional atom creation
                list_to_atom("class " ++ atom_to_list(Sel))
             || #{selector := Sel} <- AllClassMethods
            ],
            InstanceSels ++ ClassSels
    end.

-doc """
Return the list of classes conforming to a protocol.

Checks all registered classes against the protocol. Returns a list of
class name atoms, sorted alphabetically for deterministic output.
""".
-spec conforming_classes(atom()) -> [atom()].
conforming_classes(ProtocolName) ->
    case protocol_info(ProtocolName) of
        undefined ->
            [];
        _Info ->
            try
                AllClasses = [
                    Name
                 || {Name, _Mod, _Pid} <-
                        beamtalk_class_registry:live_class_entries()
                ],
                Conforming = [C || C <- AllClasses, conforms_to(C, ProtocolName)],
                lists:sort(Conforming)
            catch
                _:_ -> []
            end
    end.

-doc """
Retrieve full protocol metadata by name.

Returns the protocol info map, or `undefined` if not registered.
""".
-spec protocol_info(atom()) -> map() | undefined.
protocol_info(ProtocolName) ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            undefined;
        _ ->
            ets:lookup_element(?PROTOCOL_TABLE, ProtocolName, 2, undefined)
    end.

-doc "Check if a name is a registered protocol.".
-spec is_protocol(atom()) -> boolean().
is_protocol(Name) ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined ->
            false;
        _ ->
            case ets:lookup(?PROTOCOL_TABLE, Name) of
                [{_, _}] -> true;
                [] -> false
            end
    end.

-doc "Return all registered protocol names.".
-spec all_protocol_names() -> [atom()].
all_protocol_names() ->
    case ets:info(?PROTOCOL_TABLE) of
        undefined -> [];
        _ -> [Name || {Name, _} <- ets:tab2list(?PROTOCOL_TABLE)]
    end.

%%% ============================================================================
%%% Internal Helpers
%%% ============================================================================

-doc "Collect all required instance methods including from extending protocols.".
-spec all_required_methods(map()) -> [map()].
all_required_methods(#{required_methods := Methods} = Info) ->
    ParentMethods =
        case maps:get(extending, Info, undefined) of
            undefined ->
                [];
            ParentName ->
                case protocol_info(ParentName) of
                    undefined -> [];
                    ParentInfo -> all_required_methods(ParentInfo)
                end
        end,
    %% Merge: own methods take precedence over parent methods with same selector
    OwnSelectors = [S || #{selector := S} <- Methods],
    FilteredParent = [
        M
     || #{selector := S} = M <- ParentMethods,
        not lists:member(S, OwnSelectors)
    ],
    Methods ++ FilteredParent;
all_required_methods(_) ->
    [].

-doc """
Collect all required class methods including from extending protocols (BT-1611).
""".
-spec all_required_class_methods(map()) -> [map()].
all_required_class_methods(Info) ->
    ClassMethods = maps:get(required_class_methods, Info, []),
    ParentClassMethods =
        case maps:get(extending, Info, undefined) of
            undefined ->
                [];
            ParentName ->
                case protocol_info(ParentName) of
                    undefined -> [];
                    ParentInfo -> all_required_class_methods(ParentInfo)
                end
        end,
    %% Merge: own class methods take precedence over parent with same selector
    OwnSelectors = [S || #{selector := S} <- ClassMethods],
    FilteredParent = [
        M
     || #{selector := S} = M <- ParentClassMethods,
        not lists:member(S, OwnSelectors)
    ],
    ClassMethods ++ FilteredParent.

-doc """
Check if a class has a class-side method (walks hierarchy + extensions) (BT-1611/BT-1617).

Walks the superclass chain checking each class's local class methods map.
Falls back to the extensions ETS table for class-side extensions registered
via `Class class >> selector` (keyed as `{'ClassName class', Selector}`).
This mirrors the actual class-side dispatch path.
""".
-spec class_has_class_method(atom(), atom()) -> boolean().
class_has_class_method(ClassName, Selector) ->
    case class_has_class_method_in_chain(ClassName, Selector) of
        true ->
            true;
        false ->
            %% BT-1617: Check extensions ETS table for class-side extensions.
            %% Class-side extensions use the metaclass tag atom (e.g. 'Integer class')
            %% as the class key in the extensions registry.
            MetaclassTag = beamtalk_class_registry:class_object_tag(ClassName),
            check_class_extension(MetaclassTag, Selector)
    end.

-doc "Walk the superclass chain checking local class methods maps.".
-spec class_has_class_method_in_chain(atom(), atom()) -> boolean().
class_has_class_method_in_chain(ClassName, Selector) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            false;
        ClassPid ->
            try
                ClassMethods = beamtalk_object_class:local_class_methods(ClassPid),
                case lists:member(Selector, ClassMethods) of
                    true ->
                        true;
                    false ->
                        %% Walk superclass chain
                        case beamtalk_object_class:superclass(ClassPid) of
                            none -> false;
                            SuperName -> class_has_class_method_in_chain(SuperName, Selector)
                        end
                end
            catch
                _:_ -> false
            end
    end.

-doc """
Safe extension registry lookup for class-side methods (BT-1617).

Guards against the ETS table not existing (e.g., during early bootstrap).
""".
-spec check_class_extension(atom(), atom()) -> boolean().
check_class_extension(MetaclassTag, Selector) ->
    try
        beamtalk_extensions:has(MetaclassTag, Selector)
    catch
        error:badarg ->
            %% ETS table doesn't exist yet (early bootstrap)
            false
    end.

%%% ============================================================================
%%% Protocol Class Object Creation (ADR 0068)
%%% ============================================================================

-doc """
Create a class object for a protocol if one doesn't already exist.

Protocol class objects are sealed abstract subclasses of Protocol that
respond to class-side messages like `requiredMethods` and `conformingClasses`.
This is called from `register_protocol/1` after storing the protocol metadata.

Skipped during early bootstrap (before the Protocol class exists) and for
hot reload (class already exists).
""".
-spec maybe_create_protocol_class(atom(), map()) -> ok.
maybe_create_protocol_class(Name, Info) ->
    %% Only create if the Protocol class exists (not during early bootstrap)
    %% and this protocol doesn't already have a class object (idempotent).
    case beamtalk_class_registry:whereis_class('Protocol') of
        undefined ->
            %% Protocol class not loaded yet — skip (early bootstrap).
            ok;
        _ProtocolPid ->
            case beamtalk_class_registry:whereis_class(Name) of
                undefined ->
                    create_protocol_class(Name, Info);
                _Pid ->
                    %% Already exists (hot reload) — no-op.
                    ok
            end
    end.

-doc """
Create a sealed abstract class object for a protocol definition.

Uses `beamtalk_protocol_object` as the shared dispatch module for all
protocol class objects. The class methods (`requiredMethods`, `conformingClasses`)
extract the protocol name from the ClassSelf tuple and query the registry.
""".
-spec create_protocol_class(atom(), map()) -> ok.
create_protocol_class(Name, Info) ->
    Doc = maps:get(doc, Info, none),
    ClassInfo = #{
        module => beamtalk_protocol_object,
        superclass => 'Protocol',
        is_sealed => true,
        is_abstract => true,
        meta => #{
            is_sealed => true,
            is_abstract => true
        },
        class_methods => #{
            requiredMethods => #{arity => 0, is_sealed => true},
            conformingClasses => #{arity => 0, is_sealed => true}
        },
        class_method_signatures => #{
            requiredMethods => <<"requiredMethods -> List">>,
            conformingClasses => <<"conformingClasses -> List">>
        },
        class_method_docs => #{
            requiredMethods => <<"Return the required method selectors for this protocol.">>,
            conformingClasses => <<"Return the classes conforming to this protocol.">>
        },
        %% ADR 0087 Phase 2 (BT-2298) / BT-2385: protocol class objects expose two
        %% class-side methods (`requiredMethods`, `conformingClasses`) dispatched
        %% by the shared Erlang module `beamtalk_protocol_object` — they have no
        %% analysable Beamtalk body, so codegen never bakes a method_xref for
        %% them. Without this the protocol class (e.g. Printable) is absent from
        %% beamtalk_xref and every navigation query source-scans it via the
        %% miss-policy fallback. Record both as `unindexed_runtime_fun` rows
        %% (the genuine sourceless category, mirroring beamtalk_bootstrap's stub
        %% classes) so the class is indexed. `beamtalk_object_class:init/1`
        %% forwards this list to beamtalk_xref before start/2 yields.
        method_xref => [
            protocol_class_xref_entry(requiredMethods),
            protocol_class_xref_entry(conformingClasses)
        ],
        instance_methods => #{},
        fields => [],
        doc => Doc
    },
    case beamtalk_object_class:start(Name, ClassInfo) of
        {ok, _Pid} ->
            ?LOG_DEBUG(
                "Created class object for protocol ~p",
                [Name],
                #{domain => [beamtalk, runtime]}
            ),
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to create class object for protocol ~p: ~p",
                [Name, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            ok
    end.

-doc """
Build a single `unindexed_runtime_fun` method_xref entry for a protocol class
object's class-side method (BT-2385).

Protocol class objects share the Erlang dispatch module
`beamtalk_protocol_object`, so `requiredMethods` / `conformingClasses` have no
analysable Beamtalk source. They are recorded with empty `sends` / `references`
and `source_status => unindexed_runtime_fun`, matching the sourceless stub-class
convention in `beamtalk_bootstrap:stub_method_entry/2`. The `line` is a
placeholder (`1`) because `beamtalk_xref` requires a `pos_integer()`.
""".
-spec protocol_class_xref_entry(atom()) -> map().
protocol_class_xref_entry(Selector) ->
    #{
        class_side => true,
        selector => Selector,
        line => 1,
        sends => [],
        references => [],
        source_status => unindexed_runtime_fun,
        provenance => class_body
    }.
