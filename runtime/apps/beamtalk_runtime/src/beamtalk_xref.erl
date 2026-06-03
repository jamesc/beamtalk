%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_xref).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Cross-reference index gen_server for SystemNavigation queries (ADR 0087).

Owns four ETS tables that maintain a runtime-resident `selector → sites` /
`class → references` index, populated at class-load time via
`register_class/2`. Reads (`senders_of/1`, `references_to/1`,
`implementors_of/1`, `defined_selectors/2`) hit ETS directly and do not
serialize through the gen_server; each filters rows to the owning class's
current generation. Writes serialize through the gen_server so per-class
generation bumps stay atomic.

Phase 1 (BT-2297) provides the skeleton: tables, API contract, supervisor
wiring. Subsequent phases land producers (codegen → `register_class/0`,
lifecycle hooks) and read-path migration in `SystemNavigation`. Phase 4
(BT-2300) adds the atomic install protocol: a whole-class (re)register
inserts the new generation's rows, publishes the gen bump in one ETS write,
and reclaims the superseded generation's rows via an async sweep — readers
filter by `current_gen` throughout, so they never observe a partially-built
or stale generation.

Storage layout (all `protected, named_table, {read_concurrency, true}`):
- `beamtalk_xref_methods` (bag): per-method definitions
- `beamtalk_xref_senders` (bag): selector → call sites
- `beamtalk_xref_references` (bag): class → reference sites
- `xref_class_gen` (set): class → current generation

See also: docs/ADR/0087-maintained-xref-index-for-system-navigation.md
""".

-include_lib("kernel/include/logger.hrl").

%% API — write path
-export([
    start_link/0,
    register_class/2,
    purge_class/1,
    put_method/4,
    purge_method/3
]).

%% API — read path
-export([
    senders_of/1,
    senders_of_bt/1,
    references_to/1,
    implementors_of/1,
    defined_selectors/2,
    method_info/3
]).

%% API — xref-entry construction helpers (ADR 0087 Phase 4, BT-2301)
-export([
    build_method_entry/5
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Table names
-define(METHODS_TABLE, beamtalk_xref_methods).
-define(SENDERS_TABLE, beamtalk_xref_senders).
-define(REFERENCES_TABLE, beamtalk_xref_references).
-define(CLASS_GEN_TABLE, xref_class_gen).

%% Bound on `read_stable/1` revalidation retries (Phase 4 / BT-2300). Converges
%% in 0-1 retries in practice; the bound only caps spin under a write storm.
-define(READ_STABLE_RETRIES, 100).

%%====================================================================
%% Types
%%====================================================================

-type class_name() :: atom().
-type selector() :: atom().
-type class_side() :: boolean().
-type gen() :: pos_integer().
-type source_status() :: indexed | unindexed_runtime_fun | synthetic.
-type provenance() :: class_body | extension | class_builder | put_method.
-type recv_kind() :: self_recv | super_recv | erlang_ffi | other.

-type send_entry() :: #{
    selector := selector(),
    line := pos_integer(),
    recv_kind => recv_kind()
}.

-type reference_entry() :: #{
    class := class_name(),
    line := pos_integer()
}.

-type method_xref_entry() :: #{
    class_side := class_side(),
    selector := selector(),
    line := pos_integer(),
    sends => [send_entry()],
    references => [reference_entry()],
    source_status => source_status(),
    provenance => provenance(),
    synthetic_origin => pos_integer() | undefined
}.

-type method_info() :: #{
    owner := class_name(),
    line := pos_integer(),
    source_status := source_status(),
    provenance := provenance(),
    gen := gen()
}.

-type site() :: #{
    owner := class_name(),
    class_side := class_side(),
    method := selector(),
    line := pos_integer(),
    recv_kind => recv_kind(),
    gen := gen()
}.

%% A site reduced to the fields `SystemNavigation` consumes — the internal
%% `gen` / `recv_kind` bookkeeping is dropped at the BT boundary (BT-2299).
-type bt_row() :: #{
    owner := class_name(),
    class_side := class_side(),
    method := selector(),
    line := pos_integer()
}.

-export_type([
    method_xref_entry/0,
    method_info/0,
    site/0,
    bt_row/0,
    source_status/0,
    provenance/0,
    recv_kind/0
]).

-record(state, {}).

%%====================================================================
%% API — write path
%%====================================================================

-doc "Start the xref gen_server.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Register all per-method xref rows for a class. Synchronous.

Whole-class (re)load follows the ADR 0087 §Atomicity protocol: the new
generation's rows are inserted first, then the generation is published in
`xref_class_gen` in one ETS write (the atomic publish), then the superseded
generation's rows are reclaimed by an async sweep. Readers filter by the
published `current_gen`, so an observer always sees a complete generation —
either the pre-register one or the fully-built post-register one — and never
an empty or partially-built view. No `purge_class/1` is required between
re-registers; the sweep handles stale rows.
""".
-spec register_class(class_name(), [method_xref_entry()]) -> ok.
register_class(Class, MethodXref) when is_atom(Class), is_list(MethodXref) ->
    gen_server:call(?MODULE, {register_class, Class, MethodXref}).

-doc """
Remove all rows belonging to a class from every xref table.
Synchronous. Idempotent — purging an unknown class is a no-op.
""".
-spec purge_class(class_name()) -> ok.
purge_class(Class) when is_atom(Class) ->
    gen_server:call(?MODULE, {purge_class, Class}).

-doc """
Replace the xref rows for a single method without touching siblings.

Used for method-level edits (`put_method/4` source patches, ADR 0082;
extension registers, ADR 0066) where only one method's data changes.
Synchronous. Unlike `register_class/2`, a single-method patch does NOT bump
the class generation — the unbumped sibling methods must stay on the current
generation rather than being stranded behind the reader's `current_gen`
filter. The addressed method's prior rows are deleted synchronously and the
new rows installed under the class's current generation (establishing
generation 1 for a never-before-registered class).
""".
-spec put_method(class_name(), class_side(), selector(), method_xref_entry()) -> ok.
put_method(Class, ClassSide, Selector, MethodXref) when
    is_atom(Class), is_boolean(ClassSide), is_atom(Selector), is_map(MethodXref)
->
    gen_server:call(?MODULE, {put_method, Class, ClassSide, Selector, MethodXref}).

-doc """
Remove the xref rows for a single method without touching siblings.

The narrow counterpart to `put_method/4` (which replaces). Used for
method-level *removal* — e.g. `beamtalk_extensions:unregister/2` (ADR 0066)
drops an extension method, so its `{Class, ClassSide, Selector}` method row
plus every sender / reference row owned by that method must be purged while
sibling methods on the same class stay intact. Synchronous. Idempotent —
purging an unknown method is a no-op.

Does not bump the class's generation: removal leaves no new rows to publish,
and the surviving siblings keep their existing generation.
""".
-spec purge_method(class_name(), class_side(), selector()) -> ok.
purge_method(Class, ClassSide, Selector) when
    is_atom(Class), is_boolean(ClassSide), is_atom(Selector)
->
    gen_server:call(?MODULE, {purge_method, Class, ClassSide, Selector}).

%%====================================================================
%% API — xref-entry construction helpers
%%====================================================================

-doc """
Build a single `method_xref_entry()` from a method's source text (ADR 0087
Phase 4, BT-2301).

This is the *runtime* counterpart to the compile-time `build_method_xref_entry`
in `crates/.../gen_server/methods.rs`: it re-parses the one method's `Source`
via the existing compiler AST FFI walkers (`find_all_sends_in_source/1` for
sent selectors + receiver kind, `find_references_to_in_source/2` — there is no
runtime "all references" walker, so class references are left empty for live
edits; the class-reference channel is fully populated only at compile time).

Used by the method-level mutation entry points that re-index a single method:
`beamtalk_object_class:put_method/4` (live `>>` patch, ADR 0082) and
`beamtalk_extensions:register/5` (sourced extension, ADR 0066).

`Source` is the method's bare body text; the method-relative definition line is
1 (the signature is the first source line). When the compiler app is absent or
`Source` is empty / unparseable, returns an entry with empty `sends` /
`references` rather than failing — the index degrades gracefully, mirroring the
miss-policy fallback. `SourceStatus` is the caller-chosen tag (`indexed` for a
sourced patch / extension, `unindexed_runtime_fun` for a sourceless one).
`Provenance` records which mutation path produced the entry.
""".
-spec build_method_entry(class_side(), selector(), binary(), source_status(), provenance()) ->
    method_xref_entry().
build_method_entry(ClassSide, Selector, Source, SourceStatus, Provenance) when
    is_boolean(ClassSide), is_atom(Selector), is_atom(SourceStatus), is_atom(Provenance)
->
    Sends =
        case SourceStatus of
            unindexed_runtime_fun -> [];
            _ -> sends_from_source(Source)
        end,
    #{
        class_side => ClassSide,
        selector => Selector,
        line => 1,
        sends => Sends,
        references => [],
        source_status => SourceStatus,
        provenance => Provenance
    }.

-doc """
Extract `send_entry()` rows from a method's source via the compiler AST FFI.

A no-op (empty list) when the source is not a binary, is empty, the compiler
app is not loaded, or the walker reports an error. Maps the FFI's `recv`
receiver tag (`self | super | erlang_ffi | other`) onto the xref `recv_kind`
(`self_recv | super_recv | erlang_ffi | other`) and converts the binary
selector to an atom.

Security: `Source` is attacker-influenceable (live `>>` patches and extension
registration both flow user text here), so the selector is converted with
`binary_to_existing_atom` — never `binary_to_atom` — to avoid growing the
global atom table from untrusted input (mirrors `beamtalk_repl_ops_nav`). A
selector that has no existing atom (e.g. a send to a never-yet-seen message)
is dropped from the index; it will be picked up once the atom exists. The
255-byte guard is redundant with `existing_atom` but kept as a cheap fast-path
reject for over-long inputs.
""".
-spec sends_from_source(term()) -> [send_entry()].
sends_from_source(Source) when is_binary(Source), Source =/= <<>> ->
    case erlang:function_exported(beamtalk_compiler, find_all_sends_in_source, 1) of
        false ->
            [];
        true ->
            case beamtalk_compiler:find_all_sends_in_source(Source) of
                {ok, Hits} -> lists:filtermap(fun send_hit_to_entry/1, Hits);
                {error, _} -> []
            end
    end;
sends_from_source(_Source) ->
    [].

-define(MAX_ATOM_BYTES, 255).

-spec send_hit_to_entry(map()) -> {true, send_entry()} | false.
send_hit_to_entry(#{selector := SelBin, line := Line} = Hit) when
    is_binary(SelBin), byte_size(SelBin) =< ?MAX_ATOM_BYTES
->
    %% binary_to_existing_atom (not binary_to_atom): the source is untrusted, so
    %% we must not let it mint new atoms. A send whose selector is not yet an
    %% atom anywhere is simply not indexed (dropped via the badarg catch).
    try binary_to_existing_atom(SelBin, utf8) of
        Selector ->
            {true, #{
                selector => Selector,
                line => Line,
                recv_kind => recv_to_recv_kind(maps:get(recv, Hit, other))
            }}
    catch
        error:badarg -> false
    end;
send_hit_to_entry(_Hit) ->
    false.

-spec recv_to_recv_kind(atom()) -> recv_kind().
recv_to_recv_kind(self) -> self_recv;
recv_to_recv_kind(super) -> super_recv;
recv_to_recv_kind(erlang_ffi) -> erlang_ffi;
recv_to_recv_kind(_) -> other.

%%====================================================================
%% API — read path
%%====================================================================

-doc """
Return all sites that send `Selector`. Direct ETS lookup — does not
go through the gen_server.

Phase 1 (BT-2297): no miss-policy fallback. Callers should treat an
empty result as "no known senders"; the source-scan fallback for
unloaded / unindexed classes lands with Phase 3 (BT-2299).

Phase 4 (BT-2300): rows are filtered to each owner class's current
generation. Because `register_class/2` and `put_method/4` install under a
new generation without synchronously purging the old one (the sweep is
async), the bag can transiently hold stale rows from an earlier
generation. Filtering by the owner's `current_gen` guarantees a reader
never observes a stale site.
""".
-spec senders_of(selector()) -> [site()].
senders_of(Selector) when is_atom(Selector) ->
    case ets:whereis(?SENDERS_TABLE) of
        undefined ->
            [];
        _ ->
            {Snapshot, Sites} = read_stable(fun() ->
                [Site || {_Sel, Site} <- ets:lookup(?SENDERS_TABLE, Selector)]
            end),
            live_gen_sites(Sites, Snapshot)
    end.

-doc """
Resolve the senders of `Selector` with the ADR 0087 miss-policy applied,
returning the partition `SystemNavigation sendersOf:` needs to assemble a
correct result without re-parsing every method (BT-2299, Phase 3).

The return map has two keys:

- `indexed` — the sender sites drawn from the ETS index, with **stale rows
  dropped** on two axes: (1) `senders_of/1` already filters to each owner's
  current generation (Phase 4 / BT-2300), so a re-register can never surface a
  site from a superseded generation here; and (2) this function additionally
  discards any site whose `owner` class is no longer reported as loaded by
  `beamtalk_class_registry`. Each surviving row is shaped `#{owner := atom(),
  class_side := boolean(), method := atom(), line := pos_integer()}` for the BT
  side to map onto `#{#class, #selector, #line}` records.

- `fallback_classes` — the class names that the registry reports as loaded,
  that have **no rows in the index at all** (an index miss, per ADR 0087
  §Authoritativeness: "the class is not considered loaded until xref has the
  rows"), *and that actually define methods*. The method gate matters: an empty
  base/protocol class (e.g. `Error`, `Printable`'s sub-hierarchy) genuinely has
  nothing to index and nothing to scan, so flagging it would be pure noise — a
  real miss is a class with methods the index should have but does not. The BT
  side source-scans exactly these classes. One miss-policy `?LOG_WARNING`
  (`domain => [beamtalk, runtime]`, `reason => xref_miss`) is emitted here per
  class so the gap surfaces from the runtime context rather than the stdlib
  one. A loaded, *indexed* class that simply has zero senders of `Selector` is
  correct and never appears here.

The loaded set is taken from `beamtalk_class_registry:live_class_entries/0`;
the indexed set is the distinct owners present in the methods table. The stale
drop and miss partition are pure ETS / set work; only the (typically empty)
miss candidate set incurs the per-class method-count `gen_server:call`, so the
cost is bounded by the miss count rather than the workspace size.
""".
-spec senders_of_bt(selector()) ->
    #{indexed := [bt_row()], fallback_classes := [class_name()]}.
senders_of_bt(Selector) when is_atom(Selector) ->
    Entries = beamtalk_class_registry:live_class_entries(),
    Loaded = sets:from_list([Name || {Name, _Mod, _Pid} <- Entries]),
    Indexed = indexed_class_set(),
    Sites = senders_of(Selector),
    %% Stale-drop: keep only sites whose owner is still loaded.
    IndexedRows = [
        site_to_bt_row(Site)
     || Site <- Sites, sets:is_element(maps:get(owner, Site), Loaded)
    ],
    %% Index miss: loaded, absent from the index, and actually defines methods.
    %% The method gate (per-pid gen_server call) only runs for the loaded set
    %% minus the indexed set, which is empty in steady state.
    FallbackClasses = [
        Name
     || {Name, _Mod, Pid} <- Entries,
        not sets:is_element(Name, Indexed),
        class_defines_methods(Pid)
    ],
    lists:foreach(fun(Class) -> log_xref_miss(Class, sendersOf) end, FallbackClasses),
    #{indexed => IndexedRows, fallback_classes => FallbackClasses}.

-doc """
Return all sites that reference `Class` (type annotations, class
literals, etc.). Direct ETS lookup, filtered to each owner's current
generation (Phase 4 / BT-2300 — see `senders_of/1`).
""".
-spec references_to(class_name()) -> [site()].
references_to(Class) when is_atom(Class) ->
    case ets:whereis(?REFERENCES_TABLE) of
        undefined ->
            [];
        _ ->
            {Snapshot, Sites} = read_stable(fun() ->
                [Site || {_Cls, Site} <- ets:lookup(?REFERENCES_TABLE, Class)]
            end),
            live_gen_sites(Sites, Snapshot)
    end.

-doc """
Return all `{Class, ClassSide}` pairs that implement `Selector`.
Direct ETS lookup via `match_object` on the methods bag, filtered to each
class's current generation (Phase 4 / BT-2300) so a stale row left by an
un-swept reload never reports a phantom implementor.
""".
-spec implementors_of(selector()) -> [{class_name(), class_side()}].
implementors_of(Selector) when is_atom(Selector) ->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            [];
        _ ->
            %% Pull the full {Key, Info} rows (not just the key fields) so the
            %% per-row `gen` is available for the current-generation filter,
            %% under a stable gen snapshot so a concurrent reload cannot make
            %% the rows and the gens disagree.
            {Snapshot, Rows} = read_stable(fun() ->
                ets:match_object(?METHODS_TABLE, {{'_', '_', Selector}, '_'})
            end),
            lists:usort([
                {Cls, CS}
             || {{Cls, CS, _Sel}, Info} <- Rows, is_live_gen(Cls, Info, Snapshot)
            ])
    end.

-doc """
Return the selectors defined on `Class` for the given side
(`ClassSide = true` for class-side methods, `false` for instance-side).

Filtered to `Class`'s current generation (Phase 4 / BT-2300): a selector
that only survives in a stale, un-swept generation is not reported.
""".
-spec defined_selectors(class_name(), class_side()) -> [selector()].
defined_selectors(Class, ClassSide) when is_atom(Class), is_boolean(ClassSide) ->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            [];
        _ ->
            %% `Class` is fixed; capture its generation and the matching rows
            %% under one stable snapshot so a concurrent reload cannot strand
            %% the filter on a generation whose rows the fetch missed or whose
            %% rows the sweep removed mid-read.
            {Snapshot, Rows} = read_stable(fun() ->
                ets:match_object(?METHODS_TABLE, {{Class, ClassSide, '_'}, '_'})
            end),
            lists:usort([
                Sel
             || {{_Cls, _CS, Sel}, Info} <- Rows, is_live_gen(Class, Info, Snapshot)
            ])
    end.

-doc """
Return the `method_info()` record for a method, or `undefined` if the
class+selector+side triple is not registered.

Used by the LSP `nav-query` op (BT-2239) to surface the method-header line
number to `textDocument/implementation` consumers (BT-2241). Direct ETS
lookup — does not go through the gen_server.

Phase 4 (BT-2300): `?METHODS_TABLE` is a bag, and `register_class/2` /
`put_method/4` install under a new generation without synchronously
purging the old one (the sweep is async). Multiple rows can therefore
share `{Class, ClassSide, Selector}`. The lookup is filtered to `Class`'s
current generation: the live row wins over any un-swept predecessor, and
a selector that survives *only* in a stale generation (it was dropped by
the latest `register_class/2`) correctly reads as `undefined`.
""".
-spec method_info(class_name(), class_side(), selector()) -> method_info() | undefined.
method_info(Class, ClassSide, Selector) when
    is_atom(Class), is_boolean(ClassSide), is_atom(Selector)
->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            undefined;
        _ ->
            {Snapshot, Rows} = read_stable(fun() ->
                ets:lookup(?METHODS_TABLE, {Class, ClassSide, Selector})
            end),
            case [Info || {_Key, Info} <- Rows, is_live_gen(Class, Info, Snapshot)] of
                [] -> undefined;
                [Info | _] -> Info
            end
    end.

%%====================================================================
%% Internal: read helpers (miss-policy)
%%====================================================================

-doc """
Whether the class behind `Pid` defines at least one method (instance- or
class-side). Used to gate the miss-policy fallback so empty base/protocol
classes are not flagged as index misses (they have nothing to scan).

A dead or unresponsive class process counts as "no methods" — a class we cannot
interrogate is not a useful source-scan target.
""".
-spec class_defines_methods(pid()) -> boolean().
class_defines_methods(Pid) ->
    try
        beamtalk_object_class:methods(Pid) =/= [] orelse
            beamtalk_object_class:local_class_methods(Pid) =/= []
    catch
        _:_ -> false
    end.

-doc """
The set of class names that currently have at least one row in the methods
table — i.e. classes that have been through `register_class/2`. A class in this
set is "indexed" for miss-policy purposes even if all its rows are
`unindexed_runtime_fun` (a sourceless stub still counts as present).
""".
-spec indexed_class_set() -> sets:set(class_name()).
indexed_class_set() ->
    case ets:whereis(?CLASS_GEN_TABLE) of
        undefined ->
            sets:new([{version, 2}]);
        _ ->
            %% `xref_class_gen` holds exactly one `{Class, Gen}` row per
            %% registered class — the authoritative "has this class been
            %% indexed" set.
            Classes = [Class || {Class, _Gen} <- ets:tab2list(?CLASS_GEN_TABLE)],
            sets:from_list(Classes)
    end.

%%====================================================================
%% Internal: generation filtering (Phase 4 / BT-2300)
%%====================================================================

-doc """
Current generation for `Class`, or `0` when the class has never been
registered. `xref_class_gen` is a `set` table, so the lookup is O(1) and a
registered class always has exactly one row.
""".
-spec current_gen(class_name()) -> non_neg_integer().
current_gen(Class) ->
    case ets:whereis(?CLASS_GEN_TABLE) of
        undefined ->
            0;
        _ ->
            case ets:lookup(?CLASS_GEN_TABLE, Class) of
                [] -> 0;
                [{_, Gen}] -> Gen
            end
    end.

%% A snapshot of the per-class generation table: class → current gen.
-type gen_snapshot() :: #{class_name() => gen()}.

-doc """
Run `Fetch` against ETS under a *stable* generation snapshot, returning
`{Snapshot, Result}`.

The lock-free atomicity protocol (ADR 0087 §Atomicity). Writers insert a new
generation's rows BEFORE publishing the gen bump, and the async sweep only
ever reclaims *strictly older* generations. So if the `xref_class_gen`
snapshot is identical immediately before and immediately after `Fetch` runs,
no publish completed during the fetch, and therefore:

- every row carrying a snapshot gen was already installed before the fetch
  began (insert-before-publish), so the fetch cannot miss a live row; and
- no row carrying a snapshot gen was swept during the fetch (the sweep that
  removes generation `G` only runs once `G` is no longer current, i.e. only
  after a later publish — which would have changed the snapshot).

If the snapshot changed across the fetch, a publish raced us; we retry, up to
`?READ_STABLE_RETRIES` times. Retries converge almost immediately because the
fetch is a single ETS read while each racing write is a full gen_server
round-trip; the bound only guards against a pathological write storm and
caps CPU spin. On the (practically unreachable) final attempt we accept the
last fetch paired with its *post*-fetch snapshot — still a correct filter for
any row installed before that snapshot, never a hang. `Fetch` MUST be
side-effect-free (it may run several times).
""".
-spec read_stable(fun(() -> T)) -> {gen_snapshot(), T} when T :: term().
read_stable(Fetch) ->
    case ets:whereis(?CLASS_GEN_TABLE) of
        undefined ->
            {#{}, Fetch()};
        _ ->
            read_stable_loop(Fetch, ?READ_STABLE_RETRIES)
    end.

-spec read_stable_loop(fun(() -> T), non_neg_integer()) -> {gen_snapshot(), T} when
    T :: term().
read_stable_loop(Fetch, 0) ->
    %% Exhausted retries — accept the fetch paired with the snapshot taken
    %% immediately after it, which is consistent with every row installed up to
    %% that point. Reachable only under a sustained write storm.
    Result = Fetch(),
    {gen_snapshot(), Result};
read_stable_loop(Fetch, Retries) ->
    Before = gen_snapshot(),
    Result = Fetch(),
    After = gen_snapshot(),
    case Before =:= After of
        true -> {Before, Result};
        false -> read_stable_loop(Fetch, Retries - 1)
    end.

-spec gen_snapshot() -> gen_snapshot().
gen_snapshot() ->
    maps:from_list(ets:tab2list(?CLASS_GEN_TABLE)).

-doc """
Whether `Info`/site map's `gen` is the live generation for `Class` per the
captured `Snapshot`. A row with no `gen` field (legacy / hand-rolled) is
treated as gen `0`, and an unregistered class as gen `0`, so such a row only
survives before any real registration has bumped the counter.
""".
-spec is_live_gen(class_name(), map(), gen_snapshot()) -> boolean().
is_live_gen(Class, Info, Snapshot) ->
    maps:get(gen, Info, 0) =:= maps:get(Class, Snapshot, 0).

-doc """
Filter a list of sender / reference `site()` maps to the rows whose `gen`
matches their `owner`'s generation in the captured `Snapshot`, dropping stale
rows left behind by an un-swept reload.
""".
-spec live_gen_sites([site()], gen_snapshot()) -> [site()].
live_gen_sites(Sites, Snapshot) ->
    [Site || Site <- Sites, is_live_gen(maps:get(owner, Site), Site, Snapshot)].

-doc "Project a full `site()` down to the `bt_row()` fields the BT layer needs.".
-spec site_to_bt_row(site()) -> bt_row().
site_to_bt_row(Site) ->
    #{
        owner => maps:get(owner, Site),
        class_side => maps:get(class_side, Site),
        method => maps:get(method, Site),
        line => maps:get(line, Site)
    }.

-doc """
Emit the ADR 0087 miss-policy warning for an index miss on `Class` under the
navigation query `Query`. A class loaded by the registry but absent from the
index is a defect (the index should grow atomically with the class registry);
the warning surfaces the gap so it self-heals at read time rather than hiding.
""".
-spec log_xref_miss(class_name(), atom()) -> ok.
log_xref_miss(Class, Query) ->
    ?LOG_WARNING(#{
        event => xref_miss,
        class => Class,
        query => Query,
        reason => xref_miss,
        domain => [beamtalk, runtime]
    }),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),

    %% Create the four ETS tables, owned by this process. Heir is undefined
    %% per the acceptance criteria — tables die with the process, supervisor
    %% restart triggers re-population from the next round of register_class/0
    %% calls (or from miss-policy fallback once Phase 3 lands).
    BagOpts = [bag, protected, named_table, {read_concurrency, true}],
    SetOpts = [set, protected, named_table, {read_concurrency, true}],
    ?METHODS_TABLE = ets:new(?METHODS_TABLE, BagOpts),
    ?SENDERS_TABLE = ets:new(?SENDERS_TABLE, BagOpts),
    ?REFERENCES_TABLE = ets:new(?REFERENCES_TABLE, BagOpts),
    ?CLASS_GEN_TABLE = ets:new(?CLASS_GEN_TABLE, SetOpts),

    ?LOG_INFO("xref started", #{
        tables => [?METHODS_TABLE, ?SENDERS_TABLE, ?REFERENCES_TABLE, ?CLASS_GEN_TABLE]
    }),
    {ok, #state{}}.

handle_call({register_class, Class, MethodXref}, _From, State) ->
    %% ADR 0087 §Atomicity, steps (1)-(4). Compute the next generation
    %% WITHOUT publishing it, insert the new rows under it, THEN publish the
    %% gen bump in one ETS write. Readers filter by `current_gen`, so until
    %% the publish they see the previous (consistent) generation and after it
    %% the fully-built new generation — never a partially-built view, and
    %% never an empty one. The old-gen rows are reclaimed asynchronously.
    NewGen = next_gen(Class),
    insert_method_xref_rows(Class, MethodXref, NewGen),
    publish_gen(Class, NewGen),
    schedule_sweep(Class, NewGen),
    {reply, ok, State};
handle_call({purge_class, Class}, _From, State) ->
    do_purge_class(Class),
    {reply, ok, State};
handle_call({put_method, Class, ClassSide, Selector, MethodXref}, _From, State) ->
    %% `put_method/4` is a SURGICAL single-method patch, not a whole-class
    %% reload, so it must NOT bump the class generation: the class's sibling
    %% methods keep their existing rows, all of which carry the current gen,
    %% and a gen bump here would strand every sibling behind the reader's
    %% `current_gen` filter. Instead, install the one method's rows under the
    %% class's *current* generation (1 for a never-registered class) and delete
    %% the addressed method's prior rows synchronously. Siblings stay live
    %% throughout. The addressed method itself is delete-then-insert on the same
    %% generation, so a reader can observe a sub-call window where just that one
    %% method is absent — the documented single-method-patch semantics (same as
    %% `purge_method/3`); whole-class atomicity is `register_class/2`'s job.
    %%
    %% For a never-registered class the gen-1 publish is deferred until AFTER
    %% the row insert (insert-before-publish, ADR 0087 §Atomicity): publishing
    %% gen 1 first would let a `read_stable` observer snapshot a stable gen 1
    %% while the row is not yet inserted and report the fresh method as absent.
    {Gen, NeedsPublish} = put_method_gen(Class),
    delete_method_rows(Class, ClassSide, Selector),
    %% Force the entry's class_side/selector to match the addressed slot —
    %% protects against accidental drift between the call args and the map.
    Normalised = MethodXref#{class_side => ClassSide, selector => Selector},
    insert_method_xref_rows(Class, [Normalised], Gen),
    case NeedsPublish of
        true -> publish_gen(Class, Gen);
        false -> ok
    end,
    {reply, ok, State};
handle_call({purge_method, Class, ClassSide, Selector}, _From, State) ->
    delete_method_rows(Class, ClassSide, Selector),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({sweep, Class, _RequestedGen}, State) ->
    %% ADR 0087 §Atomicity step (4): reclaim rows left under a generation
    %% older than the published one. Pure memory hygiene — readers already
    %% ignore these rows via the `current_gen` filter, so a delayed or dropped
    %% sweep is never a correctness problem.
    %%
    %% The keep-gen is re-read here rather than taken from the cast payload:
    %% several writes to the same class can enqueue several sweep casts, and an
    %% earlier sweep must not delete rows belonging to a generation a later
    %% write has since published. Re-reading the live `current_gen` makes every
    %% sweep converge on "keep only the current generation", which is always
    %% safe regardless of cast ordering.
    sweep_stale_gens(Class, current_gen(Class)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ETS tables are owned by this process; they die automatically. The
    %% supervisor restart brings up fresh empty tables — the miss-policy
    %% fallback (Phase 3) repopulates them at read time as classes are
    %% navigated, and subsequent reloads re-bake their rows via register_class.
    ok.

code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal: generation management
%%====================================================================

-doc """
Compute the next per-class generation WITHOUT publishing it. On first
registration the next generation is 1, otherwise `current + 1`.

ADR 0087 §Atomicity separates computing the new generation (this) from
publishing it (`publish_gen/2`): rows are inserted under the new gen first,
and only then is the gen made visible to readers, so there is no instant at
which `current_gen` points at a half-built generation.
""".
-spec next_gen(class_name()) -> gen().
next_gen(Class) ->
    case ets:lookup(?CLASS_GEN_TABLE, Class) of
        [] -> 1;
        [{_, Current}] -> Current + 1
    end.

-doc """
Publish `Gen` as `Class`'s current generation in one ETS write — the atomic
publish step (ADR 0087 §Atomicity step 3). After this returns, readers
filtering by `current_gen` observe the just-installed rows.
""".
-spec publish_gen(class_name(), gen()) -> ok.
publish_gen(Class, Gen) ->
    true = ets:insert(?CLASS_GEN_TABLE, {Class, Gen}),
    ok.

-doc """
Resolve the generation under which a `put_method/4` patch installs its row,
returning `{Gen, NeedsPublish}`.

A single-method patch does NOT bump the class generation (that would strand
the unbumped siblings behind the reader's `current_gen` filter); it joins the
class's current generation, so `NeedsPublish` is `false`. A class that has
never been registered has no gen row, so the patch establishes generation 1
and `NeedsPublish` is `true` — but the caller must publish *after* inserting
the row (insert-before-publish), so the publish is the caller's job, not this
function's. Establishing the gen here (rather than publishing it) keeps this
read-only: it never makes a half-built generation visible.
""".
-spec put_method_gen(class_name()) -> {gen(), boolean()}.
put_method_gen(Class) ->
    case ets:lookup(?CLASS_GEN_TABLE, Class) of
        [] -> {1, true};
        [{_, Current}] -> {Current, false}
    end.

-doc """
Asynchronously reclaim rows for `Class` carrying a generation other than
`KeepGen` (ADR 0087 §Atomicity step 4). Cast to self so the synchronous
write call returns to the caller immediately; the sweep is memory hygiene,
not a correctness step (readers already filter stale gens out).
""".
-spec schedule_sweep(class_name(), gen()) -> ok.
schedule_sweep(Class, KeepGen) ->
    gen_server:cast(?MODULE, {sweep, Class, KeepGen}),
    ok.

-doc """
Delete every row for `Class` whose `gen` is not `KeepGen`, across all three
index tables. Runs inside the gen_server (the tables are `protected`), so it
never races a concurrent write to the same class.

`methods` rows are matched by their `{Class, _, _}` key with the owner gen
inside the value map; `senders` / `references` rows are keyed by selector /
referenced class with the owner + gen inside the value map. All three use a
`select_delete` guard `owner =:= Class andalso gen < KeepGen`.

The guard is `<` (strictly older), not `=/=`: when several writes to the
same class enqueue several sweep casts, an earlier sweep re-reads the live
`current_gen` (see `handle_cast`), but a row carrying a *newer* generation
than the one this sweep computed must never be deleted. Bounding deletion to
strictly-older generations makes every sweep idempotent and order-insensitive
— it only ever reclaims rows a later generation has already superseded.
""".
-spec sweep_stale_gens(class_name(), gen()) -> ok.
sweep_stale_gens(Class, KeepGen) ->
    %% Methods: key carries the class; value carries the gen.
    _ = ets:select_delete(
        ?METHODS_TABLE,
        [
            {
                {{Class, '_', '_'}, #{gen => '$1'}},
                [{'<', '$1', {const, KeepGen}}],
                [true]
            }
        ]
    ),
    StaleSiteSpec = [
        {
            {'_', #{owner => '$1', gen => '$2'}},
            [{'=:=', '$1', {const, Class}}, {'<', '$2', {const, KeepGen}}],
            [true]
        }
    ],
    _ = ets:select_delete(?SENDERS_TABLE, StaleSiteSpec),
    _ = ets:select_delete(?REFERENCES_TABLE, StaleSiteSpec),
    ok.

%%====================================================================
%% Internal: write helpers
%%====================================================================

-doc """
Insert all rows for a class's method_xref list under the given generation.
Each entry contributes one row to `methods`, plus one row per send to
`senders`, plus one row per reference to `references`.
""".
-spec insert_method_xref_rows(class_name(), [method_xref_entry()], gen()) -> ok.
insert_method_xref_rows(Class, MethodXref, Gen) ->
    lists:foreach(
        fun(Entry) -> insert_one_method(Class, Entry, Gen) end,
        MethodXref
    ),
    ok.

-spec insert_one_method(class_name(), method_xref_entry(), gen()) -> ok.
insert_one_method(Class, Entry, Gen) ->
    ClassSide = maps:get(class_side, Entry, false),
    Selector = maps:get(selector, Entry),
    Line = maps:get(line, Entry),
    SourceStatus = maps:get(source_status, Entry, indexed),
    Provenance = maps:get(provenance, Entry, class_body),

    MethodInfo = #{
        owner => Class,
        line => Line,
        source_status => SourceStatus,
        provenance => Provenance,
        gen => Gen
    },
    true = ets:insert(?METHODS_TABLE, {{Class, ClassSide, Selector}, MethodInfo}),

    Sends = maps:get(sends, Entry, []),
    lists:foreach(
        fun(Send) ->
            SendSelector = maps:get(selector, Send),
            Site = #{
                owner => Class,
                class_side => ClassSide,
                method => Selector,
                line => maps:get(line, Send, Line),
                recv_kind => maps:get(recv_kind, Send, other),
                gen => Gen
            },
            true = ets:insert(?SENDERS_TABLE, {SendSelector, Site})
        end,
        Sends
    ),

    References = maps:get(references, Entry, []),
    lists:foreach(
        fun(Ref) ->
            RefClass = maps:get(class, Ref),
            Site = #{
                owner => Class,
                class_side => ClassSide,
                method => Selector,
                line => maps:get(line, Ref, Line),
                gen => Gen
            },
            true = ets:insert(?REFERENCES_TABLE, {RefClass, Site})
        end,
        References
    ),
    ok.

-doc """
Delete all rows belonging to a class from every table, including the
class's generation counter. Idempotent.

`senders` and `references` are keyed by selector / referenced class, so
the class-owner predicate lives inside the value map. ETS match_spec
maps support partial-key pattern matching (`#{owner => '$1'}`), so we
use `select_delete` with a guard on the owner field rather than the
non-existent literal-map `match_delete` form.
""".
-spec do_purge_class(class_name()) -> ok.
do_purge_class(Class) ->
    %% Methods are keyed by {Class, ClassSide, Selector} — direct key match.
    true = ets:match_delete(?METHODS_TABLE, {{Class, '_', '_'}, '_'}),
    %% Senders / references — match on the value map's owner field.
    _ = ets:select_delete(
        ?SENDERS_TABLE,
        [{{'_', #{owner => '$1'}}, [{'=:=', '$1', {const, Class}}], [true]}]
    ),
    _ = ets:select_delete(
        ?REFERENCES_TABLE,
        [{{'_', #{owner => '$1'}}, [{'=:=', '$1', {const, Class}}], [true]}]
    ),
    true = ets:delete(?CLASS_GEN_TABLE, Class),
    ok.

-doc """
Delete rows for one specific method (Class, ClassSide, Selector).
Senders / references entries for the method are scrubbed by matching
the value map's owner + class_side + method fields via `select_delete`.
""".
-spec delete_method_rows(class_name(), class_side(), selector()) -> ok.
delete_method_rows(Class, ClassSide, Selector) ->
    true = ets:match_delete(?METHODS_TABLE, {{Class, ClassSide, Selector}, '_'}),
    MatchSpec = [
        {
            {'_', #{owner => '$1', class_side => '$2', method => '$3'}},
            [
                {'=:=', '$1', {const, Class}},
                {'=:=', '$2', {const, ClassSide}},
                {'=:=', '$3', {const, Selector}}
            ],
            [true]
        }
    ],
    _ = ets:select_delete(?SENDERS_TABLE, MatchSpec),
    _ = ets:select_delete(?REFERENCES_TABLE, MatchSpec),
    ok.
