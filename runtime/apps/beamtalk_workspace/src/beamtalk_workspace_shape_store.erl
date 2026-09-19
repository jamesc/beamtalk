%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_store).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Per-class shape-generation store (ADR 0105 Phase 2).

Companion to `beamtalk_workspace_signature_store`, but for a
class's *shape* — its `state:`/`field:` slot set and their declared types —
rather than a single method's signature. A full class-body reload (the
REPL's inline `subclass:` redefinition, `:load <file>`, or a file reload
after an on-disk edit — `beamtalk_repl_loader:load_class_module/3`,
`load_compiled_module/6`, `reload_compile_and_load/4`) replaces the class's
compiled module wholesale, and with it `__beamtalk_meta/0`'s `field_types`
map — so, exactly as ADR 0105's Context caveat documents for method
signatures, the pre-reload shape is gone from live class state the moment
the new module installs. This store is the plumbing that survives that
replacement.

## Two-phase capture, not one

`beamtalk_workspace_signature_store:capture/4` is a single call because its
caller already has the freshly-compiled signature in hand *before* install
(the compiler port response, ADR 0105 Mechanism step 1). A full class
reload has no equivalent "new shape" value computed ahead of the install —
the simplest, plumbing-free way to learn it is to read the *installed*
module's own `__beamtalk_meta/0` afterwards, exactly as
`beamtalk_workspace_signature_store:seed_from_meta/3` already does for a
first-ever method capture. That splits capture into two calls, both reading
`__beamtalk_meta/0` on the currently-loaded module of `ClassNameBin` at two
different times:

- `prime/1` — called *before* `code:load_binary` replaces the module. Seeds
  the store from the about-to-be-replaced module's *current* `field_types`,
  but **only if nothing is recorded yet** for this class this session (the
  same "seed once" laziness `seed_from_meta/3` uses) — an already-captured
  entry from an earlier reload this session already *is* the correct
  previous generation and must not be re-read (the module it would read
  from at that point is about to be gone anyway).
- `capture/1` — called *after* `code:load_binary` + `register_class/0`
  succeed. Reads the *installed* module's fresh `field_types`, diffs it
  against whatever `prime/1` last recorded, and stores the new generation
  for next time. A caller that reaches `capture/1` without ever calling
  `prime/1` first (the method-patch, method-removal, new-class, and
  protocol install paths, none of which change a class's shape) makes this
  self-seed from the same *just-installed* value it is about to diff
  against, which always classifies as `no_op` — harmless by construction,
  never a false positive.

## No rollback

Unlike `beamtalk_workspace_signature_store:capture/4`, there is no
`rollback/N`: `prime/1` never overwrites an existing entry and only ever
seeds from whatever module is *currently* live, so a subsequent
`code:load_binary` failure leaves the store exactly as accurate as it was
before the failed attempt — there is nothing to undo.

## Session-only, never persisted

Mirrors `beamtalk_workspace_signature_store`: state lives in this
gen_server's `#state{}` map, supervised under `beamtalk_workspace_sup`, so a
workspace restart starts fresh. `Workspace changes revert:` does not call
`clear/0` — a revert re-installs a method/class through the normal
`prime/1`/`capture/1` path, which already keeps this store's per-class
generations correct one class at a time. `clear/0` is test-only: it gives
tests an explicit full reset without a restart.

## Flattened shape, and the generation record (ADR 0123 Phase 4, BT-3538)

A captured generation is no longer a bare `shape()` (own-class `field_types`
only): `read_shape_from_meta/1` (still exported for TEST, and still returning
a bare `shape()`) now **flattens** the class's own `field_types` with every
ancestor's, so a superclass-only field change shows up in a concrete
subclass's diff too, even on a reload that never touches the superclass's own
module. The ancestor walk reads `beamtalk_class_metadata` only (module +
superclass, both ETS-resident) — deliberately **not**
`beamtalk_behaviour_intrinsics:classAllFieldTypesByName/1`, which needs a
*live* class-registry process (`beamtalk_class_registry:whereis_class/1`) per
ancestor; this store's own contract (mirrored from `prime/1`'s "seed once"
degrade) only ever required a class to be *registered* in
`beamtalk_class_metadata`, never a live process, and this module's tests
build fixtures that way. Both walks share `beamtalk_hierarchy:walk_ancestors/3`
(the one depth-guarded ancestor-walk primitive every hierarchy walker in this
codebase is built on), so the two are a shared-leaf-module split on *how a
node is probed* (registry pid vs. metadata row), not two independent
reimplementations of *how to walk and merge*.

`prime/1`/`capture/1` now store and return a `generation()` — the flattened
`shape()` plus the two `'shape_version'`/`'shape_migrations'` keys BT-3537's
language surface populates (`beamtalk_shape_diff:generation/0`) — so a
caller can join `capture/1`'s field-level `diff_result()` against the
previous and new generation's version history
(`beamtalk_shape_diff:reload_findings/4`) without a second read of
`__beamtalk_meta/0`.

## Known, accepted concurrency gap (adversarial review)

`prime/1` runs synchronously on the reloading session's own process,
*before* `code:load_binary` — it is not, and cannot be, serialised against a
second session reloading the *same* class name concurrently (each session's
install sequence — prime, `code:load_binary`, `capture/1` via
`beamtalk_workspace_shape_recheck_worker` — runs on its own process; only
this store's own gen_server calls, and now `capture/1`'s enqueued recheck
work, are serialised against each other). Two sessions racing to reload the
same class can therefore interleave their prime/install steps in a way
where a `capture/1` reads a *different* installed generation than the
`prime/1` it is nominally diffing against — the same shape of gap
`beamtalk_workspace_findings_store`'s moduledoc already accepts for its own
narrower cross-session races, on the same reasoning: a wrong/stale shape
diff here is advisory noise, not data corruption, and reaching it requires
two independent sessions genuinely racing an edit to the same class, not a
single session's normal edit-reload loop. `beamtalk_workspace_shape_recheck_worker`
(from the adversarial review above) narrows the *capture* half of this considerably
— every `capture/1` call this store ever sees is now serialised one at a
time through that worker's mailbox, so two `capture/1` calls can no longer
race *each other* — but does not and cannot close the outer install-ordering
race between two concurrently-reloading sessions. Not fixed here; closing it
fully would need the same source-generation-fenced write the findings store's
moduledoc already declined for the same complexity/benefit tradeoff.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([start_link/0, prime/1, capture/1, previous/1, clear/0, field_type_to_binary/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([read_shape_from_meta/1, read_generation_from_meta/1, ancestor_field_types/1]).
-endif.

-export_type([shape/0, maybe_shape/0, generation/0, maybe_generation/0]).

-type shape() :: beamtalk_shape_diff:shape().
-type maybe_shape() :: beamtalk_shape_diff:maybe_shape().
-type generation() :: beamtalk_shape_diff:generation().
-type maybe_generation() :: beamtalk_shape_diff:maybe_generation().

-record(state, {generations = #{} :: #{binary() => generation()}}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the shape-generation store (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Seed `ClassNameBin`'s entry from the *currently-loaded* module's flattened
generation (`shape()` + `'shape_version'`/`'shape_migrations'`), but only if
nothing is recorded for it yet this session. Call before `code:load_binary`
replaces the module — see the moduledoc's "Two-phase capture" section. A
no-op in effect (not an error) when the class has never been loaded (a
brand-new class): `read_generation_from_meta/1` degrades to `undefined`, and
an `undefined` seed is indistinguishable from "nothing recorded" to
`capture/1`'s diff — which is the correct `no_op` outcome for a class with no
previous generation.
""".
-spec prime(binary()) -> ok.
prime(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {prime, ClassNameBin}).

-doc """
Read the *installed* module's fresh generation, diff its flattened `shape()`
against whatever `prime/1` (or an earlier `capture/1`) recorded, store the
new generation for next time, and return `{PreviousGeneration,
NewGeneration, DiffResult}` — see the moduledoc for the self-seeding
fallback when `prime/1` was never called for this reload. Call after
`code:load_binary` + `register_class/0` succeed.

`NewGeneration` carries the just-installed `'shape_version'`/
`'shape_migrations'` alongside its shape — a caller joining `DiffResult`
against version history (`beamtalk_shape_diff:reload_findings/4`, ADR 0123
§4) needs both generations, not just the previous one `DiffResult` alone
implies.
""".
-spec capture(binary()) ->
    {maybe_generation(), generation(), beamtalk_shape_diff:diff_result()}.
capture(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {capture, ClassNameBin}).

-doc """
Read-only lookup of what `capture/1` would currently treat as "previous" for
`ClassNameBin` — from the store if this class has been primed/captured this
session, else `undefined`. Does not mutate the store, does not read live
meta. Exposed for the shape re-check trigger and for tests.
""".
-spec previous(binary()) -> maybe_generation().
previous(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {previous, ClassNameBin}).

-doc "Clear every recorded generation. Test-only (see the moduledoc).".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{}}.

handle_call({prime, ClassNameBin}, _From, State = #state{generations = Generations}) ->
    NewGenerations =
        case maps:is_key(ClassNameBin, Generations) of
            true -> Generations;
            false -> Generations#{ClassNameBin => read_generation_from_meta(ClassNameBin)}
        end,
    {reply, ok, State#state{generations = NewGenerations}};
handle_call({capture, ClassNameBin}, _From, State = #state{generations = Generations}) ->
    NewGen = read_generation_from_meta(ClassNameBin),
    Prev = maps:get(ClassNameBin, Generations, NewGen),
    DiffResult = beamtalk_shape_diff:diff(generation_shape(Prev), generation_shape(NewGen)),
    {reply, {Prev, NewGen, DiffResult}, State#state{
        generations = Generations#{ClassNameBin => NewGen}
    }};
handle_call({previous, ClassNameBin}, _From, State = #state{generations = Generations}) ->
    {reply, maps:get(ClassNameBin, Generations, undefined), State};
handle_call(clear, _From, State) ->
    {reply, ok, State#state{generations = #{}}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec generation_shape(maybe_generation()) -> maybe_shape().
generation_shape(undefined) -> undefined;
generation_shape(#{shape := Shape}) -> Shape.

-doc """
Read `ClassNameBin`'s own currently-installed `__beamtalk_meta/0`, if
resolvable. Shared by `read_shape_from_meta/1` and
`read_generation_from_meta/1` so both read the meta map exactly once.
Best-effort: any resolution failure (class not registered, no
`__beamtalk_meta/0` exported) returns `undefined` rather than raising —
mirrors `beamtalk_workspace_signature_store:seed_from_meta/3`.
""".
-spec read_own_meta(binary()) -> {ok, atom(), map()} | undefined.
read_own_meta(ClassNameBin) ->
    try
        ClassAtom = binary_to_existing_atom(ClassNameBin, utf8),
        {ok, Module} = beamtalk_class_metadata:lookup_module(ClassAtom),
        %% A qualified remote call auto-loads an as-yet-unloaded module (see
        %% beamtalk_workspace_signature_store:seed_from_meta/3's identical
        %% reasoning) — matters for a rarely-touched class mid-session.
        Meta = Module:'__beamtalk_meta'(),
        {ok, ClassAtom, Meta}
    catch
        %% Expected, ordinary resolution failures — silent, no log:
        %%   badarg   — ClassNameBin isn't an atom yet (brand-new this session).
        %%   {badmatch, not_found} — class not registered in beamtalk_class_metadata.
        %%   undef    — Module has no __beamtalk_meta/0 (or no code at all).
        error:badarg ->
            undefined;
        error:{badmatch, not_found} ->
            undefined;
        error:undef ->
            undefined;
        %% Anything else is unexpected — still degrade to `undefined` (this
        %% function must never crash the prime/capture hook), but log it so a
        %% real bug doesn't silently masquerade as "no baseline to compare
        %% against".
        Class:Reason:Stack ->
            ?LOG_WARNING(
                "Unexpected failure reading shape from __beamtalk_meta/0",
                #{
                    error_class => Class,
                    reason => Reason,
                    stack => Stack,
                    class => ClassNameBin,
                    domain => [beamtalk, runtime]
                }
            ),
            undefined
    end.

-doc """
Read `ClassNameBin`'s currently-installed, **flattened** `field_types`
(ADR 0123 Phase 4, BT-3538: own class merged with every ancestor's, closer
levels winning), normalised to `shape()` (binary field name -> binary type
name, `<<"Dynamic">>` for an untyped field — the same sentinel
`beamtalk_workspace_signature_store:meta_type_to_binary/1` uses, so a
meta-seeded shape compares equal to a freshly-compiled one when nothing
changed). Best-effort: any resolution failure on `ClassNameBin` itself (class
not registered, no `__beamtalk_meta/0` exported) returns `undefined` rather
than raising; an ancestor that fails to resolve simply contributes no fields
at its level (see the moduledoc's "Flattened shape" section) rather than
failing the whole read.
""".
-spec read_shape_from_meta(binary()) -> maybe_shape().
read_shape_from_meta(ClassNameBin) ->
    case read_own_meta(ClassNameBin) of
        undefined ->
            undefined;
        {ok, ClassAtom, Meta} ->
            normalize_field_types(flattened_field_types(ClassAtom, Meta))
    end.

-doc """
Like `read_shape_from_meta/1` (reused directly for the `shape` field — the
`capture`/`prime` handlers below call this, not `read_shape_from_meta/1`
itself, which is why that function stays reachable in a production build),
but returns the full `generation()`: the flattened `shape()`, the
un-flattened `own_shape()` (this class's own `field_types` only), the
un-flattened `ancestor_shape()` (BT-3560: every ancestor's own `field_types`,
none of this class's own — see the moduledoc/`beamtalk_shape_diff:
generation/0`'s doc for why a pre-save precheck needs `ancestor_shape`
captured directly rather than derived from `shape`/`own_shape`), and
`'shape_version'` (default `1`, BT-3537) / `'shape_migrations'` (default
`#{}}`). `undefined` under the exact same conditions `read_shape_from_meta/1`
degrades under (they share `read_own_meta/1`).
""".
-spec read_generation_from_meta(binary()) -> maybe_generation().
read_generation_from_meta(ClassNameBin) ->
    case read_own_meta(ClassNameBin) of
        undefined ->
            undefined;
        {ok, ClassAtom, Meta} ->
            #{
                shape => read_shape_from_meta(ClassNameBin),
                own_shape => normalize_field_types(maps:get(field_types, Meta, #{})),
                ancestor_shape => normalize_field_types(ancestor_field_types(ClassAtom)),
                version => maps:get(shape_version, Meta, 1),
                migrations => maps:get(shape_migrations, Meta, #{})
            }
    end.

-doc """
`ClassAtom`'s own `field_types` (from its already-read `OwnMeta`) merged with
every ancestor's own `field_types` (`ancestor_field_types/1`) — the
closer-to-`ClassAtom` level always wins a same-named-field conflict, via
`maps:merge/2`'s "second argument wins" rule with `OwnFieldTypes` as the
second argument.
""".
-spec flattened_field_types(atom(), map()) -> #{atom() => atom()}.
flattened_field_types(ClassAtom, OwnMeta) ->
    OwnFieldTypes = maps:get(field_types, OwnMeta, #{}),
    maps:merge(ancestor_field_types(ClassAtom), OwnFieldTypes).

-doc """
`ClassAtom`'s ancestor-only `field_types` contribution (BT-3560): every
ancestor's own `field_types`, walked via `beamtalk_class_metadata` (module +
superclass, ETS-resident) and merged closer-ancestor-wins — but, unlike
`flattened_field_types/2`, **never** folding in `ClassAtom`'s own fields. This
is what a pre-save precheck (`beamtalk_repl_loader:precheck_class_shape/2`)
needs to recompute a *pending* edit's flattened shape without re-walking the
ancestor chain: the ancestor contribution alone, which a same-class edit
cannot itself have changed, kept separate from `own_shape` rather than folded
into `shape` and later subtracted back out — a subtraction that is lossy for
a shadowed ancestor field (see `beamtalk_shape_diff:generation/0`'s doc). See
the moduledoc's "Flattened shape" section for why this walk does not reuse
`beamtalk_behaviour_intrinsics:classAllFieldTypesByName/1`. Values are still
raw atoms at this point (`field_type_to_binary/1` normalises after).
""".
-spec ancestor_field_types(atom()) -> #{atom() => atom()}.
ancestor_field_types(ClassAtom) ->
    case beamtalk_class_metadata:lookup_superclass(ClassAtom) of
        {ok, none} -> #{};
        {ok, Super} -> merge_ancestor_field_types(Super, #{});
        not_found -> #{}
    end.

-doc """
Walk from `StartSuper` up to the hierarchy root via
`beamtalk_hierarchy:walk_ancestors/3` (the shared depth-guarded ancestor
walker every hierarchy walk in this codebase is built on), merging each
level's own `field_types` into `AccSoFar` — `AccSoFar` (the more-derived
levels already folded in) always wins a conflict, `maps:merge/2`'s "second
argument wins" applied with the accumulator as the second argument at every
step. Degrades to the partial merge on a hierarchy cycle
(`?MAX_HIERARCHY_DEPTH` exceeded), logging a warning naming the cycle point —
the same degrade `beamtalk_behaviour_intrinsics:walk_hierarchy/3` uses.
""".
-spec merge_ancestor_field_types(atom(), #{atom() => atom()}) -> #{atom() => atom()}.
merge_ancestor_field_types(StartSuper, AccSoFar) ->
    StepFun = fun({CurrentClass, Acc}, _Depth) ->
        NewAcc = maps:merge(ancestor_own_field_types(CurrentClass), Acc),
        case beamtalk_class_metadata:lookup_superclass(CurrentClass) of
            {ok, none} -> {found, NewAcc};
            {ok, Super} -> {next, {Super, NewAcc}};
            not_found -> {found, NewAcc}
        end
    end,
    case beamtalk_hierarchy:walk_ancestors({StartSuper, AccSoFar}, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, Result} ->
            Result;
        {max_depth_exceeded, {CycleClass, Partial}} ->
            ?LOG_WARNING(
                "Shape flatten: max hierarchy depth ~p exceeded at ~p — possible cycle",
                [?MAX_HIERARCHY_DEPTH, CycleClass],
                #{domain => [beamtalk, runtime]}
            ),
            Partial;
        not_found ->
            %% Unreachable: StepFun above always resolves to {found, _} — a
            %% `none` superclass or an unregistered ancestor is translated to
            %% a terminal {found, _}. walk_ancestors/3 only returns not_found
            %% for a `none` StartNode, and every caller here only invokes
            %% this with a real (non-`none`) StartSuper.
            AccSoFar
    end.

-doc """
An ancestor's own `field_types`, tolerant of every way it can be unavailable
(unregistered, no `__beamtalk_meta/0`, a non-map or raising meta call) —
mirrors `beamtalk_behaviour_intrinsics:meta_for_module/1`'s degrade, since a
dynamic/legacy ancestor contributing nothing at its level must not fail the
whole flatten (see the moduledoc).

Deliberately a **qualified call inside a `try`**, not an
`erlang:function_exported/3` pre-check: `function_exported/3` only consults
the currently-*loaded* module table and never triggers a load, so an
ancestor whose module happens not to be loaded yet this session (rather than
genuinely lacking `__beamtalk_meta/0`) would wrongly read back `#{}` — the
same "qualified call auto-loads" reasoning `read_own_meta/1` already relies
on for the starting class. `error:undef` is the "no such function" case this
degrades on; anything else escapes to the outer catch-all.
""".
-spec ancestor_own_field_types(atom()) -> #{atom() => atom()}.
ancestor_own_field_types(ClassAtom) ->
    case beamtalk_class_metadata:lookup_module(ClassAtom) of
        {ok, Module} ->
            try Module:'__beamtalk_meta'() of
                Meta when is_map(Meta) -> maps:get(field_types, Meta, #{});
                _ -> #{}
            catch
                error:undef -> #{};
                _:_ -> #{}
            end;
        not_found ->
            #{}
    end.

-spec normalize_field_types(#{atom() => atom()}) -> shape().
normalize_field_types(FieldTypes) ->
    maps:fold(
        fun(FieldAtom, TypeAtom, Acc) ->
            Acc#{atom_to_binary(FieldAtom, utf8) => field_type_to_binary(TypeAtom)}
        end,
        #{},
        FieldTypes
    ).

-doc """
The `none` -> `<<"Dynamic">>` sentinel normalisation every `field_types`
read in this module uses. Exported (not `-ifdef(TEST)`-gated, unlike
`read_shape_from_meta/1`/`read_generation_from_meta/1`) so
`beamtalk_repl_loader:precheck_class_shape/2`'s pending-generation read can
normalise a *pending*, not-yet-captured `field_types` map the exact same way
without a second implementation of this one-line rule (CLAUDE.md's
no-duplicate-implementations rule).
""".
-spec field_type_to_binary(atom()) -> binary().
field_type_to_binary(none) -> <<"Dynamic">>;
field_type_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
