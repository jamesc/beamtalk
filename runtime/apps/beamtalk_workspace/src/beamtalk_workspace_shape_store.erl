%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_shape_store).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Per-class shape-generation store (ADR 0105 Phase 2, BT-2780).

Companion to `beamtalk_workspace_signature_store` (BT-2777), but for a
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
workspace restart starts fresh. `clear/0` gives callers (`Workspace changes
revert:`, tests) an explicit reset without a restart.

## Known, accepted concurrency gap (adversarial review, BT-2780)

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
(BT-2780 adversarial review) narrows the *capture* half of this considerably
— every `capture/1` call this store ever sees is now serialised one at a
time through that worker's mailbox, so two `capture/1` calls can no longer
race *each other* — but does not and cannot close the outer install-ordering
race between two concurrently-reloading sessions. Not fixed here; closing it
fully would need the same source-generation-fenced write the findings store's
moduledoc already declined for the same complexity/benefit tradeoff.
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, prime/1, capture/1, previous/1, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([read_shape_from_meta/1]).
-endif.

-export_type([shape/0, maybe_shape/0]).

-type shape() :: beamtalk_shape_diff:shape().
-type maybe_shape() :: beamtalk_shape_diff:maybe_shape().

-record(state, {shapes = #{} :: #{binary() => shape()}}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the shape-generation store (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Seed `ClassNameBin`'s entry from the *currently-loaded* module's
`field_types`, but only if nothing is recorded for it yet this session. Call
before `code:load_binary` replaces the module — see the moduledoc's
"Two-phase capture" section. A no-op in effect (not an error) when the class
has never been loaded (a brand-new class): `read_shape_from_meta/1`
degrades to `undefined`, and an `undefined` seed is indistinguishable from
"nothing recorded" to `capture/1`'s diff — which is the correct `no_op`
outcome for a class with no previous generation.
""".
-spec prime(binary()) -> ok.
prime(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {prime, ClassNameBin}).

-doc """
Read the *installed* module's fresh `field_types`, diff it against whatever
`prime/1` (or an earlier `capture/1`) recorded, store the new generation for
next time, and return `{PreviousShape, DiffResult}` — see the moduledoc for
the self-seeding fallback when `prime/1` was never called for this reload.
Call after `code:load_binary` + `register_class/0` succeed.
""".
-spec capture(binary()) -> {maybe_shape(), beamtalk_shape_diff:diff_result()}.
capture(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {capture, ClassNameBin}).

-doc """
Read-only lookup of what `capture/1` would currently treat as "previous" for
`ClassNameBin` — from the store if this class has been primed/captured this
session, else `undefined`. Does not mutate the store, does not read live
meta. Exposed for the shape re-check trigger and for tests.
""".
-spec previous(binary()) -> maybe_shape().
previous(ClassNameBin) when is_binary(ClassNameBin) ->
    gen_server:call(?MODULE, {previous, ClassNameBin}).

-doc "Clear every recorded generation (`Workspace changes revert:`, tests).".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{}}.

handle_call({prime, ClassNameBin}, _From, State = #state{shapes = Shapes}) ->
    NewShapes =
        case maps:is_key(ClassNameBin, Shapes) of
            true -> Shapes;
            false -> Shapes#{ClassNameBin => read_shape_from_meta(ClassNameBin)}
        end,
    {reply, ok, State#state{shapes = NewShapes}};
handle_call({capture, ClassNameBin}, _From, State = #state{shapes = Shapes}) ->
    NewShape = read_shape_from_meta(ClassNameBin),
    Prev = maps:get(ClassNameBin, Shapes, NewShape),
    DiffResult = beamtalk_shape_diff:diff(Prev, NewShape),
    {reply, {Prev, DiffResult}, State#state{shapes = Shapes#{ClassNameBin => NewShape}}};
handle_call({previous, ClassNameBin}, _From, State = #state{shapes = Shapes}) ->
    {reply, maps:get(ClassNameBin, Shapes, undefined), State};
handle_call(clear, _From, State) ->
    {reply, ok, State#state{shapes = #{}}};
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

-doc """
Read `ClassNameBin`'s currently-installed `field_types` from
`__beamtalk_meta/0`, normalised to `shape()` (binary field name -> binary
type name, `<<"Dynamic">>` for an untyped field — the same sentinel
`beamtalk_workspace_signature_store:meta_type_to_binary/1` uses, so a
meta-seeded shape compares equal to a freshly-compiled one when nothing
changed). Best-effort: any resolution failure (class not registered, no
`__beamtalk_meta/0` exported, no `field_types` key) returns `undefined`
rather than raising — mirrors
`beamtalk_workspace_signature_store:seed_from_meta/3`.
""".
-spec read_shape_from_meta(binary()) -> maybe_shape().
read_shape_from_meta(ClassNameBin) ->
    try
        ClassAtom = binary_to_existing_atom(ClassNameBin, utf8),
        {ok, Module} = beamtalk_class_metadata:lookup_module(ClassAtom),
        %% A qualified remote call auto-loads an as-yet-unloaded module (see
        %% beamtalk_workspace_signature_store:seed_from_meta/3's identical
        %% reasoning) — matters for a rarely-touched class mid-session.
        Meta = Module:'__beamtalk_meta'(),
        FieldTypes = maps:get(field_types, Meta, #{}),
        maps:fold(
            fun(FieldAtom, TypeAtom, Acc) ->
                Acc#{atom_to_binary(FieldAtom, utf8) => field_type_to_binary(TypeAtom)}
            end,
            #{},
            FieldTypes
        )
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

-spec field_type_to_binary(atom()) -> binary().
field_type_to_binary(none) -> <<"Dynamic">>;
field_type_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
