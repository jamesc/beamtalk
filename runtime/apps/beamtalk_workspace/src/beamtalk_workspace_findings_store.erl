%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_findings_store).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Live store of reload-induced findings, keyed by `{caller class, changed
class}` (ADR 0105 Phase 1, BT-2779).

`beamtalk_recheck:trigger/4` (BT-2778) produces findings but does not keep
them anywhere — this module is where they live between reloads, and it is
the sole source of truth every surface (LSP, REPL, workspace/cockpit UI)
publishes from.

## Clearing-by-replacement (ADR 0105 §Mechanism step 4) — scoped per origin

The store's only write operation is `put_owner_origin/3`, which **replaces**
one caller's finding set *for one changed class* — never appends, never
merges, and never touches that caller's findings attributed to a
*different* changed class. This per-origin scoping is load-bearing, not
just tidiness: a caller can be broken by two independent reloads (`C` calls
both `A>>foo` and `B>>bar`; both get removed in separate reloads). If
replacement were scoped to the caller alone, re-checking `C` after `A`
reloads would `put_owner(C, [only-the-foo-finding])` and silently discard
`C`'s still-valid `B>>bar` finding — clearing-by-replacement would delete a
real, uninvestigated problem, which is worse than showing a stale one. Per
`{Owner, ChangedClass}` replacement is what the ADR's rule actually needs:
every re-check of a caller **against one specific changed class** replaces
*that changed class's* contribution to the caller's findings — clean or
different — while other changed classes' contributions are independent
buckets. Two otherwise-easy-to-miss cases still fall out of this for free,
now correctly scoped:

- **reload-fixes-reload** — a re-check that finds nothing calls
  `put_owner_origin(Owner, ChangedClass, [])`, which is exactly "clear this
  caller's findings *attributed to this changed class*" — no separate clear
  path needed, and a different changed class's findings for the same caller
  are untouched.
- **supersession** — back-to-back reloads of the *same* changed class each
  call `put_owner_origin/3` for the same `{Owner, ChangedClass}` key; the
  second call's replacement is unconditional, so generation-A findings can
  never survive alongside generation-B ones for that origin.

## Caller-cap staleness marking (ADR 0105, BT-2802, widened by BT-2828)

`beamtalk_recheck:apply_cap/2` keeps only the alphabetically-first N
candidates per reload; a candidate the cap drops is never re-checked that
reload, so `put_owner_origin/3` above is never called for it. Left alone,
an origin bucket recorded while that owner *was* within the cap would
silently keep asserting itself as current forever, even after whatever it
flagged is fixed upstream — the cap is a re-check *capacity* limit, not a
statement that the dropped candidates are fine. The same "never actually
re-verified" gap also reaches a candidate that stayed *inside* the cap but
whose individual re-check came back `skipped` (no live source recorded) or
`failed` (a compile/compiler-port error) — BT-2828. `get_origin/2` exists so
`beamtalk_repl_loader:maybe_run_recheck/4` can check, for each such
not-verified candidate (`beamtalk_recheck:result()`'s `not_verified_owners`
— the cap-dropped set unioned with the skipped/failed one), whether this
changed class already left a finding on it — and if so, overwrite that
finding's `note` in place (still via `put_owner_origin/3`, same replace
semantics) to say it was not re-verified this reload, rather than either
deleting it (could hide a real, still-live problem) or leaving it looking
freshly-verified.

`clear_owner/1` is the *other*, deliberately un-scoped operation: it wipes
**every** origin bucket for one caller class, fired whenever that caller's
*own* source changes (`beamtalk_repl_loader:maybe_trigger_recheck/4` calls
`clear_owner(ClassNameBin)` unconditionally before deciding whether a
dependent re-check is warranted). This is correct precisely because it is
un-scoped: when the caller's own source is reinstalled, every finding about
it — regardless of which upstream class originally caused it — has a
`start`/`end` byte offset into source that no longer exists, so the whole
entry must go. This is a deliberate broadening of the ADR's explicit
"clears on `Workspace changes revert:`" bullet: a revert re-installs a
method through the exact same install path as any other patch (`do_revert/2`
-> `install_revert_patch/4` / `revert_removal/3` -> `load_recompiled_method/8`
/ `remove_method/3`), so covering "any install clears that class's own
findings" satisfies the revert case as a special instance, with no bespoke
revert-specific hook, and also closes the same gap for a plain hand-edit
that fixes what a reload broke.

## Session-only, never persisted

State lives in this gen_server's `#state{}` map — plain in-memory, no ETS,
no disk — mirroring `beamtalk_workspace_signature_store` (BT-2777). It is
supervised under `beamtalk_workspace_sup` alongside the signature store, so a
workspace restart (a fresh BEAM node) starts a fresh, empty store — exactly
the ADR's "workspace restart... session state, never persisted" clearing
rule. `clear/0` gives callers (and tests) an explicit full reset without a
restart.

## Known, accepted concurrency gaps (adversarial review, BT-2779)

Two narrower races survive the per-origin scoping above — both accepted
rather than fixed, matching the risk tolerance
`beamtalk_workspace_signature_store`'s own moduledoc already sets for this
feature (a wrong/stale finding is noise, not data corruption, on an
advisory-only surface):

1. **Concurrent reloads of the *same* `{Owner, ChangedClass}` origin** (two
   sessions racing to save the exact same method) can interleave so
   whichever `put_owner_origin/3` call reaches this gen_server last wins —
   indistinguishable from ordinary sequential supersession from the store's
   point of view, but which write "wins" is scheduling-determined rather
   than reload-chronology-determined when they are genuinely concurrent.
   There is no meaningful "correct" answer to reconcile against here (both
   writes reflect a real, just-computed generation) — the same shape of gap
   `beamtalk_workspace_signature_store`'s rollback race already documents.
2. **A caller's own `clear_owner/1` racing a dependent's `put_owner_origin/3`
   for that same caller**: if caller `C` is being edited (triggering
   `clear_owner(C)`) at the same moment a *different* reload's re-check of
   `C` (as a dependent) is mid-flight, and that re-check's `beamtalk_recheck:
   trigger/4` read `C`'s live source *before* the edit landed, its finding
   can land in the store *after* `clear_owner(C)` — leaving one origin
   bucket stale until something else touches `C`. Fully closing this would
   need either serialising every `{Owner, _}` write for a caller through one
   gen_server call embedding the read (source-generation-fenced, not just
   time-fenced) or accepting the same complexity `beamtalk_workspace_signature_store`
   already declined for its own concurrent-rollback race. Not fixed here.

Neither gap is reachable by a *single* session's normal edit-save-reload
loop — both require two independent sessions racing the same
caller/selector within the same request window.

## Known gap: no reconcile-on-restart for already-attached clients

A crash + supervisor restart of *this* gen_server (distinct from a full
workspace/node restart) starts a fresh, empty store — correct for a client
that reconnects afterwards, since it re-subscribes and/or re-reads
`all/0` fresh. But a client that was **already attached** before the crash
(an open LSP session, a live workspace/cockpit UI tab) has no signal that
the store reset: it keeps showing whatever reload-induced diagnostics it
had cached until the *next* reload happens to touch that specific caller
again (which replaces its entry) or the client itself reconnects. Not fixed
here — this store's own restart is expected to be rare (its operations are
pure map manipulation with no I/O to fail on), and a full fix would need
either a generation counter surfaced to every consumer (so a stale cache
can be told apart from a merely-quiet one) or a broadcast on `init/1`
telling every live subscriber "treat everything you're showing as
provisional until your next push" — meaningful protocol surface for a rare
edge case on an already-advisory-only feature.
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, put_owner_origin/3, clear_owner/1, get_origin/2, for_owner/1, all/0, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export_type([finding/0]).

-type finding() :: beamtalk_recheck:finding().

%% `{Owner, ChangedClass}` — see the moduledoc's "Clearing-by-replacement"
%% section for why replacement is scoped to this pair, not the owner alone.
-type origin_key() :: {binary(), binary()}.

-record(state, {by_origin = #{} :: #{origin_key() => [finding()]}}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the findings store (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Replace `OwnerBin`'s finding set *attributed to `ChangedClassBin`* with
`Findings` (possibly `[]`) — findings attributed to any other changed class
for the same `OwnerBin` are untouched (see the moduledoc for why this
per-origin scoping is load-bearing, not cosmetic). Returns the *previous*
list for this exact `{OwnerBin, ChangedClassBin}` pair, so a caller can
detect "was there anything to clear" without a separate read
(`beamtalk_repl_loader` uses this to decide whether an install that produced
no dependent re-check is still worth announcing, because it silently cleared
a stale finding).
""".
-spec put_owner_origin(binary(), binary(), [finding()]) -> [finding()].
put_owner_origin(OwnerBin, ChangedClassBin, Findings) when
    is_binary(OwnerBin), is_binary(ChangedClassBin), is_list(Findings)
->
    gen_server:call(?MODULE, {put_owner_origin, OwnerBin, ChangedClassBin, Findings}).

-doc """
Clear **every** origin bucket for `OwnerBin` — unlike `put_owner_origin/3`,
this is deliberately un-scoped (see the moduledoc: `OwnerBin`'s own source
just changed, so every finding about it is stale regardless of which
changed class originally produced it). Returns the previous flattened list
across all origins, so a caller can detect "was there anything to clear".
""".
-spec clear_owner(binary()) -> [finding()].
clear_owner(OwnerBin) when is_binary(OwnerBin) ->
    gen_server:call(?MODULE, {clear_owner, OwnerBin}).

-doc "Read-only lookup of `OwnerBin`'s current finding set (every origin, flattened). Does not mutate the store.".
-spec for_owner(binary()) -> [finding()].
for_owner(OwnerBin) when is_binary(OwnerBin) ->
    gen_server:call(?MODULE, {for_owner, OwnerBin}).

-doc """
Read-only lookup of the single `{OwnerBin, ChangedClassBin}` origin bucket
(BT-2802) — `[]` when nothing is stored for that exact pair. Unlike
`for_owner/1`, this does not flatten across origins: a caller
(`beamtalk_repl_loader:maybe_run_recheck/4`) needs to know whether *this
specific* changed class already left a finding on `OwnerBin` before deciding
whether there is anything to mark stale when `OwnerBin` gets dropped by the
caller cap on a later reload of the same changed class. Does not mutate the
store.
""".
-spec get_origin(binary(), binary()) -> [finding()].
get_origin(OwnerBin, ChangedClassBin) when is_binary(OwnerBin), is_binary(ChangedClassBin) ->
    gen_server:call(?MODULE, {get_origin, OwnerBin, ChangedClassBin}).

-doc """
Every currently-live finding across every caller class and origin,
flattened. Sorted by `{owner, selector, message}` for a deterministic order
— callers that render this list (REPL notice, workspace UI) should not
depend on insertion order.
""".
-spec all() -> [finding()].
all() ->
    gen_server:call(?MODULE, all).

-doc """
Clear every recorded finding for every owner and origin (workspace restart
is the natural clear via supervision; this is the explicit reset for
`Workspace changes revert:` callers that want a full wipe, and for tests).
""".
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{}}.

handle_call(
    {put_owner_origin, OwnerBin, ChangedClassBin, Findings},
    _From,
    State = #state{by_origin = ByOrigin}
) ->
    Key = {OwnerBin, ChangedClassBin},
    Prev = maps:get(Key, ByOrigin, []),
    NewByOrigin =
        case Findings of
            [] -> maps:remove(Key, ByOrigin);
            _ -> ByOrigin#{Key => Findings}
        end,
    {reply, Prev, State#state{by_origin = NewByOrigin}};
handle_call({clear_owner, OwnerBin}, _From, State = #state{by_origin = ByOrigin}) ->
    OwnedKeys = [Key || {Owner, _ChangedClass} = Key <- maps:keys(ByOrigin), Owner =:= OwnerBin],
    Prev = lists:append([maps:get(Key, ByOrigin) || Key <- OwnedKeys]),
    Rest = maps:without(OwnedKeys, ByOrigin),
    {reply, Prev, State#state{by_origin = Rest}};
handle_call({for_owner, OwnerBin}, _From, State = #state{by_origin = ByOrigin}) ->
    Findings = lists:append([
        F
     || {{Owner, _ChangedClass}, F} <- maps:to_list(ByOrigin), Owner =:= OwnerBin
    ]),
    {reply, Findings, State};
handle_call(
    {get_origin, OwnerBin, ChangedClassBin}, _From, State = #state{by_origin = ByOrigin}
) ->
    Findings = maps:get({OwnerBin, ChangedClassBin}, ByOrigin, []),
    {reply, Findings, State};
handle_call(all, _From, State = #state{by_origin = ByOrigin}) ->
    All = lists:append(maps:values(ByOrigin)),
    Sorted = lists:sort(fun sort_key/2, All),
    {reply, Sorted, State};
handle_call(clear, _From, State) ->
    {reply, ok, State#state{by_origin = #{}}};
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

-spec sort_key(finding(), finding()) -> boolean().
sort_key(#{owner := O1, selector := S1, message := M1}, #{
    owner := O2, selector := S2, message := M2
}) ->
    {O1, S1, M1} =< {O2, S2, M2}.
