%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% ChangeEntry record, its field types and the table/ring constants, shared
%%% by `beamtalk_workspace_changelog` and the modules split out of it
%%% (`beamtalk_workspace_changelog_codec`, `beamtalk_workspace_changelog_log`,
%%% `beamtalk_workspace_changelog_views`). BT-4907.

-define(ETS_TABLE, beamtalk_changelog_entries).
%% Bounded ring: keep at most this many entries on disk before rotating older
%% segments into archive/ (ADR 0082, "ChangeLog growth").
-define(MAX_ENTRIES, 1000).

%%% ----------------------------------------------------------------------------
%%% Types
%%% ----------------------------------------------------------------------------

%% `kind` is an open enum (ADR 0082): newer writers may add values this beam does
%% not know. Decoding maps any unrecognised value to `unknown` so history is
%% preserved across versions rather than dropped. `'remove-method'` is ADR
%% 0112's method-removal kind. `'class-def'` is ADR 0082's
%% extension for redefining an *existing* class's whole definition —
%% the cockpit `:def` tab's "Compile" action, as opposed to `'new-class'`
%% (a brand-new class created via `newClass:at:`). `'rename-class'`/
%% `'rename-method'` are ADR 0114's `renameTo:`/`renameSelector:to:` kinds
%% — the first two kinds whose rewrite spans a *set* of files
%% (`sites`/`candidate_sites`) rather than one, see those fields' docs below.
-type kind() :: beamtalk_workspace_kinds:kind().
%% ADR 0112: which method table a `'remove-method'` entry targets. Stored
%% explicitly only for that kind — legacy `instance`/`class`-kind patch
%% entries derive their side from `kind` itself (`entry_side/1`), so the field
%% is additive, not a breaking schema change (ADR 0112 § ChangeLog interaction).
%% ADR 0114: `'rename-method'` stores `side` the same explicit way;
%% `'rename-class'` always has `side = undefined` (null) — a class identity
%% change has no method-table side.
-type side() :: instance | class.
-type intent() :: durable | ephemeral | unknown.
-type author_kind() :: human | agent | unknown.
-type span() :: #{start := non_neg_integer(), 'end' := non_neg_integer()}.

%% ADR 0114: one rewritten reference location in a `'rename-class'`/
%% `'rename-method'` entry's `sites` list. `source_ref`/`prev_source_ref` name
%% the recorded pre/post rewrite bodies exactly like the top-level fields do
%% for a single-file kind (undefined for a site not yet populated with a
%% recorded body — the site-discovery/rewrite mechanism itself is
%% out of scope here). A bare `undefined` in place of a `site()` map (rather
%% than a map with `source_file = undefined`) is the ADR's documented
%% `sites[0] = null` case: a dynamic (ClassBuilder) class being renamed has no
%% backing file for its own declaration site to point at.
-type site() :: #{
    source_file := binary() | undefined,
    span := span() | undefined,
    source_ref := binary() | undefined,
    prev_source_ref := binary() | undefined
}.

%% ADR 0114: one reported-but-never-rewritten sender in a
%% `'rename-method'` entry's `candidate_sites` list. No `source_ref`/
%% `prev_source_ref` — nothing here is ever spliced, so there is no prior/new
%% body to record (ADR 0114 § ChangeLog schema).
-type candidate_site() :: #{
    source_file := binary(),
    span := span()
}.

%% A ChangeEntry as stored in memory. Bodies are not kept in the record —
%% only the `source_ref` / `prev_source_ref` filenames — so the ETS footprint
%% stays small regardless of method size. The bodies live as files in sources/.
-record(entry, {
    seq :: non_neg_integer(),
    ts :: integer(),
    epoch :: non_neg_integer(),
    class :: binary(),
    selector :: binary() | undefined,
    kind :: kind(),
    %% `undefined` for every entry except a `'remove-method'` one — read via
    %% `entry_side/1`, never this field directly.
    side :: side() | undefined,
    %% `undefined` for a `'remove-method'` entry (ADR 0112: nothing replaces
    %% the deleted text, so there is no new body to store).
    source_ref :: binary() | undefined,
    prev_source_ref :: binary() | undefined,
    source_file :: binary() | undefined,
    span :: span() | undefined,
    %% ADR 0114: `'rename-class'`-only — the pre-rename class name.
    %% `undefined` for every other kind.
    old_class :: binary() | undefined,
    %% ADR 0114: `'rename-method'`-only — the pre-rename selector
    %% (`selector` itself holds the *new* selector, mirroring how `class`
    %% holds the *new* name for `'rename-class'`). `undefined` for every
    %% other kind.
    old_selector :: binary() | undefined,
    %% ADR 0114: `'rename-class'`-only — the file path before/after
    %% a rename that also moves the backing file (or a pure `Workspace
    %% moveClass:to:` move). `undefined` for a dynamic class (no backing
    %% file) and for every other kind.
    old_path :: binary() | undefined,
    new_path :: binary() | undefined,
    %% ADR 0114: `'rename-class'`/`'rename-method'`-only — the
    %% multi-site shape neither field above can express: `sites[0]` is always
    %% the definition/declaration site, `sites[1..]` are every other rewritten
    %% reference. `undefined` (not `[]`) for every other kind, matching the
    %% "undefined means not applicable" convention every other optional field
    %% here already follows.
    sites :: [site() | undefined] | undefined,
    %% ADR 0114: `'rename-method'`-only — the reported, never
    %% auto-rewritten candidate sender list. `undefined` for every other kind
    %% (including `'rename-class'`, which has no candidate tier).
    candidate_sites :: [candidate_site()] | undefined,
    intent :: intent(),
    flushable :: boolean(),
    not_flushable_reason :: binary() | undefined,
    author :: binary(),
    author_kind :: author_kind(),
    %% True once a `Workspace flush` has written this entry's patch to disk
    %% (ADR 0082 Phase 2) — OR, for a `'remove-class'` entry specifically,
    %% once `Workspace changes revert:` has undone it (ADR 0113), OR,
    %% the same way, for a `'rename-class'`/`'rename-method'` entry once its
    %% own `revert:` has undone it (ADR 0114): no disk write
    %% happened, but the entry's effect is equally resolved and must equally
    %% drop out of the active/pending view. Persisted so the
    %% entry stays excluded from the active view across workspace restarts:
    %% history is preserved in the log for audit, but the entry is no longer
    %% considered "dirty". Don't read this field alone as "this reached disk"
    %% — for that, check the entry's `kind` too.
    flushed = false :: boolean(),
    %% True when `flushed` was set by retiring the entry (a never-flushed new
    %% class was removed, BT-5016) rather than by writing it to disk: nothing
    %% reached disk, so history must not report it as flushed (BT-5084).
    cancelled = false :: boolean(),
    %% Derived, in-memory only — not persisted (recomputed on restart).
    prior_epoch = false :: boolean(),
    orphan = false :: boolean()
}).
