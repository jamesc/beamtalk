%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_revert).

%%% **DDD Context:** Workspace Context

-moduledoc """
ChangeLog revert machinery (ADR 0082 Phase 4; `remove-method`/`remove-class`
extension ADR 0113; side-aware `revert_method/3` ADR 0112).

Split out of `beamtalk_workspace_interface_primitives`, which keeps thin
delegates for the FFI-reached entry points (`changeLogRevert/1`,
`revert_method/2,3`). Everything else here is internal to reverting a
`ChangeEntry`: target extraction, the modify/add/remove-class revert paths, the
external-drift check and the structured revert errors.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([changeLogRevert/1, revert_method/2, revert_method/3]).

-doc """
Revert a single ChangeEntry (ADR 0082 Phase 4; `remove-method`/
`remove-class` extension ADR 0113).

Called via `(Erlang beamtalk_workspace_interface_primitives) changeLogRevert: anEntry`
from `ChangeLog>>revert:`. `Entry` is a `ChangeEntry` value-object map (or a
plain map shaped the same way) carrying at least `className` (Symbol) and
`selector` (Symbol). Looks up the most recent active entry for that
`(class, selector)` target and, depending on what kind of entry it is:

  - a *modify* (`instance`/`class` patch, or a `'remove-method'` removal) —
    re-installs its recorded `prev_source` via the durable `compile:source:`
    install path, emitting a fresh `#instance`/`#class` ChangeEntry (per ADR
    0082, revert is "itself a patch, not log mutation", so the original
    entry's audit history is preserved);
  - an *add* (`'new-class'`, or a brand-new method) — removes what was added;
  - a `'remove-class'` removal — recompiles and reinstalls the whole class
    from `prev_source_ref`, reusing the `newClass:at:` install path (ADR 0113).

Once flushed, an entry drops out of the active view (`is_active/1`) and
`find_revert_target/3` reports `{error, no_entry}` for it — post-flush revert
is unsupported by design (ADR 0082/0113: "best-effort, pre-flush semantics
only"; post-flush undo is git's job, or a fresh corrective operation from the
archived `prev_source_ref`).
""".
-spec changeLogRevert(term()) -> term().
changeLogRevert(Entry) ->
    case extract_revert_target(Entry) of
        {ok, ClassNameBin, SelectorAtom, Side} ->
            do_revert(ClassNameBin, SelectorAtom, Side);
        {error, Err} ->
            beamtalk_error:raise(Err)
    end.

-doc """
Revert a single method patch by `(Class, Selector)` binaries, returning a
structured result instead of raising (ADR 0082 Phase 5).

Side-agnostic: falls back to `find_revert_target/3`'s highest-seq selection
across both sides, same as passing `undefined` to `revert_method/3`. Kept for
callers with no side information (and for backward compatibility — this was
the original LiveView-facing surface); the LiveView `Workspace changes` row
now calls `revert_method/3` with its own `side` instead (ADR 0112),
since a same-selector instance-side and class-side entry are otherwise
indistinguishable by `(Class, Selector)` alone and the wrong one — whichever
has the higher `seq` — would be reverted.

See `revert_method/3` for the full doc (error contract, atom-safety note on
`SelectorBin`, etc.) — this is `revert_method/3(ClassNameBin, SelectorBin,
undefined)`.
""".
-spec revert_method(binary(), binary()) -> {ok, term()} | {error, #beamtalk_error{}}.
revert_method(ClassNameBin, SelectorBin) when
    is_binary(ClassNameBin), is_binary(SelectorBin)
->
    revert_method(ClassNameBin, SelectorBin, undefined).

-doc """
Revert a single method patch by `(Class, Selector, Side)`, returning a
structured result instead of raising (ADR 0082 Phase 5; side
parameter added ADR 0112).

The message-send entry point `changeLogRevert/1` `error/1`-raises a wrapped
`#beamtalk_error{}` on any failure, which crosses an `rpc:call/4` as an opaque
`{badrpc, {'EXIT', _}}`. The LiveView Attach client (`BtAttach.Workspace`)
needs the same clean `{ok, _} | {error, #beamtalk_error{}}` contract that
`beamtalk_repl_eval:compile_method/6` and `new_class/2` already offer, so this
wrapper catches the wrapped error and returns it directly. The `Selector` is
the method-name binary carried by a `Workspace changes` row; a new-class entry
(no selector) is rejected the same way as `changeLogRevert/1`, via `do_revert`.

`Side` disambiguates a same-selector instance-side entry from a class-side one
(e.g. an unflushed instance-side patch to `#foo` and a later class-side
`removeSelector: #foo`, both active): without it, `find_revert_target/3` would
pick whichever has the higher `seq` regardless of which row the owner actually
clicked "revert" on. `SideArg` accepts the atoms `instance`/`class` (an
in-node Erlang caller) or the binaries `<<"instance">>`/`<<"class">>` (the
LiveView row's `phx-value-side`, which rides the LiveSocket as a string);
`revert_side_field/1` (shared with `changeLogRevert/1`'s ChangeEntry-map path)
normalises either to `find_revert_target/3`'s expected shape, and anything
else (`undefined`, `nil`, an empty binary for a sideless new-class row) falls
back to the side-agnostic highest-seq selection.

`SelectorBin` is owner-controlled (it rides the `phx-value-selector` attribute
over the LiveSocket), so the binary is resolved with `binary_to_existing_atom/2`
rather than `binary_to_atom/2`: a never-compiled selector has no atom and
therefore nothing to revert, and — critically — we must not mint a fresh atom
per request, which would let a crafted loop exhaust the (un-GC'd) atom table and
crash the node.

`ClassNameBin` needs no such guard: it is used only as a binary comparison key
(`find_revert_target/2` matches it against each entry's `#entry.class` binary and
fails early for an unknown class), never converted to an atom.
""".
-spec revert_method(binary(), binary(), term()) -> {ok, term()} | {error, #beamtalk_error{}}.
revert_method(ClassNameBin, SelectorBin, SideArg) when
    is_binary(ClassNameBin), is_binary(SelectorBin)
->
    Side = revert_side_field(SideArg),
    case existing_selector_atom(SelectorBin) of
        {ok, SelectorAtom} ->
            try
                {ok, do_revert(ClassNameBin, SelectorAtom, Side)}
            catch
                %% `beamtalk_error:raise/1` wraps the structured error in a
                %% `#{'$beamtalk_class' => _, error => #beamtalk_error{}}` map
                %% and `error/1`-raises it; unwrap back to the structured error.
                error:#{error := #beamtalk_error{} = Err} ->
                    {error, Err};
                %% Any other exception is an internal contract violation (e.g. a
                %% case_clause if find_revert_target/2 returned an unexpected
                %% term). Without this arm it would escape as an `rpc:call/4`
                %% `{badrpc, {'EXIT', _}}` and the Attach client would mislabel it
                %% "workspace unreachable". Log it loudly (so the bug is not
                %% hidden) and return an accurate structured error.
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR(
                        #{
                            msg => "revert_method internal error",
                            class => Class,
                            reason => Reason,
                            target_class => ClassNameBin,
                            selector => SelectorAtom,
                            side => Side,
                            stacktrace => Stacktrace
                        },
                        #{domain => [beamtalk, runtime]}
                    ),
                    {error, revert_state_error(<<"revert: internal error; see node logs">>)}
            end;
        error ->
            %% No atom exists for this selector, so no method by that name has
            %% ever been compiled — there is nothing to revert. Surface the same
            %% "nothing to revert" error `do_revert` raises for `no_entry`.
            {error,
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: no active ChangeEntry found for ">>,
                        ClassNameBin,
                        <<">>">>,
                        SelectorBin,
                        <<"; nothing to revert">>
                    ])
                )}
    end.

%% Resolve a selector binary to its atom WITHOUT minting a new one. Returns
%% `error` when no such atom exists (the selector was never compiled), so the
%% caller can reject owner-supplied junk without growing the atom table.
-spec existing_selector_atom(binary()) -> {ok, atom()} | error.
existing_selector_atom(SelectorBin) ->
    beamtalk_text:to_existing_atom(SelectorBin).

%% Pull the `(class, selector, side)` triple out of a ChangeEntry. The argument
%% is normally the `$beamtalk_class => ChangeEntry`-tagged map produced by the
%% FFI surface, so the keys are atoms (`className`, `selector`, `side`). We
%% also accept the raw `#beamtalk_object{}` defensively so tests / future
%% Beamtalk-side callers can synthesise the input from a different shape.
-spec extract_revert_target(term()) ->
    {ok, binary(), atom(), instance | class | undefined} | {error, #beamtalk_error{}}.
extract_revert_target(#{'$beamtalk_class' := 'ChangeEntry'} = M) ->
    extract_revert_target_from_map(M);
extract_revert_target(#{className := _} = M) ->
    extract_revert_target_from_map(M);
extract_revert_target(_Other) ->
    {error, revert_type_error(<<"revert: expects a ChangeEntry, got an unrelated value">>)}.

-spec extract_revert_target_from_map(map()) ->
    {ok, binary(), atom(), instance | class | undefined} | {error, #beamtalk_error{}}.
extract_revert_target_from_map(M) ->
    %% ADR 0112 required fix: a same-selector instance-side entry and
    %% class-side entry are otherwise indistinguishable to `find_revert_target/2`
    %% (which is keyed on `(class, selector)` only) — it would pick whichever has
    %% the higher `seq`, silently reverting the wrong side. `side` rides the same
    %% ChangeEntry map (`change_entry.bt`'s `side` field, populated from
    %% `entry_to_value/2`'s `side_symbol(entry_side(E))`), so extracting it here
    %% and threading it into `find_revert_target/3` lets the caller-selected
    %% entry's side (not just its class+selector) constrain which candidate wins.
    Side = revert_side_field(maps:get(side, M, undefined)),
    case {maps:get(className, M, undefined), maps:get(selector, M, undefined)} of
        {ClassAtom, SelectorAtom} when
            is_atom(ClassAtom),
            is_atom(SelectorAtom),
            SelectorAtom =/= nil,
            SelectorAtom =/= undefined
        ->
            {ok, atom_to_binary(ClassAtom, utf8), SelectorAtom, Side};
        {ClassAtom, nil} when is_atom(ClassAtom), ClassAtom =/= nil, ClassAtom =/= undefined ->
            %% A new-class entry carries `selector = nil` (it has no selector). Map
            %% it to the `'new-class'` placeholder so `do_revert`/`find_revert_target`
            %% resolve the class's new-class entry and remove the class.
            %% A new-class entry is always sideless, so `Side` is `undefined` here
            %% regardless of what the map carried.
            {ok, atom_to_binary(ClassAtom, utf8), 'new-class', undefined};
        _ ->
            {error,
                revert_type_error(
                    <<
                        "revert: ChangeEntry is missing className/selector fields; pass an "
                        "entry obtained from `Workspace changes do:` or `select:`"
                    >>
                )}
    end.

%% Normalise a caller-supplied side value to `find_revert_target/3`'s expected
%% shape. Shared by both revert entry points (ADR 0112): a
%% ChangeEntry map's `side` field arrives as the atom `instance`/`class`/`nil`
%% (`extract_revert_target_from_map/1`), while a LiveView `phx-value-side`
%% attribute (`revert_method/3`) arrives as the binary `<<"instance">>`/
%% `<<"class">>` over the LiveSocket. `nil`/`undefined`/an empty binary (a
%% sideless new-class row's `side`) and any other unrecognised value all mean
%% "no side constraint".
-spec revert_side_field(term()) -> instance | class | undefined.
revert_side_field(instance) -> instance;
revert_side_field(class) -> class;
revert_side_field(<<"instance">>) -> instance;
revert_side_field(<<"class">>) -> class;
revert_side_field(_Other) -> undefined.

%% `TargetSide` (ADR 0112) narrows `find_revert_target/3`'s candidate
%% search to entries on that side, so a same-selector instance-side entry and
%% class-side entry are not ambiguous by `(class, selector)` alone — see
%% `extract_revert_target_from_map/1`'s doc. Pass `undefined` (from a caller
%% with no side information, e.g. `revert_method/2`) to fall back to the
%% side-agnostic, highest-seq selection.
-spec do_revert(binary(), atom(), instance | class | undefined) -> term().
do_revert(ClassNameBin, SelectorAtom, TargetSide) ->
    %% ChangeLog revert (ADR 0082/0113/0114) is a workspace operation: its
    %% branches reinstall, remove or recompile classes and methods, bypassing
    %% the gates on `compile:source:`/`removeFromSystem`/`newClass:at:`. Every
    %% entry point (`changeLogRevert/1`, and the LiveView RPC `revert_method/2,3`)
    %% funnels through here, so one check refuses it on a release node, even
    %% one built with `include_compiler` (ADR 0125 §1.5).
    ok = beamtalk_capability:require(
        'changes revert:', 'Workspace', <<"Workspace changes revert:">>
    ),
    case beamtalk_workspace_changelog:find_revert_target(ClassNameBin, SelectorAtom, TargetSide) of
        {ok, PrevBody, Entry} ->
            %% A *modify* revert: re-install the recorded prior body on the
            %% entry's side. Instance and class side are both supported.
            %% This also covers reverting a `'remove-method'` entry
            %% (ADR 0112) — when its `prev_source_ref` is set (the
            %% removed method's prior body), it reaches this branch exactly
            %% like an ordinary modify, and re-installing that body IS what
            %% undoes the removal. `prev_source_ref` is NOT always set, though:
            %% `beamtalk_repl_loader:resolve_removal_span_entry/6`'s
            %% `not_on_disk` branch (a live, never-flushed method being
            %% removed) records no `prev_source`/`source_file` at all, so that
            %% case never reaches here — `find_revert_target/3` raises a loud
            %% `no_prev_source` error instead, which is the correct behavior
            %% (never silently drop the revert). `revert_side/1` resolves the
            %% side for both shapes via `entry_side/1` (derived from `kind` for
            %% legacy instance/class entries, read directly for
            %% `'remove-method'` ones). A `'new-class'`/`unknown` kind cannot
            %% reach this branch: new-class yields `{remove, _}`, and any
            %% future sideless kind fails loudly via `revert_side/1`.
            Side = revert_side(Entry),
            install_revert_patch(ClassNameBin, SelectorAtom, PrevBody, Side);
        {remove, Entry} ->
            %% An *add* revert: the pre-patch state was "absent", so undo the add
            %% by removing the method or the new class. The kind tells us which.
            revert_removal(ClassNameBin, SelectorAtom, Entry);
        {reinstall_class, PrevBody, Entry} ->
            %% A `'remove-class'` revert (ADR 0113): the pre-removal
            %% state was "this class existed", so undo the removal by
            %% recompiling and reinstalling the whole class from its recorded
            %% prior source — a class-level target, not a single-method patch.
            reinstall_reverted_class(ClassNameBin, PrevBody, Entry);
        {revert_rename, Entry} ->
            %% A `'rename-class'`/`'rename-method'` revert (ADR 0114): a
            %% multi-site target, not a single prior body — undo
            %% by rewriting every one of `Entry`'s own `sites` back to its own
            %% recorded `prev_source_ref`.
            revert_rename_entry(ClassNameBin, Entry);
        {error, no_entry} ->
            beamtalk_error:raise(
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: no active ChangeEntry found for ">>,
                        ClassNameBin,
                        <<">>">>,
                        atom_to_binary(SelectorAtom, utf8),
                        <<"; nothing to revert">>
                    ])
                )
            );
        {error, no_prev_source} ->
            %% Shared by every kind that can raise it: a method modify/removal
            %% (the ChangeLog's own `sources/` copy is missing and the on-disk
            %% span no longer resolves) and a `'remove-class'` removal (the
            %% ChangeLog copy is missing AND either there is no recorded
            %% `sourceFile` to fall back to — a dynamically-defined class — or
            %% that file itself can no longer be read). "Prior body" covers
            %% both a method body and a whole-file class source without
            %% picking the wrong noun for either.
            beamtalk_error:raise(
                revert_state_error(
                    <<
                        "revert: this entry's prior body could not be recovered (the "
                        "ChangeLog's recorded copy is missing, and no readable on-disk "
                        "fallback exists). Cannot reconstruct the pre-patch state"
                    >>
                )
            )
    end.

%% Resolve a ChangeEntry's revert install/remove side (ADR 0112's
%% required fix to ADR 0082's shipped revert logic — see
%% `beamtalk_workspace_changelog:entry_side/1`'s doc). Delegates the actual
%% instance/class resolution to that single accessor rather than pattern-
%% matching `kind` here, so this stays correct for both legacy
%% `instance`/`class`-kind entries (side derived from `kind`) and
%% `'remove-method'` entries (side read from the entry's own `side` field).
%% `entry_side/1` returning `undefined` (a `'new-class'`/`unknown` kind, or
%% any future sideless kind reaching this path unexpectedly) raises a loud
%% structured error rather than silently picking a side.
-spec revert_side(beamtalk_workspace_changelog:entry()) -> instance | class.
revert_side(Entry) ->
    case beamtalk_workspace_changelog:entry_side(Entry) of
        undefined ->
            beamtalk_error:raise(
                revert_kind_error(
                    iolist_to_binary([
                        <<"revert: cannot revert a ">>,
                        atom_to_binary(beamtalk_workspace_changelog:entry_kind(Entry), utf8),
                        <<" entry as a method patch; use `Workspace changes clear` to discard it">>
                    ])
                )
            );
        Side ->
            Side
    end.

%% Perform an *add* revert by removing what was added. A `new-class` entry removes
%% the class; a method entry removes that method on its side.
%% The original add entry stays in the audit log; the removal
%% itself does not emit a ChangeEntry (it is the inverse of the add, not a new
%% patch — symmetrical with how a modify-revert's re-install is the only entry).
-spec revert_removal(binary(), atom(), beamtalk_workspace_changelog:entry()) -> term().
revert_removal(ClassNameBin, SelectorAtom, Entry) ->
    case beamtalk_workspace_changelog:entry_kind(Entry) of
        'new-class' ->
            remove_reverted_class(ClassNameBin);
        _ ->
            %% `revert_side/1` resolves instance/class (and raises a loud
            %% structured error for any other, sideless kind) via
            %% `entry_side/1` — see its doc.
            remove_reverted_method(ClassNameBin, SelectorAtom, revert_side(Entry))
    end.

-spec remove_reverted_class(binary()) -> term().
remove_reverted_class(ClassNameBin) ->
    case beamtalk_repl_eval:remove_class(ClassNameBin) of
        {ok, _Module} ->
            nil;
        {error, Reason} ->
            beamtalk_error:raise(ensure_revert_error(Reason, ClassNameBin, undefined))
    end.

-doc """
Undo a `'remove-class'` entry (ADR 0113): recompile and reinstall the
whole class from `PrevBody` (the entry's recorded `prev_source_ref`), reusing
the same `newClass:at:` install chokepoint `new-class` revert already uses
(ADR 0082) rather than a second whole-class-install mechanism.

`Entry`'s `sourceFile` — the removed class's own recorded absolute path — is
the reinstall target. A class removed with no recorded source file (a
dynamically-defined class with nothing on disk to begin with, `flushable:
false, not_flushable_reason: "dynamic"`) has nothing to reinstall *from*; this
is a loud structured error rather than a silent no-op, matching this module's
error contract elsewhere (never guess, never drop a revert silently).

Before reinstalling, `check_no_external_drift/3` compares the file currently
on disk at `SourceFile` against `PrevBody`: a still-*pending* `'remove-class'` entry never touches disk
itself (Tier 2 removals only unlink on an explicit
`flushIncludingDestructive`), so in the common case the file this reads is
exactly what was there when the class was removed — unless someone edited it
out-of-band (another session, git, an editor) while the removal sat pending,
in which case reinstalling `PrevBody` over that edit and retiring the entry
as "cleanly reverted" would silently discard it. (`PrevBody` is the class's
*tracked in-memory* source at removal time, not necessarily a fresh disk
read — an earlier durable-but-unflushed patch to this same class, still
pending independently, would also make disk legitimately differ; the check
below cannot distinguish that case from a genuine external edit, so it
raises the same structured error either way rather than guessing, and the
error message says so.) On any mismatch, this raises a structured error and
returns *before* reinstalling or retiring — the class stays removed and the
original entry stays pending, exactly as if revert had never been called.
""".
-spec reinstall_reverted_class(binary(), binary(), beamtalk_workspace_changelog:entry()) -> term().
reinstall_reverted_class(ClassNameBin, PrevBody, Entry) ->
    case beamtalk_workspace_changelog:entry_source_file(Entry) of
        undefined ->
            beamtalk_error:raise(
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: cannot recreate ">>,
                        ClassNameBin,
                        <<
                            "; its removal recorded no source file (a dynamically-defined "
                            "class has nothing on disk to reinstall from)"
                        >>
                    ])
                )
            );
        SourceFile ->
            case check_no_external_drift(SourceFile, PrevBody, ClassNameBin) of
                ok ->
                    reinstall_reverted_class_body(ClassNameBin, PrevBody, SourceFile, Entry);
                {error, Err} ->
                    beamtalk_error:raise(Err)
            end
    end.

%% Actually recompile+reinstall and retire the original entry, once
%% `check_no_external_drift/3` has confirmed disk still matches `PrevBody`.
-spec reinstall_reverted_class_body(
    binary(), binary(), binary(), beamtalk_workspace_changelog:entry()
) ->
    term().
reinstall_reverted_class_body(ClassNameBin, PrevBody, SourceFile, Entry) ->
    case beamtalk_repl_loader_newclass:revert_remove_class(PrevBody, SourceFile) of
        {ok, _Objects} ->
            %% Mirrors `revert_removal/3`'s "undoing an add emits no new
            %% entry" symmetry: undoing this removal doesn't emit one
            %% either (`new_class_install/8`'s `no_log` mode), so the
            %% original `'remove-class'` entry must be retired here —
            %% otherwise it stays active/pending forever even though
            %% its effect has been undone (ADR 0113 review
            %% fix: a stale `'remove-class'` entry would misreport
            %% `skipped: destructive`/block a real future removal).
            retire_reverted_remove_class_entry(ClassNameBin, Entry);
        {error, Reason} ->
            beamtalk_error:raise(ensure_revert_error(Reason, ClassNameBin, undefined))
    end.

-doc """
Drift check for a pending `'remove-class'` entry's reinstall (ADR 0113 —
closes a gap where reverting a pending remove-class entry could silently
discard a concurrent out-of-band edit).

Reads `SourceFile` off disk and compares it byte-for-byte against `PrevBody`
(the entry's recorded whole-file `prev_source_ref` snapshot), the same
comparison mechanism `beamtalk_workspace_flush_sites:splice_with_prev/5` uses to
detect an `external_edit` conflict on the ordinary flush path — just applied
to a whole file instead of a byte span, since a `'remove-class'` entry
records the whole prior file rather than a span. Three outcomes:

  - disk bytes == `PrevBody`: no drift, `ok` — the file is exactly what it
    was when the class was removed, safe to reinstall over.
  - disk bytes differ: the on-disk content no longer matches the recorded
    pre-removal snapshot — either an out-of-band edit (another session, git,
    an editor) landed while the removal sat pending, or the class had an
    earlier durable-but-unflushed patch that was never written to disk (both
    look identical from here: a content mismatch with no further signal to
    tell them apart). A structured error either way, naming the mismatch so
    the caller can reconcile manually rather than risk losing either kind of
    change silently.
  - the file is missing or unreadable: it was deleted or replaced out-of-band
    (an even bigger divergence than an edit). Also a structured error — never
    silently treat "gone" as "safe to overwrite with the old snapshot".

Either error case returns *without* reinstalling or retiring the entry, so a
failed revert leaves the class removed and the original entry pending — the
caller (`reinstall_reverted_class/3`) never reaches the reinstall step.
""".
-spec check_no_external_drift(binary(), binary(), binary()) -> ok | {error, #beamtalk_error{}}.
check_no_external_drift(SourceFile, PrevBody, ClassNameBin) ->
    %% `binary_to_list/1` here mirrors `beamtalk_workspace_flush_sites:prepare_splice/2`'s
    %% / `prepare_remove_class/3`'s own `file:read_file(binary_to_list(File))`
    %% convention for a ChangeEntry's recorded `sourceFile` binary, rather than
    %% passing the binary straight through.
    case file:read_file(binary_to_list(SourceFile)) of
        {ok, PrevBody} ->
            ok;
        {ok, _DiskBody} ->
            {error,
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: cannot reinstall ">>,
                        ClassNameBin,
                        <<
                            "; its file's current on-disk content no longer matches "
                            "the class's recorded pre-removal snapshot: "
                        >>,
                        SourceFile,
                        <<
                            ". This means either the file was edited externally "
                            "(another session, git, an editor) while the removal was "
                            "pending, or the class had an earlier durable patch that "
                            "was never flushed to disk. Reconcile the file manually "
                            "before reverting, or use `Workspace changes clear` to "
                            "discard this pending removal without reinstalling"
                        >>
                    ])
                )};
        {error, enoent} ->
            {error,
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: cannot reinstall ">>,
                        ClassNameBin,
                        <<
                            "; its recorded source file no longer exists on disk (deleted "
                            "externally while the removal was pending): "
                        >>,
                        SourceFile
                    ])
                )};
        {error, Reason} ->
            {error,
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: cannot reinstall ">>,
                        ClassNameBin,
                        <<"; its recorded source file could not be read (">>,
                        atom_to_binary(Reason, utf8),
                        <<"): ">>,
                        SourceFile
                    ])
                )}
    end.

%% The class is already reinstalled and live by the time this runs — a
%% ChangeLog write failure here must never surface as a raw crash (or, worse,
%% leave the original entry silently stuck active/pending forever, which is
%% the exact phantom-entry bug this whole revert path exists to avoid).
%% Mirrors `beamtalk_workspace_flush:complete_flush/5`'s handling of the same
%% `mark_flushed/1` call: catch a `gen_server` exit (unreachable/timeout) as
%% well as an explicit `{error, _}`, and raise a structured error either way.
-spec retire_reverted_remove_class_entry(binary(), beamtalk_workspace_changelog:entry()) ->
    term().
retire_reverted_remove_class_entry(ClassNameBin, Entry) ->
    MarkResult =
        try
            beamtalk_workspace_changelog:mark_flushed([
                beamtalk_workspace_changelog:entry_seq(Entry)
            ])
        catch
            exit:ExitReason -> {error, {changelog_unreachable, ExitReason}}
        end,
    case MarkResult of
        ok ->
            class_object_for(ClassNameBin);
        {error, Reason} ->
            ?LOG_ERROR(
                "revert: reinstalled class but failed to retire its remove-class entry",
                #{
                    class => ClassNameBin,
                    seq => beamtalk_workspace_changelog:entry_seq(Entry),
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            beamtalk_error:raise(
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: ">>,
                        ClassNameBin,
                        <<
                            " was reinstalled, but its old removal entry could not be "
                            "retired (ChangeLog unreachable); it may still show as "
                            "pending until a future flush or manual cleanup"
                        >>
                    ])
                )
            )
    end.

-doc """
Undo a `'rename-class'`/`'rename-method'` entry (ADR 0114):
delegates the actual multi-site reverse-splice to `beamtalk_repl_loader_rewrite:
revert_rename_sites/1` (see that function's own doc for the full mechanism —
locating each site's current position, resolving its owning class, and, for
`'rename-class'`, restoring the class's registry identity), then retires the
original entry on success — mirroring `reinstall_reverted_class_body/4`'s
"undoing an add emits no new entry, so the original entry must be retired
here" convention for `'remove-class'`, since a rename revert is symmetric:
the reverted state IS the pre-rename state, nothing new needs recording.
""".
-spec revert_rename_entry(binary(), beamtalk_workspace_changelog:entry()) -> term().
revert_rename_entry(ClassNameBin, Entry) ->
    case beamtalk_repl_loader_rewrite:revert_rename_sites(Entry) of
        {ok, RevertedClassBin} ->
            retire_reverted_rename_entry(RevertedClassBin, Entry);
        {error, Reason} ->
            beamtalk_error:raise(ensure_revert_error(Reason, ClassNameBin, undefined))
    end.

%% The class is already reinstalled/reverted live by the time this runs — a
%% ChangeLog write failure here must never surface as a raw crash (or, worse,
%% leave the original entry silently stuck active/pending forever). Mirrors
%% `retire_reverted_remove_class_entry/2`'s identical handling exactly, just
%% generalized to name either rename kind rather than assuming "remove-class".
-spec retire_reverted_rename_entry(binary(), beamtalk_workspace_changelog:entry()) -> term().
retire_reverted_rename_entry(RevertedClassBin, Entry) ->
    MarkResult =
        try
            beamtalk_workspace_changelog:mark_flushed([
                beamtalk_workspace_changelog:entry_seq(Entry)
            ])
        catch
            exit:ExitReason -> {error, {changelog_unreachable, ExitReason}}
        end,
    case MarkResult of
        ok ->
            class_object_for(RevertedClassBin);
        {error, Reason} ->
            ?LOG_ERROR(
                "revert: reverted rename but failed to retire its rename entry",
                #{
                    class => RevertedClassBin,
                    seq => beamtalk_workspace_changelog:entry_seq(Entry),
                    reason => Reason,
                    domain => [beamtalk, runtime]
                }
            ),
            beamtalk_error:raise(
                revert_state_error(
                    iolist_to_binary([
                        <<"revert: ">>,
                        RevertedClassBin,
                        <<
                            " was reverted, but its old rename entry could not be "
                            "retired (ChangeLog unreachable); it may still show as "
                            "pending until a future flush or manual cleanup"
                        >>
                    ])
                )
            )
    end.

-spec remove_reverted_method(binary(), atom(), instance | class) -> term().
remove_reverted_method(ClassNameBin, SelectorAtom, Side) ->
    case beamtalk_repl_eval:remove_method(ClassNameBin, SelectorAtom, Side) of
        {ok, _ClassName} ->
            class_object_for(ClassNameBin);
        {error, Reason} ->
            beamtalk_error:raise(ensure_revert_error(Reason, ClassNameBin, SelectorAtom))
    end.

%% Wrap a removal failure as a structured revert error. An already-structured
%% `#beamtalk_error{}` (e.g. from `remove_class/1`) passes through; a raw reason
%% becomes a `revert_install_error` so the caller always sees a clean error.
-spec ensure_revert_error(term(), binary(), atom() | undefined) -> #beamtalk_error{}.
ensure_revert_error(#beamtalk_error{} = Err, _ClassNameBin, _SelectorAtom) ->
    Err;
ensure_revert_error(Reason, ClassNameBin, SelectorAtom) ->
    revert_install_error(ClassNameBin, SelectorAtom, Reason).

-spec install_revert_patch(binary(), atom(), binary(), instance | class) -> term().
install_revert_patch(ClassNameBin, SelectorAtom, PrevBody, Side) ->
    %% Reverts are durable, human-authored patches that re-install the prior
    %% source on the original side. The install chokepoint emits the new
    %% ChangeEntry; the original entry is left in place for audit (ADR 0082,
    %% "Undo": revert is itself a patch, not a log mutation).
    case
        beamtalk_repl_eval:compile_method(
            ClassNameBin, SelectorAtom, PrevBody, durable, <<"repl/revert">>, human, Side
        )
    of
        {ok, _ClassName} ->
            class_object_for(ClassNameBin);
        {error, Reason} ->
            beamtalk_error:raise(revert_install_error(ClassNameBin, SelectorAtom, Reason))
    end.

-spec class_object_for(binary()) -> term().
class_object_for(ClassNameBin) ->
    case safe_existing_atom(ClassNameBin) of
        undefined ->
            nil;
        Atom ->
            case beamtalk_runtime_api:whereis_class(Atom) of
                undefined ->
                    nil;
                ClassPid ->
                    Mod = beamtalk_runtime_api:module_name(ClassPid),
                    Tag = beamtalk_runtime_api:class_object_tag(Atom),
                    #beamtalk_object{class = Tag, class_mod = Mod, pid = ClassPid}
            end
    end.

-spec safe_existing_atom(binary()) -> atom() | undefined.
safe_existing_atom(Bin) ->
    case beamtalk_text:to_existing_atom(Bin) of
        {ok, Atom} -> Atom;
        error -> undefined
    end.

-spec revert_type_error(binary()) -> #beamtalk_error{}.
revert_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'ChangeLog'),
    Err1 = beamtalk_error:with_selector(Err0, 'revert:'),
    beamtalk_error:with_message(Err1, Message).

-spec revert_kind_error(binary()) -> #beamtalk_error{}.
revert_kind_error(Message) ->
    Err0 = beamtalk_error:new(unsupported_revert_kind, 'ChangeLog'),
    Err1 = beamtalk_error:with_selector(Err0, 'revert:'),
    beamtalk_error:with_message(Err1, Message).

-spec revert_state_error(binary()) -> #beamtalk_error{}.
revert_state_error(Message) ->
    Err0 = beamtalk_error:new(revert_not_possible, 'ChangeLog'),
    Err1 = beamtalk_error:with_selector(Err0, 'revert:'),
    beamtalk_error:with_message(Err1, Message).

-spec revert_install_error(binary(), atom() | undefined, term()) -> #beamtalk_error{}.
revert_install_error(ClassNameBin, SelectorAtom, Reason) ->
    Err0 = beamtalk_error:new(revert_install_failed, 'ChangeLog'),
    Err1 = beamtalk_error:with_selector(Err0, 'revert:'),
    Msg = iolist_to_binary([
        <<"revert: could not undo change for ">>,
        ClassNameBin,
        revert_target_suffix(SelectorAtom)
    ]),
    Err2 = beamtalk_error:with_message(Err1, Msg),
    beamtalk_error:with_details(Err2, #{reason => Reason}).

%% `>>selector` suffix for a method target, or empty for a class-level target
%% (new-class revert, where there is no selector).
-spec revert_target_suffix(atom() | undefined) -> binary().
revert_target_suffix(undefined) -> <<>>;
revert_target_suffix(SelectorAtom) -> <<">>", (atom_to_binary(SelectorAtom, utf8))/binary>>.
