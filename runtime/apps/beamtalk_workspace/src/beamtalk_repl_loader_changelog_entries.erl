%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_changelog_entries).

%%% **DDD Context:** REPL Session Context

-moduledoc """
ChangeLog entry construction for live REPL mutations (ADR 0082 and its
extensions: ADR 0112 method removal, ADR 0113 class removal, ADR 0114
multi-site rewrites).

Extracted from `beamtalk_repl_loader` (BT-4683). The loader installs a change
in memory first and then calls one of the `emit_*` functions here; each one
builds the entry and appends it to `beamtalk_workspace_changelog`. Appending
is best effort: a failure is logged and swallowed by
`best_effort_changelog_append/3`, because the change is already live and a
ChangeLog write must never undo it.

Flushability (does the entry name an in-project `.bt` file and a resolvable
on-disk span that a later `Workspace flush` can splice into?) is decided by
`resolve_flushability/4,5`. It owns the shared steps: classify the class's
source file, read it from disk, and downgrade to memory-only when the read
fails. Each entry kind passes its own span resolver as a fun.
""".

-export([
    %% Entry emitters (best effort; always return `ok`).
    emit_class_def_entry/3,
    emit_rewrite_change_entry/2,
    emit_new_class_entry/3,
    emit_change_entry/1,
    emit_remove_change_entry/5,
    emit_extension_remove_change_entry/7,
    capture_class_removal_snapshot/1,
    emit_remove_class_change_entry/4,
    %% Flushability classification, reused by the loader's own write paths
    %% and by `beamtalk_repl_ops_load` / `beamtalk_repl_ops_browse` (through
    %% the loader's exports).
    add_class_def_flushability/2,
    class_source_file/1,
    classify_source_file/1,
    no_source_reason/1,
    class_module/1,
    %% Shared helpers the loader also uses outside ChangeLog emission.
    new_class_author/0,
    new_class_author_kind/0,
    method_selector_binary/1,
    method_source_binary/1,
    patch_side/1,
    match_trailing_newline/2,
    sibling_method_indent/1,
    span_error_entry/3,
    new_method_entry/3
]).

-include_lib("kernel/include/logger.hrl").

%% An entry kind's span resolver for `resolve_flushability/4,5`: given the
%% in-project `SourceFile` and its current on-disk contents, return the
%% finished entry (flushable with a span, or downgraded with a reason).
-type span_resolver() :: fun((SourceFile :: binary(), DiskSource :: binary()) -> map()).

%%% ----------------------------------------------------------------------------
%%% Shared skeletons
%%% ----------------------------------------------------------------------------

-doc """
Build a ChangeLog entry with `BuildFun` and append it, logging and swallowing
any failure.

Every `emit_*` function in this module runs after its change is already live
in memory (ADR 0082: "install is authoritative, log is best effort"), so a
failure while building or appending the entry must never reach the caller.
On failure this logs `Label` at warning level with `Meta` plus
`error_class`/`reason`/`stack`/`domain`, and returns `ok`.
""".
-spec best_effort_changelog_append(string(), map(), fun(() -> map())) -> ok.
best_effort_changelog_append(Label, Meta, BuildFun) ->
    %% bt-catcher-audit: not-applicable-no-block - BuildFun is an Erlang closure
    %% from this module's emit_* functions that builds the entry map; it never
    %% runs a Beamtalk block.
    try
        Entry = BuildFun(),
        _ = beamtalk_workspace_changelog:append(Entry),
        ok
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING(Label, Meta#{
                error_class => Class,
                reason => Reason,
                stack => Stack,
                domain => [beamtalk, runtime]
            }),
            ok
    end.

-doc """
Decide whether an entry for `ClassNameBin` can be flushed, and add the
flushability keys to `Base`.

Owns the steps every span-carrying entry kind shares:

  1. No backing `.bt` file (`class_source_file/1` is `nil`): `flushable =>
     false` with `no_source_reason/1` (`"stdlib"` / `"dynamic"`).
  2. A file outside the project (`classify_source_file/1`): `flushable =>
     false` with the `"dependency:<path>"` reason.
  3. An in-project file: read it from disk. A read failure logs
     `ReadFailedMsg` and downgrades to memory-only
     (`not_flushable_reason => <<"disk_read_failed">>`); a successful read
     calls `ResolveFun(SourceFile, DiskSource)`, which resolves the entry
     kind's own span and returns the finished entry.

Equivalent to `resolve_flushability/5` with no pre-read check.
""".
-spec resolve_flushability(map(), binary(), string(), span_resolver()) -> map().
resolve_flushability(Base, ClassNameBin, ReadFailedMsg, ResolveFun) ->
    resolve_flushability(
        Base, ClassNameBin, ReadFailedMsg, fun(_SourceFile) -> continue end, ResolveFun
    ).

-doc """
`resolve_flushability/4` with a `PreReadFun` that runs after the file is
classified as in-project but before it is read.

`PreReadFun(SourceFile)` returns `continue` to go on to the read, or the
finished entry map to stop there. The `'class-def'` chain uses it to refuse
a `source` that is not header+state-only before touching the disk, which
keeps its original check order (that refusal wins over a read failure).
""".
-spec resolve_flushability(
    map(), binary(), string(), fun((binary()) -> continue | map()), span_resolver()
) -> map().
resolve_flushability(Base, ClassNameBin, ReadFailedMsg, PreReadFun, ResolveFun) ->
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, AbsPath} ->
                    case PreReadFun(SourceFile) of
                        continue ->
                            read_and_resolve(
                                Base, SourceFile, AbsPath, ReadFailedMsg, ResolveFun
                            );
                        Entry when is_map(Entry) ->
                            Entry
                    end;
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% Read the current on-disk file and hand it to the entry kind's span
%% resolver. An unreadable file downgrades to memory-only rather than
%% blocking the install that already happened.
-spec read_and_resolve(map(), binary(), string(), string(), span_resolver()) -> map().
read_and_resolve(Base, SourceFile, AbsPath, ReadFailedMsg, ResolveFun) ->
    case file:read_file(AbsPath) of
        {ok, DiskSource} ->
            ResolveFun(SourceFile, DiskSource);
        {error, ReadReason} ->
            ?LOG_WARNING(
                ReadFailedMsg,
                #{
                    source_file => SourceFile,
                    reason => ReadReason,
                    domain => [beamtalk, runtime]
                }
            ),
            Base#{flushable => false, not_flushable_reason => <<"disk_read_failed">>}
    end.

%% Emit the durable `new-class` ChangeEntry for a freshly created class. Best
%% effort: a ChangeLog write must never fail or undo the in-memory install (the
%% class is already live), mirroring emit_change_entry/1's contract.
-spec emit_new_class_entry(binary(), binary(), binary()) -> ok.
emit_new_class_entry(ClassNameBin, Source, SourceFile) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for new class (class still installed)",
        #{class_name => ClassNameBin},
        fun() ->
            #{
                class => ClassNameBin,
                kind => 'new-class',
                source => Source,
                %% Explicit per ADR 0082 contract: new-class entries carry no
                %% prior disk body and no byte span (the file does not yet
                %% exist).
                prev_source => undefined,
                span => undefined,
                intent => durable,
                flushable => true,
                source_file => SourceFile,
                author => new_class_author(),
                author_kind => new_class_author_kind()
            }
        end
    ).

%% Audit author for a new-class entry. MCP `save_class` stamps `agent` into the
%% process dictionary at the submission boundary (same mechanism `compile:source:`
%% uses); a direct REPL call defaults to `human`/`repl`.
-spec new_class_author() -> binary().
new_class_author() ->
    case erlang:get('$beamtalk_author') of
        A when is_binary(A) -> A;
        _ -> new_class_default_author()
    end.

-spec new_class_default_author() -> binary().
new_class_default_author() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> <<"agent">>;
        _ -> <<"repl">>
    end.

-spec new_class_author_kind() -> human | agent.
new_class_author_kind() ->
    case erlang:get('$beamtalk_author_kind') of
        agent -> agent;
        _ -> human
    end.

%% Emit a ChangeLog entry for a live in-memory method patch (ADR 0082 Phase 1).
%%
%% Called from load_recompiled_method/7 *after* the patched class is installed in
%% memory. Captures (per the ADR's "Method patch flow"):
%%   - `kind' (`instance' / `class') from the patch side,
%%   - `intent' (`durable' for `>>' / `compile:source:'; `ephemeral' for
%%     `tryCompile:source:'),
%%   - `author_kind' (`human' / `agent') from the eval submission metadata,
%%   - `flushable' + `sourceFile' + `span' + `prev_source' when the class is
%%     backed by an in-project `.bt' file whose method span resolves cleanly.
%%
%% Non-flushable classes (stdlib / dynamic / dependency — `sourceFile' nil or
%% out-of-project) still log an entry, with `flushable: false' and a reason, so
%% the audit trail stays exhaustive ("every in-memory method mutation produces a
%% ChangeEntry"). A disk-read or span-resolution failure downgrades the entry to
%% non-flushable rather than failing the install.
%%
%% Best-effort: every failure path is logged and swallowed. The method is already
%% live; a ChangeLog write must not undo or block that (the all-or-nothing rule
%% only requires that the entry is emitted *after* a successful install — if
%% emission itself fails, the patch still stands).
-spec emit_change_entry(map()) -> ok.
emit_change_entry(MethodInfo) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for live patch (patch still installed)",
        #{method_info => maps:with([class_name, selector], MethodInfo)},
        fun() -> change_entry(MethodInfo) end
    ).

-spec change_entry(map()) -> map().
change_entry(MethodInfo) ->
    ClassNameBin = maps:get(class_name, MethodInfo),
    SelectorBin = maps:get(selector, MethodInfo),
    IsClassMethod = maps:get(is_class_method, MethodInfo, false),
    %% `>>' and `compile:source:' are durable; `tryCompile:source:' is ephemeral.
    Intent = maps:get(intent, MethodInfo, durable),
    %% MCP-issued patches tag `agent'; REPL / `>>' default to `human'.
    AuthorKind = maps:get(author_kind, MethodInfo, human),
    Author = maps:get(author, MethodInfo, <<"repl">>),
    Source = method_source_binary(MethodInfo),
    Kind = patch_side(IsClassMethod),
    Side = patch_side(IsClassMethod),
    Base = #{
        class => ClassNameBin,
        selector => SelectorBin,
        kind => Kind,
        source => Source,
        intent => Intent,
        author => Author,
        author_kind => AuthorKind
    },
    add_flushability(Base, ClassNameBin, SelectorBin, Side).

%% Emit the durable `'class-def'` ChangeEntry for one redefined class. Best
%% effort: a ChangeLog write must never fail or undo the in-memory install
%% (the class is already live), mirroring emit_change_entry/1's contract.
-spec emit_class_def_entry(binary(), string(), string()) -> ok.
emit_class_def_entry(ClassNameBin, NewSource, PrevSource) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for class redefinition (class still installed)",
        #{class_name => ClassNameBin},
        fun() ->
            NewSourceBin = unicode:characters_to_binary(NewSource),
            PrevSourceBin = unicode:characters_to_binary(PrevSource),
            Base0 = #{
                class => ClassNameBin,
                kind => 'class-def',
                source => NewSourceBin,
                intent => durable,
                %% Reuses the generic (despite its name) newClass:at: author
                %% helpers — both read the same `$beamtalk_author(_kind)`
                %% process-dictionary convention an eval-driven install (this
                %% path) shares with the FFI chokepoint, defaulting to
                %% human/repl when unset.
                author => new_class_author(),
                author_kind => new_class_author_kind()
            },
            Base = maybe_put_prev_source(Base0, PrevSourceBin),
            add_class_def_flushability(Base, ClassNameBin)
        end
    ).

-doc """
Classify a `'class-def'` entry's flushability.

Reuses `class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`
(the same classification `add_flushability/4` uses for a method patch) to
decide whether the class is genuinely an in-project file at all — stdlib,
dependency, and file-less (dynamic) classes stay `flushable: false` exactly
like a method patch on that kind of class.

A later change made the cockpit `:def` tab's resubmitted skeleton
(`beamtalk_repl_ops_browse:class_definition_text/7`) round-trip-safe: it now
carries the `sealed`/`abstract`/`typed` modifier keywords, the
`field:`/`state:` keyword choice, and `::` type annotations, closing the gap
an earlier version of this function's doc described — and the gap that made
an even earlier version of this function (which resolved a real on-disk span
and marked the entry flushable whenever the span resolved, regardless of what
the skeleton actually contained) unsafe: it would have silently downgraded
e.g. `sealed typed Value subclass: Foo` + `field: x :: Integer` to
`Value subclass: Foo` + `state: x` on every ordinary flush, caught in
adversarial review before this shipped.

With the skeleton itself fixed, this function now resolves the real on-disk
span (`beamtalk_compiler:resolve_class_span/2` — the same resolver
`beamtalk_workspace_changelog:disk_class_body/2` already used read-only for
the CHANGES-dock diff) and marks the entry flushable when it resolves — the
same span-resolve-or-downgrade shape `add_flushability/4` uses for a method
patch, just resolved at whole-class (header + state) granularity rather than
one method. Unlike a method body, the `:def` tab's skeleton is already built
at the on-disk column-0-header / 2-space-state-line shape, so this never needs
`reindent_method_source` — the only reshape is matching the resolved span's
trailing-newline state (the skeleton itself never carries one).

This chokepoint is NOT exclusive to the cockpit `:def` tab, though: any
successful redefinition of an already-loaded class (e.g. a raw REPL eval of a
FULL class body with methods) reaches `load_class_module/3` → here too, with
whatever text the caller compiled as `Base`'s `source`. Before trusting the
disk span at all, `class_def_pre_read_check/3` first confirms `source`
ITSELF is header+state-only (`class_def_source_is_skeleton_shaped/2`, reusing
`resolve_class_span/2` against the candidate text rather than the disk file) —
otherwise splicing a `source` that also carries methods into the disk span
(which, by construction, stops before the file's first method) would jam the
new methods into the header/state region while leaving the file's real
methods stale and duplicated right after it. Same bug class an earlier
adversarial review caught for the disk side; this closes the mirror-image gap
on the resubmitted-text side.

A second, independent adversarial-review finding closes a further gap: a
compiled (non-`ClassBuilder`) class's field DEFAULT-VALUE TEXT is not
recoverable from live reflection at all (only whether a field has one —
`__beamtalk_meta/0`'s `field_has_default`), so `class_definition_
text/7`'s skeleton always renders `default => null` for every field of any
real, file-backed class — even one whose on-disk declaration has one.
`resolve_class_def_span_entry/4` therefore ALSO compares each field's
default-presence between the disk source and the candidate replacement
(`class_def_preserves_field_defaults/3`, via
`beamtalk_compiler:class_state_field_defaults/2`) and refuses to flush when a
field that had a default on disk would lose it — otherwise a byte-accurate,
method-safe, modifier-complete splice could still silently delete a default.

A third finding (Claude BeamTalk Review, this same PR) closes a related span
gap: `resolve_class_span/2` clamps its span to end before the class's first
method, so a `state:`/`field:` declared AT OR AFTER a method (legal Beamtalk)
is excluded from the span/`PrevSource` entirely — but the live-reflected
skeleton includes it regardless of position, so splicing the skeleton into
the clamped span duplicates that field's declaration rather than losing it
(the field survives, untouched, past the spliced region too).
`class_def_span_contains_all_state_fields/3` refuses to flush whenever the
disk class has any field outside the span that would be spliced.
""".
-spec add_class_def_flushability(map(), binary()) -> map().
add_class_def_flushability(Base, ClassNameBin) ->
    resolve_flushability(
        Base,
        ClassNameBin,
        "ChangeLog: could not read sourceFile for class-def entry; recording memory-only",
        fun(SourceFile) -> class_def_pre_read_check(Base, ClassNameBin, SourceFile) end,
        fun(SourceFile, DiskSource) ->
            resolve_class_def_span_entry(Base, ClassNameBin, SourceFile, DiskSource)
        end
    ).

%% Confirm `Base`'s own `source` is header+state-only
%% (`class_def_source_is_skeleton_shaped/2`) BEFORE the disk file is read
%% and its span trusted. `add_class_def_flushability/2` is reached from
%% `load_class_module/3`, which is NOT exclusive to the cockpit `:def` tab's
%% own skeleton — any successful redefinition of an already-loaded class
%% routes through it, e.g. a raw REPL eval of a FULL class body (header +
%% state + methods). Splicing such a `source` into the disk span — which, by
%% `resolve_class_span/2`'s own "never reaches a method" guarantee, stops
%% before the *first* method — would jam the new text's methods into the
%% header/state region while leaving the file's actual (now stale/duplicated)
%% methods untouched right after it: exactly the same class of bug this guard
%% exists to catch, just triggered from the resubmitted text's shape instead
%% of the disk span's. Runs as `resolve_flushability/5`'s pre-read check, so
%% this refusal wins over a disk-read failure, as it always has.
-spec class_def_pre_read_check(map(), binary(), binary()) -> continue | map().
class_def_pre_read_check(#{source := Canonical} = Base, ClassNameBin, SourceFile) ->
    case class_def_source_is_skeleton_shaped(Canonical, ClassNameBin) of
        false ->
            Base#{
                flushable => false,
                not_flushable_reason => <<"class_def_source_not_skeleton_shaped">>,
                source_file => SourceFile
            };
        true ->
            continue
    end.

%% True iff `Source` (the resubmitted `'class-def'` text) is ITSELF entirely
%% header + state — i.e. `resolve_class_span/2` applied to `Source` (not the
%% on-disk file) resolves a span reaching all the way to the end of `Source`
%% AND starting at its very beginning, modulo whitespace on either side.
%% Reuses the exact same resolver the disk side uses, just pointed at the
%% candidate replacement text instead:
%%
%%  * trailing bytes past the resolved span — if `Source` carries a method,
%%    `resolve_class_span` clamps its span to end BEFORE that method (its own
%%    "never reaches a method" guarantee — see `class_span.rs`'s module doc),
%%    so any non-blank bytes left over past the resolved span are exactly the
%%    method text this guard exists to catch.
%%  * leading bytes before the resolved span — `resolve_class_span`'s span
%%    deliberately starts at the class's OWN declaration line, never backing
%%    up across a `///` doc comment or `//` license header (see that module's
%%    "Span boundaries" doc). A candidate `Source` with such a leading
%%    comment (e.g. a raw REPL eval that includes a NEW doc comment, unlike
%%    the `:def` tab's own skeleton, which never carries one) would still
%%    pass a trailing-only check, and splicing the whole `Source` — comment
%%    included — into the disk span (which starts only at the header,
%%    excluding the disk's own doc comment) would duplicate/misplace it.
-spec class_def_source_is_skeleton_shaped(binary(), binary()) -> boolean().
class_def_source_is_skeleton_shaped(Source, ClassNameBin) ->
    case beamtalk_compiler:resolve_class_span(Source, ClassNameBin) of
        {ok, #{start := Start, 'end' := End}, _Body} when
            Start =< End, End =< byte_size(Source)
        ->
            %% `string:trim/1` (not a hand-rolled whitespace scan): blank
            %% bytes on both sides of the resolved span means `Source` is
            %% entirely header+state; anything else (a method, a leading
            %% comment) is not blank.
            string:trim(binary:part(Source, 0, Start)) =:= <<>> andalso
                string:trim(binary:part(Source, End, byte_size(Source) - End)) =:= <<>>;
        _ ->
            %% Resolution failure (`class_not_found`/`ambiguous`) on text that
            %% was just successfully compiled under this exact class name
            %% should not happen — but fails closed (not skeleton-shaped)
            %% rather than risking an unsafe splice on a resolver surprise.
            false
    end.

-spec resolve_class_def_span_entry(map(), binary(), binary(), binary()) -> map().
resolve_class_def_span_entry(#{source := Canonical} = Base, ClassNameBin, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_class_span(DiskSource, ClassNameBin) of
        {ok, Span, PrevSource} ->
            case class_def_span_contains_all_state_fields(DiskSource, PrevSource, ClassNameBin) of
                false ->
                    Base#{
                        flushable => false,
                        not_flushable_reason => <<"class_def_state_after_method">>,
                        source_file => SourceFile
                    };
                true ->
                    case class_def_preserves_field_defaults(DiskSource, Canonical, ClassNameBin) of
                        true ->
                            store_class_def_disk_shaped_entry(Base, SourceFile, Span, PrevSource);
                        false ->
                            Base#{
                                flushable => false,
                                not_flushable_reason => <<"class_def_would_drop_field_default">>,
                                source_file => SourceFile
                            }
                    end
            end;
        {error, Reason, _Message} ->
            %% `class_not_found` (renamed/moved out from under the live class)
            %% or `ambiguous` (duplicate declarations) — either downgrades to
            %% memory-only, same as a method patch's span-resolution failure.
            span_error_entry(Base, SourceFile, Reason)
    end.

%% True iff EVERY `state:`/`field:` declaration `ClassNameBin` has anywhere in
%% `DiskSource` (the whole file) is also visible within `SpanText` (the
%% `resolve_class_span/2`-clamped region that a flush would actually splice
%% into) — i.e. the disk class has no field declared at or after its first
%% method.
%%
%% Why this exists: `resolve_class_span/2` deliberately clamps its span to
%% end BEFORE the class's first method (its own "never reaches a method" guarantee — see
%% `class_span.rs`'s module doc) — legal Beamtalk allows a `state:`/`field:`
%% declaration positioned at/after a method, and such a field is EXCLUDED
%% from the clamped span/`PrevSource`. But the `:def` tab's skeleton is built
%% from LIVE reflection (`instance_variables`), which is position-agnostic —
%% it always includes every current field regardless of where it was
%% declared on disk. `class_def_preserves_field_defaults/3` alone does not
%% catch this: a field with no default (so nothing to "lose") still gets
%% written into the skeleton, and splicing that skeleton into the clamped
%% span duplicates the field's declaration — once newly inserted at the
%% span's location, once still present, untouched, after the method. Repro:
%% `Actor subclass: Foo\n  method1 => 1\n\n  state: b\n` (b has no default)
%% resolves a span of just `Actor subclass: Foo\n`; the regenerated skeleton
%% `Actor subclass: Foo\n  state: b\n` spliced into that span leaves `state:
%% b` duplicated in the file (once before `method1`, once after).
%%
%% Reuses `beamtalk_compiler:class_state_field_defaults/2` for its field-NAME
%% enumeration (the boolean default-presence values themselves are unused
%% here) against BOTH `DiskSource` (every field on disk, any position) and
%% `SpanText` (only the fields within the region that would actually be
%% spliced) — any field present in the former but not the latter is exactly
%% the "declared after the first method" case this guards against.
-spec class_def_span_contains_all_state_fields(binary(), binary(), binary()) -> boolean().
class_def_span_contains_all_state_fields(DiskSource, SpanText, ClassNameBin) ->
    case
        {
            beamtalk_compiler:class_state_field_defaults(DiskSource, ClassNameBin),
            beamtalk_compiler:class_state_field_defaults(SpanText, ClassNameBin)
        }
    of
        {{ok, AllFields}, {ok, SpanFields}} ->
            sets:is_subset(
                sets:from_list(maps:keys(AllFields), [{version, 2}]),
                sets:from_list(maps:keys(SpanFields), [{version, 2}])
            );
        _ ->
            %% Either side failed to resolve — fail closed, same posture as
            %% `class_def_preserves_field_defaults/3`.
            false
    end.

%% True iff redefining `ClassNameBin` in `OldSource` (the on-disk file) with
%% `NewSource` (the candidate replacement — already confirmed skeleton-shaped
%% by `class_def_source_is_skeleton_shaped/2`) would NOT silently drop any
%% field's default value.
%%
%% Why this exists: `class_definition_
%% text/7`'s `default` field is read from LIVE class reflection
%% (`beamtalk_repl_ops_browse:state_slots/2`'s `field_defaults`), and that
%% reflection is populated ONLY for a `beamtalk_class_builder`-created class
%% (file-less, so it never reaches this function at all — `class_source_
%% file/1` already gates on a real `sourceFile`). A COMPILED `.bt` class's
%% `__beamtalk_meta/0` carries `field_has_default` (a boolean per field)
%% but never the default-value TEXT, so `field_defaults` reflection
%% is always empty and the skeleton always renders `default => null` — even
%% for a field the on-disk source declares with one. Splicing that skeleton
%% into a byte-accurate disk span (the span/method-safety guarantee
%% `resolve_class_span/2` gives) would still silently DELETE the default —
%% exactly the class of data-loss bug this whole feature exists to prevent,
%% just triggered by an unreflectable value instead of an unrendered
%% modifier keyword.
%%
%% Reuses `beamtalk_compiler:class_state_field_defaults/2` (the parser's own
%% `StateDeclaration.default_value`, not a text heuristic) against BOTH
%% texts: a field present in both with a default in `OldSource` must also
%% have one in `NewSource`. A field ABSENT from `NewSource` (an intentional
%% removal — the user deleted that field via the editor) is not a violation;
%% only "still declared, default silently gone" is.
-spec class_def_preserves_field_defaults(binary(), binary(), binary()) -> boolean().
class_def_preserves_field_defaults(OldSource, NewSource, ClassNameBin) ->
    case
        {
            beamtalk_compiler:class_state_field_defaults(OldSource, ClassNameBin),
            beamtalk_compiler:class_state_field_defaults(NewSource, ClassNameBin)
        }
    of
        {{ok, OldDefaults}, {ok, NewDefaults}} ->
            maps:fold(
                fun(Field, HadDefault, Acc) ->
                    Acc andalso
                        (not HadDefault orelse
                            case maps:find(Field, NewDefaults) of
                                {ok, StillHasDefault} -> StillHasDefault;
                                error -> true
                            end)
                end,
                true,
                OldDefaults
            );
        _ ->
            %% Either side failed to resolve (transport error, or the class
            %% somehow doesn't parse under its own name — pathological given
            %% `resolve_class_span/2` on `OldSource` just succeeded and
            %% `NewSource` was already confirmed skeleton-shaped): fail
            %% closed rather than risk an unsafe splice on a resolver
            %% surprise.
            false
    end.

%% Store the flushable shape of a `'class-def'` entry once its span has
%% resolved. Unlike `store_disk_shaped_entry/4` (a method patch), the
%% skeleton's `source` needs no `reindent_method_source` reshape — a class
%% header and its `state:`/`field:` lines are already built at the on-disk
%% column-0 / 2-space convention (`class_definition_text/7`) — only the
%% trailing-newline shape can mismatch (the skeleton never carries one; the
%% resolved span usually does), matched the same way a method patch's body is.
-spec store_class_def_disk_shaped_entry(
    map(), binary(), #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()
) -> map().
store_class_def_disk_shaped_entry(#{source := Canonical} = Base, SourceFile, Span, PrevSource) ->
    DiskShaped = match_trailing_newline(Canonical, PrevSource),
    Base#{
        source => DiskShaped,
        flushable => true,
        source_file => SourceFile,
        span => Span,
        prev_source => PrevSource
    }.

-doc """
Emit a `'rename-class'`/`'rename-method'` ChangeLog entry for a just-completed
`rewrite_sites/2` call (ADR 0114) — mirrors `emit_remove_change_entry/5`'s
placement (called by the caller AFTER the rewrite is already live; a
ChangeLog write failure never undoes an installed rewrite) and its best-
effort/self-swallowing failure handling.

`Spec` carries the identity fields the `sites`/`candidate_sites` schema
itself does not (ADR 0114 § ChangeLog schema) — `kind` (`'rename-class'` |
`'rename-method'`), the new `class` name, `intent`/`author`/`author_kind`,
and whichever of `selector`/`old_selector`/`side` (rename-method) or
`old_class`/`old_path`/`new_path` (rename-class) apply; `candidate_sites`
(rename-method's reported-never-rewritten senders) passes through verbatim —
this function neither computes nor validates it, since candidate-site
discovery is a future primitive's concern, not this mechanism's.

`flushable`/`not_flushable_reason` are derived generically from `RewriteResult`
via `classify_installed_site/1` + `beamtalk_workspace_changelog:sites_flushable/1`
— true iff every site (definition included) resolves to a flushable file,
matching both kinds' documented rule exactly. A `Definition` of `undefined`
(the dynamic-class case) always classifies the whole entry
`{not_flushable, <<"dynamic">>}`, matching both schemas' `not_flushable_reason:
"dynamic"` case. Each site's body is persisted to the ChangeLog's `sources/`
directory via `beamtalk_workspace_changelog:store_site_body/1` before the
entry itself is appended (a site's `source_ref`/`prev_source_ref` must
already be a written ref by the time `append/1` sees it — see that
function's doc).
""".
-spec emit_rewrite_change_entry(map(), beamtalk_repl_loader_rewrite:rewrite_result()) -> ok.
emit_rewrite_change_entry(Spec, RewriteResult) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for multi-site rewrite (rewrite still installed)",
        #{spec => Spec},
        fun() -> rewrite_change_entry(Spec, RewriteResult) end
    ).

-spec rewrite_change_entry(map(), beamtalk_repl_loader_rewrite:rewrite_result()) -> map().
rewrite_change_entry(Spec, #{definition := Definition, sites := Sites}) ->
    #{
        kind := Kind,
        class := ClassNameBin,
        intent := Intent,
        author := Author,
        author_kind := AuthorKind
    } = Spec,
    AllInstalled = [Definition | Sites],
    {Flushable, NotFlushableReason} =
        beamtalk_workspace_changelog:sites_flushable(
            [classify_installed_site(S) || S <- AllInstalled]
        ),
    #{
        class => ClassNameBin,
        kind => Kind,
        selector => maps:get(selector, Spec, undefined),
        old_selector => maps:get(old_selector, Spec, undefined),
        side => maps:get(side, Spec, undefined),
        old_class => maps:get(old_class, Spec, undefined),
        old_path => maps:get(old_path, Spec, undefined),
        new_path => maps:get(new_path, Spec, undefined),
        sites => [site_append_input(S) || S <- AllInstalled],
        candidate_sites => maps:get(candidate_sites, Spec, undefined),
        intent => Intent,
        flushable => Flushable,
        not_flushable_reason => NotFlushableReason,
        author => Author,
        author_kind => AuthorKind
    }.

%% `undefined` (the dynamic-class definition-site case) always classifies as
%% not-flushable/"dynamic", matching both `'rename-class'`'s and
%% `'rename-method'`'s documented dynamic-class row (ADR 0114 § Refusal vs
%% flushability) — a class with no backing file can never be flushed
%% regardless of what its other sites look like. Reuses `classify_source_file/1`
%% (already exported for exactly this kind of reuse — see its own callers)
%% rather than re-deriving stdlib/dependency/project classification.
-spec classify_installed_site(beamtalk_repl_loader_rewrite:installed_rewrite_site() | undefined) ->
    flushable | {not_flushable, binary()}.
classify_installed_site(undefined) ->
    {not_flushable, <<"dynamic">>};
classify_installed_site(#{source_file := undefined}) ->
    {not_flushable, <<"dynamic">>};
classify_installed_site(#{source_file := SourceFile}) ->
    case classify_source_file(SourceFile) of
        {flushable, _AbsPath} -> flushable;
        {not_flushable, Reason} -> {not_flushable, Reason}
    end.

-spec site_append_input(beamtalk_repl_loader_rewrite:installed_rewrite_site() | undefined) ->
    beamtalk_workspace_changelog:site() | undefined.
site_append_input(undefined) ->
    undefined;
site_append_input(#{
    source_file := SourceFile, span := Span, prev_source := PrevSource, source := Source
}) ->
    #{
        source_file => SourceFile,
        span => Span,
        source_ref => store_rewrite_site_ref(Source),
        prev_source_ref => store_rewrite_site_ref(PrevSource)
    }.

-spec store_rewrite_site_ref(binary()) -> binary() | undefined.
store_rewrite_site_ref(Body) ->
    case beamtalk_workspace_changelog:store_site_body(Body) of
        {ok, Ref} ->
            Ref;
        undefined ->
            undefined;
        {error, Reason} ->
            ?LOG_WARNING(
                "Failed to persist rewrite-site body to ChangeLog sources/ (site recorded without a body ref)",
                #{reason => Reason, domain => [beamtalk, runtime]}
            ),
            undefined
    end.

-spec patch_side(boolean()) -> instance | class.
patch_side(true) -> class;
patch_side(false) -> instance.

%% Best source for the patched method body, preferring the compiler-extracted
%% `method_source' (the `selector => body' fragment) and falling back to the raw
%% `expression' (the full `Class >> selector => body' line).
-spec method_source_binary(map()) -> binary().
method_source_binary(MethodInfo) ->
    case maps:get(method_source, MethodInfo, undefined) of
        Bin when is_binary(Bin) ->
            Bin;
        Str when is_list(Str) ->
            list_to_binary(Str);
        undefined ->
            case maps:get(expression, MethodInfo, <<>>) of
                B when is_binary(B) -> B;
                L when is_list(L) -> list_to_binary(L);
                _ -> <<>>
            end
    end.

%% Derive flushability and, when flushable, the on-disk span + prev_source.
%%
%% A class is flushable iff its `sourceFile' is non-nil AND lies inside the active
%% project tree. For flushable classes we resolve the method's byte span against
%% the *current on-disk* file (not the in-memory combined source) so a later
%% flush splices into exactly what is on disk. If the file cannot be read or the
%% span cannot be resolved, the patch downgrades to non-flushable.
-spec add_flushability(map(), binary(), binary(), instance | class) -> map().
add_flushability(Base, ClassNameBin, SelectorBin, Side) ->
    resolve_flushability(
        Base,
        ClassNameBin,
        "ChangeLog: could not read sourceFile for live patch; recording memory-only",
        fun(SourceFile, DiskSource) ->
            resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource)
        end
    ).

%% Reason for a class with no backing `.bt' sourceFile (`class_source_file/1'
%% returned nil). Distinguishes a stdlib class — whose module is compiled to a
%% `.beam' on disk (`code:which/1' returns a path) — from a dynamically-created
%% / live-only class whose module exists only in memory (`code:which/1' returns
%% a non-path atom such as `non_existing', `cover_compiled', or `preloaded').
%% Matches the ChangeLog schema's documented "stdlib" / "dynamic" reasons so flush
%% summaries can tell the two apart.
-spec no_source_reason(binary()) -> binary().
no_source_reason(ClassNameBin) ->
    case class_module(ClassNameBin) of
        {ok, Module} ->
            case code:which(Module) of
                %% A non-EMPTY path means a real `.beam' on disk → a stdlib /
                %% precompiled class. `code:which/1' returns `[]' (an empty list,
                %% so `is_list/1' is still true) for a module loaded from binary
                %% with no file — that is a live-only / dynamic class, NOT stdlib.
                Path when is_list(Path), Path =/= [] -> <<"stdlib">>;
                _NonPathOrEmpty -> <<"dynamic">>
            end;
        error ->
            <<"dynamic">>
    end.

%% Resolve a class name binary to its loaded BEAM module name, if the class is
%% registered. Returns `error' when the class is unknown / not loaded.
-spec class_module(binary()) -> {ok, atom()} | error.
class_module(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    {ok, beamtalk_object_class:module_name_safe(Pid)};
                _ ->
                    protocol_module(ClassName)
            end;
        {error, _} ->
            error
    end.

-spec resolve_span_entry(map(), binary(), binary(), instance | class, binary(), binary()) -> map().
resolve_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, ClassNameBin, SelectorBin, Side) of
        {ok, Span, PrevSource} ->
            store_disk_shaped_entry(Base, SourceFile, Span, PrevSource);
        {error, selector_not_found, _Message} ->
            %% Selector absent on disk (a brand-new method added live) is normal:
            %% record a flushable entry with no prev span — a later flush appends
            %% the method. The compiler's canonical body is column-0, but the
            %% class body on disk is indented, so reshape the body to the class's
            %% sibling-method indentation at store time — mirroring
            %% `store_disk_shaped_entry`'s reshape — so flush's `append_method`
            %% stays a verbatim append into an indented body.
            new_method_entry(Base, SourceFile, DiskSource);
        {error, Reason, _Message} ->
            %% Any other resolution failure downgrades to memory-only.
            span_error_entry(Base, SourceFile, Reason)
    end.

%% Reshape a brand-new method (no prior on-disk span) to the target class body's
%% indentation at store time, so flush's `append_method' stays a verbatim append.
%% The compiler's canonical `unparse_method' body is column-0, but the
%% class body on disk is indented; an un-reshaped append would write the method
%% at column 0 into an indented class. The base indentation is derived from a
%% sibling method already on disk (falling back to the project's 2-space step),
%% mirroring `store_disk_shaped_entry''s store-time reshape. If the reshape FFI is
%% unavailable, downgrade to memory-only rather than store a column-0 body that
%% flush would append un-indented.
-spec new_method_entry(map(), binary(), binary()) -> map().
new_method_entry(#{source := Canonical} = Base, SourceFile, DiskSource) ->
    BaseIndent = sibling_method_indent(DiskSource),
    %% Reshape via the compiler port: it re-lays-out the canonical body at the
    %% target indent (re-breaking width-sensitive lines, which a pure whitespace
    %% shift cannot), so the stored body is byte-identical to what
    %% `bt fmt' produces on disk. The port is already up here (the method was just
    %% compiled through it); a transient failure downgrades to memory-only rather
    %% than store a column-0 body flush would append un-indented.
    case beamtalk_compiler:reindent_method_source(Canonical, BaseIndent) of
        {ok, Reindented} ->
            Base#{source => Reindented, flushable => true, source_file => SourceFile};
        {error, ReindentReason, _Msg} ->
            ?LOG_WARNING(
                "ChangeLog: could not reshape new-method body to class indentation; "
                "recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReindentReason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% This memory-only path is deliberate: the pure-Erlang shift was
            %% total but reshaped width-sensitive methods wrongly; port
            %% re-layout is correct, at the cost of a rare transient-failure
            %% downgrade.
            Base#{flushable => false, not_flushable_reason => <<"reindent_failed">>}
    end.

%% Derive the base indentation for a brand-new method from the class body on
%% disk: the leading whitespace of the first indented, non-blank, non-comment
%% line (a sibling method or field definition). Falls back to the project's
%% 2-space convention when the class body has no indented member yet (e.g. an
%% empty class). Comment lines (`//`, `///`) and the unindented class header are
%% skipped so the step reflects member indentation, not column 0.
-spec sibling_method_indent(binary()) -> binary().
sibling_method_indent(DiskSource) ->
    Lines = binary:split(DiskSource, <<"\n">>, [global]),
    sibling_method_indent_lines(Lines).

-spec sibling_method_indent_lines([binary()]) -> binary().
sibling_method_indent_lines([]) ->
    default_method_indent();
sibling_method_indent_lines([Line | Rest]) ->
    Indent = beamtalk_workspace_reshape:leading_ws(Line),
    Content = strip_leading_ws(Line),
    case Indent =/= <<>> andalso not is_comment_or_blank(Content) of
        true -> Indent;
        false -> sibling_method_indent_lines(Rest)
    end.

%% The project's default member indentation: two spaces (ADR 0082 / stdlib
%% convention). Used for a class with no indented member to copy.
-spec default_method_indent() -> binary().
default_method_indent() -> <<"  ">>.

%% True for a line whose content (leading whitespace already stripped) is empty
%% or a line comment — such lines do not establish member indentation.
-spec is_comment_or_blank(binary()) -> boolean().
is_comment_or_blank(<<>>) -> true;
is_comment_or_blank(<<"//", _/binary>>) -> true;
is_comment_or_blank(_) -> false.

%% Drop the leading run of spaces/tabs from a line.
-spec strip_leading_ws(binary()) -> binary().
strip_leading_ws(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t ->
    strip_leading_ws(Rest);
strip_leading_ws(Bin) ->
    Bin.

%% Reshape the stored `source' (the compiler's canonical column-0
%% `unparse_method' body) to the on-disk byte-span shape so the ChangeEntry's
%% `source_ref' is a drop-in for `disk[span]' — `source_ref == disk[span]' by
%% construction. A later `Workspace flush' then splices it verbatim
%% with no reindent. The base indentation is the leading whitespace of the disk
%% slice (`PrevSource') the patch replaces. If the reshape FFI is unavailable,
%% downgrade to memory-only rather than store a column-0 body that flush would
%% splice into an indented region and corrupt the file.
-spec store_disk_shaped_entry(
    map(), binary(), #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()
) -> map().
store_disk_shaped_entry(#{source := Canonical} = Base, SourceFile, Span, PrevSource) ->
    BaseIndent = beamtalk_workspace_reshape:leading_ws(PrevSource),
    %% Reshape via the compiler port (re-layout at the span's indent),
    %% so `source_ref == disk[span]' even for width-sensitive methods a pure shift
    %% would reformat. The port is already up (the patch was just compiled through
    %% it); a transient failure downgrades to memory-only rather than store a
    %% column-0 body flush would splice into an indented region and corrupt it.
    case beamtalk_compiler:reindent_method_source(Canonical, BaseIndent) of
        {ok, Reindented} ->
            %% The disk byte-span ends in a trailing newline unless the method is
            %% the last line of the file with no terminator (ADR 0082). Match that
            %% trailing-newline state on the reshaped body — regardless of whether
            %% the canonical body carries its own — so the splice is a true drop-in
            %% and never glues the next line or leaves a stray blank one.
            DiskShaped = match_trailing_newline(Reindented, PrevSource),
            Base#{
                source => DiskShaped,
                flushable => true,
                source_file => SourceFile,
                span => Span,
                prev_source => PrevSource
            };
        {error, ReindentReason, _Msg} ->
            ?LOG_WARNING(
                "ChangeLog: could not reshape patch body to disk indentation; "
                "recording memory-only",
                #{
                    source_file => SourceFile,
                    reason => ReindentReason,
                    domain => [beamtalk, runtime]
                }
            ),
            %% This memory-only path is deliberate (see new_method_entry/3):
            %% port re-layout is correct where the pure-Erlang shift reshaped
            %% width-sensitive methods wrongly.
            Base#{flushable => false, not_flushable_reason => <<"reindent_failed">>}
    end.

%% Make `Source''s trailing-newline state match `Reference''s: append a single
%% `\n' when the reference ends in one and the source does not; strip trailing
%% `\n's when the reference has none (the clamped-at-EOF last-method case). Keeps
%% the reshaped body a true drop-in for the disk slice it replaces.
-spec match_trailing_newline(binary(), binary()) -> binary().
match_trailing_newline(Source, Reference) ->
    case ends_with_newline(Reference) of
        true -> beamtalk_workspace_reshape:ensure_trailing_newline(Source);
        false -> beamtalk_workspace_reshape:strip_trailing_newlines(Source)
    end.

-spec ends_with_newline(binary()) -> boolean().
ends_with_newline(<<>>) -> false;
ends_with_newline(Bin) -> binary:last(Bin) =:= $\n.

%% A genuine span-resolution failure (`ambiguous', a port/transport error, ...)
%% downgrades the entry to memory-only with a reason. The brand-new-method case
%% (`selector_not_found') does NOT come here — it is reshaped and recorded
%% flushable by `new_method_entry/3'.
-spec span_error_entry(map(), binary(), atom()) -> map().
span_error_entry(Base, _SourceFile, Reason) ->
    Base#{
        flushable => false,
        not_flushable_reason =>
            iolist_to_binary([<<"span_unresolved:">>, atom_to_binary(Reason, utf8)])
    }.

%%% ----------------------------------------------------------------------------
%%% Method-removal ChangeLog entry (ADR 0112 Phase 3)
%%% ----------------------------------------------------------------------------

-doc """
Emit a `"remove-method"` ChangeLog entry for a just-completed LOCAL (non-
extension) method removal.

Called by `beamtalk_behaviour_intrinsics:remove_selector/2` (backing
`Behaviour>>removeSelector:` / `removeSelector:ifAbsent:`) *after*
`remove_method/3` has already spliced the method out and hot-reloaded the
class — mirrors `emit_change_entry/1`'s placement for a patch (step 3, after
the memory install; ADR 0082's "install is authoritative, log is best-effort"
ordering, reused verbatim for removal per ADR 0112 § ChangeLog interaction)
and its best-effort/self-swallowing failure handling: a ChangeLog write
failure never undoes the removal, which is already live.

`sourceFile` / `flushable` / `not_flushable_reason` are derived by the exact
same classification `add_flushability/4` uses for a patch
(`class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`,
reused directly rather than re-derived) — stdlib/dynamic/dependency classes
get `flushable: false` with the matching reason; an in-project class gets
`flushable: true`. `span` / `prev_source` are resolved against the CURRENT
on-disk file (not the in-memory source `remove_method/3` just spliced) —
matching how a patch's span is always resolved against disk rather than
memory — since that is the byte span a future flush-excise step will
need. A selector already absent from disk (the method being removed was
itself a live, never-flushed addition) has nothing to excise there either, so
the entry downgrades to `flushable: false, not_flushable_reason: "not_on_disk"`
rather than inventing a span. `source_ref` is always absent — a removal has
no new body to store (ADR 0112 § ChangeLog interaction: `source_ref: null`);
the append input map below simply carries no `source` key, which
`beamtalk_workspace_changelog:do_append/2` treats as "no source_ref".
""".
-spec emit_remove_change_entry(
    binary(), atom() | binary(), instance | class, binary(), human | agent
) ->
    ok.
emit_remove_change_entry(ClassNameBin, Selector, Side, Author, AuthorKind) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for method removal (removal still installed)",
        #{class => ClassNameBin, selector => Selector},
        fun() ->
            SelectorBin = method_selector_binary(Selector),
            Base = #{
                class => ClassNameBin,
                selector => SelectorBin,
                kind => 'remove-method',
                side => Side,
                intent => durable,
                author => Author,
                author_kind => AuthorKind
            },
            add_removal_flushability(Base, ClassNameBin, SelectorBin, Side)
        end
    ).

%% Derive flushability + (when flushable) the on-disk span/prev_source for a
%% removal entry. Same classification as `add_flushability/4` (both go
%% through `resolve_flushability/4`) but never reshapes/stores a new
%% `source` — a removal has none to reshape.
-spec add_removal_flushability(map(), binary(), binary(), instance | class) -> map().
add_removal_flushability(Base, ClassNameBin, SelectorBin, Side) ->
    resolve_flushability(
        Base,
        ClassNameBin,
        "ChangeLog: could not read sourceFile for method removal; recording memory-only",
        fun(SourceFile, DiskSource) ->
            resolve_removal_span_entry(
                Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource
            )
        end
    ).

-spec resolve_removal_span_entry(
    map(), binary(), binary(), instance | class, binary(), binary()
) -> map().
resolve_removal_span_entry(Base, ClassNameBin, SelectorBin, Side, SourceFile, DiskSource) ->
    case beamtalk_compiler:resolve_method_span(DiskSource, ClassNameBin, SelectorBin, Side) of
        {ok, Span, PrevSource} ->
            Base#{
                flushable => true,
                source_file => SourceFile,
                span => Span,
                prev_source => PrevSource
            };
        {error, selector_not_found, _Message} ->
            %% Nothing on disk to excise (the method was a live, never-flushed
            %% addition) — nothing for a future flush-excise step to
            %% act on either.
            Base#{flushable => false, not_flushable_reason => <<"not_on_disk">>};
        {error, Reason, _Message} ->
            %% Any other resolution failure (ambiguous / port error / the file
            %% changed underneath us) downgrades to memory-only, exactly as a
            %% patch's span-resolution failure does.
            span_error_entry(Base, SourceFile, Reason)
    end.

-doc """
Emit a `"remove-method"` ChangeLog entry for a just-completed EXTENSION
method removal (ADR 0066 open classes; ADR 0112 § Extension methods, §
ChangeLog interaction).

Unlike `emit_remove_change_entry/5` (a local class-body method, whose
span/prev_source resolve against the target class's own on-disk file), an
extension method's source lives in a *different* file than the extended
class's — `beamtalk_extensions` tracks only the owning package/module atom
(`Owner`, ADR 0070), not which file registered a given `{Class, Selector}`,
so this attributes the entry the most it can honestly claim:

  - `sourceFile` resolves iff `Owner` also happens to name a currently-loaded
    class (the common case: a file with no explicit package declaration
    registers under its own module-derived name) — reuses
    `class_source_file/1`, the same resolver an ordinary class uses. Any
    other `Owner` (a genuine multi-class package, or one that never loaded)
    leaves `sourceFile` unset rather than guessing wrong.
  - `span` is always `undefined` — locating a standalone `TargetClass >>
    selector` definition's byte offsets inside an arbitrary owner file has no
    resolver in this codebase (`beamtalk_compiler:resolve_method_span/4`
    looks for a method *inside* a named class's own body, not a foreign
    extension line); building one is out of this issue's scope.
  - The entry is always `flushable: false, not_flushable_reason:
    "extension"` — precise disk attribution/excise for extension removals is
    unbuilt infrastructure (same boundary as every other
    `remove-method` flush-excise case), not something invented here.

`Owner` and `PrevBody` (the extension's stored source text) must be captured
by the CALLER before unregistering — `beamtalk_extensions:unregister/3`
deletes both the moment it returns, so by the time this function runs neither
is recoverable from the registry itself. `PrevBody` becomes `prev_source` so
the audit trail at least records what was removed, even though automated
revert cannot yet act on it (reverting a `sourceFile`-less, `span`-less entry
has nothing to re-install against).
""".
-spec emit_extension_remove_change_entry(
    binary(),
    atom() | binary(),
    instance | class,
    atom() | undefined,
    binary() | undefined,
    binary(),
    human | agent
) -> ok.
emit_extension_remove_change_entry(
    ClassNameBin, Selector, Side, Owner, PrevBody, Author, AuthorKind
) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for extension removal (removal still installed)",
        #{class => ClassNameBin, selector => Selector},
        fun() ->
            SelectorBin = method_selector_binary(Selector),
            Base0 = #{
                class => ClassNameBin,
                selector => SelectorBin,
                kind => 'remove-method',
                side => Side,
                intent => durable,
                author => Author,
                author_kind => AuthorKind,
                flushable => false,
                not_flushable_reason => <<"extension">>
            },
            Base1 = maybe_put_prev_source(Base0, PrevBody),
            maybe_put_extension_source_file(Base1, Owner)
        end
    ).

-spec maybe_put_prev_source(map(), binary() | undefined) -> map().
maybe_put_prev_source(Base, undefined) -> Base;
maybe_put_prev_source(Base, PrevBody) when is_binary(PrevBody) -> Base#{prev_source => PrevBody}.

-spec maybe_put_extension_source_file(map(), atom() | undefined) -> map().
maybe_put_extension_source_file(Base, undefined) ->
    Base;
maybe_put_extension_source_file(Base, Owner) when is_atom(Owner) ->
    case class_source_file(atom_to_binary(Owner, utf8)) of
        nil -> Base;
        SourceFile -> Base#{source_file => SourceFile}
    end.

%%% ----------------------------------------------------------------------------
%%% Class-removal ChangeLog entry
%%% ----------------------------------------------------------------------------

-doc """
Snapshot a class's current full source and its on-disk flushability
classification, for a `"remove-class"` ChangeLog entry that will be appended
later — after `classRemoveFromSystemByName/1`'s teardown has already run.

Must be called BEFORE that teardown starts: `class_source_file/1` needs the
class's still-live registry pid to resolve its module (teardown stops the
class gen_server), and `beamtalk_workspace_meta:get_class_source/1` needs the
`class_sources` entry that `beamtalk_class_lifecycle:class_removed/2` purges
as part of the same teardown. Capturing here — mirrors the
"read+parse before mutate" ordering `compile:source:`'s patch hook already
uses — is what gives `revert:` (a later phase) something to restore
from.

Reuses `class_source_file/1` / `classify_source_file/1` / `no_source_reason/1`
— the exact same classification `add_flushability/4` uses for a patch — so a
dynamically-created (ClassBuilder) class gets `flushable => false,
not_flushable_reason => <<"dynamic">>` and an ordinary in-project class gets
`flushable => true, source_file => SourceFile`. `classRemoveFromSystemByName/1`
already refuses stdlib classes before this is ever called, so the `"stdlib"`
reason `no_source_reason/1` can also produce never actually reaches here.
Unlike the method-removal snapshot, there is no span to resolve (a class
removal excises the whole file entry, not a byte range within it) — the
ChangeLog entry this feeds always carries `span: null`.
""".
-spec capture_class_removal_snapshot(binary()) -> map().
capture_class_removal_snapshot(ClassNameBin) ->
    Base = maybe_put_prev_source(#{}, current_class_source(ClassNameBin)),
    case class_source_file(ClassNameBin) of
        nil ->
            Base#{flushable => false, not_flushable_reason => no_source_reason(ClassNameBin)};
        SourceFile when is_binary(SourceFile) ->
            case classify_source_file(SourceFile) of
                {flushable, _AbsPath} ->
                    Base#{flushable => true, source_file => SourceFile};
                {not_flushable, Reason} ->
                    Base#{flushable => false, not_flushable_reason => Reason}
            end
    end.

%% The class's current tracked source text (whole-file, per `set_class_source/2`),
%% or `undefined` when nothing has been recorded for it (mirrors
%% `beamtalk_workspace_meta:get_class_source/1`'s own `undefined` degrade for
%% an untracked class). `get_class_source/1` always returns a plain list
%% (`string()`) or `undefined` per its own spec, never a binary.
-spec current_class_source(binary()) -> binary() | undefined.
current_class_source(ClassNameBin) ->
    case beamtalk_workspace_meta:get_class_source(ClassNameBin) of
        Source when is_list(Source) -> unicode:characters_to_binary(Source);
        undefined -> undefined
    end.

-doc """
Emit a `"remove-class"` ChangeLog entry for a just-completed
`removeFromSystem`.

Called by `beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1` at
its existing success point — immediately after `publish_class_removed/2` —
with `Snapshot` already captured by `capture_class_removal_snapshot/1` before
teardown began. Mirrors `emit_remove_change_entry/5`'s placement (ADR 0082's
"install is authoritative, log is best-effort" ordering) and its
best-effort/self-swallowing failure handling: a ChangeLog write failure never
undoes the removal, which is already irreversible in memory by this point.

`selector` and `side` are always absent (`null` in the persisted entry) — a
class removal has no method-level target (ADR 0082's established schema,
reused verbatim: `side: null` for every kind but `"remove-method"`).
""".
-spec emit_remove_class_change_entry(binary(), map(), binary(), human | agent) -> ok.
emit_remove_class_change_entry(ClassNameBin, Snapshot, Author, AuthorKind) ->
    best_effort_changelog_append(
        "Failed to emit ChangeLog entry for class removal (removal still installed)",
        #{class => ClassNameBin},
        fun() ->
            Base = #{
                class => ClassNameBin,
                kind => 'remove-class',
                intent => durable,
                author => Author,
                author_kind => AuthorKind
            },
            maps:merge(
                Base,
                maps:with([flushable, not_flushable_reason, source_file, prev_source], Snapshot)
            )
        end
    ).

%% Resolve the class's source file via the BEAM module attribute (the same
%% source-of-truth `Behaviour>>sourceFile' reads). Returns nil for classes with
%% no backing file (stdlib / dynamic / not loaded).
-spec class_source_file(binary()) -> binary() | nil.
class_source_file(ClassNameBin) ->
    case beamtalk_repl_server:safe_to_existing_atom(ClassNameBin) of
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                Pid when is_pid(Pid) ->
                    ModuleName = beamtalk_object_class:module_name_safe(Pid),
                    beamtalk_reflection:source_file_from_module(ModuleName);
                _ ->
                    %% ADR 0127: a protocol's provision patch/removal is
                    %% recorded (and flushed) against the PROTOCOL's own file.
                    case protocol_module(ClassName) of
                        {ok, ProtocolModule} ->
                            beamtalk_reflection:source_file_from_module(ProtocolModule);
                        error ->
                            nil
                    end
            end;
        {error, _} ->
            nil
    end.

%% Classify a class's source file as flushable (in-project) or not.
%%
%% `flushable' requires the file to resolve to an absolute path inside the active
%% project source tree (per workspace metadata `project_path'). Files outside it
%% are a dependency; a workspace with no project path treats all paths as
%% non-flushable (nothing to flush into).
-spec classify_source_file(binary()) ->
    {flushable, string()} | {not_flushable, binary()}.
classify_source_file(SourceFile) ->
    SourceStr = binary_to_list(SourceFile),
    AbsPath = filename:absname(SourceStr),
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
            ProjectRoot = filename:absname(binary_to_list(ProjectPath)),
            case beamtalk_workspace_file:path_inside(ProjectRoot, AbsPath) of
                true -> {flushable, AbsPath};
                false -> {not_flushable, dependency_reason(SourceFile)}
            end;
        _ ->
            %% No project context — cannot flush into a tree we do not know.
            {not_flushable, dependency_reason(SourceFile)}
    end.

-spec dependency_reason(binary()) -> binary().
dependency_reason(SourceFile) ->
    iolist_to_binary([<<"dependency:">>, SourceFile]).

%% The BEAM module that defines protocol `Name`, if it is registered.
-spec protocol_module(atom()) -> {ok, atom()} | error.
protocol_module(Name) ->
    case beamtalk_protocol_registry:protocol_info(Name) of
        #{module := Module} -> {ok, Module};
        _ -> error
    end.

-spec method_selector_binary(atom() | binary()) -> binary().
method_selector_binary(Sel) when is_binary(Sel) -> Sel;
method_selector_binary(Sel) when is_atom(Sel) -> atom_to_binary(Sel, utf8).
