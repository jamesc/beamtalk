%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_interface_primitives).

%%% **DDD Context:** Workspace Context

-moduledoc """
Primitive implementations for the WorkspaceInterface sealed Object.

Implements methods for the WorkspaceInterface class. WorkspaceInterface is
a `sealed Object subclass:` (value type, no gen_server process). Methods
are called via Erlang FFI from the compiled Beamtalk module.

## State management

User-registered bindings (`bind:as:` / `unbind:`) are stored in a public
named ETS table `beamtalk_wi_user_bindings` keyed by `Name` (atom).
Since WorkspaceInterface is a singleton (one per workspace), no per-process
keying is needed. The ETS table is normally created during workspace startup
via `create_bindings_table/0` (called by `beamtalk_workspace_bootstrap`),
which ensures the long-lived bootstrap process owns it. A lazy fallback via
`ensure_bindings_table/0` is also available for cases where the table is
accessed before bootstrap completes. Both paths produce the same singleton
table. The table is accessible from external processes via
`get_user_bindings/0` and `get_session_bindings/0`.

## External API

`get_user_bindings/0` and `get_session_bindings/0` are called by
`beamtalk_repl_eval` and `beamtalk_repl_shell` to inject workspace bindings
into REPL session state. Workspace readiness is detected via
`whereis(beamtalk_workspace_meta)`.

## Methods

| Selector      | Description                                         |
|---------------|-----------------------------------------------------|
| `actors'      | List all live actor object references               |
| `actorAt:'    | Look up actor by pid string                         |
| `classes'     | List all loaded user classes                        |
| `load:'       | Compile and load a .bt file                         |
| `globals'     | Full workspace namespace snapshot (Dictionary)      |
| `sync'        | Incremental project sync (compile changed files)   |
| `bind:as:'    | Register a value in workspace namespace             |
| `unbind:'     | Remove a value from workspace namespace             |
| `recheckImage'| Whole-image re-check (ADR 0105 Phase 3, BT-2782)    |
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).
%% Stable external API (called by repl_eval and repl_shell)
-export([get_user_bindings/0, get_session_bindings/0]).
%% BT-2365 (ADR 0081 Phase 1): shared lazy name resolver. Single source of truth
%% for bare-name resolution (REPL codegen fallthrough) and Session resolve:.
-export([resolve_name/2]).
%% BT-2365: capitalised class-reference resolution (REPL codegen). Shares the
%% singleton + class-registry tiers with resolve_name/2 but keeps the
%% class_not_found terminal so the "Class 'X' not found" error is preserved.
-export([resolve_class_reference/2]).
%% BT-2365: singleton-instance lookup for the binding-aware class-send fallback
%% (a message sent to a singleton receiver, e.g. `Workspace bind:as:`). Returns
%% the live instance so dispatch goes to it rather than a non-existent class.
-export([resolve_singleton_instance/1]).
%% Called by beamtalk_workspace_bootstrap to create the ETS table under a
%% long-lived process (prevents table from being deleted when eval workers exit)
-export([create_bindings_table/0]).
%% Direct exports for Erlang FFI calls from sealed Object WorkspaceInterface
-export([actors/0, actorAt/1, classes/0, load/1, globals/0, bind/2, unbind/1, rootSupervisor/0]).

-export([currentSession/0, sessions/0]).
%% Supervisor lifecycle management (BT-1341)
-export([startSupervisor/1, stopSupervisor/1, supervisors/0]).
%% Package reflection (ADR 0070 Phase 5)
-export([dependencies/0]).
%% Project sync (BT-1723)
-export([sync/0]).
%% New-class creation (ADR 0082 Phase 1, BT-2285)
-export([newClass/2]).
%% Class file move — pure path change, no identity change (ADR 0114 Phase 2,
%% BT-3272).
-export([moveClass/2]).
%% Workspace flush (ADR 0082 Phase 2, BT-2286; destructive tiering ADR 0113
%% Phase 2, BT-3207).
-export([flush/0, flush/1, flush/2, flushIncludingDestructive/0]).
%% Whole-image re-check (ADR 0105 Phase 3, BT-2782)
-export([recheckImage/0]).
%% ChangeLog Phase 4 operations and autoflush setting (ADR 0082 Phase 4, BT-2290)
-export([changeLogRevert/1, changeLogClear/0, changeLogFlushKinds/1, changeLogFlushKinds/2]).
%% Clean-returning revert for non-FFI callers (the LiveView Attach client, ADR
%% 0082 Phase 5, BT-2293). `revert_method/3` (ADR 0112, BT-3187) is the
%% side-aware surface the LiveView `Workspace changes` row now calls with its
%% own `side`; `revert_method/2` stays side-agnostic (highest-seq selection)
%% for callers with no side information.
-export([revert_method/2, revert_method/3]).
-export([autoflush/0, setAutoflush/1]).
%% Shared with beamtalk_repl_loader:new_class/2 to surface created classes to the
%% REPL identically to a file load.
-export([loaded_class_objects/1]).

%% ETS table name for user workspace bindings
-define(WI_BINDINGS_TABLE, beamtalk_wi_user_bindings).

%%% ============================================================================
%%% dispatch/3 — kept for backward compatibility; delegates to direct functions
%%% ============================================================================

-doc """
Dispatch a primitive method call for WorkspaceInterface.

Retained for backward compatibility. The compiled sealed Object module
uses Erlang FFI calls to the direct exports instead of this dispatch/3.
""".
-spec dispatch(atom(), list(), term()) -> term().
dispatch(actors, [], _Self) ->
    actors();
dispatch('actorAt:', [PidStr], _Self) ->
    actorAt(PidStr);
dispatch(classes, [], _Self) ->
    classes();
dispatch('load:', [Path], _Self) ->
    load(Path);
dispatch(globals, [], _Self) ->
    globals();
dispatch('bind:as:', [Value, Name], _Self) ->
    bind(Value, Name);
dispatch('unbind:', [Name], _Self) ->
    unbind(Name);
dispatch(rootSupervisor, [], _Self) ->
    rootSupervisor();
dispatch(currentSession, [], _Self) ->
    currentSession();
dispatch(sessions, [], _Self) ->
    sessions();
dispatch('startSupervisor:', [ClassArg], _Self) ->
    startSupervisor(ClassArg);
dispatch('stopSupervisor:', [ClassArg], _Self) ->
    stopSupervisor(ClassArg);
dispatch(supervisors, [], _Self) ->
    supervisors();
dispatch(sync, [], _Self) ->
    sync();
dispatch('newClass:at:', [Source, Path], _Self) ->
    newClass(Source, Path);
dispatch('moveClass:to:', [ClassArg, NewPath], _Self) ->
    moveClass(ClassArg, NewPath);
dispatch(flush, [], _Self) ->
    flush();
dispatch('flush:', [Filter], _Self) ->
    flush(Filter);
dispatch('flush:confirmDestructive:', [Filter, ConfirmDestructive], _Self) ->
    flush(Filter, ConfirmDestructive);
dispatch(flushIncludingDestructive, [], _Self) ->
    flushIncludingDestructive();
dispatch(recheckImage, [], _Self) ->
    recheckImage();
dispatch(autoflush, [], _Self) ->
    autoflush();
dispatch('autoflush:', [Value], _Self) ->
    setAutoflush(Value);
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Err1, <<"To list available selectors, use: Workspace methods">>
        )
    ).

%%% ============================================================================
%%% Direct exports for Erlang FFI (called via ErlangModule proxy)
%%% ============================================================================

-doc """
Return a list of all live actors as beamtalk_object references.
Called via `(Erlang beamtalk_workspace_interface_primitives) actors`.
""".
-spec actors() -> [tuple()].
actors() ->
    handle_actors().

-doc """
Look up a specific actor by pid string.
Called via `(Erlang beamtalk_workspace_interface_primitives) actorAt: pidString`.
""".
-spec actorAt(binary() | list() | term()) -> tuple() | 'nil'.
actorAt(PidStr) ->
    handle_actor_at(PidStr).

-doc """
Return all loaded user classes.
Called via `(Erlang beamtalk_workspace_interface_primitives) classes`.
""".
-spec classes() -> [tuple()].
classes() ->
    handle_classes().

-doc """
Compile and load a .bt file.
Called via `(Erlang beamtalk_workspace_interface_primitives) load: path`.
Returns the loaded class object (or a list of class objects for multi-class files).
""".
-spec load(term()) -> term().
load(Path) ->
    case handle_load(Path) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Create a brand-new class from a source String at a target path (ADR 0082 Phase 1).

Called via `(Erlang beamtalk_workspace_interface_primitives) newClass: source at: path`.
Compiles and installs the class in memory and logs a durable `kind: "new-class"`
ChangeEntry (no disk write — that happens later in `Workspace flush`, Phase 2).
Raises a loud, specific `#beamtalk_error{}` (no silent fallback) when the target
already exists, lies outside the project tree, the declared class name does not
match the path basename, or a class of that name is already loaded. On success
returns the loaded class object(s), matching `load:`.
""".
-spec newClass(term(), term()) -> term().
newClass(Source, Path) ->
    case validate_new_class_args(Source, Path) of
        {ok, SourceBin, PathBin} ->
            case beamtalk_repl_eval:new_class(SourceBin, PathBin) of
                {ok, ClassObjects} -> ClassObjects;
                {error, Err} -> beamtalk_error:raise(Err)
            end;
        {error, Err} ->
            beamtalk_error:raise(Err)
    end.

%% Both arguments must be Strings. A non-String surfaces a typed error at the
%% `newClass:at:` boundary rather than a deep crash inside the loader.
-spec validate_new_class_args(term(), term()) ->
    {ok, binary(), binary()} | {error, #beamtalk_error{}}.
validate_new_class_args(Source, Path) when is_binary(Source), is_binary(Path) ->
    {ok, Source, Path};
validate_new_class_args(Source, _Path) when not is_binary(Source) ->
    {error, new_class_arg_type_error(<<"source">>, Source)};
validate_new_class_args(_Source, Path) ->
    {error, new_class_arg_type_error(<<"path">>, Path)}.

-spec new_class_arg_type_error(binary(), term()) -> #beamtalk_error{}.
new_class_arg_type_error(ArgName, Value) ->
    TypeName = value_type_name(Value),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'newClass:at:'),
    beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"newClass:at: expects a String ">>, ArgName, <<", got ">>, TypeName
        ])
    ).

-doc """
Move `aClass`'s `.bt` file to `aNewPath` without changing its name (ADR 0114
Phase 2, BT-3272).

Called via `(Erlang beamtalk_workspace_interface_primitives) moveClass:
aClass to: aNewPath`, backing `Workspace moveClass:to:` — a pure filesystem-
organization operation modelled on `newClass:at:` (ADR 0082) the same way
`newClass:at:` itself is: a `Workspace`-level concern, not a class-protocol
message, since the class itself is completely unaffected (no name change, no
reference rewrite — every existing call site still names the class
correctly, because the class's own identity never changes).

Delegates classification, span resolution, and ChangeLog bookkeeping to
`beamtalk_repl_loader:move_class/2` (via `beamtalk_repl_eval:move_class/2`)
— see that function's doc for the full refusal table (`no_source_file` for a
dynamic class, stdlib/dependency refusal mirroring `renameTo:`) and for why
this never needs to reinstall or re-resolve a class object: the class's own
pid never moves, so `aClass` itself is still the correct return value.

Raises a structured `#beamtalk_error{}` for a non-class-object `aClass`, a
non-String `aNewPath`, or any failure `move_class/2` reports. Returns `aClass`
unchanged on success.
""".
-spec moveClass(term(), term()) -> #beamtalk_object{}.
moveClass(ClassArg, NewPath) ->
    case beamtalk_class_registry:is_class_object(ClassArg) of
        false ->
            beamtalk_error:raise(move_class_arg_type_error(<<"a Behaviour for aClass">>, ClassArg));
        true ->
            ok
    end,
    NewPathBin =
        case NewPath of
            P when is_binary(P) ->
                P;
            _ ->
                beamtalk_error:raise(
                    move_class_arg_type_error(<<"a String for aNewPath">>, NewPath)
                )
        end,
    ClassPid = erlang:element(4, ClassArg),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    case beamtalk_repl_eval:move_class(ClassName, NewPathBin) of
        ok -> ClassArg;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

-spec move_class_arg_type_error(binary(), term()) -> #beamtalk_error{}.
move_class_arg_type_error(Expected, Value) ->
    TypeName = value_type_name(Value),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'moveClass:to:'),
    beamtalk_error:with_message(
        Err1,
        iolist_to_binary([
            <<"moveClass:to: expects ">>, Expected, <<", got ">>, TypeName
        ])
    ).

-doc """
Flush every pending durable+flushable Tier 1 ChangeEntry to disk (ADR 0082
Phase 2; Tier 1/2 split per ADR 0113 Phase 2).

Called via `(Erlang beamtalk_workspace_interface_primitives) flush`. Returns a
`FlushResult`-tagged map summarising what was written, what was skipped, and
what conflicted — a pending `'remove-class'` (Tier 2) entry is reported in
`skipped` (`reason => <<"destructive">>`), never applied here; see
`flushIncludingDestructive/0`. Hard runtime errors (e.g. the changelog server
is missing) are raised as structured `#beamtalk_error{}` so they surface at
the call site.
""".
-spec flush() -> map().
flush() ->
    case beamtalk_workspace_flush:flush() of
        {ok, Summary} -> Summary;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

-doc """
Flush only the Tier 1 ChangeEntries that match `Filter` (ADR 0082 Phase 2).

Called via `(Erlang beamtalk_workspace_interface_primitives) flush: filter`.
`Filter` may be a Class, a Symbol (e.g. `#'new-class'`), or a Dictionary
`#{ #file => "..." }` (Symbol-keyed). Anything else surfaces a structured
`#beamtalk_error{}`. Equivalent to `flush(Filter, false)`.
""".
-spec flush(term()) -> map().
flush(Filter) ->
    case beamtalk_workspace_flush:flush(Filter) of
        {ok, Summary} -> Summary;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

-doc """
Flush the ChangeEntries that match `Filter`, additionally applying Tier 2
(`'remove-class'`) entries within that scope when `ConfirmDestructive` is
`true` (ADR 0113 Phase 2).

Called via `(Erlang beamtalk_workspace_interface_primitives) flush: filter
confirmDestructive: confirmDestructive`, backing `Workspace flush: aClass
confirmDestructive: true`. `ConfirmDestructive` must be a literal Boolean —
never read from a workspace setting.
""".
-spec flush(term(), term()) -> map().
flush(Filter, ConfirmDestructive) ->
    case beamtalk_workspace_flush:flush(Filter, ConfirmDestructive) of
        {ok, Summary} -> Summary;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

-doc """
Flush every pending durable+flushable ChangeEntry, Tier 1 *and* Tier 2
(ADR 0113 Phase 2).

Called via `(Erlang beamtalk_workspace_interface_primitives)
flushIncludingDestructive`, backing the unscoped `Workspace
flushIncludingDestructive` bare-unary selector.
""".
-spec flushIncludingDestructive() -> map().
flushIncludingDestructive() ->
    case beamtalk_workspace_flush:flush_including_destructive() of
        {ok, Summary} -> Summary;
        {error, Err} -> beamtalk_error:raise(Err)
    end.

-doc """
Whole-image re-check (ADR 0105 Phase 3, BT-2782).

Called via `(Erlang beamtalk_workspace_interface_primitives) recheckImage`
from `Workspace recheckImage` / the REPL `:recheck image` alias. The
"complete but unbounded" path ADR 0105's Alternatives section keeps out of
the default per-reload trigger: re-checks every live class the workspace
has a recorded source for, unlike the automatic post-reload check (which
only re-checks the xref-filtered dependents of one changed selector).
Returns a Dictionary: `#findings` (a List of finding Dictionaries —
`#owner`, `#severity`, `#category`, `#message`, `#start`, `#end`),
`#checked` (classes a re-check round-trip completed for), `#stale`
(distinct classes with at least one finding).
""".
-spec recheckImage() -> map().
recheckImage() ->
    beamtalk_recheck:trigger_image().

-doc """
Revert a single ChangeEntry (ADR 0082 Phase 4, BT-2290; `remove-method`/
`remove-class` extension ADR 0113, BT-3208).

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
  - an *add* (`'new-class'`, or a brand-new method) — removes what was added
    (BT-2663/BT-2664/BT-2665);
  - a `'remove-class'` removal — recompiles and reinstalls the whole class
    from `prev_source_ref`, reusing the `newClass:at:` install path (ADR 0113,
    BT-3208).

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
structured result instead of raising (ADR 0082 Phase 5, BT-2293).

Side-agnostic: falls back to `find_revert_target/3`'s highest-seq selection
across both sides, same as passing `undefined` to `revert_method/3`. Kept for
callers with no side information (and for backward compatibility — this was
the original LiveView-facing surface); the LiveView `Workspace changes` row
now calls `revert_method/3` with its own `side` instead (ADR 0112, BT-3187),
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
structured result instead of raising (ADR 0082 Phase 5, BT-2293; side
parameter added ADR 0112, BT-3187).

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
    try
        {ok, binary_to_existing_atom(SelectorBin, utf8)}
    catch
        error:badarg -> error
    end.

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
    %% ADR 0112 (BT-3187) required fix: a same-selector instance-side entry and
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
            %% resolve the class's new-class entry and remove the class (BT-2664).
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
%% shape. Shared by both revert entry points (ADR 0112, BT-3187): a
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

%% `TargetSide` (ADR 0112, BT-3187) narrows `find_revert_target/3`'s candidate
%% search to entries on that side, so a same-selector instance-side entry and
%% class-side entry are not ambiguous by `(class, selector)` alone — see
%% `extract_revert_target_from_map/1`'s doc. Pass `undefined` (from a caller
%% with no side information, e.g. `revert_method/2`) to fall back to the
%% side-agnostic, highest-seq selection.
-spec do_revert(binary(), atom(), instance | class | undefined) -> term().
do_revert(ClassNameBin, SelectorAtom, TargetSide) ->
    case beamtalk_workspace_changelog:find_revert_target(ClassNameBin, SelectorAtom, TargetSide) of
        {ok, PrevBody, Entry} ->
            %% A *modify* revert: re-install the recorded prior body on the
            %% entry's side. Instance and class side are both supported
            %% (BT-2665). This also covers reverting a `'remove-method'` entry
            %% (ADR 0112, BT-3187) — when its `prev_source_ref` is set (the
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
            %% by removing the method (BT-2663/BT-2665) or the new class
            %% (BT-2664). The kind tells us which.
            revert_removal(ClassNameBin, SelectorAtom, Entry);
        {reinstall_class, PrevBody, Entry} ->
            %% A `'remove-class'` revert (ADR 0113, BT-3208): the pre-removal
            %% state was "this class existed", so undo the removal by
            %% recompiling and reinstalling the whole class from its recorded
            %% prior source — a class-level target, not a single-method patch.
            reinstall_reverted_class(ClassNameBin, PrevBody, Entry);
        {revert_rename, Entry} ->
            %% A `'rename-class'`/`'rename-method'` revert (ADR 0114,
            %% BT-3274): a multi-site target, not a single prior body — undo
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

%% Resolve a ChangeEntry's revert install/remove side (ADR 0112, BT-3187's
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
%% the class (BT-2664); a method entry removes that method on its side
%% (BT-2663/BT-2665). The original add entry stays in the audit log; the removal
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
Undo a `'remove-class'` entry (ADR 0113, BT-3208): recompile and reinstall the
whole class from `PrevBody` (the entry's recorded `prev_source_ref`), reusing
the same `newClass:at:` install chokepoint `new-class` revert already uses
(ADR 0082, BT-2664) rather than a second whole-class-install mechanism.

`Entry`'s `sourceFile` — the removed class's own recorded absolute path — is
the reinstall target. A class removed with no recorded source file (a
dynamically-defined class with nothing on disk to begin with, `flushable:
false, not_flushable_reason: "dynamic"`) has nothing to reinstall *from*; this
is a loud structured error rather than a silent no-op, matching this module's
error contract elsewhere (never guess, never drop a revert silently).

Before reinstalling, `check_no_external_drift/3` compares the file currently
on disk at `SourceFile` against `PrevBody` (BT-3213, Claude review follow-up
on BT-3208): a still-*pending* `'remove-class'` entry never touches disk
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
    case beamtalk_repl_eval:revert_remove_class(PrevBody, SourceFile) of
        {ok, _Objects} ->
            %% Mirrors `revert_removal/3`'s "undoing an add emits no new
            %% entry" symmetry: undoing this removal doesn't emit one
            %% either (`new_class_install/8`'s `no_log` mode), so the
            %% original `'remove-class'` entry must be retired here —
            %% otherwise it stays active/pending forever even though
            %% its effect has been undone (ADR 0113, BT-3208 review
            %% fix: a stale `'remove-class'` entry would misreport
            %% `skipped: destructive`/block a real future removal).
            retire_reverted_remove_class_entry(ClassNameBin, Entry);
        {error, Reason} ->
            beamtalk_error:raise(ensure_revert_error(Reason, ClassNameBin, undefined))
    end.

-doc """
Drift check for a pending `'remove-class'` entry's reinstall (ADR 0113,
BT-3213 — Claude review follow-up on BT-3208, "revert: of a pending
remove-class entry can silently discard a concurrent out-of-band edit").

Reads `SourceFile` off disk and compares it byte-for-byte against `PrevBody`
(the entry's recorded whole-file `prev_source_ref` snapshot), the same
comparison mechanism `beamtalk_workspace_flush:splice_with_prev/5` uses to
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
    %% `binary_to_list/1` here mirrors `beamtalk_workspace_flush:prepare_splice/2`'s
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
Undo a `'rename-class'`/`'rename-method'` entry (ADR 0114, BT-3274):
delegates the actual multi-site reverse-splice to `beamtalk_repl_eval:
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
    case beamtalk_repl_eval:revert_rename_sites(Entry) of
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
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> Atom
    catch
        error:badarg -> undefined
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

-doc """
Discard every pending ChangeLog entry without writing to disk
(ADR 0082 Phase 4, BT-2290).

Called via `(Erlang beamtalk_workspace_interface_primitives) changeLogClear`
from `ChangeLog>>clear`. Drops every entry from the in-memory active view and
truncates the on-disk metadata segment (audit history is therefore reset).
Memory still holds the latest patched method versions until the next workspace
restart, when disk wins — matching the ADR contract:

  > `Workspace changes clear` discards the ChangeLog without writing. Memory
  > still holds the latest patched versions until the next workspace restart,
  > when disk wins.

Returns `nil` (Beamtalk's unit value). Idempotent: clearing an empty log is a
successful no-op.
""".
-spec changeLogClear() -> nil.
changeLogClear() ->
    ok = beamtalk_workspace_changelog:clear(),
    nil.

-doc """
Flush only the Tier 1 ChangeEntries whose kind or author_kind is in `KindsSet`
(ADR 0082 Phase 4, BT-2290).

Called via `(Erlang beamtalk_workspace_interface_primitives) changeLogFlushKinds: aSet`
from `ChangeLog>>flushKinds:`. `KindsSet` is a Beamtalk `Set` (tagged map)
or a List of Symbols. Accepted symbols:

  - entry kinds: `#instance`, `#class`, `#'new-class'`, `#'remove-method'`,
    `#'remove-class'`
  - author kinds: `#human`, `#agent`

Returns the same `FlushResult` summary as `Workspace flush`. Equivalent to
`changeLogFlushKinds(KindsSet, false)` — a matching Tier 2 entry
(`#'remove-class'`) is reported in `skipped`, not applied; see
`changeLogFlushKinds/2`.
""".
-spec changeLogFlushKinds(term()) -> map().
changeLogFlushKinds(KindsSet) ->
    changeLogFlushKinds(KindsSet, false).

-doc """
Flush the ChangeEntries whose kind or author_kind is in `KindsSet`,
additionally applying Tier 2 (`#'remove-class'`) entries within that scope
when `ConfirmDestructive` is `true` (ADR 0113 Phase 2).

Called via `(Erlang beamtalk_workspace_interface_primitives) changeLogFlushKinds:
aSet confirmDestructive: confirmDestructive` from `ChangeLog>>flushKinds:confirmDestructive:`,
backing `Workspace changes flushKinds: aSet confirmDestructive: true`.
""".
-spec changeLogFlushKinds(term(), term()) -> map().
changeLogFlushKinds(KindsSet, ConfirmDestructive) ->
    case kinds_to_list(KindsSet) of
        {ok, Kinds} ->
            case beamtalk_workspace_flush:flush_kinds(Kinds, ConfirmDestructive) of
                {ok, Summary} -> Summary;
                {error, Err} -> beamtalk_error:raise(Err)
            end;
        {error, Err} ->
            beamtalk_error:raise(Err)
    end.

%% Accept a Beamtalk Set (tagged map) or a List of atoms. Anything else
%% surfaces a type error rather than being silently coerced — a typo at the
%% callsite (`#agent` written as `agent`, a non-symbol element) should fail
%% loudly. An empty list flows through to `flush_kinds/1`, which rejects it
%% with the "use Workspace flush to flush everything" message.
-spec kinds_to_list(term()) -> {ok, [atom()]} | {error, #beamtalk_error{}}.
kinds_to_list(#{'$beamtalk_class' := 'Set', elements := Elements}) when is_list(Elements) ->
    {ok, Elements};
kinds_to_list(List) when is_list(List) ->
    {ok, List};
kinds_to_list(_Other) ->
    {error,
        filter_error_for(
            'flushKinds:',
            <<"flushKinds: expects a Set or List of kind Symbols">>
        )}.

-spec filter_error_for(atom(), binary()) -> #beamtalk_error{}.
filter_error_for(Selector, Message) ->
    Err0 = beamtalk_error:new(type_error, 'ChangeLog'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:with_message(Err1, Message).

-doc """
Read the `autoflush` workspace setting (ADR 0082 Phase 4, BT-2290).

Default is `false`. When `true`, every successful durable in-memory patch
triggers `Workspace flush` synchronously after the install. Called via
`(Erlang beamtalk_workspace_interface_primitives) autoflush`.
""".
-spec autoflush() -> boolean().
autoflush() ->
    beamtalk_workspace_meta:get_setting(autoflush, false).

-doc """
Set the `autoflush` workspace setting (ADR 0082 Phase 4, BT-2290).

`Value` must be a Boolean. Setting `true` enables auto-flush after every
successful durable in-memory patch; `false` reverts to the explicit-flush
default. Persists to `metadata.json` so the setting survives workspace
restart. Called via
`(Erlang beamtalk_workspace_interface_primitives) autoflush: aBoolean`.

Returns the new value so the caller sees the effective state.
""".
-spec setAutoflush(term()) -> boolean().
setAutoflush(Value) when is_boolean(Value) ->
    ok = beamtalk_workspace_meta:set_setting(autoflush, Value),
    Value;
setAutoflush(Other) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'autoflush:'),
    Msg = iolist_to_binary(
        io_lib:format("autoflush: expects a Boolean, got: ~p", [Other])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Msg)).

-doc """
Return the full workspace namespace snapshot.
Called via `(Erlang beamtalk_workspace_interface_primitives) globals`.
""".
-spec globals() -> map().
globals() ->
    UserBindings = all_user_bindings(),
    handle_globals(UserBindings).

-doc """
Register a value in the workspace namespace under a given atom name.
Called via `(Erlang beamtalk_workspace_interface_primitives) bind: value as: name`.
""".
-spec bind(term(), As :: atom() | term()) -> 'nil'.
bind(Value, Name) ->
    ensure_bindings_table(),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case check_bind_conflicts(AtomName) of
                ok ->
                    ets:insert(?WI_BINDINGS_TABLE, {AtomName, Value}),
                    spawn(fun() -> maybe_warn_loaded_class(AtomName) end),
                    nil;
                {error, Err} ->
                    beamtalk_error:raise(Err)
            end
    end.

-doc """
Remove a registered name from the workspace namespace.
Called via `(Erlang beamtalk_workspace_interface_primitives) unbind: name`.
""".
-spec unbind(atom() | term()) -> 'nil'.
unbind(Name) ->
    ensure_bindings_table(),
    case to_atom_name(Name) of
        {error, Err} ->
            beamtalk_error:raise(Err);
        AtomName ->
            case ets:lookup(?WI_BINDINGS_TABLE, AtomName) of
                [] ->
                    Err0 = beamtalk_error:new(name_not_found, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'unbind:'),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Err1,
                            iolist_to_binary([
                                atom_to_binary(AtomName, utf8),
                                <<" is not a registered workspace name">>
                            ])
                        )
                    );
                _ ->
                    ets:delete(?WI_BINDINGS_TABLE, AtomName),
                    nil
            end
    end.

-doc """
Return the OTP application root supervisor, or nil (BT-1191).

Called via `(Erlang beamtalk_workspace_interface_primitives) rootSupervisor`.
Delegates to `beamtalk_supervisor:get_root/0` which reads from the ETS
registry populated by the generated `beamtalk_{appname}_app:start/2`.

Returns the `{beamtalk_supervisor, ClassName, Module, Pid}` tuple when an
OTP application with `[application] supervisor` has started, or the atom
`nil` if no root has been registered.
""".
-spec rootSupervisor() -> tuple() | nil.
rootSupervisor() ->
    beamtalk_supervisor:get_root().

%%% ============================================================================
%%% Session navigation (ADR 0081 Phases 5 & 7, BT-2368)
%%% ============================================================================

-doc """
Return the calling session as a `Session` value, or `nil` outside a REPL eval.

Called via `(Erlang beamtalk_workspace_interface_primitives) currentSession`,
backing `Workspace currentSession`. Delegates to `beamtalk_session_primitives:current/0`
so it returns the *identical* value to `Session current` (the same minted
`Session` carrying the calling session's id and shell PID, or `nil` when no
session context is seeded — e.g. compiled program code). There is no `hasSession`
predicate: callers guard with `Workspace currentSession isNil` /
`Session current ifNotNil: [:s | ...]`.
""".
-spec currentSession() -> beamtalk_session_primitives:session() | nil.
currentSession() ->
    beamtalk_session_primitives:current().

-doc """
Return a `List` of `Session` values, one per live shell, backing
`Workspace sessions`.

Called via `(Erlang beamtalk_workspace_interface_primitives) sessions`. Delegates
to `beamtalk_session_primitives:liveSessions/0`, which enumerates live shells and
mints each `Session` value the same way `withId/1` does, so the values work with
the instance reads (`s id`, `s bindings keys`) and reject cross-session writes.
""".
-spec sessions() -> [beamtalk_session_primitives:session()].
sessions() ->
    beamtalk_session_primitives:liveSessions().

%%% ============================================================================
%%% Supervisor lifecycle management (BT-1341)
%%% ============================================================================

-doc """
Start and attach a user supervisor to the workspace supervision tree.

Called via `(Erlang beamtalk_workspace_interface_primitives) startSupervisor: MySup`.
The class must be a Supervisor or DynamicSupervisor subclass. The supervisor
is started as a dynamic child of `beamtalk_workspace_sup` with `temporary`
restart (user supervisors are not auto-restarted — the user re-attaches
explicitly during iterative development).

If the supervisor is already running under the workspace tree, returns the
existing instance (idempotent).
""".
-spec startSupervisor(term()) -> term().
startSupervisor(ClassArg) ->
    case beamtalk_class_registry:is_class_object(ClassArg) of
        false ->
            raise_start_supervisor_type_error(<<"Expected a Supervisor class">>);
        true ->
            ok
    end,
    ClassPid = element(4, ClassArg),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    Module = beamtalk_object_class:module_name_safe(ClassPid),
    case beamtalk_supervisor:is_supervisor(ClassName) of
        false ->
            NameBin = atom_to_binary(ClassName, utf8),
            raise_start_supervisor_type_error(
                iolist_to_binary([NameBin, <<" is not a Supervisor or DynamicSupervisor subclass">>])
            );
        true ->
            ok
    end,
    do_start_supervisor(ClassName, Module).

do_start_supervisor(ClassName, Module) ->
    ChildId = {user_supervisor, ClassName},
    ChildSpec = #{
        id => ChildId,
        start => {Module, start_link, []},
        restart => temporary,
        shutdown => infinity,
        type => supervisor,
        modules => [Module]
    },
    case supervisor:start_child(beamtalk_workspace_sup, ChildSpec) of
        {ok, Pid} ->
            {beamtalk_supervisor, ClassName, Module, Pid};
        {error, {already_started, _Pid}} ->
            %% Process is already running. If the child spec is registered
            %% under our workspace supervisor (same ChildId), this is an
            %% idempotent call — return the actual handle from which_children
            %% (not the caller's Module, which may be stale after a reload).
            %% Otherwise, the supervisor was started standalone.
            case workspace_child_handle(ChildId) of
                {ok, Handle} ->
                    Handle;
                not_found ->
                    NameBin = atom_to_binary(ClassName, utf8),
                    raise_start_supervisor_error(
                        iolist_to_binary([
                            NameBin,
                            <<" is already running; stop it first, then attach to the workspace">>
                        ])
                    )
            end;
        {error, already_present} ->
            %% Child spec exists but process is not running (was previously stopped).
            %% Delete the stale spec and re-add to support iterative development.
            _ = supervisor:delete_child(beamtalk_workspace_sup, ChildId),
            case supervisor:start_child(beamtalk_workspace_sup, ChildSpec) of
                {ok, Pid} ->
                    {beamtalk_supervisor, ClassName, Module, Pid};
                {error, Reason} ->
                    raise_start_supervisor_error(Reason)
            end;
        {error, Reason} ->
            raise_start_supervisor_error(Reason)
    end.

-doc """
Return the actual attached handle for a workspace child.
Reads from which_children to get the real module and pid (which may differ
from the caller's ClassArg after a class reload).
""".
-spec workspace_child_handle(term()) -> {ok, tuple()} | not_found.
workspace_child_handle(ChildId = {user_supervisor, ClassName}) ->
    case lists:keyfind(ChildId, 1, supervisor:which_children(beamtalk_workspace_sup)) of
        {ChildId, Pid, supervisor, [RunningModule]} when is_pid(Pid) ->
            {ok, {beamtalk_supervisor, ClassName, RunningModule, Pid}};
        _ ->
            not_found
    end.

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_start_supervisor_type_error/1}).
raise_start_supervisor_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message)).

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_start_supervisor_error/1}).
raise_start_supervisor_error(Message) when is_binary(Message) ->
    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message));
raise_start_supervisor_error(Reason) ->
    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'startSupervisor:'),
    beamtalk_error:raise(
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary(io_lib:format("Failed to start supervisor: ~p", [Reason]))
        )
    ).

-doc """
Stop and remove a supervisor from the workspace.

Called via `(Erlang beamtalk_workspace_interface_primitives) stopSupervisor: MySup`.
Works for both workspace-attached supervisors and the root application
supervisor. Cleanly shuts down the supervisor and all its children.
""".
-spec stopSupervisor(term()) -> nil.
stopSupervisor(ClassArg) ->
    case beamtalk_class_registry:is_class_object(ClassArg) of
        false ->
            raise_stop_supervisor_type_error(<<"Expected a Supervisor class">>);
        true ->
            ok
    end,
    ClassPid = element(4, ClassArg),
    ClassName = beamtalk_object_class:class_name(ClassPid),
    case beamtalk_supervisor:is_supervisor(ClassName) of
        false ->
            raise_stop_supervisor_type_error(
                iolist_to_binary([
                    atom_to_binary(ClassName, utf8),
                    <<" is not a Supervisor or DynamicSupervisor subclass">>
                ])
            );
        true ->
            ok
    end,
    do_stop_supervisor(ClassName).

do_stop_supervisor(ClassName) ->
    ChildId = {user_supervisor, ClassName},
    case supervisor:terminate_child(beamtalk_workspace_sup, ChildId) of
        ok ->
            _ = supervisor:delete_child(beamtalk_workspace_sup, ChildId),
            nil;
        {error, not_found} ->
            %% Not a workspace-attached supervisor — check if it's the root
            %% application supervisor. If so, stop it and clear the ETS entry.
            case beamtalk_supervisor:get_root() of
                {beamtalk_supervisor, ClassName, _Module, Pid} ->
                    try
                        gen_server:stop(Pid)
                    catch
                        exit:{noproc, _} -> ok
                    end,
                    beamtalk_supervisor:clear_root(),
                    nil;
                _ ->
                    NameBin = atom_to_binary(ClassName, utf8),
                    Err0 = beamtalk_error:new(runtime_error, 'WorkspaceInterface'),
                    Err1 = beamtalk_error:with_selector(Err0, 'stopSupervisor:'),
                    beamtalk_error:raise(
                        beamtalk_error:with_message(
                            Err1,
                            iolist_to_binary([NameBin, <<" is not attached to the workspace">>])
                        )
                    )
            end
    end.

% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({no_return, raise_stop_supervisor_type_error/1}).
raise_stop_supervisor_type_error(Message) ->
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'stopSupervisor:'),
    beamtalk_error:raise(beamtalk_error:with_message(Err1, Message)).

-doc """
List all supervisors in the workspace supervision tree.

Called via `(Erlang beamtalk_workspace_interface_primitives) supervisors`.
Returns a list of `{beamtalk_supervisor, ClassName, Module, Pid}` tuples
including the root application supervisor (if registered) and all
supervisors attached via `startSupervisor:`.
""".
-spec supervisors() -> [tuple()].
supervisors() ->
    Root =
        case beamtalk_supervisor:get_root() of
            nil -> [];
            RootSup -> [RootSup]
        end,
    Children = supervisor:which_children(beamtalk_workspace_sup),
    UserSups = lists:filtermap(
        fun
            ({{user_supervisor, ClassName}, Pid, supervisor, [Module]}) when is_pid(Pid) ->
                {true, {beamtalk_supervisor, ClassName, Module, Pid}};
            (_) ->
                false
        end,
        Children
    ),
    Root ++ UserSups.

-doc """
Return the direct dependencies of the current workspace package.

Called via `(Erlang beamtalk_workspace_interface_primitives) dependencies`.
Returns a map of `#{PackageName => PackageInfoMap}` for each direct dependency
of the current workspace package. Returns an empty map if the workspace
has no package name or no dependencies.
""".
-spec dependencies() -> #{binary() => map()}.
dependencies() ->
    case beamtalk_workspace_meta:get_package_name() of
        undefined ->
            #{};
        PkgName ->
            DepNames = beamtalk_package:dependencies(PkgName),
            maps:from_list(
                lists:filtermap(
                    fun(DepName) ->
                        try
                            Info = beamtalk_package:named(DepName),
                            {true, {DepName, Info}}
                        catch
                            error:_ -> false
                        end
                    end,
                    DepNames
                )
            )
    end.

%%% ============================================================================
%%% Project sync (BT-1723)
%%% ============================================================================

-doc """
Perform an incremental project sync from the current working directory.

Called via `(Erlang beamtalk_workspace_interface_primitives) sync`.
Delegates to `beamtalk_repl_ops_load:sync_project/2` which handles
incremental compilation (mtime tracking, native .erl files, dependency
ordering) and returns a result map.

Returns a Dictionary with keys:
  - `#summary` — human-readable summary (e.g. "Reloaded 2 of 5 files (3 unchanged)")
  - `#classes` — List of loaded class name Strings
  - `#errors` — List of error Dictionaries (empty on success)
  - `#changedCount` — number of files reloaded
  - `#unchangedCount` — number of unchanged files
  - `#deletedCount` — number of deleted files
""".
-spec sync() -> map().
sync() ->
    case beamtalk_repl_ops_load:sync_project(".", #{}) of
        {ok, Result} ->
            #{
                summary => maps:get(summary, Result),
                classes => maps:get(classes, Result),
                errors => maps:get(errors, Result),
                changedCount => maps:get(changed_count, Result),
                unchangedCount => maps:get(unchanged_count, Result),
                deletedCount => maps:get(deleted_count, Result)
            };
        {error, Err} ->
            beamtalk_error:raise(Err)
    end.

%%% ============================================================================
%%% Stable external API
%%% ============================================================================

-doc """
Return workspace-level user bindings (for REPL session injection).
Called before each eval to merge workspace bindings into session bindings.
Returns #{} when the workspace meta process is not running (workspace down).
""".
-spec get_user_bindings() -> #{atom() => term()}.
get_user_bindings() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            #{};
        _ ->
            all_user_bindings()
    end.

-doc """
Return non-class workspace globals for session binding injection.
Includes singletons (Transcript, Beamtalk, Workspace) and user-registered
bind:as: names. Class objects are excluded.
""".
-spec get_session_bindings() -> #{atom() => term()}.
get_session_bindings() ->
    case whereis(beamtalk_workspace_meta) of
        undefined ->
            #{};
        _ ->
            UserBindings = all_user_bindings(),
            handle_session_bindings(UserBindings)
    end.

-doc """
Resolve a bare name against the session locals and the live workspace sources.

BT-2365 (ADR 0081 Phase 1): the single shared resolver. Replaces eager workspace
injection — instead of copying globals into each session at init, a free
identifier is resolved lazily, in order:

1. session locals map (`Locals`) — checked first so a session local **shadows**
   a workspace global of the same name (e.g. `x := 5` then `x` resolves to the
   local even if `x` is also a `bind:as:` entry);
2. `bind:as:` registry (the workspace user-bindings ETS table);
3. singleton registry (`Transcript`/`Beamtalk`/`Workspace`, resolved live from
   their class instances via `beamtalk_workspace_config:singletons/0` +
   `value_singletons/0`);
4. class registry (`Counter`, `Integer`, `Session`, …) → a class object;
5. otherwise raise `undefined_variable`.

Used by **both** the REPL codegen free-identifier fallthrough and (later)
`Session resolve:`, so the two cannot drift. `Locals` is the eval-time
bindings map, which carries internal keys (the `__`-prefixed convention, e.g.
`__workspace_user_bindings__`); these never collide with a resolved name
because they are not valid source identifiers, so callers never look them up
here.
""".
-spec resolve_name(map(), atom()) -> term().
resolve_name(Locals, Name) when is_map(Locals), is_atom(Name) ->
    case maps:find(Name, Locals) of
        {ok, Value} ->
            Value;
        error ->
            resolve_workspace_name(Name)
    end.

%% Tiers 2–5 of resolve_name/2: the live workspace sources, checked after locals.
-spec resolve_workspace_name(atom()) -> term().
resolve_workspace_name(Name) ->
    case lookup_user_binding(Name) of
        {ok, BindValue} ->
            BindValue;
        error ->
            case lookup_singleton(Name) of
                {ok, SingletonValue} ->
                    SingletonValue;
                error ->
                    case lookup_class_object(Name) of
                        {ok, ClassObj} ->
                            ClassObj;
                        error ->
                            raise_undefined_variable(Name)
                    end
            end
    end.

%% Tier 2: bind:as: registry (workspace user bindings ETS).
-spec lookup_user_binding(atom()) -> {ok, term()} | error.
lookup_user_binding(Name) ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            error;
        _ ->
            case ets:lookup(?WI_BINDINGS_TABLE, Name) of
                [{Name, Value}] -> {ok, Value};
                [] -> error
            end
    end.

%% Tier 3: singleton registry (Transcript/Beamtalk/Workspace), resolved live.
%%
%% Delegates to handle_session_bindings/1 — the same builder the eager-injection
%% path used — so a singleton resolves to exactly the value it would have had if
%% injected (including the `Workspace` tagged-map fallback when its class var is
%% not yet wired). Only the three singleton binding names ever match here; any
%% other name returns `error` so resolution falls through to the class registry.
-spec lookup_singleton(atom()) -> {ok, term()} | error.
lookup_singleton(Name) ->
    case is_singleton_binding_name(Name) of
        false ->
            error;
        true ->
            Singletons = handle_session_bindings(#{}),
            case maps:find(Name, Singletons) of
                {ok, Value} -> {ok, Value};
                error -> error
            end
    end.

%% True iff Name is one of the configured singleton binding names
%% (Transcript / Beamtalk / Workspace).
-spec is_singleton_binding_name(atom()) -> boolean().
is_singleton_binding_name(Name) ->
    Configs =
        beamtalk_workspace_config:singletons() ++
            beamtalk_workspace_config:value_singletons(),
    lists:any(fun(#{binding_name := BName}) -> BName =:= Name end, Configs).

%% Tier 4: class registry → a class object tuple, mirroring REPL codegen for a
%% capitalised name (`{beamtalk_object, '<Name> class', Module, ClassPid}`).
%%
%% Caveat: reachable from inside an ADR 0109 foreign-process block (directly,
%% or via the unrestricted `Erlang <module>` FFI gateway), so the module is
%% resolved via `beamtalk_runtime_api:module_name/1`, which delegates to
%% `beamtalk_object_class:module_name_safe/1` rather than an unsafe
%% `gen_server:call`-based precheck. That means the catch-all below cannot
%% detect a class process killed via an untrappable `kill` signal before
%% `terminate/2` ran — see `module_name_safe/1`'s doc for the full
%% explanation.
-spec lookup_class_object(atom()) -> {ok, tuple()} | error.
lookup_class_object(Name) ->
    case beamtalk_runtime_api:whereis_class(Name) of
        undefined ->
            error;
        ClassPid ->
            try beamtalk_runtime_api:module_name(ClassPid) of
                Module ->
                    Tag = beamtalk_runtime_api:class_object_tag(Name),
                    {ok, {beamtalk_object, Tag, Module, ClassPid}}
            catch
                %% Class died between whereis and module_name — treat as unknown.
                _:_ -> error
            end
    end.

%% Tier 5: genuinely unknown name — raise undefined_variable (same error the
%% REPL surfaces today for an unbound identifier).
-spec raise_undefined_variable(atom()) -> no_return().
raise_undefined_variable(Name) ->
    beamtalk_error:raise(beamtalk_repl_errors:ensure_structured_error({undefined_variable, Name})).

-doc """
Resolve a capitalised class reference whose name is not a session local.

BT-2365 (ADR 0081 Phase 1): the REPL codegen for a `ClassReference` checks the
session locals map first (so a session local of the same name takes precedence)
and, on a miss, calls this. Reuses the same singleton + class-registry tiers as
`resolve_name/2` so resolution cannot drift, but raises `class_not_found`
(not `undefined_variable`) for a genuinely unknown class — preserving the
existing "Class 'X' not found" REPL error.

(Note: a capitalised name parses as a `ClassReference`, not an assignment
target, so it cannot itself be rebound via `:=`; the locals check still runs
for symmetry with `resolve_name/2` and is essentially always a miss here.)

`Locals` is accepted for symmetry with `resolve_name/2` and to allow a future
caller to thread the session map; the codegen has already excluded a local hit
before reaching here.
""".
-spec resolve_class_reference(map(), atom()) -> term().
resolve_class_reference(_Locals, Name) when is_atom(Name) ->
    case lookup_singleton(Name) of
        {ok, SingletonValue} ->
            SingletonValue;
        error ->
            case lookup_class_object(Name) of
                {ok, ClassObj} ->
                    ClassObj;
                error ->
                    raise_class_not_found(Name)
            end
    end.

-doc """
Resolve a singleton binding name to its live instance, or `error`.

BT-2365 (ADR 0081 Phase 1): used by the REPL codegen's binding-aware class-send
fallback. When a message is sent to a singleton receiver (`Workspace bind:as:`,
`Transcript show:`) the name is no longer eagerly injected into the session map,
so the `maps:find` receiver lookup misses. This recovers the live instance so the
message dispatches to it (via `beamtalk_message_dispatch:send`) instead of being
mis-routed to a non-existent class. Returns `error` for any non-singleton name so
real class names still fall through to class-method dispatch.
""".
-spec resolve_singleton_instance(atom()) -> {ok, term()} | error.
resolve_singleton_instance(Name) when is_atom(Name) ->
    lookup_singleton(Name).

-spec raise_class_not_found(atom()) -> no_return().
raise_class_not_found(Name) ->
    Err0 = beamtalk_error:new(class_not_found, Name),
    Hint = iolist_to_binary([
        <<"Define ">>,
        atom_to_binary(Name, utf8),
        <<" with: Object subclass: ">>,
        atom_to_binary(Name, utf8)
    ]),
    beamtalk_error:raise(beamtalk_error:with_hint(Err0, Hint)).

%%% ============================================================================
%%% Internal helpers — ETS management
%%% ============================================================================

-doc """
Create the bindings ETS table owned by the calling process.
Must be called from a long-lived process (beamtalk_workspace_bootstrap) so that
the table is not deleted when short-lived eval worker processes exit.
""".
-spec create_bindings_table() -> ok.
create_bindings_table() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            ets:new(?WI_BINDINGS_TABLE, [set, public, named_table]),
            ok;
        _ ->
            ok
    end.

-doc """
Ensure the bindings ETS table exists.
Creates it on first call; safe to call repeatedly.
""".
-spec ensure_bindings_table() -> ok.
ensure_bindings_table() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            try
                ets:new(?WI_BINDINGS_TABLE, [set, public, named_table])
            catch
                error:badarg ->
                    %% Another process created it concurrently — that's fine
                    ok
            end,
            ok;
        _ ->
            ok
    end.

-doc "Read all user bindings as a map.".
-spec all_user_bindings() -> #{atom() => term()}.
all_user_bindings() ->
    case ets:info(?WI_BINDINGS_TABLE, id) of
        undefined ->
            #{};
        _ ->
            maps:from_list(ets:tab2list(?WI_BINDINGS_TABLE))
    end.

%%% ============================================================================
%%% Internal method implementations
%%% ============================================================================

-doc "Get all live actors as beamtalk_object references.".
-spec handle_actors() -> [tuple()].
handle_actors() ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            [];
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            lists:filtermap(fun wrap_actor/1, Actors)
    end.

-doc "Look up a specific actor by pid string.".
-spec handle_actor_at(binary() | list()) -> tuple() | 'nil'.
handle_actor_at(PidStr) when is_binary(PidStr) ->
    handle_actor_at(binary_to_list(PidStr));
handle_actor_at(PidStr) when is_list(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case whereis(beamtalk_actor_registry) of
            undefined ->
                nil;
            RegistryPid ->
                case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                    {ok, Metadata} ->
                        case wrap_actor(Metadata) of
                            {true, Obj} -> Obj;
                            false -> nil
                        end;
                    {error, not_found} ->
                        nil
                end
        end
    catch
        error:badarg -> nil
    end;
handle_actor_at(_) ->
    nil.

-doc "Return all loaded user classes (those with a source file recorded).".
-spec handle_classes() -> [tuple()].
handle_classes() ->
    beamtalk_runtime_api:user_classes().

-doc """
Load a .bt file, compiling and registering the class.
On success, returns the loaded class object(s) so the REPL displays what was loaded.
""".
-spec handle_load(term()) -> term() | {error, #beamtalk_error{}}.
handle_load(Path) when is_binary(Path) ->
    handle_load(binary_to_list(Path));
handle_load(Path) when is_list(Path) ->
    %% BT-2091: BT-1719 demand-driven native .erl recompilation. Previously
    %% wired into the deprecated `load-file` op handler; mirror the same
    %% pre-step here so `Workspace load: "path"` keeps native FFI working
    %% for package projects with `native/*.erl` sources.
    %%
    %% Native compile failures are surfaced as a structured `#beamtalk_error{}`
    %% so callers see the FFI break at the `load:` boundary rather than a
    %% silent log + downstream "function undefined" at runtime.
    case
        beamtalk_repl_ops_load:maybe_recompile_native_deps(
            Path, beamtalk_repl_ops_load:find_project_root(Path)
        )
    of
        {ok, _Count} ->
            handle_load_after_native(Path);
        {error, NativeErrors} ->
            ?LOG_ERROR(
                "Workspace load: native .erl compilation failed for ~s: ~p",
                [Path, NativeErrors],
                #{domain => [beamtalk, workspace]}
            ),
            Err0 = beamtalk_error:new(native_compile_failed, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([
                    <<"Native .erl compilation failed for ">>,
                    Path
                ])
            ),
            Err3 = beamtalk_error:with_details(Err2, #{path => Path, errors => NativeErrors}),
            {error, Err3}
    end;
handle_load(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'load:'),
    {error,
        beamtalk_error:with_message(
            Err1,
            iolist_to_binary([<<"load: expects a String path, got ">>, TypeName])
        )}.

%% BT-2091: Path post-step extracted so the native-compile error path
%% short-circuits without falling through to reload_class_file/1.
-spec handle_load_after_native(string()) -> term() | {error, #beamtalk_error{}}.
handle_load_after_native(Path) ->
    case beamtalk_repl_eval:reload_class_file(Path) of
        {ok, ClassNames} ->
            %% BT-2091: record class source so subsequent `Class >> selector => body`
            %% method-patch syntax (which depends on workspace_meta:get_class_source/1)
            %% keeps working. The deprecated `load-file` op's session-aware path
            %% recorded sources via store_file_class_sources/3; the stateless
            %% `Workspace load:` path now mirrors that.
            case file:read_file(Path) of
                {ok, SourceBin} ->
                    SourceStr = binary_to_list(SourceBin),
                    %% Defensive match: skip entries that don't follow the
                    %% `#{name := ...}` shape (loaded_class_objects/1 already
                    %% treats those as recoverable; we shouldn't crash on
                    %% drifted reload payloads either).
                    lists:foreach(
                        fun
                            (#{name := Atom}) when is_atom(Atom) ->
                                beamtalk_workspace_meta:set_class_source(
                                    atom_to_binary(Atom, utf8), SourceStr
                                );
                            (#{name := Bin}) when is_binary(Bin) ->
                                beamtalk_workspace_meta:set_class_source(Bin, SourceStr);
                            (#{name := Str}) when is_list(Str) ->
                                beamtalk_workspace_meta:set_class_source(
                                    list_to_binary(Str), SourceStr
                                );
                            (#{name := _Unsupported}) ->
                                ok;
                            (_Other) ->
                                ok
                        end,
                        ClassNames
                    );
                {error, _} ->
                    ok
            end,
            loaded_class_objects(ClassNames);
        {error, {file_not_found, _}} ->
            Err0 = beamtalk_error:new(file_not_found, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'load:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"File not found: ">>, Path])
                )};
        {error, Reason} ->
            %% BT-2091: surface structured compile/semantic errors through `Workspace load:`
            %% so e2e callers see specific error reasons (cannot subclass sealed class,
            %% cannot assign to field, etc.) rather than a generic "Failed to load".
            %% The migration target for the deprecated `load-file` op was already running
            %% through `ensure_structured_error/1`; mirror that here.
            {error, beamtalk_repl_errors:ensure_structured_error(Reason)}
    end.

-doc "Return the full workspace globals snapshot.".
-spec handle_globals(map()) -> map().
handle_globals(UserBindings) ->
    Base = handle_session_bindings(UserBindings),
    Classes = handle_classes(),
    lists:foldl(
        fun
            (#beamtalk_object{class = ClassTag} = ClassObj, Acc) ->
                ClassName = base_class_name(ClassTag),
                Acc#{ClassName => ClassObj};
            (_, Acc) ->
                Acc
        end,
        Base,
        Classes
    ).

-doc "Return non-class session bindings: singletons + user bind:as: entries.".
-spec handle_session_bindings(map()) -> map().
handle_session_bindings(UserBindings) ->
    Base0 = UserBindings,
    Base1 =
        case resolve_singleton('TranscriptStream') of
            nil -> Base0;
            TranscriptObj -> Base0#{'Transcript' => TranscriptObj}
        end,
    Base2 =
        case resolve_singleton('BeamtalkInterface') of
            nil -> Base1;
            BeamtalkObj -> Base1#{'Beamtalk' => BeamtalkObj}
        end,
    %% Resolve Workspace from singleton state, same as Beamtalk/Transcript.
    %% Falls back to a plain tagged-map if the class var hasn't been wired yet.
    WorkspaceObj =
        case resolve_singleton('WorkspaceInterface') of
            nil -> #{'$beamtalk_class' => 'WorkspaceInterface'};
            Obj -> Obj
        end,
    Base2#{'Workspace' => WorkspaceObj}.

-doc "Convert a name argument to an atom.".
-spec to_atom_name(term()) -> atom() | {error, #beamtalk_error{}}.
to_atom_name(Name) when is_atom(Name) -> Name;
to_atom_name(Other) ->
    TypeName = value_type_name(Other),
    Err0 = beamtalk_error:new(type_error, 'WorkspaceInterface'),
    {error,
        beamtalk_error:with_message(
            Err0,
            iolist_to_binary([<<"Expected a Symbol name, got ">>, TypeName])
        )}.

-doc "Check if a name conflicts with Beamtalk system globals.".
-spec check_bind_conflicts(atom()) -> ok | {error, #beamtalk_error{}}.
check_bind_conflicts(AtomName) ->
    case is_protected_name(AtomName) of
        true ->
            Err0 = beamtalk_error:new(name_conflict, 'WorkspaceInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'bind:as:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([
                        atom_to_binary(AtomName, utf8),
                        <<" is a system name and cannot be shadowed">>
                    ])
                )};
        false ->
            ok
    end.

-spec is_protected_name(atom()) -> boolean().
is_protected_name('Transcript') -> true;
is_protected_name('Beamtalk') -> true;
is_protected_name('Workspace') -> true;
is_protected_name(_) -> false.

-doc "Warn if name is an existing loaded class.".
-spec maybe_warn_loaded_class(atom()) -> ok.
maybe_warn_loaded_class(AtomName) ->
    Classes = handle_classes(),
    IsLoadedClass = lists:any(
        fun
            (#beamtalk_object{class = ClassTag}) ->
                base_class_name(ClassTag) =:= AtomName;
            (_) ->
                false
        end,
        Classes
    ),
    case IsLoadedClass of
        true ->
            WarningMsg = iolist_to_binary([
                <<"Warning: ">>,
                atom_to_binary(AtomName, utf8),
                <<" is a loaded class. Use reload instead.">>
            ]),
            ?LOG_WARNING("~s", [WarningMsg], #{domain => [beamtalk, runtime]});
        false ->
            ok
    end.

-doc "Wrap actor metadata into a beamtalk_object tuple.".
-spec wrap_actor(beamtalk_repl_actors:actor_metadata()) -> {true, tuple()} | false.
wrap_actor(#{pid := Pid, class := Class, module := Module}) ->
    case is_process_alive(Pid) of
        true ->
            {true, {beamtalk_object, Class, Module, Pid}};
        false ->
            false
    end.

-doc "Resolve a singleton class instance.".
-spec resolve_singleton(atom()) -> tuple() | 'nil'.
resolve_singleton(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            nil;
        ClassPid ->
            try
                beamtalk_class_dispatch:class_send(ClassPid, current, [])
            catch
                _:_ -> nil
            end
    end.

-doc """
Extract the base class name from a class tag (e.g. 'Counter class' -> 'Counter').
""".
-spec base_class_name(atom()) -> atom().
base_class_name(Tag) ->
    Bin = beamtalk_runtime_api:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> Tag
    end.

-doc """
Resolve loaded ClassNames maps to a Beamtalk List of class objects.
Always returns a List (possibly empty) so callers have a uniform type.
""".
-spec loaded_class_objects([map()]) -> list().
loaded_class_objects(ClassNames) ->
    Objects = lists:filtermap(
        fun
            (#{name := Name}) when is_list(Name) ->
                try list_to_existing_atom(Name) of
                    Atom ->
                        case beamtalk_runtime_api:whereis_class(Atom) of
                            undefined ->
                                ?LOG_WARNING(
                                    "loaded_class_objects: class ~p not found in registry after load",
                                    [Name],
                                    #{domain => [beamtalk, runtime]}
                                ),
                                false;
                            ClassPid ->
                                Mod = beamtalk_runtime_api:module_name(ClassPid),
                                Tag = beamtalk_runtime_api:class_object_tag(Atom),
                                {true, #beamtalk_object{
                                    class = Tag, class_mod = Mod, pid = ClassPid
                                }}
                        end
                catch
                    error:badarg ->
                        ?LOG_WARNING(
                            "loaded_class_objects: class name ~p is not a known atom",
                            [Name],
                            #{domain => [beamtalk, runtime]}
                        ),
                        false
                end;
            (Entry) ->
                ?LOG_WARNING("loaded_class_objects: unexpected entry shape ~p", [Entry], #{
                    domain => [beamtalk, runtime]
                }),
                false
        end,
        ClassNames
    ),
    Objects.

-doc "Return a human-readable type name for an Erlang/Beamtalk value.".
-spec value_type_name(term()) -> binary().
value_type_name(V) when is_integer(V) -> <<"Integer">>;
value_type_name(V) when is_float(V) -> <<"Float">>;
value_type_name(V) when is_boolean(V) -> <<"Boolean">>;
value_type_name(nil) -> <<"nil">>;
value_type_name(V) when is_atom(V) -> <<"Symbol">>;
value_type_name(V) when is_list(V) -> <<"List">>;
value_type_name(V) when is_map(V) -> <<"Dictionary">>;
value_type_name(#beamtalk_object{}) -> <<"Object">>;
value_type_name(_) -> <<"Unknown">>.
