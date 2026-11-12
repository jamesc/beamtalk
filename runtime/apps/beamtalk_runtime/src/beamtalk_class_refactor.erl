%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_refactor).

%%% **DDD Context:** Object System Context

-moduledoc """
Class-level live-refactoring primitives: method removal, class rename and
live method patch.

These back the `Behaviour` refactoring primitives declared in
`stdlib/src/behaviour.bt` and are emitted by name from
`beamtalk-codegen`'s `primitives/behaviour.rs`. They were split out of
`beamtalk_behaviour_intrinsics` (BT-4703), whose identity is class
introspection; the code moved verbatim.

| Erlang function                | Primitive                                              |
|--------------------------------|--------------------------------------------------------|
| classRemoveSelector/2          | `removeSelector:` (ADR 0112 Phase 2)                   |
| classRemoveSelectorIfAbsent/3  | `removeSelector:ifAbsent:` (ADR 0112 Phase 2)          |
| classRenameTo/2                | `renameTo:` (ADR 0114 Phase 2)                         |
| install_class_rename/3         | identity-move-then-retire step of `renameTo:`; also called by `beamtalk_repl_loader` for the `'rename-class'` revert (ADR 0114 Phase 4) |
| classCompileSource/3           | `compile:source:` (ADR 0082 Phase 1)                   |
| classTryCompileSource/3        | `tryCompile:source:` (ADR 0082 Phase 1)                |
| classPrecheckCompileSource/3   | `precheckCompile:source:` (ADR 0105 Phase 3)           |

## Dependencies

* `beamtalk_refactor_support` — reentrancy/capability guards and the
  removal change-log helpers shared with `beamtalk_behaviour_intrinsics`
  and `beamtalk_selector_refactor`.
* **Workspace layer (dynamic).** The runtime application cannot depend on
  `beamtalk_workspace` or `beamtalk_compiler` at compile time (they sit
  above it), so every call into them goes through `erlang:apply/3` and is
  invisible to `rebar3 xref`. Here these are `beamtalk_repl_eval`
  (`rewrite_sites/2`, `validate_sites/2`, `emit_rewrite_change_entry/2`,
  `remove_method/4`, `emit_remove_change_entry/5`,
  `emit_extension_remove_change_entry/7`, `compile_method/N`,
  `precheck_method/N`), `beamtalk_repl_loader:class_source_file/1`,
  `beamtalk_workspace_meta:get_class_source/1` and
  `beamtalk_compiler:resolve_class_span/2` / `resolve_method_span/4`. The
  same indirection is used by `beamtalk_selector_refactor` and
  `beamtalk_refactor_support`.

## Exports used by `beamtalk_selector_refactor`

`class_source_file_for/1`, `current_class_source/1`, `method_token_span/4`,
`refuse_if_protocol_provided/4`, `removal_target/1`, `rewrite_class_sites/5`
and `selector_not_found_error/2` are exported only because method rename
(`beamtalk_selector_refactor`) reuses the class-rename site-rewrite pipeline
and the removal target/refusal helpers; they are not part of any
Beamtalk-visible surface.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    classRemoveSelector/2,
    classRemoveSelectorIfAbsent/3,
    classRenameTo/2,
    %% ADR 0114 Phase 4: `Workspace changes revert:`'s `'rename-class'` undo needs the
    %% SAME identity-move-then-retire sequence `classRenameTo/2`'s own forward path
    %% uses (`beamtalk_repl_loader:finish_rename_class_revert/1` calls this directly;
    %% `beamtalk_workspace` depends on `beamtalk_runtime` at compile time, so no
    %% `erlang:apply/3` indirection is needed) rather than duplicating its
    %% whereis/stop/purge sequence.
    install_class_rename/3,
    classCompileSource/3,
    classTryCompileSource/3,
    classPrecheckCompileSource/3,
    %% Shared with beamtalk_selector_refactor (see moduledoc).
    class_source_file_for/1,
    current_class_source/1,
    method_token_span/4,
    refuse_if_protocol_provided/4,
    removal_target/1,
    rewrite_class_sites/5,
    selector_not_found_error/2
]).

-ifdef(TEST).
-export([rewrite_after_identity_move/4]).
-endif.

-doc """
Remove `Selector` from the receiver, raising if it is not defined locally or
as an extension.

Backs `@primitive "classRemoveSelector"` (`Behaviour>>removeSelector:`). Side
follows `classIncludesSelector/2`'s own convention: a `Self` tagged
`class = 'Metaclass'` (i.e. `Counter class removeSelector: #foo`) touches the
class-side method table; any other `Self` (`Counter removeSelector: #foo`)
touches the instance-side table.

Resolution mirrors `beamtalk_dispatch:lookup/5`'s own order (ADR 0112 §
Extension methods): the extension registry (ADR 0066 open classes) is
checked first — an extension shadows a same-named local method, so removing
it re-exposes the local one (a second `removeSelector:` call removes that
too) — then the local method table. Removing a locally-defined override
re-exposes whatever the superclass chain supplies with no restart needed
(ADR 0032's chain-walk dispatch does the work; nothing here invalidates a
cache because there isn't one).

Installs unconditionally — no receiver-side stdlib/sealed refusal, unlike
`classRemoveFromSystemByName/1`. What varies is whether the change is
flushable to disk (a later concern, ADR 0112 Phase 3), not whether it takes
effect in memory — the same "flushability, not refusal" rule
`classCompileSource/3` already follows for stdlib/dynamic/dependency classes.

Raises a `selector_not_found` `#beamtalk_error{}` — deliberately not
`does_not_understand` (see the ADR's *Error behaviour on absent selector*) —
when the selector resolves nowhere.
""".
-spec classRemoveSelector(#beamtalk_object{}, atom()) -> #beamtalk_object{}.
classRemoveSelector(Self, Selector) ->
    case remove_selector(Self, Selector, 'removeSelector:') of
        removed -> Self;
        absent -> beamtalk_error:raise(selector_not_found_error(Self, Selector))
    end.

-doc """
Remove `Selector` from the receiver like `classRemoveSelector/2`, but
evaluate `AbsentBlock` instead of raising when the selector resolves nowhere.

Backs `@primitive "classRemoveSelectorIfAbsent"`
(`Behaviour>>removeSelector:ifAbsent:`). `AbsentBlock` arrives as an ordinary
compiled Block value — a 0-arity Erlang fun (Beamtalk blocks compile to
funs; see `block.bt`) — called directly here, exactly as
`beamtalk_actor.erl`'s `onExit:` callback calls its own block argument.

Unlike a block argument received by a *locally-defined* class method (which
runs inside that class's own gen_server call, per AGENTS.md's "Blocks into
class methods" rule), this primitive — like every `Behaviour` tower
primitive — is itself reached via the Class -> Behaviour chain-walk
fallthrough (`beamtalk_dispatch:lookup/5`) when sent to a class object,
which runs in the *sender's* process, not the receiver's. `AbsentBlock`
therefore runs there too: messaging the receiver class back from inside it
is an ordinary cross-process send, not a re-entrant one — verified
empirically (`tests/repl-protocol/cases/remove_selector.btscript`) rather
than assumed from the general class-method-block rule, which does not apply
to this call shape.

Returns the receiver on success, or `AbsentBlock`'s value on absence.
""".
-spec classRemoveSelectorIfAbsent(#beamtalk_object{}, atom(), fun(() -> term())) ->
    #beamtalk_object{} | term().
classRemoveSelectorIfAbsent(Self, Selector, AbsentBlock) ->
    case remove_selector(Self, Selector, 'removeSelector:ifAbsent:') of
        removed -> Self;
        absent -> AbsentBlock()
    end.

%% Shared resolution + removal for classRemoveSelector/2 and
%% classRemoveSelectorIfAbsent/3. Resolves `Selector` against whichever table
%% currently supplies dispatch for it on `Self`'s side — extension registry
%% first, local method table second (ADR 0112 § Extension methods) — removes
%% it there, and reports whether anything was found. A removal that resolves
%% but then fails unexpectedly (e.g. a local-method recompile error) raises
%% directly rather than returning through this function, so its return type
%% only distinguishes "removed" from "nothing to remove here".
%% `Sent` is the selector that reached here, for a refusal's error.
-spec remove_selector(#beamtalk_object{}, atom(), atom()) -> removed | absent.
remove_selector(Self, Selector, Sent) ->
    {Side, ClassPid} = removal_target(Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, Sent),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    case Side =:= instance andalso beamtalk_protocol_registry:is_protocol(ClassName) of
        true -> remove_protocol_selector(ClassName, Selector);
        false -> remove_class_selector(Self, ClassName, Side, Selector)
    end.

%% ADR 0127 §11: `Describable removeSelector: #sel` removes a protocol's
%% PROVISION (a required signature is not removable this way — that is a
%% protocol-definition edit) and re-expands its users through the same
%% two-stage, all-or-nothing fan-out a protocol file reload uses. The
%% workspace loader performs it (and raises its structured refusal for a
%% stdlib protocol or a user that can no longer compile); this side only
%% decides present-vs-absent.
-spec remove_protocol_selector(atom(), atom()) -> removed | absent.
remove_protocol_selector(ClassName, Selector) ->
    case lists:member(Selector, beamtalk_protocol_registry:provided_methods(ClassName)) of
        false ->
            absent;
        true ->
            remove_local_method(ClassName, Selector, instance),
            removed
    end.

-spec remove_class_selector(#beamtalk_object{}, atom(), instance | class, atom()) ->
    removed | absent.
remove_class_selector(Self, ClassName, Side, Selector) ->
    %% ADR 0142 §2 (BT-4299): a class-side removal is a class-metadata write;
    %% erase the derived direct-call flag before it becomes visible.
    case Side of
        class -> beamtalk_class_shadow_flags:invalidate(ClassName);
        instance -> ok
    end,
    EtsClass = extension_ets_class(ClassName, Side),
    case beamtalk_extensions:has(EtsClass, Selector) of
        true ->
            remove_extension_selector(ClassName, EtsClass, Selector, Side),
            removed;
        false ->
            remove_runtime_or_local_selector(Self, ClassName, Side, Selector)
    end.

%% BT-4438: after the extension registry, a class-side removal checks the
%% class's ADR 0084 runtime fun (`put_class_method`) before its compiled
%% method -- the order `beamtalk_class_dispatch:apply_class_method_body/7`
%% resolves them in. The class process undoes the put and answers `absent`
%% if the fun went in the meantime, in which case the compiled method (if
%% any) is the next thing to remove.
-spec remove_runtime_or_local_selector(
    #beamtalk_object{}, atom(), instance | class, atom()
) -> removed | absent.
remove_runtime_or_local_selector(Self, ClassName, class, Selector) ->
    case beamtalk_refactor_support:has_runtime_class_fun(ClassName, Selector) of
        true ->
            case beamtalk_object_class:remove_class_method(erlang:element(4, Self), Selector) of
                removed -> removed;
                absent -> remove_local_selector(Self, ClassName, class, Selector)
            end;
        false ->
            remove_local_selector(Self, ClassName, class, Selector)
    end;
remove_runtime_or_local_selector(Self, ClassName, instance, Selector) ->
    remove_local_selector(Self, ClassName, instance, Selector).

-spec remove_local_selector(#beamtalk_object{}, atom(), instance | class, atom()) ->
    removed | absent.
remove_local_selector(Self, ClassName, Side, Selector) ->
    case beamtalk_behaviour_intrinsics:compiled_includes_selector(Self, Selector) of
        true ->
            ok = refuse_if_protocol_provided(ClassName, Side, Selector, remove),
            remove_local_method(ClassName, Selector, Side),
            removed;
        false ->
            absent
    end.

%% ADR 0127 §11: `DateTime removeSelector: #max:` where `max:` is
%% trait-provided is an error naming the trait and the `excluding:` escape
%% hatch — flattening put the provision in the class's own module, but it is
%% still conceptually the trait's, and a silent drop would desync the class
%% from what its `uses:` line promises. `renameSelector:to:` refuses the same
%% selector for the symmetric reason (ADR 0114/0127 §12): the site closure
%% for a trait-provided selector spans every user, which a single-class
%% rename cannot safely rewrite.
%%
%% Keyed off `beamtalk_xref:method_origin/3` (`provenance := protocol,
%% origin`, ADR 0127 §12) rather than a separate check — the same row
%% `CompiledMethod origin` reads. Codegen stamps `'provenance' => 'protocol'`
%% and `'origin'` on every flattened `uses:` provision (`xref.rs`), so this
%% refusal fires for real compiled classes; `rename_selector_test.bt`
%% covers it end-to-end against `DateTime` (`uses: Comparable`), and the
%% EUnit tests install a `protocol`-provenance row directly.
%%
%% Does NOT cover the second §11 case — removing a class's own override that
%% would *re-expose* a hidden provision failing §3a — which needs per-class
%% knowledge of which provisions an override shadowed. That bookkeeping does
%% not exist at runtime yet (tracked as a follow-up; see this issue's
%% completion notes).
-spec refuse_if_protocol_provided(atom(), instance | class, atom(), remove | rename) -> ok.
refuse_if_protocol_provided(ClassName, Side, Selector, Op) ->
    case beamtalk_xref:method_origin(ClassName, Side =:= class, Selector) of
        nil ->
            ok;
        Protocol ->
            beamtalk_error:raise(
                protocol_provided_selector_error(ClassName, Selector, Protocol, Op)
            )
    end.

-spec protocol_provided_selector_error(atom(), atom(), atom(), remove | rename) ->
    #beamtalk_error{}.
protocol_provided_selector_error(ClassName, Selector, Protocol, Op) ->
    SelectorBin = atom_to_binary(Selector, utf8),
    ProtocolBin = atom_to_binary(Protocol, utf8),
    Error0 = beamtalk_error:new(protocol_provided_selector, ClassName, Selector),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"#">>,
            SelectorBin,
            <<" is provided by trait ">>,
            ProtocolBin,
            <<" on ">>,
            atom_to_binary(ClassName, utf8)
        ])
    ),
    beamtalk_error:with_hint(Error1, protocol_provided_selector_hint(SelectorBin, ProtocolBin, Op)).

-spec protocol_provided_selector_hint(binary(), binary(), remove | rename) -> binary().
protocol_provided_selector_hint(SelectorBin, ProtocolBin, remove) ->
    iolist_to_binary([
        <<"exclude it with uses: ">>,
        ProtocolBin,
        <<" excluding: #(#">>,
        SelectorBin,
        <<") or remove it from the trait">>
    ]);
protocol_provided_selector_hint(SelectorBin, ProtocolBin, rename) ->
    iolist_to_binary([
        <<"rename #">>,
        SelectorBin,
        <<" on protocol ">>,
        ProtocolBin,
        <<" itself; every class using it shares this one definition">>
    ]).

%% Remove an extension method and best-effort log its removal (ADR 0112 § Extension
%% methods, § ChangeLog interaction). Captures the extension's owner +
%% stored source BEFORE unregistering — both are gone from `beamtalk_extensions`'s
%% ETS tables the instant `unregister/3` returns — so the ChangeLog entry can
%% still attribute the removal and record what was removed for the audit trail.
-spec remove_extension_selector(atom(), atom(), atom(), instance | class) -> ok.
remove_extension_selector(ClassName, EtsClass, Selector, Side) ->
    Owner = extension_owner(EtsClass, Selector),
    PrevSource = extension_prev_source(EtsClass, Selector),
    ok = beamtalk_extensions:unregister(ClassName, Selector, Side =:= class),
    log_extension_removal(ClassName, Selector, Side, Owner, PrevSource).

-spec extension_owner(atom(), atom()) -> atom() | undefined.
extension_owner(EtsClass, Selector) ->
    case beamtalk_extensions:lookup(EtsClass, Selector) of
        {ok, _Fun, Owner} -> Owner;
        not_found -> undefined
    end.

-spec extension_prev_source(atom(), atom()) -> binary() | undefined.
extension_prev_source(EtsClass, Selector) ->
    %% BT-4173: getSource/2 answers a String; the ChangeLog entry
    %% persists raw bytes — unwrap at this call site.
    case beamtalk_extensions:getSource(EtsClass, Selector) of
        {ok, {beamtalk_string, Source}} -> Source;
        not_found -> undefined
    end.

%% Best-effort ChangeLog append for an extension removal — mirrors
%% log_local_removal/3's `error:undef` guard below: the extension install
%% already succeeded above, so a missing/unavailable workspace here must
%% never surface as a caller-visible error.
-spec log_extension_removal(
    atom(), atom(), instance | class, atom() | undefined, binary() | undefined
) ->
    ok.
log_extension_removal(ClassName, Selector, Side, Owner, PrevSource) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    {Author, AuthorKind} = beamtalk_refactor_support:current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, emit_extension_remove_change_entry, [
            ClassNameBin, Selector, Side, Owner, PrevSource, Author, AuthorKind
        ])
    catch
        error:undef -> ok
    end,
    ok.

%% `Self`'s removal side and class pid. A metaclass-tagged receiver
%% (`Counter class removeSelector: #foo`) is class-side; any other class
%% object (`Counter removeSelector: #foo`) is instance-side — the same
%% branch `classIncludesSelector/2` already makes.
-spec removal_target(#beamtalk_object{}) -> {instance | class, pid()}.
removal_target(#beamtalk_object{class = 'Metaclass', pid = ClassPid}) ->
    {class, ClassPid};
removal_target(Self) ->
    {instance, erlang:element(4, Self)}.

%% The `beamtalk_extensions` ETS key for `Side` — the established
%% convention: an instance-side extension is keyed under the bare class name,
%% a class-side one under the metaclass tag (`unregister/3`'s own `ClassSide`
%% resolution, mirrored here so a lookup and its matching removal always
%% agree on the key).
-spec extension_ets_class(atom(), instance | class) -> atom().
extension_ets_class(ClassName, instance) -> ClassName;
extension_ets_class(ClassName, class) -> beamtalk_class_registry:class_object_tag(ClassName).

%% Remove `Selector` from `ClassName`'s own (non-extension) method table via
%% the existing revert-of-add removal mechanism (ADR 0082, generalized
%% by ADR 0112 rather than duplicated — see the ADR's *Implementation*
%% section), with the stdlib gate relaxed (the `allow_stdlib` policy)
%% since `removeSelector:` installs unconditionally. Routed via
%% `erlang:apply/3` to avoid a compile-time dependency from `beamtalk_runtime`
%% to `beamtalk_workspace` — the same indirection `do_compile_source/4` uses.
%% Raises `no_workspace` when no workspace is running on this node, and a
%% structured `runtime_error` if the removal itself fails (recompile error). Best-effort logs a
%% `"remove-method"` ChangeLog entry after a successful removal (ADR 0112
%% Phase 3) — see log_local_removal/3.
-spec remove_local_method(atom(), atom(), instance | class) -> ok.
remove_local_method(ClassName, Selector, Side) ->
    ok = beamtalk_capability:require_workspace('removeSelector:', ClassName),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    case
        erlang:apply(beamtalk_repl_eval, remove_method, [
            ClassNameBin, Selector, Side, allow_stdlib
        ])
    of
        {ok, _} ->
            log_local_removal(ClassNameBin, Selector, Side),
            ok;
        {error, #beamtalk_error{} = Structured} ->
            %% A refusal the workspace already worded (e.g. a stdlib protocol,
            %% a protocol edit rejected by its users' recompile).
            beamtalk_error:raise(Structured);
        {error, Reason} ->
            Error0 = beamtalk_error:new(runtime_error, ClassName, Selector),
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("Could not remove method: ~tp", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    end.

%% Best-effort ChangeLog append after a successful local-method removal (ADR
%% 0112 Phase 3) — the install already succeeded above, so a logging
%% failure must never surface to the caller. `emit_remove_change_entry/5`
%% already self-swallows every internal failure (mirrors `emit_change_entry/1`
%% for a patch); the `error:undef` catch here only guards the degenerate case
%% where the workspace app itself is unreachable, which cannot realistically
%% happen immediately after the removal above succeeded through the identical
%% `erlang:apply/3` seam, but is cheap to guard defensively all the same.
-spec log_local_removal(binary(), atom(), instance | class) -> ok.
log_local_removal(ClassNameBin, Selector, Side) ->
    {Author, AuthorKind} = beamtalk_refactor_support:current_author_context(),
    try
        erlang:apply(beamtalk_repl_eval, emit_remove_change_entry, [
            ClassNameBin, Selector, Side, Author, AuthorKind
        ])
    catch
        error:undef -> ok
    end,
    ok.

-doc """
Rename the receiver class to `NewName`, rewriting every in-project reference
site found via `SystemNavigation referencesTo:` / `beamtalk_class_registry:
direct_subclasses/1`, and re-registering it under `NewName` in memory.

Backs `@primitive "classRenameTo"` (`Behaviour>>renameTo:`), modelled on
`classRemoveSelector/2`'s shape immediately above: resolve the target,
validate, mutate, best-effort log, return the receiver. Unlike
`removeSelector:`'s "flushable, not refusal" rule, this primitive follows
ADR 0114 § "Refusal vs flushability"'s per-operation table — a stdlib or
dependency class is refused BEFORE any memory mutation (the xref index only
covers in-project source, so site discovery for either could never be
complete), while a dynamic (`ClassBuilder`) class is allowed with
`flushable: false` (`"dynamic"`), same as a project class's disk half
(out of scope here — this primitive is in-memory only).

## Ordering

1. Resolve `OldName` from the receiver's live class-object state (mirrors
   `className/1` — never trust `Self`'s own possibly-stale `class` field).
2. Collision refusal: `NewName` already a loaded class — raised before any
   other check, cheapest and most fundamental ("nothing about this call can
   proceed").
3. Stdlib/dependency refusal, reusing `capture_class_removal_snapshot/1`'s
   existing flushability classification (no new stdlib/dependency/dynamic
   detection logic — see that function's own doc). Still read-only.
4. Site discovery (`discover_rename_sites/3`) — read-only: computes the
   union of `referencesTo:`/`direct_subclasses/1` translated into
   `beamtalk_repl_loader:rewrite_site()` maps, plus the class's own
   declaration-header span as the definition site.
5. Mutate: for an ordinary (project) class, `rewrite_sites/2` (the shared
   mechanism) installs every site transactionally and that same
   install is what publishes the new pid, so `install_class_rename/3` only
   retires the old registry identity afterward. For a dynamic class, the
   two mutations are unrelated calls into different subsystems with no
   shared rollback, so `do_rename_and_rewrite/7` validates the rewrite
   FIRST (`validate_class_sites/4`, non-mutating), moves the registry
   identity from `OldName` to `NewName` second (`install_class_rename/3` —
   see its own doc for the dynamic-vs-compiled split), and only then
   performs the real rewrite — see `do_rename_and_rewrite/7`'s own doc for
   why.
6. Best-effort ChangeLog append (`log_class_rename/4`), mirroring
   `log_local_removal/3`'s placement — the rename is already live in memory
   by this point, so a logging failure must never surface to the caller.

Raises a structured `#beamtalk_error{}` for the collision/stdlib/dependency
refusals and for a `rewrite_sites/2` failure (validation or partial
install); returns the receiver, re-pointed at the newly-installed class
object, on success.
""".
-spec classRenameTo(#beamtalk_object{}, atom()) -> #beamtalk_object{}.
classRenameTo(Self, NewName) when is_atom(NewName) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, 'renameTo:'),
    OldName = beamtalk_object_class:class_name_safe(ClassPid),
    ok = beamtalk_refactor_support:require_class_capability(
        'renameTo:', OldName, [<<"renameTo: #">>, atom_to_binary(NewName, utf8)]
    ),
    ok = beamtalk_refactor_support:require_class_responsive(ClassPid, OldName, 'renameTo:'),
    ok = ensure_rename_collision_free(OldName, NewName),
    OldNameBin = atom_to_binary(OldName, utf8),
    Classification = beamtalk_refactor_support:capture_class_removal_snapshot(OldNameBin),
    ok = ensure_class_renamable(OldName, Classification),
    NewNameBin = atom_to_binary(NewName, utf8),
    {DefinitionSite, ReferenceSites} =
        discover_rename_sites(OldName, OldNameBin, NewNameBin, Classification),
    do_rename_and_rewrite(
        OldName, OldNameBin, NewName, NewNameBin, DefinitionSite, ReferenceSites, Classification
    ).

%% Ordering for a DYNAMIC class differs from an ordinary one (review
%% feedback on PR #3523): a dynamic class's registry-identity move
%% (`install_class_rename/3` -> `beamtalk_object_class:rename/2`) is a
%% SEPARATE call from its reference-site rewrite — `rewrite_class_sites/4`
%% only ever touches OTHER classes' files for a dynamic class, since it has
%% no source of its own to fold into that transaction. Committing either
%% mutation before confirming the OTHER would succeed risks the same class
%% of half-applied state either way round: identity-move-first leaves
%% referencing files un-rewritten if the rewrite then fails (a real,
%% reachable case — `validation_failed`/`invalid_or_overlapping_span`/
%% `workspace_unavailable`, not just the TOCTOU registration race the
%% comment on `install_class_rename/3` describes); rewrite-first leaves
%% referencing files pointing at a name the class never adopted if the
%% identity move then fails. Neither mutation has a rollback, so the fix
%% is to validate the rewrite FIRST via `validate_class_sites/4` (the same
%% non-mutating compile-only check `rewrite_sites/2` itself runs before its
%% own install pass, exposed standalone for exactly this cross-subsystem
%% ordering need) — only once that passes does the identity move run,
%% and only then the real (now expected-to-succeed) rewrite. A rewrite
%% failure at that final step despite passing validation is the same
%% already-accepted residual risk `rewrite_sites/2`'s own doc names for its
%% `partial_install_failure` case (no cross-gen-server rollback), not a new
%% gap this function introduces — surfaced via `rename_partial_failure_error/3`
%% rather than `rename_rewrite_failed_error/2` since it is a genuinely
%% different situation for the caller: the class HAS been renamed at that
%% point. For an ordinary (project) class, the definition site's own
%% recompile (part of the SAME `rewrite_sites/2` transaction as the
%% reference sites) is what installs the new pid — there is no separate
%% identity-move step to reorder, so the original
%% site-rewrite-then-retire-old-identity order stands.
-spec do_rename_and_rewrite(
    atom(), binary(), atom(), binary(), map() | undefined, [map()], map()
) -> #beamtalk_object{}.
do_rename_and_rewrite(
    OldName,
    OldNameBin,
    NewName,
    NewNameBin,
    DefinitionSite,
    ReferenceSites,
    #{not_flushable_reason := <<"dynamic">>} = Classification
) ->
    case validate_class_sites(OldName, DefinitionSite, ReferenceSites, Classification) of
        ok ->
            NewPid = install_class_rename(OldName, NewName, Classification),
            case
                rewrite_after_identity_move(
                    OldName, DefinitionSite, ReferenceSites, Classification
                )
            of
                {ok, RewriteResult} ->
                    log_class_rename(OldNameBin, NewNameBin, Classification, RewriteResult),
                    beamtalk_class_registry:class_object_from_pid(NewPid);
                {error, Reason} ->
                    beamtalk_error:raise(
                        rename_partial_failure_error(OldName, NewName, Reason)
                    )
            end;
        {error, Reason} ->
            beamtalk_error:raise(rename_rewrite_failed_error(OldName, Reason))
    end;
do_rename_and_rewrite(
    OldName, OldNameBin, NewName, NewNameBin, DefinitionSite, ReferenceSites, Classification
) ->
    case rewrite_class_sites(OldName, DefinitionSite, ReferenceSites, Classification) of
        {ok, RewriteResult} ->
            NewPid = install_class_rename(OldName, NewName, Classification),
            log_class_rename(OldNameBin, NewNameBin, Classification, RewriteResult),
            beamtalk_class_registry:class_object_from_pid(NewPid);
        {error, Reason} ->
            beamtalk_error:raise(rename_rewrite_failed_error(OldName, Reason))
    end.

%% Collision refusal (ADR 0114 § Decision): `renameTo: #Existing` when
%% `Existing` already names a loaded class raises rather than silently
%% overwriting — exact hint text from the ADR's own worked example.
-spec ensure_rename_collision_free(atom(), atom()) -> ok.
ensure_rename_collision_free(OldName, NewName) ->
    case beamtalk_class_registry:whereis_class(NewName) of
        undefined -> ok;
        _Pid -> beamtalk_error:raise(rename_collision_error(OldName, NewName))
    end.

-spec rename_collision_error(atom(), atom()) -> #beamtalk_error{}.
rename_collision_error(OldName, NewName) ->
    OldBin = atom_to_binary(OldName, utf8),
    NewBin = atom_to_binary(NewName, utf8),
    Error0 = beamtalk_error:new(class_already_exists, OldName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"cannot rename ">>,
            OldBin,
            <<" to ">>,
            NewBin,
            <<" — "/utf8>>,
            NewBin,
            <<" already exists">>
        ])
    ),
    beamtalk_error:with_hint(Error1, <<"remove or rename the existing class first">>).

%% Stdlib/dependency refusal (ADR 0114 § "Refusal vs flushability"): a
%% dynamic class (`not_flushable_reason: "dynamic"`) and an ordinary project
%% class (`flushable: true`) both proceed; only "stdlib" and "dependency:*"
%% refuse, BEFORE any site discovery or mutation runs.
-spec ensure_class_renamable(atom(), map()) -> ok.
ensure_class_renamable(ClassName, #{not_flushable_reason := <<"stdlib">>}) ->
    beamtalk_error:raise(stdlib_rename_refusal_error(ClassName));
ensure_class_renamable(ClassName, #{not_flushable_reason := <<"dependency:", _/binary>> = Reason}) ->
    beamtalk_error:raise(dependency_rename_refusal_error(ClassName, Reason));
ensure_class_renamable(_ClassName, _Classification) ->
    ok.

-spec stdlib_rename_refusal_error(atom()) -> #beamtalk_error{}.
stdlib_rename_refusal_error(ClassName) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"Cannot rename stdlib class '">>, atom_to_binary(ClassName, utf8), <<"'">>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        <<
            "Stdlib classes are protected and cannot be renamed; the xref "
            "index only covers in-project source, so references outside the "
            "project could never be found and rewritten."
        >>
    ).

-spec dependency_rename_refusal_error(atom(), binary()) -> #beamtalk_error{}.
dependency_rename_refusal_error(ClassName, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"Cannot rename dependency class '">>, atom_to_binary(ClassName, utf8), <<"'">>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"This class is defined outside the project (">>,
            Reason,
            <<
                "); the xref index only covers in-project source, so its "
                "references cannot be found and rewritten safely."
            >>
        ])
    ).

-spec rename_rewrite_failed_error(atom(), term()) -> #beamtalk_error{}.
rename_rewrite_failed_error(OldName, workspace_unavailable) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    beamtalk_error:with_message(
        Error0,
        <<"Workspace not available; renameTo: requires a running workspace">>
    );
%% A non-dynamic classification with neither a definition nor any reference
%% site — the divergent-classification edge case `rewrite_class_sites/4`'s
%% own doc describes, falling through to a real `{error, no_sites}` from
%% `rewrite_sites/2` instead of that function's dynamic-only trivial-success
%% shortcut.
rename_rewrite_failed_error(OldName, no_sites) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    beamtalk_error:with_message(
        Error0,
        <<"renameTo: found no declaration or reference site to rewrite">>
    );
rename_rewrite_failed_error(OldName, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    Msg = beamtalk_error:chars_to_binary(io_lib:format("Could not rename class: ~tp", [Reason])),
    beamtalk_error:with_message(Error0, Msg).

%% A dynamic class's identity move committed AFTER `validate_class_sites/4`
%% already passed, yet the real rewrite still failed (`do_rename_and_rewrite/7`'s
%% own doc names this the same already-accepted residual risk
%% `rewrite_sites/2`'s `partial_install_failure` case describes — no
%% cross-gen-server rollback). Deliberately a distinct error from
%% `rename_rewrite_failed_error/2`: that one always means "nothing changed",
%% this one means the class IS renamed but some reference sites are not.
-spec rename_partial_failure_error(atom(), atom(), term()) -> #beamtalk_error{}.
rename_partial_failure_error(OldName, NewName, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format(
            "Renamed ~tp to ~tp, but rewriting its reference sites failed after "
            "validation passed (partial state — the class's own identity has "
            "already moved; investigate before retrying): ~tp",
            [OldName, NewName, Reason]
        )
    ),
    beamtalk_error:with_message(Error0, Msg).

%% Full site list: the union of `referencesTo:`/`direct_subclasses/1`
%% translated into `rewrite_site()` maps, plus the class's own declaration
%% header as the definition site. `referencesTo:`/`direct_subclasses/1` only
%% say WHICH method/header mentions `OldName` (owner, side, selector) — they
%% carry no byte span — so each hit here is independently re-resolved to an
%% exact byte span via `beamtalk_compiler:resolve_method_span/4` /
%% `resolve_class_span/2` (both already-shipped, ADR 0082) rather than
%% reimplementing span resolution. A dynamic class (no `source_file` in
%% `Classification`) has no declaration to rewrite, matching the ChangeLog
%% schema's `sites[0] = null` case.
-spec discover_rename_sites(atom(), binary(), binary(), map()) ->
    {map() | undefined, [map()]}.
discover_rename_sites(OldName, OldNameBin, NewNameBin, Classification) ->
    DefinitionSite = definition_rewrite_site(OldNameBin, NewNameBin, Classification),
    ReferenceSites =
        reference_rewrite_sites(OldName, NewNameBin) ++
            subclass_header_rewrite_sites(OldName, OldNameBin, NewNameBin),
    {DefinitionSite, ReferenceSites}.

-spec definition_rewrite_site(binary(), binary(), map()) ->
    map() | undefined.
definition_rewrite_site(OldNameBin, NewNameBin, #{source_file := SourceFile}) ->
    case current_class_source(OldNameBin) of
        undefined ->
            undefined;
        Source ->
            Pattern = <<"subclass:\\s*(", OldNameBin/binary, ")\\b">>,
            case header_token_span(Source, OldNameBin, Pattern) of
                {ok, Span} ->
                    #{
                        class => OldNameBin,
                        source_file => SourceFile,
                        span => Span,
                        new_text => NewNameBin
                    };
                not_found ->
                    undefined
            end
    end;
definition_rewrite_site(_OldNameBin, _NewNameBin, _Classification) ->
    %% Dynamic class: no backing source (ChangeLog schema's sites[0] = null).
    undefined.

%% Every direct subclass's own declaration header names `OldName` as its
%% superclass (`OldName subclass: Sub`) — the one reference kind
%% `referencesTo:` doesn't cover (ADR 0114 § Decision).
-spec subclass_header_rewrite_sites(atom(), binary(), binary()) ->
    [map()].
subclass_header_rewrite_sites(OldName, OldNameBin, NewNameBin) ->
    Subs = beamtalk_class_registry:direct_subclasses(OldName),
    lists:filtermap(
        fun(Sub) -> subclass_header_rewrite_site(Sub, OldNameBin, NewNameBin) end,
        Subs
    ).

-spec subclass_header_rewrite_site(atom(), binary(), binary()) ->
    {true, map()} | false.
subclass_header_rewrite_site(Sub, OldNameBin, NewNameBin) ->
    SubBin = atom_to_binary(Sub, utf8),
    case current_class_source(SubBin) of
        undefined ->
            false;
        Source ->
            Pattern = <<"(", OldNameBin/binary, ")\\s+subclass:">>,
            case header_token_span(Source, SubBin, Pattern) of
                {ok, Span} ->
                    {true, #{
                        class => SubBin,
                        source_file => class_source_file_for(SubBin),
                        span => Span,
                        new_text => NewNameBin
                    }};
                not_found ->
                    false
            end
    end.

%% Resolve `ClassNameBin`'s declaration-header + state-declaration span
%% (`beamtalk_compiler:resolve_class_span/2`, ADR 0082 — deliberately
%% never a method body) and search `Pattern` WITHIN that slice only, not the
%% whole file — a doc comment example mentioning the identical header text
%% (common in stdlib doc comments) must never be mistaken for the real
%% declaration. Returns the matched capture group's span translated back to
%% absolute offsets into `Source`.
-spec header_token_span(binary(), binary(), binary()) ->
    {ok, map()} | not_found.
header_token_span(Source, ClassNameBin, Pattern) ->
    case class_header_span(Source, ClassNameBin) of
        {ok, {HStart, HEnd}} ->
            HeaderText = binary:part(Source, HStart, HEnd - HStart),
            case re:run(HeaderText, Pattern, [{capture, [1], index}]) of
                {match, [{Start, Len}]} ->
                    {ok, #{start => HStart + Start, 'end' => HStart + Start + Len}};
                nomatch ->
                    not_found
            end;
        not_found ->
            not_found
    end.

-spec class_header_span(binary(), binary()) ->
    {ok, {non_neg_integer(), non_neg_integer()}} | not_found.
class_header_span(Source, ClassNameBin) ->
    try erlang:apply(beamtalk_compiler, resolve_class_span, [Source, ClassNameBin]) of
        {ok, #{start := S, 'end' := E}, _PrevSource} -> {ok, {S, E}};
        {error, _Reason, _Message} -> not_found
    catch
        error:undef -> not_found
    end.

%% Every `{owner, class_side, method}` triple `referencesTo:` reports for
%% `OldName`, deduped across its possibly-multiple line-rows (ADR 0114
%% site-discovery spike finding #2: two mentions on two different lines of
%% the same method are two rows; this only needs the method once).
-spec reference_rewrite_sites(atom(), binary()) -> [map()].
reference_rewrite_sites(OldName, NewNameBin) ->
    Sites = beamtalk_xref:references_to(OldName),
    OldNameBin = atom_to_binary(OldName, utf8),
    Triples = lists:usort([
        {Owner, ClassSide, Method}
     || #{owner := Owner, class_side := ClassSide, method := Method} <- Sites
    ]),
    lists:flatmap(
        fun({Owner, ClassSide, Method}) ->
            method_body_rewrite_sites(Owner, ClassSide, Method, OldNameBin, NewNameBin)
        end,
        Triples
    ).

%% Every whole-word occurrence of `OldNameBin` within `{Owner, ClassSide,
%% Method}`'s own resolved byte span (`resolve_method_span/4`) becomes its
%% own rewrite site — a method can mention the renamed class more than once
%% (e.g. a param type AND a return type, spike finding #2's `Duration>>+`
%% case), each needing its own splice.
-spec method_body_rewrite_sites(atom(), boolean(), atom(), binary(), binary()) ->
    [map()].
method_body_rewrite_sites(Owner, IsClassSide, Method, OldNameBin, NewNameBin) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case current_class_source(OwnerBin) of
        undefined ->
            [];
        Source ->
            SelectorBin = atom_to_binary(Method, utf8),
            %% `beamtalk_xref:site()`'s `class_side` field is a boolean (`true`
            %% = class-side, `false` = instance-side) — translate to the
            %% `instance | class` atom `resolve_method_span/4` expects.
            Side =
                case IsClassSide of
                    true -> class;
                    false -> instance
                end,
            case method_token_span(Source, OwnerBin, SelectorBin, Side) of
                {ok, {MStart, MEnd}} ->
                    MethodText = binary:part(Source, MStart, MEnd - MStart),
                    SourceFile = class_source_file_for(OwnerBin),
                    [
                        #{
                            class => OwnerBin,
                            source_file => SourceFile,
                            span => #{start => MStart + LStart, 'end' => MStart + LEnd},
                            new_text => NewNameBin
                        }
                     || {LStart, LEnd} <- word_occurrence_spans(MethodText, OldNameBin)
                    ];
                not_found ->
                    []
            end
    end.

-spec method_token_span(binary(), binary(), binary(), instance | class) ->
    {ok, {non_neg_integer(), non_neg_integer()}} | not_found.
method_token_span(Source, OwnerBin, SelectorBin, ClassSide) ->
    try
        erlang:apply(beamtalk_compiler, resolve_method_span, [
            Source, OwnerBin, SelectorBin, ClassSide
        ])
    of
        {ok, #{start := S, 'end' := E}, _PrevSource} -> {ok, {S, E}};
        {error, _Reason, _Message} -> not_found
    catch
        error:undef -> not_found
    end.

%% Every non-overlapping whole-word occurrence of `WordBin` in `Text`, as
%% `{Start, End}` byte offsets relative to `Text`'s own start.
-spec word_occurrence_spans(binary(), binary()) -> [{non_neg_integer(), non_neg_integer()}].
word_occurrence_spans(Text, WordBin) ->
    Pattern = <<"\\b", WordBin/binary, "\\b">>,
    case re:run(Text, Pattern, [global, {capture, first, index}]) of
        {match, Matches} -> [{S, S + L} || [{S, L}] <- Matches];
        nomatch -> []
    end.

%% `ClassNameBin`'s CURRENT tracked source (`beamtalk_workspace_meta:
%% get_class_source/1` — the in-memory-merged text `rewrite_sites/2` itself
%% splices against, per that function's own doc; NOT necessarily identical
%% to what's on disk). Routed via `erlang:apply/3` to avoid a compile-time
%% dependency from `beamtalk_runtime` to `beamtalk_workspace`, the same
%% indirection every other cross-app call in this module already uses.
-spec current_class_source(binary()) -> binary() | undefined.
current_class_source(ClassNameBin) ->
    try erlang:apply(beamtalk_workspace_meta, get_class_source, [ClassNameBin]) of
        Source when is_list(Source) -> unicode:characters_to_binary(Source);
        undefined -> undefined
    catch
        error:undef -> undefined
    end.

%% `ClassNameBin`'s on-disk source file, for a `rewrite_site()`'s
%% `source_file` ChangeLog-attribution field only (see that type's doc) —
%% reuses `beamtalk_repl_loader:class_source_file/1` (already exported for
%% exactly this kind of cross-module reuse) rather than re-deriving
%% it. `undefined` for a class with no backing file.
-spec class_source_file_for(binary()) -> binary() | undefined.
class_source_file_for(ClassNameBin) ->
    try erlang:apply(beamtalk_repl_loader, class_source_file, [ClassNameBin]) of
        SourceFile when is_binary(SourceFile) -> SourceFile;
        nil -> undefined
    catch
        error:undef -> undefined
    end.

%% Thin `erlang:apply/3` forwarding to the shared multi-site rewrite
%% mechanism — same indirection every other cross-app call in this
%% module already uses. A dynamic class with neither a definition site nor
%% any reference site (never mentioned anywhere in-project) has nothing to
%% rewrite at all; `rewrite_sites/2` itself refuses that shape
%% (`{error, no_sites}`) since it is normally a caller bug, but here it is
%% the ordinary, legitimate "freestanding dynamic class" case, so it is
%% special-cased to a trivial success rather than surfaced as an error.
%%
%% The trivial-success shortcut is gated on `Classification` actually being
%% `"dynamic"`, NOT merely on the site shape (per review feedback on PR
%% #3523): `current_class_source/1` (`beamtalk_workspace_meta:
%% get_class_source/1`) is a separate source of truth from the BEAM-module
%% check `capture_class_removal_snapshot/1` classifies from, and the two
%% could in principle diverge for a project class (e.g. tracked source never
%% set). Were the shortcut keyed on shape alone, that divergence would
%% silently skip `rewrite_sites/2` for an ORDINARY class too, and
%% `install_class_rename/3` would then stop and purge the old class with
%% nothing ever installed under `NewName` — a `class_not_found`-flavoured
%% crash after the old class is already gone. Falling through to the real
%% `rewrite_sites/2` call for that shape on a non-dynamic classification
%% instead surfaces a clean `{error, no_sites}` — translated by
%% `rename_rewrite_failed_error/2` into a structured error — before any
%% mutation happens.
-spec rewrite_class_sites(atom(), map() | undefined, [map()], map()) ->
    {ok, map()} | {error, term()}.
rewrite_class_sites(OldName, DefinitionSite, ReferenceSites, Classification) ->
    rewrite_class_sites(OldName, DefinitionSite, ReferenceSites, Classification, 'renameTo:').

%% As `rewrite_class_sites/4`, naming `Selector` (the operation the caller
%% actually sent) in a `no_workspace` refusal.
-spec rewrite_class_sites(atom(), map() | undefined, [map()], map(), atom()) ->
    {ok, map()} | {error, term()}.
rewrite_class_sites(
    _OldName, undefined, [], #{not_flushable_reason := <<"dynamic">>}, _Selector
) ->
    {ok, #{definition => undefined, sites => []}};
rewrite_class_sites(OldName, DefinitionSite, ReferenceSites, _Classification, Selector) ->
    ok = beamtalk_capability:require_workspace(Selector, OldName),
    erlang:apply(beamtalk_repl_eval, rewrite_sites, [DefinitionSite, ReferenceSites]).

%% The reference-site rewrite that runs AFTER `install_class_rename/3` has
%% already moved the registry identity. A `no_workspace` refusal here (the
%% workspace stopped between validation and this rewrite) must not escape as
%% a bare raise: it is reported as `{error, workspace_unavailable}` so the
%% caller raises `rename_partial_failure_error/3`, which tells the user the
%% class HAS been renamed but its reference sites were not rewritten.
-spec rewrite_after_identity_move(atom(), map() | undefined, [map()], map()) ->
    {ok, map()} | {error, term()}.
rewrite_after_identity_move(OldName, DefinitionSite, ReferenceSites, Classification) ->
    try
        rewrite_class_sites(OldName, DefinitionSite, ReferenceSites, Classification)
    catch
        error:#{error := #beamtalk_error{kind = no_workspace}} ->
            {error, workspace_unavailable}
    end.

%% `rewrite_class_sites/4`'s own non-mutating validation half — same shape
%% and same trivial-success shortcut for a freestanding dynamic class with
%% nothing to rewrite, but reporting `ok`/`{error, _}` rather than
%% `{ok, map()}`/`{error, _}` since there is no install result to return.
%% Used by `do_rename_and_rewrite/7`'s dynamic-class branch to confirm the
%% reference-site rewrite WOULD succeed before committing the separate,
%% unrelated registry-identity move — see that function's own doc for why.
-spec validate_class_sites(atom(), map() | undefined, [map()], map()) ->
    ok | {error, term()}.
validate_class_sites(_OldName, undefined, [], #{not_flushable_reason := <<"dynamic">>}) ->
    ok;
validate_class_sites(OldName, DefinitionSite, ReferenceSites, _Classification) ->
    ok = beamtalk_capability:require_workspace('renameTo:', OldName),
    erlang:apply(beamtalk_repl_eval, validate_sites, [DefinitionSite, ReferenceSites]).

-doc """
Move the class registry identity from `OldName` to `NewName` after a
successful `rewrite_sites/2` install, returning the pid now serving
`NewName`.

Exported (ADR 0114 Phase 4) beyond `classRenameTo/2`'s own forward
path: reverting a pending `'rename-class'` ChangeEntry re-splices the
definition site's `prev_source_ref` back in, which makes `rewrite_sites/2`'s
own install pipeline register a fresh pid under `old_class` exactly the same
way the forward rename's splice originally registered `NewName` — so undo
only needs the identical retire-the-stale-registration step below, called
with the two names swapped (`beamtalk_repl_loader:finish_rename_class_
revert/1`), not a second copy of it.

Ordinary (project) class: the definition site's recompile already installed
a fresh class-object process under `NewName` as an ordinary side effect of
the standard `activate_module/4` -> `register_class/0` ->
`beamtalk_class_builder:register/1` pipeline (a NEW pid — hot-reload only
reuses the SAME pid when the registered name is unchanged, which a rename by
definition is not). This branch therefore only needs to retire `OldName`:
stop its now-orphaned class-object gen_server (`terminate/2` cleans up the
ETS hierarchy entry, pg group, and loaded-class/backing-module indexes —
NOT actors, per ADR 0114 Constraint 3: existing instances dispatch via
`class_mod` bound at spawn, never by looking the class up by its registered
name) and purge the four name-keyed derived registries
(`beamtalk_class_lifecycle:purge_class_registries/1`, reused rather than
`class_removed/2`'s full teardown — see that function's doc for why
`purge_protocol/1`'s module-keyed purge must NOT run here: the SAME BEAM
module atom still backs the renamed class, in-memory-only, no disk flush).

Dynamic (`ClassBuilder`) class: nothing above ever ran (no source, no
`rewrite_sites/2` recompile) — `beamtalk_object_class:rename/2` moves the
SAME live process to the new registered name in place.
""".
-spec install_class_rename(atom(), atom(), map()) -> pid().
install_class_rename(OldName, NewName, #{not_flushable_reason := <<"dynamic">>}) ->
    case beamtalk_object_class:rename(OldName, NewName) of
        {ok, Pid} ->
            Pid;
        {error, Reason} ->
            Error0 = beamtalk_error:new(runtime_error, OldName),
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format(
                    "The dynamic class's own registration could not move, so no reference sites were rewritten: ~tp",
                    [
                        Reason
                    ]
                )
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    end;
install_class_rename(OldName, NewName, _Classification) ->
    %% Defensive backstop (review feedback on PR #3523): a successful
    %% `rewrite_sites/2` call for an ordinary class is expected to have
    %% already installed `NewName` via the normal compile/activate pipeline
    %% (see this function's own doc) — checked here BEFORE touching
    %% `OldName` so an unexpected miss raises cleanly with the old class
    %% left intact, rather than retiring `OldName` first and crashing on a
    %% `pid()`-typed caller (`class_object_from_pid/1`) handed `undefined`.
    case beamtalk_class_registry:whereis_class(NewName) of
        undefined ->
            beamtalk_error:raise(rename_install_incomplete_error(OldName, NewName));
        NewPid ->
            case beamtalk_class_registry:whereis_class(OldName) of
                undefined ->
                    ok;
                OldPid ->
                    beamtalk_class_monitor:unwatch(OldName),
                    gen_server:stop(OldPid)
            end,
            ok = beamtalk_class_lifecycle:purge_class_registries(OldName),
            NewPid
    end.

-spec rename_install_incomplete_error(atom(), atom()) -> #beamtalk_error{}.
rename_install_incomplete_error(OldName, NewName) ->
    Error0 = beamtalk_error:new(runtime_error, OldName),
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format(
            "renameTo: reported success but '~tp' was never installed under '~tp'; "
            "the old class was left untouched",
            [OldName, NewName]
        )
    ),
    beamtalk_error:with_message(Error0, Msg).

%% Best-effort ChangeLog append after a successful `renameTo:` (ADR 0114 §
%% ChangeLog schema, `kind: "rename-class"`) — mirrors `log_local_removal/3`'s
%% placement and self-swallowing failure handling: the rename is already
%% live in memory by this point, so a logging failure must never surface to
%% the caller. `side`/`selector`/`old_selector` are absent (`null`) — a
%% class rename has no method-level target, matching the schema.
-spec log_class_rename(binary(), binary(), map(), map()) -> ok.
log_class_rename(OldNameBin, NewNameBin, Classification, RewriteResult) ->
    {Author, AuthorKind} = beamtalk_refactor_support:current_author_context(),
    Spec = #{
        kind => 'rename-class',
        class => NewNameBin,
        old_class => OldNameBin,
        old_path => maps:get(source_file, Classification, undefined),
        new_path => undefined,
        intent => durable,
        author => Author,
        author_kind => AuthorKind
    },
    try
        erlang:apply(beamtalk_repl_eval, emit_rewrite_change_entry, [Spec, RewriteResult])
    catch
        error:undef -> ok
    end,
    ok.

%% Structured `selector_not_found` error for the bare `removeSelector:` form
%% (ADR 0112 § Error behaviour on absent selector) — deliberately distinct
%% from `does_not_understand`: the message itself (`removeSelector:`) was
%% understood and executed; only its *argument* had nothing to act on. Carries
%% a hint pointing at the discovery/escape-hatch methods a caller most likely
%% wants next.
-spec selector_not_found_error(#beamtalk_object{}, atom()) -> #beamtalk_error{}.
selector_not_found_error(Self, Selector) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    Error0 = beamtalk_error:new(selector_not_found, ClassName, Selector),
    Msg = iolist_to_binary([
        atom_to_binary(ClassName, utf8),
        <<" does not define #">>,
        atom_to_binary(Selector, utf8),
        <<" locally (or as an extension)">>
    ]),
    Error1 = beamtalk_error:with_message(Error0, Msg),
    beamtalk_error:with_hint(
        Error1,
        <<
            "Use includesSelector: or whichClassIncludesSelector: to check "
            "first, or removeSelector:ifAbsent: to supply a fallback."
        >>
    ).

-doc """
Compile a method body String and install it in this class as a **durable** live
patch (ADR 0082 Phase 1).

Backs `@primitive "classCompileSource"' (`Behaviour>>compile:source:') and the
target of MCP `save_method' / the browser "Save" action. The `>>' patcher form
and `compile:source:' are distinct front doors that converge at the runtime
install chokepoint (`beamtalk_repl_loader:load_recompiled_method/7'): `>>' is not
parser sugar for this primitive, but both produce the same in-memory patch and
ChangeLog entry there. `Selector' is a Symbol (atom), `Source' the method body
String (binary) passed as a value. Installs in memory and attempts (best-effort)
to record a durable ChangeLog entry; returns the receiver class.
""".
-spec classCompileSource(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classCompileSource(Self, Selector, Source) ->
    do_compile_source(Self, Selector, Source, durable).

-doc """
Compile a method body String and install it as an **ephemeral** live patch
(ADR 0082 Phase 1).

Backs `@primitive "classTryCompileSource"' (`Behaviour>>tryCompile:source:') and
the MCP `try_method' tool. Identical install to `classCompileSource/3' but the
ChangeLog entry is tagged `intent: ephemeral' so it auto-prunes on flush and on
workspace restart unless promoted via `compile:source:'. Returns the receiver.
""".
-spec classTryCompileSource(#beamtalk_object{}, atom(), binary()) -> #beamtalk_object{}.
classTryCompileSource(Self, Selector, Source) ->
    do_compile_source(Self, Selector, Source, ephemeral).

-spec compile_source_op(durable | ephemeral) -> beamtalk_capability:operation().
compile_source_op(durable) -> 'compile:source:';
compile_source_op(ephemeral) -> 'tryCompile:source:'.

%% Shared compile-and-install path for compile:source: / tryCompile:source:.
%% Routes to beamtalk_repl_eval:compile_method/6 via erlang:apply to keep
%% beamtalk_runtime free of a compile-time dependency on beamtalk_workspace
%% (the same indirection classReload/1 uses).
-spec do_compile_source(#beamtalk_object{}, atom(), binary(), durable | ephemeral) ->
    #beamtalk_object{}.
do_compile_source(Self, Selector, Source, Intent) ->
    ClassPid = erlang:element(4, Self),
    ok = beamtalk_refactor_support:refuse_in_own_class_process(ClassPid, compile_source_op(Intent)),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    ok = beamtalk_capability:require_workspace(compile_source_op(Intent), ClassName),
    ok = beamtalk_refactor_support:require_method_capability(
        compile_source_op(Intent), ClassName, Selector
    ),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    SourceBin = ensure_source_binary(Selector, Source, Intent, ClassName),
    {Author, AuthorKind} = beamtalk_refactor_support:current_author_context(),
    case
        erlang:apply(beamtalk_repl_eval, compile_method, [
            ClassNameBin, Selector, SourceBin, Intent, Author, AuthorKind
        ])
    of
        {ok, _ClassNameBin} ->
            Self;
        {error, Reason} ->
            Error0 = beamtalk_error:new(compile_failed, ClassName),
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("Could not compile method: ~tp", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    end.

-doc """
Compile a pending method edit and report would-be-stale dependents,
**without installing** (ADR 0105 Phase 3).

Backs `@primitive "classPrecheckCompileSource"' (`Behaviour>>precheckCompile:
source:') — the editor/LSP's "check before save" hook (ADR 0105's Phase 3
steelman accommodation: non-blocking, the post-reload image check remains
the authority; this is an early warning against the *pending* edit).
`Selector' is a Symbol (atom), `Source' the pending method body String
(binary), same argument shape as `compile:source:'. Nothing installs and
nothing is recorded to the ChangeLog — this is read-only. Returns a
Dictionary with keys `#findings` (List of finding Dictionaries: `#owner`,
`#changedClass`, `#selector`, `#classification`, `#severity`, `#category`,
`#message`, `#note`, `#sites`, `#start`, `#end`), `#checked`,
`#totalCandidates`, `#notChecked`, `#capNote`, `#checkedOwners` — mirroring
`beamtalk_recheck:result()`/`finding()`, camelCased for the Beamtalk surface
(matching `Workspace flush`'s `#newClasses` convention;
`precheck_result_to_dictionary/1` is this primitive's own encoder, parallel
to `beamtalk_ws_handler:encode_reload_check_event/1`'s wire encoder for the
same underlying shape).
""".
-spec classPrecheckCompileSource(#beamtalk_object{}, atom(), binary()) -> map().
classPrecheckCompileSource(Self, Selector, Source) ->
    ClassPid = erlang:element(4, Self),
    ClassName = beamtalk_object_class:class_name_safe(ClassPid),
    ok = beamtalk_capability:require_workspace('precheckCompile:source:', ClassName),
    ok = beamtalk_refactor_support:require_method_capability(
        'precheckCompile:source:', ClassName, Selector
    ),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    SourceBin = ensure_precheck_source_binary(Source, ClassName),
    %% instance-side only: like classCompileSource/3, the class-side
    %% patch path (`Class class >> sel') does not route through this
    %% primitive — see beamtalk_repl_loader's patch_side/1 callers.
    case
        erlang:apply(beamtalk_repl_eval, precheck_method, [
            ClassNameBin, Selector, SourceBin, instance
        ])
    of
        {ok, Result} ->
            precheck_result_to_dictionary(Result);
        {error, Reason} ->
            Error0 = beamtalk_error:new(compile_failed, ClassName),
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("Could not precheck method: ~tp", [Reason])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg))
    end.

%% Validate the `Source' argument for `precheckCompile:source:' is a String
%% (binary). Distinct from ensure_source_binary/4 because there is no
%% `Intent' to derive the error's selector name from — precheck only ever
%% backs one public selector.
-spec ensure_precheck_source_binary(term(), atom()) -> binary().
%% BT-4173: a String body arrives tagged — unwrap to bytes.
ensure_precheck_source_binary({beamtalk_string, Source}, _ClassName) when is_binary(Source) ->
    Source;
ensure_precheck_source_binary(Source, _ClassName) when is_binary(Source) ->
    Source;
ensure_precheck_source_binary(Source, ClassName) ->
    Error0 = beamtalk_error:new(type_error, ClassName),
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format("precheckCompile:source: expects a String body, got: ~tp", [Source])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

%% Encode a `beamtalk_recheck:result()' as a Beamtalk-facing Dictionary,
%% camelCasing the compound-word keys (`total_candidates' -> `totalCandidates',
%% etc.) — Beamtalk-facing maps use camelCase (`Workspace flush''s
%% `newClasses'), while the internal ADR 0105 result/finding shape is
%% snake_case throughout the Erlang side. Parallels
%% `beamtalk_ws_handler:encode_reload_check_event/1', which does the same
%% translation for the `reload_check' WS push frame's JSON wire shape.
%% `map()`, not `beamtalk_recheck:result()`: beamtalk_runtime has no
%% compile-time dependency on beamtalk_workspace (same reason do_compile_source/4
%% dispatches via erlang:apply/3 rather than a direct call) — the shape is
%% documented above instead of type-referenced.
-spec precheck_result_to_dictionary(map()) -> map().
precheck_result_to_dictionary(#{
    findings := Findings,
    checked := Checked,
    total_candidates := TotalCandidates,
    not_checked := NotChecked,
    cap_note := CapNote,
    checked_owners := CheckedOwners
}) ->
    %% BT-4173: finding text answers Strings (scripts bind `at: #message`
    %% through String methods); counts pass through.
    S = fun beamtalk_primitive:string_from_binary/1,
    MaybeS = fun
        (undefined) -> undefined;
        (Bin) when is_binary(Bin) -> S(Bin)
    end,
    #{
        findings => [precheck_finding_to_dictionary(F) || F <- Findings],
        checked => Checked,
        totalCandidates => TotalCandidates,
        notChecked => NotChecked,
        capNote => MaybeS(CapNote),
        checkedOwners => [S(O) || O <- CheckedOwners]
    }.

-spec precheck_finding_to_dictionary(map()) -> map().
precheck_finding_to_dictionary(#{
    owner := Owner,
    changed_class := ChangedClass,
    selector := Selector,
    classification := Classification,
    severity := Severity,
    category := Category,
    message := Message,
    note := Note,
    sites := Sites,
    start := Start,
    'end' := End
}) ->
    %% BT-4173: text fields answer Strings (see precheck_result_to_dictionary/1).
    S = fun beamtalk_primitive:string_from_binary/1,
    MaybeS = fun
        (undefined) -> undefined;
        (Bin) when is_binary(Bin) -> S(Bin)
    end,
    #{
        owner => S(Owner),
        changedClass => S(ChangedClass),
        selector => S(Selector),
        classification => Classification,
        severity => S(Severity),
        category => MaybeS(Category),
        message => S(Message),
        note => MaybeS(Note),
        sites => Sites,
        start => Start,
        'end' => End
    }.

%% Validate the `Source' argument is a String (binary). Raises a typed error for
%% a non-binary so callers get a clear message instead of a deep crash. The
%% message names the actual selector invoked (derived from `Intent') so callers
%% of `tryCompile:source:' do not see a misleading `compile:source:' message.
-spec ensure_source_binary(atom(), term(), durable | ephemeral, atom()) -> binary().
%% BT-4173: a String body arrives tagged — unwrap to bytes.
ensure_source_binary(_Selector, {beamtalk_string, Source}, _Intent, _ClassName) when
    is_binary(Source)
->
    Source;
ensure_source_binary(_Selector, Source, _Intent, _ClassName) when is_binary(Source) ->
    Source;
ensure_source_binary(_Selector, Source, Intent, ClassName) ->
    Error0 = beamtalk_error:new(type_error, ClassName),
    SelectorName = intent_selector(Intent),
    Msg = beamtalk_error:chars_to_binary(
        io_lib:format("~ts expects a String body, got: ~tp", [SelectorName, Source])
    ),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Msg)).

%% Surface selector for the public message: `compile:source:' is durable,
%% `tryCompile:source:' is ephemeral.
-spec intent_selector(durable | ephemeral) -> binary().
intent_selector(ephemeral) -> <<"tryCompile:source:">>;
intent_selector(durable) -> <<"compile:source:">>.
