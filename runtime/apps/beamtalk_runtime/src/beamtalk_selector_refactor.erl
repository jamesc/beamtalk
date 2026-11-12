%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_selector_refactor).

%%% **DDD Context:** Object System Context

-moduledoc """
Method-rename live-refactoring primitives (ADR 0114 Phase 3).

| Erlang function                | Primitive                                  |
|--------------------------------|--------------------------------------------|
| classRenameSelector/3          | `renameSelector:to:`                       |
| classRenameSelectorIfAbsent/4  | `renameSelector:to:ifAbsent:`              |

Split out of `beamtalk_behaviour_intrinsics` (BT-4703); the code moved
verbatim. Called by name from `beamtalk-codegen`'s
`primitives/behaviour.rs` and reached from `stdlib/src/behaviour.bt`
through `@primitive`.

## Dependencies

Method rename reuses the class-rename site-rewrite pipeline and the removal
target/refusal helpers exported by `beamtalk_class_refactor`, and the
guards in `beamtalk_refactor_support`. The cross-file rewrite itself goes
through `beamtalk_class_refactor:rewrite_class_sites/5`, which reaches the
workspace layer dynamically (see that module's moduledoc).
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    classRenameSelector/3,
    classRenameSelectorIfAbsent/4
]).

-doc """
Rename `OldSelector` to `NewSelector` on the receiver's side, auto-rewriting
only the self/super sends the xref index can prove are structurally safe,
raising if `OldSelector` is not defined locally.

Backs `@primitive "classRenameSelector"` (`Behaviour>>renameSelector:to:`),
modelled on `classRemoveSelector/2`'s shape immediately above and
`classRenameTo/2`'s ordering: resolve the target, validate, discover sites,
mutate, best-effort log, return the receiver. Side follows
`classIncludesSelector/2`'s own convention (a `Self` tagged `class =
'Metaclass'` is class-side; any other class object is instance-side).

## Local-method-table scope only — extensions deliberately excluded

Unlike `classRemoveSelector/2` (which checks the extension registry, ADR
0066, FIRST since an extension shadows a same-named local method),
`OldSelector`/`NewSelector` resolution here goes straight to
`classIncludesSelector/2` — local method table only. ADR 0114's text never
mentions extensions for rename, and an extension method has a different
owner/attribution model entirely that this primitive's
site-discovery (keyed on `Self`'s own class only) has no way to reason
about safely. A class whose `OldSelector` is only an extension (no local
override) is therefore reported `absent` here — exactly like
`removeSelector:` reports `absent` for a selector that resolves nowhere
locally OR as an extension would, MINUS the extension half. A caller wanting
to rename an extension method can still do so manually via `compile:source:`
+ `removeSelector:`.

## Ordering

1. Resolve `OldSelector` locally via `classIncludesSelector/2` — `absent` if
   not found (raises `selector_not_found`, reusing ADR 0112's kind, or runs
   `AbsentBlock` for the `...ifAbsent:` form).
2. Collision refusal: `NewSelector` already locally defined on the same side
   raises `selector_already_exists` — exact hint text from the ADR's own
   worked example.
3. Flushability classification (`capture_class_removal_snapshot/1`, reused
   verbatim) — `renameSelector:to:` never refuses BASED ON THIS
   CLASSIFICATION per the ADR's "Refusal vs flushability" table (stdlib/
   dependency proceed with `flushable: false`); this classification exists
   only to drive `rewrite_class_sites/4`'s existing dynamic-class
   trivial-success gate below, not to block anything by itself. A genuinely
   dynamic (ClassBuilder) class — no tracked `.bt` source at all — IS still
   refused, but earlier, by site discovery itself (step 4): see
   `definition_selector_sites/4`'s own doc for why this is a real,
   documented divergence from the ADR's literal "Allowed" table entry
   (there is no existing mechanism to rewrite a dynamic class's own method
   table without source, mirroring `removeSelector:`'s identical existing
   gap for the same shape).
4. Site discovery (`discover_rename_selector_sites/6`) — read-only: the
   definition's own selector-token span(s), confirmed self/super reference
   sites (owner-in-hierarchy AND override-free), and candidate sites
   (everything else `senders_of/1` finds for this selector) — see that
   function's own doc for the full override-freedom mechanics (ADR 0114 §
   "`renameSelector:to:` auto-rewrites only `self`/`super` sends").
5. Mutate: `rewrite_class_sites/4` (the SAME shared multi-site rewrite
   mechanism `classRenameTo/2` already uses) installs the
   definition + confirmed reference sites transactionally. Unlike class
   rename, there is no separate registry-identity move — a method rename
   never changes what a class is registered under — so there is no
   validate-then-install-identity-then-rewrite ordering concern here; a
   single `rewrite_class_sites/4` call is both validation and installation.
6. Best-effort ChangeLog append (`log_selector_rename/6`), mirroring
   `log_class_rename/4`'s placement — the rename is already live in memory
   by this point, so a logging failure must never surface to the caller.

Raises a structured `#beamtalk_error{}` for the absent-selector (bare form
only)/collision/shape-mismatch refusals and for a `rewrite_class_sites/4`
failure; returns the receiver unchanged on success (unlike `classRenameTo/2`,
a method rename never changes the class's own identity/pid).
""".
-spec classRenameSelector(#beamtalk_object{}, atom(), atom()) -> #beamtalk_object{}.
classRenameSelector(Self, OldSelector, NewSelector) when
    is_atom(OldSelector), is_atom(NewSelector)
->
    case rename_selector(Self, OldSelector, NewSelector, 'renameSelector:to:') of
        renamed ->
            Self;
        absent ->
            beamtalk_error:raise(
                beamtalk_class_refactor:selector_not_found_error(Self, OldSelector)
            )
    end.

-doc """
Rename `OldSelector` to `NewSelector` like `classRenameSelector/3`, but
evaluate `AbsentBlock` instead of raising when `OldSelector` resolves
nowhere locally.

Backs `@primitive "classRenameSelectorIfAbsent"`
(`Behaviour>>renameSelector:to:ifAbsent:`). `AbsentBlock` runs in the
sender's process, exactly like `classRemoveSelectorIfAbsent/3`'s own block
argument (see that function's doc for the full "why the sender's process,
not the receiver's" rationale — the same chain-walk-fallthrough call shape
applies here unchanged).

The collision refusal (`NewSelector` already locally defined) always
raises regardless of this escape hatch — `AbsentBlock` only substitutes for
"`OldSelector` not found", the same scoping `removeSelector:ifAbsent:`
gives its own fallback block.

Returns the receiver on success, or `AbsentBlock`'s value on absence.
""".
-spec classRenameSelectorIfAbsent(#beamtalk_object{}, atom(), atom(), fun(() -> term())) ->
    #beamtalk_object{} | term().
classRenameSelectorIfAbsent(Self, OldSelector, NewSelector, AbsentBlock) when
    is_atom(OldSelector), is_atom(NewSelector)
->
    case rename_selector(Self, OldSelector, NewSelector, 'renameSelector:to:ifAbsent:') of
        renamed -> Self;
        absent -> AbsentBlock()
    end.

%% Shared resolution + rename for classRenameSelector/3 and
%% classRenameSelectorIfAbsent/4. Local-method-table only (see this
%% section's own moduledoc-style comment above for why extensions are out
%% of scope). A collision or a shape-mismatch is a hard raise even from
%% this shared helper — only "OldSelector not found" is reported as `absent`
%% for the two public functions above to handle differently.
%% `Sent` is the selector that reached here, for a refusal's error.
-spec rename_selector(#beamtalk_object{}, atom(), atom(), atom()) -> renamed | absent.
rename_selector(Self, OldSelector, NewSelector, Sent) ->
    ok = beamtalk_refactor_support:refuse_in_own_class_process(erlang:element(4, Self), Sent),
    ok = beamtalk_refactor_support:require_method_capability(
        'renameSelector:to:',
        beamtalk_object_class:class_name_safe(erlang:element(4, Self)),
        OldSelector
    ),
    %% A runtime class-method fun has no source to rewrite, so rename sees
    %% only the compiled table (BT-4438).
    case beamtalk_behaviour_intrinsics:compiled_includes_selector(Self, OldSelector) of
        false ->
            absent;
        true ->
            ok = ensure_rename_selector_collision_free(Self, NewSelector),
            {Side, ClassPid} = beamtalk_class_refactor:removal_target(Self),
            ClassName = beamtalk_object_class:class_name_safe(ClassPid),
            ok = beamtalk_class_refactor:refuse_if_protocol_provided(
                ClassName, Side, OldSelector, rename
            ),
            ClassNameBin = atom_to_binary(ClassName, utf8),
            OldSelectorBin = atom_to_binary(OldSelector, utf8),
            NewSelectorBin = atom_to_binary(NewSelector, utf8),
            %% classification feeds `rewrite_class_sites/4`'s
            %% existing dynamic-class trivial-success gate for the rare edge
            %% case where a class WITH real tracked source still classifies
            %% "dynamic" (see that function's own doc on why the two can
            %% diverge) — `renameSelector:to:` never refuses based on
            %% classification alone (ADR 0114 § "Refusal vs flushability").
            %% The COMMON dynamic-class case (no tracked source at all) is
            %% refused earlier, by `discover_rename_selector_sites/6` itself
            %% — see `definition_selector_sites/4`'s own doc for why.
            Classification = beamtalk_refactor_support:capture_class_removal_snapshot(ClassNameBin),
            case
                discover_rename_selector_sites(
                    ClassName,
                    ClassNameBin,
                    Side,
                    OldSelector,
                    OldSelectorBin,
                    NewSelector,
                    NewSelectorBin
                )
            of
                {error, selector_shape_mismatch} ->
                    beamtalk_error:raise(
                        rename_selector_shape_mismatch_error(
                            ClassName, OldSelectorBin, NewSelectorBin
                        )
                    );
                {error, dynamic_class_no_source} ->
                    beamtalk_error:raise(
                        rename_selector_dynamic_no_source_error(ClassName, OldSelectorBin)
                    );
                {error, {target_selector_collision, Collisions}} ->
                    beamtalk_error:raise(
                        rename_selector_target_collision_error(
                            ClassName, NewSelectorBin, Collisions
                        )
                    );
                {ok, DefinitionSite, ReferenceSites, CandidateSites} ->
                    case
                        beamtalk_class_refactor:rewrite_class_sites(
                            ClassName,
                            DefinitionSite,
                            ReferenceSites,
                            Classification,
                            'renameSelector:to:'
                        )
                    of
                        {ok, RewriteResult} ->
                            log_selector_rename(
                                ClassNameBin,
                                Side,
                                OldSelectorBin,
                                NewSelectorBin,
                                CandidateSites,
                                RewriteResult
                            ),
                            renamed;
                        {error, Reason} ->
                            beamtalk_error:raise(
                                rename_selector_rewrite_failed_error(
                                    ClassName, OldSelectorBin, Reason
                                )
                            )
                    end
            end
    end.

%% Collision refusal (ADR 0114 § Decision): `renameSelector: #a to: #b` when
%% `#b` is already locally defined on the same side raises rather than
%% silently overwriting — exact hint text from the ADR's own worked example.
%% `selector_already_exists` is a NEW error kind (not a reuse of
%% `class_already_exists`, which is a class-identity collision, a
%% different concept) — mirrors `selector_not_found`'s own existing
%% selector-level-vs-class-level kind split (ADR 0112).
-spec ensure_rename_selector_collision_free(#beamtalk_object{}, atom()) -> ok.
ensure_rename_selector_collision_free(Self, NewSelector) ->
    case beamtalk_behaviour_intrinsics:classIncludesSelector(Self, NewSelector) of
        false ->
            ok;
        true ->
            ClassPid = erlang:element(4, Self),
            ClassName = beamtalk_object_class:class_name_safe(ClassPid),
            beamtalk_error:raise(rename_selector_collision_error(ClassName, NewSelector))
    end.

-spec rename_selector_collision_error(atom(), atom()) -> #beamtalk_error{}.
rename_selector_collision_error(ClassName, NewSelector) ->
    ClassBin = atom_to_binary(ClassName, utf8),
    NewBin = atom_to_binary(NewSelector, utf8),
    Error0 = beamtalk_error:new(selector_already_exists, ClassName, NewSelector),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            ClassBin,
            <<" already defines #">>,
            NewBin,
            <<" locally — refusing to overwrite"/utf8>>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        iolist_to_binary([
            <<"removeSelector: #">>,
            NewBin,
            <<" first, or choose a different target name">>
        ])
    ).

%% Review finding on PR #3529: refuses BEFORE any mutation when a confirmed
%% rewrite site's OWNER already independently defines `NewSelectorBin` on
%% the same side — see `confirmed_owners_already_defining/3`'s own doc for
%% the exact silent-dispatch-redirection failure mode this prevents.
-spec rename_selector_target_collision_error(atom(), binary(), [atom()]) -> #beamtalk_error{}.
rename_selector_target_collision_error(ClassName, NewSelectorBin, Collisions) ->
    CollisionBins = [atom_to_binary(Cls, utf8) || Cls <- Collisions],
    CollisionList = iolist_to_binary(lists:join(<<", ">>, CollisionBins)),
    Error0 = beamtalk_error:new(selector_already_exists, ClassName),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary([
            <<"Cannot rename to #">>,
            NewSelectorBin,
            <<" — "/utf8>>,
            CollisionList,
            <<" (in the confirmed-rewrite hierarchy) already defines #">>,
            NewSelectorBin,
            <<"; rewriting would silently redirect dispatch there">>
        ])
    ),
    beamtalk_error:with_hint(
        Error1,
        <<
            "Choose a different target name, or removeSelector: it from "
            "the colliding class(es) first"
        >>
    ).

-spec rename_selector_shape_mismatch_error(atom(), binary(), binary()) -> #beamtalk_error{}.
rename_selector_shape_mismatch_error(ClassName, OldSelectorBin, NewSelectorBin) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Msg = iolist_to_binary([
        <<"Cannot rename #">>,
        OldSelectorBin,
        <<" to #">>,
        NewSelectorBin,
        <<" — "/utf8>>,
        <<
            "the new selector must have the same shape and arity "
            "(unary/binary/keyword, same number of keyword parts) as the "
            "original"
        >>
    ]),
    beamtalk_error:with_message(Error0, Msg).

%% A dynamic (ClassBuilder) class has no tracked `.bt` source to splice a
%% definition rewrite against — see `definition_selector_sites/4`'s own doc
%% for why this is a hard refusal rather than a silent no-op, mirroring
%% `beamtalk_repl_loader:remove_method/3`'s existing "class source not
%% available" behavior for the identical shape.
-spec rename_selector_dynamic_no_source_error(atom(), binary()) -> #beamtalk_error{}.
rename_selector_dynamic_no_source_error(ClassName, OldSelectorBin) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Msg = iolist_to_binary([
        <<"Class source not available for ">>,
        atom_to_binary(ClassName, utf8),
        <<" (cannot rename #">>,
        OldSelectorBin,
        <<")">>
    ]),
    beamtalk_error:with_message(Error0, Msg).

-spec rename_selector_rewrite_failed_error(atom(), binary(), term()) -> #beamtalk_error{}.
rename_selector_rewrite_failed_error(ClassName, _OldSelectorBin, workspace_unavailable) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    beamtalk_error:with_message(
        Error0,
        <<"Workspace not available; renameSelector:to: requires a running workspace">>
    );
%% Defensive backstop (mirrors `rename_rewrite_failed_error/2`'s own
%% `no_sites` clause): reachable only if a class's own classification and
%% its `beamtalk_workspace_meta` tracked-source state have diverged (see
%% `rewrite_class_sites/4`'s own doc for the analogous class-rename case) —
%% not expected in practice since `OldSelector` was just confirmed to
%% resolve locally.
rename_selector_rewrite_failed_error(ClassName, OldSelectorBin, no_sites) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Msg = iolist_to_binary([
        <<"renameSelector: #">>,
        OldSelectorBin,
        <<" found no definition or reference site to rewrite">>
    ]),
    beamtalk_error:with_message(Error0, Msg);
rename_selector_rewrite_failed_error(ClassName, _OldSelectorBin, Reason) ->
    Error0 = beamtalk_error:new(runtime_error, ClassName),
    Msg = beamtalk_error:chars_to_binary(io_lib:format("Could not rename selector: ~tp", [Reason])),
    beamtalk_error:with_message(Error0, Msg).

-doc """
Full site list for a `(ClassName, Side, OldSelector)` rename: the
definition's own selector-token span(s), every CONFIRMED self/super
reference site, and every CANDIDATE site (reported, never rewritten) —
see ADR 0114 § Decision for the full override-freedom rule this
implements.

## The override-freedom check — the crux of this ADR

`recv_kind: self_recv`/`super_recv` plus `owner`-in-`ClassName`'s-subclass-
tree narrowing is NECESSARY but not SUFFICIENT: an override anywhere in
that subclass tree can intercept a same-hierarchy `self`/`super` send
before it ever reaches `ClassName`'s own implementation (late-bound
dispatch starts at the runtime receiver's actual class, not at
`ClassName`). The only structurally sound closure: `beamtalk_xref:
implementors_of/1` (already shipped, ADR 0087) filtered to this
rename's own `Side`, intersected with `ClassName`'s subclass closure
(`beamtalk_class_registry:all_subclasses/1` — the TRANSITIVE closure, unlike
`classRenameTo/2`'s one-level-only `direct_subclasses/1` use for its
superclass-header case, a different reference kind entirely), MINUS
`ClassName` itself. A non-empty intersection means override-freedom FAILS
for the WHOLE selector: every self/super site — including ones owned by
`ClassName` itself — becomes a candidate, never just the sites downstream
of the specific override (a single shared call-site text is executed
polymorphically across every possible receiver class, so safety must hold
for ALL of them, not just the common case).

## Judgment call: keyword selectors and `sites[0]`

The ChangeLog schema's `sites[0]` slot is a SINGLE `rewrite_site()`. A
keyword selector's own definition needs one splice PER keyword part (each
`KeywordPart` has its own independent span,
`beamtalk_core::method_source_walker::find_definition_selector_spans`'s own
doc). This function designates the FIRST keyword part's definition-token
span as `DefinitionSite` (`sites[0]`) and folds every OTHER keyword part's
definition-token span into the `ReferenceSites` list (`sites[1..]`) — the
`rewrite_site()`/splice mechanism itself has no opinion on "why" a site is
being rewritten, so this costs nothing mechanically; it only means a
keyword-selector rename's ChangeLog `sites[1..]` mixes "other keyword
parts of the same definition" with "confirmed reference sites", which the
schema's own comment does not explicitly anticipate. Flagged here as a
real interpretation call the ADR text does not spell out byte-for-byte.

## Judgment call: candidate-site spans

`senders_of/1`'s `other`/`erlang_ffi` rows (and any self/super row demoted
to candidate) carry no byte span — only a `line`. Since `candidate_sites`
are NEVER spliced by flush (`beamtalk_workspace_changelog.erl`'s
own doc comment), the OWNING METHOD's own whole span
(`resolve_method_span`, via `method_token_span/4`) is used as the
candidate's `span` — a "somewhere in this method" pointer for human/agent
review, not a precise token. This does not affect correctness since nothing
ever splices against it.

## Judgment call: definition resolution failure vs. a genuine shape mismatch

`find_definition_selector_spans` returning `{ok, []}` when `OldSelector`
was just confirmed to exist locally is a reliable, cheap signal that
`NewSelector` has a different shape/arity than `OldSelector` (see that
function's own "skip, don't panic" contract) — this is surfaced as a hard
`selector_shape_mismatch` refusal rather than silently proceeding with a
half-renamed definition. A genuine resolution FAILURE (`class_not_found` /
`selector_not_found` / `ambiguous` against the tracked source — e.g. stale
source after an unsynced live patch) is instead treated as the same
accepted completeness gap `classRenameTo/2`'s own `definition_rewrite_site/3`
already documents: degrade to "no definition site" rather than blocking
the whole rename.
""".
-spec discover_rename_selector_sites(
    atom(), binary(), instance | class, atom(), binary(), atom(), binary()
) ->
    {ok, map() | undefined, [map()], [map()]}
    | {error,
        selector_shape_mismatch | dynamic_class_no_source | {target_selector_collision, [atom()]}}.
discover_rename_selector_sites(
    ClassName, ClassNameBin, Side, OldSelector, OldSelectorBin, NewSelector, NewSelectorBin
) ->
    case definition_selector_sites(ClassNameBin, Side, OldSelectorBin, NewSelectorBin) of
        {error, _Reason} = Err ->
            Err;
        {ok, DefinitionSites} ->
            {DefinitionSite, ExtraDefinitionSites} =
                case DefinitionSites of
                    [] -> {undefined, []};
                    [First | Rest] -> {First, Rest}
                end,
            IsClassSide = (Side =:= class),
            RawSites = beamtalk_xref:senders_of(OldSelector),
            {SelfSuperSites, OtherSites} = lists:partition(fun is_self_or_super_site/1, RawSites),
            HierarchySet = sets:from_list([
                ClassName | beamtalk_class_registry:all_subclasses(ClassName)
            ]),
            {InHierarchySelfSuper, OutOfHierarchySelfSuper} = lists:partition(
                fun(#{owner := Owner, class_side := CS} = Site) ->
                    CS =:= IsClassSide andalso sets:is_element(Owner, HierarchySet) andalso
                        not is_unreachable_super_send(Site, ClassName)
                end,
                SelfSuperSites
            ),
            OverrideFree = selector_override_free(
                OldSelector, IsClassSide, ClassName, HierarchySet
            ),
            {ConfirmedSelfSuper, CandidateSelfSuper} =
                case OverrideFree of
                    true -> {InHierarchySelfSuper, OutOfHierarchySelfSuper};
                    false -> {[], SelfSuperSites}
                end,
            case
                confirmed_owners_already_defining(
                    ConfirmedSelfSuper, HierarchySet, ClassName, NewSelector, IsClassSide
                )
            of
                [] ->
                    ReferenceSites =
                        ExtraDefinitionSites ++
                            confirmed_reference_rewrite_sites(
                                ConfirmedSelfSuper, OldSelectorBin, NewSelectorBin
                            ),
                    CandidateSites = candidate_site_maps(OtherSites ++ CandidateSelfSuper),
                    {ok, DefinitionSite, ReferenceSites, CandidateSites};
                Collisions ->
                    {error, {target_selector_collision, Collisions}}
            end
    end.

%% Review finding on PR #3529 (round 2): a confirmed self/super site is
%% about to be rewritten to send `NewSelector` instead of `OldSelector` —
%% but `self`/`super` dispatch is LATE-BOUND to the runtime receiver's
%% actual class, which can be ANY class in `HierarchySet`, not just the
%% confirmed site's own textual owner (this is the identical late-binding
%% fact `selector_override_free/4`'s own doc already relies on for
%% `OldSelector`). A subclass that merely INHERITS a confirmed self-send
%% without declaring one of its own — and independently already defines
%% `NewSelector` — would slip through an owner-scoped check entirely: round
%% 1's fix (checking only `ConfirmedSelfSuper`'s owners) missed exactly
%% this case. Fixed by scanning the SAME `HierarchySet` `selector_override_
%% free/4` scans, not just the sites' owners — mirroring that function
%% arm-for-arm, applied to `NewSelector` instead of `OldSelector`.
%%
%% Gated on `ConfirmedSelfSuper =/= []`: when override-freedom already
%% failed for `OldSelector` (`ConfirmedSelfSuper = []`), no reference site
%% will be rewritten at all — only the definition, which `ensure_rename_
%% selector_collision_free/2` (step 2 of `rename_selector/3`) already
%% guards against `ClassName` itself — so scanning the whole hierarchy in
%% that branch would only produce spurious refusals for renames that touch
%% nothing outside the definition.
%%
%% `ClassName` itself is excluded (`Cls =/= ClassName`) for the same reason
%% `selector_override_free/4` excludes it from its own override scan:
%% already covered by step 2's check, not this one's job to repeat.
-spec confirmed_owners_already_defining([map()], sets:set(atom()), atom(), atom(), boolean()) ->
    [atom()].
confirmed_owners_already_defining([], _HierarchySet, _ClassName, _NewSelector, _IsClassSide) ->
    [];
confirmed_owners_already_defining(
    _ConfirmedSelfSuper, HierarchySet, ClassName, NewSelector, IsClassSide
) ->
    Implementors = beamtalk_xref:implementors_of(NewSelector),
    lists:usort([
        Cls
     || {Cls, CS} <- Implementors,
        CS =:= IsClassSide,
        Cls =/= ClassName,
        sets:is_element(Cls, HierarchySet)
    ]).

-spec is_self_or_super_site(map()) -> boolean().
is_self_or_super_site(#{recv_kind := RecvKind}) ->
    RecvKind =:= self_recv orelse RecvKind =:= super_recv;
is_self_or_super_site(_) ->
    false.

%% Review finding on PR #3529 (round 3): a `super OldSelector` send sited
%% INSIDE the class being renamed itself can never dispatch to that
%% class's own implementation — `beamtalk_dispatch:super/5`'s own doc says
%% it "does NOT check the CurrentClass's method table", always starting
%% the lookup at `CurrentClass`'s superclass instead (`CurrentClass` here
%% being the class where the SENDING method is lexically defined, i.e.
%% this site's `owner` — a compile-time-fixed reference point, unlike
%% `self`'s late-bound receiver class). So when `Owner =:= ClassName`, a
%% `super OldSelector` site is necessarily calling some ANCESTOR's
%% same-named method (the classic override-then-call-`super` idiom) — not
%% `ClassName`'s own, just-renamed one — and rewriting it would silently
%% target a selector the ancestor never defined. Every OTHER owner in
%% `HierarchySet` (any subclass at any depth) is unaffected: `super`
%% there starts its walk at that subclass's OWN immediate superclass,
%% which the override-freedom check already guarantees resolves up to
%% `ClassName`'s implementation with nothing intervening.
%%
%% Excluding this shape entirely (regardless of override-freedom) folds it
%% into `candidate_sites` — human/agent review, never auto-rewritten —
%% exactly like every other structurally-unprovable site this ADR reports.
-spec is_unreachable_super_send(map(), atom()) -> boolean().
is_unreachable_super_send(#{owner := Owner, recv_kind := super_recv}, ClassName) ->
    Owner =:= ClassName;
is_unreachable_super_send(_Site, _ClassName) ->
    false.

%% The override-freedom check (ADR 0114 § Decision): sound iff no class in
%% `HierarchySet`, other than `ClassName` itself, implements `OldSelector`
%% on the same side.
-spec selector_override_free(atom(), boolean(), atom(), sets:set(atom())) -> boolean().
selector_override_free(OldSelector, IsClassSide, ClassName, HierarchySet) ->
    Implementors = beamtalk_xref:implementors_of(OldSelector),
    Overrides = [
        Cls
     || {Cls, CS} <- Implementors,
        CS =:= IsClassSide,
        Cls =/= ClassName,
        sets:is_element(Cls, HierarchySet)
    ],
    Overrides =:= [].

%% Definition-site rewrite target(s) for `(ClassNameBin, Side, OldSelectorBin)`
%% — see `discover_rename_selector_sites/6`'s own doc for the keyword-selector
%% multi-span / resolution-failure-vs-shape-mismatch judgment calls this
%% implements.
%%
%% A dynamic (ClassBuilder) class has NO tracked source at all — unlike
%% `classRenameTo/2`, which has a genuinely source-free path for a
%% freestanding dynamic class (its identity move is a separate,
%% non-source-based `beamtalk_object_class:rename/2` call), there is no
%% equivalent "rename this selector in place" mechanism for a dynamic
%% class's own method table today: `rewrite_sites/2` only knows how to
%% splice `.bt` SOURCE TEXT and recompile, which a sourceless class has
%% none of. This is surfaced as `{error, dynamic_class_no_source}` — a hard
%% refusal, not a silent no-op-success — deliberately mirroring
%% `beamtalk_repl_loader:remove_method/3`'s OWN existing behavior for this
%% exact shape (`removeSelector:` on a dynamic class already fails today
%% with "Class source not available ... (cannot remove method)"; nothing
%% in this codebase currently mutates a dynamic class's method table
%% by selector name). This is a real, documented divergence from ADR
%% 0114's "Refusal vs flushability" table, which lists dynamic classes as
%% simply "Allowed" for `renameSelector:to:` without engineering how a
%% dynamic class's OWN definition gets mutated without source — building
%% that mechanism (a `beamtalk_object_class`-level method-rename primitive,
%% analogous to `rename/2` for class identity) is out of scope for this
%% issue and flagged here rather than silently worked around.
-spec definition_selector_sites(binary(), instance | class, binary(), binary()) ->
    {ok, [map()]} | {error, selector_shape_mismatch | dynamic_class_no_source}.
definition_selector_sites(ClassNameBin, Side, OldSelectorBin, NewSelectorBin) ->
    case beamtalk_class_refactor:current_class_source(ClassNameBin) of
        undefined ->
            {error, dynamic_class_no_source};
        Source ->
            case
                definition_selector_span_call(
                    Source, ClassNameBin, OldSelectorBin, NewSelectorBin, Side
                )
            of
                {ok, []} ->
                    {error, selector_shape_mismatch};
                {ok, Spans} ->
                    SourceFile = beamtalk_class_refactor:class_source_file_for(ClassNameBin),
                    {ok, [
                        #{
                            class => ClassNameBin,
                            source_file => SourceFile,
                            span => #{start => S, 'end' => E},
                            new_text => NewText
                        }
                     || #{start := S, 'end' := E, new_text := NewText} <- Spans
                    ]};
                {error, _Reason} ->
                    %% Resolver couldn't locate the definition against
                    %% currently-tracked source (e.g. stale/unsynced live
                    %% patch) — accepted completeness gap, same category
                    %% `classRenameTo/2`'s `definition_rewrite_site/3`
                    %% already documents; degrade rather than block.
                    {ok, []}
            end
    end.

-spec definition_selector_span_call(binary(), binary(), binary(), binary(), instance | class) ->
    {ok, [map()]} | {error, term()}.
definition_selector_span_call(Source, ClassNameBin, OldSelectorBin, NewSelectorBin, Side) ->
    try
        erlang:apply(beamtalk_compiler, find_definition_selector_spans, [
            Source, ClassNameBin, OldSelectorBin, NewSelectorBin, Side
        ])
    catch
        error:undef -> {error, workspace_unavailable}
    end.

%% Every CONFIRMED self/super site, resolved to exact `rewrite_site()` maps
%% by (a) deduping to distinct `{Owner, ClassSide, Method}` sending-method
%% triples (`senders_of/1` returns one row PER OCCURRENCE, so two sends of
%% `OldSelector` in the same method are two rows that must collapse to one
%% owning-method resolution), then (b) resolving that owning method's own
%% span once and running `find_selector_send_spans` against its sliced text
%% to recover EVERY occurrence within it in one AST walk — this is what
%% makes per-occurrence `line` correlation unnecessary entirely.
-spec confirmed_reference_rewrite_sites([map()], binary(), binary()) -> [map()].
confirmed_reference_rewrite_sites(ConfirmedSelfSuperSites, OldSelectorBin, NewSelectorBin) ->
    Triples = lists:usort([
        {Owner, ClassSide, Method}
     || #{owner := Owner, class_side := ClassSide, method := Method} <- ConfirmedSelfSuperSites
    ]),
    lists:flatmap(
        fun({Owner, IsClassSide, Method}) ->
            owner_method_rewrite_sites(Owner, IsClassSide, Method, OldSelectorBin, NewSelectorBin)
        end,
        Triples
    ).

-spec owner_method_rewrite_sites(atom(), boolean(), atom(), binary(), binary()) -> [map()].
owner_method_rewrite_sites(Owner, IsClassSide, Method, OldSelectorBin, NewSelectorBin) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case beamtalk_class_refactor:current_class_source(OwnerBin) of
        undefined ->
            [];
        Source ->
            OwnerSide = class_side_atom(IsClassSide),
            MethodBin = atom_to_binary(Method, utf8),
            case
                beamtalk_class_refactor:method_token_span(Source, OwnerBin, MethodBin, OwnerSide)
            of
                {ok, {MStart, MEnd}} ->
                    MethodText = binary:part(Source, MStart, MEnd - MStart),
                    SourceFile = beamtalk_class_refactor:class_source_file_for(OwnerBin),
                    Occurrences = selector_send_spans_call(
                        MethodText, OldSelectorBin, NewSelectorBin
                    ),
                    [
                        #{
                            class => OwnerBin,
                            source_file => SourceFile,
                            span => #{start => MStart + S, 'end' => MStart + E},
                            new_text => NewText
                        }
                     || SpansForOneOccurrence <- Occurrences,
                        #{start := S, 'end' := E, new_text := NewText} <- SpansForOneOccurrence
                    ];
                not_found ->
                    []
            end
    end.

-spec selector_send_spans_call(binary(), binary(), binary()) -> [[map()]].
selector_send_spans_call(MethodText, OldSelectorBin, NewSelectorBin) ->
    try
        erlang:apply(beamtalk_compiler, find_selector_send_spans, [
            MethodText, OldSelectorBin, NewSelectorBin
        ])
    of
        {ok, Occurrences} -> Occurrences;
        {error, _Reason, _Message} -> []
    catch
        error:undef -> []
    end.

%% Every CANDIDATE site (`other`/`erlang_ffi` sends, plus any self/super
%% site demoted to candidate) reduced to a `candidate_site()`-shaped map —
%% see `discover_rename_selector_sites/6`'s own doc for why the OWNING
%% METHOD's whole span is used rather than a precise token span. Deduped to
%% distinct `{Owner, ClassSide, Method}` triples first, same rationale as
%% `confirmed_reference_rewrite_sites/3` (multiple occurrences in one
%% method need only one candidate entry — nothing here is ever spliced
%% per-occurrence anyway). A triple whose owner has no tracked source or no
%% resolvable `source_file` is dropped: `candidate_site()`'s `source_file`
%% field is non-optional, and there is nothing meaningful to point at.
-spec candidate_site_maps([map()]) -> [map()].
candidate_site_maps(Sites) ->
    Triples = lists:usort([
        {Owner, ClassSide, Method}
     || #{owner := Owner, class_side := ClassSide, method := Method} <- Sites
    ]),
    lists:filtermap(fun candidate_site_map/1, Triples).

-spec candidate_site_map({atom(), boolean(), atom()}) -> {true, map()} | false.
candidate_site_map({Owner, IsClassSide, Method}) ->
    OwnerBin = atom_to_binary(Owner, utf8),
    case beamtalk_class_refactor:current_class_source(OwnerBin) of
        undefined ->
            false;
        Source ->
            OwnerSide = class_side_atom(IsClassSide),
            MethodBin = atom_to_binary(Method, utf8),
            case
                beamtalk_class_refactor:method_token_span(Source, OwnerBin, MethodBin, OwnerSide)
            of
                {ok, {MStart, MEnd}} ->
                    case beamtalk_class_refactor:class_source_file_for(OwnerBin) of
                        undefined ->
                            false;
                        SourceFile ->
                            {true, #{
                                source_file => SourceFile,
                                span => #{start => MStart, 'end' => MEnd}
                            }}
                    end;
                not_found ->
                    false
            end
    end.

%% `beamtalk_xref:site()`'s `class_side` field is a boolean (`true` =
%% class-side, `false` = instance-side) — translate to the `instance |
%% class` atom `resolve_method_span/4`/`find_selector_send_spans`'s wire
%% shape expects. Mirrors `method_body_rewrite_sites/5`'s own inline
%% translation for the class-rename case.
-spec class_side_atom(boolean()) -> instance | class.
class_side_atom(true) -> class;
class_side_atom(false) -> instance.

%% Best-effort ChangeLog append after a successful `renameSelector:to:` (ADR
%% 0114 § ChangeLog schema, `kind: "rename-method"`) — mirrors
%% `log_class_rename/4`'s placement and self-swallowing failure handling:
%% the rename is already live in memory by this point, so a logging failure
%% must never surface to the caller. `candidate_sites` passes through
%% verbatim, exactly like `log_class_rename/4`'s own `Spec` (there are none
%% for `'rename-class'`, this is `'rename-method'`'s own tier).
-spec log_selector_rename(binary(), instance | class, binary(), binary(), [map()], map()) -> ok.
log_selector_rename(
    ClassNameBin, Side, OldSelectorBin, NewSelectorBin, CandidateSites, RewriteResult
) ->
    {Author, AuthorKind} = beamtalk_refactor_support:current_author_context(),
    Spec = #{
        kind => 'rename-method',
        class => ClassNameBin,
        selector => NewSelectorBin,
        old_selector => OldSelectorBin,
        side => Side,
        candidate_sites => CandidateSites,
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
