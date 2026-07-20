%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_alias_xref).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Alias-name → dependent-class index (ADR 0108 hot-reload re-check trigger,
BT-2899).

## Why this exists

`beamtalk_xref` (ADR 0087) is a **selector**-keyed call-site index — it
answers "which classes send selector S", built from `method_xref` entries
baked into a class's compiled module at BEAM-compile time. A `::` annotation
naming a type alias is not a message send, so `beamtalk_xref:senders_of/1`
has nothing to look up for it (the same gap ADR 0107's `trigger_leaf_change/1`
hit for leaf-status changes — see `beamtalk_recheck.erl`'s moduledoc). Unlike
that trigger, which has no natural lookup key and falls back to sweeping
every live class's source, **the alias name is a natural key**: this module
is a lightweight `alias name -> [dependent class name]` index, so a live
alias redefinition's re-check can query straight to its candidate set instead
of recompiling the whole image (ADR 0108 Implementation: "record alias-name
-> annotation-site edges at resolution time ... re-check only dependents,
not every live annotation site").

## Populated from the compiler-port response, not from codegen

Unlike `beamtalk_xref` (populated from a `method_xref` list baked into
`register_class/0`'s generated Core Erlang — a real BEAM-codegen surface),
aliases erase entirely at resolution time (ADR 0108 Semantics) and have no
runtime representation for codegen to attach anything to. Instead, every
class-defining compile (`compile`/`compile_method` — see
`crates/beamtalk-compiler-port/src/main.rs`'s `handle_compile`/
`handle_compile_method`) now returns a `referenced_aliases` field: the sorted
set of alias names that compile's own annotations transitively depended on
(`AnalysisResult::referenced_aliases`, built during type-checking by
`resolve_type_annotation_with_alias_deps` — see that Rust function's doc for
why the set already spans the full transitive expansion walk: resolving `p
:: B` where `type B = A | #z` reports both `B` and `A`). `beamtalk_repl_loader`
reads this field at class-install time and calls `register_class/2` here —
exactly mirroring how `beamtalk_object_class:init/1` reads `method_xref` out
of `__beamtalk_meta/0` and calls `beamtalk_xref:register_class/2`, just from
a compiler-port response instead of installed BEAM metadata.

## Whole-set replacement, not incremental — except REPL-inline compiles (BT-2955)

`register_class/2` takes the *complete* current set of aliases `ClassNameBin`
references, not a delta — a class that stops referencing an alias (the
annotation was edited away) must stop appearing in that alias's dependent
set on the very next reload, or a later redefinition of the no-longer-
referenced alias would re-check a class that no longer has anything to do
with it. The reverse index (`class_to_aliases`) exists purely to make this
cheap: computing "what did this class use to depend on" from the forward
index alone would mean scanning every alias's dependent set on every reload.

This whole-set-replace contract only holds when a call site has the
*complete* picture of aliases a class references — true for a real file
compile (`compile_file_core/4`), which sees every `type Name = ...` the file
itself declares, but not for a REPL-inline class/protocol redefinition
(`beamtalk_repl_compiler:compile_class_definition_result/2`,
`compile_protocol_definition_result/2`): those only ever see aliases
declared *at the REPL*, so a file-local alias (declared in the same file the
class originally came from, not the REPL session) is invisible to them.
Calling `register_class/2` from there would silently clobber a real edge a
prior file `:load` registered (BT-2955's concrete `stdlib/src/Ets.bt` +
`EtsTableType` repro). `register_class_additive/2` exists for exactly those
two call sites: additive-only (never removes an edge), trading "a class's
dependency set can only grow across a session, never accurately shrink from
a REPL-inline redefinition alone" for "never silently lose a real edge" —
the same "over-approximation is safe, under-approximation is not" risk
tolerance the rest of this module already accepts (see "Recorded
limitation" below).

## Recorded limitation

Scoped to whichever classes have been compiled (via `compile`/
`compile_method`) this session — a class whose source was never reloaded/
recompiled this session (declared before the workspace started, or loaded
through some other path) has no entry here, the same "session-only, live
classes only" limitation `beamtalk_xref`, `beamtalk_workspace_meta`, and
every other ADR 0105/0108 hot-reload mechanism already documents and accepts.

## Session-only, never persisted

Mirrors every other ADR 0105/0108 store: state lives in this gen_server's
`#state{}`, supervised under `beamtalk_workspace_sup`. `clear/0` gives tests
(and a future `Workspace changes revert:`) an explicit reset without a
restart.
""".

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, register_class/2, register_class_additive/2, dependents_of/1, clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    %% alias name -> set of dependent class names.
    alias_to_classes = #{} :: #{binary() => sets:set(binary())},
    %% class name -> set of alias names it currently references — the
    %% reverse index `register_class/2`'s whole-set replacement needs to
    %% cheaply undo a class's *previous* generation's edges.
    class_to_aliases = #{} :: #{binary() => sets:set(binary())}
}).

%%====================================================================
%% API
%%====================================================================

-doc "Start the alias-xref index (one per workspace).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Record that `ClassNameBin`'s current (just-installed) generation
transitively references exactly `AliasNames` (the compiler port's
`referenced_aliases` response field for that compile — already sorted/
deduplicated, but this accepts any order/duplication defensively). Replaces
whatever this class's previous generation recorded — see the moduledoc's
"Whole-set replacement" section for why a delta isn't accepted here.

A class newly referencing zero aliases (`AliasNames = []`) is legal and
common (the overwhelmingly typical case: no alias usage at all) — it simply
removes the class from every alias's dependent set without adding it back
anywhere.

Fire-and-forget cast, mirroring `beamtalk_compiler_server:register_class/2`:
silently dropped if this gen_server is not running — this module is only
started in REPL mode (see `beamtalk_workspace_sup`'s `repl_child_specs/6`),
so every `compile_file_core/4` call in run mode (`repl=false`) would
otherwise crash the compile on `noproc`.
""".
-spec register_class(binary(), [binary()]) -> ok.
register_class(ClassNameBin, AliasNames) when is_binary(ClassNameBin), is_list(AliasNames) ->
    try
        gen_server:cast(?MODULE, {register_class, ClassNameBin, sets:from_list(AliasNames)})
    catch
        _:_ -> ok
    end,
    ok.

-doc """
Additive-only sibling of `register_class/2` for REPL-inline compiles
(BT-2955): records that `ClassNameBin` also references `AliasNames`, without
touching any alias edge already recorded for it. Never removes an edge —
the "whole-set replacement" behaviour `register_class/2` documents does not
apply here.

Why this exists: a REPL-inline redefinition (`beamtalk_repl_compiler:
compile_class_definition_result/2`, `compile_protocol_definition_result/2`)
only ever sees aliases declared *at the REPL* — a `type Name = ...` alias
declared in the *same source file* as the class/protocol being redefined
(e.g. `stdlib/src/Ets.bt`'s `EtsTableType`, referenced by `Ets`'s own class
method signature in that same file) is invisible to that compile's
`AliasRegistry`, so its `referenced_aliases` response silently omits it —
not because the class stopped using it, but because this compile had no way
to know. Calling `register_class/2` with that incomplete set would clobber
the real edge a prior `:load` of the file registered. Using this additive
variant instead means a REPL-inline redefinition can only ever *under-report
an addition*, never *falsely report a removal* — matching this module's
documented "over-approximation is safe, under-approximation is not" risk
tolerance (an edge that lingers after a class genuinely stops referencing an
alias just means one extra, harmless re-check candidate; a lost edge means a
live alias redefinition silently skips checking a real dependent).

`compile_file_core/4` (the file-compile path, which always has the complete
picture for its own file) keeps calling `register_class/2` — it remains the
sole authority that can ever retire a stale edge.
""".
-spec register_class_additive(binary(), [binary()]) -> ok.
register_class_additive(ClassNameBin, AliasNames) when
    is_binary(ClassNameBin), is_list(AliasNames)
->
    try
        gen_server:cast(
            ?MODULE, {register_class_additive, ClassNameBin, sets:from_list(AliasNames)}
        )
    catch
        _:_ -> ok
    end,
    ok.

-doc """
The dependent class names currently recorded against `AliasNameBin`, sorted
for deterministic ordering. `[]` for an alias name with no recorded
dependents this session (never referenced by any compiled class, or a name
that was but no longer is) — never an error, matching every other ADR 0105/
0108 lookup's "absence is not exceptional" convention.
""".
-spec dependents_of(binary()) -> [binary()].
dependents_of(AliasNameBin) when is_binary(AliasNameBin) ->
    try
        gen_server:call(?MODULE, {dependents_of, AliasNameBin})
    catch
        exit:{noproc, _} -> [];
        exit:{timeout, _} -> []
    end.

-doc "Clear every recorded edge (tests; a future `Workspace changes revert:`).".
-spec clear() -> ok.
clear() ->
    try
        gen_server:call(?MODULE, clear)
    catch
        exit:{noproc, _} -> ok;
        exit:{timeout, _} -> ok
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    {ok, #state{}}.

handle_call(
    {dependents_of, AliasNameBin}, _From, State = #state{alias_to_classes = AliasToClasses}
) ->
    Dependents =
        case maps:find(AliasNameBin, AliasToClasses) of
            {ok, ClassSet} -> lists:sort(sets:to_list(ClassSet));
            error -> []
        end,
    {reply, Dependents, State};
handle_call(clear, _From, _State) ->
    {reply, ok, #state{}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_class, ClassNameBin, NewAliasSet}, State) ->
    {noreply, apply_class_registration(replace, ClassNameBin, NewAliasSet, State)};
handle_cast({register_class_additive, ClassNameBin, NewAliasSet}, State) ->
    {noreply, apply_class_registration(additive, ClassNameBin, NewAliasSet, State)};
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

%% Shared forward/reverse-index bookkeeping for `register_class/2` (`replace`)
%% and `register_class_additive/2` (`additive`) — the two modes differ only
%% in whether a name present in the class's *old* alias set but absent from
%% `NewAliasSet` is retired: `replace` computes and removes it (whole-set
%% replacement); `additive` never does (the final set is the union of old
%% and new, so an edge can only grow). See those two functions' docs for the
%% full rationale.
-spec apply_class_registration(replace | additive, binary(), sets:set(binary()), #state{}) ->
    #state{}.
apply_class_registration(
    Mode,
    ClassNameBin,
    NewAliasSet,
    State = #state{alias_to_classes = AliasToClasses, class_to_aliases = ClassToAliases}
) ->
    OldAliasSet = maps:get(ClassNameBin, ClassToAliases, sets:new()),
    AddedAliases = sets:to_list(sets:subtract(NewAliasSet, OldAliasSet)),
    AliasToClasses1 =
        case Mode of
            replace ->
                RemovedAliases = sets:to_list(sets:subtract(OldAliasSet, NewAliasSet)),
                lists:foldl(
                    fun(AliasName, Acc) -> remove_dependent(AliasName, ClassNameBin, Acc) end,
                    AliasToClasses,
                    RemovedAliases
                );
            additive ->
                AliasToClasses
        end,
    AliasToClasses2 = lists:foldl(
        fun(AliasName, Acc) -> add_dependent(AliasName, ClassNameBin, Acc) end,
        AliasToClasses1,
        AddedAliases
    ),
    FinalAliasSet =
        case Mode of
            replace -> NewAliasSet;
            additive -> sets:union(OldAliasSet, NewAliasSet)
        end,
    ClassToAliases1 =
        case sets:is_empty(FinalAliasSet) of
            true -> maps:remove(ClassNameBin, ClassToAliases);
            false -> ClassToAliases#{ClassNameBin => FinalAliasSet}
        end,
    State#state{alias_to_classes = AliasToClasses2, class_to_aliases = ClassToAliases1}.

-spec add_dependent(binary(), binary(), #{binary() => sets:set(binary())}) ->
    #{binary() => sets:set(binary())}.
add_dependent(AliasName, ClassNameBin, AliasToClasses) ->
    ClassSet = maps:get(AliasName, AliasToClasses, sets:new()),
    AliasToClasses#{AliasName => sets:add_element(ClassNameBin, ClassSet)}.

-spec remove_dependent(binary(), binary(), #{binary() => sets:set(binary())}) ->
    #{binary() => sets:set(binary())}.
remove_dependent(AliasName, ClassNameBin, AliasToClasses) ->
    case maps:find(AliasName, AliasToClasses) of
        error ->
            AliasToClasses;
        {ok, ClassSet} ->
            NewSet = sets:del_element(ClassNameBin, ClassSet),
            case sets:is_empty(NewSet) of
                true -> maps:remove(AliasName, AliasToClasses);
                false -> AliasToClasses#{AliasName => NewSet}
            end
    end.
