%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hierarchy_docs).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared hierarchy-walking helpers for documentation/reflection lookups.

`beamtalk_stdlib`'s `beamtalk_interface` (backs the programmatic `Beamtalk
help:` / `Beamtalk help:selector:` reflection API) and `beamtalk_workspace`'s
`beamtalk_repl_docs` (backs the REPL's `:help` command) each hand-rolled an
identical set of helpers for walking the superclass chain to resolve
documentation — literally byte-identical apart from which lookup mechanism
(direct `gen_server:call` vs the `beamtalk_runtime_api` facade) each used to
reach the same class-registry/gen_server state.

Every per-level read here is a message-free read of the state each class
publishes to ETS (`beamtalk_object_class:instance_methods_safe/1`,
`superclass_safe/1`, ...), never a `gen_server:call` into the class process
(BT-3832): these run inside a class handler whenever user code calls
`Beamtalk help:` from a class method, and a blocking call into another class
process there deadlocks two busy classes until the 5s timeout.

Both apps depend on `beamtalk_runtime` (see the `applications` list in each
app's `.app.src`: `beamtalk_stdlib` depends on `beamtalk_runtime`, and
`beamtalk_workspace` depends on both), so this module lives here as the
single shared implementation both callers delegate to, rather than the
mirrored copies + "mirrors ..." comments that used to stand in for a test.

Each helper delegates its walk (depth guard, cycle warning, advance-to-
superclass) to `beamtalk_hierarchy:walk_ancestors/3`, supplying only the
per-ancestor probe. Depth-exhaustion is intentionally uniform across all of
them: an audit found `beamtalk_interface`'s copies silently swallowing a
hierarchy cycle (returning the receiver's own class / an empty map) while
`beamtalk_repl_docs`'s copies warned via `?LOG_WARNING` — a hierarchy cycle
was diagnosable via `:help` in the REPL but invisible via the programmatic
`Beamtalk help:` call. Every helper here now logs on depth exhaustion,
matching `beamtalk_hierarchy`'s own convention.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    find_defining_class/2,
    find_defining_class_method/2,
    collect_flattened_methods/2,
    collect_flattened_class_methods/2,
    metaclass_method_doc/1
]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Find which class in the hierarchy (starting at ClassPid) defines Selector as
an instance method. Returns the defining class's name atom.

If Selector is not found anywhere in the chain, or the walk exhausts
`?MAX_HIERARCHY_DEPTH` (a hierarchy cycle), returns ClassPid's own class name
and — in the cycle case only — logs a `?LOG_WARNING`.
""".
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining(ClassPid, Selector, find_defining_class, fun(Pid) ->
        beamtalk_object_class:instance_methods_safe(Pid)
    end).

-doc """
Find which class in the hierarchy (starting at ClassPid) defines Selector as
a class-side method. Same walk as `find_defining_class/2` (shared
`find_defining/4`) for the class side — probes the published local
class-method selectors at each level instead of the instance method table.
""".
-spec find_defining_class_method(pid(), atom()) -> atom().
find_defining_class_method(ClassPid, Selector) ->
    find_defining(ClassPid, Selector, find_defining_class_method, fun(Pid) ->
        beamtalk_object_class:local_class_method_selectors_safe(Pid)
    end).

%% The one "which ancestor defines `Selector`?" walk behind both public
%% finders. `SelectorsFun(Pid)` is the membership probe (the level's instance
%% selectors, or its local class-method selectors); `Label` names the walk in
%% the cycle warning. A level that does not define it advances to its
%% superclass, and the chain's end (no superclass, or an unregistered one)
%% answers the last level reached. A cycle answers the receiver's own class.
-spec find_defining(pid(), atom(), atom(), fun((pid()) -> [atom()])) -> atom().
find_defining(ClassPid, Selector, Label, SelectorsFun) ->
    ReceiverName = beamtalk_object_class:class_name_safe(ClassPid),
    StepFun = fun(CurrentPid, _Depth) ->
        CurrentName = beamtalk_object_class:class_name_safe(CurrentPid),
        case lists:member(Selector, SelectorsFun(CurrentPid)) of
            true ->
                {found, CurrentName};
            false ->
                case superclass_level(CurrentPid) of
                    {SuperPid, _SuperName} -> {next, SuperPid};
                    none -> {found, CurrentName}
                end
        end
    end,
    Ctx = #{
        start => ReceiverName,
        selector => Selector,
        node_name => fun beamtalk_object_class:class_name_safe/1
    },
    case beamtalk_hierarchy:walk_or_warn(ClassPid, StepFun, Label, Ctx) of
        {found, DefiningClass} ->
            DefiningClass;
        {cycle, _CyclePid} ->
            ReceiverName;
        not_found ->
            %% Unreachable: StepFun above always resolves to {found, _} — a
            %% `none` superclass or an unregistered ancestor is translated to
            %% a terminal {found, CurrentName}, never a bare `none` node.
            erlang:error({unreachable, not_found, ReceiverName, Selector})
    end.

%% A class process's superclass as `{Pid, Name}`, or `none` when it has no
%% superclass or the superclass is not registered (the chain ends there).
-spec superclass_level(pid()) -> {pid(), atom()} | none.
superclass_level(ClassPid) ->
    case beamtalk_object_class:superclass_safe(ClassPid) of
        none ->
            none;
        SuperName ->
            case beamtalk_class_registry:whereis_class(SuperName) of
                undefined -> none;
                SuperPid -> {SuperPid, SuperName}
            end
    end.

-doc """
Walk the class hierarchy from ClassName/ClassPid upward, building a
flattened `#{Selector => {DefiningClass, MethodInfo}}` map of instance
methods. Local methods shadow inherited ones (Smalltalk-style override):
a selector redefined by a subclass is tagged with the subclass as its
`DefiningClass`, never the ancestor it shadows.

If the walk exhausts `?MAX_HIERARCHY_DEPTH` (a hierarchy cycle), returns the
partial map folded up through the ancestors actually visited before the
guard tripped — not an empty map — and logs a `?LOG_WARNING`
naming the ancestor where the cycle was detected.
""".
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    flatten_walk(ClassName, ClassPid, collect_flattened_methods, fun(Name, Pid) ->
        Local = beamtalk_object_class:local_instance_method_infos_safe(Pid),
        maps:map(fun(_Sel, Info) -> {Name, Info} end, Local)
    end).

-doc """
Walk the class hierarchy collecting class-side methods.
Returns #{Selector => DefiningClass} — local methods shadow inherited ones.

On depth exhaustion (`?MAX_HIERARCHY_DEPTH`, a hierarchy cycle) returns the
partial map folded up through the ancestors actually visited before the
guard tripped — not `#{}` — and logs a `?LOG_WARNING` naming the
ancestor where the cycle was detected.

Moved here from `beamtalk_repl_docs` (its only prior caller) so
`beamtalk_repl_ops_dev`'s "inherited-methods" ws op can reuse the same
class-side walk instead of a second copy; it shares `flatten_walk/4` with
`collect_flattened_methods/2`.
""".
-spec collect_flattened_class_methods(atom(), pid()) -> #{atom() => atom()}.
collect_flattened_class_methods(ClassName, ClassPid) ->
    flatten_walk(ClassName, ClassPid, collect_flattened_class_methods, fun(Name, Pid) ->
        maps:from_list([
            {Sel, Name}
         || Sel <- beamtalk_object_class:local_class_method_selectors_safe(Pid)
        ])
    end).

%% The one root-to-leaf fold behind both `collect_flattened_*` functions.
%% `LocalFun(Name, Pid)` is one level's own contribution. The accumulator
%% already reflects every closer (lower-depth) ancestor winning over farther
%% ones, so it goes second in `maps:merge/2` as each farther level is folded in.
-spec flatten_walk(atom(), pid(), atom(), fun((atom(), pid()) -> map())) -> map().
flatten_walk(ClassName, ClassPid, Label, LocalFun) ->
    StepFun = fun({CurrentName, CurrentPid, AccMap}, _Depth) ->
        NewAcc = maps:merge(LocalFun(CurrentName, CurrentPid), AccMap),
        case superclass_level(CurrentPid) of
            {SuperPid, SuperName} -> {next, {SuperName, SuperPid, NewAcc}};
            none -> {found, NewAcc}
        end
    end,
    Ctx = #{start => ClassName, node_name => fun({Name, _Pid, _Acc}) -> Name end},
    case beamtalk_hierarchy:walk_or_warn({ClassName, ClassPid, #{}}, StepFun, Label, Ctx) of
        {found, Result} ->
            Result;
        {cycle, {_CycleName, _CyclePid, PartialAcc}} ->
            PartialAcc;
        not_found ->
            %% Unreachable: the step always resolves to {found, _} or {next, _}.
            erlang:error({unreachable, not_found, ClassName})
    end.

-doc "Lookup doc text for the hardcoded Metaclass class-side methods.".
-spec metaclass_method_doc(binary()) -> {ok, binary()} | not_found.
metaclass_method_doc(<<"new">>) ->
    {ok, <<"Create a new instance of the class.">>};
metaclass_method_doc(<<"spawn">>) ->
    {ok, <<"Create a new actor instance. Returns an actor reference.">>};
metaclass_method_doc(<<"spawnWith:">>) ->
    {ok, <<"Create a new actor with initial state from a Dictionary.">>};
metaclass_method_doc(_) ->
    not_found.
