%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_hierarchy_docs).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared hierarchy-walking helpers for documentation/reflection lookups (BT-3087).

`beamtalk_stdlib`'s `beamtalk_interface` (backs the programmatic `Beamtalk
help:` / `Beamtalk help:selector:` reflection API) and `beamtalk_workspace`'s
`beamtalk_repl_docs` (backs the REPL's `:help` command) each hand-rolled an
identical set of helpers for walking the superclass chain to resolve
documentation — literally byte-identical apart from which lookup mechanism
(direct `gen_server:call` vs the `beamtalk_runtime_api` facade) each used to
reach the same class-registry/gen_server state.

Both apps depend on `beamtalk_runtime` (see the `applications` list in each
app's `.app.src`: `beamtalk_stdlib` depends on `beamtalk_runtime`, and
`beamtalk_workspace` depends on both), so this module lives here as the
single shared implementation both callers delegate to, rather than the
mirrored copies + "mirrors ..." comments that used to stand in for a test.

Each helper delegates its walk (depth guard, cycle warning, advance-to-
superclass) to `beamtalk_hierarchy:walk_ancestors/3`, supplying only the
per-ancestor probe. Depth-exhaustion is intentionally uniform across all of
them: BT-3087 found `beamtalk_interface`'s copies silently swallowing a
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
    ReceiverName = beamtalk_object_class:class_name(ClassPid),
    StepFun = fun(CurrentPid, _Depth) ->
        CurrentName = beamtalk_object_class:class_name(CurrentPid),
        case gen_server:call(CurrentPid, {method, Selector}, 5000) of
            nil ->
                case gen_server:call(CurrentPid, superclass, 5000) of
                    none ->
                        {found, CurrentName};
                    SuperName ->
                        case beamtalk_class_registry:whereis_class(SuperName) of
                            undefined -> {found, CurrentName};
                            SuperPid -> {next, SuperPid}
                        end
                end;
            _MethodInfo ->
                {found, CurrentName}
        end
    end,
    case beamtalk_hierarchy:walk_ancestors(ClassPid, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, DefiningClass} ->
            DefiningClass;
        max_depth_exceeded ->
            ?LOG_WARNING(
                "find_defining_class: max hierarchy depth ~p exceeded at ~p for selector ~p — possible cycle",
                [?MAX_HIERARCHY_DEPTH, ReceiverName, Selector],
                #{domain => [beamtalk, runtime]}
            ),
            ReceiverName;
        not_found ->
            %% Unreachable: StepFun above always resolves to {found, _} — a
            %% `none` superclass or an unregistered ancestor is translated to
            %% a terminal {found, CurrentName}, never a bare `none` node.
            erlang:error({unreachable, not_found, ReceiverName, Selector})
    end.

-doc """
Find which class in the hierarchy (starting at ClassPid) defines Selector as
a class-side method. Mirrors `find_defining_class/2` for the class side —
walks `get_local_class_methods` at each level instead of the instance
method table.
""".
-spec find_defining_class_method(pid(), atom()) -> atom().
find_defining_class_method(ClassPid, Selector) ->
    ReceiverName = beamtalk_object_class:class_name(ClassPid),
    StepFun = fun(CurrentPid, _Depth) ->
        CurrentName = beamtalk_object_class:class_name(CurrentPid),
        LocalClassMethods = gen_server:call(CurrentPid, get_local_class_methods, 5000),
        case maps:is_key(Selector, LocalClassMethods) of
            true ->
                {found, CurrentName};
            false ->
                case gen_server:call(CurrentPid, superclass, 5000) of
                    none ->
                        {found, CurrentName};
                    SuperName ->
                        case beamtalk_class_registry:whereis_class(SuperName) of
                            undefined -> {found, CurrentName};
                            SuperPid -> {next, SuperPid}
                        end
                end
        end
    end,
    case beamtalk_hierarchy:walk_ancestors(ClassPid, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, DefiningClass} ->
            DefiningClass;
        max_depth_exceeded ->
            ?LOG_WARNING(
                "find_defining_class_method: max hierarchy depth ~p exceeded at ~p for selector ~p — possible cycle",
                [?MAX_HIERARCHY_DEPTH, ReceiverName, Selector],
                #{domain => [beamtalk, runtime]}
            ),
            ReceiverName;
        not_found ->
            %% Unreachable: see find_defining_class/2.
            erlang:error({unreachable, not_found, ReceiverName, Selector})
    end.

-doc """
Walk the class hierarchy from ClassName/ClassPid upward, building a
flattened `#{Selector => {DefiningClass, MethodInfo}}` map of instance
methods. Local methods shadow inherited ones (Smalltalk-style override):
a selector redefined by a subclass is tagged with the subclass as its
`DefiningClass`, never the ancestor it shadows.

Returns `#{}` if the walk exhausts `?MAX_HIERARCHY_DEPTH` (a hierarchy
cycle), logging a `?LOG_WARNING`.
""".
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    StepFun = fun({CurrentName, CurrentPid, AccMap}, _Depth) ->
        {ok, LocalMethods} = gen_server:call(CurrentPid, get_instance_methods, 5000),
        LocalFlat = maps:map(fun(_Sel, Info) -> {CurrentName, Info} end, LocalMethods),
        %% AccMap already reflects every closer (lower-depth) ancestor
        %% winning over farther ones; putting it second keeps that
        %% invariant as this (farther) level's LocalFlat is folded in.
        NewAcc = maps:merge(LocalFlat, AccMap),
        case gen_server:call(CurrentPid, superclass, 5000) of
            none ->
                {found, NewAcc};
            SuperName ->
                case beamtalk_class_registry:whereis_class(SuperName) of
                    undefined -> {found, NewAcc};
                    SuperPid -> {next, {SuperName, SuperPid, NewAcc}}
                end
        end
    end,
    case
        beamtalk_hierarchy:walk_ancestors(
            {ClassName, ClassPid, #{}}, StepFun, ?MAX_HIERARCHY_DEPTH
        )
    of
        {found, Result} ->
            Result;
        max_depth_exceeded ->
            ?LOG_WARNING(
                "collect_flattened_methods: max hierarchy depth ~p exceeded at ~p — possible cycle",
                [?MAX_HIERARCHY_DEPTH, ClassName],
                #{domain => [beamtalk, runtime]}
            ),
            #{};
        not_found ->
            %% Unreachable: see find_defining_class/2.
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
