%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_navigation).

%%% **DDD Context:** Actor System Context

-moduledoc """
Runtime shim for supervision-tree introspection (ADR 0092).

A thin, read-only wrapper over OTP's `supervisor:which_children/1` and
`erlang:process_info/2` that walks the live workspace supervision tree and
returns a flat list of immutable `SupervisionNode` value records (tagged
maps). It adds **no** new bookkeeping process — no gen_server, no ETS, no
maintained index — it queries OTP at snapshot time and freezes the result.

## Snapshot semantics (ADR 0092 §4)

Construction walks `which_children` top-down once and freezes the structure
(pids, names, kinds, child counts, adjacency) into immutable records. The walk
is *best-effort point-in-time*: it is assembled from many separate
`which_children` calls and the tree can mutate between them, so a child can
appear or vanish during construction. A supervisor that dies mid-walk yields a
*partial* tree, never a raise (every `which_children` is guarded).

## Node shape

Each node is a `SupervisionNode` value (a tagged map, minted here so the
stdlib class — BT-2427 — needs no wrapping layer):

```
#{
  '$beamtalk_class' => 'SupervisionNode',
  pid             => pid() | nil,        % nil for a child mid-restart
  registeredName  => atom() | nil,
  kind            => beamtalkSupervisor | beamtalkActor
                   | otpSupervisor | otpProcess | restarting,
  behaviourClass  => beamtalk_object() | nil,  % the Beamtalk class object, nil for foreign
  childCount      => non_neg_integer(),  % live children (supervisors only)
  parent_pid      => pid() | nil         % adjacency for tree reconstruction (BT-2429)
}
```

## Scope (ADR 0092 §6)

Phase 1 implements the `default` scope only: it roots the walk at the
workspace application tree (the OTP application root supervisor plus any
supervisors attached via `Workspace startSupervisor:`) and filters runtime
plumbing via the centralised infra deny-list (`infra_deny_list/0`). Foreign
classification, the `system` scope, the dynamic-supervisor child cap, `from:`,
and the lazy guarded `status` fetch land in Phase 2 (BT-2428).

## References

- ADR 0092: Supervision Tree Introspection API (§3 "Children mid-restart", §6, §7)
- `beamtalk_supervisor` (`get_root/0`, `is_supervisor/1`)
- `beamtalk_actor` (`is_beamtalk_actor/1`)
""".

-export([
    default_snapshot/0,
    snapshot_from_pids/2,
    infra_deny_list/0,
    is_infra/1
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Bounded timeout (ms) for the guarded `which_children` call. The standard
%% `supervisor:which_children/1` blocks with `infinity`; the snapshot must never
%% wedge on a busy or stuck supervisor, so the walk uses a bounded call and
%% treats a timeout as "no children" (best-effort, ADR 0092 §4).
-define(WHICH_CHILDREN_TIMEOUT, 5000).

%% A scope selector. Phase 1 implements `default`; `system` lands in BT-2428.
-type scope() :: default | system.
%% A `SupervisionNode` value record (tagged map). The exact `'$beamtalk_class'`
%% field lets the Beamtalk type checker infer FFI results as `SupervisionNode`
%% (the same mechanism `beamtalk_workspace_changelog:changeLog/0` uses for
%% `ChangeLog`), so `Workspace processes` types as `List(SupervisionNode)`.
-type node_map() :: #{
    '$beamtalk_class' := 'SupervisionNode',
    pid := pid() | nil,
    registeredName := atom() | nil,
    kind := atom(),
    behaviourClass := beamtalk_object() | nil,
    childCount := non_neg_integer(),
    parent_pid := pid() | nil
}.

-export_type([scope/0, node_map/0]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
Return the flat `default`-scope supervision snapshot.

Roots the walk at the workspace application tree — the OTP application root
supervisor (`beamtalk_supervisor:get_root/0`) plus any supervisors attached
via `Workspace startSupervisor:` — and filters runtime plumbing via the infra
deny-list. Returns a flat `[node_map()]` (pre-order: parents precede children);
the parent/child adjacency is carried in each node's `parent_pid` for tree
reconstruction (BT-2429).

Best-effort and crash-free: a supervisor that dies mid-walk contributes a
partial subtree rather than raising.
""".
-spec default_snapshot() -> [node_map()].
default_snapshot() ->
    snapshot_from_pids(default_root_pids(), default).

-doc """
Walk the given root pids under `Scope`, returning the flat node list.

The shared snapshot engine behind `default_snapshot/0`; also the seam reused by
`from:` (BT-2428) and by tests that need to root the walk at a specific
supervisor. Each root is classified and walked recursively via
`supervisor:which_children/1`. Under the `default` scope, nodes whose
registered name is in the infra deny-list are excluded along with their
subtree; under `system` nothing is filtered.

Cycles and shared children are visited once (pids are de-duplicated).
""".
-spec snapshot_from_pids([pid()], scope()) -> [node_map()].
snapshot_from_pids(RootPids, Scope) ->
    {AccRev, _Visited} = lists:foldl(
        fun(Pid, {Acc, Visited}) ->
            walk(Pid, nil, Scope, Acc, Visited)
        end,
        {[], sets:new([{version, 2}])},
        RootPids
    ),
    lists:reverse(AccRev).

-doc """
Return the infra deny-list — the registered names of runtime plumbing that the
`default` scope filters out (ADR 0092 §6).

Centralised here because this module owns the definition of "infrastructure".
The deny-list parity test (BT-2433) pins this set: a new runtime supervisor
that should be hidden from `default` but is missing here fails CI rather than
leaking into the user-facing snapshot.
""".
-spec infra_deny_list() -> sets:set(atom()).
infra_deny_list() ->
    sets:from_list(
        [
            %% Runtime top supervisor and its workers (ADR 0092 §6).
            beamtalk_runtime_sup,
            beamtalk_xref,
            beamtalk_bootstrap,
            beamtalk_stdlib,
            beamtalk_object_instances,
            beamtalk_trace_store,
            beamtalk_subprocess_sup,
            beamtalk_reactive_subprocess_sup,
            %% Workspace plumbing: the workspace supervisor itself, the
            %% ChangeLog, the compiler server/port, the REPL compiler.
            beamtalk_workspace_sup,
            beamtalk_workspace_meta,
            beamtalk_workspace_changelog,
            beamtalk_compiler_server,
            beamtalk_compiler_port,
            beamtalk_repl_compiler
        ],
        [{version, 2}]
    ).

-doc """
Return `true` if `Pid` is runtime infrastructure — i.e. it is registered under
a name in the infra deny-list (`infra_deny_list/0`). A pid with no registered
name, or one registered under a name not in the deny-list, is not infra.
""".
-spec is_infra(term()) -> boolean().
is_infra(Pid) when is_pid(Pid) ->
    case registered_name(Pid) of
        nil -> false;
        Name -> sets:is_element(Name, infra_deny_list())
    end;
is_infra(_) ->
    false.

%%% ============================================================================
%%% Roots
%%% ============================================================================

-doc """
The `default`-scope root pids: the supervision-tree roots reachable in the
workspace.

Three sources, in walk order:
  1. the OTP application root supervisor (`beamtalk_supervisor:get_root/0`),
  2. the supervisors attached to the workspace via `Workspace startSupervisor:`
     (the `{user_supervisor, _}` children of `beamtalk_workspace_sup`),
  3. any standalone Beamtalk supervisor started via `SupClass supervise`, found
     by scanning `erlang:registered/0` (Beamtalk supervisors register under
     `{local, ?MODULE}`).

Order matters: (1) and (2) are walked first, so a *nested* supervisor — which is
both reachable as a child and registered under its module — is captured under
its real parent (correct `parent_pid`) before the registered scan reaches it;
the visited-set then skips the duplicate. Only genuinely top-level standalone
supervisors survive (3) as roots. The workspace supervisor is referenced by
registered name only (no code dependency on the workspace app), so this degrades
gracefully when the workspace layer is not running.
""".
-spec default_root_pids() -> [pid()].
default_root_pids() ->
    RootSup =
        case beamtalk_supervisor:get_root() of
            Tuple when is_tuple(Tuple), is_pid(element(4, Tuple)) ->
                [element(4, Tuple)];
            _ ->
                []
        end,
    RootSup ++ workspace_user_supervisor_pids() ++ registered_beamtalk_supervisor_pids().

%% Pids of standalone Beamtalk supervisors found in the global name registry —
%% those started via `SupClass supervise` (registered under `{local, ?MODULE}`).
%% Foreign supervisors and non-supervisor registered processes are excluded; the
%% `default` walk additionally drops any that are infra via the deny-list.
-spec registered_beamtalk_supervisor_pids() -> [pid()].
registered_beamtalk_supervisor_pids() ->
    lists:filtermap(
        fun(Name) ->
            case erlang:whereis(Name) of
                Pid when is_pid(Pid) ->
                    IsBtSup =
                        looks_like_supervisor(Pid) andalso
                            beamtalk_supervisor_class(Pid, undefined) =/= error,
                    case IsBtSup of
                        true -> {true, Pid};
                        false -> false
                    end;
                _ ->
                    false
            end
        end,
        erlang:registered()
    ).

-spec workspace_user_supervisor_pids() -> [pid()].
workspace_user_supervisor_pids() ->
    case erlang:whereis(beamtalk_workspace_sup) of
        undefined ->
            [];
        _ ->
            safe_children_pids(beamtalk_workspace_sup, fun
                ({{user_supervisor, _Class}, Pid, supervisor, _Mods}) when is_pid(Pid) ->
                    {true, Pid};
                (_) ->
                    false
            end)
    end.

-spec safe_children_pids(pid() | atom(), fun((tuple()) -> {true, pid()} | false)) -> [pid()].
safe_children_pids(SupRef, Filter) ->
    try
        lists:filtermap(Filter, supervisor:which_children(SupRef))
    catch
        Class:Reason ->
            ?LOG_DEBUG("process navigation: which_children failed for root", #{
                supervisor => SupRef,
                error_class => Class,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            []
    end.

%%% ============================================================================
%%% The walk
%%% ============================================================================

-spec walk(pid(), pid() | nil, scope(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
walk(Pid, ParentPid, Scope, Acc, Visited) when is_pid(Pid) ->
    case sets:is_element(Pid, Visited) of
        true ->
            {Acc, Visited};
        false ->
            case Scope =:= default andalso is_infra(Pid) of
                true ->
                    %% Deny-listed plumbing: skip this node and its whole subtree.
                    {Acc, Visited};
                false ->
                    Visited1 = sets:add_element(Pid, Visited),
                    {Kind, BClass} = classify(Pid, undefined, undefined),
                    Children = safe_which_children(Pid),
                    Node = build_node(Pid, ParentPid, Kind, BClass, child_count(Children)),
                    walk_children(Children, Pid, Scope, [Node | Acc], Visited1)
            end
    end.

-spec walk_children([tuple()], pid(), scope(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
walk_children(Children, ParentPid, Scope, Acc, Visited) ->
    lists:foldl(
        fun(ChildTuple, {A, V}) ->
            walk_child(ChildTuple, ParentPid, Scope, A, V)
        end,
        {Acc, Visited},
        Children
    ).

-spec walk_child(tuple(), pid(), scope(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
%% A child OTP is currently restarting (ADR 0092 §3): the pid slot is the literal
%% atom `restarting`, NOT a pid. Produce a first-class `#restarting` node with
%% `pid => nil` — never call process_info on the atom.
walk_child({Id, restarting, _Type, _Mods}, ParentPid, _Scope, Acc, Visited) ->
    Node = build_restarting_node(Id, ParentPid),
    {[Node | Acc], Visited};
%% A child whose spec exists but is not currently running (pid `undefined`):
%% there is no process to introspect, so it is omitted from the snapshot.
walk_child({_Id, undefined, _Type, _Mods}, _ParentPid, _Scope, Acc, Visited) ->
    {Acc, Visited};
walk_child({Id, ChildPid, Type, _Mods}, ParentPid, Scope, Acc, Visited) when is_pid(ChildPid) ->
    case sets:is_element(ChildPid, Visited) of
        true ->
            {Acc, Visited};
        false ->
            case Scope =:= default andalso is_infra(ChildPid) of
                true ->
                    {Acc, Visited};
                false ->
                    Visited1 = sets:add_element(ChildPid, Visited),
                    {Kind, BClass} = classify(ChildPid, Id, Type),
                    GrandChildren = safe_which_children(ChildPid),
                    Node = build_node(
                        ChildPid, ParentPid, Kind, BClass, child_count(GrandChildren)
                    ),
                    walk_children(GrandChildren, ChildPid, Scope, [Node | Acc], Visited1)
            end
    end;
walk_child(_Other, _ParentPid, _Scope, Acc, Visited) ->
    {Acc, Visited}.

%%% ============================================================================
%%% Classification (ADR 0092 §7)
%%% ============================================================================

-doc false.
%% Classify a pid into `{Kind, BehaviourClassObject | nil}`.
%%
%% `Id` and `Type` are the `which_children` hints for a child (the child id and
%% the OTP child type), or `undefined` for a root pid with no parent context.
%% A Beamtalk actor is identified by the `'$beamtalk_actor'` process-dictionary
%% marker (its value IS the behaviour class). A Beamtalk supervisor is an OTP
%% supervisor whose child id is a registered Beamtalk Supervisor/DynamicSupervisor
%% subclass. Everything else is foreign — minimally tagged here in Phase 1
%% (`otpSupervisor`/`otpProcess`); behaviour-based foreign detection is BT-2428.
-spec classify(pid(), atom() | undefined, atom() | undefined) ->
    {atom(), beamtalk_object() | nil}.
classify(Pid, Id, Type) ->
    case beamtalk_actor:is_beamtalk_actor(Pid) of
        true ->
            {beamtalkActor, class_object(actor_class_name(Pid))};
        false ->
            classify_non_actor(Pid, Id, Type)
    end.

-spec classify_non_actor(pid(), atom() | undefined, atom() | undefined) ->
    {atom(), beamtalk_object() | nil}.
classify_non_actor(Pid, Id, Type) ->
    case is_supervisor_pid(Pid, Type) of
        true ->
            case beamtalk_supervisor_class(Pid, Id) of
                {ok, ClassName} -> {beamtalkSupervisor, class_object(ClassName)};
                error -> {otpSupervisor, nil}
            end;
        false ->
            {otpProcess, nil}
    end.

%% Resolve the Beamtalk Supervisor/DynamicSupervisor class for a supervisor pid,
%% or `error` for a foreign OTP supervisor. Prefers the `which_children` child-id
%% hint (a class atom for a nested static supervisor); for a root pid with no
%% hint, falls back to the supervisor's registered module — Beamtalk supervisors
%% register under `{local, ?MODULE}`, so the module reverses to the class via the
%% same mapping that drives stack-trace class resolution.
-spec beamtalk_supervisor_class(pid(), atom() | undefined) -> {ok, atom()} | error.
beamtalk_supervisor_class(Pid, Id) ->
    case is_atom(Id) andalso Id =/= undefined andalso beamtalk_supervisor:is_supervisor(Id) of
        true ->
            {ok, Id};
        false ->
            case registered_name(Pid) of
                nil ->
                    error;
                ModName ->
                    case beamtalk_stack_frame:module_to_class(ModName) of
                        nil ->
                            error;
                        ClassName ->
                            case beamtalk_supervisor:is_supervisor(ClassName) of
                                true -> {ok, ClassName};
                                false -> error
                            end
                    end
            end
    end.

%% A pid is a supervisor when the `which_children` hint says so, or — for a root
%% with no hint — when its `'$initial_call'` marks it as an OTP supervisor.
-spec is_supervisor_pid(pid(), atom() | undefined) -> boolean().
is_supervisor_pid(_Pid, supervisor) ->
    true;
is_supervisor_pid(_Pid, worker) ->
    false;
is_supervisor_pid(Pid, _Unknown) ->
    looks_like_supervisor(Pid).

%% Cheap, message-free supervisor probe: an OTP supervisor stores
%% `'$initial_call' => {supervisor, Module, 1}` in its process dictionary
%% (planted by `proc_lib`). This avoids sending `which_children` to a plain
%% worker pid, which — not being a gen_server — would never reply and block the
%% walk. Foreign supervisors with an exotic start path are caught by the
%% behaviour-based probe in BT-2428; Phase 1 relies on the standard marker.
-spec looks_like_supervisor(pid()) -> boolean().
looks_like_supervisor(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$initial_call', 1, Dict) of
                {'$initial_call', {supervisor, _Mod, _Arity}} -> true;
                _ -> false
            end;
        _ ->
            false
    end.

%%% ============================================================================
%%% Node construction
%%% ============================================================================

-spec build_node(pid(), pid() | nil, atom(), beamtalk_object() | nil, non_neg_integer()) ->
    node_map().
build_node(Pid, ParentPid, Kind, BClass, ChildCount) ->
    #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => Pid,
        registeredName => registered_name(Pid),
        kind => Kind,
        behaviourClass => BClass,
        childCount => ChildCount,
        parent_pid => ParentPid
    }.

%% Build the node for a child mid-restart (ADR 0092 §3): `pid => nil`, the
%% behaviour class derived from the child id where it names a Beamtalk class.
-spec build_restarting_node(atom() | term(), pid() | nil) -> node_map().
build_restarting_node(Id, ParentPid) ->
    BClass =
        case is_atom(Id) of
            true -> class_object(Id);
            false -> nil
        end,
    #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => nil,
        registeredName => nil,
        kind => restarting,
        behaviourClass => BClass,
        childCount => 0,
        parent_pid => ParentPid
    }.

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Live child count from a `which_children` result (children with a real pid).
-spec child_count([tuple()]) -> non_neg_integer().
child_count(Children) ->
    length([P || {_Id, P, _Type, _Mods} <- Children, is_pid(P)]).

%% Guarded `which_children`: returns the child list for a supervisor, or `[]`
%% for a worker pid, a dead/wedged supervisor, or any other failure
%% (best-effort walk). Probes the process dictionary first so a non-supervisor
%% is never sent the `which_children` call (which it would never answer), then
%% issues the documented supervisor request with a bounded timeout so a busy
%% supervisor cannot wedge the snapshot.
-spec safe_which_children(pid()) -> [tuple()].
safe_which_children(Pid) ->
    case looks_like_supervisor(Pid) of
        false ->
            [];
        true ->
            try
                gen_server:call(Pid, which_children, ?WHICH_CHILDREN_TIMEOUT)
            catch
                Class:Reason ->
                    ?LOG_DEBUG("process navigation: which_children failed", #{
                        pid => Pid,
                        error_class => Class,
                        reason => Reason,
                        domain => [beamtalk, runtime]
                    }),
                    []
            end
    end.

%% The Beamtalk class object for a class name atom, or `nil` when the class is
%% not loaded (foreign processes, or a child id that is not a Beamtalk class).
-spec class_object(atom() | nil) -> beamtalk_object() | nil.
class_object(nil) ->
    nil;
class_object(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> nil;
        Pid when is_pid(Pid) -> beamtalk_class_registry:class_object_from_pid(Pid)
    end;
class_object(_) ->
    nil.

%% The Beamtalk class name for an actor pid, read from the `'$beamtalk_actor'`
%% process-dictionary marker planted by every actor's `init/1`.
-spec actor_class_name(pid()) -> atom() | nil.
actor_class_name(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor', 1, Dict) of
                {'$beamtalk_actor', Class} when is_atom(Class) -> Class;
                _ -> nil
            end;
        _ ->
            nil
    end.

%% The registered name of a pid, or the Beamtalk `nil` atom when unregistered
%% or dead.
-spec registered_name(pid()) -> atom() | nil.
registered_name(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} when is_atom(Name) -> Name;
        _ -> nil
    end.
