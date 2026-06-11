%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_navigation).
-compile(nowarn_export_var_subexpr).

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
  '$beamtalk_class'  => 'SupervisionNode',
  pid              => pid() | nil,        % nil for a child mid-restart
  registeredName   => atom() | nil,
  kind             => beamtalkSupervisor | beamtalkActor
                    | otpSupervisor | otpProcess | restarting,
  behaviourClass   => beamtalk_object() | nil,  % the Beamtalk class object, nil for foreign
  childCount       => non_neg_integer(),  % live children (supervisors only)
  strategy         => atom() | nil,       % #oneForOne … ; Beamtalk supervisors only
  restartIntensity => #{maxRestarts, window} | nil,  % configured budget; supervisors only
  truncated        => boolean(),          % true when children exceeded the cap
  parent_pid       => pid() | nil         % adjacency for tree reconstruction (BT-2429)
}
```

## Scope (ADR 0092 §6)

The `default` scope roots the walk at the workspace application tree (the OTP
application root supervisor, supervisors attached via `Workspace
startSupervisor:`, and standalone Beamtalk supervisors started via `supervise`)
and filters runtime plumbing via the centralised infra deny-list
(`infra_deny_list/0`). The `system` scope additionally roots at the runtime and
workspace top supervisors and applies no deny-list, so it spans every process
kind including runtime internals — a privileged view (ADR 0091).

## Foreign processes, cap, `from:`, lazy status (ADR 0092 §1, §3, §5)

Non-Beamtalk pids are first-class nodes: an OTP supervisor (recognised by its
`'$initial_call'`) is `#otpSupervisor`, anything else `#otpProcess`, both with
`behaviourClass => nil`. A supervisor with more live children than the cap is
not materialised child-by-child — it reports `childCount` + `truncated => true`.
`from/1,2` roots a walk at a user-supplied `Supervisor` handle or raw `Pid`,
returning a structured error for a dead root. `status/1` is the lazy,
timeout-guarded `sys:get_status/1` fetch — never called during snapshotting.

## References

- ADR 0092: Supervision Tree Introspection API (§3 "Children mid-restart", §6, §7)
- `beamtalk_supervisor` (`get_root/0`, `is_supervisor/1`)
- `beamtalk_actor` (`is_beamtalk_actor/1`)
""".

-export([
    default_snapshot/0,
    default_snapshot/1,
    system_snapshot/0,
    system_snapshot/1,
    snapshot_from_pids/2,
    snapshot_from_pids/3,
    from/1,
    from/2,
    status/1,
    guarded_state/1,
    guarded_state/2,
    infra_deny_list/0,
    is_infra/1,
    %% Node / tree accessors consumed by the SupervisionNode & SupervisionTree
    %% stdlib classes (BT-2429).
    enrich/1,
    rootOf/1,
    childrenOf/1,
    parentOf/1,
    parentPidOf/1,
    strategyOf/1,
    restartIntensityOf/1,
    truncatedOf/1
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% Bounded timeout (ms) for the guarded `which_children` / `count_children`
%% calls. The standard `supervisor:*` calls block with `infinity`; the snapshot
%% must never wedge on a busy or stuck supervisor, so the walk uses bounded calls
%% and treats a timeout as "no children" (best-effort, ADR 0092 §4).
-define(WHICH_CHILDREN_TIMEOUT, 5000).

%% Bounded timeout (ms) for the lazy `sys:get_status/1` fetch. A busy, wedged, or
%% non-`sys`-compliant process must yield `nil`, never block the caller
%% (ADR 0092 §5).
-define(STATUS_TIMEOUT, 2000).

%% Bounded timeout (ms) for the guarded `sys:get_state/1` fetch shared with the
%% Inspector (ADR 0095 §4). Same discipline as `status/1`: a busy, wedged, dead,
%% or non-`sys`-compliant process yields `unavailable`, never blocks the caller.
-define(GET_STATE_TIMEOUT, 2000).

%% Per-supervisor child-expansion cap (ADR 0092 §Constraints 1). A supervisor
%% with more than this many live children — typically a `simple_one_for_one`
%% `DynamicSupervisor` (connection pool, per-request worker farm) — is NOT
%% materialised child-by-child (which would copy the whole child list out of the
%% supervisor and `process_info` every pid, a real performance cliff). Instead
%% the node reports `childCount` (from the cheap `count_children`) plus a
%% `truncated => true` marker. Full expansion is opt-in via a higher `Limit`.
-define(DEFAULT_CHILD_LIMIT, 200).

%% A scope selector: `default` filters runtime plumbing via the infra deny-list;
%% `system` shows everything, including runtime infrastructure (ADR 0092 §6).
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
    strategy := atom() | nil,
    restartIntensity := #{maxRestarts := term(), window := term()} | nil,
    truncated := boolean(),
    parent_pid := pid() | nil,
    %% Optional: the shared sibling set attached by `enrich/1` so a node can
    %% navigate to its parent/children. Absent on the lite nodes the shim mints.
    siblings => [node_map()]
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
    default_snapshot(?DEFAULT_CHILD_LIMIT).

-doc """
Return the flat `default`-scope snapshot with an explicit per-supervisor child
cap (`Limit`). See `default_snapshot/0`.
""".
-spec default_snapshot(non_neg_integer()) -> [node_map()].
default_snapshot(Limit) ->
    snapshot_from_pids(default_root_pids(), default, Limit).

-doc """
Return the flat `system`-scope supervision snapshot — everything, including
runtime infrastructure (ADR 0092 §6).

Roots at the runtime top supervisor, the workspace supervisor, the application
root, and every registered supervisor, applying no deny-list. This is the
privileged view (ADR 0091): it exposes the whole-node process map (runtime
internals, foreign processes). `default`'s node set is a subset of `system`'s.
""".
-spec system_snapshot() -> [node_map()].
system_snapshot() ->
    system_snapshot(?DEFAULT_CHILD_LIMIT).

-doc """
Return the flat `system`-scope snapshot with an explicit per-supervisor child
cap (`Limit`). See `system_snapshot/0`.
""".
-spec system_snapshot(non_neg_integer()) -> [node_map()].
system_snapshot(Limit) ->
    snapshot_from_pids(system_root_pids(), system, Limit).

-doc """
Root a snapshot at a user-supplied `Root` — a Beamtalk `Supervisor` handle
(`{beamtalk_supervisor, _, _, Pid}`, pid in tuple position 4) or a raw `Pid`
(ADR 0092 §1).

Returns `{ok, [node_map()]}` rooted at `Root` (walked with `system` semantics —
no deny-list, since the root is explicit). A `Root` that is the right type but a
dead process yields `{error, #beamtalk_error{kind = stale_handle}}`; a wrong-type
`Root` (defensive runtime fallback for untyped/FFI input — the type checker
rejects it statically) yields `{error, #beamtalk_error{kind = type_error}}`. A
pid that is simply not a supervisor (a plain worker, or one that errors on
`which_children`) is **not** an error — it returns `{ok, [SingleNode]}`.
""".
-spec from(term()) -> {ok, [node_map()]} | {error, #beamtalk_error{}}.
from(Root) ->
    from(Root, ?DEFAULT_CHILD_LIMIT).

-doc """
Root a snapshot at `Root` with an explicit per-supervisor child cap (`Limit`).
The opt-in full expansion of a large dynamic supervisor: `from(aSup, N)` with a
high `N`. See `from/1`.
""".
-spec from(term(), non_neg_integer()) -> {ok, [node_map()]} | {error, #beamtalk_error{}}.
from(Root, Limit) ->
    case root_pid(Root) of
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {ok, snapshot_from_pids([Pid], system, Limit)};
                false ->
                    {error, from_error(stale_handle, <<"the root process is not alive">>)}
            end;
        error ->
            {error,
                from_error(
                    type_error,
                    <<"from: expects a Supervisor handle or a Pid">>
                )}
    end.

-doc """
Lazily fetch the OTP status report for a node's pid (ADR 0092 §5).

Issues a timeout-guarded `sys:get_status/1` *at call time* (never during
snapshotting). Returns a `Dictionary` (`#{module, sysState}`) for an alive,
`sys`-compliant process; returns `nil` — logged at debug, never raised — for a
process that is dead, timed out, or not `sys`-compliant.
""".
-spec status(term()) -> map() | nil.
status(Pid) when is_pid(Pid) ->
    try sys:get_status(Pid, ?STATUS_TIMEOUT) of
        {status, _Pid, {module, Mod}, Data} ->
            #{module => Mod, sysState => sys_state(Data)}
    catch
        Class:Reason ->
            ?LOG_DEBUG("process navigation: status fetch failed", #{
                pid => Pid,
                error_class => Class,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            nil
    end;
status(_) ->
    nil.

%% The OTP sys state (`running` | `suspended`) from `sys:get_status/1`'s status
%% data — `[ProcDict, SysState, Parent, Dbg, Misc]`. Defaults to `running` if the
%% report has an unexpected shape (defensive — never crash a status fetch).
-spec sys_state([term()]) -> atom().
sys_state([_ProcDict, SysState | _]) when is_atom(SysState) -> SysState;
sys_state(_) -> running.

-doc """
Timeout-guarded `sys:get_state/1` — the shared safe live-state read (ADR 0095 §4).

Returns `{ok, State}` for an alive, `sys`-compliant process; `unavailable` —
logged at debug, never raised — for a process that is dead, busy past the
timeout, or not `sys`-compliant. This is the one place that knows how to safely
read a live process's state; the Inspector (`beamtalk_inspector`) shares it so
there is a single guarded-read discipline across the runtime (the same intent
`status/1` serves for `sys:get_status/1`).
""".
-spec guarded_state(term()) -> {ok, term()} | unavailable.
guarded_state(Pid) ->
    guarded_state(Pid, ?GET_STATE_TIMEOUT).

-spec guarded_state(term(), timeout()) -> {ok, term()} | unavailable.
guarded_state(Pid, Timeout) when is_pid(Pid) ->
    try sys:get_state(Pid, Timeout) of
        State ->
            {ok, State}
    catch
        Class:Reason ->
            ?LOG_DEBUG("guarded state fetch failed", #{
                pid => Pid,
                error_class => Class,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            unavailable
    end;
guarded_state(_, _) ->
    unavailable.

%%% ============================================================================
%%% Node / tree accessors (BT-2429)
%%%
%%% The flat snapshot is a list of immutable node maps carrying parent linkage
%%% (`parent_pid`). Parent/child navigation from a *bare* node needs the sibling
%%% set, so the tree accessors embed the shared flat list under a `siblings` key
%%% (a single shared term — no per-node copy, no cycle: the embedded list holds
%%% lite nodes, the returned nodes are transient enriched copies). `Supervision
%%% Tree` and `SupervisionNode` (stdlib) call these.
%%% ============================================================================

-doc """
Enrich each node in `FlatList` with the shared sibling set, so parent/child
navigation works on any node returned by the tree. Backs `SupervisionTree
nodes`.
""".
-spec enrich([node_map()]) -> [node_map()].
enrich(FlatList) ->
    [attach_siblings(N, FlatList) || N <- FlatList].

-doc """
Return the snapshot root — the first node with no parent — enriched, or `nil`
for an empty snapshot. Backs `SupervisionTree root`.
""".
-spec rootOf([node_map()]) -> node_map() | nil.
rootOf(FlatList) ->
    case lists:search(fun(N) -> maps:get(parent_pid, N, nil) =:= nil end, FlatList) of
        {value, Root} -> attach_siblings(Root, FlatList);
        false -> nil
    end.

-doc """
Return the (enriched) snapshot children of `Node` — the sibling nodes whose
`parent_pid` is `Node`'s pid. Backs `SupervisionNode children`.
""".
-spec childrenOf(node_map()) -> [node_map()].
childrenOf(Node) ->
    Siblings = maps:get(siblings, Node, []),
    case maps:get(pid, Node, nil) of
        nil ->
            [];
        SelfPid ->
            [
                attach_siblings(Child, Siblings)
             || Child <- Siblings, maps:get(parent_pid, Child, nil) =:= SelfPid
            ]
    end.

-doc """
Return the (enriched) snapshot parent of `Node`, or `nil` for a root node or a
node mid-restart. Backs `SupervisionNode parent`.
""".
-spec parentOf(node_map()) -> node_map() | nil.
parentOf(Node) ->
    Siblings = maps:get(siblings, Node, []),
    case maps:get(parent_pid, Node, nil) of
        nil ->
            nil;
        ParentPid ->
            case lists:search(fun(N) -> maps:get(pid, N, nil) =:= ParentPid end, Siblings) of
                {value, Parent} -> attach_siblings(Parent, Siblings);
                false -> nil
            end
    end.

-doc """
Return a node's parent pid directly (`pid() | nil`), without resolving the full
parent node. The O(1) adjacency key for serialisation — avoids the O(n) sibling
search `parentOf/1` does when only the parent's identity is needed.
""".
-spec parentPidOf(node_map()) -> pid() | nil.
parentPidOf(Node) ->
    maps:get(parent_pid, Node, nil).

-doc "Return a node's configured supervisor `strategy` (supervisors only; else `nil`).".
-spec strategyOf(node_map()) -> atom() | nil.
strategyOf(Node) ->
    maps:get(strategy, Node, nil).

-doc "Return a node's configured `restartIntensity` budget (supervisors only; else `nil`).".
-spec restartIntensityOf(node_map()) -> map() | nil.
restartIntensityOf(Node) ->
    maps:get(restartIntensity, Node, nil).

-doc "Return whether a node's children were truncated by the child cap.".
-spec truncatedOf(node_map()) -> boolean().
truncatedOf(Node) ->
    maps:get(truncated, Node, false).

%% Attach the shared sibling set to a node so its parent/child accessors resolve.
-spec attach_siblings(node_map(), [node_map()]) -> node_map().
attach_siblings(Node, Siblings) ->
    Node#{siblings => Siblings}.

-doc """
Walk the given root pids under `Scope`, returning the flat node list (default
child cap). See `snapshot_from_pids/3`.
""".
-spec snapshot_from_pids([pid()], scope()) -> [node_map()].
snapshot_from_pids(RootPids, Scope) ->
    snapshot_from_pids(RootPids, Scope, ?DEFAULT_CHILD_LIMIT).

-doc """
Walk the given root pids under `Scope` with per-supervisor child cap `Limit`,
returning the flat node list.

The shared snapshot engine behind `default_snapshot/0` and `system_snapshot/0`;
also the seam reused by `from/2` and by tests that root the walk at a specific
supervisor. Each root is classified and walked recursively. Under `default`,
nodes whose registered name is in the infra deny-list are excluded along with
their subtree; under `system` nothing is filtered. A supervisor with more than
`Limit` live children is reported truncated (its children are not materialised).

Cycles and shared children are visited once (pids are de-duplicated).
""".
-spec snapshot_from_pids([pid()], scope(), non_neg_integer()) -> [node_map()].
snapshot_from_pids(RootPids, Scope, Limit) ->
    {AccRev, _Visited} = lists:foldl(
        fun(Pid, {Acc, Visited}) ->
            walk(Pid, nil, Scope, Limit, Acc, Visited)
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

-doc """
The `system`-scope root pids: the runtime and workspace top supervisors, the
application root, and every registered supervisor (foreign or Beamtalk).

The infra top supervisors are walked first so their whole subtrees (runtime
internals) are captured; registered standalone supervisors then add anything not
already reached. No deny-list is applied at root selection — filtering is a
`default`-only concern handled during the walk.
""".
-spec system_root_pids() -> [pid()].
system_root_pids() ->
    NamedTops = lists:filtermap(
        fun(Name) ->
            case erlang:whereis(Name) of
                Pid when is_pid(Pid) -> {true, Pid};
                _ -> false
            end
        end,
        [beamtalk_runtime_sup, beamtalk_workspace_sup]
    ),
    RootSup =
        case beamtalk_supervisor:get_root() of
            Tuple when is_tuple(Tuple), is_pid(element(4, Tuple)) ->
                [element(4, Tuple)];
            _ ->
                []
        end,
    NamedTops ++ RootSup ++ registered_supervisor_pids().

%% Pids of all registered supervisors (foreign or Beamtalk) — used as `system`
%% roots so standalone supervisors not under a tracked top supervisor are still
%% reached. The visited-set dedups any already captured under a top supervisor.
-spec registered_supervisor_pids() -> [pid()].
registered_supervisor_pids() ->
    lists:filtermap(
        fun(Name) ->
            case erlang:whereis(Name) of
                Pid when is_pid(Pid) ->
                    case looks_like_supervisor(Pid) of
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

-spec walk(pid(), pid() | nil, scope(), non_neg_integer(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
walk(Pid, ParentPid, Scope, Limit, Acc, Visited) when is_pid(Pid) ->
    case skip_pid(Pid, Scope, Visited) of
        true ->
            {Acc, Visited};
        false ->
            Visited1 = sets:add_element(Pid, Visited),
            {Kind, BClass} = classify(Pid, undefined, undefined),
            emit_node(Pid, ParentPid, Kind, BClass, Scope, Limit, Acc, Visited1)
    end.

%% A pid is skipped when already visited, or — under `default` — when it is
%% deny-listed infra (which prunes its whole subtree).
-spec skip_pid(pid(), scope(), sets:set(pid())) -> boolean().
skip_pid(Pid, Scope, Visited) ->
    sets:is_element(Pid, Visited) orelse (Scope =:= default andalso is_infra(Pid)).

-spec walk_children([tuple()], pid(), scope(), non_neg_integer(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
walk_children(Children, ParentPid, Scope, Limit, Acc, Visited) ->
    lists:foldl(
        fun(ChildTuple, {A, V}) ->
            walk_child(ChildTuple, ParentPid, Scope, Limit, A, V)
        end,
        {Acc, Visited},
        Children
    ).

-spec walk_child(tuple(), pid(), scope(), non_neg_integer(), [node_map()], sets:set(pid())) ->
    {[node_map()], sets:set(pid())}.
%% A child OTP is currently restarting (ADR 0092 §3): the pid slot is the literal
%% atom `restarting`, NOT a pid. Produce a first-class `#restarting` node with
%% `pid => nil` — never call process_info on the atom.
walk_child({Id, restarting, _Type, _Mods}, ParentPid, _Scope, _Limit, Acc, Visited) ->
    Node = build_restarting_node(Id, ParentPid),
    {[Node | Acc], Visited};
%% A child whose spec exists but is not currently running (pid `undefined`):
%% there is no process to introspect, so it is omitted from the snapshot.
walk_child({_Id, undefined, _Type, _Mods}, _ParentPid, _Scope, _Limit, Acc, Visited) ->
    {Acc, Visited};
walk_child({Id, ChildPid, Type, _Mods}, ParentPid, Scope, Limit, Acc, Visited) when
    is_pid(ChildPid)
->
    case skip_pid(ChildPid, Scope, Visited) of
        true ->
            {Acc, Visited};
        false ->
            Visited1 = sets:add_element(ChildPid, Visited),
            {Kind, BClass} = classify(ChildPid, Id, Type),
            emit_node(ChildPid, ParentPid, Kind, BClass, Scope, Limit, Acc, Visited1)
    end;
walk_child(_Other, _ParentPid, _Scope, _Limit, Acc, Visited) ->
    {Acc, Visited}.

%% Emit a node for `Pid` and recurse into its children when it is a supervisor.
%% Non-supervisor leaves emit a single node. A supervisor reports `childCount`
%% from the cheap `count_children` and, for a Beamtalk supervisor, its `strategy`
%% and `restartIntensity`; if its live child count exceeds `Limit` it is reported
%% truncated (children are NOT materialised — ADR 0092 §Constraints 1), otherwise
%% its children are walked.
-spec emit_node(
    pid(),
    pid() | nil,
    atom(),
    beamtalk_object() | nil,
    scope(),
    non_neg_integer(),
    [node_map()],
    sets:set(pid())
) ->
    {[node_map()], sets:set(pid())}.
emit_node(Pid, ParentPid, Kind, BClass, Scope, Limit, Acc, Visited) ->
    case is_supervisor_kind(Kind) of
        false ->
            {[leaf_node(Pid, ParentPid, Kind, BClass) | Acc], Visited};
        true ->
            {Active, Total} = safe_child_counts(Pid),
            {Strategy, RestartIntensity} = supervisor_config(Kind, BClass),
            %% Cap on the TOTAL child count, not just the active count: a
            %% `which_children` call copies *every* child (incl. `restarting` /
            %% `undefined`) out of the supervisor, so a flapping pool with a low
            %% active count but a huge child-spec count must still truncate (the
            %% perf cliff the cap exists to prevent — ADR 0092 §Constraints 1).
            %% `childCount` still reports the live (active) count.
            case Total > Limit of
                true ->
                    Node = supervisor_node(
                        Pid, ParentPid, Kind, BClass, Active, Strategy, RestartIntensity, true
                    ),
                    {[Node | Acc], Visited};
                false ->
                    Children = safe_which_children(Pid),
                    Node = supervisor_node(
                        Pid, ParentPid, Kind, BClass, Active, Strategy, RestartIntensity, false
                    ),
                    walk_children(Children, Pid, Scope, Limit, [Node | Acc], Visited)
            end
    end.

-spec is_supervisor_kind(atom()) -> boolean().
is_supervisor_kind(beamtalkSupervisor) -> true;
is_supervisor_kind(otpSupervisor) -> true;
is_supervisor_kind(_) -> false.

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
%% hint, maps the supervisor's callback module (from its `'$initial_call'`) to
%% the class via the live class registry — a reliable lookup, not a lossy
%% snake_case→CamelCase name heuristic (which mangles e.g. `E2EAppSupervisor`).
-spec beamtalk_supervisor_class(pid(), atom() | undefined) -> {ok, atom()} | error.
beamtalk_supervisor_class(Pid, Id) ->
    case is_atom(Id) andalso Id =/= undefined andalso beamtalk_supervisor:is_supervisor(Id) of
        true ->
            {ok, Id};
        false ->
            case class_name_for_module(supervisor_callback_module(Pid)) of
                nil ->
                    error;
                ClassName ->
                    case beamtalk_supervisor:is_supervisor(ClassName) of
                        true -> {ok, ClassName};
                        false -> error
                    end
            end
    end.

%% The OTP supervisor's callback module, read from its `'$initial_call'`
%% (`{supervisor, Module, 1}`). For a Beamtalk supervisor this is the class's
%% generated module; `nil` if the process is not a marked supervisor.
-spec supervisor_callback_module(pid()) -> module() | nil.
supervisor_callback_module(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$initial_call', 1, Dict) of
                {'$initial_call', {supervisor, Module, _Arity}} -> Module;
                _ -> nil
            end;
        _ ->
            nil
    end.

%% Map a compiled module atom to its Beamtalk class name via the live class
%% registry, or `nil` for a non-Beamtalk (foreign) module. Reliable: it matches
%% the registry's recorded `module_name`, never reconstructs the name from text.
-spec class_name_for_module(module() | nil) -> atom() | nil.
class_name_for_module(nil) ->
    nil;
class_name_for_module(Module) ->
    case lists:keyfind(Module, 2, beamtalk_class_registry:live_class_entries()) of
        {Name, Module, _Pid} -> Name;
        false -> nil
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

%% A non-supervisor node (worker / foreign process): no children, no strategy.
-spec leaf_node(pid(), pid() | nil, atom(), beamtalk_object() | nil) -> node_map().
leaf_node(Pid, ParentPid, Kind, BClass) ->
    #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => Pid,
        registeredName => registered_name(Pid),
        kind => Kind,
        behaviourClass => BClass,
        childCount => 0,
        strategy => nil,
        restartIntensity => nil,
        truncated => false,
        parent_pid => ParentPid
    }.

%% A supervisor node, carrying its live child count, configured strategy /
%% restart budget (Beamtalk supervisors only; `nil` for foreign), and the
%% truncation marker.
-spec supervisor_node(
    pid(),
    pid() | nil,
    atom(),
    beamtalk_object() | nil,
    non_neg_integer(),
    atom() | nil,
    map() | nil,
    boolean()
) ->
    node_map().
supervisor_node(Pid, ParentPid, Kind, BClass, ChildCount, Strategy, RestartIntensity, Truncated) ->
    #{
        '$beamtalk_class' => 'SupervisionNode',
        pid => Pid,
        registeredName => registered_name(Pid),
        kind => Kind,
        behaviourClass => BClass,
        childCount => ChildCount,
        strategy => Strategy,
        restartIntensity => RestartIntensity,
        truncated => Truncated,
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
        strategy => nil,
        restartIntensity => nil,
        truncated => false,
        parent_pid => ParentPid
    }.

%% Resolve a supervisor's configured `{Strategy, RestartIntensity}` from its
%% Beamtalk class (public, stable: ADR 0092 §3). A `DynamicSupervisor` is always
%% `simpleOneForOne`; a static `Supervisor` reports its class-side `strategy`.
%% `restartIntensity` is the configured `#{maxRestarts, window}` budget — never
%% a per-child restart *count* (deliberately out of scope, ADR §3). Foreign
%% supervisors (no Beamtalk class) report `{nil, nil}`.
-spec supervisor_config(atom(), beamtalk_object() | nil) -> {atom() | nil, map() | nil}.
supervisor_config(beamtalkSupervisor, BClass) when is_tuple(BClass) ->
    ClassPid = element(4, BClass),
    {supervisor_strategy(ClassPid), supervisor_restart_intensity(ClassPid)};
supervisor_config(_Kind, _BClass) ->
    {nil, nil}.

-spec supervisor_strategy(pid()) -> atom() | nil.
supervisor_strategy(ClassPid) ->
    try
        ClassName = beamtalk_object_class:class_name(ClassPid),
        case beamtalk_class_registry:inherits_from(ClassName, 'DynamicSupervisor') of
            true ->
                simpleOneForOne;
            false ->
                case beamtalk_object_class:class_send(ClassPid, strategy, []) of
                    S when is_atom(S) -> S;
                    _ -> nil
                end
        end
    catch
        _:_ -> nil
    end.

-spec supervisor_restart_intensity(pid()) -> map() | nil.
supervisor_restart_intensity(ClassPid) ->
    try
        Max = beamtalk_object_class:class_send(ClassPid, maxRestarts, []),
        Window = beamtalk_object_class:class_send(ClassPid, restartWindow, []),
        #{maxRestarts => Max, window => Window}
    catch
        _:_ -> nil
    end.

%%% ============================================================================
%%% Roots / `from:` helpers
%%% ============================================================================

%% Extract the root pid from a `from:` argument — a raw pid or a Beamtalk
%% `Supervisor` handle (`{beamtalk_supervisor, _, _, Pid}`). Anything else is a
%% type error (the type checker rejects it statically; this is the runtime
%% defence for untyped/FFI input).
-spec root_pid(term()) -> {ok, pid()} | error.
root_pid(Pid) when is_pid(Pid) ->
    {ok, Pid};
root_pid({beamtalk_supervisor, _Class, _Module, Pid}) when is_pid(Pid) ->
    {ok, Pid};
root_pid(_) ->
    error.

%% Build the structured `#beamtalk_error{}` returned by `from/1,2` for a dead
%% root (`stale_handle`) or a wrong-type root (`type_error`).
-spec from_error(atom(), binary()) -> #beamtalk_error{}.
from_error(Kind, Hint) ->
    beamtalk_error:new(Kind, 'ProcessNavigation', 'from:', Hint).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% Guarded `count_children`: the cheap child counts (does not copy the child
%% list out of the supervisor). Returns `{Active, Total}` — `Active` is the live
%% child count (`childCount`), `Total` is the full child-spec count that bounds
%% the `which_children` copy cost (the cap metric). `{0, 0}` on any failure or a
%% non-supervisor. Probes the process dictionary first so a non-supervisor is
%% never sent the call.
-spec safe_child_counts(pid()) -> {non_neg_integer(), non_neg_integer()}.
safe_child_counts(Pid) ->
    case looks_like_supervisor(Pid) of
        false ->
            {0, 0};
        true ->
            try gen_server:call(Pid, count_children, ?WHICH_CHILDREN_TIMEOUT) of
                Counts when is_list(Counts) ->
                    Active = proplists:get_value(active, Counts, 0),
                    Specs = proplists:get_value(specs, Counts, Active),
                    {Active, max(Active, Specs)};
                _ ->
                    {0, 0}
            catch
                _:_ -> {0, 0}
            end
    end.

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
