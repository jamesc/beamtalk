%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_node_monitor).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Cluster membership events (ADR 0126 §8, Phase 1).

Subscribes to `net_kernel:monitor_nodes(true, [nodedown_reason])` and
re-announces each membership change on `SystemAnnouncer current` as a typed
`NodeUp` / `NodeDown` announcement, so node up/down is observable through the
ordinary announcement substrate instead of a per-caller `monitor_node/2` or a
per-node process.

Only **visible** nodes announce: `monitor_nodes/2` without a `node_type`
option reports visible nodes only, so hidden (tooling) nodes never produce an
event (§8, §9 item 5). The local node's own `nodeup` — delivered when this VM
becomes distributed after the monitor started (e.g. `net_kernel:start/1` at
runtime) — is not a membership change and is dropped.

Starting on a non-distributed node is fine: node monitoring is handled by the
VM, not by `net_kernel`, so the subscription is taken at boot and simply
starts delivering once distribution comes up.

Announcements stay node-local (ADR 0093): each node announces its own view
of membership.

## Shape-version skew (ADR 0126 §5.3, Phase 4)

Alongside membership, this module makes shape-version skew between
connected nodes visible *proactively* — an early-warning `NodeShapeSkew`
announcement — rather than only discovered reactively when a wire send
actually fails on it (the strict per-envelope version check, `shape_version_ahead`
etc., is Phase 3b/BT-3601's safety net and is untouched here). It never
gates a connection: refusing on skew would make ADR 0125's rolling-upgrade
procedure impossible.

Two triggers, both funnelled through `check_shape_skew_on_connect/1` /
`check_shape_skew_on_reload/2`:

- **Connect** (`nodeup`, alongside the `NodeUp` announcement above): fetch
  the peer's full manifest via `erpc` (`beamtalk_node:remote_shape_manifest/1`
  — the same helper `Node>>shapeManifest` uses) and compare every class this
  node also has, via `beamtalk_release:shape_manifest/0` — the exact BT-3575
  projection, never re-derived.
- **Reload**: this module also subscribes to `ClassLoaded` (the reload
  handler fun forwards it to this process as an ordinary message, matching
  the `nodeup`/`nodedown` handling above); for a class that is one of this
  node's own project-scope classes, it re-queries each currently-connected
  visible peer's entry for *that one class*
  (`beamtalk_release_shapes:class_shape_entry/1`, the same per-class
  primitive `shape_manifest/0` folds over) and re-announces if it now
  differs.

Both paths announce at most one `NodeShapeSkew` per (peer, class) pair per
check, are best-effort (a fault-isolated `try`/`catch`, matching `announce/1`
below — a skew check must never crash the monitor or block `NodeUp`/`NodeDown`),
and only ever consider *visible* peers, matching this module's existing
hidden-node exclusion.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, normalize_reason/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    %% This VM's own current distributed identity, pending its matching
    %% `nodedown` — not a membership change and never announced. Only the
    %% single most recent self-identity is tracked (not a growing history):
    %% if distribution restarts under a different name later, a genuine peer
    %% that reuses an old self-name must still be able to announce.
    own_name = undefined :: node() | undefined,
    %% This process's own `ClassLoaded` subscription ref (Phase 4), so
    %% `terminate/2` can unsubscribe it symmetrically with
    %% `monitor_nodes(false, ...)` below.
    class_loaded_sub :: reference() | undefined
}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the node monitor (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Normalise a `nodedown_reason` to the `NodeDown` event's `reason :: Symbol`
field. OTP documents every reason as an atom (`connection_closed`,
`disconnect`, `net_tick_timeout`, `connection_setup_failed`, …); anything
else collapses to `unknown`, the field's declared default.
""".
-spec normalize_reason(term()) -> atom().
normalize_reason(Reason) when is_atom(Reason), Reason =/= undefined -> Reason;
normalize_reason(_Reason) -> unknown.

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    ClassLoadedSub = subscribe_class_loaded(),
    OwnName =
        case is_alive() of
            true -> node();
            false -> undefined
        end,
    {ok, #state{own_name = OwnName, class_loaded_sub = ClassLoadedSub}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node, _Info}, State) when Node =:= node() ->
    %% This VM just became distributed. Remember the name: by the time the
    %% matching `nodedown` arrives (`net_kernel:stop/0`), `node()` has
    %% already reverted to `nonode@nohost`. Only the current identity is
    %% tracked, not a history, so a later genuine peer that reuses an old
    %% self-name still announces normally.
    {noreply, State#state{own_name = Node}};
handle_info({nodeup, Node, _Info}, State) ->
    announce('NodeUp', #{node => beamtalk_node:from_atom(Node)}),
    check_shape_skew_on_connect(Node),
    {noreply, State};
handle_info({nodedown, Node, _Info}, #state{own_name = Node} = State) ->
    %% Own pending self-nodedown — clear it, not a membership change.
    {noreply, State#state{own_name = undefined}};
handle_info({nodedown, Node, Info}, State) ->
    Reason = normalize_reason(proplists:get_value(nodedown_reason, Info, unknown)),
    announce('NodeDown', #{node => beamtalk_node:from_atom(Node), reason => Reason}),
    {noreply, State};
handle_info({'$beamtalk_class_loaded', ClassName}, State) ->
    %% Only visible, currently-connected peers participate — matching the
    %% `NodeUp`/`NodeDown` hidden-node exclusion above.
    check_shape_skew_on_reload(ClassName, nodes()),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{class_loaded_sub = ClassLoadedSub}) ->
    _ = net_kernel:monitor_nodes(false, [nodedown_reason]),
    case ClassLoadedSub of
        undefined -> ok;
        SubRef -> beamtalk_announcements:unsubscribe(SubRef)
    end,
    ok.

%%% ============================================================================
%%% Internal
%%% ============================================================================

-doc """
Publish a membership event on the system bus. Best-effort and fault-isolated,
matching the other runtime emit points (`beamtalk_actor`'s lifecycle
announcements): a failing announce must never crash the monitor and lose the
`monitor_nodes` subscription.
""".
-spec announce(atom(), map()) -> ok.
announce(EventClass, Fields) ->
    try
        beamtalk_announcements:system_announce(EventClass, Fields)
    catch
        Class:Reason ->
            ?LOG_WARNING("Failed to announce cluster event", #{
                event => EventClass, class => Class, reason => Reason
            })
    end,
    ok.

%%% ============================================================================
%%% Shape-version skew (ADR 0126 §5.3, Phase 4) — see the moduledoc.
%%% ============================================================================

-doc """
Subscribe this process to `ClassLoaded` on the system bus, forwarding each
event to itself as `{'\$beamtalk_class_loaded', ClassName}` (a plain
`handle_info` message, matching the `nodeup`/`nodedown` messages this
gen_server already handles) rather than a raw `{beamtalk_announcement,
...}` tuple — `system_announce/2` runs a `fun/1` handler caller-side in its
own transient process (`beamtalk_announcements:dispatch_one_veneer/5`), so
this extracts just the field this module needs before handing off.

Returns the subscription ref for `terminate/2` to unsubscribe, or
`undefined` if the announcements bus is not up (should not happen in normal
supervised startup — `beamtalk_runtime_sup` starts `beamtalk_announcements`
before this module — but a test harness that starts this gen_server bare
must not crash `init/1` over it).
""".
-spec subscribe_class_loaded() -> reference() | undefined.
subscribe_class_loaded() ->
    Self = self(),
    Handler = fun(#{className := ClassName}) -> Self ! {'$beamtalk_class_loaded', ClassName} end,
    case beamtalk_announcements:subscribe('ClassLoaded', Self, Handler, false) of
        {ok, SubRef} -> SubRef;
        {error, _Reason} -> undefined
    end.

-doc """
Connect-time shape-manifest comparison (ADR 0126 §5.3): fetch `PeerNode`'s
manifest via `erpc` (`beamtalk_node:remote_shape_manifest/1` — the same
helper `Node>>shapeManifest` uses, CLAUDE.md's no-duplicate-implementations
rule) and compare every class shared with this node's own
`beamtalk_release:shape_manifest/0` (the BT-3575 projection, never
re-derived). One `NodeShapeSkew` per class whose `version` differs;
matching-version and non-shared classes stay silent.

Best-effort and fault-isolated like `announce/2` above: an unreachable peer
(already gone by the time this runs, a `connect_policy` mismatch — should
not occur since `nodeup` implies an established connection — or any other
erpc failure) is logged and dropped rather than crashing the monitor or
blocking the `NodeUp` announcement this runs alongside.
""".
-spec check_shape_skew_on_connect(node()) -> ok.
check_shape_skew_on_connect(PeerNode) ->
    try
        LocalManifest = beamtalk_release:shape_manifest(),
        case beamtalk_node:remote_shape_manifest(PeerNode) of
            {ok, RemoteManifest} ->
                maps:foreach(
                    fun(ClassName, LocalEntry) ->
                        case maps:find(ClassName, RemoteManifest) of
                            {ok, RemoteEntry} ->
                                announce_if_skewed(PeerNode, ClassName, LocalEntry, RemoteEntry);
                            error ->
                                ok
                        end
                    end,
                    LocalManifest
                );
            {error, FetchReason} ->
                ?LOG_WARNING("Could not fetch peer shape manifest on connect", #{
                    node => PeerNode, reason => FetchReason
                })
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Shape-skew check failed on connect", #{
                node => PeerNode, class => Class, reason => Reason, stacktrace => Stacktrace
            })
    end,
    ok.

-doc """
Reload-time re-check (ADR 0126 §5.3): re-runs the comparison for exactly
`ClassName` against every currently-connected visible `Peers` node. Skipped
entirely — no `erpc` calls at all — when `Peers` is empty (the common case:
most class loads happen before any peer is connected, e.g. every stdlib
class at boot) or when `ClassName` is not one of this node's own
project-scope classes (`beamtalk_release:shape_manifest/0` already excludes
stdlib, ADR 0098 — a stdlib class reload is not this feature's concern).
""".
-spec check_shape_skew_on_reload(atom(), [node()]) -> ok.
check_shape_skew_on_reload(_ClassName, []) ->
    ok;
check_shape_skew_on_reload(ClassName, Peers) ->
    try
        case maps:find(ClassName, beamtalk_release:shape_manifest()) of
            {ok, LocalEntry} ->
                lists:foreach(
                    fun(PeerNode) -> check_one_peer_class(PeerNode, ClassName, LocalEntry) end,
                    Peers
                );
            error ->
                ok
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Shape-skew check failed on reload", #{
                className => ClassName, class => Class, reason => Reason, stacktrace => Stacktrace
            })
    end,
    ok.

-doc """
Re-query `PeerNode`'s entry for exactly `ClassName`
(`beamtalk_release_shapes:class_shape_entry/1` — the same per-class
primitive `shape_manifest/0` folds over, run remotely via `erpc`) and
announce if it now differs from `LocalEntry`. `undefined` (the class is not
registered on `PeerNode` at all — not shared) and any `erpc` failure are
silent, matching `check_shape_skew_on_connect/1`'s "unreachable peer" and
"non-shared class" handling.
""".
-spec check_one_peer_class(node(), atom(), beamtalk_release_shapes:shape_entry()) -> ok.
check_one_peer_class(PeerNode, ClassName, LocalEntry) ->
    try
        erpc:call(
            PeerNode,
            beamtalk_release_shapes,
            class_shape_entry,
            [ClassName],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        undefined ->
            ok;
        RemoteEntry when is_map(RemoteEntry) ->
            announce_if_skewed(PeerNode, ClassName, LocalEntry, RemoteEntry)
    catch
        error:{erpc, _Reason} -> ok;
        error:{exception, _Reason, _Stack} -> ok;
        exit:{exception, _Reason} -> ok
    end,
    ok.

-doc "Announce `NodeShapeSkew` iff `LocalEntry`/`RemoteEntry`'s `version` differ.".
-spec announce_if_skewed(
    node(), atom(), beamtalk_release_shapes:shape_entry(), beamtalk_release_shapes:shape_entry()
) -> ok.
announce_if_skewed(PeerNode, ClassName, #{version := LocalVersion}, #{version := RemoteVersion}) when
    LocalVersion =/= RemoteVersion
->
    announce('NodeShapeSkew', #{
        node => beamtalk_node:from_atom(PeerNode),
        className => ClassName,
        localVersion => LocalVersion,
        remoteVersion => RemoteVersion
    });
announce_if_skewed(_PeerNode, _ClassName, _LocalEntry, _RemoteEntry) ->
    ok.
