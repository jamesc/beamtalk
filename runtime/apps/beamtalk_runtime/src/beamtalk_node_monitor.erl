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

Two triggers, both funnelled through `check_shape_skew_on_connect/3` /
`check_shape_skew_on_reload/3`:

- **Connect** (`nodeup`, alongside the `NodeUp` announcement above, and
  again on the peer's `hello`, below): fetch
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
  primitive `shape_manifest/0` folds over) and announces if the result
  changed.

Both paths announce a `NodeShapeSkew` when a (peer, class) pair becomes
skewed, or its versions change, and not again for the same skew on the
same connection (BT-5265);
they are best-effort (a fault-isolated `try`/`catch`, matching `announce/2`
below — a skew check must never crash the monitor or block `NodeUp`/`NodeDown`),
and only ever consider *visible* peers, matching this module's existing
hidden-node exclusion. Each check also runs in its own spawned, fire-and-forget
process (not inline in `handle_info`) — a slow or partitioned peer's bounded
but still multi-second `erpc` round trip must never delay this single
gen_server's delivery of some *other* node's `NodeUp`/`NodeDown`/reload event,
which is exactly the scenario a rolling upgrade (ADR 0125) with several
peers at different versions is most likely to hit.

## Queryable skew snapshot (ADR 0126 §8, Phase 7 / BT-3605)

`NodeShapeSkew` above is a push-only announcement — useful for "tell me when
skew appears", useless for "how much skew is there right now" (the question
the `nodes` cross-surface op answers). `announce_if_skewed/6` therefore also
casts this process a `skew_detected` / `skew_cleared` message per class it
checks, kept in `#state.skew` (per peer, each checked class and whether it
is currently skewed) and read back via `skew_count/1` (`Node>>shapeSkew`) — the tally is
exactly "what this module's own checks have found and not yet found
resolved again", not a re-derivation of `NodeShapeSkew`'s own criteria.

The tally only holds entries for connected peers: `nodeup` (and `init/1`,
for peers already connected) opens an empty entry, `nodedown` removes it.
Each entry carries a **generation**, a fresh `make_ref()` per connection,
and every check is spawned with the generation of the connection it checks
(BT-5240). A `skew_detected` / `skew_cleared` cast whose generation is not
the peer's current one is dropped, so a check that finishes after its peer
has gone, or after the peer has reconnected, never changes the current
connection's tally.

Within one connection, each check also carries a **stamp**, taken from a
monotonic counter when the monitor dispatches it. The tally keeps, per
class, the stamp of the result it holds, and drops an older result that
arrives late: the checks run in separate processes, so their casts are not
ordered. Every change on either side is followed by a check dispatched
after it (a reload here, or a peer's reload notification below), and that
check reads the changed side after the change, so the result of the last
check dispatched is the current one.

The checks never announce themselves. A check casts its result, with the
versions it compared and an announce mode, and the monitor announces in
the same callback that applies the generation and stamp tests. So nothing
from a stale or superseded check is announced: no shape skew that a newer
check has already found resolved, no version event for a peer after its
`NodeDown`, and no second version event when a read outlives a reconnect.

## Hello (BT-5265)

When a monitor opens an entry for a peer, it casts the peer's monitor
`{hello, Node, Gen, WantReply}` with that entry's generation. The peer
stores it in its own entry for this node as `peer_gen`. OTP delivers
`nodeup` before any message over the new connection, and `nodedown` after
the last one, so a `hello` always lands in the entry for the connection it
came over. The generation it carries can still be an earlier connection's:
if the link flaps before the sender has handled the `nodedown`, the
`hello` goes out on the new connection. So the sender pairs each
generation with the stamp of its last `hello` (`hello_at`), and notifies
the peer only for checks dispatched after it: those read both sides after
that connection was up. On a `hello` the receiver also runs a connect-time shape check:
the sender has handled its `nodeup` by then, so anything it reloaded first
is in place.

A connection therefore gets two connect-time shape checks on each side:
one at `nodeup` and one at the peer's `hello`. A peer too old to say
`hello` (0.6.0) still gets the first. Every shape check, at connect or
reload, announces only a result that differs from the one the tally holds
(`should_announce/3`), so a skew is announced once whichever check finds it
first.

When the monitor (re)starts, peers that are already connected get a fresh
entry, a connect-time shape check and a `hello` asking for one back
(`WantReply`), so their counts are rebuilt and their reload notifications
accepted without waiting for a reconnect. The connection is not new, so
the state these checks find is not announced, and `NodeVersionSkew` is
not read at all; a later change on the connection is announced
(`baseline`, see `should_announce/3`).

A reload-time check updates **both** sides: after comparing its own entry
with a peer's, the reloading node casts the pair to the peer's monitor
(`{peer_class_checked, Node, Gen, ...}` with its generation, its new entry
and the reload stamp), which re-reads its own entry and runs the same
`announce_if_skewed/6` against its own tally and bus. The reloader's own
monitor sends that cast, and only while the check's generation is still
current there. The receiver accepts it only when `Gen` is the `peer_gen`
of its current entry. If the link drops and comes back before the sender
has handled the `nodedown`, a notification for the old connection can
still go out on the new one, but it arrives before the sender's new
`hello`, so the receiver's entry holds no `peer_gen` or a newer one and
drops it. It is sent with `noconnect`, so a peer that has dropped is not
re-dialled. The peer's monitor keeps the newest reload stamp seen per
class and ignores an older one, so two quick reloads of one class that
arrive out of order leave the newer result.

A peer too old to know `hello` or the new `peer_class_checked` ignores
them (`handle_cast/2`'s catch-all): 0.6.0 sends no reload notifications at
all, and only gets connect-time checks. The two earlier forms, without the
sender's generation (BT-5232 and BT-5240, nightly builds only), are still
accepted, unchecked against `peer_gen`; a node that says `hello` never
sends them. This node sends only the new form, so such a nightly peer
drops this node's reload notifications.

## Version skew at connect (ADR 0126 Amendment (BT-5193) D3)

On `nodeup` a second spawned check reads the peer's
`beamtalk_version:dist_versions/0` (`{wireVersion, beamtalkVersion}`) and
compares it with this node's. On a mismatch it announces one
`NodeVersionSkew`. A peer that answers with an error (too old to export the
function, say) is announced too, with `nil` remote fields: that is the
rolling-upgrade case the event exists for. A read that **times out** is
only logged as a warning, not announced: a slow peer is not evidence of
skew (BT-5240). Nor is a peer that dropped during the read. The connection
is never refused.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, skew_count/1]).
-ifdef(TEST).
-export([
    normalize_reason/1,
    check_shape_skew_on_reload/3,
    check_version_skew_on_connect/2,
    version_read_outcome/1,
    version_skew_fields/3,
    peer_entry/1,
    peer_entry/2,
    should_announce/3,
    initial_state/0
]).
-endif.
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
    class_loaded_sub :: reference() | undefined,
    %% Queryable per-peer shape-skew tally (ADR 0126 §8, Phase 7 / BT-3605):
    %% each connected peer's `peer_entry()` (connection generation, the
    %% latest kept result per checked class, peer reload stamps), kept in
    %% sync by the same fault-isolated `announce_if_skewed/6` check that
    %% fires the `NodeShapeSkew` announcement below — the `nodes` surface op
    %% needs a snapshot to *read*, not just the fire-and-forget event stream
    %% `NodeShapeSkew` already is. An entry exists exactly while the peer is
    %% connected: opened on `nodeup`, removed on `nodedown` (BT-5232).
    skew = #{} :: #{node() => peer_entry()}
}).

%% One connected peer's tally entry (BT-5240). `gen` is this connection's
%% generation; `baseline` is `pending` on a connection that was already up
%% when this monitor started, until its first connect-time shape check has
%% finished, and `done` otherwise (see `should_announce/3`); `peer_gen` is the peer
%% monitor's generation for it, from its `hello` (BT-5265), `undefined`
%% until one arrives; `hello_at` is the stamp of the last `hello` this
%% monitor sent the peer with `gen`; `classes` holds, per checked class, the stamp of the
%% check whose result is kept and that result; `reload_seq` holds the newest
%% reload stamp the peer has sent per class (`peer_class_checked`).
-type peer_entry() :: #{
    gen := reference(),
    baseline := pending | done,
    peer_gen := reference() | undefined,
    hello_at := integer(),
    classes := #{atom() => {integer(), check_result()}},
    reload_seq := #{atom() => integer()}
}.

%% One check's result for a class: `clear`, or skewed with this node's and
%% the peer's shape versions.
-type check_result() :: clear | {skewed, term(), term()}.

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

-doc """
The number of classes currently skewed against `Node` (ADR 0126 §8, Phase 7)
— `0` for a node with no detected skew, including one this VM isn't even
connected to. Backs `Node>>shapeSkew` (BT-3633); the `nodes` tool maps it over
`Node connected`.
""".
-spec skew_count(node()) -> non_neg_integer().
skew_count(Node) ->
    gen_server:call(?MODULE, {skew_count, Node}).

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
    %% Peers already connected when the monitor (re)starts get a fresh
    %% entry and a connect-time shape check (BT-5240), so their counts are
    %% rebuilt now rather than on their next reconnect. Checks still running
    %% from a previous monitor carry that monitor's generations and are
    %% dropped. The connection is not new, so nothing is announced. Each
    %% peer also gets a `hello` asking for its own in return (BT-5265).
    Skew = maps:from_list([{Peer, open_peer_check(Peer, existing)} || Peer <- nodes()]),
    {ok, #state{own_name = OwnName, class_loaded_sub = ClassLoadedSub, skew = Skew}}.

handle_call({skew_count, Node}, _From, #state{skew = Skew} = State) ->
    {reply, skew_count_of(maps:get(Node, Skew, undefined)), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({skew_detected, Node, Gen, Stamp, ClassName, {LocalV, RemoteV}}, State) ->
    %% Announced here, in the callback that keeps the result, so a stale or
    %% superseded check never announces (BT-5240).
    Result = {skewed, LocalV, RemoteV},
    case record_result(Node, Gen, Stamp, ClassName, Result, State) of
        {kept, Previous, Baseline, NewState} ->
            case should_announce(Baseline, Previous, Result) of
                true ->
                    announce('NodeShapeSkew', #{
                        node => beamtalk_node:from_atom(Node),
                        className => ClassName,
                        localVersion => LocalV,
                        remoteVersion => RemoteV
                    });
                false ->
                    ok
            end,
            {noreply, NewState};
        dropped ->
            {noreply, State}
    end;
handle_cast({skew_cleared, Node, Gen, Stamp, ClassName}, State) ->
    case record_result(Node, Gen, Stamp, ClassName, clear, State) of
        {kept, _Previous, _Baseline, NewState} -> {noreply, NewState};
        dropped -> {noreply, State}
    end;
handle_cast({connect_check_done, Node, Gen}, #state{skew = Skew} = State) ->
    %% The baseline of a connection this monitor found already up is in
    %% place: from now on a class's first result is announced too.
    case maps:find(Node, Skew) of
        {ok, #{gen := Gen, baseline := pending} = Entry} ->
            {noreply, State#state{skew = Skew#{Node := Entry#{baseline := done}}}};
        _ ->
            {noreply, State}
    end;
handle_cast({version_skew, Node, Gen, Fields}, #state{skew = Skew} = State) ->
    %% Only for the connection the read was made on (BT-5240): not after the
    %% peer's NodeDown, and not twice when a read outlives a reconnect.
    case is_current_gen(Node, Gen, Skew) of
        true -> announce('NodeVersionSkew', Fields);
        false -> ok
    end,
    {noreply, State};
handle_cast(
    {send_peer_class_checked, PeerNode, Gen, ClassName, LocalEntry, Seq},
    #state{skew = Skew} = State
) ->
    %% A reload check's notification to the peer (BT-5232), sent from here so
    %% that it goes out only while the check's connection is current
    %% (BT-5240). It carries this monitor's generation for the connection,
    %% which the peer matches against our `hello` (BT-5265), and goes out
    %% only for a check dispatched after the last `hello` this monitor sent
    %% with it (`hello_at`). A `hello` can go out on a later connection than
    %% the one its generation was made for, if the link flaps first; a
    %% check dispatched after it read both sides after that connection was
    %% up. An earlier check's notification is dropped: the peer's `hello`
    %% check reads this node after the `hello`, which covers it.
    case maps:find(PeerNode, Skew) of
        {ok, #{gen := Gen, hello_at := HelloAt}} when Seq > HelloAt ->
            send_to_peer(PeerNode, {peer_class_checked, node(), Gen, ClassName, LocalEntry, Seq});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({hello, PeerNode, PeerGen, WantReply}, State) when
    is_atom(PeerNode), is_reference(PeerGen), is_boolean(WantReply)
->
    {noreply, handle_hello(PeerNode, PeerGen, WantReply, State)};
handle_cast({peer_class_checked, PeerNode, PeerGen, ClassName, PeerEntry, Seq}, State) when
    is_atom(PeerNode), is_reference(PeerGen), is_atom(ClassName), is_map(PeerEntry), is_integer(Seq)
->
    %% Only from the peer's monitor generation that said `hello` on this
    %% connection (BT-5265). A notification for an earlier connection that
    %% went out on this one arrives before the peer's new `hello`, so it
    %% never matches.
    case maps:find(PeerNode, State#state.skew) of
        {ok, #{peer_gen := PeerGen}} ->
            {noreply, dispatch_peer_reload(PeerNode, ClassName, PeerEntry, Seq, State)};
        _ ->
            {noreply, State}
    end;
handle_cast({peer_class_checked, PeerNode, ClassName, PeerEntry}, State) when
    is_atom(PeerNode), is_atom(ClassName), is_map(PeerEntry)
->
    %% The BT-5232 form, without a reload stamp, from a peer on an earlier
    %% nightly build: checked, but not ordered against other reloads of the
    %% class. Such a peer never says `hello`, and a peer that does never
    %% sends this form.
    {noreply, dispatch_peer_reload(PeerNode, ClassName, PeerEntry, unordered, State)};
handle_cast({peer_class_checked, PeerNode, ClassName, PeerEntry, Seq}, State) when
    is_atom(PeerNode), is_atom(ClassName), is_map(PeerEntry), is_integer(Seq)
->
    %% The BT-5240 form, without the sender's generation: same as above.
    {noreply, dispatch_peer_reload(PeerNode, ClassName, PeerEntry, Seq, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec dispatch_peer_reload(node(), atom(), map(), integer() | unordered, #state{}) -> #state{}.
dispatch_peer_reload(PeerNode, ClassName, PeerEntry, Seq, #state{skew = Skew} = State) ->
    %% `PeerNode` reloaded `ClassName` (BT-5232): compare its new entry with
    %% this node's *current* one, re-read here rather than trusting the copy
    %% the peer saw (this side may have reloaded since). Spawned, like the
    %% other checks, so this gen_server never computes a shape entry inline.
    %% An older reload of the class that arrives after a newer one is
    %% dropped (BT-5240), as is one from a peer that is not connected.
    case maps:find(PeerNode, Skew) of
        {ok, #{gen := Gen, reload_seq := Seqs} = Entry} ->
            case maps:find(ClassName, Seqs) of
                {ok, Newer} when is_integer(Seq), Newer >= Seq ->
                    State;
                _ ->
                    Stamp = new_stamp(),
                    spawn(fun() -> check_peer_reload(PeerNode, Gen, Stamp, ClassName, PeerEntry) end),
                    NewSeqs =
                        case Seq of
                            unordered -> Seqs;
                            _ -> Seqs#{ClassName => Seq}
                        end,
                    State#state{skew = Skew#{PeerNode := Entry#{reload_seq := NewSeqs}}}
            end;
        error ->
            State
    end.

-doc """
A peer's `hello` (BT-5265): its monitor's generation for the connection
this message came over. OTP delivers `nodeup` before any message over the
new connection, so the entry here is already this connection's. Store the
generation, so `peer_class_checked` from this monitor is accepted, and run
a connect-time shape check now: the peer has handled its `nodeup` for this
node, so anything it reloaded before that is already in place, and any
reload notification of its that this node dropped before the `hello` is
covered. Its results are announced as `should_announce/3` says, so the
`nodeup` check and this one never announce the same skew twice.

`WantReply` comes from a monitor that has just restarted and needs this
monitor's generation in return. The reply moves this entry's `hello_at`
on, so that only checks dispatched after the reply notify the peer (see
the `send_peer_class_checked` cast).
""".
-spec handle_hello(node(), reference(), boolean(), #state{}) -> #state{}.
handle_hello(PeerNode, PeerGen, WantReply, #state{skew = Skew} = State) ->
    case maps:find(PeerNode, Skew) of
        {ok, #{gen := Gen} = Entry} ->
            Stamp = new_stamp(),
            Entry1 =
                case WantReply of
                    true ->
                        send_hello(PeerNode, Gen, false),
                        Entry#{hello_at := Stamp};
                    false ->
                        Entry
                end,
            spawn(fun() -> check_shape_skew_on_connect(PeerNode, Gen, Stamp) end),
            State#state{skew = Skew#{PeerNode := Entry1#{peer_gen := PeerGen}}};
        error ->
            %% Not a visible, connected peer of this monitor.
            State
    end.

-doc """
Send this monitor's generation `Gen` for the connection to `PeerNode`'s
monitor (BT-5265). A peer too old to know the message ignores it.
""".
-spec send_hello(node(), reference(), boolean()) -> ok.
send_hello(PeerNode, Gen, WantReply) ->
    send_to_peer(PeerNode, {hello, node(), Gen, WantReply}).

-doc """
Cast `Msg` to `PeerNode`'s monitor (`gen_server:cast/2`'s message shape).
`noconnect`: a peer that has gone is not re-dialled; the message is
dropped instead.
""".
-spec send_to_peer(node(), term()) -> ok.
send_to_peer(PeerNode, Msg) ->
    _ = erlang:send({?MODULE, PeerNode}, {'$gen_cast', Msg}, [noconnect]),
    ok.

-doc """
Keep one check's result for `ClassName` in `Node`'s entry (BT-5240),
answering the result it replaces (`none` for the first) and the entry's
`baseline`. Dropped when `Node`
is not connected (BT-5232), when `Gen` is not its current connection's
generation, or when the entry already holds a result from a check with a
newer `Stamp`.
""".
-spec record_result(node(), reference(), integer(), atom(), check_result(), #state{}) ->
    {kept, check_result() | none, pending | done, #state{}} | dropped.
record_result(Node, Gen, Stamp, ClassName, Result, #state{skew = Skew} = State) ->
    case maps:find(Node, Skew) of
        {ok, #{gen := Gen, classes := Classes, baseline := Baseline} = Entry} ->
            case maps:find(ClassName, Classes) of
                {ok, {Newer, _}} when Newer > Stamp ->
                    dropped;
                Found ->
                    Previous =
                        case Found of
                            {ok, {_, Kept}} -> Kept;
                            error -> none
                        end,
                    NewEntry = Entry#{classes := Classes#{ClassName => {Stamp, Result}}},
                    {kept, Previous, Baseline, State#state{skew = Skew#{Node := NewEntry}}}
            end;
        _ ->
            dropped
    end.

-doc """
Whether a kept skewed `Result` is announced as `NodeShapeSkew` (BT-5265):
only when it differs from the result it replaced (the class has just
become skewed, or its versions moved). One skew can be found by several
checks — the `nodeup` and `hello` checks of a connection, or a `hello`
check and a peer's reload notification — and is announced once. A new
connection's tally starts empty, so a reconnect announces its skew again.

While the entry's `baseline` is `pending` (this monitor restarted while
the peer stayed connected, and no connect-time shape check has compared
the manifests yet), a class's first result is taken as the state the
connection was already in, from whichever check it comes, and is not
announced. A later change is. A reload in that window is not told apart
from skew that was already there, so its first result is not announced
either; it is still counted. If no connect check on the connection ever
reads the peer (a 0.6.0 peer, which never answers the `hello`, whose one
read failed, say), the baseline stays `pending` until the peer reconnects:
classes that first skew in that time are counted but not announced. That
is accepted: announcements are best-effort, and taking the baseline from
any other check would announce skew the connection already had.
""".
-spec should_announce(pending | done, check_result() | none, check_result()) -> boolean().
should_announce(pending, none, _Result) -> false;
should_announce(_Baseline, Previous, Result) -> Previous =/= Result.

-spec skew_count_of(peer_entry() | undefined) -> non_neg_integer().
skew_count_of(undefined) ->
    0;
skew_count_of(#{classes := Classes}) ->
    maps:fold(
        fun
            (_ClassName, {_Stamp, {skewed, _, _}}, N) -> N + 1;
            (_ClassName, {_Stamp, clear}, N) -> N
        end,
        0,
        Classes
    ).

-spec is_current_gen(node(), reference(), #{node() => peer_entry()}) -> boolean().
is_current_gen(Node, Gen, Skew) ->
    case maps:find(Node, Skew) of
        {ok, #{gen := Gen}} -> true;
        _ -> false
    end.

-doc """
An empty tally entry with generation `Gen`, for a connection that is `new`
or that was already up (`existing`) when this monitor started, whose
`hello` was sent at stamp `HelloAt`.
""".
-spec peer_entry(reference(), new | existing, integer()) -> peer_entry().
peer_entry(Gen, Connection, HelloAt) ->
    #{
        gen => Gen,
        baseline =>
            case Connection of
                new -> done;
                existing -> pending
            end,
        peer_gen => undefined,
        hello_at => HelloAt,
        classes => #{},
        reload_seq => #{}
    }.

-ifdef(TEST).
-doc "An empty tally entry for a new connection with generation `Gen`.".
-spec peer_entry(reference()) -> peer_entry().
peer_entry(Gen) ->
    peer_entry(Gen, new).

-doc "As `peer_entry/1`, for a `new` or `existing` connection.".
-spec peer_entry(reference(), new | existing) -> peer_entry().
peer_entry(Gen, Connection) ->
    peer_entry(Gen, Connection, new_stamp()).

-doc "A fresh, empty monitor state, for tests that drive the callbacks directly.".
-spec initial_state() -> #state{}.
initial_state() ->
    #state{}.
-endif.

-doc """
Open a tally entry with a new generation for `Peer`, spawn its connect-time
checks, tagged with it (BT-5240), and say `hello` to the peer's monitor
with it (BT-5265). For a `new` connection both checks run and announce. For
an `existing` one (already connected when the monitor starts) only the
shape check runs, and the state it finds is not announced (`baseline`);
its `hello` asks for one back, since the peer's monitor said its own to the monitor that was here
before. Spawned, not inline: the checks' erpc round trips must never delay
this gen_server's delivery of another node's NodeUp/NodeDown. Two
processes, so a peer slow to answer one read never delays the other.

The shape check runs here as well as on the peer's `hello`: a peer too old
to say `hello` (0.6.0) still gets a connect-time check. It announces only a
changed result, like the `hello` check, so one skew is announced once.
""".
-spec open_peer_check(node(), new | existing) -> peer_entry().
open_peer_check(Peer, Connection) ->
    Gen = make_ref(),
    Stamp = new_stamp(),
    case Connection of
        new -> spawn(fun() -> check_version_skew_on_connect(Peer, Gen) end);
        existing -> ok
    end,
    spawn(fun() -> check_shape_skew_on_connect(Peer, Gen, Stamp) end),
    %% Reload checks dispatched from here on carry larger stamps than
    %% `Stamp`, so they may notify the peer (`hello_at`).
    send_hello(Peer, Gen, Connection =:= existing),
    peer_entry(Gen, Connection, Stamp).

-doc """
A check's stamp: strictly increasing in dispatch order on this node, and
never reused while the VM runs, so a monitor restart cannot reset it.
""".
-spec new_stamp() -> integer().
new_stamp() ->
    erlang:unique_integer([monotonic]).

handle_info({nodeup, Node, _Info}, State) when Node =:= node() ->
    %% This VM just became distributed. Remember the name: by the time the
    %% matching `nodedown` arrives (`net_kernel:stop/0`), `node()` has
    %% already reverted to `nonode@nohost`. Only the current identity is
    %% tracked, not a history, so a later genuine peer that reuses an old
    %% self-name still announces normally.
    {noreply, State#state{own_name = Node}};
handle_info({nodeup, Node, _Info}, #state{skew = Skew} = State) ->
    announce('NodeUp', #{node => beamtalk_node:from_atom(Node)}),
    %% A new connection gets a new generation (BT-5240): checks still
    %% running for an earlier connection to `Node` can no longer touch it.
    %% The peer learns it from our `hello` (BT-5265).
    {noreply, State#state{skew = Skew#{Node => open_peer_check(Node, new)}}};
handle_info({nodedown, Node, _Info}, #state{own_name = Node} = State) ->
    %% Own pending self-nodedown — clear it, not a membership change.
    {noreply, State#state{own_name = undefined}};
handle_info({nodedown, Node, Info}, #state{skew = Skew} = State) ->
    Reason = normalize_reason(proplists:get_value(nodedown_reason, Info, unknown)),
    announce('NodeDown', #{node => beamtalk_node:from_atom(Node), reason => Reason}),
    %% A disconnected peer has no meaningful skew count — drop its entry so
    %% `skew_count/1` readers can't leak a
    %% stale count if the peer reconnects and is momentarily not yet
    %% re-checked, and so the tally map doesn't grow unboundedly across
    %% repeated connect/disconnect cycles.
    {noreply, State#state{skew = maps:remove(Node, Skew)}};
handle_info({'$beamtalk_class_loaded', ClassName}, #state{skew = Skew} = State) ->
    %% Only visible, currently-connected peers participate — the tally's
    %% entries (matching the `NodeUp`/`NodeDown` hidden-node exclusion
    %% above), each with its connection's generation (BT-5240), that are
    %% still in `nodes()`: a peer whose `nodedown` is still queued behind
    %% this message must not be re-dialled by the check's erpc. Spawned, not
    %% inline — see open_peer_check/2: this one is worse if run
    %% synchronously, since it's N sequential erpc calls (one per connected
    %% peer), not just one. The stamp doubles as the reload stamp the peers
    %% receive in `peer_class_checked`.
    Connected = nodes(),
    Peers = [
        {Peer, Gen}
     || Peer := #{gen := Gen} <- Skew, lists:member(Peer, Connected)
    ],
    Stamp = new_stamp(),
    spawn(fun() -> check_shape_skew_on_reload(ClassName, Peers, Stamp) end),
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
                domain => [beamtalk, runtime],
                event => EventClass,
                class => Class,
                reason => Reason
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
helper `Node>>shapeManifest` uses, AGENTS.md's no-duplicate-implementations
rule) and compare every class shared with this node's own
`beamtalk_release:shape_manifest/0` (the BT-3575 projection, never
re-derived). Each shared class's result goes to the tally, and a skew is
announced as `should_announce/3` says; matching-version and non-shared
classes stay silent. Once the comparison has run, it tells the monitor
(`connect_check_done`), which takes the connection's baseline as in place;
a check that could not read the peer does not.

Best-effort and fault-isolated like `announce/2` above: an unreachable peer
(already gone by the time this runs, a `connect_policy` mismatch — should
not occur since `nodeup` implies an established connection — or any other
erpc failure) is logged and dropped rather than crashing the monitor or
blocking the `NodeUp` announcement this runs alongside.
""".
-spec check_shape_skew_on_connect(node(), reference(), integer()) -> ok.
check_shape_skew_on_connect(PeerNode, Gen, Stamp) ->
    try
        LocalManifest = beamtalk_release:shape_manifest(),
        case beamtalk_node:remote_shape_manifest(PeerNode) of
            {ok, RemoteManifest} ->
                maps:foreach(
                    fun(ClassName, LocalEntry) ->
                        case maps:find(ClassName, RemoteManifest) of
                            {ok, RemoteEntry} ->
                                announce_if_skewed(
                                    PeerNode,
                                    Gen,
                                    Stamp,
                                    ClassName,
                                    LocalEntry,
                                    RemoteEntry
                                );
                            error ->
                                ok
                        end
                    end,
                    LocalManifest
                ),
                %% After its results, from the same process, so they are
                %% in place first.
                gen_server:cast(?MODULE, {connect_check_done, PeerNode, Gen});
            {error, FetchReason} ->
                ?LOG_WARNING("Could not fetch peer shape manifest on connect", #{
                    domain => [beamtalk, runtime],
                    node => PeerNode,
                    reason => FetchReason
                })
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Shape-skew check failed on connect", #{
                domain => [beamtalk, runtime],
                node => PeerNode,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            })
    end,
    ok.

-doc """
Reload-time re-check (ADR 0126 §5.3): re-runs the comparison for exactly
`ClassName` against every currently-connected visible peer in `Peers`
(each with its connection's generation), tagging every result with `Stamp`
(BT-5240). Skipped
entirely — no `erpc` calls at all — when `Peers` is empty (the common case:
most class loads happen before any peer is connected, e.g. every stdlib
class at boot) or when `ClassName` is not one of this node's own
project-scope classes (`beamtalk_release:project_class_shape_entry/1`
answers `undefined` for stdlib, ADR 0098 — a stdlib class reload is not this
feature's concern). Only that one class's entry is computed, never the full
manifest.
""".
-spec check_shape_skew_on_reload(atom(), [{node(), reference()}], integer()) -> ok.
check_shape_skew_on_reload(_ClassName, [], _Stamp) ->
    ok;
check_shape_skew_on_reload(ClassName, Peers, Stamp) ->
    try
        %% Just this class's entry, not the whole manifest (BT-5232).
        case beamtalk_release:project_class_shape_entry(ClassName) of
            undefined ->
                ok;
            LocalEntry ->
                lists:foreach(
                    fun({PeerNode, Gen}) ->
                        check_one_peer_class(PeerNode, Gen, Stamp, ClassName, LocalEntry)
                    end,
                    Peers
                )
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Shape-skew check failed on reload", #{
                domain => [beamtalk, runtime],
                className => ClassName,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            })
    end,
    ok.

-doc """
The receiving side of a peer's reload check (BT-5232): compare `PeerNode`'s
new `PeerEntry` for `ClassName` with this node's current entry, updating
this node's tally and bus for the connection `Gen`, as a check stamped
`Stamp`. Silent when the class is not a project class here.
Fault-isolated like the other checks.
""".
-spec check_peer_reload(
    node(), reference(), integer(), atom(), beamtalk_release_shapes:shape_entry()
) -> ok.
check_peer_reload(PeerNode, Gen, Stamp, ClassName, PeerEntry) ->
    try
        case beamtalk_release:project_class_shape_entry(ClassName) of
            undefined ->
                ok;
            OwnEntry ->
                announce_if_skewed(PeerNode, Gen, Stamp, ClassName, OwnEntry, PeerEntry)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Shape-skew check failed for a peer's reload", #{
                domain => [beamtalk, runtime],
                node => PeerNode,
                className => ClassName,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            })
    end,
    ok.

-doc """
Connect-time version comparison (ADR 0126 Amendment (BT-5193) D3): read
`PeerNode`'s `beamtalk_version:dist_versions/0` and announce one
`NodeVersionSkew` when it differs from this node's (see
`version_read_outcome/1` for how a failed read counts). Nothing is announced
unless `Gen` is still `PeerNode`'s current connection when the read
finishes (BT-5240): a peer that has disconnected has had its `NodeDown`, and
a peer that has reconnected since has its own check, so announcing here
would describe a connection that is gone, or announce it twice.
Fault-isolated like the shape check.
""".
-spec check_version_skew_on_connect(node(), reference()) -> ok.
check_version_skew_on_connect(PeerNode, Gen) ->
    try
        Read = beamtalk_call_failure:erpc(
            PeerNode, beamtalk_version, dist_versions, [], ?BT_REMOTE_CALL_TIMEOUT
        ),
        case version_read_outcome(Read) of
            {remote, Remote} ->
                announce_version_skew(PeerNode, Gen, Remote);
            unknown ->
                log_failed_version_read(PeerNode, Read, true),
                announce_version_skew(PeerNode, Gen, unknown);
            skip ->
                log_failed_version_read(PeerNode, Read, false)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Version-skew check failed on connect", #{
                domain => [beamtalk, runtime],
                node => PeerNode,
                class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            })
    end,
    ok.

-spec announce_version_skew(node(), reference(), {integer(), binary()} | unknown) -> ok.
announce_version_skew(PeerNode, Gen, Remote) ->
    case version_skew_fields(PeerNode, beamtalk_version:dist_versions(), Remote) of
        none ->
            ok;
        Fields ->
            %% The monitor announces it, only if `Gen` is still current.
            gen_server:cast(?MODULE, {version_skew, PeerNode, Gen, Fields})
    end.

-spec log_failed_version_read(node(), term(), boolean()) -> ok.
log_failed_version_read(PeerNode, Read, Announced) ->
    ?LOG_WARNING("Could not read peer versions on connect", #{
        domain => [beamtalk, runtime],
        node => PeerNode,
        reason => Read,
        announced => Announced
    }).

-doc """
How a connect-time `dist_versions` read counts for `NodeVersionSkew`
(ADR 0126 Amendment (BT-5193) D3, as amended by BT-5240):

- `{remote, {Wire, Version}}` — the peer answered; compare.
- `unknown` — the peer answered with an error, or with something that is
  not a version pair: most likely too old to export the function. That is
  real skew, announced with `nil` remote fields.
- `skip` — the read timed out, or the peer dropped during it. Neither is
  evidence of skew, so nothing is announced; the failure is only logged.
""".
-spec version_read_outcome({ok, term()} | {error, beamtalk_call_failure:erpc_error()}) ->
    {remote, {integer(), binary()}} | unknown | skip.
version_read_outcome({ok, #{wireVersion := W, beamtalkVersion := V}}) when
    is_integer(W), is_binary(V)
->
    {remote, {W, V}};
version_read_outcome({error, timeout}) ->
    skip;
version_read_outcome({error, {node_down, _}}) ->
    skip;
version_read_outcome(_Other) ->
    unknown.

-doc """
The `NodeVersionSkew` fields for `PeerNode`, or `none` when the local and
remote `{wireVersion, beamtalkVersion}` pairs match. `unknown` (the peer
could not answer) always differs, with `nil` remote fields.
""".
-spec version_skew_fields(
    node(),
    #{wireVersion := pos_integer(), beamtalkVersion := binary()},
    {integer(), binary()} | unknown
) -> map() | none.
version_skew_fields(_PeerNode, #{wireVersion := W, beamtalkVersion := V}, {W, V}) ->
    none;
version_skew_fields(PeerNode, #{wireVersion := LocalW, beamtalkVersion := LocalV}, Remote) ->
    {RemoteW, RemoteV} =
        case Remote of
            {W, V} -> {W, beamtalk_primitive:string_from_binary(V)};
            unknown -> {nil, nil}
        end,
    #{
        node => beamtalk_node:from_atom(PeerNode),
        localWireVersion => LocalW,
        remoteWireVersion => RemoteW,
        localBeamtalkVersion => beamtalk_primitive:string_from_binary(LocalV),
        remoteBeamtalkVersion => RemoteV
    }.

-doc """
Re-query `PeerNode`'s entry for exactly `ClassName`
(`beamtalk_release_shapes:class_shape_entry/1` — the same per-class
primitive `shape_manifest/0` folds over, run remotely via `erpc`) and
announce if it now differs from `LocalEntry`. `undefined` (the class is not
registered on `PeerNode` at all — not shared) is silent, matching
`check_shape_skew_on_connect/3`'s "non-shared class" handling; an `erpc`
failure is logged the same way that function logs its own fetch failure,
so a peer that goes unreachable mid-reload-check doesn't look identical to
"checked, no skew found" when debugging a missed announcement.
""".
-spec check_one_peer_class(
    node(), reference(), integer(), atom(), beamtalk_release_shapes:shape_entry()
) -> ok.
check_one_peer_class(PeerNode, Gen, Stamp, ClassName, LocalEntry) ->
    case
        beamtalk_call_failure:erpc(
            PeerNode,
            beamtalk_release_shapes,
            class_shape_entry,
            [ClassName],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        {ok, undefined} ->
            ok;
        {ok, RemoteEntry} when is_map(RemoteEntry) ->
            announce_if_skewed(PeerNode, Gen, Stamp, ClassName, LocalEntry, RemoteEntry),
            %% Tell the peer, so its tally and bus see the reload too
            %% (BT-5232), with this reload's stamp so it can order two
            %% reloads of one class (BT-5240). Sent by the monitor, and only
            %% while `Gen` is current.
            gen_server:cast(
                ?MODULE, {send_peer_class_checked, PeerNode, Gen, ClassName, LocalEntry, Stamp}
            );
        {error, Reason} ->
            ?LOG_WARNING("Could not fetch peer class shape entry on reload", #{
                domain => [beamtalk, runtime],
                node => PeerNode,
                className => ClassName,
                reason => Reason
            })
    end,
    ok.

-doc """
Report whether `LocalEntry`/`RemoteEntry`'s `version` differ, so that the
monitor keeps its queryable skew tally (`skew_count/1`) in sync either way —
the `nodes` op's per-peer count is exactly "how many classes this function
has found skewed for that peer and not yet found matching again" (ADR 0126
§8, Phase 7 / BT-3605). Runs in a spawned, non-gen_server process (see
the moduledoc), so the result is a cast, not a direct state mutation. The
cast carries the check's generation and stamp (BT-5240); the monitor
announces `NodeShapeSkew` only if it keeps the result and
`should_announce/3` allows it.
""".
-spec announce_if_skewed(
    node(),
    reference(),
    integer(),
    atom(),
    beamtalk_release_shapes:shape_entry(),
    beamtalk_release_shapes:shape_entry()
) -> ok.
announce_if_skewed(
    PeerNode,
    Gen,
    Stamp,
    ClassName,
    #{version := LocalVersion},
    #{version := RemoteVersion}
) when
    LocalVersion =/= RemoteVersion
->
    gen_server:cast(
        ?MODULE,
        {skew_detected, PeerNode, Gen, Stamp, ClassName, {LocalVersion, RemoteVersion}}
    );
announce_if_skewed(PeerNode, Gen, Stamp, ClassName, _LocalEntry, _RemoteEntry) ->
    gen_server:cast(?MODULE, {skew_cleared, PeerNode, Gen, Stamp, ClassName}).
