%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_test_helper).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node test harness (ADR 0126 Phase 0 / BT-3578).

No `peer`-based test existed anywhere in `runtime/` before this — every
later phase of ADR 0126 (and BT-3569's own peer-booted wire check, if it
lands first) needs a way to stand up a genuinely distributed second node
and tear it down reliably, so this is the one shared place that logic
lives (CLAUDE.md "No duplicate implementations").

## Usage

```erlang
setup() ->
    beamtalk_dist_test_helper:ensure_distribution(),
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer(),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).
```

`start_peer/0,1` boots the peer with the same code path as the test node
(`code:get_path/0`, forwarded as `-pa` args) and the same distribution
cookie, then starts the `beamtalk_runtime` application there via
`rpc:call/4` over ordinary Erlang distribution — not `peer:call/4,5`'s own
control channel, which this module deliberately avoids driving application
code through (see start_peer/1's doc).

## EUnit timeouts

Booting a peer and starting `beamtalk_runtime` on it routinely takes more
than EUnit's 5-second *per-test* default on a shared CI runner (it is well
under that locally, which is what makes the failure CI-only). Call
`start_peer/0,1,2` from an untimed `setup` (as above), or — if a test body
boots its own peer — wrap that test in its own `{timeout, Seconds, Fun}`.
An outer `{timeout, N, {setup, ...}}` does **not** help there: it bounds
the group, and each test inside still gets the 5-second default (BT-3609;
see `beamtalk_node_tests:two_node_test_/0`). A test killed that way is
reported as *cancelled* with `{timeout, #{stacktrace => ...}}` pointing
into `peer:start_it/2` — the stacktrace only says where it was parked.
""".

-include_lib("kernel/include/logger.hrl").

-export([
    ensure_distribution/0,
    start_peer/0,
    start_peer/1,
    start_peer/2,
    stop_peer/1,
    unreachable_pid/1
]).

%% `peer:start/1`'s boot budget — see start_peer/2's doc.
-define(PEER_WAIT_BOOT_MS, 60000).

%% Host part of every node name this harness creates — see unique_node_name/1.
-define(PEER_HOST, "localhost").

%% Readiness wait for the first distribution connection to a booted peer
%% (BT-3933): up to PING_ATTEMPTS pings, PING_INTERVAL_MS apart.
-define(PING_ATTEMPTS, 50).
-define(PING_INTERVAL_MS, 100).

%% `peer`'s control channel unless a caller picks one — see start_peer/2's doc.
-define(PEER_CONNECTION_DEFAULT, standard_io).

%% `nodedown_reason`s for a peer that are the expected outcome of something
%% this side or the peer did on purpose — see watch_peer_connection/2.
-define(EXPECTED_NODEDOWN_REASONS, [disconnect, connection_closed]).

-doc """
Make this (test) node distributed if it isn't already. Starts epmd first
(idempotent: `epmd -daemon` against an epmd that is already listening on
4369 just logs and exits 0), then `net_kernel:start/1` with a unique short
name so repeated `just test` runs and parallel CI jobs on the same host
never collide on a stale node name. No-op (returns the existing node name)
if this node is already distributed.
""".
-spec ensure_distribution() -> node().
ensure_distribution() ->
    case node() of
        'nonode@nohost' ->
            ensure_epmd(),
            Name = unique_node_name("beamtalk_dist_test"),
            case net_kernel:start([Name, shortnames]) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok
            end,
            node();
        Existing ->
            Existing
    end.

-doc "Same as start_peer/1 with the default name prefix.".
-spec start_peer() -> {ok, peer:server_ref(), node()} | {error, term()}.
start_peer() ->
    start_peer("beamtalk_dist_test_peer").

-doc """
Start a second, `peer`-booted node (OTP `peer`, OTP 25+; OTP 28 per
`.tool-versions`) sharing this node's code path and distribution cookie,
and start `beamtalk_runtime` on it. Ensures this node is itself
distributed first (`ensure_distribution/0`).

Deliberately drives the peer through `rpc:call/4` over ordinary
distribution (established here with `net_adm:ping/1`) rather than
`peer:call/4,5`'s own control channel: the two are independent transports,
and every later two-node test needs real cross-node `gen_server:call` /
`erlang:monitor/2` behavior anyway — routing app startup through the same
distribution path the tests themselves exercise keeps the harness honest
about what it's proving, and `peer:call/4,5`'s dedicated channel is not
needed once distribution is up.

Returns `{ok, Peer, PeerNode}` — `Peer` is the control handle for
`stop_peer/1` (and `peer:stop/1`), `PeerNode` is the node name for
`rpc:call/4`, `erlang:monitor/2`, etc. `{error, Reason}` on any failure to
boot the node, connect distribution, or start `beamtalk_runtime`; in the
last case the peer is stopped before returning so no orphaned node leaks.
""".
-spec start_peer(string()) -> {ok, peer:server_ref(), node()} | {error, term()}.
start_peer(NamePrefix) ->
    start_peer(NamePrefix, #{}).

-doc """
start_peer/1 with options:

- `extra_args` — extra `erl` arguments for the peer (e.g. `["-hidden"]`).
- `connection` — `peer`'s control channel: `standard_io` (the default,
  see below) or `distribution` (`peer:start/1`'s own default: no
  alternative channel, the control link rides on the distribution
  connection itself).

## Why `standard_io` is the default (BT-3929)

With the distribution-based channel the peer halts itself the moment its
connection to this node drops, for *any* reason — `peer`'s own origin link
is a process monitor across that connection, and `noconnection` is a
halt. That turns one lost connection into a dead peer for the rest of the
fixture: every later test in the group fails with `node_down` /
`{badrpc, nodedown}`.

BT-3929 was exactly that: in a full `just test-runtime` run, the connection
owner for a freshly booted peer died with reason `killed` (so
`nodedown_reason = killed` on this side, `noconnection` on the peer's)
with no `erlang:disconnect_node/1`, `net_kernel:disconnect/1` or
`exit(_, kill)` call behind it on either node. That is the signature of
the emulator itself tearing the connection down
(`erts_kill_dist_connection`, which sends the kill from the `init`
process) after it rejected an incoming distribution message, and it hit
at the same point every time — the first *error* reply a compiled actor
sent back over the wire — on every OTP 27 CI run and roughly one full
local run in three under load. The emulator only reports that at
`warning` level, below the `error` floor `test/sys.config` sets, so the
run showed nothing but the six downstream `node_down` failures.

Whatever rejects that one message, the harness must not amplify it. Over
`standard_io` the control channel is the peer's stdin/stdout, so a
dropped distribution connection is just that: the peer stays up, the
next `rpc:call/4` / `gen_server:call` auto-connects again, and the test
that was mid-call is the only one affected. The peer still can never
outlive this node — its stdin reaching EOF halts it (`peer`'s own
`io_server` does that), which is the same "no orphaned nodes" guarantee
the distribution channel gave. The peer's `user` I/O (its logger's
default handler) also travels over the pipe rather than the distribution
connection, so a chatty peer no longer interleaves with wire traffic.

Pass `distribution` to get `peer`'s native channel back, e.g. for a test
of that halt-on-disconnect behaviour itself.

## Unexpected disconnects are logged

Each peer gets a watcher (`watch_peer_connection/2`) that subscribes to
`net_kernel:monitor_nodes/2` for the peer's lifetime and logs, at
`error` level (so it clears the test `sys.config` floor), any
`nodedown` whose reason is neither `disconnect` (this side disconnected
on purpose — `beamtalk_dist_shape_skew_tests`, `beamtalk_node_tests`) nor
`connection_closed` (the peer halted or was stopped). A `killed` there
is the BT-3929 signature above; look for the emulator's "corrupted
external term" / "corrupted distribution header" warning with
`logger_level` lowered to `warning` if it recurs.

`peer:start/1`'s own `wait_boot` defaults to 15 seconds
(`peer:?WAIT_BOOT_TIMEOUT`) and raises a hard `exit(timeout)` if the peer's
`erl` process doesn't finish booting in time. A large `-pa` list (every path
in this node's own `code:get_path/0`, forwarded below) makes that boot
slower, and a shared CI runner under load can be much slower than a local
machine, so `wait_boot` is raised well past the default here rather than
left to it. Note the caller's own EUnit per-test timeout (5 seconds by
default) is usually the tighter bound — see the moduledoc.

Any boot failure — `wait_boot` expiring, the peer's `erl` exiting during
boot, or the control process dying — is caught and returned as
`{error, {peer_boot_failed, Details}}` rather than escaping as a bare
`exit(timeout)`, with `Details` carrying the node name, the elapsed time
and the boot-time budget, so a CI log shows *how long* the boot ran and
against what limit.
""".
-spec start_peer(string(), #{
    extra_args => [string()], connection => standard_io | distribution
}) ->
    {ok, peer:server_ref(), node()} | {error, term()}.
start_peer(NamePrefix, Opts) ->
    ensure_distribution(),
    Host = ?PEER_HOST,
    PeerName = unique_short_name(NamePrefix),
    CodePathArgs = lists:append([["-pa", Dir] || Dir <- code:get_path()]),
    %% Warning level so an ERTS dist-rejection warning on the peer shows (BT-3937).
    CookieArgs = [
        "-setcookie", atom_to_list(erlang:get_cookie()), "-kernel", "logger_level", "warning"
    ],
    ExtraArgs = maps:get(extra_args, Opts, []),
    PeerOpts0 = #{
        name => PeerName,
        host => Host,
        args => CookieArgs ++ ExtraArgs ++ CodePathArgs,
        wait_boot => ?PEER_WAIT_BOOT_MS
    },
    PeerOpts =
        case maps:get(connection, Opts, ?PEER_CONNECTION_DEFAULT) of
            standard_io -> PeerOpts0#{connection => standard_io};
            distribution -> PeerOpts0
        end,
    case boot_peer(PeerOpts) of
        {ok, Peer, PeerNode} ->
            case await_ping(PeerNode, ?PING_ATTEMPTS) of
                pong ->
                    %% Bounded, not rpc:call/4's implicit infinity: a peer
                    %% that hangs during beamtalk_runtime startup (e.g. a
                    %% stdlib bootstrap deadlock) must fail this call rather
                    %% than wedge the whole test run indefinitely.
                    case
                        rpc:call(
                            PeerNode, application, ensure_all_started, [beamtalk_runtime], 30000
                        )
                    of
                        {ok, _Started} ->
                            _ = spawn(fun() -> watch_peer_connection(Peer, PeerNode) end),
                            {ok, Peer, PeerNode};
                        {error, Reason} ->
                            peer:stop(Peer),
                            {error, {beamtalk_runtime_start_failed, Reason}};
                        {badrpc, Reason} ->
                            peer:stop(Peer),
                            {error, {badrpc, Reason}}
                    end;
                pang ->
                    peer:stop(Peer),
                    {error, {distribution_connect_failed, PeerNode}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-doc "Ping `Node` until it answers `pong` or `Attempts` are used up (BT-3933).".
-spec await_ping(node(), non_neg_integer()) -> pong | pang.
await_ping(Node, Attempts) ->
    case net_adm:ping(Node) of
        pong ->
            pong;
        pang when Attempts =< 1 ->
            pang;
        pang ->
            timer:sleep(?PING_INTERVAL_MS),
            await_ping(Node, Attempts - 1)
    end.

-doc """
Tear down a peer started by start_peer/0,1. Idempotent-ish: `peer:stop/1`
on an already-stopped peer raises `noproc`, and on a peer whose control
process is already exiting (a concurrent `stop_peer/1`, e.g. a test that
kills the peer mid-call from a spawned process and then tears it down
again in `after`) `sys:terminate` fails with that process's own `normal`
or `shutdown` exit reason. Both are caught and treated as success
(matching the harness's "tear everything down reliably" requirement — a
test's cleanup should never itself fail because the node under test
already died, which is exactly what some of these tests do to it on
purpose).
""".
-spec stop_peer(peer:server_ref()) -> ok.
stop_peer(Peer) ->
    try
        peer:stop(Peer)
    catch
        exit:noproc -> ok;
        exit:{noproc, _} -> ok;
        exit:{normal, {sys, terminate, _}} -> ok;
        exit:{shutdown, {sys, terminate, _}} -> ok
    end,
    ok.

%%% Internal

-doc """
Log, at `error` level, every `nodedown` for `PeerNode` whose reason is not
one a test or `stop_peer/1` produces on purpose (`?EXPECTED_NODEDOWN_REASONS`)
— see start_peer/2's doc. Runs in its own process for the lifetime of the
peer's control process `Peer` (it exits on that process's `DOWN`), so a
caller never has to clean it up.
""".
-spec watch_peer_connection(peer:server_ref(), node()) -> ok.
watch_peer_connection(Peer, PeerNode) ->
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    MRef = erlang:monitor(process, Peer),
    watch_peer_connection_loop(MRef, PeerNode).

-spec watch_peer_connection_loop(reference(), node()) -> ok.
watch_peer_connection_loop(MRef, PeerNode) ->
    receive
        {nodedown, PeerNode, Info} ->
            report_nodedown(PeerNode, Info),
            watch_peer_connection_loop(MRef, PeerNode);
        {'DOWN', MRef, process, _Peer, _Reason} ->
            %% The control process is gone (stop_peer/1, or it crashed). A
            %% `nodedown` for the same event can already be queued behind
            %% this `DOWN` — report it before leaving rather than lose it.
            receive
                {nodedown, PeerNode, Info} -> report_nodedown(PeerNode, Info)
            after 0 -> ok
            end;
        _Other ->
            watch_peer_connection_loop(MRef, PeerNode)
    end.

-spec report_nodedown(node(), [{atom(), term()}]) -> ok.
report_nodedown(PeerNode, Info) ->
    Reason = proplists:get_value(nodedown_reason, Info, undefined),
    case lists:member(Reason, ?EXPECTED_NODEDOWN_REASONS) of
        true ->
            ok;
        false ->
            ?LOG_ERROR("Distribution connection to a test peer was lost unexpectedly", #{
                node => PeerNode,
                nodedown_reason => Reason,
                hint =>
                    "'killed' means the emulator tore the connection down itself "
                    "(BT-3929); see beamtalk_test_warnings.log (BT-3937) for the emulator warning",
                domain => [beamtalk, runtime]
            })
    end.

-doc """
`peer:start/1`, with a boot failure turned into a descriptive `{error, _}`
(see start_peer/2's doc). `peer:start/1` signals a failed boot by raising
`exit(timeout)` (`wait_boot` expired), `exit({boot_failed, _})` (the peer
reported a failed boot) or the control process's own exit reason (it died
while the peer was booting) — all caught here. Only the `exit` class is
caught: `peer:start/1` never raises `error`/`throw` for a boot problem, and
an EUnit test-timeout kill is an exit *signal* to the test process, not an
exception, so it is not swallowed by this `try`.
""".
-spec boot_peer(peer:start_options()) ->
    {ok, peer:server_ref(), node()} | {error, term()}.
boot_peer(#{name := Name, host := Host, wait_boot := WaitBoot} = PeerOpts) ->
    Started = erlang:monotonic_time(millisecond),
    try peer:start(PeerOpts) of
        {ok, Peer, PeerNode} -> {ok, Peer, PeerNode};
        {error, Reason} -> {error, Reason}
    catch
        exit:Reason ->
            {error,
                {peer_boot_failed, #{
                    reason => Reason,
                    node => list_to_atom(atom_to_list(Name) ++ "@" ++ Host),
                    elapsed_ms => erlang:monotonic_time(millisecond) - Started,
                    wait_boot_ms => WaitBoot,
                    connection => maps:get(connection, PeerOpts, distribution)
                }}}
    end.

-doc """
Idempotent epmd start — see ensure_distribution/0's doc.

Pins `ERL_EPMD_ADDRESS=127.0.0.1` on the spawned `epmd` process, matching
this project's own loopback-only security posture (ADR 0125 §1.6, ADR
0020/0058/0091: a workspace/release node that starts its own epmd
constrains it to loopback the same way). This only governs an epmd *this
call starts* — `ERL_EPMD_ADDRESS` does not re-bind an epmd that is already
running (see `crates/beamtalk-cli/src/commands/workspace/epmd.rs`'s
`EpmdPosture` doc for the same caveat) — so a pre-existing non-loopback
epmd on the host is unaffected either way; this just ensures the harness
itself never *introduces* one.
""".
-spec ensure_epmd() -> ok.
ensure_epmd() ->
    _ = os:cmd("ERL_EPMD_ADDRESS=127.0.0.1 epmd -daemon"),
    ok.

-doc """
A deterministic pid on `Name@localhost`, a node that is never started, built
from the external term format (`NEW_PID_EXT`) so no peer is needed:
`binary_to_term/1` does not require the encoded node to exist or be
connected. For tests of how a handle behaves when its process's node cannot
be reached.

The host part is `localhost` (?PEER_HOST), never a made-up host such as
`nohost` (BT-4289). Once this VM is distributed (any test that called
ensure_distribution/0 leaves it so for the rest of the run), a call or
monitor to the pid auto-connects to its node, and that connect first
resolves the host. `localhost` resolves from the hosts file and its epmd
(ensure_epmd/0, 127.0.0.1) answers at once that `Name` is not registered,
so the call fails with `nodedown` in milliseconds. An unknown host goes to
DNS, which on a GitHub Actions runner can take over a second per lookup:
every call then blocks that long, and a test making sixteen of them
overran EUnit's 5-second per-test timeout. A VM that is not distributed
fails the call at once either way.
""".
-spec unreachable_pid(atom()) -> pid().
unreachable_pid(Name) ->
    NodeBin = atom_to_binary(list_to_atom(atom_to_list(Name) ++ "@" ++ ?PEER_HOST), utf8),
    %% 88 = NEW_PID_EXT; 118 = ATOM_UTF8_EXT, whose length is 2 bytes
    %% (`:16`). SMALL_ATOM_UTF8_EXT (119) takes a 1-byte length: paired with
    %% `:16` it decodes the node as the empty atom ''.
    binary_to_term(
        <<131, 88, 118, (byte_size(NodeBin)):16, NodeBin/binary, 1:32, 0:32, 0:32>>
    ).

-doc """
A short node name intended to avoid collisions across repeated test runs
on the same host: `Prefix_<os pid>_<unique integer>@localhost`.
`unique_integer/1` is only unique within this BEAM instance's lifetime, so
the OS pid is what keeps two independently started VMs (parallel CI jobs or
agents on one host) from picking the same name (BT-3933).

The host part is always `localhost` (?PEER_HOST), never
`inet:gethostname/0` (BT-3609). ensure_epmd/0 binds epmd to 127.0.0.1
only, and connecting to `Name@Host` asks the epmd at *Host's resolved
address* for the port. Locally the machine's own hostname usually
resolves to a loopback address, but on a GitHub Actions runner it
resolves to the VM's private (non-loopback) IP, where nothing answers on
4369 — so with `@<hostname>` names every peer boot hangs until
`wait_boot` expires (the peer can never connect back) and every ping
answers `pang`. `localhost` always resolves to loopback, and it is also
the host the workspace/attach nodes use.
""".
-spec unique_node_name(string()) -> node().
unique_node_name(Prefix) ->
    list_to_atom(atom_to_list(unique_short_name(Prefix)) ++ "@" ++ ?PEER_HOST).

-doc """
Just the short-name part (no `@host`) of unique_node_name/1 — what
`peer:start/1`'s `name` option expects (it takes `host` as a separate
option and joins them itself).
""".
-spec unique_short_name(string()) -> atom().
unique_short_name(Prefix) ->
    Unique = erlang:unique_integer([positive, monotonic]),
    %% The OS pid makes the name unique across concurrent VMs on one host
    %% (BT-3933): `unique_integer/1` alone restarts at 1 in every VM, so two
    %% parallel runs both picked `<Prefix>_1@localhost` and collided in epmd.
    list_to_atom(Prefix ++ "_" ++ os:getpid() ++ "_" ++ integer_to_list(Unique)).
