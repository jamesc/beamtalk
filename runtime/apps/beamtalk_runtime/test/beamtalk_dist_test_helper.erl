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
""".

-export([
    ensure_distribution/0,
    start_peer/0,
    start_peer/1,
    start_peer/2,
    stop_peer/1
]).

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
- `connection` — `peer`'s control-channel option. The default (a
  distribution-based channel) halts the peer as soon as this node
  disconnects from it, so a test that disconnects on purpose and expects
  the peer to survive (e.g. to reconnect) passes `standard_io`.

`peer:start/1`'s own `wait_boot` defaults to 15 seconds
(`peer:?WAIT_BOOT_TIMEOUT`) and raises a hard `exit(timeout)` — not a
return value this module can turn into `{error, _}` — if the peer's `erl`
process doesn't finish booting in time. A large `-pa` list (every path in
this node's own `code:get_path/0`, forwarded below) makes that boot slower,
and a shared CI runner under load can exceed 15 seconds even though it
completes in a few seconds locally, so `wait_boot` is raised well past the
default here rather than left to it.
""".
-spec start_peer(string(), #{extra_args => [string()], connection => standard_io}) ->
    {ok, peer:server_ref(), node()} | {error, term()}.
start_peer(NamePrefix, Opts) ->
    ensure_distribution(),
    {ok, Host} = inet:gethostname(),
    PeerName = unique_short_name(NamePrefix),
    CodePathArgs = lists:append([["-pa", Dir] || Dir <- code:get_path()]),
    CookieArgs = ["-setcookie", atom_to_list(erlang:get_cookie())],
    ExtraArgs = maps:get(extra_args, Opts, []),
    PeerOpts0 = #{
        name => PeerName,
        host => Host,
        args => CookieArgs ++ ExtraArgs ++ CodePathArgs,
        wait_boot => 60000
    },
    PeerOpts =
        case Opts of
            #{connection := Connection} -> PeerOpts0#{connection => Connection};
            #{} -> PeerOpts0
        end,
    case peer:start(PeerOpts) of
        {ok, Peer, PeerNode} ->
            case net_adm:ping(PeerNode) of
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

-doc """
Tear down a peer started by start_peer/0,1. Idempotent-ish: `peer:stop/1`
on an already-stopped peer raises `noproc`, which this catches and treats
as success (matching the harness's "tear everything down reliably"
requirement — a test's cleanup should never itself fail because the node
under test already died, which is exactly what some of these tests do to
it on purpose).
""".
-spec stop_peer(peer:server_ref()) -> ok.
stop_peer(Peer) ->
    try
        peer:stop(Peer)
    catch
        exit:noproc -> ok;
        exit:{noproc, _} -> ok
    end,
    ok.

%%% Internal

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
A short node name intended to avoid collisions across repeated test runs
on the same host: `Prefix_<unique integer>@<hostname>`. `unique_integer/1`
is only unique within this BEAM instance's lifetime, so two independently
started VMs (e.g. two parallel CI jobs on the same runner) have no
cross-process coordination and could in principle pick overlapping names;
a collision fails/flakes the affected test rather than corrupting state.
Uses `inet:gethostname/0` rather than a hardcoded "localhost" so the
harness works on whatever host CI resolves this machine's shortname to
(never a `/tmp`-style hardcoded path — the same "don't hardcode
environment specifics" principle CLAUDE.md states for temp paths).
""".
-spec unique_node_name(string()) -> node().
unique_node_name(Prefix) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(atom_to_list(unique_short_name(Prefix)) ++ "@" ++ Host).

-doc """
Just the short-name part (no `@host`) of unique_node_name/1 — what
`peer:start/1`'s `name` option expects (it takes `host` as a separate
option and joins them itself).
""".
-spec unique_short_name(string()) -> atom().
unique_short_name(Prefix) ->
    Unique = erlang:unique_integer([positive, monotonic]),
    list_to_atom(Prefix ++ "_" ++ integer_to_list(Unique)).
