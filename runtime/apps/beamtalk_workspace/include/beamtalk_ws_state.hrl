%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% WebSocket handler session state (beamtalk_ws_handler).
%%%
%%% Extracted to a shared header so EUnit suites can construct handler
%%% state and drive the websocket_handle/2 + websocket_info/2 callbacks
%%% directly (deterministic coverage without a live WS client).

-record(ws_state, {
    session_id :: binary() | undefined,
    session_pid :: pid() | undefined,
    session_mon :: reference() | undefined,
    authenticated = false :: boolean(),
    peer :: term(),
    %% Pending eval request for streaming output correlation.
    pending_eval :: term() | undefined,
    %% IO capture process pid and correlation ref for stdin routing.
    io_capture_pid :: pid() | undefined,
    stdin_ref :: reference() | undefined,
    %% Whether this session is subscribed to log streaming.
    log_subscribed = false :: boolean(),
    %% ADR 0150: the connect snapshot's pids, so an `ActorSpawned` the snapshot
    %% already covers is not pushed twice (`beamtalk_workspace_actors:feed_*`).
    actor_feed = beamtalk_workspace_actors:feed_new([]) :: beamtalk_workspace_actors:feed(),
    %% Version-exempt admin surface (BT-4327): when the client authenticated
    %% with `client = shutdown`, the version handshake is skipped and this
    %% connection may only issue the `shutdown` op (enforced in
    %% `handle_protocol/3`). Shutdown loads no code, so version skew is
    %% irrelevant — but it must grant nothing else.
    shutdown_only = false :: boolean(),
    %% BT-5185: monitor on the connection-cap table's owner; when it goes the
    %% table is recreated empty, so this connection closes rather than live on
    %% uncounted (`beamtalk_ws_conn_cap`).
    cap_owner_mon :: reference() | undefined
}).
