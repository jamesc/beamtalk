%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% WebSocket handler session state (beamtalk_ws_handler).
%%%
%%% Extracted to a shared header so EUnit suites can construct handler
%%% state and drive the websocket_handle/2 + websocket_info/2 callbacks
%%% directly (deterministic coverage without a live WS client). BT-2389.

-record(ws_state, {
    session_id :: binary() | undefined,
    session_pid :: pid() | undefined,
    session_mon :: reference() | undefined,
    authenticated :: boolean(),
    peer :: term(),
    %% BT-696: Pending eval request for streaming output correlation
    pending_eval :: term() | undefined,
    %% BT-698: IO capture process pid and correlation ref for stdin routing
    io_capture_pid :: pid() | undefined,
    stdin_ref :: reference() | undefined,
    %% BT-1433: Whether this session is subscribed to log streaming
    log_subscribed :: boolean()
}).
