%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_version).

%%% **DDD Context:** Runtime Context

-moduledoc """
Workspace-side version reporting for the desktop-attach readiness handshake.

ADR 0097 (Desktop Attach Client) specifies that the front's `/readiness`
endpoint performs a version handshake: the workspace reports its
runtime/protocol version and the front refuses or warns on mismatch
(Implementation §1c, Consequences → "version skew"). Before this module,
there was no callable RPC target on the workspace node for that report —
`beamtalk_repl_ops`/`beamtalk_repl_protocol` (the modules
`editors/liveview/lib/bt_attach/workspace.ex` already RPCs into) expose no
version query of their own.

`get/0` is the RPC target: the front calls it the same way it already calls
other workspace-internal functions, e.g.
`:rpc.call(WorkspaceNode, beamtalk_version, get, [])`.

This module is purely additive — it does not change any existing RPC call
site or the shape of any existing response.
""".

-export([get/0]).

-export_type([version_report/0]).

-type version_report() :: #{
    runtime_version := binary(),
    protocol_version := binary(),
    otp_release := binary(),
    erts_version := binary()
}.

-doc """
Return a structured version report for the desktop-attach readiness handshake.

Fields:
- `runtime_version` — the `beamtalk_runtime` application version, derived
  from the repo-root `VERSION` file plus build metadata (see
  `scripts/version.escript`). `<<"unknown">>` if the application is not
  loaded, which should not happen on a running workspace node.
- `protocol_version` — the RPC/protocol surface version that
  `beamtalk_repl_ops`/`beamtalk_repl_protocol` implement. Mirrors the
  `<<"protocol">>` value already reported by the `describe` op
  (`beamtalk_repl_ops_dev:handle_term/4`) — keep the two literals in sync
  when the wire protocol changes.
- `otp_release` / `erts_version` — the hosting OTP release and ERTS
  version, so the front can also detect an Erlang distribution-protocol
  mismatch (ADR 0097 Consequences: "the dist protocol itself must also
  stay OTP-compatible across the gap").
""".
-spec get() -> version_report().
get() ->
    #{
        runtime_version => app_vsn(beamtalk_runtime),
        %% Keep in sync with the "protocol" value in
        %% beamtalk_repl_ops_dev:handle_term(<<"describe">>, ...) (BT-2091).
        protocol_version => <<"2.0">>,
        otp_release => iolist_to_binary(erlang:system_info(otp_release)),
        erts_version => iolist_to_binary(erlang:system_info(version))
    }.

%%====================================================================
%% Internal helpers
%%====================================================================

-doc "Look up an application's version as a binary, or `<<\"unknown\">>` if unavailable.".
-spec app_vsn(atom()) -> binary().
app_vsn(App) ->
    case application:get_key(App, vsn) of
        {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
        {ok, Vsn} when is_binary(Vsn) -> Vsn;
        _ -> <<"unknown">>
    end.
