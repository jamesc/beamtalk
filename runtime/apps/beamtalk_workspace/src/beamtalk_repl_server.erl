%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_server).
-behaviour(gen_server).

%%% **DDD Context:** REPL Session Context

-moduledoc """
WebSocket server for Beamtalk REPL (ADR 0020).

Starts a cowboy WebSocket listener for the REPL protocol. This is the shared
transport used by the CLI REPL, MCP, and LSP clients, which connect over
`ws://host:port/ws`; the Phoenix LiveView IDE instead reaches the term-op
layer directly over dist Attach (ADR 0017 Phase 3).
Client connections are handled by beamtalk_ws_handler.
This module manages the listener lifecycle, port/nonce discovery,
and protocol request dispatch.

The Phase-1 vanilla-JS browser workspace (static HTML/JS/CSS served from
`priv/`, the `/` and `/static/[...]` routes, and the optional separate `--web`
HTTP listener) was removed in BT-2415; it is superseded by the Phoenix LiveView
IDE (ADR 0017 Phase 3). Only the `/ws` protocol transport remains.

Op handlers are delegated to domain-specific modules (BT-705):
- beamtalk_repl_ops_eval: eval, clear, bindings
- beamtalk_repl_ops_load: load-source, load-project, unload
- beamtalk_repl_ops_actors: actors, inspect, kill, interrupt
- beamtalk_repl_ops_session: sessions, clone, close, health, shutdown
- beamtalk_repl_ops_dev: complete, describe, show-codegen
- beamtalk_repl_ops_perf: enable-tracing, get-traces, actor-stats, export-traces (ADR 0069)

The deprecated ops `docs`, `load-file`, `reload`, and `modules` were removed
in BT-2091. Use the Beamtalk-native message sends instead:
- `docs` → `Beamtalk help: ClassName` (optionally `selector: #sel`)
- `load-file` → `Workspace load: "path"`
- `reload` → `ClassName reload`
- `modules` → `Workspace classes`
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    start_link/1,
    get_port/0,
    get_nonce/0,
    handle_protocol_request/2,
    generate_session_id/0,
    parse_request/1,
    safe_to_existing_atom/1,
    ensure_structured_error/1
]).
-ifdef(TEST).
-export([
    generate_nonce/0,
    validate_actor_pid/1,
    is_known_actor/1,
    get_completions/1,
    resolve_class_to_module/1,
    ensure_structured_error/2,
    make_class_not_found_error/1,
    format_name/1,
    base_protocol_response/1,
    resolve_module_atoms/2,
    handle_op/4,
    write_port_file/3
]).
-endif.

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(LISTENER_REF, beamtalk_repl_ws).

-record(state, {
    port :: inet:port_number(),
    workspace_id :: binary() | undefined,
    nonce :: binary()
}).

%%% Public API

-doc """
Start the REPL WebSocket server (ADR 0020).
Listens on the specified port for incoming REPL connections.
Accepts a map with `port` key, or a plain port number for backward compatibility.
""".
-spec start_link(map() | inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(#{port := _} = Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []);
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{port => Port}, []).

-doc """
Get the actual port the server is listening on.
Useful when port 0 was requested (OS-assigned ephemeral port).
""".
-spec get_port() -> {ok, inet:port_number()}.
get_port() ->
    gen_server:call(?MODULE, get_port).

-doc """
Get the nonce for this server instance.
Used for stale port file detection (BT-611).
""".
-spec get_nonce() -> {ok, binary()}.
get_nonce() ->
    gen_server:call(?MODULE, get_nonce).

%%% gen_server callbacks

init(Config) ->
    Port = maps:get(port, Config),
    WorkspaceId = maps:get(workspace_id, Config, undefined),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
    %% Generate a random nonce for stale port file detection (BT-611)
    Nonce = generate_nonce(),
    %% BT-666: Session registry for interrupt routing.
    beamtalk_session_table:new(),
    %% ADR 0020: Start cowboy WebSocket listener.
    %% Default: bind to 127.0.0.1 so only local processes can connect.
    %% --bind flag allows binding to other addresses (BT-691).
    %% Cookie handshake in beamtalk_ws_handler provides auth on shared machines.
    %% Only the `/ws` protocol route remains; the Phase-1 browser static routes
    %% (`/`, `/static/[...]`) were removed in BT-2415 (ADR 0017 Phase 3).
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", beamtalk_ws_handler, []}
        ]}
    ]),
    TransportOpts = #{
        socket_opts => [{ip, BindAddr}, {port, Port}],
        num_acceptors => 10
    },
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(?LISTENER_REF, TransportOpts, ProtocolOpts) of
        {ok, _Pid} ->
            %% Discover actual bound port (important when Port=0 for ephemeral port)
            ActualPort = ranch:get_port(?LISTENER_REF),
            %% Write port file so CLI can discover the actual port and nonce
            write_port_file(WorkspaceId, ActualPort, Nonce),
            {ok, #state{
                port = ActualPort,
                workspace_id = WorkspaceId,
                nonce = Nonce
            }};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(get_nonce, _From, State) ->
    {reply, {ok, State#state.nonce}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(shutdown_requested, State) ->
    %% Shutdown endpoint (BT-611): initiate OTP-level graceful teardown.
    ?LOG_INFO("Executing requested shutdown", #{domain => [beamtalk, runtime]}),
    init:stop(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{port = Port}) ->
    ?LOG_INFO("REPL server shutting down", #{
        reason => Reason, port => Port, domain => [beamtalk, runtime]
    }),
    cowboy:stop_listener(?LISTENER_REF),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Port File

-doc """
Write the actual bound port to a file in the workspace directory.
This is needed when port=0 is used (OS assigns ephemeral port) so the
CLI can discover the actual port after BEAM startup.
""".
-spec write_port_file(binary() | undefined, inet:port_number(), binary()) -> ok.
write_port_file(undefined, _Port, _Nonce) ->
    ok;
write_port_file(WorkspaceId, Port, Nonce) ->
    case beamtalk_platform:home_dir() of
        false ->
            ?LOG_WARNING(
                "HOME/USERPROFILE not set; skipping port file write for workspace ~p",
                [WorkspaceId],
                #{domain => [beamtalk, runtime]}
            ),
            ok;
        Home ->
            PortFilePath = filename:join([
                Home,
                ".beamtalk",
                "workspaces",
                binary_to_list(WorkspaceId),
                "port"
            ]),
            case filelib:ensure_dir(PortFilePath) of
                ok ->
                    %% Format: PORT\nNONCE (two lines for stale detection, BT-611)
                    Content = [integer_to_list(Port), "\n", binary_to_list(Nonce)],
                    %% Write atomically via tmp + rename(2) so the Rust CLI never
                    %% reads a partially-written port file (file:write_file/2 is
                    %% open→write→close, not atomic on its own).
                    TmpPath = PortFilePath ++ ".tmp",
                    case file:write_file(TmpPath, Content) of
                        ok ->
                            case file:rename(TmpPath, PortFilePath) of
                                ok ->
                                    ?LOG_DEBUG(
                                        "Wrote port file: ~s (port ~p, nonce ~s)",
                                        [
                                            PortFilePath, Port, Nonce
                                        ],
                                        #{domain => [beamtalk, runtime]}
                                    ),
                                    ok;
                                {error, RenReason} ->
                                    ?LOG_WARNING(
                                        "Failed to rename port tmp file ~s -> ~s: ~p; "
                                        "falling back to direct write",
                                        [TmpPath, PortFilePath, RenReason],
                                        #{domain => [beamtalk, runtime]}
                                    ),
                                    _ = file:delete(TmpPath),
                                    %% Fallback: write directly so the CLI can still
                                    %% discover the port even if rename(2) fails (e.g.
                                    %% cross-device move on unusual mount layouts).
                                    case file:write_file(PortFilePath, Content) of
                                        ok ->
                                            ?LOG_DEBUG(
                                                "Wrote port file (fallback): ~s (port ~p, nonce ~s)",
                                                [PortFilePath, Port, Nonce],
                                                #{domain => [beamtalk, runtime]}
                                            ),
                                            ok;
                                        {error, FbReason} ->
                                            ?LOG_WARNING(
                                                "Fallback write of port file ~s failed: ~p",
                                                [PortFilePath, FbReason],
                                                #{domain => [beamtalk, runtime]}
                                            ),
                                            ok
                                    end
                            end;
                        {error, Reason} ->
                            ?LOG_WARNING(
                                "Failed to write port file ~s: ~p",
                                [TmpPath, Reason],
                                #{domain => [beamtalk, runtime]}
                            ),
                            ok
                    end;
                {error, Reason} ->
                    ?LOG_WARNING(
                        "Failed to create directory for port file ~s: ~p",
                        [PortFilePath, Reason],
                        #{domain => [beamtalk, runtime]}
                    ),
                    ok
            end
    end.

%%% Nonce Generation

-doc """
Generate a random nonce for stale port file detection (BT-611).
Returns a 16-character hex string.
""".
-spec generate_nonce() -> binary().
generate_nonce() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary(
        lists:flatten(
            [io_lib:format("~2.16.0b", [B]) || <<B>> <= Bytes]
        )
    ).

%%% Session ID Generation

-doc """
Generate a unique session ID.
Uses crypto:strong_rand_bytes for unpredictable IDs (prevents session ID guessing).
""".
-spec generate_session_id() -> binary().
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    RandomBytes = crypto:strong_rand_bytes(8),
    RandomHex = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= RandomBytes]),
    list_to_binary(io_lib:format("session_~p_~s", [Timestamp, RandomHex])).

%%% Protocol-aware request handling

-doc """
Handle a protocol message by dispatching to appropriate API.
Returns JSON binary response using protocol encoding.
Called by beamtalk_ws_handler after authentication.
""".
handle_protocol_request(Msg, SessionPid) ->
    Op = beamtalk_repl_protocol:get_op(Msg),
    Params = beamtalk_repl_protocol:get_params(Msg),
    try
        handle_op(Op, Params, Msg, SessionPid)
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR("REPL protocol handler crash", #{
                class => Class,
                reason => Reason,
                stack => lists:sublist(Stack, 5),
                op => Op,
                domain => [beamtalk, runtime]
            }),
            WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason, Class),
            beamtalk_repl_json:encode_error(WrappedReason, Msg)
    end.

-doc """
Dispatch a protocol op and encode the result for the WebSocket transport.

Routing and term-result production live in `beamtalk_repl_ops:dispatch/4`; JSON
encoding happens at this transport edge via `beamtalk_repl_ops:encode/2`
(BT-2399, ADR 0017 Phase 3). The browser wire format is unchanged. Dist-attached
clients (Phoenix LiveView, runtime-attached LSP / MCP) call
`beamtalk_repl_ops:dispatch/4` directly and consume the live term result without
this JSON step.

The deprecated ops `docs`, `load-file`, `reload`, and `modules` were removed in
BT-2091 (protocol 2.0); sending them now returns `unknown_op`. The `bindings`
and `clear` ops were removed in BT-2369 (ADR 0081 Phase 6) — session state is
now read and mutated through the Beamtalk-native `Session` API
(`Session current bindings`, `Session current clear`) via `eval`, so sending
`bindings`/`clear` now returns `unknown_op`.
""".
handle_op(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(
        beamtalk_repl_ops:dispatch(Op, Params, Msg, SessionPid), Msg
    ).

%%% Protocol Parsing and Formatting

-doc """
Parse a request from the CLI (legacy interface).
Expected format: JSON with "type" field.
New code should use beamtalk_repl_protocol:decode/1 instead.
Implementation lives in beamtalk_repl_protocol (extracted BT-865).
""".
-spec parse_request(binary()) ->
    {eval, string()}
    | {load_source, binary()}
    | {list_actors}
    | {kill_actor, string()}
    | {health}
    | {shutdown, string()}
    | {error, term()}.
parse_request(Data) -> beamtalk_repl_protocol:parse_request(Data).

%%% Delegated helpers (BT-705)
%%%
%%% These functions have been extracted to domain-specific ops modules.
%%% Thin delegates are kept here so existing TEST exports and callers
%%% continue to work without modification.

-ifdef(TEST).
-doc "Delegate to beamtalk_repl_ops_dev.".
base_protocol_response(Msg) -> beamtalk_repl_ops_dev:base_protocol_response(Msg).

-doc "Delegate to beamtalk_repl_ops_actors.".
validate_actor_pid(PidStr) -> beamtalk_repl_ops_actors:validate_actor_pid(PidStr).
is_known_actor(Pid) -> beamtalk_repl_ops_actors:is_known_actor(Pid).

-doc "Delegate to beamtalk_repl_ops_load.".
resolve_class_to_module(ClassName) -> beamtalk_repl_ops_load:resolve_class_to_module(ClassName).
resolve_module_atoms(ModuleAtom, Classes) ->
    beamtalk_repl_ops_load:resolve_module_atoms(ModuleAtom, Classes).

-doc "Delegate to beamtalk_repl_ops_dev.".
get_completions(Prefix) -> beamtalk_repl_ops_dev:get_completions(Prefix).
make_class_not_found_error(ClassName) ->
    beamtalk_repl_ops_dev:make_class_not_found_error(ClassName).

-doc "Delegate to beamtalk_repl_errors (extracted BT-865).".
ensure_structured_error(Reason, Class) ->
    beamtalk_repl_errors:ensure_structured_error(Reason, Class).
format_name(Name) -> beamtalk_repl_errors:format_name(Name).
-endif.

-doc """
Safely convert a binary to an existing atom, returning error instead of creating new atoms.
Implementation lives in beamtalk_repl_errors (extracted BT-865).
""".
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(Bin) -> beamtalk_repl_errors:safe_to_existing_atom(Bin).

-doc """
Ensure an error reason is a structured #beamtalk_error{} record.
Implementation lives in beamtalk_repl_errors (extracted BT-865).
""".
-spec ensure_structured_error(term()) -> #beamtalk_error{}.
ensure_structured_error(Reason) -> beamtalk_repl_errors:ensure_structured_error(Reason).
