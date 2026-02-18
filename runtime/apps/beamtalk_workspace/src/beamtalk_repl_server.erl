%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc WebSocket server for Beamtalk REPL (ADR 0020).
%%%
%%% **DDD Context:** REPL
%%%
%%% Starts a cowboy HTTP/WebSocket listener for the REPL protocol.
%%% Client connections are handled by beamtalk_ws_handler.
%%% This module manages the listener lifecycle, port/nonce discovery,
%%% and protocol request dispatch.

-module(beamtalk_repl_server).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1, get_port/0, get_web_port/0, get_nonce/0, handle_protocol_request/2,
         generate_session_id/0, parse_request/1, safe_to_existing_atom/1,
         ensure_structured_error/1]).
-ifdef(TEST).
-export([generate_nonce/0, validate_actor_pid/1, is_known_actor/1,
         get_completions/1, get_symbol_info/1, resolve_class_to_module/1,
         ensure_structured_error/2,
         make_class_not_found_error/1, format_name/1,
         base_protocol_response/1,
         resolve_module_atoms/2,
         handle_op/4,
         write_port_file/3]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(LISTENER_REF, beamtalk_repl_ws).
-define(WEB_LISTENER_REF, beamtalk_web_http).

-record(state, {
    port :: inet:port_number(),
    workspace_id :: binary() | undefined,
    nonce :: binary(),
    web_port :: inet:port_number() | undefined
}).

%%% Public API

%% @doc Start the REPL WebSocket server (ADR 0020).
%% Listens on the specified port for incoming REPL connections.
%% Accepts a map with `port` key, or a plain port number for backward compatibility.
-spec start_link(map() | inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(#{port := _} = Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []);
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{port => Port}, []).

%% @doc Get the actual port the server is listening on.
%% Useful when port 0 was requested (OS-assigned ephemeral port).
-spec get_port() -> {ok, inet:port_number()}.
get_port() ->
    gen_server:call(?MODULE, get_port).

%% @doc Get the nonce for this server instance.
%% Used for stale port file detection (BT-611).
-spec get_nonce() -> {ok, binary()}.
get_nonce() ->
    gen_server:call(?MODULE, get_nonce).

%% @doc Get the web port if a separate browser listener was started.
%% Returns `undefined` if --web-port was not specified.
-spec get_web_port() -> {ok, inet:port_number() | undefined}.
get_web_port() ->
    gen_server:call(?MODULE, get_web_port).

%%% gen_server callbacks

%% @private
init(Config) ->
    Port = maps:get(port, Config),
    WorkspaceId = maps:get(workspace_id, Config, undefined),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
    WebPort = maps:get(web_port, Config, undefined),
    %% Generate a random nonce for stale port file detection (BT-611)
    Nonce = generate_nonce(),
    %% BT-666: Session registry for interrupt routing
    ets:new(beamtalk_sessions, [named_table, public, {read_concurrency, true}]),
    %% ADR 0020: Start cowboy WebSocket listener.
    %% Default: bind to 127.0.0.1 so only local processes can connect.
    %% --bind flag allows binding to other addresses (BT-691).
    %% Cookie handshake in beamtalk_ws_handler provides auth on shared machines.
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", beamtalk_ws_handler, []},
            {"/", cowboy_static, {priv_file, beamtalk_workspace, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, beamtalk_workspace, "static"}}
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
            %% BT-689: Optionally start a separate browser HTTP listener on web_port
            ActualWebPort = maybe_start_web_listener(WebPort, BindAddr),
            {ok, #state{port = ActualPort,
                        workspace_id = WorkspaceId,
                        nonce = Nonce,
                        web_port = ActualWebPort}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

%% @private
handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(get_nonce, _From, State) ->
    {reply, {ok, State#state.nonce}, State};
handle_call(get_web_port, _From, State) ->
    {reply, {ok, State#state.web_port}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(shutdown_requested, State) ->
    %% Shutdown endpoint (BT-611): initiate OTP-level graceful teardown.
    ?LOG_INFO("Executing requested shutdown", #{}),
    init:stop(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, #state{port = Port, web_port = WebPort}) ->
    ?LOG_INFO("REPL server shutting down", #{reason => Reason, port => Port}),
    cowboy:stop_listener(?LISTENER_REF),
    case WebPort of
        undefined -> ok;
        _ -> cowboy:stop_listener(?WEB_LISTENER_REF)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Web Listener (BT-689)

%% @private
%% @doc Optionally start a separate cowboy HTTP listener for browser access.
%% When web_port is undefined, no additional listener is started.
%% When web_port is specified, starts a listener serving the browser UI
%% on that port, separate from the WebSocket protocol port.
-spec maybe_start_web_listener(undefined | inet:port_number(), inet:ip_address()) ->
    undefined | inet:port_number().
maybe_start_web_listener(undefined, _BindAddr) ->
    undefined;
maybe_start_web_listener(WebPort, BindAddr) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", beamtalk_ws_handler, []},
            {"/", cowboy_static, {priv_file, beamtalk_workspace, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, beamtalk_workspace, "static"}}
        ]}
    ]),
    TransportOpts = #{
        socket_opts => [{ip, BindAddr}, {port, WebPort}],
        num_acceptors => 5
    },
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(?WEB_LISTENER_REF, TransportOpts, ProtocolOpts) of
        {ok, _Pid} ->
            ActualWebPort = ranch:get_port(?WEB_LISTENER_REF),
            ?LOG_INFO("Browser workspace listener started", #{port => ActualWebPort}),
            ActualWebPort;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start browser workspace listener", #{
                port => WebPort, reason => Reason
            }),
            undefined
    end.

%%% Port File

%% @private
%% @doc Write the actual bound port to a file in the workspace directory.
%% This is needed when port=0 is used (OS assigns ephemeral port) so the
%% CLI can discover the actual port after BEAM startup.
-spec write_port_file(binary() | undefined, inet:port_number(), binary()) -> ok.
write_port_file(undefined, _Port, _Nonce) ->
    ok;
write_port_file(WorkspaceId, Port, Nonce) ->
    case beamtalk_platform:home_dir() of
        false ->
            ?LOG_WARNING("HOME/USERPROFILE not set; skipping port file write for workspace ~p",
                           [WorkspaceId]),
            ok;
        Home ->
            PortFilePath = filename:join([Home, ".beamtalk", "workspaces",
                                          binary_to_list(WorkspaceId), "port"]),
            case filelib:ensure_dir(PortFilePath) of
                ok ->
                    %% Format: PORT\nNONCE (two lines for stale detection, BT-611)
                    Content = [integer_to_list(Port), "\n", binary_to_list(Nonce)],
                    case file:write_file(PortFilePath, Content) of
                        ok ->
                            ?LOG_DEBUG("Wrote port file: ~s (port ~p, nonce ~s)", [PortFilePath, Port, Nonce]),
                            ok;
                        {error, Reason} ->
                            ?LOG_WARNING("Failed to write port file ~s: ~p",
                                           [PortFilePath, Reason]),
                            ok
                    end;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to create directory for port file ~s: ~p",
                                   [PortFilePath, Reason]),
                    ok
            end
    end.

%%% Nonce Generation

%% @private
%% @doc Generate a random nonce for stale port file detection (BT-611).
%% Returns a 16-character hex string.
-spec generate_nonce() -> binary().
generate_nonce() ->
    Bytes = crypto:strong_rand_bytes(8),
    list_to_binary(lists:flatten(
        [io_lib:format("~2.16.0b", [B]) || <<B>> <= Bytes])).

%%% Session ID Generation

%% @doc Generate a unique session ID.
%% Uses crypto:strong_rand_bytes for unpredictable IDs (prevents session ID guessing).
-spec generate_session_id() -> binary().
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    RandomBytes = crypto:strong_rand_bytes(8),
    RandomHex = lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= RandomBytes]),
    list_to_binary(io_lib:format("session_~p_~s", [Timestamp, RandomHex])).

%%% Protocol-aware request handling

%% @doc Handle a protocol message by dispatching to appropriate API.
%% Returns JSON binary response using protocol encoding.
%% Called by beamtalk_ws_handler after authentication.
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
                op => Op
            }),
            WrappedReason = ensure_structured_error(Reason, Class),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end.

%% @private
handle_op(<<"eval">>, Params, Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to evaluate.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_result(
                        Result, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings);
                {error, ErrorReason, Output, Warnings} ->
                    WrappedReason = ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1, Output, Warnings)
            end
    end;

handle_op(<<"clear">>, _Params, Msg, SessionPid) ->
    ok = beamtalk_repl_shell:clear_bindings(SessionPid),
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"bindings">>, _Params, Msg, SessionPid) ->
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
    %% ADR 0019 Phase 3: Filter out workspace convenience bindings from display.
    %% Users shouldn't see Transcript/Beamtalk/Workspace as "their" bindings.
    UserBindings = maps:without(beamtalk_workspace_config:binding_names(), Bindings),
    beamtalk_repl_protocol:encode_bindings(
        UserBindings, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"load-file">>, Params, Msg, SessionPid) ->
    Path = binary_to_list(maps:get(<<"path">>, Params, <<>>)),
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun beamtalk_repl_json:term_to_json/1);
        {error, Reason} ->
            WrappedReason = ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end;

handle_op(<<"load-source">>, Params, Msg, SessionPid) ->
    Source = maps:get(<<"source">>, Params, <<>>),
    case Source of
        <<>> ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty source">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter Beamtalk source code to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_shell:load_source(SessionPid, Source) of
                {ok, Classes} ->
                    beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun beamtalk_repl_json:term_to_json/1);
                {error, Reason} ->
                    WrappedReason = ensure_structured_error(Reason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
            end
    end;

handle_op(<<"reload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case maps:get(<<"path">>, Params, undefined) of
        undefined when ModuleBin =/= <<>> ->
            case safe_to_existing_atom(ModuleBin) of
                {ok, ModuleAtom} ->
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
                    case beamtalk_repl_modules:get_module_info(ModuleAtom, Tracker) of
                        {ok, Info} ->
                            case beamtalk_repl_modules:get_source_file(Info) of
                                undefined ->
                                    Err0 = beamtalk_error:new(no_source_file, 'Module'),
                                    Err1 = beamtalk_error:with_message(Err0,
                                        iolist_to_binary([<<"No source file recorded for module: ">>,
                                                          ModuleBin])),
                                    Err2 = beamtalk_error:with_hint(Err1,
                                        <<"Use :load <path> to load it first.">>),
                                    beamtalk_repl_protocol:encode_error(
                                        Err2, Msg,
                                        fun beamtalk_repl_json:format_error_message/1);
                                SourcePath ->
                                    do_reload(SourcePath, ModuleAtom, Msg, SessionPid)
                            end;
                        {error, not_found} ->
                            Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                            Err1 = beamtalk_error:with_message(Err0,
                                iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                            Err2 = beamtalk_error:with_hint(Err1,
                                <<"Use :load <path> to load it first.">>),
                            beamtalk_repl_protocol:encode_error(
                                Err2, Msg,
                                fun beamtalk_repl_json:format_error_message/1)
                    end;
                {error, badarg} ->
                    %% Atom doesn't exist — module was never loaded
                    Err0 = beamtalk_error:new(module_not_loaded, 'Module'),
                    Err1 = beamtalk_error:with_message(Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                    Err2 = beamtalk_error:with_hint(Err1,
                        <<"Use :load <path> to load it first.">>),
                    beamtalk_repl_protocol:encode_error(
                        Err2, Msg,
                        fun beamtalk_repl_json:format_error_message/1)
            end;
        undefined ->
            Err0 = beamtalk_error:new(missing_argument, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0,
                <<"Missing module name for reload">>),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        Path ->
            PathStr = binary_to_list(Path),
            do_reload(PathStr, undefined, Msg, SessionPid)
    end;

handle_op(<<"actors">>, _Params, Msg, _SessionPid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            beamtalk_repl_protocol:encode_actors([], Msg, fun beamtalk_repl_json:term_to_json/1);
        RegistryPid ->
            Actors = beamtalk_repl_actors:list_actors(RegistryPid),
            beamtalk_repl_protocol:encode_actors(Actors, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;

handle_op(<<"inspect">>, Params, Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, <<>>)),
    PidBin = list_to_binary(PidStr),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            Err0 = beamtalk_error:new(Reason, 'Actor'),
            Err1 = beamtalk_error:with_message(Err0,
                iolist_to_binary([<<"Invalid actor PID: ">>, PidBin])),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Use :actors to list valid actor PIDs.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        {ok, Pid} ->
            case is_process_alive(Pid) of
                true ->
                    %% Get actor state via sys:get_state
                    try
                        State = sys:get_state(Pid, 5000),
                        InspectStr = case State of
                            M when is_map(M) ->
                                case beamtalk_tagged_map:is_tagged(M) of
                                    true -> beamtalk_reflection:inspect_string(M);
                                    false -> beamtalk_primitive:print_string(M)
                                end;
                            _ ->
                                iolist_to_binary(io_lib:format("~p", [State]))
                        end,
                        beamtalk_repl_protocol:encode_inspect(
                            InspectStr, Msg)
                    catch
                        _:_ ->
                            Err3 = beamtalk_error:new(inspect_failed, 'Actor'),
                            Err4 = beamtalk_error:with_message(Err3,
                                iolist_to_binary([<<"Failed to inspect actor: ">>, PidBin])),
                            beamtalk_repl_protocol:encode_error(
                                Err4, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end;
                false ->
                    Err3 = beamtalk_error:new(actor_not_alive, 'Actor'),
                    Err4 = beamtalk_error:with_message(Err3,
                        iolist_to_binary([<<"Actor is not alive: ">>, PidBin])),
                    beamtalk_repl_protocol:encode_error(
                        Err4, Msg, fun beamtalk_repl_json:format_error_message/1)
            end
    end;

handle_op(<<"kill">>, Params, Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, maps:get(<<"pid">>, Params, <<>>))),
    case validate_actor_pid(PidStr) of
        {error, Reason} ->
            PidBin = list_to_binary(PidStr),
            Err0 = beamtalk_error:new(Reason, 'Actor'),
            Err1 = beamtalk_error:with_message(Err0,
                iolist_to_binary([<<"Invalid actor PID: ">>, PidBin])),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Use :actors to list valid actor PIDs.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        {ok, Pid} ->
            exit(Pid, kill),
            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;

handle_op(<<"interrupt">>, Params, Msg, SessionPid) ->
    %% BT-666: Interrupt a running evaluation.
    %% If a session ID is provided, look up the target session (for cross-connection interrupt).
    %% Otherwise, interrupt the current connection's session.
    TargetPid = case maps:get(<<"session">>, Params, undefined) of
        undefined ->
            SessionPid;
        TargetSession ->
            case ets:lookup(beamtalk_sessions, TargetSession) of
                [{_, Pid}] -> Pid;
                [] ->
                    ?LOG_WARNING("Interrupt target session not found", #{
                        session => TargetSession
                    }),
                    SessionPid
            end
    end,
    try
        ok = beamtalk_repl_shell:interrupt(TargetPid),
        beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    catch
        exit:{noproc, _} ->
            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;

handle_op(<<"modules">>, _Params, Msg, SessionPid) ->
    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
    TrackedModules = beamtalk_repl_modules:list_modules(Tracker),
    RegistryPid = whereis(beamtalk_actor_registry),
    ModulesWithInfo = lists:map(
        fun({ModName, ModInfo}) ->
            ActorCount = beamtalk_repl_modules:get_actor_count(ModName, RegistryPid, Tracker),
            Info = beamtalk_repl_modules:format_module_info(ModInfo, ActorCount),
            {ModName, Info}
        end,
        TrackedModules
    ),
    beamtalk_repl_protocol:encode_modules(ModulesWithInfo, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"unload">>, Params, Msg, SessionPid) ->
    ModuleBin = maps:get(<<"module">>, Params, <<>>),
    case ModuleBin of
        <<>> ->
            Err0 = beamtalk_error:new(missing_argument, 'Module'),
            Err1 = beamtalk_error:with_message(Err0,
                <<"Missing module name for unload">>),
            Err2 = beamtalk_error:with_hint(Err1,
                <<"Usage: :unload <ModuleName>">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case safe_to_existing_atom(ModuleBin) of
                {error, badarg} ->
                    %% Atom never created — module was never loaded
                    Err0 = beamtalk_error:new(module_not_found, 'Module'),
                    Err1 = beamtalk_error:with_message(Err0,
                        iolist_to_binary([<<"Module not loaded: ">>, ModuleBin])),
                    Err2 = beamtalk_error:with_hint(Err1, <<"Module was never loaded">>),
                    beamtalk_repl_protocol:encode_error(
                        Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
                {ok, Module} ->
                    %% Resolve class name to BEAM module name if needed
                    ResolvedModule = case code:is_loaded(Module) of
                        {file, _} -> Module;
                        false -> resolve_class_to_module(Module)
                    end,
                    case beamtalk_repl_shell:unload_module(SessionPid, ResolvedModule) of
                        ok ->
                            beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);
                        {error, #beamtalk_error{} = Err} ->
                            beamtalk_repl_protocol:encode_error(
                                Err, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end
            end
    end;

handle_op(<<"sessions">>, _Params, Msg, _SessionPid) ->
    %% List active sessions from supervisor (may not be started in legacy mode)
    case whereis(beamtalk_session_sup) of
        undefined ->
            beamtalk_repl_protocol:encode_sessions([], Msg, fun beamtalk_repl_json:term_to_json/1);
        _Sup ->
            Children = supervisor:which_children(beamtalk_session_sup),
            Sessions = lists:filtermap(
                fun({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                    {true, #{id => list_to_binary(pid_to_list(Pid))}};
                   (_) ->
                    false
                end,
                Children
            ),
            beamtalk_repl_protocol:encode_sessions(Sessions, Msg, fun beamtalk_repl_json:term_to_json/1)
    end;

handle_op(<<"clone">>, _Params, Msg, _SessionPid) ->
    %% Create a new session
    NewSessionId = generate_session_id(),
    case beamtalk_session_sup:start_session(NewSessionId) of
        {ok, _NewPid} ->
            beamtalk_repl_protocol:encode_result(
                NewSessionId, Msg, fun beamtalk_repl_json:term_to_json/1);
        {error, Reason} ->
            Err0 = beamtalk_error:new(session_error, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0,
                iolist_to_binary([<<"Failed to create session: ">>,
                    io_lib:format("~p", [Reason])])),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1)
    end;

handle_op(<<"close">>, _Params, Msg, _SessionPid) ->
    %% Close the current session - acceptor cleanup handles stop
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"complete">>, Params, Msg, _SessionPid) ->
    %% Autocompletion - return matching identifiers from loaded modules
    Prefix = maps:get(<<"code">>, Params, <<>>),
    Completions = get_completions(Prefix),
    case beamtalk_repl_protocol:is_legacy(Msg) of
        true ->
            jsx:encode(#{<<"type">> => <<"completions">>,
                        <<"completions">> => Completions});
        false ->
            Base = base_protocol_response(Msg),
            jsx:encode(Base#{<<"completions">> => Completions, <<"status">> => [<<"done">>]})
    end;

handle_op(<<"info">>, Params, Msg, _SessionPid) ->
    %% Symbol info lookup
    Symbol = maps:get(<<"symbol">>, Params, <<>>),
    Info = get_symbol_info(Symbol),
    case beamtalk_repl_protocol:is_legacy(Msg) of
        true ->
            jsx:encode(#{<<"type">> => <<"info">>, <<"info">> => Info});
        false ->
            Base = base_protocol_response(Msg),
            jsx:encode(Base#{<<"info">> => Info, <<"status">> => [<<"done">>]})
    end;

handle_op(<<"docs">>, Params, Msg, _SessionPid) ->
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    case safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            beamtalk_repl_protocol:encode_error(
                make_class_not_found_error(ClassBin), Msg, fun beamtalk_repl_json:format_error_message/1);
        {ok, ClassName} ->
            Selector = maps:get(<<"selector">>, Params, undefined),
            case Selector of
                undefined ->
                    case beamtalk_repl_docs:format_class_docs(ClassName) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName), Msg, fun beamtalk_repl_json:format_error_message/1)
                    end;
                SelectorBin ->
                    case beamtalk_repl_docs:format_method_doc(ClassName, SelectorBin) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName), Msg, fun beamtalk_repl_json:format_error_message/1);
                        {error, {method_not_found, _, _}} ->
                            NameBin = to_binary(ClassName),
                            SelectorAtom = binary_to_atom(SelectorBin, utf8),
                            Err0 = beamtalk_error:new(does_not_understand, ClassName),
                            Err1 = beamtalk_error:with_selector(Err0, SelectorAtom),
                            Err2 = beamtalk_error:with_message(Err1,
                                iolist_to_binary([NameBin, <<" does not understand ">>, SelectorBin])),
                            Err3 = beamtalk_error:with_hint(Err2,
                                iolist_to_binary([<<"Use :help ">>, NameBin, <<" to see available methods.">>])),
                            beamtalk_repl_protocol:encode_error(
                                Err3, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end
            end
    end;

handle_op(<<"describe">>, _Params, Msg, _SessionPid) ->
    %% Capability discovery — returns supported ops, protocol version, and
    %% server capabilities for the authenticated REPL connection (ADR 0020).
    Ops = describe_ops(),
    BeamtalkVsnBin =
        case application:get_key(beamtalk_workspace, vsn) of
            {ok, Vsn} when is_list(Vsn)   -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _                             -> <<"0.1.0">>
        end,
    Versions = #{
        <<"protocol">> => <<"1.0">>,
        <<"beamtalk">> => BeamtalkVsnBin
    },
    beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg);

handle_op(<<"health">>, _Params, Msg, _SessionPid) ->
    %% Health probe — returns workspace_id and nonce for stale detection (BT-611).
    %% No authentication required (read-only, equivalent to filesystem info).
    {ok, Nonce} = get_nonce(),
    WorkspaceId = case beamtalk_workspace_meta:get_metadata() of
        {ok, Meta} -> maps:get(workspace_id, Meta, <<>>);
        {error, _} -> <<>>
    end,
    Base = beamtalk_repl_protocol:base_response(Msg),
    jsx:encode(Base#{<<"workspace_id">> => WorkspaceId,
                     <<"nonce">> => Nonce,
                     <<"status">> => [<<"done">>]});

handle_op(<<"shutdown">>, Params, Msg, _SessionPid) ->
    %% Graceful shutdown — cookie-authenticated (BT-611).
    %% Triggers init:stop() for OTP-level supervisor tree teardown.
    ProvidedCookie = maps:get(<<"cookie">>, Params, <<>>),
    NodeCookie = atom_to_binary(erlang:get_cookie(), utf8),
    %% Timing-safe comparison to prevent side-channel attacks.
    %% crypto:hash_equals/2 raises badarg when binary lengths differ.
    ValidCookie = is_binary(ProvidedCookie)
        andalso byte_size(ProvidedCookie) =:= byte_size(NodeCookie)
        andalso crypto:hash_equals(ProvidedCookie, NodeCookie),
    case ValidCookie of
        true ->
            ?LOG_INFO("Shutdown requested via protocol", #{}),
            %% Schedule shutdown via gen_server handle_info after response is sent.
            erlang:send_after(100, ?MODULE, shutdown_requested),
            beamtalk_repl_protocol:encode_status(ok, Msg,
                fun beamtalk_repl_json:term_to_json/1);
        false ->
            ?LOG_WARNING("Shutdown rejected: invalid cookie", #{}),
            Err0 = beamtalk_error:new(auth_error, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0, <<"Invalid cookie">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Provide the correct node cookie for shutdown.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg,
                fun beamtalk_repl_json:format_error_message/1)
    end;

handle_op(<<"show-codegen">>, Params, Msg, SessionPid) ->
    %% BT-700: Compile expression and return Core Erlang source without evaluating.
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_shell:show_codegen(SessionPid, Code) of
                {ok, CoreErlang, Warnings} ->
                    Base = beamtalk_repl_protocol:base_response(Msg),
                    Result = Base#{<<"core_erlang">> => CoreErlang,
                                   <<"status">> => [<<"done">>]},
                    Result1 = case Warnings of
                        [] -> Result;
                        _ -> Result#{<<"warnings">> => Warnings}
                    end,
                    jsx:encode(Result1);
                {error, ErrorReason, Warnings} ->
                    WrappedReason = ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1, <<>>, Warnings)
            end
    end;

handle_op(Op, _Params, Msg, _SessionPid) ->
    Err0 = beamtalk_error:new(unknown_op, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Unknown operation: ">>, Op])),
    beamtalk_repl_protocol:encode_error(
        Err1, Msg, fun beamtalk_repl_json:format_error_message/1).

%%% Describe Metadata

%% @doc Returns the map of supported operations with their parameters.
%% Centralised here so describe responses and future tests share a single source.
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"eval">>        => #{<<"params">> => [<<"code">>]},
        <<"stdin">>       => #{<<"params">> => [<<"value">>]},
        <<"complete">>    => #{<<"params">> => [<<"code">>]},
        <<"info">>        => #{<<"params">> => [<<"symbol">>]},
        <<"docs">>        => #{<<"params">> => [<<"class">>],
                               <<"optional">> => [<<"selector">>]},
        <<"load-file">>   => #{<<"params">> => [<<"path">>]},
        <<"load-source">> => #{<<"params">> => [<<"source">>]},
        <<"reload">>      => #{<<"params">> => [],
                               <<"optional">> => [<<"module">>, <<"path">>]},
        <<"clear">>       => #{<<"params">> => []},
        <<"bindings">>    => #{<<"params">> => []},
        <<"sessions">>    => #{<<"params">> => []},
        <<"clone">>       => #{<<"params">> => []},
        <<"close">>       => #{<<"params">> => []},
        <<"actors">>      => #{<<"params">> => []},
        <<"inspect">>     => #{<<"params">> => [<<"actor">>]},
        <<"kill">>        => #{<<"params">> => [<<"actor">>]},
        <<"interrupt">>   => #{<<"params">> => []},
        <<"modules">>     => #{<<"params">> => []},
        <<"unload">>      => #{<<"params">> => [<<"module">>]},
        <<"health">>      => #{<<"params">> => []},
        <<"show-codegen">> => #{<<"params">> => [<<"code">>]},
        <<"describe">>    => #{<<"params">> => []},
        <<"shutdown">>    => #{<<"params">> => [<<"cookie">>]}
    }.

%%% Protocol Parsing and Formatting

%% @doc Parse a request from the CLI (legacy interface).
%% Expected format: JSON with "type" field.
%% New code should use beamtalk_repl_protocol:decode/1 instead.
-spec parse_request(binary()) -> 
    {eval, string()} | 
    {clear_bindings} | 
    {get_bindings} | 
    {load_file, string()} | 
    {load_source, binary()} |
    {list_actors} |
    {kill_actor, string()} |
    {list_modules} |
    {unload_module, string()} |
    {get_docs, binary(), binary() | undefined} |
    {health} |
    {shutdown, string()} |
    {error, term()}.
parse_request(Data) when is_binary(Data) ->
    try
        %% Remove trailing newline if present
        Trimmed = string:trim(Data),
        %% Try to parse as JSON
        case beamtalk_repl_json:parse_json(Trimmed) of
            {ok, #{<<"op">> := Op} = Map} ->
                %% New protocol format - translate to internal tuples
                op_to_request(Op, Map);
            {ok, #{<<"type">> := <<"eval">>, <<"expression">> := Expr}} ->
                {eval, binary_to_list(Expr)};
            {ok, #{<<"type">> := <<"clear">>}} ->
                {clear_bindings};
            {ok, #{<<"type">> := <<"bindings">>}} ->
                {get_bindings};
            {ok, #{<<"type">> := <<"load">>, <<"path">> := Path}} ->
                {load_file, binary_to_list(Path)};
            {ok, #{<<"type">> := <<"actors">>}} ->
                {list_actors};
            {ok, #{<<"type">> := <<"modules">>}} ->
                {list_modules};
            {ok, #{<<"type">> := <<"unload">>, <<"module">> := ModuleName}} ->
                {unload_module, binary_to_list(ModuleName)};
            {ok, #{<<"type">> := <<"kill">>, <<"pid">> := PidStr}} ->
                {kill_actor, binary_to_list(PidStr)};
            {ok, _Other} ->
                {error, {invalid_request, unknown_type}};
            {error, _Reason} ->
                %% Not JSON, treat as raw expression for robustness
                %% Supports manual testing (e.g., netcat) and third-party tools
                case Trimmed of
                    <<>> -> {error, empty_expression};
                    _ -> {eval, binary_to_list(Trimmed)}
                end
        end
    catch
        _:Error ->
            {error, {parse_error, Error}}
    end.

%% @private
%% @doc Translate a protocol operation name to an internal request tuple.
-spec op_to_request(binary(), map()) ->
    {eval, string()} | {clear_bindings} | {get_bindings} |
    {load_file, string()} | {load_source, binary()} |
    {list_actors} | {list_modules} |
    {kill_actor, string()} | {unload_module, string()} |
    {get_docs, binary(), binary() | undefined} |
    {health} | {shutdown, string()} | {error, term()}.
op_to_request(<<"eval">>, Map) ->
    Code = maps:get(<<"code">>, Map, <<>>),
    {eval, binary_to_list(Code)};
op_to_request(<<"clear">>, _Map) ->
    {clear_bindings};
op_to_request(<<"bindings">>, _Map) ->
    {get_bindings};
op_to_request(<<"load-file">>, Map) ->
    Path = maps:get(<<"path">>, Map, <<>>),
    {load_file, binary_to_list(Path)};
op_to_request(<<"load-source">>, Map) ->
    Source = maps:get(<<"source">>, Map, <<>>),
    {load_source, Source};
op_to_request(<<"actors">>, _Map) ->
    {list_actors};
op_to_request(<<"modules">>, _Map) ->
    {list_modules};
op_to_request(<<"unload">>, Map) ->
    Module = maps:get(<<"module">>, Map, <<>>),
    {unload_module, binary_to_list(Module)};
op_to_request(<<"kill">>, Map) ->
    Pid = maps:get(<<"actor">>, Map, maps:get(<<"pid">>, Map, <<>>)),
    {kill_actor, binary_to_list(Pid)};
op_to_request(<<"docs">>, Map) ->
    ClassName = maps:get(<<"class">>, Map, <<>>),
    Selector = maps:get(<<"selector">>, Map, undefined),
    {get_docs, ClassName, Selector};
op_to_request(<<"health">>, _Map) ->
    {health};
op_to_request(<<"shutdown">>, Map) ->
    Cookie = maps:get(<<"cookie">>, Map, <<>>),
    {shutdown, binary_to_list(Cookie)};
op_to_request(Op, _Map) ->
    {error, {unknown_op, Op}}.


%%% Reload helpers

%% @private
%% @doc Execute reload: load file, trigger code_change for affected actors,
%% and return response with actor count and migration results.
-spec do_reload(string(), atom() | undefined, beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
do_reload(Path, ModuleAtom, Msg, SessionPid) ->
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            %% Trigger sys:change_code/4 for affected actors
            {ActorCount, MigrationFailures} =
                trigger_actor_code_change(ModuleAtom, Classes),
            beamtalk_repl_json:encode_reloaded(Classes, ActorCount, MigrationFailures, Msg);
        {error, Reason} ->
            WrappedReason = ensure_structured_error(Reason),
            beamtalk_repl_protocol:encode_error(
                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end.

%% @private
%% @doc Trigger code_change for actors using the reloaded module.
%% Returns {ActorCount, Failures} where Failures is list of {Pid, Reason}.
-spec trigger_actor_code_change(atom() | undefined, [map()]) ->
    {non_neg_integer(), [{pid(), term()}]}.
trigger_actor_code_change(ModuleAtom, Classes) ->
    ModuleAtoms = lists:usort(resolve_module_atoms(ModuleAtom, Classes)),
    case whereis(beamtalk_actor_registry) of
        undefined -> {0, []};
        RegistryPid ->
            {Count, FailsRev} = lists:foldl(fun(Mod, {CountAcc, FailAcc}) ->
                case beamtalk_repl_actors:get_pids_for_module(RegistryPid, Mod) of
                    {ok, []} ->
                        {CountAcc, FailAcc};
                    {ok, Pids} ->
                        {ok, Upgraded, Failures} =
                            beamtalk_hot_reload:trigger_code_change(Mod, Pids),
                        NewFailAcc = lists:foldl(
                            fun(F, A) -> [F | A] end, FailAcc, Failures),
                        {CountAcc + Upgraded, NewFailAcc};
                    {error, _} ->
                        {CountAcc, FailAcc}
                end
            end, {0, []}, ModuleAtoms),
            {Count, lists:reverse(FailsRev)}
    end.

%% @private
%% @doc Resolve module atoms from an explicit module name or loaded class names.
-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(fun(ClassMap) ->
        case maps:get(name, ClassMap, "") of
            "" -> false;
            Name when is_list(Name) ->
                case safe_to_existing_atom(list_to_binary(Name)) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            Name when is_binary(Name) ->
                case safe_to_existing_atom(Name) of
                    {ok, Atom} -> {true, Atom};
                    {error, badarg} -> false
                end;
            _ -> false
        end
    end, Classes).


%%% Completion and Info helpers

%% @private
%% @doc Build base response map with id/session fields for protocol responses.
-spec base_protocol_response(term()) -> map().
base_protocol_response(Msg) ->
    Id = beamtalk_repl_protocol:get_id(Msg),
    Session = beamtalk_repl_protocol:get_session(Msg),
    M0 = #{},
    M1 = case Id of undefined -> M0; _ -> M0#{<<"id">> => Id} end,
    case Session of undefined -> M1; _ -> M1#{<<"session">> => Session} end.

%% @private
%% @doc Validate a PID string refers to a known Beamtalk actor.
%% Consolidates PID parsing + registry lookup used by inspect/kill operations.
-spec validate_actor_pid(string()) -> {ok, pid()} | {error, invalid_pid | unknown_actor}.
validate_actor_pid(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case is_known_actor(Pid) of
            true -> {ok, Pid};
            false -> {error, unknown_actor}
        end
    catch
        _:_ -> {error, invalid_pid}
    end.

%% @private
%% @doc Check whether a PID belongs to a registered Beamtalk actor.
-spec is_known_actor(pid()) -> boolean().
is_known_actor(Pid) when is_pid(Pid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            false;
        RegistryPid ->
            case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                {ok, _} -> true;
                {error, not_found} -> false
            end
    end.

%% @private
%% @doc Resolve a class name (e.g. 'Counter') to its BEAM module name (e.g. 'counter').
-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids = try beamtalk_class_registry:all_classes()
                catch _:_ -> [] end,
    resolve_class_to_module(ClassName, ClassPids).

resolve_class_to_module(ClassName, []) ->
    ClassName;
resolve_class_to_module(ClassName, [Pid | Rest]) ->
    try
        case gen_server:call(Pid, class_name, 1000) of
            ClassName ->
                gen_server:call(Pid, module_name, 1000);
            _ ->
                resolve_class_to_module(ClassName, Rest)
        end
    catch _:_ ->
        resolve_class_to_module(ClassName, Rest)
    end.

%% @private
%% @doc Get autocompletion suggestions matching a given prefix.
-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) -> [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    %% Collect completions from loaded class processes
    ClassPids = try beamtalk_class_registry:all_classes()
                catch _:_ -> [] end,
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_object_class:class_name(Pid),
                {true, atom_to_binary(Name, utf8)}
            catch _:_ -> false
            end
        end,
        ClassPids
    ),
    %% Filter by prefix
    [Name || Name <- ClassNames, binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}]
    ++
    %% Add workspace binding names (Transcript, Beamtalk, Workspace)
    [atom_to_binary(B, utf8) || B <- try beamtalk_workspace_config:binding_names()
                                      catch _:_ -> [] end,
     binary:match(atom_to_binary(B, utf8), Prefix) =:= {0, byte_size(Prefix)}]
    ++
    %% Add built-in keywords
    [Kw || Kw <- builtin_keywords(), binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
           PrefixStr =/= ""].

%% @private
%% @doc Return the list of built-in language keywords for autocompletion.
-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [<<"self">>, <<"super">>, <<"true">>, <<"false">>, <<"nil">>,
     <<"ifTrue:">>, <<"ifFalse:">>, <<"ifTrue:ifFalse:">>,
     <<"whileTrue:">>, <<"timesRepeat:">>,
     <<"subclass:">>, <<"spawn">>, <<"new">>].

%% @private
%% @doc Look up information about a symbol, returning whether it is a known class.
-spec get_symbol_info(binary()) -> map().
get_symbol_info(Symbol) when is_binary(Symbol) ->
    SymAtom = try binary_to_existing_atom(Symbol, utf8)
              catch _:_ -> undefined end,
    case SymAtom of
        undefined ->
            #{<<"found">> => false,
              <<"symbol">> => Symbol};
        _ ->
            %% Check if it's a known class
            IsClass = try
                ClassPids2 = beamtalk_class_registry:all_classes(),
                lists:any(fun(Pid) ->
                    try
                        beamtalk_object_class:class_name(Pid) =:= SymAtom
                    catch _:_ -> false
                    end
                end, ClassPids2)
            catch _:_ -> false end,
            case IsClass of
                true ->
                    #{<<"found">> => true,
                      <<"symbol">> => Symbol,
                      <<"kind">> => <<"class">>};
                false ->
                    #{<<"found">> => false,
                      <<"symbol">> => Symbol}
            end
    end.

%% @private
%% @doc Safely convert a binary to an existing atom, returning error instead of creating new atoms.
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(<<>>) -> {error, badarg};
safe_to_existing_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, badarg}
    end;
safe_to_existing_atom(_) -> {error, badarg}.

%% @private
%% @doc Ensure an error reason is a structured #beamtalk_error{} record.
%% If already structured (or a wrapped exception), passes through unchanged.
%% Handles known bare tuple error patterns from the compile pipeline.
%% Otherwise wraps the raw term in an internal_error.
-spec ensure_structured_error(term()) -> #beamtalk_error{}.
ensure_structured_error(#beamtalk_error{} = Err) -> Err;
ensure_structured_error(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}) -> Err;
ensure_structured_error({eval_error, _Class, #{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}}) -> Err;
ensure_structured_error({eval_error, _Class, #beamtalk_error{} = Err}) -> Err;
ensure_structured_error({eval_error, _Class, Reason}) ->
    %% Delegate to /1 for known tuple patterns; fall back to generic wrapper.
    ensure_structured_error(Reason);
ensure_structured_error({compile_error, Msg}) when is_binary(Msg) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0, Msg);
ensure_structured_error({compile_error, Msg}) when is_list(Msg) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0, list_to_binary(Msg));
ensure_structured_error({compile_error, Reason}) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Compile error: ">>, format_name(Reason)]));
ensure_structured_error({undefined_variable, Name}) ->
    Err0 = beamtalk_error:new(undefined_variable, 'REPL'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Undefined variable: ">>, format_name(Name)]));
ensure_structured_error({file_not_found, Path}) ->
    Err0 = beamtalk_error:new(file_not_found, 'File'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"File not found: ">>, format_name(Path)]));
ensure_structured_error({read_error, Reason}) ->
    Err0 = beamtalk_error:new(io_error, 'File'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Failed to read file: ">>, format_name(Reason)]));
ensure_structured_error({load_error, Reason}) ->
    Err0 = beamtalk_error:new(io_error, 'File'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Failed to load bytecode: ">>, format_name(Reason)]));
ensure_structured_error({parse_error, Details}) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Parse error: ">>, format_name(Details)]));
ensure_structured_error({invalid_request, Reason}) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Invalid request: ">>, format_name(Reason)]));
ensure_structured_error(empty_expression) ->
    Err0 = beamtalk_error:new(empty_expression, 'REPL'),
    beamtalk_error:with_message(Err0, <<"Empty expression">>);
ensure_structured_error(timeout) ->
    Err0 = beamtalk_error:new(timeout, 'REPL'),
    beamtalk_error:with_message(Err0, <<"Request timed out">>);
ensure_structured_error(Reason) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary(io_lib:format("~p", [Reason]))).

%% @private
%% @doc Ensure an error reason is structured, with exception class context.
%% Delegates known tuple patterns to ensure_structured_error/1 to preserve
%% specific error kinds, only falling back to generic wrapper for unknown terms.
-spec ensure_structured_error(term(), atom()) -> #beamtalk_error{}.
ensure_structured_error(#beamtalk_error{} = Err, _Class) -> Err;
ensure_structured_error(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}, _Class) -> Err;
ensure_structured_error({compile_error, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({eval_error, _, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({undefined_variable, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({file_not_found, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({read_error, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({load_error, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({parse_error, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error({invalid_request, _} = Reason, _Class) -> ensure_structured_error(Reason);
ensure_structured_error(Reason, Class) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(Err0,
        iolist_to_binary([atom_to_binary(Class, utf8), <<": ">>,
            io_lib:format("~p", [Reason])])).

%% @private
%% @doc Build a structured class_not_found error.
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin = to_binary(ClassName),
    Err0 = beamtalk_error:new(class_not_found, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Unknown class: ">>, NameBin])),
    beamtalk_error:with_hint(Err1,
        <<"Use :modules to see loaded classes.">>).

%% @private
%% @doc Convert an atom or binary to binary.
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.

%% @private
%% @doc Format a name for error messages.
-spec format_name(term()) -> binary().
format_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) -> Name;
format_name(Name) when is_list(Name) -> list_to_binary(Name);
format_name(Name) -> iolist_to_binary(io_lib:format("~p", [Name])).
