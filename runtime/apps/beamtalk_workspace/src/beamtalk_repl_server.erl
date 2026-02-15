%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TCP server and client handling for Beamtalk REPL
%%%
%%% This module handles TCP connections, client communication, and
%%% dispatching REPL protocol messages via beamtalk_repl_protocol.

-module(beamtalk_repl_server).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_workspace.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1, get_port/0, handle_client/2, parse_request/1, safe_to_existing_atom/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(RECV_TIMEOUT, 30000).

%% Maximum line length for recv_line/2. Prevents unbounded memory growth
%% if a client sends a very long line. 1 MB is generous for any REPL request.
-define(MAX_LINE_LENGTH, 1048576).

-record(state, {
    listen_socket :: gen_tcp:socket(),
    port :: inet:port_number(),
    acceptor_pid :: pid() | undefined
}).

%%% Public API

%% @doc Start the REPL TCP server.
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

%%% gen_server callbacks

%% @private
init(Config) ->
    Port = maps:get(port, Config),
    WorkspaceId = maps:get(workspace_id, Config, undefined),
    %% SECURITY: Bind to loopback only (127.0.0.1) so that only local
    %% processes can connect. No authentication is performed — the loopback
    %% restriction is the sole access control, matching the security model of
    %% erl, iex, and other language REPLs. If remote binding is ever added,
    %% authentication must be required. See docs/beamtalk-security.md (BT-184).
    %% buffer must be large enough for the longest request line to avoid
    %% {packet, line} splitting multi-byte UTF-8 sequences (BT-388).
    ListenOpts = [binary, {packet, line}, {active, false}, {reuseaddr, true},
                  {ip, {127, 0, 0, 1}}, {buffer, 65536}],
    case gen_tcp:listen(Port, ListenOpts) of
        {ok, ListenSocket} ->
            %% Discover actual bound port (important when Port=0 for OS-assigned ephemeral port)
            {ok, ActualPort} = inet:port(ListenSocket),
            %% Write port file so CLI can discover the actual port
            write_port_file(WorkspaceId, ActualPort),
            %% Start acceptor process
            {ok, AcceptorPid} = start_acceptor(ListenSocket),
            {ok, #state{listen_socket = ListenSocket, port = ActualPort, acceptor_pid = AcceptorPid}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

%% @private
handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, AcceptorPid, Reason}, State = #state{acceptor_pid = AcceptorPid, listen_socket = ListenSocket}) ->
    %% Acceptor died, restart it
    ?LOG_WARNING("REPL acceptor died: ~p, restarting", [Reason]),
    {ok, NewAcceptorPid} = start_acceptor(ListenSocket),
    {noreply, State#state{acceptor_pid = NewAcceptorPid}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, #state{listen_socket = ListenSocket, port = Port}) ->
    ?LOG_INFO("REPL server shutting down", #{reason => Reason, port => Port}),
    gen_tcp:close(ListenSocket),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Port File

%% @private
%% @doc Write the actual bound port to a file in the workspace directory.
%% This is needed when port=0 is used (OS assigns ephemeral port) so the
%% CLI can discover the actual port after BEAM startup.
-spec write_port_file(binary() | undefined, inet:port_number()) -> ok.
write_port_file(undefined, _Port) ->
    ok;
write_port_file(WorkspaceId, Port) ->
    case os:getenv("HOME") of
        false ->
            ?LOG_WARNING("HOME not set; skipping port file write for workspace ~p",
                           [WorkspaceId]),
            ok;
        Home ->
            PortFilePath = filename:join([Home, ".beamtalk", "workspaces",
                                          binary_to_list(WorkspaceId), "port"]),
            case filelib:ensure_dir(PortFilePath) of
                ok ->
                    case file:write_file(PortFilePath, integer_to_list(Port)) of
                        ok ->
                            ?LOG_DEBUG("Wrote port file: ~s (port ~p)", [PortFilePath, Port]),
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

%%% Acceptor Process

%% @private
start_acceptor(ListenSocket) ->
    Pid = spawn_link(fun() -> acceptor_loop(ListenSocket) end),
    {ok, Pid}.

%% @private
acceptor_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            %% Generate unique session ID
            SessionId = generate_session_id(),
            Peer = case inet:peername(ClientSocket) of
                       {ok, {PeerAddr, PeerPort}} -> {PeerAddr, PeerPort};
                       _ -> unknown
                   end,
            
            %% Start session under supervisor
            case beamtalk_session_sup:start_session(SessionId) of
                {ok, SessionPid} ->
                    ?LOG_INFO("REPL client connected", #{
                        session => SessionId,
                        session_pid => SessionPid,
                        peer => Peer
                    }),
                    
                    %% Mark activity - new session connected
                    beamtalk_workspace_meta:update_activity(),
                    
                    %% Spawn client handler monitoring the session.
                    %% If the session crashes, handle_client eventually
                    %% gets a recv error (closed socket) or timeout, and
                    %% the handler exits cleanly.
                    spawn(fun() ->
                        MonRef = erlang:monitor(process, SessionPid),
                        handle_client(ClientSocket, SessionPid),
                        %% Client disconnected — terminate the session
                        ?LOG_INFO("REPL client disconnected", #{
                            session => SessionId,
                            session_pid => SessionPid,
                            peer => Peer
                        }),
                        erlang:demonitor(MonRef, [flush]),
                        beamtalk_repl_shell:stop(SessionPid)
                    end);
                {error, Reason} ->
                    ?LOG_ERROR("Failed to create session: ~p", [Reason]),
                    ErrorJson = beamtalk_repl_json:format_error({session_creation_failed, Reason}),
                    gen_tcp:send(ClientSocket, [ErrorJson, "\n"]),
                    gen_tcp:close(ClientSocket)
            end,
            acceptor_loop(ListenSocket);
        {error, Reason} ->
            ?LOG_ERROR("Accept error: ~p", [Reason]),
            timer:sleep(1000),
            acceptor_loop(ListenSocket)
    end.

%% @doc Generate a unique session ID.
%% Format: "session_{timestamp}_{random}"
-spec generate_session_id() -> binary().
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(99999),
    list_to_binary(io_lib:format("session_~p_~p", [Timestamp, Random])).

%%% Client Handling

%% @doc Handle a client connection in session mode (runs in separate process).
%%
%% The SessionPid is a beamtalk_repl_shell process that handles
%% protocol-aware request routing via beamtalk_repl_protocol.
%% Called by: beamtalk_repl_server acceptor_loop
-spec handle_client(gen_tcp:socket(), pid()) -> ok.
handle_client(Socket, SessionPid) ->
    inet:setopts(Socket, [{active, false}, {packet, line}, {buffer, 65536}]),
    handle_client_loop(Socket, SessionPid).

%% @private
%% Session mode: forward requests to beamtalk_repl_shell via protocol-aware dispatch.
%% The try/catch is scoped to a single iteration so that the recursive
%% tail-call to handle_client_loop/2 happens outside the try block,
%% allowing proper tail-call optimisation and preventing stack growth.
handle_client_loop(Socket, SessionPid) ->
    Result = try handle_client_once(Socket, SessionPid)
             catch
                 Class:Reason:Stack ->
                     ?LOG_ERROR("REPL client handler crash", #{
                         class => Class,
                         reason => Reason,
                         stack => lists:sublist(Stack, 10)
                     }),
                     catch gen_tcp:close(Socket),
                     stop
             end,
    case Result of
        continue -> handle_client_loop(Socket, SessionPid);
        stop     -> ok
    end.

%% @private
%% Read a complete line from the socket, accumulating partial reads.
%% With {packet, line}, a complete line includes the trailing \n.
%% If the line exceeds the buffer, recv returns data without \n
%% and we must accumulate until we get the newline (BT-388).
%% Guards against unbounded accumulation and slowloris attacks.
-spec recv_line(gen_tcp:socket(), binary()) -> {ok, binary()} | {error, term()}.
recv_line(_Socket, Acc) when byte_size(Acc) >= ?MAX_LINE_LENGTH ->
    {error, line_too_long};
recv_line(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, Data} ->
            Combined = <<Acc/binary, Data/binary>>,
            case byte_size(Combined) >= ?MAX_LINE_LENGTH of
                true  -> {error, line_too_long};
                false ->
                    case binary:last(Data) of
                        $\n -> {ok, Combined};
                        _   -> recv_line(Socket, Combined)
                    end
            end;
        {error, timeout} when byte_size(Acc) > 0 ->
            %% Timeout while accumulating a partial line — keep waiting.
            %% The MAX_LINE_LENGTH guard above bounds total accumulation.
            recv_line(Socket, Acc);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Process a single recv/send iteration. Returns 'continue' or 'stop'.
handle_client_once(Socket, SessionPid) ->
    case recv_line(Socket, <<>>) of
        {ok, Data} ->
            %% Decode request using protocol module
            case beamtalk_repl_protocol:decode(Data) of
                {ok, Msg} ->
                    %% Process request via session or workspace APIs
                    Response = handle_protocol_request(Msg, SessionPid),
                    
                    %% Send response
                    case gen_tcp:send(Socket, [Response, "\n"]) of
                        ok ->
                            continue;
                        {error, SendError} ->
                            ?LOG_WARNING("REPL client send error: ~p", [SendError]),
                            gen_tcp:close(Socket),
                            stop
                    end;
                {error, DecodeError} ->
                    ErrorJson = beamtalk_repl_json:format_error(DecodeError),
                    case gen_tcp:send(Socket, [ErrorJson, "\n"]) of
                        ok ->
                            continue;
                        {error, SendError} ->
                            ?LOG_WARNING("REPL client send error: ~p", [SendError]),
                            gen_tcp:close(Socket),
                            stop
                    end
            end;
        {error, closed} ->
            ?LOG_DEBUG("REPL client TCP closed", #{reason => closed, session_pid => SessionPid}),
            stop;
        {error, timeout} ->
            %% No data within RECV_TIMEOUT — just retry.
            %% The idle monitor handles truly abandoned sessions.
            continue;
        {error, RecvError} ->
            ?LOG_WARNING("REPL client recv error: ~p", [RecvError]),
            gen_tcp:close(Socket),
            stop
    end.


%%% Protocol-aware request handling

%% @private
%% Handle a protocol message by dispatching to appropriate API.
%% Returns JSON binary response using protocol encoding.
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
            beamtalk_repl_protocol:encode_error(
                {eval_error, Class, Reason}, Msg, fun beamtalk_repl_json:format_error_message/1)
    end.

%% @private
handle_op(<<"eval">>, Params, Msg, SessionPid) ->
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    case Code of
        [] ->
            beamtalk_repl_protocol:encode_error(
                empty_expression, Msg, fun beamtalk_repl_json:format_error_message/1);
        _ ->
            case beamtalk_repl_shell:eval(SessionPid, Code) of
                {ok, Result, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_result(
                        Result, Msg, fun beamtalk_repl_json:term_to_json/1, Output, Warnings);
                {error, ErrorReason, Output, Warnings} ->
                    beamtalk_repl_protocol:encode_error(
                        ErrorReason, Msg, fun beamtalk_repl_json:format_error_message/1, Output, Warnings)
            end
    end;

handle_op(<<"clear">>, _Params, Msg, SessionPid) ->
    ok = beamtalk_repl_shell:clear_bindings(SessionPid),
    beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"bindings">>, _Params, Msg, SessionPid) ->
    {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
    %% ADR 0019 Phase 3: Filter out workspace convenience bindings from display.
    %% Users shouldn't see Transcript/Beamtalk/Workspace as "their" bindings.
    UserBindings = maps:without(?WORKSPACE_BINDINGS, Bindings),
    beamtalk_repl_protocol:encode_bindings(
        UserBindings, Msg, fun beamtalk_repl_json:term_to_json/1);

handle_op(<<"load-file">>, Params, Msg, SessionPid) ->
    Path = binary_to_list(maps:get(<<"path">>, Params, <<>>)),
    case beamtalk_repl_shell:load_file(SessionPid, Path) of
        {ok, Classes} ->
            beamtalk_repl_protocol:encode_loaded(Classes, Msg, fun beamtalk_repl_json:term_to_json/1);
        {error, Reason} ->
            beamtalk_repl_protocol:encode_error(
                Reason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end;

handle_op(<<"reload">>, Params, Msg, SessionPid) ->
    Module = binary_to_list(maps:get(<<"module">>, Params, <<>>)),
    case maps:get(<<"path">>, Params, undefined) of
        undefined when Module =/= [] ->
            %% Use list_to_existing_atom to prevent atom table exhaustion
            case catch list_to_existing_atom(Module) of
                ModuleAtom when is_atom(ModuleAtom) ->
                    {ok, Tracker} = beamtalk_repl_shell:get_module_tracker(SessionPid),
                    case beamtalk_repl_modules:get_module_info(ModuleAtom, Tracker) of
                        {ok, Info} ->
                            case beamtalk_repl_modules:get_source_file(Info) of
                                undefined ->
                                    beamtalk_repl_protocol:encode_error(
                                        {no_source_file, Module}, Msg,
                                        fun beamtalk_repl_json:format_error_message/1);
                                SourcePath ->
                                    do_reload(SourcePath, ModuleAtom, Msg, SessionPid)
                            end;
                        {error, not_found} ->
                            beamtalk_repl_protocol:encode_error(
                                {module_not_loaded, Module}, Msg,
                                fun beamtalk_repl_json:format_error_message/1)
                    end;
                _ ->
                    %% Atom doesn't exist — module was never loaded
                    beamtalk_repl_protocol:encode_error(
                        {module_not_loaded, Module}, Msg,
                        fun beamtalk_repl_json:format_error_message/1)
            end;
        undefined ->
            beamtalk_repl_protocol:encode_error(
                {missing_module_name, reload}, Msg, fun beamtalk_repl_json:format_error_message/1);
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
    try
        Pid = list_to_pid(PidStr),
        case is_known_actor(Pid) of
            false ->
                beamtalk_repl_protocol:encode_error(
                    {unknown_actor, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1);
            true ->
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
                                beamtalk_repl_protocol:encode_error(
                                    {inspect_failed, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1)
                        end;
                    false ->
                        beamtalk_repl_protocol:encode_error(
                            {actor_not_alive, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1)
                end
        end
    catch
        _:_ ->
            beamtalk_repl_protocol:encode_error(
                {invalid_pid, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1)
    end;

handle_op(<<"kill">>, Params, Msg, _SessionPid) ->
    PidStr = binary_to_list(maps:get(<<"actor">>, Params, maps:get(<<"pid">>, Params, <<>>))),
    try
        Pid = list_to_pid(PidStr),
        case is_known_actor(Pid) of
            false ->
                beamtalk_repl_protocol:encode_error(
                    {unknown_actor, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1);
            true ->
                exit(Pid, kill),
                beamtalk_repl_protocol:encode_status(ok, Msg, fun beamtalk_repl_json:term_to_json/1)
        end
    catch
        _:_ ->
            beamtalk_repl_protocol:encode_error(
                {invalid_pid, PidStr}, Msg, fun beamtalk_repl_json:format_error_message/1)
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
            beamtalk_repl_protocol:encode_error(
                {invalid_module, missing}, Msg, fun beamtalk_repl_json:format_error_message/1);
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
            beamtalk_repl_protocol:encode_error(
                {session_creation_failed, Reason}, Msg, fun beamtalk_repl_json:format_error_message/1)
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
                {class_not_found, ClassBin}, Msg, fun beamtalk_repl_json:format_error_message/1);
        {ok, ClassName} ->
            Selector = maps:get(<<"selector">>, Params, undefined),
            case Selector of
                undefined ->
                    case beamtalk_repl_docs:format_class_docs(ClassName) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                {class_not_found, ClassName}, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end;
                SelectorBin ->
                    case beamtalk_repl_docs:format_method_doc(ClassName, SelectorBin) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                {class_not_found, ClassName}, Msg, fun beamtalk_repl_json:format_error_message/1);
                        {error, {method_not_found, _, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                {method_not_found, ClassName, SelectorBin}, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end
            end
    end;

handle_op(Op, _Params, Msg, _SessionPid) ->
    beamtalk_repl_protocol:encode_error(
        {unknown_op, Op}, Msg, fun beamtalk_repl_json:format_error_message/1).

%%% Protocol Parsing and Formatting

%% @doc Parse a request from the CLI (legacy interface).
%% Expected format: JSON with "type" field.
%% New code should use beamtalk_repl_protocol:decode/1 instead.
-spec parse_request(binary()) -> 
    {eval, string()} | 
    {clear_bindings} | 
    {get_bindings} | 
    {load_file, string()} | 
    {list_actors} |
    {kill_actor, string()} |
    {list_modules} |
    {unload_module, string()} |
    {get_docs, binary(), binary() | undefined} |
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
%% Translate new protocol op to internal request tuple.
-spec op_to_request(binary(), map()) ->
    {eval, string()} | {clear_bindings} | {get_bindings} |
    {load_file, string()} | {list_actors} | {list_modules} |
    {kill_actor, string()} | {unload_module, string()} |
    {get_docs, binary(), binary() | undefined} | {error, term()}.
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
op_to_request(Op, _Map) ->
    {error, {unknown_op, Op}}.


%%% Reload helpers

%% @private
%% Execute reload: load file, trigger code_change for affected actors,
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
            beamtalk_repl_protocol:encode_error(
                Reason, Msg, fun beamtalk_repl_json:format_error_message/1)
    end.

%% @private
%% Trigger code_change for actors using the reloaded module.
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
%% Resolve module atoms from explicit module name or loaded class names.
-spec resolve_module_atoms(atom() | undefined, [map()]) -> [atom()].
resolve_module_atoms(ModuleAtom, _Classes) when is_atom(ModuleAtom), ModuleAtom =/= undefined ->
    [ModuleAtom];
resolve_module_atoms(undefined, Classes) ->
    lists:filtermap(fun(ClassMap) ->
        case maps:get(name, ClassMap, "") of
            "" -> false;
            Name ->
                case catch list_to_existing_atom(Name) of
                    Atom when is_atom(Atom) -> {true, Atom};
                    _ -> false
                end
        end
    end, Classes).


%%% Completion and Info helpers

%% @private
%% Build base response map with id/session for protocol responses.
-spec base_protocol_response(term()) -> map().
base_protocol_response(Msg) ->
    Id = beamtalk_repl_protocol:get_id(Msg),
    Session = beamtalk_repl_protocol:get_session(Msg),
    M0 = #{},
    M1 = case Id of undefined -> M0; _ -> M0#{<<"id">> => Id} end,
    case Session of undefined -> M1; _ -> M1#{<<"session">> => Session} end.

%% @private
%% Check whether a PID belongs to a registered Beamtalk actor.
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
%% Resolve a class name (e.g. 'Counter') to its BEAM module name (e.g. 'counter').
-spec resolve_class_to_module(atom()) -> atom().
resolve_class_to_module(ClassName) ->
    ClassPids = try beamtalk_object_class:all_classes()
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
%% Get autocompletion suggestions for a prefix.
-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) -> [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    %% Collect completions from loaded class processes
    ClassPids = try beamtalk_object_class:all_classes()
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
    %% Add built-in keywords
    [Kw || Kw <- builtin_keywords(), binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
           PrefixStr =/= ""].

%% @private
-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [<<"self">>, <<"super">>, <<"true">>, <<"false">>, <<"nil">>,
     <<"ifTrue:">>, <<"ifFalse:">>, <<"ifTrue:ifFalse:">>,
     <<"whileTrue:">>, <<"timesRepeat:">>,
     <<"subclass:">>, <<"spawn">>, <<"new">>].

%% @private
%% Get info about a symbol.
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
                ClassPids2 = beamtalk_object_class:all_classes(),
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

%% @private Safe atom conversion — returns error instead of creating new atoms.
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(<<>>) -> {error, badarg};
safe_to_existing_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, badarg}
    end;
safe_to_existing_atom(_) -> {error, badarg}.
