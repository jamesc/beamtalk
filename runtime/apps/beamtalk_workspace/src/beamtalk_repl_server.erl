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
%%%
%%% Op handlers are delegated to domain-specific modules (BT-705):
%%% - beamtalk_repl_ops_eval: eval, clear, bindings
%%% - beamtalk_repl_ops_load: load-file, load-source, reload, unload, modules
%%% - beamtalk_repl_ops_actors: actors, inspect, kill, interrupt
%%% - beamtalk_repl_ops_session: sessions, clone, close, health, shutdown
%%% - beamtalk_repl_ops_dev: complete, info, docs, describe

-module(beamtalk_repl_server).
-behaviour(gen_server).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1, get_port/0, get_nonce/0, handle_protocol_request/2,
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

-record(state, {
    port :: inet:port_number(),
    workspace_id :: binary() | undefined,
    nonce :: binary()
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

%%% gen_server callbacks

%% @private
init(Config) ->
    Port = maps:get(port, Config),
    WorkspaceId = maps:get(workspace_id, Config, undefined),
    BindAddr = maps:get(bind_addr, Config, {127, 0, 0, 1}),
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
            {ok, #state{port = ActualPort,
                        workspace_id = WorkspaceId,
                        nonce = Nonce}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

%% @private
handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(get_nonce, _From, State) ->
    {reply, {ok, State#state.nonce}, State};
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
terminate(Reason, #state{port = Port}) ->
    ?LOG_INFO("REPL server shutting down", #{reason => Reason, port => Port}),
    cowboy:stop_listener(?LISTENER_REF),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
%% @doc Dispatch protocol ops to domain-specific handler modules (BT-705).
handle_op(Op, Params, Msg, SessionPid) when
        Op =:= <<"eval">>; Op =:= <<"clear">>; Op =:= <<"bindings">> ->
    beamtalk_repl_ops_eval:handle(Op, Params, Msg, SessionPid);
handle_op(Op, Params, Msg, SessionPid) when
        Op =:= <<"load-file">>; Op =:= <<"load-source">>; Op =:= <<"reload">>;
        Op =:= <<"unload">>; Op =:= <<"modules">> ->
    beamtalk_repl_ops_load:handle(Op, Params, Msg, SessionPid);
handle_op(Op, Params, Msg, SessionPid) when
        Op =:= <<"actors">>; Op =:= <<"inspect">>; Op =:= <<"kill">>;
        Op =:= <<"interrupt">> ->
    beamtalk_repl_ops_actors:handle(Op, Params, Msg, SessionPid);
handle_op(Op, Params, Msg, SessionPid) when
        Op =:= <<"sessions">>; Op =:= <<"clone">>; Op =:= <<"close">>;
        Op =:= <<"health">>; Op =:= <<"shutdown">> ->
    beamtalk_repl_ops_session:handle(Op, Params, Msg, SessionPid);
handle_op(Op, Params, Msg, SessionPid) when
        Op =:= <<"complete">>; Op =:= <<"info">>; Op =:= <<"docs">>;
        Op =:= <<"describe">> ->
    beamtalk_repl_ops_dev:handle(Op, Params, Msg, SessionPid);
handle_op(Op, _Params, Msg, _SessionPid) ->
    Err0 = beamtalk_error:new(unknown_op, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Unknown operation: ">>, Op])),
    beamtalk_repl_protocol:encode_error(
        Err1, Msg, fun beamtalk_repl_json:format_error_message/1).

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


%%% Delegated helpers (BT-705)
%%%
%%% These functions have been extracted to domain-specific ops modules.
%%% Thin delegates are kept here so existing TEST exports and callers
%%% continue to work without modification.

-ifdef(TEST).
%% @private Delegate to beamtalk_repl_ops_dev.
base_protocol_response(Msg) -> beamtalk_repl_ops_dev:base_protocol_response(Msg).

%% @private Delegate to beamtalk_repl_ops_actors.
validate_actor_pid(PidStr) -> beamtalk_repl_ops_actors:validate_actor_pid(PidStr).
is_known_actor(Pid) -> beamtalk_repl_ops_actors:is_known_actor(Pid).

%% @private Delegate to beamtalk_repl_ops_load.
resolve_class_to_module(ClassName) -> beamtalk_repl_ops_load:resolve_class_to_module(ClassName).
resolve_module_atoms(ModuleAtom, Classes) -> beamtalk_repl_ops_load:resolve_module_atoms(ModuleAtom, Classes).

%% @private Delegate to beamtalk_repl_ops_dev.
get_completions(Prefix) -> beamtalk_repl_ops_dev:get_completions(Prefix).
get_symbol_info(Symbol) -> beamtalk_repl_ops_dev:get_symbol_info(Symbol).
make_class_not_found_error(ClassName) -> beamtalk_repl_ops_dev:make_class_not_found_error(ClassName).
-endif.

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
%% @doc Format a name for error messages.
-spec format_name(term()) -> binary().
format_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) -> Name;
format_name(Name) when is_list(Name) -> list_to_binary(Name);
format_name(Name) -> iolist_to_binary(io_lib:format("~p", [Name])).
