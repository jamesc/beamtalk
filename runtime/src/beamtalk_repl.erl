%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk REPL Backend (TCP Server)
%%%
%%% The REPL backend runs as a gen_server in the BEAM node, accepting TCP
%%% connections from the REPL CLI. It receives Beamtalk expressions, compiles
%%% them via the compiler daemon, loads the resulting bytecode, and evaluates.
%%%
%%% ## Architecture
%%%
%%% ```
%%% REPL CLI (Rust) ←→ TCP ←→ REPL Server (this module)
%%%                               ↓
%%%                         Compiler Daemon (TCP/Unix socket)
%%%                               ↓
%%%                         Load & Evaluate
%%% ```
%%%
%%% ## Protocol (JSON over TCP)
%%%
%%% Request from CLI:
%%% ```json
%%% {"type": "eval", "expression": "counter := Counter spawn"}
%%% ```
%%%
%%% Response to CLI:
%%% ```json
%%% {"type": "result", "value": "#<Counter pid=0.123.0>"}
%%% {"type": "error", "message": "Undefined variable: foo"}
%%% ```
%%%
%%% ## Session State
%%%
%%% The REPL maintains variable bindings across evaluations within a session:
%%% ```erlang
%%% #{
%%%   bindings => #{"counter" => <0.123.0>, "result" => 42},
%%%   session_id => "abc123",
%%%   compiler_host => "localhost",
%%%   compiler_port => 8081
%%% }
%%% ```
%%%
%%% ## Example
%%%
%%% ```erlang
%%% %% Start REPL server on port 9000
%%% {ok, Pid} = beamtalk_repl:start_link(9000),
%%%
%%% %% Or with compiler daemon config
%%% {ok, Pid} = beamtalk_repl:start_link(9000, #{
%%%     compiler_host => "localhost",
%%%     compiler_port => 8081
%%% }).
%%% ```

-module(beamtalk_repl).
-behaviour(gen_server).

%% Public API
-export([start_link/1, start_link/2, stop/1]).
-export([eval/2, get_bindings/1, clear_bindings/1, get_port/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for testing
-export([parse_request/1, format_response/1, format_error/1]).

-define(DEFAULT_DAEMON_SOCKET_PATH, "/tmp/.beamtalk/daemon.sock").
-define(ACCEPT_TIMEOUT, 100).
-define(RECV_TIMEOUT, 30000).
-define(DAEMON_CONNECT_TIMEOUT, 5000).

-record(state, {
    listen_socket :: gen_tcp:socket() | undefined,
    port :: inet:port_number(),
    bindings :: map(),
    daemon_socket_path :: string(),
    eval_counter :: non_neg_integer()
}).

%%% Public API

%% @doc Start the REPL server on the given port.
%% Uses default compiler daemon socket path.
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    start_link(Port, #{}).

%% @doc Start the REPL server with options.
%% Options:
%%   - daemon_socket_path: Unix socket path for compiler daemon
%%                         (default: ~/.beamtalk/daemon.sock)
-spec start_link(inet:port_number(), map()) -> {ok, pid()} | {error, term()}.
start_link(Port, Options) ->
    gen_server:start_link(?MODULE, {Port, Options}, []).

%% @doc Stop the REPL server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Evaluate an expression directly (for testing).
%% Returns {ok, FormattedResult} or {error, Reason}.
-spec eval(pid(), string()) -> {ok, term()} | {error, term()}.
eval(Pid, Expression) ->
    gen_server:call(Pid, {eval, Expression}, ?RECV_TIMEOUT).

%% @doc Get current variable bindings.
-spec get_bindings(pid()) -> map().
get_bindings(Pid) ->
    gen_server:call(Pid, get_bindings).

%% @doc Clear all variable bindings.
-spec clear_bindings(pid()) -> ok.
clear_bindings(Pid) ->
    gen_server:call(Pid, clear_bindings).

%% @doc Get the actual port the server is listening on.
%% Useful when started with port 0 (OS assigns port).
-spec get_port(pid()) -> inet:port_number().
get_port(Pid) ->
    gen_server:call(Pid, get_port).

%%% gen_server callbacks

%% @private
init({Port, Options}) ->
    %% Determine daemon socket path (default: ~/.beamtalk/daemon.sock)
    DaemonSocketPath = case maps:get(daemon_socket_path, Options, undefined) of
        undefined ->
            case os:getenv("HOME") of
                false -> ?DEFAULT_DAEMON_SOCKET_PATH;
                Home -> filename:join([Home, ".beamtalk", "daemon.sock"])
            end;
        Path -> Path
    end,
    
    %% Open TCP listen socket
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, line}]) of
        {ok, ListenSocket} ->
            %% Get actual port (important when Port=0)
            {ok, ActualPort} = inet:port(ListenSocket),
            %% Start accepting connections asynchronously
            self() ! accept,
            {ok, #state{
                listen_socket = ListenSocket,
                port = ActualPort,
                bindings = #{},
                daemon_socket_path = DaemonSocketPath,
                eval_counter = 0
            }};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

%% @private
handle_call({eval, Expression}, _From, State) ->
    case do_eval(Expression, State) of
        {ok, Result, NewState} ->
            {reply, {ok, Result}, NewState};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call(get_bindings, _From, State) ->
    {reply, State#state.bindings, State};

handle_call(clear_bindings, _From, State) ->
    {reply, ok, State#state{bindings = #{}}};

handle_call(get_port, _From, State) ->
    {reply, State#state.port, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(accept, State = #state{listen_socket = ListenSocket}) ->
    %% Non-blocking accept with short timeout
    case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
        {ok, ClientSocket} ->
            %% Handle client in a separate process
            %% Use spawn (not spawn_link) so client crashes don't take down the server
            Self = self(),
            spawn(fun() -> handle_client(ClientSocket, Self) end),
            %% Continue accepting
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            %% No connection waiting, continue accepting
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            %% Listen socket closed, stop
            {stop, normal, State};
        {error, Reason} ->
            %% Other error, log and continue
            io:format(standard_error, "REPL accept error: ~p~n", [Reason]),
            self() ! accept,
            {noreply, State}
    end;

handle_info({client_request, Request, ClientPid}, State) ->
    %% Handle request from client handler process
    case parse_request(Request) of
        {eval, Expression} ->
            case do_eval(Expression, State) of
                {ok, Result, NewState} ->
                    ClientPid ! {response, format_response(Result)},
                    {noreply, NewState};
                {error, Reason, NewState} ->
                    ClientPid ! {response, format_error(Reason)},
                    {noreply, NewState}
            end;
        {clear_bindings} ->
            ClientPid ! {response, format_response(ok)},
            {noreply, State#state{bindings = #{}}};
        {get_bindings} ->
            ClientPid ! {response, format_bindings(State#state.bindings)},
            {noreply, State};
        {error, ParseError} ->
            ClientPid ! {response, format_error(ParseError)},
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    case ListenSocket of
        undefined -> ok;
        _ -> gen_tcp:close(ListenSocket)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

%% @private
%% Handle a client connection (runs in separate process)
handle_client(Socket, ReplPid) ->
    inet:setopts(Socket, [{active, false}, {packet, line}]),
    handle_client_loop(Socket, ReplPid).

handle_client_loop(Socket, ReplPid) ->
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, Data} ->
            %% Send request to REPL server
            ReplPid ! {client_request, Data, self()},
            %% Wait for response
            receive
                {response, Response} ->
                    gen_tcp:send(Socket, [Response, "\n"]),
                    handle_client_loop(Socket, ReplPid)
            after ?RECV_TIMEOUT ->
                gen_tcp:send(Socket, [format_error(timeout), "\n"]),
                gen_tcp:close(Socket)
            end;
        {error, closed} ->
            ok;
        {error, timeout} ->
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format(standard_error, "REPL client error: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

%% @private
%% Parse a request from the CLI.
%% Expected format: JSON with "type" field.
-spec parse_request(binary()) -> {eval, string()} | {clear_bindings} | {get_bindings} | {error, term()}.
parse_request(Data) when is_binary(Data) ->
    try
        %% Remove trailing newline if present
        Trimmed = string:trim(Data),
        %% Try to parse as JSON
        case parse_json(Trimmed) of
            {ok, #{<<"type">> := <<"eval">>, <<"expression">> := Expr}} ->
                {eval, binary_to_list(Expr)};
            {ok, #{<<"type">> := <<"clear">>}} ->
                {clear_bindings};
            {ok, #{<<"type">> := <<"bindings">>}} ->
                {get_bindings};
            {ok, _Other} ->
                {error, {invalid_request, unknown_type}};
            {error, _Reason} ->
                %% Not JSON, treat as raw expression for backwards compatibility
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
%% Simple JSON parser for requests.
%% Only supports the subset of JSON needed for REPL protocol.
-spec parse_json(binary()) -> {ok, map()} | {error, term()}.
parse_json(Data) ->
    %% Very basic JSON parsing - just extract type and expression
    %% A proper implementation would use a JSON library
    case Data of
        <<"{", _/binary>> ->
            parse_json_object(Data);
        _ ->
            {error, not_json}
    end.

parse_json_object(Data) ->
    %% Extract key-value pairs from JSON object
    %% This is a simplified parser that handles our specific protocol
    try
        %% Remove outer braces and whitespace
        Inner = string:trim(binary:part(Data, 1, byte_size(Data) - 2)),
        %% Split into key-value pairs
        Pairs = parse_json_pairs(Inner, #{}),
        {ok, Pairs}
    catch
        _:_ ->
            {error, invalid_json}
    end.

parse_json_pairs(<<>>, Acc) ->
    Acc;
parse_json_pairs(Data, Acc) ->
    %% Find key
    case binary:match(Data, <<":">>) of
        nomatch ->
            Acc;
        {ColonPos, _} ->
            KeyPart = binary:part(Data, 0, ColonPos),
            Key = extract_string(string:trim(KeyPart)),
            %% Find value
            Rest = string:trim(binary:part(Data, ColonPos + 1, byte_size(Data) - ColonPos - 1)),
            {Value, Remaining} = extract_value(Rest),
            NewAcc = maps:put(Key, Value, Acc),
            %% Continue with remaining pairs
            RemainingTrimmed = string:trim(Remaining),
            case RemainingTrimmed of
                <<",", More/binary>> ->
                    parse_json_pairs(string:trim(More), NewAcc);
                _ ->
                    NewAcc
            end
    end.

extract_string(Data) ->
    %% Remove quotes from string
    case Data of
        <<"\"", Rest/binary>> ->
            %% Find closing quote
            case binary:match(Rest, <<"\"">>)  of
                {Pos, _} -> binary:part(Rest, 0, Pos);
                nomatch -> Rest
            end;
        _ ->
            Data
    end.

extract_value(Data) ->
    case Data of
        <<"\"", _/binary>> ->
            %% String value - find closing quote
            Rest = binary:part(Data, 1, byte_size(Data) - 1),
            case binary:match(Rest, <<"\"">>)  of
                {Pos, _} ->
                    Value = binary:part(Rest, 0, Pos),
                    Remaining = binary:part(Rest, Pos + 1, byte_size(Rest) - Pos - 1),
                    {Value, Remaining};
                nomatch ->
                    {Rest, <<>>}
            end;
        _ ->
            %% Other value - read until comma or end
            case binary:match(Data, <<",">>) of
                {Pos, _} ->
                    Value = string:trim(binary:part(Data, 0, Pos)),
                    Remaining = binary:part(Data, Pos, byte_size(Data) - Pos),
                    {Value, Remaining};
                nomatch ->
                    {string:trim(Data), <<>>}
            end
    end.

%% @private
%% Format a successful response as JSON.
-spec format_response(term()) -> binary().
format_response(Value) ->
    FormattedValue = format_value(Value),
    iolist_to_binary([<<"{\"type\":\"result\",\"value\":">>, FormattedValue, <<"}">>]).

%% @private
%% Format an error response as JSON.
-spec format_error(term()) -> binary().
format_error(Reason) ->
    Message = format_error_message(Reason),
    iolist_to_binary([<<"{\"type\":\"error\",\"message\":\"">>, escape_json_string(Message), <<"\"}">>]).

%% @private
%% Format bindings response as JSON.
-spec format_bindings(map()) -> binary().
format_bindings(Bindings) ->
    %% Format each binding
    Pairs = maps:fold(
        fun(Name, Value, Acc) ->
            NameBin = if
                is_atom(Name) -> atom_to_binary(Name, utf8);
                is_list(Name) -> list_to_binary(Name);
                is_binary(Name) -> Name;
                true -> list_to_binary(io_lib:format("~p", [Name]))
            end,
            ValueBin = format_value(Value),
            [[<<"\"">>, NameBin, <<"\":">>, ValueBin] | Acc]
        end,
        [],
        Bindings
    ),
    BindingsJson = case Pairs of
        [] -> <<"{}">>;
        _ -> iolist_to_binary([<<"{">>, lists:join(<<",">>, Pairs), <<"}">>])
    end,
    iolist_to_binary([<<"{\"type\":\"bindings\",\"bindings\":">>, BindingsJson, <<"}">>]).

%% @private
%% Format a value as JSON.
-spec format_value(term()) -> binary().
format_value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_value(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
format_value(Value) when is_atom(Value) ->
    iolist_to_binary([<<"\"">>, atom_to_binary(Value, utf8), <<"\"">>]);
format_value(Value) when is_binary(Value) ->
    iolist_to_binary([<<"\"">>, escape_json_string(Value), <<"\"">>]);
format_value(Value) when is_list(Value) ->
    %% Could be a string or a list
    case io_lib:printable_list(Value) of
        true ->
            iolist_to_binary([<<"\"">>, escape_json_string(list_to_binary(Value)), <<"\"">>]);
        false ->
            %% Format as array
            Elements = [format_value(E) || E <- Value],
            iolist_to_binary([<<"[">>, lists:join(<<",">>, Elements), <<"]">>])
    end;
format_value(Value) when is_pid(Value) ->
    %% Format pid as string
    PidStr = pid_to_list(Value),
    iolist_to_binary([<<"\"#<pid ">>, list_to_binary(PidStr), <<">\"">>]);
format_value(Value) when is_map(Value) ->
    %% Format map as JSON object
    Pairs = maps:fold(
        fun(K, V, Acc) ->
            KeyBin = format_value(K),
            ValueBin = format_value(V),
            [[KeyBin, <<":">>, ValueBin] | Acc]
        end,
        [],
        Value
    ),
    iolist_to_binary([<<"{">>, lists:join(<<",">>, Pairs), <<"}">>]);
format_value(Value) when is_tuple(Value) ->
    %% Format tuple as JSON array with marker
    Elements = [format_value(E) || E <- tuple_to_list(Value)],
    iolist_to_binary([<<"{\"__tuple__\":[">>, lists:join(<<",">>, Elements), <<"]}">>]);
format_value(Value) ->
    %% Fallback: format using io_lib
    iolist_to_binary([<<"\"">>, escape_json_string(iolist_to_binary(io_lib:format("~p", [Value]))), <<"\"">>]).

%% @private
%% Escape a string for JSON.
-spec escape_json_string(binary() | string()) -> binary().
escape_json_string(Str) when is_list(Str) ->
    escape_json_string(list_to_binary(Str));
escape_json_string(Bin) when is_binary(Bin) ->
    escape_json_chars(Bin, <<>>).

escape_json_chars(<<>>, Acc) ->
    Acc;
escape_json_chars(<<$\\, Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, $\\, $\\>>);
escape_json_chars(<<$", Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, $\\, $">>);
escape_json_chars(<<$\n, Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, $\\, $n>>);
escape_json_chars(<<$\r, Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, $\\, $r>>);
escape_json_chars(<<$\t, Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, $\\, $t>>);
escape_json_chars(<<C, Rest/binary>>, Acc) when C < 32 ->
    %% Control character - escape as \uXXXX
    Hex = io_lib:format("\\u~4.16.0B", [C]),
    escape_json_chars(Rest, <<Acc/binary, (list_to_binary(Hex))/binary>>);
escape_json_chars(<<C, Rest/binary>>, Acc) ->
    escape_json_chars(Rest, <<Acc/binary, C>>).

%% @private
%% Format an error reason as a human-readable message.
-spec format_error_message(term()) -> binary().
format_error_message(empty_expression) ->
    <<"Empty expression">>;
format_error_message(timeout) ->
    <<"Request timed out">>;
format_error_message({compile_error, Msg}) when is_binary(Msg) ->
    Msg;
format_error_message({compile_error, Msg}) when is_list(Msg) ->
    list_to_binary(Msg);
format_error_message({undefined_variable, Name}) ->
    iolist_to_binary([<<"Undefined variable: ">>, format_name(Name)]);
format_error_message({invalid_request, Reason}) ->
    iolist_to_binary([<<"Invalid request: ">>, format_name(Reason)]);
format_error_message({parse_error, Details}) ->
    iolist_to_binary([<<"Parse error: ">>, format_name(Details)]);
format_error_message({eval_error, Class, Reason}) ->
    iolist_to_binary([<<"Evaluation error: ">>, atom_to_binary(Class, utf8), <<":">>, format_name(Reason)]);
format_error_message({load_error, Reason}) ->
    iolist_to_binary([<<"Failed to load bytecode: ">>, format_name(Reason)]);
format_error_message(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

format_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) ->
    Name;
format_name(Name) when is_list(Name) ->
    list_to_binary(Name);
format_name(Name) ->
    iolist_to_binary(io_lib:format("~p", [Name])).

%% @private
%% Evaluate a Beamtalk expression.
%% This is the core of the REPL - compile, load, and execute.
-spec do_eval(string(), #state{}) -> {ok, term(), #state{}} | {error, term(), #state{}}.
do_eval(Expression, State = #state{bindings = Bindings, eval_counter = Counter}) ->
    %% Generate unique module name for this evaluation
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = State#state{eval_counter = Counter + 1},
    
    %% For now, we have a simplified evaluation that handles basic expressions
    %% TODO: Connect to compiler daemon for full Beamtalk compilation
    case compile_expression(Expression, ModuleName, Bindings, NewState) of
        {ok, Binary, _ResultExpr} ->
            %% Load the compiled module
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    %% Execute the eval function
                    try
                        Result = apply(ModuleName, eval, [Bindings]),
                        %% Check if this was an assignment
                        case extract_assignment(Expression) of
                            {ok, VarName} ->
                                NewBindings = maps:put(VarName, Result, Bindings),
                                {ok, Result, NewState#state{bindings = NewBindings}};
                            none ->
                                {ok, Result, NewState}
                        end
                    catch
                        Class:Reason ->
                            {error, {eval_error, Class, Reason}, NewState}
                    after
                        %% Clean up the temporary module
                        code:purge(ModuleName),
                        code:delete(ModuleName)
                    end;
                {error, Reason} ->
                    {error, {load_error, Reason}, NewState}
            end;
        {error, Reason} ->
            {error, {compile_error, Reason}, NewState}
    end.

%% @private
%% Compile a Beamtalk expression to bytecode via compiler daemon.
-spec compile_expression(string(), atom(), map(), #state{}) ->
    {ok, binary(), term()} | {error, term()}.
compile_expression(Expression, ModuleName, _Bindings, State) ->
    case compile_via_daemon(Expression, ModuleName, State) of
        {ok, Binary} ->
            {ok, Binary, {daemon_compiled}};
        {error, daemon_unavailable} ->
            {error, <<"Compiler daemon not running. Start with: beamtalk daemon start --foreground">>};
        {error, {compile_error, Diagnostics}} ->
            {error, format_daemon_diagnostics(Diagnostics)};
        {error, {core_compile_error, Errors}} ->
            {error, iolist_to_binary(io_lib:format("Core Erlang compile error: ~p", [Errors]))};
        {error, {daemon_error, Msg}} ->
            {error, iolist_to_binary([<<"Daemon error: ">>, Msg])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Compilation error: ~p", [Reason]))}
    end.

%% @private
%% Compile expression via compiler daemon using JSON-RPC over Unix socket.
-spec compile_via_daemon(string(), atom(), #state{}) ->
    {ok, binary()} | {error, term()}.
compile_via_daemon(Expression, ModuleName, #state{daemon_socket_path = SocketPath}) ->
    %% Try to connect to daemon
    case connect_to_daemon(SocketPath) of
        {ok, Socket} ->
            try
                compile_via_daemon_socket(Expression, ModuleName, Socket)
            after
                gen_tcp:close(Socket)
            end;
        {error, _Reason} ->
            {error, daemon_unavailable}
    end.

%% @private
%% Connect to the compiler daemon Unix socket.
-spec connect_to_daemon(string()) -> {ok, gen_tcp:socket()} | {error, term()}.
connect_to_daemon(SocketPath) ->
    %% Use local address family for Unix socket
    case gen_tcp:connect({local, SocketPath}, 0, 
                         [binary, {active, false}, {packet, line}],
                         ?DAEMON_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% Send compile request to daemon and process response.
-spec compile_via_daemon_socket(string(), atom(), gen_tcp:socket()) ->
    {ok, binary()} | {error, term()}.
compile_via_daemon_socket(Expression, ModuleName, Socket) ->
    %% Build JSON-RPC request
    %% Note: Using simple string formatting since we don't have jsx dependency yet
    RequestId = erlang:unique_integer([positive]),
    ExpressionEscaped = escape_json_string(list_to_binary(Expression)),
    ModuleNameBin = atom_to_binary(ModuleName, utf8),
    Request = iolist_to_binary([
        <<"{\"jsonrpc\":\"2.0\",\"id\":">>,
        integer_to_binary(RequestId),
        <<",\"method\":\"compile_expression\",\"params\":{\"source\":\"">>,
        ExpressionEscaped,
        <<"\",\"module_name\":\"">>,
        ModuleNameBin,
        <<"\"}}\n">>
    ]),
    
    %% Send request
    ok = gen_tcp:send(Socket, Request),
    
    %% Receive response
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, ResponseLine} ->
            parse_daemon_response(ResponseLine, ModuleName);
        {error, Reason} ->
            {error, {recv_error, Reason}}
    end.

%% @private
%% Parse daemon JSON-RPC response.
-spec parse_daemon_response(binary(), atom()) -> {ok, binary()} | {error, term()}.
parse_daemon_response(ResponseLine, ModuleName) ->
    %% Simple JSON parsing for daemon response
    case extract_json_field(ResponseLine, <<"error">>) of
        {ok, _ErrorObj} ->
            %% JSON-RPC error
            case extract_json_string(ResponseLine, <<"message">>) of
                {ok, Msg} -> {error, {daemon_error, Msg}};
                error -> {error, {daemon_error, <<"Unknown error">>}}
            end;
        error ->
            %% Check for result
            case extract_json_field(ResponseLine, <<"result">>) of
                {ok, _Result} ->
                    %% Check success field
                    case extract_json_bool(ResponseLine, <<"success">>) of
                        {ok, true} ->
                            %% Get Core Erlang and compile to BEAM
                            case extract_json_string(ResponseLine, <<"core_erlang">>) of
                                {ok, CoreErlang} ->
                                    compile_core_erlang(CoreErlang, ModuleName);
                                error ->
                                    {error, {daemon_error, <<"No core_erlang in response">>}}
                            end;
                        {ok, false} ->
                            %% Compilation failed - extract diagnostics
                            {error, {compile_error, extract_diagnostics(ResponseLine)}};
                        error ->
                            {error, {daemon_error, <<"Invalid response format">>}}
                    end;
                error ->
                    {error, {daemon_error, <<"Invalid JSON-RPC response">>}}
            end
    end.

%% @private
%% Compile Core Erlang source to BEAM bytecode.
-spec compile_core_erlang(binary(), atom()) -> {ok, binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin, ModuleName) ->
    %% Write Core Erlang to temp file (filename must match module name)
    TempDir = os:getenv("TMPDIR", "/tmp"),
    TempFile = filename:join(TempDir, atom_to_list(ModuleName) ++ ".core"),
    try
        ok = file:write_file(TempFile, CoreErlangBin),
        %% Compile Core Erlang to BEAM
        case compile:file(TempFile, [from_core, binary, return_errors]) of
            {ok, _CompiledModule, Binary} ->
                {ok, Binary};
            {ok, _CompiledModule, Binary, _Warnings} ->
                {ok, Binary};
            {error, Errors, _Warnings} ->
                {error, {core_compile_error, format_compile_errors(Errors)}}
        end
    catch
        _:Error ->
            {error, {core_compile_error, Error}}
    after
        file:delete(TempFile)
    end.

%% @private
%% Extract a JSON field value (very simple extraction).
-spec extract_json_field(binary(), binary()) -> {ok, binary()} | error.
extract_json_field(Json, Field) ->
    Pattern = <<"\"", Field/binary, "\":">>,
    case binary:match(Json, Pattern) of
        {Pos, Len} ->
            %% Found field, extract value
            Start = Pos + Len,
            {ok, binary:part(Json, Start, byte_size(Json) - Start)};
        nomatch ->
            error
    end.

%% @private
%% Extract a JSON string value (with unescaping).
-spec extract_json_string(binary(), binary()) -> {ok, binary()} | error.
extract_json_string(Json, Field) ->
    Pattern = <<"\"", Field/binary, "\":\"">>,
    case binary:match(Json, Pattern) of
        {Pos, Len} ->
            Start = Pos + Len,
            Rest = binary:part(Json, Start, byte_size(Json) - Start),
            %% Find closing quote (handling escapes)
            case find_string_end(Rest, 0) of
                {ok, EndPos} ->
                    EscapedStr = binary:part(Rest, 0, EndPos),
                    {ok, unescape_json_string(EscapedStr)};
                error ->
                    error
            end;
        nomatch ->
            error
    end.

%% @private
%% Find end of JSON string (handling escapes).
-spec find_string_end(binary(), non_neg_integer()) -> {ok, non_neg_integer()} | error.
find_string_end(<<>>, _Pos) ->
    error;
find_string_end(<<"\\", _, Rest/binary>>, Pos) ->
    %% Escaped character, skip
    find_string_end(Rest, Pos + 2);
find_string_end(<<"\"", _/binary>>, Pos) ->
    {ok, Pos};
find_string_end(<<_, Rest/binary>>, Pos) ->
    find_string_end(Rest, Pos + 1).

%% @private
%% Unescape JSON string escape sequences.
-spec unescape_json_string(binary()) -> binary().
unescape_json_string(Str) ->
    unescape_json_string(Str, <<>>).

unescape_json_string(<<>>, Acc) ->
    Acc;
unescape_json_string(<<"\\n", Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, $\n>>);
unescape_json_string(<<"\\r", Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, $\r>>);
unescape_json_string(<<"\\t", Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, $\t>>);
unescape_json_string(<<"\\\"", Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, $\">>);
unescape_json_string(<<"\\\\", Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, $\\>>);
unescape_json_string(<<C, Rest/binary>>, Acc) ->
    unescape_json_string(Rest, <<Acc/binary, C>>).

%% @private
%% Extract a JSON boolean value.
-spec extract_json_bool(binary(), binary()) -> {ok, boolean()} | error.
extract_json_bool(Json, Field) ->
    case binary:match(Json, <<"\"", Field/binary, "\":true">>) of
        {_, _} -> {ok, true};
        nomatch ->
            case binary:match(Json, <<"\"", Field/binary, "\":false">>) of
                {_, _} -> {ok, false};
                nomatch -> error
            end
    end.

%% @private
%% Extract diagnostics from response (simplified).
-spec extract_diagnostics(binary()) -> list().
extract_diagnostics(Json) ->
    %% For now, just extract the message strings
    case extract_json_string(Json, <<"message">>) of
        {ok, Msg} -> [Msg];
        error -> []
    end.

%% @private
%% Format daemon diagnostics for display.
-spec format_daemon_diagnostics(list()) -> binary().
format_daemon_diagnostics([]) ->
    <<"Compilation failed">>;
format_daemon_diagnostics(Diagnostics) ->
    iolist_to_binary(lists:join(<<"\n">>, Diagnostics)).

%% @private
%% Format Erlang compile errors.
format_compile_errors(Errors) ->
    lists:flatten([
        io_lib:format("~s:~p: ~s~n", [File, Line, erl_lint:format_error(Desc)])
        || {File, FileErrors} <- Errors,
           {Line, _Module, Desc} <- FileErrors
    ]).

%% @private
%% Extract variable name from assignment expression.
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    case re:run(Expression, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=", [{capture, [1], list}]) of
        {match, [VarName]} ->
            {ok, list_to_atom(VarName)};
        nomatch ->
            none
    end.
