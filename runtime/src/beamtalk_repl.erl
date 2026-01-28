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
-export([eval/2, get_bindings/1, clear_bindings/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for testing
-export([parse_request/1, format_response/1, format_error/1]).

-define(DEFAULT_COMPILER_HOST, "localhost").
-define(DEFAULT_COMPILER_PORT, 8081).
-define(ACCEPT_TIMEOUT, 100).
-define(RECV_TIMEOUT, 30000).

-record(state, {
    listen_socket :: gen_tcp:socket() | undefined,
    port :: inet:port_number(),
    bindings :: map(),
    compiler_host :: string(),
    compiler_port :: inet:port_number(),
    eval_counter :: non_neg_integer()
}).

%%% Public API

%% @doc Start the REPL server on the given port.
%% Uses default compiler daemon settings (localhost:8081).
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    start_link(Port, #{}).

%% @doc Start the REPL server with options.
%% Options:
%%   - compiler_host: hostname of compiler daemon (default: "localhost")
%%   - compiler_port: port of compiler daemon (default: 8081)
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

%%% gen_server callbacks

%% @private
init({Port, Options}) ->
    CompilerHost = maps:get(compiler_host, Options, ?DEFAULT_COMPILER_HOST),
    CompilerPort = maps:get(compiler_port, Options, ?DEFAULT_COMPILER_PORT),
    
    %% Open TCP listen socket
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, line}]) of
        {ok, ListenSocket} ->
            %% Start accepting connections asynchronously
            self() ! accept,
            {ok, #state{
                listen_socket = ListenSocket,
                port = Port,
                bindings = #{},
                compiler_host = CompilerHost,
                compiler_port = CompilerPort,
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
            {error, Reason} ->
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
        {ok, Binary, ResultExpr} ->
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
%% Compile a Beamtalk expression to bytecode.
%% This is a simplified version - full implementation would call compiler daemon.
-spec compile_expression(string(), atom(), map(), #state{}) ->
    {ok, binary(), term()} | {error, term()}.
compile_expression(Expression, ModuleName, Bindings, _State) ->
    %% For now, implement a basic eval that can handle simple expressions
    %% This will be replaced with proper compiler daemon integration
    
    %% Parse the expression to determine what operations are needed
    case parse_simple_expression(Expression, Bindings) of
        {ok, ErlangExpr} ->
            %% Generate a simple module that evaluates the expression
            Source = generate_eval_module(ModuleName, ErlangExpr, Bindings),
            %% Compile to beam
            case compile_source(Source) of
                {ok, Binary} ->
                    {ok, Binary, ErlangExpr};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% Parse a simple expression into an Erlang term/expression.
%% This handles basic cases while full compiler integration is developed.
-spec parse_simple_expression(string(), map()) -> {ok, term()} | {error, term()}.
parse_simple_expression(Expression, Bindings) ->
    Trimmed = string:trim(Expression),
    
    %% Check for various expression types
    case Trimmed of
        "" ->
            {error, empty_expression};
        _ ->
            %% Try to parse as different expression types
            parse_expression_type(Trimmed, Bindings)
    end.

parse_expression_type(Expr, Bindings) ->
    %% Assignment: varName := expression
    case re:run(Expr, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=\\s*(.+)$", [{capture, [1, 2], list}]) of
        {match, [_VarName, ValueExpr]} ->
            %% Parse the value expression
            parse_expression_type(ValueExpr, Bindings);
        nomatch ->
            parse_value_expression(Expr, Bindings)
    end.

parse_value_expression(Expr, Bindings) ->
    %% Integer literal
    case re:run(Expr, "^-?[0-9]+$", [{capture, none}]) of
        match ->
            {ok, {literal, list_to_integer(Expr)}};
        nomatch ->
            parse_float_or_string(Expr, Bindings)
    end.

parse_float_or_string(Expr, Bindings) ->
    %% Float literal
    case re:run(Expr, "^-?[0-9]+\\.[0-9]+$", [{capture, none}]) of
        match ->
            {ok, {literal, list_to_float(Expr)}};
        nomatch ->
            parse_string_or_symbol(Expr, Bindings)
    end.

parse_string_or_symbol(Expr, Bindings) ->
    %% String literal (single or double quoted)
    case re:run(Expr, "^['\"](.*)['\"]$", [{capture, [1], list}]) of
        {match, [StrValue]} ->
            {ok, {literal, list_to_binary(StrValue)}};
        nomatch ->
            parse_symbol_or_true(Expr, Bindings)
    end.

parse_symbol_or_true(Expr, Bindings) ->
    %% Symbol literal (#symbol)
    case re:run(Expr, "^#([a-zA-Z_][a-zA-Z0-9_]*)$", [{capture, [1], list}]) of
        {match, [SymName]} ->
            {ok, {literal, list_to_atom(SymName)}};
        nomatch ->
            parse_boolean_or_nil(Expr, Bindings)
    end.

parse_boolean_or_nil(Expr, Bindings) ->
    %% Boolean and nil literals
    case Expr of
        "true" -> {ok, {literal, true}};
        "false" -> {ok, {literal, false}};
        "nil" -> {ok, {literal, nil}};
        _ -> parse_variable_or_spawn(Expr, Bindings)
    end.

parse_variable_or_spawn(Expr, Bindings) ->
    %% Actor spawn: ClassName spawn
    case re:run(Expr, "^([A-Z][a-zA-Z0-9_]*)\\s+spawn$", [{capture, [1], list}]) of
        {match, [ClassName]} ->
            {ok, {spawn, list_to_atom(string:lowercase(ClassName))}};
        nomatch ->
            parse_spawn_with_args(Expr, Bindings)
    end.

parse_spawn_with_args(Expr, Bindings) ->
    %% Actor spawn with args: ClassName spawnWith: args  
    case re:run(Expr, "^([A-Z][a-zA-Z0-9_]*)\\s+spawnWith:\\s*(.+)$", [{capture, [1, 2], list}]) of
        {match, [ClassName, ArgsExpr]} ->
            case parse_expression_type(ArgsExpr, Bindings) of
                {ok, ArgsParsed} ->
                    {ok, {spawn_with, list_to_atom(string:lowercase(ClassName)), ArgsParsed}};
                Error ->
                    Error
            end;
        nomatch ->
            parse_message_send(Expr, Bindings)
    end.

parse_message_send(Expr, Bindings) ->
    %% Message send: receiver selector (unary)
    case re:run(Expr, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s+([a-zA-Z_][a-zA-Z0-9_]*)$", [{capture, [1, 2], list}]) of
        {match, [Receiver, Selector]} ->
            %% Check if receiver is a bound variable
            ReceiverAtom = list_to_atom(Receiver),
            case maps:is_key(ReceiverAtom, Bindings) of
                true ->
                    {ok, {send, {var, ReceiverAtom}, list_to_atom(Selector), []}};
                false ->
                    %% Could be a class name for spawn
                    {error, {undefined_variable, Receiver}}
            end;
        nomatch ->
            parse_keyword_message(Expr, Bindings)
    end.

parse_keyword_message(Expr, Bindings) ->
    %% Simple keyword message: receiver selector: arg
    case re:run(Expr, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s+([a-zA-Z_][a-zA-Z0-9_]*):\\s*(.+)$", 
                [{capture, [1, 2, 3], list}]) of
        {match, [Receiver, Selector, ArgExpr]} ->
            ReceiverAtom = list_to_atom(Receiver),
            case maps:is_key(ReceiverAtom, Bindings) of
                true ->
                    case parse_expression_type(ArgExpr, Bindings) of
                        {ok, ArgParsed} ->
                            SelectorAtom = list_to_atom(Selector ++ ":"),
                            {ok, {send, {var, ReceiverAtom}, SelectorAtom, [ArgParsed]}};
                        Error ->
                            Error
                    end;
                false ->
                    {error, {undefined_variable, Receiver}}
            end;
        nomatch ->
            parse_await(Expr, Bindings)
    end.

parse_await(Expr, Bindings) ->
    %% Await: expr await
    case re:run(Expr, "^(.+)\\s+await$", [{capture, [1], list}]) of
        {match, [InnerExpr]} ->
            case parse_expression_type(InnerExpr, Bindings) of
                {ok, Inner} ->
                    {ok, {await, Inner}};
                Error ->
                    Error
            end;
        nomatch ->
            parse_variable_ref(Expr, Bindings)
    end.

parse_variable_ref(Expr, Bindings) ->
    %% Variable reference
    case re:run(Expr, "^[a-zA-Z_][a-zA-Z0-9_]*$", [{capture, none}]) of
        match ->
            VarName = list_to_atom(Expr),
            case maps:is_key(VarName, Bindings) of
                true ->
                    {ok, {var, VarName}};
                false ->
                    {error, {undefined_variable, Expr}}
            end;
        nomatch ->
            {error, {syntax_error, io_lib:format("Cannot parse expression: ~s", [Expr])}}
    end.

%% @private
%% Generate an Erlang module that evaluates the parsed expression.
-spec generate_eval_module(atom(), term(), map()) -> string().
generate_eval_module(ModuleName, ParsedExpr, Bindings) ->
    %% Generate the expression evaluation code
    %% Bindings are accessed via helper function to avoid Erlang's single-assignment
    ExprCode = generate_expr_code(ParsedExpr),
    
    %% Generate binding helper function
    BindingHelperCode = generate_binding_helper(Bindings),
    
    lists:flatten(io_lib:format(
        "-module('~s').~n"
        "-export([eval/1]).~n"
        "~n"
        "eval(Bindings) ->~n"
        "    ~s.~n"
        "~n"
        "~s",
        [ModuleName, ExprCode, BindingHelperCode]
    )).

generate_binding_helper(Bindings) when map_size(Bindings) == 0 ->
    %% No bindings - generate a simple function that always returns error
    "get_binding(_Name, _Bindings) -> error({undefined_variable, _Name}).\n";
generate_binding_helper(_Bindings) ->
    %% Generate helper function to look up bindings
    "get_binding(Name, Bindings) ->\n"
    "    case maps:find(Name, Bindings) of\n"
    "        {ok, Value} -> Value;\n"
    "        error -> error({undefined_variable, Name})\n"
    "    end.\n".

generate_expr_code({literal, Value}) when is_integer(Value) ->
    io_lib:format("~p", [Value]);
generate_expr_code({literal, Value}) when is_float(Value) ->
    io_lib:format("~p", [Value]);
generate_expr_code({literal, Value}) when is_atom(Value) ->
    %% Use ~p for safe escaping of atom names with special characters
    io_lib:format("~p", [Value]);
generate_expr_code({literal, Value}) when is_binary(Value) ->
    %% Use ~p for safe escaping to prevent code injection
    io_lib:format("~p", [Value]);
generate_expr_code({var, Name}) ->
    %% Use helper function to look up binding (avoids Erlang single-assignment issues)
    io_lib:format("get_binding('~s', Bindings)", [Name]);
generate_expr_code({spawn, ClassName}) ->
    io_lib:format("beamtalk_actor:spawn_actor('~s', [])", [ClassName]);
generate_expr_code({spawn_with, ClassName, ArgsExpr}) ->
    ArgsCode = generate_expr_code(ArgsExpr),
    io_lib:format("beamtalk_actor:spawn_actor('~s', ~s)", [ClassName, ArgsCode]);
generate_expr_code({send, Receiver, Selector, Args}) ->
    ReceiverCode = generate_expr_code(Receiver),
    ArgsCode = case Args of
        [] -> "[]";
        _ -> "[" ++ string:join([generate_expr_code(A) || A <- Args], ", ") ++ "]"
    end,
    %% Create a future and send async message
    io_lib:format(
        "begin~n"
        "        __Future__ = beamtalk_future:new(),~n"
        "        gen_server:cast(~s, {'~s', ~s, __Future__}),~n"
        "        __Future__~n"
        "    end",
        [ReceiverCode, Selector, ArgsCode]
    );
generate_expr_code({await, Inner}) ->
    InnerCode = generate_expr_code(Inner),
    io_lib:format("beamtalk_future:await(~s)", [InnerCode]);
generate_expr_code(Other) ->
    io_lib:format("~p", [Other]).

%% @private
%% Compile Erlang source to bytecode.
-spec compile_source(string()) -> {ok, binary()} | {error, term()}.
compile_source(Source) ->
    %% Write to temporary file and compile
    %% Alternative: use compile:forms/2 with erl_scan/erl_parse
    TempDir = os:getenv("TMPDIR", "/tmp"),
    TempFile = filename:join(
        TempDir,
        "beamtalk_repl_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".erl"
    ),
    try
        ok = file:write_file(TempFile, Source),
        case compile:file(TempFile, [binary, return_errors]) of
            {ok, _ModuleName, Binary} ->
                {ok, Binary};
            {ok, _ModuleName, Binary, _Warnings} ->
                {ok, Binary};
            {error, Errors, _Warnings} ->
                {error, format_compile_errors(Errors)}
        end
    catch
        _:Error ->
            {error, Error}
    after
        file:delete(TempFile)
    end.

%% @private
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
