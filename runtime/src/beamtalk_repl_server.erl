%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TCP server and client handling for Beamtalk REPL
%%%
%%% This module handles TCP connections, client communication, and
%%% the JSON protocol for REPL requests/responses.

-module(beamtalk_repl_server).

-export([handle_client/2, parse_request/1, format_response/1, format_error/1,
         format_bindings/1, format_loaded/1]).

-define(RECV_TIMEOUT, 30000).

%%% Client Handling

%% @doc Handle a client connection (runs in separate process).
handle_client(Socket, ReplPid) ->
    inet:setopts(Socket, [{active, false}, {packet, line}]),
    handle_client_loop(Socket, ReplPid).

%% @private
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

%%% Protocol Parsing and Formatting

%% @doc Parse a request from the CLI.
%% Expected format: JSON with "type" field.
-spec parse_request(binary()) -> {eval, string()} | {clear_bindings} | {get_bindings} | {load_file, string()} | {error, term()}.
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
            {ok, #{<<"type">> := <<"load">>, <<"path">> := Path}} ->
                {load_file, binary_to_list(Path)};
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

%% @doc Format a successful response as JSON.
-spec format_response(term()) -> binary().
format_response(Value) ->
    jsx:encode(#{<<"type">> => <<"result">>, <<"value">> => term_to_json(Value)}).

%% @doc Format an error response as JSON.
-spec format_error(term()) -> binary().
format_error(Reason) ->
    Message = format_error_message(Reason),
    jsx:encode(#{<<"type">> => <<"error">>, <<"message">> => Message}).

%% @doc Format bindings response as JSON.
-spec format_bindings(map()) -> binary().
format_bindings(Bindings) ->
    %% Convert bindings map to JSON-safe format
    JsonBindings = maps:fold(
        fun(Name, Value, Acc) ->
            NameBin = if
                is_atom(Name) -> atom_to_binary(Name, utf8);
                is_list(Name) -> list_to_binary(Name);
                is_binary(Name) -> Name;
                true -> list_to_binary(io_lib:format("~p", [Name]))
            end,
            maps:put(NameBin, term_to_json(Value), Acc)
        end,
        #{},
        Bindings
    ),
    jsx:encode(#{<<"type">> => <<"bindings">>, <<"bindings">> => JsonBindings}).

%% @doc Format a loaded file response as JSON.
-spec format_loaded([string()]) -> binary().
format_loaded(Classes) ->
    jsx:encode(#{<<"type">> => <<"loaded">>, <<"classes">> => [list_to_binary(C) || C <- Classes]}).

%%% JSON Parsing

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

%% @private
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

%% @private
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

%% @private
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

%% @private
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

%%% JSON Formatting

%% @private
%% Convert an Erlang term to a JSON-encodable value for jsx.
%% Returns a term that jsx:encode/1 can handle.
-spec term_to_json(term()) -> term().
term_to_json(Value) when is_integer(Value); is_float(Value); is_boolean(Value) ->
    Value;
term_to_json(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
term_to_json(Value) when is_binary(Value) ->
    Value;
term_to_json(Value) when is_list(Value) ->
    %% Could be a string or a list
    case io_lib:printable_list(Value) of
        true ->
            list_to_binary(Value);
        false ->
            [term_to_json(E) || E <- Value]
    end;
term_to_json(Value) when is_pid(Value) ->
    %% Format pid as Actor (class lookup not yet implemented)
    %% pid_to_list/1 returns "<0.123.0>", so strip the outer angle brackets
    PidStr = pid_to_list(Value),
    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
    iolist_to_binary([<<"#Actor<">>, Inner, <<">">>]);
term_to_json(Value) when is_function(Value) ->
    %% Format function with arity as "a Block/N"
    {arity, Arity} = erlang:fun_info(Value, arity),
    iolist_to_binary([<<"a Block/">>, integer_to_binary(Arity)]);
term_to_json(Value) when is_map(Value) ->
    %% Convert map keys and values
    maps:fold(
        fun(K, V, Acc) ->
            KeyBin = if
                is_atom(K) -> atom_to_binary(K, utf8);
                is_binary(K) -> K;
                is_list(K) ->
                    case io_lib:printable_list(K) of
                        true -> list_to_binary(K);
                        false -> list_to_binary(io_lib:format("~p", [K]))
                    end;
                true -> list_to_binary(io_lib:format("~p", [K]))
            end,
            maps:put(KeyBin, term_to_json(V), Acc)
        end,
        #{},
        Value
    );
term_to_json(Value) when is_tuple(Value) ->
    %% Format tuple with marker
    #{<<"__tuple__">> => [term_to_json(E) || E <- tuple_to_list(Value)]};
term_to_json(Value) ->
    %% Fallback: format using io_lib
    iolist_to_binary(io_lib:format("~p", [Value])).

%%% Error Formatting

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
format_error_message({file_not_found, Path}) ->
    iolist_to_binary([<<"File not found: ">>, format_name(Path)]);
format_error_message({read_error, Reason}) ->
    iolist_to_binary([<<"Failed to read file: ">>, format_name(Reason)]);
format_error_message(daemon_unavailable) ->
    <<"Unable to connect to compiler daemon. Start with: beamtalk daemon start --foreground">>;
format_error_message(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private
format_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) ->
    Name;
format_name(Name) when is_list(Name) ->
    list_to_binary(Name);
format_name(Name) ->
    iolist_to_binary(io_lib:format("~p", [Name])).
