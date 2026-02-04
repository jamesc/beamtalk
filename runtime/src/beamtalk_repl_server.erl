%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TCP server and client handling for Beamtalk REPL
%%%
%%% This module handles TCP connections, client communication, and
%%% the JSON protocol for REPL requests/responses.

-module(beamtalk_repl_server).

-include("beamtalk.hrl").

-export([handle_client/2, parse_request/1, format_response/1, format_error/1,
         format_bindings/1, format_loaded/1, format_actors/1, format_modules/1]).

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
-spec parse_request(binary()) -> 
    {eval, string()} | 
    {clear_bindings} | 
    {get_bindings} | 
    {load_file, string()} | 
    {list_actors} |
    {kill_actor, string()} |
    {list_modules} |
    {unload_module, string()} |
    {error, term()}.
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

%% @doc Format a successful response as JSON.
-spec format_response(term()) -> binary().
format_response(Value) ->
    try
        JsonValue = term_to_json(Value),
        jsx:encode(#{<<"type">> => <<"result">>, <<"value">> => JsonValue})
    catch
        Class:Reason:_Stack ->
            %% Fallback with details about what went wrong
            ErrorMsg = io_lib:format("Internal error formatting ~p: ~p:~p", [Value, Class, Reason]),
            jsx:encode(#{<<"type">> => <<"error">>, 
                        <<"message">> => iolist_to_binary(ErrorMsg)})
    end.

%% @doc Format an error response as JSON.
-spec format_error(term()) -> binary().
format_error(Reason) ->
    try
        Message = format_error_message(Reason),
        jsx:encode(#{<<"type">> => <<"error">>, <<"message">> => Message})
    catch
        Class:FormatError:Stack ->
            %% Log formatting failure for debugging
            io:format(standard_error,
                      "Failed to format error:~nClass: ~p~nError: ~p~nStack: ~p~nReason: ~p~n",
                      [Class, FormatError, lists:sublist(Stack, 5), Reason]),
            %% Return fallback error response
            jsx:encode(#{<<"type">> => <<"error">>, 
                        <<"message">> => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))})
    end.

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
%% Classes is a list of #{name => string(), superclass => string()} maps.
-spec format_loaded([map()]) -> binary().
format_loaded(Classes) ->
    ClassNames = [list_to_binary(maps:get(name, C, "")) || C <- Classes],
    jsx:encode(#{<<"type">> => <<"loaded">>, <<"classes">> => ClassNames}).

%% @doc Format an actors list response as JSON.
-spec format_actors([beamtalk_repl_actors:actor_metadata()]) -> binary().
format_actors(Actors) ->
    JsonActors = lists:map(
        fun(#{pid := Pid, class := Class, module := Module, spawned_at := SpawnedAt}) ->
            #{
                <<"pid">> => list_to_binary(pid_to_list(Pid)),
                <<"class">> => atom_to_binary(Class, utf8),
                <<"module">> => atom_to_binary(Module, utf8),
                <<"spawned_at">> => SpawnedAt
            }
        end,
        Actors
    ),
    jsx:encode(#{<<"type">> => <<"actors">>, <<"actors">> => JsonActors}).

%% @doc Format a modules list response as JSON.
-spec format_modules([{atom(), map()}]) -> binary().
format_modules(ModulesWithInfo) ->
    JsonModules = lists:map(
        fun({_ModuleName, Info}) ->
            #{
                <<"name">> => maps:get(name, Info),
                <<"source_file">> => list_to_binary(maps:get(source_file, Info)),
                <<"actor_count">> => maps:get(actor_count, Info),
                <<"load_time">> => maps:get(load_time, Info),
                <<"time_ago">> => list_to_binary(lists:flatten(maps:get(time_ago, Info)))
            }
        end,
        ModulesWithInfo
    ),
    jsx:encode(#{<<"type">> => <<"modules">>, <<"modules">> => JsonModules}).

%%% JSON Parsing

%% @private
%% Parse JSON requests using jsx library.
-spec parse_json(binary()) -> {ok, map()} | {error, term()}.
parse_json(Data) ->
    %% Use jsx library for proper JSON parsing
    try
        Decoded = jsx:decode(Data, [return_maps]),
        {ok, Decoded}
    catch
        Class:Reason:Stack ->
            %% Log parse failures for debugging REPL protocol issues
            io:format(standard_error, 
                      "JSON parse failed:~nClass: ~p~nReason: ~p~nStack: ~p~nData: ~p~n",
                      [Class, Reason, lists:sublist(Stack, 3), Data]),
            {error, not_json}
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
    %% Check if this is a Future process
    %% Futures are running beamtalk_future:pending/resolved/rejected functions
    case is_process_alive(Value) of
        true ->
            %% Handle race condition: process could die between alive check and process_info
            case process_info(Value, current_function) of
                {current_function, {beamtalk_future, pending, _}} ->
                    iolist_to_binary(<<"#Future<pending>">>);
                {current_function, {beamtalk_future, resolved, _}} ->
                    %% Future has been resolved; we only know its state here
                    iolist_to_binary(<<"#Future<resolved>">>);
                {current_function, {beamtalk_future, rejected, _}} ->
                    %% Future has been rejected; we only know its state here
                    iolist_to_binary(<<"#Future<rejected>">>);
                undefined ->
                    %% Process died between alive check and process_info
                    PidStr = pid_to_list(Value),
                    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
                    iolist_to_binary([<<"#Dead<">>, Inner, <<">">>]);
                _ ->
                    %% Regular process/actor
                    PidStr = pid_to_list(Value),
                    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
                    iolist_to_binary([<<"#Actor<">>, Inner, <<">">>])
            end;
        false ->
            %% Dead process
            PidStr = pid_to_list(Value),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Dead<">>, Inner, <<">">>])
    end;
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
term_to_json(#beamtalk_error{} = Error) ->
    %% Format beamtalk_error records as user-friendly strings
    iolist_to_binary(beamtalk_error:format(Error));
term_to_json(Value) when is_tuple(Value) ->
    %% Special handling for known tuple types
    case Value of
        {beamtalk_object, Class, _Module, Pid} ->
            %% Format actor object as #Actor<Class, Pid>
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Actor<">>, atom_to_binary(Class, utf8), <<",">>, Inner, <<">">>]);
        {future_timeout, Pid} when is_pid(Pid) ->
            %% Future that timed out
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Future<timeout,">>, Inner, <<">">>]);
        {future_rejected, Reason} ->
            %% Future that was rejected - format the reason as error message
            iolist_to_binary([<<"#Future<rejected: ">>, format_rejection_reason(Reason), <<">">>]);
        _ ->
            %% Format generic tuple with marker
            #{<<"__tuple__">> => [term_to_json(E) || E <- tuple_to_list(Value)]}
    end;
term_to_json(Value) ->
    %% Fallback: format using io_lib
    iolist_to_binary(io_lib:format("~p", [Value])).

%% @private
%% Format a rejection reason for display in #Future<rejected: ...>
-spec format_rejection_reason(term()) -> iolist().
format_rejection_reason(#beamtalk_error{} = Error) ->
    beamtalk_error:format(Error);
format_rejection_reason(Reason) ->
    %% Fallback: format arbitrary rejection reasons as printable terms
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%% Error Formatting

%% @private
%% Format an error reason as a human-readable message.
-spec format_error_message(term()) -> binary().
format_error_message(#beamtalk_error{} = Error) ->
    %% Format structured beamtalk_error using the error helper
    iolist_to_binary(beamtalk_error:format(Error));
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
format_error_message({module_not_found, ModuleName}) ->
    iolist_to_binary([<<"Module not loaded: ">>, ModuleName]);
format_error_message({invalid_module_name, ModuleName}) ->
    iolist_to_binary([<<"Invalid module name: ">>, ModuleName]);
format_error_message({actors_exist, ModuleName, Count}) ->
    CountStr = integer_to_list(Count),
    ActorWord = if Count == 1 -> <<"actor">>; true -> <<"actors">> end,
    iolist_to_binary([
        <<"Cannot unload ">>, atom_to_binary(ModuleName, utf8), 
        <<": ">>, CountStr, <<" ">>, ActorWord, <<" still running. Kill them first with :kill">>
    ]);
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
