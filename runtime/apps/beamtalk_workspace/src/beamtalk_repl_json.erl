%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc JSON formatting for REPL protocol responses.
%%%
%%% **DDD Context:** Language Service
%%%
%%% Pure formatting logic extracted from beamtalk_repl_server.
%%% Converts Erlang terms to JSON-encodable values and builds
%%% JSON response/error binaries for the REPL protocol.

-module(beamtalk_repl_json).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    format_response/1,
    format_error/1,
    format_response_with_warnings/2,
    format_error_with_warnings/2,
    format_bindings/1,
    format_loaded/1,
    format_actors/1,
    format_modules/1,
    format_docs/1,
    term_to_json/1,
    format_error_message/1,
    parse_json/1,
    encode_reloaded/4, encode_reloaded/5
]).

%%% JSON Parsing

%% @doc Parse JSON binary into a map using jsx.
-spec parse_json(binary()) -> {ok, map()} | {error, term()}.
parse_json(Data) ->
    try
        Decoded = jsx:decode(Data, [return_maps]),
        {ok, Decoded}
    catch
        Class:Reason:Stack ->
            ?LOG_DEBUG("JSON parse failed", #{
                class => Class,
                reason => Reason,
                stack => lists:sublist(Stack, 3),
                data => Data
            }),
            {error, not_json}
    end.

%%% Response Formatting

%% @doc Format a successful response as JSON.
-spec format_response(term()) -> binary().
format_response(Value) ->
    try
        JsonValue = term_to_json(Value),
        jsx:encode(#{<<"type">> => <<"result">>, <<"value">> => JsonValue})
    catch
        Class:Reason:_Stack ->
            ErrorMsg = io_lib:format("Internal error formatting ~p: ~p:~p", [Value, Class, Reason]),
            jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => iolist_to_binary(ErrorMsg)
            })
    end.

%% @doc Format an error response as JSON.
-spec format_error(term()) -> binary().
format_error(Reason) ->
    try
        Message = format_error_message(Reason),
        jsx:encode(#{<<"type">> => <<"error">>, <<"message">> => Message})
    catch
        Class:FormatError:Stack ->
            ?LOG_DEBUG("Failed to format error", #{
                class => Class,
                reason => FormatError,
                stack => lists:sublist(Stack, 5),
                original_reason => Reason
            }),
            jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
            })
    end.

%% @doc Format a successful response with warnings as JSON.
-spec format_response_with_warnings(term(), [binary()]) -> binary().
format_response_with_warnings(Value, Warnings) ->
    try
        JsonValue = term_to_json(Value),
        Response = #{<<"type">> => <<"result">>, <<"value">> => JsonValue},
        case Warnings of
            [] -> jsx:encode(Response);
            _ -> jsx:encode(maps:put(<<"warnings">>, Warnings, Response))
        end
    catch
        Class:Reason:_Stack ->
            ErrorMsg = io_lib:format("Internal error formatting ~p: ~p:~p", [Value, Class, Reason]),
            jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => iolist_to_binary(ErrorMsg)
            })
    end.

%% @doc Format an error response with warnings as JSON.
-spec format_error_with_warnings(term(), [binary()]) -> binary().
format_error_with_warnings(Reason, Warnings) ->
    try
        Message = format_error_message(Reason),
        Response = #{<<"type">> => <<"error">>, <<"message">> => Message},
        case Warnings of
            [] -> jsx:encode(Response);
            _ -> jsx:encode(maps:put(<<"warnings">>, Warnings, Response))
        end
    catch
        Class:FormatError:Stack ->
            ?LOG_DEBUG("Failed to format error", #{
                class => Class,
                reason => FormatError,
                stack => lists:sublist(Stack, 5),
                original_reason => Reason
            }),
            jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
            })
    end.

%% @doc Format bindings response as JSON.
-spec format_bindings(map()) -> binary().
format_bindings(Bindings) ->
    JsonBindings = maps:fold(
        fun(Name, Value, Acc) ->
            NameBin =
                if
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

%% @doc Format a documentation response as JSON.
-spec format_docs(binary()) -> binary().
format_docs(DocText) ->
    jsx:encode(#{<<"type">> => <<"docs">>, <<"docs">> => DocText}).

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

%% @doc Encode reload response with classes, affected actor count, and migration results.
-spec encode_reloaded(
    [map()],
    non_neg_integer(),
    [{pid(), term()}],
    beamtalk_repl_protocol:protocol_msg()
) -> binary().
encode_reloaded(Classes, ActorCount, MigrationFailures, Msg) ->
    encode_reloaded(Classes, ActorCount, MigrationFailures, Msg, []).

%% @doc Encode a reload response with optional class collision warnings.
%% BT-737: Warnings are surfaced when a reload causes a cross-package class collision.
-spec encode_reloaded(
    [map()],
    non_neg_integer(),
    [{pid(), term()}],
    beamtalk_repl_protocol:protocol_msg(),
    [binary()]
) -> binary().
encode_reloaded(Classes, ActorCount, MigrationFailures, Msg, Warnings) ->
    ClassNames = [list_to_binary(maps:get(name, C, "")) || C <- Classes],
    Base = beamtalk_repl_protocol:base_response(Msg),
    FailureCount = length(MigrationFailures),
    Full = Base#{
        <<"classes">> => ClassNames,
        <<"affected_actors">> => ActorCount,
        <<"migration_failures">> => FailureCount,
        <<"status">> => [<<"done">>]
    },
    jsx:encode(maybe_add_warnings_reloaded(Full, Warnings)).

%% @private
-spec maybe_add_warnings_reloaded(map(), [binary()]) -> map().
maybe_add_warnings_reloaded(Map, []) -> Map;
maybe_add_warnings_reloaded(Map, Warnings) -> Map#{<<"warnings">> => Warnings}.

%%% Term-to-JSON Conversion

%% @doc Convert an Erlang term to a JSON-encodable value for jsx.
-spec term_to_json(term()) -> term().
term_to_json(Value) when is_integer(Value); is_float(Value); is_boolean(Value) ->
    Value;
term_to_json(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
term_to_json(Value) when is_binary(Value) ->
    beamtalk_transcript_stream:ensure_utf8(Value);
term_to_json(Value) when is_list(Value) ->
    case Value of
        [] ->
            [];
        _ ->
            case io_lib:printable_list(Value) of
                true ->
                    case unicode:characters_to_binary(Value) of
                        Bin when is_binary(Bin) -> Bin;
                        {error, _, _} -> list_to_binary(io_lib:format("~p", [Value]));
                        {incomplete, _, _} -> list_to_binary(io_lib:format("~p", [Value]))
                    end;
                false ->
                    [term_to_json(E) || E <- Value]
            end
    end;
term_to_json(Value) when is_pid(Value) ->
    case is_process_alive(Value) of
        true ->
            case process_info(Value, current_function) of
                {current_function, {beamtalk_future, pending, _}} ->
                    iolist_to_binary(<<"#Future<pending>">>);
                {current_function, {beamtalk_future, resolved, _}} ->
                    iolist_to_binary(<<"#Future<resolved>">>);
                {current_function, {beamtalk_future, rejected, _}} ->
                    iolist_to_binary(<<"#Future<rejected>">>);
                undefined ->
                    PidStr = pid_to_list(Value),
                    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
                    iolist_to_binary([<<"#Dead<">>, Inner, <<">">>]);
                _ ->
                    PidStr = pid_to_list(Value),
                    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
                    iolist_to_binary([<<"#Actor<">>, Inner, <<">">>])
            end;
        false ->
            PidStr = pid_to_list(Value),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Dead<">>, Inner, <<">">>])
    end;
term_to_json(Value) when is_function(Value) ->
    {arity, Arity} = erlang:fun_info(Value, arity),
    iolist_to_binary([<<"a Block/">>, integer_to_binary(Arity)]);
term_to_json(Value) when is_map(Value) ->
    beamtalk_primitive:print_string(Value);
term_to_json(#beamtalk_error{} = Error) ->
    iolist_to_binary(beamtalk_error:format(Error));
term_to_json(Value) when is_tuple(Value) ->
    case Value of
        {beamtalk_object, 'Metaclass', _Module, Pid} ->
            %% ADR 0036: Metaclass objects display as "ClassName class" (e.g. "Integer class").
            ClassName = beamtalk_object_class:class_name(Pid),
            iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
        {beamtalk_object, Class, _Module, Pid} ->
            case beamtalk_class_registry:is_class_name(Class) of
                true ->
                    beamtalk_class_registry:class_display_name(Class);
                false ->
                    ClassBin = atom_to_binary(Class, utf8),
                    PidStr = pid_to_list(Pid),
                    Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
                    iolist_to_binary([<<"#Actor<">>, ClassBin, <<",">>, Inner, <<">">>])
            end;
        {future_timeout, Pid} when is_pid(Pid) ->
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Future<timeout,">>, Inner, <<">">>]);
        {future_rejected, Reason} ->
            iolist_to_binary([<<"#Future<rejected: ">>, format_rejection_reason(Reason), <<">">>]);
        _ ->
            ElementStrs = [beamtalk_primitive:print_string(E) || E <- tuple_to_list(Value)],
            iolist_to_binary([<<"{">>, lists:join(<<", ">>, ElementStrs), <<"}">>])
    end;
term_to_json(Value) ->
    beamtalk_transcript_stream:ensure_utf8(iolist_to_binary(io_lib:format("~p", [Value]))).

%%% Error Formatting

%% @doc Format an error reason as a human-readable message.
-spec format_error_message(term()) -> binary().
format_error_message(#{'$beamtalk_class' := Class, error := Error}) ->
    ClassName = atom_to_binary(Class, utf8),
    iolist_to_binary([ClassName, <<": ">>, beamtalk_error:format(Error)]);
format_error_message(#beamtalk_error{} = Error) ->
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
format_error_message({eval_error, _Class, #{'$beamtalk_class' := ExClass, error := Error}}) ->
    ClassName = atom_to_binary(ExClass, utf8),
    iolist_to_binary([ClassName, <<": ">>, beamtalk_error:format(Error)]);
format_error_message({eval_error, _Class, #beamtalk_error{} = Error}) ->
    iolist_to_binary(beamtalk_error:format(Error));
format_error_message({eval_error, Class, Reason}) ->
    iolist_to_binary([
        <<"Evaluation error: ">>, atom_to_binary(Class, utf8), <<":">>, format_name(Reason)
    ]);
format_error_message({load_error, Reason}) ->
    iolist_to_binary([<<"Failed to load bytecode: ">>, format_name(Reason)]);
format_error_message({file_not_found, Path}) ->
    iolist_to_binary([<<"File not found: ">>, format_name(Path)]);
format_error_message({read_error, Reason}) ->
    iolist_to_binary([<<"Failed to read file: ">>, format_name(Reason)]);
format_error_message({module_not_found, ModuleName}) ->
    iolist_to_binary([<<"Module not loaded: ">>, ModuleName]);
format_error_message({invalid_module_name, ModuleName}) ->
    iolist_to_binary([<<"Invalid module name: ">>, ModuleName]);
format_error_message({actors_exist, ModuleName, Count}) ->
    CountStr = integer_to_list(Count),
    ActorWord =
        if
            Count == 1 -> <<"actor">>;
            true -> <<"actors">>
        end,
    iolist_to_binary([
        <<"Cannot unload ">>,
        atom_to_binary(ModuleName, utf8),
        <<": ">>,
        CountStr,
        <<" ">>,
        ActorWord,
        <<" still running. Kill them first with :kill">>
    ]);
format_error_message({class_not_found, ClassName}) ->
    NameBin = to_binary(ClassName),
    iolist_to_binary([
        <<"Unknown class: ">>,
        NameBin,
        <<". Use :modules to see loaded classes.">>
    ]);
format_error_message({method_not_found, ClassName, Selector}) ->
    NameBin = to_binary(ClassName),
    iolist_to_binary([
        NameBin,
        <<" does not understand ">>,
        Selector,
        <<". Use :help ">>,
        NameBin,
        <<" to see available methods.">>
    ]);
format_error_message({unknown_op, Op}) ->
    iolist_to_binary([<<"Unknown operation: ">>, Op]);
format_error_message({inspect_failed, PidStr}) ->
    iolist_to_binary([<<"Failed to inspect actor: ">>, list_to_binary(PidStr)]);
format_error_message({actor_not_alive, PidStr}) ->
    iolist_to_binary([<<"Actor is not alive: ">>, list_to_binary(PidStr)]);
format_error_message({no_source_file, Module}) ->
    iolist_to_binary([
        <<"No source file recorded for module: ">>,
        list_to_binary(Module),
        <<". Try :load <path> to load it first.">>
    ]);
format_error_message({module_not_loaded, Module}) ->
    iolist_to_binary([
        <<"Module not loaded: ">>,
        format_name(Module),
        <<". Use :load <path> to load it first.">>
    ]);
format_error_message({missing_module_name, reload}) ->
    <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>;
format_error_message({session_creation_failed, Reason}) ->
    iolist_to_binary([<<"Failed to create session: ">>, format_name(Reason)]);
format_error_message(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%% Internal Helpers

%% @doc Format a rejection reason for display in #Future<rejected: ...>.
-spec format_rejection_reason(term()) -> iolist().
format_rejection_reason(#beamtalk_error{} = Error) ->
    beamtalk_error:format(Error);
format_rejection_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Format a term as a binary name for use in error messages.
-spec format_name(term()) -> binary().
format_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) ->
    Name;
format_name(Name) when is_list(Name) ->
    list_to_binary(Name);
format_name(Name) ->
    iolist_to_binary(io_lib:format("~p", [Name])).

%% @doc Convert atom or binary to binary.
-spec to_binary(atom() | binary()) -> binary().
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_binary(V) -> V.
