%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc REPL message protocol encoder/decoder
%%%
%%% **DDD Context:** REPL
%%%
%%% Implements the nREPL-inspired message protocol for REPL communication.
%%% Supports both the new protocol format (op/id/session) and the legacy
%%% format (type/expression) for backward compatibility.
%%%
%%% New protocol format:
%%%   Request:  {"op": "eval", "id": "msg-001", "session": "s1", "code": "1 + 2"}
%%%   Response: {"id": "msg-001", "session": "s1", "value": "3", "status": ["done"]}
%%%
%%% Legacy format (backward compatible):
%%%   Request:  {"type": "eval", "expression": "1 + 2"}
%%%   Response: {"type": "result", "value": "3"}

-module(beamtalk_repl_protocol).

-export([
    decode/1,
    parse_request/1,
    encode_result/3, encode_result/4, encode_result/5,
    encode_error/3, encode_error/4, encode_error/5,
    encode_status/3,
    encode_out/3,
    encode_need_input/2,
    encode_bindings/3,
    encode_loaded/3, encode_loaded/4,
    encode_actors/3,
    encode_modules/3,
    encode_sessions/3,
    encode_inspect/2, encode_inspect/3,
    encode_docs/2,
    encode_describe/3,
    encode_test_results/2,
    is_legacy/1,
    get_op/1,
    get_id/1,
    get_session/1,
    get_params/1,
    base_response/1
]).

%% Protocol request record
-record(protocol_msg, {
    %% Operation name
    op :: binary(),
    %% Message correlation ID
    id :: binary() | undefined,
    %% Session ID
    session :: binary() | undefined,
    %% Operation-specific parameters
    params :: map(),
    %% Whether request used legacy format
    legacy :: boolean()
}).

-type protocol_msg() :: #protocol_msg{}.
-export_type([protocol_msg/0]).

%%% Legacy request parsing (deprecated — new code should use decode/1)

%% @doc Parse a request from the CLI (legacy interface).
%% Expected format: JSON with "type" field.
%% New code should use beamtalk_repl_protocol:decode/1 instead.
-spec parse_request(binary()) ->
    {eval, string()}
    | {clear_bindings}
    | {get_bindings}
    | {load_file, string()}
    | {load_source, binary()}
    | {list_actors}
    | {kill_actor, string()}
    | {list_modules}
    | {get_docs, binary(), binary() | undefined}
    | {health}
    | {shutdown, string()}
    | {error, term()}.
parse_request(Data) when is_binary(Data) ->
    try
        %% Remove trailing newline if present
        Trimmed = string:trim(Data),
        %% Try to parse as JSON
        case parse_json(Trimmed) of
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
    {eval, string()}
    | {clear_bindings}
    | {get_bindings}
    | {load_file, string()}
    | {load_source, binary()}
    | {list_actors}
    | {list_modules}
    | {kill_actor, string()}
    | {get_docs, binary(), binary() | undefined}
    | {health}
    | {shutdown, string()}
    | {error, term()}.
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

%%% Decoding

%% @doc Decode a raw binary message into a protocol message.
%% Supports both new format (op field) and legacy format (type field).
-spec decode(binary()) -> {ok, protocol_msg()} | {error, term()}.
decode(Data) when is_binary(Data) ->
    Trimmed = string:trim(Data),
    case parse_json(Trimmed) of
        {ok, Map} when is_map(Map) ->
            decode_map(Map);
        {ok, _NonMap} ->
            {error, {invalid_request, non_object_json}};
        {error, _} ->
            %% Not JSON - treat as raw eval expression
            case Trimmed of
                <<>> ->
                    {error, empty_expression};
                _ ->
                    {ok, #protocol_msg{
                        op = <<"eval">>,
                        id = undefined,
                        session = undefined,
                        params = #{<<"code">> => Trimmed},
                        legacy = true
                    }}
            end
    end.

%% @doc Check if a decoded message used the legacy format.
-spec is_legacy(protocol_msg()) -> boolean().
is_legacy(#protocol_msg{legacy = Legacy}) -> Legacy.

%% @doc Get the operation name from a protocol message.
-spec get_op(protocol_msg()) -> binary().
get_op(#protocol_msg{op = Op}) -> Op.

%% @doc Get the message correlation ID.
-spec get_id(protocol_msg()) -> binary() | undefined.
get_id(#protocol_msg{id = Id}) -> Id.

%% @doc Get the session identifier.
-spec get_session(protocol_msg()) -> binary() | undefined.
get_session(#protocol_msg{session = Session}) -> Session.

%% @doc Get the operation-specific parameters.
-spec get_params(protocol_msg()) -> map().
get_params(#protocol_msg{params = Params}) -> Params.

%%% Encoding

%% @doc Encode a successful result response.
-spec encode_result(term(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_result(Value, Msg, TermToJson) ->
    encode_result(Value, Msg, TermToJson, <<>>, []).

%% @doc Encode a successful result response with captured stdout.
-spec encode_result(term(), protocol_msg(), fun((term()) -> term()), binary()) -> binary().
encode_result(Value, Msg, TermToJson, Output) ->
    encode_result(Value, Msg, TermToJson, Output, []).

%% @doc Encode a successful result response with captured stdout and warnings.
-spec encode_result(term(), protocol_msg(), fun((term()) -> term()), binary(), [binary()]) ->
    binary().
encode_result(Value, Msg, TermToJson, Output, Warnings) ->
    JsonValue = TermToJson(Value),
    case Msg#protocol_msg.legacy of
        true ->
            Base = #{<<"type">> => <<"result">>, <<"value">> => JsonValue},
            jsx:encode(maybe_add_warnings(maybe_add_output(Base, Output), Warnings));
        false ->
            Base = base_response(Msg),
            Full = Base#{<<"value">> => JsonValue, <<"status">> => [<<"done">>]},
            jsx:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    end.

%% @doc Encode an error response.
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary())) -> binary().
encode_error(Reason, Msg, FormatError) ->
    encode_error(Reason, Msg, FormatError, <<>>, []).

%% @doc Encode an error response with captured stdout.
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary()), binary()) -> binary().
encode_error(Reason, Msg, FormatError, Output) ->
    encode_error(Reason, Msg, FormatError, Output, []).

%% @doc Encode an error response with captured stdout and warnings.
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary()), binary(), [binary()]) ->
    binary().
encode_error(Reason, Msg, FormatError, Output, Warnings) ->
    Message = FormatError(Reason),
    case Msg#protocol_msg.legacy of
        true ->
            Base = #{<<"type">> => <<"error">>, <<"message">> => Message},
            jsx:encode(maybe_add_warnings(maybe_add_output(Base, Output), Warnings));
        false ->
            Base = base_response(Msg),
            Full = Base#{<<"error">> => Message, <<"status">> => [<<"done">>, <<"error">>]},
            jsx:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    end.

%% @doc Encode a status-only response (e.g., for clear, close).
-spec encode_status(atom(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_status(Status, Msg, TermToJson) ->
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"result">>, <<"value">> => TermToJson(Status)});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"value">> => TermToJson(Status), <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a streaming stdout chunk (BT-696).
%% Sent as an intermediate message during eval before the final done message.
%% In legacy mode, this is a no-op (output is buffered in the final response).
-spec encode_out(binary(), protocol_msg(), binary()) -> binary().
encode_out(_Chunk, #protocol_msg{legacy = true}, _Stream) ->
    %% Legacy clients don't support streaming — output will be in final response
    <<>>;
encode_out(Chunk, Msg, Stream) ->
    Base = base_response(Msg),
    jsx:encode(Base#{Stream => Chunk}).

%% @doc Encode a need-input status message (BT-698).
%% Sent when eval code requests stdin input (e.g. io:get_line).
-spec encode_need_input(binary(), protocol_msg()) -> binary().
encode_need_input(Prompt, Msg) ->
    Base = base_response(Msg),
    jsx:encode(Base#{<<"status">> => [<<"need-input">>], <<"prompt">> => Prompt}).

%% @doc Encode a bindings response.
-spec encode_bindings(map(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_bindings(Bindings, Msg, TermToJson) ->
    JsonBindings = maps:fold(
        fun(Name, Value, Acc) ->
            NameBin = to_binary(Name),
            maps:put(NameBin, TermToJson(Value), Acc)
        end,
        #{},
        Bindings
    ),
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"bindings">>, <<"bindings">> => JsonBindings});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"bindings">> => JsonBindings, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a loaded file response.
-spec encode_loaded([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_loaded(Classes, Msg, TermToJson) ->
    encode_loaded(Classes, Msg, TermToJson, []).

%% @doc Encode a loaded file response with warnings.
%% BT-737: Warnings are class collision warnings from loading classes that
%% shadow classes already registered from a different module.
-spec encode_loaded([map()], protocol_msg(), fun((term()) -> term()), [binary()]) -> binary().
encode_loaded(Classes, Msg, _TermToJson, Warnings) ->
    ClassNames = [list_to_binary(maps:get(name, C, "")) || C <- Classes],
    case Msg#protocol_msg.legacy of
        true ->
            Base = #{<<"type">> => <<"loaded">>, <<"classes">> => ClassNames},
            jsx:encode(maybe_add_warnings(Base, Warnings));
        false ->
            Base = base_response(Msg),
            Full = Base#{<<"classes">> => ClassNames, <<"status">> => [<<"done">>]},
            jsx:encode(maybe_add_warnings(Full, Warnings))
    end.

%% @doc Encode an actors list response.
-spec encode_actors([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_actors(Actors, Msg, _TermToJson) ->
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
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"actors">>, <<"actors">> => JsonActors});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"actors">> => JsonActors, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a modules list response.
-spec encode_modules([{atom(), map()}], protocol_msg(), fun((term()) -> term())) -> binary().
encode_modules(ModulesWithInfo, Msg, _TermToJson) ->
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
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"modules">>, <<"modules">> => JsonModules});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"modules">> => JsonModules, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a sessions list response.
-spec encode_sessions([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_sessions(Sessions, Msg, _TermToJson) ->
    JsonSessions = lists:map(
        fun(#{id := Id} = S) ->
            Base = #{<<"id">> => Id},
            case maps:find(created_at, S) of
                {ok, T} -> Base#{<<"created_at">> => T};
                error -> Base
            end
        end,
        Sessions
    ),
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"sessions">>, <<"sessions">> => JsonSessions});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"sessions">> => JsonSessions, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode an actor inspect response with a pre-formatted string.
-spec encode_inspect(binary(), protocol_msg()) -> binary().
encode_inspect(InspectStr, Msg) ->
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"inspect">>, <<"state">> => InspectStr});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"state">> => InspectStr, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode an actor inspect response.
-spec encode_inspect(map(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_inspect(ActorState, Msg, TermToJson) ->
    JsonState = maps:fold(
        fun(K, V, Acc) ->
            maps:put(to_binary(K), TermToJson(V), Acc)
        end,
        #{},
        ActorState
    ),
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"inspect">>, <<"state">> => JsonState});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"state">> => JsonState, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a documentation response.
-spec encode_docs(binary(), protocol_msg()) -> binary().
encode_docs(DocText, Msg) ->
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{<<"type">> => <<"docs">>, <<"docs">> => DocText});
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{<<"docs">> => DocText, <<"status">> => [<<"done">>]})
    end.

%% @doc Encode a describe response with ops, versions, and capabilities.
-spec encode_describe(map(), map(), protocol_msg()) -> binary().
encode_describe(Ops, Versions, Msg) ->
    case Msg#protocol_msg.legacy of
        true ->
            jsx:encode(#{
                <<"type">> => <<"describe">>,
                <<"ops">> => Ops,
                <<"versions">> => Versions
            });
        false ->
            Base = base_response(Msg),
            jsx:encode(Base#{
                <<"ops">> => Ops,
                <<"versions">> => Versions,
                <<"status">> => [<<"done">>]
            })
    end.

%% @doc Encode a test results response.
%%
%% Uses `["done", "test-error"]` status when any tests failed, `["done"]`
%% otherwise. The `results` field contains a JSON map with `passed`,
%% `failed`, `total`, `duration`, and `tests` keys.
-spec encode_test_results(map(), protocol_msg()) -> binary().
encode_test_results(
    #{
        '$beamtalk_class' := 'TestResult',
        total := Total,
        passed := Passed,
        failed := Failed,
        duration := Duration,
        tests := Tests
    },
    Msg
) ->
    Status =
        case Failed > 0 of
            true -> [<<"done">>, <<"test-error">>];
            false -> [<<"done">>]
        end,
    ResultMap = #{
        <<"passed">> => Passed,
        <<"failed">> => Failed,
        <<"total">> => Total,
        <<"duration">> => Duration,
        <<"tests">> => [encode_test_entry(T) || T <- Tests]
    },
    Base = base_response(Msg),
    jsx:encode(Base#{<<"status">> => Status, <<"results">> => ResultMap}).

%% @private
-spec encode_test_entry(map()) -> map().
encode_test_entry(#{name := Name, status := pass}) ->
    #{<<"name">> => atom_to_binary(Name, utf8), <<"status">> => <<"pass">>};
encode_test_entry(#{name := Name, status := fail, error := Error}) ->
    ErrorBin =
        case is_binary(Error) of
            true -> Error;
            false -> iolist_to_binary(io_lib:format("~p", [Error]))
        end,
    #{
        <<"name">> => atom_to_binary(Name, utf8),
        <<"status">> => <<"fail">>,
        <<"error">> => ErrorBin
    };
encode_test_entry(Entry) ->
    %% Fallback for unexpected shapes
    Name = maps:get(name, Entry, unknown),
    #{<<"name">> => atom_to_binary(Name, utf8), <<"status">> => <<"unknown">>}.

%%% Internal functions

%% @private
%% @doc Decode a parsed JSON map into a protocol message.
-spec decode_map(map()) -> {ok, protocol_msg()} | {error, term()}.
decode_map(#{<<"op">> := Op} = Map) ->
    %% New protocol format
    {ok, #protocol_msg{
        op = Op,
        id = maps:get(<<"id">>, Map, undefined),
        session = maps:get(<<"session">>, Map, undefined),
        params = maps:without([<<"op">>, <<"id">>, <<"session">>], Map),
        legacy = false
    }};
decode_map(#{<<"type">> := Type} = Map) ->
    %% Legacy format - translate to protocol message
    {Op, Params} = legacy_to_op(Type, Map),
    {ok, #protocol_msg{
        op = Op,
        id = undefined,
        session = undefined,
        params = Params,
        legacy = true
    }};
decode_map(_) ->
    {error, {invalid_request, missing_op_or_type}}.

%% @private
%% @doc Translate legacy type+fields to op+params.
-spec legacy_to_op(binary(), map()) -> {binary(), map()}.
legacy_to_op(<<"eval">>, Map) ->
    Code = maps:get(<<"expression">>, Map, <<>>),
    {<<"eval">>, #{<<"code">> => Code}};
legacy_to_op(<<"clear">>, _Map) ->
    {<<"clear">>, #{}};
legacy_to_op(<<"bindings">>, _Map) ->
    {<<"bindings">>, #{}};
legacy_to_op(<<"load">>, Map) ->
    Path = maps:get(<<"path">>, Map, <<>>),
    {<<"load-file">>, #{<<"path">> => Path}};
legacy_to_op(<<"actors">>, _Map) ->
    {<<"actors">>, #{}};
legacy_to_op(<<"modules">>, _Map) ->
    {<<"modules">>, #{}};
legacy_to_op(<<"unload">>, Map) ->
    Module = maps:get(<<"module">>, Map, <<>>),
    {<<"unload">>, #{<<"module">> => Module}};
legacy_to_op(<<"kill">>, Map) ->
    Pid = maps:get(<<"pid">>, Map, <<>>),
    {<<"kill">>, #{<<"actor">> => Pid}};
legacy_to_op(Type, _Map) ->
    {Type, #{}}.

%% @doc Build base response map with id and session fields.
-spec base_response(protocol_msg()) -> map().
base_response(#protocol_msg{id = Id, session = Session}) ->
    M0 = #{},
    M1 =
        case Id of
            undefined -> M0;
            _ -> M0#{<<"id">> => Id}
        end,
    case Session of
        undefined -> M1;
        _ -> M1#{<<"session">> => Session}
    end.

%% @private
%% @doc Parse a JSON binary into a decoded term.
-spec parse_json(binary()) -> {ok, term()} | {error, term()}.
parse_json(Data) ->
    try
        Decoded = jsx:decode(Data, [return_maps]),
        {ok, Decoded}
    catch
        _:_ -> {error, not_json}
    end.

%% @private
%% @doc Convert a term to binary for use as a JSON key.
-spec to_binary(term()) -> binary().
to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
to_binary(Name) when is_binary(Name) ->
    Name;
to_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
to_binary(Name) ->
    list_to_binary(io_lib:format("~p", [Name])).

%% @private
%% @doc Add output field to response map only when non-empty.
-spec maybe_add_output(map(), binary()) -> map().
maybe_add_output(Map, <<>>) ->
    Map;
maybe_add_output(Map, Output) when is_binary(Output) ->
    Map#{<<"output">> => Output}.

%% @private
%% @doc Add warnings field to response map only when non-empty.
-spec maybe_add_warnings(map(), [binary()]) -> map().
maybe_add_warnings(Map, []) -> Map;
maybe_add_warnings(Map, Warnings) -> Map#{<<"warnings">> => Warnings}.
