%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_protocol).

%%% **DDD Context:** REPL Session Context

-moduledoc """
REPL message protocol encoder/decoder

Implements the nREPL-inspired message protocol for REPL communication
(op/id/session):

  Request:  {"op": "eval", "id": "msg-001", "session": "s1", "code": "1 + 2"}
  Response: {"id": "msg-001", "session": "s1", "value": "3", "status": ["done"]}

A raw non-JSON line is accepted as an eval expression for robustness
(manual testing via netcat and similar tools); the response uses the
standard protocol shape. The pre-0.5.0 legacy format ("type" field
requests, "type"-keyed responses) was removed in BT-2789.
""".

-export([
    decode/1,
    encode_result/3, encode_result/4, encode_result/5,
    encode_script_exit/4,
    encode_error/3, encode_error/4, encode_error/5, encode_error/6,
    encode_status/3,
    encode_out/3,
    encode_need_input/2,
    encode_loaded/3, encode_loaded/4,
    encode_actors/3,
    encode_modules/3,
    encode_sessions/3,
    encode_inspect/2, encode_inspect/3,
    encode_docs/2,
    encode_describe/3,
    encode_test_results/2,
    encode_trace_result/5,
    encode_completions/2,
    encode_diagnostics/2,
    encode_codegen/3,
    encode_methods/3,
    encode_class_list/2,
    encode_health/3,
    encode_load_project/5,
    %% BT-2801: per-finding encoder for the `reload-findings` op response,
    %% shared with `beamtalk_ws_handler`'s `reload_check` push frame so the
    %% two wire shapes can never drift apart.
    encode_reload_finding/1,
    undefined_to_null/1,
    get_op/1,
    get_id/1,
    get_session/1,
    get_params/1,
    base_response/1,
    to_binary/1
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
    params :: map()
}).

-type protocol_msg() :: #protocol_msg{}.
-export_type([protocol_msg/0]).

%%% Decoding

-doc """
Decode a raw binary message into a protocol message.
Requests must carry an `op` field; a raw non-JSON line is accepted as an
eval expression.
""".
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
                        params = #{<<"code">> => Trimmed}
                    }}
            end
    end.

-doc "Get the operation name from a protocol message.".
-spec get_op(protocol_msg()) -> binary().
get_op(#protocol_msg{op = Op}) -> Op.

-doc "Get the message correlation ID.".
-spec get_id(protocol_msg()) -> binary() | undefined.
get_id(#protocol_msg{id = Id}) -> Id.

-doc "Get the session identifier.".
-spec get_session(protocol_msg()) -> binary() | undefined.
get_session(#protocol_msg{session = Session}) -> Session.

-doc "Get the operation-specific parameters.".
-spec get_params(protocol_msg()) -> map().
get_params(#protocol_msg{params = Params}) -> Params.

%%% Encoding

-doc "Encode a successful result response.".
-spec encode_result(term(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_result(Value, Msg, TermToJson) ->
    encode_result(Value, Msg, TermToJson, <<>>, []).

-doc "Encode a successful result response with captured stdout.".
-spec encode_result(term(), protocol_msg(), fun((term()) -> term()), binary()) -> binary().
encode_result(Value, Msg, TermToJson, Output) ->
    encode_result(Value, Msg, TermToJson, Output, []).

-doc "Encode a successful result response with captured stdout and warnings.".
-spec encode_result(term(), protocol_msg(), fun((term()) -> term()), binary(), [binary()]) ->
    binary().
encode_result(Value, Msg, TermToJson, Output, Warnings) ->
    JsonValue = TermToJson(Value),
    Base = base_response(Msg),
    Full = Base#{<<"value">> => JsonValue, <<"status">> => [<<"done">>]},
    iolist_to_binary(
        json:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    ).

-doc """
Encode a connected-session `Program exit:` response (BT-2688, ADR 0099 §3).

A successful `done` response (NOT an error) that additionally carries the POSIX
exit status in a dedicated `exit_code` field, so the connecting client can adopt
it as its own process exit code. `value` is `null` — `Program exit:` does not
return a value. The session shell has already terminated by the time this is
encoded; the shared node stays up.
""".
-spec encode_script_exit(non_neg_integer(), protocol_msg(), binary(), [binary()]) -> binary().
encode_script_exit(Code, Msg, Output, Warnings) ->
    Base = base_response(Msg),
    Full = Base#{
        <<"value">> => null,
        <<"status">> => [<<"done">>],
        <<"exit_code">> => Code
    },
    iolist_to_binary(
        json:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    ).

-doc "Encode an error response.".
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary())) -> binary().
encode_error(Reason, Msg, FormatError) ->
    encode_error(Reason, Msg, FormatError, <<>>, []).

-doc "Encode an error response with captured stdout.".
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary()), binary()) -> binary().
encode_error(Reason, Msg, FormatError, Output) ->
    encode_error(Reason, Msg, FormatError, Output, []).

-doc "Encode an error response with captured stdout and warnings.".
-spec encode_error(term(), protocol_msg(), fun((term()) -> binary()), binary(), [binary()]) ->
    binary().
encode_error(Reason, Msg, FormatError, Output, Warnings) ->
    encode_error(Reason, Msg, FormatError, Output, Warnings, #{}).

-doc """
Encode an error response with captured stdout, warnings, and extra metadata fields.
BT-1235: Metadata may include `<<"line">>' and `<<"hint">>' for compile errors.
""".
-spec encode_error(
    term(), protocol_msg(), fun((term()) -> binary()), binary(), [binary()], map()
) -> binary().
encode_error(Reason, Msg, FormatError, Output, Warnings, Metadata) ->
    Message = FormatError(Reason),
    Base = base_response(Msg),
    Full0 = Base#{<<"error">> => Message, <<"status">> => [<<"done">>, <<"error">>]},
    Full = maps:merge(Full0, Metadata),
    iolist_to_binary(
        json:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    ).

-doc "Encode a status-only response (e.g., for clear, close).".
-spec encode_status(atom(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_status(Status, Msg, TermToJson) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"value">> => TermToJson(Status), <<"status">> => [<<"done">>]})
    ).

-doc """
Encode a streaming stdout chunk (BT-696).
Sent as an intermediate message during eval before the final done message.
""".
-spec encode_out(binary(), protocol_msg(), binary()) -> binary().
encode_out(Chunk, Msg, Stream) ->
    Base = base_response(Msg),
    iolist_to_binary(json:encode(Base#{Stream => Chunk})).

-doc """
Encode a need-input status message (BT-698).
Sent when eval code requests stdin input (e.g. io:get_line).
""".
-spec encode_need_input(binary(), protocol_msg()) -> binary().
encode_need_input(Prompt, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"status">> => [<<"need-input">>], <<"prompt">> => Prompt})
    ).

-doc "Encode a loaded file response.".
-spec encode_loaded([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_loaded(Classes, Msg, TermToJson) ->
    encode_loaded(Classes, Msg, TermToJson, []).

-doc """
Encode a loaded file response with warnings.
BT-737: Warnings are class collision warnings from loading classes that
shadow classes already registered from a different module.
""".
-spec encode_loaded([map()], protocol_msg(), fun((term()) -> term()), [binary()]) -> binary().
encode_loaded(Classes, Msg, _TermToJson, Warnings) ->
    ClassNames = [list_to_binary(maps:get(name, C, "")) || C <- Classes],
    Base = base_response(Msg),
    Full = Base#{<<"classes">> => ClassNames, <<"status">> => [<<"done">>]},
    iolist_to_binary(json:encode(maybe_add_warnings(Full, Warnings))).

-doc "Encode an actors list response.".
-spec encode_actors([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_actors(Actors, Msg, _TermToJson) ->
    JsonActors = [
        #{
            <<"pid">> => list_to_binary(pid_to_list(Pid)),
            <<"class">> => atom_to_binary(Class, utf8),
            <<"module">> => atom_to_binary(Module, utf8),
            <<"spawned_at">> => SpawnedAt
        }
     || #{pid := Pid, class := Class, module := Module, spawned_at := SpawnedAt} <- Actors
    ],
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"actors">> => JsonActors, <<"status">> => [<<"done">>]})
    ).

-doc "Encode a modules list response.".
-spec encode_modules([{atom(), map()}], protocol_msg(), fun((term()) -> term())) -> binary().
encode_modules(ModulesWithInfo, Msg, _TermToJson) ->
    JsonModules = [
        #{
            <<"name">> => maps:get(name, Info),
            <<"source_file">> => list_to_binary(maps:get(source_file, Info)),
            <<"actor_count">> => maps:get(actor_count, Info),
            <<"load_time">> => maps:get(load_time, Info),
            <<"time_ago">> => list_to_binary(lists:flatten(maps:get(time_ago, Info)))
        }
     || {_ModuleName, Info} <- ModulesWithInfo
    ],
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"modules">> => JsonModules, <<"status">> => [<<"done">>]})
    ).

-doc "Encode a sessions list response.".
-spec encode_sessions([map()], protocol_msg(), fun((term()) -> term())) -> binary().
encode_sessions(Sessions, Msg, _TermToJson) ->
    JsonSessions = lists:map(
        fun(#{id := Id} = S) ->
            Base = #{<<"id">> => Id},
            % elp:fixme W0032 maps:find with complex branch logic
            case maps:find(created_at, S) of
                {ok, T} -> Base#{<<"created_at">> => T};
                error -> Base
            end
        end,
        Sessions
    ),
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"sessions">> => JsonSessions, <<"status">> => [<<"done">>]})
    ).

-doc "Encode an actor inspect response with a pre-formatted string.".
-spec encode_inspect(binary(), protocol_msg()) -> binary().
encode_inspect(InspectStr, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"state">> => InspectStr, <<"status">> => [<<"done">>]})
    ).

-doc "Encode an actor inspect response.".
-spec encode_inspect(map(), protocol_msg(), fun((term()) -> term())) -> binary().
encode_inspect(ActorState, Msg, TermToJson) ->
    JsonState = maps:fold(
        fun(K, V, Acc) ->
            Acc#{to_binary(K) => TermToJson(V)}
        end,
        #{},
        ActorState
    ),
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"state">> => JsonState, <<"status">> => [<<"done">>]})
    ).

-doc "Encode a documentation response.".
-spec encode_docs(binary(), protocol_msg()) -> binary().
encode_docs(DocText, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"docs">> => DocText, <<"status">> => [<<"done">>]})
    ).

-doc "Encode a describe response with ops, versions, and capabilities.".
-spec encode_describe(map(), map(), protocol_msg()) -> binary().
encode_describe(Ops, Versions, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{
            <<"ops">> => Ops,
            <<"versions">> => Versions,
            <<"status">> => [<<"done">>]
        })
    ).

-doc """
Encode a test results response.

Uses `["done", "test-error"]` status when any tests failed, `["done"]`
otherwise. The `results` field contains a JSON map with `passed`,
`failed`, `total`, `duration`, and `tests` keys.
""".
-spec encode_test_results(map(), protocol_msg()) -> binary().
encode_test_results(
    #{
        '$beamtalk_class' := 'TestResult',
        total := Total,
        passed := Passed,
        failed := Failed,
        skipped := Skipped,
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
        <<"skipped">> => Skipped,
        <<"total">> => Total,
        <<"duration">> => Duration,
        <<"tests">> => [encode_test_entry(T) || T <- Tests]
    },
    Base = base_response(Msg),
    iolist_to_binary(json:encode(Base#{<<"status">> => Status, <<"results">> => ResultMap})).

-spec encode_test_entry(map()) -> map().
encode_test_entry(#{name := Name, status := pass} = Entry) ->
    maybe_add_class(
        #{<<"name">> => atom_to_binary(Name, utf8), <<"status">> => <<"pass">>},
        Entry
    );
encode_test_entry(#{name := Name, status := fail, error := Error} = Entry) ->
    ErrorBin =
        case is_binary(Error) of
            true -> Error;
            false -> iolist_to_binary(io_lib:format("~p", [Error]))
        end,
    maybe_add_class(
        #{
            <<"name">> => atom_to_binary(Name, utf8),
            <<"status">> => <<"fail">>,
            <<"error">> => ErrorBin
        },
        Entry
    );
encode_test_entry(#{name := Name, status := skip} = Entry) ->
    Base0 = #{
        <<"name">> => atom_to_binary(Name, utf8),
        <<"status">> => <<"skip">>
    },
    Base =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(reason, Entry) of
            {ok, Reason} when is_binary(Reason) ->
                Base0#{<<"reason">> => Reason};
            {ok, Reason} ->
                Base0#{<<"reason">> => iolist_to_binary(io_lib:format("~p", [Reason]))};
            error ->
                Base0
        end,
    maybe_add_class(Base, Entry);
encode_test_entry(Entry) ->
    %% Fallback for unexpected shapes
    Name = maps:get(name, Entry, unknown),
    maybe_add_class(
        #{<<"name">> => atom_to_binary(Name, utf8), <<"status">> => <<"unknown">>},
        Entry
    ).

-doc "Add class name to encoded test entry if available.".
-spec maybe_add_class(map(), map()) -> map().
maybe_add_class(Encoded, #{class := ClassName}) when is_atom(ClassName) ->
    Encoded#{<<"class">> => atom_to_binary(ClassName, utf8)};
maybe_add_class(Encoded, _Entry) ->
    Encoded.

-doc """
Encode a trace result response (BT-1238).

`Steps' is `[{SourceBin, Value}]' — one entry per top-level statement.
The response includes a `steps' array where each entry has `src' and `value' fields.
""".
-spec encode_trace_result(
    [{binary(), term()}], protocol_msg(), fun((term()) -> term()), binary(), [binary()]
) -> binary().
encode_trace_result(Steps, Msg, TermToJson, Output, Warnings) ->
    JsonSteps = [
        #{<<"src">> => Src, <<"value">> => TermToJson(Val)}
     || {Src, Val} <- Steps
    ],
    Base = base_response(Msg),
    Full = Base#{<<"steps">> => JsonSteps, <<"status">> => [<<"done">>]},
    iolist_to_binary(
        json:encode(maybe_add_warnings(maybe_add_output(Full, Output), Warnings))
    ).

-doc """
Encode a completions response (BT-2402).

The base response with a `completions` array and a `done` status. Used by
the `complete` and `erlang-complete` ops.
""".
-spec encode_completions([binary()], protocol_msg()) -> binary().
encode_completions(Completions, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{
            <<"completions">> => Completions, <<"status">> => [<<"done">>]
        })
    ).

-doc """
Encode a diagnostics response (BT-2556).

Carries the parse-only diagnostics for an editor buffer: a list of maps, each
with `message`, `severity`, `start`, and `end` (byte offsets into the buffer).
`diagnostics` is consumed by the cockpit CodeMirror editors. The cockpit
consumes the `{diagnostics, _}`
TERM directly over distribution (`dispatch/4`); this JSON encoder exists only
for the browser WebSocket transport edge, mirroring `encode_completions/2`.
""".
-spec encode_diagnostics([map()], protocol_msg()) -> binary().
encode_diagnostics(Diagnostics, Msg) ->
    %% Diagnostic maps carry atom keys (message/severity/start/end); project to
    %% the binary-keyed wire shape so the JSON encoder emits stable string keys.
    Wire = [
        #{
            <<"message">> => maps:get(message, D, <<>>),
            <<"severity">> => maps:get(severity, D, <<"error">>),
            <<"start">> => maps:get(start, D, 0),
            <<"end">> => maps:get('end', D, 0)
        }
     || D <- Diagnostics
    ],
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"diagnostics">> => Wire, <<"status">> => [<<"done">>]})
    ).

-doc """
Encode one `beamtalk_recheck:finding()` map for the `reload_check` push
frame (`beamtalk_ws_handler:encode_reload_check_event/1`) and the
`reload-findings` op response (`beamtalk_repl_ops_dev:handle_term/4`,
BT-2801) — the one shared wire shape both surfaces rely on, so a client that
reads the initial snapshot via the op and then applies live `reload_check`
pushes never sees the two disagree on shape.
""".
-spec encode_reload_finding(map()) -> map().
encode_reload_finding(Finding) ->
    #{
        <<"owner">> => maps:get(owner, Finding, <<>>),
        <<"changedClass">> => maps:get(changed_class, Finding, <<>>),
        <<"selector">> => maps:get(selector, Finding, <<>>),
        <<"classification">> => atom_to_binary(maps:get(classification, Finding, removal), utf8),
        <<"severity">> => maps:get(severity, Finding, <<"hint">>),
        <<"category">> => undefined_to_null(maps:get(category, Finding, undefined)),
        <<"message">> => maps:get(message, Finding, <<>>),
        <<"note">> => undefined_to_null(maps:get(note, Finding, undefined)),
        <<"sites">> => [encode_reload_site(S) || S <- maps:get(sites, Finding, [])],
        <<"start">> => maps:get(start, Finding, 0),
        <<"end">> => maps:get('end', Finding, 0)
    }.

-doc "Encode one call-site reference (`#{method, line}`) for a reload finding.".
-spec encode_reload_site(map()) -> map().
encode_reload_site(Site) ->
    #{
        <<"method">> => maps:get(method, Site, <<>>),
        <<"line">> => maps:get(line, Site, 0)
    }.

-doc """
Map the Erlang "missing value" idiom `undefined` (used by `beamtalk_recheck`'s
optional finding fields — `category`, `note`, `cap_note` — none of which are
Beamtalk-side `nil`) to JSON `null`.
""".
-spec undefined_to_null(term()) -> term().
undefined_to_null(undefined) -> null;
undefined_to_null(Value) -> Value.

-doc """
Encode a show-codegen response (BT-2402).

Carries the generated Core Erlang source and any compiler warnings for the
`show-codegen` op.
""".
-spec encode_codegen(binary(), [binary()], protocol_msg()) -> binary().
encode_codegen(CoreErlang, Warnings, Msg) ->
    Base = base_response(Msg),
    Result = Base#{<<"core_erlang">> => CoreErlang, <<"status">> => [<<"done">>]},
    iolist_to_binary(json:encode(maybe_add_warnings(Result, Warnings))).

-doc """
Encode a methods response (BT-2402).

`Methods` is a list of method-descriptor maps and `StateVars` a list of
instance-variable name binaries for the `methods` op.
""".
-spec encode_methods([map()], [binary()], protocol_msg()) -> binary().
encode_methods(Methods, StateVars, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{
            <<"methods">> => Methods,
            <<"state_vars">> => StateVars,
            <<"status">> => [<<"done">>]
        })
    ).

-doc """
Encode a list-classes response (BT-2402).

`ClassList` is the sorted list of class-info maps for the `list-classes` op.
""".
-spec encode_class_list([map()], protocol_msg()) -> binary().
encode_class_list(ClassList, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{<<"class_list">> => ClassList, <<"status">> => [<<"done">>]})
    ).

-doc """
Encode a health response (BT-2402).

Carries the workspace identifier and the connection nonce for the `health` op.
""".
-spec encode_health(binary(), binary(), protocol_msg()) -> binary().
encode_health(WorkspaceId, Nonce, Msg) ->
    Base = base_response(Msg),
    iolist_to_binary(
        json:encode(Base#{
            <<"workspace_id">> => WorkspaceId,
            <<"nonce">> => Nonce,
            <<"status">> => [<<"done">>]
        })
    ).

-doc """
Encode a load-project response (BT-2402).

`Classes` is the list of loaded class-name binaries, `Errors` the combined
per-file and dependency-activation error maps, `Summary` the human-readable
reload summary, and `Warnings` any class-collision warnings.
""".
-spec encode_load_project([binary()], [map()], binary(), [binary()], protocol_msg()) -> binary().
encode_load_project(Classes, Errors, Summary, Warnings, Msg) ->
    Base = base_response(Msg),
    Response = Base#{
        <<"status">> => [<<"done">>],
        <<"classes">> => Classes,
        <<"errors">> => Errors,
        <<"summary">> => Summary
    },
    iolist_to_binary(json:encode(maybe_add_warnings(Response, Warnings))).

%%% Utilities

-doc "Normalise an atom, binary, list, or arbitrary term to a binary.".
-spec to_binary(term()) -> binary().
to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
to_binary(Name) when is_binary(Name) ->
    Name;
to_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
to_binary(Name) ->
    list_to_binary(io_lib:format("~p", [Name])).

%%% Internal functions

-doc "Decode a parsed JSON map into a protocol message.".
-spec decode_map(map()) -> {ok, protocol_msg()} | {error, term()}.
decode_map(#{<<"op">> := Op} = Map) ->
    {ok, #protocol_msg{
        op = Op,
        id = maps:get(<<"id">>, Map, undefined),
        session = maps:get(<<"session">>, Map, undefined),
        params = maps:without([<<"op">>, <<"id">>, <<"session">>], Map)
    }};
decode_map(_) ->
    {error, {invalid_request, missing_op}}.

-doc "Build base response map with id and session fields.".
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

-doc "Parse a JSON binary into a decoded term.".
-spec parse_json(binary()) -> {ok, term()} | {error, term()}.
parse_json(Data) ->
    try
        Decoded = json:decode(Data),
        {ok, Decoded}
    catch
        _:_ -> {error, not_json}
    end.

-doc "Add output field to response map only when non-empty.".
-spec maybe_add_output(map(), binary()) -> map().
maybe_add_output(Map, <<>>) ->
    Map;
maybe_add_output(Map, Output) when is_binary(Output) ->
    Map#{<<"output">> => Output}.

-doc "Add warnings field to response map only when non-empty.".
-spec maybe_add_warnings(map(), [binary()]) -> map().
maybe_add_warnings(Map, []) -> Map;
maybe_add_warnings(Map, Warnings) -> Map#{<<"warnings">> => Warnings}.
