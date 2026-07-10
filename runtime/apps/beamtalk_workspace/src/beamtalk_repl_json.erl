%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_json).

%%% **DDD Context:** REPL Session Context

-moduledoc """
JSON formatting for REPL protocol responses.

Pure formatting logic extracted from beamtalk_repl_server.
Converts Erlang terms to JSON-encodable values and builds
JSON response/error binaries for the REPL protocol.
Uses OTP `json` module (OTP 27+) for encoding/decoding.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% ADR 0094, Critical Risk #3: short timeout for the printString dispatch on a
%% live actor before falling back to the tuple-derived label. Kept small so the
%% REPL never visibly stalls on a wedged actor.
-define(ACTOR_PRINT_TIMEOUT_MS, 250).

-export([
    format_response/1,
    format_error/1,
    format_response_with_warnings/2,
    format_error_with_warnings/2,
    format_loaded/1,
    format_actors/1,
    format_modules/1,
    format_docs/1,
    term_to_json/1,
    format_error_message/1,
    parse_json/1,
    encode_reloaded/4, encode_reloaded/5,
    encode_error/2, encode_error/4, encode_error/5
]).

%%% JSON Parsing

-doc "Parse JSON binary into an Erlang term (maps, lists, binaries, etc.).".
-spec parse_json(binary()) -> {ok, term()} | {error, term()}.
parse_json(Data) ->
    try
        Decoded = json:decode(Data),
        {ok, Decoded}
    catch
        Class:Reason:Stack ->
            ?LOG_DEBUG("JSON parse failed", #{
                class => Class,
                reason => Reason,
                stack => lists:sublist(Stack, 3),
                data => Data,
                domain => [beamtalk, runtime]
            }),
            {error, not_json}
    end.

%%% Response Formatting

-doc "Format a successful response as JSON.".
-spec format_response(term()) -> binary().
format_response(Value) ->
    try
        JsonValue = term_to_json(Value),
        iolist_to_binary(json:encode(#{<<"type">> => <<"result">>, <<"value">> => JsonValue}))
    catch
        Class:Reason:_Stack ->
            ErrorMsg = io_lib:format("Internal error formatting ~p: ~p:~p", [Value, Class, Reason]),
            iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"error">>,
                    <<"message">> => iolist_to_binary(ErrorMsg)
                })
            )
    end.

-doc "Format an error response as JSON.".
-spec format_error(term()) -> binary().
format_error(Reason) ->
    try
        Message = format_error_message(Reason),
        iolist_to_binary(json:encode(#{<<"type">> => <<"error">>, <<"message">> => Message}))
    catch
        Class:FormatError:Stack ->
            ?LOG_DEBUG("Failed to format error", #{
                class => Class,
                reason => FormatError,
                stack => lists:sublist(Stack, 5),
                original_reason => Reason,
                domain => [beamtalk, runtime]
            }),
            iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"error">>,
                    <<"message">> => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
                })
            )
    end.

-doc "Format a successful response with warnings as JSON.".
-spec format_response_with_warnings(term(), [binary()]) -> binary().
format_response_with_warnings(Value, Warnings) ->
    try
        JsonValue = term_to_json(Value),
        Response = #{<<"type">> => <<"result">>, <<"value">> => JsonValue},
        case Warnings of
            [] -> iolist_to_binary(json:encode(Response));
            _ -> iolist_to_binary(json:encode(Response#{<<"warnings">> => Warnings}))
        end
    catch
        Class:Reason:_Stack ->
            ErrorMsg = io_lib:format("Internal error formatting ~p: ~p:~p", [Value, Class, Reason]),
            iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"error">>,
                    <<"message">> => iolist_to_binary(ErrorMsg)
                })
            )
    end.

-doc "Format an error response with warnings as JSON.".
-spec format_error_with_warnings(term(), [binary()]) -> binary().
format_error_with_warnings(Reason, Warnings) ->
    try
        Message = format_error_message(Reason),
        Response = #{<<"type">> => <<"error">>, <<"message">> => Message},
        case Warnings of
            [] -> iolist_to_binary(json:encode(Response));
            _ -> iolist_to_binary(json:encode(Response#{<<"warnings">> => Warnings}))
        end
    catch
        Class:FormatError:Stack ->
            ?LOG_DEBUG("Failed to format error", #{
                class => Class,
                reason => FormatError,
                stack => lists:sublist(Stack, 5),
                original_reason => Reason,
                domain => [beamtalk, runtime]
            }),
            iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"error">>,
                    <<"message">> => iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
                })
            )
    end.

-doc "Format a documentation response as JSON.".
-spec format_docs(binary()) -> binary().
format_docs(DocText) ->
    iolist_to_binary(json:encode(#{<<"type">> => <<"docs">>, <<"docs">> => DocText})).

-doc """
Format a loaded file response as JSON.
Classes is a list of #{name => string(), superclass => string()} maps.
""".
-spec format_loaded([map()]) -> binary().
format_loaded(Classes) ->
    ClassNames = [list_to_binary(maps:get(name, C, "")) || C <- Classes],
    iolist_to_binary(json:encode(#{<<"type">> => <<"loaded">>, <<"classes">> => ClassNames})).

-doc "Format an actors list response as JSON.".
-spec format_actors([beamtalk_repl_actors:actor_metadata()]) -> binary().
format_actors(Actors) ->
    JsonActors = [
        #{
            <<"pid">> => list_to_binary(pid_to_list(Pid)),
            <<"class">> => atom_to_binary(Class, utf8),
            <<"module">> => atom_to_binary(Module, utf8),
            <<"spawned_at">> => SpawnedAt
        }
     || #{pid := Pid, class := Class, module := Module, spawned_at := SpawnedAt} <- Actors
    ],
    iolist_to_binary(json:encode(#{<<"type">> => <<"actors">>, <<"actors">> => JsonActors})).

-doc "Format a modules list response as JSON.".
-spec format_modules([{atom(), map()}]) -> binary().
format_modules(ModulesWithInfo) ->
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
    iolist_to_binary(json:encode(#{<<"type">> => <<"modules">>, <<"modules">> => JsonModules})).

-doc """
Encode reload response with classes, affected actor count, and migration results.
""".
-spec encode_reloaded(
    [map()],
    non_neg_integer(),
    [{pid(), term()}],
    beamtalk_repl_protocol:protocol_msg()
) -> binary().
encode_reloaded(Classes, ActorCount, MigrationFailures, Msg) ->
    encode_reloaded(Classes, ActorCount, MigrationFailures, Msg, []).

-doc """
Encode a reload response with optional class collision warnings.
BT-737: Warnings are surfaced when a reload causes a cross-package class collision.
""".
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
    iolist_to_binary(json:encode(maybe_add_warnings_reloaded(Full, Warnings))).

-spec maybe_add_warnings_reloaded(map(), [binary()]) -> map().
maybe_add_warnings_reloaded(Map, []) -> Map;
maybe_add_warnings_reloaded(Map, Warnings) -> Map#{<<"warnings">> => Warnings}.

-doc "Encode an error response using the REPL error formatter.".
-spec encode_error(term(), beamtalk_repl_protocol:protocol_msg()) -> binary().
encode_error(Err, Msg) ->
    beamtalk_repl_protocol:encode_error(Err, Msg, fun format_error_message/1).

-doc "Encode an error response with captured stdout and warnings using the REPL error formatter.".
-spec encode_error(term(), beamtalk_repl_protocol:protocol_msg(), binary(), [binary()]) -> binary().
encode_error(Err, Msg, Output, Warnings) ->
    beamtalk_repl_protocol:encode_error(Err, Msg, fun format_error_message/1, Output, Warnings).

-doc """
Encode an error response with captured stdout, warnings, and extra metadata fields
using the REPL error formatter.
""".
-spec encode_error(
    term(), beamtalk_repl_protocol:protocol_msg(), binary(), [binary()], map()
) -> binary().
encode_error(Err, Msg, Output, Warnings, Metadata) ->
    beamtalk_repl_protocol:encode_error(
        Err, Msg, fun format_error_message/1, Output, Warnings, Metadata
    ).

%%% Term-to-JSON Conversion

-doc "Convert an Erlang term to a JSON-encodable value.".
-spec term_to_json(term()) -> term().
term_to_json(Value) when is_integer(Value); is_boolean(Value) ->
    Value;
term_to_json(Value) when is_float(Value) ->
    %% BT-1336: Convert floats to explicit decimal strings so JSX cannot
    %% strip the ".0" from whole-number floats (e.g. 6.0 → "6").
    format_float(Value);
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
term_to_json({beamtalk_future, Pid}) when is_pid(Pid) ->
    %% BT-840: Tagged future — display based on the underlying process state.
    term_to_json_future_pid(Pid);
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
    iolist_to_binary([<<"Block/">>, integer_to_binary(Arity)]);
term_to_json(Value) when is_map(Value) ->
    %% For tagged value objects (user-defined classes), dispatch Beamtalk printString
    %% so that class overrides (e.g. TestResult) are used. Untagged maps fall back
    %% to the Erlang formatter which produces proper #{...} syntax.
    case beamtalk_runtime_api:tagged_map_class_of(Value) of
        undefined ->
            beamtalk_runtime_api:print_string(Value);
        ClassName ->
            try
                beamtalk_runtime_api:dispatch_lookup(
                    'printString', [], Value, Value, ClassName
                )
            of
                {reply, Result, _} when is_binary(Result) -> Result;
                _ -> beamtalk_runtime_api:print_string(Value)
            catch
                _:_ -> beamtalk_runtime_api:print_string(Value)
            end
    end;
term_to_json(#beamtalk_error{} = Error) ->
    iolist_to_binary(beamtalk_error:format(Error));
term_to_json(Value) when is_tuple(Value) ->
    case Value of
        {beamtalk_supervisor, _Class, _Module, _Pid} = Sup ->
            %% ADR 0094: Supervisor instances render kind-headed and positional —
            %% Supervisor(Class, pid) / DynamicSupervisor(Class, pid) by ancestry.
            %% Supervisors dispatch in-process (no gen_server:call), so a direct
            %% tuple-derived label is both safe and canonical.
            beamtalk_runtime_api:process_label(Sup);
        #beamtalk_object{class = 'Metaclass', pid = Pid} ->
            %% ADR 0036: Metaclass objects display as "ClassName class" (e.g. "Integer class").
            ClassName = beamtalk_runtime_api:class_name(Pid),
            iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
        #beamtalk_object{class = Class} = Obj ->
            case beamtalk_runtime_api:is_class_name(Class) of
                true ->
                    %% Class object — bare class name (ADR 0094).
                    beamtalk_runtime_api:class_display_name(Class);
                false ->
                    %% ADR 0094 / Critical Risk #3: live actor instance. Attempt a
                    %% printString dispatch with a short timeout so custom overrides
                    %% are honoured, and fall back to the tuple-derived
                    %% `Actor(ClassName, pid)` label (no message round-trip) on
                    %% timeout/error/dead-process. The REPL never hangs on a wedged
                    %% actor.
                    actor_label_with_fallback(Obj, Class)
            end;
        {future_timeout, {beamtalk_future, Pid}} when is_pid(Pid) ->
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Future<timeout,">>, Inner, <<">">>]);
        {future_timeout, Pid} when is_pid(Pid) ->
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Future<timeout,">>, Inner, <<">">>]);
        {future_rejected, Reason} ->
            iolist_to_binary([<<"#Future<rejected: ">>, format_rejection_reason(Reason), <<">">>]);
        _ ->
            ElementStrs = [beamtalk_runtime_api:print_string(E) || E <- tuple_to_list(Value)],
            iolist_to_binary([<<"{">>, lists:join(<<", ">>, ElementStrs), <<"}">>])
    end;
term_to_json(Value) ->
    beamtalk_transcript_stream:ensure_utf8(
        iolist_to_binary(io_lib:format("~p", [Value]))
    ).

-doc "Format a tagged future's underlying pid for display.".
-spec term_to_json_future_pid(pid()) -> binary().
term_to_json_future_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case process_info(Pid, current_function) of
                {current_function, {beamtalk_future, pending, _}} ->
                    iolist_to_binary(<<"#Future<pending>">>);
                {current_function, {beamtalk_future, resolved, _}} ->
                    iolist_to_binary(<<"#Future<resolved>">>);
                {current_function, {beamtalk_future, rejected, _}} ->
                    iolist_to_binary(<<"#Future<rejected>">>);
                _ ->
                    iolist_to_binary(<<"#Future<unknown>">>)
            end;
        false ->
            iolist_to_binary(<<"#Future<completed>">>)
    end.

-doc """
Render a live actor instance for REPL display (ADR 0094, Critical Risk #3).

Attempts a `printString` dispatch with a short timeout so a custom override is
honoured when the actor is responsive, and falls back to the tuple-derived
`Actor(ClassName, pid)` label (read directly from the `#beamtalk_object{}`
tuple, no message round-trip) on timeout / error / dead-process. The REPL
therefore never hangs on a wedged actor and never shows stale state.
""".
-spec actor_label_with_fallback(#beamtalk_object{}, atom()) -> binary().
actor_label_with_fallback(#beamtalk_object{pid = Pid} = Obj, _Class) when is_pid(Pid) ->
    %% is_process_alive/1 raises badarg for remote pids, so probe defensively —
    %% a non-local or otherwise unreachable actor degrades to the tuple label.
    case is_local_process_alive(Pid) of
        false ->
            %% Dead/unreachable actor — never message it; use the tuple-derived label.
            beamtalk_runtime_api:process_label(Obj);
        true ->
            try
                beamtalk_runtime_api:message_send(Obj, 'printString', [], ?ACTOR_PRINT_TIMEOUT_MS)
            of
                Result when is_binary(Result) ->
                    Result;
                _ ->
                    %% Non-binary override result — fall back to the canonical label.
                    beamtalk_runtime_api:process_label(Obj)
            catch
                _:_ ->
                    %% Timeout / error / mid-call — degrade to the tuple-derived label.
                    beamtalk_runtime_api:process_label(Obj)
            end
    end;
actor_label_with_fallback(Obj, _Class) ->
    %% Name-resolving proxy ({registered, Name}) or malformed identity slot:
    %% no live pid to message, so use the tuple-derived label directly.
    beamtalk_runtime_api:process_label(Obj).

-doc "Liveness probe that treats remote/unreachable pids as not alive (never raises).".
-spec is_local_process_alive(pid()) -> boolean().
is_local_process_alive(Pid) ->
    try
        is_process_alive(Pid)
    catch
        _:_ -> false
    end.

%%% Error Formatting

-doc "Format an error reason as a human-readable message.".
-spec format_error_message(term()) -> binary().
format_error_message(#{'$beamtalk_class' := Class, error := Error}) ->
    Enriched = maybe_use_singleton_binding_name(Error),
    ClassName = atom_to_binary(Class, utf8),
    iolist_to_binary([ClassName, <<": ">>, beamtalk_error:format(Enriched)]);
format_error_message(#beamtalk_error{} = Error) ->
    Enriched = maybe_use_singleton_binding_name(Error),
    iolist_to_binary(beamtalk_error:format(Enriched));
format_error_message(empty_expression) ->
    <<"Empty expression">>;
format_error_message(timeout) ->
    <<"Request timed out">>;
format_error_message({compile_error, [#{message := Msg} | _]}) ->
    %% BT-1235: structured diagnostic list — use the first diagnostic's message
    Msg;
format_error_message({compile_error, Msg}) when is_binary(Msg) ->
    Msg;
format_error_message({compile_error, Msg}) when is_list(Msg) ->
    try
        list_to_binary(Msg)
    catch
        error:badarg -> iolist_to_binary(io_lib:format("~p", [Msg]))
    end;
format_error_message({undefined_variable, Name}) ->
    iolist_to_binary([<<"Undefined variable: ">>, beamtalk_repl_errors:format_name(Name)]);
format_error_message({invalid_request, Reason}) ->
    iolist_to_binary([<<"Invalid request: ">>, beamtalk_repl_errors:format_name(Reason)]);
format_error_message({parse_error, Details}) ->
    iolist_to_binary([<<"Parse error: ">>, beamtalk_repl_errors:format_name(Details)]);
format_error_message({eval_error, _Class, #{'$beamtalk_class' := ExClass, error := Error}}) ->
    Enriched = maybe_use_singleton_binding_name(Error),
    ClassName = atom_to_binary(ExClass, utf8),
    iolist_to_binary([ClassName, <<": ">>, beamtalk_error:format(Enriched)]);
format_error_message({eval_error, _Class, #beamtalk_error{} = Error}) ->
    Enriched = maybe_use_singleton_binding_name(Error),
    iolist_to_binary(beamtalk_error:format(Enriched));
format_error_message({eval_error, Class, Reason}) ->
    iolist_to_binary([
        <<"Evaluation error: ">>,
        atom_to_binary(Class, utf8),
        <<":">>,
        beamtalk_repl_errors:format_name(Reason)
    ]);
format_error_message({load_error, Reason}) ->
    iolist_to_binary([<<"Failed to load bytecode: ">>, beamtalk_repl_errors:format_name(Reason)]);
format_error_message({file_not_found, Path}) ->
    iolist_to_binary([<<"File not found: ">>, beamtalk_repl_errors:format_name(Path)]);
format_error_message({read_error, Reason}) ->
    iolist_to_binary([<<"Failed to read file: ">>, beamtalk_repl_errors:format_name(Reason)]);
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
        <<". Use Workspace classes to see loaded classes.">>
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
        beamtalk_repl_errors:format_name(Module),
        <<". Use :load <path> to load it first.">>
    ]);
format_error_message({missing_module_name, reload}) ->
    <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>;
format_error_message({session_creation_failed, Reason}) ->
    iolist_to_binary([<<"Failed to create session: ">>, beamtalk_repl_errors:format_name(Reason)]);
format_error_message(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%% Internal Helpers

-doc """
Format a float as a binary string, ensuring a decimal point is always present.
BT-1336: Uses ~p for clean representation, then appends ".0" if ~p omits the decimal.
""".
-spec format_float(float()) -> binary().
format_float(Value) ->
    Bin = iolist_to_binary(io_lib:format("~p", [Value])),
    case binary:match(Bin, <<".">>) of
        nomatch -> <<Bin/binary, ".0">>;
        _ -> Bin
    end.

-doc "Format a rejection reason for display in #Future<rejected: ...>.".
-spec format_rejection_reason(term()) -> binary().
format_rejection_reason(#beamtalk_error{} = Error) ->
    beamtalk_error:format(Error);
format_rejection_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

-doc "Convert atom or binary to binary.".
-spec to_binary(atom() | binary()) -> binary().
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_binary(V) -> V.

-doc """
Rewrite DNU error class names for singleton instances.

When a singleton instance (e.g., Workspace) gets a DNU, the error uses the
class name (WorkspaceInterface) which is confusing — the user typed "Workspace".
Replace the class name with the binding name so errors read naturally.
""".
-spec maybe_use_singleton_binding_name(beamtalk_error:error()) -> beamtalk_error:error().
maybe_use_singleton_binding_name(
    #beamtalk_error{kind = does_not_understand, class = Class} = Error
) when is_atom(Class) ->
    case beamtalk_workspace_config:binding_name_for_class(Class) of
        {ok, BindingName} ->
            %% Regenerate the message with the binding name.
            NewMessage = beamtalk_error:generate_message(
                does_not_understand, BindingName, Error#beamtalk_error.selector
            ),
            Error#beamtalk_error{message = NewMessage};
        undefined ->
            Error
    end;
maybe_use_singleton_binding_name(Error) ->
    Error.
