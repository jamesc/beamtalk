%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_port).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
OTP Port interface to the Rust compiler binary (ADR 0022, Phase 0).

Spawns `beamtalk-compiler-port` as an OTP port with `{packet, 4}' framing
and ETF-encoded requests/responses.

Phase 0 is a wire check — no supervisor, no backend dispatch. Manual
verification that BEAM can invoke the Rust compiler via a port.
""".

-export([
    open/0, open/1,
    compile_expression/4, compile_expression/5,
    compile_expression_trace/4, compile_expression_trace/5,
    resolve_completion_type/3,
    find_senders_in_source/3,
    find_all_sends_in_source/2,
    find_references_to_in_source/3,
    find_field_readers_in_source/3,
    find_field_writers_in_source/3,
    find_ffi_sites_in_source/5,
    close/1
]).

-ifdef(TEST).
-export([
    handle_response/1,
    find_compiler_binary/0,
    find_project_root/0
]).
-endif.

-include_lib("kernel/include/logger.hrl").

-doc """
Open a port to the compiler binary.
Finds the binary relative to the project root or via PATH.
""".
-spec open() -> port().
open() ->
    open(find_compiler_binary()).

-doc "Open a port to the compiler binary at the given path.".
-spec open(file:filename_all()) -> port().
open(BinaryPath) ->
    ?LOG_INFO("Opening compiler port", #{domain => [beamtalk, runtime], binary => BinaryPath}),
    open_port({spawn_executable, BinaryPath}, [
        {packet, 4},
        binary,
        exit_status,
        use_stdio
    ]).

-doc """
Compile a REPL expression through the port.

Sends an ETF-encoded request and receives an ETF-encoded response.
Returns `{ok, CoreErlang, Warnings}' on success,
`{ok, class_definition, ClassInfo}' for inline class definitions (BT-571),
`{ok, method_definition, MethodInfo}' for standalone method definitions (BT-571),
`{ok, protocol_definition, ProtocolInfo}' for protocol definitions (BT-1612),
or `{error, Diagnostics}' on failure, where each diagnostic is a map with
`message', `line' (1-based), and optionally `hint'.
""".
-spec compile_expression(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {error, [map()]}.
compile_expression(Port, Source, ModuleName, KnownVars) ->
    compile_expression(Port, Source, ModuleName, KnownVars, #{}).

-doc """
Compile a REPL expression with optional compilation options.

Options:
  class_superclass_index => #{binary() => binary()} — cross-file superclass info
  class_module_index => #{binary() => binary()} — cross-directory module name mapping

When provided, these indexes are forwarded to the compiler port so that
inline class definitions correctly resolve inherited types and module names
from already-loaded files.
""".
-spec compile_expression(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {error, [map()]}.
compile_expression(Port, Source, ModuleName, KnownVars, Options) ->
    SuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    ModuleIndex = maps:get(class_module_index, Options, #{}),
    Request0 = #{
        command => compile_expression,
        source => Source,
        module => ModuleName,
        known_vars => KnownVars
    },
    %% BT-907: Include superclass index only when non-empty to keep the
    %% protocol backward-compatible with older port binaries.
    Request1 =
        case map_size(SuperclassIndex) of
            0 -> Request0;
            _ -> Request0#{class_superclass_index => SuperclassIndex}
        end,
    %% Include module index for correct cross-directory class references.
    Request2 =
        case map_size(ModuleIndex) of
            0 -> Request1;
            _ -> Request1#{class_module_index => ModuleIndex}
        end,
    %% ADR 0050 Phase 4: Forward class hierarchy to the Rust compiler port.
    ClassHierarchy = maps:get(class_hierarchy, Options, #{}),
    Request3 =
        case map_size(ClassHierarchy) of
            0 -> Request2;
            _ -> Request2#{class_hierarchy => ClassHierarchy}
        end,
    %% BT-1670: Forward module_name override for inline class definitions
    %% so package-mode produces consistent module names across all paths.
    ModuleNameOverride = maps:get(module_name, Options, undefined),
    Request =
        case ModuleNameOverride of
            undefined -> Request3;
            _ -> Request3#{module_name => ModuleNameOverride}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response ->
                            handle_response(Response)
                    catch
                        error:badarg:Stack ->
                            ?LOG_ERROR("Compiler port decode error", #{
                                domain => [beamtalk, runtime],
                                port => Port,
                                response_size => byte_size(ResponseBin),
                                stacktrace => Stack
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout", #{domain => [beamtalk, runtime], port => Port}),
                %% Close the port so any late response cannot poison the next request.
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg:Stack ->
            ?LOG_ERROR("Compiler port not available", #{
                domain => [beamtalk, runtime], port => Port, stacktrace => Stack
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end.

-doc """
Compile a REPL expression in trace mode (BT-1238).

Same request format as `compile_expression/4' but sends the
`compile_expression_trace' command.  The returned Core Erlang module's
`eval/1' returns `{[{<<"src0">>, Val0}, ...], FinalState}' instead of
`{Result, FinalState}'.
""".
-spec compile_expression_trace(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Port, Source, ModuleName, KnownVars) ->
    compile_expression_trace(Port, Source, ModuleName, KnownVars, #{}).

-doc "Compile in trace mode with optional compilation options.".
-spec compile_expression_trace(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]} | {error, [map()]}.
compile_expression_trace(Port, Source, ModuleName, KnownVars, Options) ->
    SuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    ModuleIndex = maps:get(class_module_index, Options, #{}),
    ClassHierarchy = maps:get(class_hierarchy, Options, #{}),
    Request0 = #{
        command => compile_expression_trace,
        source => Source,
        module => ModuleName,
        known_vars => KnownVars
    },
    Request1 =
        case map_size(SuperclassIndex) of
            0 -> Request0;
            _ -> Request0#{class_superclass_index => SuperclassIndex}
        end,
    Request2 =
        case map_size(ModuleIndex) of
            0 -> Request1;
            _ -> Request1#{class_module_index => ModuleIndex}
        end,
    Request =
        case map_size(ClassHierarchy) of
            0 -> Request2;
            _ -> Request2#{class_hierarchy => ClassHierarchy}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (trace)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during trace compile", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (trace)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (trace)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end.

-doc """
Resolve the type of an expression for REPL completion fallback (BT-1068).

Sends an ETF-encoded `resolve_completion_type' request and returns
`{ok, ClassName}' when the type is statically known, or
`{error, type_unknown}' when the type cannot be determined.

`ClassHierarchy' is the accumulated class cache map from
`beamtalk_compiler_server' (ADR 0050 Phase 4).
""".
-spec resolve_completion_type(port(), binary(), #{atom() => map()}) ->
    {ok, atom()} | {error, type_unknown}.
resolve_completion_type(Port, Expression, ClassHierarchy) ->
    Request0 = #{
        command => resolve_completion_type,
        expression => Expression
    },
    Request =
        case map_size(ClassHierarchy) of
            0 -> Request0;
            _ -> Request0#{class_hierarchy => ClassHierarchy}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response ->
                            handle_resolve_response(Response)
                    catch
                        error:badarg ->
                            {error, type_unknown}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during completion type resolution", #{
                        domain => [beamtalk, runtime],
                        status => Status
                    }),
                    {error, type_unknown}
            after 5000 ->
                %% Use a shorter timeout for completion — latency budget per ADR 0045.
                ?LOG_ERROR("Compiler port timeout during completion type resolution", #{
                    domain => [beamtalk, runtime],
                    port => Port
                }),
                %% Close the port so any late response cannot poison the next request.
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, type_unknown}
            end
    catch
        error:badarg ->
            {error, type_unknown}
    end.

-doc """
Find call sites of a selector in a single method's source (BT-2190).

Sends an ETF-encoded `find_senders_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each
line is a 1-based line number relative to `Source'.

Used by `SystemNavigation sendersOf:' via `beamtalk_interface' to power
System Browser-style "who calls this method?" navigation.
""".
-spec find_senders_in_source(port(), binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_senders_in_source(Port, Source, Selector) when
    is_atom(Selector) orelse is_binary(Selector)
->
    SelectorBin =
        case Selector of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Request = #{
        command => find_senders_in_source,
        source => Source,
        selector => SelectorBin
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_senders_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (senders)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during senders query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (senders)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (senders)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
find_senders_in_source(_Port, _Source, _Selector) ->
    {error, [
        #{
            message =>
                <<"find_senders_in_source: selector must be an atom or binary">>
        }
    ]}.

-doc """
Find every message send within a single method's source (BT-2206).

Single-pass companion to `find_senders_in_source/3': instead of filtering by
one known selector, returns EVERY send. Sends an ETF-encoded
`find_all_sends_in_source' request and returns `{ok, [Send]}' on success or
`{error, [Diagnostic]}' on failure. Each `Send' is a map
`#{selector := binary(), line := pos_integer(), recv := self | super | erlang_ffi | other}'.

Used by `SystemNavigation unimplementedSelectors' via `beamtalk_interface' to
compute `allSentSelectors − allDefinedSelectors' (the classic typo-finder).
""".
-spec find_all_sends_in_source(port(), binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_all_sends_in_source(Port, Source) when is_binary(Source) ->
    Request = #{
        command => find_all_sends_in_source,
        source => Source
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_all_sends_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (all sends)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during all-sends query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (all sends)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (all sends)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
find_all_sends_in_source(_Port, _Source) ->
    {error, [
        #{
            message =>
                <<"find_all_sends_in_source: source must be a binary">>
        }
    ]}.

-doc """
Find references to a class within a single method's source (BT-2203).

Sends an ETF-encoded `find_references_to_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each
line is a 1-based line number relative to `Source'.

Used by `SystemNavigation referencesTo:' via `beamtalk_interface' to power
System Browser-style "who mentions this class?" navigation. Mirrors
`find_senders_in_source/3' (BT-2190) but the visitor matches `ClassReference'
AST nodes (and class names in type annotations) instead of `MessageSend' nodes.
""".
-spec find_references_to_in_source(port(), binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_references_to_in_source(Port, Source, ClassName) when
    is_atom(ClassName) orelse is_binary(ClassName)
->
    ClassNameBin =
        case ClassName of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Request = #{
        command => find_references_to_in_source,
        source => Source,
        class_name => ClassNameBin
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_references_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (references)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during references query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (references)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (references)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
find_references_to_in_source(_Port, _Source, _ClassName) ->
    {error, [
        #{
            message =>
                <<"find_references_to_in_source: class name must be an atom or binary">>
        }
    ]}.

-doc """
Find reads of an field in a single method's source (BT-2208).

Sends an ETF-encoded `find_field_readers_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each line is
a 1-based line number relative to `Source' where the named slot is read
(`self.x' outside an assignment target).

Used by `SystemNavigation fieldReadersOf:in:' via `beamtalk_interface' to
power System Browser-style "which methods read this slot?" navigation.
""".
-spec find_field_readers_in_source(port(), binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_readers_in_source(Port, Source, Field) ->
    field_access_query(Port, find_field_readers_in_source, Source, Field, <<"field readers">>).

-doc """
Find writes of an field in a single method's source (BT-2208).

Sends an ETF-encoded `find_field_writers_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each line is
a 1-based line number relative to `Source' where the named slot is written
(`self.x := ...', the assignment target).

Used by `SystemNavigation fieldWritersOf:in:' via `beamtalk_interface' to
power System Browser-style "which methods write this slot?" navigation.
""".
-spec find_field_writers_in_source(port(), binary(), atom() | binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_field_writers_in_source(Port, Source, Field) ->
    field_access_query(Port, find_field_writers_in_source, Source, Field, <<"field writers">>).

-doc """
Shared driver for the field reader/writer queries (BT-2208).

Both queries take a `Source' binary and an field name and return a
list of 1-based line numbers, so they share the request/response plumbing.
`Command' selects the port command atom; `Label' is used only in log messages.
""".
-spec field_access_query(port(), atom(), binary(), atom() | binary(), binary()) ->
    {ok, [pos_integer()]} | {error, [map()]}.
field_access_query(Port, Command, Source, Field, Label) when
    is_atom(Field) orelse is_binary(Field)
->
    IVarBin =
        case Field of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Request = #{
        command => Command,
        source => Source,
        field => IVarBin
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_field_response(Response, Label)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (~s)", [Label], #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during ~s query", [Label], #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (~s)", [Label], #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (~s)", [Label], #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
field_access_query(_Port, Command, _Source, _IVar, _Label) ->
    {error, [
        #{
            message =>
                iolist_to_binary([
                    atom_to_binary(Command, utf8),
                    <<": field name must be an atom or binary">>
                ])
        }
    ]}.

-doc """
Find Erlang FFI call sites in a single method's source (BT-2211).

Sends an ETF-encoded `find_ffi_sites_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each line is
a 1-based line number relative to `Source' where the named Erlang function
(`Module':`Function', optionally constrained to `Arity') is invoked through the
`Erlang' FFI bridge. `Arity' is either a non-negative integer (match only that
argument count) or the atom `any' (match any arity).

Used by `SystemNavigation ffiSitesFor:' via `beamtalk_interface' to power
System Browser-style "who calls this Erlang function?" navigation.
""".
-spec find_ffi_sites_in_source(
    port(), binary(), atom() | binary(), atom() | binary(), non_neg_integer() | any
) ->
    {ok, [pos_integer()]} | {error, [map()]}.
find_ffi_sites_in_source(Port, Source, Module, Function, Arity) when
    is_binary(Source),
    (is_atom(Module) orelse is_binary(Module)),
    (is_atom(Function) orelse is_binary(Function)),
    (Arity =:= any orelse (is_integer(Arity) andalso Arity >= 0))
->
    ModuleBin = to_binary(Module),
    FunctionBin = to_binary(Function),
    BaseRequest = #{
        command => find_ffi_sites_in_source,
        source => Source,
        module => ModuleBin,
        function => FunctionBin
    },
    %% `any' means "match any arity" — omit the field entirely so the Rust side
    %% sees it as absent. A non-negative integer constrains the match.
    Request =
        case Arity of
            any -> BaseRequest;
            N when is_integer(N), N >= 0 -> BaseRequest#{arity => N}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_ffi_sites_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (ffi sites)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during ffi-sites query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (ffi sites)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (ffi sites)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
find_ffi_sites_in_source(_Port, _Source, _Module, _Function, _Arity) ->
    {error, [
        #{
            message =>
                <<
                    "find_ffi_sites_in_source: source must be a binary, "
                    "module/function must be atoms or binaries, and arity "
                    "must be any or a non-negative integer"
                >>
        }
    ]}.

%% Normalise an atom-or-binary identifier to a binary.
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.

-doc "Close the compiler port.".
-spec close(port()) -> true.
close(Port) ->
    port_close(Port).

%%% Internal functions

-doc """
Handle ETF response from the compiler port.
BT-571: Extended to handle class_definition and method_definition responses.
BT-1235: Diagnostics in error responses are maps with `message', `line', and optional `hint'.
""".
-spec handle_response(map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {error, [map()]}.
handle_response(
    #{
        status := ok,
        kind := class_definition,
        core_erlang := CoreErlang,
        module_name := ModuleName,
        classes := Classes,
        warnings := Warnings
    } = Response
) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    BaseInfo = #{
        core_erlang => PrettyCore,
        module_name => ModuleName,
        classes => Classes,
        warnings => Warnings
    },
    %% BT-903: Forward trailing_core_erlang when present (inline class + trailing expressions)
    ClassInfo =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(trailing_core_erlang, Response) of
            {ok, TrailingCoreErlang} ->
                BaseInfo#{trailing_core_erlang => TrailingCoreErlang};
            error ->
                BaseInfo
        end,
    {ok, class_definition, ClassInfo};
handle_response(#{
    status := ok,
    kind := method_definition,
    class_name := ClassName,
    selector := Selector,
    is_class_method := IsClassMethod,
    method_source := MethodSource,
    warnings := Warnings
}) ->
    {ok, method_definition, #{
        class_name => ClassName,
        selector => Selector,
        is_class_method => IsClassMethod,
        method_source => MethodSource,
        warnings => Warnings
    }};
%% BT-1612: Protocol definition response
handle_response(#{
    status := ok,
    kind := protocol_definition,
    core_erlang := CoreErlang,
    module_name := ModuleName,
    protocols := Protocols,
    warnings := Warnings
}) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    {ok, protocol_definition, #{
        core_erlang => PrettyCore,
        module_name => ModuleName,
        protocols => Protocols,
        warnings => Warnings
    }};
handle_response(#{status := ok, core_erlang := CoreErlang, warnings := Warnings}) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    {ok, PrettyCore, Warnings};
handle_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_response(Other) ->
    ?LOG_ERROR("Unexpected compiler response", #{domain => [beamtalk, runtime], response => Other}),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Handle ETF response from a resolve_completion_type request.
Returns `{ok, ClassName}' when the class name is a known existing atom,
or `{error, type_unknown}' for not-found or malformed responses.
""".
-spec handle_resolve_response(map()) -> {ok, atom()} | {error, type_unknown}.
handle_resolve_response(#{status := ok, class_name := ClassName}) when is_binary(ClassName) ->
    try binary_to_existing_atom(ClassName, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, type_unknown}
    end;
handle_resolve_response(_) ->
    {error, type_unknown}.

-doc """
Handle ETF response from a find_senders_in_source request (BT-2190).
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_senders_response(map()) -> {ok, [non_neg_integer()]} | {error, [map()]}.
handle_senders_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_senders_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_senders_response(Other) ->
    ?LOG_ERROR("Unexpected senders response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Handle ETF response from a find_all_sends_in_source request (BT-2206).
Returns `{ok, [Send]}' on success (each `Send' a
`#{selector := binary(), line := pos_integer(), recv := atom()}' map, passed
through unchanged), `{error, [Diagnostic]}' on failure.
""".
-spec handle_all_sends_response(map()) -> {ok, [map()]} | {error, [map()]}.
handle_all_sends_response(#{status := ok, sends := Sends}) when is_list(Sends) ->
    {ok, Sends};
handle_all_sends_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_all_sends_response(Other) ->
    ?LOG_ERROR("Unexpected all-sends response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Handle ETF response from a find_references_to_in_source request (BT-2203).
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_references_response(map()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_references_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_references_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_references_response(Other) ->
    ?LOG_ERROR("Unexpected references response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Handle ETF response from a find_field_readers/writers_in_source request
(BT-2208). Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on
failure. `Label' is used only to disambiguate the unexpected-response log line.
""".
-spec handle_field_response(map(), binary()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_field_response(#{status := ok, lines := Lines}, _Label) when is_list(Lines) ->
    {ok, Lines};
handle_field_response(#{status := error, diagnostics := Diagnostics}, _Label) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_field_response(Other, Label) ->
    ?LOG_ERROR("Unexpected ~s response", [Label], #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Handle ETF response from a find_ffi_sites_in_source request (BT-2211).
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_ffi_sites_response(map()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_ffi_sites_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_ffi_sites_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_ffi_sites_response(Other) ->
    ?LOG_ERROR("Unexpected ffi-sites response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

-doc """
Normalize a list of diagnostics to a uniform list of maps.
BT-1235: The port now returns maps with `message', `line', `hint'.
Plain binaries (legacy/protocol errors) are wrapped as `#{message => Bin}'.
""".
-spec normalize_diagnostics([term()]) -> [map()].
normalize_diagnostics(Diagnostics) when is_list(Diagnostics) ->
    [normalize_diagnostic(D) || D <- Diagnostics].

-spec normalize_diagnostic(term()) -> map().
normalize_diagnostic(D) when is_map(D) ->
    %% Ensure `message` is present and binary
    Msg0 = maps:get(message, D, <<"Unknown diagnostic">>),
    MsgBin = ensure_binary(Msg0),
    D#{message := MsgBin};
normalize_diagnostic(D) when is_binary(D) -> #{message => D};
normalize_diagnostic(D) ->
    #{message => iolist_to_binary(io_lib:format("~p", [D]))}.

-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

%% Try to pretty-print textual Core Erlang using Erlang's core parser/pretty-printer.
%% Falls back to the original Core Erlang text on any failure.
-spec maybe_pretty_core(binary()) -> binary().
maybe_pretty_core(CoreErlang) when is_binary(CoreErlang) ->
    try
        CoreStr = binary_to_list(CoreErlang),
        {ok, Tokens, _} = core_scan:string(CoreStr),
        {ok, CoreModule} = core_parse:parse(Tokens),
        Formatted = core_pp:format(CoreModule),
        %% Halve indentation from 4-space to 2-space and return binary.
        halve_indent(iolist_to_binary(Formatted))
    catch
        _:_ -> CoreErlang
    end;
maybe_pretty_core(Other) when not is_binary(Other) -> erlang:error(badarg).

-doc "Reduce Core Erlang indentation from 4 spaces per level to 2 spaces per level.".
-spec halve_indent(binary()) -> binary().
halve_indent(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Normalized = [halve_leading_spaces(Line) || Line <- Lines],
    iolist_to_binary(lists:join(<<"\n">>, Normalized)).

-spec halve_leading_spaces(binary()) -> binary().
halve_leading_spaces(Line) ->
    {N, Rest} = count_leading_spaces(Line, 0),
    Indent = binary:copy(<<" ">>, N div 2),
    <<Indent/binary, Rest/binary>>.

-spec count_leading_spaces(binary(), non_neg_integer()) -> {non_neg_integer(), binary()}.
count_leading_spaces(<<" ", Rest/binary>>, N) -> count_leading_spaces(Rest, N + 1);
count_leading_spaces(Rest, N) -> {N, Rest}.

-doc """
Find the compiler binary.
Looks for the binary in the cargo target directory first (development),
then falls back to PATH.
""".
find_compiler_binary() ->
    %% 1. Check explicit env var (set by CLI for installed mode)
    case os:getenv("BEAMTALK_COMPILER_PORT_BIN") of
        false ->
            find_compiler_binary_dev();
        "" ->
            find_compiler_binary_dev();
        EnvPath ->
            case filelib:is_regular(EnvPath) of
                true -> EnvPath;
                false -> find_compiler_binary_dev()
            end
    end.

find_compiler_binary_dev() ->
    %% Try cargo target directory (development mode)
    ProjectRoot = find_project_root(),

    %% On Windows, executables have .exe extension
    ExeName =
        case os:type() of
            {win32, _} -> "beamtalk-compiler-port.exe";
            _ -> "beamtalk-compiler-port"
        end,

    DevPath = filename:join([ProjectRoot, "target", "debug", ExeName]),
    case filelib:is_regular(DevPath) of
        true ->
            DevPath;
        false ->
            %% Try release build
            ReleasePath = filename:join([ProjectRoot, "target", "release", ExeName]),
            case filelib:is_regular(ReleasePath) of
                true ->
                    ReleasePath;
                false ->
                    %% Fall back to PATH
                    case os:find_executable("beamtalk-compiler-port") of
                        false ->
                            error({compiler_not_found, "beamtalk-compiler-port binary not found"});
                        Path ->
                            Path
                    end
            end
    end.

-doc "Find the project root by looking for Cargo.toml.".
find_project_root() ->
    Cwd = filename:absname(""),
    find_project_root(Cwd).

find_project_root(Dir) ->
    %% Check if we've reached the filesystem root (portable: works on "/" and "C:\")
    case filename:dirname(Dir) of
        Dir ->
            %% dirname(Dir) == Dir means we're at the root — fallback to cwd
            filename:absname("");
        Parent ->
            case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
                true -> Dir;
                false -> find_project_root(Parent)
            end
    end.
