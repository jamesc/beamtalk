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
    find_announce_sites_in_source/2,
    resolve_method_span/5,
    resolve_class_span/3,
    categorize_methods/3,
    class_state_field_defaults/3,
    reindent_method_source/3,
    close/1
]).

-ifdef(TEST).
-export([
    handle_response/1,
    find_compiler_binary/0,
    find_project_root/0,
    to_binary/1
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
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Port, Source, ModuleName, KnownVars) ->
    compile_expression(Port, Source, ModuleName, KnownVars, #{}).

-doc """
Compile a REPL expression with optional compilation options.

Options:
  class_superclass_index => #{binary() => binary()} — cross-file superclass info
  class_module_index => #{binary() => binary()} — cross-directory module name mapping
  known_type_aliases => [binary()] — ADR 0108 Phase 8 (BT-2902): reparseable
    `type Name = <expansion>` lines for aliases declared in earlier turns of
    this REPL session, so `::` annotations in the current turn resolve them

When provided, these indexes are forwarded to the compiler port so that
inline class definitions correctly resolve inherited types and module names
from already-loaded files.
""".
-spec compile_expression(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {ok, type_alias_definition, map()}
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
    Request4 =
        case ModuleNameOverride of
            undefined -> Request3;
            _ -> Request3#{module_name => ModuleNameOverride}
        end,
    %% ADR 0108 Phase 8 (BT-2902): forward earlier-turn alias declarations.
    KnownTypeAliases = maps:get(known_type_aliases, Options, []),
    Request =
        case KnownTypeAliases of
            [] -> Request4;
            _ -> Request4#{known_type_aliases => KnownTypeAliases}
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
    Request3 =
        case map_size(ClassHierarchy) of
            0 -> Request2;
            _ -> Request2#{class_hierarchy => ClassHierarchy}
        end,
    %% ADR 0108 Phase 8 (BT-2902), BT-2956: forward earlier-turn/ambient alias
    %% declarations, mirroring `compile_expression/5` above — without this,
    %% `::` annotations in traced expressions can never resolve an alias
    %% declared in an earlier REPL turn.
    KnownTypeAliases = maps:get(known_type_aliases, Options, []),
    Request =
        case KnownTypeAliases of
            [] -> Request3;
            _ -> Request3#{known_type_aliases => KnownTypeAliases}
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
Find every `announce:' emission within a single method's source (BT-2475).

Backs `SystemNavigation announcementsSentBy:' — the static dual of
`AnnouncementNavigation'. Sends an ETF-encoded `find_announce_sites_in_source'
request and returns `{ok, [Site]}' on success or `{error, [Diagnostic]}' on
failure. Each `Site' is a map `#{selector := binary(), line := pos_integer(),
announcement_class := binary()}', where `announcement_class' is the
syntactically-resolved announcement class name (empty binary `<<>>' when the
event argument is unresolvable). The maps are passed through unchanged.
""".
-spec find_announce_sites_in_source(port(), binary()) ->
    {ok, [map()]} | {error, [map()]}.
find_announce_sites_in_source(Port, Source) when is_binary(Source) ->
    Request = #{
        command => find_announce_sites_in_source,
        source => Source
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_announce_sites_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (announce sites)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during announce-sites query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (announce sites)", #{
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
            ?LOG_ERROR("Compiler port not available (announce sites)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end;
find_announce_sites_in_source(_Port, _Source) ->
    {error, [
        #{
            message =>
                <<"find_announce_sites_in_source: source must be a binary">>
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

-doc """
Resolve the byte span of a method definition in `Source' (ADR 0082 Phase 1).

Given the current on-disk source of a `.bt' file and a target
`(ClassName, Selector, Side)', returns the exact byte span of that method's
definition plus the bytes currently occupying it (`prev_source'). The
live-patch install hook uses both to record a flushable ChangeEntry:
`span'/`prev_source' let a later `Workspace flush' splice the patched body back
into the file by byte-span replacement, and let restart detect disk drift.

`Side' is `instance' or `class'. Returns
`{ok, #{start := S, end := E}, PrevSource}' on success. Resolution failures
(class not found, selector not found, ambiguous) come back as
`{error, Reason, Message}' with `Reason' an atom — the hook downgrades to a
memory-only patch rather than failing the install. Transport failures (port
down, timeout) return `{error, port_error, Message}'.
""".
-spec resolve_method_span(port(), binary(), atom() | binary(), atom() | binary(), instance | class) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
resolve_method_span(Port, Source, ClassName, Selector, Side) when
    is_binary(Source),
    (is_atom(ClassName) orelse is_binary(ClassName)),
    (is_atom(Selector) orelse is_binary(Selector)),
    (Side =:= instance orelse Side =:= class)
->
    Request = #{
        command => resolve_method_span,
        source => Source,
        class_name => to_binary(ClassName),
        selector => to_binary(Selector),
        side => Side
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_method_span_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (method span)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, port_error, <<"Compiler port response is malformed">>}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during method-span query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, port_error, <<"Compiler port exited unexpectedly">>}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (method span)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, port_error, <<"Compiler port timed out">>}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (method span)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, port_error, <<"Compiler port is not available">>}
    end;
resolve_method_span(_Port, _Source, _ClassName, _Selector, _Side) ->
    {error, bad_argument, <<
        "resolve_method_span: source/class/selector must be binary or atom, "
        "side instance or class"
    >>}.

-spec handle_method_span_response(map()) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
handle_method_span_response(#{
    status := ok, span := #{start := Start, 'end' := End}, prev_source := PrevSource
}) when is_integer(Start), is_integer(End), is_binary(PrevSource) ->
    {ok, #{start => Start, 'end' => End}, PrevSource};
handle_method_span_response(#{status := error, reason := Reason} = Resp) ->
    Message = maps:get(message, Resp, atom_to_binary(Reason, utf8)),
    {error, Reason, Message};
handle_method_span_response(Other) ->
    ?LOG_ERROR("Unexpected method-span response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, port_error, <<"Unexpected compiler response">>}.

-doc """
Resolve the byte span of a class's header + state declarations in `Source'
(ADR 0082 extension, BT-3248) — never its methods.

Given the current on-disk source of a `.bt' file and a target `ClassName',
returns the byte span of that class's declaration line through its last
`state:'/`field:' declaration, plus the bytes currently occupying it
(`prev_source'). Backs the CHANGES dock's disk-vs-memory diff for a
`'class-def'' entry (the cockpit `:def' tab's redefinition of an *existing*
class) — see `beamtalk_core::source_analysis::resolve_class_span''s module
doc for why the span must stop before any method.

Returns `{ok, #{start := S, end := E}, PrevSource}' on success. Resolution
failures (class not found, ambiguous) come back as `{error, Reason, Message}'
with `Reason' an atom — the hook downgrades to a memory-only patch rather than
failing the install. Transport failures (port down, timeout) return
`{error, port_error, Message}'.
""".
-spec resolve_class_span(port(), binary(), atom() | binary()) ->
    {ok, #{start := non_neg_integer(), 'end' := non_neg_integer()}, binary()}
    | {error, atom(), binary()}.
resolve_class_span(Port, Source, ClassName) when
    is_binary(Source), (is_atom(ClassName) orelse is_binary(ClassName))
->
    Request = #{
        command => resolve_class_span,
        source => Source,
        class_name => to_binary(ClassName)
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_method_span_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (class span)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, port_error, <<"Compiler port response is malformed">>}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during class-span query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, port_error, <<"Compiler port exited unexpectedly">>}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (class span)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, port_error, <<"Compiler port timed out">>}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (class span)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, port_error, <<"Compiler port is not available">>}
    end;
resolve_class_span(_Port, _Source, _ClassName) ->
    {error, bad_argument, <<"resolve_class_span: source/class must be binary or atom">>}.

-doc """
Group a class's methods by its `// === Name ===' section dividers (BT-3239,
extended by BT-3238).

Given the current on-disk source of a `.bt' file and a target `ClassName',
returns the class's methods grouped by the divider comments that precede
them, in source order — the same recognition rules
`beamtalk_core::source_analysis::categorize_methods_in_source' locks down
for every surface (see that module's doc). This is the bridge that lets
Erlang surfaces reach it without a second, drift-prone implementation of
the divider grammar (CLAUDE.md's "No duplicate implementations" rule):
`beamtalk_interface:format_class_help/2' (BT-3239, REPL/MCP `:help'/`docs')
and the Cockpit System Browser's grouped method view + `save-section'
(BT-3238).

Returns `{ok, Categories}' on success, where each category is `#{name :=
binary() | undefined, divider_span := #{start := S, 'end' := E} |
undefined, methods := [#{selector := binary(), side := instance | class,
span := #{start := S, 'end' := E}}]}'. `name'/`divider_span' are always
present — using `undefined' as the "absent" sentinel, never an omitted key
— for the implicit leading (unnamed) category, matching
`MethodCategory.name: Option<String>' on the Rust side. `divider_span`/each
method's `span` are BT-3238's addition over BT-3239's original shape: the
Cockpit's `save-section` write path needs a divider's or method's exact
byte range to splice a rename/insert; the REPL/MCP path (BT-3239) ignores
them. Resolution failures (class not found, ambiguous) come back as
`{error, Reason, Message}' with `Reason' an atom. Transport failures (port
down, timeout) return `{error, port_error, Message}'.
""".
-spec categorize_methods(port(), binary(), atom() | binary()) ->
    {ok, [map()]} | {error, atom(), binary()}.
categorize_methods(Port, Source, ClassName) when
    is_binary(Source), (is_atom(ClassName) orelse is_binary(ClassName))
->
    Request = #{
        command => categorize_methods,
        source => Source,
        class_name => to_binary(ClassName)
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_categorize_methods_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (categorize methods)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, port_error, <<"Compiler port response is malformed">>}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during categorize-methods query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, port_error, <<"Compiler port exited unexpectedly">>}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (categorize methods)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, port_error, <<"Compiler port timed out">>}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (categorize methods)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, port_error, <<"Compiler port is not available">>}
    end;
categorize_methods(_Port, _Source, _ClassName) ->
    {error, bad_argument, <<"categorize_methods: source/class must be binary or atom">>}.

-spec handle_categorize_methods_response(map()) -> {ok, [map()]} | {error, atom(), binary()}.
handle_categorize_methods_response(#{status := ok, categories := Categories}) when
    is_list(Categories)
->
    normalize_categories(Categories);
handle_categorize_methods_response(#{status := error, reason := Reason} = Resp) ->
    Message = maps:get(message, Resp, atom_to_binary(Reason, utf8)),
    {error, Reason, Message};
handle_categorize_methods_response(Other) ->
    ?LOG_ERROR("Unexpected categorize-methods response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, port_error, <<"Unexpected compiler response">>}.

%% Review finding (BT-3238): `normalize_category/1`/`normalize_categorized_method/1`
%% used to have no catch-all clause, unlike this function's own `Other ->
%% {error, port_error, ...}` fallback above — a category or method map
%% missing an expected key (or an unrecognized `side`) raised `function_clause`
%% instead of degrading, crashing the calling `beamtalk_compiler_server`
%% `gen_server:call` for every caller sharing that process rather than
%% returning a structured error to just this one. Wrapping the comprehension
%% in a `try` and giving both normalizers a catch-all that throws a tagged
%% term converts that crash into the same `{error, port_error, _}` shape
%% `handle_categorize_methods_response/1`'s own catch-all already returns.
-spec normalize_categories([map()]) -> {ok, [map()]} | {error, port_error, binary()}.
normalize_categories(Categories) ->
    try
        {ok, [normalize_category(C) || C <- Categories]}
    catch
        error:{malformed_categorize_methods_response, Malformed} ->
            ?LOG_ERROR("Malformed categorize-methods category/method", #{
                domain => [beamtalk, runtime], malformed => Malformed
            }),
            {error, port_error, <<"Unexpected compiler response">>}
    end.

%% Reshape one raw decoded category map into its canonical Erlang-side
%% contract: `#{name := binary() | undefined, divider_span := #{start :=
%% non_neg_integer(), 'end' := non_neg_integer()} | undefined, methods :=
%% [#{selector := binary(), side := instance | class, span := #{...}}]}'.
%%
%% Pattern-matching every key here — rather than passing the decoded map
%% through opaquely, as the previous version of this function did — keeps
%% `name'/`divider_span'/`selector'/`side'/`methods' resident in THIS
%% module's own compiled literal table. `binary_to_term(_, [safe])' (the
%% caller, above) requires every atom in the decoded term to already exist
%% in the receiving node's atom table; none of those atoms otherwise appear
%% anywhere in `beamtalk_compiler''s own source (only in `beamtalk_workspace',
%% the one caller today), so a future caller that links `beamtalk_compiler'
%% without `beamtalk_workspace' ever loaded would otherwise hit a `badarg'
%% decode failure the first time a category carried a name.
-spec normalize_category(map()) -> map().
normalize_category(#{name := Name, divider_span := DividerSpan, methods := Methods}) ->
    #{
        name => Name,
        divider_span => normalize_span(DividerSpan),
        methods => [normalize_categorized_method(M) || M <- Methods]
    };
normalize_category(Other) ->
    error({malformed_categorize_methods_response, Other}).

-spec normalize_span(map() | undefined) -> map() | undefined.
normalize_span(undefined) ->
    undefined;
normalize_span(#{start := Start, 'end' := End}) ->
    #{start => Start, 'end' => End}.

-spec normalize_categorized_method(map()) -> map().
normalize_categorized_method(#{selector := Selector, side := Side, span := Span}) when
    Side =:= instance; Side =:= class
->
    #{selector => Selector, side => Side, span => normalize_span(Span)};
normalize_categorized_method(Other) ->
    error({malformed_categorize_methods_response, Other}).

-doc """
Field-level default-value presence for a class's `state:'/`field:' declarations
(ADR 0082 extension, BT-3254).

Backs `beamtalk_repl_loader:class_def_source_is_skeleton_shaped/2''s sibling
safety check before marking a `'class-def'' ChangeEntry flushable: whether the
resubmitted skeleton text would silently drop a field's default value compared
against the on-disk source — see
`beamtalk_core::source_analysis::class_state_field_defaults''s module doc for
the full "why" (live class reflection cannot recover a compiled class's
default-value TEXT, only whether one exists).

Returns `{ok, #{FieldNameBin => boolean()}}' — one entry per declared field —
on success. Resolution failures (class not found, ambiguous) come back as
`{error, Reason, Message}', collapsed to the single reason `class_not_found'
(this caller has no splice-safety span to report, unlike `resolve_class_span',
so the finer not-found/ambiguous distinction isn't needed). Transport failures
(port down, timeout) return `{error, port_error, Message}'.
""".
-spec class_state_field_defaults(port(), binary(), atom() | binary()) ->
    {ok, #{binary() => boolean()}} | {error, atom(), binary()}.
class_state_field_defaults(Port, Source, ClassName) when
    is_binary(Source), (is_atom(ClassName) orelse is_binary(ClassName))
->
    Request = #{
        command => class_state_field_defaults,
        source => Source,
        class_name => to_binary(ClassName)
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_class_state_field_defaults_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR(
                                "Compiler port decode error (class state field defaults)", #{
                                    domain => [beamtalk, runtime], port => Port
                                }
                            ),
                            {error, port_error, <<"Compiler port response is malformed">>}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during class-state-field-defaults query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, port_error, <<"Compiler port exited unexpectedly">>}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout (class state field defaults)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, port_error, <<"Compiler port timed out">>}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (class state field defaults)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, port_error, <<"Compiler port is not available">>}
    end;
class_state_field_defaults(_Port, _Source, _ClassName) ->
    {error, bad_argument, <<
        "class_state_field_defaults: source/class must be binary or atom"
    >>}.

-spec handle_class_state_field_defaults_response(term()) ->
    {ok, #{binary() => boolean()}} | {error, atom(), binary()}.
handle_class_state_field_defaults_response(#{status := ok, field_defaults := FieldDefaults}) when
    is_map(FieldDefaults)
->
    {ok, FieldDefaults};
handle_class_state_field_defaults_response(#{status := error, reason := Reason} = Resp) ->
    Message = maps:get(message, Resp, atom_to_binary(Reason, utf8)),
    {error, Reason, Message};
handle_class_state_field_defaults_response(Other) ->
    ?LOG_ERROR("Unexpected class-state-field-defaults response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, port_error, <<"Unexpected compiler response">>}.

-doc """
Re-indent a canonical (column-0) method body to `BaseIndent' (BT-2584).

Shifts the compiler's canonical `unparse_method' output (column 0, 2-space
relative steps) so its least-indented line sits at `BaseIndent', producing the
on-disk byte-span shape. The live-patch install hook calls this so the stored
ChangeEntry `source' is a drop-in for `disk[span]' — flush then splices it
verbatim with no reshaping. Pure string transform: returns `{ok, Source}' or,
on a transport failure, `{error, port_error, Message}'.
""".
-spec reindent_method_source(port(), binary(), binary()) ->
    {ok, binary()} | {error, port_error | bad_argument, binary()}.
reindent_method_source(Port, Source, BaseIndent) when
    is_binary(Source), is_binary(BaseIndent)
->
    Request = #{
        command => reindent_method_source,
        source => Source,
        base_indent => BaseIndent
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> handle_reindent_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error (reindent)", #{
                                domain => [beamtalk, runtime], port => Port
                            }),
                            {error, port_error, <<"Compiler port response is malformed">>}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during reindent query", #{
                        domain => [beamtalk, runtime], status => Status
                    }),
                    {error, port_error, <<"Compiler port exited unexpectedly">>}
            after 5000 ->
                %% Shorter than the 30s used by the parse/search commands: this
                %% is a pure in-memory string reshape, so a stall means a wedged
                %% port, not a slow operation. Fail fast to limit the blast radius
                %% on the shared compiler gen_server during a live-patch install.
                ?LOG_ERROR("Compiler port timeout (reindent)", #{
                    domain => [beamtalk, runtime], port => Port
                }),
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                {error, port_error, <<"Compiler port timed out">>}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available (reindent)", #{
                domain => [beamtalk, runtime], port => Port
            }),
            {error, port_error, <<"Compiler port is not available">>}
    end;
reindent_method_source(_Port, _Source, _BaseIndent) ->
    {error, bad_argument, <<"reindent_method_source: source and base_indent must be binary">>}.

-spec handle_reindent_response(map()) -> {ok, binary()} | {error, port_error, binary()}.
handle_reindent_response(#{status := ok, source := Source}) when is_binary(Source) ->
    {ok, Source};
handle_reindent_response(Other) ->
    ?LOG_ERROR("Unexpected reindent response", #{
        domain => [beamtalk, runtime], response => Other
    }),
    {error, port_error, <<"Unexpected compiler response">>}.

%% Normalise an atom-or-binary identifier to a binary.
%%
%% BT-3090: this is the same two-clause shape as `beamtalk_text:to_binary/1`
%% (the runtime-side canonical helper covering atom/binary/list/other), but it
%% deliberately stays local rather than delegating: `beamtalk_compiler` is a
%% peer of `beamtalk_runtime`, not a dependent (ADR 0022 — "the compiler has
%% no dependency on the runtime"), so it cannot reach `beamtalk_text` without
%% breaking that boundary. Every call site here guards
%% `is_atom(X) orelse is_binary(X)` before calling this, so the list/other
%% case `beamtalk_text:to_binary/1` handles can never actually be reached from
%% this module — this is a narrower, correctly-scoped function, not an
%% incomplete copy of the wider one. Pinned by
%% `beamtalk_compiler_port_tests:to_binary_test_/0` so a future edit can't
%% silently narrow or widen it.
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
BT-2902: Extended to handle type_alias_definition responses (ADR 0108 Phase 8).
""".
-spec handle_response(map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {ok, type_alias_definition, map()}
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
    %% ADR 0108 hot-reload re-check trigger (BT-2899 / BT-2952 follow-up):
    %% the alias names this REPL-inline class definition's annotations
    %% transitively referenced — `[]` when omitted (an older compiler-port
    %% binary predating BT-2952). Forwarded so `beamtalk_repl_compiler` can
    %% register `beamtalk_alias_xref` dependency edges for it, mirroring
    %% `beamtalk_compiler_server:handle_compile_response/1`'s identical field.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    BaseInfo = #{
        core_erlang => PrettyCore,
        module_name => ModuleName,
        classes => Classes,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
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
handle_response(
    #{
        status := ok,
        kind := method_definition,
        class_name := ClassName,
        selector := Selector,
        is_class_method := IsClassMethod,
        method_source := MethodSource,
        warnings := Warnings
    } = Response
) ->
    %% ADR 0105 Phase 1 (BT-2777): return_type/param_types carry the compiled
    %% method's declared signature so the workspace can capture it into the
    %% signature-generation store before the patch installs. Defaulted so an
    %% older compiler-port binary (pre-BT-2777) that omits these keys still
    %% decodes.
    ReturnType = maps:get(return_type, Response, <<"Dynamic">>),
    ParamTypes = maps:get(param_types, Response, []),
    {ok, method_definition, #{
        class_name => ClassName,
        selector => Selector,
        is_class_method => IsClassMethod,
        method_source => MethodSource,
        return_type => ReturnType,
        param_types => ParamTypes,
        warnings => Warnings
    }};
%% BT-1612: Protocol definition response
handle_response(
    #{
        status := ok,
        kind := protocol_definition,
        core_erlang := CoreErlang,
        module_name := ModuleName,
        protocols := Protocols,
        warnings := Warnings
    } = Response
) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    %% ADR 0108 hot-reload re-check trigger (BT-2899 / BT-2917 / BT-2952
    %% follow-up): see the class-definition clause above's identical field —
    %% a REPL-inline protocol definition's own method-signature annotations
    %% get the same forwarding so `beamtalk_repl_compiler` can register the
    %% same `beamtalk_alias_xref` dependency edges a class-defining compile
    %% gets.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    {ok, protocol_definition, #{
        core_erlang => PrettyCore,
        module_name => ModuleName,
        protocols => Protocols,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
    }};
%% ADR 0108 Phase 8 (BT-2902): type alias definition response — no
%% core_erlang, since an alias erases entirely at resolution time and has no
%% runtime representation to compile (ADR 0108 Semantics).
handle_response(#{
    status := ok,
    kind := type_alias_definition,
    alias_name := AliasName,
    expansion := Expansion,
    doc_comment := DocComment,
    warnings := Warnings
}) ->
    %% `DocComment` decodes as the atom `undefined` (Rust's `None`) or a
    %% binary (Rust's `Some(text)`) — passed through as-is.
    {ok, type_alias_definition, #{
        alias_name => AliasName,
        expansion => Expansion,
        doc_comment => DocComment,
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
`#{selector := binary(), line := pos_integer(), recv := atom(),
target_module := binary()}' map, passed through unchanged — `target_module' is
the native module an `erlang_ffi' send targets (BT-2669), `<<>>' otherwise),
`{error, [Diagnostic]}' on failure.
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
Handle ETF response from a find_announce_sites_in_source request (BT-2475).
Returns `{ok, [Site]}' on success (each `Site' a
`#{selector := binary(), line := pos_integer(), announcement_class := binary()}'
map, passed through unchanged), `{error, [Diagnostic]}' on failure.
""".
-spec handle_announce_sites_response(map()) -> {ok, [map()]} | {error, [map()]}.
handle_announce_sites_response(#{status := ok, sites := Sites}) when is_list(Sites) ->
    {ok, Sites};
handle_announce_sites_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_announce_sites_response(Other) ->
    ?LOG_ERROR("Unexpected announce-sites response", #{
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
