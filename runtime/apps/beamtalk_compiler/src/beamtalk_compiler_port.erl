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
    reply_atoms/0,
    unexpected_port_response/2,
    unexpected_diag_response/2,
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
    complete_method_source/3,
    find_selector_send_spans/4,
    find_definition_selector_spans/6,
    build_class_module_index_in_source/4,
    module_name_for_path/4,
    scan_source_dependencies/2,
    close/1,
    port_roundtrip/6,
    add_compile_options/2
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

%% Default per-request budget for a port round trip.
-define(PORT_TIMEOUT_MS, 30000).
%% Completion lookups run on the REPL keystroke path, so they get a shorter
%% budget — latency budget per ADR 0045.
-define(COMPLETION_TIMEOUT_MS, 5000).
%% Re-indenting a single method body is a small, bounded request.
-define(REINDENT_TIMEOUT_MS, 5000).

%% A compiler warning as the port sends it (BT-4449): the diagnostic's
%% message, plus its hint as a separate field when it has one. Never folded
%% into one string here — each REPL surface renders the hint itself.
-type warning() :: #{message := binary(), hint => binary()}.
-type expression_info() ::
    #{assigned_var := binary() | none} | #{step_assignments := [binary() | none]}.
-export_type([warning/0, expression_info/0]).

-type transport_failure() :: malformed | exited | timeout | unavailable.
-type shape() :: diag | port | unknown | fun((transport_failure()) -> term()).

%%% ----------------------------------------------------------------------
%%% Shared port transport
%%% ----------------------------------------------------------------------

-doc """
Send `Request' to the compiler port and decode the reply.

Owns the whole transport: ETF-encode, `port_command', receive, safe decode,
timeout, closing the port on timeout (so a late response cannot poison the
next request) and logging. `Tag' names the command in log messages.
`HandleFun' maps the decoded response to the command's result. `Shape' picks
how transport failures are reported to the caller:

* `diag'    — `{error, [#{message => Msg}]}'
* `port'    — `{error, port_error, Msg}'
* `unknown' — `{error, type_unknown}'
* a fun   — called with the failure kind (`malformed | exited | timeout |
  unavailable') to build a bespoke result
""".
-spec port_roundtrip(port(), map(), term(), timeout(), fun((term()) -> term()), shape()) -> term().
port_roundtrip(Port, Request, Tag, TimeoutMs, HandleFun, Shape) ->
    try port_command(Port, term_to_binary(Request)) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        Response -> HandleFun(Response)
                    catch
                        error:badarg:Stack ->
                            ?LOG_ERROR("Compiler port decode error", #{
                                domain => [beamtalk, runtime],
                                command => Tag,
                                port => Port,
                                response_size => byte_size(ResponseBin),
                                stacktrace => Stack
                            }),
                            transport_error(Shape, malformed)
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited", #{
                        domain => [beamtalk, runtime], command => Tag, status => Status
                    }),
                    transport_error(Shape, exited)
            after TimeoutMs ->
                ?LOG_ERROR("Compiler port timeout", #{
                    domain => [beamtalk, runtime], command => Tag, port => Port
                }),
                %% Close the port so any late response cannot poison the next request.
                (try
                    port_close(Port)
                catch
                    _:_ -> ok
                end),
                transport_error(Shape, timeout)
            end
    catch
        error:badarg:Stack ->
            ?LOG_ERROR("Compiler port not available", #{
                domain => [beamtalk, runtime], command => Tag, port => Port, stacktrace => Stack
            }),
            transport_error(Shape, unavailable)
    end.

-spec transport_error(shape(), transport_failure()) -> term().
transport_error(Fun, Failure) when is_function(Fun, 1) -> Fun(Failure);
transport_error(diag, Failure) -> {error, [#{message => transport_message(Failure)}]};
transport_error(port, Failure) -> {error, port_error, transport_message(Failure)};
transport_error(unknown, _Failure) -> {error, type_unknown}.

-spec transport_message(transport_failure()) -> binary().
transport_message(malformed) -> <<"Compiler port response is malformed">>;
transport_message(exited) -> <<"Compiler port exited unexpectedly">>;
transport_message(timeout) -> <<"Compiler port timed out">>;
transport_message(unavailable) -> <<"Compiler port is not available">>.

-doc """
Shared `#{status := error, reason := R}' tail for `{error, Reason, Message}'
handlers: the message defaults to the reason's name.
""".
-spec reason_error(#{status := error, reason := atom(), _ => _}) -> {error, atom(), binary()}.
reason_error(#{status := error, reason := Reason} = Resp) ->
    {error, Reason, maps:get(message, Resp, atom_to_binary(Reason, utf8))}.

-doc "Shared \"Unexpected response\" tail for `{error, port_error, Message}' handlers.".
-spec unexpected_port_response(term(), term()) -> {error, port_error, binary()}.
unexpected_port_response(Fmt, Other) ->
    log_unexpected(Fmt, Other),
    {error, port_error, <<"Unexpected compiler response">>}.

-doc "Shared \"Unexpected response\" tail for `{error, [Diagnostic]}' handlers.".
-spec unexpected_diag_response(term(), term()) -> {error, [map()]}.
unexpected_diag_response(Fmt, Other) ->
    log_unexpected(Fmt, Other),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

log_unexpected({Fmt, Args}, Other) ->
    ?LOG_ERROR(Fmt, Args, #{domain => [beamtalk, runtime], response => summarize_response(Other)});
log_unexpected(Fmt, Other) ->
    ?LOG_ERROR(Fmt, #{domain => [beamtalk, runtime], response => summarize_response(Other)}).

%% A bounded description of an unexpected port response (BT-5046): the decoded
%% response can be a large map (a whole compile result), so log its shape — keys
%% and size — never its contents.
-spec summarize_response(term()) -> map().
summarize_response(Resp) when is_map(Resp) ->
    #{type => map, size => map_size(Resp), keys => lists:sort(maps:keys(Resp))};
summarize_response(Resp) when is_list(Resp) ->
    #{type => list, length => length(Resp)};
summarize_response(Resp) when is_binary(Resp) ->
    #{type => binary, size => byte_size(Resp)};
summarize_response(Resp) when is_tuple(Resp) ->
    #{type => tuple, size => tuple_size(Resp)};
summarize_response(Resp) ->
    #{type => term, value => Resp}.

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
Returns `{ok, CoreErlang, Warnings, ExprInfo}' on success,
`{ok, class_definition, ClassInfo}' for inline class definitions,
`{ok, method_definition, MethodInfo}' for standalone method definitions,
`{ok, protocol_definition, ProtocolInfo}' for protocol definitions,
or `{error, Diagnostics}' on failure, where each diagnostic is a map with
`message', `line' (1-based), and optionally `hint'.
""".
-spec compile_expression(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [warning()], expression_info()}
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
  known_type_aliases => [binary()] — ADR 0108 Phase 8: reparseable
    `type Name = <expansion>` lines for aliases declared in earlier turns of
    this REPL session, so `::` annotations in the current turn resolve them

When provided, these indexes are forwarded to the compiler port so that
inline class definitions correctly resolve inherited types and module names
from already-loaded files.
""".
-spec compile_expression(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [warning()], expression_info()}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {ok, protocol_definition, map()}
    | {ok, type_alias_definition, map()}
    | {error, [map()]}.
compile_expression(Port, Source, ModuleName, KnownVars, Options) ->
    Request0 = #{
        command => compile_expression,
        source => Source,
        module => ModuleName,
        known_vars => KnownVars
    },
    %% Forward module_name override for inline class definitions
    %% so package-mode produces consistent module names across all paths.
    Request =
        case maps:get(module_name, Options, undefined) of
            undefined ->
                add_compile_options(Request0, Options);
            ModuleNameOverride ->
                (add_compile_options(Request0, Options))#{module_name => ModuleNameOverride}
        end,
    port_roundtrip(
        Port, Request, compile_expression, ?PORT_TIMEOUT_MS, fun handle_response/1, diag
    ).

%% Forward the optional compile-time context shared by `compile_expression/5',
%% `compile_expression_trace/5' and the server's file/method compiles. Each field rides along only when
%% non-empty, keeping the protocol backward-compatible with older port
%% binaries:
%%   class_superclass_index / class_module_index — cross-file inheritance and
%%     cross-directory module names;
%%   class_hierarchy (ADR 0050 Phase 4) / protocol_registry — the ambient
%%     caches (see `beamtalk_compiler_server:inject_ambient/2');
%%   known_type_aliases (ADR 0108 Phase 8) — aliases declared in earlier turns.
-spec add_compile_options(map(), map()) -> map().
add_compile_options(Request, Options) ->
    lists:foldl(
        fun({Key, Empty}, Acc) ->
            case maps:get(Key, Options, Empty) of
                Empty -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        Request,
        [
            {class_superclass_index, #{}},
            {class_module_index, #{}},
            {class_hierarchy, #{}},
            {protocol_registry, #{}},
            {known_type_aliases, []}
        ]
    ).

-doc """
Compile a REPL expression in trace mode.

Same request format as `compile_expression/4' but sends the
`compile_expression_trace' command.  The returned Core Erlang module's
`eval/1' returns `{[{<<"src0">>, Val0}, ...], FinalState}' instead of
`{Result, FinalState}'.
""".
-spec compile_expression_trace(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [warning()], expression_info()} | {error, [map()]}.
compile_expression_trace(Port, Source, ModuleName, KnownVars) ->
    compile_expression_trace(Port, Source, ModuleName, KnownVars, #{}).

-doc "Compile in trace mode with optional compilation options.".
-spec compile_expression_trace(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [warning()], expression_info()} | {error, [map()]}.
compile_expression_trace(Port, Source, ModuleName, KnownVars, Options) ->
    Request0 = #{
        command => compile_expression_trace,
        source => Source,
        module => ModuleName,
        known_vars => KnownVars
    },
    Request = add_compile_options(Request0, Options),
    port_roundtrip(
        Port, Request, compile_expression_trace, ?PORT_TIMEOUT_MS, fun handle_response/1, diag
    ).

-doc """
Resolve the type of an expression for REPL completion fallback.

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
    port_roundtrip(
        Port,
        Request,
        completion_type,
        ?COMPLETION_TIMEOUT_MS,
        fun handle_resolve_response/1,
        unknown
    ).

-doc """
Find call sites of a selector in a single method's source.

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
    port_roundtrip(Port, Request, senders, ?PORT_TIMEOUT_MS, fun handle_senders_response/1, diag);
find_senders_in_source(_Port, _Source, _Selector) ->
    {error, [
        #{
            message =>
                <<"find_senders_in_source: selector must be an atom or binary">>
        }
    ]}.

-doc """
Find every message send within a single method's source.

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
    port_roundtrip(
        Port, Request, all_sends, ?PORT_TIMEOUT_MS, fun handle_all_sends_response/1, diag
    );
find_all_sends_in_source(_Port, _Source) ->
    {error, [
        #{
            message =>
                <<"find_all_sends_in_source: source must be a binary">>
        }
    ]}.

-doc """
Find every `announce:' emission within a single method's source.

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
    port_roundtrip(
        Port, Request, announce_sites, ?PORT_TIMEOUT_MS, fun handle_announce_sites_response/1, diag
    );
find_announce_sites_in_source(_Port, _Source) ->
    {error, [
        #{
            message =>
                <<"find_announce_sites_in_source: source must be a binary">>
        }
    ]}.

-doc """
Find references to a class within a single method's source.

Sends an ETF-encoded `find_references_to_in_source' request and returns
`{ok, [Line]}' on success or `{error, [Diagnostic]}' on failure. Each
line is a 1-based line number relative to `Source'.

Used by `SystemNavigation referencesTo:' via `beamtalk_interface' to power
System Browser-style "who mentions this class?" navigation. Mirrors
`find_senders_in_source/3' but the visitor matches `ClassReference'
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
    port_roundtrip(
        Port, Request, references, ?PORT_TIMEOUT_MS, fun handle_references_response/1, diag
    );
find_references_to_in_source(_Port, _Source, _ClassName) ->
    {error, [
        #{
            message =>
                <<"find_references_to_in_source: class name must be an atom or binary">>
        }
    ]}.

-doc """
Find reads of an field in a single method's source.

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
Find writes of an field in a single method's source.

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
Shared driver for the field reader/writer queries.

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
    port_roundtrip(
        Port,
        Request,
        Label,
        ?PORT_TIMEOUT_MS,
        fun(Response) -> handle_field_response(Response, Label) end,
        diag
    );
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
Find Erlang FFI call sites in a single method's source.

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
    port_roundtrip(
        Port, Request, ffi_sites, ?PORT_TIMEOUT_MS, fun handle_ffi_sites_response/1, diag
    );
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
    port_roundtrip(
        Port, Request, method_span, ?PORT_TIMEOUT_MS, fun handle_method_span_response/1, port
    );
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
handle_method_span_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_method_span_response(Other) ->
    unexpected_port_response("Unexpected method-span response", Other).

-doc """
Resolve the byte span of a class's header + state declarations in `Source'
(ADR 0082 extension) — never its methods.

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
    port_roundtrip(
        Port, Request, class_span, ?PORT_TIMEOUT_MS, fun handle_method_span_response/1, port
    );
resolve_class_span(_Port, _Source, _ClassName) ->
    {error, bad_argument, <<"resolve_class_span: source/class must be binary or atom">>}.

-doc """
Resolve the exact byte span(s) of every self/super-directed send of
`OldSelector' within `MethodSource' (ADR 0114).

Backs `Behaviour>>renameSelector:to:''s reference-site rewrite:
`beamtalk_xref:senders_of/1' only carries a *line* number per sending
method, and a whole-method span is too coarse to splice a single send's
selector token(s) without corrupting the rest of the body — see
`beamtalk_core::queries::selector_rename_query::find_selector_send_spans''s
module doc for the full "why not regex" rationale (a multi-keyword
selector like `at:put:' can have arbitrary nested expressions between its
keyword parts). `MethodSource' is expected to be the OWNING method's own
body text, already sliced via `resolve_method_span/5' — the caller
translates the spans this returns back to absolute file offsets by adding
that method's own span start.

Returns `{ok, Occurrences}' where `Occurrences' is a list of lists — one
inner list per matched self/super send (a keyword selector contributes one
`#{start := S, 'end' := E, new_text := NewText}' map per keyword part, in
keyword-part order; unary/binary contribute a single-element inner list).
An empty outer list means no matching send was found, `MethodSource'
didn't parse, or a match's keyword arity mismatched `NewSelector''s — this
resolver has no other failure mode (see its own doc), so unlike
`resolve_method_span/5' there is no `{error, Reason, Message}' shape for a
malformed/absent match, only for transport failure (port down, timeout) or
a bad argument.
""".
-spec find_selector_send_spans(port(), binary(), atom() | binary(), atom() | binary()) ->
    {ok, [[#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]]}
    | {error, atom(), binary()}.
find_selector_send_spans(Port, MethodSource, OldSelector, NewSelector) when
    is_binary(MethodSource),
    (is_atom(OldSelector) orelse is_binary(OldSelector)),
    (is_atom(NewSelector) orelse is_binary(NewSelector))
->
    Request = #{
        command => find_selector_send_spans,
        method_source => MethodSource,
        old_selector => to_binary(OldSelector),
        new_selector => to_binary(NewSelector)
    },
    port_roundtrip(
        Port,
        Request,
        selector_send_spans,
        ?PORT_TIMEOUT_MS,
        fun handle_selector_send_spans_response/1,
        port
    );
find_selector_send_spans(_Port, _MethodSource, _OldSelector, _NewSelector) ->
    {error, bad_argument, <<
        "find_selector_send_spans: method_source must be binary, "
        "old_selector/new_selector must be atom or binary"
    >>}.

-spec handle_selector_send_spans_response(map()) ->
    {ok, [[#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]]}
    | {error, atom(), binary()}.
handle_selector_send_spans_response(#{status := ok, occurrences := Occurrences}) when
    is_list(Occurrences)
->
    {ok, [decode_selector_send_spans(Spans) || Spans <- Occurrences]};
handle_selector_send_spans_response(Other) ->
    unexpected_port_response("Unexpected selector-send-spans response", Other).

-doc """
Resolve `ClassName''s `(OldSelector, Side)' method DEFINITION's own bare
selector-token span(s) within `Source' (ADR 0114) — the narrow
rewrite counterpart to `find_selector_send_spans/4' for the definition site
itself: a `'rename-method'' ChangeLog entry's `sites[0]' must be a narrow
selector-token splice, never the whole method body, or a rewrite would
corrupt the method's own parameter names/logic. See
`beamtalk_core::queries::selector_rename_query::find_definition_selector_spans''s
module doc for the full resolution strategy (exact `KeywordPart' spans for
a keyword selector; a small modifier-skipping re-lex for unary/binary,
which the parser gives no dedicated span at all).

Returns `{ok, Spans}' — `Spans' a list of `#{start := S, 'end' := E,
new_text := NewText}' maps, one per keyword part for a keyword selector, or
a single-element list for unary/binary. An empty list means
`OldSelector'/`NewSelector' differ in keyword arity (never a panic).
Resolution failures (class not found, selector not found, ambiguous) come
back as `{error, Reason, Message}' exactly like `resolve_method_span/5'.
Transport failures (port down, timeout) return `{error, port_error,
Message}'.
""".
-spec find_definition_selector_spans(
    port(), binary(), atom() | binary(), atom() | binary(), atom() | binary(), instance | class
) ->
    {ok, [#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]}
    | {error, atom(), binary()}.
find_definition_selector_spans(Port, Source, ClassName, OldSelector, NewSelector, Side) when
    is_binary(Source),
    (is_atom(ClassName) orelse is_binary(ClassName)),
    (is_atom(OldSelector) orelse is_binary(OldSelector)),
    (is_atom(NewSelector) orelse is_binary(NewSelector)),
    (Side =:= instance orelse Side =:= class)
->
    Request = #{
        command => find_definition_selector_spans,
        source => Source,
        class_name => to_binary(ClassName),
        old_selector => to_binary(OldSelector),
        new_selector => to_binary(NewSelector),
        side => Side
    },
    port_roundtrip(
        Port,
        Request,
        definition_selector_spans,
        ?PORT_TIMEOUT_MS,
        fun handle_definition_selector_spans_response/1,
        port
    );
find_definition_selector_spans(
    _Port, _Source, _ClassName, _OldSelector, _NewSelector, _Side
) ->
    {error, bad_argument, <<
        "find_definition_selector_spans: source must be binary, class_name/"
        "old_selector/new_selector must be atom or binary, side instance or class"
    >>}.

-spec handle_definition_selector_spans_response(map()) ->
    {ok, [#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}]}
    | {error, atom(), binary()}.
handle_definition_selector_spans_response(#{status := ok, spans := Spans}) when is_list(Spans) ->
    {ok, decode_selector_send_spans(Spans)};
handle_definition_selector_spans_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_definition_selector_spans_response(Other) ->
    unexpected_port_response("Unexpected definition-selector-spans response", Other).

%% Shared decoder for both selector-rename span commands' `#{start, 'end',
%% new_text}' map lists — filters out any malformed element defensively
%% rather than crashing (mirrors this module's other `[safe]'-decoded list
%% responses).
-spec decode_selector_send_spans([map()]) ->
    [#{start := non_neg_integer(), 'end' := non_neg_integer(), new_text := binary()}].
decode_selector_send_spans(Spans) when is_list(Spans) ->
    [
        #{start => Start, 'end' => End, new_text => NewText}
     || #{start := Start, 'end' := End, new_text := NewText} <- Spans,
        is_integer(Start),
        is_integer(End),
        is_binary(NewText)
    ].

-doc """
Group a class's methods by its `// === Name ===' section dividers.

Given the current on-disk source of a `.bt' file and a target `ClassName',
returns the class's methods grouped by the divider comments that precede
them, in source order — the same recognition rules
`beamtalk_core::source_analysis::categorize_methods_in_source' locks down
for every surface (see that module's doc). This is the bridge that lets
Erlang surfaces reach it without a second, drift-prone implementation of
the divider grammar (AGENTS.md's "No duplicate implementations" rule):
`beamtalk_interface:format_class_help/2' (REPL/MCP `:help'/`docs')
and the Cockpit System Browser's grouped method view + `save-section'.

Returns `{ok, Categories}' on success, where each category is `#{name :=
binary() | undefined, divider_span := #{start := S, 'end' := E} |
undefined, methods := [#{selector := binary(), side := instance | class,
span := #{start := S, 'end' := E}}]}'. `name'/`divider_span' are always
present — using `undefined' as the "absent" sentinel, never an omitted key
— for the implicit leading (unnamed) category, matching
`MethodCategory.name: Option<String>' on the Rust side. `divider_span`/each
method's `span` are a later addition over the original shape: the
Cockpit's `save-section` write path needs a divider's or method's exact
byte range to splice a rename/insert; the REPL/MCP path ignores
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
    port_roundtrip(
        Port,
        Request,
        categorize_methods,
        ?PORT_TIMEOUT_MS,
        fun handle_categorize_methods_response/1,
        port
    );
categorize_methods(_Port, _Source, _ClassName) ->
    {error, bad_argument, <<"categorize_methods: source/class must be binary or atom">>}.

-spec handle_categorize_methods_response(map()) -> {ok, [map()]} | {error, atom(), binary()}.
handle_categorize_methods_response(#{status := ok, categories := Categories}) when
    is_list(Categories)
->
    normalize_categories(Categories);
handle_categorize_methods_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_categorize_methods_response(Other) ->
    unexpected_port_response("Unexpected categorize-methods response", Other).

%% A category or method map missing an expected key (or carrying an
%% unrecognized `side`) must not raise `function_clause` — that would crash
%% the calling `beamtalk_compiler_server` `gen_server:call` for every caller
%% sharing that process. Wrapping the comprehension in a `try` and giving
%% both normalizers a catch-all that throws a tagged term converts that
%% crash into the same `{error, port_error, _}` shape
%% `handle_categorize_methods_response/1`'s own catch-all already returns.
-spec normalize_categories([map()]) -> {ok, [map()]} | {error, port_error, binary()}.
normalize_categories(Categories) ->
    try
        {ok, [normalize_category(C) || C <- Categories]}
    catch
        error:{malformed_categorize_methods_response, Malformed} ->
            %% The malformed term is a single small category/method map, and its
            %% contents (e.g. an unrecognised `side`) are what debugging needs.
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
(ADR 0082 extension).

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
    port_roundtrip(
        Port,
        Request,
        class_state_field_defaults,
        ?PORT_TIMEOUT_MS,
        fun handle_class_state_field_defaults_response/1,
        port
    );
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
handle_class_state_field_defaults_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_class_state_field_defaults_response(Other) ->
    unexpected_port_response("Unexpected class-state-field-defaults response", Other).

-doc """
Parse `Source' with the real grammar and report what the load-project
ordering and native-module dependency scan need (BT-4861): every declared
class with its (unqualified) superclass, and every Erlang module the file
references via a `native: module' class header or an `Erlang module' FFI
receiver. Unlike the whole-file regexes this replaces, nothing inside a `///'
doc comment or a string literal is reported, and a file declaring several
classes reports all of them.

Returns `{ok, #{classes := [#{name := binary(), superclass => binary()}],
native_refs := [binary()]}}'. `superclass' is absent for a root class and for
a package-qualified superclass. Transport failures return
`{error, port_error, Message}'.
""".
-spec scan_source_dependencies(port(), binary()) ->
    {ok, #{classes := [map()], native_refs := [binary()]}} | {error, atom(), binary()}.
scan_source_dependencies(Port, Source) when is_binary(Source) ->
    port_roundtrip(
        Port,
        #{command => scan_source_dependencies, source => Source},
        scan_source_dependencies,
        ?PORT_TIMEOUT_MS,
        fun handle_scan_source_dependencies_response/1,
        port
    );
scan_source_dependencies(_Port, _Source) ->
    {error, bad_argument, <<"scan_source_dependencies: source must be a binary">>}.

-spec handle_scan_source_dependencies_response(term()) ->
    {ok, #{classes := [map()], native_refs := [binary()]}} | {error, atom(), binary()}.
handle_scan_source_dependencies_response(
    #{status := ok, classes := Classes, native_refs := NativeRefs}
) when is_list(Classes), is_list(NativeRefs) ->
    {ok, #{classes => Classes, native_refs => NativeRefs}};
handle_scan_source_dependencies_response(#{status := error, reason := Reason} = Resp) ->
    Message = maps:get(message, Resp, atom_to_binary(Reason, utf8)),
    {error, Reason, Message};
handle_scan_source_dependencies_response(Other) ->
    unexpected_port_response("Unexpected scan-source-dependencies response", Other).

-doc """
Build the class→module-name index for a single `src/**/*.bt` file.

Backs the REPL/workspace cold-load fallback for `class_module_index' (ADR
0050) — `beamtalk_workspace_project_sync:build_source_class_module_index/2' calls
this once per project source file instead of independently re-parsing class
declarations with a regex and re-deriving the module-name casing rule in
Erlang. `Source' is the file's raw text, `RelativePath' is its path relative
to `src/' (extension included, `/'-joined — e.g. `<<"util/http_response.bt">>'),
and `PackageName' is the project's package name (e.g. `<<"web">>'). The
compiler port parses `Source' with the real grammar and computes the module
name via the same shared leaf (`relative_module_segments') the CLI's own
`build_class_module_index'/`compute_relative_module' use, so the result is
byte-identical to what a full `beamtalk build' would produce for the same
file.

Returns `{ok, ModuleName, ClassNames}' on success (`ClassNames' may be
empty for a file that declares no class). `RelativePath' is always derived
from a real on-disk project file, never client input, but a path segment
outside `[A-Za-z0-9_]' comes back as `{error, invalid_path_segment,
Message}' rather than a crash — the caller skips that file's contribution
to the cold-load index (see that function's doc for the "never crash"
fallback contract). Transport failures (port down, timeout) return
`{error, port_error, Message}'.
""".
-spec build_class_module_index_in_source(port(), binary(), binary(), binary()) ->
    {ok, binary(), [binary()]} | {error, atom(), binary()}.
build_class_module_index_in_source(Port, Source, RelativePath, PackageName) when
    is_binary(Source), is_binary(RelativePath), is_binary(PackageName)
->
    Request = #{
        command => build_class_module_index_in_source,
        source => Source,
        relative_path => RelativePath,
        package_name => PackageName
    },
    port_roundtrip(
        Port,
        Request,
        build_class_module_index,
        ?PORT_TIMEOUT_MS,
        fun handle_build_class_module_index_in_source_response/1,
        port
    );
build_class_module_index_in_source(_Port, _Source, _RelativePath, _PackageName) ->
    {error, bad_argument, <<
        "build_class_module_index_in_source: source/relative_path/package_name must be binaries"
    >>}.

-spec handle_build_class_module_index_in_source_response(term()) ->
    {ok, binary(), [binary()]} | {error, atom(), binary()}.
handle_build_class_module_index_in_source_response(
    #{status := ok, module_name := ModuleName, classes := Classes}
) when is_binary(ModuleName), is_list(Classes) ->
    {ok, ModuleName, Classes};
handle_build_class_module_index_in_source_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_build_class_module_index_in_source_response(Other) ->
    unexpected_port_response("Unexpected class-module-index response", Other).

-doc """
The module name of the source file at `Path` — the REPL loader's one path
to the shared naming rule (BT-4868).

`RootKind' says what `Path' is relative to: `src' (the package's `src/`,
giving `bt@{Package}@{path}'), `test' (its `test/`, giving
`bt@{Package}@test@{path}'), or `none' (a file under neither root, giving
`bt@{stem}' from the file name alone; `Package' is ignored). The port builds
it with the same Rust function `beamtalk build' and `beamtalk test' call, so
no second copy of the casing/validation rule lives in Erlang. No source is
read or parsed.

Returns `{ok, ModuleName}', `{error, invalid_path_segment, Message}' for a
segment outside `[A-Za-z0-9_]' or `{error, reserved_test_directory, Message}' for a `src/test/` path, or `{error, port_error, Message}' on a
transport failure.
""".
-spec module_name_for_path(port(), binary(), binary(), src | test | none) ->
    {ok, binary()} | {error, atom(), binary()}.
module_name_for_path(Port, Path, PackageName, RootKind) when
    is_binary(Path),
    is_binary(PackageName),
    (RootKind =:= src orelse RootKind =:= test orelse RootKind =:= none)
->
    Request = #{
        command => module_name_for_path,
        path => Path,
        package_name => PackageName,
        root_kind => atom_to_binary(RootKind)
    },
    port_roundtrip(
        Port,
        Request,
        module_name_for_path,
        ?PORT_TIMEOUT_MS,
        fun handle_module_name_for_path_response/1,
        port
    );
module_name_for_path(_Port, _Path, _PackageName, _RootKind) ->
    {error, bad_argument, <<
        "module_name_for_path: path/package_name must be binaries and root_kind src | test | none"
    >>}.

-spec handle_module_name_for_path_response(term()) ->
    {ok, binary()} | {error, atom(), binary()}.
handle_module_name_for_path_response(#{status := ok, module_name := ModuleName}) when
    is_binary(ModuleName)
->
    {ok, ModuleName};
handle_module_name_for_path_response(#{status := error, reason := _} = Resp) ->
    reason_error(Resp);
handle_module_name_for_path_response(Other) ->
    unexpected_port_response("Unexpected module-name-for-path response", Other).

-doc """
Every atom the Rust compiler port (`crates/beamtalk-compiler-port`) can put in
a reply: status values, error reasons, map keys and enum-like values.

`port_roundtrip/6` decodes replies with `binary_to_term(Bin, [safe])`, which
refuses any atom the VM has never seen, so each of these has to exist in the
atom table before the first reply is decoded. Listing them in the body of an
exported function puts them in this module's literal table, which the
compiler cannot merge or drop the way it does identical function clauses;
loading the module (which `port_roundtrip/6` needs anyway) interns them all
(BT-4938).

The list is pinned in both directions to the shared fixture
`test/fixtures/compiler_port_reply_atoms.json`: the Rust port's test fails if
the port emits an atom the fixture lacks, and `beamtalk_compiler_port_atoms_tests`
fails if an atom in the fixture does not exist in a fresh VM once this module is loaded.
""".
-spec reply_atoms() -> [atom()].
reply_atoms() ->
    [
        alias_name,
        ambiguous,
        announcement_class,
        assigned_var,
        categories,
        category,
        class,
        class_definition,
        class_name,
        class_not_found,
        classes,
        core_erlang,
        diagnostics,
        divider_span,
        doc_comment,
        'end',
        erlang_ffi,
        error,
        expansion,
        'false',
        field_defaults,
        hint,
        instance,
        invalid_path_segment,
        invalid_span,
        is_class_method,
        kind,
        line,
        lines,
        merged_class_source,
        message,
        method_definition,
        method_source,
        methods,
        module_name,
        name,
        native_refs,
        new_text,
        none,
        not_found,
        occurrences,
        ok,
        other,
        param_types,
        prev_source,
        protocol_definition,
        protocols,
        reason,
        recv,
        referenced_aliases,
        reserved_test_directory,
        return_type,
        selector,
        selector_not_found,
        self,
        sends,
        severity,
        side,
        sites,
        source,
        span,
        spans,
        start,
        status,
        step_assignments,
        super,
        superclass,
        target_module,
        trailing_core_erlang,
        'true',
        type_alias_definition,
        undefined,
        version,
        warnings
    ].

-doc """
Re-indent a canonical (column-0) method body to `BaseIndent'.

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
    port_roundtrip(
        Port, Request, reindent, ?REINDENT_TIMEOUT_MS, fun handle_reindent_response/1, port
    );
reindent_method_source(_Port, _Source, _BaseIndent) ->
    {error, bad_argument, <<"reindent_method_source: source and base_indent must be binary">>}.

-doc """
The complete method definition for `Source' patching `Selector' (BT-4911).

`Source' is a full definition (returned unchanged) or a bare method body, in
which case the compiler synthesises the `Selector => ' header in front of it
(after any leading comments). The same rule `compile_method' applies, exposed
for callers that splice the definition into a file themselves rather than
compiling it. Pure text transform: parse problems are not reported here.
""".
-spec complete_method_source(port(), binary(), binary()) ->
    {ok, binary()} | {error, port_error | bad_argument, binary()}.
complete_method_source(Port, Source, Selector) when is_binary(Source), is_binary(Selector) ->
    Request = #{command => complete_method_source, source => Source, selector => Selector},
    port_roundtrip(
        Port,
        Request,
        complete_method_source,
        ?REINDENT_TIMEOUT_MS,
        fun handle_reindent_response/1,
        port
    );
complete_method_source(_Port, _Source, _Selector) ->
    {error, bad_argument, <<"complete_method_source: source and selector must be binary">>}.

-spec handle_reindent_response(map()) -> {ok, binary()} | {error, port_error, binary()}.
handle_reindent_response(#{status := ok, source := Source}) when is_binary(Source) ->
    {ok, Source};
handle_reindent_response(Other) ->
    unexpected_port_response("Unexpected reindent response", Other).

%% Normalise an atom-or-binary identifier to a binary.
%%
%% This is the same two-clause shape as `beamtalk_text:to_binary/1`
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
Handles class_definition, method_definition, and type_alias_definition
responses (ADR 0108 Phase 8). Diagnostics in error responses are maps with
`message', `line', and optional `hint'.
""".
-spec handle_response(map()) ->
    {ok, binary(), [warning()], expression_info()}
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
    %% ADR 0108 hot-reload re-check trigger: the alias names this REPL-inline
    %% class definition's annotations transitively referenced — `[]` when
    %% omitted (an older compiler-port binary predating this field). Forwarded
    %% so `beamtalk_repl_compiler` can register `beamtalk_alias_xref`
    %% dependency edges for it, mirroring
    %% `beamtalk_compiler_server:handle_compile_response/1`'s identical field.
    ReferencedAliases = maps:get(referenced_aliases, Response, []),
    BaseInfo = #{
        core_erlang => PrettyCore,
        module_name => ModuleName,
        classes => Classes,
        warnings => Warnings,
        referenced_aliases => ReferencedAliases
    },
    %% Forward trailing_core_erlang when present (inline class + trailing expressions)
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
    %% ADR 0105 Phase 1: return_type/param_types carry the compiled
    %% method's declared signature so the workspace can capture it into the
    %% signature-generation store before the patch installs. Defaulted so an
    %% older compiler-port binary that omits these keys still decodes.
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
%% Protocol definition response
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
    %% ADR 0108 hot-reload re-check trigger: see the class-definition clause above's identical field —
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
%% ADR 0108 Phase 8: type alias definition response — no
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
handle_response(#{status := ok, core_erlang := CoreErlang, warnings := Warnings} = Response) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    {ok, PrettyCore, Warnings, expression_info(Response)};
handle_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_response(Other) ->
    unexpected_diag_response("Unexpected compiler response", Other).

%% BT-4911: what the compiler learned about the source's top-level statements,
%% so the workspace never re-parses it. `assigned_var' (`compile_expression')
%% is the binary name of the variable a single `name := ...' statement assigns,
%% or `none'; `step_assignments' (`compile_expression_trace') is one such entry
%% per trace step. A response from an older port binary that omits the keys
%% decodes as "no assignment".
-spec expression_info(map()) -> expression_info().
expression_info(#{step_assignments := Steps}) when is_list(Steps) ->
    #{step_assignments => Steps};
expression_info(Response) ->
    #{assigned_var => maps:get(assigned_var, Response, none)}.

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
Handle ETF response from a find_senders_in_source request.
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_senders_response(map()) -> {ok, [non_neg_integer()]} | {error, [map()]}.
handle_senders_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_senders_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_senders_response(Other) ->
    unexpected_diag_response("Unexpected senders response", Other).

-doc """
Handle ETF response from a find_all_sends_in_source request.
Returns `{ok, [Send]}' on success (each `Send' a
`#{selector := binary(), line := pos_integer(), recv := atom(),
target_module := binary()}' map, passed through unchanged — `target_module' is
the native module an `erlang_ffi' send targets, `<<>>' otherwise),
`{error, [Diagnostic]}' on failure.
""".
-spec handle_all_sends_response(map()) -> {ok, [map()]} | {error, [map()]}.
handle_all_sends_response(#{status := ok, sends := Sends}) when is_list(Sends) ->
    {ok, Sends};
handle_all_sends_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_all_sends_response(Other) ->
    unexpected_diag_response("Unexpected all-sends response", Other).

-doc """
Handle ETF response from a find_announce_sites_in_source request.
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
    unexpected_diag_response("Unexpected announce-sites response", Other).

-doc """
Handle ETF response from a find_references_to_in_source request.
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_references_response(map()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_references_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_references_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_references_response(Other) ->
    unexpected_diag_response("Unexpected references response", Other).

-doc """
Handle ETF response from a find_field_readers/writers_in_source request.
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on
failure. `Label' is used only to disambiguate the unexpected-response log line.
""".
-spec handle_field_response(map(), binary()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_field_response(#{status := ok, lines := Lines}, _Label) when is_list(Lines) ->
    {ok, Lines};
handle_field_response(#{status := error, diagnostics := Diagnostics}, _Label) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_field_response(Other, Label) ->
    unexpected_diag_response({"Unexpected ~s response", [Label]}, Other).

-doc """
Handle ETF response from a find_ffi_sites_in_source request.
Returns `{ok, [Line]}' on success, `{error, [Diagnostic]}' on failure.
""".
-spec handle_ffi_sites_response(map()) -> {ok, [pos_integer()]} | {error, [map()]}.
handle_ffi_sites_response(#{status := ok, lines := Lines}) when is_list(Lines) ->
    {ok, Lines};
handle_ffi_sites_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_ffi_sites_response(Other) ->
    unexpected_diag_response("Unexpected ffi-sites response", Other).

-doc """
Normalize a list of diagnostics to a uniform list of maps. The port
returns maps with `message', `line', `hint'.
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
