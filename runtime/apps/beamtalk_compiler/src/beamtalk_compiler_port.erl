%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc OTP Port interface to the Rust compiler binary (ADR 0022, Phase 0).
%%
%%% **DDD Context:** Compilation (Anti-Corruption Layer)
%%
%% Spawns `beamtalk-compiler-port` as an OTP port with `{packet, 4}' framing
%% and ETF-encoded requests/responses.
%%
%% Phase 0 is a wire check — no supervisor, no backend dispatch. Manual
%% verification that BEAM can invoke the Rust compiler via a port.

-module(beamtalk_compiler_port).

-export([
    open/0, open/1,
    compile_expression/4, compile_expression/5,
    resolve_completion_type/3,
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

%% @doc Open a port to the compiler binary.
%% Finds the binary relative to the project root or via PATH.
-spec open() -> port().
open() ->
    open(find_compiler_binary()).

%% @doc Open a port to the compiler binary at the given path.
-spec open(file:filename_all()) -> port().
open(BinaryPath) ->
    ?LOG_INFO("Opening compiler port", #{binary => BinaryPath}),
    open_port({spawn_executable, BinaryPath}, [
        {packet, 4},
        binary,
        exit_status,
        use_stdio
    ]).

%% @doc Compile a REPL expression through the port.
%%
%% Sends an ETF-encoded request and receives an ETF-encoded response.
%% Returns `{ok, CoreErlang, Warnings}' on success,
%% `{ok, class_definition, ClassInfo}' for inline class definitions (BT-571),
%% `{ok, method_definition, MethodInfo}' for standalone method definitions (BT-571),
%% or `{error, Diagnostics}' on failure, where each diagnostic is a map with
%% `message', `line' (1-based), and optionally `hint'.
-spec compile_expression(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [map()]}.
compile_expression(Port, Source, ModuleName, KnownVars) ->
    compile_expression(Port, Source, ModuleName, KnownVars, #{}).

%% @doc Compile a REPL expression with optional compilation options.
%%
%% Options:
%%   class_superclass_index => #{binary() => binary()} — cross-file superclass info
%%   class_module_index => #{binary() => binary()} — cross-directory module name mapping
%%
%% When provided, these indexes are forwarded to the compiler port so that
%% inline class definitions correctly resolve inherited types and module names
%% from already-loaded files.
-spec compile_expression(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
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
                        Response ->
                            handle_response(Response)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error", #{port => Port}),
                            {error, [#{message => <<"Compiler port response is malformed">>}]}
                    end;
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited", #{status => Status}),
                    {error, [#{message => <<"Compiler port exited unexpectedly">>}]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout", #{port => Port}),
                %% Close the port so any late response cannot poison the next request.
                catch port_close(Port),
                {error, [#{message => <<"Compiler port timed out">>}]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available", #{port => Port}),
            {error, [#{message => <<"Compiler port is not available">>}]}
    end.

%% @doc Resolve the type of an expression for REPL completion fallback (BT-1068).
%%
%% Sends an ETF-encoded `resolve_completion_type' request and returns
%% `{ok, ClassName}' when the type is statically known, or
%% `{error, type_unknown}' when the type cannot be determined.
%%
%% `ClassHierarchy' is the accumulated class cache map from
%% `beamtalk_compiler_server' (ADR 0050 Phase 4).
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
                        status => Status
                    }),
                    {error, type_unknown}
            after 5000 ->
                %% Use a shorter timeout for completion — latency budget per ADR 0045.
                ?LOG_ERROR("Compiler port timeout during completion type resolution", #{
                    port => Port
                }),
                %% Close the port so any late response cannot poison the next request.
                catch port_close(Port),
                {error, type_unknown}
            end
    catch
        error:badarg ->
            {error, type_unknown}
    end.

%% @doc Close the compiler port.
-spec close(port()) -> true.
close(Port) ->
    port_close(Port).

%%% Internal functions

%% @private Handle ETF response from the compiler port.
%% BT-571: Extended to handle class_definition and method_definition responses.
%% BT-1235: Diagnostics in error responses are maps with `message', `line', and optional `hint'.
-spec handle_response(map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
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
handle_response(#{status := ok, core_erlang := CoreErlang, warnings := Warnings}) ->
    PrettyCore = maybe_pretty_core(CoreErlang),
    {ok, PrettyCore, Warnings};
handle_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, normalize_diagnostics(Diagnostics)};
handle_response(Other) ->
    ?LOG_ERROR("Unexpected compiler response", #{response => Other}),
    {error, [#{message => <<"Unexpected compiler response">>}]}.

%% @private Handle ETF response from a resolve_completion_type request.
%% Returns `{ok, ClassName}' when the class name is a known existing atom,
%% or `{error, type_unknown}' for not-found or malformed responses.
-spec handle_resolve_response(map()) -> {ok, atom()} | {error, type_unknown}.
handle_resolve_response(#{status := ok, class_name := ClassName}) when is_binary(ClassName) ->
    try binary_to_existing_atom(ClassName, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, type_unknown}
    end;
handle_resolve_response(_) ->
    {error, type_unknown}.

%% @private Normalize a list of diagnostics to a uniform list of maps.
%% BT-1235: The port now returns maps with `message', `line', `hint'.
%% Plain binaries (legacy/protocol errors) are wrapped as `#{message => Bin}'.
-spec normalize_diagnostics([term()]) -> [map()].
normalize_diagnostics(Diagnostics) when is_list(Diagnostics) ->
    [normalize_diagnostic(D) || D <- Diagnostics].

-spec normalize_diagnostic(term()) -> map().
normalize_diagnostic(D) when is_map(D) -> D;
normalize_diagnostic(D) when is_binary(D) -> #{message => D}.

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

%% @private Reduce Core Erlang indentation from 4 spaces per level to 2 spaces per level.
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

%% @private Find the compiler binary.
%% Looks for the binary in the cargo target directory first (development),
%% then falls back to PATH.
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

%% @private Find the project root by looking for Cargo.toml.
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
