%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc OTP Port interface to the Rust compiler binary (ADR 0022, Phase 0).
%%
%%% **DDD Context:** Compilation (Anti-Corruption Layer boundary)
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
%% or `{error, Diagnostics}' on failure.
-spec compile_expression(port(), binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
compile_expression(Port, Source, ModuleName, KnownVars) ->
    compile_expression(Port, Source, ModuleName, KnownVars, #{}).

%% @doc Compile a REPL expression with optional compilation options.
%%
%% Options:
%%   class_superclass_index => #{binary() => binary()} — BT-907: cross-file superclass info
%%
%% When class_superclass_index is provided, it is forwarded to the compiler port
%% so that inline class definitions correctly resolve inherited types from
%% already-loaded files.
-spec compile_expression(port(), binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
compile_expression(Port, Source, ModuleName, KnownVars, Options) ->
    SuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    Request0 = #{
        command => compile_expression,
        source => Source,
        module => ModuleName,
        known_vars => KnownVars
    },
    %% BT-907: Include superclass index only when non-empty to keep the
    %% protocol backward-compatible with older port binaries.
    Request =
        case map_size(SuperclassIndex) of
            0 -> Request0;
            _ -> Request0#{class_superclass_index => SuperclassIndex}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    Response = binary_to_term(ResponseBin),
                    handle_response(Response);
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited", #{status => Status}),
                    {error, [<<"Compiler port exited unexpectedly">>]}
            after 30000 ->
                ?LOG_ERROR("Compiler port timeout", #{port => Port}),
                {error, [<<"Compiler port timed out">>]}
            end
    catch
        error:badarg ->
            ?LOG_ERROR("Compiler port not available", #{port => Port}),
            {error, [<<"Compiler port is not available">>]}
    end.

%% @doc Close the compiler port.
-spec close(port()) -> true.
close(Port) ->
    port_close(Port).

%%% Internal functions

%% @private Handle ETF response from the compiler port.
%% BT-571: Extended to handle class_definition and method_definition responses
-spec handle_response(map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
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
    {error, Diagnostics};
handle_response(Other) ->
    ?LOG_ERROR("Unexpected compiler response", #{response => Other}),
    {error, [<<"Unexpected compiler response">>]}.

%% Try to pretty-print textual Core Erlang using Erlang's core parser/pretty-printer.
%% Falls back to the original Core Erlang text on any failure.
-spec maybe_pretty_core(binary()) -> binary().
maybe_pretty_core(CoreErlang) when is_binary(CoreErlang) ->
    CoreStr = binary_to_list(CoreErlang),
    case catch core_scan:string(CoreStr) of
        {ok, Tokens, _} ->
            case catch core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    case catch core_pp:format(CoreModule) of
                        {'EXIT', _} ->
                            CoreErlang;
                        Formatted ->
                            %% Halve indentation from 4-space to 2-space and return binary.
                            try halve_indent(iolist_to_binary(Formatted)) of
                                Bin -> Bin
                            catch
                                _:_ -> CoreErlang
                            end
                    end;
                _ ->
                    CoreErlang
            end;
        _ ->
            CoreErlang
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
