%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Gen_server wrapping the compiler OTP Port (ADR 0022, Phase 1).
%%
%% DDD Context: Compilation (Anti-Corruption Layer)
%%
%% Owns the port process and serializes compilation requests. The supervisor
%% restarts this server (and thus re-opens the port) on crashes.
%%
%% Implements in-memory Core Erlang compilation via
%% `core_scan:string/1' → `core_parse:parse/1' → `compile:forms/2'
%% to avoid temp files on disk (BT-48).

-module(beamtalk_compiler_server).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/0, start_link/1,
    compile_expression/3,
    compile/2,
    diagnostics/1,
    version/0,
    compile_core_erlang/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifdef(TEST).
-export([
    handle_compile_response/1,
    handle_diagnostics_response/1,
    handle_version_response/1,
    flush_port_messages/1
]).
-endif.

-record(state, {
    port :: port() | undefined
}).

%%% Public API

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link([]).

-spec start_link(list()) -> gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc Compile a REPL expression.
%% Returns `{ok, CoreErlang, Warnings}' for expressions,
%% `{ok, class_definition, ClassInfo}' for inline class definitions (BT-571),
%% `{ok, method_definition, MethodInfo}' for standalone method definitions (BT-571),
%% or `{error, Diagnostics}' on failure.
-spec compile_expression(binary(), binary(), [binary()]) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
compile_expression(Source, ModuleName, KnownVars) ->
    gen_server:call(?MODULE, {compile_expression, Source, ModuleName, KnownVars}, 30000).

%% @doc Compile a file/class definition.
%% Options: #{path => binary(), stdlib_mode => boolean(), workspace_mode => boolean()}
%% Returns `{ok, #{core_erlang, module_name, classes, warnings}}' or `{error, Diagnostics}'.
-spec compile(binary(), map()) ->
    {ok, map()} | {error, [binary()]}.
compile(Source, Options) ->
    gen_server:call(?MODULE, {compile, Source, Options}, 30000).

%% @doc Get diagnostics for source code (no code generation).
-spec diagnostics(binary()) ->
    {ok, [map()]} | {error, [binary()]}.
diagnostics(Source) ->
    gen_server:call(?MODULE, {diagnostics, Source}, 30000).

%% @doc Get compiler version.
-spec version() -> {ok, binary()} | {error, term()}.
version() ->
    gen_server:call(?MODULE, version, 5000).

%% @doc Compile Core Erlang source to BEAM bytecode in memory.
%% Uses core_scan → core_parse → compile:forms (no temp files).
-spec compile_core_erlang(binary()) -> {ok, atom(), binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin) ->
    CoreErlangStr = binary_to_list(CoreErlangBin),
    case core_scan:string(CoreErlangStr) of
        {ok, Tokens, _} ->
            case core_parse:parse(Tokens) of
                {ok, CoreModule} ->
                    case compile:forms(CoreModule, [from_core, binary, return_errors]) of
                        {ok, ModuleName, Binary} ->
                            {ok, ModuleName, Binary};
                        {ok, ModuleName, Binary, _Warnings} ->
                            {ok, ModuleName, Binary};
                        {error, Errors, _Warnings} ->
                            {error, {core_compile_error, Errors}}
                    end;
                {error, ParseError} ->
                    {error, {core_parse_error, ParseError}}
            end;
        {error, ScanError, _Loc} ->
            {error, {core_scan_error, ScanError}}
    end.

%%% gen_server callbacks

init(_Args) ->
    process_flag(trap_exit, true),
    Port = open_port(),
    {ok, #state{port = Port}}.

handle_call({compile_expression, Source, ModuleName, KnownVars}, _From, State) ->
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars
    ),
    {reply, Result, State};
handle_call({compile, Source, Options}, _From, State) ->
    Result = do_compile(State#state.port, Source, Options),
    {reply, Result, State};
handle_call({diagnostics, Source}, _From, State) ->
    Result = do_diagnostics(State#state.port, Source),
    {reply, Result, State};
handle_call(version, _From, State) ->
    Result = do_version(State#state.port),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    ?LOG_ERROR("Compiler port exited unexpectedly", #{status => Status}),
    {stop, {port_exit_status, Status}, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    ?LOG_ERROR("Compiler port EXIT", #{reason => Reason}),
    {stop, {port_exit, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) when is_port(Port) ->
    catch beamtalk_compiler_port:close(Port),
    ok;
terminate(_Reason, _State) ->
    ok.

%%% Internal functions

open_port() ->
    try
        beamtalk_compiler_port:open()
    catch
        error:{compiler_not_found, _} = Err ->
            ?LOG_ERROR("Failed to open compiler port", #{error => Err}),
            error(Err)
    end.

%% Send a compile (file) request via the port.
do_compile(Port, Source, Options) ->
    StdlibMode = maps:get(stdlib_mode, Options, false),
    WorkspaceMode = maps:get(workspace_mode, Options, true),
    %% BT-775: Optional module_name override for package-qualified naming
    ModuleName = maps:get(module_name, Options, undefined),
    Request0 = #{
        command => compile,
        source => Source,
        stdlib_mode => StdlibMode,
        workspace_mode => WorkspaceMode
    },
    Request =
        case ModuleName of
            undefined -> Request0;
            _ -> Request0#{module_name => ModuleName}
        end,
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    Response = binary_to_term(ResponseBin),
                    handle_compile_response(Response);
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during compile", #{status => Status}),
                    {error, [<<"Compiler port exited unexpectedly">>]}
            after 30000 ->
                flush_port_messages(Port),
                {error, [<<"Compiler port timed out">>]}
            end
    catch
        error:badarg ->
            {error, [<<"Compiler port is not available">>]}
    end.

%% Send a diagnostics request via the port.
do_diagnostics(Port, Source) ->
    Request = #{
        command => diagnostics,
        source => Source
    },
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    Response = binary_to_term(ResponseBin),
                    handle_diagnostics_response(Response);
                {Port, {exit_status, Status}} ->
                    ?LOG_ERROR("Compiler port exited during diagnostics", #{status => Status}),
                    {error, [<<"Compiler port exited unexpectedly">>]}
            after 30000 ->
                flush_port_messages(Port),
                {error, [<<"Compiler port timed out">>]}
            end
    catch
        error:badarg ->
            {error, [<<"Compiler port is not available">>]}
    end.

%% Send a version request via the port.
do_version(Port) ->
    Request = #{command => version},
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    Response = binary_to_term(ResponseBin),
                    handle_version_response(Response);
                {Port, {exit_status, _}} ->
                    {error, port_exited}
            after 5000 ->
                flush_port_messages(Port),
                {error, timeout}
            end
    catch
        error:badarg ->
            {error, port_not_available}
    end.

%% @private Drain any stale messages from a port after a timeout.
%% Prevents late responses from being consumed by the next request's receive.
flush_port_messages(Port) ->
    receive
        {Port, {data, _}} -> flush_port_messages(Port);
        {Port, {exit_status, _}} -> ok
    after 0 ->
        ok
    end.

%% Handle response from compile command.
handle_compile_response(#{
    status := ok,
    core_erlang := CoreErlang,
    module_name := ModuleName,
    classes := Classes,
    warnings := Warnings
}) ->
    {ok, #{
        core_erlang => CoreErlang,
        module_name => ModuleName,
        classes => Classes,
        warnings => Warnings
    }};
handle_compile_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, Diagnostics};
handle_compile_response(Other) ->
    ?LOG_ERROR("Unexpected compile response", #{response => Other}),
    {error, [<<"Unexpected compiler response">>]}.

%% Handle response from diagnostics command.
handle_diagnostics_response(#{status := ok, diagnostics := Diagnostics}) ->
    {ok, Diagnostics};
handle_diagnostics_response(#{status := error, diagnostics := Diagnostics}) ->
    {error, Diagnostics};
handle_diagnostics_response(Other) ->
    ?LOG_ERROR("Unexpected diagnostics response", #{response => Other}),
    {error, [<<"Unexpected compiler response">>]}.

%% Handle response from version command.
handle_version_response(#{status := ok, version := Version}) ->
    {ok, Version};
handle_version_response(Other) ->
    ?LOG_ERROR("Unexpected version response", #{response => Other}),
    {error, unexpected_response}.
