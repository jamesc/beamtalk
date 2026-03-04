%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Gen_server wrapping the compiler OTP Port (ADR 0022, Phase 1).
%%
%%% **DDD Context:** Compilation (Anti-Corruption Layer)
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
    compile_expression/3, compile_expression/4,
    compile/2,
    diagnostics/1,
    version/0,
    compile_core_erlang/1,
    register_class/2,
    resolve_completion_type/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifdef(TEST).
-export([
    handle_compile_response/1,
    handle_diagnostics_response/1,
    handle_version_response/1,
    get_classes/0,
    clear_classes/0
]).
-endif.

-record(state, {
    port :: port() | undefined,
    %% ADR 0050 Phase 3: Accumulated class metadata cache.
    %% Maps class name atom → __beamtalk_meta/0 map.
    %% Populated via register_class/2 casts and crash recovery on init.
    classes = #{} :: #{atom() => map()}
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
    compile_expression(Source, ModuleName, KnownVars, #{}).

%% @doc Compile a REPL expression with optional compilation options.
%%
%% Options:
%%   class_superclass_index => #{binary() => binary()} — BT-907: cross-file superclass info
-spec compile_expression(binary(), binary(), [binary()], map()) ->
    {ok, binary(), [binary()]}
    | {ok, class_definition, map()}
    | {ok, method_definition, map()}
    | {error, [binary()]}.
compile_expression(Source, ModuleName, KnownVars, Options) ->
    gen_server:call(
        ?MODULE, {compile_expression, Source, ModuleName, KnownVars, Options}, 30000
    ).

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

-ifdef(TEST).
%% @doc Return the current class cache map (test use only).
-spec get_classes() -> #{atom() => map()}.
get_classes() ->
    gen_server:call(?MODULE, get_classes, 5000).

%% @doc Clear all cached class metadata (test use only).
%%
%% ADR 0050 Phase 3: Used for test isolation — call before tests that need a
%% clean class cache. Synchronous so the next compile sees an empty cache.
-spec clear_classes() -> ok.
clear_classes() ->
    gen_server:call(?MODULE, clear_classes, 5000).
-endif.

%% @doc Resolve the type of an expression for REPL completion fallback (BT-1068).
%%
%% `Expression' is the receiver expression-up-to-cursor with the incomplete
%% prefix already stripped. The class hierarchy is injected automatically from
%% the server state (ADR 0050 Phase 4).
%%
%% Returns `{ok, ClassName}' when the type is statically known, or
%% `{error, type_unknown}' when the type cannot be determined or the compiler
%% is unavailable.
-spec resolve_completion_type(binary()) -> {ok, atom()} | {error, type_unknown}.
resolve_completion_type(Expression) ->
    try
        gen_server:call(?MODULE, {resolve_completion_type, Expression}, 5000)
    catch
        exit:{noproc, _} -> {error, type_unknown};
        exit:timeout -> {error, type_unknown}
    end.

%% @doc Register a class with its metadata in the compiler server cache.
%%
%% ADR 0050 Phase 3: Fire-and-forget cast. Silently dropped if the server is
%% not running (e.g. non-REPL compilation or test runs without the server).
-spec register_class(atom(), map()) -> ok.
register_class(ClassName, MetaMap) ->
    catch gen_server:cast(?MODULE, {register_class, ClassName, MetaMap}),
    ok.

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
    %% ADR 0050 Phase 3: Recover class cache synchronously on start/restart.
    %% Compile requests arriving before recovery completes are queued by the
    %% gen_server mailbox — they will not see an empty cache.
    Classes = recover_from_beam_modules(),
    {ok, #state{port = Port, classes = Classes}}.

handle_call({compile_expression, Source, ModuleName, KnownVars, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    Options1 = Options#{class_hierarchy => State#state.classes},
    Result = beamtalk_compiler_port:compile_expression(
        State#state.port, Source, ModuleName, KnownVars, Options1
    ),
    {reply, Result, State};
handle_call({compile, Source, Options}, _From, State) ->
    %% ADR 0050 Phase 4: Inject class cache so the Rust compiler sees REPL-session classes.
    Options1 = Options#{class_hierarchy => State#state.classes},
    Result = do_compile(State#state.port, Source, Options1),
    {reply, Result, State};
handle_call({resolve_completion_type, Expression}, _From, State) ->
    %% BT-1068: Forward class hierarchy so user-defined classes are visible.
    Result = beamtalk_compiler_port:resolve_completion_type(
        State#state.port, Expression, State#state.classes
    ),
    {reply, Result, State};
handle_call({diagnostics, Source}, _From, State) ->
    Result = do_diagnostics(State#state.port, Source),
    {reply, Result, State};
handle_call(version, _From, State) ->
    Result = do_version(State#state.port),
    {reply, Result, State};
handle_call(clear_classes, _From, State) ->
    {reply, ok, State#state{classes = #{}}};
handle_call(get_classes, _From, State) ->
    {reply, State#state.classes, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_class, ClassName, MetaMap}, State) ->
    %% ADR 0050 Phase 3: Accumulate class metadata; overwrite on redefinition.
    NewClasses = maps:put(ClassName, MetaMap, State#state.classes),
    {noreply, State#state{classes = NewClasses}};
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

%% @private Scan all currently loaded BEAM modules and recover user-class metadata.
%%
%% ADR 0050 Phase 3 (crash recovery): On compiler server restart the port process
%% is gone but every Beamtalk class BEAM module is still loaded in the VM.  We
%% call `__beamtalk_meta/0` on each loaded module and collect the metadata, skipping:
%%   * Modules that do not export `__beamtalk_meta/0` (non-Beamtalk modules).
%%   * Classes in `all_builtins/0` — the Rust compiler already has richer data.
%%   * Old-format modules (no `meta_version` key) are included with their partial
%%     data; absent keys are treated as zero-values by the Rust deserializer.
%%
%% Runs synchronously in `init/1` so compile requests queue during recovery.
-spec recover_from_beam_modules() -> #{atom() => map()}.
recover_from_beam_modules() ->
    %% Guard: beamtalk_class_hierarchy_table may not be loaded if the runtime
    %% app is absent from the release (e.g. compiler-only deployment).
    Builtins =
        case erlang:function_exported(beamtalk_class_hierarchy_table, all_builtins, 0) of
            true -> beamtalk_class_hierarchy_table:all_builtins();
            false -> []
        end,
    BuiltinSet = sets:from_list(Builtins),
    AllModules = code:all_loaded(),
    lists:foldl(
        fun({Module, _Path}, Acc) ->
            case erlang:function_exported(Module, '__beamtalk_meta', 0) of
                false ->
                    Acc;
                true ->
                    case (catch Module:'__beamtalk_meta'()) of
                        Meta when is_map(Meta) ->
                            ClassName = maps:get(class, Meta, undefined),
                            case
                                ClassName =/= undefined andalso
                                    is_atom(ClassName) andalso
                                    not sets:is_element(ClassName, BuiltinSet)
                            of
                                true -> maps:put(ClassName, Meta, Acc);
                                false -> Acc
                            end;
                        _ ->
                            Acc
                    end
            end
        end,
        #{},
        AllModules
    ).

open_port() ->
    try
        beamtalk_compiler_port:open()
    catch
        error:{compiler_not_found, _} = Err ->
            ?LOG_ERROR("Failed to open compiler port", #{error => Err}),
            error(Err)
    end.

%% Send a request via the port and receive the response.
%%
%% Returns:
%%   {ok, Response}         — decoded ETF response from the port
%%   {exit_status, Status}  — port exited before responding
%%   timeout                — port did not respond within Timeout ms
%%   port_not_available     — port is closed (badarg from port_command)
%%   decode_error           — response could not be decoded (unexpected atoms)
-spec send_port_request(port(), map(), timeout()) ->
    {ok, term()} | {exit_status, non_neg_integer()} | timeout | port_not_available | decode_error.
send_port_request(Port, Request, Timeout) ->
    RequestBin = term_to_binary(Request),
    try port_command(Port, RequestBin) of
        true ->
            receive
                {Port, {data, ResponseBin}} ->
                    try binary_to_term(ResponseBin, [safe]) of
                        %% [safe] prevents atom exhaustion: all response atoms are
                        %% literals in this module and guaranteed to exist.
                        Response -> {ok, Response}
                    catch
                        error:badarg ->
                            ?LOG_ERROR("Compiler port decode error", #{port => Port}),
                            decode_error
                    end;
                {Port, {exit_status, Status}} ->
                    {exit_status, Status}
            after Timeout ->
                %% Close the port so any late response cannot poison the next request.
                catch port_close(Port),
                timeout
            end
    catch
        error:badarg ->
            port_not_available
    end.

%% Send a compile (file) request via the port.
do_compile(Port, Source, Options) ->
    StdlibMode = maps:get(stdlib_mode, Options, false),
    WorkspaceMode = maps:get(workspace_mode, Options, true),
    %% BT-775: Optional module_name override for package-qualified naming
    ModuleName = maps:get(module_name, Options, undefined),
    %% BT-845/BT-860: Optional source file path for beamtalk_source attribute
    SourcePath = maps:get(source_path, Options, undefined),
    %% BT-905: Optional class superclass index for cross-file value-object inheritance
    ClassSuperclassIndex = maps:get(class_superclass_index, Options, #{}),
    %% Optional class module index for correct cross-directory module names
    ClassModuleIndex = maps:get(class_module_index, Options, #{}),
    Request0 = #{
        command => compile,
        source => Source,
        stdlib_mode => StdlibMode,
        workspace_mode => WorkspaceMode
    },
    Request1 =
        case ModuleName of
            undefined -> Request0;
            _ -> Request0#{module_name => ModuleName}
        end,
    Request2 =
        case SourcePath of
            undefined -> Request1;
            _ -> Request1#{source_path => SourcePath}
        end,
    Request3 =
        case map_size(ClassSuperclassIndex) of
            0 -> Request2;
            _ -> Request2#{class_superclass_index => ClassSuperclassIndex}
        end,
    Request4 =
        case map_size(ClassModuleIndex) of
            0 -> Request3;
            _ -> Request3#{class_module_index => ClassModuleIndex}
        end,
    %% ADR 0050 Phase 4: Inject accumulated class metadata into the port request.
    Classes = maps:get(class_hierarchy, Options, #{}),
    RequestFinal =
        case map_size(Classes) of
            0 -> Request4;
            _ -> Request4#{class_hierarchy => Classes}
        end,
    case send_port_request(Port, RequestFinal, 30000) of
        {ok, Response} ->
            handle_compile_response(Response);
        {exit_status, Status} ->
            ?LOG_ERROR("Compiler port exited during compile", #{status => Status}),
            {error, [<<"Compiler port exited unexpectedly">>]};
        timeout ->
            {error, [<<"Compiler port timed out">>]};
        port_not_available ->
            {error, [<<"Compiler port is not available">>]};
        decode_error ->
            {error, [<<"Compiler port response is malformed">>]}
    end.

%% Send a diagnostics request via the port.
do_diagnostics(Port, Source) ->
    Request = #{command => diagnostics, source => Source},
    case send_port_request(Port, Request, 30000) of
        {ok, Response} ->
            handle_diagnostics_response(Response);
        {exit_status, Status} ->
            ?LOG_ERROR("Compiler port exited during diagnostics", #{status => Status}),
            {error, [<<"Compiler port exited unexpectedly">>]};
        timeout ->
            {error, [<<"Compiler port timed out">>]};
        port_not_available ->
            {error, [<<"Compiler port is not available">>]};
        decode_error ->
            {error, [<<"Compiler port response is malformed">>]}
    end.

%% Send a version request via the port.
do_version(Port) ->
    Request = #{command => version},
    case send_port_request(Port, Request, 5000) of
        {ok, Response} ->
            handle_version_response(Response);
        {exit_status, _} ->
            {error, port_exited};
        timeout ->
            {error, timeout};
        port_not_available ->
            {error, port_not_available};
        decode_error ->
            {error, decode_error}
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
