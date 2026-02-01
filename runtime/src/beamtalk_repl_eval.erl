%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Expression evaluation for Beamtalk REPL
%%%
%%% This module handles compilation, bytecode generation, and evaluation
%%% of Beamtalk expressions via the compiler daemon.
%%%
%%% ## Platform Requirements
%%%
%%% The secure temp file handling in this module requires Unix-like systems
%%% with support for file permissions (chmod). Windows is not currently
%%% supported due to its different security model (ACLs vs Unix permissions).
%%% The `ensure_secure_temp_dir/0` and `ensure_dir_with_mode/2` functions
%%% rely on `file:change_mode/2` which has limited or no effect on Windows.

-module(beamtalk_repl_eval).

-include_lib("kernel/include/file.hrl").

-export([do_eval/2, handle_load/2]).

%% Exported for testing (only in test builds)
-ifdef(TEST).
-export([derive_module_name/1, extract_assignment/1, format_daemon_diagnostics/1,
         compile_core_erlang/2, extract_class_names/1, ensure_secure_temp_dir/0,
         ensure_dir_with_mode/2, maybe_await_future/1]).
-endif.

-define(RECV_TIMEOUT, 30000).
-define(DAEMON_CONNECT_TIMEOUT, 5000).

%%% Public API

%% @doc Evaluate a Beamtalk expression.
%% This is the core of the REPL - compile, load, and execute.
%% If the result is a Future PID, automatically awaits it before returning.
-spec do_eval(string(), beamtalk_repl_state:state()) -> 
    {ok, term(), beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
do_eval(Expression, State) ->
    %% Generate unique module name for this evaluation
    Counter = beamtalk_repl_state:get_eval_counter(State),
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    
    Bindings = beamtalk_repl_state:get_bindings(State),
    
    %% Compile expression via daemon
    case compile_expression(Expression, ModuleName, Bindings, State) of
        {ok, Binary, _ResultExpr} ->
            %% Load the compiled module
            case code:load_binary(ModuleName, "", Binary) of
                {module, ModuleName} ->
                    %% Execute the eval function
                    try
                        RawResult = apply(ModuleName, eval, [Bindings]),
                        %% Auto-await futures for synchronous REPL experience
                        Result = maybe_await_future(RawResult),
                        %% Check if this was an assignment
                        case extract_assignment(Expression) of
                            {ok, VarName} ->
                                NewBindings = maps:put(VarName, Result, Bindings),
                                FinalState = beamtalk_repl_state:set_bindings(NewBindings, NewState),
                                {ok, Result, FinalState};
                            none ->
                                {ok, Result, NewState}
                        end
                    catch
                        Class:Reason ->
                            {error, {eval_error, Class, Reason}, NewState}
                    after
                        %% Clean up the temporary module
                        code:purge(ModuleName),
                        code:delete(ModuleName)
                    end;
                {error, Reason} ->
                    {error, {load_error, Reason}, NewState}
            end;
        {error, Reason} ->
            {error, {compile_error, Reason}, NewState}
    end.

%% @doc Load a Beamtalk file and register its classes.
-spec handle_load(string(), beamtalk_repl_state:state()) -> 
    {ok, [string()], beamtalk_repl_state:state()} | {error, term(), beamtalk_repl_state:state()}.
handle_load(Path, State) ->
    %% Check if file exists
    case filelib:is_file(Path) of
        false ->
            {error, {file_not_found, Path}, State};
        true ->
            %% Read file source
            case file:read_file(Path) of
                {error, Reason} ->
                    {error, {read_error, Reason}, State};
                {ok, SourceBin} ->
                    Source = binary_to_list(SourceBin),
                    %% Derive module name from path
                    ModuleName = derive_module_name(Path),
                    %% Compile via daemon
                    case compile_file_via_daemon(Source, ModuleName, State) of
                        {ok, Binary, ClassNames} ->
                            %% Load the module (persistent, not deleted)
                            case code:load_binary(ModuleName, Path, Binary) of
                                {module, ModuleName} ->
                                    %% Register classes with beamtalk_classes
                                    register_classes(ClassNames, ModuleName),
                                    %% Track loaded module (avoid duplicates on reload)
                                    LoadedModules = beamtalk_repl_state:get_loaded_modules(State),
                                    NewState = case lists:member(ModuleName, LoadedModules) of
                                        true -> State;
                                        false -> beamtalk_repl_state:add_loaded_module(ModuleName, State)
                                    end,
                                    {ok, ClassNames, NewState};
                                {error, Reason} ->
                                    {error, {load_error, Reason}, State}
                            end;
                        {error, Reason} ->
                            {error, Reason, State}
                    end
            end
    end.

%%% Internal functions

%% Compile a Beamtalk expression to bytecode via compiler daemon.
-spec compile_expression(string(), atom(), map(), beamtalk_repl_state:state()) ->
    {ok, binary(), term()} | {error, term()}.
compile_expression(Expression, ModuleName, _Bindings, State) ->
    case compile_via_daemon(Expression, ModuleName, State) of
        {ok, Binary} ->
            {ok, Binary, {daemon_compiled}};
        {error, daemon_unavailable} ->
            {error, <<"Compiler daemon not running. Start with: beamtalk daemon start --foreground">>};
        {error, {compile_error, Diagnostics}} ->
            {error, format_daemon_diagnostics(Diagnostics)};
        {error, {core_compile_error, Errors}} ->
            {error, iolist_to_binary(io_lib:format("Core Erlang compile error: ~p", [Errors]))};
        {error, {daemon_error, Msg}} ->
            {error, iolist_to_binary([<<"Daemon error: ">>, Msg])};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Compilation error: ~p", [Reason]))}
    end.

%% Compile expression via compiler daemon using JSON-RPC over Unix socket.
-spec compile_via_daemon(string(), atom(), beamtalk_repl_state:state()) ->
    {ok, binary()} | {error, term()}.
compile_via_daemon(Expression, ModuleName, State) ->
    SocketPath = beamtalk_repl_state:get_daemon_socket_path(State),
    %% Try to connect to daemon
    case connect_to_daemon(SocketPath) of
        {ok, Socket} ->
            try
                compile_via_daemon_socket(Expression, ModuleName, Socket)
            after
                gen_tcp:close(Socket)
            end;
        {error, _Reason} ->
            {error, daemon_unavailable}
    end.

%% Connect to the compiler daemon Unix socket.
%% Uses a large receive buffer (64KB) to handle large JSON responses containing
%% Core Erlang code, since {packet, line} mode truncates lines longer than recbuf.
-spec connect_to_daemon(string()) -> {ok, gen_tcp:socket()} | {error, term()}.
connect_to_daemon(SocketPath) ->
    %% Use local address family for Unix socket
    %% Set recbuf to 64KB to handle large JSON responses (Core Erlang can be big)
    case gen_tcp:connect({local, SocketPath}, 0, 
                         [binary, {active, false}, {packet, line}, {recbuf, 65536}],
                         ?DAEMON_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

%% Send compile request to daemon and process response.
-spec compile_via_daemon_socket(string(), atom(), gen_tcp:socket()) ->
    {ok, binary()} | {error, term()}.
compile_via_daemon_socket(Expression, ModuleName, Socket) ->
    %% Build JSON-RPC request using jsx
    RequestId = erlang:unique_integer([positive]),
    Request = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"compile_expression">>,
        <<"params">> => #{
            <<"source">> => list_to_binary(Expression),
            <<"module_name">> => atom_to_binary(ModuleName, utf8)
        }
    }),
    
    %% Send request (with newline delimiter)
    ok = gen_tcp:send(Socket, [Request, <<"\n">>]),
    
    %% Receive response
    case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, ResponseLine} ->
            parse_daemon_response(ResponseLine, ModuleName);
        {error, Reason} ->
            {error, {recv_error, Reason}}
    end.

%% Parse daemon JSON-RPC response using jsx.
-spec parse_daemon_response(binary(), atom()) -> {ok, binary()} | {error, term()}.
parse_daemon_response(ResponseLine, ModuleName) ->
    try
        Response = jsx:decode(ResponseLine, [return_maps]),
        case maps:get(<<"error">>, Response, undefined) of
            undefined ->
                %% No error, check for result
                case maps:get(<<"result">>, Response, undefined) of
                    undefined ->
                        {error, {daemon_error, <<"Invalid JSON-RPC response">>}};
                    Result ->
                        parse_compile_result(Result, ModuleName)
                end;
            ErrorObj ->
                %% JSON-RPC error
                Msg = maps:get(<<"message">>, ErrorObj, <<"Unknown error">>),
                {error, {daemon_error, Msg}}
        end
    catch
        Class:Error:Stack ->
            io:format(standard_error, "Failed to parse daemon response:~nClass: ~p~nError: ~p~nStack: ~p~nResponse: ~p~n", 
                      [Class, Error, Stack, ResponseLine]),
            {error, {daemon_error, <<"Invalid JSON in response">>}}
    end.

%% Parse compile result from daemon response.
-spec parse_compile_result(map(), atom()) -> {ok, binary()} | {error, term()}.
parse_compile_result(Result, ModuleName) ->
    case maps:get(<<"success">>, Result, false) of
        true ->
            case maps:get(<<"core_erlang">>, Result, undefined) of
                undefined ->
                    {error, {daemon_error, <<"No core_erlang in response">>}};
                CoreErlang ->
                    compile_core_erlang(CoreErlang, ModuleName)
            end;
        false ->
            %% Compilation failed - extract diagnostics
            Diagnostics = maps:get(<<"diagnostics">>, Result, []),
            Messages = [maps:get(<<"message">>, D, <<"Unknown error">>) || D <- Diagnostics],
            {error, {compile_error, Messages}}
    end.

%% Compile Core Erlang source to BEAM bytecode.
%% Uses a secure per-user temp directory to prevent symlink attacks (CWE-377, CWE-59).
-spec compile_core_erlang(binary(), atom()) -> {ok, binary()} | {error, term()}.
compile_core_erlang(CoreErlangBin, ModuleName) ->
    case ensure_secure_temp_dir() of
        {ok, SecureTempDir} ->
            %% Generate unique filename to prevent race conditions
            %% Format: {module_name}_{random_suffix}.core
            RandomSuffix = integer_to_list(erlang:unique_integer([positive])),
            TempFile = filename:join(SecureTempDir, 
                                     atom_to_list(ModuleName) ++ "_" ++ RandomSuffix ++ ".core"),
            try
                ok = file:write_file(TempFile, CoreErlangBin),
                %% Compile Core Erlang to BEAM
                case compile:file(TempFile, [from_core, binary, return_errors]) of
                    {ok, _CompiledModule, Binary} ->
                        {ok, Binary};
                    {ok, _CompiledModule, Binary, _Warnings} ->
                        {ok, Binary};
                    {error, Errors, _Warnings} ->
                        {error, {core_compile_error, format_compile_errors(Errors)}}
                end
            catch
                _:Error ->
                    {error, {core_compile_error, Error}}
            after
                file:delete(TempFile)
            end;
        {error, Reason} ->
            {error, {temp_dir_error, Reason}}
    end.

%% Ensure a secure per-user temp directory exists for Core Erlang compilation.
%% Creates ~/.beamtalk/tmp/ with mode 0700 (user-only access) to prevent
%% symlink attacks (CWE-377, CWE-59).
%%
%% The directory is created with restricted permissions so that:
%% - Only the user can read/write/execute (mode 0700)
%% - Other users cannot create symlinks in this directory
%% - Combined with unique filenames, this prevents symlink race conditions
%%
%% Returns {ok, Path} where Path is the absolute path to the secure temp directory,
%% or {error, Reason} if the directory cannot be created.
-spec ensure_secure_temp_dir() -> {ok, string()} | {error, term()}.
ensure_secure_temp_dir() ->
    %% Use ~/.beamtalk/tmp/ as secure per-user temp directory
    %% Requires HOME to be set to avoid falling back to insecure /tmp
    case os:getenv("HOME") of
        false ->
            %% Do not fall back to /tmp to avoid reintroducing symlink attacks.
            %% Mirror behavior of beamtalk_repl_state: require HOME to be set.
            {error, no_home_dir};
        HomeDir ->
            BeamtalkDir = filename:join(HomeDir, ".beamtalk"),
            TempDir = filename:join(BeamtalkDir, "tmp"),
            
            %% Ensure parent directory exists with secure permissions (0700)
            case ensure_dir_with_mode(BeamtalkDir, 8#700) of
                {ok, _} ->
                    %% Ensure temp directory exists with secure permissions (0700)
                    ensure_dir_with_mode(TempDir, 8#700);
                {error, _} = Error ->
                    Error
            end
    end.

%% Ensure a directory exists with the specified mode (octal permissions).
%% If the directory exists, verifies and corrects permissions if needed.
%%
%% ## Platform Support
%%
%% This function requires Unix-like systems. On Windows, file:change_mode/2
%% has limited or no effect, and the security guarantees cannot be ensured.
%%
%% Returns {ok, Path} | {error, Reason}.
-spec ensure_dir_with_mode(string(), non_neg_integer()) -> {ok, string()} | {error, term()}.
ensure_dir_with_mode(Dir, Mode) ->
    case filelib:is_dir(Dir) of
        true ->
            %% Directory exists, verify permissions
            case file:read_file_info(Dir) of
                {ok, FileInfo} ->
                    CurrentMode = FileInfo#file_info.mode,
                    %% Check if mode matches (compare only permission bits)
                    case CurrentMode band 8#777 of
                        Mode ->
                            {ok, Dir};
                        _ ->
                            %% Fix permissions
                            case file:change_mode(Dir, Mode) of
                                ok -> {ok, Dir};
                                {error, ChmodReason} -> {error, {chmod_failed, Dir, ChmodReason}}
                            end
                    end;
                {error, StatReason} ->
                    {error, {stat_failed, Dir, StatReason}}
            end;
        false ->
            case file:make_dir(Dir) of
                ok ->
                    %% Set restrictive permissions
                    case file:change_mode(Dir, Mode) of
                        ok -> {ok, Dir};
                        {error, ChmodReason} -> {error, {chmod_failed, Dir, ChmodReason}}
                    end;
                {error, eexist} ->
                    %% Race condition: dir was created between check and make_dir
                    %% Recurse to verify permissions
                    ensure_dir_with_mode(Dir, Mode);
                {error, MkdirReason} ->
                    {error, {make_dir_failed, Dir, MkdirReason}}
            end
    end.

%% Format daemon diagnostics for display.
-spec format_daemon_diagnostics(list()) -> binary().
format_daemon_diagnostics([]) ->
    <<"Compilation failed">>;
format_daemon_diagnostics(Diagnostics) ->
    iolist_to_binary(lists:join(<<"\n">>, Diagnostics)).

%% Format Erlang compile errors.
format_compile_errors(Errors) ->
    lists:flatten([
        io_lib:format("~s:~p: ~s~n", [File, Line, erl_lint:format_error(Desc)])
        || {File, FileErrors} <- Errors,
           {Line, _Module, Desc} <- FileErrors
    ]).

%% Extract variable name from assignment expression.
-spec extract_assignment(string()) -> {ok, atom()} | none.
extract_assignment(Expression) ->
    case re:run(Expression, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*:=", [{capture, [1], list}]) of
        {match, [VarName]} ->
            {ok, list_to_atom(VarName)};
        nomatch ->
            none
    end.

%% Derive module name from file path.
%% Example: "examples/counter.bt" -> counter
%%
%% ## Module Name Collision Warning
%% User-loaded modules use the file basename directly as the module name.
%% This could theoretically collide with system modules if a user loads a file
%% named after a built-in module (e.g., `beamtalk_repl.bt`). However:
%% - Runtime modules consistently use `beamtalk_*` prefix
%% - Eval modules use `beamtalk_repl_eval_N` pattern
%% - Users are unlikely to name files with `beamtalk_` prefix
%%
%% If this becomes a problem in practice, we could add a prefix (e.g., `user_`)
%% or check against a blocklist of system module names.
%% See: https://linear.app/beamtalk/issue/BT-88 (TODO: create follow-up issue)
%%
%% ## Path Security Note
%% The file path is accepted as-is without validation. In a trusted REPL
%% environment this is acceptable, but be aware that relative paths like
%% `../../sensitive/file.bt` would be processed. The REPL is intended for
%% local development use where the user has filesystem access anyway.
-spec derive_module_name(string()) -> atom().
derive_module_name(Path) ->
    %% Get base filename without extension
    Basename = filename:basename(Path, ".bt"),
    list_to_atom(Basename).

%% Compile a file via the daemon and extract class metadata.
-spec compile_file_via_daemon(string(), atom(), beamtalk_repl_state:state()) ->
    {ok, binary(), [string()]} | {error, term()}.
compile_file_via_daemon(Source, ModuleName, State) ->
    SocketPath = beamtalk_repl_state:get_daemon_socket_path(State),
    case connect_to_daemon(SocketPath) of
        {ok, Socket} ->
            try
                %% Use compile method (not compile_expression) for files
                RequestId = erlang:unique_integer([positive]),
                Request = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => RequestId,
                    <<"method">> => <<"compile">>,
                    <<"params">> => #{
                        <<"path">> => list_to_binary(atom_to_list(ModuleName) ++ ".bt"),
                        <<"source">> => list_to_binary(Source)
                    }
                }),
                
                ok = gen_tcp:send(Socket, [Request, <<"\n">>]),
                
                case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
                    {ok, ResponseLine} ->
                        parse_file_compile_response(ResponseLine, ModuleName);
                    {error, Reason} ->
                        {error, {recv_error, Reason}}
                end
            after
                gen_tcp:close(Socket)
            end;
        {error, _Reason} ->
            {error, daemon_unavailable}
    end.

%% Parse compile response and extract bytecode + class metadata.
-spec parse_file_compile_response(binary(), atom()) ->
    {ok, binary(), [string()]} | {error, term()}.
parse_file_compile_response(ResponseLine, ModuleName) ->
    try
        Response = jsx:decode(ResponseLine, [return_maps]),
        case maps:get(<<"error">>, Response, undefined) of
            undefined ->
                case maps:get(<<"result">>, Response, undefined) of
                    undefined ->
                        {error, {daemon_error, <<"Invalid JSON-RPC response">>}};
                    Result ->
                        parse_file_compile_result(Result, ModuleName)
                end;
            ErrorObj ->
                Msg = maps:get(<<"message">>, ErrorObj, <<"Unknown error">>),
                {error, {daemon_error, Msg}}
        end
    catch
        Class:Error:Stack ->
            io:format(standard_error, "Failed to parse file compile response:~nClass: ~p~nError: ~p~nStack: ~p~nResponse (first 200 bytes): ~p~n",  
                      [Class, Error, Stack, binary:part(ResponseLine, 0, min(200, byte_size(ResponseLine)))]),
            {error, {daemon_error, <<"Invalid JSON in response">>}}
    end.

%% Parse file compile result and extract bytecode + class names.
-spec parse_file_compile_result(map(), atom()) ->
    {ok, binary(), [string()]} | {error, term()}.
parse_file_compile_result(Result, ModuleName) ->
    case maps:get(<<"success">>, Result, false) of
        true ->
            case maps:get(<<"core_erlang">>, Result, undefined) of
                undefined ->
                    {error, {daemon_error, <<"No core_erlang in response">>}};
                CoreErlang ->
                    %% Compile Core Erlang to BEAM
                    case compile_core_erlang(CoreErlang, ModuleName) of
                        {ok, Binary} ->
                            %% Extract class names from result metadata
                            ClassNames = extract_class_names(Result),
                            {ok, Binary, ClassNames};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        false ->
            Diagnostics = maps:get(<<"diagnostics">>, Result, []),
            Messages = [maps:get(<<"message">>, D, <<"Unknown error">>) || D <- Diagnostics],
            {error, {compile_error, format_daemon_diagnostics(Messages)}}
    end.

%% Extract class names from compile result.
-spec extract_class_names(map()) -> [string()].
extract_class_names(Result) ->
    case maps:get(<<"classes">>, Result, undefined) of
        undefined ->
            [];
        ClassesBinList ->
            [binary_to_list(C) || C <- ClassesBinList]
    end.

%% Register loaded classes with beamtalk_classes.
-spec register_classes([string()], atom()) -> ok.
register_classes([], _ModuleName) ->
    ok;
register_classes(ClassNames, ModuleName) ->
    lists:foreach(
        fun(ClassName) ->
            ClassInfo = #{
                module => ModuleName,
                superclass => none,
                methods => #{},
                instance_variables => [],
                class_variables => #{},
                source_file => undefined
            },
            case beamtalk_classes:register_class(list_to_atom(ClassName), ClassInfo) of
                ok -> ok;
                {error, Reason} ->
                    io:format(standard_error, "Warning: Failed to register class ~s: ~p~n", 
                              [ClassName, Reason])
            end
        end,
        ClassNames
    ).

%% Auto-await a Future if the result is a Future PID.
%% This provides a synchronous REPL experience for async message sends.
%% Returns the awaited value, or the original value if not a Future.
-spec maybe_await_future(term()) -> term().
maybe_await_future(Value) when is_pid(Value) ->
    %% For REPL purposes, we attempt to await any PID result.
    %% If it's a Future, it will respond with the future protocol.
    %% If it's an actor or other process, it will ignore the message and
    %% we'll timeout waiting for a response.
    %% We use a short timeout (100ms) to detect non-futures quickly.
    TestTimeout = 100,
    Value ! {await, self(), TestTimeout},
    receive
        {future_resolved, Value, AwaitedValue} ->
            %% It was a resolved future
            AwaitedValue;
        {future_rejected, Value, Reason} ->
            %% It was a rejected future - return error tuple for REPL inspection.
            %% We use tuples instead of throwing (like beamtalk_future:await/1 does)
            %% because REPL should be forgiving and allow users to inspect errors.
            {future_rejected, Reason};
        {future_timeout, Value} ->
            %% Future explicitly timed out waiting for resolution.
            %% This confirms it IS a future. Try a longer await.
            case beamtalk_future:await(Value, 5000) of
                {ok, AwaitedValue} ->
                    AwaitedValue;
                {error, timeout} ->
                    {future_timeout, Value};
                {error, Reason} ->
                    {future_rejected, Reason}
            end
    after TestTimeout + 50 ->
        %% Not a future process - it didn't respond to the protocol.
        %% Use TestTimeout + 50 to give the future time to send {future_timeout, Value}.
        %% Flush any late responses to avoid mailbox pollution.
        receive
            {future_resolved, Value, _} -> ok;
            {future_rejected, Value, _} -> ok;
            {future_timeout, Value} -> ok
        after 20 ->
            ok
        end,
        %% Return the PID as-is (likely an actor or other process)
        Value
    end;
maybe_await_future({beamtalk_object, _, _, _} = Object) ->
    %% Beamtalk objects (actors) are not futures themselves
    %% Return as-is
    Object;
maybe_await_future(Value) ->
    %% Not a PID or special type, return as-is
    Value.
