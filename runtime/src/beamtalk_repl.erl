%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Beamtalk REPL Backend (Main Coordinator)
%%%
%%% The REPL backend runs as a gen_server in the BEAM node, accepting TCP
%%% connections from the REPL CLI. It receives Beamtalk expressions, compiles
%%% them via the compiler daemon, loads the resulting bytecode, and evaluates.
%%%
%%% This module coordinates between:
%%% - beamtalk_repl_server: TCP server and JSON protocol
%%% - beamtalk_repl_eval: Expression compilation and evaluation
%%% - beamtalk_repl_state: State management
%%%
%%% ## Architecture
%%%
%%% ```
%%% REPL CLI (Rust) ←→ TCP ←→ REPL Server (beamtalk_repl_server)
%%%                               ↓
%%%                         REPL Coordinator (this module)
%%%                               ↓
%%%                         REPL Eval (beamtalk_repl_eval)
%%%                               ↓
%%%                         Compiler Daemon (TCP/Unix socket)
%%%                               ↓
%%%                         Load & Evaluate
%%% ```
%%%
%%% ## Protocol (JSON over TCP)
%%%
%%% Request from CLI:
%%% ```json
%%% {"type": "eval", "expression": "counter := Counter spawn"}
%%% ```
%%%
%%% Response to CLI:
%%% ```json
%%% {"type": "result", "value": "#<Counter pid=0.123.0>"}
%%% {"type": "error", "message": "Undefined variable: foo"}
%%% ```
%%%
%%% ## Example
%%%
%%% ```erlang
%%% %% Start REPL server on port 9000
%%% {ok, Pid} = beamtalk_repl:start_link(9000),
%%%
%%% %% Or with compiler daemon config
%%% {ok, Pid} = beamtalk_repl:start_link(9000, #{
%%%     compiler_host => "localhost",
%%%     compiler_port => 8081
%%% }).
%%% ```

-module(beamtalk_repl).
-behaviour(gen_server).

%% Public API
-export([start_link/0, start_link/1, start_link/2, stop/1]).
-export([eval/2, get_bindings/1, clear_bindings/1, get_port/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for testing (delegate to server module)
-export([parse_request/1, format_response/1, format_error/1]).

-define(ACCEPT_TIMEOUT, 100).

%%% Public API

%% @doc Start the REPL server on default port from application environment.
%% Falls back to port 9000 if not configured.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Port = application:get_env(beamtalk_runtime, repl_port, 9000),
    start_link(Port, #{}).

%% @doc Start the REPL server on the given port.
%% Uses default compiler daemon socket path.
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    start_link(Port, #{}).

%% @doc Start the REPL server with options.
%% Options:
%%   - daemon_socket_path: Unix socket path for compiler daemon
%%                         (default: ~/.beamtalk/daemon.sock)
-spec start_link(inet:port_number(), map()) -> {ok, pid()} | {error, term()}.
start_link(Port, Options) ->
    gen_server:start_link(?MODULE, {Port, Options}, []).

%% @doc Stop the REPL server.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Evaluate an expression directly (for testing).
%% Uses infinite timeout since compilation and evaluation can be slow.
%% Returns {ok, FormattedResult} or {error, Reason}.
-spec eval(pid(), string()) -> {ok, term()} | {error, term()}.
eval(Pid, Expression) ->
    gen_server:call(Pid, {eval, Expression}, infinity).

%% @doc Get current variable bindings.
-spec get_bindings(pid()) -> map().
get_bindings(Pid) ->
    gen_server:call(Pid, get_bindings).

%% @doc Clear all variable bindings.
-spec clear_bindings(pid()) -> ok.
clear_bindings(Pid) ->
    gen_server:call(Pid, clear_bindings).

%% @doc Get the actual port the server is listening on.
%% Useful when started with port 0 (OS assigns port).
-spec get_port(pid()) -> inet:port_number().
get_port(Pid) ->
    gen_server:call(Pid, get_port).

%%% gen_server callbacks

%% @private
init({Port, Options}) ->
    %% Ensure beamtalk runtime is started (for class registry, bootstrap, etc.)
    application:ensure_all_started(beamtalk_runtime),
    
    %% Open TCP listen socket
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, line}]) of
        {ok, ListenSocket} ->
            %% Get actual port (important when Port=0)
            {ok, ActualPort} = inet:port(ListenSocket),
            %% Start actor registry for this REPL session
            {ok, RegistryPid} = beamtalk_repl_actors:start_link(),
            %% Start accepting connections asynchronously
            self() ! accept,
            State0 = beamtalk_repl_state:new(ListenSocket, ActualPort, Options),
            State1 = beamtalk_repl_state:set_actor_registry(RegistryPid, State0),
            %% Auto-load standard library (Beamtalk class for reflection)
            State = load_stdlib(State1),
            {ok, State};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

%% @private
%% Load standard library modules at REPL startup.
%% This enables `Beamtalk allClasses` to work without manual `:load`.
load_stdlib(State) ->
    StdlibPath = find_stdlib_path(),
    case StdlibPath of
        undefined ->
            logger:debug("Stdlib not found, skipping auto-load"),
            State;
        Path ->
            case beamtalk_repl_eval:handle_load(Path, State) of
                {ok, _Result, NewState} ->
                    logger:info("Auto-loaded stdlib: ~s", [Path]),
                    NewState;
                {error, Reason, _State} ->
                    logger:warning("Failed to auto-load stdlib ~s: ~p", [Path, Reason]),
                    State
            end
    end.

%% @private
%% Find the stdlib beamtalk.bt file in common locations
find_stdlib_path() ->
    %% Try various locations relative to the running code
    BaseCandidates = [
        "lib/beamtalk.bt",                    % Running from repo root
        "../lib/beamtalk.bt"                  % Running from runtime/
    ],
    %% code:lib_dir can return {error, bad_name} if app not loaded
    LibDir = code:lib_dir(beamtalk_runtime),
    RuntimeCandidate =
        case LibDir of
            {error, _} ->
                [];
            _ ->
                [filename:join([LibDir, "..", "..", "lib", "beamtalk.bt"])]
        end,
    Candidates = BaseCandidates ++ RuntimeCandidate,
    find_first_existing(Candidates).

find_first_existing([]) -> undefined;
find_first_existing([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> Path;
        false -> find_first_existing(Rest)
    end.

%% @private
handle_call({eval, Expression}, _From, State) ->
    case beamtalk_repl_eval:do_eval(Expression, State) of
        {ok, Result, NewState} ->
            {reply, {ok, Result}, NewState};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call(get_bindings, _From, State) ->
    Bindings = beamtalk_repl_state:get_bindings(State),
    {reply, Bindings, State};

handle_call(clear_bindings, _From, State) ->
    NewState = beamtalk_repl_state:clear_bindings(State),
    {reply, ok, NewState};

handle_call(get_port, _From, State) ->
    Port = beamtalk_repl_state:get_port(State),
    {reply, Port, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(accept, State) ->
    ListenSocket = beamtalk_repl_state:get_listen_socket(State),
    %% Non-blocking accept with short timeout
    case gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT) of
        {ok, ClientSocket} ->
            %% Handle client in a separate process
            %% Use spawn (not spawn_link) so client crashes don't take down the server
            Self = self(),
            spawn(fun() -> beamtalk_repl_server:handle_client(ClientSocket, Self) end),
            %% Continue accepting
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            %% No connection waiting, continue accepting
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            %% Listen socket closed, stop
            {stop, normal, State};
        {error, Reason} ->
            %% Other error, log and continue
            io:format(standard_error, "REPL accept error: ~p~n", [Reason]),
            self() ! accept,
            {noreply, State}
    end;

handle_info({client_request, Request, ClientPid}, State) ->
    %% Handle request from client handler process
    case beamtalk_repl_server:parse_request(Request) of
        {eval, Expression} ->
            try
                case beamtalk_repl_eval:do_eval(Expression, State) of
                    {ok, Result, NewState} ->
                        ClientPid ! {response, beamtalk_repl_server:format_response(Result)},
                        {noreply, NewState};
                    {error, ErrorReason, NewState} ->
                        ClientPid ! {response, beamtalk_repl_server:format_error(ErrorReason)},
                        {noreply, NewState}
                end
            catch
                Class:CrashReason:Stack ->
                    %% Crash in eval - log and send error response
                    io:format(standard_error,
                              "REPL eval error:~nClass: ~p~nReason: ~p~nStack: ~p~nExpression: ~p~n",
                              [Class, CrashReason, lists:sublist(Stack, 5), Expression]),
                    ClientPid ! {response, beamtalk_repl_server:format_error({eval_error, Class, CrashReason})},
                    {noreply, State}
            end;
        {clear_bindings} ->
            NewState = beamtalk_repl_state:clear_bindings(State),
            ClientPid ! {response, beamtalk_repl_server:format_response(ok)},
            {noreply, NewState};
        {get_bindings} ->
            Bindings = beamtalk_repl_state:get_bindings(State),
            ClientPid ! {response, beamtalk_repl_server:format_bindings(Bindings)},
            {noreply, State};
        {load_file, Path} ->
            case beamtalk_repl_eval:handle_load(Path, State) of
                {ok, Classes, NewState} ->
                    ClientPid ! {response, beamtalk_repl_server:format_loaded(Classes)},
                    {noreply, NewState};
                {error, Reason, NewState} ->
                    ClientPid ! {response, beamtalk_repl_server:format_error(Reason)},
                    {noreply, NewState}
            end;
        {list_actors} ->
            case beamtalk_repl_state:get_actor_registry(State) of
                undefined ->
                    ClientPid ! {response, beamtalk_repl_server:format_error(no_registry)},
                    {noreply, State};
                RegistryPid ->
                    Actors = beamtalk_repl_actors:list_actors(RegistryPid),
                    ClientPid ! {response, beamtalk_repl_server:format_actors(Actors)},
                    {noreply, State}
            end;
        {list_modules} ->
            Tracker = beamtalk_repl_state:get_module_tracker(State),
            RegistryPid = beamtalk_repl_state:get_actor_registry(State),
            ModulesList = beamtalk_repl_modules:list_modules(Tracker),
            %% Enrich each module with current actor count
            ModulesWithInfo = lists:map(
                fun({ModuleName, _Info}) ->
                    ActorCount = beamtalk_repl_modules:get_actor_count(ModuleName, RegistryPid, Tracker),
                    FormattedInfo = beamtalk_repl_modules:format_module_info(_Info, ActorCount),
                    {ModuleName, FormattedInfo}
                end,
                ModulesList
            ),
            ClientPid ! {response, beamtalk_repl_server:format_modules(ModulesWithInfo)},
            {noreply, State};
        {unload_module, ModuleNameStr} ->
            %% Convert module name string to atom
            try list_to_existing_atom(ModuleNameStr) of
                ModuleName ->
                    Tracker = beamtalk_repl_state:get_module_tracker(State),
                    case beamtalk_repl_modules:module_exists(ModuleName, Tracker) of
                        false ->
                            ClientPid ! {response, beamtalk_repl_server:format_error({module_not_found, ModuleNameStr})},
                            {noreply, State};
                        true ->
                            %% Check if module has actors
                            RegistryPid = beamtalk_repl_state:get_actor_registry(State),
                            ActorCount = beamtalk_repl_modules:get_actor_count(ModuleName, RegistryPid, Tracker),
                            if
                                ActorCount > 0 ->
                                    %% Refuse to unload if actors exist
                                    ClientPid ! {response, beamtalk_repl_server:format_error({actors_exist, ModuleName, ActorCount})},
                                    {noreply, State};
                                true ->
                                    %% Safe to unload - no actors
                                    %% Purge and delete module
                                    code:purge(ModuleName),
                                    code:delete(ModuleName),
                                    %% Remove from tracker
                                    NewTracker = beamtalk_repl_modules:remove_module(ModuleName, Tracker),
                                    NewState1 = beamtalk_repl_state:set_module_tracker(NewTracker, State),
                                    %% Remove from loaded modules list
                                    LoadedModules = beamtalk_repl_state:get_loaded_modules(NewState1),
                                    NewLoadedModules = lists:delete(ModuleName, LoadedModules),
                                    NewState2 = beamtalk_repl_state:set_loaded_modules(NewLoadedModules, NewState1),
                                    ClientPid ! {response, beamtalk_repl_server:format_response(ok)},
                                    {noreply, NewState2}
                            end
                    end
            catch
                error:badarg ->
                    ClientPid ! {response, beamtalk_repl_server:format_error({invalid_module_name, ModuleNameStr})},
                    {noreply, State}
            end;
        {kill_actor, PidStr} ->
            case beamtalk_repl_state:get_actor_registry(State) of
                undefined ->
                    ClientPid ! {response, beamtalk_repl_server:format_error(no_registry)},
                    {noreply, State};
                RegistryPid ->
                    case parse_pid_string(PidStr) of
                        {ok, ActorPid} ->
                            case beamtalk_repl_actors:kill_actor(RegistryPid, ActorPid) of
                                ok ->
                                    ClientPid ! {response, beamtalk_repl_server:format_response(ok)},
                                    {noreply, State};
                                {error, Reason} ->
                                    ClientPid ! {response, beamtalk_repl_server:format_error(Reason)},
                                    {noreply, State}
                            end;
                        {error, Reason} ->
                            ClientPid ! {response, beamtalk_repl_server:format_error(Reason)},
                            {noreply, State}
                    end
            end;
        {error, ParseError} ->
            ClientPid ! {response, beamtalk_repl_server:format_error(ParseError)},
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Terminate actor registry (kills all actors)
    case beamtalk_repl_state:get_actor_registry(State) of
        undefined -> ok;
        RegistryPid when is_pid(RegistryPid) ->
            gen_server:stop(RegistryPid)
    end,
    %% Close listen socket
    ListenSocket = beamtalk_repl_state:get_listen_socket(State),
    case ListenSocket of
        undefined -> ok;
        _ -> gen_tcp:close(ListenSocket)
    end,
    ok.

%% @private
code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%% Internal exports for testing (delegate to server module)

%% @doc Parse a request (for testing).
-spec parse_request(binary()) -> term().
parse_request(Data) ->
    beamtalk_repl_server:parse_request(Data).

%% @doc Format a response (for testing).
-spec format_response(term()) -> binary().
format_response(Value) ->
    beamtalk_repl_server:format_response(Value).

%% @doc Format an error (for testing).
-spec format_error(term()) -> binary().
format_error(Reason) ->
    beamtalk_repl_server:format_error(Reason).

%%% Helper functions

%% @private
%% Parse a PID string like "<0.123.0>" into a PID.
-spec parse_pid_string(string()) -> {ok, pid()} | {error, invalid_pid}.
parse_pid_string(PidStr) ->
    try
        list_to_pid(PidStr)
    of
        Pid when is_pid(Pid) ->
            {ok, Pid}
    catch
        _:_ ->
            {error, invalid_pid}
    end.
