%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc State management for Beamtalk REPL
%%%
%%% This module defines the REPL state record and provides utilities
%%% for manipulating state during REPL sessions.

-module(beamtalk_repl_state).

-export([new/2, new/3, get_bindings/1, set_bindings/2, clear_bindings/1,
         get_eval_counter/1, increment_eval_counter/1,
         get_loaded_modules/1, add_loaded_module/2, set_loaded_modules/2,
         get_daemon_socket_path/1, get_listen_socket/1, get_port/1,
         get_actor_registry/1, set_actor_registry/2,
         get_module_tracker/1, set_module_tracker/2]).

-export_type([state/0]).

-record(state, {
    listen_socket :: gen_tcp:socket() | undefined,
    port :: inet:port_number(),
    bindings :: map(),
    daemon_socket_path :: string(),
    eval_counter :: non_neg_integer(),
    loaded_modules :: [atom()],
    actor_registry :: pid() | undefined,
    module_tracker :: beamtalk_repl_modules:module_tracker()
}).

-opaque state() :: #state{}.

%% @doc Create a new REPL state with default daemon socket path.
-spec new(gen_tcp:socket() | undefined, inet:port_number()) -> state().
new(ListenSocket, Port) ->
    new(ListenSocket, Port, #{}).

%% @doc Create a new REPL state with options.
%% Options:
%%   - daemon_socket_path: Unix socket path for compiler daemon
%%                         (default: ~/.beamtalk/daemon.sock)
-spec new(gen_tcp:socket() | undefined, inet:port_number(), map()) -> state().
new(ListenSocket, Port, Options) ->
    DaemonSocketPath = case maps:get(daemon_socket_path, Options, undefined) of
        undefined -> default_daemon_socket_path();
        Path -> Path
    end,
    #state{
        listen_socket = ListenSocket,
        port = Port,
        bindings = #{},
        daemon_socket_path = DaemonSocketPath,
        eval_counter = 0,
        loaded_modules = [],
        actor_registry = undefined,
        module_tracker = beamtalk_repl_modules:new()
    }.

%% @doc Get current variable bindings.
-spec get_bindings(state()) -> map().
get_bindings(#state{bindings = Bindings}) ->
    Bindings.

%% @doc Set variable bindings.
-spec set_bindings(map(), state()) -> state().
set_bindings(Bindings, State) ->
    State#state{bindings = Bindings}.

%% @doc Clear all variable bindings.
-spec clear_bindings(state()) -> state().
clear_bindings(State) ->
    State#state{bindings = #{}}.

%% @doc Get current eval counter.
-spec get_eval_counter(state()) -> non_neg_integer().
get_eval_counter(#state{eval_counter = Counter}) ->
    Counter.

%% @doc Increment eval counter and return new state.
-spec increment_eval_counter(state()) -> state().
increment_eval_counter(State = #state{eval_counter = Counter}) ->
    State#state{eval_counter = Counter + 1}.

%% @doc Get loaded modules list.
-spec get_loaded_modules(state()) -> [atom()].
get_loaded_modules(#state{loaded_modules = Modules}) ->
    Modules.

%% @doc Add a loaded module to the state.
-spec add_loaded_module(atom(), state()) -> state().
add_loaded_module(Module, State = #state{loaded_modules = Modules}) ->
    State#state{loaded_modules = [Module | Modules]}.

%% @doc Set the loaded modules list.
-spec set_loaded_modules([atom()], state()) -> state().
set_loaded_modules(Modules, State) ->
    State#state{loaded_modules = Modules}.

%% @doc Get daemon socket path.
-spec get_daemon_socket_path(state()) -> string().
get_daemon_socket_path(#state{daemon_socket_path = Path}) ->
    Path.

%% @doc Get listen socket.
-spec get_listen_socket(state()) -> gen_tcp:socket() | undefined.
get_listen_socket(#state{listen_socket = Socket}) ->
    Socket.

%% @doc Get port number.
-spec get_port(state()) -> inet:port_number().
get_port(#state{port = Port}) ->
    Port.

%% @doc Get actor registry PID.
-spec get_actor_registry(state()) -> pid() | undefined.
get_actor_registry(#state{actor_registry = Registry}) ->
    Registry.

%% @doc Set actor registry PID.
-spec set_actor_registry(pid() | undefined, state()) -> state().
set_actor_registry(Registry, State) ->
    State#state{actor_registry = Registry}.

%% @doc Get module tracker.
-spec get_module_tracker(state()) -> beamtalk_repl_modules:module_tracker().
get_module_tracker(#state{module_tracker = Tracker}) ->
    Tracker.

%% @doc Set module tracker.
-spec set_module_tracker(beamtalk_repl_modules:module_tracker(), state()) -> state().
set_module_tracker(Tracker, State) ->
    State#state{module_tracker = Tracker}.

%%% Internal functions

%% @private
%% Determine daemon socket path
%% Priority:
%%   1. BEAMTALK_DAEMON_SOCKET environment variable (if set)
%%   2. Default: ~/.beamtalk/daemon.sock
%% If HOME is unset, fail explicitly so behavior matches the daemon
%% (which requires HOME to be set).
default_daemon_socket_path() ->
    case os:getenv("BEAMTALK_DAEMON_SOCKET") of
        false -> default_daemon_socket_from_home();
        "" -> default_daemon_socket_from_home();
        SocketPath -> SocketPath
    end.

%% Get default socket path from HOME directory
default_daemon_socket_from_home() ->
    case os:getenv("HOME") of
        false ->
            erlang:error({missing_env_var,
                         "HOME",
                         "Beamtalk REPL requires HOME to be set to locate the compiler daemon "
                         "socket. Either set HOME, BEAMTALK_DAEMON_SOCKET, or pass daemon_socket_path in Options."});
        Home ->
            filename:join([Home, ".beamtalk", "daemon.sock"])
    end.
