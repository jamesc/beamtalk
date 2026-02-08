%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Session supervisor for workspace
%%%
%%% Supervises REPL session shell processes.
%%% Each REPL connection creates one session process that maintains
%%% bindings and evaluation state for that specific connection.
%%%
%%% Sessions are ephemeral - they start when a REPL connects and
%%% terminate when the connection closes.

-module(beamtalk_session_sup).
-behaviour(supervisor).

-export([start_link/0, start_session/1]).
-export([init/1]).

%%% Public API

%% @doc Start the session supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a new session under supervision.
%% SessionId should be a unique identifier for the session (e.g., alice, bob).
-spec start_session(atom() | binary()) -> {ok, pid()} | {error, term()}.
start_session(SessionId) ->
    supervisor:start_child(?MODULE, [SessionId]).

%%% supervisor callbacks

%% @private
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Child spec template for sessions
    %% NOTE: This is a placeholder. The actual session implementation
    %% (beamtalk_repl_shell or similar) needs to be created.
    ChildSpec = #{
        id => beamtalk_session,
        start => {beamtalk_repl_shell, start_link, []},
        restart => temporary,  % Sessions don't restart
        shutdown => 5000,
        type => worker,
        modules => [beamtalk_repl_shell]
    },
    
    {ok, {SupFlags, [ChildSpec]}}.
