%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bindings-changed event pub/sub for workspace WebSocket push.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Tracks WebSocket handler subscribers and broadcasts
%%% `{bindings_changed, SessionId}` messages whenever a REPL session
%%% successfully evaluates an expression (binding values may have changed).
%%%
%%% Analogous to `beamtalk_class_events`. Called by `beamtalk_repl_shell`
%%% after each successful eval so that all connected WebSocket clients
%%% (including the VS Code extension sidebar) can refresh their bindings
%%% display regardless of which session performed the evaluation.

-module(beamtalk_bindings_events).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/1,
    subscribe/0,
    unsubscribe/0,
    on_bindings_changed/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    subscribers :: #{pid() => reference()}
}).

%%% Public API

%% @doc Start the bindings events server with a registered name.
-spec start_link(registered) -> {ok, pid()} | {error, term()}.
start_link(registered) ->
    gen_server:start_link({local, beamtalk_bindings_events}, ?MODULE, [], []).

%% @doc Subscribe the calling process to bindings-changed events.
%% Subscriber receives `{bindings_changed, SessionId}` after each successful eval.
-spec subscribe() -> ok.
subscribe() ->
    gen_server:cast(beamtalk_bindings_events, {subscribe, self()}).

%% @doc Unsubscribe the calling process from bindings-changed events.
-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:cast(beamtalk_bindings_events, {unsubscribe, self()}).

%% @doc Notify all subscribers that bindings changed for a session.
%% Called by `beamtalk_repl_shell` after each successful eval.
%% Safe to call when the server is not running — silently drops the event.
-spec on_bindings_changed(binary()) -> ok.
on_bindings_changed(SessionId) when is_binary(SessionId) ->
    case whereis(beamtalk_bindings_events) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {bindings_changed, SessionId})
    end.

%%% gen_server callbacks

%% @private
init([]) ->
    {ok, #state{subscribers = #{}}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({subscribe, Pid}, State) when is_pid(Pid) ->
    #state{subscribers = Subs} = State,
    case maps:is_key(Pid, Subs) of
        true ->
            {noreply, State};
        false ->
            Ref = erlang:monitor(process, Pid),
            {noreply, State#state{subscribers = Subs#{Pid => Ref}}}
    end;
handle_cast({unsubscribe, Pid}, State) ->
    #state{subscribers = Subs} = State,
    case maps:find(Pid, Subs) of
        {ok, Ref} ->
            erlang:demonitor(Ref, [flush]),
            {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};
        error ->
            {noreply, State}
    end;
handle_cast({bindings_changed, SessionId}, State) ->
    notify_subscribers({bindings_changed, SessionId}, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) ->
    #state{subscribers = Subs} = State,
    {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal

%% @private
-spec notify_subscribers(term(), #state{}) -> ok.
notify_subscribers(Event, #state{subscribers = Subs}) ->
    maps:foreach(
        fun(Pid, _Ref) ->
            Pid ! Event
        end,
        Subs
    ),
    ok.
