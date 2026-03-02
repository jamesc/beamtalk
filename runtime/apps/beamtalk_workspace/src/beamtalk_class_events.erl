%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Class-loaded event pub/sub for workspace WebSocket push (BT-1020).
%%%
%%% **DDD Context:** REPL
%%%
%%% Tracks WebSocket handler subscribers and broadcasts `{class_loaded, ClassName}`
%%% messages when classes are loaded, reloaded, or eval-defined.
%%%
%%% Analogous to the actor lifecycle subscriber pattern in `beamtalk_repl_actors`.
%%% Called by `beamtalk_class_builder` via the `class_load_callback` application
%%% environment variable — same pattern as `actor_spawn_callback`.

-module(beamtalk_class_events).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% Public API
-export([
    start_link/1,
    subscribe/0,
    unsubscribe/0,
    on_class_loaded/1
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

%% @doc Start the class events server with a registered name.
-spec start_link(registered) -> {ok, pid()} | {error, term()}.
start_link(registered) ->
    gen_server:start_link({local, beamtalk_class_events}, ?MODULE, [], []).

%% @doc Subscribe the calling process to class-loaded events.
%% Subscriber receives `{class_loaded, ClassName}` when a class is registered.
-spec subscribe() -> ok.
subscribe() ->
    gen_server:cast(beamtalk_class_events, {subscribe, self()}).

%% @doc Unsubscribe the calling process from class-loaded events.
-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:cast(beamtalk_class_events, {unsubscribe, self()}).

%% @doc Notify all subscribers that a class was loaded.
%% Called by `beamtalk_class_builder` via the `class_load_callback` env var.
%% Safe to call when the server is not running — silently drops the event.
-spec on_class_loaded(atom()) -> ok.
on_class_loaded(ClassName) when is_atom(ClassName) ->
    case whereis(beamtalk_class_events) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {class_loaded, ClassName})
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
handle_cast({class_loaded, ClassName}, State) ->
    notify_subscribers({class_loaded, ClassName}, State),
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

%%% Internal Functions

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
