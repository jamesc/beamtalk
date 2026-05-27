%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_flush_events).
-behaviour(gen_server).

%%% **DDD Context:** Workspace Context

-moduledoc """
Flush-completion event pub/sub for workspace WebSocket push (ADR 0082 Phase 3,
BT-2289).

Tracks WebSocket handler subscribers and broadcasts `{flush_completed, Files}`
messages when `Workspace flush` (`beamtalk_workspace_flush`) finishes writing
one or more `.bt` source files to disk. LSP clients receive a
`workspace/applyEdit` per touched file so open editor buffers refresh against
the new on-disk state.

Analogous to `beamtalk_class_events` and `beamtalk_bindings_events` — same
subscribe / unsubscribe / `notify_subscribers` shape. Subscribers are
auto-monitored so a crashed WebSocket handler is dropped from the subscriber
table without leaking entries.

The event is emitted from `beamtalk_workspace_flush:complete_flush/4` once a
Phase B rename sequence has fully completed (zero or more files renamed +
`mark_flushed` returned). Subscribers receive an Erlang term
`{flush_completed, [binary()]}` where each binary is an absolute path to a
flushed source file. Path normalisation (relative vs absolute) happens at the
WebSocket-handler / JSON-encode boundary, not here.
""".

%% Public API
-export([
    start_link/1,
    subscribe/0,
    unsubscribe/0,
    on_files_flushed/1
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

-doc "Start the flush events server with a registered name.".
-spec start_link(registered) -> {ok, pid()} | {error, term()}.
start_link(registered) ->
    gen_server:start_link({local, beamtalk_flush_events}, ?MODULE, [], []).

-doc """
Subscribe the calling process to flush-completion events.
Subscriber receives `{flush_completed, Files}` after each successful
`Workspace flush` Phase B sequence, where `Files` is a list of absolute
binary paths to the files that were renamed onto disk.
""".
-spec subscribe() -> ok.
subscribe() ->
    gen_server:cast(beamtalk_flush_events, {subscribe, self()}).

-doc "Unsubscribe the calling process from flush-completion events.".
-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:cast(beamtalk_flush_events, {unsubscribe, self()}).

-doc """
Notify all subscribers that one or more files were flushed.
Called by `beamtalk_workspace_flush:complete_flush/4`.
Safe to call when the server is not running — silently drops the event so the
flush path never fails on a missing subscriber server.
""".
-spec on_files_flushed([binary()]) -> ok.
on_files_flushed([]) ->
    %% Nothing to broadcast — skip the cast entirely so we don't wake handlers
    %% for empty-rename completions.
    ok;
on_files_flushed(Files) when is_list(Files) ->
    case whereis(beamtalk_flush_events) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, {flush_completed, Files})
    end.

%%% gen_server callbacks

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    {ok, #state{subscribers = #{}}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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
handle_cast({flush_completed, Files}, State) ->
    notify_subscribers({flush_completed, Files}, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) ->
    #state{subscribers = Subs} = State,
    {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

-spec notify_subscribers(term(), #state{}) -> ok.
notify_subscribers(Event, #state{subscribers = Subs}) ->
    maps:foreach(
        fun(Pid, _Ref) ->
            Pid ! Event
        end,
        Subs
    ),
    ok.
