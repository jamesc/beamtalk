%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_monitor).
-behaviour(gen_server).

%%% **DDD Context:** Object System Context

-moduledoc """
Eager crash recovery for class gen_server processes (BT-3236).

`beamtalk_class_sup` starts class processes as `temporary` children, so OTP
never auto-restarts them (see that module's doc for why). This monitor owns
the restart policy instead: `beamtalk_object_class:start/2` registers every
started class here via `watch/2`, and on an abnormal `'DOWN'` the monitor
eagerly rebuilds the class through
`beamtalk_class_registry:restart_class/1` — fresh from ETS metadata and the
module's `__beamtalk_meta/0` — without waiting for the next message send
(which was the pre-BT-3236 lazy path, kept as a fallback in
`beamtalk_class_dispatch:class_send_with_recovery/3`).

A per-class restart budget (`max_restarts` within `window_ms`) stops a
crash-looping class from restarting forever: once exhausted, the class is
dropped with a `?LOG_ERROR` and stays down until something sends to it (lazy
recovery) or it is re-registered. The budget is per class, so one bad class
never affects the others — the failure-isolation property a shared
supervisor restart-intensity counter cannot provide.

Normal exits (`normal`, `shutdown`, `{shutdown, _}`) are deliberate stops —
`removeFromSystem` uses `gen_server:stop/1` — and are never restarted.
""".

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1, watch/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_MAX_RESTARTS, 3).
-define(DEFAULT_WINDOW_MS, 30000).

-record(state, {
    max_restarts :: pos_integer(),
    window_ms :: pos_integer(),
    %% pid → monitor ref, to ignore duplicate watch casts for a live pid
    watched = #{} :: #{pid() => reference()},
    %% monitor ref → class name, for 'DOWN' resolution
    refs = #{} :: #{reference() => atom()},
    %% class name → recent restart timestamps (monotonic ms), pruned to window
    history = #{} :: #{atom() => [integer()]}
}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Start the monitor with the default restart budget (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-doc """
Start the monitor with an explicit restart budget — `max_restarts` abnormal
exits per class within `window_ms`. Exposed for tests; production uses
`start_link/0`.
""".
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-doc """
Register a class process for crash monitoring.

Called by `beamtalk_object_class:start/2` after every successful start
(including restarts). A cast to an unregistered name is a silent no-op, so
EUnit suites that start class processes without the runtime supervision
tree need no guard.
""".
-spec watch(atom(), pid()) -> ok.
watch(ClassName, Pid) ->
    gen_server:cast(?MODULE, {watch, ClassName, Pid}).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

init(Opts) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    {ok, #state{
        max_restarts = maps:get(max_restarts, Opts, ?DEFAULT_MAX_RESTARTS),
        window_ms = maps:get(window_ms, Opts, ?DEFAULT_WINDOW_MS)
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({watch, ClassName, Pid}, #state{watched = Watched, refs = Refs} = State) ->
    case maps:is_key(Pid, Watched) of
        true ->
            {noreply, State};
        false ->
            Ref = erlang:monitor(process, Pid),
            {noreply, State#state{
                watched = Watched#{Pid => Ref},
                refs = Refs#{Ref => ClassName}
            }}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, #state{watched = Watched, refs = Refs} = State) ->
    State1 = State#state{
        watched = maps:remove(Pid, Watched),
        refs = maps:remove(Ref, Refs)
    },
    case maps:find(Ref, Refs) of
        error ->
            {noreply, State1};
        {ok, ClassName} ->
            case is_deliberate_stop(Reason) of
                true -> {noreply, State1};
                false -> {noreply, restart_within_budget(ClassName, Reason, State1)}
            end
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%% ============================================================================
%%% Internal
%%% ============================================================================

is_deliberate_stop(normal) -> true;
is_deliberate_stop(shutdown) -> true;
is_deliberate_stop({shutdown, _}) -> true;
is_deliberate_stop(_) -> false.

restart_within_budget(
    ClassName,
    Reason,
    #state{max_restarts = Max, window_ms = Window, history = History} = State
) ->
    Now = erlang:monotonic_time(millisecond),
    Recent = [T || T <- maps:get(ClassName, History, []), Now - T < Window],
    case length(Recent) >= Max of
        true ->
            ?LOG_ERROR(
                "Class process for '~p' crashed ~p times within ~pms — giving up "
                "on eager restart (lazy recovery on next send may still apply)",
                [ClassName, Max, Window],
                #{class => ClassName, reason => Reason}
            ),
            State#state{history = maps:remove(ClassName, History)};
        false ->
            %% restart_class/1 rebuilds from ETS + __beamtalk_meta/0 and logs
            %% its own warning about dropped hot patches / class-var state; a
            %% successful restart re-enters watch/2 via
            %% beamtalk_object_class:start/2, so no explicit re-watch here.
            case beamtalk_class_registry:restart_class(ClassName) of
                {ok, _NewPid} ->
                    ok;
                {error, RestartReason} ->
                    ?LOG_ERROR(
                        "Eager restart of crashed class '~p' failed: ~p",
                        [ClassName, RestartReason],
                        #{class => ClassName, reason => RestartReason}
                    )
            end,
            State#state{history = History#{ClassName => [Now | Recent]}}
    end.
