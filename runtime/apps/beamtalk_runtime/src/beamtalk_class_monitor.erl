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
-export([start_link/0, start_link/1, watch/2, unwatch/1]).
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
(including restarts). Synchronous on purpose: `start/2` must not return
until the monitor is armed, otherwise a kill delivered before the watch is
processed surfaces as a `noproc` 'DOWN' — indistinguishable from the
deliberate-stop race `is_deliberate_stop(noproc)` exists for — and the
crash would silently not be recovered. A no-op when the monitor is not
running (EUnit suites without the runtime supervision tree).
""".
-spec watch(atom(), pid()) -> ok.
watch(ClassName, Pid) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Monitor when Monitor =:= self() ->
            %% The monitor's own eager restart re-enters start/2 → watch/2
            %% from inside handle_info; a call-to-self would deadlock. The
            %% self-queued cast is processed before any later message, so the
            %% arming guarantee still effectively holds for this path.
            gen_server:cast(?MODULE, {watch, ClassName, Pid});
        _ ->
            gen_server:call(?MODULE, {watch, ClassName, Pid})
    end.

-doc """
Stop monitoring `ClassName` ahead of deliberate removal.

`classRemoveFromSystemByName/1` kills the class's live actors before
stopping the class gen_server itself (`gen_server:stop/1`, reason
`normal`/`shutdown`). BT-3243 removed the link that used to exist between an
actor and the class process it was spawned through: dynamic-dispatch
`{spawn, _}` and `self spawn`/`self spawnWith:` now spawn unlinked
(`beamtalk_actor:safe_spawn/2` uses `gen_server:start/3`, not
`start_link/3`), and `self spawnAs:`/`self spawnWith:as:` unlink
immediately after a successful spawn (`beamtalk_class_instantiation:do_class_self_named_spawn/6`)
— so an actor kill can no longer take the class process down with it. This
unwatch stays as defense in depth against the class's own deliberate stop
racing eager recovery (a kill/crash landing on the class process itself,
from any other source, in the removal window). Synchronous (a cast could
lose the race
against the 'DOWN' delivery); a no-op when the monitor is not running.
""".
-spec unwatch(atom()) -> ok.
unwatch(ClassName) ->
    case whereis(?MODULE) of
        undefined -> ok;
        _ -> gen_server:call(?MODULE, {unwatch, ClassName})
    end.

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

init(Opts) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),
    State0 = #state{
        max_restarts = maps:get(max_restarts, Opts, ?DEFAULT_MAX_RESTARTS),
        window_ms = maps:get(window_ms, Opts, ?DEFAULT_WINDOW_MS)
    },
    %% If this monitor crashed and was restarted by beamtalk_runtime_sup, the
    %% surviving class processes are no longer watched — re-adopt them from
    %% the pg enumeration group so eager recovery is not silently lost for
    %% the rest of the node's life. pg may not be running in EUnit contexts.
    Survivors =
        try
            [
                {Name, Pid}
             || Pid <- pg:get_members(beamtalk_classes),
                {ok, Name} <- [beamtalk_class_registry:class_name_for_pid(Pid)]
            ]
        catch
            _:_ -> []
        end,
    State1 = lists:foldl(
        fun({Name, Pid}, Acc) -> do_watch(Name, Pid, Acc) end,
        State0,
        Survivors
    ),
    {ok, State1}.

handle_call({watch, ClassName, Pid}, _From, State) ->
    {reply, ok, do_watch(ClassName, Pid, State)};
handle_call({unwatch, ClassName}, _From, #state{watched = Watched, refs = Refs} = State) ->
    ClassRefs = [R || R := N <- Refs, N =:= ClassName],
    lists:foreach(fun(R) -> erlang:demonitor(R, [flush]) end, ClassRefs),
    RefSet = sets:from_list(ClassRefs, [{version, 2}]),
    {reply, ok, State#state{
        watched = maps:filter(fun(_P, R) -> not sets:is_element(R, RefSet) end, Watched),
        refs = maps:without(ClassRefs, Refs)
    }};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({watch, ClassName, Pid}, State) ->
    {noreply, do_watch(ClassName, Pid, State)};
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

do_watch(ClassName, Pid, #state{watched = Watched, refs = Refs} = State) ->
    case maps:is_key(Pid, Watched) of
        true ->
            State;
        false ->
            Ref = erlang:monitor(process, Pid),
            State#state{
                watched = Watched#{Pid => Ref},
                refs = Refs#{Ref => ClassName}
            }
    end.

is_deliberate_stop(normal) -> true;
is_deliberate_stop(shutdown) -> true;
is_deliberate_stop({shutdown, _}) -> true;
%% The pid was already dead when the watch cast was processed (e.g. defined
%% and immediately removed in the REPL). We never saw it alive, so this is
%% not a crash to recover from — resurrecting here would defeat a deliberate
%% stop that raced the cast. A genuine crash in that window is still covered
%% by lazy class_send_with_recovery on the next send.
is_deliberate_stop(noproc) -> true;
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
            %% Keep the pruned history: if a lazy recovery re-registers the
            %% class and it crashes again inside the same window, the budget
            %% stays exhausted rather than resetting to a fresh allowance.
            State#state{history = History#{ClassName => Recent}};
        false ->
            %% restart_class/1 rebuilds from ETS + __beamtalk_meta/0 and logs
            %% its own warning about dropped hot patches / class-var state; a
            %% successful restart re-enters watch/2 via
            %% beamtalk_object_class:start/2, so no explicit re-watch here.
            %% The catch matters: supervisor:start_child exits (rather than
            %% returning an error) while beamtalk_class_sup is itself dead or
            %% restarting, and a monitor crash here would silently lose every
            %% remaining watch.
            try beamtalk_class_registry:restart_class(ClassName) of
                {ok, _NewPid} ->
                    ok;
                {error, RestartReason} ->
                    ?LOG_ERROR(
                        "Eager restart of crashed class '~p' failed: ~p",
                        [ClassName, RestartReason],
                        #{class => ClassName, reason => RestartReason}
                    )
            catch
                ErrClass:CatchReason ->
                    ?LOG_ERROR(
                        "Eager restart of crashed class '~p' raised ~p:~p",
                        [ClassName, ErrClass, CatchReason],
                        #{class => ClassName, reason => CatchReason}
                    )
            end,
            State#state{history = History#{ClassName => [Now | Recent]}}
    end.
