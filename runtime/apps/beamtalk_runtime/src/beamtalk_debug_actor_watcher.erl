%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_debug_actor_watcher).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Drops `Logger enableDebug:` state for actors that exit (BT-5022).

`beamtalk_logging_config` records each debug-enabled actor pid in a table and
serves them all from one primary logger filter. This process monitors those
pids; on `'DOWN'` it calls `beamtalk_logging_config:actor_down/1`, which
deletes the row and removes the filter once no actor remains.

The table, not this process, is the source of truth: a restart re-monitors
every actor row (a pid that died meanwhile is pruned at once). Started under
`beamtalk_runtime_sup`.
""".

-export([start_link/0, watch/1, unwatch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-doc "Start the watcher (called by beamtalk_runtime_sup).".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Monitor `Pid` so its debug state is dropped when it exits.

A no-op when the watcher is not running or is going down (a caller without the runtime app);
the row then lives until `disableDebug:` / `disableAllDebug`.
""".
-spec watch(pid()) -> ok.
watch(Pid) ->
    try
        gen_server:call(?MODULE, {watch, Pid})
    catch
        %% Not running / dying / restarting: init/1 re-monitors from the table.
        %% A timeout is not swallowed: the watcher is alive but stuck.
        exit:{Reason, {gen_server, call, _}} when Reason =/= timeout -> ok
    end.

-doc "Stop monitoring `Pid` (its debug state was removed explicitly).".
-spec unwatch(pid()) -> ok.
unwatch(Pid) ->
    gen_server:cast(?MODULE, {unwatch, Pid}).

init([]) ->
    beamtalk_logging_config:set_domain(runtime),
    ok = beamtalk_logging_config:ensure_table(),
    Monitors = lists:foldl(
        fun({Pid, actor_instance, _Class}, Acc) -> Acc#{Pid => monitor(process, Pid)} end,
        #{},
        ets:select(beamtalk_debug_targets, [
            {{'$1', actor_instance, '$2'}, [], [{{'$1', actor_instance, '$2'}}]}
        ])
    ),
    {ok, Monitors}.

handle_call({watch, Pid}, _From, Monitors) ->
    %% The table is the source of truth: a concurrent disable may already have
    %% deleted the row (and its unwatch may have been handled before this call).
    case {Monitors, ets:member(beamtalk_debug_targets, Pid)} of
        {#{Pid := _}, _} -> {reply, ok, Monitors};
        {_, false} -> {reply, ok, Monitors};
        {_, true} -> {reply, ok, Monitors#{Pid => monitor(process, Pid)}}
    end;
handle_call(_Request, _From, Monitors) ->
    {reply, {error, unknown_request}, Monitors}.

handle_cast({unwatch, Pid}, Monitors) ->
    %% The table is the source of truth: a concurrent re-enable may have
    %% re-inserted the row after the disable that sent this cast.
    case {ets:member(beamtalk_debug_targets, Pid), maps:take(Pid, Monitors)} of
        {false, {MRef, Rest}} ->
            demonitor(MRef, [flush]),
            {noreply, Rest};
        _ ->
            {noreply, Monitors}
    end;
handle_cast(_Msg, Monitors) ->
    {noreply, Monitors}.

handle_info({'DOWN', _MRef, process, Pid, _Reason}, Monitors) ->
    ok = beamtalk_logging_config:actor_down(Pid),
    {noreply, maps:remove(Pid, Monitors)};
handle_info(_Info, Monitors) ->
    {noreply, Monitors}.
