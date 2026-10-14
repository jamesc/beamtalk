%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_class_teardown).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit teardown for suites that leave live class processes behind
(BT-3810, BT-3815).

A class registered through `beamtalk_object_class:start/2` is watched by
`beamtalk_class_monitor` whenever the runtime supervision tree is up (other
test modules leave it running). Killing such a class makes the monitor
eagerly restart it from `beamtalk_class_metadata`, racing the metadata
cleanup in teardown: when the monitor wins, a fresh class process (e.g.
`Counter`) is resurrected after teardown and a later fixture's `start_link/2`
fails with `{error, {already_started, Pid}}`. `stop_class_process/1`
therefore unwatches the class (synchronously) before killing it, which turns
the kill into a deliberate stop that nothing revives.

Lives in `beamtalk_test_support` because several `beamtalk_runtime` suites
use it (see `beamtalk_test_corpus`'s moduledoc for why shared test helpers
live in this app).
""".

-export([stop_pg_class_processes/0, stop_class_process/1]).

-doc "Stop every live class process in the `beamtalk_classes` pg group.".
-spec stop_pg_class_processes() -> ok.
stop_pg_class_processes() ->
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,
    lists:foreach(fun stop_class_process/1, Members).

-doc """
Unwatch (when watched) then kill one class process and wait for it to exit.
Non-pids and dead pids are ignored.
""".
-spec stop_class_process(term()) -> ok.
stop_class_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case beamtalk_class_registry:class_name_for_pid(Pid) of
                {ok, Name} -> ok = beamtalk_class_monitor:unwatch(Name);
                not_found -> ok
            end,
            MRef = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 1000 ->
                ok
            end;
        false ->
            ok
    end;
stop_class_process(_) ->
    ok.
