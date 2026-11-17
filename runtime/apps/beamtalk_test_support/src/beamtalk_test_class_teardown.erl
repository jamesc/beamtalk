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

-export([
    stop_pg_class_processes/0,
    stop_class_process/1,
    is_class_process/1,
    leak_free/1
]).

-doc """
Module-level EUnit wrapper for a suite that loads compiled `bt@stdlib@*` modules
without starting the runtime app (BT-4962). Loading such a module runs its
`on_load`, which starts bare class processes; with no app, they outlive the
suite and later own the class boot tables with no heir. Use it as the suite's
`eunit_wrapper_/1`: `eunit_wrapper_(Tests) -> beamtalk_test_class_teardown:leak_free(Tests).`
After the suite it stops the class processes, but only when `beamtalk_runtime`
is not running: with the app up the classes belong to it.
""".
-spec leak_free(term()) -> term().
leak_free(Tests) ->
    {setup, fun() -> ok end, fun(ok) -> stop_classes_without_app() end, Tests}.

stop_classes_without_app() ->
    case lists:keymember(beamtalk_runtime, 1, application:which_applications()) of
        true -> ok;
        false -> stop_pg_class_processes()
    end.

-doc """
Stop every live class process: the members of the `beamtalk_classes` pg group,
and every registered class gen_server not in it. An earlier suite that restarted
`pg` leaves live class processes (still registered under their
`beamtalk_class_<Name>` name) that the group no longer lists; left running, they
shadow the classes a later bootstrap registers (BT-4934).
""".
-spec stop_pg_class_processes() -> ok.
stop_pg_class_processes() ->
    Members =
        try
            pg:get_members(beamtalk_classes)
        catch
            _:_ -> []
        end,
    Named = [Pid || Name <- registered(), Pid <- [whereis(Name)], is_class_process(Pid)],
    Pids = lists:usort(Members ++ Named),
    lists:foreach(fun stop_class_process/1, Pids).

-doc """
True for a class gen_server (`beamtalk_object_class`), as opposed to the other
processes registered under a `beamtalk_class_*` name (sup, monitor, caches).
""".
-spec is_class_process(term()) -> boolean().
is_class_process(Pid) when is_pid(Pid) ->
    try proc_lib:translate_initial_call(Pid) of
        {beamtalk_object_class, _, _} -> true;
        _ -> false
    catch
        _:_ -> false
    end;
is_class_process(_) ->
    false.

-doc """
Unwatch (when watched) then kill one class process and wait for it to exit.
Non-pids and dead pids are ignored.
""".
-spec stop_class_process(term()) -> ok.
stop_class_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case class_name_of(Pid) of
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

%% The class name of a live class process. The pid->name index can be gone (it
%% is a shared table that dies with the ETS heir when an earlier suite stops the
%% runtime), and skipping the unwatch then lets the class monitor revive the
%% killed class from its stdlib metadata; the `beamtalk_class_<Name>` registered
%% name of the process carries the same answer (BT-4934).
class_name_of(Pid) ->
    case beamtalk_class_registry:class_name_for_pid(Pid) of
        {ok, _} = Found ->
            Found;
        not_found ->
            case process_info(Pid, registered_name) of
                {registered_name, Reg} ->
                    case atom_to_list(Reg) of
                        "beamtalk_class_" ++ Name when Name =/= "" -> {ok, list_to_atom(Name)};
                        _ -> not_found
                    end;
                _ ->
                    not_found
            end
    end.
