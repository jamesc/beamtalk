%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_class_teardown).

%%% **DDD Context:** Runtime Context

-moduledoc """
Registration teardown for a class process, split out of `beamtalk_object_class` (BT-5047).

What `beamtalk_object_class:terminate/2` runs, and what the registry runs for
a class process that died without terminating: removing the class's registry
rows and indexes, and announcing the removal. `terminate/2` itself stays in
`beamtalk_object_class` because it is the gen_server callback.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    remove_class_registration/1,
    remove_dead_class_registration/2
]).

-doc """
The removal teardown `terminate/2` runs for a deliberate stop
(`beamtalk_class_registry:is_deliberate_stop/1`): announce `ClassRemoved`
and drop the class's metadata, reverse-index, pg, loaded-class and
backing-module entries, so the class is gone from every registry.
""".
-spec remove_class_registration(beamtalk_object_class:class_name()) -> ok.
remove_class_registration(ClassName) ->
    remove_registration(ClassName, fun(Pid) ->
        beamtalk_class_registry:forget_class_pids(Pid, ClassName),
        beamtalk_class_registry:forget_loaded_class(ClassName, Pid),
        beamtalk_class_registry:forget_backing_module_entries(ClassName, Pid),
        %% Only this (terminating) process leaves the group. The dead-class
        %% path never does: pg drops dead members itself, and its caller
        %% may be an unrelated live class.
        pg:leave(beamtalk_classes, Pid)
    end).

-doc """
The same teardown as `remove_class_registration/1` for a class whose process
is already dead and cannot be restarted (BT-4646: a crashed module-less
`ClassBuilder` class). Runs in the caller, so the pid-keyed index rows are
dropped by liveness (`beamtalk_class_registry:forget_dead_incarnations/1`)
rather than by `self()`. Also runs the registry purge
(`beamtalk_class_lifecycle:class_removed/2`), tells the workspace layer as
`removeFromSystem` does, and flushes the derived-fact cache.
""".
-spec remove_dead_class_registration(beamtalk_object_class:class_name(), module()) ->
    ok | {alive, pid()}.
remove_dead_class_registration(ClassName, Module) ->
    %% Registration does not take the removal lock, so a class of the same name
    %% can be rebuilt while this runs (BT-4794). The metadata row is therefore
    %% deleted only while it still names the dead owner seen here
    %% (`delete_if_owner/2`, one atomic claim), and the pid-keyed index rows are
    %% dropped per dead pid, so a rebuilt class's rows are never touched.
    %% `ClassRemoved` is announced last so a subscriber that rebuilds the class
    %% in response cannot have its fresh rows deleted by this teardown.
    case beamtalk_class_registry:whereis_class(ClassName) of
        Pid when is_pid(Pid) ->
            {alive, Pid};
        undefined ->
            case beamtalk_class_metadata:lookup_owner(ClassName) of
                not_found ->
                    %% Already removed by someone else: nothing left to claim
                    %% or announce.
                    beamtalk_class_registry:forget_dead_incarnations(ClassName);
                {ok, Owner} ->
                    %% The name is registered before `init/1` writes the row,
                    %% so a rebuild between the lookup above and this read makes
                    %% its live pid the owner: only a dead owner is claimed.
                    case is_process_alive(Owner) of
                        true ->
                            %% Alive and registered: a live class. Alive but
                            %% not registered is a process on its way out
                            %% (`terminate/2` unregistered it and removes its
                            %% own rows): nothing to hand back or remove.
                            case beamtalk_class_registry:whereis_class(ClassName) of
                                LivePid when is_pid(LivePid) -> {alive, LivePid};
                                undefined -> ok
                            end;
                        false ->
                            remove_claimed_dead_class(ClassName, Module, Owner)
                    end
            end
    end.

-spec remove_claimed_dead_class(beamtalk_object_class:class_name(), module(), pid()) ->
    ok | {alive, pid()}.
remove_claimed_dead_class(ClassName, Module, Owner) ->
    case beamtalk_class_metadata:delete_if_owner(ClassName, Owner) of
        false ->
            %% The row names another owner (the class was rebuilt after the
            %% re-check), or is already gone (another teardown won; the dead
            %% owner's own dependents are dropped either way). Leave any
            %% rebuilt class registered and announce nothing: the `ClassRemoved`
            %% announcement belongs to the teardown that won the claim. The dead
            %% pid's index rows are kept: a holder of the old class object
            %% recovers through the reverse-index row (BT-4552), exactly as
            %% after `restart_compiled_class/4`; the stale loaded-class/backing
            %% rows are filtered by liveness.
            case beamtalk_class_registry:whereis_class(ClassName) of
                NewPid when is_pid(NewPid) -> {alive, NewPid};
                undefined -> ok
            end;
        true ->
            %% One more look before the name-keyed purges and the announcement:
            %% a rebuild that landed after the last teardown check must not be
            %% purged or announced as removed, nor lose its predecessor's
            %% reverse-index row (BT-4552 recovery).
            case beamtalk_class_registry:whereis_class(ClassName) of
                RebuiltPid when is_pid(RebuiltPid) ->
                    {alive, RebuiltPid};
                undefined ->
                    beamtalk_class_registry:forget_dead_incarnations(ClassName),
                    announce_dead_class_removed(ClassName, Module)
            end
    end.

-spec announce_dead_class_removed(beamtalk_object_class:class_name(), module()) -> ok.
announce_dead_class_removed(ClassName, Module) ->
    _ =
        (try
            beamtalk_class_lifecycle:class_removed(ClassName, Module)
        catch
            _:_ -> ok
        end),
    _ =
        (try
            beamtalk_behaviour_intrinsics:publish_class_removed(ClassName, Module)
        catch
            _:_ -> ok
        end),
    _ =
        (try
            beamtalk_class_derived_cache:invalidate()
        catch
            _:_ -> ok
        end),
    beamtalk_class_publish:announce_class_lifecycle('ClassRemoved', ClassName),
    ok.

-spec remove_registration(beamtalk_object_class:class_name(), fun((pid()) -> term())) -> ok.
remove_registration(ClassName, ForgetPidRows) ->
    %% ADR 0093 §2: Announce ClassRemoved on the system bus as the class
    %% process shuts down (e.g. removeFromSystem). The event carries only the
    %% class name, so it is independent of the metadata-row deletion below.
    beamtalk_class_publish:announce_class_lifecycle('ClassRemoved', ClassName),
    teardown_registration(ClassName, ForgetPidRows).

-spec teardown_registration(beamtalk_object_class:class_name(), fun((pid()) -> term())) -> ok.
teardown_registration(ClassName, ForgetPidRows) ->
    %% Single metadata row → single owner-scoped delete (BT-4808: never a
    %% rebuilt class's row); it also drops this process's class-method funs,
    %% method records, flags and subclass edge. Wrapped in try/catch to be
    %% safe during node shutdown when ETS/pg may be gone.
    _ =
        (try
            beamtalk_class_metadata:delete_if_owner(ClassName, self())
        catch
            _:_ -> ok
        end),
    %% Drop the pid reverse index, loaded-class and backing-module rows with
    %% the metadata row: the class is removed and its rows must not keep
    %% naming it. A crash keeps them for auto-restart recovery (BT-4552,
    %% BT-4622, `restart_class/1`). The pid-guarded forgets never clobber a
    %% replacement pid's row (a reload that already recorded one).
    _ =
        (try
            ForgetPidRows(self())
        catch
            _:_ -> ok
        end),
    ok.
