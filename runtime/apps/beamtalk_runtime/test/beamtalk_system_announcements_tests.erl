%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_system_announcements_tests).

-moduledoc """
EUnit tests for the system Announcement subclasses + runtime emit points
(BT-2445 / ADR 0093 Phase 3 §2).

Covers the acceptance criterion "subscribing to each system event receives it on
the triggering action":

* `beamtalk_announcements:system_announce/2` builds the typed `Value` payload and
  delivers it to a (fun-handler) subscriber for every well-known system event
  class — `ActorSpawned`/`ActorStopped`, `ClassLoaded`/`ClassRemoved`,
  `BindingChanged`, `SupervisionChildAdded`/`SupervisionChildCrashed`.
* `ClassLoaded` fires from `beamtalk_object_class` init after the metadata row is
  written (announce-after-commit), and `ClassRemoved` fires when the class
  process terminates — both verified against a real class gen_server.

The actor spawn/stop emit points are exercised end-to-end by the BUnit suite
`stdlib/test/system_announcements_test.bt` (they route through generated `init/1`
codegen, which EUnit cannot drive directly).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    %% pg backs class-registry membership used by beamtalk_object_class.
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    %% The announcements bus: reuse a supervised instance or stand one up.
    case whereis(beamtalk_announcements) of
        undefined -> {ok, _} = beamtalk_announcements:start_link();
        _ -> ok
    end,
    clear_subscriptions(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_methods_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_class_registry:ensure_loaded_classes_table(),
    ok.

teardown(_) ->
    clear_subscriptions(),
    ok.

clear_subscriptions() ->
    try
        ets:delete_all_objects(beamtalk_announcement_subs),
        ets:delete_all_objects(beamtalk_announcement_by_class)
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%% Subscribe `self()` to `EventClass` with a fun handler that forwards the
%% received event payload back to the test process. Returns the SubRef.
subscribe_self(EventClass) ->
    Collector = self(),
    Handler = fun(Event) -> Collector ! {got, EventClass, Event} end,
    {ok, SubRef} = beamtalk_announcements:subscribe(EventClass, self(), Handler, false),
    SubRef.

%% Block until the system event for `EventClass` is delivered, returning the
%% payload map; fail after a timeout.
expect_event(EventClass) ->
    receive
        {got, EventClass, Event} -> Event
    after 1000 ->
        ?assert(false)
    end.

%%====================================================================
%% system_announce/2 delivers each well-known system event
%%====================================================================

system_announce_delivers_each_event_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(check_system_event('ActorSpawned', #{actorClass => 'Counter'})),
            ?_test(
                check_system_event('ActorStopped', #{actorClass => 'Counter', reason => normal})
            ),
            ?_test(check_system_event('ClassLoaded', #{className => 'Counter'})),
            ?_test(check_system_event('ClassRemoved', #{className => 'Counter'})),
            ?_test(check_system_event('BindingChanged', #{name => x, value => 5})),
            ?_test(
                check_system_event('SupervisionChildAdded', #{
                    supervisor => 'Sup', childClass => 'Worker'
                })
            ),
            ?_test(
                check_system_event('SupervisionChildCrashed', #{
                    supervisor => 'Sup', childClass => 'Worker', reason => child_start_failed
                })
            )
        ]
    end}.

%% Subscribe, announce via system_announce/2, and assert the payload arrives with
%% the `$beamtalk_class` tag set to EventClass plus the supplied fields.
check_system_event(EventClass, Fields) ->
    _SubRef = subscribe_self(EventClass),
    ok = beamtalk_announcements:system_announce(EventClass, Fields),
    Event = expect_event(EventClass),
    ?assertEqual(EventClass, maps:get('$beamtalk_class', Event)),
    maps:foreach(
        fun(K, V) -> ?assertEqual(V, maps:get(K, Event)) end,
        Fields
    ).

%% A subscriber to one event class does not receive a different one.
system_announce_is_type_targeted_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            _SubRef = subscribe_self('ClassRemoved'),
            ok = beamtalk_announcements:system_announce('ClassLoaded', #{className => 'Counter'}),
            receive
                {got, 'ClassRemoved', _} -> ?assert(false)
            after 200 ->
                ok
            end
        end)
    end}.

%%====================================================================
%% ClassLoaded / ClassRemoved fire on the real class lifecycle
%%====================================================================

class_loaded_fires_on_start_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            _SubRef = subscribe_self('ClassLoaded'),
            ClassInfo = #{
                name => 'SysAnnLoadedClass',
                module => test_class,
                superclass => 'Object',
                instance_methods => #{},
                instance_variables => []
            },
            {ok, Pid} = beamtalk_object_class:start_link('SysAnnLoadedClass', ClassInfo),
            Event = expect_event('ClassLoaded'),
            ?assertEqual('SysAnnLoadedClass', maps:get(className, Event)),
            %% Announce-after-commit: the metadata row exists by delivery time.
            ?assertMatch({ok, _}, beamtalk_class_metadata:lookup_module('SysAnnLoadedClass')),
            gen_server:stop(Pid)
        end)
    end}.

class_removed_fires_on_stop_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            ClassInfo = #{
                name => 'SysAnnRemovedClass',
                module => test_class,
                superclass => 'Object',
                instance_methods => #{},
                instance_variables => []
            },
            {ok, Pid} = beamtalk_object_class:start_link('SysAnnRemovedClass', ClassInfo),
            %% Subscribe only now, so the start-time ClassLoaded is not in our mailbox.
            _SubRef = subscribe_self('ClassRemoved'),
            gen_server:stop(Pid),
            Event = expect_event('ClassRemoved'),
            ?assertEqual('SysAnnRemovedClass', maps:get(className, Event))
        end)
    end}.
