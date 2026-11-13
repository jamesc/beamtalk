%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_supervisor_errors_tests).
-moduledoc """
Pins the fields of every error kind the supervisor modules build (BT-4793):
kind, class, selector, message, hint and details, as built through the shared
`beamtalk_error` constructors.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HINT(B), {beamtalk_string, B}).

%% --- Shared formatted-text constructors -------------------------------------

new_fmt_test() ->
    E = beamtalk_error:new_fmt(runtime_error, 'C', 'sel', "got ~tp and ~ts", [1, "λ"]),
    ?assertEqual(runtime_error, E#beamtalk_error.kind),
    ?assertEqual('C', E#beamtalk_error.class),
    ?assertEqual('sel', E#beamtalk_error.selector),
    ?assertEqual(
        beamtalk_error:new(runtime_error, 'C', 'sel', <<"got 1 and λ"/utf8>>), E
    ).

with_message_fmt_test() ->
    E0 = beamtalk_error:new(child_not_found, 'S', 'x'),
    E = beamtalk_error:with_message_fmt(E0, "No child ~tp", [a]),
    ?assertEqual(beamtalk_error:with_message(E0, <<"No child a">>), E).

bad_value_test() ->
    E = beamtalk_error:bad_value('C', 'sel', "bad ~tp", [7], 7),
    ?assertEqual(type_error, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"bad 7">>), E#beamtalk_error.hint),
    ?assertEqual(#{value => 7}, E#beamtalk_error.details).

reserved_name_test() ->
    E = beamtalk_error:reserved_name('C', 'spawnAs:', 'Transcript'),
    ?assertEqual(reserved_name, E#beamtalk_error.kind),
    ?assertEqual('spawnAs:', E#beamtalk_error.selector),
    ?assertEqual(
        ?HINT(<<"Cannot start a supervisor under reserved name 'Transcript'">>),
        E#beamtalk_error.hint
    ).

name_registered_test() ->
    E = beamtalk_error:name_registered('C', 'spawnAs:', n),
    ?assertEqual(name_registered, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"Name 'n' is already registered">>), E#beamtalk_error.hint).

%% --- Supervisor error kinds --------------------------------------------------

child_not_found_test() ->
    E = beamtalk_supervisor:child_not_found_error('S', 'childWithId:', a),
    ?assertEqual(child_not_found, E#beamtalk_error.kind),
    ?assertEqual('S', E#beamtalk_error.class),
    ?assertEqual('childWithId:', E#beamtalk_error.selector),
    ?assertEqual(?HINT(<<"No child with id a">>), E#beamtalk_error.message),
    ?assertEqual(#{id => a}, E#beamtalk_error.details).

child_running_test() ->
    E = beamtalk_supervisor:child_running_error('S', 'deleteChild:', a, running),
    ?assertEqual(child_running, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"Child a is running">>), E#beamtalk_error.message),
    ?assertEqual(?HINT(<<"Terminate the child with terminateChild: first">>), E#beamtalk_error.hint),
    ?assertEqual(#{id => a, status => running}, E#beamtalk_error.details).

ambiguous_child_test() ->
    E = beamtalk_supervisor:ambiguous_child_error(
        'S', 'terminateChild:', [{b, undefined, worker, []}, {a, undefined, worker, []}]
    ),
    ?assertEqual(ambiguous_child, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"Several children share this class: [a,b]">>), E#beamtalk_error.message),
    ?assertEqual(#{ids => [a, b]}, E#beamtalk_error.details).

delete_failed_test() ->
    E = beamtalk_supervisor:delete_failed_error('S', 'deleteChild:', running),
    ?assertEqual(delete_failed, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"running">>), E#beamtalk_error.hint).

stop_timeout_test() ->
    E = beamtalk_supervisor:stop_timeout_error('S', 50),
    ?assertEqual(timeout, E#beamtalk_error.kind),
    ?assertEqual('stopWithin:', E#beamtalk_error.selector),
    ?assertEqual(
        ?HINT(<<"Supervisor did not stop within 50 ms; it keeps shutting down in the background">>),
        E#beamtalk_error.hint
    ),
    ?assertEqual(#{timeout_ms => 50}, E#beamtalk_error.details).

check_stop_timeout_test() ->
    {error, E} = beamtalk_supervisor:check_stop_timeout('S', -1),
    ?assertEqual(type_error, E#beamtalk_error.kind),
    ?assertEqual('stopWithin:', E#beamtalk_error.selector),
    ?assertEqual(#{value => -1}, E#beamtalk_error.details).

check_integer_test() ->
    {error, E} = beamtalk_supervisor:check_integer('S', maxRestarts, x, 0),
    ?assertEqual(type_error, E#beamtalk_error.kind),
    ?assertEqual(maxRestarts, E#beamtalk_error.selector),
    ?assertEqual(?HINT(<<"maxRestarts must be an Integer >= 0, got x">>), E#beamtalk_error.hint),
    ?assertEqual(#{value => x}, E#beamtalk_error.details).

check_strategy_test() ->
    {error, E} = beamtalk_supervisor:check_strategy('S', nope),
    ?assertEqual(type_error, E#beamtalk_error.kind),
    ?assertEqual(strategy, E#beamtalk_error.selector),
    ?assertEqual(
        ?HINT(<<"strategy must be #oneForOne, #oneForAll or #restForOne, got nope">>),
        E#beamtalk_error.hint
    ),
    ?assertEqual(#{value => nope}, E#beamtalk_error.details).

max_children_test() ->
    {error, E} = beamtalk_supervisor_capacity:max_children_error('S', 'startChild:', 3),
    ?assertEqual(max_children, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"DynamicSupervisor is at its maxChildren limit (3)">>), E#beamtalk_error.message),
    ?assertEqual(?HINT(<<"Back off and retry, or free a slot with terminateChild:">>), E#beamtalk_error.hint),
    ?assertEqual(#{max_children => 3}, E#beamtalk_error.details).

check_max_children_test() ->
    {error, E} = beamtalk_supervisor_capacity:check_max_children('S', lots),
    ?assertEqual(type_error, E#beamtalk_error.kind),
    ?assertEqual(maxChildren, E#beamtalk_error.selector),
    ?assertEqual(#{value => lots}, E#beamtalk_error.details).

unavailable_node_down_test() ->
    Self = {beamtalk_supervisor, 'S', mod, self()},
    {error, E} = beamtalk_supervisor:supervisor_unavailable_error(
        Self, 'count', {unreachable, nodedown}
    ),
    ?assertEqual(node_down, E#beamtalk_error.kind),
    ?assertEqual(
        ?HINT(<<"Supervisor's node ", (atom_to_binary(node()))/binary, " is unreachable (nodedown)">>),
        E#beamtalk_error.message
    ),
    ?assertEqual(
        ?HINT(<<"The supervisor may still be running; retry when its node is reachable">>),
        E#beamtalk_error.hint
    ),
    ?assertEqual(#{node => node(), reason => nodedown}, E#beamtalk_error.details).

unavailable_stale_test() ->
    Self = {beamtalk_supervisor, 'S', mod, self()},
    {error, E} = beamtalk_supervisor:supervisor_unavailable_error(Self, 'count', dead),
    ?assertEqual(stale_handle, E#beamtalk_error.kind),
    ?assertEqual(?HINT(<<"supervisor is not running; the handle is stale">>), E#beamtalk_error.hint).
