%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_facade_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
Tests for `beamtalk_workspace_facade`, the backing module of the `Workspace`
class-side facade (ADR 0129 §4): the `no_workspace` guard and `isAvailable/0`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-define(RUN, #{mode => run, include_compiler => false}).

%% Every guarded entry point, as {Fun, Args}.
guarded_entries() ->
    [
        {actors, []},
        {processes, []},
        {nodes, []},
        {actorAt, [<<"<0.1.0>">>]},
        {classes, []},
        {globals, []},
        {currentSession, []},
        {sessions, []},
        {sync, []},
        {changes, []},
        {load, [<<"x.bt">>]},
        {newClass, [<<"src">>, <<"p">>]},
        {moveClass, [nil, <<"p">>]},
        {flush, []},
        {flush, [nil]},
        {flush, [nil, false]},
        {flushIncludingDestructive, []},
        {recheckImage, []},
        {testScope, []},
        {testTarget, [nil]},
        {bind, [1, 'X']},
        {unbind, ['X']},
        {supervisor, []},
        {startSupervisor, [nil]},
        {stopSupervisor, [nil]},
        {supervisors, []},
        {autoflush, []},
        {autoflush, [false]},
        {dependencies, []}
    ].

guard_raises_no_workspace_when_unavailable_test_() ->
    [
        {atom_to_list(F) ++ "/" ++ integer_to_list(length(A)), fun() ->
            beamtalk_capability:clear(),
            ?assertError(
                #{error := #beamtalk_error{kind = no_workspace, class = 'Workspace'}},
                erlang:apply(beamtalk_workspace_facade, F, A)
            )
        end}
     || {F, A} <- guarded_entries()
    ].

test_helpers_pass_through_when_available_test() ->
    ok = beamtalk_capability:set(?RUN),
    try
        ?assertEqual(0, beamtalk_workspace_facade:testScope()),
        ?assertEqual(some_class, beamtalk_workspace_facade:testTarget(some_class))
    after
        beamtalk_capability:clear()
    end.

is_available_reflects_recorded_capabilities_test() ->
    beamtalk_capability:clear(),
    ?assertEqual(false, beamtalk_workspace_facade:isAvailable()),
    ok = beamtalk_capability:set(?RUN),
    try
        ?assertEqual(true, beamtalk_workspace_facade:isAvailable())
    after
        beamtalk_capability:clear()
    end,
    ?assertEqual(false, beamtalk_workspace_facade:isAvailable()).
