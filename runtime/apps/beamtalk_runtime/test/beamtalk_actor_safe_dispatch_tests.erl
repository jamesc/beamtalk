%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_safe_dispatch_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Cross-process `Self` identity for a compiled actor's `safe_dispatch/4`
(BT-3692 follow-up to jamesc/beamtalk#4188).

Contract (see `gen_server/dispatch.rs`, `generate_safe_dispatch`):

- `safe_dispatch/4 (Selector, Args, Self, State)` hands the caller-supplied
  `Self` to the method verbatim. It never rebuilds it, so the `Self` the
  method sees is the supplied `#beamtalk_object{}` whichever process happens
  to execute the call (an open self-send in the actor, or a block that
  captured `Self` and runs in a Timer/foreign process).
- `safe_dispatch/3 (Selector, Args, State)` is for the gen_server entry
  points only, which always run in the actor's own process. It derives
  `Self` with `beamtalk_actor:make_self/1`, which uses `self()`, so its
  identity follows the executing process. That is why the open self-send call
  site must use `/4`: a `/3` call from a foreign process would hand the
  method a `Self` whose pid is the foreign process, not the actor.

Uses `test_fixtures/self_identity_actor.bt`, real compiled `.bt` source.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(MOD, 'bt@self_identity_actor').

%%====================================================================
%% Fixture
%%====================================================================

setup() ->
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(self_identity_actor, 'SelfIdentityActor'),
    Object = ?MOD:spawn(),
    {Object, Object#beamtalk_object.pid}.

cleanup({_Object, Pid}) ->
    gen_server:stop(Pid).

safe_dispatch_self_identity_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun({Object, Pid}) ->
        [
            {"actor-process open self-send sees the actor's own Self", fun() ->
                {ok, Result} = gen_server:call(Pid, {viaSelfSend, []}),
                ?assertEqual(Object, Result)
            end},
            {"safe_dispatch/4 run in a foreign process keeps the supplied Self", fun() ->
                State = sys:get_state(Pid),
                {Foreign, {reply, SelfSeen, _}} =
                    run_in_foreign(fun() -> ?MOD:safe_dispatch(whoAmI, [], Object, State) end),
                ?assertEqual(Object, SelfSeen),
                ?assertEqual(Pid, SelfSeen#beamtalk_object.pid),
                ?assertNotEqual(Foreign, SelfSeen#beamtalk_object.pid)
            end},
            {"safe_dispatch/4 in the actor process and in a foreign one agree on Self", fun() ->
                State = sys:get_state(Pid),
                {_Foreign, {reply, FromForeign, _}} =
                    run_in_foreign(fun() -> ?MOD:safe_dispatch(whoAmI, [], Object, State) end),
                {ok, FromActor} = gen_server:call(Pid, {whoAmI, []}),
                ?assertEqual(FromActor, FromForeign)
            end},
            {"safe_dispatch/3 derives Self from the executing process (entry points only)", fun() ->
                State = sys:get_state(Pid),
                {Foreign, {reply, SelfSeen, _}} =
                    run_in_foreign(fun() -> ?MOD:safe_dispatch(whoAmI, [], State) end),
                ?assertEqual(Foreign, SelfSeen#beamtalk_object.pid)
            end}
        ]
    end}.

-doc "Run `Fun` in a fresh process; return `{ForeignPid, Result}`.".
run_in_foreign(Fun) ->
    Caller = self(),
    Foreign = spawn_link(fun() -> Caller ! {self(), Fun()} end),
    receive
        {Foreign, Result} -> {Foreign, Result}
    after 5000 -> error(timeout)
    end.
