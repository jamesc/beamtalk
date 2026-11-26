%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_class_scope_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node EUnit tests for node-local class and REPL-shell groups (BT-5192).

The `beamtalk_classes` and `beamtalk_repl_shells` groups use `pg`'s default
scope, which `pg` synchronises with every connected node. Reads must take the
local members only:

1. `all_classes/0`, `live_class_entries/0` and `user_classes/0` on this node
   list none of the peer's class processes (before the fix they listed them and
   resolved each with a blocking `gen_server:call`).
2. A class removal published on this node does not reach a shell that joined
   `beamtalk_repl_shells` on the peer (before the fix it was broadcast to it).

Each test first waits until this node's `pg` has synced the peer's member, so a
pass cannot come from `pg` not having caught up yet.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

-export([remote_shell_loop/1]).

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5192_class_scope"),
    {ok, _Started} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

class_scope_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"class enumeration on this node excludes the peer's classes", fun() ->
                    class_enumeration_excludes_peer_classes(PeerNode)
                end},
                {"a class removal here is not broadcast to the peer's REPL shells", fun() ->
                    class_removal_does_not_reach_peer_shells(PeerNode)
                end}
            ]
        end}}.

%%====================================================================
%% 1. Class enumeration is node-local
%%====================================================================

class_enumeration_excludes_peer_classes(PeerNode) ->
    ClassName = 'BT5192PeerOnlyClass',
    ClassInfo = #{superclass => none, methods => #{}, class_methods => #{}},
    %% `start/2` puts the class under the peer's `beamtalk_class_sup`, so it
    %% outlives the rpc process.
    {ok, RemotePid} = rpc:call(PeerNode, beamtalk_object_class, start, [ClassName, ClassInfo]),
    try
        ?assertEqual(PeerNode, node(RemotePid)),
        %% Readiness: pg on this node has synced the peer's member.
        ok = beamtalk_test_wait:until(fun() ->
            lists:member(RemotePid, pg:get_members(beamtalk_classes))
        end),
        ?assertNot(lists:member(RemotePid, beamtalk_class_registry:all_classes())),
        ?assertEqual(
            [],
            [
                E
             || {_, _, Pid} = E <- beamtalk_class_registry:live_class_entries(),
                node(Pid) =/= node()
            ]
        ),
        ?assertEqual(
            [],
            [
                E
             || {beamtalk_object, _, _, Pid} = E <- beamtalk_class_registry:user_classes(),
                node(Pid) =/= node()
            ]
        ),
        %% And every member listed is this node's own.
        ?assertEqual(
            [], [P || P <- beamtalk_class_registry:all_classes(), node(P) =/= node()]
        )
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% 2. Class-removed broadcast is node-local
%%====================================================================

class_removal_does_not_reach_peer_shells(PeerNode) ->
    RemoteShell = rpc:call(PeerNode, erlang, spawn, [?MODULE, remote_shell_loop, [self()]]),
    ?assert(is_pid(RemoteShell)),
    ?awaitMessage({joined, RemoteShell}),
    try
        %% Readiness: pg on this node has synced the peer's shell.
        ok = beamtalk_test_wait:until(fun() ->
            lists:member(RemoteShell, pg:get_members(beamtalk_repl_shells))
        end),
        ok = beamtalk_behaviour_intrinsics:publish_class_removed(
            'BT5192RemovedClass', bt5192_removed_class
        ),
        %% Signals from one process to another arrive in send order, so the
        %% marker reaches the peer shell after any broadcast this process sent
        %% it; the shell forwards everything it gets, in order.
        RemoteShell ! marker,
        receive
            {forwarded, RemoteShell, marker} ->
                ok;
            {forwarded, RemoteShell, {class_removed, _, _} = Msg} ->
                erlang:error({class_removed_reached_peer_shell, Msg})
        after 5000 ->
            erlang:error(peer_shell_marker_timeout)
        end
    after
        RemoteShell ! stop
    end.

-doc "A stand-in REPL shell on the peer: joins the group, forwards every message.".
-spec remote_shell_loop(pid()) -> ok.
remote_shell_loop(Collector) ->
    ok = pg:join(beamtalk_repl_shells, self()),
    Collector ! {joined, self()},
    remote_shell_forward(Collector).

remote_shell_forward(Collector) ->
    receive
        stop ->
            ok;
        Msg ->
            Collector ! {forwarded, self(), Msg},
            remote_shell_forward(Collector)
    end.
