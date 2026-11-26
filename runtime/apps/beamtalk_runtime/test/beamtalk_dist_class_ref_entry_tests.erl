%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_class_ref_entry_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node EUnit tests for class objects that reach a node **without** crossing
the wire codec (BT-5274, ADR 0126 §5.1/§5.5).

`beamtalk_wire:decode/1` resolves a class object from another node to the
receiving node's class of the same name. A raw FFI result, an `rpc`/`erpc`
result or a raw message skips that walk and can carry the peer's class pid.

1. **FFI:** an Erlang function that hands back a class object it got from the
   peer in a raw message answers, through the FFI boundary
   (`beamtalk_erlang_proxy`), this node's class.
2. **rpc:** `rpc:call/4` and `erpc:call/4` into the peer, through the same
   boundary, answer this node's class.
3. An `{ok, ClassObj}` answer becomes a `Result` holding this node's class.
4. Either answers `class_not_found` when the class is not registered here.
5. **Send:** a peer class object that was not normalised (nested in an FFI
   result) is resolved by name when it is sent a message, so the send runs
   against this node's class and never calls the peer's class process. A bare
   peer class pid handed to `class_send` is refused. Re-encoding such an object
   for the wire, and using it as a supervisor child, do not call the peer
   either.

Every send test first suspends the peer's class process (`sys:suspend/1`): a
`gen_server:call` into it would block and time out, so a pass cannot come from
the peer answering, and the peer's empty message queue afterwards proves that
nothing was sent to it.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

-export([peer_class_by_raw_message/2, send_class_object/2, ok_class_object/1]).

-define(HELPER, beamtalk_class_dispatch_test_helper).

%% A send that wrongly goes to the suspended peer class process fails with a
%% timeout after this long instead of hanging for the default 60 s.
-define(SEND_TIMEOUT_MS, 2000).

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt5274_class_ref"),
    {ok, _Started} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

class_ref_entry_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"FFI: a peer class object from a raw message is this node's class", fun() ->
                    with_classes(PeerNode, fun ffi_raw_message_is_local/3)
                end},
                {"rpc: rpc:call into the peer answers this node's class", fun() ->
                    with_classes(PeerNode, fun rpc_call_is_local/3)
                end},
                {"rpc: erpc:call into the peer answers this node's class", fun() ->
                    with_classes(PeerNode, fun erpc_call_is_local/3)
                end},
                {"rpc: an {ok, ClassObj} answer is a Result of this node's class", fun() ->
                    with_classes(PeerNode, fun erpc_ok_result_is_local/3)
                end},
                {"FFI and rpc: a class not registered here is class_not_found", fun() ->
                    peer_only_class_is_class_not_found(PeerNode)
                end},
                {"send: a nested peer class object runs on this node's class", fun() ->
                    with_classes(PeerNode, fun nested_peer_ref_send_is_local/3)
                end},
                {"send: a peer-only class object is class_not_found, not a remote call", fun() ->
                    peer_only_class_send_is_class_not_found(PeerNode)
                end},
                {"send: a bare peer class pid is refused, not called", fun() ->
                    with_classes(PeerNode, fun bare_peer_pid_is_refused/3)
                end},
                {"wire: re-encoding a peer class object does not call the peer", fun() ->
                    with_classes(PeerNode, fun reencode_peer_ref_is_by_tag/3)
                end},
                {"supervisor: a peer child class is this node's class", fun() ->
                    with_classes(PeerNode, fun supervisor_child_peer_ref_is_local/3)
                end}
            ]
        end}}.

%%====================================================================
%% Fixture
%%====================================================================

class_info() ->
    #{
        superclass => none,
        module => ?HELPER,
        class_methods => #{testClassSelf => #{arity => 0}},
        meta => #{process_bound_class_selectors => [new, spawn]}
    }.

%% The same class registered on this node and on the peer, each with its own
%% class process.
with_classes(PeerNode, Test) ->
    Name = beamtalk_test_unique:atom("BT5274Class"),
    LocalPid = beamtalk_test_class:start(Name, class_info()),
    {ok, PeerPid} = rpc:call(PeerNode, beamtalk_object_class, start, [Name, class_info()]),
    try
        ?assertEqual(PeerNode, node(PeerPid)),
        Test(Name, LocalPid, PeerPid)
    after
        beamtalk_test_class:stop(LocalPid),
        rpc:call(PeerNode, gen_server, stop, [PeerPid])
    end.

%% A class registered on the peer only.
with_peer_only_class(PeerNode, Test) ->
    Name = beamtalk_test_unique:atom("BT5274PeerOnly"),
    {ok, PeerPid} = rpc:call(PeerNode, beamtalk_object_class, start, [Name, class_info()]),
    try
        ?assertEqual(undefined, beamtalk_class_registry:whereis_class(Name)),
        Test(Name, PeerPid)
    after
        rpc:call(PeerNode, gen_server, stop, [PeerPid])
    end.

peer_class_object(Name, PeerPid) ->
    #beamtalk_object{
        class = beamtalk_class_registry:class_object_tag(Name), class_mod = ?HELPER, pid = PeerPid
    }.

%% Run `Fun` with the peer's class process suspended, then check that nothing
%% was sent to it.
with_peer_suspended(PeerPid, Fun) ->
    PeerNode = node(PeerPid),
    ok = rpc:call(PeerNode, sys, suspend, [PeerPid]),
    try
        Fun(),
        ?assertEqual(
            {message_queue_len, 0},
            rpc:call(PeerNode, erlang, process_info, [PeerPid, message_queue_len])
        )
    after
        ok = rpc:call(PeerNode, sys, resume, [PeerPid])
    end.

-doc """
The raw-FFI shape: an Erlang function that receives a class object from the
peer in a plain message (no wire codec) and returns it.
""".
-spec peer_class_by_raw_message(node(), atom()) -> term().
peer_class_by_raw_message(PeerNode, Name) ->
    Pid = spawn(PeerNode, ?MODULE, send_class_object, [self(), Name]),
    receive
        {class_object, Pid, Obj} -> Obj
    after 10000 -> erlang:error(peer_class_object_timeout)
    end.

-doc "Runs on the peer: send that node's class object for `Name` to `To`.".
-spec send_class_object(pid(), atom()) -> term().
send_class_object(To, Name) ->
    To ! {class_object, self(), beamtalk_class_registry:resolve_class_object(Name)}.

-doc "Runs on the peer: `{ok, ClassObj}` for that node's class `Name`.".
-spec ok_class_object(atom()) -> {ok, term()}.
ok_class_object(Name) ->
    {ok, beamtalk_class_registry:resolve_class_object(Name)}.

%% What an inline `(Erlang Module) fn: …` FFI send runs (stock-mode module).
ffi(Module, Fun, Args) ->
    beamtalk_erlang_proxy:direct_call(Module, Fun, Args, stock).

assert_local_class(Name, LocalPid, Obj) ->
    ?assertMatch(#beamtalk_object{}, Obj),
    ?assertEqual(node(), node(Obj#beamtalk_object.pid)),
    ?assertEqual(LocalPid, Obj#beamtalk_object.pid),
    ?assertEqual(beamtalk_class_registry:resolve_class_object(Name), Obj).

%%====================================================================
%% 1-3. FFI and rpc results
%%====================================================================

ffi_raw_message_is_local(Name, LocalPid, PeerPid) ->
    %% The raw message really carries the peer's pid.
    Raw = peer_class_by_raw_message(node(PeerPid), Name),
    ?assertEqual(PeerPid, Raw#beamtalk_object.pid),
    assert_local_class(
        Name, LocalPid, ffi(?MODULE, peer_class_by_raw_message, [node(PeerPid), Name])
    ).

rpc_call_is_local(Name, LocalPid, PeerPid) ->
    Args = [node(PeerPid), beamtalk_class_registry, resolve_class_object, [Name]],
    ?assertEqual(PeerPid, (apply(rpc, call, Args))#beamtalk_object.pid),
    assert_local_class(Name, LocalPid, ffi(rpc, call, Args)).

erpc_call_is_local(Name, LocalPid, PeerPid) ->
    Args = [node(PeerPid), beamtalk_class_registry, resolve_class_object, [Name]],
    ?assertEqual(PeerPid, (apply(erpc, call, Args))#beamtalk_object.pid),
    assert_local_class(Name, LocalPid, ffi(erpc, call, Args)).

erpc_ok_result_is_local(Name, LocalPid, PeerPid) ->
    Args = [node(PeerPid), ?MODULE, ok_class_object, [Name]],
    {ok, Raw} = apply(erpc, call, Args),
    ?assertEqual(PeerPid, Raw#beamtalk_object.pid),
    Result = ffi(erpc, call, Args),
    ?assertMatch(#{'isOk' := true}, Result),
    assert_local_class(Name, LocalPid, maps:get('okValue', Result)).

peer_only_class_is_class_not_found(PeerNode) ->
    with_peer_only_class(PeerNode, fun(Name, PeerPid) ->
        ?assertBtError(
            class_not_found, Name, ffi(?MODULE, peer_class_by_raw_message, [PeerNode, Name])
        ),
        ?assertBtError(
            class_not_found,
            Name,
            ffi(erpc, call, [PeerNode, beamtalk_class_registry, resolve_class_object, [Name]])
        ),
        %% A supervisor child: the error names the `supervisionSpec` send.
        ?assertBtError(
            class_not_found,
            Name,
            supervisionSpec,
            beamtalk_supervisor_spec:build_child_spec(peer_class_object(Name, PeerPid), static)
        ),
        ?assertBtError(
            class_not_found,
            Name,
            beamtalk_supervisor_spec:spec_to_otp(
                supervision_spec(peer_class_object(Name, PeerPid)), static
            )
        ),
        %% A DynamicSupervisor `childClass` keeps its init-config contract: an
        %% exit with the structured error, which `supervisor_start_failed` reports.
        ?assertExit(
            #beamtalk_error{kind = class_not_found, class = Name},
            beamtalk_supervisor_init_config:child_class_entry(
                sup, peer_class_object(Name, PeerPid)
            )
        )
    end).

%%====================================================================
%% 4. Sends never call a peer's class process
%%====================================================================

nested_peer_ref_send_is_local(Name, LocalPid, PeerPid) ->
    %% Nested in a returned list, the FFI boundary (shallow) leaves the peer's
    %% pid in place; the send resolves it.
    [PeerRef] = ffi(lists, reverse, [[peer_class_object(Name, PeerPid)]]),
    ?assertEqual(PeerPid, PeerRef#beamtalk_object.pid),
    with_peer_suspended(PeerPid, fun() ->
        %% `testClassSelf` answers the receiver the class method ran with.
        assert_local_class(
            Name,
            LocalPid,
            beamtalk_message_dispatch:send(PeerRef, testClassSelf, [], ?SEND_TIMEOUT_MS)
        ),
        assert_local_class(
            Name, LocalPid, beamtalk_message_dispatch:send(PeerRef, testClassSelf, [])
        )
    end).

peer_only_class_send_is_class_not_found(PeerNode) ->
    with_peer_only_class(PeerNode, fun(Name, PeerPid) ->
        PeerRef = peer_class_object(Name, PeerPid),
        with_peer_suspended(PeerPid, fun() ->
            ?assertBtError(
                class_not_found,
                Name,
                testClassSelf,
                beamtalk_message_dispatch:send(PeerRef, testClassSelf, [], ?SEND_TIMEOUT_MS)
            )
        end)
    end).

bare_peer_pid_is_refused(_Name, _LocalPid, PeerPid) ->
    with_peer_suspended(PeerPid, fun() ->
        ?assertBtError(
            class_not_found,
            unknown,
            testClassSelf,
            beamtalk_class_dispatch:class_send(PeerPid, testClassSelf, [], ?SEND_TIMEOUT_MS)
        )
    end).

reencode_peer_ref_is_by_tag(Name, _LocalPid, PeerPid) ->
    %% Nested in a list, so the FFI boundary leaves the peer's pid in place.
    [PeerRef] = ffi(lists, reverse, [[peer_class_object(Name, PeerPid)]]),
    with_peer_suspended(PeerPid, fun() ->
        ?assertEqual({ok, {'$beamtalk_class_ref', Name}}, beamtalk_wire:encode(PeerRef))
    end).

supervisor_child_peer_ref_is_local(Name, LocalPid, PeerPid) ->
    PeerRef = peer_class_object(Name, PeerPid),
    with_peer_suspended(PeerPid, fun() ->
        %% The child's module and class are this node's.
        ?assertEqual(
            {Name, ?HELPER}, beamtalk_supervisor_init_config:child_class_entry(sup, PeerRef)
        ),
        %% The `supervisionSpec` send reaches this node's class, which does
        %% not define it, rather than the (suspended) peer's.
        ?assertBtError(
            does_not_understand,
            Name,
            supervisionSpec,
            beamtalk_supervisor_spec:build_child_spec(PeerRef, static)
        ),
        ?assert(is_process_alive(LocalPid)),
        %% A `SupervisionSpec` whose `start` names the peer's class: the child
        %% spec is built against this node's class, never asking the peer.
        ?assertMatch(
            #{id := 1}, beamtalk_supervisor_spec:spec_to_otp(supervision_spec(PeerRef), static)
        )
    end).

%% The Erlang shape of a `SupervisionSpec` whose `start` is
%% `#[ChildClass, #spawn, #()]`.
supervision_spec(ChildClass) ->
    #{
        id => 1,
        start => #{
            ?BT_CLASS_TAG_KEY => 'Array',
            data => #{0 => ChildClass, 1 => spawn, 2 => []}
        },
        restart => permanent,
        shutdown => 5000,
        type => worker
    }.
