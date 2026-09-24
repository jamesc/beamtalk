%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_wire_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node wire integration suite (ADR 0126 Phase 3b, BT-3601).

Exercises the `'\$beamtalk_wire'` tag end to end — sender-side encode,
`gen_server:call`/`cast` across a real `peer` node (BT-3578 harness,
`beamtalk_dist_test_helper`), the receiver's `handle_call`/`handle_cast`
prelude, and the reply/future-resolution encode path — rather than
`beamtalk_wire`'s own encode/decode unit tests (`beamtalk_wire_tests.erl`,
BT-3600) or the raw block/NLR/future mechanics the Phase 0.5 spike already
proved (`beamtalk_dist_wirecheck_tests.erl`, BT-3579/BT-3582).

Two of the ADR's required scenarios — NLR relay over a sync call, and async
future resolution/rejection across nodes — are **not** duplicated here:
every remote send this issue's sender-side changes make now goes through
the `'\$beamtalk_wire'` tag automatically (decided purely by
`node(ActorPid) =/= node()`, ADR 0126 §5.1), so
`beamtalk_dist_wirecheck_tests.erl`'s existing (c) and (d) scenarios already
exercise the real wire path once this issue lands — re-running that
unmodified suite is the regression check for both, not a second copy of the
same assertions here.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([
    ensure_fixture_loaded/2,
    install_meta_stub/2,
    restore_meta_stub/1
]).

%%====================================================================
%% Shared fixture
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3601_wire"),
    ok = ensure_fixture_loaded(shape_point, 'ShapePoint'),
    ok = ensure_fixture_loaded(shape_chain_cart, 'ShapeChainCart'),
    ok = rpc:call(PeerNode, ?MODULE, ensure_fixture_loaded, [shape_point, 'ShapePoint']),
    ok = rpc:call(PeerNode, ?MODULE, ensure_fixture_loaded, [shape_chain_cart, 'ShapeChainCart']),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc "A short, run-unique atom — repeated `just test` runs never collide on a stale name.".
unique_name(Prefix) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))).

-doc """
Load and register a `test_fixtures/`-compiled class if it is not already
registered — idempotent, the same convention
`beamtalk_shape_migration_tests.erl`/`beamtalk_wire_tests.erl` each already
carry their own copy of (their own comments call out the precedent
directly). Runs on whichever node calls it — used both locally and via
`rpc:call/4` on the peer, since each node's `beamtalk_class_registry` is its
own, node-local table.
""".
ensure_fixture_loaded(Basename, ClassName) ->
    Module = list_to_atom("bt@" ++ atom_to_list(Basename)),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    case erlang:function_exported(Module, register_class, 0) of
                        true ->
                            Module:register_class(),
                            ok;
                        false ->
                            ok
                    end;
                _Pid ->
                    ok
            end;
        {error, Reason} ->
            error({fixture_module_not_found, Basename, Reason})
    end.

wire_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"a Value nested inside Array/Dictionary round-trips across the wire", fun() ->
                    values_in_array_and_dictionary_round_trip(PeerNode)
                end},
                {"an Ets nested inside an Array argument is rejected, sender-side, naming the path",
                    fun() -> ets_nested_in_array_rejected(PeerNode) end},
                {"an unassigned late slot travels as absent and raises on first read", fun() ->
                    late_slot_absent_raises_on_first_read(PeerNode)
                end},
                {"version-ahead on a request is refused before the callee's state changes", fun() ->
                        version_ahead_on_request_refused_before_dispatch(PeerNode)
                    end},
                {"version-ahead on a reply carries direction => reply", fun() ->
                    version_ahead_on_reply_carries_direction(PeerNode)
                end},
                {"an older envelope migrates forward through a migrateFromVN: chain", fun() ->
                    migrate_forward_through_wire(PeerNode)
                end},
                {"remote_code_mismatch: a block whose module is missing on the callee (undef)",
                    fun() -> remote_code_mismatch_undef(PeerNode) end},
                {"remote_code_mismatch: a block whose module is a different version (badfun)",
                    fun() -> remote_code_mismatch_badfun(PeerNode) end},
                {
                    "async future rejection double-wrap: a method that calls "
                    "beamtalk_error:raise/1 on itself rejects with the wrapped map buried in "
                    "details.original_reason, not the bare record",
                    fun() -> async_future_rejection_double_wrap(PeerNode) end
                }
            ]
        end}}.

%%====================================================================
%% Values nested inside Array/Dictionary round-trip (ADR 0126 §5.1)
%%====================================================================

values_in_array_and_dictionary_round_trip(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        Point = #{'$beamtalk_class' => 'ShapePoint', x => 1, y => 2},
        ExpectedPoint = #{
            '$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 1, y => 2
        },

        Array = beamtalk_array:from_list([Point]),
        EchoedArray = beamtalk_actor:sync_send(RemotePid, 'echo:', [Array]),
        ?assertEqual(#{'$beamtalk_class' => 'Array', data => #{0 => ExpectedPoint}}, EchoedArray),

        Dict = #{label => Point},
        EchoedDict = beamtalk_actor:sync_send(RemotePid, 'echo:', [Dict]),
        ?assertEqual(#{label => ExpectedPoint}, EchoedDict)
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Ets nested inside an Array is rejected, sender-side (ADR 0126 §5.4)
%%====================================================================

ets_nested_in_array_rejected(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [7]),
    Table = unique_name("bt3601_ets_in_array"),
    Ets = beamtalk_ets:new(Table, set),
    try
        Array = beamtalk_array:from_list([Ets]),
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = not_serialisable, class = 'Ets'}
            },
            beamtalk_actor:sync_send(RemotePid, 'echo:', [Array])
        ),
        %% Sender-side encode failure: the network was never touched, so the
        %% actor's state is exactly as it was at spawn.
        ?assertEqual(7, beamtalk_actor:sync_send(RemotePid, getValue, []))
    after
        beamtalk_ets:deleteTable(Ets),
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Late slot travels as absent (ADR 0126 §5.4)
%%====================================================================

late_slot_absent_raises_on_first_read(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = uninitialized_state_error, class = 'WireActor'}
            },
            beamtalk_actor:sync_send(RemotePid, lateOp, [])
        ),
        %% Assign it, sending a Value through the same absent-then-present
        %% slot — then the read answers normally.
        Point = #{'$beamtalk_class' => 'ShapePoint', x => 5, y => 6},
        Assigned = beamtalk_actor:sync_send(RemotePid, 'setLate:', [Point]),
        ?assertMatch(#{'$beamtalk_class' := 'ShapePoint'}, Assigned),
        Read = beamtalk_actor:sync_send(RemotePid, lateOp, []),
        ?assertMatch(#{'$beamtalk_class' := 'ShapePoint', x := 5, y := 6}, Read)
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Version skew — request direction (ADR 0126 §5.2)
%%====================================================================

version_ahead_on_request_refused_before_dispatch(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [42]),
    try
        %% ShapePoint's real, unmocked, identical-on-both-nodes shapeVersion
        %% is 1 — an envelope at version 99 is ahead everywhere, with no
        %% per-node meta override needed to prove the refusal.
        TooNew = {beamtalk_shape, 'ShapePoint', 99, #{x => 1, y => 2}},
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = shape_version_ahead, class = 'ShapePoint', details = Details
                }
            } when not is_map_key(direction, Details),
            beamtalk_actor:sync_send(RemotePid, 'echo:', [TooNew])
        ),
        %% Refused before the callee ever saw the message (§5.2 "Request
        %% direction") — its state is untouched, still the spawn-time value.
        ?assertEqual(42, beamtalk_actor:sync_send(RemotePid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Version skew — reply direction (ADR 0126 §5.2)
%%====================================================================

version_ahead_on_reply_carries_direction(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = shape_version_ahead,
                    class = 'ShapeChainCart',
                    details = #{direction := reply}
                }
            },
            beamtalk_actor:sync_send(RemotePid, shapeAheadReply, [])
        )
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Migrate forward through a migrateFromVN: chain (ADR 0125 + 0126 §5.2)
%%====================================================================

-doc """
Runs ON the peer node (via `rpc:call/4`), installing a delegate-proxy stub
(`beamtalk_shape_migration_tests:build_delegate_proxy/3`) that overrides
`ShapeChainCart`'s `'__beamtalk_meta'/0` to report `shape_version = 3` with
a real two-step `migrateFromV1:`/`migrateFromV2:` chain, by rewriting its
class-registry module row (`beamtalk_class_metadata:merge_identity/5`) to
the stub. The `migrateFromV1:`/`migrateFromV2:` hooks themselves are real,
compiled `ShapeChainCart` class methods, untouched by the stub — only the
meta lookup `resolve_migrations/1` reads is faked, exactly like
`beamtalk_shape_migration_tests:with_shape_chain_cart_meta/3`, whose
override value (kept in the setup process's own process dictionary) is not
reachable from the *different* process — this node's `test_wire_actor`
gen_server — that actually calls `'__beamtalk_meta'/0` during dispatch, so
this bakes the override values into the generated stub source as literals
instead. Returns an opaque restore context for `restore_meta_stub/1`.
""".
install_meta_stub(ShapeVersion, Migrations) ->
    RealMod = 'bt@shape_chain_cart',
    StubMod = bt_3601_shape_chain_cart_meta_stub,
    OverrideSrc =
        "'__beamtalk_meta'() -> "
        "Real = apply('" ++ atom_to_list(RealMod) ++
            "', '__beamtalk_meta', []), "
            "Real#{shape_version => " ++ integer_to_list(ShapeVersion) ++
            ", "
            "shape_migrations => " ++ lists:flatten(io_lib:format("~w", [Migrations])) ++ "}.",
    ok = beamtalk_shape_migration_tests:build_delegate_proxy(RealMod, StubMod, [
        {{'__beamtalk_meta', 0}, OverrideSrc}
    ]),
    {ok, OrigModule, Selectors} = beamtalk_class_metadata:lookup_methods('ShapeChainCart'),
    {ok, Superclass} = beamtalk_class_metadata:lookup_superclass('ShapeChainCart'),
    {ok, IsAbstract} = beamtalk_class_metadata:lookup_is_abstract('ShapeChainCart'),
    ok = beamtalk_class_metadata:merge_identity(
        'ShapeChainCart', StubMod, Selectors, Superclass, IsAbstract
    ),
    {OrigModule, Selectors, Superclass, IsAbstract}.

-doc "Runs ON the peer node — reverts install_meta_stub/2's class-registry row swap and purges the stub module.".
restore_meta_stub({OrigModule, Selectors, Superclass, IsAbstract}) ->
    beamtalk_class_metadata:merge_identity(
        'ShapeChainCart', OrigModule, Selectors, Superclass, IsAbstract
    ),
    StubMod = bt_3601_shape_chain_cart_meta_stub,
    code:purge(StubMod),
    code:delete(StubMod),
    ok.

migrate_forward_through_wire(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    Ctx = erpc:call(
        PeerNode, ?MODULE, install_meta_stub, [3, #{1 => 'migrateFromV1:', 2 => 'migrateFromV2:'}]
    ),
    try
        %% itemCount => 4 at v1: migrateFromV1: sets total := itemCount * 10
        %% (=40); migrateFromV2: sets tag := "migrated" — decoded (and
        %% migrated) on the peer, the receiving node, during the request's
        %% own decode (ADR 0126 §5.2's ordinary "older envelope migrated
        %% forward" row), before echo: ever runs.
        Old =
            {beamtalk_shape, 'ShapeChainCart', 1, #{itemCount => 4, total => 0, tag => <<"none">>}},
        Migrated = beamtalk_actor:sync_send(RemotePid, 'echo:', [Old]),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'ShapeChainCart',
                '__shape_version__' := 3,
                itemCount := 4,
                total := 40,
                tag := <<"migrated">>
            },
            Migrated
        )
    after
        erpc:call(PeerNode, ?MODULE, restore_meta_stub, [Ctx]),
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% remote_code_mismatch (ADR 0126 §5.5, BT-3579 finding (b))
%%====================================================================

remote_code_mismatch_undef(PeerNode) ->
    ModName = unique_name("bt3601_undef"),
    Binary = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v1"),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary),
    Fun = ModName:make_block(),
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = remote_code_mismatch, class = 'Block', details = #{module := ModName}
                }
            },
            beamtalk_actor:sync_send(RemotePid, 'invokeBlockWith:', [Fun])
        )
    after
        code:delete(ModName),
        code:purge(ModName),
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

remote_code_mismatch_badfun(PeerNode) ->
    ModName = unique_name("bt3601_badfun"),
    BinaryV1 = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v1"),
    BinaryV2 = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v2"),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", BinaryV1),
    Fun = ModName:make_block(),
    {module, ModName} = rpc:call(
        PeerNode, code, load_binary, [ModName, atom_to_list(ModName) ++ ".erl", BinaryV2]
    ),
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = remote_code_mismatch, class = 'Block', details = #{module := ModName}
                }
            },
            beamtalk_actor:sync_send(RemotePid, 'invokeBlockWith:', [Fun])
        )
    after
        code:delete(ModName),
        code:purge(ModName),
        rpc:call(PeerNode, code, delete, [ModName]),
        rpc:call(PeerNode, code, purge, [ModName]),
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% Async future rejection — the "double-wrap" case (ADR 0126 Amendment (d))
%%====================================================================

-doc """
`beamtalk_future:await/2` signals rejection as a `throw` of
`{future_rejected, Reason}` (not `error/1`) on every path — the plain case
(a method that raises the bare `#beamtalk_error{}` directly, e.g.
`test_wirecheck_actor:handle_boom/2`) is already exercised, over the real
wire, by `beamtalk_dist_wirecheck_tests:async_future_rejects/1` (every
remote send now goes through the `'\$beamtalk_wire'` tag automatically, per
this module's moduledoc). This test covers the other path the Amendment
calls out: `test_wire_actor:handle_boomWrapped/2` raises via
`beamtalk_error:raise/1` on itself, which double-wraps — `Reason` here is a
fresh `runtime_error` whose `details.original_reason` holds the
already-wrapped Exception map, not the bare `#beamtalk_error{}` `boom`
produces.
""".
async_future_rejection_double_wrap(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wire_actor, start, [0]),
    try
        Future = beamtalk_future:new(),
        ok = beamtalk_actor:async_send(RemotePid, boomWrapped, [], beamtalk_future:pid(Future)),
        ?assertThrow(
            {future_rejected, #beamtalk_error{
                kind = runtime_error,
                class = 'WireActor',
                selector = boomWrapped,
                details = #{original_reason := #{'$beamtalk_class' := 'RuntimeError', error := _}}
            }},
            beamtalk_future:await(Future, 5000)
        )
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.
