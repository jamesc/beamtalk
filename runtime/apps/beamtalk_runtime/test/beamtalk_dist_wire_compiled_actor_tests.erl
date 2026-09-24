%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_wire_compiled_actor_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
Two-node wire integration suite for a genuinely COMPILED `.bt` actor class
(ADR 0126 Phase 3c, BT-3613 — a fast-follow to Phase 3b/BT-3601, jamesc/
beamtalk#4024).

BT-3601 wired the `'\$beamtalk_wire'` tag's encode/decode into actor send/
receive paths, but the recognition prelude it added lives in
`beamtalk_actor.erl`'s own `handle_call/3`/`handle_cast/2` — reached only by
a hand-written (`__methods__`-map) actor. A genuinely compiled actor's
`handle_call/3`/`handle_cast/2` are generated directly by Rust codegen
(`crates/beamtalk-codegen/src/core_erlang/gen_server/callbacks.rs`) and,
before this issue, had no clause recognising the tag at all — a wire-tagged
send to a remote compiled actor hit a `case_clause` on the callee. This
module is the regression suite proving that gap is closed, using
`ArithmeticActor` and `WireBlockActor` (`test_fixtures/arithmetic_actor.bt`,
`test_fixtures/wire_block_actor.bt`) — real `.bt` source compiled by
`beamtalk`, spawned remotely via `spawnOn:`'s runtime entry point
(`beamtalk_actor:remote_spawn/4`) — never a `__methods__`-map fixture.

`beamtalk_dist_wire_tests.erl` (BT-3601) already proves the wire tag/codec
end to end for the hand-written path; this module does not repeat those
scenarios (value round-tripping, version skew, shape migration) — only what
is specific to a *compiled* actor's own generated dispatch: that the wire
tag is recognised and decoded at all (sync and fire-and-forget cast), that
state threads correctly across a wire-tagged sync call, that a reply
(success and error) travels back to the caller correctly, and that
`remote_code_mismatch` (ADR 0126 §5.5) is still produced for a compiled
actor's own generated `safe_dispatch/3` catch, not only the hand-written
`wrap_method_error/6` path.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Shared fixture
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3613_compiled_wire"),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(arithmetic_actor, 'ArithmeticActor'),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(wire_block_actor, 'WireBlockActor'),
    ok = rpc:call(
        PeerNode, beamtalk_dist_wire_tests, ensure_fixture_loaded, [
            arithmetic_actor, 'ArithmeticActor'
        ]
    ),
    ok = rpc:call(
        PeerNode, beamtalk_dist_wire_tests, ensure_fixture_loaded, [
            wire_block_actor, 'WireBlockActor'
        ]
    ),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc "A short, run-unique atom — repeated `just test` runs never collide on a stale name.".
unique_name(Prefix) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))).

-doc "Spawn `ClassName` on `PeerNode` via `remote_spawn/4` — the `spawnOn:` runtime entry point.".
spawn_remote(PeerNode, ClassName) ->
    {ok, Pid} = beamtalk_actor:remote_spawn(PeerNode, ClassName, undefined, 'spawnOn:'),
    ?assert(is_pid(Pid)),
    ?assertEqual(PeerNode, node(Pid)),
    Pid.

compiled_actor_wire_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {
                    "a wire-tagged sync call reaches a compiled actor's generated "
                    "handle_call/3 and threads state correctly",
                    fun() -> sync_call_state_threads_over_wire(PeerNode) end
                },
                {"a wire-tagged sync call's error reply travels back correctly", fun() ->
                    sync_call_error_reply_over_wire(PeerNode)
                end},
                {
                    "a wire-tagged fire-and-forget cast reaches a compiled actor's "
                    "generated handle_cast/2",
                    fun() -> cast_reaches_compiled_actor_over_wire(PeerNode) end
                },
                {
                    "remote_code_mismatch (undef): a block whose module is missing on "
                    "a compiled actor's node",
                    fun() -> remote_code_mismatch_undef_compiled(PeerNode) end
                },
                {
                    "remote_code_mismatch (badfun): a block whose module is a "
                    "different version on a compiled actor's node",
                    fun() -> remote_code_mismatch_badfun_compiled(PeerNode) end
                },
                {
                    "an encode failure on a compiled actor's successful reply "
                    "raises not_serialisable rather than returning it as a "
                    "normal value",
                    fun() -> handle_scoped_reply_raises_not_serialisable(PeerNode) end
                }
            ]
        end}}.

%%====================================================================
%% Sync call — state threading over the wire (ADR 0126 §5.1)
%%====================================================================

sync_call_state_threads_over_wire(PeerNode) ->
    Pid = spawn_remote(PeerNode, 'ArithmeticActor'),
    try
        %% Before this issue, either send below hit a `case_clause` on the
        %% callee: ArithmeticActor's generated handle_call/3 had no clause
        %% recognising `'$beamtalk_wire'` at all.
        ?assertEqual(1, beamtalk_actor:sync_send(Pid, increment, [])),
        ?assertEqual(2, beamtalk_actor:sync_send(Pid, increment, [])),
        ?assertEqual(2, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% Sync call — error reply over the wire
%%====================================================================

sync_call_error_reply_over_wire(PeerNode) ->
    Pid = spawn_remote(PeerNode, 'ArithmeticActor'),
    try
        %% divide: 0 raises inside the compiled method body — the reply
        %% travels back through handle_call_dispatch_case's error arm
        %% (encode_reply_for/maybe_reclassify_compiled_dispatch_error) and
        %% is reraised locally, exactly as a local (non-wire) call to the
        %% same compiled actor already reraises it.
        ?assertError(_, beamtalk_actor:sync_send(Pid, 'divide:', [0])),
        %% The actor survived the error — still responsive, state untouched.
        ?assertEqual(0, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% Fire-and-forget cast over the wire (ADR 0126 §5.1)
%%====================================================================

cast_reaches_compiled_actor_over_wire(PeerNode) ->
    Pid = spawn_remote(PeerNode, 'ArithmeticActor'),
    try
        %% cast_send/3 wire-tags a remote fire-and-forget cast the same way
        %% sync_send wire-tags a call (beamtalk_actor.erl's cast_send/3).
        %% Before this issue, ArithmeticActor's generated handle_cast/2 had
        %% no clause recognising it, so this would have silently dropped
        %% (fallen through to the generated catch-all) rather than
        %% incrementing.
        ok = beamtalk_actor:cast_send(Pid, increment, []),
        ok = beamtalk_actor:cast_send(Pid, increment, []),
        %% Sync call after the casts — ordinary gen_server mailbox FIFO
        %% guarantees both casts landed before this call is answered.
        ?assertEqual(2, beamtalk_actor:sync_send(Pid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% remote_code_mismatch (ADR 0126 §5.5) against a compiled actor's own
%% generated safe_dispatch/3 — beamtalk_actor:
%% maybe_reclassify_compiled_dispatch_error/2, called from
%% handle_call_dispatch_case's error arm.
%%====================================================================

remote_code_mismatch_undef_compiled(PeerNode) ->
    ModName = unique_name("bt3613_undef"),
    Binary = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v1"),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary),
    Fun = ModName:make_block(),
    Pid = spawn_remote(PeerNode, 'WireBlockActor'),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = remote_code_mismatch, class = 'Block', details = #{module := ModName}
                }
            },
            beamtalk_actor:sync_send(Pid, 'invokeBlockWith:', [Fun])
        )
    after
        code:delete(ModName),
        code:purge(ModName),
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

remote_code_mismatch_badfun_compiled(PeerNode) ->
    ModName = unique_name("bt3613_badfun"),
    BinaryV1 = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v1"),
    BinaryV2 = beamtalk_dist_wirecheck_tests:compile_block_module_variant(ModName, "v2"),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", BinaryV1),
    Fun = ModName:make_block(),
    {module, ModName} = rpc:call(
        PeerNode, code, load_binary, [ModName, atom_to_list(ModName) ++ ".erl", BinaryV2]
    ),
    Pid = spawn_remote(PeerNode, 'WireBlockActor'),
    try
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{
                    kind = remote_code_mismatch, class = 'Block', details = #{module := ModName}
                }
            },
            beamtalk_actor:sync_send(Pid, 'invokeBlockWith:', [Fun])
        )
    after
        code:delete(ModName),
        code:purge(ModName),
        rpc:call(PeerNode, code, delete, [ModName]),
        rpc:call(PeerNode, code, purge, [ModName]),
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

%%====================================================================
%% Encode-failure reply contract (ADR 0126 §5.1) against a compiled
%% actor's generated handle_call_dispatch_case success arm.
%%====================================================================

handle_scoped_reply_raises_not_serialisable(PeerNode) ->
    Pid = spawn_remote(PeerNode, 'WireBlockActor'),
    try
        %% makeHandle returns an Ets handle — a node-bound HandleScoped
        %% value beamtalk_wire:encode/1 always rejects. Before this fix,
        %% handle_call_dispatch_case's success arm unconditionally wrapped
        %% encode_reply_for/3's result as {'ok', EncodedResult}, so an
        %% encode failure ({'error', EncErr}) became {'ok', {'error',
        %% EncErr}} on the wire — which decodes and unwraps back to the
        %% plain tuple {error, EncErr} as a *successful* method return
        %% value instead of raising.
        ?assertError(
            #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = not_serialisable}
            },
            beamtalk_actor:sync_send(Pid, makeHandle, [])
        )
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.
