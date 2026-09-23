%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_wirecheck_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
ADR 0126 Phase 0.5 wire-check spike (BT-3579): proves or disproves, on two
real `peer` nodes (`beamtalk_dist_test_helper`, BT-3578), the five
assumptions the rest of ADR 0126 depends on that reading code cannot settle:

  (a) `erpc` + `safe_spawn_named/3` + immediate `unlink/1` leaves a live,
      registered actor (ADR 0126 §3).
  (b) a block whose defining module is missing or at a different version on
      the invoking node raises an identifiable `badfun`/`undef` (§5.5).
  (c) a non-local return (`^`) thrown by a block invoked on a remote actor
      during a sync call (ADR 0041/0110) relays back to the defining method
      (§5.5, §5.1).
  (d) an async send's future resolves across nodes, on both the success and
      error paths (§5.1).
  (e) a class object returned from a remote actor carries the remote class
      gen_server's pid today, and a by-name rewrite/resolve fixes a
      class-side send onto it to run on the *receiving* node (§5.1, §5.5).

Per the issue: this code is throwaway spike code, not a permanent runtime
feature (no `beamtalk_wire`, no `spawnOn:`) — but the **tests** are the
point and are kept as the Phase 2/3 regression suite. Findings are written
back into `docs/ADR/0126-distribution-location-transparent-actors.md` as a
Phase 0.5 amendment, not just left in this module's comments.

Two simplifications from the full ADR machinery, deliberate for a spike:
  - (b), (c), (d) use raw `__methods__`-map actor fixtures
    (`test_wirecheck_actor.erl`, mirroring `test_counter.erl`) rather than
    compiled `.bt` classes — blocks are already plain Erlang funs and NLR is
    already the raw `{'$bt_nlr', ...}` tuple at this layer, so a compiled
    class adds toolchain weight without changing what's being proven.
  - (e) uses a minimal stand-in "class" gen_server
    (`test_wirecheck_fakeclass.erl`) registered the same way a real class
    is (`beamtalk_class_registry:registry_name/1`) rather than booting the
    full compiled stdlib `Counter` class on both peers.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-export([
    spawn_via_erpc/2,
    invoke_and_capture/2,
    rewrite_class_ref/1,
    resolve_class_ref/1
]).

%%====================================================================
%% Shared fixture
%%====================================================================

setup() ->
    {ok, Peer, PeerNode} = beamtalk_dist_test_helper:start_peer("bt3579_wirecheck"),
    {Peer, PeerNode}.

cleanup({Peer, _PeerNode}) ->
    beamtalk_dist_test_helper:stop_peer(Peer).

-doc "A short, run-unique atom, so repeated `just test` runs never collide on a stale registered name.".
unique_name(Prefix) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive, monotonic]))).

wirecheck_test_() ->
    {timeout, 60,
        {setup, fun setup/0, fun cleanup/1, fun({_Peer, PeerNode}) ->
            [
                {"(a) erpc + safe_spawn_named + unlink leaves a live, registered actor", fun() ->
                    named_spawn_survives_when_unlinked(PeerNode)
                end},
                {"(a) negative: erpc + safe_spawn_named WITHOUT unlink — record what happens",
                    fun() -> named_spawn_without_unlink(PeerNode) end},
                {"(b) block whose module is entirely missing on the invoking node raises undef",
                    fun() -> block_missing_module_raises(PeerNode) end},
                {"(b) block whose module is a different version on the invoking node raises badfun",
                    fun() -> block_different_version_raises(PeerNode) end},
                {"(c) NLR thrown by a block invoked on a remote actor during a sync call", fun() ->
                    nlr_across_nodes(PeerNode)
                end},
                {"(d) async future resolves across nodes on success", fun() ->
                    async_future_resolves(PeerNode)
                end},
                {"(d) async future resolves (rejects) across nodes on error", fun() ->
                    async_future_rejects(PeerNode)
                end},
                {
                    "(e) class object returned from a remote actor carries the remote class pid; "
                    "by-name rewrite + resolve fixes a class-side send onto the receiving node",
                    fun() -> class_object_rewrite_lands_locally(PeerNode) end
                }
            ]
        end}}.

%%====================================================================
%% (a) Remote named spawn survives (ADR 0126 §3)
%%====================================================================

-doc """
Runs ON the target node under `erpc:call/4` — mirrors
`beamtalk_class_instantiation:do_class_self_named_spawn/6`'s own
spawn-then-conditionally-unlink sequence, with `erpc`'s temporary worker
process standing in for that function's caller.

Calls the **public** `beamtalk_actor:'spawnAs'/3` (reserved-name check +
`safe_spawn_named/3`), not `safe_spawn_named/3` directly — that internal
function is not `-export`ed, so a real `remote_spawn` (and this spike) can
only reach it the way `do_class_self_named_spawn/6` already does. The ADR
text names `safe_spawn_named/3` itself; see the Phase 0.5 amendment for
this correction.
""".
-spec spawn_via_erpc(atom(), boolean()) -> pid().
spawn_via_erpc(Name, DoUnlink) ->
    {ok, Pid} = beamtalk_actor:'spawnAs'(Name, test_counter, 0),
    case DoUnlink of
        true -> unlink(Pid);
        false -> ok
    end,
    Pid.

named_spawn_survives_when_unlinked(PeerNode) ->
    Name = unique_name("bt3579a_pos"),
    Pid = erpc:call(PeerNode, ?MODULE, spawn_via_erpc, [Name, true]),
    ?assert(is_pid(Pid)),
    try
        %% The erpc worker that ran spawn_via_erpc/2 has, by construction,
        %% already returned by the time erpc:call/4 replies here — no
        %% sleep needed to "let it exit"; the question is only whether the
        %% actor noticed that exit.
        ?assert(rpc:call(PeerNode, erlang, is_process_alive, [Pid])),
        ?assertEqual(Pid, rpc:call(PeerNode, erlang, whereis, [Name])),
        ?assertEqual(10, beamtalk_actor:sync_send(Pid, getValue, []) + 10)
    after
        rpc:call(PeerNode, gen_server, stop, [Pid])
    end.

-doc """
Negative case: same sequence, no `unlink/1`. ADR 0126 §3 asserts the actor
dies once the erpc worker exits (because that worker's own exit reason is
non-`normal`) — this pins whatever `erpc` actually does today rather than
trusting that prose, per BT-3579's mandate to test the assumption itself.
""".
named_spawn_without_unlink(PeerNode) ->
    Name = unique_name("bt3579a_neg"),
    Pid = erpc:call(PeerNode, ?MODULE, spawn_via_erpc, [Name, false]),
    ?assert(is_pid(Pid)),
    %% Unlike the positive case, there is a real race here: the actor's
    %% exit (if any) is triggered by an EXIT signal delivered asynchronously
    %% after the erpc worker's own exit, which happens strictly after
    %% erpc:call/4 has already returned to us. Poll rather than asserting
    %% instantaneously — up to 2s total, generous for a loaded CI runner,
    %% since a false "still alive" here would misreport the finding, not
    %% just flake a green test.
    Alive = wait_until_settled(
        fun() -> rpc:call(PeerNode, erlang, is_process_alive, [Pid]) end, 80
    ),
    %% Record the finding either way; see the ADR amendment for the
    %% narrative. If this ever flips to `true` on some OTP/platform
    %% combination, that is itself the finding — fail loudly rather than
    %% silently accept either outcome.
    ?assertEqual(false, Alive).

wait_until_settled(Fun, 0) ->
    Fun();
wait_until_settled(Fun, Retries) ->
    case Fun() of
        false ->
            false;
        true ->
            timer:sleep(25),
            wait_until_settled(Fun, Retries - 1)
    end.

%%====================================================================
%% (b) Block code-version mismatch is identifiable (ADR 0126 §5.5)
%%====================================================================

-doc "Invoke `Fun(Arg)`, capturing the class/reason/stacktrace instead of letting it crash the caller.".
-spec invoke_and_capture(fun((term()) -> term()), term()) ->
    {ok, term()} | {caught, atom(), term(), list()}.
invoke_and_capture(Fun, Arg) ->
    try Fun(Arg) of
        Result -> {ok, Result}
    catch
        Class:Reason:Stack -> {caught, Class, Reason, Stack}
    end.

-doc """
Compiles a tiny module in memory (`compile:forms/2`, never touching disk —
no temp-file path needed) exporting `make_block/0`, which returns a closure
over `X`. `Suffix` is baked into the closure's body as a literal so two
calls with different suffixes produce genuinely different bytecode (a
different Md5) for the same module name, standing in for "a different
version of the class deployed on this node."
""".
compile_block_module_variant(ModName, Suffix) ->
    Forms = [
        parse_form(io_lib:format("-module(~p).", [ModName])),
        parse_form("-export([make_block/0])."),
        parse_form(
            io_lib:format(
                "make_block() -> fun(X) -> {block_result, X, <<\"~s\">>} end.", [Suffix]
            )
        )
    ],
    {ok, ModName, Binary} = compile:forms(Forms, [binary, return_errors]),
    Binary.

parse_form(IoData) ->
    Str = lists:flatten(IoData),
    {ok, Tokens, _EndLine} = erl_scan:string(Str),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Form.

block_missing_module_raises(PeerNode) ->
    ModName = unique_name("bt3579b_missing"),
    Binary = compile_block_module_variant(ModName, "v1"),
    %% Loaded on THIS node only — never written to any path on
    %% code:get_path(), so the peer never sees it (it is not "shared code
    %% path", it is a module that exists only in this node's code server).
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary),
    Fun = ModName:make_block(),
    try
        Capture = rpc:call(PeerNode, ?MODULE, invoke_and_capture, [Fun, 42]),
        ?assertMatch({caught, error, undef, _}, Capture),
        %% Finding: for `undef`, the raised reason is the bare atom
        %% `undef` — it does NOT itself carry the module. What identifies
        %% it is the fun value the caller already held (the block
        %% argument, unchanged by the failed call) via
        %% `erlang:fun_info/2`, not anything parsed out of the exception
        %% or its stacktrace. A `remote_code_mismatch` mapping must keep
        %% that fun around across the call, not try to recover the module
        %% from the `undef` term itself.
        ?assertEqual({module, ModName}, erlang:fun_info(Fun, module))
    after
        code:purge(ModName),
        code:delete(ModName)
    end.

block_different_version_raises(PeerNode) ->
    ModName = unique_name("bt3579b_skew"),
    BinaryV1 = compile_block_module_variant(ModName, "v1"),
    BinaryV2 = compile_block_module_variant(ModName, "v2"),
    {module, ModName} = code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", BinaryV1),
    Fun = ModName:make_block(),
    {module, ModName} = rpc:call(
        PeerNode, code, load_binary, [ModName, atom_to_list(ModName) ++ ".erl", BinaryV2]
    ),
    try
        Capture = rpc:call(PeerNode, ?MODULE, invoke_and_capture, [Fun, 42]),
        %% Finding: unlike `undef`, the reason here is `{badfun, Fun}` —
        %% a *tuple* carrying the fun value itself, not the bare atom
        %% `badfun`. So the raised term DOES carry enough to identify the
        %% module in this case, via the embedded Fun's `erlang:fun_info/2`
        %% — matching, but not needing, the caller's already-held copy.
        ?assertMatch({caught, error, {badfun, Fun}, _}, Capture),
        {caught, error, {badfun, CaughtFun}, _} = Capture,
        ?assertEqual({module, ModName}, erlang:fun_info(CaughtFun, module))
    after
        code:purge(ModName),
        code:delete(ModName),
        rpc:call(PeerNode, code, purge, [ModName]),
        rpc:call(PeerNode, code, delete, [ModName])
    end.

%%====================================================================
%% (c) Non-local return across nodes (ADR 0126 §5.5, §5.1; ADR 0041/0110)
%%====================================================================

-doc """
A block containing `^`, invoked on a remote actor during a sync call, must
relay `{'$bt_nlr', Token, Value, State}` back to the defining method rather
than surface as an ordinary method error — ADR 0126 §5.5 flags this as
"the claim most likely to be wrong in practice". This pins whichever way it
actually goes today.
""".
nlr_across_nodes(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wirecheck_actor, start, [0]),
    Token = make_ref(),
    NlrValue = bt3579_nlr_marker,
    NlrBlock = fun() -> throw({'$bt_nlr', Token, NlrValue, defining_method_state}) end,
    try
        Outcome =
            try
                {returned, beamtalk_actor:sync_send(RemotePid, 'invokeBlock:', [NlrBlock])}
            catch
                Class:Reason -> {caught, Class, Reason}
            end,
        %% Finding: today this does NOT come back as the NLR value/relay —
        %% `beamtalk_actor:dispatch_user_method/4`'s catch-all
        %% (`Class:Reason:Stacktrace -> wrap_method_error(...)`) has no
        %% `?IS_NLR` exclusion (unlike `beamtalk_class_dispatch.erl`'s
        %% `class_send_dispatch/3` clauses), so the thrown NLR tuple is
        %% caught there and reported as an ordinary `runtime_error` — the
        %% original `{'$bt_nlr', Token, Value, State}` tuple survives only
        %% as opaque debugging detail (`details.original_reason`), not as
        %% the control-flow signal it should relay. See the ADR amendment
        %% for the full narrative and the fix this implies for Phase 3.
        ?assertMatch(
            {caught, error, #{
                '$beamtalk_class' := 'RuntimeError',
                error := #beamtalk_error{
                    kind = runtime_error,
                    details = #{
                        original_class := throw,
                        original_reason := {'$bt_nlr', Token, NlrValue, defining_method_state}
                    }
                }
            }},
            Outcome
        ),
        %% The callee node must still be alive and answer further sends —
        %% the catch-all is at minimum a safety valve, even though the
        %% semantics are wrong.
        ?assertEqual(0, beamtalk_actor:sync_send(RemotePid, getValue, []))
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% (d) Async futures resolve across nodes (ADR 0126 §5.1)
%%====================================================================

async_future_resolves(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wirecheck_actor, start, [42]),
    try
        Future = beamtalk_future:new(),
        ok = beamtalk_actor:async_send(RemotePid, getValue, [], beamtalk_future:pid(Future)),
        ?assertEqual(42, beamtalk_future:await(Future, 5000))
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

async_future_rejects(PeerNode) ->
    {ok, RemotePid} = rpc:call(PeerNode, test_wirecheck_actor, start, [0]),
    try
        Future = beamtalk_future:new(),
        ok = beamtalk_actor:async_send(RemotePid, boom, [], beamtalk_future:pid(Future)),
        %% beamtalk_future:await/2's own doc: rejection surfaces as a
        %% *throw* of `{future_rejected, Reason}`, not an `error/1` —
        %% and `Reason` here is the bare `#beamtalk_error{}` record
        %% `dispatch_user_method/4`'s handle_cast path rejects the future
        %% with directly (`beamtalk_actor:handle_cast/2` →
        %% `beamtalk_future:reject(FuturePid, Reason)`), not map-wrapped —
        %% the map-wrap in (c)'s finding above is specific to the
        %% *sync* path's `beamtalk_exception_handler:reraise/1` at the
        %% caller.
        ?assertThrow(
            {future_rejected, #beamtalk_error{
                kind = runtime_error, class = 'WirecheckActor', selector = boom
            }},
            beamtalk_future:await(Future, 5000)
        )
    after
        rpc:call(PeerNode, gen_server, stop, [RemotePid])
    end.

%%====================================================================
%% (e) Class objects crossing nodes (ADR 0126 §5.1, §5.5)
%%====================================================================

-doc """
Prototype of the §5.1 by-name rewrite: a class object whose pid resolves to
a *different* node is rewritten to `{'$beamtalk_class_ref', ClassName}`.
Real codegen would apply this at the wire-encode boundary (§5.1); here it
is a plain function so the spike can call it directly.
""".
-spec rewrite_class_ref(#beamtalk_object{}) ->
    #beamtalk_object{} | {'$beamtalk_class_ref', atom()}.
rewrite_class_ref(#beamtalk_object{pid = Pid} = Obj) ->
    case node(Pid) =:= node() of
        true ->
            Obj;
        false ->
            ClassName = gen_server:call(Pid, class_name),
            {'$beamtalk_class_ref', ClassName}
    end.

-doc """
Prototype of the §5.1 by-name resolve: looked up on the *receiving* (local)
node's class registry.

`beamtalk_behaviour_intrinsics:atom_to_class_object/1` is the real runtime's
equivalent of exactly this — `whereis_class/1` + `module_name` +
`class_object_tag/1` — but it is not `-export`ed (only called from within
that module), so a real by-name resolver has the same "no public entry
point" gap `spawn_via_erpc/2`'s doc notes for (a)'s `safe_spawn_named/3`.
This spike reimplements the four lines rather than export the internal
function purely for the test — see the ADR amendment.
""".
-spec resolve_class_ref({'$beamtalk_class_ref', atom()} | #beamtalk_object{}) ->
    #beamtalk_object{} | nil.
resolve_class_ref({'$beamtalk_class_ref', ClassName}) ->
    class_object_for(ClassName);
resolve_class_ref(Obj) ->
    Obj.

class_object_for(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        ClassPid ->
            Module = gen_server:call(ClassPid, module_name),
            #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = Module,
                pid = ClassPid
            }
    end.

class_object_rewrite_lands_locally(PeerNode) ->
    ClassName = unique_name("WirecheckDemoClass"),
    {ok, LocalClassPid} = test_wirecheck_fakeclass:start(ClassName),
    {ok, RemoteClassPid} = rpc:call(PeerNode, test_wirecheck_fakeclass, start, [ClassName]),
    try
        %% What "remoteCounter class" produces *today*, unpatched:
        %% B's class gen_server pid, crossing transparently because a pid
        %% is just a pid on the wire.
        RemoteClassObj = class_object_for(ClassName),
        ?assertMatch(#beamtalk_object{}, RemoteClassObj),
        ?assertEqual(node(), node(RemoteClassObj#beamtalk_object.pid)),
        %% That's *this* node's own fake class (started above), which is
        %% the crux of the bug the ADR calls out: without the rewrite,
        %% resolving "the class of a remote instance" from the wrong side
        %% is silently indistinguishable from resolving it locally.
        %% Simulate what A actually receives when B replies with its own
        %% class object (raw, unpatched — as if B had sent it back over an
        %% ordinary message send):
        RawFromB = #beamtalk_object{
            class = beamtalk_class_registry:class_object_tag(ClassName),
            class_mod = test_wirecheck_fakeclass,
            pid = RemoteClassPid
        },
        ?assertEqual(PeerNode, node(RawFromB#beamtalk_object.pid)),

        %% Prototype rewrite + resolve:
        WireRef = rewrite_class_ref(RawFromB),
        ?assertEqual({'$beamtalk_class_ref', ClassName}, WireRef),
        Resolved = resolve_class_ref(WireRef),
        ?assertMatch(#beamtalk_object{}, Resolved),
        ?assertEqual(node(), node(Resolved#beamtalk_object.pid)),
        ?assertNotEqual(RemoteClassPid, Resolved#beamtalk_object.pid),

        %% "a class-side send runs on A": call through the resolved
        %% object's pid and confirm the reply names *this* node, not the peer.
        {ReplyNode, ReplyPid} = gen_server:call(Resolved#beamtalk_object.pid, {demo_call, []}),
        ?assertEqual(node(), ReplyNode),
        ?assertEqual(Resolved#beamtalk_object.pid, ReplyPid)
    after
        gen_server:stop(LocalClassPid),
        rpc:call(PeerNode, gen_server, stop, [RemoteClassPid])
    end.
