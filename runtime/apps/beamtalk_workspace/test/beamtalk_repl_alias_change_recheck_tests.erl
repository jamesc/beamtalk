%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_alias_change_recheck_tests).

-moduledoc """
End-to-end wiring tests for ADR 0108's hot-reload re-check trigger (BT-2899):
a real live alias redefinition (`beamtalk_repl_eval:handle_type_alias_definition/3`,
the one production call site) through `beamtalk_alias_xref`'s dependent
lookup, `beamtalk_recheck:trigger_alias_change/1`'s re-check, and the same
publish path BT-2779/BT-2780/BT-2856 established
(`beamtalk_workspace_findings_store` + the `'ReloadCheckCompleted'`
announcement).

Mirrors `beamtalk_repl_loader_leaf_change_recheck_tests.erl`'s structure —
same fixture shape, same real-compiler-port/real-class-hierarchy philosophy
— but exercises a dependent that a real lookup (`beamtalk_alias_xref`) finds,
not a whole-image sweep: the dependent class is actually loaded/compiled
(`beamtalk_repl_loader:handle_load/2`), not just registered via
`beamtalk_workspace_meta:set_class_source/2`, because populating
`beamtalk_alias_xref`'s index requires a real compile (see
`beamtalk_repl_compiler:register_alias_xref_for_classes/2`).

The regression pin ADR 0108's Implementation section calls for directly: "a
live image holding a `matchExhaustive:` proof against a stale alias member
set is impossible after this lands" — declare an alias, `matchExhaustive:`
over it, redefine the alias to add a member, confirm the proof re-fires as
newly non-exhaustive.
""".

-include_lib("eunit/include/eunit.hrl").

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

%%====================================================================
%% Integration fixture: real compiler port + alias_xref + workspace_meta +
%% stores
%%====================================================================

alias_recheck_setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"alias_change_recheck_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    %% Clears the compiler_server's ambient class *and* alias caches (ADR
    %% 0108, BT-2899 — clear_classes/0 now clears both, see its doc).
    beamtalk_compiler_server:clear_classes(),
    case whereis(beamtalk_alias_xref) of
        undefined -> ok;
        AliasXrefPid -> gen_server:stop(AliasXrefPid)
    end,
    {ok, _} = beamtalk_alias_xref:start_link(),
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    {ok, _} = beamtalk_workspace_shape_recheck_worker:start_link(),
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    {ok, _} = beamtalk_workspace_findings_store:start_link(),
    ok = beamtalk_announcements:ensure_started(),
    ok.

alias_recheck_teardown(_) ->
    beamtalk_repl_subscriptions:unsubscribe(reload_check, self()),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    case whereis(beamtalk_alias_xref) of
        undefined -> ok;
        AliasXrefPid -> gen_server:stop(AliasXrefPid)
    end,
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    beamtalk_compiler_server:clear_classes(),
    _ = application:stop(beamtalk_compiler),
    ok.

subscribe_self_to_reload_check() ->
    ok = beamtalk_repl_subscriptions:subscribe(reload_check, self()).

receive_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_reload_check_announcement)
    end.

%% Forces the (asynchronous, `beamtalk_workspace_shape_recheck_worker`-queued)
%% alias-change re-check to have run to completion before a test asserts on
%% its outcome — mirrors `beamtalk_repl_loader_leaf_change_recheck_tests`'s
%% own reliance on the worker's mailbox being strictly FIFO. Alias-change
%% shares the same worker/queue as leaf-change and shape re-checks (see
%% `beamtalk_workspace_shape_recheck_worker`'s moduledoc).
wait_for_alias_change_worker() ->
    _ = sys:get_state(beamtalk_workspace_shape_recheck_worker),
    ok.

declare_alias(Name, Expansion, State) ->
    AliasInfo = #{alias_name => Name, expansion => Expansion, doc_comment => undefined},
    {ok, _, _, _, NewState} = beamtalk_repl_eval:handle_type_alias_definition(AliasInfo, [], State),
    NewState.

%%====================================================================
%% Fixtures
%%====================================================================

user_source() ->
    <<
        "Object subclass: AliasChangeRecheckUser\n"
        "  test: x :: AliasChangeDirection => x matchExhaustive: [\n"
        "    #north -> 0;\n"
        "    #south -> 1;\n"
        "    #east -> 2\n"
        "  ]\n"
    >>.

%%====================================================================
%% End-to-end: a live alias redefinition invalidates a `matchExhaustive:`
%% proof over a dependent class real-compiled/indexed via `beamtalk_alias_xref`
%%====================================================================

live_alias_redefinition_invalidates_a_matchexhaustive_proof_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% Turn 1: declare the alias with three members — the
                    %% dependent's matchExhaustive: below covers exactly
                    %% these three, so it is sound (no exhaustiveness error)
                    %% at the moment the dependent compiles.
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    State1 = declare_alias(
                        <<"AliasChangeDirection">>,
                        <<"#north | #south | #east">>,
                        State0
                    ),

                    %% Sanity check: the dependent's matchExhaustive: compiles
                    %% clean while the alias still has exactly the three
                    %% members it matches against — no error diagnostics.
                    {ok, SanityDiagnostics} = beamtalk_compiler:diagnostics(
                        user_source(), <<"expression">>, #{class_hierarchy => true}
                    ),
                    ?assertEqual(
                        [],
                        [D || D <- SanityDiagnostics, maps:get(severity, D) =:= <<"error">>]
                    ),

                    %% Load the dependent for real (not set_class_source/2 —
                    %% populating beamtalk_alias_xref's index requires an
                    %% actual compile, see this module's moduledoc) so its
                    %% referenced_aliases (["AliasChangeDirection"]) land in
                    %% the alias-xref index that trigger_alias_change/1
                    %% looks up by key, rather than sweeping every live class.
                    UserPath = filename:join(
                        temp_dir(),
                        io_lib:format("alias_recheck_user_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(UserPath, user_source()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(UserPath, State1),

                    ?assertEqual(
                        [<<"AliasChangeRecheckUser">>],
                        beamtalk_alias_xref:dependents_of(<<"AliasChangeDirection">>)
                    ),

                    subscribe_self_to_reload_check(),

                    %% Turn 2: redefine the alias, adding `#west` — the
                    %% dependent's matchExhaustive: (still only covering
                    %% north/south/east) is now newly non-exhaustive. This is
                    %% the ADR 0108 headline scenario: "a live image holding
                    %% a matchExhaustive: proof against a stale alias member
                    %% set is impossible after this lands".
                    _State3 = declare_alias(
                        <<"AliasChangeDirection">>,
                        <<"#north | #south | #east | #west">>,
                        State1
                    ),
                    wait_for_alias_change_worker(),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(
                        <<"AliasChangeDirection">>, maps:get(changedClass, Event)
                    ),
                    ?assertEqual(alias_change, maps:get(classification, Event)),
                    ?assert(
                        lists:member(
                            <<"AliasChangeRecheckUser">>, maps:get(checkedOwners, Event)
                        )
                    ),

                    Findings = beamtalk_workspace_findings_store:for_owner(
                        <<"AliasChangeRecheckUser">>
                    ),
                    ?assert(length(Findings) > 0),
                    ?assert(
                        lists:all(
                            fun(F) -> maps:get(classification, F) =:= alias_change end,
                            Findings
                        )
                    ),
                    ?assert(
                        lists:any(
                            fun(F) ->
                                binary:match(maps:get(message, F), <<"exhaustive">>) =/= nomatch
                            end,
                            Findings
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Regression: redefining the alias to the SAME expansion (no member added)
%% must not produce a spurious non-exhaustive finding — the dependent's
%% matchExhaustive: is still sound.
%%====================================================================

live_alias_redefinition_to_same_expansion_does_not_produce_findings_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    State1 = declare_alias(
                        <<"AliasChangeDirection">>,
                        <<"#north | #south | #east">>,
                        State0
                    ),

                    UserPath = filename:join(
                        temp_dir(),
                        io_lib:format("alias_recheck_user_same_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(UserPath, user_source()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(UserPath, State1),

                    subscribe_self_to_reload_check(),
                    %% Same expansion, verbatim — no new staleness.
                    _State3 = declare_alias(
                        <<"AliasChangeDirection">>,
                        <<"#north | #south | #east">>,
                        State1
                    ),
                    wait_for_alias_change_worker(),

                    ?assertError(
                        timeout_waiting_for_reload_check_announcement,
                        receive_reload_check_announcement()
                    ),
                    ?assertEqual(
                        [],
                        beamtalk_workspace_findings_store:for_owner(
                            <<"AliasChangeRecheckUser">>
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Regression (adversarial review, BT-2899): the `>>` method-patch path
%% (`beamtalk_repl_compiler:compile_method_reload/3`, the structured
%% single-method compile backing IDE save / `compile:source:` / REPL `>>`)
%% must populate `beamtalk_alias_xref` exactly like a fresh class load does
%% — only `handle_load/2`'s path (a full file compile) was covered above;
%% a class whose alias-typed method arrives via a *patch* instead is a
%% materially different code path (`compile_method_reload/3` ->
%% `register_alias_xref_for_classes/2`, not `compile_file_core/4`) that
%% needs its own proof it wires the index the same way.
%%====================================================================

compile_method_patch_registers_alias_dependency_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    _State1 = declare_alias(
                        <<"AliasChangeMethodPatchDirection">>,
                        <<"#north | #south">>,
                        State0
                    ),

                    ClassSource =
                        <<"Object subclass: AliasChangeMethodPatchUser\n  hello => 42\n">>,
                    MethodSource =
                        <<
                            "test: x :: AliasChangeMethodPatchDirection => x matchExhaustive: [\n"
                            "    #north -> 0;\n"
                            "    #south -> 1\n"
                            "  ]\n"
                        >>,
                    Options = #{
                        class_name => <<"AliasChangeMethodPatchUser">>,
                        is_class_method => false,
                        workspace_mode => true
                    },
                    Result = beamtalk_repl_compiler:compile_method_reload(
                        ClassSource, MethodSource, Options
                    ),
                    ?assertMatch({ok, _}, Result),

                    ?assertEqual(
                        [<<"AliasChangeMethodPatchUser">>],
                        beamtalk_alias_xref:dependents_of(
                            <<"AliasChangeMethodPatchDirection">>
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% End-to-end (BT-2916, BT-2899 follow-up): a live redefinition of a
%% *deeply* (3-level) transitively referenced alias still fires the
%% dependent's re-check.
%%
%% `type AliasChangeDeepC = AliasChangeDeepB`,
%% `type AliasChangeDeepB = AliasChangeDeepA` — the dependent class's only
%% written annotation is `:: AliasChangeDeepC`, yet its compile's
%% `referenced_aliases` must span the *full* transitive walk (all three
%% names — see `resolve_type_annotation_with_alias_deps`'s doc and the
%% Rust-level `alias_deps_records_both_levels_of_a_chained_alias` unit test
%% for the 2-level version of this same guarantee), so
%% `beamtalk_alias_xref` records the class as a dependent of the
%% *innermost* name too. Redefining only `AliasChangeDeepA` (never
%% `AliasChangeDeepB`/`AliasChangeDeepC` themselves) must still trigger
%% `beamtalk_recheck:trigger_alias_change/1` for that class — proving the
%% chain-recording guarantee actually reaches the live hot-reload trigger,
%% not just the recording logic in isolation.
%%====================================================================

deep_chain_user_source() ->
    <<
        "Object subclass: AliasChangeDeepChainUser\n"
        "  test: x :: AliasChangeDeepC => x matchExhaustive: [\n"
        "    #north -> 0;\n"
        "    #south -> 1;\n"
        "    #east -> 2\n"
        "  ]\n"
    >>.

live_redefinition_of_a_deeply_transitive_alias_triggers_recheck_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    %% Turn 1-3: build the 3-level chain
                    %% C -> B -> A, innermost first so each declaration's RHS
                    %% resolves against an already-known name.
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    State1 = declare_alias(
                        <<"AliasChangeDeepA">>,
                        <<"#north | #south | #east">>,
                        State0
                    ),
                    State2 = declare_alias(
                        <<"AliasChangeDeepB">>,
                        <<"AliasChangeDeepA">>,
                        State1
                    ),
                    State3 = declare_alias(
                        <<"AliasChangeDeepC">>,
                        <<"AliasChangeDeepB">>,
                        State2
                    ),

                    %% Sanity check: the dependent's matchExhaustive: (written
                    %% against `:: AliasChangeDeepC`) compiles clean while the
                    %% chain still bottoms out at exactly the three members it
                    %% matches against.
                    {ok, SanityDiagnostics} = beamtalk_compiler:diagnostics(
                        deep_chain_user_source(), <<"expression">>, #{class_hierarchy => true}
                    ),
                    ?assertEqual(
                        [],
                        [D || D <- SanityDiagnostics, maps:get(severity, D) =:= <<"error">>]
                    ),

                    %% Load the dependent for real so its referenced_aliases
                    %% (all three chain names, per the full-transitive-walk
                    %% guarantee) land in beamtalk_alias_xref's index.
                    UserPath = filename:join(
                        temp_dir(),
                        io_lib:format("alias_recheck_deep_chain_user_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(UserPath, deep_chain_user_source()),
                    {ok, _, State4} = beamtalk_repl_loader:handle_load(UserPath, State3),

                    %% The class only ever wrote `:: AliasChangeDeepC`, but
                    %% the innermost name of the chain, `AliasChangeDeepA`,
                    %% must still have it recorded as a dependent — this is
                    %% the crux of the deep-chain guarantee.
                    ?assertEqual(
                        [<<"AliasChangeDeepChainUser">>],
                        beamtalk_alias_xref:dependents_of(<<"AliasChangeDeepA">>)
                    ),
                    %% And so must the middle name.
                    ?assertEqual(
                        [<<"AliasChangeDeepChainUser">>],
                        beamtalk_alias_xref:dependents_of(<<"AliasChangeDeepB">>)
                    ),
                    %% And the directly written annotation name.
                    ?assertEqual(
                        [<<"AliasChangeDeepChainUser">>],
                        beamtalk_alias_xref:dependents_of(<<"AliasChangeDeepC">>)
                    ),

                    subscribe_self_to_reload_check(),

                    %% Turn 4: redefine only the innermost alias, adding
                    %% `#west` — never touching `AliasChangeDeepB` or
                    %% `AliasChangeDeepC` directly. The dependent's
                    %% matchExhaustive: (still only covering north/south/east)
                    %% is now newly non-exhaustive.
                    _State5 = declare_alias(
                        <<"AliasChangeDeepA">>,
                        <<"#north | #south | #east | #west">>,
                        State4
                    ),
                    wait_for_alias_change_worker(),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(
                        <<"AliasChangeDeepA">>, maps:get(changedClass, Event)
                    ),
                    ?assertEqual(alias_change, maps:get(classification, Event)),
                    ?assert(
                        lists:member(
                            <<"AliasChangeDeepChainUser">>, maps:get(checkedOwners, Event)
                        )
                    ),

                    Findings = beamtalk_workspace_findings_store:for_owner(
                        <<"AliasChangeDeepChainUser">>
                    ),
                    ?assert(length(Findings) > 0),
                    ?assert(
                        lists:any(
                            fun(F) ->
                                binary:match(maps:get(message, F), <<"exhaustive">>) =/= nomatch
                            end,
                            Findings
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% BT-2917 (BT-2899 follow-up): a protocol's own method-signature
%% annotation registers the same `beamtalk_alias_xref` dependency edge a
%% class-defining compile gets — exercised through the file-compile
%% (`handle_load/2` -> `protocol_definition`) path, the protocol-shaped
%% sibling of `compile_method_patch_registers_alias_dependency_test_` above.
%%
%% Unlike a class, a protocol has no live source tracked in
%% `beamtalk_workspace_meta` (`load_protocol_module/3` never calls
%% `set_class_source/2` — only classes do), so `trigger_alias_change/1`'s
%% re-check of the protocol as an owner degrades to `skipped` (no source to
%% recompile against) rather than `ok`. That's a separate, larger gap
%% (protocol source isn't tracked for re-check at all yet) than this issue's
%% scope: the assertion below proves the alias-xref edge is registered *and*
%% that the trigger mechanism recognises the protocol as a genuine re-check
%% candidate (`total_candidates => 1`) the moment the alias changes — i.e. a
%% re-check is triggered for it — without depending on that separate gap
%% being closed.
%%====================================================================

protocol_definition_registers_alias_dependency_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    State1 = declare_alias(
                        <<"AliasChangeProtocolDirection">>,
                        <<"#north | #south | #east">>,
                        State0
                    ),

                    ProtocolSource =
                        <<
                            "Protocol define: AliasChangeProtocolDirectional\n"
                            "  heading: d :: AliasChangeProtocolDirection -> Boolean\n"
                        >>,
                    ProtocolPath = filename:join(
                        temp_dir(),
                        io_lib:format("alias_recheck_protocol_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(ProtocolPath, ProtocolSource),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(ProtocolPath, State1),

                    %% The protocol itself gets the same alias-xref
                    %% dependency edge a class-defining compile gets
                    %% (BT-2917 acceptance criterion #3).
                    ?assertEqual(
                        [<<"AliasChangeProtocolDirectional">>],
                        beamtalk_alias_xref:dependents_of(<<"AliasChangeProtocolDirection">>)
                    ),

                    %% A live redefinition of the alias picks the protocol up
                    %% as a real re-check candidate — confirming a re-check
                    %% is actually triggered for it (BT-2917 acceptance
                    %% criterion #4), not just silently indexed.
                    Result = beamtalk_recheck:trigger_alias_change([
                        <<"AliasChangeProtocolDirection">>
                    ]),
                    ?assertEqual(1, maps:get(total_candidates, Result)),
                    ?assert(
                        lists:member(
                            <<"AliasChangeProtocolDirectional">>,
                            maps:get(not_verified_owners, Result)
                        )
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Regression (adversarial review, BT-2917): a REPL-inline redefinition of
%% the SAME protocol (`compile_expression`, not `:load`) must NOT clobber
%% the alias-xref edge a prior file-compile registered.
%%
%% `beamtalk_alias_xref:register_class/2` is whole-set replacement, not a
%% delta (see its own doc). `compile_protocol_definition_result/1` is shared
%% by both the file-compile and REPL-inline paths, but only the file-compile
%% path (`compile_protocol_definition_result_for_file/1`) has a trustworthy
%% `referenced_aliases` — the REPL-inline path hardcodes `[]` today (BT-2952
%% tracks fixing that). Before this fix, registration lived inside the
%% shared function, so retyping the identical protocol inline at the REPL
%% would register `[]` and silently erase the edge the earlier `:load` had
%% registered. This pins that registration now only happens from the
%% file-compile wrapper, so the REPL-inline recompile leaves the existing
%% edge untouched instead of clobbering it.
%%====================================================================

protocol_definition_repl_inline_recompile_does_not_clobber_file_registered_alias_dependency_test_() ->
    {timeout, 30,
        {setup, fun alias_recheck_setup/0, fun alias_recheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    State1 = declare_alias(
                        <<"AliasChangeProtocolClobberDirection">>,
                        <<"#north | #south | #east">>,
                        State0
                    ),

                    ProtocolSource =
                        "Protocol define: AliasChangeProtocolClobberDirectional\n"
                        "  heading: d :: AliasChangeProtocolClobberDirection -> Boolean\n",

                    %% Turn 1: `:load` the protocol from a file — registers
                    %% the real alias-xref edge (BT-2917 acceptance criterion
                    %% #3, same as protocol_definition_registers_alias_
                    %% dependency_test_ above).
                    ProtocolPath = filename:join(
                        temp_dir(),
                        io_lib:format("alias_recheck_protocol_clobber_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(ProtocolPath, ProtocolSource),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(ProtocolPath, State1),
                    ?assertEqual(
                        [<<"AliasChangeProtocolClobberDirectional">>],
                        beamtalk_alias_xref:dependents_of(
                            <<"AliasChangeProtocolClobberDirection">>
                        )
                    ),

                    %% Turn 2: retype the IDENTICAL protocol inline at the
                    %% REPL (`compile_expression`, the `Protocol define:`
                    %% typed-directly path) — must compile successfully but
                    %% must NOT touch the edge Turn 1 registered.
                    ExprResult = beamtalk_repl_compiler:compile_expression(
                        ProtocolSource, alias_recheck_protocol_clobber_expr, #{}
                    ),
                    ?assertMatch({ok, protocol_definition, _, _}, ExprResult),
                    ?assertEqual(
                        [<<"AliasChangeProtocolClobberDirectional">>],
                        beamtalk_alias_xref:dependents_of(
                            <<"AliasChangeProtocolClobberDirection">>
                        )
                    )
                end)
            ]
        end}}.
