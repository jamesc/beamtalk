%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_mode_refusal_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
End-to-end EUnit coverage of ADR 0125 §1.5's release-mode refusals at the
real entry points: the REPL op seam (`beamtalk_repl_ops:dispatch/4`), the
`Workspace` primitives, and the `Behaviour` intrinsics. Each §1.5 row is
asserted in both release variants — default (no compiler) and
`include_compiler` — by recording the node's capabilities the way
`beamtalk_workspace_sup:init/1` does (`beamtalk_capability:set/1`).

The classification table itself is unit-tested in
`beamtalk_capability_tests`; this suite proves the entry points consult it.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-define(RELEASE, #{mode => release, include_compiler => false}).
-define(RELEASE_WITH_COMPILER, #{mode => release, include_compiler => true}).

%%====================================================================
%% Helpers
%%====================================================================

with_capabilities(Caps, Fun) ->
    ok = beamtalk_capability:set(Caps),
    try
        Fun()
    after
        beamtalk_capability:clear()
    end.

msg(Op) ->
    {protocol_msg, Op, <<"id-1">>, undefined, #{}}.

dispatch(Op, Params) ->
    beamtalk_repl_ops:dispatch(Op, Params, msg(Op), self()).

refusal_kind({error, #beamtalk_error{kind = Kind}}) -> Kind;
refusal_kind(_) -> none.

is_release_refusal(Result) ->
    lists:member(refusal_kind(Result), [release_mode_no_compiler, release_mode_no_workspace]).

%% The kind a raised Beamtalk exception carries, or `none` if Fun returns.
raised_kind(Fun) ->
    try Fun() of
        _ -> none
    catch
        error:#{error := #beamtalk_error{kind = Kind}} -> Kind;
        error:#beamtalk_error{kind = Kind} -> Kind;
        _:_ -> other
    end.

class_setup() ->
    case pg:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    ok.

with_dynamic_class(ClassName, Fun) ->
    class_setup(),
    {ClassObj, Pid} = beamtalk_test_dynamic_class:register(ClassName),
    try
        Fun(ClassObj)
    after
        try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end
    end.

%%====================================================================
%% REPL op seam — compiler rows
%%====================================================================

compiler_ops_refused_without_compiler_test_() ->
    [
        ?_assertEqual(
            release_mode_no_compiler,
            with_capabilities(?RELEASE, fun() -> refusal_kind(dispatch(Op, Params)) end)
        )
     || {Op, Params} <- [
            {<<"eval">>, #{<<"code">> => <<"3 + 4">>}},
            {<<"load-source">>, #{<<"source">> => <<"Object subclass: Foo">>}},
            {<<"load-project">>, #{}},
            {<<"show-codegen">>, #{<<"code">> => <<"3 + 4">>}},
            {<<"diagnostics">>, #{<<"code">> => <<"3 +">>}}
        ]
    ].

test_file_form_refused_without_compiler_test() ->
    %% `test` with a `file` compiles it; the op name alone is `always`, so the
    %% handler consults the gate for the file form.
    with_capabilities(?RELEASE, fun() ->
        ?assertEqual(
            release_mode_no_compiler,
            refusal_kind(dispatch(<<"test">>, #{<<"file">> => <<"test/foo_test.bt">>}))
        )
    end).

direct_compile_entry_points_refused_without_compiler_test() ->
    %% Entry points reached outside the REPL op seam — the Inspector's
    %% `evaluate:` and the LiveView reload-from-disk RPC — consult the gate too.
    with_capabilities(?RELEASE, fun() ->
        ?assertEqual(
            release_mode_no_compiler,
            refusal_kind(beamtalk_repl_eval:eval_with_self(nil, <<"3 + 4">>))
        ),
        ?assertEqual(
            release_mode_no_compiler, refusal_kind(beamtalk_repl_eval:reload_file("x.bt"))
        )
    end).

eval_refusal_message_names_release_mode_test() ->
    with_capabilities(?RELEASE, fun() ->
        {error, Err} = dispatch(<<"eval">>, #{<<"code">> => <<"3 + 4">>}),
        ?assertNotEqual(nomatch, binary:match(Err#beamtalk_error.message, <<"OTP release">>)),
        ?assertNotEqual(
            nomatch, binary:match(Err#beamtalk_error.hint, <<"include-compiler = true">>)
        )
    end).

compiler_ops_pass_the_gate_with_include_compiler_test() ->
    %% Empty `eval` code short-circuits in its handler — proving the op got
    %% past the capability gate to the handler, with no compiler round-trip.
    with_capabilities(?RELEASE_WITH_COMPILER, fun() ->
        Result = dispatch(<<"eval">>, #{<<"code">> => <<>>}),
        ?assertMatch({error, #beamtalk_error{}}, Result),
        ?assertNot(is_release_refusal(Result))
    end).

%%====================================================================
%% REPL op seam — workspace row
%%====================================================================

workspace_ops_refused_in_both_release_variants_test_() ->
    [
        ?_assertEqual(
            release_mode_no_workspace,
            with_capabilities(Caps, fun() -> refusal_kind(dispatch(Op, #{})) end)
        )
     || Op <- [<<"unload">>, <<"save-native-source">>, <<"save-section">>],
        Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

%%====================================================================
%% REPL op seam — still-available row
%%====================================================================

actors_still_available_test_() ->
    [
        ?_assertEqual(
            {actors, []}, with_capabilities(Caps, fun() -> dispatch(<<"actors">>, #{}) end)
        )
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

describe_still_available_test_() ->
    [
        ?_assertMatch(
            {describe, _, _}, with_capabilities(Caps, fun() -> dispatch(<<"describe">>, #{}) end)
        )
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

inspect_and_pid_stats_reach_their_handlers_test_() ->
    %% An invalid pid is rejected by the handler itself, not by the gate.
    [
        ?_assertNot(
            with_capabilities(Caps, fun() ->
                is_release_refusal(dispatch(Op, #{<<"actor">> => <<"not-a-pid">>}))
            end)
        )
     || Op <- [<<"inspect">>, <<"pid-stats">>], Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

still_available_ops_pass_the_gate_test_() ->
    %% `run-entry` and `sessions` need a live session / session table to
    %% dispatch for real; the gate itself must let them through.
    [
        ?_assertEqual(ok, with_capabilities(Caps, fun() -> beamtalk_capability:check(Op) end))
     || Op <- [<<"run-entry">>, <<"actor-stats">>, <<"sessions">>, <<"complete">>],
        Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

%%====================================================================
%% Workspace primitives
%%====================================================================

workspace_flush_refused_in_both_release_variants_test_() ->
    [
        ?_assertEqual(release_mode_no_workspace, with_capabilities(Caps, Call))
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER],
        Call <- [
            fun() -> raised_kind(fun() -> beamtalk_workspace_interface_primitives:flush() end) end,
            fun() ->
                raised_kind(fun() -> beamtalk_workspace_interface_primitives:flush('new-class') end)
            end,
            fun() ->
                raised_kind(fun() ->
                    beamtalk_workspace_interface_primitives:flush('new-class', true)
                end)
            end,
            fun() ->
                raised_kind(fun() ->
                    beamtalk_workspace_interface_primitives:flushIncludingDestructive()
                end)
            end
        ]
    ].

changelog_flush_kinds_and_revert_refused_in_both_release_variants_test_() ->
    %% `Workspace changes flushKinds:` and `revert:` reach the same flush /
    %% install / remove machinery as the gated primitives above; the LiveView
    %% RPC `revert_method/2,3` returns the refusal instead of raising. The
    %% gate runs before any ChangeLog lookup, so no ChangeLog server is needed.
    [
        ?_assertEqual(release_mode_no_workspace, with_capabilities(Caps, Call))
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER],
        Call <- [
            fun() ->
                raised_kind(fun() ->
                    beamtalk_workspace_interface_primitives:changeLogFlushKinds(['new-class'])
                end)
            end,
            fun() ->
                raised_kind(fun() ->
                    beamtalk_workspace_interface_primitives:changeLogFlushKinds(
                        ['remove-class'], true
                    )
                end)
            end,
            fun() ->
                raised_kind(fun() ->
                    beamtalk_workspace_interface_primitives:changeLogRevert(#{
                        className => 'Counter', selector => increment
                    })
                end)
            end,
            fun() ->
                refusal_kind(
                    beamtalk_workspace_interface_primitives:revert_method(
                        <<"Counter">>, <<"increment">>
                    )
                )
            end,
            fun() ->
                refusal_kind(
                    beamtalk_workspace_interface_primitives:revert_method(
                        <<"Counter">>, <<"increment">>, <<"class">>
                    )
                )
            end
        ]
    ].

workspace_load_refused_without_compiler_test_() ->
    [
        ?_assertEqual(release_mode_no_compiler, with_capabilities(?RELEASE, Call))
     || Call <- [
            fun() ->
                raised_kind(fun() -> beamtalk_workspace_interface_primitives:load(<<"x.bt">>) end)
            end,
            fun() -> raised_kind(fun() -> beamtalk_workspace_interface_primitives:sync() end) end
        ]
    ].

%%====================================================================
%% Behaviour intrinsics
%%====================================================================

compile_source_refused_without_compiler_test() ->
    with_dynamic_class('BT3568CompileSrc', fun(ClassObj) ->
        with_capabilities(?RELEASE, fun() ->
            ?assertEqual(
                release_mode_no_compiler,
                raised_kind(fun() ->
                    beamtalk_behaviour_intrinsics:classCompileSource(
                        ClassObj, increment, <<"increment => 1">>
                    )
                end)
            ),
            ?assertEqual(
                release_mode_no_compiler,
                raised_kind(fun() ->
                    beamtalk_behaviour_intrinsics:classTryCompileSource(
                        ClassObj, increment, <<"increment => 1">>
                    )
                end)
            )
        end)
    end).

compile_source_refusal_names_the_method_test() ->
    with_dynamic_class('BT3568CompileMsg', fun(ClassObj) ->
        with_capabilities(?RELEASE, fun() ->
            try
                beamtalk_behaviour_intrinsics:classCompileSource(
                    ClassObj, increment, <<"increment => 1">>
                ),
                ?assert(false)
            catch
                error:#{error := #beamtalk_error{message = Message}} ->
                    ?assertMatch(
                        {0, _},
                        binary:match(
                            Message, <<"BT3568CompileMsg >> increment cannot be compiled.">>
                        )
                    )
            end
        end)
    end).

compile_source_passes_the_gate_with_include_compiler_test() ->
    with_dynamic_class('BT3568CompileInc', fun(ClassObj) ->
        with_capabilities(?RELEASE_WITH_COMPILER, fun() ->
            Kind = raised_kind(fun() ->
                beamtalk_behaviour_intrinsics:classCompileSource(
                    ClassObj, increment, <<"increment => 1">>
                )
            end),
            ?assertNotEqual(release_mode_no_compiler, Kind)
        end)
    end).

reload_refused_without_compiler_test() ->
    with_dynamic_class('BT3568Reload', fun(ClassObj) ->
        with_capabilities(?RELEASE, fun() ->
            ?assertEqual(
                release_mode_no_compiler,
                raised_kind(fun() -> beamtalk_behaviour_intrinsics:classReload(ClassObj) end)
            )
        end)
    end).

reload_passes_the_gate_with_include_compiler_test() ->
    %% A dynamic class has no source file, so once past the gate reload
    %% fails on that instead.
    with_dynamic_class('BT3568ReloadInc', fun(ClassObj) ->
        with_capabilities(?RELEASE_WITH_COMPILER, fun() ->
            ?assertEqual(
                no_source_file,
                raised_kind(fun() -> beamtalk_behaviour_intrinsics:classReload(ClassObj) end)
            )
        end)
    end).

remove_from_system_refused_in_both_release_variants_test_() ->
    [
        ?_assertEqual(
            release_mode_no_workspace,
            with_capabilities(Caps, fun() ->
                raised_kind(fun() ->
                    beamtalk_behaviour_intrinsics:classRemoveFromSystemByName('BT3568NoSuchClass')
                end)
            end)
        )
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

rename_refused_in_both_release_variants_test_() ->
    [
        ?_test(
            with_dynamic_class('BT3568Rename', fun(ClassObj) ->
                with_capabilities(Caps, fun() ->
                    ?assertEqual(
                        release_mode_no_workspace,
                        raised_kind(fun() ->
                            beamtalk_behaviour_intrinsics:classRenameTo(ClassObj, 'BT3568Renamed')
                        end)
                    ),
                    ?assertEqual(
                        release_mode_no_workspace,
                        raised_kind(fun() ->
                            beamtalk_behaviour_intrinsics:classRenameSelector(ClassObj, foo, bar)
                        end)
                    )
                end)
            end)
        )
     || Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].
