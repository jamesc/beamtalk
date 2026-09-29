%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_capability_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_capability` — the ADR 0125 §1.5 classification of
which operations a node may perform in each mode. One test group per §1.5
row, each asserted against both release variants (default, and
`include_compiler`) and against the development modes.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(RELEASE, #{mode => release, include_compiler => false}).
-define(RELEASE_WITH_COMPILER, #{mode => release, include_compiler => true}).
-define(WORKSPACE, #{mode => workspace, include_compiler => true}).
-define(RUN, #{mode => run, include_compiler => true}).
-define(RUN_NO_COMPILER, #{mode => run, include_compiler => false}).

%%% §1.5 rows

%% Row 1: compile-free ops — available everywhere.
always_available_ops() ->
    [
        <<"run-entry">>,
        <<"inspect">>,
        <<"actors">>,
        <<"actor-stats">>,
        <<"pid-stats">>,
        <<"sessions">>,
        %% `complete` and `describe` need no compiler themselves;
        %% `complete`'s compiler fallback is classified separately below.
        <<"complete">>,
        <<"describe">>
    ].

%% Rows 2-4: operations that compile source.
compiler_ops() ->
    [
        <<"eval">>,
        <<"load-source">>,
        <<"load-project">>,
        <<"load-tests">>,
        <<"show-codegen">>,
        <<"diagnostics">>,
        'compile:source:',
        'tryCompile:source:',
        'precheckCompile:source:',
        reload,
        'load:',
        sync,
        'newClass:at:',
        test_file,
        'evaluate:',
        completion_type_inference
    ].

%% Row 5: operations on the working tree / on-disk ChangeLog.
workspace_ops() ->
    [
        <<"unload">>,
        <<"save-native-source">>,
        <<"save-section">>,
        flush,
        'flush:',
        'flush:confirmDestructive:',
        flushIncludingDestructive,
        'changes flushKinds:',
        'changes revert:',
        autoflush,
        'moveClass:to:',
        removeFromSystem,
        'renameTo:',
        'renameSelector:to:'
    ].

classify_test_() ->
    [?_assertEqual(always, beamtalk_capability:classify(Op)) || Op <- always_available_ops()] ++
        [?_assertEqual(compiler, beamtalk_capability:classify(Op)) || Op <- compiler_ops()] ++
        [?_assertEqual(workspace, beamtalk_capability:classify(Op)) || Op <- workspace_ops()].

unknown_op_is_always_test() ->
    ?assertEqual(always, beamtalk_capability:classify(<<"no-such-op">>)),
    ?assertEqual(always, beamtalk_capability:classify(noSuchSelector)).

%%% Availability per mode

still_available_in_both_release_variants_test_() ->
    [
        ?_assertEqual(ok, beamtalk_capability:check(Op, 'REPL', <<"x">>, Caps))
     || Op <- always_available_ops(), Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

compiler_ops_refused_in_default_release_test_() ->
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = release_mode_no_compiler}},
            beamtalk_capability:check(Op, 'REPL', <<"x">>, ?RELEASE)
        )
     || Op <- compiler_ops()
    ].

compiler_ops_available_with_include_compiler_test_() ->
    [
        ?_assertEqual(ok, beamtalk_capability:check(Op, 'REPL', <<"x">>, ?RELEASE_WITH_COMPILER))
     || Op <- compiler_ops()
    ].

workspace_ops_refused_in_both_release_variants_test_() ->
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = release_mode_no_workspace}},
            beamtalk_capability:check(Op, 'REPL', <<"x">>, Caps)
        )
     || Op <- workspace_ops(), Caps <- [?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

development_modes_allow_everything_test_() ->
    [
        ?_assert(beamtalk_capability:available(Op, Caps))
     || Op <- always_available_ops() ++ compiler_ops() ++ workspace_ops(),
        Caps <- [?WORKSPACE, ?RUN]
    ].

%%% Compiler availability in run mode (ADR 0129 §4)

run_mode_without_compiler_refuses_compiler_ops_test_() ->
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = run_mode_no_compiler}},
            beamtalk_capability:check(Op, 'REPL', <<"x">>, ?RUN_NO_COMPILER)
        )
     || Op <- compiler_ops()
    ].

run_mode_without_compiler_keeps_other_ops_test_() ->
    [
        ?_assert(beamtalk_capability:available(Op, ?RUN_NO_COMPILER))
     || Op <- always_available_ops() ++ workspace_ops()
    ].

run_mode_with_compiler_allows_compiler_ops_test_() ->
    [?_assert(beamtalk_capability:available(Op, ?RUN)) || Op <- compiler_ops()].

run_mode_no_compiler_refusal_shape_test() ->
    {error, Err} = beamtalk_capability:check(
        'load:', 'Workspace', <<"Workspace load:">>, ?RUN_NO_COMPILER
    ),
    ?assertEqual(run_mode_no_compiler, Err#beamtalk_error.kind),
    ?assertEqual('Workspace', Err#beamtalk_error.class),
    ?assertMatch(
        {0, _}, binary:match(Err#beamtalk_error.message, <<"Workspace load: cannot be compiled.">>)
    ),
    ?assertEqual(#{mode => run, operation => 'load:'}, Err#beamtalk_error.details).

%%% Refusal wording (ADR 0125 §1.5)

compiler_refusal_names_mode_and_alternative_test() ->
    {error, Err} = beamtalk_capability:check(
        'compile:source:', 'Counter', <<"Counter >> increment">>, ?RELEASE
    ),
    ?assertEqual(release_mode_no_compiler, Err#beamtalk_error.kind),
    ?assertEqual('Counter', Err#beamtalk_error.class),
    Message = Err#beamtalk_error.message,
    Hint = Err#beamtalk_error.hint,
    ?assertMatch({0, _}, binary:match(Message, <<"Counter >> increment cannot be compiled.">>)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"running an OTP release">>)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"ships no compiler and no">>)),
    ?assertNotEqual(nomatch, binary:match(Hint, <<"`beamtalk release`, and deploy">>)),
    ?assertNotEqual(nomatch, binary:match(Hint, <<"[release] include-compiler = true">>)),
    ?assertEqual(
        #{mode => release, operation => 'compile:source:'}, Err#beamtalk_error.details
    ).

workspace_refusal_names_mode_and_alternative_test() ->
    {error, Err} = beamtalk_capability:check(
        flush, 'Workspace', <<"Workspace flush">>, ?RELEASE_WITH_COMPILER
    ),
    ?assertEqual(release_mode_no_workspace, Err#beamtalk_error.kind),
    Message = Err#beamtalk_error.message,
    Hint = Err#beamtalk_error.hint,
    ?assertMatch({0, _}, binary:match(Message, <<"Workspace flush cannot change the workspace.">>)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"running an OTP release">>)),
    ?assertNotEqual(nomatch, binary:match(Message, <<"no working tree">>)),
    ?assertNotEqual(nomatch, binary:match(Hint, <<"`beamtalk release`, and deploy">>)),
    %% Names why include-compiler is not the alternative here.
    ?assertNotEqual(nomatch, binary:match(Hint, <<"does not re-enable workspace">>)).

protocol_op_default_subject_test() ->
    with_capabilities(?RELEASE, fun() ->
        {error, Err} = beamtalk_capability:check(<<"load-source">>),
        ?assertEqual('REPL', Err#beamtalk_error.class),
        ?assertMatch(
            {0, _},
            binary:match(
                Err#beamtalk_error.message, <<"The 'load-source' operation cannot be compiled.">>
            )
        )
    end).

lazy_subject_only_evaluated_on_refusal_test() ->
    Boom = fun() -> error(subject_evaluated) end,
    ?assertEqual(ok, beamtalk_capability:check(reload, 'Counter', Boom, ?WORKSPACE)),
    {error, Err} = beamtalk_capability:check(
        reload, 'Counter', fun() -> <<"Counter reload">> end, ?RELEASE
    ),
    ?assertMatch({0, _}, binary:match(Err#beamtalk_error.message, <<"Counter reload">>)).

%%% Node capabilities

default_capabilities_are_workspace_test() ->
    beamtalk_capability:clear(),
    ?assertEqual(#{mode => workspace, include_compiler => true}, beamtalk_capability:current()),
    ?assertEqual(ok, beamtalk_capability:check(<<"eval">>)),
    ?assertEqual(ok, beamtalk_capability:check(flush, 'Workspace', <<"Workspace flush">>)).

set_and_clear_test() ->
    with_capabilities(?RELEASE, fun() ->
        ?assertEqual(?RELEASE, beamtalk_capability:current()),
        ?assertNot(beamtalk_capability:available(<<"eval">>)),
        ?assert(beamtalk_capability:available(<<"run-entry">>))
    end),
    ?assertEqual(#{mode => workspace, include_compiler => true}, beamtalk_capability:current()).

%%% require_workspace/1 (ADR 0129 §4)

require_workspace_raises_when_never_recorded_test() ->
    beamtalk_capability:clear(),
    ?assertEqual(none, beamtalk_capability:recorded()),
    try
        beamtalk_capability:require_workspace('load:'),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertMatch(
                #beamtalk_error{kind = no_workspace, class = 'Workspace', selector = 'load:'}, Err
            ),
            ?assertEqual(
                <<"Workspace>>load: needs a running workspace; none is running on this node">>,
                Err#beamtalk_error.message
            ),
            ?assertNotEqual(
                nomatch, binary:match(Err#beamtalk_error.hint, <<"not under beamtalk test">>)
            ),
            ?assertNotEqual(
                nomatch, binary:match(Err#beamtalk_error.hint, <<"Beamtalk classNamed:">>)
            )
    end.

require_workspace_names_class_test() ->
    beamtalk_capability:clear(),
    ?assertError(
        #{error := #beamtalk_error{kind = no_workspace, class = 'Counter'}},
        beamtalk_capability:require_workspace('reload', 'Counter')
    ).

require_workspace_ok_when_recorded_in_any_mode_test_() ->
    [
        ?_test(
            with_capabilities(Caps, fun() ->
                ?assertEqual(ok, beamtalk_capability:require_workspace('load:')),
                ?assertEqual({ok, Caps}, beamtalk_capability:recorded())
            end)
        )
     || Caps <- [?RUN, ?RUN_NO_COMPILER, ?WORKSPACE, ?RELEASE, ?RELEASE_WITH_COMPILER]
    ].

current_and_classify_keep_permissive_default_test() ->
    beamtalk_capability:clear(),
    ?assertEqual(none, beamtalk_capability:recorded()),
    ?assertEqual(#{mode => workspace, include_compiler => true}, beamtalk_capability:current()),
    ?assertEqual(compiler, beamtalk_capability:classify('load:')),
    ?assert(beamtalk_capability:available('load:')).

clear_makes_workspace_absent_again_test() ->
    ok = beamtalk_capability:set(?RUN),
    ?assertEqual(ok, beamtalk_capability:require_workspace('load:')),
    ok = beamtalk_capability:clear(),
    ?assertError(
        #{error := #beamtalk_error{kind = no_workspace}},
        beamtalk_capability:require_workspace('load:')
    ).

set_rejects_invalid_mode_test() ->
    ?assertError(
        function_clause, beamtalk_capability:set(#{mode => repl, include_compiler => false})
    ).

require_raises_refusal_test() ->
    with_capabilities(?RELEASE, fun() ->
        ?assertError(
            #{error := #beamtalk_error{kind = release_mode_no_compiler}},
            beamtalk_capability:require(reload, 'Counter', <<"Counter reload">>)
        ),
        ?assertEqual(ok, beamtalk_capability:require(<<"inspect">>, 'REPL', <<"inspect">>))
    end).

with_capabilities(Caps, Fun) ->
    ok = beamtalk_capability:set(Caps),
    try
        Fun()
    after
        beamtalk_capability:clear()
    end.
