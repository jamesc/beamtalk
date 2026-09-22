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
-define(WORKSPACE, #{mode => workspace, include_compiler => false}).
-define(RUN, #{mode => run, include_compiler => false}).

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
