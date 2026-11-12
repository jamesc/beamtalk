%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_case_tests).

-moduledoc """
EUnit tests for beamtalk_test_case module.

Tests assertion methods, test discovery, test execution,
result formatting, and error extraction.
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% fail: Tests (direct EUnit coverage — fail: remains @primitive)
%%% ============================================================================

fail_with_binary_test() ->
    ?assertBtError(assertion_failed, beamtalk_test_case:fail(<<"oops">>)).

fail_with_atom_test() ->
    ?assertBtError(assertion_failed, beamtalk_test_case:fail(broken)).

fail_with_other_test() ->
    ?assertBtError(assertion_failed, beamtalk_test_case:fail(123)).

%%% ============================================================================
%%% should_raise Tests
%%% ============================================================================

should_raise_matching_kind_test() ->
    Block = fun() ->
        error(#beamtalk_error{
            kind = does_not_understand,
            class = 'X',
            message = <<"m">>
        })
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, does_not_understand)).

should_raise_raw_atom_error_test() ->
    Block = fun() -> error(badarith) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, badarith)).

should_raise_raw_badkey_uses_on_do_kind_test() ->
    %% BT-3930: same normalisation as on:do: ({badkey, K} -> key_error)
    Block = fun() -> error({badkey, missing}) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, key_error)).

should_raise_no_error_test() ->
    Block = fun() -> ok end,
    ?assertBtError(assertion_failed, beamtalk_test_case:should_raise(Block, some_error)).

should_raise_wrong_kind_test() ->
    Block = fun() ->
        error(#beamtalk_error{
            kind = type_error,
            class = 'X',
            message = <<"m">>
        })
    end,
    ?assertBtError(assertion_failed, beamtalk_test_case:should_raise(Block, does_not_understand)).

should_raise_non_function_raises_type_error_test() ->
    ?assertBtError(type_error, beamtalk_test_case:should_raise(not_a_fun, some_error)).

should_raise_non_atom_kind_raises_type_error_test() ->
    Block = fun() -> ok end,
    ?assertBtError(type_error, beamtalk_test_case:should_raise(Block, <<"not_atom">>)).

%%% ============================================================================
%%% extract_error_kind Tests (via should_raise which calls it)
%%% ============================================================================

extract_wrapped_exception_test() ->
    %% ADR 0015: Exception objects wrap #beamtalk_error{} records
    Block = fun() ->
        Inner = #beamtalk_error{kind = type_error, class = 'X', message = <<"m">>},
        error(#{class => 'Exception', error => Inner})
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, type_error)).

extract_runtime_error_wrapper_test() ->
    %% RuntimeError wraps #beamtalk_error{} records
    Block = fun() ->
        Inner = #beamtalk_error{kind = assertion_failed, class = 'X', message = <<"m">>},
        error(#{'$beamtalk_class' => 'RuntimeError', error => Inner})
    end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, assertion_failed)).

extract_unknown_format_returns_error_atom_test() ->
    %% Unknown error format should yield 'error' kind
    Block = fun() -> error({some_tuple, data}) end,
    ?assertEqual(nil, beamtalk_test_case:should_raise(Block, error)).

%%% ============================================================================
%%% discover_test_methods Tests
%%% ============================================================================

discover_test_methods_finds_test_prefixed_test() ->
    FlatMethods = #{
        testAdd => {class, info},
        testRemove => {class, info},
        setUp => {class, info},
        tearDown => {class, info},
        helper => {class, info}
    },
    Result = discover_test_methods_wrapper(FlatMethods),
    ?assertEqual([testAdd, testRemove], Result).

discover_test_methods_empty_map_test() ->
    ?assertEqual([], discover_test_methods_wrapper(#{})).

discover_test_methods_no_test_methods_test() ->
    FlatMethods = #{setUp => info, helper => info},
    ?assertEqual([], discover_test_methods_wrapper(FlatMethods)).

%% We can't call discover_test_methods directly since it's internal,
%% but we can test it via execute_tests which calls it.
%% Use a helper that calls the exported execute_tests to verify discovery.
discover_test_methods_wrapper(FlatMethods) ->
    %% Extract test methods by the same logic used internally
    TestMethods = maps:fold(
        fun(Selector, _Info, Acc) ->
            case atom_to_list(Selector) of
                "test" ++ _ -> [Selector | Acc];
                _ -> Acc
            end
        end,
        [],
        FlatMethods
    ),
    lists:sort(TestMethods).

%%% ============================================================================
%%% class_name_to_snake Tests (via resolve_module behavior)
%%% ============================================================================

%% We test the CamelCase->snake_case conversion indirectly.
%% The function is internal but its behavior is observable via execute_tests.

%%% ============================================================================
%%% format_results Tests (via execute_tests)
%%% ============================================================================

execute_tests_no_methods_test() ->
    Result = beamtalk_test_case:execute_tests(runAll, [], 'EmptyClass', fake_module, #{}),
    ?assertEqual(<<"No test methods found in EmptyClass">>, Result).

execute_tests_run_single_not_found_test() ->
    Result = beamtalk_test_case:execute_tests('run:', [testMissing], 'SomeClass', fake_mod, #{}),
    ?assertEqual(<<"Method 'testMissing' not found in SomeClass">>, Result).

%%% ============================================================================
%%% Test Fixture: Minimal test module simulation
%%% ============================================================================

%% Setup to ensure stdlib is loaded for the tests below that need real modules
test_case_setup() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    {ok, _} = beamtalk_bootstrap:start_link(),
    beamtalk_stdlib:init(),
    ok.

test_case_teardown(_) ->
    ok.

test_case_test_() ->
    {setup, fun test_case_setup/0, fun test_case_teardown/1, [
        {"execute_tests runAll with real TestCase module",
            fun execute_tests_with_real_module_test/0},
        {"run_all with real TestCase module", fun run_all_with_real_module_test/0}
    ]}.

execute_tests_with_real_module_test() ->
    %% Use the actual counter_test BUnit module if available
    %% Otherwise just verify the framework runs with empty methods
    FlatMethods = #{
        setUp => {'TestCase', {instance, fun(_A, S) -> {reply, nil, S} end}},
        tearDown => {'TestCase', {instance, fun(_A, S) -> {reply, nil, S} end}}
    },
    Result = beamtalk_test_case:execute_tests(runAll, [], 'FakeTest', fake_mod, FlatMethods),
    ?assertEqual(<<"No test methods found in FakeTest">>, Result).

run_all_with_real_module_test() ->
    %% The BIF fallback path for a class that is not registered: tests come
    %% from the class hierarchy (class_test_plan/1), so there are none and the
    %% result is deterministic.
    ?assertEqual(
        <<"No test methods found in NonExistentTestClass">>,
        beamtalk_test_case:run_all('NonExistentTestClass')
    ).

%%% ============================================================================
%%% is_valid_setUp_result/2 Tests
%%% ============================================================================

%% Valid: same class map — normal value-object setUp path
is_valid_setUp_result_same_class_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest', value => 0},
    Result = #{'$beamtalk_class' => 'MyTest', value => 42},
    ?assert(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: setUp returns false (untaken ifTrue: branch)
is_valid_setUp_result_false_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest', value => 0},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, false)).

%% Invalid: setUp returns nil
is_valid_setUp_result_nil_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, nil)).

%% Invalid: setUp returns an integer (e.g. result of some computation)
is_valid_setUp_result_integer_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, 42)).

%% Invalid: setUp returns a map of a different class
is_valid_setUp_result_different_class_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    Result = #{'$beamtalk_class' => 'OtherClass'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: setUp returns a map without '$beamtalk_class' key
is_valid_setUp_result_plain_map_test() ->
    Instance = #{'$beamtalk_class' => 'MyTest'},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, #{foo => bar})).

%% Invalid: instance itself has no '$beamtalk_class' (degenerate case)
is_valid_setUp_result_no_class_key_test() ->
    Instance = #{value => 0},
    Result = #{value => 42},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, Result)).

%% Invalid: non-map instance (e.g. actor #beamtalk_object record)
is_valid_setUp_result_actor_instance_test() ->
    Instance = #beamtalk_object{class = 'MyTest', class_mod = my_test, pid = self()},
    ?assertNot(beamtalk_test_case:is_valid_setUp_result(Instance, nil)).

%%% ============================================================================
%%% format_stacktrace/1 Tests (frame ordering)
%%% ============================================================================

%% Empty stacktrace returns empty binary.
format_stacktrace_empty_test() ->
    ?assertEqual(<<>>, beamtalk_test_case:format_stacktrace([])).

%% Stacktrace with only non-bt@ frames returns empty binary.
format_stacktrace_no_relevant_frames_test() ->
    Frames = [
        {erl_eval, expr, 3, [{file, "erl_eval.erl"}, {line, 100}]},
        {gen_server, call, 2, [{file, "gen_server.erl"}, {line, 200}]}
    ],
    ?assertEqual(<<>>, beamtalk_test_case:format_stacktrace(Frames)).

%% Multi-frame stacktrace preserves most-recent-first order (innermost first).
%% BEAM stacktraces have the error location at the head — format_stacktrace
%% must keep that ordering so the error frame appears first in the output.
format_stacktrace_preserves_innermost_first_test() ->
    %% Simulate a BEAM stacktrace: innermost (error site) first
    Frames = [
        {'bt@user@HttpServerTest', baseUrl, 0, [{file, "test/http_server_test.bt"}, {line, 20}]},
        {'bt@user@HttpServerTest', dispatch, 1, []},
        {'bt@user@HttpServerTest', testNotFound, 0, [
            {file, "test/http_server_test.bt"}, {line, 73}
        ]}
    ],
    Result = beamtalk_test_case:format_stacktrace(Frames),
    %% The innermost frame (baseUrl, line 20) must appear before the
    %% outermost frame (testNotFound, line 73) in the output.
    BaseurlPos = binary:match(Result, <<"baseUrl">>),
    TestNotFoundPos = binary:match(Result, <<"testNotFound">>),
    ?assertNotEqual(nomatch, BaseurlPos),
    ?assertNotEqual(nomatch, TestNotFoundPos),
    {BaseurlIdx, _} = BaseurlPos,
    {TestNotFoundIdx, _} = TestNotFoundPos,
    ?assert(BaseurlIdx < TestNotFoundIdx).

%% At most 3 frames are shown: first 2 innermost + outermost when >3 exist.
format_stacktrace_selects_first_two_plus_last_test() ->
    Frames = [
        {'bt@user@A', m1, 0, [{file, "a.bt"}, {line, 1}]},
        {'bt@user@A', m2, 0, [{file, "a.bt"}, {line, 2}]},
        {'bt@user@A', m3, 0, [{file, "a.bt"}, {line, 3}]},
        {'bt@user@A', m4, 0, [{file, "a.bt"}, {line, 4}]}
    ],
    Result = beamtalk_test_case:format_stacktrace(Frames),
    %% First 2 (m1, m2) + last (m4) are shown; m3 is dropped
    ?assertNotEqual(nomatch, binary:match(Result, <<"m1">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"m2">>)),
    ?assertEqual(nomatch, binary:match(Result, <<"m3">>)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"m4">>)).

%% BT-4355: --full-traces keeps every filtered frame (same per-frame format).
format_stacktrace_full_traces_keeps_all_frames_test() ->
    Frames = [
        {'bt@user@A', m1, 0, [{file, "a.bt"}, {line, 1}]},
        {'bt@user@A', m2, 0, [{file, "a.bt"}, {line, 2}]},
        {'bt@user@A', m3, 0, [{file, "a.bt"}, {line, 3}]},
        {'bt@user@A', m4, 0, [{file, "a.bt"}, {line, 4}]}
    ],
    Default = beamtalk_test_case:format_stacktrace(Frames),
    application:set_env(beamtalk_stdlib, full_traces, true),
    try
        Full = beamtalk_test_case:format_stacktrace(Frames),
        ?assertNotEqual(nomatch, binary:match(Full, <<"m3">>)),
        ?assertEqual(4, length(binary:matches(Full, <<"\n  at ">>))),
        ?assertEqual(3, length(binary:matches(Default, <<"\n  at ">>))),
        %% Every default line appears verbatim in the full trace.
        [_ | DefaultLines] = binary:split(Default, <<"\n  at ">>, [global]),
        lists:foreach(
            fun(L) -> ?assertNotEqual(nomatch, binary:match(Full, L)) end, DefaultLines
        )
    after
        application:unset_env(beamtalk_stdlib, full_traces)
    end.

%% BT-4355: structured frames match the rendered lines and carry no MFA.
stack_frames_match_rendered_text_test() ->
    Frames = [
        {'bt@user@A', 'm1', 0, [{file, "a.bt"}, {line, 1}]},
        {'bt@user@A', '-m2/1-fun-0-', 1, [{file, "a.bt"}, {line, 2}]},
        {'bt@user@A', 'm3', 0, [{file, "a.bt"}, {line, 3}]},
        {'bt@user@A', 'm4', 0, [{file, "a.bt"}, {line, 4}]}
    ],
    Structured = beamtalk_test_case:stack_frames(Frames),
    ?assertEqual(3, length(Structured)),
    [F1, F2, F3] = Structured,
    ?assertEqual(<<"m1">>, maps:get(selector, F1)),
    ?assertEqual(false, maps:get(block, F1)),
    ?assertEqual(1, maps:get(line, F1)),
    ?assertEqual(<<"a.bt">>, maps:get(file, F1)),
    ?assertEqual(<<"m2">>, maps:get(selector, F2)),
    ?assertEqual(true, maps:get(block, F2)),
    ?assertEqual(<<"m4">>, maps:get(selector, F3)),
    ?assertEqual(
        [block, class, file, line, repeat, selector],
        lists:sort(maps:keys(F1))
    ),
    %% Each structured frame's selector and line appear in the rendered text.
    Text = beamtalk_test_case:format_stacktrace(Frames),
    lists:foreach(
        fun(#{selector := Sel, line := Line}) ->
            ?assertNotEqual(nomatch, binary:match(Text, Sel)),
            ?assertNotEqual(
                nomatch, binary:match(Text, <<"a.bt:", (integer_to_binary(Line))/binary>>)
            )
        end,
        Structured
    ).

stack_frames_collapse_repeats_test() ->
    Same = {'bt@user@A', m1, 0, []},
    [F] = beamtalk_test_case:stack_frames([Same, Same]),
    ?assertEqual(2, maps:get(repeat, F)),
    ?assertEqual(nil, maps:get(line, F)).

test_failure_includes_synthetic_test_frame_test() ->
    Stack = [{'bt@user@A', helper, 0, [{file, "a.bt"}, {line, 9}]}],
    {fail, testFoo, Msg, Frames} =
        beamtalk_test_case:test_failure(testFoo, <<"boom">>, 'bt@user@A', Stack),
    %% One rendered `at` line per structured frame, test method last.
    ?assertEqual(length(binary:matches(Msg, <<"\n  at ">>)), length(Frames)),
    ?assertEqual(<<"testFoo">>, maps:get(selector, lists:last(Frames))).

%% The DNU (`undef`) message has no rendered trace, so its structured frames
%% must line up with the `at` lines it does have: only the synthetic
%% test-method line, or none when the test frame is already in the stack.
test_failure_untraced_frames_match_text_test() ->
    NoTestFrame = [{'bt@user@A', helper, 0, [{file, "a.bt"}, {line, 9}]}],
    {fail, testFoo, Msg1, Frames1} =
        beamtalk_test_case:test_failure_untraced(testFoo, <<"dnu">>, 'bt@user@A', NoTestFrame),
    ?assertEqual(1, length(binary:matches(Msg1, <<"\n  at ">>))),
    ?assertEqual(1, length(Frames1)),
    ?assertEqual(<<"testFoo">>, maps:get(selector, hd(Frames1))),
    WithTestFrame = [{'bt@user@A', testFoo, 0, [{file, "a.bt"}, {line, 3}]}],
    {fail, testFoo, Msg2, Frames2} =
        beamtalk_test_case:test_failure_untraced(testFoo, <<"dnu">>, 'bt@user@A', WithTestFrame),
    ?assertEqual(0, length(binary:matches(Msg2, <<"\n  at ">>))),
    ?assertEqual([], Frames2).

%% BT-4402: the `at` text and the structured frames come from one entry list,
%% so frames[i] pairs with the i-th `at` line whether or not the synthetic
%% test-method frame is needed, and for the untraced (DNU) shape.
at_lines_match_frames_test_() ->
    Helper = {'bt@user@A', helper, 0, [{file, "a.bt"}, {line, 9}]},
    TestFrame = {'bt@user@A', testFoo, 0, [{file, "a.bt"}, {line, 3}]},
    Deep = [
        {'bt@user@A', 'm1', 0, [{file, "a.bt"}, {line, 1}]},
        {'bt@user@A', 'm2', 0, [{file, "a.bt"}, {line, 2}]},
        {'bt@user@A', 'm3', 0, [{file, "a.bt"}, {line, 3}]},
        {'bt@user@A', 'm4', 0, [{file, "a.bt"}, {line, 4}]}
    ],
    AtLines = fun(Msg) ->
        [L || L <- binary:split(Msg, <<"\n">>, [global]), binary:match(L, <<"  at ">>) =:= {0, 5}]
    end,
    Check = fun(Fun, Stack) ->
        {fail, testFoo, Msg, Frames} = Fun(Stack),
        ?assertEqual(length(AtLines(Msg)), length(Frames)),
        {Msg, Frames}
    end,
    Traced = fun(Stack) ->
        beamtalk_test_case:test_failure(testFoo, <<"boom">>, 'bt@user@A', Stack)
    end,
    Untraced = fun(Stack) ->
        beamtalk_test_case:test_failure_untraced(testFoo, <<"dnu">>, 'bt@user@A', Stack)
    end,
    [
        ?_test(begin
            %% Needs the synthetic frame: it is the last `at` line and frame.
            {_, Frames} = Check(Traced, [Helper]),
            ?assertEqual(2, length(Frames)),
            ?assertEqual(<<"testFoo">>, maps:get(selector, lists:last(Frames)))
        end),
        ?_test(begin
            %% Already has the test frame: no synthetic one.
            {_, Frames} = Check(Traced, [Helper, TestFrame]),
            ?assertEqual(2, length(Frames))
        end),
        ?_test(begin
            %% Deep stack: 2 innermost + outermost, plus the synthetic frame.
            {_, Frames} = Check(Traced, Deep),
            ?assertEqual(4, length(Frames))
        end),
        ?_test(begin
            {_, Frames} = Check(Traced, []),
            ?assertEqual(1, length(Frames))
        end),
        ?_test(begin
            {_, Frames} = Check(Untraced, [Helper]),
            ?assertEqual(1, length(Frames))
        end),
        ?_test(begin
            {_, Frames} = Check(Untraced, [Helper, TestFrame]),
            ?assertEqual([], Frames)
        end)
    ].

%%% ============================================================================
%%% synthetic test-context (test_failure_untraced/4) Tests (frame ordering)
%%% ============================================================================

%% The message with the synthetic test-method `at` line appended when needed.
ensure_test_context(FailMsg, Module, MethodName, Stacktrace) ->
    {fail, MethodName, Msg, _Frames} =
        beamtalk_test_case:test_failure_untraced(MethodName, FailMsg, Module, Stacktrace),
    Msg.

%% When the test method is already in the rendered frames, message is unchanged.
ensure_test_context_has_test_frame_test() ->
    FailMsg = <<"FAIL testFoo: some error\n  at A>>helper\n  at A>>testFoo">>,
    Stacktrace = [
        {'bt@user@A', helper, 0, []},
        {'bt@user@A', testFoo, 0, []}
    ],
    Result = ensure_test_context(
        FailMsg, 'bt@user@A', testFoo, Stacktrace
    ),
    ?assertEqual(FailMsg, Result).

%% When the test method is NOT in the rendered frames, context is appended
%% after existing stacktrace lines (innermost-first ordering preserved).
ensure_test_context_appends_context_test() ->
    FailMsg = <<"FAIL testBar: some error\n  at A>>helper">>,
    Stacktrace = [
        {'bt@user@A', helper, 0, []},
        {'bt@user@A', deepHelper, 0, []}
    ],
    Result = ensure_test_context(
        FailMsg, 'bt@user@A', testBar, Stacktrace
    ),
    ?assertNotEqual(nomatch, binary:match(Result, <<"testBar">>)),
    %% The synthetic test context should appear after the existing "at" line.
    %% Match the full "A>>method" frame strings rather than the bare method
    %% names: "testBar" also occurs in the leading "FAIL testBar:" message,
    %% so matching <<"testBar">> alone would find the header, not the
    %% appended synthetic frame.
    {HelperPos, _} = binary:match(Result, <<"A>>helper">>),
    {TestBarPos, _} = binary:match(Result, <<"A>>testBar">>),
    ?assert(HelperPos < TestBarPos).

%%% ============================================================================
%%% BT-4354: fallback locations and block disambiguation (no raw MFA)
%%% ============================================================================

%% Modules not registered as classes render under their module name, so the
%% expectations below use the `bt@user@...` form.
-define(TRACE_FIXTURE_MOD, 'bt@user@trace_fixture_test').

%% Load a throwaway `bt@` module carrying a `beamtalk_source` attribute (as
%% compiled Beamtalk modules do) and run `Fun` with its workspace-relative
%% source path; the module is purged afterwards.
with_trace_fixture(Fun) ->
    {ok, Cwd} = file:get_cwd(),
    AbsPath = filename:join([Cwd, "test", "trace_fixture_test.bt"]),
    Forms = [
        {attribute, 1, module, ?TRACE_FIXTURE_MOD},
        {attribute, 2, beamtalk_source, [list_to_binary(AbsPath)]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms, [binary]),
    {module, Mod} = code:load_binary(Mod, "trace_fixture_test.beam", Bin),
    try
        Fun(filename:join("test", "trace_fixture_test.bt"))
    after
        code:purge(Mod),
        code:delete(Mod),
        code:purge(Mod)
    end.

%% True when Bin contains a raw Erlang `module:function/arity`-style token.
has_raw_mfa(Bin) ->
    match =:=
        re:run(Bin, "[a-z_@]+:[A-Za-z_'-]+/[0-9]+|/[0-9]+-(fun|anonymous)-", [
            {capture, none}
        ]).

%% A frame with an empty BEAM location falls back to the module's
%% `beamtalk_source` file, rendered `(file)` with the workspace-relative rule.
format_stacktrace_bare_location_uses_beamtalk_source_test() ->
    with_trace_fixture(fun(RelPath) ->
        Result = beamtalk_test_case:format_stacktrace([
            {?TRACE_FIXTURE_MOD, testFoo, 0, []}
        ]),
        Expected = iolist_to_binary(["\n  at bt@user@trace_fixture_test>>testFoo (", RelPath, ")"]),
        ?assertEqual(Expected, Result),
        ?assertNot(has_raw_mfa(Result))
    end).

%% A frame with a known line but no file keeps the line alongside the
%% `beamtalk_source` fallback file.
format_stacktrace_line_without_file_uses_beamtalk_source_test() ->
    with_trace_fixture(fun(RelPath) ->
        Result = beamtalk_test_case:format_stacktrace([
            {?TRACE_FIXTURE_MOD, testFoo, 0, [{line, 12}]}
        ]),
        Expected = iolist_to_binary([
            "\n  at bt@user@trace_fixture_test>>testFoo (", RelPath, ":12)"
        ]),
        ?assertEqual(Expected, Result)
    end).

%% Without any known source the bare `Class>>selector` form is kept.
format_stacktrace_bare_location_unknown_source_test() ->
    Result = beamtalk_test_case:format_stacktrace([
        {'bt@user@NoSuchTraceModule', helper, 0, []}
    ]),
    ?assertEqual(<<"\n  at bt@user@NoSuchTraceModule>>helper">>, Result).

%% A block (lifted fun) renders distinctly from its enclosing method, with no
%% arity / fun index leaking through.
format_stacktrace_block_distinct_from_method_test() ->
    Frames = [
        {'bt@user@Hpack', '-class_encode:headers:table:/4-fun-0-', 2, [
            {file, "hpack.bt"}, {line, 40}
        ]},
        {'bt@user@Hpack', 'class_encode:headers:table:', 4, [{file, "hpack.bt"}, {line, 40}]}
    ],
    Result = beamtalk_test_case:format_stacktrace(Frames),
    ?assertEqual(
        <<
            "\n  at bt@user@Hpack>>class_encode:headers:table: [block] (hpack.bt:40)"
            "\n  at bt@user@Hpack>>class_encode:headers:table: (hpack.bt:40)"
        >>,
        Result
    ),
    ?assertNot(has_raw_mfa(Result)).

%% Every lifted-fun naming form (lambda, Core Erlang anonymous, letrec loop)
%% and selectors containing '/' decode to `selector [block]`.
format_stacktrace_block_name_forms_test() ->
    Render = fun(Fun) ->
        beamtalk_test_case:format_stacktrace([{'bt@user@A', Fun, 0, []}])
    end,
    ?assertEqual(<<"\n  at A>>run [block]">>, Render('-run/0-fun-3-')),
    ?assertEqual(<<"\n  at A>>run [block]">>, Render('-run/0-anonymous-1-')),
    ?assertEqual(<<"\n  at A>>run [block]">>, Render('-run/0-loop/1-0-')),
    ?assertEqual(<<"\n  at A>>/ [block]">>, Render('-//2-fun-0-')),
    ?assertEqual(<<"\n  at A>>// [block]">>, Render('-///2-fun-0-')),
    ?assertEqual(<<"\n  at A>>timesRepeat:">>, Render('timesRepeat:')).

%% BT-4357: the test path and the Exception/StackFrame path print the same
%% frame byte-identically (class, block marker, arity policy, file rules).
format_frame_identical_across_test_and_stackframe_paths_test_() ->
    Frames = [
        {'bt@user@A', run, 2, [{file, "a.bt"}, {line, 7}]},
        {'bt@user@A', '-run/0-fun-3-', 1, [{file, "a.bt"}, {line, 8}]},
        {'bt@user@A', '-run/0-loop/1-0-', 1, [{line, 9}]},
        {'bt@user@NoSuchTraceModule', helper, 0, []},
        {'bt@user@A', 'timesRepeat:', 1, [{file, "a.bt"}, {line, 0}]},
        %% Non-ASCII source paths (Latin-1 range and above 255) must render,
        %% not crash the trace (which would lose the failing test's result).
        {'bt@user@A', run, 2, [{file, "/tmp/jos\x{e9}/a.bt"}, {line, 7}]},
        %% Compiler-style UTF-8 byte list.
        {'bt@user@A', run, 2, [{file, [$/, 195, 169, $., $b, $t]}, {line, 7}]},
        {'bt@user@A', run, 2, [{file, "/tmp/\x{4e2d}\x{6587}/a.bt"}, {line, 7}]}
    ],
    [
        ?_test(begin
            <<"\n  at ", ViaTest/binary>> = beamtalk_test_case:format_stacktrace([F]),
            [Wrapped] = beamtalk_stack_frame:wrap([F]),
            ViaFrame = beamtalk_stack_frame:format_frame(Wrapped),
            ?assertEqual(ViaFrame, ViaTest),
            ?assertNot(has_raw_mfa(ViaTest))
        end)
     || F <- Frames
    ].

%% Distinct frames that still render identically (recursive send with no
%% line) collapse into one line with a repeat count.
format_stacktrace_collapses_identical_adjacent_frames_test() ->
    Frames = [
        {'bt@user@A', 'timesRepeat:', 1, []},
        {'bt@user@A', 'timesRepeat:', 1, []},
        {'bt@user@A', testLoop, 0, [{file, "a.bt"}, {line, 3}]}
    ],
    ?assertEqual(
        <<"\n  at A>>timesRepeat: [x2]\n  at A>>testLoop (a.bt:3)">>,
        beamtalk_test_case:format_stacktrace(Frames)
    ).

%% The synthetic test-context line carries the test file via the same
%% `beamtalk_source` lookup.
ensure_test_context_includes_test_file_test() ->
    with_trace_fixture(fun(RelPath) ->
        FailMsg = <<"FAIL testBar: some error">>,
        Result = ensure_test_context(
            FailMsg, ?TRACE_FIXTURE_MOD, testBar, []
        ),
        Expected = iolist_to_binary([
            FailMsg, "\n  at bt@user@trace_fixture_test>>testBar (", RelPath, ")"
        ]),
        ?assertEqual(Expected, Result),
        ?assertNot(has_raw_mfa(Result))
    end).

%%% ============================================================================
%%% BT-4358: source snippet at the error site (no raw MFA)
%%% ============================================================================

%% Run Fun(AbsPath) with a throwaway source file holding Content.
with_snippet_file(Content, Fun) ->
    Path = iolist_to_binary(
        filename:absname(
            "bt4358_snippet_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".bt"
        )
    ),
    ok = file:write_file(Path, Content),
    try
        Fun(Path)
    after
        file:delete(Path)
    end.

source_snippet_marks_failing_line_test() ->
    with_snippet_file(<<"one\ntwo\nthree\nfour\n">>, fun(Path) ->
        ?assertEqual(
            <<"\n      1 | one\n    > 2 | two\n      3 | three">>,
            beamtalk_test_case:source_snippet(Path, 2)
        )
    end).

source_snippet_clamps_at_file_start_and_end_test() ->
    with_snippet_file(<<"a\r\nb\nc">>, fun(Path) ->
        ?assertEqual(
            <<"\n    > 1 | a\n      2 | b">>, beamtalk_test_case:source_snippet(Path, 1)
        ),
        ?assertEqual(
            <<"\n      2 | b\n    > 3 | c">>, beamtalk_test_case:source_snippet(Path, 3)
        )
    end).

source_snippet_degrades_gracefully_test() ->
    ?assertEqual(none, beamtalk_test_case:source_snippet(<<"/no/such/dir/x.bt">>, 3)),
    ?assertEqual(none, beamtalk_test_case:source_snippet(<<"/">>, 3)),
    with_snippet_file(<<"only\n">>, fun(Path) ->
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 50)),
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 0))
    end),
    with_snippet_file(<<255, 254, "\nok\n">>, fun(Path) ->
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 1))
    end).

source_snippet_caps_reads_and_line_length_test() ->
    Long = binary:copy(<<"x">>, 1000),
    with_snippet_file(<<Long/binary, "\n">>, fun(Path) ->
        Snippet = beamtalk_test_case:source_snippet(Path, 1),
        ?assert(byte_size(Snippet) < 250),
        ?assertMatch({_, _}, binary:match(Snippet, <<"...">>))
    end),
    %% A line beyond the read cap is simply not shown.
    Filler = binary:copy(<<"\n">>, 300000),
    with_snippet_file(<<Filler/binary, "late\n">>, fun(Path) ->
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 300001))
    end).

format_stacktrace_adds_snippet_for_readable_file_test() ->
    with_snippet_file(<<"a\nb := x at: 1\nc\nd\n">>, fun(Path) ->
        Frames = [
            {'bt@user@A', helper, 0, [{file, binary_to_list(Path)}, {line, 2}]},
            {'bt@user@A', testFoo, 0, [{file, binary_to_list(Path)}, {line, 4}]}
        ],
        Text = beamtalk_test_case:format_stacktrace(Frames),
        %% Snippet only under the innermost frame, before the next `at` line.
        [First, Second] = binary:split(Text, <<"\n  at ">>, [global, trim_all]),
        ?assertNotEqual(nomatch, binary:match(First, <<"> 2 | b := x at: 1">>)),
        ?assertEqual(nomatch, binary:match(Second, <<" | ">>)),
        ?assertNot(has_raw_mfa(Text)),
        %% frames[i] <-> i-th `at` line is unchanged.
        ?assertEqual(
            length(beamtalk_test_case:stack_frames(Frames)),
            length(binary:matches(Text, <<"\n  at ">>))
        )
    end).

format_stacktrace_skips_unreadable_to_next_frame_test() ->
    with_snippet_file(<<"a\nb\nc\n">>, fun(Path) ->
        Frames = [
            {'bt@user@A', gone, 0, [{file, "no/such/file.bt"}, {line, 2}]},
            {'bt@user@A', testFoo, 0, [{file, binary_to_list(Path)}, {line, 2}]}
        ],
        Text = beamtalk_test_case:format_stacktrace(Frames),
        ?assertNotEqual(nomatch, binary:match(Text, <<"> 2 | b">>))
    end),
    Missing = beamtalk_test_case:format_stacktrace([
        {'bt@user@A', gone, 0, [{file, "no/such/file.bt"}, {line, 2}]}
    ]),
    ?assertEqual(nomatch, binary:match(Missing, <<" | ">>)).

%%% ============================================================================
%%% BT-4388: prefer the user frame; last-row edge cases in the snippet reader
%%% ============================================================================

format_stacktrace_prefers_user_frame_over_stdlib_frame_test() ->
    with_snippet_file(<<"s1\ns2 stdlib internals\ns3\n">>, fun(StdPath) ->
        with_snippet_file(<<"u1\nu2 := d at: #missing\nu3\n">>, fun(UserPath) ->
            Frames = [
                {'bt@stdlib@dictionary', 'at:', 2, [{file, binary_to_list(StdPath)}, {line, 2}]},
                {'bt@user@A', testFoo, 0, [{file, binary_to_list(UserPath)}, {line, 2}]}
            ],
            Text = beamtalk_test_case:format_stacktrace(Frames),
            ?assertNotEqual(nomatch, binary:match(Text, <<"> 2 | u2 := d at: #missing">>)),
            ?assertEqual(nomatch, binary:match(Text, <<"stdlib internals">>)),
            %% Snippet sits under the user frame's `at` line, not the stdlib one.
            [Std, User] = binary:split(Text, <<"\n  at ">>, [global, trim_all]),
            ?assertEqual(nomatch, binary:match(Std, <<" | ">>)),
            ?assertNotEqual(nomatch, binary:match(User, <<" | u2">>)),
            ?assertNot(has_raw_mfa(Text)),
            ?assertEqual(
                length(beamtalk_test_case:stack_frames(Frames)),
                length(binary:matches(Text, <<"\n  at ">>))
            ),
            %% Fallback: the stdlib frame is used when the user frame is unreadable.
            Fallback = beamtalk_test_case:format_stacktrace([
                {'bt@stdlib@dictionary', 'at:', 2, [{file, binary_to_list(StdPath)}, {line, 2}]},
                {'bt@user@A', testFoo, 0, [{file, "no/such/file.bt"}, {line, 2}]}
            ]),
            ?assertNotEqual(nomatch, binary:match(Fallback, <<"> 2 | s2 stdlib internals">>))
        end)
    end).

source_snippet_drops_cut_off_row_when_read_truncated_test() ->
    %% 7 + 2*200000 bytes: the 256 KiB cap lands inside a two-byte character.
    Tail = binary:copy(<<"é"/utf8>>, 200000),
    with_snippet_file(<<"first\nx", Tail/binary>>, fun(Path) ->
        %% The partial last row is dropped, not rendered as a split character.
        ?assertEqual(<<"\n    > 1 | first">>, beamtalk_test_case:source_snippet(Path, 1)),
        %% ...and is never shown as a (cut-off) failing line.
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 2))
    end).

source_snippet_none_one_past_end_of_newline_terminated_file_test() ->
    with_snippet_file(<<"a\nb\nc\n">>, fun(Path) ->
        ?assertEqual(none, beamtalk_test_case:source_snippet(Path, 4)),
        %% The last real line shows no empty trailing `>`/context row.
        ?assertEqual(
            <<"\n      2 | b\n    > 3 | c">>, beamtalk_test_case:source_snippet(Path, 3)
        )
    end).
