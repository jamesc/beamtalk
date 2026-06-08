%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stdlib_test_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_stdlib_test module (BT-2236).

Covers format_result/1, matches_pattern/2, and run_and_assert/2 — the
core functions of the stdlib expression test runner that had no direct
EUnit coverage.  All three are exercised only indirectly by
`just test-stdlib`, which runs outside the rebar3 eunit coverage run.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% format_result/1 — Integer
%%% ============================================================================

format_result_integer_positive_test() ->
    ?assertEqual(<<"42">>, beamtalk_stdlib_test:format_result(42)).

format_result_integer_zero_test() ->
    ?assertEqual(<<"0">>, beamtalk_stdlib_test:format_result(0)).

format_result_integer_negative_test() ->
    ?assertEqual(<<"-5">>, beamtalk_stdlib_test:format_result(-5)).

format_result_large_integer_test() ->
    ?assertEqual(<<"1000000">>, beamtalk_stdlib_test:format_result(1000000)).

%%% ============================================================================
%%% format_result/1 — Float
%%% ============================================================================

format_result_float_test() ->
    ?assertEqual(<<"3.14">>, beamtalk_stdlib_test:format_result(3.14)).

format_result_float_whole_test() ->
    ?assertEqual(<<"1.0">>, beamtalk_stdlib_test:format_result(1.0)).

%%% ============================================================================
%%% format_result/1 — Boolean and Nil
%%% ============================================================================

format_result_true_test() ->
    ?assertEqual(<<"true">>, beamtalk_stdlib_test:format_result(true)).

format_result_false_test() ->
    ?assertEqual(<<"false">>, beamtalk_stdlib_test:format_result(false)).

format_result_nil_test() ->
    ?assertEqual(<<"nil">>, beamtalk_stdlib_test:format_result(nil)).

%%% ============================================================================
%%% format_result/1 — Atom (non-special)
%%% ============================================================================

format_result_atom_lowercase_test() ->
    ?assertEqual(<<"hello">>, beamtalk_stdlib_test:format_result(hello)).

format_result_atom_mixed_case_test() ->
    ?assertEqual(<<"MyClass">>, beamtalk_stdlib_test:format_result('MyClass')).

%%% ============================================================================
%%% format_result/1 — Binary
%%% ============================================================================

format_result_binary_nonempty_test() ->
    ?assertEqual(<<"hello world">>, beamtalk_stdlib_test:format_result(<<"hello world">>)).

format_result_binary_empty_test() ->
    ?assertEqual(<<>>, beamtalk_stdlib_test:format_result(<<>>)).

%%% ============================================================================
%%% format_result/1 — Function (Block)
%%% ============================================================================

format_result_block_arity0_test() ->
    ?assertEqual(<<"Block/0">>, beamtalk_stdlib_test:format_result(fun() -> ok end)).

format_result_block_arity1_test() ->
    ?assertEqual(<<"Block/1">>, beamtalk_stdlib_test:format_result(fun(_X) -> ok end)).

format_result_block_arity2_test() ->
    ?assertEqual(<<"Block/2">>, beamtalk_stdlib_test:format_result(fun(_, _) -> ok end)).

%%% ============================================================================
%%% format_result/1 — PID
%%% ============================================================================

format_result_pid_starts_with_actor_tag_test() ->
    Result = beamtalk_stdlib_test:format_result(self()),
    ?assertMatch(<<"#Actor<", _/binary>>, Result).

format_result_pid_ends_with_angle_bracket_test() ->
    Result = beamtalk_stdlib_test:format_result(self()),
    ?assertEqual($>, binary:last(Result)).

format_result_pid_inner_matches_pid_to_list_test() ->
    Pid = self(),
    Result = beamtalk_stdlib_test:format_result(Pid),
    PidStr = pid_to_list(Pid),
    Inner = list_to_binary(lists:sublist(PidStr, 2, length(PidStr) - 2)),
    ?assertEqual(<<"#Actor<", Inner/binary, ">">>, Result).

%%% ============================================================================
%%% format_result/1 — List
%%% ============================================================================

format_result_empty_list_test() ->
    ?assertEqual(<<"[]">>, beamtalk_stdlib_test:format_result([])).

%%% ============================================================================
%%% matches_pattern/2 — Exact match
%%% ============================================================================

matches_pattern_exact_success_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"42">>, <<"42">>)).

matches_pattern_exact_failure_test() ->
    ?assertNot(beamtalk_stdlib_test:matches_pattern(<<"42">>, <<"43">>)).

matches_pattern_empty_both_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<>>, <<>>)).

%%% ============================================================================
%%% matches_pattern/2 — Wildcard
%%% ============================================================================

matches_pattern_bare_wildcard_matches_any_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"_">>, <<"anything">>)).

matches_pattern_bare_wildcard_matches_empty_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"_">>, <<>>)).

matches_pattern_wildcard_in_angle_brackets_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"#Actor<_>">>, <<"#Actor<0.123.0>">>)).

matches_pattern_wildcard_angle_bracket_fails_test() ->
    ?assertNot(beamtalk_stdlib_test:matches_pattern(<<"#Actor<_>">>, <<"something else">>)).

matches_pattern_wildcard_in_middle_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"a _ b">>, <<"a xyz b">>)).

matches_pattern_wildcard_in_middle_no_match_test() ->
    ?assertNot(beamtalk_stdlib_test:matches_pattern(<<"a _ b">>, <<"a xyz c">>)).

matches_pattern_multiple_wildcards_test() ->
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"_ and _">>, <<"first and second">>)).

matches_pattern_actor_with_class_name_test() ->
    ?assert(
        beamtalk_stdlib_test:matches_pattern(
            <<"#Actor<Counter,_>">>, <<"#Actor<Counter,0.123.0>">>
        )
    ).

%%% ============================================================================
%%% matches_pattern/2 — Literal underscore in identifiers
%%% ============================================================================

matches_pattern_identifier_underscore_exact_match_test() ->
    %% Underscore flanked on both sides by alphanums is NOT a wildcard.
    ?assert(
        beamtalk_stdlib_test:matches_pattern(<<"does_not_understand">>, <<"does_not_understand">>)
    ).

matches_pattern_identifier_underscore_no_wildcard_test() ->
    %% Same pattern must NOT match a different identifier via the underscore.
    ?assertNot(
        beamtalk_stdlib_test:matches_pattern(<<"does_not_understand">>, <<"does_xyz_understand">>)
    ).

matches_pattern_trailing_underscore_is_wildcard_test() ->
    %% Underscore NOT flanked on the right by an alnum is a wildcard.
    ?assert(beamtalk_stdlib_test:matches_pattern(<<"hello_">>, <<"hello_world">>)).

%%% ============================================================================
%%% run_and_assert/2 — Helpers (dynamic eval module builders)
%%% ============================================================================

unique_mod(Prefix) ->
    list_to_atom(Prefix ++ integer_to_list(erlang:unique_integer([positive]))).

load_value_eval(Mod, Value) ->
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [
                {tuple, 1, [erl_parse:abstract(Value), {var, 1, 'Bindings'}]}
            ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

load_crashing_eval(Mod) ->
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [
                {call, 1, {atom, 1, error}, [{atom, 1, test_crash}]}
            ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

load_atom_error_eval(Mod, Kind) ->
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [
                {call, 1, {atom, 1, error}, [erl_parse:abstract(Kind)]}
            ]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

load_bt_error_eval(Mod, Kind) ->
    %% Raise a #beamtalk_error{} record — matched by run_one's direct-record clause.
    %% Use record syntax so the compiler catches field-order changes in beamtalk.hrl.
    ErrRecord = #beamtalk_error{
        kind = Kind,
        class = 'TestClass',
        selector = undefined,
        message = <<"test">>,
        hint = undefined,
        details = #{}
    },
    ErrTuple = erl_parse:abstract(ErrRecord),
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [{call, 1, {atom, 1, error}, [ErrTuple]}]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

load_wrapped_bt_error_eval(Mod, Kind) ->
    %% Raise an ADR-0015 wrapped exception: error(#{'$beamtalk_class' := _, error := Record}).
    %% Use record syntax so the compiler catches field-order changes in beamtalk.hrl.
    ErrRecord = #beamtalk_error{
        kind = Kind,
        class = 'TestClass',
        selector = undefined,
        message = <<"test">>,
        hint = undefined,
        details = #{}
    },
    ErrTuple = erl_parse:abstract(ErrRecord),
    WrappedMap = erl_parse:abstract(#{'$beamtalk_class' => 'RuntimeError', error => placeholder}),
    %% Patch the 'error' field in the abstract form to point to ErrTuple.
    {map, L, Fields} = WrappedMap,
    PatchedFields = lists:map(
        fun
            ({map_field_assoc, FL, {atom, _, error}, _}) ->
                {map_field_assoc, FL, {atom, FL, error}, ErrTuple};
            (F) ->
                F
        end,
        Fields
    ),
    PatchedMap = {map, L, PatchedFields},
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [{call, 1, {atom, 1, error}, [PatchedMap]}]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

load_future_rejected_eval(Mod, Kind) ->
    %% Throw a future_rejected tuple — matched by run_one's future-rejected clause.
    %% Use record syntax so the compiler catches field-order changes in beamtalk.hrl.
    ErrRecord = #beamtalk_error{
        kind = Kind,
        class = 'TestClass',
        selector = undefined,
        message = <<"test">>,
        hint = undefined,
        details = #{}
    },
    ErrTuple = erl_parse:abstract(ErrRecord),
    ThrowArg = {tuple, 1, [{atom, 1, future_rejected}, ErrTuple]},
    Forms = [
        {attribute, 1, module, Mod},
        {attribute, 1, export, [{eval, 1}]},
        {function, 1, eval, 1, [
            {clause, 1, [{var, 1, 'Bindings'}], [], [{call, 1, {atom, 1, throw}, [ThrowArg]}]}
        ]}
    ],
    {ok, Mod, Bin} = compile:forms(Forms),
    {module, Mod} = code:load_binary(Mod, [], Bin).

%%% ============================================================================
%%% run_and_assert/2 — Output-suppression and module-lifetime helpers
%%% ============================================================================

%% Redirect IO from Fun() to a sink process so run_and_assert FAIL/RESULTS
%% lines don't appear in EUnit output when testing failure paths.
suppress_output(Fun) ->
    Sink = spawn(fun sink/0),
    OldGL = group_leader(),
    group_leader(Sink, self()),
    try
        Fun()
    after
        group_leader(OldGL, self()),
        Sink ! stop
    end.

sink() ->
    receive
        stop ->
            ok;
        {io_request, From, ReplyAs, _Request} ->
            From ! {io_reply, ReplyAs, ok},
            sink();
        _ ->
            sink()
    end.

%% Run Fun() then delete and purge the dynamically loaded Mod so it does not
%% accumulate in the code server across EUnit runs.
with_module(Mod, Fun) ->
    try
        Fun()
    after
        code:delete(Mod),
        code:purge(Mod)
    end.

%%% ============================================================================
%%% run_and_assert/2 — Passing assertions
%%% ============================================================================

run_and_assert_value_pass_test() ->
    Mod = unique_mod("btst_vp_"),
    load_value_eval(Mod, 42),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value, Mod, <<"42">>, none, <<"test:1 `42`">>}
                ])
            end)
        )
    end).

run_and_assert_value_wildcard_pass_test() ->
    Mod = unique_mod("btst_wcp_"),
    load_value_eval(Mod, <<"hello world">>),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value_wildcard, Mod, <<"hello _">>, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_value_any_pass_test() ->
    Mod = unique_mod("btst_ap_"),
    load_value_eval(Mod, anything_at_all),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value_any, Mod, none, <<"test:1 `_`">>}
                ])
            end)
        )
    end).

run_and_assert_error_atom_pass_test() ->
    Mod = unique_mod("btst_eap_"),
    load_atom_error_eval(Mod, badarith),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, badarith, none, <<"test:1 `1/0`">>}
                ])
            end)
        )
    end).

run_and_assert_error_bt_record_pass_test() ->
    Mod = unique_mod("btst_btp_"),
    load_bt_error_eval(Mod, does_not_understand),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, does_not_understand, none, <<"test:1 `x foo`">>}
                ])
            end)
        )
    end).

run_and_assert_error_wrapped_exception_pass_test() ->
    Mod = unique_mod("btst_wep_"),
    load_wrapped_bt_error_eval(Mod, type_error),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, type_error, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_error_future_rejected_pass_test() ->
    Mod = unique_mod("btst_frp_"),
    load_future_rejected_eval(Mod, assertion_failed),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, assertion_failed, none, <<"test:1 `future`">>}
                ])
            end)
        )
    end).

%%% ============================================================================
%%% run_and_assert/2 — Failing assertions
%%% ============================================================================

run_and_assert_value_mismatch_fails_test() ->
    Mod = unique_mod("btst_vm_"),
    load_value_eval(Mod, 42),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 1},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value, Mod, <<"99">>, none, <<"test:1 `42`">>}
                ])
            end)
        )
    end).

run_and_assert_wildcard_mismatch_fails_test() ->
    Mod = unique_mod("btst_wcm_"),
    load_value_eval(Mod, <<"hello world">>),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 1},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value_wildcard, Mod, <<"goodbye _">>, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_crash_counts_as_failure_test() ->
    Mod = unique_mod("btst_cr_"),
    load_crashing_eval(Mod),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 1},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value, Mod, <<"42">>, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_error_not_raised_fails_test() ->
    Mod = unique_mod("btst_enr_"),
    load_value_eval(Mod, 42),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 1},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, badarith, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_error_wrong_kind_fails_test() ->
    Mod = unique_mod("btst_ewk_"),
    load_bt_error_eval(Mod, type_error),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 1},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {error, Mod, does_not_understand, none, <<"test:1 `expr`">>}
                ])
            end)
        )
    end).

%%% ============================================================================
%%% run_and_assert/2 — Binding propagation and counting
%%% ============================================================================

run_and_assert_bindings_propagate_test() ->
    Mod = unique_mod("btst_bp_"),
    load_value_eval(Mod, 42),
    with_module(Mod, fun() ->
        ?assertEqual(
            ok,
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value, Mod, <<"42">>, myvar, <<"test:1 `expr`">>},
                    {value, Mod, <<"42">>, none, <<"test:2 `expr`">>}
                ])
            end)
        )
    end).

run_and_assert_multiple_failures_counted_test() ->
    Mod = unique_mod("btst_mf_"),
    load_value_eval(Mod, 42),
    with_module(Mod, fun() ->
        ?assertError(
            {test_failures, 2},
            suppress_output(fun() ->
                beamtalk_stdlib_test:run_and_assert(btst_label, [
                    {value, Mod, <<"99">>, none, <<"test:1 `expr`">>},
                    {value, Mod, <<"88">>, none, <<"test:2 `expr`">>},
                    {value, Mod, <<"42">>, none, <<"test:3 `expr`">>}
                ])
            end)
        )
    end).
