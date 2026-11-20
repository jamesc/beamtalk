%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_support_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
Unit tests for the pure helper functions in `beamtalk_repl_ops_browse_support`
(BT-4907 helpers, split in BT-5012).

The module's `safe_class_call/2` happy path and the error-builder functions are
exercised indirectly by `beamtalk_repl_ops_browse_tests` and
`beamtalk_repl_ops_dev_tests`.  This file targets the branches those indirect
tests do not reach:
- `atom_or_null/1`'s `none` clause
- `safe_bool/1`'s exception-catch arm
- `safe_class_call/2`'s `exit:{noproc,_}` arm
- Direct, assertion-level checks on `arg_error/2` and `not_found_error/2`

No workspace server or compiler is required — all five functions are pure or
depend only on standard OTP primitives.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% -----------------------------------------------------------------------
%%% atom_or_null/1
%%% -----------------------------------------------------------------------

atom_or_null_none_returns_null_test() ->
    ?assertEqual(null, beamtalk_repl_ops_browse_support:atom_or_null(none)).

atom_or_null_atom_returns_utf8_binary_test() ->
    ?assertEqual(<<"MyClass">>, beamtalk_repl_ops_browse_support:atom_or_null('MyClass')).

%%% -----------------------------------------------------------------------
%%% safe_bool/1
%%% -----------------------------------------------------------------------

safe_bool_true_returns_true_test() ->
    ?assertEqual(true, beamtalk_repl_ops_browse_support:safe_bool(fun() -> true end)).

safe_bool_false_returns_false_test() ->
    ?assertEqual(false, beamtalk_repl_ops_browse_support:safe_bool(fun() -> false end)).

%% The throw arm covers the `_:_` catch branch that indirect tests do not reach
%% (they never pass a throwing fun to safe_bool).
safe_bool_throwing_function_returns_false_test() ->
    ?assertEqual(false, beamtalk_repl_ops_browse_support:safe_bool(fun() -> throw(boom) end)).

%%% -----------------------------------------------------------------------
%%% arg_error/2
%%% -----------------------------------------------------------------------

arg_error_returns_argument_error_kind_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = argument_error}},
        beamtalk_repl_ops_browse_support:arg_error(<<"list-methods">>, <<"No class">>)
    ).

arg_error_message_includes_op_and_reason_test() ->
    {error, Err} = beamtalk_repl_ops_browse_support:arg_error(<<"list-methods">>, <<"No class">>),
    {beamtalk_string, Msg} = Err#beamtalk_error.message,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"list-methods">>)),
    ?assertNotEqual(nomatch, binary:match(Msg, <<"No class">>)).

%%% -----------------------------------------------------------------------
%%% not_found_error/2
%%% -----------------------------------------------------------------------

not_found_error_returns_argument_error_mentioning_class_test() ->
    {error, Err} = beamtalk_repl_ops_browse_support:not_found_error(<<"list-methods">>, 'Counter'),
    ?assertEqual(argument_error, Err#beamtalk_error.kind),
    {beamtalk_string, Msg} = Err#beamtalk_error.message,
    ?assertNotEqual(nomatch, binary:match(Msg, <<"Counter">>)).

%%% -----------------------------------------------------------------------
%%% safe_class_call/2 — noproc path
%%% -----------------------------------------------------------------------

%% A dead PID triggers exit:{noproc,_} inside gen_server:call. Monitor the
%% spawned process and wait for DOWN to guarantee the PID is gone before
%% the call, covering the `exit:{noproc, _}` catch arm.
safe_class_call_dead_pid_returns_undefined_test() ->
    Pid = spawn(fun() -> ok end),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end,
    ?assertEqual(undefined, beamtalk_repl_ops_browse_support:safe_class_call(Pid, some_call)).
