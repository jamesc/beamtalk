%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_test_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Test helper module for beamtalk_class_dispatch_tests (BT-1085).

Provides minimal class method implementations so beamtalk_class_dispatch
tests can exercise the successful invoke_class_method path without touching
production code.

Function naming convention follows Beamtalk's `class_` prefix scheme:
  class_<selector>/2  for zero-argument class methods
  class_<selector>/3  for one-argument class methods
""".

-export([
    class_testSuccess/2,
    class_testClassVar/2,
    'class_testWith:'/3,
    class_testInternalUndef/2,
    class_testRaise/2,
    'class_testTwoArgs:and:'/4,
    class_testSupervisorNew/2
]).

-doc """
Zero-argument class method that returns a plain value.

Exercises the `Result -> {reply, {ok, Result}, ClassVars}` path in
invoke_class_method.
""".
-spec class_testSuccess(term(), map()) -> term().
class_testSuccess(_ClassSelf, _ClassVars) ->
    test_success_result.

-doc """
Zero-argument class method that returns a class_var_result tuple.

Exercises the `{class_var_result, Value, NewClassVars}` path in
invoke_class_method, updating class-level variables.
""".
-spec class_testClassVar(term(), map()) -> {class_var_result, term(), map()}.
class_testClassVar(_ClassSelf, _ClassVars) ->
    {class_var_result, class_var_updated_value, #{updated => true}}.

-doc """
One-argument keyword class method that echoes its argument.

Exercises the keyword-selector arity path: erlang:apply receives
[ClassSelf, ClassVars, Arg] → function/3 → returns Arg.
""".
-spec 'class_testWith:'(term(), map(), term()) -> term().
'class_testWith:'(_ClassSelf, _ClassVars, Arg) ->
    {with_arg, Arg}.

-doc """
Zero-argument class method that calls a non-existent function internally.

Exercises the `false` branch of is_dispatch_undef — undef raised from
inside the method body (not at the dispatch site).
""".
-spec class_testInternalUndef(term(), map()) -> no_return().
class_testInternalUndef(_ClassSelf, _ClassVars) ->
    %% Call a function that does not exist in this module — triggers undef
    %% from inside the method body, not at the dispatch level.
    beamtalk_class_dispatch_test_helper:nonexistent_internal_function().

-doc """
Zero-argument class method that raises a runtime error.

Exercises the ErrClass:Error:ErrST catch branch in invoke_class_method.
""".
-spec class_testRaise(term(), map()) -> no_return().
class_testRaise(_ClassSelf, _ClassVars) ->
    error(test_deliberate_error).

-doc """
Two-argument keyword class method for arity testing.

Exercises dispatch with multiple keyword arguments:
erlang:apply receives [ClassSelf, ClassVars, Arg1, Arg2] → function/4.
""".
-spec 'class_testTwoArgs:and:'(term(), map(), term(), term()) -> term().
'class_testTwoArgs:and:'(_ClassSelf, _ClassVars, Arg1, Arg2) ->
    {two_args, Arg1, Arg2}.

-doc """
Zero-argument class method that returns a `beamtalk_supervisor_new` tuple.

BT-1981: Exercises the supervisor_new rewrap path in class_send_dispatch
where a freshly-started supervisor tuple is converted to the standard
supervisor tag after running the initialize: lifecycle hook.
""".
-spec class_testSupervisorNew(term(), map()) -> tuple().
class_testSupervisorNew(_ClassSelf, _ClassVars) ->
    %% Use self() as the supervisor pid; beamtalk_supervisor:run_initialize
    %% will attempt to dispatch initialize: on it which will no-op on the
    %% absence of an initialize: method. We just care that the rewrap branch
    %% runs; we accept any outcome from run_initialize here.
    {beamtalk_supervisor_new, 'BT1981SupTestClass', bt1981_sup_mod, self()}.
