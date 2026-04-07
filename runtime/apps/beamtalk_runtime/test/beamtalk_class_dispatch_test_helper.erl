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
    'class_testWith:'/3
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
