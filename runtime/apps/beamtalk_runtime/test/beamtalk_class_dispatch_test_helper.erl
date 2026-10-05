%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_test_helper).

%%% **DDD Context:** Runtime — Object System

-moduledoc """
Test helper module for beamtalk_class_dispatch_tests.

Provides minimal class method implementations so beamtalk_class_dispatch
tests can exercise the successful invoke_class_method path without touching
production code.

Function naming convention follows Beamtalk's `class_` prefix scheme:
  class_<selector>/1  for zero-argument class methods
  class_<selector>/2  for one-argument class methods

(ADR 0130 §3: `ClassSelf` plus the arguments; class variables are accessed
through `beamtalk_class_vars`, never passed or returned.)
""".

-export([
    class_testSuccess/1,
    class_testSelf/1,
    class_testClassVar/1,
    'class_testWith:'/2,
    class_testInternalUndef/1,
    class_testRaise/1,
    class_testScriptExit/1,
    class_testNlrThrow/1,
    class_testWriteThenRaise/1,
    class_testWriteThenNlr/1,
    'class_testTwoArgs:and:'/3,
    class_testSupervisorNew/1,
    class_testAlreadyStarted/1,
    class_testSupError/1,
    class_testGroupLeader/1,
    'class_initialize:'/2
]).

-doc """
Zero-argument class method that returns a plain value.

Exercises the `Result -> {reply, {ok, Result}}` path in
invoke_class_method.
""".
-spec class_testSelf(term()) -> pid().
class_testSelf(_ClassSelf) ->
    self().

-spec class_testSuccess(term()) -> term().
class_testSuccess(_ClassSelf) ->
    test_success_result.

-doc """
Zero-argument class method that writes a class variable.

Writes `updated => true` through `beamtalk_class_vars:put/3` (the invocation's
installed home, ADR 0130 §1) and returns a bare value. Exercises the
`{ok, Result} -> {reply, {ok, Result}, get(Key)}` path in
invoke_class_method: the write is read back as the new class-variable map.
""".
-spec class_testClassVar(term()) -> term().
class_testClassVar(ClassSelf) ->
    beamtalk_class_vars:put(ClassSelf, updated, true),
    class_var_updated_value.

-doc """
Zero-argument class method that writes a class variable and then raises.

Exercises the `{error, E}` path: the escaping error discards the write and the
entry replies with the pre-call map (ADR 0130 §1, §4).
""".
-spec class_testWriteThenRaise(term()) -> no_return().
class_testWriteThenRaise(ClassSelf) ->
    beamtalk_class_vars:put(ClassSelf, count, 99),
    error(test_deliberate_error).

-doc """
Zero-argument class method that writes a class variable and then throws a `^`
non-local-return signal.

Exercises the `{nlr_relay, Nlr, _}` path: writes made before a foreign `^` are
kept (ADR 0130 §1).
""".
-spec class_testWriteThenNlr(term()) -> no_return().
class_testWriteThenNlr(ClassSelf) ->
    beamtalk_class_vars:put(ClassSelf, count, 99),
    throw({'$bt_nlr', bt3707_token, nlr_value, nlr_state}).

-doc """
One-argument keyword class method that echoes its argument.

Exercises the keyword-selector arity path: erlang:apply receives
[ClassSelf, Arg] → function/3 → returns Arg.
""".
-spec 'class_testWith:'(term(), term()) -> term().
'class_testWith:'(_ClassSelf, Arg) ->
    {with_arg, Arg}.

-doc """
Zero-argument class method that calls a non-existent function internally.

Exercises the `false` branch of is_dispatch_undef — undef raised from
inside the method body (not at the dispatch site).
""".
-spec class_testInternalUndef(term()) -> no_return().
class_testInternalUndef(_ClassSelf) ->
    %% Call a function that does not exist in this module — triggers undef
    %% from inside the method body, not at the dispatch level.
    beamtalk_class_dispatch_test_helper:nonexistent_internal_function().

-doc """
Zero-argument class method that raises a runtime error.

Exercises the ErrClass:Error:ErrST catch branch in invoke_class_method.
""".
-spec class_testRaise(term()) -> no_return().
class_testRaise(_ClassSelf) ->
    error(test_deliberate_error).

-doc """
Zero-argument class method that raises the connected `Program exit:` signal
(ADR 0099 §3) — `throw({beamtalk_script_exit, 7})` — exactly as
`beamtalk_program:'exit:'/1` does inside a method body. Exercises the
script-exit pass-through in the class-method apply catch + the re-raise in
class_send_dispatch/3.
""".
-spec class_testScriptExit(term()) -> no_return().
class_testScriptExit(_ClassSelf) ->
    throw({beamtalk_script_exit, 7}).

-doc """
Zero-argument class method that throws a `^` non-local return signal
(ADR 0110) — the state-carrying `{'$bt_nlr', Token, Value, State}`
4-tuple codegen throws for a foreign `^` unwinding out of a class method.
Exercises the `{nlr_relay, Nlr, ST}` catch in `invoke_class_method/7`.
""".
-spec class_testNlrThrow(term()) -> no_return().
class_testNlrThrow(_ClassSelf) ->
    throw({'$bt_nlr', bt3036_token, nlr_value, nlr_state}).

-doc """
Two-argument keyword class method for arity testing.

Exercises dispatch with multiple keyword arguments:
erlang:apply receives [ClassSelf, Arg1, Arg2] → function/4.
""".
-spec 'class_testTwoArgs:and:'(term(), term(), term()) -> term().
'class_testTwoArgs:and:'(_ClassSelf, Arg1, Arg2) ->
    {two_args, Arg1, Arg2}.

-doc """
Zero-argument class method that returns a Result-wrapped
`beamtalk_supervisor_new` tuple.

ADR 0080 Phase 0a (option 2): exercises the
supervisor_new rewrap path in `class_send_dispatch` where a
freshly-started supervisor tuple — now wrapped in a `Result` tagged
map by FFI coercion at the `(Erlang beamtalk_supervisor) startLink: self`
boundary — is converted to the standard supervisor tag after running
the `initialize:` lifecycle hook, and re-wrapped in the same `Result`
tagged map.
""".
-spec class_testSupervisorNew(term()) -> map().
class_testSupervisorNew(_ClassSelf) ->
    %% Inner tuple uses the helper module so run_initialize finds the
    %% `class_initialize:/2` defined below directly (no hierarchy walk),
    %% letting the test assert the happy-path rewrite deterministically.
    beamtalk_result:from_tagged_tuple(
        {ok, {beamtalk_supervisor_new, 'BT1981SupNewClass', ?MODULE, self()}}
    ).

-doc """
Zero-argument class method that returns a Result-wrapped
`beamtalk_supervisor` tuple (the already_started / idempotent path).

ADR 0080 Phase 1: exercises the class_send_dispatch hook's
pass-through behaviour for already-normalised supervisor tuples. The
hook must NOT call run_initialize on this path — only the `_new` tag
triggers initialization.
""".
-spec class_testAlreadyStarted(term()) -> map().
class_testAlreadyStarted(_ClassSelf) ->
    %% Use the ClassName from the class_send caller — it will match
    %% whatever ClassName beamtalk_object_class was started with.
    ClassName = get(beamtalk_class_name),
    beamtalk_result:from_tagged_tuple(
        {ok, {beamtalk_supervisor, ClassName, ?MODULE, self()}}
    ).

-doc """
Zero-argument class method that returns a Result error tagged map.

ADR 0080 Phase 1: exercises the class_send_dispatch hook's
pass-through behaviour for error Results. The hook must NOT call
run_initialize on error paths.
""".
-spec class_testSupError(term()) -> map().
class_testSupError(_ClassSelf) ->
    BtError = beamtalk_error:new(
        supervisor_start_failed,
        'TestClass',
        supervise,
        <<"test error">>
    ),
    beamtalk_result:from_tagged_tuple({error, BtError}).

-doc """
Zero-argument class method that reports where its output would go.

Returns the class gen_server's group leader *as seen from inside the
method body* together with the `beamtalk_entry_group_leader` key it was seeded
with. `Console` writes resolve `standard_io` to the group leader, so the first
element is exactly the sink a `Console printLine:` in this method would reach;
the second is what a nested class-method call would re-propagate.
""".
-spec class_testGroupLeader(term()) -> {pid(), pid() | undefined}.
class_testGroupLeader(_ClassSelf) ->
    {group_leader(), get(beamtalk_entry_group_leader)}.

-doc """
Synchronous `class_initialize:` target for the supervisor-new rewrap test.

Records that it ran in the process dictionary so the test can assert the
post-dispatch hook actually invoked `run_initialize/1` (rather than only
that the hook pattern-matched the Result tagged map).
""".
-spec 'class_initialize:'(term(), term()) -> term().
'class_initialize:'(_ClassSelf, _SupTuple) ->
    put(bt1994_initialize_called, true),
    nil.
