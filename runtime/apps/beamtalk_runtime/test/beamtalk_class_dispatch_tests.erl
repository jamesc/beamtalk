%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_dispatch (BT-1085).

Coverage target: ≥ 60% of beamtalk_class_dispatch.erl.

Test groups:

  1. Pure unit tests — class_method_fun_name/1, is_test_execution_selector/1
  2. handle_class_method_call/6 — found, not_found, class_var_result, test_spawn
  3. handle_async_dispatch/5 — all branches (methods, superclass, class_name, …)
  4. unwrap_class_call/1 — ok and error paths
  5. undef classification — module_not_loaded vs method_not_found (BT-999)
  6. class_send/3 — undefined class, unknown selector, new/spawn
  7. class_name_from_pid/1 — via registered/unregistered processes
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Selector used in undef-classification tests (kept from BT-999 tests).
-define(TEST_SELECTOR, bt999_dispatch_test_method).

%%% ============================================================================
%%% Setup / Teardown helpers
%%% ============================================================================

%% Minimal setup: pg process group + ETS hierarchy table.
%% Use start_link result instead of whereis/2 to avoid TOCTOU race under
%% parallel test execution (another process can start pg between the check
%% and the call, making the {ok, _} match fail).
setup_minimal() ->
    case pg:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    [].

teardown_pids(Pids) ->
    lists:foreach(
        fun(Pid) -> catch gen_server:stop(Pid, normal, 5000) end,
        Pids
    ).

%% Full runtime setup used for class_send integration tests.
%% Pattern-match return values so setup failures produce clear errors
%% rather than confusing downstream test failures.
setup_runtime() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

teardown_runtime(_) ->
    ok.

%% Start a minimal class gen_server whose implementation module is Module.
start_test_class(ClassName, Module) ->
    ClassInfo = #{
        superclass => none,
        module => Module,
        class_methods => #{?TEST_SELECTOR => <<>>},
        class_state => #{}
    },
    beamtalk_object_class:start_link(ClassName, ClassInfo).

%% Start a class gen_server with a custom class_methods map.
start_test_class(ClassName, Module, ClassMethods) ->
    ClassInfo = #{
        superclass => none,
        module => Module,
        class_methods => ClassMethods,
        class_state => #{}
    },
    beamtalk_object_class:start_link(ClassName, ClassInfo).

%%% ============================================================================
%%% 1. Pure unit tests — class_method_fun_name/1 and is_test_execution_selector/1
%%% ============================================================================

class_method_fun_name_test_() ->
    [
        {"unary selector gets class_ prefix",
            ?_assertEqual(class_foo, beamtalk_class_dispatch:class_method_fun_name(foo))},
        {"keyword selector gets class_ prefix",
            ?_assertEqual(
                'class_with:',
                beamtalk_class_dispatch:class_method_fun_name('with:')
            )},
        {"multi-keyword selector gets class_ prefix",
            ?_assertEqual(
                'class_from:to:',
                beamtalk_class_dispatch:class_method_fun_name('from:to:')
            )},
        {"binary selector gets class_ prefix",
            ?_assertEqual('class_+', beamtalk_class_dispatch:class_method_fun_name('+'))},
        {"selector with uppercase gets class_ prefix",
            ?_assertEqual(
                'class_DefaultValue',
                beamtalk_class_dispatch:class_method_fun_name('DefaultValue')
            )}
    ].

is_test_execution_selector_test_() ->
    [
        {"runAll is a test execution selector",
            ?_assert(beamtalk_class_dispatch:is_test_execution_selector(runAll))},
        {"runAll: is a test execution selector",
            ?_assert(beamtalk_class_dispatch:is_test_execution_selector('runAll:'))},
        {"run: is a test execution selector",
            ?_assert(beamtalk_class_dispatch:is_test_execution_selector('run:'))},
        {"run:method: is a test execution selector",
            ?_assert(beamtalk_class_dispatch:is_test_execution_selector('run:method:'))},
        {"increment is not a test execution selector",
            ?_assertNot(beamtalk_class_dispatch:is_test_execution_selector(increment))},
        {"new is not a test execution selector",
            ?_assertNot(beamtalk_class_dispatch:is_test_execution_selector(new))},
        {"anything is not a test execution selector",
            ?_assertNot(beamtalk_class_dispatch:is_test_execution_selector(anything))}
    ].

%%% ============================================================================
%%% 2. handle_class_method_call/6 — direct tests (BT-1085)
%%% ============================================================================

handle_class_method_call_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"selector not in local methods and not in chain returns not_found",
                fun test_hcmc_not_found/0},
            {"selector in local methods, successful return", fun test_hcmc_success/0},
            {"selector in local methods, class_var_result return",
                fun test_hcmc_class_var_result/0},
            {"runAll selector with TestCase class returns test_spawn", fun test_hcmc_test_spawn/0},
            {"run: selector with TestCase class returns test_spawn",
                fun test_hcmc_test_spawn_run/0},
            {"keyword selector with one argument dispatches correctly", fun test_hcmc_keyword_arg/0}
        ]
    end}.

%% Selector absent from both local map and class chain → {error, not_found}.
%% Class not in hierarchy table so chain immediately returns not_found.
test_hcmc_not_found() ->
    Result = beamtalk_class_dispatch:handle_class_method_call(
        nonExistentSelector,
        [],
        'HcmcTestClass',
        some_module,
        #{},
        #{}
    ),
    ?assertEqual({error, not_found}, Result).

%% Selector in local class_methods, module has the function → {reply, {ok, V}, CVars}.
test_hcmc_success() ->
    LocalMethods = #{testSuccess => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        testSuccess,
        [],
        'HcmcSuccessClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        #{}
    ),
    ?assertMatch({reply, {ok, test_success_result}, _}, Result).

%% Class method returns {class_var_result, Value, NewVars} → state updated.
test_hcmc_class_var_result() ->
    LocalMethods = #{testClassVar => <<>>},
    InitVars = #{},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        testClassVar,
        [],
        'HcmcClassVarClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        InitVars
    ),
    ?assertMatch({reply, {ok, class_var_updated_value}, #{updated := true}}, Result).

%% runAll selector with ClassName='TestCase' → test_spawn (no erlang:apply call).
test_hcmc_test_spawn() ->
    LocalMethods = #{runAll => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        runAll,
        [],
        'TestCase',
        some_module,
        LocalMethods,
        #{}
    ),
    ?assertEqual(test_spawn, Result).

%% run: selector with ClassName='TestCase' → test_spawn.
test_hcmc_test_spawn_run() ->
    LocalMethods = #{'run:' => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        'run:',
        [some_test],
        'TestCase',
        some_module,
        LocalMethods,
        #{}
    ),
    ?assertEqual(test_spawn, Result).

%% One-argument keyword selector dispatched with correct arity.
test_hcmc_keyword_arg() ->
    LocalMethods = #{'testWith:' => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        'testWith:',
        [hello],
        'HcmcKeywordClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        #{}
    ),
    ?assertMatch({reply, {ok, {with_arg, hello}}, _}, Result).

%%% ============================================================================
%%% 3. handle_async_dispatch/5 — all message branches
%%% ============================================================================

handle_async_dispatch_test_() ->
    {foreach, fun setup_minimal/0, fun teardown_pids/1, [
        fun test_async_methods/1,
        fun test_async_superclass_atom/1,
        fun test_async_superclass_none/1,
        fun test_async_class_name/1,
        fun test_async_module_name/1,
        fun test_async_method_query/1,
        fun test_async_unknown_selector/1,
        fun test_async_bad_message/1
    ]}.

test_async_methods(_) ->
    {"methods returns list of instance method selectors", fun() ->
        InstanceMethods = #{increment => #{}, getValue => #{}},
        FuturePid = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {methods, []}, 'Counter', InstanceMethods, 'Actor', counter
        ),
        %% Reuse self() as FuturePid — not used in methods branch.
        %% The actual FuturePid in the message is from the cast tuple.
        %% Directly test the branch via gen_server cast on a class process.
        %% Since handle_async_dispatch only sends to FuturePid in the Msg tuple,
        %% build the tuple correctly.
        Ref = make_ref(),
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {methods, [], Me},
            'Counter',
            InstanceMethods,
            'Actor',
            counter
        ),
        receive
            {resolve, Keys} ->
                ?assertEqual(lists:sort([increment, getValue]), lists:sort(Keys))
        after 1000 ->
            ?assert(false)
        end,
        _ = Ref
    end}.

test_async_superclass_atom(_) ->
    {"superclass returns the superclass atom when defined", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {superclass, [], Me},
            'Counter',
            #{},
            'Actor',
            counter
        ),
        receive
            {resolve, 'Actor'} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_superclass_none(_) ->
    {"superclass returns nil when superclass is none", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {superclass, [], Me},
            'ProtoObject',
            #{},
            none,
            proto_object
        ),
        receive
            {resolve, nil} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_class_name(_) ->
    {"class_name returns the class name atom", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {class_name, [], Me},
            'MyTestClass',
            #{},
            none,
            my_test_module
        ),
        receive
            {resolve, 'MyTestClass'} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_module_name(_) ->
    {"module_name returns the module atom", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {module_name, [], Me},
            'MyTestClass',
            #{},
            none,
            my_test_module
        ),
        receive
            {resolve, my_test_module} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_method_query(_) ->
    {"method query returns reject with type_error", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {{method, increment}, [], Me},
            'Counter',
            #{},
            'Actor',
            counter
        ),
        receive
            {reject, #beamtalk_error{kind = type_error}} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_unknown_selector(_) ->
    {"unknown selector returns reject with does_not_understand", fun() ->
        Me = self(),
        beamtalk_class_dispatch:handle_async_dispatch(
            {unknownSelector, [], Me},
            'Counter',
            #{},
            'Actor',
            counter
        ),
        receive
            {reject, #beamtalk_error{kind = does_not_understand}} -> ok
        after 1000 ->
            ?assert(false)
        end
    end}.

test_async_bad_message(_) ->
    {"malformed message is ignored gracefully", fun() ->
        Result = beamtalk_class_dispatch:handle_async_dispatch(
            not_a_valid_dispatch_message,
            'Counter',
            #{},
            'Actor',
            counter
        ),
        ?assertEqual(ok, Result)
    end}.

%%% ============================================================================
%%% 4. unwrap_class_call/1
%%% ============================================================================

unwrap_class_call_test_() ->
    [
        {"unwrap {ok, Value} returns Value",
            ?_assertEqual(42, beamtalk_class_dispatch:unwrap_class_call({ok, 42}))},
        {"unwrap {ok, nil} returns nil",
            ?_assertEqual(nil, beamtalk_class_dispatch:unwrap_class_call({ok, nil}))},
        {"unwrap {ok, atom} returns atom",
            ?_assertEqual(hello, beamtalk_class_dispatch:unwrap_class_call({ok, hello}))},
        {"unwrap {error, _} raises an error",
            ?_assertError(
                _,
                beamtalk_class_dispatch:unwrap_class_call(
                    {error, beamtalk_error:new(does_not_understand, 'Foo', bar)}
                )
            )}
    ].

%%% ============================================================================
%%% 5. undef classification — module_not_loaded vs method_not_found (BT-999)
%%% ============================================================================

dispatch_undef_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"module not loaded returns class_not_found error", fun test_module_not_loaded/0},
            {"method not found returns does_not_understand error", fun test_method_not_found/0}
        ]
    end}.

%% When the class module is not loaded, `{class_method_call, …}` returns
%% a structured `class_not_found` error instead of a raw Erlang `undef`.
test_module_not_loaded() ->
    ClassName = 'BT999ModuleNotLoadedTest',
    %% bt999_not_loaded_module_bt999 is deliberately never loaded.
    {ok, Pid} = start_test_class(ClassName, bt999_not_loaded_module_bt999),
    try
        Result = gen_server:call(Pid, {class_method_call, ?TEST_SELECTOR, []}),
        ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result),
        %% The hint must name the receiver class.
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(binary:match(Hint, atom_to_binary(ClassName, utf8)) =/= nomatch)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%% When the module IS loaded but the class method function is absent,
%% `{class_method_call, …}` returns `does_not_understand` instead of raw undef.
test_method_not_found() ->
    ClassName = 'BT999MethodNotFoundTest',
    %% `erlang` is always loaded; `erlang:class_bt999_dispatch_test_method/2`
    %% does not exist, so erlang:apply raises undef at the dispatch call site.
    {ok, Pid} = start_test_class(ClassName, erlang),
    try
        Result = gen_server:call(Pid, {class_method_call, ?TEST_SELECTOR, []}),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
        %% The hint must name the selector.
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(
            binary:match(Hint, atom_to_binary(?TEST_SELECTOR, utf8)) =/= nomatch
        )
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%%% ============================================================================
%%% 6. class_send/3 — integration tests
%%% ============================================================================

class_send_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"class_send with undefined pid raises class_not_found",
                fun test_class_send_undefined/0},
            {"class_send spawn creates an actor instance", fun test_class_send_new/0},
            {"class_send unknown selector raises does_not_understand", fun test_class_send_dnu/0},
            {"class_send successful user-defined class method", fun test_class_send_user_method/0}
        ]
    end}.

%% class_send(undefined, ...) raises class_not_found immediately.
test_class_send_undefined() ->
    ?assertError(
        #{error := #beamtalk_error{kind = class_not_found}},
        beamtalk_class_dispatch:class_send(undefined, someMethod, [])
    ).

%% class_send(Pid, spawn, []) calls gen_server and creates a Counter actor instance.
%% Counter inherits from Actor, so spawn (not new) creates instances.
test_class_send_new() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertNotEqual(undefined, CounterPid),
    Result = beamtalk_class_dispatch:class_send(CounterPid, spawn, []),
    try
        %% spawn returns a beamtalk_object (Counter actor instance with pid)
        ?assertMatch(#beamtalk_object{class = 'Counter'}, Result)
    after
        %% Clean up the spawned actor process to prevent cross-test leakage.
        case Result of
            #beamtalk_object{pid = ActorPid} when is_pid(ActorPid) ->
                catch gen_server:stop(ActorPid, normal, 5000);
            _ ->
                ok
        end
    end.

%% class_send for a selector not defined anywhere raises does_not_understand.
test_class_send_dnu() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertError(
        #{error := #beamtalk_error{kind = does_not_understand, class = 'Counter'}},
        beamtalk_class_dispatch:class_send(CounterPid, definitelyNotAMethod, [])
    ).

%% class_send for a user-defined class method returns the result.
%% Uses a small test class with a real helper module.
test_class_send_user_method() ->
    ClassName = 'BT1085ClassSendTest',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSuccess => <<>>},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        Result = beamtalk_class_dispatch:class_send(Pid, testSuccess, []),
        ?assertEqual(test_success_result, Result)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%%% ============================================================================
%%% 7. class_name_from_pid — via registered/unregistered processes
%%%
%%% class_name_from_pid/1 is private but reachable via the self-instantiation
%%% error path: class_send(self(), spawn, []) when ClassPid =:= self() and the
%%% process dictionary has no beamtalk_class_name key calls
%%% handle_self_instantiation_error → class_name_from_pid(self()).
%%% ============================================================================

class_name_from_pid_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"beamtalk_class_ prefix extracted as class name",
                fun test_class_name_from_registered_pid/0},
            {"unregistered process yields unknown class name",
                fun test_class_name_from_unregistered_pid/0},
            {"wrong-prefix registered process yields unknown class name",
                fun test_class_name_from_wrong_prefix/0}
        ]
    end}.

%% Process registered as beamtalk_class_<ClassName>: class_name_from_pid/1
%% strips the prefix and returns <ClassName> as the class atom.
test_class_name_from_registered_pid() ->
    ClassName = 'BT1085NameTestPid',
    RegName = list_to_atom("beamtalk_class_" ++ atom_to_list(ClassName)),
    register(RegName, self()),
    try
        %% Trigger the self-instantiation error path:
        %%   class_send(self(), spawn, []) → handle_self_instantiation →
        %%   handle_self_instantiation_error → class_name_from_pid(self()).
        %% Process dict has no beamtalk_class_name so error path fires.
        %% class_name_from_pid reads registered_name, strips prefix, returns ClassName.
        ?assertError(
            #{error := #beamtalk_error{kind = instantiation_error, class = ClassName}},
            beamtalk_class_dispatch:class_send(self(), spawn, [])
        )
    after
        unregister(RegName)
    end.

%% An anonymous (unregistered) process: class_name_from_pid/1 returns `unknown`.
test_class_name_from_unregistered_pid() ->
    Parent = self(),
    %% Spawn a fresh, completely unregistered process.  Its process dict is
    %% empty so handle_self_instantiation_error fires, and class_name_from_pid
    %% finds no registered_name → returns `unknown`.
    spawn(fun() ->
        try
            beamtalk_class_dispatch:class_send(self(), spawn, [])
        catch
            error:#{error := Error} ->
                Parent ! {error_received, Error}
        end
    end),
    receive
        {error_received, #beamtalk_error{kind = instantiation_error, class = unknown}} ->
            ok
    after 5000 ->
        ?assert(false)
    end.

%% Process registered under a non-beamtalk_class_ prefix: class_name_from_pid/1
%% sees a registered name but the prefix does not match, so it returns `unknown`.
test_class_name_from_wrong_prefix() ->
    RegName = bt1085_test_process_wrong_prefix,
    register(RegName, self()),
    try
        ?assertError(
            #{error := #beamtalk_error{kind = instantiation_error, class = unknown}},
            beamtalk_class_dispatch:class_send(self(), spawn, [])
        )
    after
        unregister(RegName)
    end.

%%% ============================================================================
%%% 8. find_class_method_in_ancestors — chain traversal
%%% ============================================================================

class_method_chain_traversal_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"selector found in parent class is returned", fun test_chain_finds_parent_method/0},
            {"selector not in any ancestor returns not_found", fun test_chain_not_found_anywhere/0},
            {"max depth guard fires at depth > 20 via 22-class chain",
                fun test_chain_depth_limit/0},
            {"ETS-missing class terminates with none before depth guard",
                fun test_chain_none_termination/0}
        ]
    end}.

%% Register a parent and child class; child's handle_class_method_call
%% walks to parent and finds the method there.
test_chain_finds_parent_method() ->
    ParentName = 'BT1085Parent',
    ChildName = 'BT1085Child',

    ParentInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSuccess => <<>>},
        class_state => #{}
    },
    ChildInfo = #{
        superclass => ParentName,
        module => some_unloaded_module_bt1085,
        class_methods => #{},
        class_state => #{}
    },

    {ok, ParentPid} = beamtalk_object_class:start_link(ParentName, ParentInfo),
    {ok, ChildPid} = beamtalk_object_class:start_link(ChildName, ChildInfo),
    try
        %% handle_class_method_call on child for testSuccess — not in child,
        %% walks to parent and invokes beamtalk_class_dispatch_test_helper:class_testSuccess/2.
        Result = gen_server:call(ChildPid, {class_method_call, testSuccess, []}),
        ?assertMatch({ok, test_success_result}, Result)
    after
        catch gen_server:stop(ChildPid, normal, 5000),
        catch gen_server:stop(ParentPid, normal, 5000)
    end.

%% Selector not found in any ancestor → {error, not_found}.
test_chain_not_found_anywhere() ->
    ClassName = 'BT1085ChainNotFound',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        Result = gen_server:call(Pid, {class_method_call, completelyAbsentSelector, []}),
        ?assertEqual({error, not_found}, Result)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%% The ?MAX_HIERARCHY_DEPTH guard terminates runaway chain walks gracefully.
%%
%% Build 22 actual class processes (BT1085Depth0 … BT1085Depth21) where each
%% has the next as its superclass, and BT1085Depth21 points to the non-running
%% BT1085Depth22.  The walk increments the depth counter at each level:
%%   BT1085Depth1 → depth 0, BT1085Depth2 → depth 1, …, BT1085Depth21 → depth 20,
%%   BT1085Depth22 → depth 21 (21 > MAX_HIERARCHY_DEPTH=20) → guard fires.
test_chain_depth_limit() ->
    %% Build a 22-process chain; the 22nd class's superclass is a phantom name
    %% (not started) so the depth guard fires before any lookup attempt on it.
    N = 21,
    Names = [
        list_to_atom("BT1085Depth" ++ integer_to_list(I))
     || I <- lists:seq(0, N)
    ],
    PhantomName = list_to_atom("BT1085Depth" ++ integer_to_list(N + 1)),
    Pids = lists:map(
        fun(I) ->
            Name = lists:nth(I + 1, Names),
            Superclass =
                if
                    I < N -> lists:nth(I + 2, Names);
                    true -> PhantomName
                end,
            {ok, Pid} = beamtalk_object_class:start_link(Name, #{
                superclass => Superclass,
                module => bt1085_depth_test_nonexistent,
                class_methods => #{},
                class_state => #{}
            }),
            Pid
        end,
        lists:seq(0, N)
    ),
    try
        %% Walk from BT1085Depth0; depth guard fires at depth 21 before
        %% touching the phantom BT1085Depth22.
        Result = beamtalk_class_dispatch:handle_class_method_call(
            chainDepthTestSelector, [], hd(Names), bt1085_depth_test_nonexistent, #{}, #{}
        ),
        ?assertEqual({error, not_found}, Result)
    after
        lists:foreach(fun(P) -> catch gen_server:stop(P, normal, 5000) end, Pids)
    end.

%% ETS-missing class (superclass_from_ets returns none) terminates immediately.
%% This is the common fast path; the depth guard above covers the deep-chain path.
test_chain_none_termination() ->
    ClassName = 'BT1085NoChain',
    %% Class not in ETS so superclass_from_ets returns none on first call.
    Result = beamtalk_class_dispatch:handle_class_method_call(
        anySelector, [], ClassName, some_module, #{}, #{}
    ),
    ?assertEqual({error, not_found}, Result).

%%% ============================================================================
%%% 9. metaclass_send via gen_server path (minimal)
%%% ============================================================================

metaclass_send_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"metaclass_method_call found locally returns result",
                fun test_metaclass_method_found/0}
        ]
    end}.

test_metaclass_method_found() ->
    ClassName = 'BT1085MetaTest',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSuccess => <<>>},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        %% metaclass_method_call uses the same handler as class_method_call
        Result = gen_server:call(Pid, {metaclass_method_call, testSuccess, []}),
        ?assertMatch({ok, test_success_result}, Result)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

ensure_counter_loaded() ->
    case beamtalk_class_registry:whereis_class('Counter') of
        undefined ->
            case code:ensure_loaded('bt@counter') of
                {module, 'bt@counter'} ->
                    case erlang:function_exported('bt@counter', register_class, 0) of
                        true ->
                            'bt@counter':register_class(),
                            ok;
                        false ->
                            error(counter_no_register_function)
                    end;
                {error, Reason} ->
                    error({counter_module_not_found, Reason})
            end;
        _Pid ->
            ok
    end.
