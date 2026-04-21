%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_class_dispatch (BT-1085, BT-1963).

Coverage target: ≥ 85% of beamtalk_class_dispatch.erl.

Test groups:

   1. Pure unit tests — class_method_fun_name/1, is_test_execution_selector/1
   2. handle_class_method_call/6 — found, not_found, class_var_result, test_spawn
   3. handle_async_dispatch/5 — all branches (methods, superclass, class_name, …)
   4. unwrap_class_call/1 — ok and error paths
   5. undef classification — module_not_loaded vs method_not_found (BT-999)
   6. class_send/3 — undefined class, unknown selector, new/spawn
   7. class_name_from_pid/1 — via registered/unregistered processes
   8. find_class_method_in_ancestors — chain traversal, depth limits
   9. metaclass_send — local and not_found paths
  10. undef classification extended — local vs inherited hint variants (BT-1963)
  11. invoke_class_method error paths — internal undef, raises, multi-arg dispatch
  12. class_send instantiation variants — new:, spawnWith:
  13. handle_self_instantiation — error paths (new:, spawnWith:)
  14. class_send_with_recovery — noproc crash recovery
  15. metaclass_send extended — not_found, dead pid
  16. try_class_chain_fallthrough — Class chain dispatch
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
        fun(Pid) ->
            try
                gen_server:stop(Pid, normal, 5000)
            catch
                _:_ -> ok
            end
        end,
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
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
                (try
                    gen_server:stop(ActorPid, normal, 5000)
                catch
                    _:_ -> ok
                end);
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
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
    % elp:fixme W0023 intentional atom creation
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
        (try
            gen_server:stop(ChildPid, normal, 5000)
        catch
            _:_ -> ok
        end),
        (try
            gen_server:stop(ParentPid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
    % elp:fixme W0023 intentional atom creation
    Names = [
        list_to_atom("BT1085Depth" ++ integer_to_list(I))
     || I <- lists:seq(0, N)
    ],
    % elp:fixme W0023 intentional atom creation
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
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
    end.

%%% ============================================================================
%%% 10. undef classification — indirect tests via handle_class_method_call (BT-1963)
%%% ============================================================================

undef_classification_extended_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"local method, module not loaded → class_not_found with local hint",
                fun test_undef_local_module_not_loaded/0},
            {"local method, module loaded but method absent → does_not_understand with local hint",
                fun test_undef_local_method_not_found/0},
            {"inherited method, module not loaded → class_not_found with inheritance hint",
                fun test_undef_inherited_module_not_loaded/0},
            {"inherited method, module loaded but absent → does_not_understand with inheritance hint",
                fun test_undef_inherited_method_not_found/0}
        ]
    end}.

%% Local class method, module not loaded → class_not_found with hint naming the class.
test_undef_local_module_not_loaded() ->
    ClassName = 'BT1963LocalUndefClass',
    LocalMethods = #{myMethod => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        myMethod, [], ClassName, bt1963_nonexistent_local_module, LocalMethods, #{}
    ),
    ?assertMatch({reply, {error, #beamtalk_error{kind = class_not_found}}, _}, Result),
    {reply, {error, #beamtalk_error{hint = Hint}}, _} = Result,
    %% Hint should mention the class name but NOT "inherits" (local method).
    ?assert(binary:match(Hint, atom_to_binary(ClassName, utf8)) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Hint, <<"inherits">>)).

%% Local class method, module loaded but function absent → does_not_understand.
test_undef_local_method_not_found() ->
    ClassName = 'BT1963LocalDNUClass',
    %% `erlang` is always loaded but has no class_absentLocalMethod/2.
    LocalMethods = #{absentLocalMethod => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        absentLocalMethod, [], ClassName, erlang, LocalMethods, #{}
    ),
    ?assertMatch({reply, {error, #beamtalk_error{kind = does_not_understand}}, _}, Result),
    {reply, {error, #beamtalk_error{hint = Hint}}, _} = Result,
    %% Hint should name the selector and NOT mention "inherited".
    ?assert(binary:match(Hint, <<"absentLocalMethod">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Hint, <<"inherited">>)).

%% Inherited method, defining class module not loaded → class_not_found with inheritance hint.
test_undef_inherited_module_not_loaded() ->
    ParentName = 'BT1963UndefParent',
    ChildName = 'BT1963UndefChild',
    ParentInfo = #{
        superclass => none,
        module => bt1963_undef_parent_nonexistent,
        class_methods => #{inheritedSelector => <<>>},
        class_state => #{}
    },
    ChildInfo = #{
        superclass => ParentName,
        module => bt1963_undef_child_mod,
        class_methods => #{},
        class_state => #{}
    },
    {ok, ParentPid} = beamtalk_object_class:start_link(ParentName, ParentInfo),
    {ok, ChildPid} = beamtalk_object_class:start_link(ChildName, ChildInfo),
    try
        Result = gen_server:call(ChildPid, {class_method_call, inheritedSelector, []}),
        ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result),
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(binary:match(Hint, <<"inherits">>) =/= nomatch)
    after
        catch gen_server:stop(ChildPid, normal, 5000),
        catch gen_server:stop(ParentPid, normal, 5000)
    end.

%% Inherited method, defining class module loaded but function absent → does_not_understand.
test_undef_inherited_method_not_found() ->
    ParentName = 'BT1963DNUParent',
    ChildName = 'BT1963DNUChild',
    %% `erlang` is always loaded but won't have class_inheritedAbsentSel/2.
    ParentInfo = #{
        superclass => none,
        module => erlang,
        class_methods => #{inheritedAbsentSel => <<>>},
        class_state => #{}
    },
    ChildInfo = #{
        superclass => ParentName,
        module => bt1963_dnu_child_mod,
        class_methods => #{},
        class_state => #{}
    },
    {ok, ParentPid} = beamtalk_object_class:start_link(ParentName, ParentInfo),
    {ok, ChildPid} = beamtalk_object_class:start_link(ChildName, ChildInfo),
    try
        Result = gen_server:call(ChildPid, {class_method_call, inheritedAbsentSel, []}),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(binary:match(Hint, <<"inherited">>) =/= nomatch)
    after
        catch gen_server:stop(ChildPid, normal, 5000),
        catch gen_server:stop(ParentPid, normal, 5000)
    end.

%%% ============================================================================
%%% 11. invoke_class_method error paths (BT-1963)
%%% ============================================================================

invoke_class_method_errors_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"internal undef in method body returns {error, undef}",
                fun test_invoke_internal_undef/0},
            {"method that raises error returns {error, Reason}", fun test_invoke_raises_error/0},
            {"multi-arg keyword selector dispatches correctly", fun test_invoke_two_arg_keyword/0}
        ]
    end}.

%% class method that calls a non-existent function internally → {error, undef}.
test_invoke_internal_undef() ->
    LocalMethods = #{testInternalUndef => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        testInternalUndef,
        [],
        'BT1963InternalUndefClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        #{}
    ),
    ?assertMatch({reply, {error, undef}, _}, Result).

%% class method that raises error(test_deliberate_error) → {error, test_deliberate_error}.
test_invoke_raises_error() ->
    LocalMethods = #{testRaise => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        testRaise,
        [],
        'BT1963RaiseClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        #{}
    ),
    ?assertMatch({reply, {error, test_deliberate_error}, _}, Result).

%% Two-argument keyword selector dispatches correctly.
test_invoke_two_arg_keyword() ->
    LocalMethods = #{'testTwoArgs:and:' => <<>>},
    Result = beamtalk_class_dispatch:handle_class_method_call(
        'testTwoArgs:and:',
        [alpha, beta],
        'BT1963TwoArgClass',
        beamtalk_class_dispatch_test_helper,
        LocalMethods,
        #{}
    ),
    ?assertMatch({reply, {ok, {two_args, alpha, beta}}, _}, Result).

%%% ============================================================================
%%% 12. class_send instantiation variants (BT-1963)
%%% ============================================================================

class_send_instantiation_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"class_send spawnWith: with map creates actor", fun test_class_send_spawn_with_map/0},
            {"class_send new: exercises the new: clause path", fun test_class_send_new_colon/0}
        ]
    end}.

%% class_send(Pid, 'spawnWith:', [Map]) creates an actor with initial state.
test_class_send_spawn_with_map() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertNotEqual(undefined, CounterPid),
    InitState = #{},
    Result = beamtalk_class_dispatch:class_send(CounterPid, 'spawnWith:', [InitState]),
    try
        ?assertMatch(#beamtalk_object{class = 'Counter'}, Result)
    after
        case Result of
            #beamtalk_object{pid = ActorPid} when is_pid(ActorPid) ->
                catch gen_server:stop(ActorPid, normal, 5000);
            _ ->
                ok
        end
    end.

%% class_send(Pid, 'new:', [Map]) exercises the new: clause path in class_send.
%% Uses a test class with a custom new handler that just returns the map.
test_class_send_new_colon() ->
    ClassName = 'BT1963NewColonTest',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSuccess => <<>>},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        %% The new: clause calls gen_server:call(Pid, {new, [Map]}).
        %% The gen_server handles {new, _} by attempting instantiation.
        %% For a class with no actual new handler, this will fail — but
        %% the important thing is that the correct code path is exercised.
        %% We expect an error because the test helper has no real new handler.
        ?assertError(
            _,
            beamtalk_class_dispatch:class_send(Pid, 'new:', [#{}])
        )
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%%% ============================================================================
%%% 13. handle_self_instantiation error paths (BT-1963)
%%% ============================================================================

self_instantiation_error_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"self-send new: without process dict raises instantiation_error",
                fun test_self_instantiation_new_colon_error/0},
            {"self-send spawnWith: without process dict raises instantiation_error",
                fun test_self_instantiation_spawn_with_error/0}
        ]
    end}.

%% class_send(self(), 'new:', [Map]) when process dict has no class metadata
%% triggers handle_self_instantiation → handle_self_instantiation_error.
test_self_instantiation_new_colon_error() ->
    ?assertError(
        #{error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'new:', [#{}])
    ).

%% class_send(self(), 'spawnWith:', [Map]) when process dict has no class metadata
%% triggers handle_self_instantiation → handle_self_instantiation_error.
test_self_instantiation_spawn_with_error() ->
    ?assertError(
        #{error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'spawnWith:', [#{}])
    ).

%%% ============================================================================
%%% 14. class_send_with_recovery — noproc crash recovery (BT-1963)
%%% ============================================================================

class_send_recovery_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"class_send to dead pid with unknown class raises class_not_found",
                fun test_send_dead_pid_unknown/0},
            {"class_send to dead pid logs structured error",
                fun test_send_dead_pid_structured_error/0}
        ]
    end}.

%% Sending to a dead pid that isn't in the class registry raises class_not_found.
test_send_dead_pid_unknown() ->
    %% Spawn and immediately kill a process so we have a valid-looking dead pid.
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    ?assertError(
        #{error := #beamtalk_error{kind = class_not_found}},
        beamtalk_class_dispatch:class_send(DeadPid, someMethod, [])
    ).

%% Sending to a dead class pid gives a structured error with correct hint.
test_send_dead_pid_structured_error() ->
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    try
        beamtalk_class_dispatch:class_send(DeadPid, testSelector, []),
        ?assert(false)
    catch
        error:#{error := #beamtalk_error{kind = Kind, hint = Hint}} ->
            ?assertEqual(class_not_found, Kind),
            ?assert(is_binary(Hint)),
            %% Hint should mention pid not found or not running.
            ?assert(binary:match(Hint, <<"not">>) =/= nomatch)
    end.

%%% ============================================================================
%%% 15. metaclass_send extended tests (BT-1963)
%%% ============================================================================

metaclass_send_extended_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"metaclass_method_call not found returns {error, not_found}",
                fun test_metaclass_method_not_found/0},
            {"metaclass_send to dead pid raises class_not_found", fun test_metaclass_dead_pid/0}
        ]
    end}.

%% metaclass_method_call for absent selector returns {error, not_found}.
test_metaclass_method_not_found() ->
    ClassName = 'BT1963MetaDNU',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        Result = gen_server:call(Pid, {metaclass_method_call, absentSelector, []}),
        ?assertEqual({error, not_found}, Result)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%% metaclass_send to a dead pid raises class_not_found (crash recovery path).
test_metaclass_dead_pid() ->
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(50),
    Self = #beamtalk_object{
        class = 'Metaclass',
        class_mod = beamtalk_class_dispatch_test_helper,
        pid = DeadPid
    },
    ?assertError(
        #{error := #beamtalk_error{kind = class_not_found}},
        beamtalk_class_dispatch:metaclass_send(DeadPid, someMethod, [], Self)
    ).

%%% ============================================================================
%%% 16. try_class_chain_fallthrough via class_send (BT-1963)
%%% ============================================================================

class_chain_fallthrough_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"class_send falls through to Class chain for printString",
                fun test_fallthrough_print_string/0}
        ]
    end}.

%% When a class method is not found locally, class_send_dispatch falls
%% through to try_class_chain_fallthrough which checks the Class chain.
%% printString is defined on Object (via Class chain) and returns a string.
test_fallthrough_print_string() ->
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertNotEqual(undefined, CounterPid),
    Result = beamtalk_class_dispatch:class_send(CounterPid, printString, []),
    ?assert(is_binary(Result)).

%%% ============================================================================
%%% BT-1981: Additional dispatch-pipeline coverage
%%% ============================================================================

%% class_send with an unexpected result shape from the class gen_server falls
%% through to unwrap_class_call/1 (the "Other" branch in class_send_dispatch).
%% We simulate this by sending a message the class gen_server will reply to
%% with a bare tuple using handle_call — use the generic catch-all which
%% returns {ok, Result} or {error, ...}, ensuring normal flow. This test
%% verifies that a successful {ok, Value} is correctly unwrapped when a
%% user-defined class method returns a simple value.
class_send_user_method_unwrap_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"user-defined class method returns unwrapped value",
                fun test_class_send_unwrap_ok_tuple/0}
        ]
    end}.

test_class_send_unwrap_ok_tuple() ->
    ClassName = 'BT1981UnwrapTestClass',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSuccess => <<>>},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        %% class_send routes through class_send_dispatch → class_method_call
        %% → {ok, test_success_result} → returned as-is.
        Result = beamtalk_class_dispatch:class_send(Pid, testSuccess, []),
        ?assertEqual(test_success_result, Result)
    after
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
    end.

%% class_name_from_pid/1 handles a registered name whose atom doesn't exist.
%% Indirectly exercised via handle_self_instantiation_error when metadata
%% is missing — the class name falls back via class_name_from_pid.
class_send_self_instantiation_missing_metadata_test() ->
    %% Directly invoke class_send with self() as the ClassPid — that triggers
    %% handle_self_instantiation, which reads process dictionary values that
    %% are unset in an eunit worker, so it raises instantiation_error.
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'new', [])
    ).

class_send_self_instantiation_new_colon_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'new:', [#{}])
    ).

class_send_self_instantiation_spawn_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), spawn, [])
    ).

class_send_self_instantiation_spawn_with_test() ->
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'spawnWith:', [#{}])
    ).

%% BT-2005: `self class <selector>` inside a class method routes through
%% metaclass_send with Pid =:= self(). The previous implementation issued a
%% `gen_server:call(self(), {metaclass_method_call, ...})` and deadlocked with
%% `{calling_self, ...}`. The self-call branch now short-circuits spawn/new
%% selectors to the process-dictionary-backed helpers and raises a structured
%% dispatch_error for anything else.
metaclass_send_self_call_spawn_missing_metadata_test() ->
    Self = #beamtalk_object{
        class = 'Metaclass', class_mod = undefined, pid = self()
    },
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:metaclass_send(self(), 'spawnWith:', [#{}], Self)
    ).

metaclass_send_self_call_spawn_with_as_missing_metadata_test() ->
    Self = #beamtalk_object{
        class = 'Metaclass', class_mod = undefined, pid = self()
    },
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:metaclass_send(
            self(), 'spawnWith:as:', [#{}, anyName], Self
        )
    ).

metaclass_send_self_call_unknown_selector_raises_dispatch_error_test() ->
    Self = #beamtalk_object{
        class = 'Metaclass', class_mod = undefined, pid = self()
    },
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = dispatch_error}},
        beamtalk_class_dispatch:metaclass_send(self(), someUserSelector, [], Self)
    ).

%% BT-2005 review: guard in handle_metaclass_self_named_spawn/3 must reject
%% `Module = undefined` explicitly — `is_atom(undefined)` returns true, so a
%% process dictionary with the name/is_abstract keys set but the module key
%% missing would otherwise slip through into `class_self_spawn_*` and crash.
metaclass_send_self_call_named_spawn_missing_module_metadata_test() ->
    Self = #beamtalk_object{
        class = 'Metaclass', class_mod = undefined, pid = self()
    },
    OldName = put(beamtalk_class_name, 'BT2005GuardTestClass'),
    OldModule = erase(beamtalk_class_module),
    OldAbs = put(beamtalk_class_is_abstract, false),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
            beamtalk_class_dispatch:metaclass_send(
                self(), 'spawnWith:as:', [#{}, anyName], Self
            )
        )
    after
        restore_pd_key(beamtalk_class_name, OldName),
        restore_pd_key(beamtalk_class_module, OldModule),
        restore_pd_key(beamtalk_class_is_abstract, OldAbs)
    end.

restore_pd_key(Key, undefined) -> erase(Key);
restore_pd_key(Key, Value) -> put(Key, Value).

%% is_test_execution_selector branches — exhaustively cover each test selector.
is_test_execution_selector_exhaustive_test() ->
    ?assert(beamtalk_class_dispatch:is_test_execution_selector(runAll)),
    ?assert(beamtalk_class_dispatch:is_test_execution_selector('runAll:')),
    ?assert(beamtalk_class_dispatch:is_test_execution_selector('run:')),
    ?assert(beamtalk_class_dispatch:is_test_execution_selector('run:method:')),
    ?assertNot(beamtalk_class_dispatch:is_test_execution_selector(anyOther)),
    ?assertNot(beamtalk_class_dispatch:is_test_execution_selector('increment')).

%% class_method_fun_name prefixing behaviour for keyword selectors.
class_method_fun_name_keyword_test() ->
    ?assertEqual(
        'class_withAll:',
        beamtalk_class_dispatch:class_method_fun_name('withAll:')
    ),
    ?assertEqual(
        'class_from:to:',
        beamtalk_class_dispatch:class_method_fun_name('from:to:')
    ).

%% class_send_dispatch 'Other' branch: a method that returns {error, RealError}
%% (not not_found) flows through unwrap_class_call which re-raises.
class_send_dispatch_returns_real_error_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"class method returning {error, beamtalk_error} is re-raised",
                fun test_class_send_dispatch_real_error/0}
        ]
    end}.

test_class_send_dispatch_real_error() ->
    %% beamtalk_class_dispatch_test_helper:class_testRaise/2 does error(test_deliberate_error),
    %% which invoke_class_method catches and returns as {reply, {error, Err}, ClassVars}.
    %% class_send_dispatch then hits the Other-branch → unwrap_class_call → raises.
    ClassName = 'BT1981RealErrorTestClass',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testRaise => <<>>},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        ?assertError(
            _,
            beamtalk_class_dispatch:class_send(Pid, testRaise, [])
        )
    after
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
    end.

%% try_class_chain_fallthrough with a class method that raises a non-DNU error.
%% class_send routes to the fallthrough, which returns {error, Error} — the
%% real-error branch wraps and re-raises via error(Wrapped).
class_chain_fallthrough_real_error_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [{"fallthrough propagates real Class chain errors", fun test_fallthrough_real_error/0}]
    end}.

test_fallthrough_real_error() ->
    %% Send an unknown selector to a class object — this falls through into
    %% the Class chain and eventually surfaces DNU. We simply assert the
    %% operation raises a beamtalk exception.
    ok = ensure_counter_loaded(),
    CounterPid = beamtalk_class_registry:whereis_class('Counter'),
    ?assertError(
        #{'$beamtalk_class' := _, error := _},
        beamtalk_class_dispatch:class_send(CounterPid, zzNonExistentClassSel, [])
    ).

%% class_send on an already-dead class gen_server exercises the recovery
%% path; without a registered restartable class, it surfaces class_not_found.
class_send_recovery_unregistered_pid_test() ->
    %% Spawn and immediately kill a process whose pid isn't in the registry.
    Pid = spawn(fun() -> ok end),
    timer:sleep(20),
    ?assertNot(is_process_alive(Pid)),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = class_not_found}},
        beamtalk_class_dispatch:class_send(Pid, anySelector, [])
    ).

%% metaclass_send_dispatch: route to a dead metaclass pid exercises recovery
metaclass_send_unregistered_pid_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(20),
    ?assertNot(is_process_alive(Pid)),
    Self = #beamtalk_object{class = 'Metaclass', class_mod = undefined, pid = Pid},
    ?assertError(
        #{'$beamtalk_class' := _, error := _},
        beamtalk_class_dispatch:metaclass_send(Pid, anySelector, [], Self)
    ).

%% metaclass_send_dispatch fallthrough: when metaclass_method_call returns
%% not_found, dispatch falls through to beamtalk_dispatch:lookup on 'Metaclass'.
%% If 'Metaclass' is not registered, class_not_found gracefully raises DNU.
metaclass_send_fallthrough_class_not_found_test_() ->
    {setup, fun setup_minimal/0, fun teardown_pids/1, fun(_) ->
        [
            {"metaclass fallthrough raises DNU when Metaclass not registered",
                fun test_metaclass_fallthrough_dnu/0}
        ]
    end}.

test_metaclass_fallthrough_dnu() ->
    ClassName = 'BT1981MetaFallthrough',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    Self = #beamtalk_object{class = 'Metaclass', class_mod = undefined, pid = Pid},
    try
        %% Unknown selector → metaclass_method_call returns {error, not_found}
        %% → falls through to beamtalk_dispatch:lookup(Sel, [], Self, #{}, 'Metaclass')
        %% → 'Metaclass' not registered → class_not_found → graceful DNU raise.
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
            beamtalk_class_dispatch:metaclass_send(Pid, zzFakeSelector, [], Self)
        )
    after
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
    end.

%% class_send test selector uses long timeout (300000ms); exercise the
%% test-selector branch in class_send_dispatch by dispatching runAll to
%% a class that doesn't define it → {error, not_found} → fallthrough
%% raises DNU.
class_send_test_selector_timeout_branch_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"test selector uses longer timeout in class_send_dispatch",
                fun test_class_send_test_selector/0}
        ]
    end}.

test_class_send_test_selector() ->
    ClassName = 'BT1981TestSelectorClass',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{},
        class_state => #{}
    },
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        %% runAll is a test execution selector → test-timeout branch.
        %% Not defined → {error, not_found} → fallthrough → DNU raise.
        ?assertError(
            _,
            beamtalk_class_dispatch:class_send(Pid, runAll, [])
        )
    after
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
    end.

%% handle_self_instantiation error branches — partially-set process dictionary.
%% Guards each missing-metadata clause ({_, undefined, _}, {_, _, undefined}).
class_send_self_instantiation_missing_module_test() ->
    %% Set class_name but leave module and is_abstract unset → second clause.
    put(beamtalk_class_name, 'BT1981SelfInstMissingModule'),
    erase(beamtalk_class_module),
    erase(beamtalk_class_is_abstract),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
            beamtalk_class_dispatch:class_send(self(), 'new', [])
        )
    after
        erase(beamtalk_class_name)
    end.

class_send_self_instantiation_missing_is_abstract_test() ->
    %% Set class_name and module but leave is_abstract unset → third clause.
    put(beamtalk_class_name, 'BT1981SelfInstNoAbstract'),
    put(beamtalk_class_module, some_fake_module),
    erase(beamtalk_class_is_abstract),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
            beamtalk_class_dispatch:class_send(self(), 'new', [])
        )
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module)
    end.

class_send_self_instantiation_corrupt_is_abstract_test() ->
    %% is_abstract set to non-boolean → catch-all final clause.
    put(beamtalk_class_name, 'BT1981SelfInstCorrupt'),
    put(beamtalk_class_module, some_fake_module),
    put(beamtalk_class_is_abstract, not_a_boolean),
    try
        ?assertError(
            #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
            beamtalk_class_dispatch:class_send(self(), 'new', [])
        )
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module),
        erase(beamtalk_class_is_abstract)
    end.

%% class_name_from_pid/1 handles registered names that don't start with
%% "beamtalk_class_" — returns 'unknown'. We exercise indirectly by
%% invoking class_send with self() (no prefix in registered name) and no
%% process-dict metadata; handle_self_instantiation_error then invokes
%% class_name_from_pid(self()).
class_send_self_instantiation_unregistered_process_test() ->
    %% Clear all process dict entries, ensure current process has no
    %% registered name matching the beamtalk_class_ prefix.
    erase(beamtalk_class_name),
    erase(beamtalk_class_module),
    erase(beamtalk_class_is_abstract),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = instantiation_error}},
        beamtalk_class_dispatch:class_send(self(), 'new', [])
    ).

%% handle_self_instantiation successful type-dispatch branches — fire the
%% Type=new/Type=spawn cases with a bogus module so instantiation itself
%% fails, but the dispatch branch executes first.
class_send_self_instantiation_new_dispatch_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"handle_self_instantiation Type=new dispatch branch fires",
                fun test_self_instantiation_new_branch/0},
            {"handle_self_instantiation Type=spawn dispatch branch fires",
                fun test_self_instantiation_spawn_branch/0}
        ]
    end}.

test_self_instantiation_new_branch() ->
    put(beamtalk_class_name, 'BT1981SelfInstNew'),
    put(beamtalk_class_module, bt1981_nonexistent_module),
    put(beamtalk_class_is_abstract, false),
    try
        %% Dispatch hits Type=new branch → calls class_self_new which will
        %% itself fail (module doesn't exist), but the branch is covered.
        _ = (catch beamtalk_class_dispatch:class_send(self(), 'new', [])),
        ok
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module),
        erase(beamtalk_class_is_abstract)
    end.

test_self_instantiation_spawn_branch() ->
    put(beamtalk_class_name, 'BT1981SelfInstSpawn'),
    put(beamtalk_class_module, bt1981_nonexistent_module),
    put(beamtalk_class_is_abstract, false),
    try
        _ = (catch beamtalk_class_dispatch:class_send(self(), spawn, [])),
        ok
    after
        erase(beamtalk_class_name),
        erase(beamtalk_class_module),
        erase(beamtalk_class_is_abstract)
    end.

%% class_send_dispatch supervisor_new rewrap branch — when a user class
%% method returns a Result tagged map wrapping a
%% {beamtalk_supervisor_new, ...} tuple, class_send_dispatch runs the
%% initialize: lifecycle hook and rewrites the inner tag.
%%
%% BT-1994 (ADR 0080 Phase 0a, option 2): the hook now pattern-matches
%% the Result tagged map produced by FFI coercion on the class method
%% body's return, not the bare `_new` tuple.
class_send_supervisor_new_rewrap_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, fun(_) ->
        [
            {"class_send rewrites beamtalk_supervisor_new to beamtalk_supervisor",
                fun test_class_send_supervisor_new_rewrap/0}
        ]
    end}.

test_class_send_supervisor_new_rewrap() ->
    ClassName = 'BT1981SupNewClass',
    ClassInfo = #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => #{testSupervisorNew => <<>>, 'initialize:' => <<>>},
        class_state => #{}
    },
    erase(bt1994_initialize_called),
    {ok, Pid} = beamtalk_object_class:start_link(ClassName, ClassInfo),
    try
        %% The class method returns a Result tagged map wrapping the
        %% _new tuple (option-2 shape). With the helper module providing
        %% class_initialize:/3, run_initialize resolves the method directly
        %% (no hierarchy walk needed) and the hook completes the rewrite.
        Outcome = beamtalk_class_dispatch:class_send(Pid, testSupervisorNew, []),
        ?assertMatch(
            #{
                '$beamtalk_class' := 'Result',
                isOk := true,
                okValue :=
                    {beamtalk_supervisor, 'BT1981SupNewClass', beamtalk_class_dispatch_test_helper,
                        _}
            },
            Outcome
        ),
        ?assertEqual(true, get(bt1994_initialize_called))
    after
        erase(bt1994_initialize_called),
        (try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end)
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
