%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_in_caller_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1b (BT-4300): a dynamic class send
(`beamtalk_class_dispatch:class_send/3`) to a class whose chain declares no
class variables, and that nothing shadows, runs the class method in the caller,
resolved through the `{class_method_owner, ClassPid, Selector}` memo. Each
decline (class variables in the class or an ancestor, a process-bound
selector, a class-variable reflection selector, a class-side extension) keeps
the class process, and an error has the same value on both paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_class_dispatch_test_helper).

-define(METHODS, #{
    testSelf => #{arity => 0},
    testClassSelf => #{arity => 0},
    testHasCount => #{arity => 0},
    testRaise => #{arity => 0},
    testSignal => #{arity => 0},
    testDnu => #{arity => 0},
    testInternalUndef => #{arity => 0},
    testScriptExit => #{arity => 0},
    testNlrThrow => #{arity => 0},
    'testWith:' => #{arity => 1},
    'run:' => #{arity => 1},
    'hasField:' => #{arity => 1}
}).

%% The generated process-bound list every compiled class carries (ADR 0142 §1
%% rule 3); its contents are Rust's, only `run:` matters here.
-define(META, #{process_bound_class_selectors => ['run:', runAll, new, spawn]}).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

start_class(Name, Superclass, ClassVars, Methods) ->
    ClassInfo = #{
        superclass => Superclass,
        module => ?HELPER,
        class_methods => Methods,
        class_state => ClassVars,
        meta => ?META
    },
    {ok, Pid} = beamtalk_object_class:start_link(Name, ClassInfo),
    unlink(Pid),
    Pid.

stop_class(Pid) ->
    catch gen_server:stop(Pid, normal, 5000),
    ok.

tag(Name) -> beamtalk_class_registry:class_object_tag(Name).

send(Pid, Selector, Args) -> beamtalk_class_dispatch:class_send(Pid, Selector, Args).

%% The outcome of a send, comparable across paths.
outcome(Pid, Selector) ->
    try send(Pid, Selector, []) of
        V -> {returned, V}
    catch
        Class:Reason -> {Class, strip_stacktrace(Reason)}
    end.

strip_stacktrace(#{} = Wrapped) -> maps:remove(raw_stacktrace, Wrapped);
strip_stacktrace(Other) -> Other.

with_class(Name, Superclass, ClassVars, Fun) ->
    Pid = start_class(Name, Superclass, ClassVars, ?METHODS),
    try
        Fun(Pid)
    after
        stop_class(Pid)
    end.

in_caller_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a stateless class's method runs in the caller", fun() ->
            with_class('Bt4300Stateless', none, #{}, fun(Pid) ->
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertEqual({with_arg, 7}, send(Pid, 'testWith:', [7])),
                %% Memoised under the class object's pid.
                ?assertMatch(
                    {owner, 'Bt4300Stateless', _, ?HELPER, 'Bt4300Stateless', ?HELPER,
                        class_testSelf},
                    beamtalk_class_derived_cache:fetch(
                        {class_method_owner, Pid, testSelf}, fun() -> {nocache, miss} end
                    )
                )
            end)
        end},
        {"the method receives the real class reference as ClassSelf", fun() ->
            with_class('Bt4300ClassSelf', none, #{}, fun(Pid) ->
                ?assertEqual(
                    #beamtalk_object{
                        class = tag('Bt4300ClassSelf'), class_mod = ?HELPER, pid = Pid
                    },
                    send(Pid, testClassSelf, [])
                )
            end)
        end},
        {"a class with class variables keeps its process", fun() ->
            with_class('Bt4300Stateful', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(Pid, send(Pid, testSelf, []))
            end)
        end},
        {"a stateless subclass of a stateful class keeps its process", fun() ->
            with_class('Bt4300Parent', none, #{count => 0}, fun(_ParentPid) ->
                Child = start_class('Bt4300Child', 'Bt4300Parent', #{}, #{}),
                try
                    ?assertEqual(Child, send(Child, testSelf, []))
                after
                    stop_class(Child)
                end
            end)
        end},
        {"an inherited method of a stateless chain runs in the caller", fun() ->
            with_class('Bt4300Root', none, #{}, fun(_RootPid) ->
                Leaf = start_class('Bt4300Leaf', 'Bt4300Root', #{}, #{}),
                try
                    ?assertEqual(self(), send(Leaf, testSelf, [])),
                    %% `self` is the receiving subclass.
                    ?assertMatch(
                        #beamtalk_object{class = 'Bt4300Leaf class', pid = Leaf},
                        send(Leaf, testClassSelf, [])
                    )
                after
                    stop_class(Leaf)
                end
            end)
        end},
        {"a process-bound selector keeps the class process", fun() ->
            with_class('Bt4300Bound', none, #{}, fun(Pid) ->
                Fun = fun() -> self() end,
                %% `run:` reaches 'class_run:' in the class process, which the
                %% helper does not export: the structured DNU proves the hop.
                ?assertMatch({error, _}, outcome_with(Pid, 'run:', [Fun])),
                ?assertEqual(
                    decline,
                    beamtalk_class_derived_cache:fetch(
                        {class_method_owner, Pid, 'run:'}, fun() -> {nocache, miss} end
                    )
                )
            end)
        end},
        {"a class-variable reflection selector keeps the class process", fun() ->
            with_class('Bt4300Reflect', none, #{}, fun(Pid) ->
                ?assertEqual(false, send(Pid, 'hasField:', [count])),
                ?assertEqual(
                    decline,
                    beamtalk_class_derived_cache:fetch(
                        {class_method_owner, Pid, 'hasField:'}, fun() -> {nocache, miss} end
                    )
                )
            end)
        end},
        {"a class-side extension on the class keeps its process", fun() ->
            with_class('Bt4300Extended', none, #{}, fun(Pid) ->
                Tag = tag('Bt4300Extended'),
                ok = beamtalk_extensions:register(Tag, bt4300Ext, fun(_, _) -> ext end, test),
                try
                    ?assertEqual(Pid, send(Pid, testSelf, []))
                after
                    beamtalk_extensions:unregister(Tag, bt4300Ext)
                end,
                ?assertEqual(self(), send(Pid, testSelf, []))
            end)
        end},
        {"a stateless class's state reads as empty in the caller", fun() ->
            with_class('Bt4300NoHome', none, #{}, fun(Pid) ->
                ?assertEqual(false, send(Pid, testHasCount, []))
            end),
            with_class('Bt4300Home', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(true, send(Pid, testHasCount, [])),
                ClassSelf = #beamtalk_object{
                    class = tag('Bt4300Home'), class_mod = ?HELPER, pid = Pid
                },
                ?assertError(
                    #{error := #beamtalk_error{kind = class_state_unreachable}},
                    beamtalk_class_vars:has(ClassSelf, count)
                )
            end)
        end},
        {"every error has the value it has through the class process", fun() ->
            %% Both paths log the failing method; keep the test output quiet.
            #{level := Level} = logger:get_primary_config(),
            ok = logger:set_primary_config(level, none),
            try
                errors_match()
            after
                logger:set_primary_config(level, Level)
            end
        end}
    ]}.

errors_match() ->
    with_class('Bt4300Errors', none, #{}, fun(Pid) ->
        Selectors = [
            testRaise, testSignal, testDnu, testInternalUndef, testScriptExit, testNlrThrow
        ],
        InCaller = [outcome(Pid, S) || S <- Selectors],
        %% The raw class-variable input bars the in-caller path, so
        %% the same sends now go through the class process.
        ok = beamtalk_class_shadow_flags:set_class_vars('Bt4300Errors', true),
        try
            ?assertEqual(Pid, send(Pid, testSelf, [])),
            ?assertEqual(InCaller, [outcome(Pid, S) || S <- Selectors])
        after
            beamtalk_class_shadow_flags:set_class_vars('Bt4300Errors', false)
        end
    end).

outcome_with(Pid, Selector, Args) ->
    try send(Pid, Selector, Args) of
        V -> {returned, V}
    catch
        Class:Reason -> {Class, Reason}
    end.

invalidation_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"invalidate/1 takes no lock when no derived flag is installed", fun() ->
            Name = 'Bt4300NoLock',
            Tag = tag(Name),
            _ = persistent_term:erase(Tag),
            Self = self(),
            Holder = spawn(fun() ->
                beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
                    Self ! locked,
                    receive
                        release -> ok
                    end
                end)
            end),
            receive
                locked -> ok
            after 5000 -> error(lock_not_taken)
            end,
            try
                {Time, ok} = timer:tc(fun() -> beamtalk_class_shadow_flags:invalidate(Name) end),
                ?assert(Time < 1000000)
            after
                Holder ! release
            end
        end},
        {"put_process_bound_selectors drops derived facts only on a change", fun() ->
            with_class('Bt4300Bound2', none, #{}, fun(Pid) ->
                Key = {class_method_owner, Pid, testSelf},
                Miss = fun() -> {nocache, miss} end,
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch(
                    {owner, _, _, _, _, _, _}, beamtalk_class_derived_cache:fetch(Key, Miss)
                ),
                ok = beamtalk_class_metadata:put_process_bound_selectors(
                    'Bt4300Bound2', maps:get(process_bound_class_selectors, ?META)
                ),
                ?assertMatch(
                    {owner, _, _, _, _, _, _}, beamtalk_class_derived_cache:fetch(Key, Miss)
                ),
                ok = beamtalk_class_metadata:put_process_bound_selectors('Bt4300Bound2', [testSelf]),
                ?assertEqual(miss, beamtalk_class_derived_cache:fetch(Key, Miss)),
                ?assertEqual(Pid, send(Pid, testSelf, []))
            end)
        end}
    ]}.
