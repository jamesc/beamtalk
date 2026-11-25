%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_in_caller_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1b (BT-4300), ADR 0153 Phase 3a (BT-5158): a dynamic class
send (`beamtalk_class_dispatch:class_send/3`) to a class that nothing shadows
runs the class method in the caller, resolved through the
`{class_method_owner, ClassPid, Selector}` memo. Class variables, in the class
or its chain, are no input. Each decline (a process-bound selector, a
class-variable reflection selector, a class-side extension) keeps the class
process. An error has the same value on both paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
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
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

class_info(Superclass, ClassVars, Methods) ->
    #{
        superclass => Superclass,
        module => ?HELPER,
        class_methods => Methods,
        class_state => ClassVars,
        meta => ?META
    }.

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
    Pid = beamtalk_test_class:start(Name, class_info(Superclass, ClassVars, ?METHODS)),
    try
        Fun(Pid)
    after
        beamtalk_test_proc:stop(Pid)
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
        {"class-variable defaults no longer keep the class process (ADR 0153)", fun() ->
            with_class('Bt5158Stateful', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch({owner, 'Bt5158Stateful', _, _, _, _, _}, memo(Pid, testSelf))
            end)
        end},
        {"a subclass of a class with class-variable defaults runs in the caller", fun() ->
            with_class('Bt5158Parent', none, #{count => 0}, fun(_ParentPid) ->
                Child = beamtalk_test_class:start(
                    'Bt5158Child', class_info('Bt5158Parent', #{}, #{})
                ),
                try
                    ?assertEqual(self(), send(Child, testSelf, []))
                after
                    beamtalk_test_proc:stop(Child)
                end
            end)
        end},
        {"an inherited method of a stateless chain runs in the caller", fun() ->
            with_class('Bt4300Root', none, #{}, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start('Bt4300Leaf', class_info('Bt4300Root', #{}, #{})),
                try
                    ?assertEqual(self(), send(Leaf, testSelf, [])),
                    %% `self` is the receiving subclass.
                    ?assertMatch(
                        #beamtalk_object{class = 'Bt4300Leaf class', pid = Leaf},
                        send(Leaf, testClassSelf, [])
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
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
        {"with no class-variable home a class's state is unreachable", fun() ->
            %% ADR 0153 Phase 3a: no per-chain class-variable fact lets a
            %% home-less read answer the empty map any more (BT-5160 deletes
            %% the access module).
            with_class('Bt5158NoHome', none, #{}, fun(Pid) ->
                ClassSelf = #beamtalk_object{
                    class = tag('Bt5158NoHome'), class_mod = ?HELPER, pid = Pid
                },
                ?assertBtError(class_state_unreachable, beamtalk_class_vars:has(ClassSelf, count)),
                ?assertBtError(
                    class_state_unreachable, beamtalk_class_vars:clear(ClassSelf, count)
                ),
                ?assertBtError(
                    class_state_unreachable, beamtalk_class_vars:put(ClassSelf, count, 1)
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
        %% A runtime-fun shadow bars the in-caller path, so the same sends now
        %% go through the class process.
        ok = beamtalk_class_shadow_flags:set(runtime_fun, 'Bt4300Errors'),
        try
            ?assertEqual(Pid, send(Pid, testSelf, [])),
            ?assertEqual(InCaller, [outcome(Pid, S) || S <- Selectors])
        after
            beamtalk_class_shadow_flags:clear(runtime_fun, 'Bt4300Errors')
        end
    end).

%% Waits until `Base`'s metadata row is gone (a stand-in install's wait).
wait_until_row_gone(Base) ->
    beamtalk_test_wait:until(fun() ->
        beamtalk_class_metadata:lookup_superclass(Base) =:= not_found
    end).

outcome_with(Pid, Selector, Args) ->
    try send(Pid, Selector, Args) of
        V -> {returned, V}
    catch
        Class:Reason -> {Class, Reason}
    end.

%% ADR 0142 §2 (BT-4301), ADR 0153 §5: the guard of every static class-side
%% direct call, `beamtalk_class_shadow_flags:direct_call_ok/2`, has no
%% class-variable or chain input; only readiness and shadows count.
static_guard_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"the guard holds whatever an ancestor's class-variable defaults", fun() ->
            with_class('Bt4301Root', none, #{}, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start('Bt4301Leaf', class_info('Bt4301Root', #{}, #{})),
                try
                    ?assert(
                        beamtalk_class_shadow_flags:direct_call_ok(tag('Bt4301Leaf'), 'Bt4301Leaf')
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end),
            with_class('Bt4301StatefulRoot', none, #{count => 0}, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start(
                    'Bt4301StatefulLeaf', class_info('Bt4301StatefulRoot', #{}, #{})
                ),
                try
                    ?assert(
                        beamtalk_class_shadow_flags:direct_call_ok(
                            tag('Bt4301StatefulLeaf'), 'Bt4301StatefulLeaf'
                        )
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"re-parenting erases the descendant's derived flag (BT-4134)", fun() ->
            with_class('Bt4301ReparentPlain', none, #{}, fun(_PlainPid) ->
                with_class('Bt4301ReparentStateful', none, #{count => 0}, fun(_StatefulPid) ->
                    Leaf = beamtalk_test_class:start(
                        'Bt4301ReparentLeaf', class_info('Bt4301ReparentPlain', #{}, #{})
                    ),
                    LeafTag = tag('Bt4301ReparentLeaf'),
                    try
                        ?assert(
                            beamtalk_class_shadow_flags:direct_call_ok(
                                LeafTag, 'Bt4301ReparentLeaf'
                            )
                        ),
                        {ok, _} = beamtalk_object_class:update_class('Bt4301ReparentLeaf', #{
                            module => ?HELPER,
                            meta => maps:put(superclass, 'Bt4301ReparentStateful', ?META)
                        }),
                        ?assertEqual(false, beamtalk_class_shadow_flags:direct_flag_value(LeafTag)),
                        ?assert(
                            beamtalk_class_shadow_flags:direct_call_ok(
                                LeafTag, 'Bt4301ReparentLeaf'
                            )
                        ),
                        ?assertEqual(self(), send(Leaf, testSelf, []))
                    after
                        beamtalk_test_proc:stop(Leaf)
                    end
                end)
            end)
        end},
        {"an ancestor with no published facts, or no row, does not bar the guard", fun() ->
            %% Before ADR 0153 the guard failed closed on such a chain
            %% (BT-5154's window); the chain is no input now.
            Parent = 'Bt4301UnpubParent',
            Leaf = 'Bt4301UnpubLeaf',
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Parent, undefined, [], 'Object', false),
            ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Parent, false),
            try
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(tag(Leaf), Leaf)),
                ok = beamtalk_class_metadata:delete(Parent),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(tag(Leaf), Leaf))
            after
                beamtalk_class_metadata:delete(Leaf),
                beamtalk_class_metadata:delete(Parent)
            end
        end},
        {"a legacy direct_class_methods meta key is never read", fun() ->
            %% Before BT-4301 a dynamic send consulted the baked
            %% `direct_class_methods` table and called the method in the
            %% caller with a `nil` class self, bypassing any shadow. A class
            %% the in-caller path declines (no process-bound list here) now
            %% goes through the class process, whatever the meta carries.
            ClassInfo = #{
                superclass => none,
                module => ?HELPER,
                class_methods => ?METHODS,
                class_state => #{},
                meta => #{direct_class_methods => #{testSelf => class_testSelf}}
            },
            {ok, Pid} = beamtalk_object_class:start_link('Bt4301Legacy', ClassInfo),
            unlink(Pid),
            try
                ?assertEqual(Pid, send(Pid, testSelf, []))
            after
                beamtalk_test_proc:stop(Pid)
            end
        end}
    ]}.

invalidation_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"an install in flight when an ancestor's metadata is written leaves no stale flag",
            fun() ->
                %% BT-4301: a static call key's install reads published
                %% metadata, so an install holding `Leaf`'s tag lock may
                %% already have derived its answer from the ancestor's old
                %% row. The ancestor's erase must wait for it (it always takes
                %% the lock, even with no flag installed yet) and then remove
                %% what it put.
                Base = 'Bt4301RaceBase',
                Leaf = 'Bt4301RaceLeaf',
                LeafTag = tag(Leaf),
                beamtalk_class_metadata:new(),
                ok = beamtalk_class_metadata:insert(Base, undefined, [], 'Object', false),
                ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Base, false),
                _ = persistent_term:erase(LeafTag),
                Self = self(),
                %% The install: under Leaf's lock, having derived `true` from
                %% the old rows, it puts that value only once released.
                Installer = spawn(fun() ->
                    beamtalk_class_shadow_flags:with_tag_lock(LeafTag, fun() ->
                        Self ! derived,
                        receive
                            release -> persistent_term:put(LeafTag, true)
                        end
                    end),
                    Self ! installed
                end),
                ?awaitMessage(derived),
                %% The writer: a metadata write to Base erases the
                %% descendants' flags (as reload and `removeSelector:` do).
                Writer = spawn(fun() ->
                    ok = beamtalk_class_shadow_flags:invalidate(Base),
                    Self ! written
                end),
                try
                    %% With no flag installed, the erase still waits for the lock.
                    receive
                        written -> error(erase_skipped_the_lock)
                    after 300 -> ok
                    end,
                    Installer ! release,
                    ?awaitMessage(installed),
                    ?awaitMessage(written),
                    ?assertEqual(false, beamtalk_class_shadow_flags:direct_flag_value(LeafTag))
                after
                    exit(Installer, kill),
                    exit(Writer, kill),
                    beamtalk_class_metadata:delete(Leaf),
                    beamtalk_class_metadata:delete(Base),
                    persistent_term:erase(LeafTag)
                end
            end},
        {"deleting an ancestor's row leaves no descendant flag derived from it", fun() ->
            %% BT-4301: an install of `Leaf`'s flag that read `Base`'s row
            %% before `delete(Base)` removed it derives `true`. Here it holds
            %% `Leaf`'s tag lock from before the delete until the row is gone,
            %% then puts that stale value. `delete/1` must erase the
            %% descendants' flags after the row is gone, waiting for the lock,
            %% so the stale value is removed. (Erasing before the removal
            %% would wait on the install, which waits on the removal: the
            %% delete never returns.)
            Base = 'Bt4301DelBase',
            Leaf = 'Bt4301DelLeaf',
            LeafTag = tag(Leaf),
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Base, undefined, [], 'Object', false),
            ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Base, false),
            _ = persistent_term:erase(LeafTag),
            Self = self(),
            Installer = spawn(fun() ->
                beamtalk_class_shadow_flags:with_tag_lock(LeafTag, fun() ->
                    Self ! derived,
                    wait_until_row_gone(Base),
                    persistent_term:put(LeafTag, true)
                end),
                Self ! {installed, self()}
            end),
            ?awaitMessage(derived),
            Writer = spawn(fun() ->
                ok = beamtalk_class_metadata:delete(Base),
                Self ! {deleted, self()}
            end),
            try
                ?awaitMessage({deleted, Writer}),
                ?awaitMessage({installed, Installer}),
                ?assertEqual(false, beamtalk_class_shadow_flags:direct_flag_value(LeafTag))
            after
                exit(Installer, kill),
                exit(Writer, kill),
                beamtalk_class_metadata:delete(Leaf),
                persistent_term:erase(LeafTag)
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

%% The in-caller owner memo is retired by every write that can change which
%% method runs. Each case reads the memo through a probe fetch (a miss computes
%% nothing) and checks the send's behaviour.
owner_memo_invalidation_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a redefinition that removes the selector retires the owner", fun() ->
            with_class('Bt4771Redefine', none, #{}, fun(Pid) ->
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch({owner, _, _, _, _, _, _}, memo(Pid, testSelf)),
                {ok, _} = beamtalk_object_class:update_class('Bt4771Redefine', #{
                    module => ?HELPER, class_methods => #{}, meta => ?META
                }),
                %% The selector is gone: the memo must not run it in the
                %% caller as if it were still defined.
                ?assertNotMatch({owner, _, _, _, _, _, _}, memo(Pid, testSelf)),
                ?assertMatch({error, _}, outcome(Pid, testSelf)),
                ?assertNotMatch({owner, _, _, _, _, _, _}, memo(Pid, testSelf)),
                {ok, _} = beamtalk_object_class:update_class('Bt4771Redefine', #{
                    module => ?HELPER, class_methods => ?METHODS, meta => ?META
                }),
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch({owner, _, _, _, _, _, _}, memo(Pid, testSelf))
            end)
        end}
    ]}.

%% The memo entry for a selector, without computing one.
memo(Pid, Selector) ->
    beamtalk_class_derived_cache:fetch(
        {class_method_owner, Pid, Selector}, fun() -> {nocache, miss} end
    ).
