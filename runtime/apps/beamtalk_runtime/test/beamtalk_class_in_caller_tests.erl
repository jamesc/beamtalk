%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_in_caller_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1b (BT-4300): a dynamic class send
(`beamtalk_class_dispatch:class_send/3`) to a class whose chain declares no
class variables, and that nothing shadows, runs the class method in the caller,
resolved through the `{class_method_owner, ClassPid, Selector}` memo. Each
decline (a process-bound selector, a class-variable reflection selector, a
class-side extension) keeps the class process, and so does a chain with class
variables, which the memo records as the `process` verdict (BT-4771). An error
has the same value on both paths.
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
    beamtalk_test_class:start(Name, ClassInfo).

tag(Name) -> beamtalk_class_registry:class_object_tag(Name).

send(Pid, Selector, Args) -> beamtalk_class_dispatch:class_send(Pid, Selector, Args).

%% The outcome of a send, comparable across paths.
outcome(Pid, Selector) ->
    try send(Pid, Selector, []) of
        V -> {returned, V}
    catch
        Class:Reason -> {Class, strip_stacktrace(Reason)}
    end.

%% The error kind of a raised outcome, whatever its wrapping.
dnu_kind({error, #{error := #beamtalk_error{kind = Kind}}}) -> Kind;
dnu_kind({error, #beamtalk_error{kind = Kind}}) -> Kind;
dnu_kind(Other) -> Other.

strip_stacktrace(#{} = Wrapped) -> maps:remove(raw_stacktrace, Wrapped);
strip_stacktrace(Other) -> Other.

with_class(Name, Superclass, ClassVars, Fun) ->
    Pid = start_class(Name, Superclass, ClassVars, ?METHODS),
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
        {"a class with class variables keeps its process", fun() ->
            with_class('Bt4300Stateful', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(Pid, send(Pid, testSelf, []))
            end)
        end},
        {"a stateful class's method is memoised as a process verdict (BT-4771)", fun() ->
            with_class('Bt4771Stateful', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                %% A positive verdict, not a miss: the caller asks the class
                %% process directly instead of walking the chain again.
                ?assertEqual(
                    process,
                    beamtalk_class_derived_cache:fetch(
                        {class_method_owner, Pid, testSelf}, fun() -> {nocache, miss} end
                    )
                ),
                %% Served from the memo, the send is unchanged.
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual({with_arg, 3}, send(Pid, 'testWith:', [3]))
            end)
        end},
        {"a stale process verdict falls back to the class process's own lookup", fun() ->
            with_class('Bt4771Stale', none, #{count => 0}, fun(Pid) ->
                %% Plant `process` for a selector the class does not define: the
                %% class process does the authoritative lookup, finds nothing
                %% and the send raises the same DNU a fresh send does.
                Fresh = outcome(Pid, bt4771Missing),
                beamtalk_class_derived_cache:invalidate(),
                _ = beamtalk_class_derived_cache:fetch(
                    {class_method_owner, Pid, bt4771Missing2}, fun() -> {cache, process} end
                ),
                Planted = outcome(Pid, bt4771Missing2),
                ?assertMatch({error, _}, Fresh),
                ?assertMatch({error, _}, Planted),
                ?assertEqual(
                    dnu_kind(Fresh),
                    dnu_kind(Planted)
                )
            end)
        end},
        {"a stateless subclass of a stateful class keeps its process", fun() ->
            with_class('Bt4300Parent', none, #{count => 0}, fun(_ParentPid) ->
                Child = start_class('Bt4300Child', 'Bt4300Parent', #{}, #{}),
                try
                    ?assertEqual(Child, send(Child, testSelf, []))
                after
                    beamtalk_test_proc:stop(Child)
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
                ?assertEqual(false, send(Pid, testHasCount, [])),
                %% `clearField:` answers the class, as against the empty map in
                %% the class process; a write still has nowhere to go.
                ClassSelf = #beamtalk_object{
                    class = tag('Bt4300NoHome'), class_mod = ?HELPER, pid = Pid
                },
                ?assertEqual(ClassSelf, beamtalk_class_vars:clear(ClassSelf, count)),
                ?assertBtError(
                    class_state_unreachable, beamtalk_class_vars:put(ClassSelf, count, 1)
                )
            end),
            with_class('Bt4300Home', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(true, send(Pid, testHasCount, [])),
                ClassSelf = #beamtalk_object{
                    class = tag('Bt4300Home'), class_mod = ?HELPER, pid = Pid
                },
                ?assertBtError(class_state_unreachable, beamtalk_class_vars:has(ClassSelf, count)),
                ?assertBtError(class_state_unreachable, beamtalk_class_vars:clear(ClassSelf, count))
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

%% Polls until `Base`'s metadata row is gone (a stand-in install's wait).
wait_until_row_gone(Base) ->
    case beamtalk_class_metadata:lookup_superclass(Base) of
        {ok, _} ->
            timer:sleep(5),
            wait_until_row_gone(Base);
        not_found ->
            ok
    end.

outcome_with(Pid, Selector, Args) ->
    try send(Pid, Selector, Args) of
        V -> {returned, V}
    catch
        Class:Reason -> {Class, Reason}
    end.

%% ADR 0142 §2 (BT-4301): the guard of every static class-side direct call,
%% `beamtalk_class_shadow_flags:direct_call_ok/2`, agrees with the dynamic
%% path's chain-wide rule 2 (`chain_has_class_variables/1`).
static_guard_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"the guard holds for a stateless chain and not under a stateful ancestor", fun() ->
            with_class('Bt4301Root', none, #{}, fun(_RootPid) ->
                Leaf = start_class('Bt4301Leaf', 'Bt4301Root', #{}, #{}),
                try
                    ?assert(
                        beamtalk_class_shadow_flags:direct_call_ok(tag('Bt4301Leaf'), 'Bt4301Leaf')
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end),
            with_class('Bt4301StatefulRoot', none, #{count => 0}, fun(_RootPid) ->
                Leaf = start_class('Bt4301StatefulLeaf', 'Bt4301StatefulRoot', #{}, #{}),
                try
                    ?assertNot(
                        beamtalk_class_shadow_flags:direct_call_ok(
                            tag('Bt4301StatefulLeaf'), 'Bt4301StatefulLeaf'
                        )
                    ),
                    %% The class-side self-send guard only asks about shadows.
                    ?assert(
                        beamtalk_class_shadow_flags:shadow_free(
                            tag('Bt4301StatefulLeaf'), 'Bt4301StatefulLeaf'
                        )
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"an ancestor gaining class variables turns a descendant's installed guard off", fun() ->
            with_class('Bt4301GainRoot', none, #{}, fun(_RootPid) ->
                Leaf = start_class('Bt4301GainLeaf', 'Bt4301GainRoot', #{}, #{}),
                LeafTag = tag('Bt4301GainLeaf'),
                try
                    ?assert(beamtalk_class_shadow_flags:direct_call_ok(LeafTag, 'Bt4301GainLeaf')),
                    ?assertEqual(true, beamtalk_class_shadow_flags:direct_flag_value(LeafTag)),
                    ok = beamtalk_class_metadata:put_class_variables('Bt4301GainRoot', true),
                    ?assertNot(
                        beamtalk_class_shadow_flags:direct_call_ok(LeafTag, 'Bt4301GainLeaf')
                    ),
                    ok = beamtalk_class_metadata:put_class_variables('Bt4301GainRoot', false),
                    ?assert(beamtalk_class_shadow_flags:direct_call_ok(LeafTag, 'Bt4301GainLeaf'))
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"re-parenting under a class with class variables turns the guard off (BT-4134)", fun() ->
            with_class('Bt4301ReparentPlain', none, #{}, fun(_PlainPid) ->
                with_class('Bt4301ReparentStateful', none, #{count => 0}, fun(_StatefulPid) ->
                    Leaf = start_class('Bt4301ReparentLeaf', 'Bt4301ReparentPlain', #{}, #{}),
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
                        ?assertNot(
                            beamtalk_class_shadow_flags:direct_call_ok(
                                LeafTag, 'Bt4301ReparentLeaf'
                            )
                        ),
                        ?assertEqual(Leaf, send(Leaf, testSelf, []))
                    after
                        beamtalk_test_proc:stop(Leaf)
                    end
                end)
            end)
        end},
        {"an ancestor whose class-variable fact is unpublished keeps the guard off", fun() ->
            %% The guard reads `chain_has_class_variables/1`, the dynamic path's
            %% own answer, and fails closed with it: an ancestor row with no
            %% published fact (or no row at all) is a doubt, never "stateless".
            Parent = 'Bt4301UnpubParent',
            Leaf = 'Bt4301UnpubLeaf',
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Parent, undefined, [], 'Object', false),
            ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Parent, false),
            ok = beamtalk_class_metadata:put_class_variables(Leaf, false),
            try
                ?assert(beamtalk_class_metadata:chain_has_class_variables(Leaf)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(tag(Leaf), Leaf)),
                ?assert(beamtalk_class_shadow_flags:shadow_free(tag(Leaf), Leaf)),
                %% Publishing the parent's fact erases the leaf's derived flag.
                ok = beamtalk_class_metadata:put_class_variables(Parent, false),
                ?assertNot(beamtalk_class_metadata:chain_has_class_variables(Leaf)),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(tag(Leaf), Leaf)),
                %% A missing ancestor row is the same doubt.
                ok = beamtalk_class_metadata:delete(Parent),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(tag(Leaf), Leaf))
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
        {"an install in flight when an ancestor gains class variables leaves no stale flag",
            fun() ->
                %% BT-4301: the derived flag reads published metadata, so an
                %% install holding `Leaf`'s tag lock may already have derived
                %% `true` from the ancestor's old row. The ancestor's write must
                %% wait for it (the erase always takes the lock, even with no
                %% flag installed yet) and then remove what it put.
                Base = 'Bt4301RaceBase',
                Leaf = 'Bt4301RaceLeaf',
                LeafTag = tag(Leaf),
                beamtalk_class_metadata:new(),
                ok = beamtalk_class_metadata:insert(Base, undefined, [], 'Object', false),
                ok = beamtalk_class_metadata:put_class_variables(Base, false),
                ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Base, false),
                ok = beamtalk_class_metadata:put_class_variables(Leaf, false),
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
                receive
                    derived -> ok
                after 5000 -> error(lock_not_taken)
                end,
                %% The writer: publishes Base's class variables, then erases
                %% the descendants' flags.
                Writer = spawn(fun() ->
                    ok = beamtalk_class_metadata:put_class_variables(Base, true),
                    Self ! written
                end),
                try
                    %% With no flag installed, the erase still waits for the lock.
                    receive
                        written -> error(erase_skipped_the_lock)
                    after 300 -> ok
                    end,
                    Installer ! release,
                    receive
                        installed -> ok
                    after 5000 -> error(install_stuck)
                    end,
                    receive
                        written -> ok
                    after 5000 -> error(write_stuck)
                    end,
                    ?assertEqual(false, beamtalk_class_shadow_flags:direct_flag_value(LeafTag)),
                    ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(LeafTag, Leaf))
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
            ok = beamtalk_class_metadata:put_class_variables(Base, false),
            ok = beamtalk_class_metadata:insert(Leaf, undefined, [], Base, false),
            ok = beamtalk_class_metadata:put_class_variables(Leaf, false),
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
            receive
                derived -> ok
            after 5000 -> error(lock_not_taken)
            end,
            Writer = spawn(fun() ->
                ok = beamtalk_class_metadata:delete(Base),
                Self ! {deleted, self()}
            end),
            try
                receive
                    {deleted, Writer} -> ok
                after 5000 -> error(delete_stuck)
                end,
                receive
                    {installed, Installer} -> ok
                after 5000 -> error(install_stuck)
                end,
                ?assertEqual(false, beamtalk_class_shadow_flags:direct_flag_value(LeafTag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(LeafTag, Leaf))
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

%% BT-4771: the `process` verdict is memoised under the same generation as the
%% in-caller owner, so every write that can change which process runs a method,
%% or which method runs, must retire it. Each case reads the memo through a
%% probe fetch (a miss computes nothing) and checks the send's behaviour.
process_verdict_invalidation_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a class-side extension added then removed retires the verdict", fun() ->
            with_class('Bt4771ExtInval', none, #{count => 0}, fun(Pid) ->
                Tag = tag('Bt4771ExtInval'),
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf)),
                ok = beamtalk_extensions:register(Tag, testSelf, fun(_, _) -> ext end, test),
                try
                    %% The extension answers first, in the class process.
                    ?assertEqual(ext, send(Pid, testSelf, [])),
                    ?assertNotEqual(process, memo(Pid, testSelf)),
                    %% BT-4800: resolved once, as a positive `extension` verdict.
                    ?assertEqual(extension, memo(Pid, testSelf)),
                    %% Redefining the extension: the class process looks it up
                    %% itself, so the new body answers.
                    ok = beamtalk_extensions:register(Tag, testSelf, fun(_, _) -> ext2 end, test),
                    ?assertEqual(ext2, send(Pid, testSelf, []))
                after
                    beamtalk_extensions:unregister(Tag, testSelf)
                end,
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf))
            end)
        end},
        {"dropping and regaining class variables retires the verdict", fun() ->
            with_class('Bt4771VarsInval', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf)),
                ok = beamtalk_class_metadata:put_class_variables('Bt4771VarsInval', false),
                try
                    %% No class variables in the chain: the caller runs it.
                    ?assertEqual(self(), send(Pid, testSelf, [])),
                    ?assertMatch({owner, 'Bt4771VarsInval', _, _, _, _, _}, memo(Pid, testSelf))
                after
                    ok = beamtalk_class_metadata:put_class_variables('Bt4771VarsInval', true)
                end,
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf))
            end)
        end},
        {"re-parenting under a class with class variables retires the owner and sets the verdict",
            fun() ->
                with_class('Bt4771ReparentPlain', none, #{}, fun(_PlainPid) ->
                    with_class('Bt4771ReparentStateful', none, #{count => 0}, fun(_StatefulPid) ->
                        Leaf = start_class('Bt4771ReparentLeaf', 'Bt4771ReparentPlain', #{}, #{}),
                        try
                            ?assertEqual(self(), send(Leaf, testSelf, [])),
                            ?assertMatch({owner, _, _, _, _, _, _}, memo(Leaf, testSelf)),
                            {ok, _} = beamtalk_object_class:update_class('Bt4771ReparentLeaf', #{
                                module => ?HELPER,
                                meta => maps:put(superclass, 'Bt4771ReparentStateful', ?META)
                            }),
                            ?assertEqual(Leaf, send(Leaf, testSelf, [])),
                            ?assertEqual(process, memo(Leaf, testSelf))
                        after
                            beamtalk_test_proc:stop(Leaf)
                        end
                    end)
                end)
            end},
        {"a redefinition that removes the selector retires the verdict", fun() ->
            with_class('Bt4771Redefine', none, #{count => 0}, fun(Pid) ->
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf)),
                {ok, _} = beamtalk_object_class:update_class('Bt4771Redefine', #{
                    module => ?HELPER, class_methods => #{}, meta => ?META
                }),
                %% The selector is gone: the verdict must not send it to the
                %% class process as if it were still defined.
                ?assertNotEqual(process, memo(Pid, testSelf)),
                ?assertMatch({error, _}, outcome(Pid, testSelf)),
                ?assertNotEqual(process, memo(Pid, testSelf)),
                {ok, _} = beamtalk_object_class:update_class('Bt4771Redefine', #{
                    module => ?HELPER, class_methods => ?METHODS, meta => ?META
                }),
                ?assertEqual(Pid, send(Pid, testSelf, [])),
                ?assertEqual(process, memo(Pid, testSelf))
            end)
        end}
    ]}.

%% The memo entry for a selector, without computing one.
memo(Pid, Selector) ->
    beamtalk_class_derived_cache:fetch(
        {class_method_owner, Pid, Selector}, fun() -> {nocache, miss} end
    ).
