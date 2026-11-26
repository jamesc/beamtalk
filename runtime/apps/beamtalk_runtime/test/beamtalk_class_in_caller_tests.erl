%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_in_caller_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1b (BT-4300), ADR 0153 Phase 3a (BT-5158): a dynamic class
send (`beamtalk_class_dispatch:class_send/3`) to a class that nothing shadows
runs the class method in the caller, resolved through the
`{ClassPid, Selector}` owner memo. Class variables, in the class
or its chain, are no input. Each decline (a process-bound selector, a
class-side extension) keeps the class
process. An error has the same value on both paths.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_class_dispatch_test_helper).

-define(METHODS, #{
    testSelf => #{arity => 0},
    testClassSelf => #{arity => 0},
    testRaise => #{arity => 0},
    testSignal => #{arity => 0},
    testDnu => #{arity => 0},
    testInternalUndef => #{arity => 0},
    testScriptExit => #{arity => 0},
    testNlrThrow => #{arity => 0},
    'testWith:' => #{arity => 1},
    'run:' => #{arity => 1}
}).

%% The generated process-bound list every compiled class carries (ADR 0142 §1
%% rule 3); its contents are Rust's, only `run:` matters here.
-define(META, #{process_bound_class_selectors => ['run:', runAll, new, spawn]}).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

class_info(Superclass, Methods) ->
    #{
        superclass => Superclass,
        module => ?HELPER,
        class_methods => Methods,
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

with_class(Name, Superclass, Fun) ->
    Pid = beamtalk_test_class:start(Name, class_info(Superclass, ?METHODS)),
    try
        Fun(Pid)
    after
        beamtalk_test_proc:stop(Pid)
    end.

in_caller_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a stateless class's method runs in the caller", fun() ->
            with_class('Bt4300Stateless', none, fun(Pid) ->
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertEqual({with_arg, 7}, send(Pid, 'testWith:', [7])),
                %% Memoised under the class object's pid, with the defining
                %% class's tag: the key of its derived flag (BT-5267).
                ?assertMatch(
                    {owner, 'Bt4300Stateless', _, ?HELPER, 'Bt4300Stateless', ?HELPER,
                        class_testSelf, 'Bt4300Stateless class'},
                    beamtalk_class_derived_cache:fetch(
                        {Pid, testSelf}, fun() -> {nocache, miss} end
                    )
                )
            end)
        end},
        {"the method receives the real class reference as ClassSelf", fun() ->
            with_class('Bt4300ClassSelf', none, fun(Pid) ->
                ?assertEqual(
                    #beamtalk_object{
                        class = tag('Bt4300ClassSelf'), class_mod = ?HELPER, pid = Pid
                    },
                    send(Pid, testClassSelf, [])
                )
            end)
        end},
        {"class-variable defaults no longer keep the class process (ADR 0153)", fun() ->
            with_class('Bt5158Stateful', none, fun(Pid) ->
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch({owner, 'Bt5158Stateful', _, _, _, _, _}, memo(Pid, testSelf))
            end)
        end},
        {"a subclass of a class with class-variable defaults runs in the caller", fun() ->
            with_class('Bt5158Parent', none, fun(_ParentPid) ->
                Child = beamtalk_test_class:start(
                    'Bt5158Child', class_info('Bt5158Parent', #{})
                ),
                try
                    ?assertEqual(self(), send(Child, testSelf, []))
                after
                    beamtalk_test_proc:stop(Child)
                end
            end)
        end},
        {"an inherited method of a stateless chain runs in the caller", fun() ->
            with_class('Bt4300Root', none, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start('Bt4300Leaf', class_info('Bt4300Root', #{})),
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
            with_class('Bt4300Bound', none, fun(Pid) ->
                Fun = fun() -> self() end,
                %% `run:` reaches 'class_run:' in the class process, which the
                %% helper does not export: the structured DNU proves the hop.
                ?assertMatch({error, _}, outcome_with(Pid, 'run:', [Fun])),
                ?assertEqual(
                    decline,
                    beamtalk_class_derived_cache:fetch(
                        {Pid, 'run:'}, fun() -> {nocache, miss} end
                    )
                )
            end)
        end},
        {"an instance-field reflection selector a class does not define is DNU", fun() ->
            with_class('Bt4300Reflect', none, fun(Pid) ->
                %% ADR 0153 decision 7 (BT-5155): a class has no fields; no
                %% class method answers it, so the owner memo declines and
                %% the `Class` protocol fallthrough refuses it.
                ?assertBtError(does_not_understand, send(Pid, 'hasField:', [count])),
                ?assertEqual(
                    decline,
                    beamtalk_class_derived_cache:fetch(
                        {Pid, 'hasField:'}, fun() -> {nocache, miss} end
                    )
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
    with_class('Bt4300Errors', none, fun(Pid) ->
        Selectors = [
            testRaise, testSignal, testDnu, testInternalUndef, testScriptExit, testNlrThrow
        ],
        InCaller = [outcome(Pid, S) || S <- Selectors],
        %% Listing the selectors as process-bound sends the same sends
        %% through the class process (a runtime-fun shadow no longer does:
        %% ADR 0153 §5, BT-5161).
        ok = beamtalk_class_metadata:put_process_bound_selectors(
            'Bt4300Errors', [testSelf | Selectors]
        ),
        ?assertEqual(Pid, send(Pid, testSelf, [])),
        ?assertEqual(InCaller, [outcome(Pid, S) || S <- Selectors])
    end).

%% ADR 0153 §5 (BT-5161): a class method that is an ADR 0084 runtime fun runs
%% in the caller, like any other class method, unless its selector is
%% process-bound. Installing the fun still goes through the class process.
runtime_fun_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a ClassBuilder class's own fun, shadowing nothing, runs in the caller", fun() ->
            with_builder_class(
                'Bt5161Builder',
                'Object',
                #{
                    whereAmI => fun(_Self) -> self() end,
                    'echo:' => fun(Self, X) -> {Self, X} end,
                    'run:' => fun(_Self, _X) -> self() end
                },
                fun(Pid) ->
                    ?assertEqual(self(), send(Pid, whereAmI, [])),
                    ?assertMatch(
                        {owner, 'Bt5161Builder', _, 'Bt5161Builder', 'Bt5161Builder', _, _},
                        memo(Pid, whereAmI)
                    ),
                    %% `self` is the class's real reference.
                    ?assertMatch(
                        {#beamtalk_object{class = 'Bt5161Builder class', pid = Pid}, 3},
                        send(Pid, 'echo:', [3])
                    ),
                    %% A process-bound selector's fun still runs in the class
                    %% process.
                    ?assertEqual(Pid, send(Pid, 'run:', [x])),
                    ?assertEqual(decline, memo(Pid, 'run:'))
                end
            )
        end},
        {"self new in a ClassBuilder fun builds with the class's field defaults", fun() ->
            %% The fun runs in the caller, which has none of the class
            %% process's dictionary: the class's own defaults come from its
            %% metadata row. `Make` is what codegen emits for `self new`.
            Make = fun(#beamtalk_object{class = Tag}) ->
                ClassName = beamtalk_primitive:class_name_from_tag(Tag),
                Module = beamtalk_class_instantiation:resolve_module_or_raise(ClassName, 'new'),
                {self(), beamtalk_class_instantiation:class_self_new(ClassName, Module, [])}
            end,
            {ok, Pid} = beamtalk_class_builder:register(#{
                className => 'Bt5161SelfNew',
                superclassRef => 'Object',
                fieldSpecs => #{a => 7},
                methodSpecs => #{},
                classMethods => #{make => Make}
            }),
            try
                Me = self(),
                %% Warm the owner memo, then suspend the class process: the
                %% in-caller `self new` must not need it (the defaults come
                %% from the metadata row, not a call that a busy class would
                %% time out into an instance without them).
                {Me, _} = send(Pid, make, []),
                ok = sys:suspend(Pid),
                try
                    {Me, Instance} = send(Pid, make, []),
                    ?assertEqual(7, maps:get(a, Instance))
                after
                    sys:resume(Pid)
                end
            after
                beamtalk_test_class:stop(Pid)
            end
        end},
        {"self new inherits a busy ClassBuilder ancestor's field defaults", fun() ->
            %% Inherited module-less defaults come from the ancestor's row
            %% too: suspending the ancestor neither stalls the send nor drops
            %% its defaults.
            Make = fun(#beamtalk_object{class = Tag}) ->
                ClassName = beamtalk_primitive:class_name_from_tag(Tag),
                Module = beamtalk_class_instantiation:resolve_module_or_raise(ClassName, 'new'),
                beamtalk_class_instantiation:class_self_new(ClassName, Module, [])
            end,
            {ok, Parent} = beamtalk_class_builder:register(#{
                className => 'Bt5161DefaultsParent',
                superclassRef => 'Object',
                fieldSpecs => #{p => 10, shared => 1},
                methodSpecs => #{}
            }),
            try
                with_builder_class_fields(
                    'Bt5161DefaultsChild',
                    'Bt5161DefaultsParent',
                    #{c => 20, shared => 2},
                    #{make => Make},
                    fun(Child) ->
                        ok = sys:suspend(Parent),
                        ok = sys:suspend(Child),
                        try
                            Instance = send(Child, make, []),
                            ?assertEqual(
                                {10, 20, 2},
                                {
                                    maps:get(p, Instance),
                                    maps:get(c, Instance),
                                    maps:get(shared, Instance)
                                }
                            )
                        after
                            sys:resume(Child),
                            sys:resume(Parent)
                        end
                    end
                )
            after
                beamtalk_test_class:stop(Parent)
            end
        end},
        {"is_module_less/1 names ClassBuilder classes only", fun() ->
            with_builder_class('Bt5161ModuleLess', 'Object', #{}, fun(_Pid) ->
                ?assert(beamtalk_class_metadata:is_module_less('Bt5161ModuleLess'))
            end),
            with_class('Bt5161Compiled', none, fun(_Pid) ->
                ?assertNot(beamtalk_class_metadata:is_module_less('Bt5161Compiled'))
            end),
            ?assertNot(beamtalk_class_metadata:is_module_less('Bt5161NoSuchClass'))
        end},
        {"a module-less subclass runs its ancestor's fun in the caller", fun() ->
            with_builder_class(
                'Bt5161BuilderRoot',
                'Object',
                #{whereAmI => fun(_Self) -> self() end},
                fun(_RootPid) ->
                    with_builder_class(
                        'Bt5161BuilderLeaf',
                        'Bt5161BuilderRoot',
                        #{},
                        fun(Leaf) -> ?assertEqual(self(), send(Leaf, whereAmI, [])) end
                    )
                end
            )
        end},
        {"a module-less class keeps every selector any ancestor lists as process-bound", fun() ->
            %% BT-4475's own-class `new:` exemption is not inherited: a
            %% compiled parent whose list leaves `new:` out does not take
            %% it out for a ClassBuilder subclass, because `Object` lists it.
            Root = beamtalk_test_class:start('Bt5161NoNew1Root', #{
                superclass => 'Object',
                module => ?HELPER,
                class_methods => #{testSelf => #{arity => 0}},
                meta => #{process_bound_class_selectors => [new]}
            }),
            try
                with_builder_class(
                    'Bt5161NoNew1Leaf',
                    'Bt5161NoNew1Root',
                    #{'new:' => fun(_Self, _X) -> self() end},
                    fun(Leaf) ->
                        ?assertEqual(Leaf, send(Leaf, 'new:', [#{}])),
                        ?assertEqual(decline, memo(Leaf, 'new:')),
                        %% The parent's compiled method runs in the caller.
                        ?assertEqual(self(), send(Leaf, testSelf, []))
                    end
                )
            after
                beamtalk_test_proc:stop(Root)
            end
        end},
        {"re-registering a ClassBuilder class never hides a fun it keeps", fun() ->
            %% The reload seeds the incoming funs before dropping stale ones,
            %% so a caller looping on a selector defined before and after
            %% never sees it missing (a `does_not_understand`).
            Methods = #{whereAmI => fun(_Self) -> self() end},
            with_builder_class('Bt5161Reregister', 'Object', Methods, fun(Pid) ->
                Self = self(),
                Looper = spawn_link(fun() -> loop_in_caller(Pid, Self, 0) end),
                ?awaitMessage({looping, Looper}),
                lists:foreach(
                    fun(_) ->
                        {ok, Pid} = beamtalk_class_builder:register(#{
                            className => 'Bt5161Reregister',
                            superclassRef => 'Object',
                            fieldSpecs => #{},
                            methodSpecs => #{},
                            classMethods => Methods
                        })
                    end,
                    lists:seq(1, 200)
                ),
                Looper ! stop,
                ?awaitMessage({stopped, Looper})
            end)
        end},
        {"a fun shadowing a compiled method runs in the caller; no static key binds it", fun() ->
            with_class('Bt5161Shadow', none, fun(Pid) ->
                Tag = tag('Bt5161Shadow'),
                Key = {?HELPER, class_testSelf, Tag},
                Target = fun() ->
                    beamtalk_class_shadow_flags:static_call_target(
                        Key, 'Bt5161Shadow', testSelf, 1
                    )
                end,
                %% Unshadowed: the static key binds the compiled function.
                ?assertMatch(#beamtalk_object{pid = Pid}, Target()),
                ?assertMatch(
                    #beamtalk_object{}, beamtalk_class_shadow_flags:static_call_key_value(Key)
                ),
                ok = beamtalk_object_class:put_class_method(
                    Pid, testSelf, fun(_Self) -> {from_fun, self()} end
                ),
                %% The fun answers, in the caller.
                ?assertEqual({from_fun, self()}, send(Pid, testSelf, [])),
                ?assertMatch(
                    {owner, 'Bt5161Shadow', _, _, 'Bt5161Shadow', _, _}, memo(Pid, testSelf)
                ),
                %% The install erased the key, and none may bind the
                %% compiled function while the fun shadows it.
                ?assertEqual(false, beamtalk_class_shadow_flags:static_call_key_value(Key)),
                ?assertEqual(false, Target()),
                ?assertEqual(false, beamtalk_class_shadow_flags:static_call_key_value(Key)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, 'Bt5161Shadow')),
                %% Removing the fun brings the compiled method back.
                removed = beamtalk_object_class:remove_class_method(Pid, testSelf),
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch(#beamtalk_object{pid = Pid}, Target())
            end)
        end},
        {"a fun installed while a caller loops is seen once installed, in the caller", fun() ->
            with_class('Bt5161Loop', none, fun(Pid) ->
                Self = self(),
                Looper = spawn_link(fun() -> loop_until_fun(Pid, Self, 0) end),
                ?awaitMessage({looping, Looper}),
                ok = beamtalk_object_class:put_class_method(
                    Pid, testSelf, fun(_Self) -> {from_fun, self()} end
                ),
                %% Every send after the first that saw the fun sees it too,
                %% and runs in the looping process.
                ?awaitMessage({saw_fun, Looper, Looper, 100})
            end)
        end}
    ]}.

%% Send `testSelf` until the fun answers, then 99 more, each of which must be
%% the fun run in this process. Reports how many fun answers it saw.
loop_until_fun(Pid, Parent, 0) ->
    Me = self(),
    case send(Pid, testSelf, []) of
        Me ->
            Parent ! {looping, Me},
            spin_until_fun(Pid, Parent);
        Other ->
            exit({unexpected, Other})
    end.

spin_until_fun(Pid, Parent) ->
    Me = self(),
    case send(Pid, testSelf, []) of
        Me -> spin_until_fun(Pid, Parent);
        {from_fun, Runner} -> after_fun(Pid, Parent, Runner, 1);
        Other -> exit({unexpected, Other})
    end.

after_fun(_Pid, Parent, Runner, 100) ->
    Parent ! {saw_fun, Runner, self(), 100};
after_fun(Pid, Parent, Runner, N) ->
    case send(Pid, testSelf, []) of
        {from_fun, Runner} -> after_fun(Pid, Parent, Runner, N + 1);
        Other -> exit({unexpected, Other})
    end.

%% Send `whereAmI` until told to stop; every answer must be this process.
loop_in_caller(Pid, Parent, N) ->
    Me = self(),
    case send(Pid, whereAmI, []) of
        Me -> ok;
        Other -> exit({unexpected, Other})
    end,
    case N of
        0 -> Parent ! {looping, Me};
        _ -> ok
    end,
    receive
        stop -> Parent ! {stopped, Me}
    after 0 -> loop_in_caller(Pid, Parent, N + 1)
    end.

%% Register a module-less class through `ClassBuilder`'s backing
%% (`beamtalk_class_builder:register/1`) with `ClassMethods` as its runtime
%% class-method funs, run `Fun(Pid)`, and remove it.
with_builder_class(Name, Superclass, ClassMethods, Fun) ->
    with_builder_class_fields(Name, Superclass, #{}, ClassMethods, Fun).

%% `with_builder_class/4` with `Fields` as the class's field defaults.
with_builder_class_fields(Name, Superclass, Fields, ClassMethods, Fun) ->
    {ok, Pid} = beamtalk_class_builder:register(#{
        className => Name,
        superclassRef => Superclass,
        fieldSpecs => Fields,
        methodSpecs => #{},
        classMethods => ClassMethods
    }),
    try
        Fun(Pid)
    after
        beamtalk_test_class:stop(Pid)
    end.

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
            with_class('Bt4301Root', none, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start('Bt4301Leaf', class_info('Bt4301Root', #{})),
                try
                    ?assert(
                        beamtalk_class_shadow_flags:direct_call_ok(tag('Bt4301Leaf'), 'Bt4301Leaf')
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end),
            with_class('Bt4301StatefulRoot', none, fun(_RootPid) ->
                Leaf = beamtalk_test_class:start(
                    'Bt4301StatefulLeaf', class_info('Bt4301StatefulRoot', #{})
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
            with_class('Bt4301ReparentPlain', none, fun(_PlainPid) ->
                with_class('Bt4301ReparentStateful', none, fun(_StatefulPid) ->
                    Leaf = beamtalk_test_class:start(
                        'Bt4301ReparentLeaf', class_info('Bt4301ReparentPlain', #{})
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
            with_class('Bt4300Bound2', none, fun(Pid) ->
                Key = {Pid, testSelf},
                Miss = fun() -> {nocache, miss} end,
                ?assertEqual(self(), send(Pid, testSelf, [])),
                ?assertMatch(
                    {owner, _, _, _, _, _, _, _}, beamtalk_class_derived_cache:fetch(Key, Miss)
                ),
                ok = beamtalk_class_metadata:put_process_bound_selectors(
                    'Bt4300Bound2', maps:get(process_bound_class_selectors, ?META)
                ),
                ?assertMatch(
                    {owner, _, _, _, _, _, _, _}, beamtalk_class_derived_cache:fetch(Key, Miss)
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
            with_class('Bt4771Redefine', none, fun(Pid) ->
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

%% BT-5267: a dynamic send to a class held in a variable reads two
%% `persistent_term` keys, the owner memo's generation and the defining
%% class's derived flag, and still sees every write that changes which body
%% runs between two sends: a runtime fun installed on an ancestor,
%% `removeSelector:` of that fun, and a hot reload of the defining class.
class_in_variable_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a send reads only the generation and the defining class's derived flag", fun() ->
            with_class('Bt5267ReadsRoot', none, fun(RootPid) ->
                Leaf = beamtalk_test_class:start(
                    'Bt5267ReadsLeaf', class_info('Bt5267ReadsRoot', #{})
                ),
                try
                    Generation = {beamtalk_class_derived_cache, generation},
                    %% Own method: the receiving class is the definer.
                    ?assertEqual(
                        [Generation, tag('Bt5267ReadsRoot')],
                        persistent_term_reads(fun() -> send(RootPid, testSelf, []) end)
                    ),
                    %% Inherited method: the definer's flag, not the receiver's.
                    ?assertEqual(
                        [Generation, tag('Bt5267ReadsRoot')],
                        persistent_term_reads(fun() -> send(Leaf, testSelf, []) end)
                    ),
                    %% A shadowed definer: the derived flag misses, readiness and
                    %% the raw flag are read once each, then the fun store.
                    ok = beamtalk_object_class:put_class_method(
                        RootPid, testSelf, fun(_Self) -> from_fun end
                    ),
                    ?assertEqual(
                        [
                            Generation,
                            tag('Bt5267ReadsRoot'),
                            beamtalk_class_shadow_ready,
                            {beamtalk_class_shadow, runtime_fun, 'Bt5267ReadsRoot'}
                        ],
                        persistent_term_reads(fun() -> from_fun = send(Leaf, testSelf, []) end)
                    )
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"an ancestor's runtime fun shadows an inherited method between two sends", fun() ->
            with_class('Bt5267ShadowRoot', none, fun(RootPid) ->
                Leaf = beamtalk_test_class:start(
                    'Bt5267ShadowLeaf', class_info('Bt5267ShadowRoot', #{})
                ),
                try
                    ?assertEqual(self(), send(Leaf, testSelf, [])),
                    ?assert(
                        beamtalk_class_shadow_flags:direct_call_ok(
                            tag('Bt5267ShadowRoot'), 'Bt5267ShadowRoot'
                        )
                    ),
                    ok = beamtalk_object_class:put_class_method(
                        RootPid, testSelf, fun(_Self) -> {from_fun, self()} end
                    ),
                    ?assertEqual({from_fun, self()}, send(Leaf, testSelf, [])),
                    ?assertEqual({from_fun, self()}, send(RootPid, testSelf, []))
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"removeSelector: of a class-side fun takes effect between two sends", fun() ->
            with_class('Bt5267RemoveRoot', none, fun(RootPid) ->
                Leaf = beamtalk_test_class:start(
                    'Bt5267RemoveLeaf', class_info('Bt5267RemoveRoot', #{})
                ),
                try
                    ok = beamtalk_object_class:put_class_method(
                        RootPid, testSelf, fun(_Self) -> {from_fun, self()} end
                    ),
                    ?assertEqual({from_fun, self()}, send(Leaf, testSelf, [])),
                    %% `BtRoot class removeSelector: #testSelf`.
                    Metaclass = #beamtalk_object{
                        class = 'Metaclass', class_mod = ?HELPER, pid = RootPid
                    },
                    _ = beamtalk_class_refactor:classRemoveSelector(Metaclass, testSelf),
                    %% The compiled method is back, on the derived-flag route.
                    ?assertEqual(self(), send(Leaf, testSelf, [])),
                    ?assertEqual(self(), send(RootPid, testSelf, []))
                after
                    beamtalk_test_proc:stop(Leaf)
                end
            end)
        end},
        {"a hot reload of the defining class takes effect between two sends", fun() ->
            Module = bt5267_reload_mod,
            ok = load_versioned_module(Module, v1),
            try
                RootPid = beamtalk_test_class:start('Bt5267ReloadRoot', #{
                    superclass => none,
                    module => Module,
                    class_methods => #{testSelf => #{arity => 0}},
                    meta => ?META
                }),
                try
                    Leaf = beamtalk_test_class:start(
                        'Bt5267ReloadLeaf', class_info('Bt5267ReloadRoot', #{})
                    ),
                    try
                        ?assertEqual(v1, send(Leaf, testSelf, [])),
                        ?assertEqual(v1, send(RootPid, testSelf, [])),
                        %% A recompile: new code, then the `-on_load` registration.
                        ok = load_versioned_module(Module, v2),
                        {ok, _} = beamtalk_object_class:update_class('Bt5267ReloadRoot', #{
                            module => Module,
                            class_methods => #{testSelf => #{arity => 0}},
                            meta => ?META
                        }),
                        ?assertEqual(v2, send(Leaf, testSelf, [])),
                        ?assertEqual(v2, send(RootPid, testSelf, []))
                    after
                        beamtalk_test_proc:stop(Leaf)
                    end
                after
                    beamtalk_test_proc:stop(RootPid)
                end
            after
                _ = code:purge(Module),
                _ = code:delete(Module),
                _ = code:purge(Module)
            end
        end}
    ]}.

%% The `persistent_term:get/2` keys one call of `Fun` reads, in order, in a
%% fresh process (after a warm-up call there, so first-send installs are not
%% counted).
persistent_term_reads(Fun) ->
    Self = self(),
    Worker = spawn_link(fun() ->
        _ = Fun(),
        Self ! {warm, self()},
        receive
            go -> ok
        end,
        _ = Fun(),
        Self ! {done, self()}
    end),
    ?awaitMessage({warm, Worker}),
    Session = trace:session_create(bt5267_reads, self(), []),
    try
        1 = trace:process(Session, Worker, true, [call]),
        _ = trace:function(Session, {persistent_term, get, 2}, true, [local]),
        Worker ! go,
        ?awaitMessage({done, Worker}),
        %% Every call trace message is in the mailbox before it is read.
        Ref = erlang:trace_delivered(Worker),
        ?awaitMessage({trace_delivered, Worker, Ref})
    after
        trace:session_destroy(Session)
    end,
    collect_reads(Worker, []).

collect_reads(Worker, Acc) ->
    receive
        {trace, Worker, call, {persistent_term, get, [Key, _Default]}} ->
            collect_reads(Worker, [Key | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

%% Load `Module` with `class_testSelf/1` answering `Version`: a stand-in for a
%% recompile of a class's module.
load_versioned_module(Module, Version) ->
    Src = io_lib:format(
        "-module(~p).~n-export([class_testSelf/1]).~nclass_testSelf(_ClassSelf) -> ~p.~n",
        [Module, Version]
    ),
    {Module, Bin} = beamtalk_test_module:compile(lists:flatten(Src)),
    _ = code:purge(Module),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin),
    ok.

%% The memo entry for a selector, without computing one: the owner (its
%% defining class's tag dropped), `decline` or `miss`.
memo(Pid, Selector) ->
    case
        beamtalk_class_derived_cache:fetch(
            {Pid, Selector}, fun() -> {nocache, miss} end
        )
    of
        {owner, C, T, M, DC, DM, F, _DefiningTag} -> {owner, C, T, M, DC, DM, F};
        Other -> Other
    end.
