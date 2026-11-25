%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_shadow_flags_tests).

-moduledoc """
BT-3669: the per-class "has shadows" flags that gate the compiled class-side
self-send fast path. Covers installed-fun transitions and concurrent access.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").
-include("beamtalk_class_vars_keys.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

setup_runtime() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

teardown_runtime(_) ->
    ok.

direct(Tag, Name, Sel) ->
    beamtalk_class_dispatch:class_self_direct_ok(Tag, Tag, Name, Sel).

%% A throwaway metadata row for a class under `Object`.
stateless_row(Name) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Object', false).

shadow_flag_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, [
        {"mark_ready raises the readiness flag the fast path reads", fun() ->
            %% BT-3676: the guard reads a readiness flag, raised once by
            %% `beamtalk_runtime_app:start/2` (idempotently). Erase it first so
            %% the assertions below fail if `mark_ready/0` stops raising it.
            _ = persistent_term:erase(beamtalk_class_shadow_ready),
            ?assertNot(beamtalk_class_shadow_flags:is_ready()),
            ok = beamtalk_class_shadow_flags:mark_ready(),
            ?assert(beamtalk_class_shadow_flags:is_ready()),
            ok = beamtalk_class_shadow_flags:mark_ready(),
            ?assert(beamtalk_class_shadow_flags:is_ready())
        end},
        {"direct_call_ok/2 is the readiness flag and not the shadow flag (BT-3690)", fun() ->
            Tag = 'Bt3690Truth class',
            Name = 'Bt3690Truth',
            ok = stateless_row(Name),
            ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
            try
                ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ok = beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                %% Readiness is never cleared in production; the derived flag
                %% (BT-3700) is, so drop both to exercise the not-ready branch.
                _ = persistent_term:erase(beamtalk_class_shadow_ready),
                _ = persistent_term:erase(Tag),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
            after
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                beamtalk_class_shadow_flags:mark_ready()
            end,
            ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
        end},
        {"derived flag is installed on first send and erased by a shadow raise (BT-3700)", fun() ->
            Tag = 'Bt3700Derived class',
            Name = 'Bt3700Derived',
            ok = stateless_row(Name),
            _ = persistent_term:erase(Tag),
            ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
            ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
            ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
            try
                %% each raise must erase the flag itself, before the next send
                ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag))
            after
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                persistent_term:erase(Tag)
            end
        end},
        {"runtime class-method install/reset flips the derived flag (BT-3700)", fun() ->
            %% Throwaway metadata row: set_runtime_class_methods/2 overwrites the
            %% row's selectors, so it must never run against the real `Object`.
            Name = 'Bt3700Rt',
            Tag = 'Bt3700Rt class',
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Object', false),
            try
                ?assert(direct(Tag, Name, bt3700_probe)),
                ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ok = beamtalk_class_metadata:set_runtime_class_methods(Name, [bt3700_rt]),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(direct(Tag, Name, bt3700_probe)),
                ok = beamtalk_class_metadata:reset_runtime_class_methods(Name),
                ?assert(direct(Tag, Name, bt3700_probe))
            after
                beamtalk_class_metadata:delete(Name),
                persistent_term:erase(Tag)
            end
        end},
        {"an install racing a raise never leaves a stale safe flag (BT-3700)", fun() ->
            Tag = 'Bt3700Race class',
            Name = 'Bt3700Race',
            _ = persistent_term:erase(Tag),
            Parent = self(),
            %% Hold the tag lock; a sender and a raiser both block behind it.
            %% Whichever runs second must win: after both, the flag is absent.
            beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
                Sender = spawn_link(fun() ->
                    Parent ! {sender, beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)}
                end),
                %% Wait until the sender is blocked behind the held lock.
                ok = beamtalk_test_wait:idle(Sender),
                Raiser = spawn_link(fun() ->
                    ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                    Parent ! raised
                end),
                ok = beamtalk_test_wait:idle(Raiser),
                _ = {Sender, Raiser}
            end),
            ?awaitMessage({sender, _}),
            ?awaitMessage(raised),
            try
                ?assert(beamtalk_class_shadow_flags:is_set(runtime_fun, Name)),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
            after
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                persistent_term:erase(Tag)
            end
        end},
        {"runtime-installed class-method funs shadow; reset/insert/delete restore", fun() ->
            Name = 'Bt3669Funs',
            Tag = 'Bt3669Funs class',
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Object', false),
            try
                ?assert(direct(Tag, Name, foo)),
                ok = beamtalk_class_metadata:set_runtime_class_methods(Name, [foo]),
                ?assertNot(direct(Tag, Name, foo)),
                ok = beamtalk_class_metadata:reset_runtime_class_methods(Name),
                ?assert(direct(Tag, Name, foo)),
                ok = beamtalk_class_metadata:set_runtime_class_methods(Name, [foo]),
                ?assertNot(direct(Tag, Name, foo)),
                %% A full-row overwrite resets the gate, so it must reset the flag.
                ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Object', false),
                ?assert(direct(Tag, Name, foo)),
                ok = beamtalk_class_metadata:set_runtime_class_methods(Name, [foo]),
                ?assertNot(direct(Tag, Name, foo)),
                ok = beamtalk_class_metadata:delete(Name),
                ?assert(direct(Tag, Name, foo))
            after
                beamtalk_class_metadata:delete(Name)
            end
        end}
    ]}.

%%% ============================================================================
%%% BT-4316: static call keys (`static_call_target/4`)
%%% ============================================================================

-define(HELPER, beamtalk_class_dispatch_test_helper).

%% Class methods the helper module defines (`class_testSelf/1`,
%% `class_testClassSelf/1`), plus one it does not export.
-define(STATIC_METHODS, #{
    testSelf => #{arity => 0},
    testClassSelf => #{arity => 0},
    testNotExported => #{arity => 0}
}).

-define(STATIC_META, #{process_bound_class_selectors => [new, spawn]}).

%% A watched class process (`start/2`, so the class monitor sees its 'DOWN').
%% A compiled class's module is loaded before its class process exists (it
%% registers from `on_load`); load the helper the same way, since
%% `function_exported/3` is false for a module not loaded yet.
start_static_class(Name, Superclass, ClassVars) ->
    {module, ?HELPER} = code:ensure_loaded(?HELPER),
    {ok, Pid} = beamtalk_object_class:start(Name, #{
        superclass => Superclass,
        module => ?HELPER,
        class_methods => ?STATIC_METHODS,
        class_state => ClassVars,
        meta => ?STATIC_META
    }),
    Pid.

stop_static_class(Pid) ->
    try
        gen_server:stop(Pid, normal, 5000)
    catch
        _:_ -> ok
    end,
    ok.

%% A class whose class methods (`testSelf`) live in `Module`.
start_class_on(Name, Superclass, Module) ->
    {ok, Pid} = beamtalk_object_class:start(Name, #{
        superclass => Superclass,
        module => Module,
        class_methods => #{testSelf => #{arity => 0}},
        class_state => #{},
        meta => ?STATIC_META
    }),
    Pid.

%% A class module defining `class_testSelf/1` whose metadata maps selectors to
%% caller-safe signal primitives as `CallerSafe` does (BT-4270).
load_raise_module(Module, CallerSafe) ->
    Body = io_lib:format(
        "class_testSelf(_ClassSelf) -> ok.~n"
        "'__beamtalk_meta'() -> #{class_var_abi => ~p, caller_safe_class_primitives => ~p}.~n",
        [?BT_CLASS_VAR_ABI, CallerSafe]
    ),
    load_module_binary(
        Module, compile_module(Module, [{class_testSelf, 1}, {'__beamtalk_meta', 0}], Body)
    ).

%% A class on `Module` whose registration publishes `CallerSafe` (BT-4493).
start_raise_class(Name, Module, CallerSafe) ->
    {ok, Pid} = beamtalk_object_class:start(Name, #{
        superclass => none,
        module => Module,
        class_methods => #{testSelf => #{arity => 0}},
        class_state => #{},
        meta => maps:put(caller_safe_class_primitives, CallerSafe, ?STATIC_META)
    }),
    Pid.

%% A class module defining `class_testSelf/1` whose metadata maps selectors to
%% their integer-literal bodies as `Constants` does (BT-4363).
load_constant_module(Module, Constants) ->
    Body = io_lib:format(
        "class_testSelf(_ClassSelf) -> 1.~n"
        "'__beamtalk_meta'() -> #{class_var_abi => ~p, constant_class_methods => ~p}.~n",
        [?BT_CLASS_VAR_ABI, Constants]
    ),
    load_module_binary(
        Module, compile_module(Module, [{class_testSelf, 1}, {'__beamtalk_meta', 0}], Body)
    ).

%% The class meta a registration of a constant module publishes (BT-4363).
constant_meta(Constants) -> maps:put(constant_class_methods, Constants, ?STATIC_META).

%% A class on `Module` whose registration publishes `Constants` (BT-4363).
start_constant_class(Name, Module, Constants) ->
    {ok, Pid} = beamtalk_object_class:start(Name, #{
        superclass => none,
        module => Module,
        class_methods => #{testSelf => #{arity => 0}},
        class_state => #{},
        meta => constant_meta(Constants)
    }),
    Pid.

%% Reload `Name` on `Module` with `Constants`, as a recompile's `-on_load`
%% registration does (BT-4363).
reload_constant_class(Name, Module, Constants) ->
    load_constant_module(Module, Constants),
    {ok, _} = beamtalk_object_class:update_class(Name, #{
        superclass => none,
        module => Module,
        class_methods => #{testSelf => #{arity => 0}},
        meta => constant_meta(Constants)
    }),
    ok.

%%% ============================================================================
%%% BT-4756: class literals (`class_ref/1`)
%%% ============================================================================

class_ref_key(Name) -> {class_ref, beamtalk_class_registry:class_object_tag(Name)}.

class_ref_installed(Name) ->
    beamtalk_class_shadow_flags:static_call_key_value(class_ref_key(Name)).

class_ref_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, [
        {"installs the class object the literal used to build, then serves it", fun() ->
            with_static_class('Bt4756Hit', fun(Pid) ->
                Tag = 'Bt4756Hit class',
                Expected = {beamtalk_object, Tag, ?HELPER, Pid},
                ?assertEqual(false, class_ref_installed('Bt4756Hit')),
                ?assertEqual(Expected, beamtalk_class_shadow_flags:class_ref('Bt4756Hit')),
                ?assertEqual(Expected, class_ref_installed('Bt4756Hit')),
                %% The call site's hit path: one read of the class's map.
                MapKey = beamtalk_class_shadow_flags:static_calls_key(Tag),
                ?assertMatch(#{{class_ref, Tag} := Expected}, persistent_term:get(MapKey))
            end)
        end},
        {"an unregistered class answers undefined and installs nothing", fun() ->
            ?assertEqual(undefined, beamtalk_class_shadow_flags:class_ref('Bt4756Absent')),
            ?assertEqual(false, class_ref_installed('Bt4756Absent')),
            ?assertEqual(undefined, beamtalk_class_shadow_flags:class_ref('Bt4756Absent'))
        end},
        {"a removed class is not served from the map", fun() ->
            Pid = start_static_class('Bt4756Gone', none, #{}),
            _ = beamtalk_class_shadow_flags:class_ref('Bt4756Gone'),
            ?assertMatch({beamtalk_object, _, _, Pid}, class_ref_installed('Bt4756Gone')),
            stop_static_class(Pid),
            beamtalk_test_wait:until(
                fun() -> class_ref_installed('Bt4756Gone') =:= false end, 500
            ),
            ?assertEqual(undefined, beamtalk_class_shadow_flags:class_ref('Bt4756Gone'))
        end},
        {"a metadata write erases the entry and the next literal re-installs", fun() ->
            with_static_class('Bt4756Write', fun(_Pid) ->
                Ref = beamtalk_class_shadow_flags:class_ref('Bt4756Write'),
                ?assertEqual(Ref, class_ref_installed('Bt4756Write')),
                ok = beamtalk_class_shadow_flags:invalidate('Bt4756Write'),
                ?assertEqual(false, class_ref_installed('Bt4756Write')),
                ?assertEqual(Ref, beamtalk_class_shadow_flags:class_ref('Bt4756Write')),
                ?assertEqual(Ref, class_ref_installed('Bt4756Write'))
            end)
        end},
        {"a descendant's reference does not depend on, or survive, its own erase", fun() ->
            Root = start_static_class('Bt4756Root', none, #{}),
            Leaf = start_static_class('Bt4756Leaf', 'Bt4756Root', #{}),
            try
                _ = beamtalk_class_shadow_flags:class_ref('Bt4756Leaf'),
                ok = beamtalk_class_shadow_flags:invalidate('Bt4756Root'),
                ?assertEqual(false, class_ref_installed('Bt4756Leaf'))
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"a reload that changes the class's module erases the entry", fun() ->
            Name = 'Bt4756Reload',
            ModB = bt4756_reload_mod,
            ok = load_constant_module(ModB, #{}),
            Pid = start_static_class(Name, none, #{}),
            try
                Tag = beamtalk_class_registry:class_object_tag(Name),
                ?assertEqual(
                    {beamtalk_object, Tag, ?HELPER, Pid},
                    beamtalk_class_shadow_flags:class_ref(Name)
                ),
                ?assertMatch({beamtalk_object, _, ?HELPER, Pid}, class_ref_installed(Name)),
                ok = reload_constant_class(Name, ModB, #{}),
                ?assertEqual(false, class_ref_installed(Name)),
                ?assertEqual(
                    {beamtalk_object, Tag, ModB, Pid},
                    beamtalk_class_shadow_flags:class_ref(Name)
                ),
                ?assertEqual(
                    {beamtalk_object, Tag, ModB, Pid}, class_ref_installed(Name)
                )
            after
                stop_static_class(Pid),
                beamtalk_test_module:unload(ModB)
            end
        end},
        {"an install blocked on the tag lock completes, and a later erase removes it", fun() ->
            Name = 'Bt4756Race',
            Tag = 'Bt4756Race class',
            with_static_class(Name, fun(_Pid) ->
                Parent = self(),
                beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
                    Installer = spawn_link(fun() ->
                        _ = beamtalk_class_shadow_flags:class_ref(Name),
                        Parent ! installed
                    end),
                    %% Wait until the installer is blocked behind the held lock.
                    ok = beamtalk_test_wait:idle(Installer)
                end),
                ?awaitMessage(installed),
                ok = beamtalk_class_shadow_flags:invalidate(Name),
                ?assertEqual(false, class_ref_installed(Name))
            end)
        end}
    ]}.

%% BT-4363's constant-value key cases, repeated for each literal kind a
%% class-side body can be (BT-4504): `V1` and `V2` are two distinct values of
%% that kind, as the terms the compiler publishes in `constant_class_methods`
%% (an Integer, a String as `{beamtalk_string, Bin}`, a Symbol as an atom,
%% and the rest).
constant_value_kinds_test_() ->
    Kinds = [
        {"integer", 1, 2},
        {"float", 1.5, 2.5},
        {"string", {beamtalk_string, <<"one">>}, {beamtalk_string, <<"two">>}},
        {"symbol", one, two},
        {"nil/true", nil, true}
    ],
    {setup, fun setup_runtime/0, fun teardown_runtime/1,
        lists:flatmap(fun constant_value_cases/1, Kinds) ++
            [
                {"a constant-value key is exact: an integer row never certifies a float",
                    fun constant_value_is_exact/0},
                {"registration publishes only constant-shaped values",
                    fun constant_row_keeps_only_constant_values/0}
            ]}.

constant_value_cases({Kind, V1, V2}) ->
    Suffix = [C || C <- Kind, C =/= $/],
    Mod = fun(Stem) -> list_to_atom("bt4504_" ++ Stem ++ "_" ++ Suffix) end,
    Cls = fun(Stem) -> list_to_atom("Bt4504" ++ Stem ++ Suffix) end,
    Tag = fun(Name) -> list_to_atom(atom_to_list(Name) ++ " class") end,
    [
        {
            Kind ++
                ": a constant-value key needs the class's published metadata to certify the literal",
            fun() ->
                %% BT-4363: the hit arm answers the literal without calling the
                %% method, so the key also needs the body to be exactly that
                %% literal, which the class's published metadata row certifies.
                M = Mod("constant_module"),
                Name = Cls("Const"),
                T = Tag(Name),
                load_constant_module(M, #{testSelf => V1}),
                Pid = start_constant_class(Name, M, #{testSelf => V1}),
                try
                    Key = {constant_value, V1, M, class_testSelf, T},
                    ?assertMatch(
                        #beamtalk_object{class = T, pid = Pid}, target(Name, Key, testSelf)
                    ),
                    ?assertMatch(#beamtalk_object{}, key_value(Key)),
                    %% A key naming another value than the body's declines.
                    ?assertNot(target(Name, {constant_value, V2, M, class_testSelf, T}, testSelf)),
                    %% A reload whose body is another literal erases the key,
                    %% and the new row no longer certifies the old value.
                    ok = reload_constant_class(Name, M, #{testSelf => V2}),
                    ?assertNot(key_value(Key)),
                    ?assertNot(target(Name, Key, testSelf)),
                    ?assertMatch(
                        #beamtalk_object{},
                        target(Name, {constant_value, V2, M, class_testSelf, T}, testSelf)
                    ),
                    %% A reload whose body is no longer a literal at all.
                    ok = reload_constant_class(Name, M, #{}),
                    ?assertNot(key_value({constant_value, V2, M, class_testSelf, T})),
                    ?assertNot(target(Name, {constant_value, V2, M, class_testSelf, T}, testSelf))
                after
                    stop_static_class(Pid)
                end
            end
        },
        {
            Kind ++
                ": a constant-value key is certified by the published row, not the current code",
            fun() ->
                %% BT-4363: a reload erases the class's keys inside the new
                %% module's `-on_load`, while the OLD code (and its old
                %% `__beamtalk_meta/0`) is still current. An install racing
                %% that window must not certify the old literal: it is checked
                %% against the row the reload publishes from the new meta.
                %% Here the loaded code still says `V1` while the published row
                %% says `V2`.
                M = Mod("constant_window_module"),
                Name = Cls("ConstWindow"),
                T = Tag(Name),
                load_constant_module(M, #{testSelf => V1}),
                Pid = start_constant_class(Name, M, #{testSelf => V1}),
                try
                    Old = {constant_value, V1, M, class_testSelf, T},
                    ?assertMatch(#beamtalk_object{}, target(Name, Old, testSelf)),
                    ok = beamtalk_class_metadata:put_constant_class_methods(Name, #{
                        testSelf => V2
                    }),
                    %% The publish erased the installed key, and the old
                    %% literal no longer installs although the loaded code
                    %% still says `V1`.
                    ?assertNot(key_value(Old)),
                    ?assertNot(target(Name, Old, testSelf)),
                    ?assertNot(key_value(Old))
                after
                    stop_static_class(Pid)
                end
            end
        },
        {
            Kind ++ ": a runtime fun of the selector erases a constant-value key",
            fun() ->
                M = Mod("constant_shadow_module"),
                Name = Cls("ConstShadow"),
                T = Tag(Name),
                load_constant_module(M, #{testSelf => V1}),
                Pid = start_constant_class(Name, M, #{testSelf => V1}),
                Key = {constant_value, V1, M, class_testSelf, T},
                try
                    ?assertMatch(#beamtalk_object{}, target(Name, Key, testSelf)),
                    %% `put_class_method` (ADR 0084 runtime fun).
                    ok = beamtalk_object_class:put_class_method(
                        Pid, testSelf, fun(_ClassSelf) -> from_runtime_fun end
                    ),
                    ?assertNot(key_value(Key)),
                    ?assertNot(target(Name, Key, testSelf)),
                    ?assertEqual(
                        from_runtime_fun, beamtalk_class_dispatch:class_send(Pid, testSelf, [])
                    ),
                    ?assertEqual(removed, beamtalk_object_class:remove_class_method(Pid, testSelf)),
                    ?assertMatch(#beamtalk_object{}, target(Name, Key, testSelf))
                after
                    stop_static_class(Pid)
                end
            end
        }
    ].

%% BT-4504: the row and the key compare with `=:=`, so an integer body never
%% certifies a call site compiled against the float of the same magnitude.
constant_value_is_exact() ->
    Mod = bt4504_constant_exact_module,
    Name = 'Bt4504ConstExact',
    Tag = 'Bt4504ConstExact class',
    load_constant_module(Mod, #{testSelf => 1}),
    Pid = start_constant_class(Name, Mod, #{testSelf => 1}),
    try
        ?assertNot(target(Name, {constant_value, 1.0, Mod, class_testSelf, Tag}, testSelf)),
        ?assertMatch(
            #beamtalk_object{},
            target(Name, {constant_value, 1, Mod, class_testSelf, Tag}, testSelf)
        )
    after
        stop_static_class(Pid)
    end.

%% BT-4504: a registration publishes a module's `constant_class_methods`
%% entries only when the value has a literal's compiled shape
%% (`beamtalk_class_metadata:is_constant_value/1`).
constant_row_keeps_only_constant_values() ->
    Mod = bt4504_constant_shape_module,
    Name = 'Bt4504ConstShape',
    Good = #{
        int => 1,
        real => 1.5,
        text => {beamtalk_string, <<"hi">>},
        sym => one,
        none => nil
    },
    Bad = #{list => [1, 2], tuple => {other, <<"x">>}, bin => <<"raw">>},
    Constants = maps:merge(Good, Bad),
    load_constant_module(Mod, Constants),
    Pid = start_constant_class(Name, Mod, Constants),
    try
        ?assertEqual(Good, beamtalk_class_metadata:lookup_constant_class_methods(Name))
    after
        stop_static_class(Pid)
    end.

%% A subclass that defines no class methods of its own: every class-side send
%% to it resolves to `Superclass`'s (BT-4270).
start_inheriting_class(Name, Superclass) ->
    {ok, Pid} = beamtalk_object_class:start(Name, #{
        superclass => Superclass,
        module => ?HELPER,
        class_methods => #{},
        class_state => #{},
        meta => ?STATIC_META
    }),
    Pid.

with_static_class(Name, Fun) ->
    Pid = start_static_class(Name, none, #{}),
    try
        Fun(Pid)
    after
        stop_static_class(Pid)
    end.

static_key(Name, Fun) -> {?HELPER, Fun, beamtalk_class_registry:class_object_tag(Name)}.

target(Name, Key, Selector) ->
    beamtalk_class_shadow_flags:static_call_target(Key, Name, Selector, 1).

key_value(Key) -> beamtalk_class_shadow_flags:static_call_key_value(Key).

warm(Name, Pid) ->
    Tag = beamtalk_class_registry:class_object_tag(Name),
    Ref = {beamtalk_object, Tag, ?HELPER, Pid},
    ?assertEqual(Ref, target(Name, static_key(Name, class_testSelf), testSelf)),
    ?assertEqual(Ref, key_value(static_key(Name, class_testSelf))),
    Ref.

key_gone(Name) -> key_value(static_key(Name, class_testSelf)) =:= false.

%% Whether the class's per-class static call key (BT-4442) is installed.
class_map_present(Tag) ->
    persistent_term:get(beamtalk_class_shadow_flags:static_calls_key(Tag), false) =/= false.

%% Hold `Tag`'s lock while an installer (whose unlocked pre-check passes) and
%% `Writer` both block behind it, then release: whichever runs second, no
%% static call key may be left.
race_install_against(Name, Tag, Writer) ->
    Parent = self(),
    beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
        Installer = spawn_link(fun() ->
            Parent ! {installed, target(Name, static_key(Name, class_testSelf), testSelf)}
        end),
        %% Wait until each process is blocked behind the held lock.
        ok = beamtalk_test_wait:idle(Installer),
        WriterPid = spawn_link(fun() ->
            ok = Writer(),
            Parent ! written
        end),
        ok = beamtalk_test_wait:idle(WriterPid)
    end),
    ?awaitMessage({installed, _}),
    ?awaitMessage(written),
    ?assert(key_gone(Name)),
    ?assertNot(target(Name, static_key(Name, class_testSelf), testSelf)),
    ?assert(key_gone(Name)).

%% A throwaway class module for the reload test, compiled from source.
compile_module(Module, Exports, Body) ->
    Src = lists:flatten([
        io_lib:format("-module(~p).~n-export([~s]).~n", [
            Module,
            lists:join(", ", [io_lib:format("~p/~p", [F, A]) || {F, A} <- Exports])
        ]),
        Body
    ]),
    {Module, Bin} = beamtalk_test_module:compile(Src),
    Bin.

load_module_binary(Module, Bin) ->
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin),
    ok.

%% What a compiled static send does with `Key` (`keyed_static_send_doc` in
%% dispatch_codegen.rs): the target in its class's per-class map (BT-4442),
%% else the miss path, else the fallback.
static_send({Module, FunName, Tag} = Key, Name, Selector) ->
    Ref =
        case persistent_term:get(beamtalk_class_shadow_flags:static_calls_key(Tag), #{}) of
            #{Key := Hit} -> Hit;
            _ -> beamtalk_class_shadow_flags:static_call_target(Key, Name, Selector, 1)
        end,
    case Ref of
        false -> fallback;
        _ -> Module:FunName(Ref)
    end.

%% Send `Key` statically in a loop until told to stop; report every exception.
static_sender(Key, Name, Selector, Parent, Errors) ->
    receive
        stop -> Parent ! {sender_errors, lists:reverse(Errors)}
    after 0 ->
        Errors1 =
            try static_send(Key, Name, Selector) of
                _ -> Errors
            catch
                Class:Reason -> [{Class, Reason} | Errors]
            end,
        static_sender(Key, Name, Selector, Parent, Errors1)
    end.

static_call_target_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, [
        {"installs the class reference the callee receives as ClassSelf", fun() ->
            with_static_class('Bt4316Install', fun(Pid) ->
                Ref = warm('Bt4316Install', Pid),
                %% Exactly the dynamic path's ClassSelf.
                ?assertEqual(Ref, beamtalk_class_dispatch:class_send(Pid, testClassSelf, []))
            end)
        end},
        {"declines when the call site's module or function is not the owner's", fun() ->
            with_static_class('Bt4316Mismatch', fun(_Pid) ->
                OtherModule = {bt4316_other_module, class_testSelf, 'Bt4316Mismatch class'},
                OtherFun = static_key('Bt4316Mismatch', class_testClassSelf),
                ?assertNot(target('Bt4316Mismatch', OtherModule, testSelf)),
                ?assertNot(target('Bt4316Mismatch', OtherFun, testSelf)),
                ?assertNot(key_value(OtherModule)),
                ?assertNot(key_value(OtherFun)),
                ?assert(key_gone('Bt4316Mismatch'))
            end)
        end},
        {"declines when the function is not exported", fun() ->
            with_static_class('Bt4316NotExported', fun(_Pid) ->
                Key = static_key('Bt4316NotExported', class_testNotExported),
                ?assertNot(target('Bt4316NotExported', Key, testNotExported)),
                ?assertNot(key_value(Key))
            end)
        end},
        {"declines when the class process is gone", fun() ->
            Pid = start_static_class('Bt4316Dead', none, #{}),
            stop_static_class(Pid),
            ?assertNot(target('Bt4316Dead', static_key('Bt4316Dead', class_testSelf), testSelf)),
            ?assert(key_gone('Bt4316Dead'))
        end},
        {"declines when shadowed; class-variable defaults are no input (ADR 0153)", fun() ->
            with_static_class('Bt4316Shadowed', fun(_Pid) ->
                ok = beamtalk_class_shadow_flags:set(runtime_fun, 'Bt4316Shadowed'),
                try
                    ?assertNot(
                        target(
                            'Bt4316Shadowed', static_key('Bt4316Shadowed', class_testSelf), testSelf
                        )
                    ),
                    ?assert(key_gone('Bt4316Shadowed'))
                after
                    beamtalk_class_shadow_flags:clear(runtime_fun, 'Bt4316Shadowed')
                end
            end),
            Pid = start_static_class('Bt4316Stateful', none, #{count => 0}),
            try
                ?assertMatch(
                    #beamtalk_object{pid = Pid},
                    target('Bt4316Stateful', static_key('Bt4316Stateful', class_testSelf), testSelf)
                )
            after
                stop_static_class(Pid)
            end
        end},
        {"every path that erases the class's derived flag erases its static call keys", fun() ->
            Name = 'Bt4316Erase',
            Tag = 'Bt4316Erase class',
            with_static_class(Name, fun(Pid) ->
                Writes = [
                    fun() -> beamtalk_class_shadow_flags:set(runtime_fun, Name) end,
                    fun() -> beamtalk_class_shadow_flags:invalidate(Name) end
                ],
                lists:foreach(
                    fun(Write) ->
                        _ = warm(Name, Pid),
                        try
                            ok = Write(),
                            ?assert(key_gone(Name)),
                            ?assertNot(class_map_present(Tag)),
                            ?assertNot(
                                lists:member(Tag, beamtalk_class_registry:static_call_key_tags())
                            )
                        after
                            beamtalk_class_shadow_flags:clear(runtime_fun, Name)
                        end
                    end,
                    Writes
                ),
                %% Erased even when the derived key is already gone.
                _ = warm(Name, Pid),
                _ = persistent_term:erase(Tag),
                ok = beamtalk_class_shadow_flags:invalidate(Name),
                ?assert(key_gone(Name))
            end)
        end},
        {"two targets on one class share one key: an erase takes both, a re-warm brings back one",
            fun() ->
                %% BT-4442: one `persistent_term` key per class, its value a
                %% map of call-site key => class reference.
                Name = 'Bt4442TwoTargets',
                Tag = 'Bt4442TwoTargets class',
                with_static_class(Name, fun(Pid) ->
                    Self = static_key(Name, class_testSelf),
                    ClassSelf = static_key(Name, class_testClassSelf),
                    Ref = {beamtalk_object, Tag, ?HELPER, Pid},
                    ?assertEqual(Ref, target(Name, Self, testSelf)),
                    ?assertEqual(Ref, target(Name, ClassSelf, testClassSelf)),
                    ?assertEqual(
                        #{Self => Ref, ClassSelf => Ref},
                        persistent_term:get(beamtalk_class_shadow_flags:static_calls_key(Tag))
                    ),
                    %% A target already present is answered, not rewritten.
                    Map = persistent_term:get(beamtalk_class_shadow_flags:static_calls_key(Tag)),
                    ?assertEqual(Ref, target(Name, Self, testSelf)),
                    ?assert(
                        erts_debug:same(
                            Map,
                            persistent_term:get(beamtalk_class_shadow_flags:static_calls_key(Tag))
                        )
                    ),
                    ok = beamtalk_class_shadow_flags:invalidate(Name),
                    ?assertNot(key_value(Self)),
                    ?assertNot(key_value(ClassSelf)),
                    ?assertNot(class_map_present(Tag)),
                    ?assertEqual(Ref, target(Name, Self, testSelf)),
                    ?assertEqual(Ref, key_value(Self)),
                    ?assertNot(key_value(ClassSelf)),
                    ok = beamtalk_class_shadow_flags:invalidate(Name),
                    ?assertNot(class_map_present(Tag))
                end)
            end},
        {"concurrent installs of different targets on one class keep both", fun() ->
            %% BT-4442: an install is a read-modify-write of the class's map,
            %% so only the tag lock keeps one install from dropping another's
            %% target. Both installers block behind the held lock, then race.
            Name = 'Bt4442RaceTargets',
            Tag = 'Bt4442RaceTargets class',
            with_static_class(Name, fun(Pid) ->
                Self = static_key(Name, class_testSelf),
                ClassSelf = static_key(Name, class_testClassSelf),
                Ref = {beamtalk_object, Tag, ?HELPER, Pid},
                Parent = self(),
                beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
                    Installers = [
                        spawn_link(fun() -> Parent ! {installed, target(Name, K, Sel)} end)
                     || {K, Sel} <- [{Self, testSelf}, {ClassSelf, testClassSelf}]
                    ],
                    %% Wait until both installers are blocked behind the held lock.
                    ok = beamtalk_test_wait:idle(Installers)
                end),
                [
                    begin
                        ?awaitMessage({installed, Got}),
                        ?assertEqual(Ref, Got)
                    end
                 || _ <- [1, 2]
                ],
                ?assertEqual(Ref, key_value(Self)),
                ?assertEqual(Ref, key_value(ClassSelf)),
                ok = beamtalk_class_shadow_flags:invalidate(Name)
            end)
        end},
        {"installs an inherited owner's function with the receiving class as ClassSelf", fun() ->
            %% BT-4270: a class-literal signal send keys `{DefiningModule,
            %% FunName, ReceiverTag}`; the value is the dynamic path's
            %% ClassSelf for the receiving class.
            Root = start_static_class('Bt4270Root', none, #{}),
            Leaf = start_inheriting_class('Bt4270Leaf', 'Bt4270Root'),
            try
                Key = static_key('Bt4270Leaf', class_testClassSelf),
                Ref = target('Bt4270Leaf', Key, testClassSelf),
                ?assertMatch(#beamtalk_object{class = 'Bt4270Leaf class', pid = Leaf}, Ref),
                ?assertEqual(Ref, key_value(Key)),
                ?assertEqual(Ref, beamtalk_class_dispatch:class_send(Leaf, testClassSelf, [])),
                %% An ancestor's write erases it, as a descendant's key.
                ok = beamtalk_class_shadow_flags:invalidate('Bt4270Root'),
                ?assertNot(key_value(Key)),
                %% Re-installed from the live classes on the next miss.
                ?assertEqual(Ref, target('Bt4270Leaf', Key, testClassSelf))
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"an inherited owner's runtime fun erases the key and declines it", fun() ->
            %% BT-4270: the in-caller path applies the defining class's ADR
            %% 0084 runtime fun instead of its compiled function, so no key
            %% may name that function while the fun is installed.
            Root = start_static_class('Bt4270FunRoot', none, #{}),
            Leaf = start_inheriting_class('Bt4270FunLeaf', 'Bt4270FunRoot'),
            try
                Key = static_key('Bt4270FunLeaf', class_testClassSelf),
                ?assertMatch(#beamtalk_object{}, target('Bt4270FunLeaf', Key, testClassSelf)),
                ok = beamtalk_object_class:put_class_method(
                    Root, testClassSelf, fun(_ClassSelf) -> from_runtime_fun end
                ),
                ?assertNot(key_value(Key)),
                ?assertNot(target('Bt4270FunLeaf', Key, testClassSelf)),
                ?assertNot(key_value(Key)),
                ?assertEqual(
                    from_runtime_fun, beamtalk_class_dispatch:class_send(Leaf, testClassSelf, [])
                )
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"removing a runtime fun restores the compiled method and its static key", fun() ->
            %% BT-4438: `X class removeSelector:` undoes `put_class_method`:
            %% the compiled function is the body again, the gate closes once
            %% no funs remain, and a warmed key re-installs on the next miss.
            Name = 'Bt4438Own',
            with_static_class(Name, fun(Pid) ->
                Key = static_key(Name, class_testClassSelf),
                Ref = target(Name, Key, testClassSelf),
                ?assertMatch(#beamtalk_object{}, Ref),
                ok = beamtalk_object_class:put_class_method(
                    Pid, testClassSelf, fun(_ClassSelf) -> from_runtime_fun end
                ),
                ok = beamtalk_object_class:put_class_method(
                    Pid, testClassSelf, fun(_ClassSelf) -> from_second_fun end
                ),
                ?assertEqual(
                    from_second_fun, beamtalk_class_dispatch:class_send(Pid, testClassSelf, [])
                ),
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Pid, testClassSelf)
                ),
                ?assertEqual(absent, beamtalk_object_class:remove_class_method(Pid, testClassSelf)),
                ?assertNot(beamtalk_class_metadata:has_runtime_class_methods(Name)),
                ?assertNot(beamtalk_class_shadow_flags:is_set(runtime_fun, Name)),
                ?assert(
                    lists:member(testClassSelf, beamtalk_object_class:local_class_methods(Pid))
                ),
                ?assertEqual(Ref, beamtalk_class_dispatch:class_send(Pid, testClassSelf, [])),
                ?assertEqual(Ref, target(Name, Key, testClassSelf)),
                ?assertEqual(Ref, key_value(Key))
            end)
        end},
        {"removing an inherited owner's runtime fun restores the descendant", fun() ->
            %% BT-4438: the fun was a selector the root's class table gained;
            %% removing it erases the descendant's key with the root's, and
            %% the descendant's send resolves to the compiled method again.
            Root = start_static_class('Bt4438FunRoot', none, #{}),
            Leaf = start_inheriting_class('Bt4438FunLeaf', 'Bt4438FunRoot'),
            try
                Key = static_key('Bt4438FunLeaf', class_testClassSelf),
                Ref = target('Bt4438FunLeaf', Key, testClassSelf),
                ok = beamtalk_object_class:put_class_method(
                    Root, extraSelector, fun(_ClassSelf) -> extra end
                ),
                ok = beamtalk_object_class:put_class_method(
                    Root, testClassSelf, fun(_ClassSelf) -> from_runtime_fun end
                ),
                ?assertEqual(
                    from_runtime_fun, beamtalk_class_dispatch:class_send(Leaf, testClassSelf, [])
                ),
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Root, testClassSelf)
                ),
                %% Another fun remains, so the gate stays open.
                ?assert(beamtalk_class_metadata:has_runtime_class_methods('Bt4438FunRoot')),
                ?assertEqual(Ref, beamtalk_class_dispatch:class_send(Leaf, testClassSelf, [])),
                ?assertEqual(Ref, target('Bt4438FunLeaf', Key, testClassSelf)),
                ?assertEqual(Ref, key_value(Key)),
                %% A selector the class never had compiled is gone outright.
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Root, extraSelector)
                ),
                ?assertNot(
                    lists:member(extraSelector, beamtalk_object_class:local_class_methods(Root))
                ),
                ?assertNot(beamtalk_class_metadata:has_runtime_class_methods('Bt4438FunRoot'))
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"removing a put over a builder fun restores the builder fun", fun() ->
            %% BT-4438: the first put stashes whatever it replaced, a
            %% builder-seeded fun included; removal reinstalls it and the gate
            %% stays open.
            {module, ?HELPER} = code:ensure_loaded(?HELPER),
            Name = 'Bt4438Builder',
            {ok, Pid} = beamtalk_object_class:start(Name, #{
                superclass => none,
                module => ?HELPER,
                class_methods => maps:put(
                    builderAnswer,
                    #{block => fun(_ClassSelf) -> from_builder end, arity => 1},
                    ?STATIC_METHODS
                ),
                class_state => #{},
                meta => ?STATIC_META
            }),
            try
                ?assertEqual(
                    from_builder, beamtalk_class_dispatch:class_send(Pid, builderAnswer, [])
                ),
                ok = beamtalk_object_class:put_class_method(
                    Pid, builderAnswer, fun(_ClassSelf) -> from_put end
                ),
                ?assertEqual(from_put, beamtalk_class_dispatch:class_send(Pid, builderAnswer, [])),
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Pid, builderAnswer)
                ),
                ?assertEqual(
                    from_builder, beamtalk_class_dispatch:class_send(Pid, builderAnswer, [])
                ),
                ?assert(beamtalk_class_metadata:has_runtime_class_methods(Name)),
                %% With no stash left, removing the builder fun drops it.
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Pid, builderAnswer)
                ),
                ?assertNot(
                    lists:member(builderAnswer, beamtalk_object_class:local_class_methods(Pid))
                ),
                ?assertNot(beamtalk_class_metadata:has_runtime_class_methods(Name))
            after
                stop_static_class(Pid)
            end
        end},
        {"a reload that carries a put fun over keeps what it hid", fun() ->
            %% BT-4438: a reload naming no class methods keeps the put fun, so
            %% removing it afterwards still restores the compiled method.
            Name = 'Bt4438Reload',
            with_static_class(Name, fun(Pid) ->
                Ref = beamtalk_class_dispatch:class_send(Pid, testClassSelf, []),
                ok = beamtalk_object_class:put_class_method(
                    Pid, testClassSelf, fun(_ClassSelf) -> from_runtime_fun end
                ),
                {ok, _} = beamtalk_object_class:update_class(Name, #{
                    superclass => none, module => ?HELPER, meta => ?STATIC_META
                }),
                ?assertEqual(
                    from_runtime_fun, beamtalk_class_dispatch:class_send(Pid, testClassSelf, [])
                ),
                ?assertEqual(
                    removed, beamtalk_object_class:remove_class_method(Pid, testClassSelf)
                ),
                ?assertEqual(Ref, beamtalk_class_dispatch:class_send(Pid, testClassSelf, []))
            end)
        end},
        {"a caller-safe raise key needs the defining module to list the selector", fun() ->
            %% BT-4270: the hit arm raises as the method body would, without
            %% calling it, so the key also needs the body to be a caller-safe
            %% primitive, which the defining module's metadata lists.
            Mod = bt4270_raise_root_module,
            load_raise_module(Mod, #{testSelf => classSignal}),
            Root = start_raise_class('Bt4270RaiseRoot', Mod, #{testSelf => classSignal}),
            Leaf = start_inheriting_class('Bt4270RaiseLeaf', 'Bt4270RaiseRoot'),
            try
                Key =
                    {caller_safe_raise, classSignal, Mod, class_testSelf, 'Bt4270RaiseLeaf class'},
                ?assertMatch(
                    #beamtalk_object{class = 'Bt4270RaiseLeaf class', pid = Leaf},
                    target('Bt4270RaiseLeaf', Key, testSelf)
                ),
                ?assertMatch(#beamtalk_object{}, key_value(Key)),
                %% A key naming another primitive than the body's declines.
                OtherPrimitive =
                    {caller_safe_raise, classSignalOther, Mod, class_testSelf,
                        'Bt4270RaiseLeaf class'},
                ?assertNot(target('Bt4270RaiseLeaf', OtherPrimitive, testSelf)),
                %% A reload whose new body is no longer the primitive: the
                %% reload publishes the new row, which erases the key and no
                %% longer certifies it.
                load_raise_module(Mod, #{}),
                ok = beamtalk_class_metadata:put_caller_safe_primitives('Bt4270RaiseRoot', #{}),
                ?assertNot(key_value(Key)),
                ?assertNot(target('Bt4270RaiseLeaf', Key, testSelf)),
                ?assertNot(key_value(Key))
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"a caller-safe raise key is certified by the published row, not the current code", fun() ->
                %% BT-4493: during a reload's `-on_load` the old code (and its
                %% `__beamtalk_meta/0`) is still current. The loaded code here
                %% still says `classSignal`, but the published row says
                %% otherwise, so the key declines.
                Mod = bt4493_raise_window_module,
                load_raise_module(Mod, #{testSelf => classSignal}),
                Root = start_raise_class('Bt4493RaiseRoot', Mod, #{testSelf => classSignal}),
                Leaf = start_inheriting_class('Bt4493RaiseLeaf', 'Bt4493RaiseRoot'),
                try
                    Key =
                        {caller_safe_raise, classSignal, Mod, class_testSelf,
                            'Bt4493RaiseLeaf class'},
                    ?assertMatch(#beamtalk_object{}, target('Bt4493RaiseLeaf', Key, testSelf)),
                    ?assertMatch(#beamtalk_object{}, key_value(Key)),
                    %% The publish erases the descendant's installed key.
                    ok = beamtalk_class_metadata:put_caller_safe_primitives('Bt4493RaiseRoot', #{}),
                    ?assertNot(key_value(Key)),
                    ?assertNot(target('Bt4493RaiseLeaf', Key, testSelf)),
                    ?assertNot(key_value(Key))
                after
                    stop_static_class(Leaf),
                    stop_static_class(Root)
                end
            end},
        {"a caller-safe raise key declines a module with no such metadata", fun() ->
            with_static_class('Bt4270NoMeta', fun(_Pid) ->
                Key =
                    {caller_safe_raise, classSignal, ?HELPER, class_testSelf, 'Bt4270NoMeta class'},
                ?assertNot(target('Bt4270NoMeta', Key, testSelf)),
                ?assertNot(key_value(Key)),
                %% The direct-call key of the same function still installs.
                _ = warm('Bt4270NoMeta', _Pid)
            end)
        end},
        {"a constant-value key declines a class that publishes no constants", fun() ->
            %% The module's own metadata certifies the literal, but the class
            %% registered without publishing it (an older registration).
            Mod = bt4363_constant_unpublished_module,
            load_constant_module(Mod, #{testSelf => 1}),
            Pid = start_class_on('Bt4363NoMeta', none, Mod),
            try
                Key = {constant_value, 1, Mod, class_testSelf, 'Bt4363NoMeta class'},
                ?assertNot(target('Bt4363NoMeta', Key, testSelf)),
                ?assertNot(key_value(Key))
            after
                stop_static_class(Pid)
            end
        end},
        {"an ancestor's write erases a descendant's static call keys", fun() ->
            Root = start_static_class('Bt4316Root', none, #{}),
            Leaf = start_static_class('Bt4316Leaf', 'Bt4316Root', #{}),
            try
                _ = warm('Bt4316Leaf', Leaf),
                ok = beamtalk_class_shadow_flags:invalidate('Bt4316Root'),
                ?assert(key_gone('Bt4316Leaf'))
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"the class monitor's 'DOWN' erases the class's static call keys", fun() ->
            Pid = start_static_class('Bt4316Down', none, #{}),
            _ = warm('Bt4316Down', Pid),
            stop_static_class(Pid),
            beamtalk_test_wait:until(fun() -> key_gone('Bt4316Down') end, 500)
        end},
        {"a (re)starting class monitor erases every class's static call keys", fun() ->
            %% A class killed while no monitor ran sent no 'DOWN', so only the
            %% fresh monitor's init/1 can erase its dead-pid keys.
            with_static_class('Bt4316MonitorInit', fun(Pid) ->
                _ = warm('Bt4316MonitorInit', Pid),
                {ok, _State} = beamtalk_class_monitor:init(#{}),
                ?assert(key_gone('Bt4316MonitorInit')),
                %% The next send re-installs from the live class.
                _ = warm('Bt4316MonitorInit', Pid)
            end)
        end},
        {"restart_class/1 erases a dead class's keys even when no 'DOWN' was seen", fun() ->
            %% A missed 'DOWN' (no monitor watching when the class died) must
            %% not let a restart keep serving the dead pid as ClassSelf: the
            %% restart re-registers via merge_identity/5, which invalidates.
            Name = 'Bt4316Restart',
            Pid = start_static_class(Name, none, #{}),
            OldRef = warm(Name, Pid),
            ok = beamtalk_class_monitor:unwatch(Name),
            MRef = erlang:monitor(process, Pid),
            exit(Pid, kill),
            ?awaitMessage({'DOWN', MRef, process, Pid, _}),
            ?assertEqual(OldRef, key_value(static_key(Name, class_testSelf))),
            {ok, NewPid} = beamtalk_class_registry:restart_class(Name),
            %% (Rebuilt from the helper module's meta, the restarted class has
            %% no class-method table, so this test checks only the erase.)
            try
                ?assert(key_gone(Name))
            after
                stop_static_class(NewPid)
            end
        end},
        {"an install racing a shadow raise never survives it", fun() ->
            Name = 'Bt4316Race',
            Tag = 'Bt4316Race class',
            with_static_class(Name, fun(_Pid) ->
                try
                    race_install_against(Name, Tag, fun() ->
                        beamtalk_class_shadow_flags:set(runtime_fun, Name)
                    end)
                after
                    beamtalk_class_shadow_flags:clear(runtime_fun, Name)
                end
            end)
        end},
        {"a call site naming a renamed class never hits the key its new name installed", fun() ->
            %% `renameTo:` keeps the module and the process: before the key
            %% carried the tag, the old name's call site hit the new name's
            %% key and ran with its class reference.
            Pid = start_static_class('Bt4316Before', none, #{}),
            OldKey = static_key('Bt4316Before', class_testSelf),
            NewKey = static_key('Bt4316After', class_testSelf),
            try
                _ = warm('Bt4316Before', Pid),
                {ok, Pid} = beamtalk_object_class:rename('Bt4316Before', 'Bt4316After'),
                ?assertNot(key_value(OldKey)),
                NewRef = warm('Bt4316After', Pid),
                ?assertEqual('Bt4316After class', NewRef#beamtalk_object.class),
                ?assertNot(key_value(OldKey)),
                ?assertNot(target('Bt4316Before', OldKey, testSelf)),
                ?assertNot(key_value(OldKey)),
                ?assertEqual(NewRef, key_value(NewKey))
            after
                stop_static_class(Pid)
            end
        end},
        {"a call site naming a removed class never hits a key a reuse of its module installed",
            fun() ->
                Old = start_static_class('Bt4316Removed', none, #{}),
                OldKey = static_key('Bt4316Removed', class_testSelf),
                _ = warm('Bt4316Removed', Old),
                stop_static_class(Old),
                beamtalk_test_wait:until(fun() -> key_gone('Bt4316Removed') end, 500),
                with_static_class('Bt4316Reused', fun(New) ->
                    NewRef = warm('Bt4316Reused', New),
                    ?assertEqual('Bt4316Reused class', NewRef#beamtalk_object.class),
                    ?assertNot(key_value(OldKey)),
                    ?assertNot(target('Bt4316Removed', OldKey, testSelf)),
                    ?assertNot(key_value(OldKey))
                end)
            end},
        {"a reload that drops a method leaves no key naming it once the new code is current",
            fun() ->
                %% The reloaded module's `-on_load` hook registers the class
                %% (`update_class/2`, as a compiled module's `register_class/0`
                %% does) while the old code is still current, then probes the
                %% dropped method's key from inside the hook: it must be gone
                %% and the miss path must decline before the new code becomes
                %% current, so no static send can reach `undef`.
                Module = bt4316_reload_mod,
                Name = 'Bt4316Reload',
                Tag = 'Bt4316Reload class',
                Kept = {Module, class_kept, Tag},
                Dropped = {Module, class_dropped, Tag},
                Probe = {?MODULE, bt4316_reload_probe},
                V1 = compile_module(
                    Module,
                    [{class_kept, 1}, {class_dropped, 1}],
                    "class_kept(_) -> kept_v1.\nclass_dropped(_) -> dropped_v1.\n"
                ),
                V2 = compile_module(
                    Module,
                    [{class_kept, 1}, {register, 0}],
                    io_lib:format(
                        "-on_load(register/0).~n"
                        "class_kept(_) -> kept_v2.~n"
                        "register() ->~n"
                        "    {ok, _} = beamtalk_object_class:update_class(~p, #{module => ~p,"
                        " class_methods => #{kept => #{arity => 0}}, meta => ~p}),~n"
                        "    persistent_term:put(~p, {"
                        "beamtalk_class_shadow_flags:static_call_key_value(~p),"
                        " beamtalk_class_shadow_flags:static_call_target(~p, ~p, dropped, 1),"
                        " erlang:function_exported(~p, class_dropped, 1)}),~n"
                        "    ok.~n",
                        [Name, Module, ?STATIC_META, Probe, Dropped, Dropped, Name, Module]
                    )
                ),
                ok = load_module_binary(Module, V1),
                {ok, Pid} = beamtalk_object_class:start(Name, #{
                    superclass => none,
                    module => Module,
                    class_methods => #{kept => #{arity => 0}, dropped => #{arity => 0}},
                    class_state => #{},
                    meta => ?STATIC_META
                }),
                try
                    ?assertEqual(kept_v1, static_send(Kept, Name, kept)),
                    ?assertEqual(dropped_v1, static_send(Dropped, Name, dropped)),
                    ?assertMatch(#beamtalk_object{}, key_value(Dropped)),
                    Parent = self(),
                    Sender = spawn_link(fun() ->
                        static_sender(Dropped, Name, dropped, Parent, [])
                    end),
                    ok = load_module_binary(Module, V2),
                    %% Inside the hook the old code still exported the dropped
                    %% function, yet its key was gone and the miss path
                    %% declined.
                    ?assertEqual({false, false, true}, persistent_term:get(Probe)),
                    ?assertNot(erlang:function_exported(Module, class_dropped, 1)),
                    %% sleep: lets the concurrent sender keep sending across the reload window
                    timer:sleep(20),
                    Sender ! stop,
                    begin
                        ?awaitMessage({sender_errors, Errors}),
                        ?assertEqual([], Errors)
                    end,
                    ?assertNot(key_value(Dropped)),
                    ?assertEqual(fallback, static_send(Dropped, Name, dropped)),
                    ?assertEqual(kept_v2, static_send(Kept, Name, kept))
                after
                    _ = persistent_term:erase(Probe),
                    stop_static_class(Pid),
                    beamtalk_test_module:unload(Module)
                end
            end}
    ]}.

%% BT-4004: the node-local tag lock is a `beamtalk_fifo_lock`. Tag-lock
%% installers bound their wait and decline (fallback), tag-lock erasers wait
%% unbounded.
bounded_lock_wait_test_() ->
    {setup,
        fun() ->
            setup_runtime(),
            application:set_env(
                beamtalk_runtime, fifo_lock_timeout_ms, #{direct_flag => 100}
            )
        end,
        fun(_) -> application:unset_env(beamtalk_runtime, fifo_lock_timeout_ms) end, [
            {"an installer behind a hung tag-lock holder declines; an eraser waits", fun() ->
                Name = 'Bt4004Cls',
                Tag = beamtalk_class_registry:class_object_tag(Name),
                stateless_row(Name),
                _ = persistent_term:erase(Tag),
                Parent = self(),
                Holder = spawn_link(fun() ->
                    beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
                        Parent ! holding,
                        receive
                            release -> ok
                        end
                    end)
                end),
                ?awaitMessage(holding),
                %% The send path gives up after the bound and takes the fallback.
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                %% An eraser must not give up: it is still waiting after the bound.
                Eraser = spawn_link(fun() ->
                    ok = beamtalk_class_shadow_flags:invalidate(Name),
                    Parent ! erased
                end),
                receive
                    erased -> error(eraser_gave_up_or_skipped_the_lock)
                after 400 -> ok
                end,
                Holder ! release,
                ?awaitMessage(erased),
                %% The lock is free again: the install now succeeds.
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
            end}
        ]}.
