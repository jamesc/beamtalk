%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_shadow_flags_tests).

-moduledoc """
BT-3669: the per-class "has shadows" flags that gate the compiled class-side
self-send fast path. Covers register / unregister / installed-fun transitions
and concurrent access (the extension registry is global, so BUnit runs can
register and unregister on the same class concurrently).
""".

-include_lib("eunit/include/eunit.hrl").

setup_runtime() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

teardown_runtime(_) ->
    ok.

direct(Tag, Name, Sel) ->
    beamtalk_class_dispatch:class_self_direct_ok(Tag, Tag, Name, Sel).

noop_fun() -> fun(_, _) -> ok end.

%% A throwaway metadata row for a stateless class under `Object`: the derived
%% flag reads the chain's published class-variable facts and fails closed on a
%% class with no row (BT-4301), so a class whose `direct_call_ok/2` must hold
%% needs one.
stateless_row(Name) ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Object', false),
    ok = beamtalk_class_metadata:put_class_variables(Name, false).

shadow_flag_test_() ->
    {setup, fun setup_runtime/0, fun teardown_runtime/1, [
        {"extension registry init raises the readiness flag the fast path reads", fun() ->
            %% BT-3676: the guard reads a readiness flag, not the extension
            %% table; `beamtalk_extensions:init/0` raises it (idempotently).
            %% Erase it first so removing `mark_ready/0` from `init/0` fails here.
            _ = persistent_term:erase(beamtalk_class_shadow_ready),
            ?assertNot(beamtalk_class_shadow_flags:is_ready()),
            ok = beamtalk_extensions:init(),
            ?assert(beamtalk_class_shadow_flags:is_ready()),
            ok = beamtalk_extensions:init(),
            ?assert(beamtalk_class_shadow_flags:is_ready())
        end},
        {"direct_call_ok/2 is the readiness flag and neither shadow flag (BT-3690)", fun() ->
            Tag = 'Bt3690Truth class',
            Name = 'Bt3690Truth',
            ok = stateless_row(Name),
            ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
            try
                ok = beamtalk_class_shadow_flags:set(extension, Tag),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ok = beamtalk_class_shadow_flags:clear(extension, Tag),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
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
                beamtalk_class_shadow_flags:clear(extension, Tag),
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
                ok = beamtalk_class_shadow_flags:set(extension, Tag),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ok = beamtalk_class_shadow_flags:clear(extension, Tag),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag))
            after
                beamtalk_class_shadow_flags:clear(extension, Tag),
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                persistent_term:erase(Tag)
            end
        end},
        {"extension register/unregister flips the derived flag before the next send (BT-3700)",
            fun() ->
                %% Throwaway class: never touch the real `Object` shared state.
                Name = 'Bt3700Ext',
                Tag = 'Bt3700Ext class',
                _ = persistent_term:erase(Tag),
                try
                    ?assert(direct(Tag, Name, bt3700_probe)),
                    ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                    ok = beamtalk_extensions:register(Tag, bt3700_ext, noop_fun(), bt3700),
                    ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                    ?assertNot(direct(Tag, Name, bt3700_probe)),
                    ok = beamtalk_extensions:unregister(Name, bt3700_ext, true),
                    ?assert(direct(Tag, Name, bt3700_probe))
                after
                    beamtalk_extensions:unregister(Name, bt3700_ext, true),
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
                timer:sleep(50),
                Raiser = spawn_link(fun() ->
                    ok = beamtalk_class_shadow_flags:set(extension, Tag),
                    Parent ! raised
                end),
                timer:sleep(50),
                _ = {Sender, Raiser}
            end),
            receive
                {sender, _} -> ok
            after 5000 -> ?assert(false)
            end,
            receive
                raised -> ok
            after 5000 -> ?assert(false)
            end,
            try
                ?assert(beamtalk_class_shadow_flags:is_set(extension, Tag)),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
            after
                beamtalk_class_shadow_flags:clear(extension, Tag),
                persistent_term:erase(Tag)
            end
        end},
        {"unregistering the last class-side extension restores the fast path", fun() ->
            Sel = bt3669_ext_restore,
            ?assert(direct('Object class', 'Object', Sel)),
            ok = beamtalk_extensions:register('Object class', Sel, noop_fun(), bt3669),
            ?assertNot(direct('Object class', 'Object', Sel)),
            ok = beamtalk_extensions:unregister('Object', Sel, true),
            ?assert(direct('Object class', 'Object', Sel))
        end},
        {"flag stays up while another extension remains on the class", fun() ->
            ok = beamtalk_extensions:register('Object class', bt3669_a, noop_fun(), bt3669),
            ok = beamtalk_extensions:register('Object class', bt3669_b, noop_fun(), bt3669),
            try
                ok = beamtalk_extensions:unregister('Object', bt3669_a, true),
                ?assertNot(direct('Object class', 'Object', bt3669_probe)),
                ok = beamtalk_extensions:unregister('Object', bt3669_b, true),
                ?assert(direct('Object class', 'Object', bt3669_probe))
            after
                beamtalk_extensions:unregister('Object', bt3669_a, true),
                beamtalk_extensions:unregister('Object', bt3669_b, true)
            end
        end},
        {"re-registering the same extension keeps it shadowing", fun() ->
            ok = beamtalk_extensions:register('Object class', bt3669_re, noop_fun(), bt3669),
            ok = beamtalk_extensions:register('Object class', bt3669_re, noop_fun(), bt3669),
            try
                ?assertNot(direct('Object class', 'Object', bt3669_re)),
                ok = beamtalk_extensions:unregister('Object', bt3669_re, true),
                ?assert(direct('Object class', 'Object', bt3669_re))
            after
                beamtalk_extensions:unregister('Object', bt3669_re, true)
            end
        end},
        {"instance-side extensions do not disable the class-side fast path", fun() ->
            ok = beamtalk_extensions:register('Object', bt3669_inst, noop_fun(), bt3669),
            try
                ?assert(direct('Object class', 'Object', bt3669_inst))
            after
                beamtalk_extensions:unregister('Object', bt3669_inst, false)
            end
        end},
        {"purge_class/1 clears the flag", fun() ->
            Tag = 'Bt3669Purge class',
            ok = beamtalk_extensions:register(Tag, sel, noop_fun(), bt3669),
            ?assert(beamtalk_class_shadow_flags:is_set(extension, Tag)),
            ok = beamtalk_extensions:purge_class(Tag),
            ?assertNot(beamtalk_class_shadow_flags:is_set(extension, Tag))
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
        end},
        {"instance-side register/unregister leaves no extension flag entry", fun() ->
            ok = beamtalk_extensions:register('Object', bt3669_inst, noop_fun(), bt3669),
            try
                ?assertNot(beamtalk_class_shadow_flags:is_set(extension, 'Object')),
                ?assertEqual(
                    [],
                    [
                        K
                     || {K = {beamtalk_class_shadow, extension, 'Object'}, _} <- persistent_term:get()
                    ]
                )
            after
                beamtalk_extensions:unregister('Object', bt3669_inst, false)
            end,
            ?assertNot(beamtalk_class_shadow_flags:is_set(extension, 'Object'))
        end},
        {"register is serialized against an in-flight last-unregister (no false flag beside a row)",
            fun() ->
                Tag = 'Bt3669Ser class',
                Parent = self(),
                ok = beamtalk_extensions:register(Tag, bt3669_ser_a, noop_fun(), bt3669),
                %% Hold the per-tag lock: an unregister of the last extension and a
                %% register both block behind it (acquisition order is not FIFO).
                %% The assertions below hold for either order.
                beamtalk_extensions:with_shadow_lock(Tag, fun() ->
                    U = spawn_link(fun() ->
                        ok = beamtalk_extensions:unregister('Bt3669Ser', bt3669_ser_a, true),
                        Parent ! {u_done, self()}
                    end),
                    timer:sleep(100),
                    R = spawn_link(fun() ->
                        ok = beamtalk_extensions:register(Tag, bt3669_ser_b, noop_fun(), bt3669),
                        Parent ! {r_done, self()}
                    end),
                    timer:sleep(100),
                    %% Neither has run: state is untouched while the lock is held.
                    ?assert(beamtalk_class_shadow_flags:is_set(extension, Tag)),
                    put(bt3669_workers, {U, R})
                end),
                {U, R} = erase(bt3669_workers),
                receive
                    {u_done, U} -> ok
                after 5000 -> error(u_timeout)
                end,
                receive
                    {r_done, R} -> ok
                after 5000 -> error(r_timeout)
                end,
                try
                    %% The row exists, so the flag must be up and the guard must decline.
                    ?assertMatch({ok, _, bt3669}, beamtalk_extensions:lookup(Tag, bt3669_ser_b)),
                    ?assert(beamtalk_class_shadow_flags:is_set(extension, Tag)),
                    ?assertNot(direct(Tag, 'Bt3669Ser', bt3669_ser_b))
                after
                    beamtalk_extensions:unregister('Bt3669Ser', bt3669_ser_b, true)
                end,
                ?assertNot(beamtalk_class_shadow_flags:is_set(extension, Tag))
            end},
        {"concurrent register/unregister settles with the flag cleared", fun() ->
            Tag = 'Bt3669Conc class',
            Parent = self(),
            Workers = [
                spawn_link(fun() ->
                    Sel = list_to_atom("bt3669_conc_" ++ integer_to_list(N)),
                    lists:foreach(
                        fun(_) ->
                            ok = beamtalk_extensions:register(Tag, Sel, noop_fun(), bt3669),
                            %% Only the settled state is asserted here.
                            ok = beamtalk_extensions:unregister('Bt3669Conc', Sel, true)
                        end,
                        lists:seq(1, 200)
                    ),
                    Parent ! {done, self()}
                end)
             || N <- lists:seq(1, 8)
            ],
            lists:foreach(
                fun(W) ->
                    receive
                        {done, W} -> ok
                    after 30000 -> error(timeout)
                    end
                end,
                Workers
            ),
            ?assertNot(beamtalk_class_shadow_flags:is_set(extension, Tag)),
            ok = beamtalk_extensions:register(Tag, bt3669_conc_final, noop_fun(), bt3669),
            ?assert(beamtalk_class_shadow_flags:is_set(extension, Tag)),
            ok = beamtalk_extensions:unregister('Bt3669Conc', bt3669_conc_final, true),
            ?assertNot(beamtalk_class_shadow_flags:is_set(extension, Tag))
        end}
    ]}.

%%% ============================================================================
%%% BT-4316: static call keys (`static_call_target/5`)
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
    catch gen_server:stop(Pid, normal, 5000),
    ok.

with_static_class(Name, Fun) ->
    Pid = start_static_class(Name, none, #{}),
    try
        Fun(Pid)
    after
        stop_static_class(Pid)
    end.

static_key(Fun) -> {?HELPER, Fun}.

target(Name, Key, Selector) ->
    Tag = beamtalk_class_registry:class_object_tag(Name),
    beamtalk_class_shadow_flags:static_call_target(Key, Tag, Name, Selector, 1).

key_value(Key) -> beamtalk_class_shadow_flags:static_call_key_value(Key).

warm(Name, Pid) ->
    Tag = beamtalk_class_registry:class_object_tag(Name),
    Ref = {beamtalk_object, Tag, ?HELPER, Pid},
    ?assertEqual(Ref, target(Name, static_key(class_testSelf), testSelf)),
    ?assertEqual(Ref, key_value(static_key(class_testSelf))),
    Ref.

key_gone() -> key_value(static_key(class_testSelf)) =:= false.

%% The class monitor erases asynchronously, on its 'DOWN'.
wait_until(Pred, 0) ->
    ?assert(Pred());
wait_until(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(10),
            wait_until(Pred, N - 1)
    end.

%% Hold `Tag`'s lock while an installer (whose unlocked pre-check passes) and
%% `Writer` both block behind it, then release: whichever runs second, no
%% static call key may be left.
race_install_against(Name, Tag, Writer) ->
    Parent = self(),
    beamtalk_class_shadow_flags:with_tag_lock(Tag, fun() ->
        _ = spawn_link(fun() ->
            Parent ! {installed, target(Name, static_key(class_testSelf), testSelf)}
        end),
        timer:sleep(50),
        _ = spawn_link(fun() ->
            ok = Writer(),
            Parent ! written
        end),
        timer:sleep(50)
    end),
    receive
        {installed, _} -> ok
    after 5000 -> ?assert(false)
    end,
    receive
        written -> ok
    after 5000 -> ?assert(false)
    end,
    ?assert(key_gone()),
    ?assertNot(target(Name, static_key(class_testSelf), testSelf)),
    ?assert(key_gone()).

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
                OtherModule = {bt4316_other_module, class_testSelf},
                OtherFun = static_key(class_testClassSelf),
                ?assertNot(target('Bt4316Mismatch', OtherModule, testSelf)),
                ?assertNot(target('Bt4316Mismatch', OtherFun, testSelf)),
                ?assertNot(key_value(OtherModule)),
                ?assertNot(key_value(OtherFun)),
                ?assert(key_gone())
            end)
        end},
        {"declines when the function is not exported", fun() ->
            with_static_class('Bt4316NotExported', fun(_Pid) ->
                Key = static_key(class_testNotExported),
                ?assertNot(target('Bt4316NotExported', Key, testNotExported)),
                ?assertNot(key_value(Key))
            end)
        end},
        {"declines when the class process is gone", fun() ->
            Pid = start_static_class('Bt4316Dead', none, #{}),
            stop_static_class(Pid),
            ?assertNot(target('Bt4316Dead', static_key(class_testSelf), testSelf)),
            ?assert(key_gone())
        end},
        {"declines when shadowed or when the class declares class variables", fun() ->
            with_static_class('Bt4316Shadowed', fun(_Pid) ->
                Tag = 'Bt4316Shadowed class',
                ok = beamtalk_class_shadow_flags:set(extension, Tag),
                try
                    ?assertNot(target('Bt4316Shadowed', static_key(class_testSelf), testSelf)),
                    ?assert(key_gone())
                after
                    beamtalk_class_shadow_flags:clear(extension, Tag)
                end
            end),
            Pid = start_static_class('Bt4316Stateful', none, #{count => 0}),
            try
                ?assertNot(target('Bt4316Stateful', static_key(class_testSelf), testSelf)),
                ?assert(key_gone())
            after
                stop_static_class(Pid)
            end
        end},
        {"every path that erases the class's derived flag erases its static call keys", fun() ->
            Name = 'Bt4316Erase',
            Tag = 'Bt4316Erase class',
            with_static_class(Name, fun(Pid) ->
                Writes = [
                    fun() -> beamtalk_class_shadow_flags:set(extension, Tag) end,
                    fun() -> beamtalk_class_shadow_flags:set(runtime_fun, Name) end,
                    fun() -> beamtalk_class_shadow_flags:set_class_vars(Name, true) end,
                    fun() -> beamtalk_class_shadow_flags:invalidate(Name) end
                ],
                lists:foreach(
                    fun(Write) ->
                        _ = warm(Name, Pid),
                        try
                            ok = Write(),
                            ?assert(key_gone()),
                            ?assertEqual([], beamtalk_class_registry:take_static_call_keys(Tag))
                        after
                            beamtalk_class_shadow_flags:clear(extension, Tag),
                            beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                            beamtalk_class_shadow_flags:set_class_vars(Name, false)
                        end
                    end,
                    Writes
                ),
                %% Erased even when the derived key is already gone.
                _ = warm(Name, Pid),
                _ = persistent_term:erase(Tag),
                ok = beamtalk_class_shadow_flags:invalidate(Name),
                ?assert(key_gone())
            end)
        end},
        {"an ancestor's write erases a descendant's static call keys", fun() ->
            Root = start_static_class('Bt4316Root', none, #{}),
            Leaf = start_static_class('Bt4316Leaf', 'Bt4316Root', #{}),
            try
                _ = warm('Bt4316Leaf', Leaf),
                ok = beamtalk_class_shadow_flags:invalidate('Bt4316Root'),
                ?assert(key_gone())
            after
                stop_static_class(Leaf),
                stop_static_class(Root)
            end
        end},
        {"the class monitor's 'DOWN' erases the class's static call keys", fun() ->
            Pid = start_static_class('Bt4316Down', none, #{}),
            _ = warm('Bt4316Down', Pid),
            stop_static_class(Pid),
            wait_until(fun key_gone/0, 500)
        end},
        {"an install racing a shadow raise never survives it", fun() ->
            Name = 'Bt4316Race',
            Tag = 'Bt4316Race class',
            with_static_class(Name, fun(_Pid) ->
                try
                    race_install_against(Name, Tag, fun() ->
                        beamtalk_class_shadow_flags:set(extension, Tag)
                    end)
                after
                    beamtalk_class_shadow_flags:clear(extension, Tag)
                end
            end)
        end},
        {"an install racing a class-variable write and its invalidate never survives it", fun() ->
            Name = 'Bt4316RaceInv',
            Tag = 'Bt4316RaceInv class',
            with_static_class(Name, fun(_Pid) ->
                try
                    race_install_against(Name, Tag, fun() ->
                        beamtalk_class_metadata:put_class_variables(Name, true)
                    end)
                after
                    beamtalk_class_metadata:put_class_variables(Name, false)
                end
            end)
        end}
    ]}.
