%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_direct_call_ok_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1a (BT-4299): `beamtalk_class_shadow_flags:direct_call_ok/2`
gains a "no class variables" input, read from class metadata, and its derived
flag is erased on every class-metadata write for the class: reload, a change to
class variables, a class-side `removeSelector:` and `put_class_method`. One
test per trigger. No dispatch behaviour changes: the open class-side
self-send guard (`shadow_free/2`) still holds for a class with class variables.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_class_dispatch_test_helper).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

start_class(Name, ClassVars, Meta) ->
    ClassInfo = #{
        superclass => none,
        module => ?HELPER,
        class_methods => #{testSelf => #{arity => 0}},
        class_state => ClassVars,
        meta => Meta
    },
    {ok, Pid} = beamtalk_object_class:start_link(Name, ClassInfo),
    unlink(Pid),
    Pid.

stop_class(Pid) ->
    catch gen_server:stop(Pid, normal, 5000),
    ok.

tag(Name) -> beamtalk_class_registry:class_object_tag(Name).

ok(Name) -> beamtalk_class_shadow_flags:direct_call_ok(tag(Name), Name).

%% Install the derived flag (a send does it lazily) and assert it is up.
install(Name) ->
    _ = beamtalk_class_shadow_flags:shadow_free(tag(Name), Name),
    ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))).

direct_call_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"the class_vars raw input makes direct_call_ok false but keeps shadow_free", fun() ->
            Name = 'Bt4299Raw',
            Tag = tag(Name),
            _ = persistent_term:erase(Tag),
            try
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertEqual(true, beamtalk_class_shadow_flags:direct_flag_value(Tag)),
                ok = beamtalk_class_shadow_flags:set_class_vars(Name, true),
                %% The raise erased the derived flag before returning.
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertEqual(class_vars, beamtalk_class_shadow_flags:direct_flag_value(Tag)),
                ?assert(beamtalk_class_shadow_flags:shadow_free(Tag, Name)),
                %% A shadow still wins over the class-variable value.
                ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                ?assertNot(beamtalk_class_shadow_flags:shadow_free(Tag, Name)),
                ok = beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                ok = beamtalk_class_shadow_flags:set_class_vars(Name, false),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name))
            after
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                beamtalk_class_shadow_flags:set_class_vars(Name, false),
                persistent_term:erase(Tag)
            end
        end},
        {"a class's own class-side self-send guard is unchanged by class variables", fun() ->
            Pid = start_class('Bt4299SelfSend', #{count => 0}, #{}),
            try
                Tag = tag('Bt4299SelfSend'),
                ?assert(
                    beamtalk_class_dispatch:class_self_direct_ok(Tag, Tag, 'Bt4299SelfSend', foo)
                ),
                ?assertNot(ok('Bt4299SelfSend'))
            after
                stop_class(Pid)
            end
        end},
        {"init publishes the class-variable fact from classState", fun() ->
            Stateless = start_class('Bt4299Stateless', #{}, #{}),
            Stateful = start_class('Bt4299Stateful', #{count => 0}, #{}),
            try
                ?assertEqual(
                    false, beamtalk_class_metadata:lookup_has_class_variables('Bt4299Stateless')
                ),
                ?assertEqual(
                    true, beamtalk_class_metadata:lookup_has_class_variables('Bt4299Stateful')
                ),
                ?assert(ok('Bt4299Stateless')),
                ?assertNot(ok('Bt4299Stateful'))
            after
                stop_class(Stateless),
                stop_class(Stateful)
            end
        end},
        {"compiled class_field_kinds is the class-variable fact when present", fun() ->
            %% A declared `late` class variable has no default, so the
            %% `classState` defaults are empty; the meta still says it exists.
            Pid = start_class('Bt4299Late', #{}, #{class_field_kinds => #{cache => late}}),
            try
                ?assertEqual(
                    true, beamtalk_class_metadata:lookup_has_class_variables('Bt4299Late')
                ),
                ?assertNot(ok('Bt4299Late'))
            after
                stop_class(Pid)
            end
        end},
        {"trigger: reload erases the derived flag", fun() ->
            Name = 'Bt4299Reload',
            Pid = start_class(Name, #{}, #{}),
            try
                install(Name),
                {ok, _} = beamtalk_object_class:update_class(Name, #{module => ?HELPER, meta => #{}}),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assert(ok(Name))
            after
                stop_class(Pid)
            end
        end},
        {"trigger: a class-variable change on reload erases the flag and flips the answer", fun() ->
                Name = 'Bt4299ClassVarChange',
                Pid = start_class(Name, #{}, #{class_field_kinds => #{}}),
                try
                    install(Name),
                    ?assert(ok(Name)),
                    {ok, _} = beamtalk_object_class:update_class(Name, #{
                        module => ?HELPER, meta => #{class_field_kinds => #{count => eager}}
                    }),
                    ?assertEqual(true, beamtalk_class_metadata:lookup_has_class_variables(Name)),
                    ?assertNot(ok(Name)),
                    {ok, _} = beamtalk_object_class:update_class(Name, #{
                        module => ?HELPER, meta => #{class_field_kinds => #{}}
                    }),
                    ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                    ?assert(ok(Name))
                after
                    stop_class(Pid)
                end
            end},
        {"trigger: put_class_variables erases the flag before publishing the fact", fun() ->
            Name = 'Bt4299PutClassVars',
            Pid = start_class(Name, #{}, #{}),
            try
                install(Name),
                ok = beamtalk_class_metadata:put_class_variables(Name, true),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assertNot(ok(Name)),
                ok = beamtalk_class_metadata:put_class_variables(Name, false),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assert(ok(Name))
            after
                stop_class(Pid)
            end
        end},
        {"trigger: put_class_method erases the derived flag", fun() ->
            Name = 'Bt4299PutClassMethod',
            Pid = start_class(Name, #{}, #{}),
            try
                install(Name),
                ok = beamtalk_object_class:put_class_method(Pid, bt4299_patched, fun(_) -> ok end),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assertNot(ok(Name))
            after
                stop_class(Pid)
            end
        end},
        {"trigger: class-side removeSelector: erases the derived flag", fun() ->
            Name = 'Bt4299RemoveSelector',
            Pid = start_class(Name, #{}, #{}),
            MetaObj = #beamtalk_object{class = 'Metaclass', class_mod = ?HELPER, pid = Pid},
            InstObj = #beamtalk_object{class = tag(Name), class_mod = ?HELPER, pid = Pid},
            try
                install(Name),
                %% Instance side does not touch the class-side flag.
                absent = beamtalk_behaviour_intrinsics:classRemoveSelectorIfAbsent(
                    InstObj, bt4299_absent, fun() -> absent end
                ),
                ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                absent = beamtalk_behaviour_intrinsics:classRemoveSelectorIfAbsent(
                    MetaObj, bt4299_absent, fun() -> absent end
                ),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assert(ok(Name))
            after
                stop_class(Pid)
            end
        end},
        {"row removal drops the class-variable input and the derived flag", fun() ->
            Name = 'Bt4299Delete',
            Pid = start_class(Name, #{count => 0}, #{}),
            install(Name),
            stop_class(Pid),
            ?assertNot(beamtalk_class_shadow_flags:is_set(class_vars, Name)),
            ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name)))
        end}
    ]}.
