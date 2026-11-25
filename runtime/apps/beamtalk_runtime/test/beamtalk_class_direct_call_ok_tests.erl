%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_direct_call_ok_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 Phase 1a (BT-4299), ADR 0153 Phase 3a (BT-5158):
`beamtalk_class_shadow_flags:direct_call_ok/2` holds exactly while the class
is ready and not shadowed by an ADR 0084 runtime fun. Class variables, and the
class's superclass chain, are no input. Its derived flag is erased on every
class-metadata write for the class: reload, a class-side `removeSelector:`
and `put_class_method`. One test per trigger.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_class_dispatch_test_helper).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

class_info(ClassVars, Meta) ->
    #{
        superclass => none,
        module => ?HELPER,
        class_methods => #{testSelf => #{arity => 0}},
        class_state => ClassVars,
        meta => Meta
    }.

tag(Name) -> beamtalk_class_registry:class_object_tag(Name).

ok(Name) -> beamtalk_class_shadow_flags:direct_call_ok(tag(Name), Name).

%% Install the derived flag (a send does it lazily) and assert it is up.
install(Name) ->
    _ = beamtalk_class_shadow_flags:shadow_free(tag(Name), Name),
    ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))).

direct_call_ok_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"class-variable defaults and metadata are no input (ADR 0153)", fun() ->
            Stateful = beamtalk_test_class:start('Bt5158Stateful', class_info(#{count => 0}, #{})),
            Late = beamtalk_test_class:start(
                'Bt5158Late', class_info(#{}, #{class_field_kinds => #{cache => late}})
            ),
            try
                ?assert(ok('Bt5158Stateful')),
                ?assert(ok('Bt5158Late')),
                Tag = tag('Bt5158Stateful'),
                ?assert(beamtalk_class_shadow_flags:shadow_free(Tag, 'Bt5158Stateful')),
                ?assert(
                    beamtalk_class_dispatch:class_self_direct_ok(Tag, Tag, 'Bt5158Stateful', foo)
                )
            after
                beamtalk_test_proc:stop(Stateful),
                beamtalk_test_proc:stop(Late)
            end
        end},
        {"an unresolvable superclass chain is no input (BT-5154 window closed)", fun() ->
            %% Before ADR 0153 the derived flag failed closed on a chain whose
            %% facts were unpublished; now only readiness and shadows count.
            Name = 'Bt5158NoChain',
            Tag = tag(Name),
            beamtalk_class_metadata:new(),
            ok = beamtalk_class_metadata:insert(Name, undefined, [], 'Bt5158Missing', false),
            _ = persistent_term:erase(Tag),
            try
                ?assert(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertEqual(true, beamtalk_class_shadow_flags:direct_flag_value(Tag)),
                %% A shadow still bars it, and erases the derived flag first.
                ok = beamtalk_class_shadow_flags:set(runtime_fun, Name),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(Tag)),
                ?assertNot(beamtalk_class_shadow_flags:direct_call_ok(Tag, Name)),
                ?assertNot(beamtalk_class_shadow_flags:shadow_free(Tag, Name))
            after
                beamtalk_class_shadow_flags:clear(runtime_fun, Name),
                beamtalk_class_metadata:delete(Name),
                persistent_term:erase(Tag)
            end
        end},
        {"trigger: reload erases the derived flag", fun() ->
            Name = 'Bt4299Reload',
            Pid = beamtalk_test_class:start(Name, class_info(#{}, #{})),
            try
                install(Name),
                {ok, _} = beamtalk_object_class:update_class(Name, #{module => ?HELPER, meta => #{}}),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assert(ok(Name))
            after
                beamtalk_test_proc:stop(Pid)
            end
        end},
        {"trigger: put_class_method erases the derived flag", fun() ->
            Name = 'Bt4299PutClassMethod',
            Pid = beamtalk_test_class:start(Name, class_info(#{}, #{})),
            try
                install(Name),
                ok = beamtalk_object_class:put_class_method(Pid, bt4299_patched, fun(_) -> ok end),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assertNot(ok(Name))
            after
                beamtalk_test_proc:stop(Pid)
            end
        end},
        {"trigger: class-side removeSelector: erases the derived flag", fun() ->
            Name = 'Bt4299RemoveSelector',
            Pid = beamtalk_test_class:start(Name, class_info(#{}, #{})),
            MetaObj = #beamtalk_object{class = 'Metaclass', class_mod = ?HELPER, pid = Pid},
            InstObj = #beamtalk_object{class = tag(Name), class_mod = ?HELPER, pid = Pid},
            try
                install(Name),
                %% Instance side does not touch the class-side flag.
                absent = beamtalk_class_refactor:classRemoveSelectorIfAbsent(
                    InstObj, bt4299_absent, fun() -> absent end
                ),
                ?assert(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                absent = beamtalk_class_refactor:classRemoveSelectorIfAbsent(
                    MetaObj, bt4299_absent, fun() -> absent end
                ),
                ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name))),
                ?assert(ok(Name))
            after
                beamtalk_test_proc:stop(Pid)
            end
        end},
        {"row removal drops the derived flag", fun() ->
            Name = 'Bt4299Delete',
            Pid = beamtalk_test_class:start(Name, class_info(#{}, #{})),
            install(Name),
            beamtalk_test_proc:stop(Pid),
            ?assertNot(beamtalk_class_shadow_flags:is_direct_flag_set(tag(Name)))
        end}
    ]}.
