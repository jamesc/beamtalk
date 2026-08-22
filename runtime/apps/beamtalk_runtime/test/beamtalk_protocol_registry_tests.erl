%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_protocol_registry_tests).

-moduledoc """
EUnit tests for beamtalk_protocol_registry (ADR 0068 Phase 2c).

Tests cover:
- Protocol registration and lookup
- Conformance checking (structural — class responds to required methods)
- Protocol queries: protocols_for_class, required_methods, conforming_classes
- Edge cases: unknown protocols, extending protocols, duplicate registration
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Setup / Teardown
%%% ============================================================================

-doc "Ensure the protocol registry ETS table exists and is clean.".
setup() ->
    beamtalk_protocol_registry:init(),
    %% Clean any existing protocols
    case ets:info(beamtalk_protocol_registry) of
        undefined -> ok;
        _ -> ets:delete_all_objects(beamtalk_protocol_registry)
    end,
    %% BT-3222: Also clear the conforms_to/2 cache so a result cached by one
    %% test can never leak into the next.
    beamtalk_protocol_registry:invalidate_conforms_cache(),
    ok.

-doc """
`setup/0` plus the scaffolding needed to start real class gen_server
processes (BT-3222 cache tests exercise class registration/hot-reload/
put_method/removal, which `setup/0` alone doesn't need). Mirrors
`beamtalk_object_class_tests:setup/0`.
""".
rt_setup() ->
    setup(),
    case whereis(pg) of
        undefined ->
            {ok, _} = pg:start_link();
        _ ->
            ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_class_registry:ensure_loaded_classes_table(),
    ok.

-doc "Start a class registered under ClassName with a single `ping` method (arity 0).".
-spec start_class_with_ping(atom()) -> {ok, pid()}.
start_class_with_ping(ClassName) ->
    ClassInfo = #{
        name => ClassName,
        module => list_to_atom("bt3222_mod_" ++ atom_to_list(ClassName)),
        instance_methods => #{ping => #{block => fun() -> pong end, arity => 0}}
    },
    beamtalk_object_class:start(ClassName, ClassInfo).

-doc "Kill a class process and wait for it to fully exit (name auto-unregisters).".
-spec stop_class_process(pid()) -> ok.
stop_class_process(Pid) ->
    case is_process_alive(Pid) of
        true ->
            MRef = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end.

%%% ============================================================================
%%% Registration Tests
%%% ============================================================================

init_creates_table_test() ->
    beamtalk_protocol_registry:init(),
    ?assertNotEqual(undefined, ets:info(beamtalk_protocol_registry)).

init_is_idempotent_test() ->
    beamtalk_protocol_registry:init(),
    beamtalk_protocol_registry:init(),
    ?assertNotEqual(undefined, ets:info(beamtalk_protocol_registry)).

register_simple_protocol_test() ->
    setup(),
    Proto = #{
        name => 'Printable',
        required_methods => [#{selector => 'asString', arity => 0}],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    ?assert(beamtalk_protocol_registry:is_protocol('Printable')),
    ?assertEqual(Proto, beamtalk_protocol_registry:protocol_info('Printable')).

register_protocol_with_module_test() ->
    %% BT-2615: the `module` key (the protocol's defining BEAM module) round-trips
    %% through registration so the System Browser can resolve a protocol class
    %% object's origin (the dispatch module beamtalk_protocol_object carries none).
    setup(),
    Proto = #{
        name => 'Printable',
        module => 'bt@stdlib@printable',
        required_methods => [#{selector => 'asString', arity => 0}],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Info = beamtalk_protocol_registry:protocol_info('Printable'),
    ?assertEqual('bt@stdlib@printable', maps:get(module, Info)).

register_protocol_with_type_params_test() ->
    setup(),
    Proto = #{
        name => 'Iterable',
        required_methods => [
            #{selector => 'size', arity => 0},
            #{selector => 'do:', arity => 1}
        ],
        type_params => ['E'],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    ?assert(beamtalk_protocol_registry:is_protocol('Iterable')),
    Info = beamtalk_protocol_registry:protocol_info('Iterable'),
    ?assertEqual(['E'], maps:get(type_params, Info)).

register_extending_protocol_test() ->
    setup(),
    %% Register parent
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Comparable',
        required_methods => [#{selector => '<', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    %% Register child
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Sortable',
        required_methods => [#{selector => 'sortKey', arity => 0}],
        type_params => [],
        extending => 'Comparable'
    }),
    ?assert(beamtalk_protocol_registry:is_protocol('Sortable')).

register_duplicate_overwrites_test() ->
    setup(),
    Proto1 = #{
        name => 'Printable',
        required_methods => [#{selector => 'asString', arity => 0}],
        type_params => [],
        extending => undefined
    },
    Proto2 = #{
        name => 'Printable',
        required_methods => [
            #{selector => 'asString', arity => 0},
            #{selector => 'display', arity => 0}
        ],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto1),
    ok = beamtalk_protocol_registry:register_protocol(Proto2),
    Info = beamtalk_protocol_registry:protocol_info('Printable'),
    ?assertEqual(2, length(maps:get(required_methods, Info))).

register_invalid_protocol_test() ->
    setup(),
    %% Missing 'name' key — should not crash
    ok = beamtalk_protocol_registry:register_protocol(#{foo => bar}).

%%% ============================================================================
%%% Lookup Tests
%%% ============================================================================

is_protocol_unknown_test() ->
    setup(),
    ?assertNot(beamtalk_protocol_registry:is_protocol('Unknown')).

protocol_info_unknown_test() ->
    setup(),
    ?assertEqual(undefined, beamtalk_protocol_registry:protocol_info('Unknown')).

all_protocol_names_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Alpha',
        required_methods => [],
        type_params => [],
        extending => undefined
    }),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Beta',
        required_methods => [],
        type_params => [],
        extending => undefined
    }),
    Names = beamtalk_protocol_registry:all_protocol_names(),
    ?assert(lists:member('Alpha', Names)),
    ?assert(lists:member('Beta', Names)),
    ?assertEqual(2, length(Names)).

%%% ============================================================================
%%% Required Methods Tests
%%% ============================================================================

required_methods_simple_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Printable',
        required_methods => [#{selector => 'asString', arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ?assertEqual(
        ['asString'],
        beamtalk_protocol_registry:required_methods('Printable')
    ).

required_methods_with_extending_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Comparable',
        required_methods => [#{selector => '<', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Sortable',
        required_methods => [#{selector => 'sortKey', arity => 0}],
        type_params => [],
        extending => 'Comparable'
    }),
    Methods = beamtalk_protocol_registry:required_methods('Sortable'),
    ?assert(lists:member('sortKey', Methods)),
    ?assert(lists:member('<', Methods)),
    ?assertEqual(2, length(Methods)).

required_methods_unknown_protocol_test() ->
    setup(),
    ?assertEqual([], beamtalk_protocol_registry:required_methods('Unknown')).

%%% ============================================================================
%%% Conformance Tests (require runtime to be running)
%%% ============================================================================

%% Note: conformance tests that depend on live class processes are tested
%% via the BUnit test file (stdlib/test/ProtocolTest.bt). The tests below
%% verify the protocol registry logic in isolation.

conforms_to_unknown_protocol_test() ->
    setup(),
    %% Unknown protocol — cannot conform to something that isn't a protocol
    ?assertNot(beamtalk_protocol_registry:conforms_to('Integer', 'Unknown')).

conforms_to_nonexistent_protocol_test() ->
    setup(),
    %% A completely made-up protocol name (typo scenario)
    ?assertNot(beamtalk_protocol_registry:conforms_to('Dictionary', 'Printable2')).

conforms_to_class_name_as_protocol_test() ->
    setup(),
    %% A class name passed where a protocol name is expected (e.g. #Integer)
    %% Integer is a class, not a protocol — should return false
    ?assertNot(beamtalk_protocol_registry:conforms_to('Dictionary', 'Integer')).

%%% ============================================================================
%%% Empty Registry Edge Cases
%%% ============================================================================

protocols_for_class_empty_registry_test() ->
    setup(),
    ?assertEqual([], beamtalk_protocol_registry:protocols_for_class('Integer')).

conforming_classes_unknown_protocol_test() ->
    setup(),
    ?assertEqual([], beamtalk_protocol_registry:conforming_classes('Unknown')).

%%% ============================================================================
%%% Class Method Extension Conformance Tests (BT-1617)
%%% ============================================================================

-doc """
Protocol with class method requirement satisfied via class-side extension
should report conformance.
""".
class_method_extension_conforms_test() ->
    setup(),
    %% Ensure extensions ETS table exists
    beamtalk_extensions:init(),
    %% Register a protocol requiring only a class method
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Parseable',
        required_methods => [],
        required_class_methods => [#{selector => 'fromString:', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    %% Register a class-side extension: 'TestExtClass class' >> fromString:
    %% The metaclass tag for TestExtClass is 'TestExtClass class'.
    ExtFun = fun(_Args, _Self) -> ok end,
    beamtalk_extensions:register('TestExtClass class', 'fromString:', ExtFun, test),
    %% Verify conformance — class has no process but extension satisfies class method
    try
        ?assert(beamtalk_protocol_registry:conforms_to('TestExtClass', 'Parseable'))
    after
        ets:delete(beamtalk_extensions, {'TestExtClass class', 'fromString:'})
    end.

-doc """
Protocol with class method requirement NOT satisfied (no extension, no process)
should report non-conformance.
""".
class_method_no_extension_does_not_conform_test() ->
    setup(),
    %% Ensure extensions ETS table exists
    beamtalk_extensions:init(),
    %% Register a protocol requiring only a class method
    beamtalk_protocol_registry:register_protocol(#{
        name => 'Parseable2',
        required_methods => [],
        required_class_methods => [#{selector => 'fromString:', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    %% No extension registered, no class process — should not conform
    ?assertNot(beamtalk_protocol_registry:conforms_to('NoExtClass', 'Parseable2')).

%%% ============================================================================
%%% required_methods with class methods tests (BT-1972)
%%% ============================================================================

required_methods_includes_class_methods_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'WithClassMethods',
        required_methods => [#{selector => 'asString', arity => 0}],
        required_class_methods => [#{selector => 'fromString:', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    Methods = beamtalk_protocol_registry:required_methods('WithClassMethods'),
    ?assert(lists:member('asString', Methods)),
    ?assert(lists:member('class fromString:', Methods)),
    ?assertEqual(2, length(Methods)).

required_methods_class_methods_only_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'ClassOnly',
        required_methods => [],
        required_class_methods => [#{selector => 'create', arity => 0}],
        type_params => [],
        extending => undefined
    }),
    Methods = beamtalk_protocol_registry:required_methods('ClassOnly'),
    ?assertEqual(['class create'], Methods).

%%% ============================================================================
%%% Extending protocol inheritance tests (BT-1972)
%%% ============================================================================

extending_protocol_inherits_class_methods_test() ->
    setup(),
    %% Parent protocol with a class method requirement
    beamtalk_protocol_registry:register_protocol(#{
        name => 'ParentProto',
        required_methods => [#{selector => 'render', arity => 0}],
        required_class_methods => [#{selector => 'create', arity => 0}],
        type_params => [],
        extending => undefined
    }),
    %% Child protocol extends parent, adds own method
    beamtalk_protocol_registry:register_protocol(#{
        name => 'ChildProto',
        required_methods => [#{selector => 'display', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => 'ParentProto'
    }),
    Methods = beamtalk_protocol_registry:required_methods('ChildProto'),
    %% Should include own + inherited instance methods
    ?assert(lists:member('display', Methods)),
    ?assert(lists:member('render', Methods)),
    %% Should include inherited class methods
    ?assert(lists:member('class create', Methods)),
    ?assertEqual(3, length(Methods)).

extending_protocol_own_methods_override_parent_test() ->
    setup(),
    %% Parent protocol
    beamtalk_protocol_registry:register_protocol(#{
        name => 'BaseProto',
        required_methods => [
            #{selector => 'foo', arity => 0},
            #{selector => 'bar', arity => 0}
        ],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    }),
    %% Child overrides 'foo' (same selector, different arity)
    beamtalk_protocol_registry:register_protocol(#{
        name => 'DerivedProto',
        required_methods => [#{selector => 'foo', arity => 1}],
        required_class_methods => [],
        type_params => [],
        extending => 'BaseProto'
    }),
    Methods = beamtalk_protocol_registry:required_methods('DerivedProto'),
    %% 'foo' from child + 'bar' from parent = 2
    ?assert(lists:member('foo', Methods)),
    ?assert(lists:member('bar', Methods)),
    ?assertEqual(2, length(Methods)).

extending_unknown_parent_test() ->
    setup(),
    %% Protocol extending a non-existent parent
    beamtalk_protocol_registry:register_protocol(#{
        name => 'OrphanProto',
        required_methods => [#{selector => 'orphan', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => 'NonExistentParent'
    }),
    Methods = beamtalk_protocol_registry:required_methods('OrphanProto'),
    ?assertEqual(['orphan'], Methods).

%%% ============================================================================
%%% protocol_info / is_protocol before table exists (BT-1972)
%%% ============================================================================

protocol_info_before_init_test() ->
    %% Temporarily destroy the table to test the guard
    case ets:info(beamtalk_protocol_registry) of
        undefined ->
            %% Table doesn't exist — test directly
            ?assertEqual(undefined, beamtalk_protocol_registry:protocol_info('Foo')),
            ?assertNot(beamtalk_protocol_registry:is_protocol('Foo')),
            ?assertEqual([], beamtalk_protocol_registry:all_protocol_names()),
            %% Restore
            beamtalk_protocol_registry:init();
        _ ->
            %% Table exists; we can't safely delete it if owned by another process.
            %% Just verify the functions work when protocol is absent.
            ?assertEqual(undefined, beamtalk_protocol_registry:protocol_info('Nonexistent1972')),
            ?assertNot(beamtalk_protocol_registry:is_protocol('Nonexistent1972'))
    end.

%%% ============================================================================
%%% all_protocol_names empty test (BT-1972)
%%% ============================================================================

all_protocol_names_empty_test() ->
    setup(),
    ?assertEqual([], beamtalk_protocol_registry:all_protocol_names()).

%%% ============================================================================
%%% BT-3105: unregister_protocol/1 — purge on defining-module removal
%%% ============================================================================

unregister_protocol_removes_matching_module_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3105Proto',
        module => 'bt3105_proto_mod',
        required_methods => [#{selector => 'asString', arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ?assert(beamtalk_protocol_registry:is_protocol('BT3105Proto')),

    ok = beamtalk_protocol_registry:unregister_protocol('bt3105_proto_mod'),

    ?assertNot(beamtalk_protocol_registry:is_protocol('BT3105Proto')),
    ?assertEqual(undefined, beamtalk_protocol_registry:protocol_info('BT3105Proto')).

%% Two protocols share nothing but the table; unregistering one module's
%% protocol must not disturb a protocol defined by a different module.
unregister_protocol_leaves_other_modules_protocols_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3105ProtoA',
        module => 'bt3105_mod_a',
        required_methods => [],
        type_params => [],
        extending => undefined
    }),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3105ProtoB',
        module => 'bt3105_mod_b',
        required_methods => [],
        type_params => [],
        extending => undefined
    }),

    ok = beamtalk_protocol_registry:unregister_protocol('bt3105_mod_a'),

    ?assertNot(beamtalk_protocol_registry:is_protocol('BT3105ProtoA')),
    ?assert(beamtalk_protocol_registry:is_protocol('BT3105ProtoB')).

%% A protocol registered without a `module` field (pre-BT-2615 shape) is
%% never matched — unregistering by module name is a harmless no-op.
unregister_protocol_skips_protocol_without_module_field_test() ->
    setup(),
    beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3105NoModule',
        required_methods => [],
        type_params => [],
        extending => undefined
    }),

    ok = beamtalk_protocol_registry:unregister_protocol('bt3105_unrelated_mod'),

    ?assert(beamtalk_protocol_registry:is_protocol('BT3105NoModule')).

unregister_protocol_unknown_module_is_noop_test() ->
    setup(),
    ?assertEqual(ok, beamtalk_protocol_registry:unregister_protocol('bt3105_never_registered')).

unregister_protocol_before_init_test() ->
    case ets:info(beamtalk_protocol_registry) of
        undefined ->
            ?assertEqual(ok, beamtalk_protocol_registry:unregister_protocol('bt3105_any_mod')),
            beamtalk_protocol_registry:init();
        _ ->
            ?assertEqual(ok, beamtalk_protocol_registry:unregister_protocol('bt3105_any_mod'))
    end.

%%% ============================================================================
%%% BT-3222: conforms_to/2 result cache + invalidation
%%% ============================================================================

%% ADR 0112 note: classRemoveSelector/2's local-method-removal branch (the
%% only branch that can change conforms_to/2's answer — extension removal
%% does not, since classCanUnderstandFromName/2 never consults the extension
%% registry) recompiles the class and hot-reloads it via
%% beamtalk_repl_eval:remove_method/4 -> ... -> beamtalk_object_class's own
%% {update_class, _} handler (see that module's `remove_method/4` doc). It is
%% therefore covered by conforms_to_invalidated_by_update_class_test/0 below
%% rather than duplicated with the heavier beamtalk_workspace live-source
%% scaffolding beamtalk_workspace_revert_tests.erl needs to drive the
%% primitive end to end.

-doc """
Base case: a repeated `{ClassName, ProtocolName}` pair returns the cached
result instead of re-walking the hierarchy. Proven by killing the class
process between the two calls — a live recompute would hit `noproc` and flip
to `false` (mirroring `compute_conforms_to/2`'s own `catch -> false`), so the
second call only reads `true` if it came from the cache, not a fresh walk.
""".
conforms_to_caches_result_across_process_death_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222PingProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    {ok, Pid} = start_class_with_ping('BT3222CacheHitClass'),
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222CacheHitClass', 'BT3222PingProto')),
    stop_class_process(Pid),
    %% The class process is gone; a fresh walk would fail — the cache must
    %% still answer `true`.
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222CacheHitClass', 'BT3222PingProto')).

-doc "register_protocol/1 must invalidate a stale cached `false`.".
conforms_to_invalidated_by_protocol_registration_test() ->
    rt_setup(),
    {ok, Pid} = start_class_with_ping('BT3222ProtoRegClass'),
    %% Protocol not registered yet — cannot conform (and this gets cached).
    ?assertNot(
        beamtalk_protocol_registry:conforms_to('BT3222ProtoRegClass', 'BT3222LatePingProto')
    ),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222LatePingProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ?assert(
        beamtalk_protocol_registry:conforms_to('BT3222ProtoRegClass', 'BT3222LatePingProto')
    ),
    stop_class_process(Pid).

-doc "unregister_protocol/1 must invalidate a stale cached `true`.".
conforms_to_invalidated_by_unregister_protocol_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222UnregProto',
        module => bt3222_unreg_proto_mod,
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    {ok, Pid} = start_class_with_ping('BT3222UnregClass'),
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222UnregClass', 'BT3222UnregProto')),
    ok = beamtalk_protocol_registry:unregister_protocol(bt3222_unreg_proto_mod),
    ?assertNot(beamtalk_protocol_registry:conforms_to('BT3222UnregClass', 'BT3222UnregProto')),
    stop_class_process(Pid).

-doc """
Class registration (`beamtalk_object_class:init/1`) must invalidate a stale
cached `false` recorded before the class existed.
""".
conforms_to_invalidated_by_class_registration_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222RegLaterProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    %% Class doesn't exist yet.
    ?assertNot(
        beamtalk_protocol_registry:conforms_to('BT3222RegLaterClass', 'BT3222RegLaterProto')
    ),
    {ok, Pid} = start_class_with_ping('BT3222RegLaterClass'),
    ?assert(
        beamtalk_protocol_registry:conforms_to('BT3222RegLaterClass', 'BT3222RegLaterProto')
    ),
    stop_class_process(Pid).

-doc """
Class re-registration / hot reload (`{update_class, _}`) must invalidate a
stale cached result — this is also the mechanism
`classRemoveSelector:`/`removeSelector:` uses to remove a *local* method (see
this section's note above).
""".
conforms_to_invalidated_by_update_class_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222HotReloadProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ClassInfo = #{
        name => 'BT3222HotReloadClass',
        module => bt3222_hot_reload_mod,
        instance_methods => #{}
    },
    {ok, Pid} = beamtalk_object_class:start('BT3222HotReloadClass', ClassInfo),
    ?assertNot(
        beamtalk_protocol_registry:conforms_to('BT3222HotReloadClass', 'BT3222HotReloadProto')
    ),
    NewInfo = ClassInfo#{instance_methods => #{ping => #{block => fun() -> pong end}}},
    {ok, _Fields} = beamtalk_object_class:update_class('BT3222HotReloadClass', NewInfo),
    ?assert(
        beamtalk_protocol_registry:conforms_to('BT3222HotReloadClass', 'BT3222HotReloadProto')
    ),
    stop_class_process(Pid).

-doc "put_method/4 (instance-side hot patch) must invalidate a stale cached `false`.".
conforms_to_invalidated_by_put_method_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222PutMethodProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ClassInfo = #{
        name => 'BT3222PutMethodClass',
        module => bt3222_put_method_mod,
        instance_methods => #{}
    },
    {ok, Pid} = beamtalk_object_class:start('BT3222PutMethodClass', ClassInfo),
    ?assertNot(
        beamtalk_protocol_registry:conforms_to('BT3222PutMethodClass', 'BT3222PutMethodProto')
    ),
    ok = beamtalk_object_class:put_method(Pid, ping, fun() -> pong end, <<"ping => pong">>),
    ?assert(
        beamtalk_protocol_registry:conforms_to('BT3222PutMethodClass', 'BT3222PutMethodProto')
    ),
    stop_class_process(Pid).

-doc "put_class_method/4 (class-side hot patch) must invalidate a stale cached `false`.".
conforms_to_invalidated_by_put_class_method_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222PutClassMethodProto',
        required_methods => [],
        required_class_methods => [#{selector => make, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    ClassInfo = #{
        name => 'BT3222PutClassMethodClass',
        module => bt3222_put_class_method_mod,
        instance_methods => #{}
    },
    {ok, Pid} = beamtalk_object_class:start('BT3222PutClassMethodClass', ClassInfo),
    ?assertNot(
        beamtalk_protocol_registry:conforms_to(
            'BT3222PutClassMethodClass', 'BT3222PutClassMethodProto'
        )
    ),
    ok = beamtalk_object_class:put_class_method(Pid, make, fun() -> ok end, <<"make => ok">>),
    ?assert(
        beamtalk_protocol_registry:conforms_to(
            'BT3222PutClassMethodClass', 'BT3222PutClassMethodProto'
        )
    ),
    stop_class_process(Pid).

-doc """
Class removal (`beamtalk_class_lifecycle:class_removed/2`, the single
teardown path `classRemoveFromSystemByName/1` drives) must invalidate a stale
cached `true` for the removed class — proven the same way as the base-case
test above: the cache still answers `true` immediately after the process
dies (nothing has flushed it yet), and only flips to `false` once
`class_removed/2` runs.
""".
conforms_to_invalidated_by_class_removal_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222RemovalProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    {ok, Pid} = start_class_with_ping('BT3222RemovalClass'),
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222RemovalClass', 'BT3222RemovalProto')),
    stop_class_process(Pid),
    %% Not yet flushed — still cached `true`.
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222RemovalClass', 'BT3222RemovalProto')),
    ok = beamtalk_class_lifecycle:class_removed('BT3222RemovalClass', bt3222_removal_class_mod),
    ?assertNot(beamtalk_protocol_registry:conforms_to('BT3222RemovalClass', 'BT3222RemovalProto')).

-doc """
Regression test for the lost-invalidation race flagged in review (BT-3222,
round 2): a `compute_conforms_to/2` result that finishes *after* a
concurrent `invalidate_conforms_cache/0` bump must never be treated as a
cache hit.

Simulated directly on the cache's public ETS table rather than with real
concurrency (this repo has no mocking library, and reliably interleaving a
real millisecond-scale `gen_server:call` walk against a bump within one
EUnit test is impractical): it inserts an entry stamped with a generation
*older* than the table's current counter — exactly what a race-losing
`cache_store/3` call would have written had it lost the race — and asserts
`conforms_to/2` recomputes instead of returning it.
""".
conforms_to_ignores_entry_stamped_with_stale_generation_test() ->
    rt_setup(),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => 'BT3222RaceProto',
        required_methods => [#{selector => ping, arity => 0}],
        type_params => [],
        extending => undefined
    }),
    {ok, Pid} = start_class_with_ping('BT3222RaceClass'),
    %% Prime the cache table + generation counter — any conforms_to/2 call
    %% does this as a side effect; the result itself doesn't matter here.
    _ = beamtalk_protocol_registry:conforms_to('BT3222RaceClass', 'BT3222RaceProto'),
    %% Bump the generation past whatever that primed entry was stamped with —
    %% mirrors a mutation's invalidate_conforms_cache/0 firing while a
    %% concurrent compute was still in flight.
    ok = beamtalk_protocol_registry:invalidate_conforms_cache(),
    CurrentGen = ets:lookup_element(
        beamtalk_protocol_conforms_cache, '$conforms_cache_generation', 2
    ),
    %% Simulate a compute that started *before* the bump above landing
    %% *after* it: a wrong (`false`) answer stamped with the now-stale
    %% pre-bump generation, written directly to bypass conforms_to/2's own
    %% (correct) generation sampling.
    StaleGen = CurrentGen - 1,
    true = ets:insert(
        beamtalk_protocol_conforms_cache,
        {{'BT3222RaceClass', 'BT3222RaceProto'}, {false, StaleGen}}
    ),
    %% A stale-generation entry must never be returned — conforms_to/2 must
    %% recompute and see the true, current answer.
    ?assert(beamtalk_protocol_registry:conforms_to('BT3222RaceClass', 'BT3222RaceProto')),
    stop_class_process(Pid).

-doc "invalidate_conforms_cache/0 is a no-op (never raises) when the cache table doesn't exist.".
invalidate_conforms_cache_before_init_test() ->
    case ets:info(beamtalk_protocol_conforms_cache) of
        undefined ->
            ?assertEqual(ok, beamtalk_protocol_registry:invalidate_conforms_cache()),
            beamtalk_protocol_registry:init();
        _ ->
            ?assertEqual(ok, beamtalk_protocol_registry:invalidate_conforms_cache())
    end.
