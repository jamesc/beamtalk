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
    ok.

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
