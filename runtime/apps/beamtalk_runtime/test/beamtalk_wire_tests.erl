%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_wire_tests).

-moduledoc """
EUnit tests for beamtalk_wire (ADR 0126 Phase 3a, BT-3600).

Unit tests of `encode/1`/`decode/1` in isolation — no actor dispatch, no
two-node `peer` harness (that integration is BT-3601's job). Reuses the
BT-3536/BT-3542 shape-migration fixtures (`ShapePoint`, `ShapePlainObject`,
`ShapeHandleBox`, `ShapeHazardWorker`, `ShapeHazardCart`) already compiled
into `test_fixtures/` — the sendability-tier conformance corpus's own
example instances — rather than adding new ones, plus the real `Ets` stdlib
class for the `HandleScoped(#node)` row (exercising the
`handleScope: #node` declaration this issue adds).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Fixture
%%====================================================================

setup() ->
    application:ensure_all_started(beamtalk_runtime),
    beamtalk_stdlib:init(),
    ok = ensure_fixture_loaded(shape_point, 'ShapePoint'),
    ok = ensure_fixture_loaded(shape_plain_object, 'ShapePlainObject'),
    ok = ensure_fixture_loaded(shape_handle_box, 'ShapeHandleBox'),
    ok = ensure_fixture_loaded(shape_hazard_worker, 'ShapeHazardWorker'),
    ok = ensure_fixture_loaded(shape_hazard_cart, 'ShapeHazardCart'),
    ok.

teardown(_) ->
    ok.

%% Load a compiled test_fixtures/*.bt module and register its class if not
%% already registered — same convention as
%% beamtalk_shape_migration_tests:ensure_fixture_loaded/2 (each fixture-
%% consuming test file owns this helper locally, deliberately, so no test
%% file has an ordering dependency on another test file registering a
%% shared fixture first).
ensure_fixture_loaded(Basename, ClassName) ->
    Module = list_to_atom("bt@" ++ atom_to_list(Basename)),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    case erlang:function_exported(Module, register_class, 0) of
                        true ->
                            Module:register_class(),
                            ok;
                        false ->
                            ok
                    end;
                _Pid ->
                    ok
            end;
        {error, Reason} ->
            error({fixture_module_not_found, Basename, Reason})
    end.

beamtalk_wire_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a Value instance is enveloped on encode and unpacked on decode",
                fun test_value_instance_round_trip/0},
            {"a builtin collection (Array) containing Values envelopes each element",
                fun test_builtin_collection_with_values/0},
            {"a Dictionary (untagged map) walks both keys and values",
                fun test_dictionary_walks_keys_and_values/0},
            {"HandleScoped(#process) is rejected at encode, naming the class",
                fun test_rejects_handle_scoped_process/0},
            {"HandleScoped(#node) (Ets, after handleScope: #node) is rejected at encode",
                fun test_rejects_handle_scoped_node_ets/0},
            {"an Unknown-tier (bare Object) instance passes as a raw, unwalked term",
                fun test_unknown_tier_raw_term/0},
            {"an actor ref (#beamtalk_object{}) is kept unchanged", fun test_actor_ref_kept/0},
            {"a local {registered, Name} ref is rewritten to {registered, Name, node()}",
                fun test_registered_ref_rewrite/0},
            {"a class object is rewritten to a by-name ref and resolves back on decode",
                fun test_class_object_rewrite_round_trip/0},
            {"an NLR tuple walks its Value slot but passes State raw, unrejected",
                fun test_nlr_tuple_walks_value_passes_state_raw/0},
            {"a legacy 3-tuple NLR walks its Value slot", fun test_nlr_legacy_3tuple/0},
            {"a bare fun and a bare pid pass through unchanged", fun test_bare_fun_and_pid/0},
            {"a #beamtalk_error{} passes through unchanged, unwalked",
                fun test_beamtalk_error_raw_passthrough/0},
            {"decode refuses an envelope ahead of this node's known shape version",
                fun test_decode_rejects_shape_version_ahead/0},
            {"pack_wire/1 allows a SendableRef field pack/1 rejects",
                fun test_pack_wire_allows_sendable_ref_field/0},
            {"pack_wire/1 node-qualifies a registered ref inside a SendableRef field",
                fun test_pack_wire_qualifies_registered_ref_in_sendable_ref_field/0}
        ]
    end}.

%%====================================================================
%% Value instance -> envelope
%%====================================================================

test_value_instance_round_trip() ->
    Instance = #{'$beamtalk_class' => 'ShapePoint', x => 3, y => 4},
    {ok, Encoded} = beamtalk_wire:encode(Instance),
    ?assertEqual({beamtalk_shape, 'ShapePoint', 1, #{x => 3, y => 4}}, Encoded),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(
        #{'$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 3, y => 4}, Decoded
    ).

%%====================================================================
%% Builtin collections — contents walked
%%====================================================================

test_builtin_collection_with_values() ->
    Point = #{'$beamtalk_class' => 'ShapePoint', x => 1, y => 2},
    Array = beamtalk_array:from_list([Point]),

    {ok, EncArray} = beamtalk_wire:encode(Array),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Array',
            data := #{0 := {beamtalk_shape, 'ShapePoint', 1, #{x := 1, y := 2}}}
        },
        EncArray
    ),

    {ok, DecArray} = beamtalk_wire:decode(EncArray),
    ?assertEqual(
        #{
            '$beamtalk_class' => 'Array',
            data => #{
                0 => #{'$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 1, y => 2}
            }
        },
        DecArray
    ).

test_dictionary_walks_keys_and_values() ->
    Point = #{'$beamtalk_class' => 'ShapePoint', x => 5, y => 6},
    Dict = #{origin => Point, label => <<"hi">>},

    {ok, EncDict} = beamtalk_wire:encode(Dict),
    ?assertEqual(
        #{
            origin => {beamtalk_shape, 'ShapePoint', 1, #{x => 5, y => 6}},
            label => <<"hi">>
        },
        EncDict
    ),

    {ok, DecDict} = beamtalk_wire:decode(EncDict),
    ?assertEqual(
        #{
            origin => #{
                '$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 5, y => 6
            },
            label => <<"hi">>
        },
        DecDict
    ).

%%====================================================================
%% HandleScoped rejection
%%====================================================================

test_rejects_handle_scoped_process() ->
    Instance = #{'$beamtalk_class' => 'ShapeHandleBox'},
    {error, Reason} = beamtalk_wire:encode(Instance),
    ?assertMatch(#beamtalk_error{kind = not_serialisable, class = 'ShapeHandleBox'}, Reason).

test_rejects_handle_scoped_node_ets() ->
    %% A real Ets instance — the actual runtime shape (table => Name), not a
    %% synthetic fixture — exercising ets.bt's new handleScope: #node.
    Instance = beamtalk_ets:new(bt_3600_wire_test_table, set),
    {error, Reason} = beamtalk_wire:encode(Instance),
    ?assertMatch(#beamtalk_error{kind = not_serialisable, class = 'Ets'}, Reason),
    beamtalk_ets:deleteTable(Instance).

%%====================================================================
%% Unknown-tier Object — raw, unwalked
%%====================================================================

test_unknown_tier_raw_term() ->
    Instance = #{'$beamtalk_class' => 'ShapePlainObject', anything => make_ref()},
    {ok, Encoded} = beamtalk_wire:encode(Instance),
    ?assertEqual(Instance, Encoded),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(Instance, Decoded).

%%====================================================================
%% Actor refs
%%====================================================================

test_actor_ref_kept() ->
    Worker = 'bt@shape_hazard_worker':spawn(),
    ?assertMatch(#beamtalk_object{}, Worker),

    {ok, Encoded} = beamtalk_wire:encode(Worker),
    ?assertEqual(Worker, Encoded),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(Worker, Decoded).

test_registered_ref_rewrite() ->
    Worker0 = 'bt@shape_hazard_worker':spawn(),
    Worker = Worker0#beamtalk_object{pid = {registered, bt_3600_wire_test_worker}},

    {ok, Encoded} = beamtalk_wire:encode(Worker),
    ThisNode = node(),
    ?assertMatch(
        #beamtalk_object{pid = {registered, bt_3600_wire_test_worker, ThisNode}}, Encoded
    ),

    %% decode keeps an actor ref unchanged (no reverse rewrite needed).
    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(Encoded, Decoded).

%%====================================================================
%% Class object rewrite
%%====================================================================

test_class_object_rewrite_round_trip() ->
    %% beamtalk_class_registry:resolve_class_object/1 is the promoted
    %% resolve helper (ADR 0126 Phase 0.5 finding (e)) — used here directly
    %% since beamtalk_behaviour_intrinsics:atom_to_class_object/1, the
    %% pre-existing equivalent, is not exported (the finding's own gap).
    ClassObj = beamtalk_class_registry:resolve_class_object('ShapeHazardWorker'),
    ?assertMatch(#beamtalk_object{}, ClassObj),

    {ok, Encoded} = beamtalk_wire:encode(ClassObj),
    ?assertEqual({'$beamtalk_class_ref', 'ShapeHazardWorker'}, Encoded),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(ClassObj, Decoded).

%%====================================================================
%% NLR relay
%%====================================================================

test_nlr_tuple_walks_value_passes_state_raw() ->
    Point = #{'$beamtalk_class' => 'ShapePoint', x => 7, y => 8},
    Token = make_ref(),
    %% State stands in for the defining method's actor state — a
    %% HandleScoped instance here, deliberately, to prove it is passed raw
    %% (unwalked, unrejected) rather than sharing the Value slot's rules.
    State = #{'$beamtalk_class' => 'ShapeHandleBox'},
    Nlr = {'$bt_nlr', Token, Point, State},

    {ok, Encoded} = beamtalk_wire:encode(Nlr),
    ?assertEqual(
        {'$bt_nlr', Token, {beamtalk_shape, 'ShapePoint', 1, #{x => 7, y => 8}}, State}, Encoded
    ),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(
        {'$bt_nlr', Token,
            #{'$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 7, y => 8}, State},
        Decoded
    ).

test_nlr_legacy_3tuple() ->
    Point = #{'$beamtalk_class' => 'ShapePoint', x => 9, y => 10},
    Token = make_ref(),
    Nlr = {'$bt_nlr', Token, Point},

    {ok, Encoded} = beamtalk_wire:encode(Nlr),
    ?assertEqual(
        {'$bt_nlr', Token, {beamtalk_shape, 'ShapePoint', 1, #{x => 9, y => 10}}}, Encoded
    ),

    {ok, Decoded} = beamtalk_wire:decode(Encoded),
    ?assertEqual(
        {'$bt_nlr', Token, #{
            '$beamtalk_class' => 'ShapePoint', '__shape_version__' => 1, x => 9, y => 10
        }},
        Decoded
    ).

%%====================================================================
%% Bare raw terms
%%====================================================================

test_bare_fun_and_pid() ->
    F = fun() -> ok end,
    ?assertEqual({ok, F}, beamtalk_wire:encode(F)),
    ?assertEqual({ok, F}, beamtalk_wire:decode(F)),

    Pid = self(),
    ?assertEqual({ok, Pid}, beamtalk_wire:encode(Pid)),
    ?assertEqual({ok, Pid}, beamtalk_wire:decode(Pid)).

test_beamtalk_error_raw_passthrough() ->
    Err = beamtalk_error:new(runtime_error, 'Object'),
    ?assertEqual({ok, Err}, beamtalk_wire:encode(Err)),
    ?assertEqual({ok, Err}, beamtalk_wire:decode(Err)).

%%====================================================================
%% decode/1 -> unpack_strict/1 version-skew refusal (ADR 0125 §3.4)
%%====================================================================

test_decode_rejects_shape_version_ahead() ->
    %% ShapePoint's declared shapeVersion is 1 (Phase 3 shapeVersion:
    %% parsing has not shipped, so every class resolves to 1 today) — an
    %% envelope claiming version 2 is ahead of what this node knows.
    Envelope = {beamtalk_shape, 'ShapePoint', 2, #{x => 1, y => 2}},
    {error, Reason} = beamtalk_wire:decode(Envelope),
    ?assertMatch(
        #beamtalk_error{kind = shape_version_ahead, class = 'ShapePoint'}, Reason
    ).

%%====================================================================
%% pack_wire/1 — SendableRef passes under wire, rejected under persist
%%====================================================================

test_pack_wire_allows_sendable_ref_field() ->
    Instance = #{
        '$beamtalk_class' => 'ShapeHazardCart',
        '__shape_version__' => 1,
        worker => nil,
        label => <<"cart">>
    },
    {ok, Envelope} = beamtalk_shape_migration:pack_wire(Instance),
    ?assertEqual(
        {beamtalk_shape, 'ShapeHazardCart', 1, #{worker => nil, label => <<"cart">>}}, Envelope
    ),

    {error, Reason} = beamtalk_shape_migration:pack(Instance),
    ?assertMatch(
        #beamtalk_error{kind = not_serialisable, class = 'ShapeHazardCart', selector = worker},
        Reason
    ).

test_pack_wire_qualifies_registered_ref_in_sendable_ref_field() ->
    %% A locally-registered actor ref (ADR 0079) reached through a
    %% sendable_ref-tier field must be node-qualified exactly like an actor
    %% ref the generic wire walk meets directly (ADR 0126 §3/§5.1) —
    %% otherwise it resolves via a local whereis/1 on the receiver instead
    %% of the sender's registry.
    Worker = #beamtalk_object{
        class = 'ShapeHazardWorker', class_mod = shape_hazard_worker, pid = {registered, worker1}
    },
    Instance = #{
        '$beamtalk_class' => 'ShapeHazardCart',
        '__shape_version__' => 1,
        worker => Worker,
        label => <<"cart">>
    },
    {ok, Envelope} = beamtalk_shape_migration:pack_wire(Instance),
    {beamtalk_shape, 'ShapeHazardCart', 1, #{worker := PackedWorker}} = Envelope,
    ?assertEqual({registered, worker1, node()}, PackedWorker#beamtalk_object.pid).
