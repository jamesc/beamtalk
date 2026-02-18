%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% **DDD Context:** Runtime — Class System

%%% @doc EUnit tests for beamtalk_class_registry (BT-708).
%%%
%%% Tests class lookup, hierarchy queries, process group management,
%%% and class object identity functions.

-module(beamtalk_class_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% registry_name tests
%%% ============================================================================

registry_name_test() ->
    ?assertEqual(beamtalk_class_Counter,
                 beamtalk_class_registry:registry_name('Counter')).

registry_name_object_test() ->
    ?assertEqual(beamtalk_class_Object,
                 beamtalk_class_registry:registry_name('Object')).

%%% ============================================================================
%%% class_object_tag tests
%%% ============================================================================

class_object_tag_test() ->
    ?assertEqual('Point class', beamtalk_class_registry:class_object_tag('Point')).

class_object_tag_object_test() ->
    ?assertEqual('Object class', beamtalk_class_registry:class_object_tag('Object')).

%%% ============================================================================
%%% is_class_name tests
%%% ============================================================================

is_class_name_true_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Integer class')).

is_class_name_false_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('Integer')).

is_class_name_short_atom_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name('ab')).

is_class_name_non_atom_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_name(<<"Integer class">>)).

is_class_name_object_class_test() ->
    ?assertEqual(true, beamtalk_class_registry:is_class_name('Object class')).

%%% ============================================================================
%%% is_class_object tests
%%% ============================================================================

is_class_object_true_test() ->
    Obj = {beamtalk_object, 'Counter class', counter, self()},
    ?assertEqual(true, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_false_instance_test() ->
    Obj = {beamtalk_object, 'Counter', counter, self()},
    ?assertEqual(false, beamtalk_class_registry:is_class_object(Obj)).

is_class_object_non_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object(42)).

is_class_object_wrong_tuple_test() ->
    ?assertEqual(false, beamtalk_class_registry:is_class_object({wrong, tuple})).

%%% ============================================================================
%%% class_display_name tests
%%% ============================================================================

class_display_name_strips_suffix_test() ->
    ?assertEqual(<<"Counter">>,
                 beamtalk_class_registry:class_display_name('Counter class')).

class_display_name_no_suffix_test() ->
    ?assertEqual(<<"Counter">>,
                 beamtalk_class_registry:class_display_name('Counter')).

class_display_name_object_test() ->
    ?assertEqual(<<"Object">>,
                 beamtalk_class_registry:class_display_name('Object class')).

%%% ============================================================================
%%% ensure_pg_started tests
%%% ============================================================================

ensure_pg_started_test() ->
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()),
    %% Calling again should also succeed (idempotent)
    ?assertEqual(ok, beamtalk_class_registry:ensure_pg_started()).

%%% ============================================================================
%%% ensure_hierarchy_table tests
%%% ============================================================================

ensure_hierarchy_table_test_() ->
    {setup,
     fun() ->
         %% Clean up any existing table
         case ets:info(beamtalk_class_hierarchy) of
             undefined -> ok;
             _ -> ets:delete(beamtalk_class_hierarchy)
         end
     end,
     fun(_) ->
         case ets:info(beamtalk_class_hierarchy) of
             undefined -> ok;
             _ -> ets:delete(beamtalk_class_hierarchy)
         end
     end,
     fun() ->
         ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table()),
         ?assertNotEqual(undefined, ets:info(beamtalk_class_hierarchy)),
         %% Idempotent
         ?assertEqual(ok, beamtalk_class_registry:ensure_hierarchy_table())
     end}.

%%% ============================================================================
%%% inherits_from tests
%%% ============================================================================

inherits_from_test_() ->
    {setup,
     fun() ->
         beamtalk_class_registry:ensure_hierarchy_table(),
         ets:insert(beamtalk_class_hierarchy, {'Object', none}),
         ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
         ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'})
     end,
     fun(_) ->
         ets:delete(beamtalk_class_hierarchy)
     end,
     [
      {"same class",
       fun() ->
           ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Counter'))
       end},
      {"direct parent",
       fun() ->
           ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Actor'))
       end},
      {"grandparent",
       fun() ->
           ?assertEqual(true, beamtalk_class_registry:inherits_from('Counter', 'Object'))
       end},
      {"not related",
       fun() ->
           ?assertEqual(false, beamtalk_class_registry:inherits_from('Object', 'Counter'))
       end},
      {"none ancestor",
       fun() ->
           ?assertEqual(false, beamtalk_class_registry:inherits_from(none, 'Object'))
       end},
      {"unknown class",
       fun() ->
           ?assertEqual(false, beamtalk_class_registry:inherits_from('Unknown', 'Object'))
       end}
     ]}.

%%% ============================================================================
%%% direct_subclasses tests
%%% ============================================================================

direct_subclasses_test_() ->
    {setup,
     fun() ->
         beamtalk_class_registry:ensure_hierarchy_table(),
         ets:insert(beamtalk_class_hierarchy, {'Object', none}),
         ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
         ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'}),
         ets:insert(beamtalk_class_hierarchy, {'Timer', 'Actor'})
     end,
     fun(_) ->
         ets:delete(beamtalk_class_hierarchy)
     end,
     [
      {"direct children of Actor",
       fun() ->
           Result = beamtalk_class_registry:direct_subclasses('Actor'),
           ?assertEqual(['Counter', 'Timer'], Result)
       end},
      {"direct children of Object",
       fun() ->
           ?assertEqual(['Actor'], beamtalk_class_registry:direct_subclasses('Object'))
       end},
      {"leaf class has no children",
       fun() ->
           ?assertEqual([], beamtalk_class_registry:direct_subclasses('Counter'))
       end},
      {"unknown class",
       fun() ->
           ?assertEqual([], beamtalk_class_registry:direct_subclasses('Unknown'))
       end}
     ]}.

%%% ============================================================================
%%% all_subclasses tests
%%% ============================================================================

all_subclasses_test_() ->
    {setup,
     fun() ->
         beamtalk_class_registry:ensure_hierarchy_table(),
         ets:insert(beamtalk_class_hierarchy, {'Object', none}),
         ets:insert(beamtalk_class_hierarchy, {'Actor', 'Object'}),
         ets:insert(beamtalk_class_hierarchy, {'Counter', 'Actor'}),
         ets:insert(beamtalk_class_hierarchy, {'Timer', 'Actor'})
     end,
     fun(_) ->
         ets:delete(beamtalk_class_hierarchy)
     end,
     [
      {"all subclasses of Object",
       fun() ->
           Result = beamtalk_class_registry:all_subclasses('Object'),
           ?assertEqual(['Actor', 'Counter', 'Timer'], Result)
       end},
      {"all subclasses of Actor",
       fun() ->
           Result = beamtalk_class_registry:all_subclasses('Actor'),
           ?assertEqual(['Counter', 'Timer'], Result)
       end},
      {"leaf class has no subclasses",
       fun() ->
           ?assertEqual([], beamtalk_class_registry:all_subclasses('Counter'))
       end}
     ]}.

%%% ============================================================================
%%% whereis_class tests
%%% ============================================================================

whereis_class_unregistered_test() ->
    ?assertEqual(undefined,
                 beamtalk_class_registry:whereis_class('NonExistentClass12345')).

whereis_class_registered_test() ->
    RegName = beamtalk_class_registry:registry_name('TestClassBT708'),
    register(RegName, self()),
    ?assertEqual(self(), beamtalk_class_registry:whereis_class('TestClassBT708')),
    unregister(RegName).

%%% ============================================================================
%%% all_classes tests
%%% ============================================================================

all_classes_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    %% all_classes returns whatever is in the pg group
    Result = beamtalk_class_registry:all_classes(),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% invalidate_subclass_flattened_tables tests
%%% ============================================================================

invalidate_subclass_flattened_tables_test() ->
    beamtalk_class_registry:ensure_pg_started(),
    Ref = make_ref(),
    Self = self(),
    %% Spawn a mock class process and join the pg group
    MockPid = spawn(fun() ->
        pg:join(beamtalk_classes, self()),
        receive
            {rebuild_flattened, _Class} = Msg -> Self ! {Ref, Msg}
        after 1000 -> Self ! {Ref, timeout}
        end,
        pg:leave(beamtalk_classes, self())
    end),
    %% Give time for pg:join to take effect
    timer:sleep(50),
    beamtalk_class_registry:invalidate_subclass_flattened_tables('TestClass'),
    Result = receive {Ref, R} -> R after 1000 -> timeout end,
    ?assertEqual({rebuild_flattened, 'TestClass'}, Result),
    %% Ensure mock process exits
    _ = (catch exit(MockPid, kill)),
    ok.
