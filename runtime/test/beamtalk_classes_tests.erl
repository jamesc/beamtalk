%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Unit tests for the beamtalk_classes registry.
%%% Tests cover registration, lookup, hierarchy queries, method mutation,
%%% and error handling for the global class registry.
-module(beamtalk_classes_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_classes:start_link_unnamed(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% Tests
%%====================================================================

register_and_lookup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     increment => #{arity => 0},
                     value => #{arity => 0}
                 },
                 instance_variables => [count],
                 class_variables => #{},
                 source_file => "Counter.bt"
             },
             
             % Register class
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Lookup should return the class info
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             ?assertEqual(ClassInfo, Retrieved),
             
             % Lookup non-existent class
             ?assertEqual(undefined, beamtalk_classes:lookup(Pid, 'NonExistent'))
         end)]
     end}.

all_classes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Initially empty
             ?assertEqual([], beamtalk_classes:all_classes(Pid)),
             
             % Register multiple classes
             ok = beamtalk_classes:register_class(Pid, 'Counter', #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'Point', #{
                 module => point,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             
             % all_classes should return both
             Classes = beamtalk_classes:all_classes(Pid),
             ?assertEqual(2, length(Classes)),
             ?assert(lists:member('Counter', Classes)),
             ?assert(lists:member('Point', Classes))
         end)]
     end}.

subclasses_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Build a simple hierarchy: Object <- Actor <- Counter
             ok = beamtalk_classes:register_class(Pid, 'Object', #{
                 module => object,
                 superclass => none,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'Actor', #{
                 module => actor,
                 superclass => 'Object',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'Counter', #{
                 module => counter,
                 superclass => 'Actor',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'Point', #{
                 module => point,
                 superclass => 'Object',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             
             % Object has two direct subclasses
             ObjectSubs = beamtalk_classes:subclasses_of(Pid, 'Object'),
             ?assertEqual(2, length(ObjectSubs)),
             ?assert(lists:member('Actor', ObjectSubs)),
             ?assert(lists:member('Point', ObjectSubs)),
             
             % Actor has one direct subclass
             ActorSubs = beamtalk_classes:subclasses_of(Pid, 'Actor'),
             ?assertEqual(['Counter'], ActorSubs),
             
             % Counter has no subclasses
             ?assertEqual([], beamtalk_classes:subclasses_of(Pid, 'Counter')),
             
             % Non-existent class returns empty list
             ?assertEqual([], beamtalk_classes:subclasses_of(Pid, 'NonExistent'))
         end)]
     end}.

add_method_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with one method
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     value => #{arity => 0}
                 },
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Add a new method
             IncrementBlock = fun() -> ok end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', increment, IncrementBlock),
             
             % Verify the method was added
             {ok, Updated} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Updated),
             ?assert(maps:is_key(increment, Methods)),
             #{block := Block} = maps:get(increment, Methods),
             ?assertEqual(IncrementBlock, Block)
         end)]
     end}.

add_method_to_nonexistent_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             Block = fun() -> ok end,
             ?assertEqual({error, class_not_found}, 
                          beamtalk_classes:add_method(Pid, 'NonExistent', foo, Block))
         end)]
     end}.

remove_method_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with two methods
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     value => #{arity => 0},
                     increment => #{arity => 0}
                 },
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Remove a method
             ok = beamtalk_classes:remove_method(Pid, 'Counter', increment),
             
             % Verify the method was removed
             {ok, Updated} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Updated),
             ?assertNot(maps:is_key(increment, Methods)),
             ?assert(maps:is_key(value, Methods))
         end)]
     end}.

remove_method_from_nonexistent_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             ?assertEqual({error, class_not_found}, 
                          beamtalk_classes:remove_method(Pid, 'NonExistent', foo))
         end)]
     end}.

overwrite_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo1 = #{
                 module => counter,
                 superclass => object,
                 methods => #{value => #{}},
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo1),
             
             % Re-register with different info (simulating hot reload)
             ClassInfo2 = #{
                 module => counter,
                 superclass => actor,
                 methods => #{value => #{}, increment => #{}},
                 instance_variables => [count, step],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo2),
             
             % Lookup should return the new info
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             ?assertEqual(ClassInfo2, Retrieved)
         end)]
     end}.

register_class_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Missing required field 'module'
             InvalidInfo1 = #{
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             {error, {missing_fields, Missing1}} = beamtalk_classes:register_class(Pid, 'BadClass1', InvalidInfo1),
             ?assert(lists:member(module, Missing1)),
             
             % Missing multiple required fields
             InvalidInfo2 = #{
                 module => bad_class
             },
             {error, {missing_fields, Missing2}} = beamtalk_classes:register_class(Pid, 'BadClass2', InvalidInfo2),
             ?assertEqual(4, length(Missing2)),  % missing 4 fields
             ?assert(lists:member(superclass, Missing2)),
             ?assert(lists:member(methods, Missing2)),
             ?assert(lists:member(instance_variables, Missing2)),
             ?assert(lists:member(class_variables, Missing2)),
             
             % Valid info should work
             ValidInfo = #{
                 module => good_class,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'GoodClass', ValidInfo)
         end)]
     end}.

add_method_preserves_arity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with a method that has arity
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     increment => #{arity => 0},
                     'add:' => #{arity => 1}
                 },
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Add a block to increment method
             IncrementBlock = fun() -> ok end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', increment, IncrementBlock),
             
             % Verify both arity and block are present
             {ok, Updated} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Updated),
             #{arity := Arity, block := Block} = maps:get(increment, Methods),
             ?assertEqual(0, Arity),
             ?assertEqual(IncrementBlock, Block),
             
             % Other method unchanged
             #{arity := AddArity} = maps:get('add:', Methods),
             ?assertEqual(1, AddArity),
             ?assertNot(maps:is_key(block, maps:get('add:', Methods)))
         end)]
     end}.

code_change_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{value => #{}},
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Get current state
             State = sys:get_state(Pid),
             
             % Call code_change directly (simulates OTP upgrade)
             {ok, NewState} = beamtalk_classes:code_change("1.0", State, []),
             
             % Verify state is preserved
             ?assertEqual(State, NewState),
             
             % Verify registry still works
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             ?assertEqual(ClassInfo, Retrieved)
         end)]
     end}.
