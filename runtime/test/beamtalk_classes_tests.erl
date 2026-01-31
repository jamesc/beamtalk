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

concurrent_registration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Spawn multiple processes that register classes concurrently
             NumClasses = 20,
             Self = self(),
             _Pids = [spawn(fun() ->
                 ClassName = list_to_atom("Class" ++ integer_to_list(N)),
                 ClassInfo = #{
                     module => list_to_atom("class" ++ integer_to_list(N)),
                     superclass => object,
                     methods => #{},
                     instance_variables => [],
                     class_variables => #{}
                 },
                 ok = beamtalk_classes:register_class(Pid, ClassName, ClassInfo),
                 Self ! {registered, N}
             end) || N <- lists:seq(1, NumClasses)],
             
             % Wait for all registrations to complete
             [receive {registered, N} -> ok after 1000 -> ?assert(false) end || N <- lists:seq(1, NumClasses)],
             
             % Verify all classes were registered
             Classes = beamtalk_classes:all_classes(Pid),
             ?assertEqual(NumClasses, length(Classes)),
             
             % Verify each class can be looked up
             [begin
                 ClassName = list_to_atom("Class" ++ integer_to_list(N)),
                 ?assertMatch({ok, _}, beamtalk_classes:lookup(Pid, ClassName))
             end || N <- lists:seq(1, NumClasses)]
         end)]
     end}.

empty_methods_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with empty methods
             ClassInfo = #{
                 module => empty_class,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'EmptyClass', ClassInfo),
             
             % Verify it can be retrieved
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'EmptyClass'),
             ?assertEqual(#{}, maps:get(methods, Retrieved))
         end)]
     end}.

terminate_callback_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register some classes
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{value => #{}},
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Call terminate directly
             ok = beamtalk_classes:terminate(normal, sys:get_state(Pid)),
             
             % This just verifies terminate/2 doesn't crash
             ok
         end)]
     end}.

handle_info_ignores_unknown_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Send an unknown info message
             Pid ! unknown_message,
             Pid ! {random, stuff},
             
             % Give it time to process
             timer:sleep(10),
             
             % Verify server still works
             ?assertEqual(1, length(beamtalk_classes:all_classes(Pid)))
         end)]
     end}.

handle_cast_ignored_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Send a cast message (not part of API, should be ignored)
             gen_server:cast(Pid, {unknown, cast}),
             
             % Give it time to process
             timer:sleep(10),
             
             % Verify server still works
             ?assertEqual(1, length(beamtalk_classes:all_classes(Pid)))
         end)]
     end}.

update_method_with_block_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with a method
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     increment => #{arity => 0}
                 },
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Add a block to existing method
             Block1 = fun() -> version1 end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', increment, Block1),
             
             % Update with a different block
             Block2 = fun() -> version2 end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', increment, Block2),
             
             % Verify the new block replaced the old one
             {ok, Updated} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Updated),
             #{block := Block} = maps:get(increment, Methods),
             ?assertEqual(Block2, Block)
         end)]
     end}.

class_variable_storage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with class variables
             ClassVars = #{
                 instance_count => 0,
                 default_name => <<"Counter">>
             },
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => ClassVars
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Retrieve and verify class variables
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             ?assertEqual(ClassVars, maps:get(class_variables, Retrieved))
         end)]
     end}.

deep_hierarchy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Build a deeper hierarchy: Object <- A <- B <- C <- D
             ok = beamtalk_classes:register_class(Pid, 'Object', #{
                 module => object, superclass => none, methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'A', #{
                 module => a, superclass => 'Object', methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'B', #{
                 module => b, superclass => 'A', methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'C', #{
                 module => c, superclass => 'B', methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             ok = beamtalk_classes:register_class(Pid, 'D', #{
                 module => d, superclass => 'C', methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             
             % Verify subclass queries at each level
             ?assertEqual(1, length(beamtalk_classes:subclasses_of(Pid, 'Object'))),
             ?assertEqual(1, length(beamtalk_classes:subclasses_of(Pid, 'A'))),
             ?assertEqual(1, length(beamtalk_classes:subclasses_of(Pid, 'B'))),
             ?assertEqual(1, length(beamtalk_classes:subclasses_of(Pid, 'C'))),
             ?assertEqual(0, length(beamtalk_classes:subclasses_of(Pid, 'D')))
         end)]
     end}.

multiple_instance_variables_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with multiple instance variables
             InstVars = [x, y, z, color, size],
             ClassInfo = #{
                 module => point3d,
                 superclass => object,
                 methods => #{},
                 instance_variables => InstVars,
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Point3D', ClassInfo),
             
             % Verify instance variables are preserved
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Point3D'),
             ?assertEqual(InstVars, maps:get(instance_variables, Retrieved))
         end)]
     end}.

unknown_call_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Send a malformed call request
             Result = gen_server:call(Pid, unknown_call_format),
             ?assertEqual({error, unknown_request}, Result)
         end)]
     end}.

source_file_optional_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with source_file
             ClassInfo1 = #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{},
                 source_file => "Counter.bt"
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo1),
             {ok, Retrieved1} = beamtalk_classes:lookup(Pid, 'Counter'),
             ?assertEqual("Counter.bt", maps:get(source_file, Retrieved1)),
             
             % Register a class without source_file
             ClassInfo2 = #{
                 module => point,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Point', ClassInfo2),
             {ok, Retrieved2} = beamtalk_classes:lookup(Pid, 'Point'),
             ?assertNot(maps:is_key(source_file, Retrieved2))
         end)]
     end}.

remove_nonexistent_method_test_() ->
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
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Remove a method that doesn't exist (should succeed silently)
             ok = beamtalk_classes:remove_method(Pid, 'Counter', nonexistent),
             
             % Verify original method is still there
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Retrieved),
             ?assert(maps:is_key(value, Methods))
         end)]
     end}.

add_method_to_class_with_no_existing_block_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Register a class with a method that has no existing block
             ClassInfo = #{
                 module => counter,
                 superclass => object,
                 methods => #{
                     value => #{}  % No block field
                 },
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Add a block to the method
             Block = fun() -> 42 end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', value, Block),
             
             % Verify block was added
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Retrieved),
             #{block := RetrievedBlock} = maps:get(value, Methods),
             ?assertEqual(Block, RetrievedBlock)
         end)]
     end}.

add_brand_new_method_test_() ->
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
                 instance_variables => [],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class(Pid, 'Counter', ClassInfo),
             
             % Add a completely new method (not in original methods map)
             NewBlock = fun() -> new_method end,
             ok = beamtalk_classes:add_method(Pid, 'Counter', increment, NewBlock),
             
             % Verify new method was added
             {ok, Retrieved} = beamtalk_classes:lookup(Pid, 'Counter'),
             Methods = maps:get(methods, Retrieved),
             ?assert(maps:is_key(increment, Methods)),
             #{block := Block} = maps:get(increment, Methods),
             ?assertEqual(NewBlock, Block),
             
             % Original method should still be there
             ?assert(maps:is_key(value, Methods))
         end)]
     end}.

wide_hierarchy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [?_test(begin
             % Build a wide hierarchy: Object <- [A, B, C, D, E]
             ok = beamtalk_classes:register_class(Pid, 'Object', #{
                 module => object, superclass => none, methods => #{},
                 instance_variables => [], class_variables => #{}
             }),
             
             % Register 5 direct subclasses of Object
             [ok = beamtalk_classes:register_class(Pid, Class, #{
                 module => list_to_atom(string:lowercase(atom_to_list(Class))),
                 superclass => 'Object',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }) || Class <- ['A', 'B', 'C', 'D', 'E']],
             
             % Verify Object has 5 subclasses
             Subs = beamtalk_classes:subclasses_of(Pid, 'Object'),
             ?assertEqual(5, length(Subs)),
             
             % Verify all subclasses are present
             [?assert(lists:member(C, Subs)) || C <- ['A', 'B', 'C', 'D', 'E']]
         end)]
     end}.
