%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_classes_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_classes:start_link(),
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
     fun(_Pid) ->
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
             ok = beamtalk_classes:register_class('Counter', ClassInfo),
             
             % Lookup should return the class info
             {ok, Retrieved} = beamtalk_classes:lookup('Counter'),
             ?assertEqual(ClassInfo, Retrieved),
             
             % Lookup non-existent class
             ?assertEqual(undefined, beamtalk_classes:lookup('NonExistent'))
         end)]
     end}.

all_classes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
             % Initially empty
             ?assertEqual([], beamtalk_classes:all_classes()),
             
             % Register multiple classes
             ok = beamtalk_classes:register_class('Counter', #{
                 module => counter,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class('Point', #{
                 module => point,
                 superclass => object,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             
             % all_classes should return both
             Classes = beamtalk_classes:all_classes(),
             ?assertEqual(2, length(Classes)),
             ?assert(lists:member('Counter', Classes)),
             ?assert(lists:member('Point', Classes))
         end)]
     end}.

subclasses_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
             % Build a simple hierarchy: Object <- Actor <- Counter
             ok = beamtalk_classes:register_class('Object', #{
                 module => object,
                 superclass => none,
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class('Actor', #{
                 module => actor,
                 superclass => 'Object',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class('Counter', #{
                 module => counter,
                 superclass => 'Actor',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             ok = beamtalk_classes:register_class('Point', #{
                 module => point,
                 superclass => 'Object',
                 methods => #{},
                 instance_variables => [],
                 class_variables => #{}
             }),
             
             % Object has two direct subclasses
             ObjectSubs = beamtalk_classes:subclasses_of('Object'),
             ?assertEqual(2, length(ObjectSubs)),
             ?assert(lists:member('Actor', ObjectSubs)),
             ?assert(lists:member('Point', ObjectSubs)),
             
             % Actor has one direct subclass
             ActorSubs = beamtalk_classes:subclasses_of('Actor'),
             ?assertEqual(['Counter'], ActorSubs),
             
             % Counter has no subclasses
             ?assertEqual([], beamtalk_classes:subclasses_of('Counter'))
         end)]
     end}.

add_method_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
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
             ok = beamtalk_classes:register_class('Counter', ClassInfo),
             
             % Add a new method
             IncrementBlock = fun() -> ok end,
             ok = beamtalk_classes:add_method('Counter', increment, IncrementBlock),
             
             % Verify the method was added
             {ok, Updated} = beamtalk_classes:lookup('Counter'),
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
     fun(_Pid) ->
         [?_test(begin
             Block = fun() -> ok end,
             ?assertEqual({error, class_not_found}, 
                          beamtalk_classes:add_method('NonExistent', foo, Block))
         end)]
     end}.

remove_method_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
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
             ok = beamtalk_classes:register_class('Counter', ClassInfo),
             
             % Remove a method
             ok = beamtalk_classes:remove_method('Counter', increment),
             
             % Verify the method was removed
             {ok, Updated} = beamtalk_classes:lookup('Counter'),
             Methods = maps:get(methods, Updated),
             ?assertNot(maps:is_key(increment, Methods)),
             ?assert(maps:is_key(value, Methods))
         end)]
     end}.

remove_method_from_nonexistent_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
             ?assertEqual({error, class_not_found}, 
                          beamtalk_classes:remove_method('NonExistent', foo))
         end)]
     end}.

overwrite_class_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
             % Register a class
             ClassInfo1 = #{
                 module => counter,
                 superclass => object,
                 methods => #{value => #{}},
                 instance_variables => [count],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class('Counter', ClassInfo1),
             
             % Re-register with different info (simulating hot reload)
             ClassInfo2 = #{
                 module => counter,
                 superclass => actor,
                 methods => #{value => {}, increment => #{}},
                 instance_variables => [count, step],
                 class_variables => #{}
             },
             ok = beamtalk_classes:register_class('Counter', ClassInfo2),
             
             % Lookup should return the new info
             {ok, Retrieved} = beamtalk_classes:lookup('Counter'),
             ?assertEqual(ClassInfo2, Retrieved)
         end)]
     end}.
