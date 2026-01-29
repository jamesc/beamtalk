%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Example showing how to use the beamtalk_classes registry.
%%
%% This demonstrates registering classes, querying the hierarchy,
%% and adding methods dynamically (for live development).
-module(class_registry_example).
-export([run/0]).

run() ->
    io:format("~n=== Beamtalk Class Registry Example ===~n~n"),
    
    %% Start the registry (normally done by the supervisor)
    {ok, _Pid} = beamtalk_classes:start_link(),
    
    %% 1. Register the Object base class
    io:format("1. Registering Object class...~n"),
    ObjectInfo = #{
        module => object,
        superclass => none,
        methods => #{
            class => #{arity => 0},
            '==' => #{arity => 1}
        },
        instance_variables => [],
        class_variables => #{}
    },
    ok = beamtalk_classes:register_class('Object', ObjectInfo),
    io:format("   ✓ Object registered~n~n"),
    
    %% 2. Register Actor (subclass of Object)
    io:format("2. Registering Actor class...~n"),
    ActorInfo = #{
        module => actor,
        superclass => 'Object',
        methods => #{
            spawn => #{arity => 0},
            'spawn:' => #{arity => 1},
            send => #{arity => 1}
        },
        instance_variables => [],
        class_variables => #{}
    },
    ok = beamtalk_classes:register_class('Actor', ActorInfo),
    io:format("   ✓ Actor registered (subclass of Object)~n~n"),
    
    %% 3. Register Counter (subclass of Actor)
    io:format("3. Registering Counter class...~n"),
    CounterInfo = #{
        module => counter,
        superclass => 'Actor',
        methods => #{
            increment => #{arity => 0},
            decrement => #{arity => 0},
            value => #{arity => 0}
        },
        instance_variables => [count],
        class_variables => #{},
        source_file => "Counter.bt"
    },
    ok = beamtalk_classes:register_class('Counter', CounterInfo),
    io:format("   ✓ Counter registered (subclass of Actor)~n~n"),
    
    %% 4. Register Point (another Object subclass)
    io:format("4. Registering Point class...~n"),
    PointInfo = #{
        module => point,
        superclass => 'Object',
        methods => #{
            'x:y:' => #{arity => 2},
            x => #{arity => 0},
            y => #{arity => 0}
        },
        instance_variables => [x, y],
        class_variables => #{}
    },
    ok = beamtalk_classes:register_class('Point', PointInfo),
    io:format("   ✓ Point registered (subclass of Object)~n~n"),
    
    %% 5. Query all classes
    io:format("5. All registered classes:~n"),
    AllClasses = beamtalk_classes:all_classes(),
    lists:foreach(fun(Class) ->
        io:format("   - ~p~n", [Class])
    end, lists:sort(AllClasses)),
    io:format("~n"),
    
    %% 6. Query the class hierarchy
    io:format("6. Class hierarchy:~n"),
    io:format("   Object subclasses: ~p~n", [beamtalk_classes:subclasses_of('Object')]),
    io:format("   Actor subclasses:  ~p~n", [beamtalk_classes:subclasses_of('Actor')]),
    io:format("   Counter subclasses: ~p~n", [beamtalk_classes:subclasses_of('Counter')]),
    io:format("~n"),
    
    %% 7. Lookup a specific class
    io:format("7. Looking up Counter class:~n"),
    {ok, CounterLookup} = beamtalk_classes:lookup('Counter'),
    io:format("   Module: ~p~n", [maps:get(module, CounterLookup)]),
    io:format("   Superclass: ~p~n", [maps:get(superclass, CounterLookup)]),
    io:format("   Instance vars: ~p~n", [maps:get(instance_variables, CounterLookup)]),
    io:format("   Methods: ~p~n", [maps:keys(maps:get(methods, CounterLookup))]),
    io:format("~n"),
    
    %% 8. Add a method dynamically (live development!)
    io:format("8. Adding 'reset' method to Counter (live development):~n"),
    ResetMethod = fun() -> io:format("   Counter reset to 0~n") end,
    ok = beamtalk_classes:add_method('Counter', reset, ResetMethod),
    io:format("   ✓ Method added~n"),
    
    %% Verify it was added
    {ok, Updated} = beamtalk_classes:lookup('Counter'),
    UpdatedMethods = maps:get(methods, Updated),
    io:format("   Counter now has methods: ~p~n", [maps:keys(UpdatedMethods)]),
    io:format("~n"),
    
    %% 9. Remove a method
    io:format("9. Removing 'decrement' method:~n"),
    ok = beamtalk_classes:remove_method('Counter', decrement),
    {ok, AfterRemove} = beamtalk_classes:lookup('Counter'),
    FinalMethods = maps:get(methods, AfterRemove),
    io:format("   Counter now has methods: ~p~n", [maps:keys(FinalMethods)]),
    io:format("~n"),
    
    %% 10. Error handling
    io:format("10. Error handling:~n"),
    case beamtalk_classes:lookup('NonExistent') of
        undefined ->
            io:format("   ✓ Looking up non-existent class returns undefined~n");
        _ ->
            io:format("   ✗ Unexpected result~n")
    end,
    
    case beamtalk_classes:add_method('NonExistent', foo, fun() -> ok end) of
        {error, class_not_found} ->
            io:format("   ✓ Adding method to non-existent class returns error~n");
        _ ->
            io:format("   ✗ Unexpected result~n")
    end,
    
    io:format("~n=== Example Complete! ===~n~n").
