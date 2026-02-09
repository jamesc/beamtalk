%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Bootstrap the three-level class hierarchy.
%%%
%%% This module creates the foundational class objects for Beamtalk:
%%% - ProtoObject class (true root, superclass = none)
%%% - Object class (superclass = ProtoObject class)
%%% - Actor class (superclass = Object class)
%%%
%%% These three class processes must exist before any user-defined classes
%%% can be instantiated. They form the parallel metaclass hierarchy:
%%%
%%% ```
%%% Instance Hierarchy:         Metaclass Hierarchy:
%%% ProtoObject           <---  ProtoObject class (none)
%%%   └─ Object          <---    └─ Object class (ProtoObject class)
%%%        └─ Actor     <---        └─ Actor class (Object class)
%%%             └─ User classes...
%%% ```
%%%
%%% ## Startup Order
%%%
%%% 1. Start pg (process group) for class registry
%%% 2. Create ProtoObject class process
%%% 3. Create Object class process (with superclass = ProtoObject)
%%% 4. Create Actor class process (with superclass = Object)
%%%
%%% Each class process is registered with `beamtalk_object_class:start_link/2` and
%%% joins the `beamtalk_classes` pg group for enumeration.
-module(beamtalk_bootstrap).

-export([start_link/0, init/1]).

%% @doc Start the bootstrap process.
%%
%% This is typically called during application startup to ensure the
%% foundational class hierarchy exists before user code runs.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    %% Use proc_lib to create a supervised process
    proc_lib:start_link(?MODULE, init, [self()]).

%% @doc Initialize the class hierarchy.
%%
%% Creates ProtoObject class, Object class, and Actor class processes.
%% Returns after all three are successfully started and registered.
-spec init(pid()) -> no_return().
init(Parent) ->
    %% Ensure pg is started
    case whereis(pg) of
        undefined ->
            {ok, _Pid} = pg:start_link();
        _ ->
            ok
    end,
    
    try
        %% Step 1: Create ProtoObject class (root of metaclass hierarchy)
        ProtoObjectClassInfo = #{
            name => 'ProtoObject',
            module => 'ProtoObject',  % Module name (if it exists)
            superclass => none,       % No superclass - true root
            instance_methods => #{
                % Core messages from ProtoObject
                class => #{arity => 0},
                'doesNotUnderstand:args:' => #{arity => 2},
                '==' => #{arity => 1},
                '~=' => #{arity => 1},
                'perform:withArguments:' => #{arity => 2}
            },
            class_methods => #{
                % Metaclass methods (if needed)
            },
            instance_variables => []
        },
        {ok, _ProtoObjectClassPid} = case beamtalk_object_class:start_link('ProtoObject', ProtoObjectClassInfo) of
            {ok, Pid1} -> {ok, Pid1};
            {error, {already_started, Pid1}} -> {ok, Pid1}
        end,
        
        %% Step 2: Create Object class (inherits from ProtoObject class)
        ObjectClassInfo = #{
            name => 'Object',
            module => 'beamtalk_object',
            superclass => 'ProtoObject',  % Metaclass superclass
            instance_methods => #{
                % Core reflection (ADR 0006 Phase 1b - beamtalk_object.erl)
                class => #{arity => 0},
                'respondsTo:' => #{arity => 1},
                'instVarNames' => #{arity => 0},
                'instVarAt:' => #{arity => 1},
                'instVarAt:put:' => #{arity => 2},
                'perform:' => #{arity => 1},
                'perform:withArguments:' => #{arity => 2},
                % Display methods (BT-275)
                'printString' => #{arity => 0},
                inspect => #{arity => 0},
                describe => #{arity => 0},
                % Utility methods
                yourself => #{arity => 0},
                hash => #{arity => 0},
                isNil => #{arity => 0},
                notNil => #{arity => 0},
                'ifNil:' => #{arity => 1},
                'ifNotNil:' => #{arity => 1},
                'ifNil:ifNotNil:' => #{arity => 2},
                'ifNotNil:ifNil:' => #{arity => 2}
            },
            class_methods => #{},
            instance_variables => []
        },
        {ok, _ObjectClassPid} = case beamtalk_object_class:start_link('Object', ObjectClassInfo) of
            {ok, Pid2} -> {ok, Pid2};
            {error, {already_started, Pid2}} -> {ok, Pid2}
        end,
        
        %% Step 3: Create Number class (abstract numeric superclass, inherits from Object)
        NumberClassInfo = #{
            name => 'Number',
            module => bt_stdlib_number,
            superclass => 'Object',
            is_abstract => true,
            instance_methods => #{
                isZero => #{arity => 0},
                isPositive => #{arity => 0},
                isNegative => #{arity => 0},
                sign => #{arity => 0},
                'between:and:' => #{arity => 2}
            },
            class_methods => #{},
            instance_variables => []
        },
        {ok, _NumberClassPid} = case beamtalk_object_class:start_link('Number', NumberClassInfo) of
            {ok, Pid3a} -> {ok, Pid3a};
            {error, {already_started, Pid3a}} -> {ok, Pid3a}
        end,
        
        %% Step 4: Create Actor class (inherits from Object class)
        ActorClassInfo = #{
            name => 'Actor',
            module => 'Actor',
            superclass => 'Object',  % Metaclass superclass
            instance_methods => #{
                % Inherits Object methods, adds:
                describe => #{arity => 0}  % Override
                % spawn and spawnWith are class methods, not instance methods
            },
            class_methods => #{
                spawn => #{arity => 0},
                'spawnWith:' => #{arity => 1}
            },
            instance_variables => []
        },
        {ok, _ActorClassPid} = case beamtalk_object_class:start_link('Actor', ActorClassInfo) of
            {ok, Pid3} -> {ok, Pid3};
            {error, {already_started, Pid3}} -> {ok, Pid3}
        end,
        
        %% Signal parent that we're ready
        proc_lib:init_ack(Parent, {ok, self()}),
        
        %% Enter a receive loop (this process stays alive as a placeholder)
        %% The actual work is done by the class processes we spawned
        bootstrap_loop()
    catch
        Class:Reason:Stacktrace ->
            proc_lib:init_ack(Parent, {error, {Class, Reason, Stacktrace}}),
            exit(Reason)
    end.

%% @private
%% Bootstrap process loop - just stays alive, doesn't handle messages
bootstrap_loop() ->
    receive
        _Any -> bootstrap_loop()
    end.
