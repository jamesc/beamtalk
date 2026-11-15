%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_mock_class).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: minimal mock class processes registered in the class
registry, answering just the calls the REPL completion / chain-resolution
code makes (BT-4900; extracted from `beamtalk_repl_server_tests`).
""".

-export([spawn/3, spawn_with_return_types/3, cleanup/2]).

%% Helper: spawn a mock class gen_server process and register it in the class registry.
spawn(Name, ClassMethods, InstanceMethods) ->
    RegName = beamtalk_class_registry:registry_name(Name),
    Self = self(),
    Pid = spawn(fun() ->
        erlang:register(RegName, self()),
        Self ! registered,
        mock_class_loop(InstanceMethods, ClassMethods)
    end),
    receive
        registered -> ok
    after 1000 ->
        error({mock_class_register_timeout, Name})
    end,
    Pid.

mock_class_loop(InstanceMethods, ClassMethods) ->
    receive
        {'$gen_call', From, methods} ->
            gen_server:reply(From, InstanceMethods),
            mock_class_loop(InstanceMethods, ClassMethods);
        {'$gen_call', From, superclass} ->
            gen_server:reply(From, none),
            mock_class_loop(InstanceMethods, ClassMethods);
        {'$gen_call', From, get_local_class_methods} ->
            gen_server:reply(From, ClassMethods),
            mock_class_loop(InstanceMethods, ClassMethods);
        stop ->
            ok;
        _ ->
            mock_class_loop(InstanceMethods, ClassMethods)
    end.

cleanup(Name, Pid) ->
    beamtalk_class_metadata:delete(Name),
    RegName = beamtalk_class_registry:registry_name(Name),
    (try
        erlang:unregister(RegName)
    catch
        _:_ -> ok
    end),
    Pid ! stop.

%% Helper: spawn a mock class that supports return-type lookups for chain resolution.
%% InstanceReturnTypes: #{selector() => atom()} — instance-side return types
%% ClassReturnTypes:    #{selector() => atom()} — class-side return types
spawn_with_return_types(Name, InstanceReturnTypes, ClassReturnTypes) ->
    RegName = beamtalk_class_registry:registry_name(Name),
    Self = self(),
    Pid = spawn(fun() ->
        erlang:register(RegName, self()),
        Self ! registered,
        mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes)
    end),
    receive
        registered -> ok
    after 1000 ->
        error({mock_class_register_timeout, Name})
    end,
    %% BT-3826: return types are read from the class's published metadata row
    %% (never by messaging the class process), so publish the mock's.
    %% A test may have pre-seeded the row's superclass edge; keep it.
    Super =
        case beamtalk_class_metadata:lookup_superclass(Name) of
            {ok, S} -> S;
            not_found -> none
        end,
    beamtalk_class_metadata:merge_identity(Name, undefined, undefined, Super, undefined),
    ok = beamtalk_class_metadata:put_reflection(Name, #{
        instance_methods => maps:keys(InstanceReturnTypes),
        instance_variables => [],
        method_return_types => InstanceReturnTypes,
        class_method_return_types => ClassReturnTypes
    }),
    Pid.

mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes) ->
    receive
        {'$gen_call', From, {get_method_return_type, Selector}} ->
            Reply =
                case maps:find(Selector, InstanceReturnTypes) of
                    {ok, ReturnType} -> {ok, ReturnType};
                    error -> {error, not_found}
                end,
            gen_server:reply(From, Reply),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, {get_class_method_return_type, Selector}} ->
            Reply =
                case maps:find(Selector, ClassReturnTypes) of
                    {ok, ReturnType} -> {ok, ReturnType};
                    error -> {error, not_found}
                end,
            gen_server:reply(From, Reply),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, superclass} ->
            gen_server:reply(From, none),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        {'$gen_call', From, methods} ->
            gen_server:reply(From, maps:keys(InstanceReturnTypes)),
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes);
        stop ->
            ok;
        _ ->
            mock_class_with_return_types_loop(InstanceReturnTypes, ClassReturnTypes)
    end.
