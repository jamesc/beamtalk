%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_protocol_object_tests).

-moduledoc """
EUnit tests for protocol class object creation (ADR 0068).

Tests that `register_protocol/1` creates a sealed abstract class process
(subclass of Protocol) for each protocol, making protocols discoverable
via `allClasses`, `:h`, and direct message sends like `Printable requiredMethods`.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Setup / Teardown
%%% ============================================================================

-doc """
Set up the class infrastructure and Protocol superclass needed for protocol
class object creation. Returns cleanup state.
""".
setup() ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_protocol_registry:init(),
    %% Clean protocol registry
    case ets:info(beamtalk_protocol_registry) of
        undefined -> ok;
        _ -> ets:delete_all_objects(beamtalk_protocol_registry)
    end,
    %% Ensure Protocol class exists (needed as superclass)
    ProtocolPid =
        case beamtalk_class_registry:whereis_class('Protocol') of
            undefined ->
                {ok, Pid} = beamtalk_object_class:start('Protocol', #{
                    superclass => 'Object',
                    is_sealed => true,
                    is_abstract => true,
                    meta => #{is_sealed => true, is_abstract => true}
                }),
                Pid;
            Pid ->
                Pid
        end,
    ProtocolPid.

cleanup(ProtocolPid) ->
    %% Clean up test protocol class processes
    lists:foreach(
        fun(TestName) ->
            case beamtalk_class_registry:whereis_class(TestName) of
                undefined ->
                    ok;
                Pid ->
                    try
                        gen_server:stop(Pid)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        ['TestProto1', 'TestProto2', 'TestProtoWithDoc']
    ),
    %% Clean protocol registry
    case ets:info(beamtalk_protocol_registry) of
        undefined -> ok;
        _ -> ets:delete_all_objects(beamtalk_protocol_registry)
    end,
    %% Only stop Protocol if we created it (check it's still alive)
    case is_process_alive(ProtocolPid) of
        true ->
            try
                gen_server:stop(ProtocolPid)
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end.

%%% ============================================================================
%%% Test Generators
%%% ============================================================================

protocol_class_creation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_ProtocolPid) ->
        [
            {"register_protocol creates a class process", fun register_creates_class/0},
            {"protocol class is sealed and abstract", fun protocol_class_is_sealed_abstract/0},
            {"protocol class superclass is Protocol", fun protocol_class_superclass/0},
            {"protocol class appears in live_class_entries", fun protocol_in_live_entries/0},
            {"requiredMethods class method dispatch works", fun required_methods_dispatch/0},
            {"conformingClasses class method dispatch works", fun conforming_classes_dispatch/0},
            {"doc propagation from protocol info", fun doc_propagation/0},
            {"idempotent re-registration", fun idempotent_reregistration/0}
        ]
    end}.

%%% ============================================================================
%%% Test Cases
%%% ============================================================================

register_creates_class() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [#{selector => 'foo', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    %% Class process should now exist
    Pid = beamtalk_class_registry:whereis_class('TestProto1'),
    ?assertNotEqual(undefined, Pid),
    ?assert(is_process_alive(Pid)).

protocol_class_is_sealed_abstract() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [#{selector => 'foo', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid = beamtalk_class_registry:whereis_class('TestProto1'),
    ?assert(beamtalk_object_class:is_sealed(Pid)),
    ?assert(beamtalk_object_class:is_abstract(Pid)).

protocol_class_superclass() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [#{selector => 'foo', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid = beamtalk_class_registry:whereis_class('TestProto1'),
    ?assertEqual('Protocol', beamtalk_object_class:superclass(Pid)).

protocol_in_live_entries() ->
    Proto = #{
        name => 'TestProto2',
        required_methods => [#{selector => 'bar', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Entries = beamtalk_class_registry:live_class_entries(),
    Names = [Name || {Name, _Mod, _Pid} <- Entries],
    ?assert(lists:member('TestProto2', Names)).

required_methods_dispatch() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [
            #{selector => 'foo', arity => 0},
            #{selector => 'bar:', arity => 1}
        ],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid = beamtalk_class_registry:whereis_class('TestProto1'),
    %% Dispatch class method via gen_server call (simulates Printable requiredMethods)
    {ok, Result} = gen_server:call(Pid, {class_method_call, requiredMethods, []}),
    ?assert(lists:member('foo', Result)),
    ?assert(lists:member('bar:', Result)).

conforming_classes_dispatch() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [#{selector => 'foo', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid = beamtalk_class_registry:whereis_class('TestProto1'),
    %% conformingClasses returns a list (possibly empty in test context)
    {ok, Result} = gen_server:call(Pid, {class_method_call, conformingClasses, []}),
    ?assert(is_list(Result)).

doc_propagation() ->
    Proto = #{
        name => 'TestProtoWithDoc',
        required_methods => [#{selector => 'render', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined,
        doc => <<"A test protocol for rendering.">>
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid = beamtalk_class_registry:whereis_class('TestProtoWithDoc'),
    Doc = gen_server:call(Pid, get_doc),
    ?assertEqual(<<"A test protocol for rendering.">>, Doc).

idempotent_reregistration() ->
    Proto = #{
        name => 'TestProto1',
        required_methods => [#{selector => 'foo', arity => 0}],
        required_class_methods => [],
        type_params => [],
        extending => undefined
    },
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid1 = beamtalk_class_registry:whereis_class('TestProto1'),
    %% Re-register — should not crash or create a second process
    ok = beamtalk_protocol_registry:register_protocol(Proto),
    Pid2 = beamtalk_class_registry:whereis_class('TestProto1'),
    ?assertEqual(Pid1, Pid2).
