%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_opaque_type_tests).

-moduledoc """
EUnit tests for opaque BEAM type dispatch.

Tests Pid, Port, and Reference Object protocol methods:
asString, printString, class, ==, =:=, hash, isAlive (Pid only).
Also tests does_not_understand for unknown messages.
""".
-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Pid dispatch tests
%%% ============================================================================

pid_class_test() ->
    Pid = self(),
    ?assertEqual('Pid', beamtalk_primitive:class_of(Pid)).

pid_test_() ->
    {foreach, fun() -> beamtalk_class_shadow_flags:mark_ready() end, fun(_) -> ok end, [
        fun pid_as_string/0,
        fun pid_print_string/0,
        fun pid_print_string_matches_as_string/0,
        fun pid_strict_equality/0,
        fun pid_inequality/0,
        fun pid_hash/0,
        fun pid_is_alive/0,
        fun pid_equality_inherited/0,
        fun pid_responds_to/0,
        fun pid_does_not_understand/0
    ]}.

pid_as_string() ->
    Pid = self(),
    Result = beamtalk_primitive:send(Pid, 'asString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Pid<", _/binary>>}, Result).

pid_print_string() ->
    Pid = self(),
    Result = beamtalk_primitive:send(Pid, 'printString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Pid<", _/binary>>}, Result).

pid_print_string_matches_as_string() ->
    Pid = self(),
    AsString = beamtalk_primitive:send(Pid, 'asString', []),
    PrintString = beamtalk_primitive:send(Pid, 'printString', []),
    ?assertEqual(AsString, PrintString).

pid_strict_equality() ->
    Pid = self(),
    ?assertEqual(true, beamtalk_primitive:send(Pid, '=:=', [Pid])),
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(false, beamtalk_primitive:send(Pid, '=:=', [Other])),
    Other ! stop.

pid_inequality() ->
    Pid = self(),
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ?assertEqual(true, beamtalk_primitive:send(Pid, '/=', [Other])),
    ?assertEqual(false, beamtalk_primitive:send(Pid, '/=', [Pid])),
    Other ! stop.

pid_hash() ->
    Pid = self(),
    Hash = beamtalk_primitive:send(Pid, 'hash', []),
    ?assert(is_integer(Hash)).

pid_is_alive() ->
    %% Current process is alive
    ?assertEqual(true, beamtalk_primitive:send(self(), 'isAlive', [])),
    %% Dead process
    Pid = spawn(fun() -> ok end),
    MRef = monitor(process, Pid),
    ?awaitMessage({'DOWN', MRef, process, Pid, _}),
    ?assertEqual(false, beamtalk_primitive:send(Pid, 'isAlive', [])).

pid_equality_inherited() ->
    %% ProtoObject == is inherited
    Pid = self(),
    ?assertEqual(true, beamtalk_primitive:send(Pid, '==', [Pid])).

pid_responds_to() ->
    Pid = self(),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'asString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'printString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, '=:=')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'hash')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'isAlive')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'class')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Pid, '==')).

pid_does_not_understand() ->
    Pid = self(),
    ?assertBtErrorTagged(
        'RuntimeError',
        does_not_understand,
        'Pid',
        _,
        beamtalk_primitive:send(Pid, 'unknownMessage', [])
    ).

%%% ============================================================================
%%% Port dispatch tests
%%% ============================================================================

port_class_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    ?assertEqual('Port', beamtalk_primitive:class_of(Port)),
    gen_tcp:close(Port).

port_test_() ->
    {foreach, fun() -> beamtalk_class_shadow_flags:mark_ready() end, fun(_) -> ok end, [
        fun port_as_string/0,
        fun port_print_string/0,
        fun port_strict_equality/0,
        fun port_hash/0,
        fun port_responds_to/0,
        fun port_does_not_understand/0
    ]}.

port_as_string() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_primitive:send(Port, 'asString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Port<", _/binary>>}, Result),
    gen_tcp:close(Port).

port_print_string() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_primitive:send(Port, 'printString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Port<", _/binary>>}, Result),
    gen_tcp:close(Port).

port_strict_equality() ->
    {ok, Port1} = gen_tcp:listen(0, []),
    {ok, Port2} = gen_tcp:listen(0, []),
    ?assertEqual(true, beamtalk_primitive:send(Port1, '=:=', [Port1])),
    ?assertEqual(false, beamtalk_primitive:send(Port1, '=:=', [Port2])),
    gen_tcp:close(Port1),
    gen_tcp:close(Port2).

port_hash() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Hash = beamtalk_primitive:send(Port, 'hash', []),
    ?assert(is_integer(Hash)),
    gen_tcp:close(Port).

port_responds_to() ->
    {ok, Port} = gen_tcp:listen(0, []),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'asString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'printString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, '=:=')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'hash')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'class')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Port, '==')),
    gen_tcp:close(Port).

port_does_not_understand() ->
    {ok, Port} = gen_tcp:listen(0, []),
    ?assertBtErrorTagged(
        'RuntimeError',
        does_not_understand,
        'Port',
        _,
        beamtalk_primitive:send(Port, 'unknownMessage', [])
    ),
    gen_tcp:close(Port).

%%% ============================================================================
%%% Reference dispatch tests
%%% ============================================================================

reference_class_test() ->
    Ref = make_ref(),
    ?assertEqual('Reference', beamtalk_primitive:class_of(Ref)).

reference_test_() ->
    {foreach, fun() -> beamtalk_class_shadow_flags:mark_ready() end, fun(_) -> ok end, [
        fun reference_as_string/0,
        fun reference_print_string/0,
        fun reference_strict_equality/0,
        fun reference_hash/0,
        fun reference_responds_to/0,
        fun reference_does_not_understand/0
    ]}.

reference_as_string() ->
    Ref = make_ref(),
    Result = beamtalk_primitive:send(Ref, 'asString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Ref<", _/binary>>}, Result).

reference_print_string() ->
    Ref = make_ref(),
    Result = beamtalk_primitive:send(Ref, 'printString', []),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertMatch({beamtalk_string, <<"#Ref<", _/binary>>}, Result).

reference_strict_equality() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    ?assertEqual(true, beamtalk_primitive:send(Ref1, '=:=', [Ref1])),
    ?assertEqual(false, beamtalk_primitive:send(Ref1, '=:=', [Ref2])).

reference_hash() ->
    Ref = make_ref(),
    Hash = beamtalk_primitive:send(Ref, 'hash', []),
    ?assert(is_integer(Hash)).

reference_responds_to() ->
    Ref = make_ref(),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'asString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'printString')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, '=:=')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'hash')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'class')),
    ?assertEqual(true, beamtalk_primitive:responds_to(Ref, '==')).

reference_does_not_understand() ->
    Ref = make_ref(),
    ?assertBtErrorTagged(
        'RuntimeError',
        does_not_understand,
        'Reference',
        _,
        beamtalk_primitive:send(Ref, 'unknownMessage', [])
    ).

%%% ============================================================================
%%% print_string/1 integration tests
%%% ============================================================================

print_string_pid_test() ->
    %% print_string/1 itself answers raw bytes; only dispatched
    %% `printString`/`asString` wrap as a tagged String.
    Result = beamtalk_primitive:print_string(self()),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"#Pid<", _/binary>>, Result).

print_string_port_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    Result = beamtalk_primitive:print_string(Port),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"#Port<", _/binary>>, Result),
    gen_tcp:close(Port).

print_string_ref_test() ->
    Ref = make_ref(),
    Result = beamtalk_primitive:print_string(Ref),
    ?assert(is_binary(Result)),
    ?assertMatch(<<"#Ref<", _/binary>>, Result).

%%% Module-level fixture (BT-4962): loading a compiled `bt@stdlib@*` module starts
%%% class processes; with no runtime app to own them they would outlive this suite.
eunit_wrapper_(Tests) -> beamtalk_test_class_teardown:leak_free(Tests).
