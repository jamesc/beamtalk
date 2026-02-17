%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for opaque BEAM type dispatch (BT-681).
%%%
%%% Tests Pid, Port, and Reference Object protocol methods:
%%% asString, printString, class, ==, =:=, hash, isAlive (Pid only).
%%% Also tests does_not_understand for unknown messages.

-module(beamtalk_opaque_type_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ============================================================================
%%% Pid dispatch tests
%%% ============================================================================

pid_class_test() ->
    Pid = self(),
    ?assertEqual('Pid', beamtalk_primitive:class_of(Pid)).

pid_as_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         Result = beamtalk_primitive:send(Pid, 'asString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Pid<", _/binary>>, Result)
     end}.

pid_print_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         Result = beamtalk_primitive:send(Pid, 'printString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Pid<", _/binary>>, Result)
     end}.

pid_print_string_matches_as_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         AsString = beamtalk_primitive:send(Pid, 'asString', []),
         PrintString = beamtalk_primitive:send(Pid, 'printString', []),
         ?assertEqual(AsString, PrintString)
     end}.

pid_strict_equality_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         ?assertEqual(true, beamtalk_primitive:send(Pid, '=:=', [Pid])),
         Other = spawn(fun() -> receive stop -> ok end end),
         ?assertEqual(false, beamtalk_primitive:send(Pid, '=:=', [Other])),
         Other ! stop
     end}.

pid_inequality_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         Other = spawn(fun() -> receive stop -> ok end end),
         ?assertEqual(true, beamtalk_primitive:send(Pid, '/=', [Other])),
         ?assertEqual(false, beamtalk_primitive:send(Pid, '/=', [Pid])),
         Other ! stop
     end}.

pid_hash_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         Hash = beamtalk_primitive:send(Pid, 'hash', []),
         ?assert(is_integer(Hash))
     end}.

pid_is_alive_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% Current process is alive
         ?assertEqual(true, beamtalk_primitive:send(self(), 'isAlive', [])),
         %% Dead process
         Pid = spawn(fun() -> ok end),
         timer:sleep(50),
         ?assertEqual(false, beamtalk_primitive:send(Pid, 'isAlive', []))
     end}.

pid_equality_inherited_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         %% ProtoObject == is inherited
         Pid = self(),
         ?assertEqual(true, beamtalk_primitive:send(Pid, '==', [Pid]))
     end}.

pid_responds_to_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'asString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'printString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, '=:=')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'hash')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'isAlive')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, 'class')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Pid, '=='))
     end}.

pid_does_not_understand_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Pid = self(),
         ?assertException(error,
                          #{'$beamtalk_class' := 'RuntimeError',
                            error := #beamtalk_error{kind = does_not_understand,
                                                     class = 'Pid'}},
                          beamtalk_primitive:send(Pid, 'unknownMessage', []))
     end}.

%%% ============================================================================
%%% Port dispatch tests
%%% ============================================================================

port_class_test() ->
    {ok, Port} = gen_tcp:listen(0, []),
    ?assertEqual('Port', beamtalk_primitive:class_of(Port)),
    gen_tcp:close(Port).

port_as_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port} = gen_tcp:listen(0, []),
         Result = beamtalk_primitive:send(Port, 'asString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Port<", _/binary>>, Result),
         gen_tcp:close(Port)
     end}.

port_print_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port} = gen_tcp:listen(0, []),
         Result = beamtalk_primitive:send(Port, 'printString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Port<", _/binary>>, Result),
         gen_tcp:close(Port)
     end}.

port_strict_equality_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port1} = gen_tcp:listen(0, []),
         {ok, Port2} = gen_tcp:listen(0, []),
         ?assertEqual(true, beamtalk_primitive:send(Port1, '=:=', [Port1])),
         ?assertEqual(false, beamtalk_primitive:send(Port1, '=:=', [Port2])),
         gen_tcp:close(Port1),
         gen_tcp:close(Port2)
     end}.

port_hash_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port} = gen_tcp:listen(0, []),
         Hash = beamtalk_primitive:send(Port, 'hash', []),
         ?assert(is_integer(Hash)),
         gen_tcp:close(Port)
     end}.

port_responds_to_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port} = gen_tcp:listen(0, []),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'asString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'printString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, '=:=')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'hash')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, 'class')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Port, '==')),
         gen_tcp:close(Port)
     end}.

port_does_not_understand_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         {ok, Port} = gen_tcp:listen(0, []),
         ?assertException(error,
                          #{'$beamtalk_class' := 'RuntimeError',
                            error := #beamtalk_error{kind = does_not_understand,
                                                     class = 'Port'}},
                          beamtalk_primitive:send(Port, 'unknownMessage', [])),
         gen_tcp:close(Port)
     end}.

%%% ============================================================================
%%% Reference dispatch tests
%%% ============================================================================

reference_class_test() ->
    Ref = make_ref(),
    ?assertEqual('Reference', beamtalk_primitive:class_of(Ref)).

reference_as_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref = make_ref(),
         Result = beamtalk_primitive:send(Ref, 'asString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Ref<", _/binary>>, Result)
     end}.

reference_print_string_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref = make_ref(),
         Result = beamtalk_primitive:send(Ref, 'printString', []),
         ?assert(is_binary(Result)),
         ?assertMatch(<<"#Ref<", _/binary>>, Result)
     end}.

reference_strict_equality_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref1 = make_ref(),
         Ref2 = make_ref(),
         ?assertEqual(true, beamtalk_primitive:send(Ref1, '=:=', [Ref1])),
         ?assertEqual(false, beamtalk_primitive:send(Ref1, '=:=', [Ref2]))
     end}.

reference_hash_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref = make_ref(),
         Hash = beamtalk_primitive:send(Ref, 'hash', []),
         ?assert(is_integer(Hash))
     end}.

reference_responds_to_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref = make_ref(),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'asString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'printString')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, '=:=')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'hash')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, 'class')),
         ?assertEqual(true, beamtalk_primitive:responds_to(Ref, '=='))
     end}.

reference_does_not_understand_test_() ->
    {setup,
     fun() -> beamtalk_extensions:init() end,
     fun(_) -> ok end,
     fun() ->
         Ref = make_ref(),
         ?assertException(error,
                          #{'$beamtalk_class' := 'RuntimeError',
                            error := #beamtalk_error{kind = does_not_understand,
                                                     class = 'Reference'}},
                          beamtalk_primitive:send(Ref, 'unknownMessage', []))
     end}.

%%% ============================================================================
%%% print_string/1 integration tests
%%% ============================================================================

print_string_pid_test() ->
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
