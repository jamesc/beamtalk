%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_native_api_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4556: `beamtalk_native_api`, the supported entry point for package native
Erlang code to send messages to Beamtalk objects. Sends to a compiled
class-side method and to instance methods, String and Result round trips,
where a class-side send runs (in the caller for a class with no class
variables, in the class process otherwise), and `send/4`'s timeout on the
class-process path.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

-define(API, beamtalk_native_api).
-define(HELPER, beamtalk_class_dispatch_test_helper).

-define(METHODS, #{
    testSelf => #{arity => 0},
    testClassSelf => #{arity => 0},
    'testWith:' => #{arity => 1},
    'testSleep:' => #{arity => 1}
}).

%% The generated process-bound list every compiled class carries (ADR 0142 §1
%% rule 3).
-define(META, #{process_bound_class_selectors => [new, spawn]}).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

with_class(Name, ClassVars, Fun) ->
    ClassInfo = #{
        superclass => none,
        module => ?HELPER,
        class_methods => ?METHODS,
        class_state => ClassVars,
        meta => ?META
    },
    {ok, Pid} = beamtalk_object_class:start_link(Name, ClassInfo),
    unlink(Pid),
    try
        {ok, Class} = ?API:class_named(Name),
        Fun(Class, Pid)
    after
        try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> ok
        end
    end.

error_kind(Fun) ->
    try Fun() of
        V -> {returned, V}
    catch
        error:#{'$beamtalk_class' := _, error := #beamtalk_error{kind = Kind}} -> Kind
    end.

native_api_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"class_named answers the class object a class reference evaluates to", fun() ->
            {ok, Class} = ?API:class_named('Result'),
            ?assertEqual(beamtalk_class_registry:resolve_class_object('Result'), Class),
            ?assertMatch(#beamtalk_object{class = 'Result class'}, Class)
        end},
        {"class_named of an unloaded class is a class_not_found error", fun() ->
            ?assertMatch(
                {error, #beamtalk_error{kind = class_not_found, class = 'Bt4556NoSuchClass'}},
                ?API:class_named('Bt4556NoSuchClass')
            )
        end},
        {"a send to a compiled class-side method answers its result", fun() ->
            {ok, ResultClass} = ?API:class_named('Result'),
            Ok = ?API:send(ResultClass, 'ok:', [42]),
            ?assertEqual({ok, 42}, ?API:result_to_tuple(Ok)),
            ?assertEqual(42, ?API:send(Ok, unwrap, []))
        end},
        {"a send to an instance method answers its result", fun() ->
            Str = ?API:string_from_binary(<<"héllo"/utf8>>),
            ?assertEqual(5, ?API:send(Str, size, [])),
            ?assertEqual(
                <<"olléh"/utf8>>, ?API:string_to_binary(?API:send(Str, reverse, []))
            ),
            ?assertEqual(7, ?API:send(3, '+', [4]))
        end},
        {"a send/4 to an instance method answers its result", fun() ->
            ?assertEqual(7, ?API:send(3, '+', [4], 1000))
        end},
        {"a failing send raises the Beamtalk error", fun() ->
            ?assertEqual(
                does_not_understand, error_kind(fun() -> ?API:send(3, bt4556NoSuchSelector, []) end)
            )
        end},
        {"String round-trips through binary", fun() ->
            Bin = <<"日本語 text"/utf8>>,
            Str = ?API:string_from_binary(Bin),
            ?assertEqual({beamtalk_string, Bin}, Str),
            ?assertEqual(Bin, ?API:string_to_binary(Str))
        end},
        {"string_from_binary rejects bytes that are not UTF-8", fun() ->
            ?assertEqual(type_error, error_kind(fun() -> ?API:string_from_binary(<<255, 0>>) end))
        end},
        {"string_to_binary rejects a bare binary", fun() ->
            ?assertEqual(type_error, error_kind(fun() -> ?API:string_to_binary(<<"x">>) end))
        end},
        {"an ok Result round-trips through a tuple", fun() ->
            R = ?API:result_from_tuple({ok, 42}),
            ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := true, 'okValue' := 42}, R),
            ?assertEqual({ok, 42}, ?API:result_to_tuple(R)),
            ?assertEqual(R, ?API:result_from_tuple(?API:result_to_tuple(R)))
        end},
        {"an error Result round-trips through a tuple", fun() ->
            Err = beamtalk_error:new(io_error, 'File', 'read:'),
            R = ?API:result_from_tuple({error, Err}),
            ?assertMatch(#{'$beamtalk_class' := 'Result', 'isOk' := false}, R),
            {error, Reason} = ?API:result_to_tuple(R),
            ?assertBtErrorValue(io_error, _, _, Reason),
            ?assertEqual(R, ?API:result_from_tuple(?API:result_to_tuple(R)))
        end},
        {"result_to_tuple rejects a non-Result", fun() ->
            ?assertEqual(type_error, error_kind(fun() -> ?API:result_to_tuple(42) end))
        end},
        {"a class with no class variables runs its class method in the caller", fun() ->
            with_class('Bt4556Stateless', #{}, fun(Class, _Pid) ->
                ?assertEqual(self(), ?API:send(Class, testSelf, [])),
                ?assertEqual({with_arg, 7}, ?API:send(Class, 'testWith:', [7])),
                %% The method's `self` is the class object the caller holds.
                ?assertEqual(Class, ?API:send(Class, testClassSelf, []))
            end)
        end},
        {"a class with class variables runs its class method in the class process", fun() ->
            with_class('Bt4556Stateful', #{count => 0}, fun(Class, Pid) ->
                ?assertEqual(Pid, ?API:send(Class, testSelf, [])),
                ?assertEqual({with_arg, 7}, ?API:send(Class, 'testWith:', [7]))
            end)
        end},
        {"send/4 bounds the wait for the class process", fun() ->
            with_class('Bt4556Slow', #{count => 0}, fun(Class, Pid) ->
                ?assertEqual(Pid, ?API:send(Class, 'testSleep:', [10], 5000)),
                ?assertEqual(
                    timeout,
                    error_kind(fun() -> ?API:send(Class, 'testSleep:', [500], 50) end)
                )
            end)
        end},
        {"send/4 does not bound a class method that runs in the caller", fun() ->
            with_class('Bt4556Fast', #{}, fun(Class, _Pid) ->
                ?assertEqual(self(), ?API:send(Class, 'testSleep:', [100], 10))
            end)
        end}
    ]}.
