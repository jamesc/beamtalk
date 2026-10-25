%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_otp_profiler_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
OTP profilers keep working in a Beamtalk node (BT-4106).

`eprof` calls `M:module_info(functions)` on every loaded module during
analysis (`eprof:collect_mfas/1`), and `tprof` walks loaded modules the
same way, so any loaded Beamtalk module without `module_info/0,1` (the
pre-BT-4067 codegen shape) crashes both profilers — `tprof` answers
`badarg` and `eprof` dies with `undef M:module_info/1`. These tests pin
the issue's repro: with Beamtalk modules loaded, `tprof` and `eprof`
profile both plain Erlang calls and Beamtalk dispatch calls.

Result shapes are matched loosely (profiled return value exactly, the
report itself opaquely): the report format is OTP's business and differs
across the OTP 27/28 support window — what this suite guards is that
profiling a function in a Beamtalk node succeeds at all.
""".

-include_lib("eunit/include/eunit.hrl").

%% The exact module from the BT-4106 report.
-define(ISSUE_MODULE, 'bt@stdlib@stack_frame').

%% The issue's tprof repro, verbatim.
tprof_profiles_erlang_call_with_beamtalk_loaded_test() ->
    ensure_beamtalk_loaded(),
    {Result, _Report} = tprof:profile(
        lists, seq, [1, 10], #{type => call_time, report => return}
    ),
    ?assertEqual(lists:seq(1, 10), Result).

%% The issue's eprof repro, verbatim.
eprof_profiles_erlang_call_with_beamtalk_loaded_test() ->
    ensure_beamtalk_loaded(),
    ?assertMatch(
        {ok, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]},
        eprof:profile([self()], fun() -> lists:seq(1, 10) end)
    ).

%% Profiling Beamtalk code itself (the Buccaneer spike's use case): the
%% profiled MFA lives in a Beamtalk-compiled module.
tprof_profiles_beamtalk_dispatch_test() ->
    ensure_beamtalk_loaded(),
    {Class, _Report} = tprof:profile(
        'bt@stdlib@integer',
        dispatch,
        ['class', [], 5],
        #{type => call_time, report => return}
    ),
    ?assertEqual('Integer', Class).

eprof_profiles_beamtalk_dispatch_test() ->
    ensure_beamtalk_loaded(),
    ?assertMatch(
        {ok, 'Integer'},
        eprof:profile(
            [self()],
            fun() -> 'bt@stdlib@integer':dispatch('class', [], 5) end
        )
    ).

%% Every loaded Beamtalk module answers module_info/0,1: BT-4067's codegen
%% guarantee, asserted at the level the profilers consume it.
loaded_beamtalk_modules_answer_module_info_test() ->
    ensure_beamtalk_loaded(),
    BtLoaded = [
        M
     || {M, _} <- code:all_loaded(),
        lists:prefix("bt@", atom_to_list(M))
    ],
    ?assert(length(BtLoaded) > 0),
    lists:foreach(
        fun(M) ->
            Exports = M:module_info(exports),
            ?assert(lists:member({module_info, 0}, Exports)),
            ?assert(lists:member({module_info, 1}, Exports))
        end,
        BtLoaded
    ).

ensure_beamtalk_loaded() ->
    {module, ?ISSUE_MODULE} = code:ensure_loaded(?ISSUE_MODULE),
    {module, 'bt@stdlib@integer'} = code:ensure_loaded('bt@stdlib@integer'),
    ok.
