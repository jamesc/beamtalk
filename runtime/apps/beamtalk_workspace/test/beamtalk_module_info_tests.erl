%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_module_info_tests).

-moduledoc """
BT-4067: every Beamtalk-compiled module exports `module_info/0,1`, so the
OTP tools that call `M:module_info/0,1` work on a node running Beamtalk.

Covers a stdlib module (`bt@stdlib@*`), a package-qualified class module
compiled here through the real compiler port and `compile_core_erlang/1`
(the same path a package build and the REPL loader use), and a `tprof`
profile and an `eprof` analysis over a Beamtalk workload. Before BT-4067
both crashed with `undef` on `M:module_info(functions)` for a `bt@` module.
""".

-include_lib("eunit/include/eunit.hrl").

-define(PKG_MODULE, 'bt@bt4067_pkg@bt4067_widget').
-define(PKG_SOURCE, <<
    "Object subclass: Bt4067Widget\n"
    "  answer => 6 * 7\n"
>>).

setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    ok = beamtalk_test_boot:boot_real_stdlib('Object'),
    {ok, Result} = beamtalk_compiler:compile(
        ?PKG_SOURCE, #{module_name => atom_to_binary(?PKG_MODULE)}
    ),
    {ok, ?PKG_MODULE, Binary} =
        beamtalk_compiler:compile_core_erlang(maps:get(core_erlang, Result)),
    {module, ?PKG_MODULE} = code:load_binary(?PKG_MODULE, "bt4067_widget.beam", Binary),
    ok = beamtalk_test_boot:wait_for_class('Bt4067Widget', 50),
    ok.

%% The uniquely-named class and its module stay loaded, like the compiled
%% fixtures other suites load. Stopping the class process and deleting its
%% module left stale class-index state that made a later suite's
%% `beamtalk_repl_loader:precheck_class_shape/2` find nothing.
teardown(_) ->
    ok.

module_info_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        {"stdlib module answers module_info/0,1", fun stdlib_module_info/0},
        {"package module answers module_info/0,1", fun package_module_info/0},
        {"tprof profiles a Beamtalk workload", fun tprof_profiles_workload/0},
        {"eprof analyses a Beamtalk workload", fun eprof_analyses_workload/0}
    ]}.

stdlib_module_info() ->
    assert_module_info('bt@stdlib@list').

package_module_info() ->
    assert_module_info(?PKG_MODULE),
    Functions = ?PKG_MODULE:module_info(functions),
    ?assert(lists:member({answer, 1}, Functions)).

assert_module_info(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    Functions = Module:module_info(functions),
    ?assert(lists:member({module_info, 0}, Functions)),
    ?assert(lists:member({module_info, 1}, Functions)),
    ?assertEqual(Module, Module:module_info(module)),
    ?assertEqual(Module, proplists:get_value(module, Module:module_info())),
    ?assert(lists:member({module_info, 1}, Module:module_info(exports))).

workload() ->
    Widget = ?PKG_MODULE:new(),
    [beamtalk_message_dispatch:send(Widget, answer, []) || _ <- lists:seq(1, 50)].

%% eprof:analyze/2 calls M:module_info(functions) on every traced module.
eprof_analyses_workload() ->
    {ok, _} = eprof:start(),
    try
        {ok, Answers} = eprof:profile([], fun workload/0, {?PKG_MODULE, '_', '_'}),
        ?assertEqual(lists:duplicate(50, 42), Answers),
        ?assertEqual(ok, eprof:analyze(total))
    after
        eprof:stop()
    end.

tprof_profiles_workload() ->
    Workload = fun workload/0,
    %% Trace only Beamtalk-compiled modules: tprof's collect step calls
    %% M:module_info(functions) on every traced module, which is what crashed
    %% before BT-4067. Tracing every function on the node would also slow
    %% and perturb the rest of the suite.
    Pattern = [{?PKG_MODULE, '_', '_'}, {'bt@stdlib@list', '_', '_'}],
    {Answers, {call_count, Profile}} =
        tprof:profile(Workload, #{type => call_count, report => return, pattern => Pattern}),
    ?assertEqual(lists:duplicate(50, 42), Answers),
    Profiled = [M || {M, _F, _A, _Calls} <- Profile],
    ?assert(lists:member(?PKG_MODULE, Profiled)).
