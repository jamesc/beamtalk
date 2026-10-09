%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_abi_tests).

-moduledoc """
Workspace-level tests for structured load refusals on the REPL reload path
(BT-3773, split from BT-3722): `beamtalk_repl_loader:install_reload_result/2`
and the pending-load-error table it drains.

* A refusal parked by a load that nothing drained (`beamtalk_module_activation`,
  code-server autoload) must not be blamed on a later, unrelated failure of the
  same class: every loader clears the class's parked entries immediately before
  its own `code:load_binary/3`, so a drain only sees errors from its own attempt.
* A module whose `-on_load` gate refuses on a stale `class_var_abi` surfaces as
  a structured `abi_mismatch` from `install_reload_result/2`, not as
  `{load_error, on_load_failure}`.

The compiler always emits the current ABI, so the stale-ABI module is
hand-assembled at test time exactly as an older compiler shaped it: a
`__beamtalk_meta/0` without `class_var_abi` and an `-on_load` hook that
registers through the ClassBuilder.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_class_registry:ensure_pending_errors_table(),
    ok.

teardown(_) ->
    lists:foreach(
        fun(Name) ->
            case beamtalk_class_registry:whereis_class(Name) of
                undefined ->
                    ok;
                Pid ->
                    try
                        gen_server:stop(Pid, normal, 5000)
                    catch
                        _:_ -> ok
                    end
            end
        end,
        ['BT3773StaleAbi']
    ),
    lists:foreach(
        fun(Mod) ->
            code:purge(Mod),
            code:delete(Mod),
            code:purge(Mod)
        end,
        ['bt@bt3773_stale_abi', 'bt3773_real_module']
    ),
    ets:delete_all_objects(beamtalk_pending_load_errors),
    ok.

%% A module with no `class_var_abi` in its meta and an on_load registration
%% hook (what an older compiler emitted).
stale_abi_binary() ->
    compile_source(
        "-module('bt@bt3773_stale_abi').\n"
        "-on_load(register_class/0).\n"
        "-export(['__beamtalk_meta'/0, register_class/0]).\n"
        "'__beamtalk_meta'() ->\n"
        "    #{class => 'BT3773StaleAbi', superclass => 'Object', kind => object}.\n"
        "register_class() ->\n"
        "    case beamtalk_class_builder:register(#{className => 'BT3773StaleAbi',\n"
        "                                         superclassRef => 'Object',\n"
        "                                         moduleName => 'bt@bt3773_stale_abi',\n"
        "                                         meta => '__beamtalk_meta'()}) of\n"
        "        {ok, _Pid} -> ok;\n"
        "        {error, _} = Err -> Err\n"
        "    end.\n"
    ).

%% A well-formed module, loaded under the wrong name so `code:load_binary/3`
%% fails with `badfile` — a load failure with no parked structured detail.
badfile_binary() ->
    compile_source(
        "-module(bt3773_real_module).\n"
        "-export([f/0]).\n"
        "f() -> ok.\n"
    ).

compile_source(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = split_forms(Tokens, [], []),
    {ok, _Mod, Binary} = compile:forms(Forms, [binary, return_errors]),
    Binary.

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, _} = Dot | Rest], Cur, Acc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([Dot | Cur])),
    split_forms(Rest, [], [Form | Acc]);
split_forms([Tok | Rest], Cur, Acc) ->
    split_forms(Rest, [Tok | Cur], Acc).

abi_error(ClassName) ->
    beamtalk_error:with_hint(
        beamtalk_error:new(abi_mismatch, ClassName), <<"Recompile with the current compiler.">>
    ).

classes(Name) ->
    [#{name => atom_to_list(Name), superclass => "Object"}].

%%====================================================================
%% Stale parked entries
%%====================================================================

stale_entries_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a stale parked abi_mismatch is not blamed on an unrelated load failure", fun() ->
                ok = beamtalk_class_registry:record_pending_load_error(
                    'BT3773StaleAbi', abi_error('BT3773StaleAbi')
                ),
                Result = beamtalk_repl_loader:install_reload_result(
                    {ok, compiled, badfile_binary(), classes('BT3773StaleAbi'),
                        'bt3773_wrong_name'},
                    "bt3773.bt"
                ),
                ?assertMatch({error, {load_error, badfile}}, Result)
            end},
            {"a stale parked stdlib_shadowing is not blamed on an unrelated load failure", fun() ->
                ok = beamtalk_class_registry:record_pending_load_error(
                    'BT3773StaleAbi', beamtalk_error:new(stdlib_shadowing, 'BT3773StaleAbi')
                ),
                Result = beamtalk_repl_loader:install_reload_result(
                    {ok, compiled, badfile_binary(), classes('BT3773StaleAbi'),
                        'bt3773_wrong_name'},
                    "bt3773.bt"
                ),
                ?assertMatch({error, {load_error, badfile}}, Result)
            end},
            {"clear_pending_load_errors_by_names/1 removes only the named classes", fun() ->
                ok = beamtalk_class_registry:record_pending_load_error(
                    'BT3773StaleAbi', abi_error('BT3773StaleAbi')
                ),
                ok = beamtalk_class_registry:record_pending_load_error(
                    'BT3773Other', abi_error('BT3773Other')
                ),
                ok = beamtalk_class_registry:clear_pending_load_errors_by_names(['BT3773StaleAbi']),
                ?assertEqual(
                    [],
                    beamtalk_class_registry:drain_pending_load_errors_by_names(['BT3773StaleAbi'])
                ),
                ?assertMatch(
                    [{'BT3773Other', #beamtalk_error{kind = abi_mismatch}}],
                    beamtalk_class_registry:drain_pending_load_errors_by_names(['BT3773Other'])
                )
            end}
        ]
    end}.

%%====================================================================
%% Stale-ABI module through install_reload_result/2
%%====================================================================

stale_abi_reload_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a stale-ABI module reloads as a structured abi_mismatch, not on_load_failure",
                fun() ->
                    Result = beamtalk_repl_loader:install_reload_result(
                        {ok, compiled, stale_abi_binary(), classes('BT3773StaleAbi'),
                            'bt@bt3773_stale_abi'},
                        "bt3773_stale.bt"
                    ),
                    ?assertMatch(
                        {error, #beamtalk_error{
                            kind = abi_mismatch,
                            class = 'BT3773StaleAbi',
                            hint = <<"Recompile", _/binary>>
                        }},
                        Result
                    ),
                    %% Drained by this attempt: nothing left parked.
                    ?assertEqual(
                        [],
                        beamtalk_class_registry:drain_pending_load_errors_by_names([
                            'BT3773StaleAbi'
                        ])
                    )
                end}
        ]
    end}.
