%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_var_abi_tests).
-moduledoc """
EUnit tests for the `class_var_abi` load gate (ADR 0130 Phase 3, last item):
`beamtalk_class_var_abi:check_class_info_abi/2`, enforced at class registration
(`beamtalk_object_class:start/2`, including through a module's `-on_load`
hook), at hot reload (`beamtalk_object_class:update_class/2`) and by the
release preflight (`beamtalk_release_shapes:extract_shapes/2`).

The "pre-change" modules are compiled at test time exactly as an older
compiler shaped them: a `__beamtalk_meta/0` with no `class_var_abi` entry (or
a different value) and an `-on_load` hook that registers through the
ClassBuilder with the meta literal.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

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
    %% Owned by this setup process, not by the short-lived `-on_load` hook
    %% process that would otherwise create it on the first parked refusal
    %% (and take it down when that process exits, before the test drains it).
    %% Created without an heir and deleted in teardown, so it is never handed
    %% to `beamtalk_runtime_sup` (which logs an unexpected 'ETS-TRANSFER').
    ensure_pending_errors_table().

ensure_pending_errors_table() ->
    case ets:info(beamtalk_pending_load_errors) of
        undefined ->
            _ = ets:new(beamtalk_pending_load_errors, [set, public, named_table]),
            created;
        _ ->
            existing
    end.

teardown(PendingTable) ->
    case PendingTable of
        created -> ets:delete(beamtalk_pending_load_errors);
        existing -> ok
    end,
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
        class_names()
    ),
    lists:foreach(
        fun(Mod) ->
            code:purge(Mod),
            code:delete(Mod),
            code:purge(Mod)
        end,
        modules()
    ).

class_names() ->
    ['BT3713AbiOldMissing', 'BT3713AbiOldZero', 'BT3713AbiCurrent', 'BT3713AbiHot', 'BT3713AbiErl'] ++
        ['BT3722AbiPending', 'BT3722AbiCollected'].

modules() ->
    [
        'bt@bt3713_abi_old_missing',
        'bt@bt3713_abi_old_zero',
        'bt@bt3713_abi_current',
        'bt@bt3713_abi_hot_old',
        'bt3713_abi_erlang_class',
        'bt@bt3722_abi_pending',
        'bt@bt3722_abi_collected'
    ].

%% The abstract forms of a compiled-class-shaped module. `AbiEntry` is the
%% source of the `class_var_abi` map entry (empty for a pre-change module).
%% `OnLoad = true` adds the `-on_load` registration hook a compiled class has.
compiled_class_source(Module, ClassName, AbiEntry, OnLoad) ->
    OnLoadAttr =
        case OnLoad of
            true -> "-on_load(register_class/0).\n";
            false -> ""
        end,
    lists:flatten(
        io_lib:format(
            "-module('~s').\n"
            "~s"
            "-export(['__beamtalk_meta'/0, register_class/0]).\n"
            "'__beamtalk_meta'() ->\n"
            "    #{class => '~s', superclass => 'Object', kind => object~s}.\n"
            "register_class() ->\n"
            "    case beamtalk_class_builder:register(#{className => '~s',\n"
            "                                         superclassRef => 'Object',\n"
            "                                         moduleName => '~s',\n"
            "                                         meta => '__beamtalk_meta'()}) of\n"
            "        {ok, _Pid} -> ok;\n"
            "        {error, _} = Err -> Err\n"
            "    end.\n",
            [Module, OnLoadAttr, ClassName, AbiEntry, ClassName, Module]
        )
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

load(Module, Binary) ->
    code:load_binary(Module, atom_to_list(Module) ++ ".beam", Binary).

old_binary(Module, ClassName, AbiEntry) ->
    compile_source(compiled_class_source(Module, ClassName, AbiEntry, true)).

%%====================================================================
%% Registration (the module's -on_load hook)
%%====================================================================

registration_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a pre-change module (no class_var_abi) is refused at load", fun() ->
                Mod = 'bt@bt3713_abi_old_missing',
                Bin = old_binary(Mod, 'BT3713AbiOldMissing', ""),
                ?assertMatch({error, on_load_failure}, load(Mod, Bin)),
                ?assertEqual(
                    undefined, beamtalk_class_registry:whereis_class('BT3713AbiOldMissing')
                )
            end},
            {"a module with a different class_var_abi (0) is refused at load", fun() ->
                Mod = 'bt@bt3713_abi_old_zero',
                Bin = old_binary(Mod, 'BT3713AbiOldZero', ", class_var_abi => 0"),
                ?assertMatch({error, on_load_failure}, load(Mod, Bin)),
                ?assertEqual(undefined, beamtalk_class_registry:whereis_class('BT3713AbiOldZero'))
            end},
            {"the refusal is parked for the REPL/CLI loaders as a structured abi_mismatch (BT-3722)",
                fun() ->
                    Mod = 'bt@bt3722_abi_pending',
                    Bin = old_binary(Mod, 'BT3722AbiPending', ", class_var_abi => 0"),
                    ?assertMatch({error, on_load_failure}, load(Mod, Bin)),
                    ?assertMatch(
                        [
                            {'BT3722AbiPending', #beamtalk_error{
                                kind = abi_mismatch,
                                class = 'BT3722AbiPending',
                                details = #{expected := _, found := 0},
                                hint = <<"Recompile", _/binary>>
                            }}
                        ],
                        beamtalk_class_registry:drain_pending_load_errors_by_names([
                            'BT3722AbiPending'
                        ])
                    ),
                    %% Drained exactly once.
                    ?assertEqual(
                        [],
                        beamtalk_class_registry:drain_pending_load_errors_by_names([
                            'BT3722AbiPending'
                        ])
                    )
                end},
            {"a refusal collected for a release preflight is not parked (BT-3722)", fun() ->
                Mod = 'bt@bt3722_abi_collected',
                Bin = old_binary(Mod, 'BT3722AbiCollected', ""),
                {Result, Refusals} = beamtalk_class_var_abi:collect_abi_refusals(fun() ->
                    load(Mod, Bin)
                end),
                ?assertMatch({error, on_load_failure}, Result),
                ?assertMatch([{Mod, #beamtalk_error{kind = abi_mismatch}}], Refusals),
                ?assertEqual(
                    [],
                    beamtalk_class_registry:drain_pending_load_errors_by_names([
                        'BT3722AbiCollected'
                    ])
                )
            end},
            {"a module with the current class_var_abi registers", fun() ->
                Mod = 'bt@bt3713_abi_current',
                Abi = lists:flatten(
                    io_lib:format(", class_var_abi => ~p", [beamtalk_class_var_abi:abi()])
                ),
                Bin = old_binary(Mod, 'BT3713AbiCurrent', Abi),
                ?assertMatch({module, Mod}, load(Mod, Bin)),
                ?assert(is_pid(beamtalk_class_registry:whereis_class('BT3713AbiCurrent')))
            end}
        ]
    end}.

%%====================================================================
%% The structured error, through the public registration API
%%====================================================================

refusal_error_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"abi_mismatch names the module and says to recompile", fun() ->
                Mod = 'bt@bt3713_abi_old_missing',
                %% No -on_load here: loaded and queried like a module that
                %% finished loading (the hot-reload shape).
                Bin = compile_source(
                    compiled_class_source(Mod, 'BT3713AbiOldMissing', "", false)
                ),
                {module, Mod} = load(Mod, Bin),
                {error, Error} = beamtalk_object_class:start(
                    'BT3713AbiOldMissing',
                    #{
                        name => 'BT3713AbiOldMissing',
                        module => Mod,
                        superclass => none,
                        instance_methods => #{},
                        class_methods => #{}
                    }
                ),
                ?assertMatch(
                    #beamtalk_error{kind = abi_mismatch, class = 'BT3713AbiOldMissing'}, Error
                ),
                ?assertNotEqual(
                    nomatch, binary:match(Error#beamtalk_error.message, atom_to_binary(Mod, utf8))
                ),
                ?assertNotEqual(nomatch, binary:match(Error#beamtalk_error.hint, <<"Recompile">>)),
                ?assertEqual(
                    #{module => Mod, expected => beamtalk_class_var_abi:abi(), found => missing},
                    Error#beamtalk_error.details
                ),
                ?assertEqual(
                    undefined, beamtalk_class_registry:whereis_class('BT3713AbiOldMissing')
                )
            end},
            {"the meta map in ClassInfo is checked while the module is still loading", fun() ->
                %% During -on_load erlang:function_exported/3 is false, so the
                %% compiler-emitted meta literal in ClassInfo is the only source.
                Meta = #{class => 'BT3713AbiOldMissing', superclass => 'Object'},
                ?assertMatch(
                    {error, #beamtalk_error{kind = abi_mismatch}},
                    beamtalk_class_var_abi:check_class_info_abi(
                        'BT3713AbiOldMissing', #{module => not_loaded_yet, meta => Meta}
                    )
                ),
                ?assertEqual(
                    ok,
                    beamtalk_class_var_abi:check_class_info_abi(
                        'BT3713AbiOldMissing',
                        #{
                            module => not_loaded_yet,
                            meta => Meta#{class_var_abi => beamtalk_class_var_abi:abi()}
                        }
                    )
                ),
                %% "Equal to the current value", never "present and unequal".
                ?assertMatch(
                    {error, #beamtalk_error{kind = abi_mismatch}},
                    beamtalk_class_var_abi:check_class_info_abi(
                        'BT3713AbiOldMissing',
                        #{
                            module => not_loaded_yet,
                            meta => Meta#{class_var_abi => beamtalk_class_var_abi:abi() + 1}
                        }
                    )
                )
            end}
        ]
    end}.

%%====================================================================
%% Invalid __beamtalk_meta/0 and refusal collection (BT-3726)
%%====================================================================

invalid_meta_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a crashing __beamtalk_meta/0 is refused as invalid_meta, not 'compiled before'",
                fun() ->
                    Mod = 'bt3726_crashing_meta',
                    Bin = compile_source(
                        "-module('bt3726_crashing_meta').\n"
                        "-export(['__beamtalk_meta'/0]).\n"
                        "'__beamtalk_meta'() -> erlang:error(boom).\n"
                    ),
                    {module, Mod} = load(Mod, Bin),
                    {error, Error} = beamtalk_class_var_abi:check_class_info_abi(
                        'BT3726Crash', #{module => Mod}
                    ),
                    ?assertEqual(abi_mismatch, Error#beamtalk_error.kind),
                    ?assertMatch(
                        #{found := invalid_meta}, Error#beamtalk_error.details
                    ),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Error#beamtalk_error.message, <<"invalid __beamtalk_meta/0">>)
                    ),
                    ?assertEqual(
                        nomatch,
                        binary:match(Error#beamtalk_error.message, <<"compiled before">>)
                    )
                end},
            {"a non-map __beamtalk_meta/0 is refused as invalid_meta", fun() ->
                Mod = 'bt3726_nonmap_meta',
                Bin = compile_source(
                    "-module('bt3726_nonmap_meta').\n"
                    "-export(['__beamtalk_meta'/0]).\n"
                    "'__beamtalk_meta'() -> not_a_map.\n"
                ),
                {module, Mod} = load(Mod, Bin),
                ?assertMatch(
                    {error, #beamtalk_error{details = #{found := invalid_meta}}},
                    beamtalk_class_var_abi:check_class_info_abi('BT3726NonMap', #{module => Mod})
                )
            end}
        ]
    end}.

collect_abi_refusals_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"collects refusals and releases the collector", fun() ->
                Meta = #{class => 'BT3726Coll', superclass => 'Object'},
                {Result, Refusals} = beamtalk_class_var_abi:collect_abi_refusals(fun() ->
                    _ = beamtalk_class_var_abi:check_class_info_abi(
                        'BT3726Coll', #{module => bt3726_coll, meta => Meta}
                    ),
                    done
                end),
                ?assertEqual(done, Result),
                ?assertMatch([{bt3726_coll, #beamtalk_error{kind = abi_mismatch}}], Refusals),
                ?assertEqual(undefined, ets:whereis(beamtalk_abi_refusals))
            end},
            {"a leftover/concurrent collection raises instead of blaming it", fun() ->
                {{ok, ok}, []} = beamtalk_class_var_abi:collect_abi_refusals(fun() ->
                    ?assertError(
                        abi_collection_in_progress,
                        beamtalk_class_var_abi:collect_abi_refusals(fun() -> ok end)
                    ),
                    {ok, ok}
                end),
                ?assertEqual(undefined, ets:whereis(beamtalk_abi_refusals))
            end},
            {"the collector is released when its owner is killed", fun() ->
                Self = self(),
                Pid = spawn(fun() ->
                    beamtalk_class_var_abi:collect_abi_refusals(fun() ->
                        Self ! collecting,
                        receive
                            never -> ok
                        end
                    end)
                end),
                receive
                    collecting -> ok
                after 5000 -> error(timeout_waiting_for_collector)
                end,
                ?assertNotEqual(undefined, ets:whereis(beamtalk_abi_refusals)),
                Ref = monitor(process, Pid),
                exit(Pid, kill),
                receive
                    {'DOWN', Ref, process, Pid, killed} -> ok
                after 5000 -> error(timeout_waiting_for_collector_owner_down)
                end,
                ?assertEqual(undefined, ets:whereis(beamtalk_abi_refusals)),
                ?assertMatch({ok, []}, beamtalk_class_var_abi:collect_abi_refusals(fun() -> ok end))
            end},
            {"the collector is released when Fun crashes", fun() ->
                ?assertError(
                    boom,
                    beamtalk_class_var_abi:collect_abi_refusals(fun() -> erlang:error(boom) end)
                ),
                ?assertEqual(undefined, ets:whereis(beamtalk_abi_refusals))
            end}
        ]
    end}.

%%====================================================================
%% Modules outside the gate: no __beamtalk_meta/0
%%====================================================================

metadata_less_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"a metadata-less Erlang class module still registers", fun() ->
                Mod = 'bt3713_abi_erlang_class',
                Bin = compile_source(
                    "-module('bt3713_abi_erlang_class').\n"
                    "-export([class_ping/1]).\n"
                    "class_ping(_ClassSelf) -> pong.\n"
                ),
                {module, Mod} = load(Mod, Bin),
                ?assertNot(erlang:function_exported(Mod, '__beamtalk_meta', 0)),
                {ok, Pid} = beamtalk_object_class:start(
                    'BT3713AbiErl',
                    #{
                        name => 'BT3713AbiErl',
                        module => Mod,
                        superclass => none,
                        instance_methods => #{},
                        class_methods => #{}
                    }
                ),
                ?assert(is_pid(Pid))
            end},
            {"a ClassInfo with neither a module nor a meta map is outside the gate", fun() ->
                ?assertEqual(
                    ok,
                    beamtalk_class_var_abi:check_class_info_abi('NoModule', #{name => 'NoModule'})
                ),
                %% A hand-built meta map without the compiler's `class` key is not
                %% compiler metadata.
                ?assertEqual(
                    ok,
                    beamtalk_class_var_abi:check_class_info_abi(
                        'NoModule', #{module => 'NoModule', meta => #{backing_module => x}}
                    )
                )
            end}
        ]
    end}.

%%====================================================================
%% Hot reload
%%====================================================================

hot_reload_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"update_class refuses a pre-change module and keeps the class", fun() ->
                Current = 'bt3713_abi_erlang_class',
                {module, Current} = load(
                    Current,
                    compile_source(
                        "-module('bt3713_abi_erlang_class').\n"
                        "-export([class_ping/1]).\n"
                        "class_ping(_ClassSelf) -> pong.\n"
                    )
                ),
                {ok, Pid} = beamtalk_object_class:start(
                    'BT3713AbiHot',
                    #{
                        name => 'BT3713AbiHot',
                        module => Current,
                        superclass => none,
                        instance_methods => #{},
                        class_methods => #{}
                    }
                ),
                Old = 'bt@bt3713_abi_hot_old',
                {module, Old} = load(
                    Old,
                    compile_source(compiled_class_source(Old, 'BT3713AbiHot', "", false))
                ),
                ?assertMatch(
                    {error, #beamtalk_error{kind = abi_mismatch, class = 'BT3713AbiHot'}},
                    beamtalk_object_class:update_class(
                        'BT3713AbiHot',
                        #{module => Old, instance_methods => #{}, class_methods => #{}}
                    )
                ),
                ?assertEqual(Current, beamtalk_object_class:module_name(Pid))
            end}
        ]
    end}.

%%====================================================================
%% The one source of the expected value
%%====================================================================

abi_single_source_test() ->
    %% The compiler emits `class_var_abi` from the Rust `ABI_VERSION`; the runtime
    %% accepts `?BT_CLASS_VAR_ABI` from the header generated from that same
    %% constant. A module compiled by the real compiler (the shape fixtures,
    %% built by `compile_fixtures.escript`) must therefore carry exactly the
    %% value the gate requires.
    Mod = 'bt@release_shapes_root',
    {module, Mod} = code:ensure_loaded(Mod),
    #{class_var_abi := Emitted} = Mod:'__beamtalk_meta'(),
    ?assertEqual(beamtalk_class_var_abi:abi(), Emitted).

%%====================================================================
%% The release preflight (extract_shapes/2)
%%====================================================================

preflight_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"extract_shapes fails on a module compiled for a different ABI", fun() ->
                Dir = scratch_dir("abi_preflight"),
                try
                    Mod = 'bt@bt3713_abi_old_missing',
                    ok = file:write_file(
                        filename:join(Dir, atom_to_list(Mod) ++ ".beam"),
                        old_binary(Mod, 'BT3713AbiOldMissing', "")
                    ),
                    Result = beamtalk_release_shapes:extract_shapes([], [Dir]),
                    ?assertMatch({error, {abi_mismatch, [_]}}, Result),
                    {error, {abi_mismatch, [Message]}} = Result,
                    ?assertNotEqual(nomatch, binary:match(Message, atom_to_binary(Mod, utf8))),
                    ?assertNotEqual(nomatch, binary:match(Message, <<"Recompile">>)),
                    %% The refusals table is gone again.
                    ?assertEqual(undefined, ets:whereis(beamtalk_abi_refusals))
                after
                    file:del_dir_r(Dir)
                end
            end}
        ]
    end}.

scratch_dir(Name) ->
    Dir = filename:join(
        binary_to_list(beamtalk_file:'tempDirectory'()),
        Name ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = filelib:ensure_path(Dir),
    Dir.
