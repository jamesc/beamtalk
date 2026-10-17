%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_threaded_dispatch_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for the ADR 0132 Phase 0 runtime floor (BT-3868).

- `beamtalk_dispatch:call_threaded/3`, the per-invocation tier gate.
- The `tier2_methods` / `tier2_class_methods` metadata cache and its accessors
  `beamtalk_class_dispatch:lookup_threaded_entry/3` and
  `lookup_threaded_class_entry/3` (read at registration, refreshed on reload).
- The **wire check**: a hand-written entry (`beamtalk_threaded_wire_test_helper`)
  reached through `beamtalk_dispatch:send_threaded/4` with a hand-packed `Acc`,
  proving the lookup, the `function_exported/3` guard, the entry call and the
  map round trip before any codegen emits an entry.
- The Erlang half of the Rust/Erlang conformance fixture
  `test/fixtures/tier2_meta_conformance.json` (the Rust half is
  `class_meta::tests::tier2_meta_matches_shared_conformance_fixture`).
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

-define(HELPER, beamtalk_threaded_wire_test_helper).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

start_class(Name, Meta) ->
    ClassInfo = #{
        superclass => none,
        module => ?HELPER,
        class_methods => #{'twice:' => #{arity => 1}, 'ghost:' => #{arity => 1}},
        class_state => #{},
        meta => Meta
    },
    {ok, Pid} = beamtalk_object_class:start_link(Name, ClassInfo),
    unlink(Pid),
    Pid.

stop_class(Pid) ->
    catch gen_server:stop(Pid, normal, 5000),
    ok.

with_class(Name, Meta, Fun) ->
    Pid = start_class(Name, Meta),
    try
        Fun(Pid)
    after
        stop_class(Pid)
    end.

%% The hand-written Tier 2 block `[t := t + 1]` over a hand-packed `Acc`:
%% `fun(Acc) -> {Value, Acc1}`, the shape ADR 0131 codegen emits.
increment_t() ->
    fun(Acc) ->
        T = maps:get('__local__t', Acc) + 1,
        {T, Acc#{'__local__t' => T}}
    end.

%% `Fun` raises `#beamtalk_error{kind = stateful_block_dispatch}` with the
%% precise ADR 0132 §4 form for `Class` and `Selector`.
assert_no_threaded_entry(Class, Selector, Fun) ->
    try Fun() of
        Unexpected -> ?assertEqual(raised, {returned, Unexpected})
    catch
        error:#{error := #beamtalk_error{} = E} ->
            assert_no_threaded_entry_error(Class, Selector, E);
        error:#beamtalk_error{} = E ->
            assert_no_threaded_entry_error(Class, Selector, E)
    end.

assert_no_threaded_entry_error(Class, Selector, E) ->
    ?assertMatch(
        #beamtalk_error{
            kind = stateful_block_dispatch,
            details = #{reason := no_threaded_entry}
        },
        E
    ),
    ?assertEqual(Class, E#beamtalk_error.class),
    ?assertEqual(Selector, E#beamtalk_error.selector),
    ClassBin = atom_to_binary(Class, utf8),
    SelBin = atom_to_binary(Selector, utf8),
    ?assertEqual(
        <<"'", SelBin/binary, "' on ", ClassBin/binary, " does not thread its block">>,
        E#beamtalk_error.message
    ),
    ?assertEqual(
        <<"`", SelBin/binary, "` on `", ClassBin/binary,
            "` does not thread its block: return the new value from the block and "
            "assign it where it is called">>,
        E#beamtalk_error.hint
    ).

%%====================================================================
%% call_threaded/3: the tier gate
%%====================================================================

call_threaded_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a Tier 1 fun is applied to Args and Acc comes back unchanged", fun() ->
            Acc = #{'__local__t' => 5},
            ?assertEqual(
                {7, Acc}, beamtalk_dispatch:call_threaded(fun(X) -> X + 1 end, [6], Acc)
            )
        end},
        {"a Tier 2 fun is applied with Acc and answers its own Acc", fun() ->
            ?assertEqual(
                {1, #{'__local__t' => 1}},
                beamtalk_dispatch:call_threaded(increment_t(), [], #{'__local__t' => 0})
            )
        end},
        {"a Tier 2 fun with arguments receives Acc last", fun() ->
            Block = fun(X, Acc) -> {X, Acc#{'__local__x' => X}} end,
            ?assertEqual(
                {3, #{'__local__x' => 3}}, beamtalk_dispatch:call_threaded(Block, [3], #{})
            )
        end},
        {"a non-fun callable gets the value-family send and Acc is unchanged", fun() ->
            %% An object answering `value:` (ADR 0128's opaque callables).
            with_class('BT3868Callable', #{}, fun(_Pid) ->
                Callable = #{'$beamtalk_class' => 'BT3868Callable'},
                Acc = #{'__local__t' => 9},
                ?assertEqual(
                    {{called_with, 4}, Acc},
                    beamtalk_dispatch:call_threaded(Callable, [4], Acc)
                )
            end)
        end},
        {"a wrong-arity fun raises a structured arity_mismatch, not badarity", fun() ->
            try beamtalk_dispatch:call_threaded(fun(A, B, C) -> {A, B, C} end, [], #{}) of
                R -> ?assertEqual(raised, {returned, R})
            catch
                error:#{error := #beamtalk_error{} = E} ->
                    ?assertMatch(
                        #beamtalk_error{kind = arity_mismatch, class = 'Block', selector = value},
                        E
                    );
                error:#beamtalk_error{} = E ->
                    ?assertMatch(
                        #beamtalk_error{kind = arity_mismatch, class = 'Block', selector = value},
                        E
                    )
            end
        end},
        {"a plain block one argument too many raises arity_mismatch, not a badarg downstream",
            fun() ->
                %% `fun(X) -> X end` called with no arguments has arity N+1, so the
                %% gate applies it as a Tier 2 fun; its reply is not `{Result, Map}`.
                try beamtalk_dispatch:call_threaded(fun(X) -> X end, [], #{}) of
                    R -> ?assertEqual(raised, {returned, R})
                catch
                    error:#{error := #beamtalk_error{} = E} ->
                        ?assertMatch(#beamtalk_error{kind = arity_mismatch, class = 'Block'}, E)
                end
            end}
    ]}.

%%====================================================================
%% Metadata cache and accessors
%%====================================================================

accessor_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"registration caches both maps from the module's meta", fun() ->
            Meta = ?HELPER:'__beamtalk_meta'(),
            with_class('BT3868Cache', Meta, fun(_Pid) ->
                ?assertEqual(
                    {maps:get(tier2_methods, Meta), maps:get(tier2_class_methods, Meta)},
                    beamtalk_class_metadata:lookup_tier2_methods('BT3868Cache')
                )
            end)
        end},
        {"listed and exported entries resolve on both sides", fun() ->
            with_class('BT3868Listed', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                ?assertEqual(
                    {ok, ?HELPER, '__t2_twice:'},
                    beamtalk_class_dispatch:lookup_threaded_entry('BT3868Listed', 'twice:', [b])
                ),
                ?assertEqual(
                    {ok, ?HELPER, '__t2_class_twice:'},
                    beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                )
            end)
        end},
        {"a listed entry that is not exported is refused (the guard)", fun() ->
            with_class('BT3868Ghost', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                ?assertEqual(
                    error,
                    beamtalk_class_dispatch:lookup_threaded_entry('BT3868Ghost', 'ghost:', [b])
                ),
                ?assertEqual(
                    error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'ghost:', [b])
                )
            end)
        end},
        {"an arity mismatch is refused", fun() ->
            with_class('BT3868Arity', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                ?assertEqual(
                    error,
                    beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b, c])
                )
            end)
        end},
        {"a module without the keys reads as having no entries", fun() ->
            Meta = maps:without([tier2_methods, tier2_class_methods], ?HELPER:'__beamtalk_meta'()),
            with_class('BT3868NoKeys', Meta, fun(Pid) ->
                ?assertEqual(
                    {#{}, #{}}, beamtalk_class_metadata:lookup_tier2_methods('BT3868NoKeys')
                ),
                ?assertEqual(
                    error,
                    beamtalk_class_dispatch:lookup_threaded_entry('BT3868NoKeys', 'twice:', [b])
                ),
                ?assertEqual(
                    error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                )
            end)
        end},
        {"an unknown class has no entries", fun() ->
            ?assertEqual(
                error,
                beamtalk_class_dispatch:lookup_threaded_entry('BT3868NoSuchClass', 'twice:', [b])
            )
        end},
        {"reload refreshes the cache", fun() ->
            Meta = ?HELPER:'__beamtalk_meta'(),
            with_class('BT3868Reload', Meta, fun(Pid) ->
                ?assertMatch(
                    {ok, _, _},
                    beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                ),
                {ok, _} = beamtalk_object_class:update_class('BT3868Reload', #{
                    module => ?HELPER,
                    meta => Meta#{tier2_methods => #{}, tier2_class_methods => #{}}
                }),
                ?assertEqual(
                    error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                ),
                ?assertEqual(
                    error,
                    beamtalk_class_dispatch:lookup_threaded_entry('BT3868Reload', 'twice:', [b])
                ),
                {ok, _} = beamtalk_object_class:update_class('BT3868Reload', #{
                    module => ?HELPER, meta => Meta
                }),
                ?assertMatch(
                    {ok, _, _},
                    beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                )
            end)
        end},
        {"runtime class-method funs disable the class-side entry", fun() ->
            with_class('BT3868Funs', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                beamtalk_class_metadata:set_runtime_class_methods('BT3868Funs', ['twice:']),
                ?assertEqual(
                    error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, 'twice:', [b])
                )
            end)
        end}
    ]}.

%%====================================================================
%% The wire check: send_threaded/4 end to end
%%====================================================================

wire_check_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"a class-side entry is reached and the Acc map round-trips", fun() ->
            with_class('BT3868Wire', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                Class = beamtalk_class_registry:class_object_from_pid(Pid),
                ?assertEqual(
                    {2, #{'__local__t' => 2}},
                    beamtalk_dispatch:send_threaded(
                        Class, 'twice:', [increment_t()], #{'__local__t' => 0}
                    )
                ),
                %% The plain entry still answers the plain result.
                ?assertEqual(2, beamtalk_class_dispatch:class_send(Pid, 'twice:', [fun() -> 2 end]))
            end)
        end},
        {"an instance-side entry is reached for a value receiver", fun() ->
            with_class('BT3868WireValue', ?HELPER:'__beamtalk_meta'(), fun(_Pid) ->
                Value = #{'$beamtalk_class' => 'BT3868WireValue'},
                ?assertEqual(
                    {2, #{'__local__t' => 2}},
                    beamtalk_dispatch:send_threaded(
                        Value, 'twice:', [increment_t()], #{'__local__t' => 0}
                    )
                )
            end)
        end},
        {"a module without the key raises the structured error", fun() ->
            Meta = maps:without([tier2_methods, tier2_class_methods], ?HELPER:'__beamtalk_meta'()),
            with_class('BT3868WireNoKey', Meta, fun(Pid) ->
                Class = beamtalk_class_registry:class_object_from_pid(Pid),
                assert_no_threaded_entry('BT3868WireNoKey class', 'twice:', fun() ->
                    beamtalk_dispatch:send_threaded(
                        Class, 'twice:', [increment_t()], #{'__local__t' => 0}
                    )
                end),
                assert_no_threaded_entry('BT3868WireNoKey', 'twice:', fun() ->
                    beamtalk_dispatch:send_threaded(
                        #{'$beamtalk_class' => 'BT3868WireNoKey'},
                        'twice:',
                        [increment_t()],
                        #{'__local__t' => 0}
                    )
                end)
            end)
        end},
        {"a listed entry that is not exported raises the structured error", fun() ->
            with_class('BT3868WireGhost', ?HELPER:'__beamtalk_meta'(), fun(Pid) ->
                Class = beamtalk_class_registry:class_object_from_pid(Pid),
                assert_no_threaded_entry('BT3868WireGhost class', 'ghost:', fun() ->
                    beamtalk_dispatch:send_threaded(
                        Class, 'ghost:', [increment_t()], #{'__local__t' => 0}
                    )
                end)
            end)
        end},
        {"a class method that is not direct-called raises (Phase 4 path)", fun() ->
            Meta = maps:remove(direct_class_methods, ?HELPER:'__beamtalk_meta'()),
            with_class('BT3868WireGen', Meta, fun(Pid) ->
                Class = beamtalk_class_registry:class_object_from_pid(Pid),
                assert_no_threaded_entry('BT3868WireGen class', 'twice:', fun() ->
                    beamtalk_dispatch:send_threaded(
                        Class, 'twice:', [increment_t()], #{'__local__t' => 0}
                    )
                end)
            end)
        end},
        {"a primitive receiver raises the structured error", fun() ->
            assert_no_threaded_entry('Integer', 'timesRepeat:', fun() ->
                beamtalk_dispatch:send_threaded(3, 'timesRepeat:', [increment_t()], #{
                    '__local__t' => 0
                })
            end)
        end}
    ]}.

%%====================================================================
%% Rust/Erlang conformance (tier2_meta_conformance.json)
%%====================================================================

conformance_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"compiled fixture meta and the registry agree with the shared fixture",
            fun conformance_matches_shared_fixture/0}
    ]}.

conformance_matches_shared_fixture() ->
    Cases = beamtalk_test_corpus:load_json_fixture([
        "runtime", "apps", "beamtalk_runtime", "test", "fixtures", "tier2_meta_conformance.json"
    ]),
    ?assert(length(Cases) > 0),
    lists:foreach(fun assert_conformance_case/1, Cases).

assert_conformance_case(Case) ->
    ClassName = binary_to_atom(maps:get(<<"class_name">>, Case), utf8),
    Basename = binary_to_atom(maps:get(<<"fixture">>, Case), utf8),
    ok = beamtalk_dist_wire_tests:ensure_fixture_loaded(Basename, ClassName),
    Module = list_to_atom("bt@" ++ atom_to_list(Basename)),
    ExpectedInstance = expected_entries(maps:get(<<"tier2_methods">>, Case)),
    ExpectedClass = expected_entries(maps:get(<<"tier2_class_methods">>, Case)),
    %% What the Rust side emitted into the compiled module.
    Meta = Module:'__beamtalk_meta'(),
    ?assertEqual(ExpectedInstance, maps:get(tier2_methods, Meta)),
    ?assertEqual(ExpectedClass, maps:get(tier2_class_methods, Meta)),
    %% What the registry cached from it.
    ?assertEqual(
        {ExpectedInstance, ExpectedClass},
        beamtalk_class_metadata:lookup_tier2_methods(ClassName)
    ),
    Pid = beamtalk_class_registry:whereis_class(ClassName),
    ?assert(is_pid(Pid)),
    lists:foreach(
        fun(SelBin) ->
            Sel = binary_to_atom(SelBin, utf8),
            ?assertEqual(
                error, beamtalk_class_dispatch:lookup_threaded_entry(ClassName, Sel, [b])
            )
        end,
        maps:get(<<"unthreaded_selectors">>, Case)
    ),
    lists:foreach(
        fun(SelBin) ->
            Sel = binary_to_atom(SelBin, utf8),
            ?assertEqual(
                error, beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, Sel, [b])
            )
        end,
        maps:get(<<"unthreaded_class_selectors">>, Case)
    ),
    maps:foreach(
        fun(Sel, {EntryFn, _Positions}) ->
            ?assertEqual(
                {ok, Module, EntryFn},
                beamtalk_class_dispatch:lookup_threaded_entry(ClassName, Sel, args_for(Sel))
            )
        end,
        ExpectedInstance
    ),
    maps:foreach(
        fun(Sel, {EntryFn, _Positions}) ->
            ?assertEqual(
                {ok, Module, EntryFn},
                beamtalk_class_dispatch:lookup_threaded_class_entry(Pid, Sel, args_for(Sel))
            )
        end,
        ExpectedClass
    ).

%% Placeholder arguments of the selector's arity (the entry's arity is checked).
args_for(Selector) ->
    lists:duplicate(length([C || C <- atom_to_list(Selector), C =:= $:]), b).

%% `{"sel": {"entry_fn": "...", "positions": [..]}}` -> `#{sel => {entry_fn, [..]}}`.
expected_entries(Json) ->
    maps:fold(
        fun(SelBin, #{<<"entry_fn">> := Fn, <<"positions">> := Positions}, Acc) ->
            Acc#{binary_to_atom(SelBin, utf8) => {binary_to_atom(Fn, utf8), Positions}}
        end,
        #{},
        Json
    ).
