%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_process_bound_selectors_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0142 §1 rule 3 (BT-4299): the process-bound class-side selector list is
owned by Rust (`ClassInfo::process_bound_class_selectors`) and reaches the
runtime as `process_bound_class_selectors` in every compiled class's
`__beamtalk_meta/0`. The runtime keeps no copy of the list, but
`beamtalk_class_dispatch` routes part of it with hard-coded clauses (the
`spawn`/`new` clauses of `class_send_plain/3` and
`handle_metaclass_self_call/2`, the Supervisor guard of `class_send/3`, and the
TestCase run-selector check in `class_self_direct_ok/4`). This conformance test
pins every selector those clauses name to the generated list, by reading the
clauses' own abstract code, so a clause added in Erlang without the Rust list
(or the reverse removal) fails here (architecture-principles §7).
""".

-include_lib("eunit/include/eunit.hrl").

-define(DISPATCH, beamtalk_class_dispatch).

setup() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    ok = beamtalk_stdlib:init(),
    ok.

%% The generated list, read from a compiled stdlib class's meta.
generated(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    {ok, Meta} = beamtalk_class_metadata:read_meta(Module),
    maps:get(process_bound_class_selectors, Meta).

generated() -> generated('bt@stdlib@test_case').

forms() ->
    %% The object code on the code path, not `code:which/1` (which answers
    %% `cover_compiled` under cover).
    {?DISPATCH, Beam, _File} = code:get_object_code(?DISPATCH),
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Beam, [abstract_code]),
    Forms.

clauses(Name, Arity) ->
    [Cs] = [Cs || {function, _, N, A, Cs} <- forms(), N =:= Name, A =:= Arity],
    Cs.

%% Atoms matched in argument position `Pos` of `Name/Arity`'s clause heads.
head_atoms(Name, Arity, Pos) ->
    lists:usort([
        S
     || {clause, _, Args, _, _} <- clauses(Name, Arity),
        {atom, _, S} <- [lists:nth(Pos, Args)]
    ]).

%% Atoms compared with `=:=` to the variable `Var` anywhere in `Name/Arity`'s guards.
guard_atoms(Name, Arity, Var) ->
    lists:usort(
        lists:flatmap(
            fun({clause, _, _, Guards, _}) -> compared_atoms(Guards, Var) end,
            clauses(Name, Arity)
        )
    ).

compared_atoms({op, _, '=:=', {var, _, V}, {atom, _, A}}, V) -> [A];
compared_atoms(T, V) when is_tuple(T) -> compared_atoms(tuple_to_list(T), V);
compared_atoms(L, V) when is_list(L) -> lists:flatmap(fun(E) -> compared_atoms(E, V) end, L);
compared_atoms(_, _) -> [].

%% Clauses of `is_test_execution_selector/1` that answer `true`.
test_run_selectors() ->
    lists:usort([
        S
     || {clause, _, [{atom, _, S}], [], [{atom, _, true}]} <-
            clauses(is_test_execution_selector, 1)
    ]).

assert_pinned(Label, Erlang) ->
    Generated = generated(),
    ?assertNotEqual({Label, []}, {Label, Erlang}),
    ?assertEqual({Label, []}, {Label, Erlang -- Generated}).

process_bound_selectors_test_() ->
    {setup, fun setup/0, fun(_) -> ok end, [
        {"every compiled class carries the same generated list", fun() ->
            Generated = generated(),
            ?assert(lists:all(fun is_atom/1, Generated)),
            ?assertEqual(Generated, generated('bt@stdlib@supervisor')),
            ?assertEqual(Generated, generated('bt@stdlib@object'))
        end},
        {"the class process publishes the generated list into its metadata row", fun() ->
            {ok, Module} = beamtalk_class_metadata:lookup_module('Object'),
            ?assertEqual(
                {ok, generated(Module)},
                beamtalk_class_metadata:lookup_process_bound_selectors('Object')
            )
        end},
        {"class_send_plain/3's dedicated selector clauses are in the generated list", fun() ->
            assert_pinned(class_send_plain, head_atoms(class_send_plain, 3, 2))
        end},
        {"handle_metaclass_self_call/2's clauses are in the generated list", fun() ->
            assert_pinned(handle_metaclass_self_call, head_atoms(handle_metaclass_self_call, 2, 1))
        end},
        {"class_send/3's Supervisor routing selectors are in the generated list", fun() ->
            assert_pinned(class_send, guard_atoms(class_send, 3, 'Selector'))
        end},
        {"the TestCase run selectors are in the generated list", fun() ->
            assert_pinned(is_test_execution_selector, test_run_selectors())
        end}
    ]}.
