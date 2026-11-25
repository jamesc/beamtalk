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
`spawn`/`new` clauses of `class_send_plain/4` and
`handle_metaclass_self_call/2`, the Supervisor guard of `class_send/4`, and the
TestCase run-selector check in `class_self_direct_ok/4`). This conformance test
pins every selector those clauses name to the generated list, by reading the
clauses' own abstract code, so a clause added in Erlang without the Rust list
(or the reverse removal) fails here (architecture-principles §7).
""".

-include_lib("eunit/include/eunit.hrl").

%% BT-5047: `handle_metaclass_self_call/2` moved to `_recovery`, and the class-self
%% dispatch to `_self`; the hard-coded clauses are read from all three modules.
-define(DISPATCH_MODULES, [
    beamtalk_class_dispatch, beamtalk_class_dispatch_self, beamtalk_class_dispatch_recovery
]).

setup() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:boot_real_stdlib('Integer'),
    ok.

%% The generated list, read from a compiled stdlib class's meta.
generated(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    {ok, Meta} = beamtalk_class_metadata:read_meta(Module),
    maps:get(process_bound_class_selectors, Meta).

generated() -> generated('bt@stdlib@test_case').

forms() ->
    lists:append([forms(Module) || Module <- ?DISPATCH_MODULES]).

forms(Module) ->
    %% The object code on the code path, not `code:which/1` (which answers
    %% `cover_compiled` under cover).
    {Module, Beam, _File} = code:get_object_code(Module),
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
        {"class_send_plain/4's dedicated selector clauses are in the generated list", fun() ->
            assert_pinned(class_send_plain, head_atoms(class_send_plain, 4, 2))
        end},
        {"handle_metaclass_self_call/2's clauses are in the generated list", fun() ->
            assert_pinned(handle_metaclass_self_call, head_atoms(handle_metaclass_self_call, 2, 1))
        end},
        {"class_send/4's Supervisor routing selectors are in the generated list", fun() ->
            assert_pinned(class_send, guard_atoms(class_send, 4, 'Selector'))
        end},
        {"the TestCase run selectors are in the generated list", fun() ->
            assert_pinned(is_test_execution_selector, test_run_selectors())
        end},
        {"the generated list holds the selectors ADR 0153 §5 names", fun() ->
            %% A pin, not a copy the runtime reads: the routing below takes
            %% the list from the generated metadata only.
            Named = [
                new,
                'new:',
                startLink,
                'startLink:',
                spawn,
                'spawnWith:',
                supervise,
                'spawnAs:',
                'spawnWith:as:',
                runAll,
                'runAll:',
                'run:',
                'run:method:'
            ],
            ?assertEqual([], Named -- generated())
        end},
        {"execution mode has one input: each generated selector keeps the class process",
            fun execution_mode_has_one_input/0}
    ]}.

%% ADR 0153 §5 (BT-5158): a class method runs in the caller unless its selector
%% is process-bound. A class that defines every generated selector as a class
%% method (and carries class-variable defaults, which are no input) declines
%% each of them and runs a plain class method in the caller; with the list
%% emptied the same selectors run in the caller, so the list is the only input.
execution_mode_has_one_input() ->
    Name = 'Bt5158ProcessBound',
    Generated = generated(),
    Methods = maps:from_list([{S, #{arity => selector_arity(S)}} || S <- [testSelf | Generated]]),
    Pid = beamtalk_test_class:start(Name, #{
        superclass => none,
        module => beamtalk_class_dispatch_test_helper,
        class_methods => Methods,
        class_state => #{count => 0},
        meta => #{process_bound_class_selectors => Generated}
    }),
    try
        [
            ?assertEqual({S, decline}, {S, beamtalk_class_dispatch:in_caller_owner(Pid, S)})
         || S <- Generated
        ],
        ?assertMatch(
            {owner, Name, _, _, Name, _, class_testSelf},
            beamtalk_class_dispatch:in_caller_owner(Pid, testSelf)
        ),
        ?assertEqual(self(), beamtalk_class_dispatch:class_send(Pid, testSelf, [])),
        ok = beamtalk_class_metadata:put_process_bound_selectors(Name, []),
        [
            ?assertMatch({S, {owner, Name, _, _, Name, _, _}}, {
                S, beamtalk_class_dispatch:in_caller_owner(Pid, S)
            })
         || S <- Generated
        ]
    after
        beamtalk_test_proc:stop(Pid)
    end.

selector_arity(S) ->
    length([C || C <- atom_to_list(S), C =:= $:]).
