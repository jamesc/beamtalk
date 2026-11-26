%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_send_selector_tests).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-5271: a sync send to a `{registered, Name, Node}` ref on another node goes
to `{Name, Node}` directly unless `beamtalk_actor_send:needs_resolved_pid/1`
says the selector is answered from the resolved pid in the caller. That list
must cover every selector the pid send paths special-case, or such a selector
would be sent to the actor instead of answered in the caller.

This test reads the special-cased selectors from the compiled clauses of
`sync_send_pid/4,5` and `cast_send_pid/4` (each clause's literal selector, and
each selector its guard compares with) and checks each against the list.
""".

-include_lib("eunit/include/eunit.hrl").

special_cased_selectors_need_the_resolved_pid_test() ->
    Selectors = special_cased_selectors(),
    %% The extraction itself must find the known clauses, or the check is empty.
    ?assert(lists:member(stop, Selectors)),
    ?assert(lists:member(kill, Selectors)),
    ?assert(lists:member('onExit:', Selectors)),
    ?assertEqual(
        [], [S || S <- Selectors, not beamtalk_actor_send:needs_resolved_pid(S)]
    ).

ordinary_selectors_go_direct_test() ->
    ?assertNot(beamtalk_actor_send:needs_resolved_pid(getValue)),
    ?assertNot(beamtalk_actor_send:needs_resolved_pid('setValue:')).

special_cased_selectors() ->
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(beam_file(beamtalk_actor_send), [abstract_code]),
    Funs = [{sync_send_pid, 4}, {sync_send_pid, 5}, {cast_send_pid, 4}],
    lists:usort(
        lists:append([
            clause_selectors(Clause)
         || {function, _, Name, Arity, Clauses} <- Forms,
            lists:member({Name, Arity}, Funs),
            Clause <- Clauses
        ])
    ).

%% The module's `.beam` on the code path: `code:which/1` answers
%% `cover_compiled` while the module is cover-compiled.
beam_file(Module) ->
    case code:which(Module) of
        Path when is_list(Path) -> Path;
        _ -> code:where_is_file(atom_to_list(Module) ++ ".beam")
    end.

%% The selector (second argument) a clause matches literally, plus the atoms
%% its guard compares the selector variable with.
clause_selectors({clause, _, [_Target, {atom, _, Selector} | _], _Guards, _Body}) ->
    [Selector];
clause_selectors({clause, _, [_Target, {var, _, Var} | _], Guards, _Body}) ->
    guard_atoms(Var, Guards);
clause_selectors({clause, _, [_Target, {match, _, {atom, _, Selector}, _} | _], _, _}) ->
    [Selector];
clause_selectors({clause, _, [_Target, {match, _, _, {atom, _, Selector}} | _], _, _}) ->
    [Selector];
clause_selectors(_Clause) ->
    [].

guard_atoms(Var, Term) when is_tuple(Term) ->
    case Term of
        {op, _, '=:=', {var, _, Var}, {atom, _, A}} -> [A];
        {op, _, '=:=', {atom, _, A}, {var, _, Var}} -> [A];
        _ -> guard_atoms(Var, tuple_to_list(Term))
    end;
guard_atoms(Var, Terms) when is_list(Terms) ->
    lists:append([guard_atoms(Var, T) || T <- Terms]);
guard_atoms(_Var, _Other) ->
    [].
