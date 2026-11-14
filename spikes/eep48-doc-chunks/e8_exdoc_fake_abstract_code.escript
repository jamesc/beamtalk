#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149), consumer test only: ExDoc's Erlang backend insists on
%% an Erlang abstract_code chunk. Swap in a synthetic one (-file, -module,
%% -export, one stub function form per documented entry) to find out whether
%% ExDoc could render Beamtalk entries at all.
%% This is exactly the post-erlc .beam rewriting the ADR does NOT propose.
%% Usage: escript e8_exdoc_fake_abstract_code.escript <in ebin> <out ebin>
main([In, Out]) ->
    ok = filelib:ensure_path(Out),
    [begin
        {ok, M, Chunks} = beam_lib:all_chunks(F),
        {ok, {M, [{exports, Ex}]}} = beam_lib:chunks(F, [exports]),
        Abst = [{attribute, 1, file, {"src/" ++ atom_to_list(M) ++ ".bt", 1}},
                {attribute, 1, module, M},
                {attribute, 1, export, Ex}] ++
               [{function, 1, N, A, [{clause, 1, [{var, 1, '_'} || _ <- lists:seq(1, A)], [], [{atom, 1, ok}]}]}
                || {{function, N, A}, _, _, _, _} <- element(7, binary_to_term(proplists:get_value("Docs", Chunks)))],
        Dbgi = term_to_binary({debug_info_v1, erl_abstract_code, {Abst, []}}),
        {ok, Bin} = beam_lib:build_module(lists:keystore("Dbgi", 1, Chunks, {"Dbgi", Dbgi})),
        ok = file:write_file(filename:join(Out, filename:basename(F)), Bin)
     end || F <- filelib:wildcard(filename:join(In, "*.beam"))].
