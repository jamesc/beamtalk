#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149): how do selector entries line up with real exports, and
%% what would flattening inherited methods into every class cost?
%% Usage: escript e6_mapping_stats.escript <stdlib ebin dir>   (after eep48_runtime_proto:report/1 wrote the chunks)
main([Ebin]) ->
    Chunks = filelib:wildcard(filename:join([filename:dirname(Ebin), "doc", "chunks", "*.chunk"])),
    Ds = [begin {ok, B} = file:read_file(F), {list_to_atom(filename:basename(F, ".chunk")), binary_to_term(B)} end || F <- Chunks],
    ByClass = maps:from_list([{maps:get(beamtalk_class, element(6, D)), D} || {_, D} <- Ds]),
    Stats = [begin
        {ok, {_, [{exports, Ex}]}} = beam_lib:chunks(filename:join(Ebin, atom_to_list(M) ++ ".beam"), [exports]),
        Es = element(7, D),
        Inst = [E || E <- Es, not maps:get(class_side, element(5, E))],
        Cls = Es -- Inst,
        Hit = fun(L) -> length([E || {{function, N, A}, _, _, _, _} = E <- L, lists:member({N, A}, Ex)]) end,
        {length(Inst), Hit(Inst), length(Cls), Hit(Cls)}
    end || {M, D} <- Ds, filelib:is_file(filename:join(Ebin, atom_to_list(M) ++ ".beam"))],
    io:format("modules without a .beam (runtime-synthesised classes): ~p~n",
              [[M || {M, _} <- Ds, not filelib:is_file(filename:join(Ebin, atom_to_list(M) ++ ".beam"))]]),
    Sum = fun(I) -> lists:sum([element(I, T) || T <- Stats]) end,
    io:format("instance-side entries: ~p, of which {Selector, Arity+1} is a real export: ~p~n"
              "class-side entries:    ~p, of which {class_Selector, Arity+1} is a real export: ~p~n",
              [Sum(1), Sum(2), Sum(3), Sum(4)]),
    Chain = fun Chain(none) -> []; Chain(C) ->
        case ByClass of #{C := D} -> [D | Chain(maps:get(superclass, element(6, D)))]; _ -> [] end end,
    Flat = lists:sum([begin
        All = lists:append([element(7, X) || X <- Chain(C)]),
        length(lists:ukeysort(1, All))
    end || C <- maps:keys(ByClass)]),
    FlatBytes = lists:sum([byte_size(term_to_binary(
        setelement(7, D, lists:ukeysort(1, lists:append([element(7, X) || X <- Chain(C)]))), [compressed]))
        || {C, D} <- maps:to_list(ByClass)]),
    Local = lists:sum([length(element(7, D)) || {_, D} <- Ds]),
    LocalBytes = lists:sum([filelib:file_size(F) || F <- Chunks]),
    io:format("local-only: ~p entries, ~p bytes compressed~nflattened (inherited copied into every class): ~p entries, ~p bytes compressed (x~.1f)~n",
              [Local, LocalBytes, Flat, FlatBytes, FlatBytes / LocalBytes]).
