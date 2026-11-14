#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149). What does carrying the docs term inside the
%% Core Erlang TEXT cost, end to end (core_scan + core_parse + compile:forms)?
%% E7 passed the chunk out of band and so did not measure this.
%%
%% Three carriers for the same docs_v1 term, in an otherwise tiny module:
%%   segments  '__beamtalk_docs'/0 returns the docs_v1 literal, binaries written
%%             one segment per byte, as codegen writes every binary today
%%   string    '__beamtalk_docs'/0 returns the term_to_binary bytes as one Core
%%             string literal; the compile step lifts it with list_to_binary/1
%%   string_z  the same with the bytes zlib-compressed first
%% Usage: escript e10_carrier_cost.escript <stdlib ebin dir>   (after E6 wrote the chunks)
main([Ebin]) ->
    Files = filelib:wildcard(filename:join([filename:dirname(Ebin), "doc", "chunks", "*.chunk"])),
    Docs = [begin {ok, B} = file:read_file(F), binary_to_term(B) end || F <- Files],
    Sized = lists:reverse(lists:keysort(1, [{byte_size(term_to_binary(D)), D} || D <- Docs])),
    {BigSize, Big} = hd(Sized),
    {_, Median} = lists:nth(length(Sized) div 2, Sized),
    io:format("== OTP ~s ==~n", [erlang:system_info(otp_release)]),
    io:format("largest class: ~s (~p bytes of docs_v1)~n", [maps:get(beamtalk_class, element(6, Big)), BigSize]),
    row("largest class", Big),
    io:format("median class: ~s~n", [maps:get(beamtalk_class, element(6, Median))]),
    row("median class", Median),
    Tot = fun(Kind) -> lists:sum([element(1, measure(Kind, D)) || D <- Docs]) end,
    Chars = fun(Kind) -> lists:sum([iolist_size(core(Kind, D)) || D <- Docs]) end,
    Base = Tot(none),
    io:format("whole stdlib (~p classes), scan+parse+compile summed, one run each:~n", [length(Docs)]),
    [io:format("  ~-9s ~8.1f ms over the no-carrier baseline, ~p KB of Core text~n",
               [K, (Tot(K) - Base) / 1000, Chars(K) div 1024]) || K <- [segments, string, string_z]].

row(Label, D) ->
    {T0, _} = best(none, D),
    [begin
        {T, Round} = best(K, D),
        io:format("  ~s ~-9s core text ~8w bytes, scan+parse+compile ~7.1f ms (+~.1f ms), docs round-trip=~p~n",
                  [Label, K, iolist_size(core(K, D)), T / 1000, (T - T0) / 1000, Round])
     end || K <- [segments, string, string_z]].

best(Kind, D) ->
    Rs = [measure(Kind, D) || _ <- lists:seq(1, 5)],
    {lists:min([T || {T, _} <- Rs]), element(2, hd(Rs))}.

measure(Kind, D) ->
    Text = lists:flatten(core(Kind, D)),
    timer:tc(fun() ->
        {ok, Toks, _} = core_scan:string(Text),
        {ok, Core0} = core_parse:parse(Toks),
        {Core, Extra, Round} = lift(Kind, Core0, D),
        {ok, _, _} = compile:forms(Core, [from_core, binary, return_errors, debug_info, clint | Extra]),
        Round
    end).

lift(none, Core, _) -> {Core, [], n_a};
lift(Kind, Core0, D) ->
    Is = fun(N) -> cerl:fname_id(N) =:= '__beamtalk_docs' end,
    {[{_, Fun}], Defs} = lists:partition(fun({N, _}) -> Is(N) end, cerl:module_defs(Core0)),
    Body = cerl:fold_literal(cerl:fun_body(Fun)),
    true = cerl:is_literal(Body),
    Bin = case Kind of
        segments -> term_to_binary(cerl:concrete(Body), [deterministic, compressed]);
        string -> list_to_binary(cerl:concrete(Body));
        string_z -> list_to_binary(cerl:concrete(Body))
    end,
    Round = case Kind of
        string_z -> binary_to_term(zlib:uncompress(Bin)) =:= D;
        _ -> binary_to_term(Bin) =:= D
    end,
    Core = cerl:update_c_module(Core0, cerl:module_name(Core0),
        [E || E <- cerl:module_exports(Core0), not Is(E)], cerl:module_attrs(Core0), Defs),
    {Core, [{extra_chunks, [{<<"Docs">>, Bin}]}], Round}.

core(Kind, D) ->
    Carrier = case Kind of
        none -> [];
        segments -> ["'__beamtalk_docs'/0 =\n    fun () -> ", term(D), "\n"];
        string -> ["'__beamtalk_docs'/0 =\n    fun () -> ", str(term_to_binary(D, [deterministic])), "\n"];
        string_z -> ["'__beamtalk_docs'/0 =\n    fun () -> ", str(zlib:compress(term_to_binary(D, [deterministic]))), "\n"]
    end,
    Exp = case Kind of none -> ""; _ -> "'__beamtalk_docs'/0, " end,
    ["module 'carrier_cost' ['inc'/1, ", Exp, "'module_info'/0, 'module_info'/1]\n    attributes []\n",
     Carrier,
     "'inc'/1 =\n    fun (_0) -> call 'erlang':'+'(_0, 1)\n"
     "'module_info'/0 =\n    fun () -> call 'erlang':'get_module_info'('carrier_cost')\n"
     "'module_info'/1 =\n    fun (_0) -> call 'erlang':'get_module_info'('carrier_cost', _0)\nend\n"].

%% Core Erlang text for a term, binaries one segment per byte (beamtalk-cerl-doc byte_segments).
term(B) when is_binary(B) ->
    ["#{", lists:join(",", [["#<", integer_to_list(X), ">(8,1,'integer',['unsigned'|['big']])"] || <<X>> <= B]), "}#"];
term(A) when is_atom(A) -> [$', [case C of $' -> "\\'"; $\\ -> "\\\\"; _ -> C end || C <- atom_to_list(A)], $'];
term(I) when is_integer(I) -> integer_to_list(I);
term(T) when is_tuple(T) -> ["{", lists:join(",", [term(E) || E <- tuple_to_list(T)]), "}"];
term(M) when is_map(M) -> ["~{", lists:join(",", [[term(K), "=>", term(V)] || {K, V} <- maps:to_list(M)]), "}~"];
term(L) when is_list(L) -> ["[", lists:join(",", [term(E) || E <- L]), "]"].

%% One Core string literal: printable ASCII as is, everything else as \ooo.
str(Bin) ->
    [$", [esc(X) || <<X>> <= Bin], $"].
esc($") -> "\\\"";
esc($\\) -> "\\\\";
esc(X) when X >= 32, X < 127 -> X;
esc(X) -> io_lib:format("\\~3.8.0b", [X]).
