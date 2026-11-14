#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149). Question 1: does a -doc/-moduledoc attribute
%% make the OTP compiler write a "Docs" chunk when the input is Core Erlang?
%% Usage: escript e1_core_attributes.escript   (run from this directory)
main(_) ->
    Tmp = "_out/e1",
    ok = filelib:ensure_path(Tmp),
    code:add_patha(Tmp),
    io:format("== OTP ~s ==~n", [erlang:system_info(otp_release)]),
    {ok, plain} = compile:file("fixture/plain.erl", [{outdir, Tmp}]),
    report("plain.erl (-moduledoc/-doc, normal Erlang pipeline)", filename:join(Tmp, "plain.beam")),
    {ok, plain, Core} = compile:file("fixture/plain.erl", [to_core, binary]),
    io:format("  doc attributes left in plain.erl's own Core Erlang: ~p~n",
              [[cerl:concrete(K) || {K, _} <- cerl:module_attrs(Core),
                lists:member(cerl:concrete(K), [doc, moduledoc])]]),
    {ok, incore} = compile:file("fixture/incore.core", [from_core, {outdir, Tmp}]),
    report("incore.core ('moduledoc'/'doc' as Core attributes, compile:file from_core)",
           filename:join(Tmp, "incore.beam")),
    {ok, Bin} = file:read_file("fixture/incore.core"),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Parsed} = core_parse:parse(Toks),
    {ok, incore, Beam} = compile:forms(Parsed, [from_core, binary, return_errors, debug_info, clint]),
    {ok, _, Chunks} = beam_lib:all_chunks(Beam),
    io:format("incore.core via compile:forms (the build worker's options): Docs chunk=~p~n",
              [lists:keymember("Docs", 1, Chunks)]).

report(Label, Beam) ->
    {ok, M, Chunks} = beam_lib:all_chunks(Beam),
    {ok, {M, [{attributes, As}]}} = beam_lib:chunks(Beam, [attributes]),
    R = case code:get_doc(M) of {ok, _} -> ok; E -> E end,
    io:format("~s~n  Docs chunk=~p  code:get_doc=~p  doc attrs kept as plain module attributes=~p~n",
              [Label, lists:keymember("Docs", 1, Chunks), R,
               [K || {K, _} <- As, K =:= doc orelse K =:= moduledoc]]).
