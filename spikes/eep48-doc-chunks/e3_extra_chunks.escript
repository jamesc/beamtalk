#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149). Compile Beamtalk Core Erlang with the build
%% worker's options plus {extra_chunks, [{<<"Docs">>, ...}]} and check that
%% the compiler itself wrote the chunk.
%% Usage: escript e3_extra_chunks.escript <outdir> <beamtalk|erlang> <load|noload> <file.core>...
%%   noload drops on_load so consumers can load the module without the runtime.
main([Out, Lang, Load | Files]) ->
    code:add_patha("_out"),
    ok = filelib:ensure_path(Out),
    code:add_patha(Out),
    put(eep48_proto_noload, Load =:= "noload"),
    io:format("== OTP ~s ==~n", [erlang:system_info(otp_release)]),
    [begin
        {ok, M, B0} = eep48_proto:compile_core(F, nodocs),
        {ok, M, B1} = eep48_proto:compile_core(F, docs, list_to_atom(Lang)),
        {ok, M, B2} = eep48_proto:compile_core(F, docs, list_to_atom(Lang)),
        ok = file:write_file(filename:join(Out, atom_to_list(M) ++ ".beam"), B1),
        {ok, _, Cs} = beam_lib:all_chunks(B1),
        {ok, D} = code:get_doc(M),
        Es = element(7, D),
        io:format("~s: ~p -> ~p bytes (+~p), Docs chunk=~p, byte-identical on recompile=~p~n"
                  "    code:get_doc: language=~p, ~p entries, ~p of them real exports~n",
            [M, byte_size(B0), byte_size(B1), byte_size(B1) - byte_size(B0),
             lists:keymember("Docs", 1, Cs), B1 =:= B2, element(3, D), length(Es),
             length([E || E <- Es, maps:get(exported, element(5, E))])])
     end || F <- Files].
