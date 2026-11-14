#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149). Question 2: can code:get_doc/1 see a module
%% that was loaded from memory (REPL-defined class), with no .beam on the code
%% path? Usage: escript e5_memory_loaded.escript <file.core>
main([CoreFile]) ->
    code:add_patha("_out"),
    io:format("== OTP ~s ==~n", [erlang:system_info(otp_release)]),
    %% on_load (register_class) needs the Beamtalk runtime; drop it for this test.
    put(eep48_proto_noload, true),
    {ok, M, Beam} = eep48_proto:compile_core(CoreFile, docs),
    Tmp = filename:absname("_out/e5"),
    file:del_dir_r(Tmp),
    ok = filelib:ensure_path(Tmp),
    Try = fun(Label, LoadPath, Prep) ->
        code:purge(M), code:delete(M), code:purge(M),
        Prep(),
        {module, M} = code:load_binary(M, LoadPath, Beam),
        R = case code:get_doc(M) of
            {ok, D} -> {ok, element(3, D), length(element(7, D))};
            E -> E
        end,
        io:format("~s~n    code:which=~p~n    code:get_doc=~p~n", [Label, code:which(M), R])
    end,
    Try("a) load_binary(M, \"\", Bin)   [what the REPL does today]", "", fun() -> ok end),
    Try("b) load_binary(M, \"<source>.bt\", Bin)", "/nonexistent/src/Counter.bt", fun() -> ok end),
    Real = filename:join(Tmp, atom_to_list(M) ++ ".beam"),
    Try("c) Bin also written to a session dir; load path = that file", Real,
        fun() -> ok = file:write_file(Real, Beam) end),
    Ghost = filename:join([Tmp, "ghost", atom_to_list(M) ++ ".beam"]),
    Try("d) no .beam on disk; only <dir>/doc/chunks/M.chunk", Ghost,
        fun() -> eep48_proto:chunk_file(CoreFile, filename:dirname(Ghost)) end),
    %% e) what a runtime `doc:` setter would have to do: rewrite the side file
    {ok, D0} = code:get_doc(M),
    Chunk = filename:join([filename:dirname(Ghost), "doc", "chunks", atom_to_list(M) ++ ".chunk"]),
    ok = file:write_file(Chunk, term_to_binary(setelement(5, D0, #{<<"en">> => <<"Doc changed at runtime.">>}))),
    {ok, D1} = code:get_doc(M),
    io:format("e) after rewriting the side file: module_doc=~p~n", [element(5, D1)]).
