%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE for the BT-4852 spike (ADR 0149). Not production code.
%%
%% "Thin layer that reads from class_state" (ADR 0033): build docs_v1 terms
%% from the live class processes and write them as `doc/chunks/<mod>.chunk`
%% side files, the fallback location `code:get_doc/1` already reads.
%%
%% Reads the private #class_state{} record by position through sys:get_state/1.
%% A real implementation would add a proper accessor; this is a measurement rig.
-module(eep48_runtime_proto).
-export([docs_for/1, write_all/1, report/1]).

-include_lib("kernel/include/eep48.hrl").

-define(NAME, 2).
-define(MODULE_, 3).
-define(SUPER, 4).
-define(IMETHODS, 9).
-define(CMETHODS, 10).
-define(ISIGS, 16).
-define(CSIGS, 17).
-define(DOC, 20).
-define(IDOCS, 21).
-define(CDOCS, 22).

docs_for(Pid) ->
    S = sys:get_state(Pid),
    class_state = element(1, S),
    Class = element(?NAME, S),
    ModDoc = case element(?DOC, S) of
        B when is_binary(B) -> #{<<"en">> => B};
        _ -> none
    end,
    Docs = entries(false, Class, element(?IMETHODS, S), element(?IDOCS, S), element(?ISIGS, S)) ++
           entries(true, Class, element(?CMETHODS, S), element(?CDOCS, S), element(?CSIGS, S)),
    {element(?MODULE_, S),
     #docs_v1{anno = erl_anno:new(1), beam_language = beamtalk, format = <<"text/markdown">>,
              module_doc = ModDoc,
              metadata = #{beamtalk_class => Class, superclass => element(?SUPER, S)},
              docs = lists:sort(Docs)}}.

entries(ClassSide, Class, Methods, Docs, Sigs) ->
    [begin
        Arity = case Info of #{arity := A} -> A + 1; _ -> 1 end,
        Name = case ClassSide of
            true -> list_to_atom("class_" ++ atom_to_list(Sel));
            false -> Sel
        end,
        Sep = case ClassSide of true -> <<" class >> ">>; false -> <<" >> ">> end,
        Sig = <<(atom_to_binary(Class))/binary, Sep/binary,
                (maps:get(Sel, Sigs, atom_to_binary(Sel)))/binary>>,
        Doc = case maps:get(Sel, Docs, none) of
            D when is_binary(D) -> #{<<"en">> => D};
            _ -> #{}
        end,
        {{function, Name, Arity}, erl_anno:new(1), [Sig], Doc,
         #{selector => Sel, class_side => ClassSide}}
     end || {Sel, Info} <- maps:to_list(Methods)].

write_all(Dir) ->
    ChunkDir = filename:join([Dir, "doc", "chunks"]),
    ok = filelib:ensure_path(ChunkDir),
    lists:foldl(fun(Pid, {N, Raw, Comp, Entries, Documented}) ->
        {Mod, D} = docs_for(Pid),
        Bin = term_to_binary(D, [deterministic, compressed]),
        ok = file:write_file(filename:join(ChunkDir, atom_to_list(Mod) ++ ".chunk"), Bin),
        Es = D#docs_v1.docs,
        {N + 1, Raw + byte_size(term_to_binary(D)), Comp + byte_size(Bin), Entries + length(Es),
         Documented + length([E || E <- Es, map_size(element(4, E)) > 0])}
    end, {0, 0, 0, 0, 0}, beamtalk_class_registry:all_classes()).

report(Ebin) ->
    {ok, _} = application:ensure_all_started(beamtalk_stdlib),
    timer:sleep(500),
    {Micros, {N, Raw, Comp, Entries, Documented}} = timer:tc(fun() -> write_all(filename:dirname(Ebin)) end),
    BeamBytes = lists:sum([filelib:file_size(F) || F <- filelib:wildcard(filename:join(Ebin, "bt@stdlib@*.beam"))]),
    io:format("OTP ~s: ~p classes, ~p method entries (~p with doc text)~n"
              "  docs_v1 total: ~p bytes raw, ~p bytes compressed; stdlib .beam total: ~p bytes (+~.1f%)~n"
              "  generated + written in ~p ms (~p us/class)~n",
              [erlang:system_info(otp_release), N, Entries, Documented, Raw, Comp, BeamBytes,
               Comp * 100 / BeamBytes, Micros div 1000, Micros div N]),
    M = 'bt@stdlib@integer',
    {ok, D} = code:get_doc(M),
    io:format("  code:which(~s)=~s~n  code:get_doc -> language=~p, ~p entries~n",
              [M, code:which(M), D#docs_v1.beam_language, length(D#docs_v1.docs)]).
