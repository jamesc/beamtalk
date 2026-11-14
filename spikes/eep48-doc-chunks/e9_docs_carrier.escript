#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149). Can codegen hand the finished docs_v1 term to
%% the build worker inside the Core Erlang module itself? A module attribute cannot (binaries
%% are a syntax error in Core attribute constants), so this uses a constant
%% function, `__beamtalk_docs/0`, that the worker lifts out, turns into the Docs
%% chunk, and removes before compiling (the `__beamtalk_meta/0` reading pattern of
%% beamtalk_compiler:class_meta_from_core_erlang/1).
main(_) ->
    {ok, Bin} = file:read_file("fixture/docs_carrier.core"),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Core0} = core_parse:parse(Toks),
    IsCarrier = fun(N) -> cerl:fname_id(N) =:= '__beamtalk_docs' andalso cerl:fname_arity(N) =:= 0 end,
    {[{_, Fun}], Defs} = lists:partition(fun({N, _}) -> IsCarrier(N) end, cerl:module_defs(Core0)),
    Docs = cerl:concrete(cerl:fold_literal(cerl:fun_body(Fun))),
    Core = cerl:update_c_module(Core0, cerl:module_name(Core0),
                                [E || E <- cerl:module_exports(Core0), not IsCarrier(E)],
                                cerl:module_attrs(Core0), Defs),
    Chunk = term_to_binary(Docs, [deterministic, compressed]),
    {ok, M, Beam} = compile:forms(Core, [from_core, binary, return_errors, debug_info, clint,
                                         {extra_chunks, [{<<"Docs">>, Chunk}]}]),
    {ok, {M, [{exports, Ex}, {"Docs", D}]}} = beam_lib:chunks(Beam, [exports, "Docs"]),
    io:format("OTP ~s: docs term read from the carrier = ~p~n  carrier function left in the .beam = ~p~n"
              "  Docs chunk round-trips = ~p~n",
              [erlang:system_info(otp_release), Docs, lists:member({'__beamtalk_docs', 0}, Ex),
               binary_to_term(D) =:= Docs]).
