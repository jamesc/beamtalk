#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE (BT-4852, ADR 0149): compile-time cost of handing the Docs chunk to compile:forms/2.
%% Usage: escript e7_compile_cost.escript <file.core>...
main(Files) ->
    code:add_patha("_out"),
    N = 30,
    [begin
        T = fun(Mode) ->
            Ts = [element(1, timer:tc(fun() -> eep48_proto:compile_core(F, Mode) end)) || _ <- lists:seq(1, N)],
            lists:nth(N div 2, lists:sort(Ts))
        end,
        _ = T(nodocs),
        A = T(nodocs), B = T(docs),
        Core = eep48_proto:parse_core(F),
        {Td, _} = timer:tc(fun() -> [term_to_binary(eep48_proto:docs_from_core(Core), [deterministic, compressed]) || _ <- lists:seq(1, N)] end),
        io:format("OTP ~s ~s: median compile ~p us without Docs, ~p us with (~.1f%); building+encoding the chunk alone ~p us~n",
                  [erlang:system_info(otp_release), filename:basename(F), A, B, (B - A) * 100 / A, Td div N])
     end || F <- Files].
