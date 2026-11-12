#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Regenerate the shared REPL op-table fixture from `beamtalk_repl_op_table`
%% (BT-4673). Usage: gen_op_table_fixture.escript <beamtalk_runtime ebin dir> <out file>
main([Ebin, Out]) ->
    true = code:add_patha(Ebin),
    Lines = [iolist_to_binary(json:encode(E)) || E <- beamtalk_repl_op_table:to_json_term()],
    Body = iolist_to_binary(lists:join(<<",\n  ">>, Lines)),
    ok = file:write_file(Out, [<<"[\n  ">>, Body, <<"\n]\n">>]);
main(_) ->
    io:format(standard_error, "usage: gen_op_table_fixture.escript <ebin> <out>~n", []),
    halt(2).
