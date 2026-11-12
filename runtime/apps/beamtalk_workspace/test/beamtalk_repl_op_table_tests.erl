%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_op_table_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for `beamtalk_repl_op_table` (BT-4673) — the one REPL op table
that drives `beamtalk_repl_ops:route/4`, the `describe` op and the capability
check.

Covers:
* op names are unique and every entry is well-formed;
* every op the table routes is handled by a real `handle_term/4`, and the ops
  the router refuses are exactly the WebSocket-transport ones;
* `describe` advertises every op in the table (including the save ops);
* the capability classification of REPL ops comes from the table;
* the table matches the shared fixture `fixtures/repl_op_table.json`, which the
  Rust `RequestBuilder` conformance test, `beamtalk-surface-drift` and the
  `docs/repl-protocol.md` check read. To refresh the fixture after editing the
  table, see the failure message of `table_matches_shared_fixture_test/0`.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-define(WS, beamtalk_ws_handler).

op_names() ->
    [Op || #{op := Op} <- beamtalk_repl_op_table:ops()].

ws_ops() ->
    [Op || #{op := Op, handler := ?WS} <- beamtalk_repl_op_table:ops()].

op_names_are_unique_test() ->
    Names = op_names(),
    ?assertEqual(length(Names), length(lists:usort(Names))).

entries_are_well_formed_test() ->
    lists:foreach(
        fun(#{op := Op, handler := H, params := P, optional := O, capability := C}) ->
            ?assert(is_binary(Op)),
            ?assert(is_atom(H)),
            ?assert(lists:all(fun is_binary/1, P)),
            ?assert(lists:all(fun is_binary/1, O)),
            %% A param is either required or optional, never both.
            ?assertEqual([], [X || X <- P, lists:member(X, O)]),
            ?assert(lists:member(C, [always, compiler, workspace]))
        end,
        beamtalk_repl_op_table:ops()
    ).

ws_ops_are_the_transport_ops_test() ->
    ?assertEqual(
        [<<"run-entry">>, <<"stdin">>, <<"subscribe-logs">>, <<"unsubscribe-logs">>],
        lists:sort(ws_ops())
    ).

routed_handlers_export_handle_term_test() ->
    Handlers = lists:usort([
        H
     || #{handler := H} <- beamtalk_repl_op_table:ops(), H =/= ?WS
    ]),
    lists:foreach(
        fun(H) ->
            ?assertMatch({module, H}, code:ensure_loaded(H)),
            ?assert(erlang:function_exported(H, handle_term, 4))
        end,
        Handlers
    ).

router_refuses_ws_and_unknown_ops_as_unknown_test() ->
    Msg = {protocol_msg, <<"x">>, <<"id">>, undefined, #{}},
    lists:foreach(
        fun(Op) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = unknown_op}},
                beamtalk_repl_ops:dispatch(Op, #{}, Msg, self())
            )
        end,
        [<<"no-such-op">> | ws_ops()]
    ).

describe_advertises_every_table_op_test() ->
    Described = beamtalk_repl_op_table:describe(),
    ?assertEqual(lists:sort(op_names()), lists:sort(maps:keys(Described))),
    %% The two save ops were routed but missing from `describe` before BT-4673.
    ?assert(maps:is_key(<<"save-section">>, Described)),
    ?assert(maps:is_key(<<"save-native-source">>, Described)),
    ?assert(maps:is_key(<<"pid-stats">>, Described)).

describe_op_returns_the_table_test() ->
    Msg = {protocol_msg, <<"describe">>, <<"id">>, undefined, #{}},
    {describe, Ops, _Versions} = beamtalk_repl_ops:dispatch(<<"describe">>, #{}, Msg, self()),
    ?assertEqual(beamtalk_repl_op_table:describe(), Ops).

describe_descriptor_shape_test() ->
    #{
        <<"eval">> := Eval,
        <<"health">> := Health,
        <<"export-traces">> := Export
    } = beamtalk_repl_op_table:describe(),
    ?assertEqual(#{<<"params">> => [<<"code">>], <<"optional">> => [<<"trace">>]}, Eval),
    %% `optional` is omitted, not empty, for ops with no optional params.
    ?assertEqual(#{<<"params">> => []}, Health),
    ?assert(maps:is_key(<<"notes">>, Export)).

describe_for_filters_by_handler_test() ->
    Ops = beamtalk_repl_op_table:describe_for(beamtalk_repl_ops_nav),
    ?assertEqual([<<"nav-query">>], maps:keys(Ops)).

capability_comes_from_the_table_test() ->
    lists:foreach(
        fun(#{op := Op, capability := Cap}) ->
            ?assertEqual(Cap, beamtalk_capability:classify(Op))
        end,
        beamtalk_repl_op_table:ops()
    ),
    ?assertEqual(compiler, beamtalk_capability:classify(<<"eval">>)),
    ?assertEqual(workspace, beamtalk_capability:classify(<<"save-section">>)),
    ?assertEqual(workspace, beamtalk_capability:classify(<<"save-native-source">>)),
    %% An op the table does not name needs nothing a release lacks.
    ?assertEqual(always, beamtalk_capability:classify(<<"no-such-op">>)).

table_matches_shared_fixture_test() ->
    Fixture = beamtalk_test_corpus:load_json_fixture([
        "runtime", "apps", "beamtalk_workspace", "test", "fixtures", "repl_op_table.json"
    ]),
    Expected = json:decode(iolist_to_binary(json:encode(beamtalk_repl_op_table:to_json_term()))),
    case Fixture =:= Expected of
        true ->
            ok;
        false ->
            erlang:error(
                {repl_op_table_fixture_stale, <<
                    "runtime/apps/beamtalk_workspace/test/fixtures/repl_op_table.json no longer "
                    "matches beamtalk_repl_op_table. Regenerate it with `just gen-op-table-fixture`."
                >>}
            )
    end.
