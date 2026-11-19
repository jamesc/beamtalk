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
* every param a handler reads (`maps:get(<<"x">>, Params, …)` and friends) is
  declared in that op's table row (BT-4865);
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

%%% ---------------------------------------------------------------------------
%%% Per-op wire payloads (BT-5049)
%%% ---------------------------------------------------------------------------
%%%
%%% `fixtures/repl_op_payloads.json` holds one example reply per op. The Erlang
%%% side (these tests) encodes `beamtalk_repl_op_payload_examples` through
%%% `beamtalk_repl_ops:encode/2` and compares; the Rust `beamtalk-repl-protocol`
%%% tests (`tests/op_payloads.rs`) and the VS Code extension tests
%%% (`src/__tests__/opPayloads.test.ts`) decode the same file into their client
%%% types. The ops handlers that build rows (`list-classes`, `methods`,
%%% `inherited-methods`, `browse-classes`) are tied to the fixture rows by
%%% `beamtalk_test_wire_payloads:assert_row_shape/3` in their own test modules.

payload_fixture() ->
    beamtalk_test_wire_payloads:fixture().

payload_examples_match_fixture_test() ->
    #{<<"payloads">> := Payloads} = payload_fixture(),
    Actual = beamtalk_repl_op_payload_examples:encode_all(),
    Stale = [
        {Id, json:encode(maps:get(Id, Actual, missing))}
     || Id <- lists:usort(maps:keys(Actual) ++ maps:keys(Payloads)),
        maps:get(Id, Actual, missing) =/= response_of(Id, Payloads)
    ],
    case Stale of
        [] ->
            ok;
        _ ->
            erlang:error(
                {repl_op_payload_fixture_stale,
                    <<
                        "runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json no longer "
                        "matches what the encoders produce for these payloads. If the wire change is "
                        "intended, update their `response` to the JSON below (and the Rust and "
                        "TypeScript decoders if they need the new field); otherwise fix the encoder."
                    >>,
                    [{Id, iolist_to_binary(J)} || {Id, J} <- Stale]}
            )
    end.

response_of(Id, Payloads) ->
    case Payloads of
        #{Id := #{<<"response">> := R}} -> R;
        _ -> missing
    end.

payload_ops_match_the_examples_test() ->
    #{<<"payloads">> := Payloads} = payload_fixture(),
    ?assertEqual(
        lists:sort([{Id, Op} || {Id, Op, _} <- beamtalk_repl_op_payload_examples:examples()]),
        lists:sort([{Id, Op} || Id := #{<<"op">> := Op} <- Payloads])
    ).

%% Every table op has a payload or a stated reason for none, never both: a new
%% op cannot reach the wire without the fixture (and so the Rust and TypeScript
%% clients) being told about it.
every_op_is_pinned_or_explained_test() ->
    #{<<"payloads">> := Payloads, <<"unpinned">> := Unpinned} = payload_fixture(),
    Pinned = lists:usort([Op || _ := #{<<"op">> := Op} <- Payloads]),
    Explained = lists:sort(maps:keys(Unpinned)),
    ?assertEqual(lists:sort(op_names()), lists:sort(Pinned ++ Explained)),
    ?assertEqual([], [Op || Op <- Explained, lists:member(Op, Pinned)]),
    ?assertEqual([], [Op || Op := Reason <- Unpinned, not is_binary(Reason) orelse Reason =:= <<>>]).

%%% ---------------------------------------------------------------------------
%%% Handlers read the params the table declares (BT-4865, BT-5049)
%%% ---------------------------------------------------------------------------
%%%
%%% The scan follows the request params through each op's handler clause:
%%%
%%% * the root is the op's `handle_term/4` clause (the second argument holds the
%%%   params) and, for any op `beamtalk_ws_handler:dispatch_protocol_op/4` also
%%%   routes, that clause too (the second argument is the whole message; the
%%%   params are what `beamtalk_repl_protocol:get_params/1` returns from it);
%%% * a variable bound to the params (`P = get_params(Msg)`, whatever its name)
%%%   holds them, and so does the parameter of every local function the params
%%%   (or the message) are passed to, at that argument position;
%%% * a literal key is read by `maps:get/find/is_key/take(<<"k">>, P, …)`, by a
%%%   map pattern on the params, `binary_param(<<"k">>, P)`, and by the
%%%   `beamtalk_repl_params` helpers (`nonempty_binary(P, <<"k">>)`, `actor_pid(P)`).
%%%
%%% Any other use that hands the params out of the scan's sight (a computed key,
%%% `maps:with/2`, a remote call it does not know) is reported as `opaque`: the
%%% test fails and names it, so a handler the scan cannot analyse is never
%%% silently treated as reading nothing. Limit: the *message* passed to a remote
%%% function is assumed not to read params there (it is only ever passed to the
%%% JSON encoders and `beamtalk_repl_protocol` accessors).

%% Envelope fields every message carries; never op params.
-define(ENVELOPE, [<<"op">>, <<"id">>, <<"session">>]).

handlers_read_only_declared_params_test() ->
    Problems = lists:append([
        undeclared_reads(Row)
     || Row <- beamtalk_repl_op_table:ops()
    ]),
    ?assertEqual([], Problems).

%% A required param no handler clause reads is a stale declaration: the Rust
%% `RequestBuilder` and both editor clients would keep sending it for nothing.
handlers_read_every_required_param_test() ->
    Problems = lists:append([
        [
            {Op, required_param_never_read, Key}
         || Key <- P, not lists:member(Key, op_reads(Row))
        ]
     || #{op := Op, params := P} = Row <- beamtalk_repl_op_table:ops()
    ]),
    ?assertEqual([], Problems).

%% Nothing the scan cannot analyse is allowed through unreported.
handler_param_use_is_fully_analysable_test() ->
    Problems = lists:append([
        [{Op, opaque_param_use, O} || O <- op_opaque(Row)]
     || #{op := Op} = Row <- beamtalk_repl_op_table:ops()
    ]),
    ?assertEqual([], Problems).

%% The scanner itself must see reads, or the tests above pass vacuously.
param_scanner_sees_known_reads_test() ->
    ?assertEqual(
        [<<"before">>, <<"class">>, <<"code">>, <<"side">>, <<"suffix">>],
        lists:sort(op_reads(row(<<"hover">>)))
    ),
    ?assertEqual([<<"actor">>], op_reads(row(<<"kill">>))),
    %% Reads made through `beamtalk_repl_params` helpers stay visible.
    ?assertEqual([<<"actor">>], op_reads(row(<<"pid-stats">>))),
    NavReads = op_reads(row(<<"nav-query">>)),
    ?assert(lists:member(<<"selector">>, NavReads)),
    ?assert(lists:member(<<"class">>, NavReads)),
    ?assert(lists:member(<<"module">>, NavReads)),
    %% WebSocket-handler ops are scanned through their dispatch clause, whose
    %% handlers name the params variable `Params` only by convention.
    ?assertEqual([<<"value">>], op_reads(row(<<"stdin">>))),
    ?assertEqual([<<"level">>], op_reads(row(<<"subscribe-logs">>))),
    ?assertEqual(
        [<<"args">>, <<"class">>, <<"selector">>], lists:sort(op_reads(row(<<"run-entry">>)))
    ).

%% The scan is not tied to the variable name `Params` or to a handler shape.
param_scanner_follows_params_under_any_name_test() ->
    Forms = forms_of(
        "-module(scan_fixture).\n"
        "handle_term(<<\"a\">>, Req, _Msg, _S) -> helper(1, Req);\n"
        "handle_term(<<\"b\">>, #{<<\"k\">> := V}, _Msg, _S) -> V;\n"
        "handle_term(<<\"c\">>, _P, Msg, _S) -> via_msg(Msg);\n"
        "handle_term(<<\"d\">>, P, _Msg, _S) -> maps:with([<<\"x\">>], P);\n"
        "handle_term(<<\"e\">>, P, _Msg, _S) -> other:read(P);\n"
        "handle_term(<<\"f\">>, P, _Msg, _S) -> maps:get(dyn(), P).\n"
        "handle_term(<<\"g\">>, P, _Msg, _S) ->\n"
        "    Text = fun(Key) -> maps:get(Key, P, <<>>) end, {Text(<<\"before\">>), Text(<<\"code\">>)}.\n"
        "helper(_N, Q) -> R = Q, maps:get(<<\"deep\">>, R, undefined).\n"
        "via_msg(M) -> W = beamtalk_repl_protocol:get_params(M), maps:is_key(<<\"w\">>, W).\n"
        "dyn() -> <<\"x\">>.\n"
    ),
    Scan = fun(Op) -> scan_op(fun(scan_fixture) -> Forms end, scan_fixture, Op, handle_term) end,
    ?assertEqual({[<<"deep">>], []}, Scan(<<"a">>)),
    ?assertEqual({[<<"k">>], []}, Scan(<<"b">>)),
    ?assertEqual({[<<"w">>], []}, Scan(<<"c">>)),
    ?assertEqual({[], [{maps, with, 2}]}, Scan(<<"d">>)),
    ?assertEqual({[], [{other, read, 1}]}, Scan(<<"e">>)),
    ?assertEqual({[], [{computed_key, get}]}, Scan(<<"f">>)),
    %% A `Text = fun(Key) -> maps:get(Key, P) end` reader names its keys at the
    %% call sites.
    ?assertEqual({[<<"before">>, <<"code">>], []}, Scan(<<"g">>)).

row(Op) ->
    [Row] = [R || #{op := O} = R <- beamtalk_repl_op_table:ops(), O =:= Op],
    Row.

undeclared_reads(#{op := Op, params := P, optional := O} = Row) ->
    [
        {Op, undeclared_param, Key}
     || Key <- op_reads(Row),
        not lists:member(Key, P ++ O ++ ?ENVELOPE)
    ].

op_reads(Row) -> element(1, op_scan(Row)).

op_opaque(Row) -> element(2, op_scan(Row)).

%% `{Keys, Opaque}` over the op's `handle_term/4` clause in its handler module
%% and its `dispatch_protocol_op/4` clause in `beamtalk_ws_handler`.
op_scan(#{op := Op, handler := Handler}) ->
    Roots = [{Handler, handle_term} || Handler =/= ?WS] ++ [{?WS, dispatch_protocol_op}],
    Scans = [scan_op(fun abstract_code/1, M, Op, Fn) || {M, Fn} <- Roots],
    {
        lists:usort(lists:append([K || {K, _} <- Scans])),
        lists:append([O || {_, O} <- Scans])
    }.

forms_of(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    [
        begin
            {ok, Form} = erl_parse:parse_form(Ts),
            Form
        end
     || Ts <- split_forms(Tokens, [], [])
    ].

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, _} = D | Rest], Cur, Acc) ->
    split_forms(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_forms([T | Rest], Cur, Acc) ->
    split_forms(Rest, [T | Cur], Acc).

abstract_code(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    {Module, Beam, _} = code:get_object_code(Module),
    {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(Beam, [abstract_code]),
    Forms.

%% Scan `Op`'s clauses of `RootFn/4` in `Module`. `Load(Module)` returns a
%% module's abstract forms. `{SortedKeys, Opaque}`.
scan_op(Load, Module, Op, RootFn) ->
    Roots = [
        C
     || {function, _, Fn, 4, Clauses} <- Load(Module),
        Fn =:= RootFn,
        {clause, _, [{bin, _, [{bin_element, _, {string, _, S}, _, _}]} | _], _, _} = C <- Clauses,
        list_to_binary(S) =:= Op
    ],
    %% `handle_term(Op, Params, Msg, …)` holds the params in argument 2 and the
    %% message in 3; `dispatch_protocol_op(Op, Msg, …)` holds the message in 2.
    Positions =
        case RootFn of
            handle_term -> #{2 => params, 3 => msg};
            dispatch_protocol_op -> #{2 => msg}
        end,
    Todo = [{clause_of, Module, C, Positions} || C <- Roots],
    {Keys, Opaque} = walk(Todo, Load, #{}, #{}, [], []),
    {lists:usort(Keys), lists:usort(Opaque)}.

%% Worklist over clauses. `Seen` keys function + which argument positions carry
%% the params/message, so a helper reached with different positions is rescanned.
%% `Mods` caches each module's `{Name, Arity} => Clauses` table.
walk([], _Load, _Mods, _Seen, Keys, Opaque) ->
    {Keys, Opaque};
walk([{clause_of, Mod, Clause, Positions} | Rest], Load, Mods, Seen, Keys, Opaque) ->
    {K, O, Calls} = scan_clause(Mod, Clause, Positions),
    {Next, Seen1, Mods1} = lists:foldl(
        fun({{M, Name, Arity} = MFA, Pos}, {Acc, S, Ms}) ->
            case S of
                #{{MFA, Pos} := _} ->
                    {Acc, S, Ms};
                _ ->
                    {Funs, Ms1} = module_funs(M, Load, Ms),
                    Clauses = maps:get({Name, Arity}, Funs, []),
                    {[{clause_of, M, C, Pos} || C <- Clauses] ++ Acc, S#{{MFA, Pos} => true}, Ms1}
            end
        end,
        {[], Seen, Mods},
        Calls
    ),
    walk(Next ++ Rest, Load, Mods1, Seen1, K ++ Keys, O ++ Opaque).

module_funs(Module, Load, Mods) ->
    case Mods of
        #{Module := Funs} ->
            {Funs, Mods};
        _ ->
            Funs =
                try
                    #{{N, A} => Cs || {function, _, N, A, Cs} <- Load(Module)}
                catch
                    _:_ -> #{}
                end,
            {Funs, Mods#{Module => Funs}}
    end.

%% Returns `{Keys, Opaque, [{{Module, Name, Arity}, Positions}]}` for one clause,
%% given which head argument positions hold the params (`params`) or the message
%% (`msg`).
scan_clause(Module, {clause, _, Head, _Guards, Body}, Positions) ->
    {HeadParams, HeadMsgs, HeadKeys} = head_vars(Head, Positions),
    {Params, Msgs} = bound_vars(Body, HeadParams, HeadMsgs),
    Ctx = #{
        module => Module,
        is_params => fun(E) -> is_params_expr(E, Params) end,
        msgs => Msgs,
        readers => key_readers(Body)
    },
    Found = collect(fun(Node) -> node_use(Node, Ctx) end, Body),
    Keys = HeadKeys ++ [K || {read, K} <- Found],
    Opaque = [O || {opaque, O} <- Found],
    Calls = [{MFA, Pos} || {call, MFA, Pos} <- Found],
    {Keys, Opaque, Calls}.

%% `Text = fun(Key) -> … maps:get(Key, Params, …) … end` is a key reader: the
%% keys it reads are the literal arguments of its `Text(<<"k">>)` calls.
%% `#{KeyVar => FunVar}`.
key_readers(Body) ->
    maps:from_list(
        collect(
            fun
                (
                    {match, _, {var, _, F},
                        {'fun', _, {clauses, [{clause, _, [{var, _, K}], _, _}]}}}
                ) ->
                    [{K, F}];
                (_) ->
                    []
            end,
            Body
        )
    ).

head_vars(Head, Positions) ->
    lists:foldl(
        fun({Idx, Pat}, {Ps, Ms, Ks}) ->
            case {maps:get(Idx, Positions, none), Pat} of
                {params, {var, _, V}} -> {[V | Ps], Ms, Ks};
                {params, {map, _, Assocs}} -> {Ps, Ms, map_pattern_keys(Assocs) ++ Ks};
                {msg, {var, _, V}} -> {Ps, [V | Ms], Ks};
                _ -> {Ps, Ms, Ks}
            end
        end,
        {[], [], []},
        lists:zip(lists:seq(1, length(Head)), Head)
    ).

map_pattern_keys(Assocs) ->
    lists:append([literal_key(K) || {map_field_exact, _, K, _} <- Assocs]).

%% Variables bound, anywhere in the clause, to the params (`P = get_params(M)`,
%% `P2 = P`) or the message (`M2 = M`): flow-insensitive, to a fixpoint.
bound_vars(Body, Params0, Msgs0) ->
    Matches = collect(
        fun
            ({match, _, {var, _, V}, Rhs}) -> [{V, Rhs}];
            (_) -> []
        end,
        Body
    ),
    bound_fix(Matches, Params0, Msgs0).

bound_fix(Matches, Params, Msgs) ->
    {P1, M1} = lists:foldl(
        fun({V, Rhs}, {Ps, Ms}) ->
            case {is_params_expr(Rhs, Ps), Rhs} of
                {true, _} ->
                    {add(V, Ps), Ms};
                {false, {var, _, W}} ->
                    case lists:member(W, Ms) of
                        true -> {Ps, add(V, Ms)};
                        false -> {Ps, Ms}
                    end;
                _ ->
                    {Ps, Ms}
            end
        end,
        {Params, Msgs},
        Matches
    ),
    case {P1, M1} =:= {Params, Msgs} of
        true -> {Params, Msgs};
        false -> bound_fix(Matches, P1, M1)
    end.

add(V, L) -> [V | L -- [V]].

is_params_expr({var, _, V}, Params) ->
    lists:member(V, Params);
is_params_expr(
    {call, _, {remote, _, {atom, _, beamtalk_repl_protocol}, {atom, _, get_params}}, [_]}, _
) ->
    true;
is_params_expr(_, _) ->
    false.

%% What one abstract-format node does with the params: a literal key read, a
%% call that passes the params/message on (a call to follow), or a use the scan
%% cannot see through (`opaque`).
node_use({match, _, {map, _, Assocs}, Rhs}, #{is_params := IsParams}) ->
    case IsParams(Rhs) of
        true -> [{read, K} || K <- map_pattern_keys(Assocs)];
        false -> []
    end;
node_use({call, _, {var, _, F}, [Arg]}, #{readers := Readers}) ->
    %% A call of a key-reader fun: the literal argument is the key read.
    case lists:member(F, maps:values(Readers)) of
        true -> [{read, K} || K <- literal_key(Arg)];
        false -> []
    end;
node_use({call, _, Callee, Args}, Ctx) ->
    call_use(Callee, Args, Ctx);
node_use(_, _) ->
    [].

call_use({remote, _, {atom, _, maps}, {atom, _, F}}, [K, P | _], Ctx) when
    F =:= get; F =:= find; F =:= is_key; F =:= take
->
    #{is_params := IsParams, readers := Readers} = Ctx,
    case IsParams(P) of
        false ->
            [];
        true ->
            case {literal_key(K), K} of
                {[], {var, _, KeyVar}} ->
                    %% Computed keys are fine inside a key-reader fun (its call
                    %% sites name the keys); anywhere else they are opaque.
                    case maps:is_key(KeyVar, Readers) of
                        true -> [];
                        false -> [{opaque, {computed_key, F}}]
                    end;
                {[], _} ->
                    [{opaque, {computed_key, F}}];
                {Keys, _} ->
                    [{read, Key} || Key <- Keys]
            end
    end;
call_use({atom, _, binary_param}, [K, P], #{is_params := IsParams}) ->
    case IsParams(P) of
        true -> [{read, Key} || Key <- literal_key(K)];
        false -> []
    end;
call_use(
    {remote, _, {atom, _, beamtalk_repl_params}, {atom, _, nonempty_binary}},
    [P, K],
    #{is_params := IsParams}
) ->
    case IsParams(P) of
        true -> [{read, Key} || Key <- literal_key(K)];
        false -> []
    end;
call_use(
    {remote, _, {atom, _, beamtalk_repl_params}, {atom, _, actor_pid}}, [P], #{
        is_params := IsParams
    }
) ->
    case IsParams(P) of
        true -> [{read, <<"actor">>}];
        false -> []
    end;
call_use({atom, _, Name}, Args, #{module := Module} = Ctx) ->
    follow(Module, Name, Args, Ctx);
call_use({remote, _, {atom, _, M}, {atom, _, F}}, Args, #{is_params := IsParams} = Ctx) ->
    case atom_to_list(M) of
        "beamtalk_repl_ops" ++ _ ->
            %% The handler family: follow the params into the sibling module.
            follow(M, F, Args, Ctx);
        _ ->
            case lists:any(IsParams, Args) of
                true -> [{opaque, {M, F, length(Args)}}];
                false -> []
            end
    end;
call_use(_, _Args, _) ->
    [].

%% Follow a call into `Module:Name/Arity` with the argument positions that carry
%% the params or the message.
follow(Module, Name, Args, #{is_params := IsParams, msgs := Msgs}) ->
    Indexed = lists:zip(lists:seq(1, length(Args)), Args),
    Pos = maps:from_list(
        [{I, params} || {I, A} <- Indexed, IsParams(A)] ++
            [{I, msg} || {I, {var, _, V}} <- Indexed, lists:member(V, Msgs)]
    ),
    [{call, {Module, Name, length(Args)}, Pos} || map_size(Pos) > 0].

literal_key({bin, _, [{bin_element, _, {string, _, S}, _, _}]}) -> [list_to_binary(S)];
literal_key(_) -> [].

%% Pre-order fold of `Fun` over every abstract-format node in `Term`.
collect(Fun, Term) when is_tuple(Term) ->
    Fun(Term) ++ lists:append([collect(Fun, X) || X <- tuple_to_list(Term)]);
collect(Fun, Term) when is_list(Term) ->
    lists:append([collect(Fun, X) || X <- Term]);
collect(_Fun, _Term) ->
    [].
