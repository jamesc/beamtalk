#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% ADR 0137 Phase 0 (BT-4007): gates 1-3 of the live-state model, measured in
%% one VM, interleaved, so the gen_server round trip (about 6 us, and the
%% dominant noise source) is out of the measurement.
%%
%% SELF-CONTAINED: the flagged compiler prototype is gone (it lives on the
%% BT-4007-adr0137-phase0-prototype branch), so nothing here needs a flag. The
%% variants are
%%   - "today": the Core Erlang the CURRENT compiler generates for
%%     `runtime/perf/self_send_bench` (field, open-actor and sealed-actor probe
%%     classes), loaded under distinct names;
%%   - "ADR as written" for self-sends: that same generated Core Erlang,
%%     hand-lowered by regex edits whose match counts are HARD assertions (an
%%     edit that stops matching, or matches more than expected, aborts the run);
%%   - "bracketed" for gate 2: two hand-written Erlang modules in this file
%%     (`sends_module/2`), the first threading `State` as the compiler does, the
%%     second with the `put(cell, State)` / `get(cell)` bracket around each
%%     send, in the shape ADR 0137 section 1 gives. The send is the same external
%%     call the generated code makes (`beamtalk_message_dispatch:send/3`, stubbed
%%     to `erlang:max/min` here).
%%   - gate 1 has no flagged variant any more: an intrinsic-only method emits no
%%     bracket by construction, so the A/A row (the same generated module under
%%     two names) is the noise floor the 1.05x bound is read against.
%%
%% Setup (one build, no flag):
%%   cp -R runtime/perf/self_send_bench /path/bench
%%   (cd /path/bench && beamtalk build)
%%   ERL_FLAGS="+S 1 -pa runtime/_build/default/lib/beamtalk_runtime/ebin" \
%%     escript runtime/perf/bench_adr0137_gates.escript /path/bench 101 100000
%% The runtime ebin is needed only for `beamtalk_actor:make_self/1`, which the
%% sealed self-send calls. Run it on a quiet machine and record `uptime`.
%%
%% Assumption of the hand-lowering (single state effect): each self-send site
%% is `let _SDn = case <dispatch>(.., StateVar) of ... end in let NextState =
%% element(2, _SDn) in`, i.e. the dispatch is the statement's only state effect,
%% so "load the next State from the cell" can replace `element(2, _SDn)`. The
%% state variable stored before the dispatch is captured from the dispatch call
%% itself (not assumed to be named `StateAcc`).
main([PkgDir, BatchesS, RepsS]) ->
    Batches = list_to_integer(BatchesS),
    Reps = list_to_integer(RepsS),
    SelfReps = max(1, Reps div 50),
    ok = load_dispatch_stub(),
    Field = "ssb_field_probe_actor",
    Open = "ssb_open_self_probe_actor",
    Sealed = "ssb_sealed_self_probe_actor",
    FieldCore = core(PkgDir, Field),
    OpenCore = core(PkgDir, Open),
    SealedCore = core(PkgDir, Sealed),
    {OpenLowered, OpenEdits} = hand_lower_open(OpenCore),
    {SealedLowered, SealedEdits} = hand_lower_sealed(SealedCore),
    io:format("hand-lowering edits (all asserted): open ~p, sealed ~p~n", [OpenEdits, SealedEdits]),
    Mods = [
        {field_a, load(FieldCore, Field, "bt4007_field_a")},
        {field_a2, load(FieldCore, Field, "bt4007_field_a2")},
        {sends_thr, compile_source(sends_module("bt4007_sends_thr", false))},
        {sends_br, compile_source(sends_module("bt4007_sends_br", true))},
        {open_a, load(OpenCore, Open, "bt4007_open_a")},
        {open_b, load(OpenLowered, Open, "bt4007_open_b")},
        {sealed_a, load(SealedCore, Sealed, "bt4007_sealed_a")},
        {sealed_b, load(SealedLowered, Sealed, "bt4007_sealed_b")}
    ],
    Mod = fun(K) -> proplists:get_value(K, Mods) end,
    St = fun(M) -> #{a => 1, b => 2, c => 3, n => 0, '$beamtalk_class' => 'Probe', '__class_mod__' => M} end,
    Self = {beamtalk_object, 'Probe', undefined, self()},
    Cases = [
        {"gate 1: field_body A/A (noise floor)", field_a, field_a2, 'body', [], 1, Reps},
        {"gate 2: field_sends (per send): threaded vs bracketed", sends_thr, sends_br, 'sends', [], 10, Reps},
        {"gate 3: open self-send top, today vs ADR", open_a, open_b, 'top:', [1000], 1000, SelfReps},
        {"gate 3: open self-send in do: arm, today vs ADR", open_a, open_b, 'arm:', [1000], 1000, SelfReps},
        {"gate 3: sealed self-send top, today vs ADR", sealed_a, sealed_b, 'top:', [1000], 1000, SelfReps},
        {"gate 3: sealed self-send in do: arm, today vs ADR", sealed_a, sealed_b, 'arm:', [1000], 1000, SelfReps}
    ],
    io:format("batches=~p field/sends reps=~p calls per batch; self-send cases reps=~p calls of 1000 "
              "self-sends per batch; schedulers=~p~n(ns per op: per call, per bracketed send, per self-send)~n",
              [Batches, Reps, SelfReps, erlang:system_info(schedulers_online)]),
    io:format("~-56s ~9s ~9s ~9s ~9s ~7s ~9s~n",
              ["case", "A med", "A p90", "B med", "B p90", "B/A", "B-A med"]),
    lists:foreach(fun({Name, A, B, Sel, Args, Per, N}) ->
        {AM, BM} = {Mod(A), Mod(B)},
        Run = fun(M) ->
            S0 = St(M),
            erlang:put('$bt_actor_state', S0),
            {T, _} = timer:tc(fun() -> loop(M, Sel, Args, Self, S0, N) end, nanosecond),
            T / (N * Per)
        end,
        _ = Run(AM), _ = Run(BM),
        Samples = [{Run(AM), Run(BM)} || _ <- lists:seq(1, Batches)],
        As = lists:sort([X || {X, _} <- Samples]),
        Bs = lists:sort([Y || {_, Y} <- Samples]),
        AMed = pct(As, 0.5), BMed = pct(Bs, 0.5),
        io:format("~-56s ~9.2f ~9.2f ~9.2f ~9.2f ~7.3f ~9.2f~n",
                  [Name, AMed, pct(As, 0.9), BMed, pct(Bs, 0.9), BMed / AMed, BMed - AMed])
    end, Cases),
    pair_bench(Batches).

pct(Sorted, Q) -> lists:nth(max(1, round(length(Sorted) * Q)), Sorted).

core(PkgDir, Class) ->
    Path = filename:join([PkgDir, "_build", "dev", "ebin", "bt@self_send_bench@" ++ Class ++ ".core"]),
    {ok, Bin} = file:read_file(Path),
    Bin.

%% `beamtalk_message_dispatch:send/3` for `max:`/`min:` on integers, the only
%% runtime call the field probe makes.
load_dispatch_stub() ->
    Src = "-module(beamtalk_message_dispatch).\n"
          "-export([send/3]).\n"
          "send(R, 'max:', [X]) -> erlang:max(R, X);\n"
          "send(R, 'min:', [X]) -> erlang:min(R, X).\n",
    compile_source(Src),
    ok.

compile_source(Src) ->
    {ok, Tokens, _} = erl_scan:string(Src),
    {ok, M, Bin} = compile:forms(split_forms(Tokens), [binary]),
    {module, M} = code:load_binary(M, "bt4007", Bin),
    M.

split_forms(Tokens) ->
    {Forms, []} = lists:foldl(fun
        ({dot, _} = D, {Acc, Cur}) ->
            {ok, F} = erl_parse:parse_form(lists:reverse([D | Cur])),
            {[F | Acc], []};
        (T, {Acc, Cur}) ->
            {Acc, [T | Cur]}
    end, {[], []}, Tokens),
    lists:reverse(Forms).

%% Gate 2 modules. Ten `self.x := a b: c` style statements.
sends_module(Name, Bracketed) ->
    Steps = [{a, b, 'max:', c}, {b, a, 'min:', c}, {c, a, 'max:', b}, {a, b, 'min:', c},
             {b, a, 'max:', c}, {c, a, 'min:', b}, {a, b, 'max:', c}, {b, a, 'min:', c},
             {c, a, 'max:', b}, {a, b, 'min:', c}],
    Stmts = lists:flatten([step(I, S, Bracketed) || {I, S} <- lists:zip(lists:seq(1, 10), Steps)]),
    lists:flatten(io_lib:format(
        "-module(~s).~n-export([dispatch/4]).~n"
        "dispatch(sends, [], _Self, S0) ->~n~s    {reply, maps:get(a, S10), S10}.~n",
        [Name, Stmts])).

step(I, {Target, Recv, Sel, Arg}, false) ->
    io_lib:format("    V~p = beamtalk_message_dispatch:send(maps:get(~p, S~p), ~p, [maps:get(~p, S~p)]),~n"
                  "    S~p = maps:put(~p, V~p, S~p),~n",
                  [I, Recv, I - 1, Sel, Arg, I - 1, I, Target, I, I - 1]);
step(I, {Target, Recv, Sel, Arg}, true) ->
    io_lib:format("    _ = erlang:put('$bt_actor_state', S~p),~n"
                  "    V~p = beamtalk_message_dispatch:send(maps:get(~p, S~p), ~p, [maps:get(~p, S~p)]),~n"
                  "    T~p = erlang:get('$bt_actor_state'),~n"
                  "    S~p = maps:put(~p, V~p, T~p),~n",
                  [I - 1, I, Recv, I - 1, Sel, Arg, I - 1, I, I, Target, I, I]).

%% Loads a generated module under a distinct name, without its `on_load` hook
%% (which registers the class with the runtime).
load(CoreBin, Class, NewName) ->
    Old = "bt@self_send_bench@" ++ Class,
    Renamed = binary:replace(CoreBin, list_to_binary("'" ++ Old ++ "'"),
                             list_to_binary("'" ++ NewName ++ "'"), [global]),
    NoHook = binary:replace(Renamed, <<"'on_load' = [{'register_class', 0}],">>, <<>>),
    Tmp = filename:join(os_tmp(), NewName ++ ".core"),
    ok = file:write_file(Tmp, NoHook),
    {ok, M, Bin} = compile:file(Tmp, [from_core, binary, report_errors]),
    {module, M} = code:load_binary(M, Tmp, Bin),
    M.

os_tmp() ->
    case os:getenv("TMPDIR") of
        false -> element(2, file:get_cwd());
        T -> T
    end.

%% ADR 0137 lowering of the open-actor self-send, by regex edits on the
%% generated Core Erlang: the caller stores the State the dispatch is called
%% with and loads the next one from the cell; the callee `foo` loads `State` from
%% the cell on entry and stores its last version on exit. Every edit asserts its
%% match count.
hand_lower_open(Core) ->
    S0 = binary_to_list(Core),
    {Pre, FooOn} = split_at(S0, "<'foo'> when 'true' ->"),
    {Foo0, Rest} = split_at(FooOn, "<'top:'> when 'true' ->"),
    {Foo, FooEdits} = lower_callee(Foo0),
    {Callers0, N1} = sub(Rest,
        "let (_SD[0-9]+) = case let (_SDArgs[0-9]+) = \\[\\] in let (_ClassMod[0-9]+) = "
        "call 'maps':'get'\\('__class_mod__', ([A-Za-z0-9_]+),",
        "let _CellStore\\1 = call 'erlang':'put'('$bt_actor_state', \\4) in "
        "let \\1 = case let \\2 = [] in let \\3 = call 'maps':'get'('__class_mod__', \\4,", 2),
    {Callers1, N2} = sub(Callers0,
        "let ([A-Za-z0-9_]+) = call 'erlang':'element'\\(2, (_SD[0-9]+)\\) in",
        "let \\1 = call 'erlang':'get'('$bt_actor_state') in", 2),
    {list_to_binary(Pre ++ Foo ++ Callers1), FooEdits + N1 + N2}.

%% The sealed actor's self-send is a direct call of `__sealed_foo/2`: the same
%% edits, the callee's entry load and exit store inside `__sealed_foo`.
hand_lower_sealed(Core) ->
    S0 = binary_to_list(Core),
    {Pre, FooOn} = split_at(S0, "'__sealed_foo'/2  ="),
    {Foo0, Rest} = split_at(FooOn, "'__sealed_top:'/3  ="),
    {Foo, FooEdits} = lower_callee(Foo0),
    All = Pre ++ Foo ++ Rest,
    {Callers0, N1} = sub(All,
        "let (_SD[0-9]+) = case call '([^']+)':'__sealed_foo'\\((_SealedSelf[0-9]+), ([A-Za-z0-9_]+)\\)",
        "let _CellStore\\1 = call 'erlang':'put'('$bt_actor_state', \\4) in "
        "let \\1 = case call '\\2':'__sealed_foo'(\\3, \\4)", 4),
    {Callers1, N2} = sub(Callers0,
        "let ([A-Za-z0-9_]+) = call 'erlang':'element'\\(2, (_SD[0-9]+)\\) in",
        "let \\1 = call 'erlang':'get'('$bt_actor_state') in", 4),
    {list_to_binary(Callers1), FooEdits + N1 + N2}.

%% The callee `foo`: `n := n + 1` on `State`. Reads and the write go through the
%% loaded copy, the load is the first statement, the store precedes the reply.
lower_callee(Foo0) ->
    {Foo1, A} = sub(Foo0, "call 'maps':'get'\\('n', State\\)", "call 'maps':'get'('n', _CellIn)", 1),
    {Foo2, B} = sub(Foo1, "call 'maps':'put'\\('n', (_Val[0-9]+), State\\)",
                    "call 'maps':'put'('n', \\1, _CellIn)", 1),
    {Foo3, C} = sub(Foo2, "let (_Val[0-9]+) = ",
                    "let _CellIn = call 'erlang':'get'('$bt_actor_state') in let \\1 = ", 1),
    {Foo4, D} = sub(Foo3, "\\{'reply', (_Val[0-9]+), State1\\}",
                    "let _CellOut = call 'erlang':'put'('$bt_actor_state', State1) in "
                    "{'reply', \\1, State1}", 1),
    {Foo4, A + B + C + D}.

%% Regex substitution that must match exactly `Expected` times.
sub(S, Re, Repl, Expected) ->
    Found = case re:run(S, Re, [global]) of
        {match, Ms} -> length(Ms);
        nomatch -> 0
    end,
    case Found of
        Expected -> ok;
        _ -> erlang:error({hand_lowering_edit_count, Re, expected, Expected, found, Found})
    end,
    {re:replace(S, Re, Repl, [global, {return, list}]), Found}.

split_at(S, Marker) ->
    split_on(S, Marker, []).

split_on([], Marker, _Acc) ->
    erlang:error({marker_not_found, Marker});
split_on(S, Marker, Acc) ->
    case lists:prefix(Marker, S) of
        true -> {lists:reverse(Acc), S};
        false ->
            [C | T] = S,
            split_on(T, Marker, [C | Acc])
    end.

loop(_M, _Sel, _Args, _Self, _S, 0) -> ok;
loop(M, Sel, Args, Self, S, N) ->
    _ = M:dispatch(Sel, Args, Self, S),
    loop(M, Sel, Args, Self, S, N - 1).

%% The isolated store+load pair (put+get of the actor state map in the process
%% dictionary), each side in a fresh process with a large heap so GC does not
%% decide the answer (ADR 0137 Cost table, row 1).
pair_bench(Batches) ->
    N = 5000000,
    Plain = fun() -> pair_plain(N, #{x => 1}) end,
    Pair = fun() -> pair_cell(N, #{x => 1}) end,
    InProc = fun(F) ->
        Me = self(),
        spawn_opt(fun() -> Me ! {done, timer:tc(F, nanosecond)} end,
                  [{min_heap_size, 2000000}]),
        receive {done, R} -> R end
    end,
    _ = InProc(Plain), _ = InProc(Pair),
    Samples = [{InProc(Plain), InProc(Pair)} || _ <- lists:seq(1, Batches)],
    As = lists:sort([A / N || {{A, _}, _} <- Samples]),
    Bs = lists:sort([B / N || {_, {B, _}} <- Samples]),
    Ds = lists:sort([B / N - A / N || {{A, _}, {B, _}} <- Samples]),
    io:format("~nstore+load pair: loop ~.2f ns, loop+pair ~.2f ns, delta median ~.2f ns, p90 ~.2f ns "
              "(~p batches of ~p)~n",
              [pct(As, 0.5), pct(Bs, 0.5), pct(Ds, 0.5), pct(Ds, 0.9), Batches, N]).

pair_plain(0, S) -> S;
pair_plain(N, S) -> pair_plain(N - 1, maps:put(x, N, S)).

pair_cell(0, S) -> S;
pair_cell(N, S) ->
    erlang:put('$bt_actor_state', S),
    S1 = erlang:get('$bt_actor_state'),
    pair_cell(N - 1, maps:put(x, N, S1)).
