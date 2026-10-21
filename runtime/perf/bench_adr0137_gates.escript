#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% ADR 0137 Phase 0 (BT-4007): in-VM, interleaved micro-benchmarks of the real
%% generated Core Erlang of `runtime/perf/self_send_bench`, compiled with the
%% `BEAMTALK_ADR0137_PROTO` flag off and on, plus one hand-lowered variant of
%% the self-send case. The modules are loaded under distinct names in one VM
%% and called as `Module:dispatch(Selector, Args, Self, State)`, so the
%% gen_server round trip (about 6 us, and the dominant noise source) is out of
%% the measurement. Medians and p90 over `Batches` interleaved batches.
%%
%% Setup (two copies of the bench package, one built per side):
%%
%%   cp -R runtime/perf/self_send_bench /path/off
%%   cp -R runtime/perf/self_send_bench /path/on
%%   (cd /path/off && beamtalk build)
%%   (cd /path/on  && BEAMTALK_ADR0137_PROTO=1 beamtalk build)
%%   ERL_FLAGS="+S 1" escript runtime/perf/bench_adr0137_gates.escript /path 31 100000
%%
%% where `/path` holds the two copies as `bench_off/` and `bench_on/`.
%%
%% Cases:
%%   field_body        gate 1: ten field reads and three writes, intrinsics only
%%   field_sends       gate 2: ten bracketed sends that each update a field;
%%                     (B - A) is the cost of one bracket (the table is per send)
%%   open_self_*       gate 3 as the flagged prototype compiles it (self-sends in
%%                     a loop body are not bracketed, so A and B are the same code)
%%   open_self_* hand  gate 3 as ADR 0137 would lower it: a store before the
%%                     self-send, a load after it, and the callee's load on entry
%%                     and store on exit (two store+load pairs per self-send)
main([Dir, BatchesS, RepsS]) ->
    Batches = list_to_integer(BatchesS),
    Reps = list_to_integer(RepsS),
    ok = load_dispatch_stub(),
    Field = "ssb_field_probe_actor",
    Open = "ssb_open_self_probe_actor",
    Mods = [
        {field_off, load(Dir, core(Dir, "off", Field), Field, "bt4007_field_off")},
        {field_on, load(Dir, core(Dir, "on", Field), Field, "bt4007_field_on")},
        {open_off, load(Dir, core(Dir, "off", Open), Open, "bt4007_open_off")},
        {open_on, load(Dir, core(Dir, "on", Open), Open, "bt4007_open_on")},
        {open_hand, load(Dir, hand_lower(core(Dir, "off", Open)), Open, "bt4007_open_hand")}
    ],
    Mod = fun(K) -> proplists:get_value(K, Mods) end,
    St = fun(M) -> #{a => 1, b => 2, c => 3, n => 0, '__class_mod__' => M} end,
    Self = {beamtalk_object, 'Probe', undefined, self()},
    Cases = [
        {"field_body (10r/3w intrinsics)", field_off, field_on, 'body', [], 1},
        {"field_sends (per bracketed send)", field_off, field_on, 'sends', [], 10},
        {"open_self_top (per self-send), off vs on", open_off, open_on, 'top:', [1000], 1000},
        {"open_self_arm (per self-send), off vs on", open_off, open_on, 'arm:', [1000], 1000},
        {"open_self_top, off vs hand-lowered", open_off, open_hand, 'top:', [1000], 1000},
        {"open_self_arm, off vs hand-lowered", open_off, open_hand, 'arm:', [1000], 1000}
    ],
    io:format("batches=~p reps=~p schedulers=~p (ns per op: per call, per bracketed send, per self-send)~n",
              [Batches, Reps, erlang:system_info(schedulers_online)]),
    io:format("~-44s ~9s ~9s ~9s ~9s ~7s ~9s~n",
              ["case", "A med", "A p90", "B med", "B p90", "B/A", "B-A med"]),
    lists:foreach(fun({Name, A, B, Sel, Args, Per}) ->
        {AM, BM} = {Mod(A), Mod(B)},
        Run = fun(M) ->
            S0 = St(M),
            erlang:put('$bt_actor_state', S0),
            {T, _} = timer:tc(fun() -> loop(M, Sel, Args, Self, S0, Reps) end, nanosecond),
            T / (Reps * Per)
        end,
        _ = Run(AM), _ = Run(BM),
        Samples = [{Run(AM), Run(BM)} || _ <- lists:seq(1, Batches)],
        As = lists:sort([X || {X, _} <- Samples]),
        Bs = lists:sort([Y || {_, Y} <- Samples]),
        AMed = pct(As, 0.5), BMed = pct(Bs, 0.5),
        io:format("~-44s ~9.2f ~9.2f ~9.2f ~9.2f ~7.3f ~9.2f~n",
                  [Name, AMed, pct(As, 0.9), BMed, pct(Bs, 0.9), BMed / AMed, BMed - AMed])
    end, Cases),
    pair_bench(Batches).

pct(Sorted, Q) -> lists:nth(max(1, round(length(Sorted) * Q)), Sorted).

core(Dir, Side, Class) ->
    Path = filename:join([Dir, "bench_" ++ Side, "_build", "dev", "ebin",
                          "bt@self_send_bench@" ++ Class ++ ".core"]),
    {ok, Bin} = file:read_file(Path),
    Bin.

%% `beamtalk_message_dispatch:send/3` for `max:`/`min:` on integers, the only
%% runtime call the field probe makes.
load_dispatch_stub() ->
    Src = "-module(beamtalk_message_dispatch).\n"
          "-export([send/3]).\n"
          "send(R, 'max:', [X]) -> erlang:max(R, X);\n"
          "send(R, 'min:', [X]) -> erlang:min(R, X).\n",
    {ok, Tokens, _} = erl_scan:string(Src),
    Forms = split_forms(Tokens),
    {ok, M, Bin} = compile:forms(Forms, [binary]),
    {module, M} = code:load_binary(M, "stub", Bin),
    ok.

split_forms(Tokens) ->
    {Forms, []} = lists:foldl(fun
        ({dot, _} = D, {Acc, Cur}) ->
            {ok, F} = erl_parse:parse_form(lists:reverse([D | Cur])),
            {[F | Acc], []};
        (T, {Acc, Cur}) ->
            {Acc, [T | Cur]}
    end, {[], []}, Tokens),
    lists:reverse(Forms).

%% Loads a generated module under a distinct name, without its `on_load` hook
%% (which registers the class with the runtime).
load(_Dir, CoreBin, Class, NewName) ->
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

%% ADR 0137 lowering of the open-actor self-send, by hand: the caller stores
%% its `StateAcc` before the dispatch and loads the next one from the cell after
%% it; the callee `foo` loads `State` from the cell on entry and stores its last
%% version on exit.
hand_lower(Core) ->
    S0 = binary_to_list(Core),
    {Pre, FooOn} = split_at(S0, "<'foo'> when 'true' ->"),
    {Foo0, Rest} = split_at(FooOn, "<'top:'> when 'true' ->"),
    Foo1 = replace_all(Foo0, "'n', State)", "'n', _CellIn)"),
    Foo2 = replace_all(Foo1, "_Val4, State)", "_Val4, _CellIn)"),
    Foo3 = replace_all(Foo2, "let _Val4 = ",
                       "let _CellIn = call 'erlang':'get'('$bt_actor_state') in let _Val4 = "),
    Foo4 = replace_all(Foo3, "{'reply', _Val4, State1}",
                       "let _CellOut = call 'erlang':'put'('$bt_actor_state', State1) in "
                       "{'reply', _Val4, State1}"),
    Callers0 = re:replace(Rest, "let (_SD[0-9]+) = case let _SDArgs",
                          "let _CellStore\\1 = call 'erlang':'put'('$bt_actor_state', StateAcc) in "
                          "let \\1 = case let _SDArgs", [global, {return, list}]),
    Callers1 = re:replace(Callers0,
                          "let (StateAcc[0-9]+) = call 'erlang':'element'\\(2, (_SD[0-9]+)\\) in",
                          "let \\1 = call 'erlang':'get'('$bt_actor_state') in",
                          [global, {return, list}]),
    list_to_binary(Pre ++ Foo4 ++ Callers1).

split_at(S, Marker) ->
    {Before, After} = split_on(S, Marker, []),
    {Before, After}.

split_on(S, Marker, Acc) ->
    case lists:prefix(Marker, S) of
        true -> {lists:reverse(Acc), S};
        false ->
            [C | T] = S,
            split_on(T, Marker, [C | Acc])
    end.

replace_all(S, From, To) ->
    re:replace(S, re:replace(From, "[.^$*+?()\\[\\]{}|\\\\]", "\\\\&", [global, {return, list}]),
               re:replace(To, "\\\\", "\\\\\\\\", [global, {return, list}]),
               [global, {return, list}]).

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
