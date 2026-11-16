%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Per-call dispatch overhead micro-benchmark (BT-4983).
%%
%% Rows are class methods of the sealed, stateless `VdBench`
%% (../beamtalk/src/vd_bench.bt), called as `'class_<selector>'(VdBench, N, ...)`
%% in a fresh worker process. Two methods, as the issue asks:
%%
%%   count  `count/1`: call-count tracing of every function of every loaded
%%          module over N operations (all processes, so an actor's
%%          gen_server side counts), reported per operation. Deterministic.
%%   time   `time/1`: median of interleaved rounds of ns/op and reductions/op
%%          of the worker, net of the `baseline` row (the bare loop). Noisy;
%%          indicative only on a shared container.
%%
%% `cost/1` joins them: it multiplies the per-op count of each mechanism by a
%% measured isolated cost of that mechanism (the mechanism's own function in a
%% tight loop), so the share of a row's time per mechanism can be read off.

-module(vd_bench).

-export([count/1, time/1, cost/1, report/1, keys/1, mechanisms/0, rows/0, boot/0]).

-define(BT, 'bt@dispatch_overhead@vd_bench').

%%====================================================================
%% Rows: {Id, Group, Label, N, Selector, ArgSpec}
%%====================================================================

rows() ->
    [
        {"0", loop, "loop overhead (subtracted)", 100000, 'baseline:', []},
        {"p1", plain, "typed sealed Value getter: p x", 100000, 'plainSealed:p:', [point]},
        {"p2", plain, "typed open Value getter: p x", 100000, 'plainOpen:p:', [open_point]},
        {"p3", plain, "untyped receiver getter: o x", 100000, 'plainUntyped:o:', [point]},
        {"p4", plain, "typed Tuple at:", 100000, 'tupleAt:t:', [tuple]},
        {"p5", plain, "untyped Tuple at:", 100000, 'tupleAtUntyped:t:', [tuple]},
        {"p6", plain, "untyped Dictionary at:", 100000, 'dictAtUntyped:d:', [dict]},
        {"p7", plain, "actor sync call: c count", 20000, 'actorCall:c:', [counter]},
        {"s1", self_send, "sealed Value: self x + self x", 100000, 'selfTwiceSealed:p:', [point]},
        {"s2", self_send, "sealed Value: self helper", 100000, 'selfHelperSealed:p:', [point]},
        {"s3", self_send, "open Value: self helper", 100000, 'selfHelperOpen:p:', [open_point]},
        {"s4", self_send, "actor method: self count", 20000, 'selfActor:c:', [counter]},
        {"c1", class_side, "`class sealed` on a sealed class", 100000, 'classSealedStatic:', []},
        {"c2", class_side, "class method on a sealed class", 100000, 'classSealedPlain:', []},
        {"c3", class_side, "class method on an open class", 100000, 'classOpen:', []},
        {"c4", class_side, "class method on a class with classState:", 20000, 'classStateful:', []},
        {"c5", class_side, "class method, class in a variable", 100000, 'classInVariable:c:', [open_math]},
        {"f1", ffi, "FFI, integer result: erlang:phash2", 100000, 'ffiInt:a:', [int]},
        {"f2", ffi, "FFI, binary result: integer_to_binary", 100000, 'ffiBinary:a:', [int]},
        {"f3", ffi, "FFI, binary argument: iolist_size", 100000, 'ffiIolistSize:s:', [string]},
        {"f4", ffi, "FFI, untyped argument: erlang:phash2", 100000, 'ffiIntUntyped:a:', [int]},
        {"f5", ffi, "FFI, untyped binary argument: iolist_size", 100000, 'ffiSizeUntyped:a:', [string]},
        {"f6", ffi, "FFI, two untyped args: lists:member/2", 100000, 'ffiTwoArgUntyped:a:', [int]},
        {"e1", exceptions, "on: Error do: without a raise", 100000, 'onDoNoRaise:', []},
        {"e2", exceptions, "ensure: without a raise", 100000, 'ensureNoRaise:', []},
        {"r1", nlr, "`^` in an inlined ifTrue:", 100000, 'retInlinedIf:', []},
        {"r2", nlr, "`^` in a do: block, taken", 50000, 'retInDo:l:', [list10]},
        {"r3", nlr, "`^` in an inlined to:do:, taken", 100000, 'retInToDo:', []},
        {"r4", nlr, "`^` in a do: block, never taken", 50000, 'noRetInDo:l:', [list10]}
    ].

fixture(point) -> ?BT:class_point(vd());
fixture(open_point) -> ?BT:class_openPoint(vd());
fixture(counter) -> ?BT:class_counter(vd());
fixture(tuple) -> ?BT:class_tuple(vd());
fixture(dict) -> ?BT:class_dict(vd());
fixture(list10) -> ?BT:class_list10(vd());
fixture(open_math) -> ?BT:class_openMath(vd());
fixture(string) -> <<"hello world">>;
fixture(int) -> 42.

%% The class reference a stateless class method takes as `ClassSelf`.
vd() -> beamtalk_class_registry:resolve_class_object('VdBench').

fun_for(Sel, Args) ->
    Fs = [fixture(A) || A <- Args],
    Self = vd(),
    fun(N) -> erlang:apply(?BT, class_fun(Sel), [Self, N | Fs]) end.

class_fun(Sel) -> list_to_atom("class_" ++ atom_to_list(Sel)).

boot() ->
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    #{level := Level} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, error),
    [load_all(Dir, "bt@stdlib@*.beam") || Dir <- code:get_path()],
    ok = logger:set_primary_config(level, Level),
    BtEbin = os:getenv("VD_BENCH_BT_EBIN"),
    load_all(filename:dirname(BtEbin), filename:basename(BtEbin) ++ "/bt@*.beam"),
    timer:sleep(500).

load_all(Dir, Pattern) ->
    [
        begin
            M = list_to_atom(filename:basename(F, ".beam")),
            catch code:ensure_loaded(M)
        end
     || F <- filelib:wildcard(filename:join(Dir, Pattern))
    ],
    ok.

selected(Ids) ->
    case Ids of
        [] -> rows();
        _ -> [R || {Id, _, _, _, _, _} = R <- rows(), lists:member(Id, ["0" | Ids])]
    end.

%%====================================================================
%% Counting
%%====================================================================

%% Mechanisms: {Name, [MFA that counts as one occurrence]}. A row's count is
%% the sum of the calls to these functions per operation.
mechanisms() ->
    [
        {persistent_term_get, [{persistent_term, get, 1}, {persistent_term, get, 2}]},
        {function_exported, [{erlang, function_exported, 3}]},
        {module_loaded, [{erlang, module_loaded, 1}]},
        {ets_lookup, [{ets, lookup, 2}, {ets, lookup_element, 3}]},
        {make_ref, [{erlang, make_ref, 0}]},
        {class_vars_snapshot, [{beamtalk_class_vars, snapshot, 0}]},
        {class_derived_cache, [{beamtalk_class_derived_cache, fetch, 3}]},
        {tuple_dispatch, [{'bt@stdlib@tuple', dispatch, 3}, {'bt@stdlib@tuple', dispatch, 4}]},
        {primitive_send, [{beamtalk_primitive, send, 3}]},
        {message_dispatch, [{beamtalk_message_dispatch, send, 3}]},
        {ffi_apply, [{beamtalk_erlang_proxy, apply_with_coercion, 6}]},
        {deep_unwrap_strings, [{beamtalk_erlang_proxy, deep_unwrap_strings, 1}]},
        {atomics_get, [{atomics, get, 2}]},
        {monotonic_time, [{erlang, monotonic_time, 1}]},
        {pdict_put, [{erlang, put, 2}]},
        {pdict_erase, [{erlang, erase, 1}]},
        {throw, [{erlang, throw, 1}]}
    ].

count(Ids) ->
    boot(),
    Rows = selected(Ids),
    io:format("# Dispatch counts per operation (call_count tracing, all processes)~n~n"),
    Results = [{R, call_counts(R)} || R <- Rows],
    Names = [Name || {Name, _} <- mechanisms()],
    io:format("| row | operation | calls/op | ~s |~n", [string:join([atom_to_list(N) || N <- Names], " | ")]),
    io:format("|---|---|--:|~s~n", [lists:duplicate(length(Names), "--:|")]),
    [print_row(R, C, Names) || {R, C} <- Results],
    io:format("~n"),
    [print_top(R, C) || {R, C} <- Results],
    ok.

print_row({Id, _, Label, N, _, _}, Counts, Names) ->
    Per = [fmt(mech_count(Name, Counts) / N) || Name <- Names],
    Total = lists:sum([C || {C, _} <- Counts]) / N,
    io:format("| ~s | ~s | ~s | ~s |~n", [Id, Label, fmt(Total), string:join(Per, " | ")]).

print_top({Id, _, Label, N, _, _}, Counts) ->
    io:format("### ~s ~s~n~n```~n", [Id, Label]),
    [
        io:format("~8s  ~p:~p/~p~n", [fmt(C / N), M, F, A])
     || {C, {M, F, A}} <- lists:sublist(Counts, 40), C / N >= 0.5
    ],
    io:format("```~n~n").

mech_count(Name, Counts) ->
    {Name, MFAs} = lists:keyfind(Name, 1, mechanisms()),
    lists:sum([C || {C, MFA} <- Counts, lists:member(MFA, MFAs)]).

fmt(X) when abs(X) < 0.05 -> "-";
fmt(X) -> lists:flatten(io_lib:format("~.1f", [X])).

call_counts({_, _, _, N0, Sel, Args}) ->
    N = min(N0, 2000),
    Fun = fun_for(Sel, Args),
    _ = Fun(N),
    Session = trace:session_create(vd_bench_count, self(), []),
    Mods = [M || {M, _} <- code:all_loaded(), M =/= ?MODULE, not traceless(M)],
    MFAs = [{M, F, A} || M <- Mods, {F, A} <- erlang:get_module_info(M, functions)],
    [trace:function(Session, {M, '_', '_'}, true, [local, call_count]) || M <- Mods],
    _ = Fun(N),
    Counts = [
        {C * (N0 / N), MFA}
     || MFA <- MFAs,
        {call_count, C} <- [trace:info(Session, MFA, call_count)],
        is_integer(C),
        C > 0
    ],
    trace:session_destroy(Session),
    lists:reverse(lists:sort(Counts)).

traceless(M) -> lists:member(M, [erts_internal, prim_eval, trace, erl_tracer, erts_trace_cleaner]).

%%====================================================================
%% What each persistent_term read fetches
%%====================================================================

%% For each row: the arguments of every `persistent_term:get` and the callers
%% (the function that made the read), over 10 operations in the worker.
keys(Ids) ->
    boot(),
    [
        begin
            Fun = fun_for(Sel, Args),
            _ = Fun(10),
            Self = self(),
            Pid = spawn_link(fun() ->
                receive go -> ok end,
                _ = Fun(10),
                Self ! done
            end),
            Session = trace:session_create(vd_bench_keys, self(), []),
            trace:process(Session, Pid, true, [call]),
            trace:function(Session, {persistent_term, get, '_'}, [{'_', [], [{message, {caller}}]}], [local]),
            Pid ! go,
            receive done -> ok end,
            trace:session_destroy(Session),
            Msgs = drain([]),
            Grouped = lists:foldl(
                fun({Key, Caller}, Acc) -> maps:update_with({Key, Caller}, fun(C) -> C + 1 end, 1, Acc) end,
                #{},
                Msgs
            ),
            io:format("### ~s ~s~n~n```~n", [Id, Label]),
            [
                io:format("~5.1f  get(~0p) from ~0p~n", [C / 10, K, Caller])
             || {{K, Caller}, C} <- lists:reverse(lists:keysort(2, maps:to_list(Grouped)))
            ],
            io:format("```~n~n")
        end
     || {Id, _, Label, _, Sel, Args} <- selected(Ids), Id =/= "0"
    ],
    ok.

drain(Acc) ->
    receive
        {trace, _, call, {persistent_term, get, A}, Caller} -> drain([{A, Caller} | Acc]);
        {trace, _, call, _, _} -> drain(Acc)
    after 100 -> Acc
    end.

%%====================================================================
%% Timing
%%====================================================================

time(Args) ->
    Rounds = case Args of [R | _] -> list_to_integer(R); _ -> 7 end,
    Ids = case Args of [_ | Rest] -> Rest; _ -> [] end,
    boot(),
    Rows = selected(Ids),
    io:format("# load average before: ~s~n", [string:trim(os:cmd("uptime"))]),
    Samples = rounds(Rows, Rounds),
    {_, _, _, _, _, _} = lists:keyfind("0", 1, Rows),
    Base = median_of("0", Samples),
    io:format("~n| row | operation | ns/op net | reductions/op net |~n|---|---|--:|--:|~n"),
    [
        begin
            {Ns, Red} = median_of(Id, Samples),
            {BNs, BRed} = Base,
            io:format("| ~s | ~s | ~s | ~s |~n", [Id, Label, fmt(Ns - BNs), fmt(Red - BRed)])
        end
     || {Id, _, Label, _, _, _} <- Rows, Id =/= "0"
    ],
    io:format("~n(baseline loop: ~s ns/op, ~s reductions/op; indicative only)~n", [
        fmt(element(1, Base)), fmt(element(2, Base))
    ]),
    ok.

rounds(Rows, Rounds) ->
    lists:append([
        [{Id, measure(R)} || {Id, _, _, _, _, _} = R <- order(Rows, I)]
     || I <- lists:seq(1, Rounds)
    ]).

order(Rows, I) when I rem 2 =:= 0 -> lists:reverse(Rows);
order(Rows, _) -> Rows.

median_of(Id, Samples) ->
    Xs = [S || {I, S} <- Samples, I =:= Id],
    {median([N || {N, _} <- Xs]), median([R || {_, R} <- Xs])}.

median(L) ->
    S = lists:sort(L),
    lists:nth((length(S) + 1) div 2, S).

%% One fresh worker per measurement: warm up, then time N operations.
measure({_, _, _, N, Sel, Args}) ->
    Fun = fun_for(Sel, Args),
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        _ = Fun(min(N, 2000)),
        garbage_collect(),
        {reductions, R0} = process_info(self(), reductions),
        T0 = erlang:monotonic_time(nanosecond),
        _ = Fun(N),
        T1 = erlang:monotonic_time(nanosecond),
        {reductions, R1} = process_info(self(), reductions),
        Parent ! {self(), {(T1 - T0) / N, (R1 - R0) / N}}
    end),
    receive
        {Pid, Res} ->
            receive {'DOWN', Ref, _, _, _} -> ok end,
            Res;
        {'DOWN', Ref, _, _, Why} ->
            error({worker_died, Why})
    end.

%%====================================================================
%% Isolated cost per mechanism
%%====================================================================

%% Cost of one occurrence of each mechanism in a tight loop, ns, net of an
%% empty loop. `ffi_apply` and `tuple_dispatch` are costed as whole calls
%% (rows f1 and p4 minus what they contain are the better estimate: see the
%% report), so only the leaf BIF mechanisms are measured here.
cost(_) ->
    boot(),
    {Empty, Costs} = costs(),
    io:format("| mechanism | ns/occurrence (net of empty loop ~s ns) |~n|---|--:|~n", [fmt(Empty)]),
    [io:format("| ~s | ~s |~n", [Name, fmt(C)]) || {Name, C} <- Costs],
    ok.

%% Isolated cost of one occurrence of each mechanism, keyed by mechanism name
%% from `mechanisms/0`. `throw` is a throw plus its catch.
costs() ->
    Key = {?MODULE, cost_probe},
    persistent_term:put(Key, #{a => 1}),
    Atomics = atomics:new(1, []),
    Table = ets:new(cost, [set, public]),
    ets:insert(Table, {k, v}),
    Loop = fun(F) -> bench(F, 2000000) end,
    Empty = Loop(fun() -> ok end),
    Net = fun(F) -> max(0.0, Loop(F) - Empty) end,
    {Empty, [
        {persistent_term_get, Net(fun() -> persistent_term:get(Key) end)},
        {function_exported, Net(fun() -> erlang:function_exported(lists, map, 2) end)},
        {module_loaded, Net(fun() -> erlang:module_loaded(lists) end)},
        {ets_lookup, Net(fun() -> ets:lookup(Table, k) end)},
        {make_ref, Net(fun() -> make_ref() end)},
        {class_vars_snapshot, Net(fun() -> erlang:get(vd_cost_key) end)},
        {atomics_get, Net(fun() -> atomics:get(Atomics, 1) end)},
        {monotonic_time, Net(fun() -> erlang:monotonic_time(nanosecond) end)},
        {pdict_put, Net(fun() -> erlang:put(vd_cost_key, 1) end)},
        {pdict_erase, Net(fun() -> erlang:put(vd_cost_key, 1), erlang:erase(vd_cost_key) end)},
        {throw, Net(fun() -> catch throw(x) end)}
    ]}.

%% Counts x isolated costs x measured time: for each row, ns/op, and the share
%% of it that each leaf mechanism accounts for (occurrences x isolated cost).
%% The remainder is the Erlang-level glue around the mechanisms (function
%% calls, pattern matches, map reads): see the per-function lists of `count/1`.
report(Args) ->
    Rounds = case Args of [R | _] -> list_to_integer(R); _ -> 7 end,
    boot(),
    {_, Costs} = costs(),
    Rows = rows(),
    Samples = rounds(Rows, Rounds),
    {BNs, _} = median_of("0", Samples),
    io:format("| row | operation | ns/op | ~s | other |~n", [string:join([atom_to_list(N) || {N, _} <- Costs], " | ")]),
    io:format("|---|---|--:|~s--:|~n", [lists:duplicate(length(Costs), "--:|")]),
    [
        begin
            {Ns0, _} = median_of(Id, Samples),
            Ns = Ns0 - BNs,
            Counts = call_counts(R),
            Parts = [{Name, mech_count(Name, Counts) / N * C} || {Name, C} <- Costs],
            Other = Ns - lists:sum([P || {_, P} <- Parts]),
            io:format("| ~s | ~s | ~s | ~s | ~s |~n", [
                Id, Label, fmt(Ns), string:join([fmt(P) || {_, P} <- Parts], " | "), fmt(Other)
            ])
        end
     || {Id, _, Label, N, _, _} = R <- Rows, Id =/= "0"
    ],
    ok.

bench(F, N) ->
    Run = fun() ->
        F(),
        ok
    end,
    [Run() || _ <- lists:seq(1, 10000)],
    Loop = fun L(0) -> ok; L(I) -> F(), L(I - 1) end,
    T0 = erlang:monotonic_time(nanosecond),
    Loop(N),
    T1 = erlang:monotonic_time(nanosecond),
    (T1 - T0) / N.
