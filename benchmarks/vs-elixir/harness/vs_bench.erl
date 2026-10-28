%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Beamtalk vs Elixir microbenchmarks: one Erlang harness, one BEAM node.
%%
%% The Beamtalk rows are class methods of the sealed, stateless `VbBench`
%% (../beamtalk/src/vb_bench.bt), called directly as
%% `'class_<selector>'(nil, N, Ctx)`, so they run in the calling process.
%% The Elixir rows are `VeBench.<row>(N, Ctx)` (../elixir/lib/ve_bench.ex).
%% Both sides run the operation N times in the same loop shape.
%%
%% Each measurement runs in a fresh worker process. Around the timed call the
%% controller takes a snapshot of every process in the node and traces every
%% process's garbage collections, so the counts include work done in other
%% processes on the operation's behalf (an actor, a GenServer, a class
%% process), not just the caller:
%%
%%   reductions/op  sum over processes of the `reductions` delta, / N
%%   words/op       words allocated on process heaps (young heap and heap
%%                  fragments), / N: for each process, the growth in
%%                  heap_size + mbuf_size between consecutive GCs, summed
%%                  over the GCs inside the window
%%   ns/op          wall time of the timed call in the worker, / N
%%
%% Every figure in the report is net of the same side's `baseline` row (the
%% loop with a constant body). Words count only process heaps: off-heap
%% (refc) binaries and ETS are not included.
%%
%% Entry points: main/1 (bench) and profile/1 (per-row call counts).

-module(vs_bench).

-export([main/1, profile/1, rows/0, ffi_ok/0]).

%% Fixture callee for row 13d: an FFI target returning the bare atom `ok`,
%% which the proxy coerces to `Result ok: nil`. No pure OTP function returns
%% bare `ok` without side effects, so the suite provides its own.
ffi_ok() -> ok.

-define(BT, 'bt@vs_elixir_bench@vb_bench').
-define(EX, 'Elixir.VeBench').

%%====================================================================
%% Row table
%%====================================================================

%% {Id, Group, Label, N, BeamtalkSelector, Ctx, [{ElixirFun, Ctx}]}
%% Ctx names a fixture (see fixtures/0); the first Elixir entry is the
%% comparison the ratio is taken against, the rest are reported alongside.
rows() ->
    [
        {"0", loop, "loop overhead (subtracted)", 200000, 'baseline:', none, [{baseline, none}]},
        {"0b", loop, "timesRepeat: whose block assigns nothing", 200000, 'loopNoAssign:', none, [
            {baseline, none}
        ]},
        {"1a", dispatch, "getter on a sealed Value: p x / p.x", 200000, 'getterSealed:p:', point, [
            {getter_dot, point}, {getter_fun, point}, {getter_match, point}
        ]},
        {"1b", dispatch, "getter on an open Value: p x / p.x", 200000, 'getterOpen:p:', open_point, [
            {getter_dot, point}
        ]},
        {"1c", dispatch, "method on a sealed Value: p sum / Point.sum(p)", 200000, 'sumSealed:p:',
            point, [{point_sum, point}]},
        {"2a", alloc, "functional update: p withX: 3 / %{p | x: 3}", 200000, 'withX:p:', point, [
            {with_x, point}
        ]},
        {"2b", alloc, "construct: VbPoint x: a y: 4 / %Point{x: a, y: 4}", 50000, 'construct:a:',
            int, [{construct, int}]},
        {"3a", dispatch, "`class sealed` method, sealed class / Mod.fun(arg)", 200000,
            'classSealedMethod:', none, [{remote_call, none}]},
        {"3b", dispatch, "class method, sealed class / Mod.fun(arg)", 50000, 'sealedClassMethod:',
            none, [{remote_call, none}]},
        {"3c", dispatch, "class method, open class / Mod.fun(arg)", 50000, 'openClassMethod:', none, [
            {remote_call, none}
        ]},
        {"4a", dispatch, "untyped receiver: o x / o.x", 200000, 'dynGetter:o:', point, [
            {getter_dot, point}
        ]},
        {"4b", dispatch, "untyped receiver: o area / protocol", 200000, 'dynArea:o:', square, [
            {protocol, square}, {apply3, mod_square}, {dyn_module, mod_square}
        ]},
        {"4c", dispatch, "typed open Value: s area / Square.area(s)", 200000, 'typedArea:s:', square,
            [{square_area, square}, {protocol, square}]},
        {"5a", dispatch, "inherited method: s base / defdelegate", 200000, 'inherited:s:', sub, [
            {delegate, sub}
        ]},
        {"5b", dispatch, "self-send to inherited: s viaSelf / local call", 200000,
            'inheritedViaSelf:s:', sub, [{delegate_via_self, sub}]},
        {"5c", machinery, "runtime-resolved method: s hasField: #v / Map.has_key?", 20000,
            'hasField:s:', sub, [{has_key, sub}]},
        {"6a", alloc, "block value: x / fun.(x)", 200000, 'blockValue:blk:', adder, [
            {fun_call, adder}
        ]},
        {"6b", alloc, "do: summing 100 elements / Enum.reduce", 5000, 'doSum:list:', list100, [
            {enum_reduce_sum, list100}, {lists_foldl, list100}
        ]},
        {"6c", alloc, "do: over 100, no assignment / Enum.each", 5000, 'doNoAssign:list:', list100, [
            {enum_each, list100}
        ]},
        {"6d", alloc, "collect: 100 elements / Enum.map", 5000, 'collect:list:', list100, [
            {enum_map, list100}
        ]},
        {"6e", alloc, "inject:into: 100 elements / Enum.reduce", 5000, 'inject:list:', list100, [
            {enum_reduce_sum, list100}, {lists_foldl, list100}
        ]},
        {"7a", machinery, "actor sync read: c count / GenServer.call", 20000, 'actorCall:c:',
            bt_counter, [{gen_call, ex_counter}]},
        {"7b", machinery, "actor state update: c increment / GenServer.call", 20000,
            'actorUpdate:c:', bt_counter, [{gen_update, ex_counter}]},
        {"7c", machinery, "actor cast: c increment! / GenServer.cast", 20000, 'actorCast:c:',
            bt_counter, [{gen_cast, ex_counter}]},
        {"7d", machinery, "actor method doing self hasField: / GenServer.call", 20000,
            'actorProbe:c:', bt_counter, [{gen_probe, ex_counter}]},
        {"8a", machinery, "on: Error do: without a raise / try-rescue", 100000, 'onDoNoRaise:',
            none, [{try_no_raise, none}]},
        {"8b", machinery, "on: Error do: with a raise / raise-rescue", 10000, 'onDoRaise:', none, [
            {try_raise, none}
        ]},
        {"9a", alloc, "(Result ok: x) value / {:ok, x} match", 100000, 'resultOk:', none, [
            {ok_match, none}
        ]},
        {"9b", dispatch, "r isOk / match?({:ok, _}, r)", 200000, 'resultIsOk:r:', bt_ok, [
            {ok_is_ok, ex_ok}
        ]},
        {"10a", dispatch, "Dictionary at:put: / Map.put", 200000, 'dictPut:d:', dict, [
            {map_put, dict}
        ]},
        {"10b", dispatch, "Dictionary at: / Map.get", 200000, 'dictAt:d:', dict, [{map_get, dict}]},
        {"10c", dispatch, "List add: (100 elements) / l ++ [x]", 20000, 'listAdd:list:', list100, [
            {list_append, list100}
        ]},
        {"10d", dispatch, "List includes: (100 elements) / Enum.member?", 20000,
            'listIncludes:list:', list100, [{list_member, list100}]},
        {"10e", alloc, "String ++ / <>", 100000, 'stringConcat:s:', string, [{binary_concat, string}]},
        {"11a", alloc, "1 to: 100 do: summing / recursive sum", 5000, 'sumToDo:', none, [
            {sum_recursive, none}, {sum_reduce_range, none}
        ]},
        {"11b", dispatch, "a + 1, a :: Integer / a + 1", 200000, 'add:a:', int, [{add, int}]},
        {"11c", dispatch, "a + 1, a untyped / a + 1", 200000, 'addUntyped:a:', int, [{add, int}]},
        {"11d", dispatch, "a > 0 ifTrue: [1] ifFalse: [2] / if", 200000, 'ifTrueIfFalse:a:', int, [
            {if_else, int}
        ]},
        {"12a", dispatch, "p class / p.__struct__", 200000, 'classOf:p:', point, [
            {struct_of, point}
        ]},
        {"12b", machinery, "p isKindOf: VbPoint / is_struct(p, Point)", 20000, 'isKindOf:p:', point,
            [{is_struct_check, point}]},
        {"13a", ffi, "FFI call, integer result / apply", 100000, 'ffiInt:a:', int, [
            {apply_phash2, int}, {direct_phash2, int}
        ]},
        {"13b", ffi, "FFI call, binary result, caller-typed / apply", 100000, 'ffiBinary:a:', int, [
            {apply_integer_to_binary, int}, {direct_integer_to_binary, int}
        ]},
        {"13c", ffi, "FFI call, {ok, V} result / apply", 100000, 'ffiOkTuple:d:', dict, [
            {apply_maps_find, dict}, {direct_maps_find, dict}
        ]},
        {"13d", ffi, "FFI call, ok-atom result / apply", 100000, 'ffiOk:', none, [
            {apply_ffi_ok, none}
        ]}
    ].

%%====================================================================
%% Fixtures
%%====================================================================

fixtures() ->
    Bt = fun(Sel) -> ?BT:(class_fn(Sel))(nil) end,
    #{
        none => 0,
        int => 5,
        point => {Bt(point), ?EX:point()},
        open_point => {Bt(openPoint), ?EX:point()},
        square => {Bt(square), ?EX:square()},
        mod_square => {Bt(square), {'Elixir.VeBench.Square', ?EX:square()}},
        sub => {Bt(sub), ?EX:sub()},
        adder => {Bt(adder), ?EX:adder()},
        list100 => {Bt(list100), ?EX:list100()},
        dict => {Bt(dict), ?EX:dict()},
        string => {Bt(string), ?EX:string()},
        bt_ok => {Bt(okResult), none},
        ex_ok => {none, ?EX:ok_result()},
        bt_counter => {Bt(counter), none},
        ex_counter => {none, ?EX:counter()}
    }.

class_fn(Sel) -> list_to_atom("class_" ++ atom_to_list(Sel)).

ctx(bt, Name, Fx) ->
    case maps:get(Name, Fx) of
        {B, _} -> B;
        V -> V
    end;
ctx(ex, Name, Fx) ->
    case maps:get(Name, Fx) of
        {_, E} -> E;
        V -> V
    end.

%% The callable for one side of one row: fun(N) -> term().
bt_fun(Sel, Ctx) ->
    F = class_fn(Sel),
    case arity_of(Sel) of
        1 -> fun(N) -> ?BT:F(nil, N) end;
        2 -> fun(N) -> ?BT:F(nil, N, Ctx) end
    end.

ex_fun(Fun, Ctx) -> fun(N) -> ?EX:Fun(N, Ctx) end.

arity_of(Sel) -> length([C || C <- atom_to_list(Sel), C =:= $:]).

%%====================================================================
%% Bench
%%====================================================================

%% Args: [Rounds] or [Rounds, Filter] (Filter: comma-separated row ids).
main(Args) ->
    Rounds =
        case Args of
            [R | _] -> list_to_integer(R);
            [] -> 7
        end,
    Filter =
        case Args of
            [_, F | _] -> string:split(F, ",", all);
            _ -> all
        end,
    boot(),
    Fx = fixtures(),
    Rows = [Row || Row = {Id, _, _, _, _, _, _} <- rows(), Filter =:= all orelse Id =:= "0" orelse
        lists:member(Id, Filter)],
    print_env(Rounds),
    Results = run_rounds(Rows, Fx, Rounds),
    io:format("# load average after: ~s~n", [string:trim(os:cmd("uptime"))]),
    report(Rows, Results),
    ok.

boot() ->
    {ok, _} = application:ensure_all_started(elixir),
    {ok, _} = application:ensure_all_started(beamtalk_runtime),
    %% The stdlib classes and the bench package register themselves in their
    %% on_load hooks: load them all up front so no row pays for it.
    %% (Quietly: the on_load failures load_all/2 reports also log a warning.)
    #{level := Level} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, error),
    [load_all(Dir, "bt@stdlib@*.beam") || Dir <- code:get_path()],
    ok = logger:set_primary_config(level, Level),
    BtEbin = os:getenv("VS_BENCH_BT_EBIN"),
    load_all(filename:dirname(BtEbin), filename:basename(BtEbin) ++ "/bt@*.beam"),
    %% Elixir protocol dispatch is only representative when consolidated.
    true = 'Elixir.Protocol':'consolidated?'('Elixir.VeBench.Area'),
    true = 'Elixir.Protocol':'consolidated?'('Elixir.Enumerable'),
    timer:sleep(1000).

load_all(Dir, Glob) ->
    lists:foreach(
        fun(B) ->
            case code:ensure_loaded(list_to_atom(filename:basename(B, ".beam"))) of
                {module, _} ->
                    ok;
                %% A few stdlib facades (e.g. Beamtalk) only register inside a
                %% workspace; no row uses them.
                {error, Why} ->
                    io:format(standard_error, "not loaded: ~s (~p)~n", [B, Why])
            end
        end,
        filelib:wildcard(filename:join(Dir, Glob))
    ).

print_env(Rounds) ->
    {ok, ExVsn} = application:get_key(elixir, vsn),
    io:format("# vs-elixir bench~n", []),
    io:format("# otp ~s, erts ~s, elixir ~s, schedulers ~p~n", [
        erlang:system_info(otp_release),
        erlang:system_info(version),
        ExVsn,
        erlang:system_info(schedulers_online)
    ]),
    io:format("# load average: ~s~n", [string:trim(os:cmd("uptime"))]),
    io:format("# rounds ~p~n", [Rounds]).

run_rounds(Rows, Fx, Rounds) ->
    lists:foldl(
        fun(Round, Acc) ->
            io:format(standard_error, "round ~p/~p~n", [Round, Rounds]),
            lists:foldl(
                fun({{Id, _, _, N, Sel, Ctx, Exs}, I}, Acc1) ->
                    BtM = {bt, Sel, fun() -> measure(bt_fun(Sel, ctx(bt, Ctx, Fx)), N) end},
                    ExMs = [
                        {ex, F, fun() -> measure(ex_fun(F, ctx(ex, C, Fx)), N) end}
                     || {F, C} <- Exs
                    ],
                    %% Alternate which side goes first, by round and by row.
                    Order =
                        case (Round + I) rem 2 of
                            0 -> [BtM | ExMs];
                            1 -> ExMs ++ [BtM]
                        end,
                    lists:foldl(
                        fun({Side, Name, M}, Acc2) ->
                            Key = {Id, Side, Name},
                            maps:update_with(Key, fun(L) -> [M() | L] end, [M()], Acc2)
                        end,
                        Acc1,
                        Order
                    )
                end,
                Acc,
                lists:zip(Rows, lists:seq(1, length(Rows)))
            )
        end,
        #{},
        lists:seq(1, Rounds)
    ).

%% One measurement: {NsPerOp, RedsPerOp, WordsPerOp, CallerRedsPerOp,
%% CallerWordsPerOp}.
measure(Fun, N) ->
    Controller = self(),
    Worker = spawn(fun() ->
        _ = Fun(min(N, 2000)),
        erlang:garbage_collect(),
        Controller ! {ready, self()},
        receive
            go -> ok
        end,
        T0 = erlang:monotonic_time(nanosecond),
        _ = Fun(N),
        T1 = erlang:monotonic_time(nanosecond),
        Controller ! {done, self(), T1 - T0},
        receive
            stop -> ok
        end
    end),
    receive
        {ready, Worker} -> ok
    end,
    Tracer = spawn(fun() -> gc_tracer(#{}) end),
    %% erts_trace_cleaner sweeps the process table after each
    %% session_destroy/1: not part of any operation.
    Excluded = [self(), Tracer | [
        P
     || P <- processes(),
        process_info(P, initial_call) =:= {initial_call, {erts_trace_cleaner, start, 0}}
    ]],
    Session = trace:session_create(vs_bench, Tracer, []),
    Before = snapshot(Excluded),
    _ = trace:process(Session, all, true, [garbage_collection]),
    Worker ! go,
    Ns =
        receive
            {done, Worker, T} -> T
        end,
    _ = trace:process(Session, all, false, [garbage_collection]),
    After = snapshot(Excluded),
    Ref = erlang:trace_delivered(all),
    receive
        {trace_delivered, all, Ref} -> ok
    end,
    Tracer ! {report, self()},
    Gcs =
        receive
            {gcs, G} -> G
        end,
    trace:session_destroy(Session),
    Worker ! stop,
    {Reds, Words} = account(Before, After, Gcs),
    debug_others(Worker, Before, After),
    {CallerReds, CallerWords} = account(
        maps:with([Worker], Before), maps:with([Worker], After), maps:with([Worker], Gcs)
    ),
    {Ns / N, Reds / N, Words / N, CallerReds / N, CallerWords / N}.

debug_others(Worker, Before, After) ->
    case os:getenv("VS_BENCH_DEBUG") of
        false ->
            ok;
        _ ->
            [
                io:format(standard_error, "  other ~p ~p reds ~p~n", [
                    P, catch process_info(P, [registered_name, current_function, initial_call]), R1 - R0
                ])
             || {P, {R1, _}} <- maps:to_list(After),
                P =/= Worker,
                {R0, _} <- [maps:get(P, Before, {0, 0})],
                R1 - R0 > 100
            ],
            ok
    end.

snapshot(Excluded) ->
    maps:from_list([
        {P, {Reds, used(Gc)}}
     || P <- processes() -- Excluded,
        [{reductions, Reds}, {garbage_collection_info, Gc}] <-
            [process_info(P, [reductions, garbage_collection_info])]
    ]).

used(GcInfo) ->
    proplists:get_value(heap_size, GcInfo) + proplists:get_value(mbuf_size, GcInfo).

%% Collects, per process, the GC start/end events in order (reversed).
gc_tracer(Acc) ->
    receive
        {trace, Pid, Ev, Info} when
            Ev =:= gc_minor_start; Ev =:= gc_major_start; Ev =:= gc_minor_end; Ev =:= gc_major_end
        ->
            Kind =
                case Ev of
                    gc_minor_start -> start;
                    gc_major_start -> start;
                    _ -> 'end'
                end,
            gc_tracer(maps:update_with(Pid, fun(L) -> [{Kind, used(Info)} | L] end, [{Kind, used(Info)}], Acc));
        {trace, _, _, _} ->
            gc_tracer(Acc);
        {report, From} ->
            From ! {gcs, maps:map(fun(_, L) -> lists:reverse(L) end, Acc)}
    end.

%% Reductions and words over all processes seen in either snapshot.
account(Before, After, Gcs) ->
    Pids = lists:usort(maps:keys(Before) ++ maps:keys(After)),
    lists:foldl(
        fun(P, {R, W}) ->
            {R0, U0} = maps:get(P, Before, {0, 0}),
            {R1, U1} = maps:get(P, After, {R0, none}),
            Events = maps:get(P, Gcs, []),
            {R + (R1 - R0), W + alloc(U0, Events, U1)}
        end,
        {0, 0},
        Pids
    ).

%% Allocation = growth of used words between GCs, summed.
alloc(U0, Events, Final) ->
    {Sum, Last} = lists:foldl(
        fun
            ({start, U}, {S, Prev}) -> {S + max(0, U - Prev), Prev};
            ({'end', U}, {S, _}) -> {S, U}
        end,
        {0, U0},
        Events
    ),
    case Final of
        %% The process exited inside the window: only its GCs are counted.
        none -> Sum;
        _ -> Sum + max(0, Final - Last)
    end.

%% What the Beamtalk side of each row compiles to (from the kept Core Erlang,
%% ../beamtalk/_build/dev/ebin/*.core) and, where it matters, where the
%% runtime takes it (from `run.sh profile`).
compiles_to("0") -> "letrec loop, last threaded as a loop argument";
compiles_to("0b") -> "send(N, 'timesRepeat:', [fun]); Integer>>timesRepeat: dispatches ifTrue:ifFalse: per step";
compiles_to("1a") -> "tag-guarded direct VbPoint:dispatch/3 (BT-4069); mismatch falls back to send/3";
compiles_to("1b") -> "send(P, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (open receiver stays dynamic)";
compiles_to("1c") -> "send(P, sum, []), same path as 1a";
compiles_to("2a") -> "send(P, 'withX:', [3]), same path as 1a + maps:put";
compiles_to("2b") -> "whereis_class + class_send: gen_server:call into the VbPoint class process";
compiles_to("3a") -> "direct call Mod:'class_double:'(nil, 21)";
compiles_to("3b") -> "whereis_class + class_send: not `class sealed`, so gen_server:call into the class process";
compiles_to("3c") -> "whereis_class + class_send: gen_server:call into the class process";
compiles_to("4a") -> "send(O, x, []) -> classify, tagged-map class lookup (ETS + cache), Mod:dispatch/3 (untyped receiver stays dynamic)";
compiles_to("4b") -> "send(O, area, []), same path as 4a";
compiles_to("4c") -> "send(S, area, []), same path as 4a (open class stays dynamic)";
compiles_to("5a") -> "send(S, base, []): VbSub:dispatch/3 misses, extensions lookup, VbBase:dispatch/3";
compiles_to("5b") -> "send(S, viaSelf, []), then `self base` is a second full send";
compiles_to("5c") -> "send(S, 'hasField:', [v]) -> beamtalk_reflection:has_field/2";
compiles_to("6a") -> "is_function(Blk, 1) guard, then apply Blk(41)";
compiles_to("6b") -> "lists:foldl over a fun that maps:get/maps:put `sum` in a state map; + with coercion try";
compiles_to("6c") -> "is_list guard, lists:foreach(fun)";
compiles_to("6d") -> "is_list guard, lists:map(fun); * with is_number guard";
compiles_to("6e") -> "is_list guard, lists:foldl(fun); + with coercion try";
compiles_to("7a") -> "send(C, count, []) -> beamtalk_actor:sync_send, gen_server:call + telemetry/trace/ctx hooks both sides";
compiles_to("7b") -> "as 7a";
compiles_to("7c") -> "beamtalk_message_dispatch:cast -> gen_server:cast + the same hooks";
compiles_to("7d") -> "as 7a; in the actor `self hasField:` misses the module and walks the runtime method resolver";
compiles_to("8a") -> "`Error` class ref built per entry (whereis_class + module_name gen_server:call), class_vars snapshot, try";
compiles_to("8b") -> "as 8a + `Error signal:` raised in the caller (class_signal_message/2, BT-4115), wrapped and class-matched; frames built only on read";
compiles_to("9a") -> "class_send(Result, 'ok:') gen_server:call; `value` as a block-or-send case";
compiles_to("9b") -> "tag-guarded direct Result:dispatch/3 (BT-4069); mismatch falls back to send/3";
compiles_to("10a") -> "send(D, 'at:put:', ...) -> Dictionary:dispatch/3 -> maps:put";
compiles_to("10b") -> "send(D, 'at:', ...) -> Dictionary:dispatch/3 -> maps:get";
compiles_to("10c") -> "send(L, 'add:', [7]) -> List primitive (++)";
compiles_to("10d") -> "send(L, 'includes:', [50]) -> List primitive (lists:member)";
compiles_to("10e") -> "is_list guard, iolist_to_binary([binary_to_list(S), binary_to_list(\"!\")])";
compiles_to("11a") -> "letrec loop; + with is_number guard and coercion try";
compiles_to("11b") -> "erlang:'+'(A, 1)";
compiles_to("11c") -> "is_number guard, erlang:'+'";
compiles_to("11d") -> "send(Bool, 'ifTrue:ifFalse:', [fun, fun]) -> True:dispatch/3";
compiles_to("12a") -> "beamtalk_primitive:class_of_object: derived-cache hit (counter + ETS read + pid liveness)";
compiles_to("12b") -> "class ref built per call (gen_server:call) + send(P, 'isKindOf:') -> includesBehaviour: via class processes";
compiles_to("13a") -> "direct_call/4 ('erlang', 'phash2', [A], 'stock'): mode precomputed (BT-4153), integer fast path";
compiles_to("13b") -> "direct_call/4 ('erlang', 'integer_to_binary', [A], 'stock'): binary fast path";
compiles_to("13c") -> "direct_call/4 ('maps', 'find', [K, D], 'stock'): mode precomputed, {ok, V} still wraps to Result";
compiles_to("13d") -> "direct_call/4 ('vs_bench', 'ffi_ok', [], 'stock'): mode precomputed, ok still wraps to Result";
compiles_to(_) -> "".

%%====================================================================
%% Report
%%====================================================================

median(L) ->
    S = lists:sort(L),
    lists:nth((length(S) + 1) div 2, S).

stat(Results, Key, Field) ->
    median([element(Field, M) || M <- maps:get(Key, Results)]).

report(Rows, Results) ->
    Base = fun(Side, Name, Field) -> stat(Results, {"0", Side, Name}, Field) end,
    Net = fun(Id, Side, Name, Field) ->
        stat(Results, {Id, Side, Name}, Field) -
            case Side of
                bt -> Base(bt, 'baseline:', Field);
                ex -> Base(ex, baseline, Field)
            end
    end,
    io:format("~n## Results (median of rounds, net of each side's loop baseline)~n~n", []),
    io:format(
        "| Row | Operation | BT ns | BT red | BT words | EX ns | EX red | EX words "
        "| ns x | red x | words x | +red | +words | Beamtalk compiles to |~n",
        []
    ),
    io:format("|---|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|---|~n", []),
    lists:foreach(
        fun({Id, _G, Label, _N, Sel, _C, [{Ex, _} | _]}) ->
            [BNs, BRed, BW] = [Net(Id, bt, Sel, F) || F <- [1, 2, 3]],
            [ENs, ERed, EW] = [Net(Id, ex, Ex, F) || F <- [1, 2, 3]],
            io:format("| ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s |~n", [
                Id,
                escape(Label),
                num(BNs),
                num(BRed),
                num(BW),
                num(ENs),
                num(ERed),
                num(EW),
                ratio(BNs, ENs),
                ratio(BRed, ERed),
                ratio(BW, EW),
                num(BRed - ERed),
                num(BW - EW),
                escape(compiles_to(Id))
            ])
        end,
        Rows
    ),
    io:format("~n## Elixir alternates (net ns / red / words)~n~n", []),
    io:format("| Row | Elixir | ns | red | words |~n|---|---|--:|--:|--:|~n", []),
    lists:foreach(
        fun({Id, _G, _Label, _N, _Sel, _C, Exs}) ->
            [
                io:format("| ~s | `~s` | ~s | ~s | ~s |~n", [
                    Id, A, num(Net(Id, ex, A, 1)), num(Net(Id, ex, A, 2)), num(Net(Id, ex, A, 3))
                ])
             || {A, _} <- Exs
            ]
        end,
        Rows
    ),
    io:format("~n## Raw (not net): baseline and caller-only share~n~n", []),
    io:format("| Row | Side | Fun | ns | red | words | caller red | caller words |~n", []),
    io:format("|---|---|---|--:|--:|--:|--:|--:|~n", []),
    lists:foreach(
        fun({Id, Side, Name} = Key) ->
            io:format("| ~s | ~s | ~s | ~s | ~s | ~s | ~s | ~s |~n", [
                Id,
                Side,
                Name
                | [num(stat(Results, Key, F)) || F <- [1, 2, 3, 4, 5]]
            ])
        end,
        lists:sort(
            fun({A, S1, N1}, {B, S2, N2}) -> {id_key(A), S1, N1} =< {id_key(B), S2, N2} end,
            maps:keys(Results)
        )
    ).

escape(S) -> string:replace(S, "|", "\\|", all).

id_key(Id) ->
    {I, Rest} = string:to_integer(Id),
    {I, Rest}.

num(X) when abs(X) >= 100 -> io_lib:format("~w", [round(X)]);
num(X) when abs(X) >= 10 -> io_lib:format("~.1f", [float(X)]);
num(X) -> io_lib:format("~.2f", [float(X)]).

%% Ratios against a near-zero Elixir figure are not meaningful.
ratio(_B, E) when abs(E) < 0.5 -> "-";
ratio(B, E) -> io_lib:format("~.1f", [B / E]).

%%====================================================================
%% Profile: which functions each row calls, per operation
%%====================================================================

%% Args: [] or [Filter]. For each Beamtalk row (and its first Elixir pair),
%% count calls to every function in every module during N = 2000 operations,
%% in every process, and print the functions called at least 0.5 times per
%% operation.
profile(Args) ->
    Filter =
        case Args of
            [F | _] -> string:split(F, ",", all);
            _ -> all
        end,
    boot(),
    Fx = fixtures(),
    N = 2000,
    lists:foreach(
        fun({Id, _, Label, _, Sel, Ctx, [{Ex, ExCtx} | _]}) ->
            case Filter =:= all orelse lists:member(Id, Filter) of
                false ->
                    ok;
                true ->
                    io:format("~n### ~s ~s~n~n", [Id, Label]),
                    io:format("Beamtalk `~s`:~n~n```~n", [Sel]),
                    print_counts(call_counts(bt_fun(Sel, ctx(bt, Ctx, Fx)), N), N),
                    io:format("```~n~nElixir `~s`:~n~n```~n", [Ex]),
                    print_counts(call_counts(ex_fun(Ex, ctx(ex, ExCtx, Fx)), N), N),
                    io:format("```~n", [])
            end
        end,
        rows()
    ).

call_counts(Fun, N) ->
    _ = Fun(N),
    Session = trace:session_create(vs_bench_profile, self(), []),
    Mods = [M || {M, _} <- code:all_loaded(), M =/= ?MODULE, not is_traceless(M)],
    MFAs = [
        {M, F, A}
     || M <- Mods,
        {F, A} <- erlang:get_module_info(M, functions)
    ],
    [trace:function(Session, {M, '_', '_'}, true, [local, call_count]) || M <- Mods],
    _ = Fun(N),
    Counts = [
        {C, MFA}
     || MFA <- MFAs,
        {call_count, C} <- [trace:info(Session, MFA, call_count)],
        is_integer(C),
        C > 0
    ],
    trace:session_destroy(Session),
    lists:reverse(lists:sort(Counts)).

%% Modules whose functions cannot (or should not) be call-count traced.
is_traceless(M) ->
    lists:member(M, [erts_internal, prim_eval, trace, erl_tracer, erts_trace_cleaner]).

print_counts(Counts, N) ->
    Shown = [{C / N, MFA} || {C, MFA} <- Counts, C / N >= 0.5],
    io:format("~.1f calls/op to ~p distinct functions~n", [
        lists:sum([C || {C, _} <- Counts]) / N, length(Shown)
    ]),
    lists:foreach(
        fun({PerOp, {M, F, A}}) -> io:format("~8.2f  ~s:~s/~p~n", [PerOp, M, F, A]) end,
        Shown
    ),
    case Shown of
        [] -> io:format("(no function called >= 0.5 times per op)~n", []);
        _ -> ok
    end.
