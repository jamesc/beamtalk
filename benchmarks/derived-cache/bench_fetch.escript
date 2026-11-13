#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% BT-4652: micro-benchmark for `beamtalk_class_derived_cache:fetch/2` hits.
%%
%% Usage: benchmarks/derived-cache/bench_fetch.escript [Iterations]
%% (run `cd runtime && rebar3 compile` first; ebin dirs are found under
%% runtime/_build/default/lib).
%%
%% Compares, for a warmed key in a process holding 1..4096 other memo keys:
%%   ets       - pre-#5225: generation read + ets:lookup (the current module)
%%   map       - #5225 (7f38838): generation read + get({Gen, Map}) + maps:find
%%   per_key   - rejected #5227 (66d4363): get({Mod, Key}) -> {Gen, V}
%% Variants are loaded from git revisions under renamed modules. NOTE: on the
%% target hardware (M5) `ets` beat both memo layouts on row 3e of
%% `just bench-vs-elixir` (142 vs 223/257 ns), the opposite of what this
%% script shows on a Linux VM, so re-measure there before adding any memo.
main(Args) ->
    Iters = case Args of [I] -> list_to_integer(I); _ -> 1000000 end,
    Root = filename:dirname(filename:dirname(filename:dirname(escript:script_name()))),
    Root1 = case filelib:is_dir(filename:join(Root, "runtime")) of
        true -> Root;
        false -> element(2, file:get_cwd())
    end,
    [code:add_path(P) || P <- filelib:wildcard(filename:join(Root1, "runtime/_build/*/lib/*/ebin"))],
    Src = "runtime/apps/beamtalk_runtime/src/beamtalk_class_derived_cache.erl",
    Variants = [
        {ets, load(ets, file(Root1, Src))},
        {map, load(map, git_show(Root1, "7f38838", Src))},
        {per_key, load(per_key, git_show(Root1, "66d4363", Src))}
    ],
    io:format("iterations per cell: ~p; ns per fetch/3 hit (allocation cost subtracted) (best of 5)~n", [Iters]),
    io:format("~-10s ~10s ~10s ~10s ~10s~n", ["variant", "1 key", "50 keys", "500 keys", "4000 keys"]),
    [begin
         Cells = [bench(Mod, N, Iters) || N <- [1, 50, 500, 4000]],
         io:format("~-10s ~10.1f ~10.1f ~10.1f ~10.1f~n", [atom_to_list(Name) | Cells])
     end || {Name, Mod} <- Variants],
    ok.

git_show(Root, Rev, Path) ->
    Out = os:cmd("cd " ++ Root ++ " && git show " ++ Rev ++ ":" ++ Path),
    unicode:characters_to_binary(Out).

file(Root, Path) ->
    {ok, B} = file:read_file(filename:join(Root, Path)),
    B.

%% Compile the module source under a unique module + ETS table name.
load(Tag, Bin) ->
    Name = list_to_atom("bench_derived_cache_" ++ atom_to_list(Tag)),
    S0 = Bin,
    S1 = re:replace(S0, "-module\\(beamtalk_class_derived_cache\\)",
                    "-module(" ++ atom_to_list(Name) ++ ")", [{return, binary}]),
    S2 = re:replace(S1, "-define\\(TABLE, beamtalk_class_derived_cache\\)",
                    "-define(TABLE, " ++ atom_to_list(Name) ++ ")", [{return, binary}]),
    Tmp = filename:join(os:getenv("TMPDIR", "."), atom_to_list(Name) ++ ".erl"),
    ok = file:write_file(Tmp, S2),
    {ok, Name, Beam} = compile:file(Tmp, [binary, return_errors, nowarn_unused_function]),
    {module, Name} = code:load_binary(Name, Tmp, Beam),
    ok = Name:new(),
    Name.

bench(Mod, NKeys, Iters) ->
    Self = self(),
    spawn_link(fun() -> Self ! {done, run(Mod, NKeys, Iters)} end),
    receive {done, R} -> R end.

%% Keys have the shape of the hot dispatch keys ({class_method_owner, Pid,
%% Sel}); the hot loop allocates a few dozen words per iteration (like a real
%% send does) so minor GCs happen and a big live term in the process
%% dictionary has to be traced. The cost of the allocation alone is measured
%% in the same process and subtracted.
run(Mod, NKeys, Iters) ->
    Pid = self(),
    Hot = {class_method_owner, Pid, hot_selector},
    [Mod:fetch({class_method_owner, Pid, list_to_atom("sel" ++ integer_to_list(N))},
               fun() -> {cache, {owner, N}} end) || N <- lists:seq(1, NKeys)],
    Mod:fetch(Hot, fun() -> {cache, {owner, hot}} end),
    Compute = fun() -> error(miss) end,
    Valid = fun(_) -> true end,
    Base = lists:min([element(1, timer:tc(fun() -> loop_base(Iters) end)) || _ <- lists:seq(1, 5)]),
    Full = lists:min([element(1, timer:tc(fun() -> loop(Mod, Hot, Compute, Valid, Iters) end))
                      || _ <- lists:seq(1, 5)]),
    (Full - Base) * 1000 / Iters.

loop_base(0) -> ok;
loop_base(N) -> _ = garbage(N), loop_base(N - 1).

loop(_, _, _, _, 0) -> ok;
loop(Mod, K, C, V, N) ->
    _ = garbage(N),
    {owner, hot} = Mod:fetch(K, C, V),
    loop(Mod, K, C, V, N - 1).

garbage(N) -> {N, [N, N + 1, N + 2, N + 3], #{a => N, b => {N, N}}, <<N:64>>}.
