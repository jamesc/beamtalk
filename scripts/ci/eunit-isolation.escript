#!/usr/bin/env escript
%% -*- erlang -*-
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% EUnit test-isolation check (BT-4934).
%%
%% The default `rebar3 eunit` run uses one fixed module order in one node, so a
%% test module that relies on state another module left behind (a registered
%% class, a running standalone `beamtalk_announcements`, an app started by an
%% earlier module) passes there and fails alone or in another order. This
%% script runs the suites in the orders that expose those dependencies.
%%
%% Usage (from runtime/, after `just build-stdlib`; use `just test-isolation`):
%%
%%   eunit-isolation.escript alone   [--jobs N] [--only M1,M2]
%%   eunit-isolation.escript reverse [--only M1,M2]
%%   eunit-isolation.escript random  [--seed S] [--only M1,M2]
%%   eunit-isolation.escript forward [--only M1,M2]       (control: sorted order)
%%   eunit-isolation.escript culprit VICTIM [--jobs N]    (who pollutes VICTIM?)
%%
%% Any mode takes `--skip-prepare` to reuse the existing `_build/test` instead of
%% running rebar3 first (debugging only: it must be up to date).
%%
%% Modes:
%%   alone    every suite in its own fresh `erl` node, N nodes at a time.
%%   reverse  every suite, in one node per group, in reverse module order.
%%   random   as reverse, in a shuffled order; the seed is printed, and
%%            `--seed S` reproduces it.
%%   forward  as reverse, in sorted order (a control for the other two).
%%   culprit  VICTIM is a module that fails in reverse/random order but passes
%%            alone: runs `[X, VICTIM]` in a fresh node for every other module X
%%            and lists the X after which VICTIM fails, i.e. who leaves state behind.
%%
%% "Group" mirrors the normal run: runtime+workspace+compiler suites share a
%% node, stdlib suites run in a second one (`just test-runtime`).
%%
%% Each node gets the environment `just test-runtime` uses
%% (BEAMTALK_NO_FILE_LOG=1, BEAMTALK_NO_FOREGROUND_PRUNE=1, cover off), the
%% rebar3 `test` profile's code paths and the EUnit `sys.config`. rebar3 is run
%% once up front only to compile the test profile and the test fixtures (a
%% rebar3 invocation costs ~45s, far too much to pay per module).
%%
%% Prints a table (module, tests, failures, time) and exits non-zero when any
%% module fails. Failure detail follows the table.

-mode(compile).

-define(RUNTIME_APPS, [beamtalk_runtime, beamtalk_workspace, beamtalk_compiler]).
-define(STDLIB_APPS, [beamtalk_stdlib]).
-define(NODE_TIMEOUT_MS, 900000).

main(Args) ->
    {Mode, Opts} = parse_args(Args),
    ok = file:set_cwd(runtime_root()),
    Seed = seed(Mode, Opts),
    Only = maps:get(only, Opts, all),
    Groups = [{runtime, discover(?RUNTIME_APPS, Only)}, {stdlib, discover(?STDLIB_APPS, Only)}],
    Total = lists:sum([length(M) || {_, M} <- Groups]),
    Total > 0 orelse fail("no EUnit modules found (--only filter too narrow?)"),
    maps:get(skip_prepare, Opts, false) orelse prepare(),
    Paths = code_paths(),
    Start = erlang:monotonic_time(millisecond),
    Results =
        case Mode of
            culprit ->
                culprit(maps:get(victim, Opts), lists:append([M || {_, M} <- Groups]), Paths,
                    maps:get(jobs, Opts, default_jobs()));
            alone ->
                Jobs = maps:get(jobs, Opts, default_jobs()),
                io:format("Mode: alone, ~b modules, ~b parallel nodes~n", [Total, Jobs]),
                run_alone(lists:append([M || {_, M} <- Groups]), Paths, Jobs);
            _ ->
                io:format("Mode: ~s, ~b modules~n", [Mode, Total]),
                Mode =:= random andalso io:format("Seed: ~b~n", [Seed]),
                lists:append([
                    run_ordered(order(Mode, Mods, Seed), Paths, Group)
                 || {Group, Mods} <- Groups, Mods =/= []
                ])
        end,
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    report(Mode, Seed, Results, Elapsed).

%%% ── arguments ────────────────────────────────────────────────────────────

parse_args(["culprit", Victim | Rest]) ->
    {culprit, (parse_opts(Rest, #{}))#{victim => Victim}};
parse_args([ModeStr | Rest]) when
    ModeStr =:= "alone"; ModeStr =:= "reverse"; ModeStr =:= "random"; ModeStr =:= "forward"
->
    {list_to_atom(ModeStr), parse_opts(Rest, #{})};
parse_args(_) ->
    fail("usage: eunit-isolation.escript alone|reverse|random|forward|culprit VICTIM [--jobs N] [--skip-prepare] [--seed S] [--only M1,M2]").

parse_opts([], Acc) ->
    Acc;
parse_opts(["--skip-prepare" | Rest], Acc) ->
    parse_opts(Rest, Acc#{skip_prepare => true});
parse_opts(["--jobs", N | Rest], Acc) ->
    parse_opts(Rest, Acc#{jobs => list_to_integer(N)});
parse_opts(["--seed", S | Rest], Acc) ->
    parse_opts(Rest, Acc#{seed => list_to_integer(S)});
parse_opts(["--only", L | Rest], Acc) ->
    parse_opts(Rest, Acc#{only => string:tokens(L, ",")});
parse_opts([Other | _], _) ->
    fail("unknown argument: " ++ Other).

seed(random, #{seed := S}) -> S;
seed(random, _) -> erlang:phash2({os:system_time(), self()}, 1 bsl 31);
seed(_, _) -> undefined.

default_jobs() ->
    case os:getenv("EUNIT_ISOLATION_JOBS") of
        false -> max(1, min(4, erlang:system_info(logical_processors_available)));
        V -> list_to_integer(V)
    end.

%%% ── discovery ────────────────────────────────────────────────────────────

%% This script lives in <repo>/scripts/ci; the runtime umbrella is <repo>/runtime.
runtime_root() ->
    Script = filename:absname(escript:script_name()),
    filename:join([filename:dirname(filename:dirname(filename:dirname(Script))), "runtime"]).

%% A module is a suite when it defines a zero-arity `*_test` / `*_test_`
%% function (EUnit's own discovery rule, the same one runtime/rebar.config.script
%% uses to decide which test-dir modules run).
discover(Apps, Only) ->
    Mods = lists:usort([
        filename:basename(F, ".erl")
     || App <- Apps,
        Sub <- ["src", "test"],
        F <- filelib:wildcard(
            filename:join(["apps", atom_to_list(App), Sub, "**", "*.erl"])
        ),
        defines_tests(F)
    ]),
    case Only of
        all -> Mods;
        Names -> [M || M <- Mods, lists:member(M, Names)]
    end.

defines_tests(File) ->
    {ok, Src} = file:read_file(File),
    match =:=
        re:run(Src, "^[a-z][A-Za-z0-9_@]*_test_?\\(\\)\\s*->", [multiline, {capture, none}]).

%%% ── preparation ──────────────────────────────────────────────────────────

%% Compile the rebar3 `test` profile (src with -DTEST, test dirs, meck) and the
%% fixtures the eunit pre-hook builds. `--module` of an existing tiny suite is
%% the cheapest way to make rebar3 do exactly what a normal eunit run does.
prepare() ->
    io:format("Preparing test build (rebar3 eunit, one module)...~n"),
    {Status, Out} = cmd("rebar3", ["eunit", "--cover=false", "--module=beamtalk_wire_tests"], []),
    case Status of
        0 -> ok;
        _ -> fail("rebar3 eunit preparation failed:\n" ++ binary_to_list(Out))
    end.

code_paths() ->
    Libs = filelib:wildcard("_build/test/lib/*"),
    Ebins = [filename:absname(filename:join(L, "ebin")) || L <- Libs],
    Tests = [
        filename:absname(filename:join(L, "test"))
     || L <- Libs, filelib:is_dir(filename:join(L, "test"))
    ],
    Ebins ++ Tests.

%%% ── ordering ─────────────────────────────────────────────────────────────

order(forward, Mods, _) ->
    Mods;
order(reverse, Mods, _) ->
    lists:reverse(Mods);
order(random, Mods, Seed) ->
    rand:seed(exsss, {Seed, Seed, Seed}),
    [M || {_, M} <- lists:sort([{rand:uniform(), M} || M <- Mods])].

%%% ── running nodes ────────────────────────────────────────────────────────

node_env() ->
    [
        {"BEAMTALK_NO_FILE_LOG", "1"},
        {"BEAMTALK_NO_FOREGROUND_PRUNE", "1"}
    ].

%% One fresh node per module, at most Jobs at a time. Each module gets its own
%% report directory and its own working directory is the runtime root (as in a
%% rebar3 run).
run_alone(Mods, Paths, Jobs) ->
    Parent = self(),
    Pending = [{M, make_ref()} || M <- Mods],
    pool(Pending, Jobs, 0, [], Parent, Paths).

pool([], _Jobs, 0, Acc, _Parent, _Paths) ->
    Acc;
pool([{M, Ref} | Rest], Jobs, Running, Acc, Parent, Paths) when Running < Jobs ->
    spawn_link(fun() -> Parent ! {done, Ref, run_node([M], Paths, M)} end),
    pool(Rest, Jobs, Running + 1, Acc, Parent, Paths);
pool(Pending, Jobs, Running, Acc, Parent, Paths) ->
    receive
        {done, _Ref, Rs} ->
            progress(Rs),
            pool(Pending, Jobs, Running - 1, Rs ++ Acc, Parent, Paths)
    end.

%% Runs `[X, Victim]` in a fresh node for every other module X; returns one
%% result per X whose run made the victim fail (and is then reported as failing
%% under X's name, so the table lists the polluters). Prints each polluter.
culprit(Victim, Mods, Paths, Jobs) ->
    lists:member(Victim, Mods) orelse fail("unknown module: " ++ Victim),
    Others = [M || M <- Mods, M =/= Victim],
    io:format("Mode: culprit for ~s, ~b candidates, ~b parallel nodes~n", [Victim, length(Others), Jobs]),
    Parent = self(),
    Pending = [{X, make_ref()} || X <- Others],
    culprit_pool(Pending, Jobs, 0, [], Parent, Paths, Victim).

culprit_pool([], _Jobs, 0, Acc, _Parent, _Paths, _Victim) ->
    Acc;
culprit_pool([{X, Ref} | Rest], Jobs, Running, Acc, Parent, Paths, Victim) when Running < Jobs ->
    spawn_link(fun() ->
        Rs = run_node([X, Victim], Paths, "culprit_" ++ Victim ++ "_after_" ++ X),
        Parent ! {culprit_done, Ref, X, Rs}
    end),
    culprit_pool(Rest, Jobs, Running + 1, Acc, Parent, Paths, Victim);
culprit_pool(Pending, Jobs, Running, Acc, Parent, Paths, Victim) ->
    receive
        {culprit_done, _Ref, X, Rs} ->
            Acc1 =
                case [R || R = #{module := M, failures := F} <- Rs, M =:= Victim, F > 0] of
                    [] ->
                        Acc;
                    [R | _] ->
                        io:format("  ~s fails after ~s~n", [Victim, X]),
                        [R#{module => X ++ " (then " ++ Victim ++ ")"} | Acc]
                end,
            culprit_pool(Pending, Jobs, Running - 1, Acc1, Parent, Paths, Victim)
    end.

progress(Rs) ->
    [io:format("  ~-60s ~s~n", [M, status_word(R)]) || R = #{module := M} <- Rs],
    ok.

status_word(#{failures := 0, crashed := false}) -> "ok";
status_word(_) -> "FAILED".

run_ordered(Mods, Paths, Group) ->
    io:format("Group ~s: ~b modules in one node~n", [Group, length(Mods)]),
    Rs = run_node(Mods, Paths, atom_to_list(Group)),
    leak_report(report_dir(atom_to_list(Group)), Mods),
    [R || R <- Rs].

%% Informational: after each module of an ordered run, which actor instances
%% (by class) were alive that were not alive after the previous module. A module
%% that leaves actors running makes later "no actors" assertions (the tracing
%% scans) order-dependent; this names the module. Does not affect the exit code.
leak_report(Dir, Mods) ->
    leak_report(Dir, Mods, []).

leak_report(_Dir, [], _Prev) ->
    ok;
leak_report(Dir, [Mod | Rest], Prev) ->
    Live =
        case file:consult(filename:join(Dir, Mod ++ ".leaks")) of
            {ok, [L]} when is_list(L) -> L;
            _ -> Prev
        end,
    case Live -- Prev of
        [] ->
            ok;
        New ->
            Counts = [{C, length([X || X <- New, X =:= C])} || C <- lists:usort(New)],
            io:format("  LEAK after ~s: ~w~n", [Mod, Counts])
    end,
    leak_report(Dir, Rest, Live).

%% Runs Mods in order inside a single fresh node. The node loops over the
%% modules and writes one surefire XML report per module, so the table has
%% per-module counts in every mode. A node that dies part-way (a suite that
%% halts the VM, an OOM, the timeout) marks every module that never reported as
%% crashed.
run_node(Mods, Paths, Tag) ->
    Dir = report_dir(Tag),
    %% Start from an empty report directory: a `.status` or XML file left by an
    %% earlier run would make a node that died before reporting look green.
    _ = file:del_dir_r(Dir),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Eval = lists:flatten(
        io_lib:format(
            "Dir = ~p, Mods = ~p, "
            "lists:foreach(fun(M) -> "
            "  io:format(user, \"~~n===== ~~s =====~~n\", [M]), "
            "  T0 = erlang:monotonic_time(millisecond), "
            "  R = (catch eunit:test(M, [{report, {eunit_surefire, [{dir, Dir}]}}])), "
            "  T1 = erlang:monotonic_time(millisecond), "
            "  ok = file:write_file(filename:join(Dir, atom_to_list(M) ++ \".status\"), "
            "         io_lib:format(\"~~p.~~n~~p.~~n\", [R, T1 - T0])), "
            "  Live = (catch lists:sort([C || {P, C} <- ets:tab2list(beamtalk_instance_class), "
            "                                is_process_alive(P)])), "
            "  ok = file:write_file(filename:join(Dir, atom_to_list(M) ++ \".leaks\"), "
            "         io_lib:format(\"~~p.~~n\", [Live])) "
            "end, Mods), halt(0).",
            [Dir, [list_to_atom(M) || M <- Mods]]
        )
    ),
    Args = lists:append([["-pa", P] || P <- Paths]) ++
        [
            "-noshell",
            "-noinput",
            "-config",
            "apps/beamtalk_runtime/test/sys.config",
            "-eval",
            Eval
        ],
    T0 = erlang:monotonic_time(millisecond),
    {_Status, Out} = cmd("erl", Args, node_env()),
    Wall = erlang:monotonic_time(millisecond) - T0,
    [collect(M, Dir, Out, Wall, length(Mods)) || M <- Mods].

report_dir(Tag) ->
    filename:absname(filename:join(["_build", "isolation", Tag])).

collect(Mod, Dir, Out, Wall, NMods) ->
    StatusFile = filename:join(Dir, Mod ++ ".status"),
    Section = section_of(Mod, Out),
    case file:consult(StatusFile) of
        {ok, [R, Ms]} ->
            {Tests, Fails, Detail} = read_surefire(Dir, Mod),
            Ok = R =:= ok,
            %% eunit reports `error` for cancelled (setup/teardown timeout) as well as
            %% failed tests; surefire may count neither, so never show "0 failures"
            %% for a module whose eunit run did not return ok.
            #{
                module => Mod,
                tests => Tests,
                failures =>
                    case Ok of
                        true -> Fails;
                        false -> max(Fails, 1)
                    end,
                time_ms => Ms,
                crashed => false,
                detail =>
                    case Ok of
                        true when Fails =:= 0 -> "";
                        _ -> iolist_to_binary([Detail, "\n--- node output ---\n", Section])
                    end
            };
        _ ->
            #{
                module => Mod,
                tests => 0,
                failures => 1,
                time_ms => Wall div max(1, NMods),
                crashed => true,
                detail => iolist_to_binary([
                    "the node exited before this module reported (VM halt, crash or timeout)\n",
                    "--- node output (tail) ---\n",
                    tail(Out, 6000)
                ])
            }
    end.

%% Output between this module's "===== M =====" marker and the next one.
section_of(Mod, Out) ->
    Marker = iolist_to_binary(["===== ", Mod, " ====="]),
    case binary:split(Out, Marker) of
        [_, After] ->
            case binary:split(After, <<"\n===== ">>) of
                [Mine | _] -> tail(Mine, 8000);
                _ -> tail(After, 8000)
            end;
        _ ->
            tail(Out, 4000)
    end.

tail(Bin, N) when byte_size(Bin) > N -> binary:part(Bin, byte_size(Bin) - N, N);
tail(Bin, _) -> Bin.

%% Counts from the surefire XML eunit wrote for the module (and its `_tests`
%% companion, which eunit:test/2 on a module atom also runs).
read_surefire(Dir, Mod) ->
    Files = filelib:wildcard(filename:join(Dir, "TEST-" ++ Mod ++ "*.xml")) ++
        filelib:wildcard(filename:join(Dir, "TEST-" ++ Mod ++ "_tests.xml")),
    Xmls = [X || F <- lists:usort(Files), {ok, X} <- [file:read_file(F)]],
    Tests = lists:sum([attr(X, "tests") || X <- Xmls]),
    Fails = lists:sum([attr(X, "failures") + attr(X, "errors") || X <- Xmls]),
    Detail = [failure_text(X) || X <- Xmls],
    {Tests, Fails, Detail}.

attr(Xml, Name) ->
    case re:run(Xml, "<testsuite[^>]*\\s" ++ Name ++ "=\"(\\d+)\"", [{capture, [1], list}]) of
        {match, [V]} -> list_to_integer(V);
        nomatch -> 0
    end.

failure_text(Xml) ->
    case
        re:run(
            Xml,
            "<testcase[^>]*name=\"([^\"]*)\"[^>]*>\\s*<(failure|error)[^>]*>(.*?)</(failure|error)>",
            [global, dotall, {capture, [1, 3], binary}]
        )
    of
        {match, Ms} ->
            [["FAILED ", N, ":\n", unescape(Body), "\n"] || [N, Body] <- Ms];
        nomatch ->
            []
    end.

unescape(B) ->
    lists:foldl(
        fun({From, To}, Acc) -> binary:replace(Acc, From, To, [global]) end,
        B,
        [{<<"&lt;">>, <<"<">>}, {<<"&gt;">>, <<">">>}, {<<"&quot;">>, <<"\"">>}, {<<"&amp;">>, <<"&">>}]
    ).

%%% ── report ───────────────────────────────────────────────────────────────

report(Mode, Seed, Results, ElapsedMs) ->
    Sorted = lists:sort(fun(#{module := A}, #{module := B}) -> A =< B end, Results),
    io:format("~n~-60s ~8s ~9s ~9s~n", ["module", "tests", "failures", "time"]),
    io:format("~s~n", [lists:duplicate(89, $-)]),
    lists:foreach(
        fun(#{module := M, tests := T, failures := F, time_ms := Ms, crashed := C}) ->
            Mark =
                case {F, C} of
                    {0, false} -> "";
                    {_, true} -> "  CRASHED";
                    _ -> "  FAILED"
                end,
            io:format("~-60s ~8b ~9b ~8.1fs~s~n", [M, T, F, Ms / 1000, Mark])
        end,
        Sorted
    ),
    Failed = [R || R = #{failures := F} <- Sorted, F > 0],
    TotalTests = lists:sum([T || #{tests := T} <- Sorted]),
    io:format("~s~n", [lists:duplicate(89, $-)]),
    io:format(
        "~b modules, ~b tests, ~b modules failed, wall time ~.1fs~n",
        [length(Sorted), TotalTests, length(Failed), ElapsedMs / 1000]
    ),
    case Failed of
        [] ->
            io:format("OK: every module passes (~s)~n", [Mode]),
            halt(0);
        _ ->
            io:format("~n===== failure detail =====~n"),
            [io:format("~n--- ~s ---~n~s~n", [M, D]) || #{module := M, detail := D} <- Failed],
            io:format(
                "~nFAILED (~s~s): ~s~n",
                [
                    Mode,
                    case Seed of
                        undefined -> "";
                        S -> io_lib:format(", seed ~b", [S])
                    end,
                    string:join([M || #{module := M} <- Failed], " ")
                ]
            ),
            halt(1)
    end.

%%% ── helpers ──────────────────────────────────────────────────────────────

%% Runs an executable with env, returns {ExitStatus, Output} (stderr merged),
%% killing it after ?NODE_TIMEOUT_MS.
cmd(Exe, Args, Env) ->
    Path = os:find_executable(Exe),
    Path =/= false orelse fail("executable not found: " ++ Exe),
    Port = open_port(
        {spawn_executable, Path},
        [exit_status, stderr_to_stdout, binary, {args, Args}, {env, Env}, {cd, "."}]
    ),
    collect_port(Port, [], erlang:monotonic_time(millisecond) + ?NODE_TIMEOUT_MS).

collect_port(Port, Acc, Deadline) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, D}} ->
            collect_port(Port, [D | Acc], Deadline);
        {Port, {exit_status, S}} ->
            {S, iolist_to_binary(lists:reverse(Acc))}
    after Left ->
        {os_pid, Pid} = erlang:port_info(Port, os_pid),
        os:cmd("kill -9 " ++ integer_to_list(Pid)),
        catch port_close(Port),
        {124, iolist_to_binary(lists:reverse([<<"\n(timed out)\n">> | Acc]))}
    end.

fail(Msg) ->
    io:format(standard_error, "eunit-isolation: ~s~n", [Msg]),
    halt(2).
