#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Evaluate auto-extract spec coverage against OTP modules (BT-1845).
%%
%% Runs beamtalk_spec_reader against all OTP .beam files and reports:
%% - Total modules and functions found
%% - Functions with useful types vs all-Dynamic
%% - Keyword name quality from spec variable names
%% - Per-module breakdown for the 20 most-used modules

-mode(compile).

main(_Args) ->
    %% Add the compiler beam path so we can call beamtalk_spec_reader
    CompilerEbin = filename:absname("runtime/_build/default/lib/beamtalk_compiler/ebin"),
    code:add_pathz(CompilerEbin),

    %% Discover all OTP .beam files
    OtpLibDir = code:lib_dir(),
    BeamFiles = discover_beam_files(OtpLibDir),
    io:format("=== OTP Spec Coverage Evaluation (BT-1845) ===~n~n"),
    io:format("OTP lib dir: ~s~n", [OtpLibDir]),
    io:format("Total .beam files discovered: ~B~n~n", [length(BeamFiles)]),

    %% Process all modules
    Results = process_modules(BeamFiles),

    %% Overall stats
    print_overall_stats(Results),

    %% Top-20 most-used modules detailed report
    Top20 = [<<"erlang">>, <<"lists">>, <<"maps">>, <<"string">>,
             <<"binary">>, <<"io">>, <<"file">>, <<"ets">>,
             <<"gen_server">>, <<"gen_statem">>, <<"supervisor">>,
             <<"timer">>, <<"proplists">>, <<"filename">>,
             <<"io_lib">>, <<"crypto">>, <<"logger">>,
             <<"application">>, <<"code">>, <<"os">>],
    print_top20_report(Top20, Results),

    %% Keyword quality analysis
    print_keyword_quality(Results),

    %% Summary recommendation
    print_recommendation(Results, Top20),

    ok.

discover_beam_files(LibDir) ->
    Apps = filelib:wildcard(filename:join(LibDir, "*")),
    lists:flatmap(
        fun(AppDir) ->
            EbinDir = filename:join(AppDir, "ebin"),
            case filelib:is_dir(EbinDir) of
                true ->
                    filelib:wildcard(filename:join(EbinDir, "*.beam"));
                false ->
                    []
            end
        end,
        Apps
    ).

process_modules(BeamFiles) ->
    lists:foldl(
        fun(BeamFile, Acc) ->
            ModName = list_to_binary(filename:basename(BeamFile, ".beam")),
            case beamtalk_spec_reader:read_specs(BeamFile) of
                {ok, Specs} ->
                    maps:put(ModName, {ok, Specs}, Acc);
                {error, Reason} ->
                    maps:put(ModName, {error, Reason}, Acc)
            end
        end,
        #{},
        BeamFiles
    ).

print_overall_stats(Results) ->
    io:format("--- Overall Statistics ---~n~n"),
    TotalModules = maps:size(Results),

    {OkCount, ErrCount, TotalFns, UsefulFns, AllDynamicFns,
     NamedParams, PositionalParams} =
        maps:fold(
            fun(_Mod, {ok, Specs}, {OkC, ErrC, TF, UF, ADF, NP, PP}) ->
                FnCount = length(Specs),
                {Useful, AllDyn} = classify_functions(Specs),
                {Named, Positional} = classify_params(Specs),
                {OkC + 1, ErrC, TF + FnCount, UF + Useful, ADF + AllDyn,
                 NP + Named, PP + Positional};
               (_Mod, {error, _}, {OkC, ErrC, TF, UF, ADF, NP, PP}) ->
                {OkC, ErrC + 1, TF, UF, ADF, NP, PP}
            end,
            {0, 0, 0, 0, 0, 0, 0},
            Results
        ),

    io:format("Modules with debug_info: ~B / ~B (~.1f%)~n",
              [OkCount, TotalModules, pct(OkCount, TotalModules)]),
    io:format("Modules without debug_info: ~B~n", [ErrCount]),
    io:format("Total specced functions: ~B~n", [TotalFns]),
    io:format("Functions with useful types (not all Dynamic): ~B (~.1f%)~n",
              [UsefulFns, pct(UsefulFns, TotalFns)]),
    io:format("Functions with all-Dynamic signatures: ~B (~.1f%)~n",
              [AllDynamicFns, pct(AllDynamicFns, TotalFns)]),
    io:format("Parameters with meaningful names: ~B (~.1f%)~n",
              [NamedParams, pct(NamedParams, NamedParams + PositionalParams)]),
    io:format("Parameters with positional names: ~B (~.1f%)~n~n",
              [PositionalParams, pct(PositionalParams, NamedParams + PositionalParams)]).

classify_functions(Specs) ->
    lists:foldl(
        fun(#{params := Params, return_type := RetType}, {Useful, AllDyn}) ->
            AllParamsDynamic = lists:all(
                fun(#{type := T}) -> T =:= <<"Dynamic">> end,
                Params
            ),
            RetDynamic = RetType =:= <<"Dynamic">>,
            %% A function is "useful" if it has at least one non-Dynamic type
            case AllParamsDynamic andalso RetDynamic of
                true -> {Useful, AllDyn + 1};
                false -> {Useful + 1, AllDyn}
            end
        end,
        {0, 0},
        Specs
    ).

classify_params(Specs) ->
    lists:foldl(
        fun(#{params := Params}, {Named, Positional}) ->
            lists:foldl(
                fun(#{name := Name}, {N, P}) ->
                    case is_positional_name(Name) of
                        true -> {N, P + 1};
                        false -> {N + 1, P}
                    end
                end,
                {Named, Positional},
                Params
            )
        end,
        {0, 0},
        Specs
    ).

is_positional_name(<<"arg">>) -> true;
is_positional_name(<<"arg", Rest/binary>>) ->
    case catch binary_to_integer(Rest) of
        N when is_integer(N) -> true;
        _ -> false
    end;
is_positional_name(_) -> false.

print_top20_report(Top20, Results) ->
    io:format("--- Top-20 Module Detailed Report ---~n~n"),
    io:format("~-20s ~6s ~8s ~10s ~10s ~10s~n",
              ["Module", "Fns", "Useful%", "AllDyn%", "Named%", "Status"]),
    io:format("~s~n", [lists:duplicate(74, $-)]),
    lists:foreach(
        fun(Mod) ->
            case maps:find(Mod, Results) of
                {ok, {ok, Specs}} ->
                    FnCount = length(Specs),
                    {Useful, AllDyn} = classify_functions(Specs),
                    {Named, Positional} = classify_params(Specs),
                    UsefulPct = pct(Useful, FnCount),
                    AllDynPct = pct(AllDyn, FnCount),
                    NamedPct = pct(Named, Named + Positional),
                    Status = quality_status(UsefulPct, NamedPct),
                    io:format("~-20s ~6B ~7.1f% ~9.1f% ~9.1f% ~s~n",
                              [Mod, FnCount, UsefulPct, AllDynPct, NamedPct, Status]);
                {ok, {error, Reason}} ->
                    io:format("~-20s   ERROR: ~p~n", [Mod, Reason]);
                error ->
                    io:format("~-20s   NOT FOUND~n", [Mod])
            end
        end,
        Top20
    ),
    io:format("~n"),

    %% Print detailed function list for a few key modules
    KeyModules = [<<"lists">>, <<"maps">>, <<"gen_server">>, <<"erlang">>],
    lists:foreach(
        fun(Mod) ->
            case maps:find(Mod, Results) of
                {ok, {ok, Specs}} ->
                    io:format("~n  Functions in ~s (~B total):~n", [Mod, length(Specs)]),
                    %% Show first 30 functions, sorted by name
                    Sorted = lists:sort(fun(A, B) ->
                        maps:get(name, A) =< maps:get(name, B)
                    end, Specs),
                    ShowSpecs = lists:sublist(Sorted, 30),
                    lists:foreach(
                        fun(#{name := Name, arity := Arity, params := Params,
                              return_type := RetType}) ->
                            ParamStr = format_params(Params),
                            Quality = fn_quality_marker(Params, RetType),
                            io:format("    ~s ~s/~B(~s) -> ~s~n",
                                      [Quality, Name, Arity, ParamStr, RetType])
                        end,
                        ShowSpecs
                    ),
                    case length(Sorted) > 30 of
                        true -> io:format("    ... (~B more)~n", [length(Sorted) - 30]);
                        false -> ok
                    end;
                _ -> ok
            end
        end,
        KeyModules
    ),
    io:format("~n").

format_params(Params) ->
    Parts = lists:map(
        fun(#{name := Name, type := Type}) ->
            iolist_to_binary([Name, " :: ", Type])
        end,
        Params
    ),
    iolist_to_binary(lists:join(<<", ">>, Parts)).

fn_quality_marker(Params, RetType) ->
    AllParamsDynamic = lists:all(
        fun(#{type := T}) -> T =:= <<"Dynamic">> end, Params),
    RetDynamic = RetType =:= <<"Dynamic">>,
    case {AllParamsDynamic, RetDynamic} of
        {true, true} -> "  ";  %% all Dynamic
        {false, false} -> "++";  %% fully typed
        _ -> "+ "  %% partially typed
    end.

quality_status(UsefulPct, NamedPct) when UsefulPct >= 80, NamedPct >= 70 ->
    "EXCELLENT";
quality_status(UsefulPct, NamedPct) when UsefulPct >= 60, NamedPct >= 50 ->
    "GOOD";
quality_status(UsefulPct, _) when UsefulPct >= 40 ->
    "FAIR";
quality_status(_, _) ->
    "POOR".

print_keyword_quality(Results) ->
    io:format("--- Keyword Name Quality Analysis ---~n~n"),
    %% Collect all param names that are not positional
    AllNames = maps:fold(
        fun(_Mod, {ok, Specs}, Acc) ->
            lists:foldl(
                fun(#{params := Params}, InnerAcc) ->
                    lists:foldl(
                        fun(#{name := Name}, NAcc) ->
                            case is_positional_name(Name) of
                                true -> NAcc;
                                false -> [Name | NAcc]
                            end
                        end,
                        InnerAcc,
                        Params
                    )
                end,
                Acc,
                Specs
            );
           (_, _, Acc) -> Acc
        end,
        [],
        Results
    ),

    %% Count frequencies
    FreqMap = lists:foldl(
        fun(Name, Acc) ->
            maps:update_with(Name, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        AllNames
    ),

    %% Sort by frequency
    FreqList = lists:sort(
        fun({_, C1}, {_, C2}) -> C1 >= C2 end,
        maps:to_list(FreqMap)
    ),

    io:format("Total unique parameter names: ~B~n", [maps:size(FreqMap)]),
    io:format("Total named parameters: ~B~n~n", [length(AllNames)]),

    io:format("Top-30 most common parameter names:~n"),
    Top30 = lists:sublist(FreqList, 30),
    lists:foreach(
        fun({Name, Count}) ->
            io:format("  ~-25s  ~B~n", [Name, Count])
        end,
        Top30
    ),

    %% Check for "bad" names (single char, all-caps internal Erlang names)
    BadNames = lists:filter(
        fun({Name, _}) ->
            byte_size(Name) =:= 1
        end,
        FreqList
    ),
    io:format("~nSingle-character parameter names (potential low quality):~n"),
    lists:foreach(
        fun({Name, Count}) ->
            io:format("  ~-25s  ~B~n", [Name, Count])
        end,
        BadNames
    ),
    io:format("~n").

print_recommendation(Results, Top20) ->
    io:format("--- Recommendation ---~n~n"),

    %% Calculate top-20 aggregate stats
    {TotalFns, UsefulFns, NamedParams, TotalParams} =
        lists:foldl(
            fun(Mod, {TF, UF, NP, TP}) ->
                case maps:find(Mod, Results) of
                    {ok, {ok, Specs}} ->
                        FC = length(Specs),
                        {U, _} = classify_functions(Specs),
                        {N, P} = classify_params(Specs),
                        {TF + FC, UF + U, NP + N, TP + N + P};
                    _ ->
                        {TF, UF, NP, TP}
                end
            end,
            {0, 0, 0, 0},
            Top20
        ),

    UsefulPct = pct(UsefulFns, TotalFns),
    NamedPct = pct(NamedParams, TotalParams),

    io:format("Top-20 modules aggregate:~n"),
    io:format("  Total functions: ~B~n", [TotalFns]),
    io:format("  With useful types: ~B (~.1f%)~n", [UsefulFns, UsefulPct]),
    io:format("  Named parameters: ~B / ~B (~.1f%)~n~n",
              [NamedParams, TotalParams, NamedPct]),

    if
        UsefulPct >= 90.0, NamedPct >= 70.0 ->
            io:format("RECOMMENDATION: Phase 2 (stubs) can be DEFERRED indefinitely.~n"),
            io:format("Auto-extract quality exceeds the 90% threshold for commonly-used~n"),
            io:format("functions. Proceed directly to Phase 3 (CLI cleanup) and Phase 4 (REPL devex).~n");
        UsefulPct >= 70.0 ->
            io:format("RECOMMENDATION: Phase 2 (stubs) should be LIMITED to targeted modules.~n"),
            io:format("Auto-extract quality is good but not excellent for some modules.~n"),
            io:format("Write stubs only for specific functions where Dynamic is unhelpful.~n");
        true ->
            io:format("RECOMMENDATION: Phase 2 (stubs) is NEEDED for commonly-used modules.~n"),
            io:format("Auto-extract quality does not meet the 90% threshold.~n"),
            io:format("Prioritize stub authoring for modules with poor coverage.~n")
    end,
    io:format("~n").

pct(_, 0) -> 0.0;
pct(N, Total) -> (N * 100.0) / Total.
