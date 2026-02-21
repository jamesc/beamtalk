%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Stdlib expression test runner for compiled `// =>` assertion tests.
%%%
%%% **DDD Context:** Runtime (Testing subdomain)
%%%
%%% Provides the runtime support for stdlib expression tests (ADR 0014 Phase 1).
%%% The Rust test compiler generates thin EUnit wrappers that call into this
%%% module rather than inlining all formatting, matching, and assertion logic.
%%%
%%% Assertion spec types:
%%%   {value, EvalModule, Expected, VarName, Location}
%%%   {value_wildcard, EvalModule, Expected, VarName, Location}
%%%   {value_any, EvalModule, VarName, Location}
%%%   {error, EvalModule, ExpectedKind, VarName, Location}
%%%
%%% Where:
%%%   EvalModule  :: atom()   - compiled expression module with eval/1
%%%   Expected    :: binary() - expected formatted result string
%%%   ExpectedKind:: atom()   - expected error kind for error assertions
%%%   VarName     :: atom() | none - variable name for binding assignment
%%%   Location    :: binary() - source location "file:line `expr`"

-module(beamtalk_stdlib_test).
-include_lib("kernel/include/logger.hrl").

-export([
    run_and_assert/2,
    format_result/1,
    matches_pattern/2
]).

%% ──────────────────────────────────────────────────────────────────────────
%% run_and_assert/2 — main entry point for EUnit wrappers
%% ──────────────────────────────────────────────────────────────────────────

%% @doc Run a list of assertion specs sequentially, threading bindings.
%%
%% Each assertion is wrapped in try/catch so failures don't cascade.
%% Variable bindings carry forward on success; on crash, the previous
%% bindings are preserved for subsequent assertions.
%%
%% Prints structured output for the Rust result parser:
%%   RESULTS:module:passed:failed
%%   FAIL location
%%     expected: value
%%     got:      value
-spec run_and_assert(atom(), list()) -> ok | no_return().
run_and_assert(TestModule, Assertions) ->
    {_FinalBindings, Results} = lists:foldl(
        fun(Spec, {Bindings, Acc}) ->
            {NewBindings, Result} = run_one(Spec, Bindings),
            {NewBindings, [Result | Acc]}
        end,
        {#{}, []},
        Assertions
    ),
    RevResults = lists:reverse(Results),
    Passed = length([ok || {pass, _} <- RevResults]),
    Failed = length(RevResults) - Passed,
    %% Print individual failures first (before summary)
    lists:foreach(fun
        ({fail, Location, Expected, Actual}) ->
            io:format("FAIL ~s~n  expected: ~s~n  got:      ~s~n",
                      [Location, Expected, Actual]);
        ({crash, Location, Class, Reason, _Stack}) ->
            io:format("FAIL ~s~n  crashed:  ~p:~p~n", [Location, Class, Reason]);
        ({pass, _}) ->
            ok
    end, RevResults),
    %% Print structured results line
    io:format("RESULTS:~s:~p:~p~n", [TestModule, Passed, Failed]),
    %% Signal EUnit pass/fail
    case Failed of
        0 -> ok;
        _ -> error({test_failures, Failed})
    end.

%% ──────────────────────────────────────────────────────────────────────────
%% Individual assertion runners
%% ──────────────────────────────────────────────────────────────────────────

%% @doc Run a single assertion spec, returning {NewBindings, Result}.
-spec run_one(tuple(), map()) -> {map(), tuple()}.

%% Value assertion: compare format_result(Value) =:= Expected
run_one({value, EvalMod, Expected, VarName, Location}, Bindings) ->
    try EvalMod:eval(Bindings) of
        {Value, RawBindings} ->
            NewBindings = maybe_bind(VarName, Value, RawBindings),
            Actual = format_result(Value),
            case Actual =:= Expected of
                true  -> {NewBindings, {pass, Location}};
                false -> {NewBindings, {fail, Location, Expected, Actual}}
            end
    catch Class:Reason:Stack ->
        {Bindings, {crash, Location, Class, Reason, Stack}}
    end;

%% Wildcard value assertion: use matches_pattern/2 (BT-502)
run_one({value_wildcard, EvalMod, Expected, VarName, Location}, Bindings) ->
    try EvalMod:eval(Bindings) of
        {Value, RawBindings} ->
            NewBindings = maybe_bind(VarName, Value, RawBindings),
            Actual = format_result(Value),
            case matches_pattern(Expected, Actual) of
                true  -> {NewBindings, {pass, Location}};
                false -> {NewBindings, {fail, Location, Expected, Actual}}
            end
    catch Class:Reason:Stack ->
        {Bindings, {crash, Location, Class, Reason, Stack}}
    end;

%% Any value assertion (bare wildcard _): execute but don't check result
run_one({value_any, EvalMod, VarName, Location}, Bindings) ->
    try EvalMod:eval(Bindings) of
        {Value, RawBindings} ->
            NewBindings = maybe_bind(VarName, Value, RawBindings),
            {NewBindings, {pass, Location}}
    catch Class:Reason:Stack ->
        {Bindings, {crash, Location, Class, Reason, Stack}}
    end;

%% Error assertion: expect expression to raise #beamtalk_error{kind = Kind}
run_one({error, EvalMod, ExpectedKind, _VarName, Location}, Bindings) ->
    try EvalMod:eval(Bindings) of
        {_Value, _RawBindings} ->
            %% Expression succeeded but we expected an error
            ExpectedBin = atom_to_binary(ExpectedKind, utf8),
            {Bindings, {fail, Location,
                        <<"ERROR: ", ExpectedBin/binary>>,
                        <<"no error raised">>}}
    catch
        %% Wrapped Exception objects (ADR 0015)
        error:#{
            '$beamtalk_class' := _,
            error := {beamtalk_error, Kind, _, _, _, _, _}
        } ->
            check_error_kind(Kind, ExpectedKind, Location, Bindings);
        %% Direct #beamtalk_error{} records
        error:{beamtalk_error, Kind, _, _, _, _, _} ->
            check_error_kind(Kind, ExpectedKind, Location, Bindings);
        %% Future-rejected errors
        throw:{future_rejected, {beamtalk_error, Kind, _, _, _, _, _}} ->
            check_error_kind(Kind, ExpectedKind, Location, Bindings);
        %% Plain atom errors (e.g., badarith, badarg)
        error:ExpectedKind ->
            {Bindings, {pass, Location}};
        %% Other errors
        error:OtherReason ->
            OtherBin = iolist_to_binary(io_lib:format("~p", [OtherReason])),
            ExpectedBin = atom_to_binary(ExpectedKind, utf8),
            {Bindings, {fail, Location,
                        <<"ERROR: ", ExpectedBin/binary>>,
                        <<"ERROR: ", OtherBin/binary>>}};
        Class:Reason:Stack ->
            {Bindings, {crash, Location, Class, Reason, Stack}}
    end.

%% @doc Check if caught error kind matches expected.
-spec check_error_kind(atom(), atom(), binary(), map()) -> {map(), tuple()}.
check_error_kind(Kind, ExpectedKind, Location, Bindings) ->
    case Kind =:= ExpectedKind of
        true ->
            {Bindings, {pass, Location}};
        false ->
            KindBin = atom_to_binary(Kind, utf8),
            ExpectedBin = atom_to_binary(ExpectedKind, utf8),
            {Bindings, {fail, Location,
                        <<"ERROR: ", ExpectedBin/binary>>,
                        <<"ERROR: ", KindBin/binary>>}}
    end.

%% @doc Optionally bind a variable name to the result value.
-spec maybe_bind(atom() | none, term(), map()) -> map().
maybe_bind(none, _Value, Bindings) ->
    Bindings;
maybe_bind(VarName, Value, Bindings) ->
    maps:put(VarName, Value, Bindings).

%% ──────────────────────────────────────────────────────────────────────────
%% format_result/1 — Beamtalk value → display string
%% ──────────────────────────────────────────────────────────────────────────

%% @doc Format a Beamtalk runtime value to its display binary string.
%%
%% Mirrors the REPL's term_to_json formatting so that expected values
%% written as strings (like E2E tests) match correctly.
-spec format_result(term()) -> binary().
format_result(V) when is_integer(V) ->
    integer_to_binary(V);
format_result(V) when is_float(V) ->
    %% Match Erlang/REPL float formatting
    list_to_binary(io_lib:format("~p", [V]));
format_result(true) ->
    <<"true">>;
format_result(false) ->
    <<"false">>;
format_result(nil) ->
    <<"nil">>;
format_result(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
format_result(V) when is_binary(V) ->
    V;
format_result(V) when is_function(V) ->
    {arity, A} = erlang:fun_info(V, arity),
    iolist_to_binary([<<"a Block/">>, integer_to_binary(A)]);
format_result(V) when is_pid(V) ->
    S = pid_to_list(V),
    I = lists:sublist(S, 2, length(S) - 2),
    iolist_to_binary([<<"#Actor<">>, I, <<">">>]);
format_result(V) when is_tuple(V), tuple_size(V) >= 2,
                       element(1, V) =:= beamtalk_object ->
    %% BT-412: Match REPL formatting for class objects vs actor instances
    Class = element(2, V),
    case beamtalk_class_registry:is_class_name(Class) of
        true ->
            beamtalk_class_registry:class_display_name(Class);
        false ->
            Pid = element(4, V),
            ClassBin = atom_to_binary(Class, utf8),
            PidStr = pid_to_list(Pid),
            Inner = lists:sublist(PidStr, 2, length(PidStr) - 2),
            iolist_to_binary([<<"#Actor<">>, ClassBin, <<",">>, Inner, <<">">>])
    end;
format_result(V) when is_map(V) ->
    %% BT-535: Use print_string for Beamtalk display format
    beamtalk_primitive:print_string(V);
format_result(V) when is_list(V) ->
    case V of
        [] -> <<"[]">>;
        _ ->
            try jsx:encode([beamtalk_repl_json:term_to_json(E) || E <- V])
            catch _:_ -> iolist_to_binary(io_lib:format("~p", [V])) end
    end;
format_result(V) ->
    iolist_to_binary(io_lib:format("~p", [V])).

%% ──────────────────────────────────────────────────────────────────────────
%% matches_pattern/2 — glob-style wildcard matching (BT-502)
%% ──────────────────────────────────────────────────────────────────────────

%% @doc Glob-style pattern matching where `_` matches any substring,
%% but only when `_` is NOT flanked by alphanumeric characters on both sides.
%% This preserves literal underscores in identifiers like `does_not_understand`.
-spec matches_pattern(binary(), binary()) -> boolean().
matches_pattern(Pattern, Actual) ->
    Segments = wildcard_segments(Pattern),
    matches_segments(Segments, Actual, 0, true).

wildcard_segments(Pattern) ->
    Chars = binary_to_list(Pattern),
    [list_to_binary(S) || S <- wildcard_split(Chars, [], [], none)].

wildcard_split([], CurRev, SegsRev, _Prev) ->
    lists:reverse([lists:reverse(CurRev) | SegsRev]);
wildcard_split([$_ | Rest], CurRev, SegsRev, Prev) ->
    Next = case Rest of [N | _] -> N; [] -> none end,
    case is_alnum(Prev) andalso is_alnum(Next) of
        true -> wildcard_split(Rest, [$_ | CurRev], SegsRev, $_);
        false -> wildcard_split(Rest, [], [lists:reverse(CurRev) | SegsRev], $_)
    end;
wildcard_split([C | Rest], CurRev, SegsRev, _Prev) ->
    wildcard_split(Rest, [C | CurRev], SegsRev, C).

is_alnum(C) when is_integer(C), C >= $0, C =< $9 -> true;
is_alnum(C) when is_integer(C), C >= $A, C =< $Z -> true;
is_alnum(C) when is_integer(C), C >= $a, C =< $z -> true;
is_alnum(_) -> false.

matches_segments([], _Actual, _Pos, IsFirst) ->
    not IsFirst;
matches_segments([<<>> | Rest], Actual, Pos, _IsFirst) ->
    matches_segments(Rest, Actual, Pos, false);
matches_segments([Seg | Rest], Actual, Pos, true) ->
    %% First segment must match at start
    case binary:match(Actual, Seg, [{scope, {Pos, byte_size(Actual) - Pos}}]) of
        {0, Len} -> matches_segments(Rest, Actual, Len, false);
        _ -> false
    end;
matches_segments([Seg], Actual, Pos, false) ->
    %% Last non-empty segment must match at end
    SLen = byte_size(Seg),
    ALen = byte_size(Actual),
    Start = ALen - SLen,
    Start >= Pos andalso binary:part(Actual, Start, SLen) =:= Seg;
matches_segments([Seg | Rest], Actual, Pos, false) ->
    case binary:match(Actual, Seg, [{scope, {Pos, byte_size(Actual) - Pos}}]) of
        {Found, Len} -> matches_segments(Rest, Actual, Found + Len, false);
        nomatch -> false
    end.
