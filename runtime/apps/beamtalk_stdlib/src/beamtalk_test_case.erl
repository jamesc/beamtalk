%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_case).

%%% **DDD Context:** Object System Context

-moduledoc """
TestCase primitive implementations for BUnit test framework.

This module provides the runtime support for TestCase assertion methods.
All assertions create structured #beamtalk_error{} records with kind
`assertion_failed` when they fail.

Part of ADR 0014: BUnit — Beamtalk Test Framework (Phase 2).
""".
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    should_raise/2,
    fail/1,
    skip/1,
    run_all/1,
    run_single/2,
    execute_tests/5,
    run_all_structured/1,
    run_single_structured/2,
    find_test_classes/0,
    test_class_selectors/1,
    class_test_plan/1,
    spawn_test_execution/6,
    %% Exported for beamtalk_test_runner
    run_test_method/4,
    run_test_method/5,
    run_suite_lifecycle/5,
    structure_results/3,
    resolve_module/1,
    %% Exported for testing
    is_valid_setUp_result/2
]).

%% FFI shims for (Erlang beamtalk_test_case) dispatch
-export([should/2, runAll/1, runClass/2, skipTest/1, suiteFixture/1]).

-ifdef(TEST).
%% Exported for testing stack trace formatting
-export([
    format_stacktrace/1,
    stack_frames/1,
    source_snippet/2,
    test_failure/4,
    test_failure_untraced/4
]).
-endif.

-doc """
Assert that a block raises an error of the specified kind.

Executes the block and verifies that it raises an error with the
given kind atom. If the block completes normally or raises a different
kind of error, the assertion fails.

Example:
  Block = fun() -> error(my_error) end,
  should_raise(Block, my_error)  % => passes

Note: Block is a zero-argument Erlang fun in Core Erlang codegen.
""".
-spec should_raise(fun(() -> term()), atom()) -> 'nil'.
should_raise(Block, ExpectedKind) when is_function(Block, 0), is_atom(ExpectedKind) ->
    %% ADR 0130 §4: this catch swallows the block's error, so it is a catch
    %% boundary; `protect/1` restores the class-variable map before we classify.
    %% bt-catcher-audit: converted - the block runs under protect/1, which restores on error
    try beamtalk_class_vars:protect(Block) of
        _ ->
            % Block completed without error
            NoRaiseErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
            NoRaiseErr1 = beamtalk_error:with_selector(NoRaiseErr0, 'should:raise:'),
            NoRaiseMsg = beamtalk_error:chars_to_binary(
                io_lib:format("Expected block to raise ~ts but it completed normally", [
                    ExpectedKind
                ])
            ),
            NoRaiseErr2 = beamtalk_error:with_message(NoRaiseErr1, NoRaiseMsg),
            NoRaiseErr3 = beamtalk_error:with_details(NoRaiseErr2, #{
                expected_kind => ExpectedKind, actual => completed
            }),
            beamtalk_error:raise(NoRaiseErr3)
    catch
        Class:Exception:Stack ->
            %% BT-3930: classify with the same normalisation `on:do:` uses
            %% (beamtalk_exception_handler:ensure_wrapped/3), so a raw Erlang
            %% error such as {badkey, K} reports the same kind (key_error) here
            %% as `e kind` does inside `on:do:`.
            ActualKind = wrapped_error_kind(Class, Exception, Stack),
            %% extract_error_kind/1 is kept only as a legacy alias so existing
            %% `should: [...] raise: #badarith` (raw atom) assertions still pass.
            case
                ExpectedKind =:= ActualKind orelse ExpectedKind =:= extract_error_kind(Exception)
            of
                true ->
                    nil;
                false ->
                    MismatchErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
                    MismatchErr1 = beamtalk_error:with_selector(MismatchErr0, 'should:raise:'),
                    MismatchMsg = beamtalk_error:chars_to_binary(
                        io_lib:format("Expected error kind ~ts but got ~ts", [
                            ExpectedKind, ActualKind
                        ])
                    ),
                    MismatchErr2 = beamtalk_error:with_message(MismatchErr1, MismatchMsg),
                    MismatchErr3 = beamtalk_error:with_details(MismatchErr2, #{
                        expected_kind => ExpectedKind, actual_kind => ActualKind
                    }),
                    beamtalk_error:raise(MismatchErr3)
            end
    end;
should_raise(Block, ExpectedKind) ->
    Error0 = beamtalk_error:new(type_error, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'should:raise:'),
    Message =
        case {is_function(Block, 0), is_atom(ExpectedKind)} of
            {false, _} ->
                <<"Expected a zero-argument block, got: ",
                    (beamtalk_primitive:print_string(Block))/binary>>;
            {_, false} ->
                iolist_to_binary(
                    [
                        <<"Expected an error kind atom, got: ">>,
                        beamtalk_primitive:print_string(ExpectedKind)
                    ]
                )
        end,
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

-doc """
Unconditionally fail with a message.

Always raises an assertion_failed error with the given message.
Used when a test detects an invariant violation.

Example:
  fail(<<"This should not happen">>)
""".
-spec fail(binary() | atom()) -> no_return().
fail(Message) when is_binary(Message) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'fail:'),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2);
fail(Message) when is_atom(Message) ->
    fail(atom_to_binary(Message, utf8));
fail(Message) ->
    fail(beamtalk_primitive:print_string(Message)).

-doc """
Signal that the current test should be skipped.

Throws {bunit_skip, Reason}, caught by run_test_method/4 as a {skip, ...}
result. A skipped test is neither a pass nor a failure.

Example:
  skip(<<"Unix only">>)
""".
-spec skip(binary() | atom()) -> no_return().
skip(Reason) when is_binary(Reason) ->
    throw({bunit_skip, Reason});
skip(Reason) when is_atom(Reason) ->
    throw({bunit_skip, atom_to_binary(Reason, utf8)});
skip(Reason) ->
    throw({bunit_skip, beamtalk_primitive:print_string(Reason)}).

-doc """
Execute tests from class-side dispatch.

Called from beamtalk_object_class handle_call via spawned process.
Receives Selector (runAll | 'run:'), Args, ClassName, Module, and
FlatMethods directly from gen_server state — no gen_server calls needed.
""".
-spec execute_tests(atom(), list(), atom(), atom(), map()) -> binary().
execute_tests(runAll, _Args, ClassName, Module, FlatMethods) ->
    TestMethods = discover_test_methods(FlatMethods),
    case TestMethods of
        [] ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "No test methods found in ~ts", [ClassName]
                )
            );
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = run_suite_lifecycle(
                ClassName,
                Module,
                FlatMethods,
                TestMethods,
                fun(SuiteFixture) ->
                    [
                        run_test_method(ClassName, Module, Method, FlatMethods, SuiteFixture)
                     || Method <- TestMethods
                    ]
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end;
execute_tests('run:', [TestMethodName], ClassName, Module, FlatMethods) ->
    case maps:is_key(TestMethodName, FlatMethods) of
        false ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Method '~ts' not found in ~ts", [TestMethodName, ClassName]
                )
            );
        true ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = run_suite_lifecycle(
                ClassName,
                Module,
                FlatMethods,
                [TestMethodName],
                fun(SuiteFixture) ->
                    [run_test_method(ClassName, Module, TestMethodName, FlatMethods, SuiteFixture)]
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end.

-doc """
Run all test methods of a class, outside the class process (BIF fallback
path). Tests are the class's own and inherited `test*` selectors, from
`class_test_plan/1`.
WARNING: May deadlock if called from within a class gen_server handle_call.
Prefer execute_tests/5 which receives gen_server state directly and avoids
any gen_server:call back to the class process.
""".
-spec run_all(atom()) -> binary().
run_all(ClassName) ->
    case run_class_tests(ClassName, all) of
        no_tests ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "No test methods found in ~ts", [ClassName]
                )
            );
        {Results, Duration} ->
            format_results(Results, Duration)
    end.

-doc """
Run a single test method (BIF fallback path), with the class's own and
inherited setUp/tearDown and setUpOnce/tearDownOnce.
WARNING: May deadlock if called from within a class gen_server handle_call.
""".
-spec run_single(atom(), atom()) -> binary().
run_single(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    {Results, Duration} = run_class_tests(ClassName, TestMethodName),
    format_results(Results, Duration).

-doc """
Run all tests for a class and return structured results.

Returns a map with per-test results suitable for JSON encoding.
Uses the BIF fallback path (safe outside gen_server); tests come from
`class_test_plan/1`, so inherited ones are included.
""".
-spec run_all_structured(atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{
                name := atom(),
                class := atom(),
                status := pass | fail | skip,
                error => binary(),
                frames => [map()],
                reason => binary()
            }
        ]
    }.
run_all_structured(ClassName) ->
    case run_class_tests(ClassName, all) of
        no_tests ->
            structure_results(ClassName, [], 0.0);
        {Results, Duration} ->
            structure_results(ClassName, Results, Duration)
    end.

-doc "Run a single test method and return structured results.".
-spec run_single_structured(atom(), atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{
                name := atom(),
                class := atom(),
                status := pass | fail | skip,
                error => binary(),
                frames => [map()],
                reason => binary()
            }
        ]
    }.
run_single_structured(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    {Results, Duration} = run_class_tests(ClassName, TestMethodName),
    structure_results(ClassName, Results, Duration).

-doc """
Shared body of the BIF-fallback entry points: run every test of the class
(`all`) or one named method, with the suite and per-test lifecycle, and time
it. Returns `no_tests` when `all` finds no test methods.
""".
-spec run_class_tests(atom(), all | atom()) ->
    no_tests
    | {
        [
            {pass, atom()}
            | {fail, atom(), binary()}
            | {fail, atom(), binary(), [map()]}
            | {skip, atom(), binary()}
        ],
        float()
    }.
run_class_tests(ClassName, Which) ->
    {AllTests, FlatMethods, Module} = class_test_plan(ClassName),
    TestMethods =
        case Which of
            all -> AllTests;
            MethodName -> [MethodName]
        end,
    case TestMethods of
        [] ->
            no_tests;
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = run_suite_lifecycle(
                ClassName,
                Module,
                FlatMethods,
                TestMethods,
                fun(SuiteFixture) ->
                    [
                        run_test_method(ClassName, Module, Method, FlatMethods, SuiteFixture)
                     || Method <- TestMethods
                    ]
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            {Results, (EndTime - StartTime) / 1000.0}
    end.

-doc """
The single answer to "which tests does this class have" outside the class
process (BT-3786): `{TestMethods, FlatMethods, Module}`.

- `FlatMethods` maps every selector from `test_class_selectors/1` (own and
  inherited below `TestCase`) to `true`, for setUp/tearDown and
  setUpOnce/tearDownOnce detection.
- `TestMethods` is the sorted `test*` subset of those selectors.
- `Module` is the module the class process was compiled with, read with the
  deadlock-safe `beamtalk_object_class:module_name_safe/1`; for a class that
  is not registered it falls back to `resolve_module/1`'s naming convention.

Used by `run_all/1`, `run_single/2`, `run_all_structured/1`,
`run_single_structured/2` and `beamtalk_test_runner`.
""".
-spec class_test_plan(atom()) -> {[atom()], map(), atom()}.
class_test_plan(ClassName) ->
    FlatMethods = maps:from_keys(test_class_selectors(ClassName), true),
    Module =
        case beamtalk_class_registry:whereis_class(ClassName) of
            undefined -> resolve_module(ClassName);
            ClassPid -> beamtalk_object_class:module_name_safe(ClassPid)
        end,
    {discover_test_methods(FlatMethods), FlatMethods, Module}.

-doc """
Find all loaded, concrete TestCase subclasses.

Uses the class hierarchy ETS table to find all classes that inherit
from TestCase. Returns class names as atoms. An `abstract` subclass is
left out (BT-3768): it holds `test*` methods for its concrete subclasses
to inherit and is never run itself.
""".
-spec find_test_classes() -> [atom()].
find_test_classes() ->
    [
        ClassName
     || ClassName <- beamtalk_class_registry:all_subclasses('TestCase'),
        beamtalk_class_metadata:lookup_is_abstract(ClassName) =/= {ok, true}
    ].

-doc """
Instance selectors of a test class: its own and those it inherits from every
superclass below `TestCase` (BT-3768), so `test*`, `setUp` and `tearDown`
methods defined on an abstract test case are run for each concrete subclass.
`TestCase`'s own methods are not included. Walks the chain with
`beamtalk_behaviour_intrinsics:walk_hierarchy/3`, reading each level's
methods from its class process; call it from outside those processes.
""".
-spec test_class_selectors(atom()) -> [atom()].
test_class_selectors(ClassName) ->
    Selectors = beamtalk_behaviour_intrinsics:walk_hierarchy(
        ClassName,
        fun
            ('TestCase', _ClassPid, Acc) ->
                {halt, Acc};
            (_Name, ClassPid, Acc) ->
                {cont, ordsets:union(Acc, ordsets:from_list(gen_server:call(ClassPid, methods)))}
        end,
        ordsets:new()
    ),
    ordsets:to_list(Selectors).

%%% Internal helpers

-doc """
Decode a BEAM error term into a human-readable binary message.

Translates common Erlang error shapes and Beamtalk wrappers into
readable messages instead of raw ~p formatting.
""".
-spec decode_beam_error(term()) -> binary().
decode_beam_error({future_rejected, Inner}) ->
    iolist_to_binary([<<"future rejected: ">>, decode_beam_error(Inner)]);
decode_beam_error({error, Map}) when is_map(Map) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(error, Map) of
        {ok, Inner} -> decode_beam_error(Inner);
        _ -> beamtalk_primitive:print_string(Map)
    end;
%% BT-4173: error messages are tagged Strings — unwrap to the raw bytes
%% this decoder's binary() contract promises.
decode_beam_error(#beamtalk_error{message = Msg}) ->
    beamtalk_text:to_binary(Msg);
decode_beam_error(#{class := 'Exception', error := #beamtalk_error{message = Msg}}) ->
    beamtalk_text:to_binary(Msg);
decode_beam_error(#{'$beamtalk_class' := _, error := #beamtalk_error{message = Msg}}) ->
    beamtalk_text:to_binary(Msg);
decode_beam_error({badkey, Key}) ->
    <<"key ", (beamtalk_primitive:print_string(Key))/binary, " not found in dictionary">>;
decode_beam_error({badmap, Map}) ->
    <<(beamtalk_primitive:print_string(Map))/binary, " is not a map">>;
decode_beam_error({badmatch, Value}) ->
    <<"no match of right-hand side value ", (beamtalk_primitive:print_string(Value))/binary>>;
decode_beam_error({case_clause, Value}) ->
    <<"no case clause matched ", (beamtalk_primitive:print_string(Value))/binary>>;
decode_beam_error({try_clause, Value}) ->
    <<"no try clause matched ", (beamtalk_primitive:print_string(Value))/binary>>;
decode_beam_error(function_clause) ->
    <<"no matching function clause">>;
decode_beam_error(if_clause) ->
    <<"no true branch in conditional">>;
decode_beam_error({badarity, {Fun, Args}}) when is_function(Fun), is_list(Args) ->
    {arity, Expected} = erlang:fun_info(Fun, arity),
    beamtalk_error:chars_to_binary(
        io_lib:format("function expects ~tp arguments but was called with ~tp", [
            Expected, length(Args)
        ])
    );
decode_beam_error({badfun, Term}) ->
    <<(beamtalk_primitive:print_string(Term))/binary, " is not a function">>;
decode_beam_error(badarg) ->
    <<"bad argument">>;
decode_beam_error(badarith) ->
    <<"bad arithmetic operation">>;
decode_beam_error(undef) ->
    <<"undefined function">>;
decode_beam_error(noproc) ->
    <<"no such process or port">>;
decode_beam_error(timeout) ->
    <<"operation timed out">>;
decode_beam_error(Other) ->
    beamtalk_primitive:print_string(Other).

-type trace_entry() :: {
    Rendered :: binary(),
    Parts :: map(),
    Count :: pos_integer(),
    Src :: binary() | nil,
    IsStdlib :: boolean()
}.

-doc """
Format a BEAM stacktrace for display in test failure messages.

Hides the frames `beamtalk_stack_frame:classify_frames/1` hides (OTP plumbing,
generated callbacks, runtime internals); shows up to 3 of the rest
(all of them, hidden ones included and each with its raw MFA, with
`beamtalk test --raw-mfa`).
The innermost non-stdlib frame whose source file and line are readable (else
the innermost readable stdlib frame) is followed by a source snippet
(BT-4358, BT-4388); snippet lines are context, not extra frames.
Error location (innermost frame) is shown first, with callers going outward.
Returns an empty binary when no relevant frames exist.
""".
-spec format_stacktrace(list()) -> binary().
format_stacktrace([]) ->
    <<>>;
format_stacktrace(Frames) ->
    %% BEAM stacktraces are most-recent-first (innermost/error frame at head).
    %% Keep that order so the error location appears first in output.
    %% When more than 3 frames exist, show the 2 innermost plus the outermost
    %% (test method) so the caller with file/line info is preserved.
    render_at(trace_entries(Frames)).

-doc """
Render entries as `\n  at ` lines (empty binary for no entries), with the
source snippet after the entry it belongs to. The single renderer for the
`at` text: every `at` line corresponds to exactly one entry, in order, and the
structured `frames` are derived from the same list (`entry_frame/1`).
""".
-spec render_at([trace_entry()]) -> binary().
render_at([]) ->
    <<>>;
render_at(Entries) ->
    Snippet = first_snippet(Entries),
    Lines = [
        [entry_line(E), snippet_after(Idx, Snippet)]
     || {Idx, E} <- lists:zip(lists:seq(1, length(Entries)), Entries)
    ],
    iolist_to_binary(["\n  at " | lists:join("\n  at ", Lines)]).

%% Snippet lines follow the `at` line of the entry they belong to. They never
%% start with `  at `, so the i-th `at` line still pairs with `frames[i]`.
-spec snippet_after(pos_integer(), {pos_integer(), binary()} | none) -> iodata().
snippet_after(Idx, {Idx, Snippet}) -> Snippet;
snippet_after(_Idx, _) -> [].

-doc """
The source snippet for the innermost entry whose source file and line are
readable, as `{Index, Snippet}` (1-based entry index), or `none` (BT-4358).

A user (non-`bt@stdlib@`) entry is preferred over a stdlib one, so a failure
raised inside a stdlib method shows the user's calling line rather than
stdlib internals; a stdlib entry is used only when no user entry is
readable (BT-4388). An Erlang entry (BT-4818) is used only when no Beamtalk
entry is readable.
""".
-spec first_snippet([trace_entry()]) -> {pos_integer(), binary()} | none.
first_snippet(Entries) ->
    Indexed = lists:zip(lists:seq(1, length(Entries)), Entries),
    {Beamtalk, Erlang} = lists:partition(
        fun({_, {_, #{kind := Kind}, _, _, _}}) -> Kind =/= erlang end, Indexed
    ),
    {Stdlib, User} = lists:partition(fun({_, {_, _, _, _, IsStdlib}}) -> IsStdlib end, Beamtalk),
    %% Erlang frames (a raising OTP or hex-dependency frame) come last: their
    %% source is not what the user can act on while a `.bt` line is readable.
    first_readable_of([User, Stdlib, Erlang]).

-spec first_readable_of([[{pos_integer(), trace_entry()}]]) -> {pos_integer(), binary()} | none.
first_readable_of([]) ->
    none;
first_readable_of([Group | Rest]) ->
    case first_readable(Group) of
        none -> first_readable_of(Rest);
        Found -> Found
    end.

-spec first_readable([{pos_integer(), trace_entry()}]) -> {pos_integer(), binary()} | none.
first_readable([]) ->
    none;
first_readable([{Idx, {_Text, #{line := Line}, _N, Src, _IsStdlib}} | Rest]) ->
    case is_integer(Line) andalso is_binary(Src) andalso source_snippet(Src, Line) of
        Snippet when is_binary(Snippet) -> {Idx, Snippet};
        _ -> first_readable(Rest)
    end.

%% Caps on what a snippet reads and shows, so a huge or minified source file
%% can neither stall a test run nor flood its output.
-define(SNIPPET_MAX_READ_BYTES, 262144).
-define(SNIPPET_MAX_LINE_CHARS, 160).
-define(SNIPPET_CONTEXT, 1).

-doc """
Render the source around `Line` of the file `Path`: the failing line marked
`>` with one line of context either side, as `"\n    12 | text"` lines.

Returns `none` (never crashes) when the file is unreadable, `Line` is beyond
the first 256 KiB or the end of the file, or the text is not valid UTF-8.
Overlong lines are truncated with `...`.
""".
-spec source_snippet(binary(), pos_integer()) -> binary() | none.
source_snippet(Path, Line) when is_binary(Path), is_integer(Line), Line > 0 ->
    case read_head(Path) of
        {ok, Data, Truncated} ->
            Rows = complete_rows(Data, Truncated),
            From = max(1, Line - ?SNIPPET_CONTEXT),
            To = Line + ?SNIPPET_CONTEXT,
            case Line =< length(Rows) of
                true -> render_snippet(Rows, From, To, Line);
                false -> none
            end;
        error ->
            none
    end;
source_snippet(_, _) ->
    none.

%% Split into complete lines. The last element is dropped when it is not a
%% whole line: a cut-off partial line when the read was truncated, or the
%% empty tail after a final newline (BT-4388).
-spec complete_rows(binary(), boolean()) -> [binary()].
complete_rows(Data, Truncated) ->
    Rows = binary:split(Data, <<"\n">>, [global]),
    case Truncated orelse binary:last(Data) =:= $\n of
        true -> lists:droplast(Rows);
        false -> Rows
    end.

-spec read_head(binary()) -> {ok, binary(), boolean()} | error.
read_head(Path) ->
    try file:open(Path, [read, binary, raw]) of
        {ok, Fd} ->
            try file:read(Fd, ?SNIPPET_MAX_READ_BYTES) of
                {ok, Data} when byte_size(Data) < ?SNIPPET_MAX_READ_BYTES ->
                    {ok, Data, false};
                {ok, Data} ->
                    %% Cap reached: truncated unless the file ends exactly here.
                    {ok, Data, file:read(Fd, 1) =/= eof};
                _ ->
                    error
            after
                file:close(Fd)
            end;
        _ ->
            error
    catch
        _:_ -> error
    end.

-spec render_snippet([binary()], pos_integer(), pos_integer(), pos_integer()) -> binary() | none.
render_snippet(Rows, From, To, Line) ->
    Window = lists:sublist(Rows, From, To - From + 1),
    Numbered = lists:zip(lists:seq(From, From + length(Window) - 1), Window),
    Width = length(integer_to_list(From + length(Window) - 1)),
    Rendered = [snippet_row(N, Text, Width, N =:= Line) || {N, Text} <- Numbered],
    case lists:member(invalid, Rendered) of
        true -> none;
        false -> iolist_to_binary(Rendered)
    end.

-spec snippet_row(pos_integer(), binary(), pos_integer(), boolean()) -> iodata() | invalid.
snippet_row(N, Raw, Width, IsFailing) ->
    Text0 = string:trim(Raw, trailing, "\r"),
    case unicode:characters_to_binary(Text0) of
        Text when is_binary(Text) ->
            Marker =
                case IsFailing of
                    true -> ">";
                    false -> " "
                end,
            Num = string:pad(integer_to_list(N), Width, leading),
            ["\n    ", Marker, " ", Num, " | ", truncate_line(Text)];
        _ ->
            invalid
    end.

-spec truncate_line(binary()) -> binary().
truncate_line(Text) ->
    case string:length(Text) > ?SNIPPET_MAX_LINE_CHARS of
        true ->
            <<
                (unicode:characters_to_binary(string:slice(Text, 0, ?SNIPPET_MAX_LINE_CHARS)))/binary,
                "..."
            >>;
        false ->
            Text
    end.

-ifdef(TEST).
-doc """
The structured form of the frames `format_stacktrace/1` renders (BT-4355).

One map per rendered line, innermost first: `class`, `selector`, `block`
(compiler-lifted block marker), `file` (display path or `nil`), `line` (`nil`
when unknown) and `repeat` (adjacent identical frames collapsed into this one,
the `[xN]` marker; the entry keeps the first collapsed frame's raw MFA, so
repeats whose lifted-fun names differ name only one of them), plus `structured`,
the `beamtalk_stack_frame:structured_frame/1` map carrying the display string, raw module/function/arity and `hidden` flag
(ADR 0139, decision 4). The synthetic test-method entry has no real stack
frame: its raw MFA is the test method's own. Built from the same selection and
`beamtalk_stack_frame:frame_parts/1` as the text, so the two cannot disagree.
""".
-spec stack_frames(list()) -> [map()].
stack_frames(Frames) ->
    [entry_frame(E) || E <- trace_entries(Frames)].
-endif.

-doc """
Whether raw output is wanted (`beamtalk test --raw-mfa`, ADR 0139 decision 4):
every frame, hidden ones included, each with its raw MFA beside the display name.
`beamtalk test` runs on a node of its own with one consumer (the CLI), so the
option travels through the `beamtalk_stdlib` application environment to reach
the test worker processes. It is the only transport: a workspace node, which
serves many clients, never sets it.
""".
-spec raw_mfa() -> boolean().
raw_mfa() ->
    application:get_env(beamtalk_stdlib, raw_mfa, false) =:= true.

-doc """
Filtered, selected and repeat-collapsed frames as `{Line, Parts, Count}`,
where Line is the rendered text of the frame.
""".
-spec trace_entries(list()) -> [trace_entry()].
trace_entries(Frames) ->
    collapse_repeats([wrapped_entry(W) || W <- select_frames(shown_frames(Frames))]).

-doc """
The wrapped frames this surface lists: those `beamtalk_stack_frame:classify_frames/1`
shows by default, or every frame with `--raw-mfa`. One `wrap/1` for the whole
stack shares the class-table fold and the loaded-module scan across frames.
""".
-spec shown_frames(list()) -> [map()].
shown_frames(Frames) ->
    shown_of(beamtalk_stack_frame:wrap(Frames)).

-spec shown_of([map()]) -> [map()].
shown_of(Wrapped) ->
    case raw_mfa() of
        true -> Wrapped;
        false -> beamtalk_stack_frame:visible_frames(Wrapped)
    end.

-spec frame_entry(term()) -> trace_entry().
frame_entry(Frame) ->
    wrapped_entry(beamtalk_stack_frame:wrap_frame(Frame)).

-spec wrapped_entry(map()) -> trace_entry().
wrapped_entry(Wrapped) ->
    {
        case raw_mfa() of
            true -> beamtalk_stack_frame:format_frame_raw(Wrapped);
            false -> beamtalk_stack_frame:format_frame(Wrapped)
        end,
        (beamtalk_stack_frame:frame_parts(Wrapped))#{
            structured => beamtalk_stack_frame:structured_frame(Wrapped)
        },
        1,
        beamtalk_stack_frame:frame_source_path(Wrapped),
        is_stdlib_module(maps:get(module, Wrapped, undefined))
    }.

-doc """
The one entry list a test failure is rendered from (BT-4402): the filtered,
selected, repeat-collapsed entries of the stack, followed by a synthetic
test-method entry when the test method's own frame is not among the selected
frames (a helper from the same module does not count). Returned as
`{Real, Synthetic}` (`Synthetic` is `[]` or one entry) so the untraced shape
can render only the synthetic one; `Real ++ Synthetic` is what the `at` text
and the structured `frames` both come from. The synthetic entry is built like
any other frame, from an empty location, so it gets the same
`beamtalk_source` file fallback and path rules.
""".
-spec build_trace(atom(), atom(), list()) -> {[trace_entry()], [trace_entry()]}.
build_trace(Module, MethodName, Stacktrace) ->
    {Real, Synthetic, _NotShown} = build_trace_counted(Module, MethodName, Stacktrace),
    {Real, Synthetic}.

-doc """
`build_trace/3` plus the number of stack frames the default output left out
(hidden by visibility or dropped by the 3-frame cap), counted from the same
single wrap and selection so the note cannot disagree with the lines.
""".
-spec build_trace_counted(atom(), atom(), list()) ->
    {[trace_entry()], [trace_entry()], non_neg_integer()}.
build_trace_counted(Module, MethodName, Stacktrace) ->
    Wrapped = beamtalk_stack_frame:wrap(Stacktrace),
    Relevant = select_frames(shown_of(Wrapped)),
    NotShown = length(Wrapped) - length(Relevant),
    HasTestFrame = lists:any(
        fun(W) ->
            maps:get(module, W, undefined) =:= Module andalso
                maps:get(function, W, undefined) =:= MethodName
        end,
        Relevant
    ),
    Synthetic =
        case HasTestFrame of
            true -> [];
            false -> [frame_entry({Module, MethodName, 0, []})]
        end,
    {collapse_repeats([wrapped_entry(W) || W <- Relevant]), Synthetic, NotShown}.

-spec entry_line(trace_entry()) -> binary().
entry_line({Line, _Parts, 1, _Src, _Std}) ->
    Line;
entry_line({Line, _Parts, N, _Src, _Std}) ->
    beamtalk_error:chars_to_binary(io_lib:format("~ts [x~B]", [Line, N])).

-spec entry_frame(trace_entry()) -> map().
entry_frame({_Line, Parts, N, _Src, _Std}) -> Parts#{repeat => N}.

-doc """
Collapse adjacent identical rendered frame lines into one.

Distinct frames that still render identically (a recursive send such as
`Integer>>timesRepeat:` whose BEAM frames carry no line) would otherwise print
the same `Class>>selector (file)` line twice. The repeat count is kept, in
Beamtalk terms, as a trailing `[xN]` marker.
""".
-spec collapse_repeats([trace_entry()]) -> [trace_entry()].
collapse_repeats([]) ->
    [];
collapse_repeats([{Line, Parts, N, Src, Std} | Rest]) ->
    {Same, Others} = lists:splitwith(fun({L, _, _, _, _}) -> L =:= Line end, Rest),
    [{Line, Parts, N + length(Same), Src, Std} | collapse_repeats(Others)].

-spec is_stdlib_module(term()) -> boolean().
is_stdlib_module(Module) when is_atom(Module) ->
    lists:prefix("bt@stdlib@", atom_to_list(Module));
is_stdlib_module(_) ->
    false.

-doc """
Select up to 3 representative frames from a filtered stacktrace.

When 3 or fewer frames exist, return all of them. When more than 3,
keep the 2 innermost (error site) plus the outermost (test method)
so the caller's file/line info is preserved rather than being truncated
and replaced by a synthetic frame from build_trace/3.
With `--raw-mfa` (`raw_mfa/0`) every frame is kept.
""".
-spec select_frames(list()) -> list().
select_frames(Frames) ->
    select_frames(Frames, raw_mfa()).

-spec select_frames(list(), boolean()) -> list().
select_frames(Frames, true) ->
    Frames;
select_frames(Frames, false) when length(Frames) =< 3 ->
    Frames;
select_frames([First, Second | _] = Frames, false) ->
    [First, Second, lists:last(Frames)].

-doc "Format a test failure message combining error description and stacktrace.".
-spec format_test_error(atom(), term(), list()) -> binary().
format_test_error(Class, Reason, Stacktrace) ->
    ST = format_stacktrace(Stacktrace),
    <<(error_head(Class, Reason))/binary, ST/binary>>.

-doc "The message of a caught exception, without any stacktrace.".
-spec error_head(atom(), term()) -> binary().
error_head(error, Reason) ->
    decode_beam_error(Reason);
error_head(throw, Reason) ->
    iolist_to_binary([<<"throw: ">>, decode_beam_error(Reason)]);
error_head(exit, Reason) ->
    iolist_to_binary([<<"exit: ">>, decode_beam_error(Reason)]);
%% Erlang only supports error/throw/exit as exception classes, so this
%% clause is never reached in practice — retained for exhaustiveness.
error_head(Class, Reason) ->
    ClassBin = atom_to_binary(Class, utf8),
    ReasonBin = decode_beam_error(Reason),
    <<ClassBin/binary, ": ", ReasonBin/binary>>.

-doc """
Build a failed-test result from the message `Head` (no stacktrace) and the
stack: the message with its `at` lines, plus the structured frames, both
rendered from the single entry list of `build_trace/3` (BT-4402), so
`frames[i]` is the i-th `at` line by construction.
""".
-spec test_failure(atom(), binary(), atom(), list()) ->
    {fail, atom(), binary(), [map()]}.
test_failure(MethodName, Head, Module, Stacktrace) ->
    {Real, Synthetic, NotShown} = build_trace_counted(Module, MethodName, Stacktrace),
    Entries = Real ++ Synthetic,
    Note = hidden_note(NotShown),
    {fail, MethodName, <<Head/binary, (render_at(Entries))/binary, Note/binary>>, [
        entry_frame(E)
     || E <- Entries
    ]}.

-doc """
The trailing line that names the `--raw-mfa` switch when the default output
left frames out: `\n  (N frames hidden; --raw-mfa shows all)`. Empty when nothing
is left out, when `--raw-mfa` is on, and on any node that did not set `raw_mfa`
(the workspace). It does not start with `  at `, so the
i-th `at` line still pairs with `frames[i]`.
""".
-spec hidden_note(non_neg_integer()) -> binary().
hidden_note(NotShown) when NotShown > 0 ->
    %% Only where `--raw-mfa` exists: `beamtalk test` sets `raw_mfa` explicitly
    %% (true or false). A workspace node never sets it, so `Workspace test`,
    %% the MCP `test` tool and the IDE get no flag they cannot pass.
    case application:get_env(beamtalk_stdlib, raw_mfa) of
        {ok, false} ->
            <<"\n  ", (beamtalk_stack_frame:hidden_note(NotShown))/binary>>;
        _ ->
            <<>>
    end;
hidden_note(_) ->
    <<>>.

-doc """
Like `test_failure/4` for failures whose message carries no rendered
stacktrace (the `undef` / does-not-understand text): the only `at` line is the
synthetic test-method one, so the structured frames hold at most that frame.
Keeps `frames[i]` aligned with the i-th `at` line.
""".
-spec test_failure_untraced(atom(), binary(), atom(), list()) ->
    {fail, atom(), binary(), [map()]}.
test_failure_untraced(MethodName, FailMsg, Module, Stacktrace) ->
    {_Real, Synthetic} = build_trace(Module, MethodName, Stacktrace),
    {fail, MethodName, <<FailMsg/binary, (render_at(Synthetic))/binary>>, [
        entry_frame(E)
     || E <- Synthetic
    ]}.

-doc """
Kind of an exception after the `on:do:` normalisation (ADR 0015).

Futures rejected by actor errors and actor gen_server exits are unwrapped first
(`ensure_wrapped/3` only unwraps these for `throw`), then the term is wrapped by
`beamtalk_exception_handler:ensure_wrapped/3`, the canonical classifier.
""".
-spec wrapped_error_kind(atom(), term(), list()) -> atom().
wrapped_error_kind(Class, {future_rejected, Reason}, Stack) ->
    wrapped_error_kind(Class, Reason, Stack);
wrapped_error_kind(Class, {error, Wrapped}, Stack) when is_map(Wrapped) ->
    wrapped_error_kind(Class, Wrapped, Stack);
wrapped_error_kind(Class, Exception, Stack) ->
    case beamtalk_exception_handler:ensure_wrapped(Class, Exception, Stack) of
        #{error := #beamtalk_error{kind = Kind}} -> Kind;
        _ -> error
    end.

-doc """
Legacy error-kind extraction (pre BT-3930), kept as an alias for raw atoms.

Handles #beamtalk_error{} records, wrapped Exception objects (ADR 0015),
and raw Erlang atom errors (e.g., badarith, badarg).
""".
-spec extract_error_kind(term()) -> atom().
extract_error_kind({future_rejected, Reason}) ->
    %% Futures rejected by actor errors wrap the error.
    %% Recursively extract so both #beamtalk_error{} and {error, Map} wrappers
    %% are handled consistently.
    extract_error_kind(Reason);
extract_error_kind({error, Wrapped}) when is_map(Wrapped) ->
    %% Actor gen_server exits wrap errors as {error, #{error => ...}}.
    extract_error_kind(Wrapped);
extract_error_kind(#beamtalk_error{kind = Kind}) ->
    Kind;
extract_error_kind(#{class := 'Exception', error := #beamtalk_error{kind = Kind}}) ->
    % ADR 0015: Exception objects wrap #beamtalk_error{} records
    Kind;
extract_error_kind(#{'$beamtalk_class' := _, error := #beamtalk_error{kind = Kind}}) ->
    % RuntimeError wraps #beamtalk_error{} records
    Kind;
extract_error_kind(Kind) when is_atom(Kind) ->
    % Raw Erlang error atoms (badarith, badarg, function_clause, etc.)
    Kind;
extract_error_kind(_) ->
    % Unknown error format - return generic 'error' kind
    error.

-doc """
Discover test methods from flattened methods map (no gen_server call).
FlatMethods is #{Selector => {DefiningClass, MethodInfo}}; only the keys are
inspected, so any map keyed by selector (e.g. #{Selector => true}) works too.
""".
-spec discover_test_methods(map()) -> [atom()].
discover_test_methods(FlatMethods) ->
    TestMethods = maps:fold(
        fun(Selector, _Info, Acc) ->
            case atom_to_list(Selector) of
                "test" ++ _ -> [Selector | Acc];
                _ -> Acc
            end
        end,
        [],
        FlatMethods
    ),
    lists:sort(TestMethods).

-doc """
Resolve the BEAM module atom for a class name (no gen_server call).
Uses the beamtalk compiler naming convention: bt@snake_case_name.
Falls back to bt@stdlib@snake_case_name for stdlib classes.

The CamelCase→snake_case conversion delegates to
`beamtalk_module_name:camel_to_snake/1`, the single Erlang-side authority
for the `ClassName ⇄ bt@[pkg@]snake_case` convention (ADR 0016).
""".
-spec resolve_module(atom()) -> atom().
resolve_module(ClassName) ->
    SnakeName = beamtalk_module_name:camel_to_snake(atom_to_list(ClassName)),
    % elp:fixme W0023 intentional atom creation
    UserModule = list_to_atom("bt@" ++ SnakeName),
    case code:is_loaded(UserModule) of
        {file, _} ->
            UserModule;
        false ->
            % elp:fixme W0023 intentional atom creation
            StdlibModule = list_to_atom("bt@stdlib@" ++ SnakeName),
            case code:is_loaded(StdlibModule) of
                {file, _} ->
                    StdlibModule;
                false ->
                    %% Try loading (module may be on code path but not yet loaded)
                    case code:ensure_loaded(UserModule) of
                        {module, _} ->
                            UserModule;
                        _ ->
                            case code:ensure_loaded(StdlibModule) of
                                {module, _} -> StdlibModule;
                                % fall back, will error at call site
                                _ -> UserModule
                            end
                    end
            end
    end.

-doc """
Run a single test method with setUp/tearDown lifecycle.

FlatMethods maps the class's own and inherited selectors (see
`class_test_plan/1`); it is used to check for setUp/tearDown.
Creates fresh instance, runs lifecycle, returns pass/fail.
tearDown always runs, even if the test fails.
""".
-spec run_test_method(atom(), atom(), atom(), map()) ->
    {pass, atom()}
    | {fail, atom(), binary()}
    | {fail, atom(), binary(), [map()]}
    | {skip, atom(), binary()}.
run_test_method(ClassName, Module, MethodName, FlatMethods) ->
    run_test_method(ClassName, Module, MethodName, FlatMethods, nil).

-doc """
Run a single test method with setUp/tearDown lifecycle and suite fixture.

Like run_test_method/4 but injects `suiteFixture => SuiteFixture` into the
instance map after setUp, so test methods can access `self.suiteFixture`.
""".
-spec run_test_method(atom(), atom(), atom(), map(), term()) ->
    {pass, atom()}
    | {fail, atom(), binary()}
    | {fail, atom(), binary(), [map()]}
    | {skip, atom(), binary()}.
run_test_method(_ClassName, Module, MethodName, FlatMethods, SuiteFixture) ->
    %% bt-catcher-audit: not-applicable-other-process - runs in the spawned test process or runner
    %% worker, no home entry
    try
        Instance = Module:new(),
        {HasSetUp, HasTearDown} = check_lifecycle_methods(Module, FlatMethods),
        %% Value objects are immutable maps — setUp returns a new instance
        %% with fields set. We must use that return value, not the original Instance.
        %% But only when setUp returns a valid instance map of the same
        %% class. If setUp ends with an untaken conditional (e.g. false ifTrue: [...]),
        %% it returns false/nil rather than self — using that as the receiver would
        %% corrupt all test method dispatches with a DNU error.
        SetUpInstance0 =
            case HasSetUp of
                true ->
                    SetUpResult = Module:dispatch(setUp, [], Instance),
                    case is_valid_setUp_result(Instance, SetUpResult) of
                        true -> SetUpResult;
                        false -> Instance
                    end;
                false ->
                    Instance
            end,
        %% Inject suite fixture so test methods can access self.suiteFixture
        SetUpInstance = inject_suite_fixture(SetUpInstance0, SuiteFixture),
        TestResult =
            %% bt-catcher-audit: not-applicable-other-process - runs in the spawned test process
            %% or runner worker, no home entry
            try
                Module:dispatch(MethodName, [], SetUpInstance),
                {pass, MethodName}
            catch
                throw:{bunit_skip, Reason} ->
                    {skip, MethodName, Reason};
                %% skip: called via FFI — proxy wraps the throw, we detect bunit_skip error kind
                error:#{error := #beamtalk_error{kind = bunit_skip, message = SkipReason}} ->
                    {skip, MethodName, SkipReason};
                %% Bare #beamtalk_error{} clauses: normally errors are wrapped
                %% by beamtalk_error:raise/1 and caught by the TestReason clause
                %% below. These remain as a safety net for any path that raises
                %% an unwrapped record directly.
                error:#beamtalk_error{kind = assertion_failed, message = AssertMsg}:TestST0 ->
                    test_failure(
                        MethodName, beamtalk_text:to_binary(AssertMsg), Module, TestST0
                    );
                error:#beamtalk_error{message = ErrMsg}:TestST1 ->
                    test_failure(MethodName, beamtalk_text:to_binary(ErrMsg), Module, TestST1);
                error:undef:TestST ->
                    #{error := #beamtalk_error{message = FailMsg}} =
                        beamtalk_exception_handler:ensure_wrapped(
                            error, undef, TestST
                        ),
                    %% BT-4173: message is a tagged String — unwrap like the
                    %% sibling clauses (the failure message is bytes).
                    %% The DNU text has no rendered trace, so its structured
                    %% frames must not carry one either (frames[i] <-> "at" line i).
                    test_failure_untraced(
                        MethodName, beamtalk_text:to_binary(FailMsg), Module, TestST
                    );
                error:TestReason:TestST ->
                    test_failure(MethodName, error_head(error, TestReason), Module, TestST);
                TestClass:TestReason:TestST ->
                    test_failure(MethodName, error_head(TestClass, TestReason), Module, TestST)
            after
                case HasTearDown of
                    true ->
                        %% bt-catcher-audit: not-applicable-other-process - runs in the spawned
                        %% test process or runner worker, no home entry
                        try
                            Module:dispatch(tearDown, [], SetUpInstance)
                            % Don't mask the original test failure
                        catch
                            _:_ -> ok
                        end;
                    false ->
                        ok
                end
            end,
        TestResult
    catch
        %% skip: called from setUp — propagate as skip, not failure
        throw:{bunit_skip, SetupSkipReason} ->
            {skip, MethodName, SetupSkipReason};
        %% skip: called from setUp via FFI — proxy wraps the throw, we detect bunit_skip error kind
        error:#{error := #beamtalk_error{kind = bunit_skip, message = SetupFfiSkipReason}} ->
            {skip, MethodName, SetupFfiSkipReason};
        %% setUp or new() itself failed
        error:#beamtalk_error{message = SetupErrMsg} ->
            Prefix = <<"setUp failed: ">>,
            {fail, MethodName, <<Prefix/binary, (beamtalk_text:to_binary(SetupErrMsg))/binary>>};
        error:undef:SetupST ->
            #{error := #beamtalk_error{message = SetupErrMsg}} =
                beamtalk_exception_handler:ensure_wrapped(
                    error, undef, SetupST
                ),
            Prefix = <<"setUp failed: ">>,
            {fail, MethodName, <<Prefix/binary, (beamtalk_text:to_binary(SetupErrMsg))/binary>>};
        SetupClass:SetupReason:SetupST ->
            Inner = format_test_error(SetupClass, SetupReason, SetupST),
            SetupMsg = <<"setUp failed: ", Inner/binary>>,
            {fail, MethodName, SetupMsg}
    end.

-doc "Check the FlatMethods map for setUp/tearDown methods.".
-spec check_lifecycle_methods(atom(), map()) -> {boolean(), boolean()}.
check_lifecycle_methods(_Module, FlatMethods) when is_map(FlatMethods) ->
    {maps:is_key(setUp, FlatMethods), maps:is_key(tearDown, FlatMethods)}.

-doc """
Return true when setUp's result is a valid instance of the same class.

Prevents setUp's accidental non-instance return (e.g. `false` from
an untaken `ifTrue:` branch) from replacing `self` in test method dispatch.
A valid result is a map carrying the same `'$beamtalk_class'` tag as the
original instance — i.e. the normal value-object case where setUp
assignments return an updated copy of self.
""".
-spec is_valid_setUp_result(term(), term()) -> boolean().
is_valid_setUp_result(Instance, Result) when is_map(Instance), is_map(Result) ->
    %% Use maps:find/2 so that two maps both lacking '$beamtalk_class' are NOT
    %% considered equal — the key must actually be present in both.
    case {maps:find('$beamtalk_class', Instance), maps:find('$beamtalk_class', Result)} of
        {{ok, Class}, {ok, Class}} -> true;
        _ -> false
    end;
is_valid_setUp_result(_Instance, _Result) ->
    false.

-doc """
Inject suite fixture into instance map.

Adds `suiteFixture => Value` to the instance so test methods and tearDown
can access it via `self.suiteFixture`. Always injects (even nil) so access
never crashes with badkey.
""".
-spec inject_suite_fixture(map(), term()) -> map().
inject_suite_fixture(Instance, SuiteFixture) when is_map(Instance) ->
    Instance#{suiteFixture => SuiteFixture};
inject_suite_fixture(Instance, _SuiteFixture) ->
    Instance.

-doc """
Run a batch of tests wrapped with setUpOnce/tearDownOnce lifecycle.

1. Creates temp instance, dispatches setUpOnce, captures return as Fixture
2. Calls TestFun(Fixture) which returns [{pass|fail|skip, ...}]
3. In `after`: creates temp instance, injects fixture, dispatches tearDownOnce

If setUpOnce fails, all test methods are failed with "setUpOnce failed: ..." message.
""".
-spec run_suite_lifecycle(
    atom(),
    atom(),
    map(),
    [atom()],
    fun(
        (term()) -> [
            {pass, atom()}
            | {fail, atom(), binary()}
            | {fail, atom(), binary(), [map()]}
            | {skip, atom(), binary()}
        ]
    )
) ->
    [
        {pass, atom()}
        | {fail, atom(), binary()}
        | {fail, atom(), binary(), [map()]}
        | {skip, atom(), binary()}
    ].
run_suite_lifecycle(_ClassName, Module, FlatMethods, TestMethods, TestFun) ->
    {HasSetUpOnce, HasTearDownOnce} = check_suite_lifecycle_methods(Module, FlatMethods),
    case HasSetUpOnce orelse HasTearDownOnce of
        false ->
            %% No suite lifecycle — skip overhead
            TestFun(nil);
        true ->
            %% bt-catcher-audit: not-applicable-other-process - runs in the spawned test process
            %% or runner worker, no home entry
            try
                Fixture =
                    case HasSetUpOnce of
                        true ->
                            TempInstance = Module:new(),
                            Module:dispatch(setUpOnce, [], TempInstance);
                        false ->
                            nil
                    end,
                try
                    TestFun(Fixture)
                after
                    run_teardown_once(Module, Fixture, HasTearDownOnce)
                end
            catch
                Class:Reason:ST ->
                    %% setUpOnce failed — fail ALL tests in this class
                    ErrorMsg =
                        <<"setUpOnce failed: ", (format_test_error(Class, Reason, ST))/binary>>,
                    [{fail, M, ErrorMsg} || M <- TestMethods]
            end
    end.

-doc "Check the FlatMethods map for setUpOnce/tearDownOnce methods.".
-spec check_suite_lifecycle_methods(atom(), map()) -> {boolean(), boolean()}.
check_suite_lifecycle_methods(_Module, FlatMethods) when is_map(FlatMethods) ->
    {maps:is_key(setUpOnce, FlatMethods), maps:is_key(tearDownOnce, FlatMethods)}.

-doc "Run tearDownOnce, swallowing errors to avoid masking test results.".
-spec run_teardown_once(atom(), term(), boolean()) -> ok.
run_teardown_once(Module, Fixture, true) ->
    %% bt-catcher-audit: not-applicable-other-process - runs in the spawned test process or runner
    %% worker, no home entry
    try
        Instance = Module:new(),
        WithFixture = inject_suite_fixture(Instance, Fixture),
        Module:dispatch(tearDownOnce, [], WithFixture)
    catch
        _:_ -> ok
    end,
    ok;
run_teardown_once(_Module, _Fixture, false) ->
    ok.

-doc "Format test results for REPL display.".
-spec format_results(
    [
        {pass, atom()}
        | {fail, atom(), binary()}
        | {fail, atom(), binary(), [map()]}
        | {skip, atom(), binary()}
    ],
    float()
) -> binary().
format_results(Results, Duration) ->
    Total = length(Results),
    Passed = length([ok || {pass, _} <- Results]),
    Skipped = length([ok || {skip, _, _} <- Results]),
    Failed = Total - Passed - Skipped,
    IoList =
        case {Failed, Skipped} of
            {0, 0} ->
                io_lib:format(
                    "~p tests, ~p passed (~.1fs)", [Total, Passed, Duration]
                );
            {0, _} ->
                io_lib:format(
                    "~p tests, ~p passed, ~p skipped (~.1fs)",
                    [Total, Passed, Skipped, Duration]
                );
            {_, 0} ->
                Summary = io_lib:format(
                    "~p tests, ~p passed, ~p failed (~.1fs)",
                    [Total, Passed, Failed, Duration]
                ),
                Failures = [
                    io_lib:format("  FAIL: ~s\n    ~s\n", [M, Msg])
                 || {M, Msg} <- failures(Results)
                ],
                [Summary, "\n\n" | Failures];
            _ ->
                Summary = io_lib:format(
                    "~p tests, ~p passed, ~p skipped, ~p failed (~.1fs)",
                    [Total, Passed, Skipped, Failed, Duration]
                ),
                Failures = [
                    io_lib:format("  FAIL: ~s\n    ~s\n", [M, Msg])
                 || {M, Msg} <- failures(Results)
                ],
                [Summary, "\n\n" | Failures]
        end,
    unicode:characters_to_binary(IoList).

-doc "Failed results as `{Name, Message}`, whether or not they carry frames.".
-spec failures(list()) -> [{atom(), binary()}].
failures(Results) ->
    lists:filtermap(
        fun
            ({fail, M, Msg}) -> {true, {M, Msg}};
            ({fail, M, Msg, _Frames}) -> {true, {M, Msg}};
            (_) -> false
        end,
        Results
    ).

-doc "Convert raw test results to structured map.".
-spec structure_results(
    atom(),
    [
        {pass, atom()}
        | {fail, atom(), binary()}
        | {fail, atom(), binary(), [map()]}
        | {skip, atom(), binary()}
    ],
    float()
) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{
                name := atom(),
                class := atom(),
                status := pass | fail | skip,
                error => binary(),
                frames => [map()],
                reason => binary()
            }
        ]
    }.
structure_results(ClassName, Results, Duration) ->
    Total = length(Results),
    Passed = length([ok || {pass, _} <- Results]),
    Skipped = length([ok || {skip, _, _} <- Results]),
    Failed = Total - Passed - Skipped,
    Tests = lists:map(
        fun
            ({pass, Name}) ->
                #{name => Name, class => ClassName, status => pass};
            ({skip, Name, Reason}) ->
                #{name => Name, class => ClassName, status => skip, reason => Reason};
            ({fail, Name, Msg}) ->
                #{name => Name, class => ClassName, status => fail, error => Msg};
            ({fail, Name, Msg, Frames}) ->
                #{name => Name, class => ClassName, status => fail, error => Msg, frames => Frames}
        end,
        Results
    ),
    #{
        class => ClassName,
        total => Total,
        passed => Passed,
        failed => Failed,
        skipped => Skipped,
        duration => Duration,
        tests => Tests
    }.

-doc """
Spawn test execution in a separate process to avoid gen_server deadlock.

Test execution calls back into the class system,
so it must run outside the class process.
""".
-spec spawn_test_execution(atom(), list(), atom(), atom(), map(), {pid(), term()}) -> pid().
spawn_test_execution(Selector, Args, ClassName, TestModule, FlatMethods, From) ->
    spawn(fun() ->
        try
            %% FlatMethods holds only the class's own instance methods; add the
            %% inherited ones (BT-3768). Safe here: this is not the class process.
            AllMethods = maps:merge(
                maps:from_keys(test_class_selectors(ClassName), true), FlatMethods
            ),
            Result = execute_tests(Selector, Args, ClassName, TestModule, AllMethods),
            gen_server:reply(From, {ok, ffi_string(Result)})
        catch
            C:E:ST ->
                Error =
                    case E of
                        #beamtalk_error{} ->
                            E;
                        _ ->
                            Err0 = beamtalk_error:new(test_execution_failed, ClassName),
                            Err1 = beamtalk_error:with_selector(Err0, Selector),
                            %% BT-4236: keep the raw crash term for `Exception>>reason`.
                            beamtalk_error:with_details(Err1, #{reason => E})
                    end,
                ?LOG_ERROR(
                    "Test execution ~p:~p failed: ~p:~p",
                    [ClassName, Selector, C, E],
                    #{
                        class => ClassName,
                        selector => Selector,
                        error_class => C,
                        error => E,
                        stacktrace => ST,
                        domain => [beamtalk, stdlib]
                    }
                ),
                gen_server:reply(From, {error, Error})
        end
    end).

%%====================================================================
%% FFI shims — (Erlang beamtalk_test_case) dispatch
%%====================================================================

%% should:raise: → should/2
should(Block, ErrorKind) -> should_raise(Block, ErrorKind).

%% runAll: → runAll/1 (ClassSelf tuple → extract class name → run_all/1)
runAll(ClassRef) -> ffi_string(run_all(ffi_extract_class_name(ClassRef))).

%% runClass:withTest: → runClass/2 (ClassSelf tuple + test name → run_single/2)
runClass(ClassRef, TestName) ->
    ffi_string(run_single(ffi_extract_class_name(ClassRef), TestName)).

%% The Beamtalk methods declare `-> String` (BT-4308), and a String is the
%% tagged `{beamtalk_string, Bin}` at run time, not a raw binary.
ffi_string(Bin) when is_binary(Bin) -> {beamtalk_string, Bin}.

%% skipTest: → skipTest/1
%%
%% Uses error (not throw) so it survives beamtalk_erlang_proxy dispatch,
%% which catches all throws and wraps them as erlang_throw errors.
%% run_test_method/4 detects bunit_skip errors and counts them as skips.
skipTest(Reason) when is_binary(Reason) ->
    Error0 = beamtalk_error:new(bunit_skip, 'TestCase'),
    beamtalk_error:raise(beamtalk_error:with_message(Error0, Reason));
skipTest(Reason) when is_atom(Reason) ->
    skipTest(atom_to_binary(Reason, utf8));
skipTest(Reason) ->
    skipTest(beamtalk_primitive:print_string(Reason)).

%% suiteFixture: → suiteFixture/1
%%
%% Returns the suite fixture from the instance map, or nil if not present.
%% The BUnit runner injects `suiteFixture => Value` into the instance map
%% before test methods run, so this returns the setUpOnce return value.
suiteFixture(Self) when is_map(Self) ->
    maps:get(suiteFixture, Self, nil);
suiteFixture(_Self) ->
    nil.

%% Extract the bare class name atom from a class reference tuple.
%% Element 2 of the tuple is 'ClassName class'; strip trailing " class" (6 chars).
%% Uses list_to_existing_atom/1 to avoid leaking atoms from malformed input.
-spec ffi_extract_class_name(tuple()) -> atom().
ffi_extract_class_name(ClassRef) when is_tuple(ClassRef) ->
    Tag = element(2, ClassRef),
    TagStr = atom_to_list(Tag),
    Len = length(TagStr),
    NameStr = lists:sublist(TagStr, Len - 6),
    try
        list_to_existing_atom(NameStr)
    catch
        error:badarg ->
            Err0 = beamtalk_error:new(does_not_understand, 'TestCase'),
            Msg = beamtalk_error:chars_to_binary(
                io_lib:format("Unknown TestCase class: ~ts", [NameStr])
            ),
            beamtalk_error:raise(beamtalk_error:with_message(Err0, Msg))
    end.
