%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TestCase primitive implementations for BUnit test framework.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% This module provides the runtime support for TestCase assertion methods.
%%% All assertions create structured #beamtalk_error{} records with kind
%%% `assertion_failed` when they fail.
%%%
%%% Part of ADR 0014: BUnit — Beamtalk Test Framework (Phase 2).

-module(beamtalk_test_case).
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
    spawn_test_execution/6,
    %% BT-762: Exported for beamtalk_test_runner
    run_test_method/4,
    run_test_method/5,
    run_suite_lifecycle/5,
    structure_results/3,
    resolve_module/1,
    %% BT-1293: Exported for testing
    is_valid_setUp_result/2
]).

%% FFI shims for (Erlang beamtalk_test_case) dispatch
-export([should/2, runAll/1, runClass/2, skipTest/1, suiteFixture/1]).

%% @doc Assert that a block raises an error of the specified kind.
%%
%% Executes the block and verifies that it raises an error with the
%% given kind atom. If the block completes normally or raises a different
%% kind of error, the assertion fails.
%%
%% Example:
%%   Block = fun() -> error(my_error) end,
%%   should_raise(Block, my_error)  % => passes
%%
%% Note: Block is a zero-argument Erlang fun in Core Erlang codegen.
-spec should_raise(fun(() -> term()), atom()) -> 'nil'.
should_raise(Block, ExpectedKind) when is_function(Block, 0), is_atom(ExpectedKind) ->
    try Block() of
        _ ->
            % Block completed without error
            NoRaiseErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
            NoRaiseErr1 = beamtalk_error:with_selector(NoRaiseErr0, 'should:raise:'),
            NoRaiseMsg = iolist_to_binary(
                io_lib:format("Expected block to raise ~s but it completed normally", [ExpectedKind])
            ),
            NoRaiseErr2 = beamtalk_error:with_message(NoRaiseErr1, NoRaiseMsg),
            NoRaiseErr3 = beamtalk_error:with_details(NoRaiseErr2, #{
                expected_kind => ExpectedKind, actual => completed
            }),
            beamtalk_error:raise(NoRaiseErr3)
    catch
        _:Exception ->
            ActualKind = extract_error_kind(Exception),
            case ActualKind of
                ExpectedKind ->
                    nil;
                _ ->
                    MismatchErr0 = beamtalk_error:new(assertion_failed, 'TestCase'),
                    MismatchErr1 = beamtalk_error:with_selector(MismatchErr0, 'should:raise:'),
                    MismatchMsg = iolist_to_binary(
                        io_lib:format("Expected error kind ~s but got ~s", [
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
                iolist_to_binary(io_lib:format("Expected a zero-argument block, got: ~p", [Block]));
            {_, false} ->
                iolist_to_binary(
                    io_lib:format("Expected an error kind atom, got: ~p", [ExpectedKind])
                )
        end,
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2).

%% @doc Unconditionally fail with a message.
%%
%% Always raises an assertion_failed error with the given message.
%% Used when a test detects an invariant violation.
%%
%% Example:
%%   fail(<<"This should not happen">>)
-spec fail(binary() | atom()) -> no_return().
fail(Message) when is_binary(Message) ->
    Error0 = beamtalk_error:new(assertion_failed, 'TestCase'),
    Error1 = beamtalk_error:with_selector(Error0, 'fail:'),
    Error2 = beamtalk_error:with_message(Error1, Message),
    beamtalk_error:raise(Error2);
fail(Message) when is_atom(Message) ->
    fail(atom_to_binary(Message, utf8));
fail(Message) ->
    MessageBin = iolist_to_binary(io_lib:format("~p", [Message])),
    fail(MessageBin).

%% @doc Signal that the current test should be skipped.
%%
%% Throws {bunit_skip, Reason}, caught by run_test_method/4 as a {skip, ...}
%% result. A skipped test is neither a pass nor a failure.
%%
%% Example:
%%   skip(<<"Unix only">>)
-spec skip(binary() | atom()) -> no_return().
skip(Reason) when is_binary(Reason) ->
    throw({bunit_skip, Reason});
skip(Reason) when is_atom(Reason) ->
    throw({bunit_skip, atom_to_binary(Reason, utf8)});
skip(Reason) ->
    throw({bunit_skip, iolist_to_binary(io_lib:format("~p", [Reason]))}).

%% @doc Execute tests from class-side dispatch (BT-440).
%%
%% Called from beamtalk_object_class handle_call via spawned process.
%% Receives Selector (runAll | 'run:'), Args, ClassName, Module, and
%% FlatMethods directly from gen_server state — no gen_server calls needed.
-spec execute_tests(atom(), list(), atom(), atom(), map()) -> binary().
execute_tests(runAll, _Args, ClassName, Module, FlatMethods) ->
    TestMethods = discover_test_methods(FlatMethods),
    case TestMethods of
        [] ->
            iolist_to_binary(
                io_lib:format(
                    "No test methods found in ~s", [ClassName]
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
                    lists:map(
                        fun(Method) ->
                            run_test_method(ClassName, Module, Method, FlatMethods, SuiteFixture)
                        end,
                        TestMethods
                    )
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end;
execute_tests('run:', [TestMethodName], ClassName, Module, FlatMethods) ->
    case maps:is_key(TestMethodName, FlatMethods) of
        false ->
            iolist_to_binary(
                io_lib:format(
                    "Method '~s' not found in ~s", [TestMethodName, ClassName]
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

%% @doc Run all test methods (BT-440 — BIF fallback path).
%% WARNING: May deadlock if called from within a class gen_server handle_call.
%% Prefer execute_tests/5 which receives gen_server state directly and avoids
%% any gen_server:call back to the class process.
-spec run_all(atom()) -> binary().
run_all(ClassName) ->
    Module = resolve_module(ClassName),
    TestMethods = discover_test_methods_from_module(Module),
    case TestMethods of
        [] ->
            iolist_to_binary(
                io_lib:format(
                    "No test methods found in ~s", [ClassName]
                )
            );
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = run_suite_lifecycle(
                ClassName,
                Module,
                none,
                TestMethods,
                fun(SuiteFixture) ->
                    lists:map(
                        fun(Method) ->
                            run_test_method(ClassName, Module, Method, none, SuiteFixture)
                        end,
                        TestMethods
                    )
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            format_results(Results, Duration)
    end.

%% @doc Run a single test method (BT-440 — BIF fallback path).
%% WARNING: May deadlock if called from within a class gen_server handle_call.
-spec run_single(atom(), atom()) -> binary().
run_single(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    Module = resolve_module(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Results = run_suite_lifecycle(
        ClassName,
        Module,
        none,
        [TestMethodName],
        fun(SuiteFixture) ->
            [run_test_method(ClassName, Module, TestMethodName, none, SuiteFixture)]
        end
    ),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    format_results(Results, Duration).

%% @doc Run all tests for a class and return structured results (BT-699).
%%
%% Returns a map with per-test results suitable for JSON encoding.
%% Uses the BIF fallback path (safe outside gen_server).
-spec run_all_structured(atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{name := atom(), status := pass | fail | skip, error => binary(), reason => binary()}
        ]
    }.
run_all_structured(ClassName) ->
    Module = resolve_module(ClassName),
    TestMethods = discover_test_methods_from_module(Module),
    case TestMethods of
        [] ->
            #{
                class => ClassName,
                total => 0,
                passed => 0,
                failed => 0,
                skipped => 0,
                duration => 0.0,
                tests => []
            };
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            Results = run_suite_lifecycle(
                ClassName,
                Module,
                none,
                TestMethods,
                fun(SuiteFixture) ->
                    lists:map(
                        fun(Method) ->
                            run_test_method(ClassName, Module, Method, none, SuiteFixture)
                        end,
                        TestMethods
                    )
                end
            ),
            EndTime = erlang:monotonic_time(millisecond),
            Duration = (EndTime - StartTime) / 1000.0,
            structure_results(ClassName, Results, Duration)
    end.

%% @doc Run a single test method and return structured results (BT-699).
-spec run_single_structured(atom(), atom()) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{name := atom(), status := pass | fail | skip, error => binary(), reason => binary()}
        ]
    }.
run_single_structured(ClassName, TestMethodName) when is_atom(TestMethodName) ->
    Module = resolve_module(ClassName),
    StartTime = erlang:monotonic_time(millisecond),
    Results = run_suite_lifecycle(
        ClassName,
        Module,
        none,
        [TestMethodName],
        fun(SuiteFixture) ->
            [run_test_method(ClassName, Module, TestMethodName, none, SuiteFixture)]
        end
    ),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,
    structure_results(ClassName, Results, Duration).

%% @doc Find all loaded TestCase subclasses (BT-699).
%%
%% Uses the class hierarchy ETS table to find all classes that inherit
%% from TestCase. Returns class names as atoms.
-spec find_test_classes() -> [atom()].
find_test_classes() ->
    beamtalk_class_registry:all_subclasses('TestCase').

%%% Internal helpers

%% @doc Decode a BEAM error term into a human-readable binary message.
%%
%% Translates common Erlang error shapes and Beamtalk wrappers into
%% readable messages instead of raw ~p formatting.
-spec decode_beam_error(term()) -> binary().
decode_beam_error({future_rejected, Inner}) ->
    iolist_to_binary([<<"future rejected: ">>, decode_beam_error(Inner)]);
decode_beam_error({error, Map}) when is_map(Map) ->
    case maps:find(error, Map) of
        {ok, Inner} -> decode_beam_error(Inner);
        _ -> iolist_to_binary(io_lib:format("~p", [Map]))
    end;
decode_beam_error(#beamtalk_error{message = Msg}) ->
    Msg;
decode_beam_error(#{class := 'Exception', error := #beamtalk_error{message = Msg}}) ->
    Msg;
decode_beam_error(#{'$beamtalk_class' := _, error := #beamtalk_error{message = Msg}}) ->
    Msg;
decode_beam_error({badkey, Key}) ->
    iolist_to_binary(io_lib:format("key ~p not found in dictionary", [Key]));
decode_beam_error({badmap, Map}) ->
    iolist_to_binary(io_lib:format("~p is not a map", [Map]));
decode_beam_error({badmatch, Value}) ->
    iolist_to_binary(io_lib:format("no match of right-hand side value ~p", [Value]));
decode_beam_error({case_clause, Value}) ->
    iolist_to_binary(io_lib:format("no case clause matched ~p", [Value]));
decode_beam_error({try_clause, Value}) ->
    iolist_to_binary(io_lib:format("no try clause matched ~p", [Value]));
decode_beam_error(function_clause) ->
    <<"no matching function clause">>;
decode_beam_error(if_clause) ->
    <<"no true branch in conditional">>;
decode_beam_error({badarity, {Fun, Args}}) when is_function(Fun), is_list(Args) ->
    {arity, Expected} = erlang:fun_info(Fun, arity),
    iolist_to_binary(
        io_lib:format("function expects ~p arguments but was called with ~p", [
            Expected, length(Args)
        ])
    );
decode_beam_error({badfun, Term}) ->
    iolist_to_binary(io_lib:format("~p is not a function", [Term]));
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
    iolist_to_binary(io_lib:format("~p", [Other])).

%% @doc Format a BEAM stacktrace for display in test failure messages.
%%
%% Filters out internal test runner and OTP frames; shows up to 3 user frames.
%% Caller frames are shown first (most useful for jumping to the failing line),
%% with assertion/library frames below.
%% Returns an empty binary when no relevant frames exist.
-spec format_stacktrace(list()) -> binary().
format_stacktrace([]) ->
    <<>>;
format_stacktrace(Frames) ->
    Relevant = lists:sublist(filter_stackframes(Frames), 3),
    case Relevant of
        [] ->
            <<>>;
        _ ->
            %% Reverse so caller (test method) appears first, assertion impl last
            Lines = [format_stackframe(F) || F <- lists:reverse(Relevant)],
            iolist_to_binary(["\n  at " | lists:join("\n  at ", Lines)])
    end.

%% @doc Format a single stacktrace frame for display using Beamtalk names.
%%
%% Translates Erlang module names to Beamtalk class names where possible.
%% Stdlib frames show just the basename (the absolute path baked at compile time
%% won't exist on the user's machine). User code frames show workspace-relative
%% paths for terminal clickability.
-spec format_stackframe({atom(), atom(), non_neg_integer() | [term()], list()}) -> iolist().
format_stackframe({Mod, Fun, ArityOrArgs, Loc}) ->
    DisplayMod = format_module(Mod),
    DisplayFun = format_fun_name(Fun),
    %% arity not displayed — Beamtalk selectors encode it
    _ = ArityOrArgs,
    File = proplists:get_value(file, Loc, ""),
    Line = proplists:get_value(line, Loc, 0),
    case {File, Line} of
        {"", 0} ->
            io_lib:format("~s>>~s", [DisplayMod, DisplayFun]);
        _ ->
            DisplayFile = format_file_path(Mod, File),
            io_lib:format("~s>>~s (~s:~p)", [
                DisplayMod, DisplayFun, DisplayFile, Line
            ])
    end.

%% @doc Format a file path for display based on module origin.
%%
%% Stdlib modules (bt@stdlib@*) use basename only — the compile-time absolute
%% path won't exist on the user's machine. User code paths are made relative
%% to CWD for terminal clickability in VS Code.
-spec format_file_path(atom(), string()) -> string().
format_file_path(Mod, File) ->
    ModStr = atom_to_list(Mod),
    case lists:prefix("bt@stdlib@", ModStr) of
        true ->
            filename:basename(File);
        false ->
            make_relative(File)
    end.

%% @doc Make a file path relative to CWD if it's under it, otherwise basename.
-spec make_relative(string()) -> string().
make_relative(File) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            CwdSlash = Cwd ++ "/",
            case lists:prefix(CwdSlash, File) of
                true ->
                    lists:nthtail(length(CwdSlash), File);
                false ->
                    filename:basename(File)
            end;
        _ ->
            filename:basename(File)
    end.

%% @doc Translate an Erlang module name to a Beamtalk class name for display.
-spec format_module(atom()) -> string().
format_module(Mod) ->
    case beamtalk_stack_frame:module_to_class(Mod) of
        nil -> atom_to_list(Mod);
        ClassName -> atom_to_list(ClassName)
    end.

%% @doc Clean up Erlang function names for display.
%%
%% Strips anonymous fun wrappers (e.g., '-sync_send/3-fun-1-' → 'sync_send')
%% and returns the base function name as a string.
-spec format_fun_name(atom()) -> string().
format_fun_name(Fun) ->
    FunStr = atom_to_list(Fun),
    case FunStr of
        [$- | Rest] ->
            %% Anonymous fun: '-base_name/N-fun-M-' → base_name
            [BaseName | _] = string:split(Rest, "/"),
            BaseName;
        _ ->
            FunStr
    end.

%% @doc Filter stacktrace frames, removing internal test runner and OTP frames.
-spec filter_stackframes(list()) -> list().
filter_stackframes(Frames) ->
    lists:filter(
        fun
            ({Mod, _Fun, _Arity, _Loc}) ->
                %% Only show Beamtalk user and stdlib code frames (bt@* modules).
                %% Internal runtime, OTP, and test infrastructure frames are noise —
                %% the error message from #beamtalk_error{} already carries the
                %% class, selector, and details.
                ModStr = atom_to_list(Mod),
                lists:prefix("bt@", ModStr);
            (_) ->
                false
        end,
        Frames
    ).

%% @doc Format a test failure message combining error description and stacktrace.
-spec format_test_error(atom(), term(), list()) -> binary().
format_test_error(error, Reason, Stacktrace) ->
    Msg = decode_beam_error(Reason),
    ST = format_stacktrace(Stacktrace),
    <<Msg/binary, ST/binary>>;
format_test_error(throw, Reason, Stacktrace) ->
    Msg = iolist_to_binary([<<"throw: ">>, decode_beam_error(Reason)]),
    ST = format_stacktrace(Stacktrace),
    <<Msg/binary, ST/binary>>;
format_test_error(exit, Reason, Stacktrace) ->
    Msg = iolist_to_binary([<<"exit: ">>, decode_beam_error(Reason)]),
    ST = format_stacktrace(Stacktrace),
    <<Msg/binary, ST/binary>>;
%% Erlang only supports error/throw/exit as exception classes, so this
%% clause is never reached in practice — retained for exhaustiveness.
format_test_error(Class, Reason, Stacktrace) ->
    ClassBin = atom_to_binary(Class, utf8),
    ReasonBin = decode_beam_error(Reason),
    Msg = <<ClassBin/binary, ": ", ReasonBin/binary>>,
    ST = format_stacktrace(Stacktrace),
    <<Msg/binary, ST/binary>>.

%% @doc Ensure the test method location appears in the failure message.
%%
%% When the BEAM stacktrace is truncated (default depth is 8), the test method
%% frame can be lost. This function checks whether the test module appears in
%% the filtered stacktrace; if not, it prepends a synthetic frame so the user
%% always sees where the failing test is defined.
-spec ensure_test_context(binary(), atom(), atom(), list()) -> binary().
ensure_test_context(FailMsg, Module, MethodName, Stacktrace) ->
    Filtered = filter_stackframes(Stacktrace),
    HasTestFrame = lists:any(
        fun({Mod, _Fun, _Arity, _Loc}) -> Mod =:= Module end,
        Filtered
    ),
    case HasTestFrame of
        true ->
            FailMsg;
        false ->
            ClassName = format_module(Module),
            MethodStr = atom_to_list(MethodName),
            Context = iolist_to_binary(
                io_lib:format("\n  at ~s>>~s", [ClassName, MethodStr])
            ),
            inject_test_context(FailMsg, Context)
    end.

%% @doc Insert the test context line after the error message, before any stacktrace.
%%
%% If FailMsg already contains "\n  at " lines, the context is inserted
%% before them (so it appears first). Otherwise it is appended.
-spec inject_test_context(binary(), binary()) -> binary().
inject_test_context(FailMsg, Context) ->
    case binary:match(FailMsg, <<"\n  at ">>) of
        {Pos, _Len} ->
            Head = binary:part(FailMsg, 0, Pos),
            Tail = binary:part(FailMsg, Pos, byte_size(FailMsg) - Pos),
            <<Head/binary, Context/binary, Tail/binary>>;
        nomatch ->
            <<FailMsg/binary, Context/binary>>
    end.

%% @doc Extract the error kind from an exception.
%%
%% Handles #beamtalk_error{} records, wrapped Exception objects (ADR 0015),
%% and raw Erlang atom errors (e.g., badarith, badarg).
-spec extract_error_kind(term()) -> atom().
extract_error_kind({future_rejected, Reason}) ->
    %% BT-838: Futures rejected by actor errors wrap the error.
    %% Recursively extract so both #beamtalk_error{} and {error, Map} wrappers
    %% are handled consistently.
    extract_error_kind(Reason);
extract_error_kind({error, Wrapped}) when is_map(Wrapped) ->
    %% BT-838: Actor gen_server exits wrap errors as {error, #{error => ...}}.
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

%% @doc Discover test methods from flattened methods map (no gen_server call).
%% FlatMethods is #{Selector => {DefiningClass, MethodInfo}}.
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

%% @doc Discover test methods from module exports (BIF fallback path).
-spec discover_test_methods_from_module(atom()) -> [atom()].
discover_test_methods_from_module(Module) ->
    Exports = Module:module_info(exports),
    TestMethods = lists:filtermap(
        fun({FunName, _Arity}) ->
            case atom_to_list(FunName) of
                "test" ++ _ -> {true, FunName};
                _ -> false
            end
        end,
        Exports
    ),
    lists:sort(TestMethods).

%% @doc Resolve the BEAM module atom for a class name (no gen_server call).
%% Uses the beamtalk compiler naming convention: bt@snake_case_name.
%% Falls back to bt@stdlib@snake_case_name for stdlib classes.
-spec resolve_module(atom()) -> atom().
resolve_module(ClassName) ->
    SnakeName = class_name_to_snake(atom_to_list(ClassName)),
    UserModule = list_to_atom("bt@" ++ SnakeName),
    case code:is_loaded(UserModule) of
        {file, _} ->
            UserModule;
        false ->
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

%% @doc Convert CamelCase class name to snake_case module name component.
%% Matches the Rust to_module_name() convention.
-spec class_name_to_snake(string()) -> string().
class_name_to_snake(Name) ->
    class_name_to_snake(Name, false, []).

class_name_to_snake([], _PrevLower, Acc) ->
    lists:reverse(Acc);
class_name_to_snake([H | T], PrevLower, Acc) when H >= $A, H =< $Z ->
    % ASCII uppercase to lowercase
    Lower = H + 32,
    case PrevLower of
        true -> class_name_to_snake(T, false, [Lower, $_ | Acc]);
        false -> class_name_to_snake(T, false, [Lower | Acc])
    end;
class_name_to_snake([H | T], _PrevLower, Acc) ->
    class_name_to_snake(T, H >= $a andalso H =< $z, [H | Acc]).

%% @doc Run a single test method with setUp/tearDown lifecycle (BT-440).
%%
%% FlatMethods is either a map (from gen_server state) or 'none' (BIF fallback).
%% When 'none', uses module_info(exports) to check for setUp/tearDown.
%% Creates fresh instance, runs lifecycle, returns pass/fail.
%% tearDown always runs, even if the test fails.
-spec run_test_method(atom(), atom(), atom(), map() | none) ->
    {pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}.
run_test_method(ClassName, Module, MethodName, FlatMethods) ->
    run_test_method(ClassName, Module, MethodName, FlatMethods, nil).

%% @doc Run a single test method with setUp/tearDown lifecycle and suite fixture (BT-1549).
%%
%% Like run_test_method/4 but injects `suiteFixture => SuiteFixture` into the
%% instance map after setUp, so test methods can access `self.suiteFixture`.
-spec run_test_method(atom(), atom(), atom(), map() | none, term()) ->
    {pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}.
run_test_method(_ClassName, Module, MethodName, FlatMethods, SuiteFixture) ->
    try
        Instance = Module:new(),
        {HasSetUp, HasTearDown} = check_lifecycle_methods(Module, FlatMethods),
        %% BT-900: Value objects are immutable maps — setUp returns a new instance
        %% with fields set. We must use that return value, not the original Instance.
        %% BT-1293: But only when setUp returns a valid instance map of the same
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
        %% BT-1549: Inject suite fixture so test methods can access self.suiteFixture
        SetUpInstance = inject_suite_fixture(SetUpInstance0, SuiteFixture),
        TestResult =
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
                    ST0 = format_stacktrace(TestST0),
                    FailMsg0 = <<AssertMsg/binary, ST0/binary>>,
                    {fail, MethodName, ensure_test_context(FailMsg0, Module, MethodName, TestST0)};
                error:#beamtalk_error{message = ErrMsg}:TestST1 ->
                    ST1 = format_stacktrace(TestST1),
                    FailMsg1 = <<ErrMsg/binary, ST1/binary>>,
                    {fail, MethodName, ensure_test_context(FailMsg1, Module, MethodName, TestST1)};
                error:undef:TestST ->
                    #{error := #beamtalk_error{message = FailMsg}} =
                        beamtalk_exception_handler:ensure_wrapped(
                            error, undef, TestST
                        ),
                    {fail, MethodName, ensure_test_context(FailMsg, Module, MethodName, TestST)};
                error:TestReason:TestST ->
                    FailMsg = format_test_error(error, TestReason, TestST),
                    {fail, MethodName, ensure_test_context(FailMsg, Module, MethodName, TestST)};
                TestClass:TestReason:TestST ->
                    FailMsg = format_test_error(TestClass, TestReason, TestST),
                    {fail, MethodName, ensure_test_context(FailMsg, Module, MethodName, TestST)}
            after
                case HasTearDown of
                    true ->
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
            {fail, MethodName, <<Prefix/binary, SetupErrMsg/binary>>};
        error:undef:SetupST ->
            #{error := #beamtalk_error{message = SetupErrMsg}} =
                beamtalk_exception_handler:ensure_wrapped(
                    error, undef, SetupST
                ),
            Prefix = <<"setUp failed: ">>,
            {fail, MethodName, <<Prefix/binary, SetupErrMsg/binary>>};
        SetupClass:SetupReason:SetupST ->
            Inner = format_test_error(SetupClass, SetupReason, SetupST),
            SetupMsg = <<"setUp failed: ", Inner/binary>>,
            {fail, MethodName, SetupMsg}
    end.

%% @doc Check for setUp/tearDown methods, using FlatMethods map or module exports.
-spec check_lifecycle_methods(atom(), map() | none) -> {boolean(), boolean()}.
check_lifecycle_methods(_Module, FlatMethods) when is_map(FlatMethods) ->
    {maps:is_key(setUp, FlatMethods), maps:is_key(tearDown, FlatMethods)};
check_lifecycle_methods(Module, none) ->
    Exports = Module:module_info(exports),
    {lists:keymember(setUp, 1, Exports), lists:keymember(tearDown, 1, Exports)}.

%% @doc Return true when setUp's result is a valid instance of the same class.
%%
%% BT-1293: Prevents setUp's accidental non-instance return (e.g. `false` from
%% an untaken `ifTrue:` branch) from replacing `self` in test method dispatch.
%% A valid result is a map carrying the same `'$beamtalk_class'` tag as the
%% original instance — i.e. the normal BT-900 value-object case where setUp
%% assignments return an updated copy of self.
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

%% @doc Inject suite fixture into instance map (BT-1549).
%%
%% Adds `suiteFixture => Value` to the instance so test methods and tearDown
%% can access it via `self.suiteFixture`. Always injects (even nil) so access
%% never crashes with badkey.
-spec inject_suite_fixture(map(), term()) -> map().
inject_suite_fixture(Instance, SuiteFixture) when is_map(Instance) ->
    Instance#{suiteFixture => SuiteFixture};
inject_suite_fixture(Instance, _SuiteFixture) ->
    Instance.

%% @doc Run a batch of tests wrapped with setUpOnce/tearDownOnce lifecycle (BT-1549).
%%
%% 1. Creates temp instance, dispatches setUpOnce, captures return as Fixture
%% 2. Calls TestFun(Fixture) which returns [{pass|fail|skip, ...}]
%% 3. In `after`: creates temp instance, injects fixture, dispatches tearDownOnce
%%
%% If setUpOnce fails, all test methods are failed with "setUpOnce failed: ..." message.
-spec run_suite_lifecycle(
    atom(),
    atom(),
    map() | none,
    [atom()],
    fun((term()) -> [{pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}])
) ->
    [{pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}].
run_suite_lifecycle(_ClassName, Module, FlatMethods, TestMethods, TestFun) ->
    {HasSetUpOnce, HasTearDownOnce} = check_suite_lifecycle_methods(Module, FlatMethods),
    case HasSetUpOnce orelse HasTearDownOnce of
        false ->
            %% No suite lifecycle — skip overhead
            TestFun(nil);
        true ->
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

%% @doc Check for setUpOnce/tearDownOnce methods (BT-1549).
-spec check_suite_lifecycle_methods(atom(), map() | none) -> {boolean(), boolean()}.
check_suite_lifecycle_methods(_Module, FlatMethods) when is_map(FlatMethods) ->
    {maps:is_key(setUpOnce, FlatMethods), maps:is_key(tearDownOnce, FlatMethods)};
check_suite_lifecycle_methods(Module, none) ->
    Exports = Module:module_info(exports),
    {lists:keymember(setUpOnce, 1, Exports), lists:keymember(tearDownOnce, 1, Exports)}.

%% @doc Run tearDownOnce, swallowing errors to avoid masking test results (BT-1549).
-spec run_teardown_once(atom(), term(), boolean()) -> ok.
run_teardown_once(Module, Fixture, true) ->
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

%% @doc Format test results for REPL display.
-spec format_results(
    [{pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}], float()
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
                 || {fail, M, Msg} <- Results
                ],
                [Summary, "\n\n" | Failures];
            _ ->
                Summary = io_lib:format(
                    "~p tests, ~p passed, ~p skipped, ~p failed (~.1fs)",
                    [Total, Passed, Skipped, Failed, Duration]
                ),
                Failures = [
                    io_lib:format("  FAIL: ~s\n    ~s\n", [M, Msg])
                 || {fail, M, Msg} <- Results
                ],
                [Summary, "\n\n" | Failures]
        end,
    unicode:characters_to_binary(IoList).

%% @doc Convert raw test results to structured map (BT-699).
-spec structure_results(
    atom(), [{pass, atom()} | {fail, atom(), binary()} | {skip, atom(), binary()}], float()
) ->
    #{
        class := atom(),
        total := non_neg_integer(),
        passed := non_neg_integer(),
        failed := non_neg_integer(),
        skipped := non_neg_integer(),
        duration := float(),
        tests := [
            #{name := atom(), status := pass | fail | skip, error => binary(), reason => binary()}
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
                #{name => Name, status => pass};
            ({skip, Name, Reason}) ->
                #{name => Name, status => skip, reason => Reason};
            ({fail, Name, Msg}) ->
                #{name => Name, status => fail, error => Msg}
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

%% @doc Spawn test execution in a separate process to avoid gen_server deadlock.
%%
%% BT-440/BT-704: Test execution calls back into the class system,
%% so it must run outside the class process.
-spec spawn_test_execution(atom(), list(), atom(), atom(), map(), {pid(), term()}) -> pid().
spawn_test_execution(Selector, Args, ClassName, TestModule, FlatMethods, From) ->
    spawn(fun() ->
        try
            Result = execute_tests(Selector, Args, ClassName, TestModule, FlatMethods),
            gen_server:reply(From, {ok, Result})
        catch
            C:E:ST ->
                Error =
                    case E of
                        #beamtalk_error{} ->
                            E;
                        _ ->
                            Err0 = beamtalk_error:new(test_execution_failed, ClassName),
                            beamtalk_error:with_selector(Err0, Selector)
                    end,
                ?LOG_ERROR(
                    "Test execution ~p:~p failed: ~p:~p",
                    [ClassName, Selector, C, E],
                    #{
                        class => ClassName,
                        selector => Selector,
                        error_class => C,
                        error => E,
                        stacktrace => ST
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
runAll(ClassRef) -> run_all(ffi_extract_class_name(ClassRef)).

%% runClass:withTest: → runClass/2 (ClassSelf tuple + test name → run_single/2)
runClass(ClassRef, TestName) -> run_single(ffi_extract_class_name(ClassRef), TestName).

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
    skipTest(iolist_to_binary(io_lib:format("~p", [Reason]))).

%% suiteFixture: → suiteFixture/1 (BT-1549)
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
            Msg = iolist_to_binary(io_lib:format("Unknown TestCase class: ~s", [NameStr])),
            beamtalk_error:raise(beamtalk_error:with_message(Err0, Msg))
    end.
