%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_exception_handler module.
%%%
%%% Tests exception wrapping, ensure_wrapped/1, and exception map patterns (ADR 0015).

-module(beamtalk_exception_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% Tests for ensure_wrapped/1 (ADR 0015) — pure, no class system needed

%% Already-wrapped Exception maps pass through unchanged
ensure_wrapped_idempotent_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    Result = beamtalk_exception_handler:ensure_wrapped(Wrapped),
    ?assertEqual(Wrapped, Result).

%% Raw beamtalk_error records get wrapped
ensure_wrapped_wraps_raw_error_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Result = beamtalk_exception_handler:ensure_wrapped(Error),
    ?assertMatch(#{'$beamtalk_class' := _, error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(type_error, Inner#beamtalk_error.kind).

%% Raw Erlang terms get wrapped as runtime_error
ensure_wrapped_wraps_raw_erlang_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(badarg),
    ?assertMatch(#{'$beamtalk_class' := _, error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(runtime_error, Inner#beamtalk_error.kind).

%% Double-wrapping doesn't happen
ensure_wrapped_no_double_wrap_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped1 = beamtalk_exception_handler:ensure_wrapped(Error),
    Wrapped2 = beamtalk_exception_handler:ensure_wrapped(Wrapped1),
    ?assertEqual(Wrapped1, Wrapped2).

%%% Tests for kind_to_class/1 (BT-452) — pure, no class system needed

kind_to_class_does_not_understand_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(does_not_understand)).

kind_to_class_arity_mismatch_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(arity_mismatch)).

kind_to_class_immutable_value_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(immutable_value)).

kind_to_class_type_error_test() ->
    ?assertEqual('TypeError', beamtalk_exception_handler:kind_to_class(type_error)).

kind_to_class_instantiation_error_test() ->
    ?assertEqual(
        'InstantiationError', beamtalk_exception_handler:kind_to_class(instantiation_error)
    ).

kind_to_class_runtime_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(runtime_error)).

kind_to_class_index_out_of_bounds_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(index_out_of_bounds)).

kind_to_class_class_not_found_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(class_not_found)).

kind_to_class_no_superclass_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(no_superclass)).

kind_to_class_class_already_exists_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(class_already_exists)).

kind_to_class_dispatch_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(dispatch_error)).

kind_to_class_callback_failed_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(callback_failed)).

kind_to_class_actor_dead_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(actor_dead)).

kind_to_class_future_not_awaited_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(future_not_awaited)).

kind_to_class_internal_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(internal_error)).

kind_to_class_user_error_falls_back_to_error_test() ->
    ?assertEqual('Error', beamtalk_exception_handler:kind_to_class(user_error)).

kind_to_class_signal_maps_to_error_test() ->
    ?assertEqual('Error', beamtalk_exception_handler:kind_to_class(signal)).

kind_to_class_unknown_falls_back_to_error_test() ->
    ?assertEqual('Error', beamtalk_exception_handler:kind_to_class(some_unknown_kind)).

%%% Tests for wrap/1 class hierarchy (BT-452) — pure, no class system needed

wrap_sets_runtime_error_class_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
    ?assertEqual('RuntimeError', Class).

wrap_sets_type_error_class_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
    ?assertEqual('TypeError', Class).

wrap_sets_instantiation_error_class_test() ->
    Error = beamtalk_error:new(instantiation_error, 'Actor'),
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
    ?assertEqual('InstantiationError', Class).

wrap_unknown_kind_falls_back_to_error_test() ->
    Error = beamtalk_error:new(some_custom_kind, 'Foo'),
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
    ?assertEqual('Error', Class).

wrap_raw_erlang_term_becomes_runtime_error_test() ->
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(badarg),
    ?assertEqual('RuntimeError', Class).

wrap_runtime_error_kind_becomes_runtime_error_test() ->
    Error = beamtalk_error:new(runtime_error, 'SomeClass'),
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
    ?assertEqual('RuntimeError', Class).

%%% Tests requiring class system (BT-475)
%%% is_exception_class/1, matches_class/2 now delegate to class hierarchy

hierarchy_test_() ->
    {setup, fun setup_class_system/0, fun teardown_class_system/1, [
        %% is_exception_class/1 tests (BT-475 — derived from class system)
        {"Exception is an exception class", fun() ->
            ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Exception'))
        end},
        {"Error is an exception class", fun() ->
            ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Error'))
        end},
        {"RuntimeError is an exception class", fun() ->
            ?assertEqual(true, beamtalk_exception_handler:is_exception_class('RuntimeError'))
        end},
        {"TypeError is an exception class", fun() ->
            ?assertEqual(true, beamtalk_exception_handler:is_exception_class('TypeError'))
        end},
        {"InstantiationError is an exception class", fun() ->
            ?assertEqual(true, beamtalk_exception_handler:is_exception_class('InstantiationError'))
        end},
        {"Dictionary is not an exception class", fun() ->
            ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Dictionary'))
        end},
        {"Integer is not an exception class", fun() ->
            ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Integer'))
        end},
        {"Unregistered class is not an exception class", fun() ->
            ?assertEqual(false, beamtalk_exception_handler:is_exception_class('NonExistent'))
        end},

        %% matches_class/2 with nil (catch all)
        {"nil matches any wrapped error", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class(nil, Wrapped))
        end},

        %% matches_class/2 with atom class names
        {"Exception atom matches wrapped error", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', Wrapped))
        end},

        %% matches_class/2 with kind atoms (backward compat removed — kinds are not class names)
        {"kind atom does not match directly (not a class name)", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(
                false, beamtalk_exception_handler:matches_class(does_not_understand, Wrapped)
            )
        end},

        %% Hierarchy-aware matches_class/2 (BT-452/BT-475)
        {"RuntimeError catches does_not_understand", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped))
        end},
        {"RuntimeError rejects type_error", fun() ->
            Error = beamtalk_error:new(type_error, 'String'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(false, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped))
        end},
        {"TypeError catches type_error", fun() ->
            Error = beamtalk_error:new(type_error, 'String'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('TypeError', Wrapped))
        end},
        {"TypeError rejects does_not_understand", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(false, beamtalk_exception_handler:matches_class('TypeError', Wrapped))
        end},
        {"Error catches all subclasses", fun() ->
            DNU = beamtalk_exception_handler:wrap(
                beamtalk_error:new(does_not_understand, 'Integer')
            ),
            TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
            IE = beamtalk_exception_handler:wrap(beamtalk_error:new(instantiation_error, 'Actor')),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', DNU)),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', TE)),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', IE))
        end},
        {"Exception catches everything", fun() ->
            DNU = beamtalk_exception_handler:wrap(
                beamtalk_error:new(does_not_understand, 'Integer')
            ),
            TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', DNU)),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', TE))
        end},

        %% Metaclass ("ClassName class") variant tests
        {"'RuntimeError class' catches does_not_understand", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(
                true, beamtalk_exception_handler:matches_class('RuntimeError class', Wrapped)
            )
        end},
        {"'TypeError class' catches type_error", fun() ->
            Error = beamtalk_error:new(type_error, 'String'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('TypeError class', Wrapped))
        end},
        {"'Exception class' catches everything", fun() ->
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception class', Wrapped))
        end},

        %% BT-480: User-defined error subclasses
        {"wrap uses exception class from error record", fun() ->
            %% When error.class is an exception class, wrap should use it
            Error = #beamtalk_error{
                kind = signal,
                class = 'RuntimeError',
                selector = undefined,
                message = <<"test">>,
                hint = undefined,
                details = #{}
            },
            #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
            ?assertEqual('RuntimeError', Class)
        end},
        {"wrap falls back to kind_to_class for non-exception class", fun() ->
            %% When error.class is NOT an exception class, fall back to kind_to_class
            Error = beamtalk_error:new(does_not_understand, 'Integer'),
            #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
            ?assertEqual('RuntimeError', Class)
        end},
        {"matches_class_name uses exception class from error record", fun() ->
            %% Error with exception class in error.class should match by that class
            Error = #beamtalk_error{
                kind = signal,
                class = 'RuntimeError',
                selector = undefined,
                message = <<"test">>,
                hint = undefined,
                details = #{}
            },
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped)),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', Wrapped)),
            ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', Wrapped))
        end},
        {"matches_class_name rejects non-matching exception class", fun() ->
            Error = #beamtalk_error{
                kind = signal,
                class = 'RuntimeError',
                selector = undefined,
                message = <<"test">>,
                hint = undefined,
                details = #{}
            },
            Wrapped = beamtalk_exception_handler:wrap(Error),
            ?assertEqual(false, beamtalk_exception_handler:matches_class('TypeError', Wrapped))
        end},
        {"signal_message/2 preserves exception class", fun() ->
            try
                beamtalk_exception_handler:signal_message(<<"test msg">>, 'RuntimeError'),
                % Should never reach here — signal_message is no_return
                ?assert(false)
            catch
                error:#{error := Inner} ->
                    ?assertEqual('RuntimeError', Inner#beamtalk_error.class),
                    ?assertEqual(signal, Inner#beamtalk_error.kind),
                    ?assertEqual(<<"test msg">>, Inner#beamtalk_error.message)
            end
        end}
    ]}.

%% BT-480: signal_from_class/1 unit test
signal_from_class_test_() ->
    {"signal_from_class preserves class and uses class name as message", fun() ->
        try
            beamtalk_exception_handler:signal_from_class('MyCustomError'),
            % Should never reach here — signal_from_class is no_return
            ?assert(false)
        catch
            error:#{error := Inner} ->
                ?assertEqual('MyCustomError', Inner#beamtalk_error.class),
                ?assertEqual(signal, Inner#beamtalk_error.kind),
                ?assertEqual(<<"MyCustomError">>, Inner#beamtalk_error.message)
        end
    end}.

%%% ===================================================================
%%% dispatch/3 tests — pure, no class system needed
%%% ===================================================================

make_test_exception() ->
    Error = #beamtalk_error{
        kind = does_not_understand,
        class = 'Integer',
        selector = 'foo',
        message = <<"Integer does not understand 'foo'">>,
        hint = <<"Check spelling">>,
        details = #{extra => data}
    },
    #{'$beamtalk_class' => 'RuntimeError', error => Error}.

dispatch_message_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        <<"Integer does not understand 'foo'">>,
        beamtalk_exception_handler:dispatch('message', [], Ex)
    ).

dispatch_hint_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        <<"Check spelling">>,
        beamtalk_exception_handler:dispatch('hint', [], Ex)
    ).

dispatch_hint_nil_when_undefined_test() ->
    Error = beamtalk_error:new(runtime_error, 'SomeClass'),
    Ex = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    ?assertEqual(nil, beamtalk_exception_handler:dispatch('hint', [], Ex)).

dispatch_kind_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        does_not_understand,
        beamtalk_exception_handler:dispatch('kind', [], Ex)
    ).

dispatch_selector_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        'foo',
        beamtalk_exception_handler:dispatch('selector', [], Ex)
    ).

dispatch_selector_nil_when_undefined_test() ->
    Error = beamtalk_error:new(runtime_error, 'SomeClass'),
    Ex = #{'$beamtalk_class' => 'RuntimeError', error => Error},
    ?assertEqual(nil, beamtalk_exception_handler:dispatch('selector', [], Ex)).

dispatch_error_class_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        'Integer',
        beamtalk_exception_handler:dispatch('errorClass', [], Ex)
    ).

dispatch_print_string_test() ->
    Ex = make_test_exception(),
    Result = beamtalk_exception_handler:dispatch('printString', [], Ex),
    ?assert(is_binary(Result) orelse is_list(Result)).

dispatch_stack_trace_with_frames_test() ->
    Ex = (make_test_exception())#{stacktrace => [frame1, frame2]},
    ?assertEqual(
        [frame1, frame2],
        beamtalk_exception_handler:dispatch('stackTrace', [], Ex)
    ).

dispatch_stack_trace_empty_when_missing_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        [],
        beamtalk_exception_handler:dispatch('stackTrace', [], Ex)
    ).

dispatch_class_test() ->
    Ex = make_test_exception(),
    ?assertEqual(
        'RuntimeError',
        beamtalk_exception_handler:dispatch('class', [], Ex)
    ).

dispatch_signal_raises_test() ->
    Ex = make_test_exception(),
    try
        beamtalk_exception_handler:dispatch('signal', [], Ex),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind)
    end.

dispatch_signal_colon_raises_test() ->
    Ex = make_test_exception(),
    try
        beamtalk_exception_handler:dispatch('signal:', [<<"custom msg">>], Ex),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(signal, Inner#beamtalk_error.kind),
            ?assertEqual(<<"custom msg">>, Inner#beamtalk_error.message),
            ?assertEqual('RuntimeError', Inner#beamtalk_error.class)
    end.

dispatch_unknown_selector_raises_test() ->
    Ex = make_test_exception(),
    try
        beamtalk_exception_handler:dispatch('nonExistent', [], Ex),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(does_not_understand, Inner#beamtalk_error.kind),
            ?assertEqual('RuntimeError', Inner#beamtalk_error.class),
            ?assertEqual('nonExistent', Inner#beamtalk_error.selector)
    end.

%%% ===================================================================
%%% has_method/1 tests — pure
%%% ===================================================================

has_method_message_test() -> ?assert(beamtalk_exception_handler:has_method('message')).
has_method_hint_test() -> ?assert(beamtalk_exception_handler:has_method('hint')).
has_method_kind_test() -> ?assert(beamtalk_exception_handler:has_method('kind')).
has_method_selector_test() -> ?assert(beamtalk_exception_handler:has_method('selector')).
has_method_error_class_test() -> ?assert(beamtalk_exception_handler:has_method('errorClass')).
has_method_print_string_test() -> ?assert(beamtalk_exception_handler:has_method('printString')).
has_method_stack_trace_test() -> ?assert(beamtalk_exception_handler:has_method('stackTrace')).
has_method_class_test() -> ?assert(beamtalk_exception_handler:has_method('class')).
has_method_signal_test() -> ?assert(beamtalk_exception_handler:has_method('signal')).
has_method_signal_colon_test() -> ?assert(beamtalk_exception_handler:has_method('signal:')).
has_method_unknown_test() -> ?assertNot(beamtalk_exception_handler:has_method('nonExistent')).

%%% ===================================================================
%%% ensure_wrapped/2 tests (BT-107 stacktrace capture)
%%% ===================================================================

ensure_wrapped_2_wraps_with_stacktrace_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Stacktrace = [{some_mod, some_fun, 2, [{file, "test.erl"}, {line, 10}]}],
    Result = beamtalk_exception_handler:ensure_wrapped(Error, Stacktrace),
    ?assertMatch(#{'$beamtalk_class' := _, error := _, stacktrace := _}, Result),
    #{stacktrace := Frames} = Result,
    ?assertEqual(1, length(Frames)),
    [Frame] = Frames,
    ?assertMatch(#{'$beamtalk_class' := 'StackFrame'}, Frame).

ensure_wrapped_2_already_wrapped_adds_stacktrace_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    Stacktrace = [{mod, fun1, 1, []}],
    Result = beamtalk_exception_handler:ensure_wrapped(Wrapped, Stacktrace),
    ?assertMatch(#{stacktrace := _}, Result),
    #{stacktrace := Frames} = Result,
    ?assertEqual(1, length(Frames)).

ensure_wrapped_2_empty_stacktrace_test() ->
    Error = beamtalk_error:new(runtime_error, 'Object'),
    Result = beamtalk_exception_handler:ensure_wrapped(Error, []),
    ?assertMatch(#{stacktrace := []}, Result).

%%% ===================================================================
%%% ensure_wrapped/3 tests (BT-728 Erlang exception type)
%%% ===================================================================

ensure_wrapped_3_exit_wraps_as_exit_error_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(exit, 1, []),
    ?assertMatch(#{'$beamtalk_class' := 'ExitError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(erlang_exit, Inner#beamtalk_error.kind).

ensure_wrapped_3_throw_wraps_as_throw_error_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(throw, 1, []),
    ?assertMatch(#{'$beamtalk_class' := 'ThrowError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(erlang_throw, Inner#beamtalk_error.kind).

ensure_wrapped_3_error_wraps_as_runtime_error_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(error, 1, []),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(runtime_error, Inner#beamtalk_error.kind).

ensure_wrapped_3_already_wrapped_passes_through_test() ->
    Error = beamtalk_error:new(type_error, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    Result = beamtalk_exception_handler:ensure_wrapped(error, Wrapped, []),
    ?assertMatch(#{'$beamtalk_class' := _, stacktrace := []}, Result).

ensure_wrapped_3_beamtalk_error_wraps_correctly_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Counter'),
    Result = beamtalk_exception_handler:ensure_wrapped(error, Error, []),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(does_not_understand, Inner#beamtalk_error.kind).

ensure_wrapped_3_exit_preserves_message_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(exit, "shutdown", []),
    #{error := Inner} = Result,
    ?assert(is_binary(Inner#beamtalk_error.message)).

ensure_wrapped_3_undef_shows_mfa_test() ->
    ST = [{some_mod, some_fun, 2, [{file, "test.erl"}, {line, 10}]}],
    Result = beamtalk_exception_handler:ensure_wrapped(error, undef, ST),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
    ?assertEqual(
        <<"Undefined function: some_mod:some_fun/2">>,
        Inner#beamtalk_error.message
    ).

ensure_wrapped_3_undef_empty_stacktrace_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(error, undef, []),
    #{error := Inner} = Result,
    ?assertEqual(<<"Undefined function">>, Inner#beamtalk_error.message).

%%% ===================================================================
%%% wrap_raw/1 — badarity wrapping
%%% ===================================================================

wrap_raw_badarity_test() ->
    Fun = fun(X) -> X end,
    Raw = {badarity, {Fun, [1, 2, 3]}},
    Result = beamtalk_exception_handler:wrap(Raw),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(arity_mismatch, Inner#beamtalk_error.kind),
    ?assertEqual('Block', Inner#beamtalk_error.class),
    ?assert(is_binary(Inner#beamtalk_error.message)),
    ?assert(is_binary(Inner#beamtalk_error.hint)),
    #{expected_args := 1, actual_args := 3} = Inner#beamtalk_error.details.

%%% ===================================================================
%%% signal_message/1 tests
%%% ===================================================================

signal_message_binary_test() ->
    try
        beamtalk_exception_handler:signal_message(<<"error msg">>),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(signal, Inner#beamtalk_error.kind),
            ?assertEqual(<<"error msg">>, Inner#beamtalk_error.message)
    end.

signal_message_atom_test() ->
    try
        beamtalk_exception_handler:signal_message(some_atom),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(<<"some_atom">>, Inner#beamtalk_error.message)
    end.

signal_message_other_test() ->
    try
        beamtalk_exception_handler:signal_message(12345),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assert(is_binary(Inner#beamtalk_error.message))
    end.

%%% ===================================================================
%%% signal/1 tests
%%% ===================================================================

signal_atom_kind_test() ->
    try
        beamtalk_exception_handler:signal(runtime_error),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual(runtime_error, Inner#beamtalk_error.kind),
            ?assertEqual('Exception', Inner#beamtalk_error.class)
    end.

%% @private Setup class system for hierarchy tests
setup_class_system() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    case whereis(beamtalk_bootstrap) of
        undefined -> beamtalk_bootstrap:start_link();
        _ -> ok
    end,
    beamtalk_stdlib:init(),
    ok.

%% @private Teardown class system
teardown_class_system(_) ->
    ok.
