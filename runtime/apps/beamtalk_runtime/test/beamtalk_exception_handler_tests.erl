%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_exception_handler_tests).

-moduledoc """
EUnit tests for beamtalk_exception_handler module.

Tests exception wrapping, ensure_wrapped/1, and exception map patterns (ADR 0015).
""".
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

%% Raw Erlang terms get wrapped and classified (BT-2707): a bare badarg with no
%% dispatch context becomes its own argument_error kind, not generic runtime_error.
ensure_wrapped_wraps_raw_erlang_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(badarg),
    ?assertMatch(#{'$beamtalk_class' := _, error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(argument_error, Inner#beamtalk_error.kind).

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
    %% After removing the does_not_understand catch-all (BT-1764),
    %% unknown selectors cause a function_clause error — the compiled
    %% bt@stdlib@exception module handles Object-inherited methods.
    ?assertError(
        function_clause,
        beamtalk_exception_handler:dispatch('nonExistent', [], Ex)
    ).

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
%%% class_to_kind/1 tests — pure, no class system needed
%%% ===================================================================

class_to_kind_instantiation_error_test() ->
    ?assertEqual(
        instantiation_error, beamtalk_exception_handler:class_to_kind('InstantiationError')
    ).

class_to_kind_type_error_test() ->
    ?assertEqual(type_error, beamtalk_exception_handler:class_to_kind('TypeError')).

class_to_kind_unknown_falls_back_to_signal_test() ->
    ?assertEqual(signal, beamtalk_exception_handler:class_to_kind('SomeUserClass')).

class_to_kind_error_falls_back_to_signal_test() ->
    ?assertEqual(signal, beamtalk_exception_handler:class_to_kind('Error')).

class_to_kind_runtime_error_falls_back_to_signal_test() ->
    ?assertEqual(signal, beamtalk_exception_handler:class_to_kind('RuntimeError')).

%%% ===================================================================
%%% ensure_wrapped/3 — future_rejected unwrapping (BT-869)
%%% ===================================================================

ensure_wrapped_3_throw_future_rejected_beamtalk_error_test() ->
    %% future_rejected with a raw beamtalk_error should be unwrapped and wrapped
    Inner = beamtalk_error:new(type_error, 'Future'),
    Reason = {future_rejected, Inner},
    Result = beamtalk_exception_handler:ensure_wrapped(throw, Reason, []),
    ?assertMatch(#{'$beamtalk_class' := 'TypeError', error := _}, Result),
    #{error := WrappedInner} = Result,
    ?assertEqual(type_error, WrappedInner#beamtalk_error.kind).

ensure_wrapped_3_throw_future_rejected_already_wrapped_test() ->
    %% future_rejected with an already-wrapped map should pass through
    Inner = beamtalk_error:new(does_not_understand, 'Integer'),
    AlreadyWrapped = beamtalk_exception_handler:wrap(Inner),
    Reason = {future_rejected, AlreadyWrapped},
    Result = beamtalk_exception_handler:ensure_wrapped(throw, Reason, []),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := UnwrappedInner} = Result,
    ?assertEqual(does_not_understand, UnwrappedInner#beamtalk_error.kind),
    ?assertEqual('Integer', UnwrappedInner#beamtalk_error.class).

ensure_wrapped_3_throw_future_rejected_error_tuple_test() ->
    %% future_rejected with {error, WrappedMap} should also be unwrapped
    Inner = beamtalk_error:new(runtime_error, 'Actor'),
    AlreadyWrapped = beamtalk_exception_handler:wrap(Inner),
    Reason = {future_rejected, {error, AlreadyWrapped}},
    Result = beamtalk_exception_handler:ensure_wrapped(throw, Reason, []),
    ?assertMatch(#{'$beamtalk_class' := 'RuntimeError', error := _}, Result),
    #{error := UnwrappedInner} = Result,
    ?assertEqual(runtime_error, UnwrappedInner#beamtalk_error.kind),
    ?assertEqual('Actor', UnwrappedInner#beamtalk_error.class).

ensure_wrapped_3_throw_non_future_wraps_as_throw_error_test() ->
    %% Non-future_rejected throw becomes ThrowError
    Result = beamtalk_exception_handler:ensure_wrapped(throw, some_reason, []),
    ?assertMatch(#{'$beamtalk_class' := 'ThrowError', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(erlang_throw, Inner#beamtalk_error.kind).

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
%%% wrap_raw/2 — raw-error classification into buckets A/B/C (BT-2707)
%%% ===================================================================

%% Helper: classify a raw reason (no context) and return the inner error.
classify(Reason) ->
    #{error := Inner} = beamtalk_exception_handler:wrap_raw(Reason),
    Inner.

%% Helper: classify a raw reason with a dispatch breadcrumb.
classify(Reason, Context) ->
    #{error := Inner} = beamtalk_exception_handler:wrap_raw(Reason, Context),
    Inner.

%%% --- Bucket A: user-input errors ---

wrap_raw_badarith_is_type_error_test() ->
    Inner = classify(badarith),
    ?assertEqual(type_error, Inner#beamtalk_error.kind),
    ?assert(is_binary(Inner#beamtalk_error.hint)).

wrap_raw_badarith_class_is_type_error_test() ->
    #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap_raw(badarith),
    ?assertEqual('TypeError', Class).

wrap_raw_badkey_is_key_error_test() ->
    Inner = classify({badkey, <<"missing">>}),
    ?assertEqual(key_error, Inner#beamtalk_error.kind),
    ?assertEqual(#{key => <<"missing">>}, Inner#beamtalk_error.details).

wrap_raw_badmap_is_type_error_test() ->
    Inner = classify({badmap, 42}),
    ?assertEqual(type_error, Inner#beamtalk_error.kind).

wrap_raw_badarg_no_context_is_argument_error_test() ->
    Inner = classify(badarg),
    ?assertEqual(argument_error, Inner#beamtalk_error.kind),
    %% Without a breadcrumb the message is unlocated.
    ?assertEqual(<<"invalid argument">>, Inner#beamtalk_error.message).

%%% --- Bucket B: internal bugs (loud, not dressed up as user errors) ---

wrap_raw_function_clause_is_internal_error_test() ->
    Inner = classify(function_clause),
    ?assertEqual(internal_error, Inner#beamtalk_error.kind),
    %% Internal bugs must read as "please report", never as a user mistake.
    ?assertEqual('RuntimeError', kind_to_class_of(Inner)),
    ?assertNotEqual(type_error, Inner#beamtalk_error.kind).

wrap_raw_case_clause_is_internal_error_test() ->
    Inner = classify({case_clause, some_value}),
    ?assertEqual(internal_error, Inner#beamtalk_error.kind).

wrap_raw_badmatch_is_internal_error_test() ->
    Inner = classify({badmatch, []}),
    ?assertEqual(internal_error, Inner#beamtalk_error.kind).

wrap_raw_if_clause_is_internal_error_test() ->
    Inner = classify(if_clause),
    ?assertEqual(internal_error, Inner#beamtalk_error.kind).

%%% --- Bucket C: resource/environment ---

wrap_raw_system_limit_is_resource_error_test() ->
    Inner = classify(system_limit),
    ?assertEqual(resource_error, Inner#beamtalk_error.kind).

wrap_raw_noproc_is_process_not_found_test() ->
    Inner = classify(noproc),
    ?assertEqual(process_not_found, Inner#beamtalk_error.kind).

wrap_raw_timeout_is_timeout_error_test() ->
    Inner = classify(timeout),
    ?assertEqual(timeout_error, Inner#beamtalk_error.kind).

wrap_raw_timeout_tuple_is_timeout_error_test() ->
    Inner = classify({timeout, gen_server_call}),
    ?assertEqual(timeout_error, Inner#beamtalk_error.kind).

%%% --- Fallback ---

wrap_raw_unknown_stays_runtime_error_test() ->
    Inner = classify({some_unknown_reason, 1, 2}),
    ?assertEqual(runtime_error, Inner#beamtalk_error.kind).

%%% --- BT-2705: breadcrumb produces located messages ---

wrap_raw_badarith_with_breadcrumb_is_located_test() ->
    Inner = classify(badarith, #{selector => sum, class => 'Tuple'}),
    ?assertEqual(type_error, Inner#beamtalk_error.kind),
    ?assertEqual(sum, Inner#beamtalk_error.selector),
    ?assertEqual('Tuple', Inner#beamtalk_error.class),
    %% Message names the originating Class>>selector.
    ?assertEqual(
        match,
        re:run(Inner#beamtalk_error.message, "Tuple>>sum", [{capture, none}])
    ).

wrap_raw_badarg_with_selector_only_is_located_test() ->
    Inner = classify(badarg, #{selector => 'at:'}),
    ?assertEqual('at:', Inner#beamtalk_error.selector),
    ?assertEqual(
        match,
        re:run(Inner#beamtalk_error.message, "'at:'", [{capture, none}])
    ).

%%% --- ensure_wrapped/4: context-aware idempotent wrapper (BT-2705) ---

ensure_wrapped_4_classifies_with_context_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(
        error, badarith, [], #{selector => sum, class => 'Tuple'}
    ),
    ?assertMatch(#{'$beamtalk_class' := 'TypeError', error := _, stacktrace := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(type_error, Inner#beamtalk_error.kind),
    ?assertEqual(sum, Inner#beamtalk_error.selector).

ensure_wrapped_4_already_wrapped_preserves_innermost_test() ->
    %% Inner frame already classified the error with its selector; an outer frame
    %% with a different breadcrumb must NOT overwrite the classification (innermost
    %% wins). A stacktrace may be backfilled when the inner map lacked one, so we
    %% compare the classified error rather than full-map equality.
    InnerWrapped = beamtalk_exception_handler:wrap_raw(badarith, #{
        selector => sum, class => 'Tuple'
    }),
    Result = beamtalk_exception_handler:ensure_wrapped(
        error, InnerWrapped, [], #{selector => 'do:', class => 'OuterThing'}
    ),
    ?assertEqual(maps:get(error, InnerWrapped), maps:get(error, Result)),
    ?assertEqual('TypeError', maps:get('$beamtalk_class', Result)).

ensure_wrapped_4_already_wrapped_with_stacktrace_kept_test() ->
    %% When the inner map already carries a stacktrace, it is preserved verbatim.
    InnerWrapped = (beamtalk_exception_handler:wrap_raw(badarith, #{selector => sum}))#{
        stacktrace => [frame_a, frame_b]
    },
    Result = beamtalk_exception_handler:ensure_wrapped(error, InnerWrapped, [], #{
        selector => 'do:'
    }),
    ?assertEqual([frame_a, frame_b], maps:get(stacktrace, Result)).

ensure_wrapped_4_empty_context_delegates_test() ->
    %% No breadcrumb → identical to ensure_wrapped/3.
    R3 = beamtalk_exception_handler:ensure_wrapped(error, badarith, []),
    R4 = beamtalk_exception_handler:ensure_wrapped(error, badarith, [], #{}),
    ?assertEqual(R3, R4).

%% Helper: the exception class an inner error maps to.
kind_to_class_of(#beamtalk_error{kind = Kind}) ->
    beamtalk_exception_handler:kind_to_class(Kind).

%%% kind_to_class/1 — new classified kinds (BT-2707)

kind_to_class_key_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(key_error)).

kind_to_class_argument_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(argument_error)).

kind_to_class_resource_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(resource_error)).

kind_to_class_process_not_found_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(process_not_found)).

kind_to_class_timeout_error_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(timeout_error)).

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

%%% ===================================================================
%%% class_signal_message/2 and class_signal/1 tests (BT-1524)
%%% ===================================================================

class_signal_message_with_beamtalk_object_test() ->
    ClassSelf = #beamtalk_object{
        class = 'MyCustomError class',
        class_mod = 'bt@stdlib@my_custom_error',
        pid = self()
    },
    try
        beamtalk_exception_handler:class_signal_message(<<"test msg">>, ClassSelf),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual('MyCustomError', Inner#beamtalk_error.class),
            ?assertEqual(signal, Inner#beamtalk_error.kind),
            ?assertEqual(<<"test msg">>, Inner#beamtalk_error.message)
    end.

class_signal_message_without_class_suffix_test() ->
    %% Class tag without " class" suffix should also work
    ClassSelf = #beamtalk_object{
        class = 'Exception',
        class_mod = 'bt@stdlib@exception',
        pid = self()
    },
    try
        beamtalk_exception_handler:class_signal_message(<<"oops">>, ClassSelf),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual('Exception', Inner#beamtalk_error.class),
            ?assertEqual(<<"oops">>, Inner#beamtalk_error.message)
    end.

class_signal_message_fallback_test() ->
    %% Non-beamtalk_object falls back to 'Exception'
    try
        beamtalk_exception_handler:class_signal_message(<<"fallback">>, not_an_object),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual('Exception', Inner#beamtalk_error.class),
            ?assertEqual(<<"fallback">>, Inner#beamtalk_error.message)
    end.

class_signal_with_beamtalk_object_test() ->
    ClassSelf = #beamtalk_object{
        class = 'RuntimeError class',
        class_mod = 'bt@stdlib@runtime_error',
        pid = self()
    },
    try
        beamtalk_exception_handler:class_signal(ClassSelf),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual('RuntimeError', Inner#beamtalk_error.class),
            ?assertEqual(signal, Inner#beamtalk_error.kind),
            ?assertEqual(<<"RuntimeError">>, Inner#beamtalk_error.message)
    end.

class_signal_fallback_test() ->
    try
        beamtalk_exception_handler:class_signal(not_an_object),
        ?assert(false)
    catch
        error:#{error := Inner} ->
            ?assertEqual('Exception', Inner#beamtalk_error.class),
            ?assertEqual(<<"Exception">>, Inner#beamtalk_error.message)
    end.

-doc "Setup class system for hierarchy tests".
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

-doc "Teardown class system".
teardown_class_system(_) ->
    ok.
