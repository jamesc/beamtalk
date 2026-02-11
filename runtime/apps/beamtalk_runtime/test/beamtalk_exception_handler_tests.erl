%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_exception_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% Tests for ensure_wrapped/1 (ADR 0015)

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

%%% Tests for matches_class/2 with wrapped exceptions (ADR 0015)

%% matches_class handles wrapped Exception maps
matches_class_wrapped_nil_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(true, beamtalk_exception_handler:matches_class(nil, Wrapped)).

matches_class_wrapped_exception_atom_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', Wrapped)).

matches_class_wrapped_kind_match_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(true, beamtalk_exception_handler:matches_class(does_not_understand, Wrapped)).

matches_class_wrapped_kind_mismatch_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(false, beamtalk_exception_handler:matches_class(does_not_understand, Wrapped)).

%%% Tests for kind_to_class/1 (BT-452)

kind_to_class_does_not_understand_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(does_not_understand)).

kind_to_class_arity_mismatch_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(arity_mismatch)).

kind_to_class_immutable_value_test() ->
    ?assertEqual('RuntimeError', beamtalk_exception_handler:kind_to_class(immutable_value)).

kind_to_class_type_error_test() ->
    ?assertEqual('TypeError', beamtalk_exception_handler:kind_to_class(type_error)).

kind_to_class_instantiation_error_test() ->
    ?assertEqual('InstantiationError', beamtalk_exception_handler:kind_to_class(instantiation_error)).

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

kind_to_class_unknown_falls_back_to_error_test() ->
    ?assertEqual('Error', beamtalk_exception_handler:kind_to_class(some_unknown_kind)).

%%% Tests for is_exception_class/1 (BT-452)

is_exception_class_test() ->
    ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Exception')),
    ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Error')),
    ?assertEqual(true, beamtalk_exception_handler:is_exception_class('RuntimeError')),
    ?assertEqual(true, beamtalk_exception_handler:is_exception_class('TypeError')),
    ?assertEqual(true, beamtalk_exception_handler:is_exception_class('InstantiationError')),
    ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Dictionary')),
    ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Integer')).

%%% Tests for wrap/1 class hierarchy (BT-452)

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

%%% Tests for hierarchy-aware matches_class/2 (BT-452)

matches_class_runtime_error_catches_dnu_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped)).

matches_class_runtime_error_rejects_type_error_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(false, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped)).

matches_class_type_error_catches_type_error_test() ->
    Error = beamtalk_error:new(type_error, 'String'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('TypeError', Wrapped)).

matches_class_type_error_rejects_dnu_test() ->
    Error = beamtalk_error:new(does_not_understand, 'Integer'),
    Wrapped = beamtalk_exception_handler:wrap(Error),
    ?assertEqual(false, beamtalk_exception_handler:matches_class('TypeError', Wrapped)).

matches_class_error_catches_all_subclasses_test() ->
    DNU = beamtalk_exception_handler:wrap(beamtalk_error:new(does_not_understand, 'Integer')),
    TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
    IE = beamtalk_exception_handler:wrap(beamtalk_error:new(instantiation_error, 'Actor')),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', DNU)),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', TE)),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', IE)).

matches_class_exception_catches_everything_test() ->
    DNU = beamtalk_exception_handler:wrap(beamtalk_error:new(does_not_understand, 'Integer')),
    TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', DNU)),
    ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', TE)).
