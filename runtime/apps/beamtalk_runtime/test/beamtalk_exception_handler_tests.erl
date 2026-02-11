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
    ?assertMatch(#{'$beamtalk_class' := 'Exception', error := _}, Result),
    #{error := Inner} = Result,
    ?assertEqual(type_error, Inner#beamtalk_error.kind).

%% Raw Erlang terms get wrapped as runtime_error
ensure_wrapped_wraps_raw_erlang_test() ->
    Result = beamtalk_exception_handler:ensure_wrapped(badarg),
    ?assertMatch(#{'$beamtalk_class' := 'Exception', error := _}, Result),
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
