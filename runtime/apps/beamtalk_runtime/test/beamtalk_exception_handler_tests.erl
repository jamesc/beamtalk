%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

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
    {setup,
     fun setup_class_system/0,
     fun teardown_class_system/1,
     [
      %% is_exception_class/1 tests (BT-475 — derived from class system)
      {"Exception is an exception class",
       fun() -> ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Exception')) end},
      {"Error is an exception class",
       fun() -> ?assertEqual(true, beamtalk_exception_handler:is_exception_class('Error')) end},
      {"RuntimeError is an exception class",
       fun() -> ?assertEqual(true, beamtalk_exception_handler:is_exception_class('RuntimeError')) end},
      {"TypeError is an exception class",
       fun() -> ?assertEqual(true, beamtalk_exception_handler:is_exception_class('TypeError')) end},
      {"InstantiationError is an exception class",
       fun() -> ?assertEqual(true, beamtalk_exception_handler:is_exception_class('InstantiationError')) end},
      {"Dictionary is not an exception class",
       fun() -> ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Dictionary')) end},
      {"Integer is not an exception class",
       fun() -> ?assertEqual(false, beamtalk_exception_handler:is_exception_class('Integer')) end},
      {"Unregistered class is not an exception class",
       fun() -> ?assertEqual(false, beamtalk_exception_handler:is_exception_class('NonExistent')) end},

      %% matches_class/2 with nil (catch all)
      {"nil matches any wrapped error",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class(nil, Wrapped))
       end},

      %% matches_class/2 with atom class names
      {"Exception atom matches wrapped error",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', Wrapped))
       end},

      %% matches_class/2 with kind atoms (backward compat removed — kinds are not class names)
      {"kind atom does not match directly (not a class name)",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(false, beamtalk_exception_handler:matches_class(does_not_understand, Wrapped))
       end},

      %% Hierarchy-aware matches_class/2 (BT-452/BT-475)
      {"RuntimeError catches does_not_understand",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped))
       end},
      {"RuntimeError rejects type_error",
       fun() ->
           Error = beamtalk_error:new(type_error, 'String'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(false, beamtalk_exception_handler:matches_class('RuntimeError', Wrapped))
       end},
      {"TypeError catches type_error",
       fun() ->
           Error = beamtalk_error:new(type_error, 'String'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('TypeError', Wrapped))
       end},
      {"TypeError rejects does_not_understand",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(false, beamtalk_exception_handler:matches_class('TypeError', Wrapped))
       end},
      {"Error catches all subclasses",
       fun() ->
           DNU = beamtalk_exception_handler:wrap(beamtalk_error:new(does_not_understand, 'Integer')),
           TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
           IE = beamtalk_exception_handler:wrap(beamtalk_error:new(instantiation_error, 'Actor')),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', DNU)),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', TE)),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Error', IE))
       end},
      {"Exception catches everything",
       fun() ->
           DNU = beamtalk_exception_handler:wrap(beamtalk_error:new(does_not_understand, 'Integer')),
           TE = beamtalk_exception_handler:wrap(beamtalk_error:new(type_error, 'String')),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', DNU)),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception', TE))
       end},

      %% Metaclass ("ClassName class") variant tests
      {"'RuntimeError class' catches does_not_understand",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('RuntimeError class', Wrapped))
       end},
      {"'TypeError class' catches type_error",
       fun() ->
           Error = beamtalk_error:new(type_error, 'String'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('TypeError class', Wrapped))
       end},
      {"'Exception class' catches everything",
       fun() ->
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           Wrapped = beamtalk_exception_handler:wrap(Error),
           ?assertEqual(true, beamtalk_exception_handler:matches_class('Exception class', Wrapped))
       end},

      %% BT-480: User-defined error subclasses
      {"wrap uses exception class from error record",
       fun() ->
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
      {"wrap falls back to kind_to_class for non-exception class",
       fun() ->
           %% When error.class is NOT an exception class, fall back to kind_to_class
           Error = beamtalk_error:new(does_not_understand, 'Integer'),
           #{'$beamtalk_class' := Class} = beamtalk_exception_handler:wrap(Error),
           ?assertEqual('RuntimeError', Class)
       end},
      {"matches_class_name uses exception class from error record",
       fun() ->
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
      {"matches_class_name rejects non-matching exception class",
       fun() ->
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
      {"signal_message/2 preserves exception class",
       fun() ->
           try
               beamtalk_exception_handler:signal_message(<<"test msg">>, 'RuntimeError'),
               ?assert(false)  % Should never reach here — signal_message is no_return
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
    {"signal_from_class preserves class and uses class name as message",
     fun() ->
         try
             beamtalk_exception_handler:signal_from_class('MyCustomError'),
             ?assert(false)  % Should never reach here — signal_from_class is no_return
         catch
             error:#{error := Inner} ->
                 ?assertEqual('MyCustomError', Inner#beamtalk_error.class),
                 ?assertEqual(signal, Inner#beamtalk_error.kind),
                 ?assertEqual(<<"MyCustomError">>, Inner#beamtalk_error.message)
         end
     end}.

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
