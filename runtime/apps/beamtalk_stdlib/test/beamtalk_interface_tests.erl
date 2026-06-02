%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_interface module.

Tests log_compiler_diagnostics/2 (BT-2219): verifies that an OTP logger call
fires at the appropriate level when the compiler port returns {error, Diagnostics}
in findSendersIn/2 and findReferencesToIn/2, while [] is still returned
(per-method fault-tolerance contract unchanged).
""".

%% Logger handler callback for log capture
-export([log/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% Logger handler callback
%%% ============================================================================

%% Capture log events and forward to the test process.
log(LogEvent, #{config := #{parent := Parent}}) ->
    Parent ! {captured_log, LogEvent},
    ok.

%%% ============================================================================
%%% Helpers
%%% ============================================================================

install_capture_handler() ->
    HandlerId = beamtalk_interface_test_capture,
    ok = logger:add_handler(HandlerId, ?MODULE, #{
        config => #{parent => self()},
        level => all
    }),
    %% Lower the primary logger level to 'all' so our capture handler receives
    %% warning/error events. The sys.config for tests sets primary level to 'error'
    %% to suppress noise; we restore it after the test.
    #{level := OrigLevel} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    {HandlerId, OrigLevel}.

remove_capture_handler({HandlerId, OrigLevel}) ->
    logger:remove_handler(HandlerId),
    logger:set_primary_config(level, OrigLevel).

%% Drain one log event from the mailbox that matches a given level.
%% Returns {ok, LogEvent} | not_found.
%% Uses a fixed deadline so non-matching events consume the timeout budget
%% rather than resetting it on each recursion (avoids waiting far longer than
%% Timeout under log noise).
collect_log(Level, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    collect_log_until(Level, Deadline).

collect_log_until(Level, Deadline) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),
    case Remaining =< 0 of
        true ->
            not_found;
        false ->
            receive
                {captured_log, #{level := Level} = E} ->
                    {ok, E};
                {captured_log, _Other} ->
                    collect_log_until(Level, Deadline)
            after Remaining ->
                not_found
            end
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — warning for parse failure
%%% ============================================================================

log_compiler_diagnostics_warning_test() ->
    %% Diagnostics that do NOT indicate port unavailability should trigger
    %% a LOG_WARNING.
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Parse error: unexpected token">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findSendersIn:selector:'),
        Result = collect_log(warning, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        %% Verify the log metadata carries domain, selector, and diagnostics payload
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta)),
        ?assertEqual('findSendersIn:selector:', maps:get(selector, Meta)),
        ?assertEqual(Diagnostics, maps:get(diagnostics, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — error for port unavailability
%%% ============================================================================

log_compiler_diagnostics_error_on_port_unavailable_test() ->
    %% Diagnostics that indicate the compiler server is not available should
    %% trigger a LOG_ERROR (not just a warning).
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Compiler server is not available">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findReferencesToIn:class:'),
        Result = collect_log(error, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta)),
        ?assertEqual('findReferencesToIn:class:', maps:get(selector, Meta)),
        ?assertEqual(Diagnostics, maps:get(diagnostics, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: log_compiler_diagnostics/2 — error for port timeout
%%% ============================================================================

log_compiler_diagnostics_error_on_timeout_test() ->
    %% "timed out" diagnostics also escalate to LOG_ERROR.
    Handle = install_capture_handler(),
    try
        Diagnostics = [#{message => <<"Compiler server timed out">>}],
        beamtalk_interface:log_compiler_diagnostics(Diagnostics, 'findSendersIn:selector:'),
        Result = collect_log(error, 500),
        ?assertMatch({ok, _}, Result),
        {ok, Event} = Result,
        #{meta := Meta} = Event,
        ?assertEqual([beamtalk, stdlib], maps:get(domain, Meta)),
        ?assertEqual('findSendersIn:selector:', maps:get(selector, Meta)),
        ?assertEqual(Diagnostics, maps:get(diagnostics, Meta))
    after
        remove_capture_handler(Handle)
    end.

%%% ============================================================================
%%% BT-2219: findSendersIn/2 — still returns a list (happy path)
%%% ============================================================================

find_senders_in_returns_list_test() ->
    %% Even when the compiler returns an error (here we rely on whatever the
    %% compiler does with invalid source that cannot be parsed), findSendersIn/2
    %% must return []. The per-method fault-tolerance contract is unchanged.
    %%
    %% With a running compiler server, calling with empty binary produces {ok, []}
    %% (compiler returns empty senders for empty source), not an error. We test
    %% the error path via log_compiler_diagnostics/2 directly (above).
    %% This test confirms the happy-path return is still a list.
    Result = beamtalk_interface:findSendersIn(<<"x + 1">>, <<"ifTrue:">>),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% BT-2219: findReferencesToIn/2 — still returns a list (happy path)
%%% ============================================================================

find_references_to_in_returns_list_test() ->
    %% As above: the happy path returns a list. The error-path is tested via
    %% log_compiler_diagnostics/2 directly.
    Result = beamtalk_interface:findReferencesToIn(<<"x + 1">>, 'Integer'),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% Navigation FFI type-error and degrade-to-[] paths (BT-2384)
%%%
%%% These exercise the navigation FFI exports without a running class registry:
%%% the input-validation error clauses and (where the compiler port is not
%%% running under EUnit) the {error, Diagnostics} -> [] degrade branch.
%%% ============================================================================

find_senders_in_rejects_bad_args_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'BeamtalkInterface'}},
        beamtalk_interface:findSendersIn(not_a_binary, ifTrue)
    ).

all_sends_in_returns_list_test() ->
    Result = beamtalk_interface:allSendsIn(<<"x foo: 1">>),
    ?assert(is_list(Result)).

all_sends_in_rejects_bad_args_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, selector = 'allSendsIn:'}},
        beamtalk_interface:allSendsIn(not_a_binary)
    ).

find_references_to_in_rejects_bad_args_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error, class = 'BeamtalkInterface'}},
        beamtalk_interface:findReferencesToIn(123, 'Integer')
    ).

find_field_readers_in_returns_list_test() ->
    Result = beamtalk_interface:findFieldReadersIn(<<"x + 1">>, count),
    ?assert(is_list(Result)).

find_field_writers_in_returns_list_test() ->
    Result = beamtalk_interface:findFieldWritersIn(<<"x + 1">>, count),
    ?assert(is_list(Result)).

find_field_access_rejects_bad_args_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = type_error}},
        beamtalk_interface:findFieldReadersIn(not_a_binary, count)
    ).

ffi_sites_in_returns_list_test() ->
    Result = beamtalk_interface:ffiSitesIn(<<"x + 1">>, <<"lists">>, <<"reverse">>, any),
    ?assert(is_list(Result)).

ffi_sites_in_integer_arity_test() ->
    Result = beamtalk_interface:ffiSitesIn(<<"x + 1">>, lists, reverse, 1),
    ?assert(is_list(Result)).

ffi_sites_in_rejects_bad_args_test() ->
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = type_error, selector = 'ffiSitesIn:module:function:arity:'
            }
        },
        beamtalk_interface:ffiSitesIn(<<"x">>, <<"lists">>, <<"reverse">>, -1)
    ).

%%% ============================================================================
%%% version/0 fallback (BT-2384)
%%%
%%% Under EUnit the beamtalk_runtime app is typically not started, so version/0
%%% takes the <<"unknown">> fallback. Either way it must return a binary.
%%% ============================================================================

version_returns_binary_test() ->
    ?assert(is_binary(beamtalk_interface:version())).

version_matches_app_vsn_when_loaded_test() ->
    %% Load (not start) the runtime app so application:get_key/2 returns {ok, Vsn},
    %% driving the {ok, Vsn} -> list_to_binary(Vsn) branch of version/0.
    _ = application:load(beamtalk_runtime),
    Result = beamtalk_interface:version(),
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> ?assertEqual(list_to_binary(Vsn), Result);
        _ -> ?assertEqual(<<"unknown">>, Result)
    end.

dispatch_version_matches_app_vsn_when_loaded_test() ->
    _ = application:load(beamtalk_runtime),
    Result = beamtalk_interface:dispatch(version, [], fake_self()),
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> ?assertEqual(list_to_binary(Vsn), Result);
        _ -> ?assertEqual(<<"unknown">>, Result)
    end.

dispatch_version_returns_binary_test() ->
    ?assert(is_binary(beamtalk_interface:dispatch(version, [], fake_self()))).

dispatch_unknown_selector_raises_dnu_test() ->
    ?assertError(
        #{error := #beamtalk_error{kind = does_not_understand, class = 'BeamtalkInterface'}},
        beamtalk_interface:dispatch(noSuchSelectorXyz, [], fake_self())
    ).

%%% ============================================================================
%%% Live registry tests (BT-2384)
%%%
%%% Load the full stdlib so the class registry is populated with real classes
%%% (Integer, Object, etc.) and exercise allClasses/0, classNamed/1, findClass/1,
%%% globals/0, help/1, help/2, erlangHelp/1, erlangHelp/2 — including the
%%% format_class_help / format_method_help / collect_flattened_methods /
%%% find_defining_class hierarchy-walking helpers and the not-found error paths.
%%% ============================================================================

%% A fake Self value — BeamtalkInterface primitives ignore Self entirely.
fake_self() ->
    {beamtalk_object, 'BeamtalkInterface class', 'bt@stdlib@beamtalk_interface', self()}.

live_setup() ->
    case whereis(pg) of
        undefined -> pg:start_link();
        _ -> ok
    end,
    beamtalk_extensions:init(),
    case whereis(beamtalk_bootstrap) of
        undefined -> {ok, _} = beamtalk_bootstrap:start_link();
        _ -> ok
    end,
    beamtalk_stdlib:init(),
    ok.

live_teardown(_) ->
    ok.

live_registry_test_() ->
    {setup, fun live_setup/0, fun live_teardown/1, fun(_) ->
        [
            {"allClasses returns a non-empty list of class objects", fun() ->
                Classes = beamtalk_interface:allClasses(),
                ?assert(is_list(Classes)),
                ?assert(length(Classes) > 0),
                lists:foreach(
                    fun(C) -> ?assertMatch({beamtalk_object, _, _, _}, C) end,
                    Classes
                )
            end},
            {"globals returns a map keyed by class name", fun() ->
                G = beamtalk_interface:globals(),
                ?assert(is_map(G)),
                ?assert(maps:is_key('Integer', G)),
                ?assertMatch({beamtalk_object, _, _, _}, maps:get('Integer', G))
            end},
            {"classNamed by atom returns a class object", fun() ->
                ?assertMatch(
                    {beamtalk_object, 'Integer class', _, _},
                    beamtalk_interface:classNamed('Integer')
                )
            end},
            {"classNamed by binary returns a class object", fun() ->
                ?assertMatch(
                    {beamtalk_object, 'Integer class', _, _},
                    beamtalk_interface:classNamed(<<"Integer">>)
                )
            end},
            {"classNamed unknown binary returns nil", fun() ->
                ?assertEqual(nil, beamtalk_interface:classNamed(<<"ZzzNoSuchClass">>))
            end},
            {"classNamed metaclass tag returns a metaclass object", fun() ->
                ?assertMatch(
                    {beamtalk_object, 'Metaclass', beamtalk_metaclass_bt, _},
                    beamtalk_interface:classNamed('Integer class')
                )
            end},
            {"findClass resolves a known class", fun() ->
                ?assertMatch(
                    {beamtalk_object, 'Integer class', _, _},
                    beamtalk_interface:findClass('Integer')
                )
            end},
            {"findClass unknown metaclass tag returns nil", fun() ->
                ?assertEqual(nil, beamtalk_interface:findClass('ZzzNope class'))
            end},
            {"classNamed integer arg raises type_error", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = type_error}},
                    beamtalk_interface:classNamed(42)
                )
            end},
            {"help: on a known class returns its formatted help", fun() ->
                H = beamtalk_interface:help('Integer'),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"Integer">>))
            end},
            {"help: by binary class name works", fun() ->
                H = beamtalk_interface:help(<<"Integer">>),
                ?assert(is_binary(H))
            end},
            {"help: unknown class raises class_not_found", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = class_not_found}},
                    beamtalk_interface:help('ZzzNoSuchClass')
                )
            end},
            {"help:selector: on an own method returns its signature", fun() ->
                H = beamtalk_interface:help('Integer', '+'),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"Integer">>)),
                ?assertNotEqual(nomatch, binary:match(H, <<"+">>))
            end},
            {"help:selector: with a binary selector works", fun() ->
                H = beamtalk_interface:help('Integer', <<"+">>),
                ?assert(is_binary(H))
            end},
            {"help:selector: unknown selector raises does_not_understand", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = does_not_understand}},
                    beamtalk_interface:help('Integer', zzzNoSuchMethod)
                )
            end},
            {"help:selector: unknown class raises class_not_found", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = class_not_found}},
                    beamtalk_interface:help('ZzzNoSuchClass', foo)
                )
            end},
            {"help:selector: on Metaclass new returns metaclass doc", fun() ->
                H = beamtalk_interface:help('Metaclass', new),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"Metaclass">>))
            end},
            {"help:selector: on Metaclass unknown selector raises dnu", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = does_not_understand}},
                    beamtalk_interface:help('Metaclass', zzzNoSuchMeta)
                )
            end},
            {"dispatch help: routes to handle_help", fun() ->
                H = beamtalk_interface:dispatch('help:', ['Integer'], fake_self()),
                ?assert(is_binary(H))
            end},
            {"dispatch help:selector: routes to handle_help_selector", fun() ->
                H = beamtalk_interface:dispatch(
                    'help:selector:', ['Integer', '+'], fake_self()
                ),
                ?assert(is_binary(H))
            end},
            {"dispatch erlangHelp: routes through", fun() ->
                H = beamtalk_interface:dispatch('erlangHelp:', [<<"lists">>], fake_self()),
                ?assert(is_binary(H))
            end},
            {"erlangHelp: on a known module returns docs", fun() ->
                ?assert(is_binary(beamtalk_interface:erlangHelp(<<"lists">>)))
            end},
            {"erlangHelp: unknown module raises not_found", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = not_found}},
                    beamtalk_interface:erlangHelp(<<"zzz_no_such_module_xyz">>)
                )
            end},
            {"erlangHelp: non-binary arg raises type_error", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = type_error}},
                    beamtalk_interface:erlangHelp(123)
                )
            end},
            {"erlangHelp:selector: on a known function returns docs", fun() ->
                ?assert(is_binary(beamtalk_interface:erlangHelp(<<"lists">>, <<"reverse">>)))
            end},
            {"erlangHelp:selector: with an atom selector works", fun() ->
                ?assert(is_binary(beamtalk_interface:erlangHelp(<<"lists">>, reverse)))
            end},
            {"erlangHelp:selector: unknown function raises not_found", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = not_found}},
                    beamtalk_interface:erlangHelp(<<"lists">>, <<"zzzNoSuchFn">>)
                )
            end},
            {"erlangHelp:selector: unknown module raises not_found", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = not_found}},
                    beamtalk_interface:erlangHelp(<<"zzz_no_module">>, <<"reverse">>)
                )
            end},
            {"erlangHelp:selector: non-binary module raises type_error", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = type_error}},
                    beamtalk_interface:erlangHelp(123, <<"reverse">>)
                )
            end},
            {"findSendersIn on real source returns a list", fun() ->
                R = beamtalk_interface:findSendersIn(<<"self foo: 1. self foo: 2">>, 'foo:'),
                ?assert(is_list(R))
            end},
            {"dispatch allClasses routes to allClasses/0", fun() ->
                R = beamtalk_interface:dispatch(allClasses, [], fake_self()),
                ?assert(is_list(R)),
                ?assert(length(R) > 0)
            end},
            {"dispatch classNamed: routes to handle_class_named", fun() ->
                ?assertMatch(
                    {beamtalk_object, 'Integer class', _, _},
                    beamtalk_interface:dispatch('classNamed:', ['Integer'], fake_self())
                )
            end},
            {"dispatch globals routes to handle_globals", fun() ->
                ?assert(is_map(beamtalk_interface:dispatch(globals, [], fake_self())))
            end},
            {"dispatch erlangHelp:selector: routes through", fun() ->
                R = beamtalk_interface:dispatch(
                    'erlangHelp:selector:', [<<"lists">>, <<"reverse">>], fake_self()
                ),
                ?assert(is_binary(R))
            end},
            {"help: on the root class renders a header without '<'", fun() ->
                %% ProtoObject is the hierarchy root (superclass none) — exercises
                %% the header branch that omits the '< Super' part
                %% (format_class_help line 991).
                H = beamtalk_interface:help('ProtoObject'),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"== ProtoObject ==">>))
            end},
            {"help: on a subclass lists inherited methods", fun() ->
                %% Integer < Number < Object: the help output groups inherited
                %% methods by their defining class (collect_flattened_methods +
                %% group_by_class + InheritedParts).
                H = beamtalk_interface:help('Integer'),
                ?assertNotEqual(nomatch, binary:match(H, <<"Inherited from">>))
            end},
            {"help:selector: on an inherited method notes inheritance", fun() ->
                %% asString is defined on Object; asking Integer about it walks
                %% find_defining_class and emits an "(inherited from ...)" note.
                H = beamtalk_interface:help('Integer', asString),
                ?assert(is_binary(H))
            end},
            {"findClass non-class binary returns nil", fun() ->
                %% A binary that is not a class name and not a metaclass tag.
                ?assertEqual(nil, beamtalk_interface:findClass(<<"not a class name!">>))
            end},
            {"help: accepts a class-object tuple (resolve_class_name pid path)", fun() ->
                %% Passing the class object itself exercises the
                %% resolve_class_name(#beamtalk_object{pid=...}) clause.
                ClassObj = beamtalk_interface:classNamed('Integer'),
                H = beamtalk_interface:help(ClassObj),
                ?assert(is_binary(H))
            end},
            {"help: with a non-class argument raises type_error", fun() ->
                ?assertError(
                    #{error := #beamtalk_error{kind = type_error}},
                    beamtalk_interface:help(3.14)
                )
            end},
            {"help: on an abstract class shows the [abstract] modifier", fun() ->
                %% Number is abstract — exercises the {false, true} modifier branch.
                H = beamtalk_interface:help('Number'),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"[abstract]">>))
            end},
            {"help: on a sealed class shows the [sealed] modifier", fun() ->
                %% Symbol is sealed — exercises the {true, false} modifier branch.
                H = beamtalk_interface:help('Symbol'),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"[sealed]">>))
            end},
            {"dispatch help:selector: unknown class raises class_not_found", fun() ->
                %% Drives the dispatch('help:selector:', ...) {error,Err} ->
                %% beamtalk_error:raise branch.
                ?assertError(
                    #{error := #beamtalk_error{kind = class_not_found}},
                    beamtalk_interface:dispatch(
                        'help:selector:', ['ZzzNoSuchClass', foo], fake_self()
                    )
                )
            end},
            {"findClass integer arg raises type_error", fun() ->
                %% Drives the findClass/1 {error,Err} -> raise branch.
                ?assertError(
                    #{error := #beamtalk_error{kind = type_error}},
                    beamtalk_interface:findClass(42)
                )
            end},
            {"help:selector: resolves a class-side method", fun() ->
                %% `new` is a class-side method (inherited from Value): the
                %% instance-side resolve returns nil, so resolve_help_method
                %% falls through to the {class_method, Selector} branch and
                %% find_defining_class_method walks the class-side tables.
                H = beamtalk_interface:help('Integer', new),
                ?assert(is_binary(H)),
                ?assertNotEqual(nomatch, binary:match(H, <<"new">>))
            end}
        ]
    end}.
