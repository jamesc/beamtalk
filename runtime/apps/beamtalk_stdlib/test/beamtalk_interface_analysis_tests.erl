%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface_analysis_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for beamtalk_interface: erlangHelp paths and source-analysis
degradation paths (BT-2248).

Tests cover:
- erlangHelp/1,2 and dispatch('erlangHelp:', ...) — happy path and error paths
- findSendersIn/2 — type guard and compiler-unavailable degradation
- allSendsIn/1   — type guard and compiler-unavailable degradation
- findReferencesToIn/2 — type guard and compiler-unavailable degradation
- findFieldReadersIn/2 / findFieldWritersIn/2 — type guard and degradation
- ffiSitesIn/4 — type guard and compiler-unavailable degradation
- log_compiler_diagnostics/2 — port-unavailable (ERROR) and parse-warn (WARNING) paths
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Fake Self — BeamtalkInterface primitives ignore Self entirely.
fake_self() ->
    {beamtalk_object, 'BeamtalkInterface class', 'bt@stdlib@beamtalk_interface', self()}.

%%====================================================================
%% erlangHelp: dispatch — happy path
%%====================================================================

erlang_help_dispatch_known_module_test() ->
    Result = beamtalk_interface:dispatch('erlangHelp:', [<<"lists">>], fake_self()),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"lists">>)).

erlang_help_dispatch_known_module_selector_atom_test() ->
    Result = beamtalk_interface:dispatch(
        'erlangHelp:selector:', [<<"lists">>, reverse], fake_self()
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"reverse">>)).

erlang_help_dispatch_known_module_selector_binary_test() ->
    Result = beamtalk_interface:dispatch(
        'erlangHelp:selector:', [<<"lists">>, <<"reverse">>], fake_self()
    ),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"reverse">>)).

%%====================================================================
%% erlangHelp: dispatch — error paths
%%====================================================================

erlang_help_dispatch_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:', [42], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

erlang_help_dispatch_unknown_module_raises_not_found_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:', [<<"no_such_module_xyz99">>], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

erlang_help_selector_dispatch_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:selector:', [42, reverse], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_selector_dispatch_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:dispatch('erlangHelp:selector:', [<<"lists">>, 42], fake_self()),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% erlangHelp/1 direct export
%%====================================================================

erlang_help_1_known_module_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>),
    ?assert(is_binary(Result)),
    ?assertNotEqual(nomatch, binary:match(Result, <<"lists">>)).

erlang_help_1_non_binary_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(lists),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

erlang_help_1_unknown_module_raises_not_found_test() ->
    try
        beamtalk_interface:erlangHelp(<<"no_such_module_xyz99">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% erlangHelp/2 direct export
%%====================================================================

erlang_help_2_atom_selector_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>, reverse),
    ?assert(is_binary(Result)).

erlang_help_2_binary_selector_test() ->
    Result = beamtalk_interface:erlangHelp(<<"lists">>, <<"reverse">>),
    ?assert(is_binary(Result)).

erlang_help_2_non_binary_module_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(42, reverse),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_2_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:erlangHelp(<<"lists">>, 42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

erlang_help_2_unknown_function_raises_not_found_test() ->
    try
        beamtalk_interface:erlangHelp(<<"lists">>, no_such_function_xyz),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(not_found, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findSendersIn/2 — compiler-unavailable degradation
%%
%% When beamtalk_compiler_server is not running (as in unit-test context),
%% beamtalk_compiler_server catches the noproc exit and returns
%% {error, [#{message => <<"Compiler server is not available">>}]}.
%% beamtalk_interface:findSendersIn/2 must then log and return [].
%%====================================================================

find_senders_in_binary_source_atom_selector_degrades_to_empty_test() ->
    Result = beamtalk_interface:findSendersIn(<<"x printNl">>, send),
    ?assertEqual([], Result).

find_senders_in_binary_source_binary_selector_degrades_to_empty_test() ->
    Result = beamtalk_interface:findSendersIn(<<"x printNl">>, <<"send">>),
    ?assertEqual([], Result).

find_senders_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findSendersIn(42, <<"sel">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

find_senders_in_integer_selector_raises_type_error_test() ->
    try
        beamtalk_interface:findSendersIn(<<"source">>, 42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% allSendsIn/1 — compiler-unavailable degradation
%%====================================================================

all_sends_in_binary_source_degrades_to_empty_test() ->
    Result = beamtalk_interface:allSendsIn(<<"x printNl">>),
    ?assertEqual([], Result).

all_sends_in_non_binary_raises_type_error_test() ->
    try
        beamtalk_interface:allSendsIn(42),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind),
            ?assertEqual('BeamtalkInterface', Err#beamtalk_error.class)
    end.

%%====================================================================
%% findReferencesToIn/2 — compiler-unavailable degradation
%%====================================================================

find_references_to_in_atom_class_degrades_to_empty_test() ->
    Result = beamtalk_interface:findReferencesToIn(<<"x := MyClass new">>, 'MyClass'),
    ?assertEqual([], Result).

find_references_to_in_binary_class_degrades_to_empty_test() ->
    Result = beamtalk_interface:findReferencesToIn(<<"x := MyClass new">>, <<"MyClass">>),
    ?assertEqual([], Result).

find_references_to_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findReferencesToIn(42, <<"MyClass">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findFieldReadersIn/2 — compiler-unavailable degradation
%%====================================================================

find_field_readers_in_atom_field_degrades_to_empty_test() ->
    Result = beamtalk_interface:findFieldReadersIn(<<"^ self.value">>, value),
    ?assertEqual([], Result).

find_field_readers_in_binary_field_degrades_to_empty_test() ->
    Result = beamtalk_interface:findFieldReadersIn(<<"^ self.value">>, <<"value">>),
    ?assertEqual([], Result).

find_field_readers_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findFieldReadersIn(42, <<"field">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% findFieldWritersIn/2 — compiler-unavailable degradation
%%====================================================================

find_field_writers_in_atom_field_degrades_to_empty_test() ->
    Result = beamtalk_interface:findFieldWritersIn(<<"self.value := 42">>, value),
    ?assertEqual([], Result).

find_field_writers_in_binary_field_degrades_to_empty_test() ->
    Result = beamtalk_interface:findFieldWritersIn(<<"self.value := 42">>, <<"value">>),
    ?assertEqual([], Result).

find_field_writers_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:findFieldWritersIn(42, <<"field">>),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% ffiSitesIn/4 — compiler-unavailable degradation
%%====================================================================

ffi_sites_in_integer_arity_degrades_to_empty_test() ->
    Result = beamtalk_interface:ffiSitesIn(<<"(Erlang lists) reverse: x">>, lists, reverse, 1),
    ?assertEqual([], Result).

ffi_sites_in_atom_any_arity_degrades_to_empty_test() ->
    Result = beamtalk_interface:ffiSitesIn(
        <<"(Erlang lists) reverse: x">>, <<"lists">>, <<"reverse">>, any
    ),
    ?assertEqual([], Result).

ffi_sites_in_non_binary_source_raises_type_error_test() ->
    try
        beamtalk_interface:ffiSitesIn(42, lists, reverse, 1),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

ffi_sites_in_negative_arity_raises_type_error_test() ->
    try
        beamtalk_interface:ffiSitesIn(<<"source">>, lists, reverse, -1),
        ?assert(false)
    catch
        error:#{error := Err} ->
            ?assertEqual(type_error, Err#beamtalk_error.kind)
    end.

%%====================================================================
%% log_compiler_diagnostics/2 — TEST export (BT-2219)
%%
%% Verifies the function returns ok without crashing on all diagnostic shapes.
%% The actual log level (ERROR vs WARNING) is not asserted — we verify
%% classification by confirming the call completes without exception.
%%====================================================================

log_compiler_diagnostics_port_unavailable_returns_ok_test() ->
    Diag = #{message => <<"Compiler server is not available">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_timed_out_returns_ok_test() ->
    Diag = #{message => <<"Compiler server timed out">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_parse_warning_returns_ok_test() ->
    Diag = #{message => <<"parse error on line 3">>},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_empty_list_returns_ok_test() ->
    Result = beamtalk_interface:log_compiler_diagnostics([], 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_multiple_diagnostics_returns_ok_test() ->
    Diags = [
        #{message => <<"parse error">>},
        #{message => <<"undefined variable x">>}
    ],
    Result = beamtalk_interface:log_compiler_diagnostics(Diags, 'test:'),
    ?assertEqual(ok, Result).

log_compiler_diagnostics_non_binary_message_returns_ok_test() ->
    Diag = #{message => some_atom},
    Result = beamtalk_interface:log_compiler_diagnostics([Diag], 'test:'),
    ?assertEqual(ok, Result).
