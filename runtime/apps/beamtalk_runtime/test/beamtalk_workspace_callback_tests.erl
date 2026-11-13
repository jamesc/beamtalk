%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_callback_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_workspace_callback` (BT-4792): every wrapper answers
`{error, no_workspace}` with nothing registered, forwards to the registered
module otherwise, and registration is removable.

This module doubles as the fake workspace: it exports the functions the
wrappers forward to and echoes their arguments, so a wrong argument order in a
wrapper fails the test.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
-include("beamtalk.hrl").

%% Fake workspace implementation (forwarded to by the wrappers).
-export([
    reload_class_file/2,
    eval_with_self/2,
    compile_method/6,
    precheck_method/4,
    remove_method/4,
    emit_remove_change_entry/5,
    emit_extension_remove_change_entry/7,
    rewrite_sites/2,
    validate_sites/2,
    emit_rewrite_change_entry/2,
    capture_class_removal_snapshot/1,
    emit_remove_class_change_entry/4
]).

reload_class_file(P, C) -> {echo, reload_class_file, [P, C]}.
eval_with_self(S, Src) -> {echo, eval_with_self, [S, Src]}.
compile_method(A, B, C, D, E, F) -> {echo, compile_method, [A, B, C, D, E, F]}.
precheck_method(A, B, C, D) -> {echo, precheck_method, [A, B, C, D]}.
remove_method(A, B, C, D) -> {echo, remove_method, [A, B, C, D]}.
emit_remove_change_entry(A, B, C, D, E) -> {echo, emit_remove_change_entry, [A, B, C, D, E]}.
emit_extension_remove_change_entry(A, B, C, D, E, F, G) ->
    {echo, emit_extension_remove_change_entry, [A, B, C, D, E, F, G]}.
rewrite_sites(A, B) -> {echo, rewrite_sites, [A, B]}.
validate_sites(A, B) -> {echo, validate_sites, [A, B]}.
emit_rewrite_change_entry(A, B) -> {echo, emit_rewrite_change_entry, [A, B]}.
capture_class_removal_snapshot(A) -> {echo, capture_class_removal_snapshot, [A]}.
emit_remove_class_change_entry(A, B, C, D) ->
    {echo, emit_remove_class_change_entry, [A, B, C, D]}.

%% One entry per wrapper: {Name, Args}.
calls() ->
    [
        {reload_class_file, ["a.bt", 'Foo']},
        {eval_with_self, [self, <<"1">>]},
        {compile_method, [<<"Foo">>, bar, <<"src">>, durable, <<"a">>, human]},
        {precheck_method, [<<"Foo">>, bar, <<"src">>, instance]},
        {remove_method, [<<"Foo">>, bar, class, allow_stdlib]},
        {emit_remove_change_entry, [<<"Foo">>, bar, instance, <<"a">>, agent]},
        {emit_extension_remove_change_entry, [
            <<"Foo">>, bar, instance, 'Owner', <<"prev">>, <<"a">>, human
        ]},
        {rewrite_sites, [undefined, [#{}]]},
        {validate_sites, [undefined, [#{}]]},
        {emit_rewrite_change_entry, [#{k => v}, #{r => s}]},
        {capture_class_removal_snapshot, [<<"Foo">>]},
        {emit_remove_class_change_entry, [<<"Foo">>, #{}, <<"a">>, human]}
    ].

with_registration(Fun) ->
    try
        Fun()
    after
        ok = beamtalk_workspace_callback:unregister()
    end.

%%% Without a workspace

nothing_registered_test() ->
    ok = beamtalk_workspace_callback:unregister(),
    ?assertEqual(none, beamtalk_workspace_callback:registered_module()).

every_wrapper_answers_no_workspace_when_unregistered_test_() ->
    ok = beamtalk_workspace_callback:unregister(),
    [
        ?_assertEqual(
            {error, no_workspace}, erlang:apply(beamtalk_workspace_callback, Name, Args)
        )
     || {Name, Args} <- calls()
    ].

%%% With a workspace

every_wrapper_forwards_to_registered_module_test_() ->
    {setup, fun() -> beamtalk_workspace_callback:register(?MODULE) end,
        fun(_) -> beamtalk_workspace_callback:unregister() end, [
            ?_assertEqual(
                {echo, Name, Args}, erlang:apply(beamtalk_workspace_callback, Name, Args)
            )
         || {Name, Args} <- calls()
        ]}.

register_then_unregister_test() ->
    with_registration(fun() ->
        ok = beamtalk_workspace_callback:register(?MODULE),
        ?assertEqual({ok, ?MODULE}, beamtalk_workspace_callback:registered_module()),
        ok = beamtalk_workspace_callback:unregister(),
        ?assertEqual(none, beamtalk_workspace_callback:registered_module()),
        ?assertEqual(
            {error, no_workspace},
            beamtalk_workspace_callback:remove_method(<<"Foo">>, bar, instance, refuse_stdlib)
        )
    end).

unregister_is_idempotent_test() ->
    ok = beamtalk_workspace_callback:unregister(),
    ?assertEqual(ok, beamtalk_workspace_callback:unregister()).

%%% Callers degrade cleanly without a workspace

inspector_evaluate_without_workspace_is_unavailable_test() ->
    ok = beamtalk_workspace_callback:unregister(),
    ?assertMatch(
        {error, #beamtalk_error{kind = eval_unavailable}},
        beamtalk_inspector:evaluate(#{subject => 1}, <<"self + 1">>)
    ).

raise_no_workspace_raises_structured_error_test() ->
    ?assertBtError(
        no_workspace,
        'Foo',
        bar,
        beamtalk_capability:raise_no_workspace(bar, 'Foo')
    ).

%%% register_if_loaded/1 and require/2 (BT-4792 CI follow-up)

register_if_loaded_registers_a_loadable_module_test() ->
    with_registration(fun() ->
        ok = beamtalk_workspace_callback:unregister(),
        ok = beamtalk_workspace_callback:register_if_loaded(?MODULE),
        ?assertEqual({ok, ?MODULE}, beamtalk_workspace_callback:registered_module())
    end).

register_if_loaded_ignores_a_missing_module_test() ->
    ok = beamtalk_workspace_callback:unregister(),
    ok = beamtalk_workspace_callback:register_if_loaded(bt4792_no_such_module),
    ?assertEqual(none, beamtalk_workspace_callback:registered_module()).

require_fails_closed_without_a_callback_test() ->
    ok = beamtalk_workspace_callback:unregister(),
    ?assertBtError(
        no_workspace,
        'Foo',
        'renameTo:',
        beamtalk_workspace_callback:require('renameTo:', 'Foo')
    ).

require_passes_with_a_callback_test() ->
    with_registration(fun() ->
        ok = beamtalk_workspace_callback:register(?MODULE),
        ?assertEqual(ok, beamtalk_workspace_callback:require('renameTo:', 'Foo'))
    end).

%%% A workspace that stops mid-rename reports the workspace-unavailable error

rename_rewrite_failed_error_maps_no_workspace_like_workspace_unavailable_test() ->
    Unavailable = beamtalk_refactor_support:rename_rewrite_failed_error(
        'Foo', <<"renameTo:">>, <<"class">>, <<"no sites">>, workspace_unavailable
    ),
    NoWorkspace = beamtalk_refactor_support:rename_rewrite_failed_error(
        'Foo', <<"renameTo:">>, <<"class">>, <<"no sites">>, no_workspace
    ),
    ?assertEqual(Unavailable, NoWorkspace).
