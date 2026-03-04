%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for BT-999: undef classification in class method dispatch.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Tests that invoke_class_method converts dispatch-level `error:undef` into
%%% structured `#beamtalk_error{}` records, distinguishing:
%%%
%%%   - Module not loaded  → `class_not_found` error
%%%   - Method not defined → `does_not_understand` error
%%%
%%% Tests exercise the full dispatch path via `gen_server:call/2`, covering:
%%%   `handle_class_method_call → invoke_class_method → is_dispatch_undef`.
%%%
%%% Note: the inherited-method hint path (ClassName ≠ DefiningClass) is verified
%%% at the language level by ClassHierarchyQueryTest in `stdlib/test/`.

-module(beamtalk_class_dispatch_tests).

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% Selector used in tests.
-define(TEST_SELECTOR, bt999_dispatch_test_method).

%%% ============================================================================
%%% Setup / Teardown
%%% ============================================================================

setup() ->
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    [].

teardown(Pids) ->
    lists:foreach(
        fun(Pid) -> catch gen_server:stop(Pid, normal, 5000) end,
        Pids
    ).

%%% ============================================================================
%%% Helpers
%%% ============================================================================

%% @doc Start a minimal class gen_server whose only class method is ?TEST_SELECTOR
%% and whose implementation module is Module (which may or may not be loaded).
start_test_class(ClassName, Module) ->
    ClassInfo = #{
        superclass => none,
        module => Module,
        class_methods => #{?TEST_SELECTOR => <<>>},
        class_state => #{}
    },
    beamtalk_object_class:start_link(ClassName, ClassInfo).

%%% ============================================================================
%%% Tests
%%% ============================================================================

dispatch_undef_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            {"module not loaded returns class_not_found error", fun test_module_not_loaded/0},
            {"method not found returns does_not_understand error", fun test_method_not_found/0}
        ]
    end}.

%% @doc When the class module is not loaded, `{class_method_call, …}` returns
%% a structured `class_not_found` error instead of a raw Erlang `undef`.
test_module_not_loaded() ->
    ClassName = 'BT999ModuleNotLoadedTest',
    %% bt999_not_loaded_module_bt999 is deliberately never loaded.
    {ok, Pid} = start_test_class(ClassName, bt999_not_loaded_module_bt999),
    try
        Result = gen_server:call(Pid, {class_method_call, ?TEST_SELECTOR, []}),
        ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result),
        %% The hint must name the receiver class.
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(binary:match(Hint, atom_to_binary(ClassName, utf8)) =/= nomatch)
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.

%% @doc When the module IS loaded but the class method function is absent,
%% `{class_method_call, …}` returns `does_not_understand` instead of raw undef.
test_method_not_found() ->
    ClassName = 'BT999MethodNotFoundTest',
    %% `erlang` is always loaded; `erlang:class_bt999_dispatch_test_method/2`
    %% does not exist, so erlang:apply raises undef at the dispatch call site.
    {ok, Pid} = start_test_class(ClassName, erlang),
    try
        Result = gen_server:call(Pid, {class_method_call, ?TEST_SELECTOR, []}),
        ?assertMatch({error, #beamtalk_error{kind = does_not_understand}}, Result),
        %% The hint must name the selector.
        {error, #beamtalk_error{hint = Hint}} = Result,
        ?assert(
            binary:match(Hint, atom_to_binary(?TEST_SELECTOR, utf8)) =/= nomatch
        )
    after
        catch gen_server:stop(Pid, normal, 5000)
    end.
