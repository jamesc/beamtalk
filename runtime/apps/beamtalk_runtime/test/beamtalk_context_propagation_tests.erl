%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for propagated context across actor boundaries (ADR 0069 Phase 2b).
%%%
%%% Tests cover:
%%% - get_propagated_ctx/0 returns a map with otel key
%%% - restore_propagated_ctx/1 is a no-op when OTel is not loaded
%%% - restore_propagated_ctx/1 with undefined otel context
%%% - erlang:function_exported guard works correctly
%%% - Context is attached to sync_send messages (3-tuple format)
%%% - Context is attached to async_send messages (4-tuple format)
%%% - Context is attached to cast_send messages (4-tuple format)
-module(beamtalk_context_propagation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Helpers
%%====================================================================

setup() ->
    %% Ensure instance registry is available
    case whereis(beamtalk_object_instances) of
        undefined ->
            {ok, _} = beamtalk_object_instances:start_link();
        _ ->
            ok
    end,
    ok.

cleanup(_) ->
    ok.

%% @doc Start a test counter actor and register it in the instance registry.
start_test_counter() ->
    start_test_counter(0).

start_test_counter(InitialValue) ->
    {ok, Pid} = beamtalk_test_native_counter:start_link(#{initial => InitialValue}),
    beamtalk_object_instances:register('TestCounter', Pid),
    Pid.

stop_test_counter(Pid) ->
    try
        gen_server:stop(Pid)
    catch
        exit:_ -> ok
    end.

%%====================================================================
%% get_propagated_ctx/0 tests
%%====================================================================

get_propagated_ctx_returns_map_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"returns a map with otel key", fun() ->
            Ctx = beamtalk_actor:get_propagated_ctx(),
            ?assert(is_map(Ctx)),
            ?assert(maps:is_key(otel, Ctx))
        end},
        {"otel value is undefined when otel_ctx not loaded", fun() ->
            %% otel_ctx is not loaded in test env
            ?assertNot(erlang:function_exported(otel_ctx, get_current, 0)),
            Ctx = beamtalk_actor:get_propagated_ctx(),
            ?assertEqual(undefined, maps:get(otel, Ctx))
        end}
    ]}.

%%====================================================================
%% restore_propagated_ctx/1 tests
%%====================================================================

restore_propagated_ctx_noop_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"no-op with undefined otel context", fun() ->
            ?assertEqual(ok, beamtalk_actor:restore_propagated_ctx(#{otel => undefined}))
        end},
        {"no-op with empty map", fun() ->
            ?assertEqual(ok, beamtalk_actor:restore_propagated_ctx(#{}))
        end},
        {"no-op when otel_ctx not loaded", fun() ->
            %% Even with a non-undefined otel value, restore is a no-op
            %% because otel_ctx:attach/1 is not exported
            ?assertNot(erlang:function_exported(otel_ctx, attach, 1)),
            ?assertEqual(ok, beamtalk_actor:restore_propagated_ctx(#{otel => some_context}))
        end}
    ]}.

%%====================================================================
%% erlang:function_exported guard tests
%%====================================================================

function_exported_guard_test_() ->
    [
        {"otel_ctx:get_current/0 is not exported in test env", fun() ->
            ?assertNot(erlang:function_exported(otel_ctx, get_current, 0))
        end},
        {"otel_ctx:attach/1 is not exported in test env", fun() ->
            ?assertNot(erlang:function_exported(otel_ctx, attach, 1))
        end},
        {"beamtalk_actor:get_propagated_ctx/0 is exported", fun() ->
            ?assert(erlang:function_exported(beamtalk_actor, get_propagated_ctx, 0))
        end},
        {"beamtalk_actor:restore_propagated_ctx/1 is exported", fun() ->
            ?assert(erlang:function_exported(beamtalk_actor, restore_propagated_ctx, 1))
        end}
    ].

%%====================================================================
%% sync_send attaches PropCtx (3-tuple message format)
%%====================================================================

sync_send_with_context_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"sync_send delivers message and works with PropCtx", fun() ->
            Pid = start_test_counter(10),
            %% sync_send now attaches PropCtx — verify it still works
            Result = beamtalk_actor:sync_send(Pid, 'getValue', []),
            ?assertEqual(10, Result),
            stop_test_counter(Pid)
        end},
        {"sync_send increment works with PropCtx", fun() ->
            Pid = start_test_counter(5),
            Result = beamtalk_actor:sync_send(Pid, increment, []),
            ?assertEqual(6, Result),
            stop_test_counter(Pid)
        end}
    ]}.

%%====================================================================
%% cast_send attaches PropCtx (4-tuple message format)
%%====================================================================

cast_send_with_context_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"cast_send attaches PropCtx to cast message", fun() ->
            %% Verify the message format includes PropCtx by sending to a
            %% collector process and inspecting the raw message.
            Self = self(),
            Collector = spawn(fun() ->
                receive
                    {'$gen_cast', Msg} ->
                        Self ! {collected, Msg}
                after 2000 ->
                    Self ! {collected, timeout}
                end
            end),
            beamtalk_actor:cast_send(Collector, 'testSelector', [arg1]),
            receive
                {collected, {cast, 'testSelector', [arg1], PropCtx}} when is_map(PropCtx) ->
                    ?assert(maps:is_key(otel, PropCtx));
                {collected, Other} ->
                    ?assertEqual({expected_4_tuple_with_propctx}, Other)
            after 2000 ->
                ?assert(false)
            end
        end}
    ]}.

%%====================================================================
%% async_send attaches PropCtx (4-tuple message format)
%%====================================================================

async_send_with_context_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"async_send attaches PropCtx to cast message", fun() ->
            %% Verify the message format includes PropCtx by sending to a
            %% collector process and inspecting the raw message.
            Self = self(),
            Collector = spawn(fun() ->
                receive
                    {'$gen_cast', Msg} ->
                        Self ! {collected, Msg}
                after 2000 ->
                    Self ! {collected, timeout}
                end
            end),
            FuturePid = spawn(fun() ->
                receive
                    _ -> ok
                end
            end),
            beamtalk_actor:async_send(Collector, 'testSelector', [arg1], FuturePid),
            receive
                {collected, {_Selector, _Args, _FuturePid, PropCtx}} when is_map(PropCtx) ->
                    ?assert(maps:is_key(otel, PropCtx));
                {collected, Other} ->
                    ?assertEqual({expected_4_tuple_with_propctx}, Other)
            after 2000 ->
                ?assert(false)
            end,
            exit(FuturePid, kill)
        end}
    ]}.

%%====================================================================
%% Context propagation with mock otel_ctx module
%%====================================================================

mock_otel_ctx_test_() ->
    {setup,
        fun() ->
            setup(),
            %% Create a mock otel_ctx module using Abstract Erlang Forms.
            %% This tests that get_propagated_ctx calls otel_ctx:get_current/0
            %% when available, and restore_propagated_ctx calls otel_ctx:attach/1.
            Forms = [
                %% -module(otel_ctx).
                {attribute, 1, module, otel_ctx},
                %% -export([get_current/0, attach/1]).
                {attribute, 2, export, [{get_current, 0}, {attach, 1}]},
                %% get_current() -> {mock_trace_id, 12345}.
                {function, 3, get_current, 0, [
                    {clause, 3, [], [], [
                        {tuple, 3, [{atom, 3, mock_trace_id}, {integer, 3, 12345}]}
                    ]}
                ]},
                %% attach(Ctx) -> erlang:put(otel_ctx_attached, Ctx), ok.
                {function, 4, attach, 1, [
                    {clause, 4, [{var, 4, 'Ctx'}], [], [
                        {call, 4, {remote, 4, {atom, 4, erlang}, {atom, 4, put}}, [
                            {atom, 4, otel_ctx_attached}, {var, 4, 'Ctx'}
                        ]},
                        {atom, 4, ok}
                    ]}
                ]}
            ],
            {ok, otel_ctx, Bin} = compile:forms(Forms, []),
            {module, otel_ctx} = code:load_binary(otel_ctx, "otel_ctx.beam", Bin),
            ok
        end,
        fun(_) ->
            %% Unload mock module
            code:purge(otel_ctx),
            code:delete(otel_ctx),
            code:purge(otel_ctx),
            ok
        end,
        [
            {"get_propagated_ctx captures OTel context when otel_ctx loaded", fun() ->
                ?assert(erlang:function_exported(otel_ctx, get_current, 0)),
                Ctx = beamtalk_actor:get_propagated_ctx(),
                ?assertEqual({mock_trace_id, 12345}, maps:get(otel, Ctx))
            end},
            {"restore_propagated_ctx attaches OTel context when otel_ctx loaded", fun() ->
                ?assert(erlang:function_exported(otel_ctx, attach, 1)),
                MockCtx = {mock_trace_id, 99999},
                ok = beamtalk_actor:restore_propagated_ctx(#{otel => MockCtx}),
                ?assertEqual(MockCtx, erlang:get(otel_ctx_attached))
            end},
            {"restore_propagated_ctx no-op for undefined even with otel_ctx loaded", fun() ->
                erlang:erase(otel_ctx_attached),
                ok = beamtalk_actor:restore_propagated_ctx(#{otel => undefined}),
                ?assertEqual(undefined, erlang:get(otel_ctx_attached))
            end}
        ]}.
