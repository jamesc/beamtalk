%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_op_term_contract_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Pins the documented term contract of the operation layer (BT-5201,
`docs/repl-protocol.md` § "Operation-layer term contract") for a
distribution-attached caller such as the LiveView front.

Every assertion goes through `beamtalk_repl_ops:request/3`, the entry point
that client uses, and pins:

* `eval` → `{ok, Value, Output, Warnings}`, a runtime failure →
  `{error, #beamtalk_error{}, Output, Warnings}`;
* `inspect` → `{inspect, FieldMap}` with live atom keys (no JSON step);
* the `#beamtalk_error{}` fields a client may match on (`kind`, `class`,
  `selector`, `message`, `hint`, `details`);
* `request/3` is `dispatch/4` over a request with no envelope;
* `render/2`, the one rendering function.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%====================================================================
%% Fixtures
%%====================================================================

setup(SessionId) ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_boot:compiler(),
    ok = beamtalk_test_wait:class('Object'),
    {ok, SessionPid} = beamtalk_repl_shell:start_link(SessionId),
    SessionPid.

teardown(SessionPid) ->
    try
        beamtalk_repl_shell:stop(SessionPid)
    catch
        _:_ -> ok
    end,
    _ = application:stop(beamtalk_compiler),
    ok.

%%====================================================================
%% eval
%%====================================================================

eval_success_shape_test_() ->
    {setup, fun() -> setup(<<"term-contract-eval">>) end, fun teardown/1, fun(Session) ->
        [
            ?_test(begin
                Result = beamtalk_repl_ops:request(
                    <<"eval">>, #{<<"code">> => <<"1 + 2">>}, Session
                ),
                %% {ok, Value, Output, Warnings}: Value is the live term (an
                %% integer here, not a printed string), Output the captured
                %% stdout, Warnings a list.
                ?assertMatch(
                    {ok, 3, Output, Warnings} when
                        is_binary(Output) andalso is_list(Warnings),
                    Result
                )
            end)
        ]
    end}.

eval_runtime_error_shape_test_() ->
    {setup, fun() -> setup(<<"term-contract-eval-err">>) end, fun teardown/1, fun(Session) ->
        [
            ?_test(begin
                Result = beamtalk_repl_ops:request(
                    <<"eval">>, #{<<"code">> => <<"1 unknownMsg">>}, Session
                ),
                ?assertMatch(
                    {error, #beamtalk_error{kind = does_not_understand}, Output, Warnings} when
                        is_binary(Output) andalso is_list(Warnings),
                    Result
                )
            end)
        ]
    end}.

%%====================================================================
%% inspect
%%====================================================================

inspect_live_actor_shape_test() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, ActorPid} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    PidBin = list_to_binary(pid_to_list(ActorPid)),
    try
        Result = beamtalk_repl_ops:request(<<"inspect">>, #{<<"actor">> => PidBin}, self()),
        %% {inspect, FieldMap}: a map with the actor's own fields under atom
        %% keys, internal bookkeeping keys filtered out.
        ?assertMatch({inspect, Fields} when is_map(Fields), Result),
        {inspect, Fields} = Result,
        ?assert(maps:is_key(value, Fields)),
        ?assertNot(maps:is_key('$beamtalk_class', Fields))
    after
        unlink(ActorPid),
        exit(ActorPid, kill)
    end.

%%====================================================================
%% #beamtalk_error{}
%%====================================================================

error_record_fields_test() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    %% An invalid actor pid is refused with a structured error term, never an
    %% exception and never JSON. The fields below are the ones a client may
    %% match on; `kind` is the stable discriminator.
    Result = beamtalk_repl_ops:request(<<"inspect">>, #{<<"actor">> => <<"not-a-pid">>}, self()),
    ?assertMatch({error, #beamtalk_error{}}, Result),
    {error, Err} = Result,
    ?assert(is_atom(Err#beamtalk_error.kind)),
    ?assert(is_atom(Err#beamtalk_error.class)),
    ?assert(is_atom(Err#beamtalk_error.selector)),
    ?assertMatch({beamtalk_string, M} when is_binary(M), Err#beamtalk_error.message),
    ?assert(
        Err#beamtalk_error.hint =:= undefined orelse
            element(1, Err#beamtalk_error.hint) =:= beamtalk_string
    ),
    ?assert(is_map(Err#beamtalk_error.details)),
    %% The record is a plain tuple tagged `beamtalk_error` (7-tuple: tag + 6 fields), so a
    %% client without the .hrl matches `{:beamtalk_error, kind, ...}`.
    ?assertEqual(7, tuple_size(Err)),
    ?assertEqual(beamtalk_error, element(1, Err)).

unknown_op_error_kind_test() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = unknown_op}},
        beamtalk_repl_ops:request(<<"no-such-op">>, #{}, self())
    ).

%%====================================================================
%% request/3 is dispatch/4 with no envelope
%%====================================================================

request_equals_dispatch_test() ->
    {ok, Msg} = beamtalk_repl_protocol:decode_map(#{<<"op">> => <<"complete">>, <<"code">> => <<>>}),
    ?assertEqual(
        beamtalk_repl_ops:dispatch(<<"complete">>, #{<<"code">> => <<>>}, Msg, self()),
        beamtalk_repl_ops:request(<<"complete">>, #{<<"code">> => <<>>}, self())
    ),
    ?assertEqual(
        {completions, []}, beamtalk_repl_ops:request(<<"complete">>, #{<<"code">> => <<>>}, self())
    ).

request_drops_envelope_keys_test() ->
    %% `op`, `id` and `session` are the frame's envelope, not params.
    Params = #{
        <<"code">> => <<>>, <<"op">> => <<"ignored">>, <<"id">> => <<"x">>, <<"session">> => <<"y">>
    },
    ?assertEqual({completions, []}, beamtalk_repl_ops:request(<<"complete">>, Params, self())).

%%====================================================================
%% render/2 — the one rendering function
%%====================================================================

render_value_matches_term_to_json_test() ->
    ?assertEqual(beamtalk_repl_json:term_to_json(42), beamtalk_repl_ops:render(value, 42)),
    ?assertEqual(42, beamtalk_repl_ops:render(value, 42)).

render_values_preserves_order_test() ->
    Terms = [1, true, undefined],
    ?assertEqual(
        [beamtalk_repl_json:term_to_json(T) || T <- Terms],
        beamtalk_repl_ops:render(values, Terms)
    ),
    ?assertEqual([], beamtalk_repl_ops:render(values, [])).

render_error_structures_and_formats_test() ->
    %% A raw reason is wrapped into a #beamtalk_error{} and rendered to text.
    {Structured, Message} = beamtalk_repl_ops:render(error, empty_expression),
    ?assertMatch(#beamtalk_error{}, Structured),
    ?assert(is_binary(Message)),
    ?assertEqual(beamtalk_repl_json:format_error_message(empty_expression), Message),
    %% A structured reason passes through unchanged.
    {Same, _} = beamtalk_repl_ops:render(error, Structured),
    ?assertEqual(Structured, Same).
