%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_init_ack_actor).
-behaviour(gen_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-3992: hand-written actor that follows the generated-actor startup
protocol — `init/1` takes the starter's ack target with
`beamtalk_actor:take_init_ack/2` and defers to `{continue, initialize}`;
the `initialize` step passes its result through
`beamtalk_actor:ack_initialized/2`. `mode` in the start args picks the
`initialize` step's behaviour:

- `slow_continue` — `initialize` succeeds at once and requests a follow-up
  `{continue, slow}` step that sleeps `delay` ms (a `continueWith:` from
  `initialize`).
- `slow_initialize` — `initialize` itself sleeps `delay` ms.
- `crash` — `initialize` fails (`{stop, {error, function_clause}, _}`).
- `stop_continue` — `initialize` succeeds and requests a `{continue, die}`
  step that stops the actor at once with reason `bad_config` (a
  `self stopWith:` in a continuation).
- `fail_after_first` (BT-4033) — bumps the `counters` ref in `starts` and
  succeeds only on the first start; every later start (an OTP restart,
  which replays the same args) fails its `initialize` like `crash`.
""".

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, terminate/2]).

init(Args) ->
    State = beamtalk_actor:take_init_ack(Args, initializing),
    {ok, State, {continue, initialize}}.

handle_continue(initialize = Continue, #{mode := slow_continue} = State) ->
    beamtalk_actor:ack_initialized(Continue, {noreply, State, {continue, slow}});
handle_continue(initialize = Continue, #{mode := slow_initialize, delay := Delay} = State) ->
    timer:sleep(Delay),
    beamtalk_actor:ack_initialized(Continue, {noreply, State});
handle_continue(initialize = Continue, #{mode := stop_continue} = State) ->
    beamtalk_actor:ack_initialized(Continue, {noreply, State, {continue, die}});
handle_continue(die, State) ->
    {stop, bad_config, State};
handle_continue(initialize = Continue, #{mode := fail_after_first, starts := Starts} = State) ->
    ok = counters:add(Starts, 1, 1),
    case counters:get(Starts, 1) of
        1 -> beamtalk_actor:ack_initialized(Continue, {noreply, State});
        _ -> beamtalk_actor:ack_initialized(Continue, {stop, {error, restart_init_failed}, State})
    end;
handle_continue(initialize = Continue, #{mode := crash} = State) ->
    beamtalk_actor:ack_initialized(Continue, {stop, {error, function_clause}, State});
handle_continue(slow = Continue, #{delay := Delay} = State) ->
    timer:sleep(Delay),
    beamtalk_actor:ack_initialized(Continue, {noreply, State#{slow_done => true}}).

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
