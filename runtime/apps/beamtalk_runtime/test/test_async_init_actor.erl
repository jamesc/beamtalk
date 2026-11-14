%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_async_init_actor).
-behaviour(gen_server).

%%% **DDD Context:** Actor System Context

-moduledoc """
BT-4477: hand-written actor for `beamtalk_actor:safe_spawn_monitored_async/2`.
`mode` in the start args picks what `init/1` answers:

- `ok` — `{ok, State}`.
- `{continue, Step}` — `{ok, State, {continue, Step}}`; the step sleeps
  `delay` ms (default 0) then records `Step` in the state.
- `hibernate` — `{ok, State, hibernate}`.
- `timeout` — `{ok, State, 20}`: `handle_info(timeout, _)` stops the actor
  with reason `timed_out`.
- `stop` — `{stop, init_refused}`.
- `ignore` — `ignore`.
- `bad_return` — an invalid `init/1` result.
- `crash` — `init/1` raises `error:boom`.
""".

-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

init(#{mode := ok} = Args) ->
    {ok, Args};
init(#{mode := {continue, Step}} = Args) ->
    {ok, Args, {continue, Step}};
init(#{mode := hibernate} = Args) ->
    {ok, Args, hibernate};
init(#{mode := timeout} = Args) ->
    {ok, Args, 20};
init(#{mode := stop}) ->
    {stop, init_refused};
init(#{mode := ignore}) ->
    ignore;
init(#{mode := bad_return}) ->
    not_a_valid_init_result;
init(#{mode := crash}) ->
    erlang:error(boom).

handle_continue(Step, #{delay := Delay} = State) ->
    %% sleep: simulates slow async init (class 2)
    timer:sleep(Delay),
    {noreply, State#{done => Step}};
handle_continue(Step, State) ->
    {noreply, State#{done => Step}}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {stop, timed_out, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
