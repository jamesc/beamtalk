%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Primitive dispatch for the Subprocess actor class (ADR 0051, BT-1132).
%%%
%%% **DDD Context:** runtime
%%%
%%% Implements class-side `@primitive` methods for `Subprocess`. The compiled
%%% `bt@stdlib@subprocess:dispatch/3` delegates here for any selector that
%%% resolves to an `@primitive` annotation.
%%%
%%% Instance method calls from external Beamtalk code bypass `bt@stdlib@subprocess`
%%% entirely: `beamtalk_message_dispatch:send/3` extracts the PID from the
%%% `#beamtalk_object{}` and calls `beamtalk_actor:sync_send/3` directly against
%%% the `beamtalk_subprocess` gen_server. This module therefore only handles
%%% class-side selectors (`open:args:`, `open:args:env:dir:`).
%%%
%%% == Selectors ==
%%%
%%% * `'open:args:'` — start subprocess, return actor reference
%%% * `'open:args:env:dir:'` — start subprocess with env and working directory

-module(beamtalk_subprocess_primitives).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).

%%% ============================================================================
%%% dispatch/3 — called from bt@stdlib@subprocess for @primitive methods
%%% ============================================================================

%% @doc Dispatch a primitive method for Subprocess.
%%
%% Class-side selectors start a `beamtalk_subprocess` gen_server and return a
%% `#beamtalk_object{}` wrapping its PID. All subsequent instance method calls
%% (readLine, writeLine:, etc.) go directly to that gen_server.
-spec dispatch(atom(), list(), term()) -> term().

dispatch('open:args:', [Command, Args], _ClassSelf) when is_binary(Command) ->
    ArgsList = bt_array_to_list(Args),
    start_subprocess(#{executable => Command, args => ArgsList}, 'open:args:');
dispatch('open:args:', [_Command, _Args], _ClassSelf) ->
    Err = beamtalk_error:new(type_error, 'Subprocess', 'open:args:'),
    beamtalk_error:raise(Err);
dispatch('open:args:env:dir:', [Command, Args, Env, Dir], _ClassSelf) when
    is_binary(Command), is_binary(Dir), is_map(Env)
->
    ArgsList = bt_array_to_list(Args),
    EnvMap = maps:without(['$beamtalk_class'], Env),
    start_subprocess(
        #{executable => Command, args => ArgsList, env => EnvMap, dir => Dir},
        'open:args:env:dir:'
    );
dispatch('open:args:env:dir:', [_Command, _Args, _Env, _Dir], _ClassSelf) ->
    Err = beamtalk_error:new(type_error, 'Subprocess', 'open:args:env:dir:'),
    beamtalk_error:raise(Err);
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'Subprocess'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(Err1).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

%% @private Start a supervised beamtalk_subprocess gen_server and return a beamtalk_object.
-spec start_subprocess(map(), atom()) -> #beamtalk_object{}.
start_subprocess(Config, Selector) ->
    case beamtalk_subprocess_sup:start_child(Config) of
        {ok, Pid} ->
            #beamtalk_object{
                class = 'Subprocess',
                class_mod = 'bt@stdlib@subprocess',
                pid = Pid
            };
        {error, Reason} ->
            Err0 = beamtalk_error:new(runtime_error, 'Subprocess', Selector),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(io_lib:format("Failed to start subprocess: ~p", [Reason]))
            ),
            beamtalk_error:raise(Err1)
    end.

%% @private Convert a Beamtalk Array (tagged map or plain list) to an Erlang list.
%%
%% Array literals in Beamtalk method call arguments compile to plain Erlang lists.
%% Array values returned from collection operations are tagged maps.
-spec bt_array_to_list(term()) -> list().
bt_array_to_list(#{'$beamtalk_class' := 'Array', 'data' := Arr}) ->
    case array:is_array(Arr) of
        true ->
            array:to_list(Arr);
        false ->
            Err = beamtalk_error:new(type_error, 'Subprocess'),
            beamtalk_error:raise(Err)
    end;
bt_array_to_list(List) when is_list(List) ->
    List;
bt_array_to_list(_Other) ->
    Err = beamtalk_error:new(type_error, 'Subprocess'),
    beamtalk_error:raise(Err).
