%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Timer class implementation — delayed and repeating work via Erlang timers.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Provides one-shot delays (`after:do:`), repeating intervals (`every:do:`),
%%% and process sleep (`sleep:`). Timer instances are cancellable value objects
%%% wrapping a spawned process.
%%%
%%% Timer objects are represented as tagged maps:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'Timer',
%%%   pid              => pid()
%%% }
%%% ```
%%%
%%% ## Cancel Protocol
%%%
%%% `cancel/1` uses an acknowledgement-based protocol to avoid TOCTOU races.
%%% The caller sends `{cancel, From, Ref}` and waits for `{Ref, canceled}`.
%%% If the timer process has already exited (fired), a process monitor DOWN
%%% message arrives instead and cancel/1 returns false. A 100 ms fallback
%%% timeout guards against unexpected stalls.

-module(beamtalk_timer).

%% Class methods
-export(['after:do:'/2, 'every:do:'/2, 'sleep:'/1]).

%% Instance methods
-export([cancel/1, 'isActive'/1, 'printString'/1]).

-export([has_method/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

%% @doc Evaluate `Block` once after `Ms` milliseconds. Returns a Timer.
-spec 'after:do:'(integer(), function()) -> map().
'after:do:'(Ms, Block) when is_integer(Ms), Ms >= 0, is_function(Block, 0) ->
    Pid = spawn(fun() ->
        receive
            {cancel, From, Ref} ->
                From ! {Ref, canceled},
                ok
        after Ms ->
            catch Block()
        end
    end),
    make_timer(Pid);
'after:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('after:do:', <<"Duration must be a non-negative Integer">>);
'after:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('after:do:', <<"Duration must be a non-negative Integer">>);
'after:do:'(_, _) ->
    raise_error('after:do:', <<"Expected an Integer duration and a Block">>).

%% @doc Evaluate `Block` every `Ms` milliseconds. Returns a Timer.
-spec 'every:do:'(integer(), function()) -> map().
'every:do:'(Ms, Block) when is_integer(Ms), Ms > 0, is_function(Block, 0) ->
    Pid = spawn(fun() -> repeat_loop(Ms, Block) end),
    make_timer(Pid);
'every:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('every:do:', <<"Duration must be a positive Integer">>);
'every:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('every:do:', <<"Duration must be a positive Integer">>);
'every:do:'(_, _) ->
    raise_error('every:do:', <<"Expected a positive Integer duration and a Block">>).

%% @doc Block the current process for `Ms` milliseconds.
-spec 'sleep:'(integer()) -> 'nil'.
'sleep:'(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    nil;
'sleep:'(_) ->
    raise_error('sleep:', <<"Duration must be a non-negative Integer">>).

%%% ============================================================================
%%% Instance Methods
%%% ============================================================================

%% @doc Cancel this timer. Returns true if cancelled before firing, false if already done.
%%
%% Uses an ack-based protocol: sends {cancel, self(), Ref} and waits for
%% {Ref, canceled}. If the timer process has already exited, the process
%% monitor DOWN message arrives instead and we return false.
-spec cancel(map()) -> boolean().
cancel(#{'$beamtalk_class' := 'Timer', pid := Pid}) ->
    Ref = make_ref(),
    Mon = erlang:monitor(process, Pid),
    Pid ! {cancel, self(), Ref},
    receive
        {Ref, canceled} ->
            erlang:demonitor(Mon, [flush]),
            true;
        {'DOWN', Mon, process, Pid, _Reason} ->
            false
    after 100 ->
        erlang:demonitor(Mon, [flush]),
        false
    end.

%% @doc True if this timer is still scheduled to fire.
-spec 'isActive'(map()) -> boolean().
'isActive'(#{'$beamtalk_class' := 'Timer', pid := Pid}) ->
    erlang:is_process_alive(Pid).

%% @doc Human-readable representation.
-spec 'printString'(map()) -> binary().
'printString'(#{'$beamtalk_class' := 'Timer', pid := Pid} = _Self) ->
    case erlang:is_process_alive(Pid) of
        true -> <<"a Timer(active)">>;
        false -> <<"a Timer(inactive)">>
    end.

%%% ============================================================================
%%% has_method/1
%%% ============================================================================

-spec has_method(atom()) -> boolean().
has_method('cancel') -> true;
has_method('isActive') -> true;
has_method('printString') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_timer(pid()) -> map().
make_timer(Pid) ->
    #{'$beamtalk_class' => 'Timer', pid => Pid}.

-spec repeat_loop(integer(), function()) -> ok.
repeat_loop(Ms, Block) ->
    receive
        {cancel, From, Ref} ->
            From ! {Ref, canceled},
            ok
    after Ms ->
        catch Block(),
        repeat_loop(Ms, Block)
    end.

-spec raise_error(atom(), binary()) -> no_return().
raise_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'Timer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).
