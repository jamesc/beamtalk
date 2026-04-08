%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_timer).

%%% **DDD Context:** Object System Context

-moduledoc """
Timer class implementation — delayed and repeating work via Erlang timers.

Provides one-shot delays (`after:do:`), repeating intervals (`every:do:`),
and process sleep (`sleep:`). Timer instances are cancellable value objects
wrapping a spawned process.

Timer objects are represented as tagged maps:
```
#{
  '$beamtalk_class' => 'Timer',
  pid              => pid()
}
```

## Cancel Protocol

`cancel/1` uses an acknowledgement-based protocol to avoid TOCTOU races.
The caller sends `{cancel, From, Ref}` and waits for `{Ref, canceled}`.
If the timer process has already exited (fired), a process monitor DOWN
message arrives instead and cancel/1 returns false. A 100 ms fallback
timeout guards against unexpected stalls.

## FFI Shims

The `beamtalk_erlang_proxy:direct_call/3` dispatch derives the Erlang function
name from the first keyword of the Beamtalk selector (stripping the trailing
colon). The colon-free shims (`after/2`, `every/2`, `sleep/1`) bridge the
gap so that `(Erlang beamtalk_timer) after: ms do: block` dispatches correctly
to the canonical `'after:do:'/2` implementation.
""".

%% Class methods (canonical colon forms — used by EUnit tests)
-export(['after:do:'/2, 'every:do:'/2, 'sleep:'/1]).

%% Instance methods
-export([cancel/1, 'isActive'/1, 'printString'/1]).

%% FFI shims for (Erlang beamtalk_timer) dispatch
-export(['after'/2, every/2, sleep/1]).

-type t() :: #{'$beamtalk_class' := 'Timer', atom() => term()}.
-export_type([t/0]).

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

-doc "Evaluate `Block` once after `Ms` milliseconds. Returns a Timer.".
-spec 'after:do:'(integer(), Do :: function()) -> t().
'after:do:'(Ms, Block) when is_integer(Ms), Ms >= 0, is_function(Block, 0) ->
    Pid = spawn_link(fun() ->
        receive
            {cancel, From, Ref} ->
                From ! {Ref, canceled},
                ok
        after Ms ->
            try
                Block()
            catch
                _:_ -> ok
            end
        end
    end),
    make_timer(Pid);
'after:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('after:do:', <<"Duration must be a non-negative Integer">>);
'after:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('after:do:', <<"Duration must be a non-negative Integer">>);
'after:do:'(_, _) ->
    raise_error('after:do:', <<"Expected an Integer duration and a Block">>).

-doc "Evaluate `Block` every `Ms` milliseconds. Returns a Timer.".
-spec 'every:do:'(integer(), Do :: function()) -> t().
'every:do:'(Ms, Block) when is_integer(Ms), Ms > 0, is_function(Block, 0) ->
    Pid = spawn_link(fun() -> repeat_loop(Ms, Block) end),
    make_timer(Pid);
'every:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('every:do:', <<"Duration must be a positive Integer">>);
'every:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('every:do:', <<"Duration must be a positive Integer">>);
'every:do:'(_, _) ->
    raise_error('every:do:', <<"Expected a positive Integer duration and a Block">>).

-doc "Block the current process for `Ms` milliseconds.".
-spec 'sleep:'(integer()) -> 'nil'.
'sleep:'(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    nil;
'sleep:'(_) ->
    raise_error('sleep:', <<"Duration must be a non-negative Integer">>).

%%% ============================================================================
%%% Instance Methods
%%% ============================================================================

-doc """
Cancel this timer. Returns true if cancelled before firing, false if already done.

Uses an ack-based protocol: sends {cancel, self(), Ref} and waits for
{Ref, canceled}. If the timer process has already exited, the process
monitor DOWN message arrives instead and we return false.
""".
-spec cancel(t()) -> boolean().
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

-doc "True if this timer is still scheduled to fire.".
-spec 'isActive'(t()) -> boolean().
'isActive'(#{'$beamtalk_class' := 'Timer', pid := Pid}) ->
    erlang:is_process_alive(Pid).

-doc "Human-readable representation.".
-spec 'printString'(t()) -> binary().
'printString'(#{'$beamtalk_class' := 'Timer', pid := Pid} = _Self) ->
    case erlang:is_process_alive(Pid) of
        true -> <<"a Timer(active)">>;
        false -> <<"a Timer(inactive)">>
    end.

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_timer) FFI uses beamtalk_erlang_proxy:direct_call/3,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector (stripping the trailing colon). These shims provide
%%% the colon-free entry points that the proxy calls:
%%%
%%%   (Erlang beamtalk_timer) after: ms do: block  → 'after'/2
%%%   (Erlang beamtalk_timer) every: ms do: block  → every/2
%%%   (Erlang beamtalk_timer) sleep: ms            → sleep/1
%%%   (Erlang beamtalk_timer) cancel: self         → cancel/1  (direct)
%%%   (Erlang beamtalk_timer) isActive: self       → isActive/1 (direct)
%%%   (Erlang beamtalk_timer) printString: self    → printString/1 (direct)
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_timer) after: ms do: block`".
-spec 'after'(integer(), Do :: function()) -> t().
'after'(Ms, Block) -> 'after:do:'(Ms, Block).

-doc "FFI shim: `(Erlang beamtalk_timer) every: ms do: block`".
-spec every(integer(), Do :: function()) -> t().
every(Ms, Block) -> 'every:do:'(Ms, Block).

-doc "FFI shim: `(Erlang beamtalk_timer) sleep: ms`".
-spec sleep(integer()) -> 'nil'.
sleep(Ms) -> 'sleep:'(Ms).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_timer(pid()) -> t().
make_timer(Pid) ->
    #{'$beamtalk_class' => 'Timer', pid => Pid}.

-spec repeat_loop(integer(), function()) -> ok.
repeat_loop(Ms, Block) ->
    receive
        {cancel, From, Ref} ->
            From ! {Ref, canceled},
            ok
    after Ms ->
        (try
            Block()
        catch
            _:_ -> ok
        end),
        repeat_loop(Ms, Block)
    end.

-spec raise_error(atom(), binary()) -> no_return().
raise_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'Timer'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).
