%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_timer).

%%% **DDD Context:** Object System Context

-moduledoc """
Timer class implementation — delayed and repeating work via Erlang timers.

Provides one-shot delays (`after:do:`), repeating intervals (`every:do:`),
message timers (`send:to:after:`, `send:to:every:`) and process sleep
(`sleep:`). Timer instances are cancellable value objects.

Timer objects are represented as tagged maps of one of two shapes:
```
#{'$beamtalk_class' => 'Timer', pid  => pid()}         %% block timers, repeating message timers
#{'$beamtalk_class' => 'Timer', tref => reference()}   %% one-shot message timers
```

## Message Timers (BT-4048)

`send:to:after:` delivers a message to a process exactly as `erlang:send/2`
would, so a Server receives it in `handleInfo:`.

- **One-shot** (`send:to:after:`) to a local target is backed by
  `erlang:send_after/3` — no extra process. `cancel` is
  `erlang:cancel_timer/1` (true when it had not fired yet) and `isActive` is
  `erlang:read_timer/1 =/= false`. `send_after/3` only accepts a local pid or
  a local registered name, so a remote target (a pid on another node, a
  `{registered, Name, Node}` actor, a global name held on another node) uses
  a small one-shot process instead, with the pid-based cancel protocol.
- **Repeating** (`send:to:every:`) uses the same spawned-process machinery
  as `every:do:` (not `timer:send_interval/3`), so `cancel`/`isActive` share
  the pid-based ack protocol of block timers. The process is not linked to
  the creator; it monitors the target (a pid, a registered name or
  `{Name, Node}`) and stops when the target dies, so a restarted named
  Server never inherits its predecessor's ticks. Each tick is scheduled
  against an absolute monotonic deadline, so intervals do not drift; ticks
  missed while the process was descheduled are skipped, not replayed.
  Because the process is not linked to its creator, a caller that targets
  another process owns the handle and must `cancel` it.

A target that has died makes the timer a silent no-op, as in OTP. A global
name is resolved once, when the timer is created. A one-shot timer reference
is node-local: `cancel`/`isActive` answer false on another node.

## Cancel Protocol

`cancel/1` uses an acknowledgement-based protocol to avoid TOCTOU races.
The caller sends `{cancel, From, Ref}` and waits for `{Ref, canceled}`.
If the timer process has already exited (fired), a process monitor DOWN
message arrives instead and cancel/1 returns false. A 100 ms fallback
timeout guards against unexpected stalls.

## FFI Shims

The `beamtalk_erlang_proxy:direct_call/4` dispatch derives the Erlang function
name from the first keyword of the Beamtalk selector (stripping the trailing
colon). The colon-free shims (`after/2`, `every/2`, `sleep/1`, `sendAfter/3`,
`sendEvery/3`) bridge the
gap so that `(Erlang beamtalk_timer) after: ms do: block` dispatches correctly
to the canonical `'after:do:'/2` implementation.
""".

%% Class methods (canonical colon forms — used by EUnit tests)
-export(['after:do:'/2, 'every:do:'/2, 'sleep:'/1]).
-export(['send:to:after:'/3, 'send:to:every:'/3]).

%% Instance methods
-export([cancel/1, 'isActive'/1, 'printString'/1]).

%% FFI shims for (Erlang beamtalk_timer) dispatch
-export(['after'/2, every/2, sleep/1, 'sendAfter'/3, 'sendEvery'/3]).

-ifdef(TEST).
-export([next_deadline/3]).
-endif.

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Largest delay `erlang:send_after/3` and `receive ... after` accept.
-define(MAX_TIMEOUT_MS, 4294967295).

-type t() :: #{'$beamtalk_class' := 'Timer', atom() => term()}.
-export_type([t/0]).

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

-doc """
Evaluate `Block` once after the given delay. Returns a Timer.

The delay is Integer milliseconds or a Duration.
""".
-spec 'after:do:'(integer() | beamtalk_duration:t(), Do :: function()) -> t().
'after:do:'(#{'$beamtalk_class' := 'Duration'} = D, Block) ->
    'after:do:'(beamtalk_duration:'asMilliseconds'(D), Block);
'after:do:'(Ms, Block) when is_integer(Ms), Ms >= 0, is_function(Block, 0) ->
    Pid = spawn_link(fun() ->
        receive
            {cancel, From, Ref} ->
                From ! {Ref, canceled},
                ok
        after Ms ->
            %% bt-catcher-audit: not-applicable-other-process - block runs in the spawn_link'd
            %% timer process
            try
                Block()
            catch
                _:_ -> ok
            end
        end
    end),
    make_timer(Pid);
'after:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('after:do:', <<"Delay must be a non-negative Integer or a non-negative Duration">>);
'after:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('after:do:', <<"Delay must be a non-negative Integer or a non-negative Duration">>);
'after:do:'(_, _) ->
    raise_error('after:do:', <<"Expected an Integer or Duration delay and a Block">>).

-doc """
Evaluate `Block` at the given repeat interval. Returns a Timer.

The interval is Integer milliseconds or a Duration.
""".
-spec 'every:do:'(integer() | beamtalk_duration:t(), Do :: function()) -> t().
'every:do:'(#{'$beamtalk_class' := 'Duration'} = D, Block) ->
    'every:do:'(beamtalk_duration:'asMilliseconds'(D), Block);
'every:do:'(Ms, Block) when is_integer(Ms), Ms > 0, is_function(Block, 0) ->
    Pid = spawn_link(fun() -> repeat_loop(Ms, Block) end),
    make_timer(Pid);
'every:do:'(Ms, _Block) when is_integer(Ms) ->
    raise_error('every:do:', <<"Interval must be a positive Integer or a positive Duration">>);
'every:do:'(_Ms, Block) when is_function(Block, 0) ->
    raise_error('every:do:', <<"Interval must be a positive Integer or a positive Duration">>);
'every:do:'(_, _) ->
    raise_error(
        'every:do:', <<"Expected a positive Integer or a positive Duration interval and a Block">>
    ).

-doc """
Send `Msg` to `Target` once after the given delay. Returns a Timer.

`Target` is an Actor (including a Server) or a raw pid; the delay is Integer
milliseconds or a Duration. Backed by `erlang:send_after/3`.
""".
-spec 'send:to:after:'(term(), term(), integer() | beamtalk_duration:t()) -> t().
'send:to:after:'(Msg, Target, #{'$beamtalk_class' := 'Duration'} = D) ->
    'send:to:after:'(Msg, Target, beamtalk_duration:'asMilliseconds'(D));
'send:to:after:'(Msg, Target, Ms) when is_integer(Ms), Ms >= 0, Ms =< ?MAX_TIMEOUT_MS ->
    case resolve_dest('send:to:after:', Target) of
        unreachable ->
            %% An unregistered global name: an inert timer — cancel answers
            %% false, isActive false.
            make_tref_timer(make_ref());
        {ok, Dest} when is_atom(Dest) ->
            make_tref_timer(erlang:send_after(Ms, Dest, Msg));
        {ok, Dest} when is_pid(Dest), node(Dest) =:= node() ->
            make_tref_timer(erlang:send_after(Ms, Dest, Msg));
        {ok, Dest} ->
            %% Remote target: send_after/3 rejects it, so a one-shot process.
            make_timer(spawn(fun() -> send_once(Ms, Dest, Msg) end))
    end;
'send:to:after:'(_Msg, _Target, _Ms) ->
    raise_error(
        'send:to:after:', <<"Delay must be an Integer or a Duration from 0 to 4294967295 ms">>
    ).

-doc """
Send `Msg` to `Target` at the given repeat interval. Returns a Timer.

`Target` is an Actor (including a Server) or a raw pid; the interval is
Integer milliseconds or a Duration. Runs in a small unlinked process (the
`every:do:` machinery) that stops on `cancel` or when a pid target dies.
""".
-spec 'send:to:every:'(term(), term(), integer() | beamtalk_duration:t()) -> t().
'send:to:every:'(Msg, Target, #{'$beamtalk_class' := 'Duration'} = D) ->
    'send:to:every:'(Msg, Target, beamtalk_duration:'asMilliseconds'(D));
'send:to:every:'(Msg, Target, Ms) when is_integer(Ms), Ms > 0, Ms =< ?MAX_TIMEOUT_MS ->
    case resolve_dest('send:to:every:', Target) of
        unreachable ->
            make_tref_timer(make_ref());
        {ok, Dest} ->
            Pid = spawn(fun() ->
                %% monitor/2 takes a pid, a local name or {Name, Node}; an
                %% unregistered name answers DOWN at once and the loop stops.
                Mon = erlang:monitor(process, Dest),
                send_loop(Ms, Dest, Msg, Mon, erlang:monotonic_time(millisecond) + Ms)
            end),
            make_timer(Pid)
    end;
'send:to:every:'(_Msg, _Target, _Ms) ->
    raise_error(
        'send:to:every:', <<"Interval must be an Integer or a Duration from 1 to 4294967295 ms">>
    ).

-doc """
Block the current process for the given delay.

The delay is Integer milliseconds or a Duration.
""".
-spec 'sleep:'(integer() | beamtalk_duration:t()) -> 'nil'.
'sleep:'(#{'$beamtalk_class' := 'Duration'} = D) ->
    'sleep:'(beamtalk_duration:'asMilliseconds'(D));
'sleep:'(Ms) when is_integer(Ms), Ms >= 0 ->
    timer:sleep(Ms),
    nil;
'sleep:'(_) ->
    raise_error('sleep:', <<"Delay must be a non-negative Integer or a non-negative Duration">>).

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
cancel(#{'$beamtalk_class' := 'Timer', tref := TRef}) ->
    erlang:cancel_timer(TRef) =/= false;
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
'isActive'(#{'$beamtalk_class' := 'Timer', tref := TRef}) ->
    erlang:read_timer(TRef) =/= false;
'isActive'(#{'$beamtalk_class' := 'Timer', pid := Pid}) ->
    erlang:is_process_alive(Pid).

-doc "Human-readable representation.".
-spec 'printString'(t()) -> binary().
'printString'(#{'$beamtalk_class' := 'Timer'} = Self) ->
    case 'isActive'(Self) of
        true -> <<"Timer(active)">>;
        false -> <<"Timer(inactive)">>
    end.

%%% ============================================================================
%%% FFI Shims
%%%
%%% The (Erlang beamtalk_timer) FFI uses beamtalk_erlang_proxy:direct_call/4,
%%% which derives the Erlang function name from the first keyword of the
%%% Beamtalk selector (stripping the trailing colon). These shims provide
%%% the colon-free entry points that the proxy calls:
%%%
%%%   (Erlang beamtalk_timer) after: ms do: block  → 'after'/2
%%%   (Erlang beamtalk_timer) every: ms do: block  → every/2
%%%   (Erlang beamtalk_timer) sleep: ms            → sleep/1
%%%   (Erlang beamtalk_timer) sendAfter: m to: t delay: ms    → sendAfter/3
%%%   (Erlang beamtalk_timer) sendEvery: m to: t interval: ms → sendEvery/3
%%%   (Erlang beamtalk_timer) cancel: self         → cancel/1  (direct)
%%%   (Erlang beamtalk_timer) isActive: self       → isActive/1 (direct)
%%%   (Erlang beamtalk_timer) printString: self    → printString/1 (direct)
%%% ============================================================================

-doc "FFI shim: `(Erlang beamtalk_timer) after: ms do: block`".
-spec 'after'(integer() | beamtalk_duration:t(), Do :: function()) -> t().
'after'(Ms, Block) -> 'after:do:'(Ms, Block).

-doc "FFI shim: `(Erlang beamtalk_timer) every: ms do: block`".
-spec every(integer() | beamtalk_duration:t(), Do :: function()) -> t().
every(Ms, Block) -> 'every:do:'(Ms, Block).

-doc "FFI shim: `(Erlang beamtalk_timer) sleep: ms`".
-spec sleep(integer() | beamtalk_duration:t()) -> 'nil'.
sleep(Ms) -> 'sleep:'(Ms).

-doc "FFI shim: `(Erlang beamtalk_timer) sendAfter: msg to: target delay: ms`".
-spec 'sendAfter'(term(), term(), integer() | beamtalk_duration:t()) -> t().
'sendAfter'(Msg, Target, Ms) -> 'send:to:after:'(Msg, Target, Ms).

-doc "FFI shim: `(Erlang beamtalk_timer) sendEvery: msg to: target interval: ms`".
-spec 'sendEvery'(term(), term(), integer() | beamtalk_duration:t()) -> t().
'sendEvery'(Msg, Target, Ms) -> 'send:to:every:'(Msg, Target, Ms).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-spec make_timer(pid()) -> t().
make_timer(Pid) ->
    #{'$beamtalk_class' => 'Timer', pid => Pid}.

-spec make_tref_timer(reference()) -> t().
make_tref_timer(TRef) ->
    #{'$beamtalk_class' => 'Timer', tref => TRef}.

%% Resolve a `to:` target to `{ok, Dest}`, where `Dest` is what
%% `erlang:send/2` accepts: a pid, a locally registered name (re-resolved on
%% every delivery, like OTP) or `{Name, Node}`. Answers `unreachable` for a
%% global name that is not currently registered; raises for a non-target.
-spec resolve_dest(atom(), term()) -> {ok, pid() | atom() | {atom(), node()}} | unreachable.
resolve_dest(_Selector, Pid) when is_pid(Pid) ->
    {ok, Pid};
resolve_dest(_Selector, #beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    {ok, Pid};
resolve_dest(_Selector, #beamtalk_object{pid = {registered, Name}}) ->
    {ok, Name};
resolve_dest(_Selector, #beamtalk_object{pid = {registered, Name, Node}}) when Node =:= node() ->
    {ok, Name};
resolve_dest(_Selector, #beamtalk_object{pid = {registered, Name, Node}}) ->
    {ok, {Name, Node}};
resolve_dest(_Selector, #beamtalk_object{pid = {global, Name}}) ->
    case global:whereis_name(Name) of
        undefined -> unreachable;
        Pid -> {ok, Pid}
    end;
resolve_dest(Selector, _Other) ->
    raise_error(Selector, <<"Target must be an Actor or a Pid">>).

%% One-shot message timer for a remote target (send_after/3 is local-only).
-spec send_once(non_neg_integer(), pid() | {atom(), node()}, term()) -> ok.
send_once(Ms, Dest, Msg) ->
    receive
        {cancel, From, Ref} ->
            From ! {Ref, canceled},
            ok
    after Ms ->
        Dest ! Msg,
        ok
    end.

%% Repeating message-timer loop. Stops on cancel or when the monitored
%% target dies. `Next` is the absolute monotonic deadline of the next tick,
%% so a slow send never shifts later ticks; missed deadlines are skipped.
-spec send_loop(
    pos_integer(), pid() | atom() | {atom(), node()}, term(), reference(), integer()
) -> ok.
send_loop(Ms, Dest, Msg, Mon, Next) ->
    Wait = max(0, Next - erlang:monotonic_time(millisecond)),
    receive
        {cancel, From, Ref} ->
            From ! {Ref, canceled},
            ok;
        {'DOWN', Mon, process, _, _} ->
            ok
    after Wait ->
        %% badarg only if a registered name vanished just before this send
        (try
            Dest ! Msg
        catch
            error:badarg -> ok
        end),
        send_loop(Ms, Dest, Msg, Mon, next_deadline(Next, erlang:monotonic_time(millisecond), Ms))
    end.

%% The deadline after the tick due at `Next`, given the time `Now` that tick
%% was sent: the first phase point (`Next + k * Ms`, k >= 1) after `Now`.
%% Ticks missed while the process was descheduled are skipped instead of
%% replayed as a burst, and the phase stays fixed (no drift).
-spec next_deadline(integer(), integer(), pos_integer()) -> integer().
next_deadline(Next, Now, Ms) ->
    Skipped = max(0, (Now - Next) div Ms),
    Next + (Skipped + 1) * Ms.

-spec repeat_loop(integer(), function()) -> ok.
repeat_loop(Ms, Block) ->
    receive
        {cancel, From, Ref} ->
            From ! {Ref, canceled},
            ok
    after Ms ->
        %% bt-catcher-audit: not-applicable-other-process - block runs in the spawn_link'd timer
        %% process
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
