%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_guard_return).

%%% **DDD Context:** Object System Context

-moduledoc """
The condition of a guard-style early return compiled as a direct return
(BT-4415).

The compiler compiles `c ifTrue: [^v]. rest` as `c ifTrue: [v] ifFalse:
[rest]`, so the method needs no non-local-return frame. On a Boolean the two
forms agree. On any other receiver the guard as written sends `ifTrue:` with a
block that returns `v` from the method when called (BT-4244), and the rest of
the method runs if the receiver never calls it. The compiled code asks this
module for the branch to take: a Boolean answers itself, and anything else is
sent the guard's own selector with a block whose call is caught here. That
block throws the same signal a `^` throws (`?NLR/3` in `beamtalk.hrl`),
so it unwinds the receiver's method, including across an actor call, exactly
as the guard's own block did.

The compiler uses this for a guard whose block is `[^v]` with a `v` that
has no effects, so it makes no difference that `v` is evaluated after the
receiver's method has unwound rather than inside it. It also uses it for a
guard on a comparison against a declared parameter (`x > 0 ifTrue: [^self
one]` with `x :: Integer`, BT-4823), where the comparison answers a Boolean
while the declaration holds and only a receiver that broke it reaches this
module. There the block may hold any statements before the `^`, and they run
after the receiver's own `ifTrue:` has unwound, like `v`. So nothing that
`ifTrue:` puts around its call to the block wraps them any more: an
`ensure:` cleanup runs before them instead of after, an `on:do:` handler no
longer catches an error they raise, and for an actor receiver they run in the
caller's process. This shows only for a receiver that broke its declaration,
defines its own ordering operator (`<`, `>`, `<=`, `>=`) and answers a
non-Boolean.
""".

-include("beamtalk.hrl").

-export([if_true/1, if_false/1]).

%% The block `block_called/2` hands the receiver only ever throws: that is how
%% its call is observed, so Dialyzer's "created fun only terminates with
%% explicit exception" is expected there.
-dialyzer({nowarn_function, [block_called/2]}).

-doc """
The branch `c ifTrue: [^v]. rest` takes: `true` returns `v`, `false` runs the
rest.
""".
-spec if_true(term()) -> boolean().
if_true(true) -> true;
if_true(false) -> false;
if_true(Receiver) -> block_called(Receiver, 'ifTrue:').

-doc """
The branch `c ifFalse: [^v]. rest` takes, as the condition of the rewritten
`c ifTrue: [rest] ifFalse: [v]`: `false` returns `v`, `true` runs the rest.
""".
-spec if_false(term()) -> boolean().
if_false(true) -> true;
if_false(false) -> false;
if_false(Receiver) -> not block_called(Receiver, 'ifFalse:').

%% Sends `Selector` to `Receiver` with a block, answering whether the
%% receiver called it.
-spec block_called(term(), 'ifTrue:' | 'ifFalse:') -> boolean().
block_called(Receiver, Selector) ->
    Token = make_ref(),
    Block = fun() -> throw(?NLR(Token, true, nil)) end,
    try beamtalk_message_dispatch:send(Receiver, Selector, [Block]) of
        _ -> false
    catch
        throw:Signal when ?IS_NLR(Signal), element(2, Signal) =:= Token -> true
    end.
