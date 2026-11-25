%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_self).

%%% **DDD Context:** Runtime Context

-moduledoc """
Class-self dispatch: a class method sending to its own class, split out of `beamtalk_class_dispatch` (BT-5047).

The chain walk, the class-object protocol fall-through, the local dispatch and
the outcome unwrapping behind `beamtalk_class_dispatch:class_self_send/3` and
`class_self_dispatch_local/4`. Those entry points stay in `beamtalk_class_dispatch`,
where generated code calls them, and call straight into this module, so a
class-self send makes the same number of calls as before the split.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    class_self_dispatch_chain/5,
    class_self_dispatch_local/4,
    class_self_protocol_fallthrough/4
]).

%% Appended to the self-call hint when a class-object protocol method, reached
%% through `class_self_protocol_fallthrough/4`, messaged the class (BT-4252).
-define(PROTOCOL_SELF_CALL_NOTE, <<
    " The selector named is the class-object protocol message sent from the "
    "class method; the call that messaged the class may be one it makes in turn."
>>).

-doc """
The class-method chain step shared by `class_self_dispatch_local/4`,
`class_self_send/3` and `class_super_dispatch/4`: each does its own chain
lookup first, then applies the lookup result here, or falls through to the class-object protocol.
""".
-spec class_self_dispatch_chain(
    beamtalk_class_dispatch:class_name(),
    beamtalk_class_dispatch:selector(),
    list(),
    {ok, beamtalk_class_dispatch:class_name(), atom()} | not_found,
    #beamtalk_object{}
) -> term() | no_return().
class_self_dispatch_chain(ClassName, Selector, Args, Found, ClassSelf) ->
    case Found of
        {ok, DefiningClass, DefiningModule} ->
            %% Route through the same internal helper the gen_server path uses
            %% (`invoke_class_method/7`), so both paths share error classification
            %% and the TestCase `test_spawn` guard. Unwrap to the raw result and
            %% raise structured errors on dispatch/body failures so callers see
            %% proper `#beamtalk_error{}` instead of a bare `undef`.
            unwrap_self_dispatch_outcome(
                ClassName,
                Selector,
                beamtalk_class_dispatch:apply_class_method_in_context(
                    Selector,
                    Args,
                    ClassName,
                    ClassSelf,
                    DefiningClass,
                    DefiningModule
                )
            );
        not_found ->
            class_self_protocol_fallthrough(ClassName, Selector, Args, ClassSelf)
    end.

-doc """
The class-object protocol fallthrough for a class-side `self`/`super` send
(BT-4252): no class method in the chain answers
`Selector`, so dispatch it the way an external send to the receiving class
object does after its class-method lookup misses
(`class_send_dispatch/3`'s `{error, not_found}` branch) — through the
`Class -> Behaviour -> Object -> ProtoObject` instance-method chain, via
`try_class_chain_fallthrough/3`. So `self name`, `super name` and
`super superclass` in a class method answer what `C name` /
`C superclass` answer from outside, and the type checker's class-side check
(which resolves an unknown class-side selector against `Class`) agrees with
the runtime by construction — one lookup, no selector list.

The receiver is the calling method's own `ClassSelf`, so a subclass receiver
answers for itself: `super name` from a `Base` class method inherited by `C`
answers `#C`. Its pid is the receiving class's registered pid, never
`self()`-derived, so the same `ClassSelf` results whether the class method
runs in the class's own process or in the caller (ADR 0142). The
`Behaviour` primitives reach class state through the message-free
`beamtalk_object_class:*_safe/1` readers, so running in the class's own
process does not self-call its gen_server. `doc` reads the class doc's
published copy, and a Beamtalk-bodied protocol method that sends to its
receiver (`printString`) re-enters here through
`class_send_from_own_process/2` (BT-4262). A protocol method that still
messages the receiving class (the `doc:` and `setDocForMethod:to:` setters)
cannot answer from inside that process: it raises the structured
`dispatch_error` of `handle_class_self_call/2`, naming the outermost protocol
send, which may not be the call that actually messaged the class (a
Beamtalk-bodied protocol method can reach it through an Erlang call of its
own). Raises the structured
`does_not_understand` when `Class` does not understand `Selector` either.
""".
-spec class_self_protocol_fallthrough(
    beamtalk_class_dispatch:class_name(),
    beamtalk_class_dispatch:selector(),
    list(),
    #beamtalk_object{}
) ->
    term() | no_return().
class_self_protocol_fallthrough(ClassName, Selector, Args, ClassSelf) ->
    case ClassSelf of
        #beamtalk_object{pid = ReceiverPid} when not is_pid(ReceiverPid) ->
            raise_class_self_dnu(ClassName, Selector);
        #beamtalk_object{pid = ReceiverPid} ->
            try beamtalk_class_dispatch:try_class_chain_fallthrough(ClassSelf, Selector, Args) of
                {ok, Result} -> Result;
                not_found -> raise_class_self_dnu(ClassName, Selector)
            catch
                %% A protocol method that messages the receiving class (`doc`'s
                %% `gen_server:call(ClassPid, get_doc)`, or a Beamtalk body
                %% whose send reaches a raw call) cannot answer from inside that
                %% class's own process. The `calling_self` exit reaches here
                %% either raw or already wrapped by the method dispatch (as a
                %% `runtime_error` carrying it under `details.reason`); report
                %% it as the structured `dispatch_error` every other send back
                %% to the running class raises (`handle_class_self_call/2`).
                %% `Selector` is the outermost protocol send, which is not
                %% necessarily the one that messaged the class (a Beamtalk
                %% body may reach it through further sends); the hint says so.
                %% Anything else, and every throw (a `^` or a script exit),
                %% propagates unchanged.
                Class:Reason:Stack when ReceiverPid =:= self(), Class =/= throw ->
                    case beamtalk_exception_handler:contains_calling_self(Reason) of
                        true ->
                            beamtalk_class_dispatch:handle_class_self_call(
                                Selector, ?PROTOCOL_SELF_CALL_NOTE
                            );
                        false ->
                            erlang:raise(Class, Reason, Stack)
                    end
            end
    end.

-spec class_self_dispatch_local(
    beamtalk_class_dispatch:class_name(),
    beamtalk_class_dispatch:selector(),
    list(),
    #beamtalk_object{}
) ->
    term() | no_return().
class_self_dispatch_local(ClassName, Selector, Args, ClassSelf) ->
    case beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) of
        {ok, _Info} ->
            %% Own runtime class method: dispatch with DefiningClass = ClassName
            %% so apply_class_method_in_context resolves the fun from the store.
            DefiningModule = self_dispatch_module(ClassName),
            unwrap_self_dispatch_outcome(
                ClassName,
                Selector,
                beamtalk_class_dispatch:apply_class_method_in_context(
                    Selector,
                    Args,
                    ClassName,
                    ClassSelf,
                    ClassName,
                    DefiningModule
                )
            );
        error ->
            %% Not a local runtime method — walk the chain from the
            %% class itself: its own compiled method (an own-class
            %% reference's fallback, BT-4301), then inherited ones,
            %% which also resolves inherited runtime funs.
            beamtalk_class_dispatch:class_side_dispatch_found(
                ClassName,
                Selector,
                Args,
                beamtalk_class_dispatch:find_class_side_method_from_class(
                    Selector, ClassName
                ),
                ClassSelf
            )
    end.

-doc "The compiled module backing `ClassName`, for a `ClassSelf` built outside the gen_server state.".
-spec self_dispatch_module(beamtalk_class_dispatch:class_name()) -> atom().
self_dispatch_module(ClassName) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        {ok, M} -> M;
        not_found -> ClassName
    end.

-doc """
Adapt a `beamtalk_class_dispatch:class_method_outcome()` from `apply_class_method_in_context/6` to the
self-dispatch caller shape: raw value on success, structured raises on failure.
Shared by `class_self_send/3`, `class_self_dispatch_local/4` and
`class_super_dispatch/4`.
""".
-spec unwrap_self_dispatch_outcome(
    beamtalk_class_dispatch:class_name(),
    beamtalk_class_dispatch:selector(),
    beamtalk_class_dispatch:class_method_outcome()
) ->
    term() | no_return().
unwrap_self_dispatch_outcome(ClassName, Selector, Outcome) ->
    case Outcome of
        {ok, Raw} ->
            Raw;
        {error, #beamtalk_error{} = Error} ->
            beamtalk_error:raise(Error);
        {error, undef_in_body} ->
            %% Preserve the `error:undef` contract for callers that rely on
            %% it (e.g. user code catching `error:undef` from a method body).
            erlang:error(undef);
        {error, {raised, ErrClass, Error, ST}} ->
            erlang:raise(ErrClass, Error, ST);
        {nlr_relay, Nlr, ST} ->
            %% ADR 0110: self-dispatch runs in the caller's own
            %% process, so resume the non-local return unwind directly —
            %% identical behavior to when these throws were tagged
            %% {error, {raised, throw, ...}}.
            erlang:raise(throw, Nlr, ST);
        test_spawn ->
            %% `TestCase>>runAll` / `run:` self-sent from inside another
            %% class method would run the test suite inline in the class
            %% gen_server process — the exact deadlock the `test_spawn`
            %% escape hatch exists to avoid. Surface a
            %% structured error instead of blocking; callers should invoke
            %% test runners from outside the class body.
            raise_test_spawn_self_dispatch_unsupported(ClassName, Selector)
    end.

-spec raise_test_spawn_self_dispatch_unsupported(
    beamtalk_class_dispatch:class_name(), beamtalk_class_dispatch:selector()
) -> no_return().
raise_test_spawn_self_dispatch_unsupported(ClassName, Selector) ->
    %% Atom-safe formatting — see note on raise_class_self_dnu/2.
    Hint = beamtalk_error:chars_to_binary(
        io_lib:format(
            "Inherited test-execution selector '~ts' cannot be self-sent from "
            "inside a class method on '~ts' — it would deadlock the class "
            "gen_server. Call `~ts ~ts` from outside the class body instead.",
            [
                atom_to_list(Selector),
                atom_to_list(ClassName),
                atom_to_list(ClassName),
                atom_to_list(Selector)
            ]
        )
    ),
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector, Hint),
    beamtalk_error:raise(Error).

-spec raise_class_self_dnu(
    beamtalk_class_dispatch:class_name(), beamtalk_class_dispatch:selector()
) -> no_return().
raise_class_self_dnu(ClassName, Selector) ->
    %% ClassName and Selector are atoms; use ~ts (unicode string) with
    %% atom_to_list/1 rather than ~s, which expects an iolist and raises
    %% badarg on atoms — that would mask the intended does_not_understand.
    Hint = beamtalk_error:chars_to_binary(
        io_lib:format(
            "Class '~ts' has no class method '~ts' and no ancestor defines it",
            [atom_to_list(ClassName), atom_to_list(Selector)]
        )
    ),
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector, Hint),
    beamtalk_error:raise(Error).
