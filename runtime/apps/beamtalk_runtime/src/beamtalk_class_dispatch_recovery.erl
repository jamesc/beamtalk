%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch_recovery).

%%% **DDD Context:** Runtime Context

-moduledoc """
Crash recovery and self-instantiation for class sends, split out of `beamtalk_class_dispatch` (BT-5047).

What runs when a class send finds its class process dead (`noproc`) and
restarts it to retry (`handle_class_crash_recovery/3`) and the metaclass self-instantiation paths
(`handle_self_instantiation/3`, `handle_metaclass_self_call/2`). These are
cold paths; `class_send_with_recovery/3` stays in `beamtalk_class_dispatch`.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    handle_class_crash_recovery/3,
    handle_metaclass_self_call/2,
    handle_self_instantiation/3
]).

-doc "Attempt to recover from a crashed class process and retry the operation.".
-spec handle_class_crash_recovery(pid(), beamtalk_class_dispatch:selector(), fun((pid()) -> term())) ->
    term().
handle_class_crash_recovery(ClassPid, Selector, Action) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_registry:restart_class(ClassName) of
                {ok, NewPid} ->
                    %% Retry with the restarted class process. Wrap in try/catch
                    %% so that if the restarted process crashes again immediately,
                    %% the caller gets a clean error instead of a raw noproc exit.
                    %% bt-catcher-audit: not-applicable-other-process - Action is a gen_server
                    %% call to the class process
                    try
                        Action(NewPid)
                    catch
                        exit:{noproc, _} ->
                            Error2 = beamtalk_error:new(
                                class_not_found,
                                ClassName,
                                Selector,
                                <<"Class process crashed again immediately after restart">>
                            ),
                            beamtalk_error:raise(Error2)
                    end;
                {error, _Reason} ->
                    Error = beamtalk_error:new(
                        class_not_found,
                        ClassName,
                        Selector,
                        <<"Class process crashed and could not be restarted">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        not_found ->
            %% Cannot identify the class — raise a clear error.
            Error = beamtalk_error:new(
                class_not_found,
                unknown,
                Selector,
                <<"Class process is not running (pid not found in registry)">>
            ),
            beamtalk_error:raise(Error)
    end.

%%% ============================================================================
%%% Internal Functions — Self-Instantiation
%%% ============================================================================
-doc """
Perform instantiation directly when a class method self-sends new/spawn.

Instead of raising an error when a class method sends new/spawn to itself,
bypasses gen_server and calls beamtalk_class_instantiation directly. Class
name and module are read from the process dictionary (set during
beamtalk_object_class:init/1).

`actor.bt` declares real `class sealed spawn`/`spawnWith:`
bodies (`beamtalk_actor_ffi:doSpawn/1`, `doSpawnWith/2`), matching the treatment
given to `new`/`new:`. This function — and the explicit `spawn`/`spawnWith:`
clauses of `beamtalk_class_dispatch:class_send_plain/4`, and `handle_metaclass_self_call/2` below
— deliberately keep routing through `class_self_spawn`/`class_self_new`
directly rather than falling through to generic inherited class-method
dispatch: doing so would mean a self-send here reaches the lifted body via
`gen_server:call(self(), ...)`, which deadlocks — the exact regression this
routing exists to avoid. Same "keep the shim, the lifted body is
the dynamic-dispatch definition, not the one every path actually calls"
choice made for `new`/`new:` — see `beamtalk_actor_ffi:doSpawn/1`'s doc.
""".
-spec handle_self_instantiation(new | spawn, atom(), list()) -> term().
handle_self_instantiation(Type, Selector, Args) ->
    ClassName = get(beamtalk_class_name),
    Module = get(beamtalk_class_module),
    IsAbstract0 = get(beamtalk_class_is_abstract),
    case {ClassName, Module, IsAbstract0} of
        {undefined, _, _} ->
            %% Fallback: process dictionary not set — called from non-class process.
            handle_self_instantiation_error(Selector);
        {_, undefined, _} ->
            handle_self_instantiation_error(Selector);
        {_, _, undefined} ->
            handle_self_instantiation_error(Selector);
        {CN, Mod, IsAbstract} when is_boolean(IsAbstract) ->
            case Type of
                new ->
                    beamtalk_class_instantiation:class_self_new(CN, Mod, Args);
                spawn ->
                    beamtalk_class_instantiation:class_self_spawn(CN, Mod, IsAbstract, Args)
            end;
        {_, _, _} ->
            %% Corrupt/invalid is_abstract metadata — fail fast with structured error.
            handle_self_instantiation_error(Selector)
    end.

-doc """
Handle `self class <selector>` self-calls from within a class method.

The class gen_server cannot `gen_server:call(self(), ...)` — the metaclass
dispatch path would deadlock. Short-circuit the spawn/new family to the same
helpers used by `self new` and `self spawnAs:`, and raise a
clear, structured error for any other selector so callers see a useful
diagnostic rather than an opaque `calling_self` timeout.
""".
-spec handle_metaclass_self_call(beamtalk_class_dispatch:selector(), list()) -> term().
handle_metaclass_self_call('new', []) ->
    handle_self_instantiation(new, 'new', []);
handle_metaclass_self_call('new:', [Map]) ->
    handle_self_instantiation(new, 'new:', [Map]);
handle_metaclass_self_call(spawn, []) ->
    handle_self_instantiation(spawn, spawn, []);
handle_metaclass_self_call('spawnWith:', [Map]) ->
    handle_self_instantiation(spawn, 'spawnWith:', [Map]);
handle_metaclass_self_call('spawnAs:', [Name]) ->
    handle_metaclass_self_named_spawn('spawnAs:', #{}, Name);
handle_metaclass_self_call('spawnWith:as:', [InitArgs, Name]) ->
    handle_metaclass_self_named_spawn('spawnWith:as:', InitArgs, Name);
handle_metaclass_self_call(Selector, _Args) ->
    ClassName = self_class_name(),
    Error0 = beamtalk_error:new(dispatch_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        beamtalk_error:chars_to_binary(
            io_lib:format(
                "'self class ~ts' inside a class method of '~ts' would deadlock "
                "(gen_server:call to self). Only spawn/new selectors can be "
                "short-circuited from the class process. Send this message "
                "from outside the class, or use `self ~ts` directly if the "
                "method is defined on the instance side.",
                [Selector, ClassName, Selector]
            )
        )
    ),
    beamtalk_error:raise(Error1).

-doc """
Perform a named spawn directly from within a class method.

Mirrors `handle_self_instantiation/3` but routes to `class_self_spawn_as` /
`class_self_spawn_with`, which return a Beamtalk `Result` rather than raising.
""".
-spec handle_metaclass_self_named_spawn('spawnAs:' | 'spawnWith:as:', term(), term()) -> term().
handle_metaclass_self_named_spawn(Selector, InitArgs, Name) ->
    ClassName = get(beamtalk_class_name),
    Module = get(beamtalk_class_module),
    IsAbstract0 = get(beamtalk_class_is_abstract),
    case {ClassName, Module, IsAbstract0} of
        {CN, Mod, IsAbstract} when
            is_atom(CN),
            is_atom(Mod),
            is_boolean(IsAbstract),
            CN =/= undefined,
            Mod =/= undefined
        ->
            case Selector of
                'spawnAs:' ->
                    beamtalk_class_instantiation:class_self_spawn_as(CN, Mod, IsAbstract, Name);
                'spawnWith:as:' ->
                    beamtalk_class_instantiation:class_self_spawn_with(
                        CN, Mod, IsAbstract, InitArgs, Name
                    )
            end;
        _ ->
            handle_self_instantiation_error(Selector)
    end.

-doc """
Fallback error when process dictionary metadata is missing.

Preserves the original selector and names the class from the process
dictionary (`self_class_name/0`) for diagnostic purposes.
""".
-spec handle_self_instantiation_error(atom()) -> no_return().
handle_self_instantiation_error(Selector) ->
    ClassName = self_class_name(),
    Error0 = beamtalk_error:new(instantiation_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        <<
            "Self-instantiation requires class metadata in the process dictionary "
            "(set during class process init). This error indicates the call "
            "originated from a non-class process or the class was not initialized correctly."
        >>
    ),
    beamtalk_error:raise(Error1).

-doc """
The class name for diagnostics raised from a class process: the one its
`init/1` stored in the process dictionary, or `unknown` for a process that
never initialised as a class.
""".
-spec self_class_name() -> atom().
self_class_name() ->
    case get(beamtalk_class_name) of
        undefined -> unknown;
        ClassName -> ClassName
    end.
