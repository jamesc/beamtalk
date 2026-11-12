%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_ffi).

%%% **DDD Context:** Actor System Context

-moduledoc """
Beamtalk stdlib FFI shims for `Actor` (ADR 0079, ADR 0126).

Split out of `beamtalk_actor` (BT-4701). These functions back
`stdlib/src/actor.bt` and `stdlib/src/reply_to.bt` via `(Erlang
beamtalk_actor_ffi)` calls. They take the class object (or actor instance) as
the first argument.

`doSpawn/1` / `doSpawnWith/2` raise on failure, matching `spawn`'s declared
`-> Self` return type (no `Result` wrapping). The named variants
(`doSpawnAs/2`, `doSpawnWith/3`) translate runtime `{ok, _} | {error, _}`
tuples into the shape ADR 0076 auto-converts to `Result` at the FFI
boundary, matching their declared `Result(Self, Error)` return type.

`allRegistered/1` is also an `erpc` target (`allRegisteredOn:` on a peer
node), so `beamtalk_actor` keeps a delegate for it; see
`beamtalk_actor_remote` for the compatibility rule.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    'allRegistered'/1,
    class_mod_for/1,
    'doAllRegisteredOn'/2,
    'doNamedOn'/3,
    'doNamedScope'/3,
    'doSpawn'/1,
    'doSpawnAs'/2,
    'doSpawnAsOn'/3,
    'doSpawnAsScope'/3,
    'doSpawnMonitored'/1,
    'doSpawnMonitoredAsync'/1,
    'doSpawnMonitoredAsyncWith'/2,
    'doSpawnMonitoredWith'/2,
    'doSpawnOn'/2,
    'doSpawnWith'/2,
    'doSpawnWith'/3,
    'doSpawnWithAsOn'/4,
    'doSpawnWithAsScope'/4,
    'doSpawnWithOn'/3,
    generic_remote_error/3,
    generic_spawn_error/3,
    'isRegistered'/1,
    named/2,
    named_lookup/3,
    named_lookup/4,
    'registerAs'/2,
    'registeredName'/1,
    unregister/1
]).

-doc """
FFI shim for `class sealed spawn -> Self`.

`Self` is the class object `{beamtalk_object, 'ClassName class', Module, ClassPid}`.
Reuses `beamtalk_class_instantiation:class_self_spawn/4` — the same helper
the compiler's `self spawn` intrinsic (`try_instantiation_intrinsic` /
ADR 0109 amendment) calls — so behaviour (safe_spawn's trap_exit +
`initialize` sync, hot-reload instance registration via the compiled
`Module:spawn/0`, abstract-class rejection, `#beamtalk_object{}` wrapping) is
byte-for-byte identical to the per-class `spawn/0` export. Unlike
`doSpawnAs/2` / `doSpawnWith/3`, this raises on failure rather than
returning `{error, _}` — `spawn`'s declared return type is `Self`, not
`Result(Self, Error)`.
""".
-spec doSpawn(#beamtalk_object{}) -> #beamtalk_object{}.
doSpawn(Self) ->
    do_spawn_or_raise(Self, [], spawn, class_self_spawn).

-doc """
FFI shim for `class sealed spawnWith: initArgs -> Self`.

Same contract as `doSpawn/1` plus the initialisation arguments passed to
the actor's `init/1` callback.
""".
-spec doSpawnWith(#beamtalk_object{}, term()) -> #beamtalk_object{}.
doSpawnWith(Self, InitArgs) ->
    do_spawn_or_raise(Self, [InitArgs], 'spawnWith:', class_self_spawn).

%% Shared implementation for doSpawn/1, doSpawnWith/2 and (BT-4104)
%% doSpawnMonitored/1, doSpawnMonitoredWith/2. Resolves ClassName/
%% Module from Self, resolves the abstract-class flag by name (the
%% metadata-lookup pattern — no process-dictionary dependency, safe to call
%% from any process), then delegates to `Helper` — `class_self_spawn/4` or
%% `class_self_spawn_monitored/4` — which raises a structured
%% `instantiation_error` on failure.
-spec do_spawn_or_raise(
    #beamtalk_object{},
    list(),
    atom(),
    class_self_spawn | class_self_spawn_monitored | class_self_spawn_monitored_async
) -> #beamtalk_object{} | {#beamtalk_object{}, reference()}.
do_spawn_or_raise(Self, Args, Selector, Helper) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            IsAbstract = beamtalk_class_instantiation:resolve_is_abstract_or_raise(
                ClassName, Selector
            ),
            beamtalk_class_instantiation:Helper(ClassName, Module, IsAbstract, Args);
        {error, #beamtalk_error{} = Err} ->
            beamtalk_error:raise(beamtalk_error:with_selector(Err, Selector))
    end.

-doc """
FFI shim for `class sealed spawnMonitored -> Tuple` (BT-4104).

Answers `{Actor, MonitorRef}` — a bare 2-tuple, deliberately not
ok-wrapped, so it reaches Beamtalk as a `Tuple` (ADR 0076 converts only
`{ok, _}`/`{error, _}`). Raises `instantiation_error` on failure, like
`doSpawn/1`. Reuses `beamtalk_class_instantiation:class_self_spawn_monitored/4`,
the helper the compiler's `self spawnMonitored` intrinsic calls, which calls
the compiled `Module:spawn_monitored/0`.

The monitor is owned by the process running this shim. A dynamic send of
`spawnMonitored` runs it in the receiver class's process, so that process
owns the monitor and its `'DOWN'` is never seen by the sender — a static
`Counter spawnMonitored` from an actor method monitors from the sender.
""".
-spec doSpawnMonitored(#beamtalk_object{}) -> {#beamtalk_object{}, reference()}.
doSpawnMonitored(Self) ->
    do_spawn_or_raise(Self, [], spawnMonitored, class_self_spawn_monitored).

-doc """
FFI shim for `class sealed spawnMonitoredWith: initArgs -> Tuple` (BT-4104).

Same contract as `doSpawnMonitored/1` plus the initialisation arguments
passed to the actor's `init/1` callback.
""".
-spec doSpawnMonitoredWith(#beamtalk_object{}, term()) -> {#beamtalk_object{}, reference()}.
doSpawnMonitoredWith(Self, InitArgs) ->
    do_spawn_or_raise(Self, [InitArgs], 'spawnMonitoredWith:', class_self_spawn_monitored).

-doc """
FFI shim for `class sealed spawnMonitoredAsync -> Tuple` (BT-4477).

Same shape and result as `doSpawnMonitored/1` (`{Actor, MonitorRef}`), but the
answer comes back without waiting for the actor's `init/1` or `initialize`
(`safe_spawn_monitored_async/2`). An `initialize` failure is not raised: it
arrives as the monitor's `'DOWN'`.
""".
-spec doSpawnMonitoredAsync(#beamtalk_object{}) -> {#beamtalk_object{}, reference()}.
doSpawnMonitoredAsync(Self) ->
    do_spawn_or_raise(Self, [], spawnMonitoredAsync, class_self_spawn_monitored_async).

-doc """
FFI shim for `class sealed spawnMonitoredAsyncWith: initArgs -> Tuple` (BT-4477).

Same contract as `doSpawnMonitoredAsync/1` plus the initialisation arguments
passed to the actor's `init/1` callback.
""".
-spec doSpawnMonitoredAsyncWith(#beamtalk_object{}, term()) ->
    {#beamtalk_object{}, reference()}.
doSpawnMonitoredAsyncWith(Self, InitArgs) ->
    do_spawn_or_raise(
        Self, [InitArgs], 'spawnMonitoredAsyncWith:', class_self_spawn_monitored_async
    ).

-doc """
FFI shim for `class spawnAs: name :: Symbol -> Result(Self, Error)`.

`Self` is the class object `{beamtalk_object, 'ClassName class', Module, ClassPid}`.
Delegates to `'spawnAs'/3` with an empty args map, then wraps the returned pid
into a `#beamtalk_object{}` carrying the receiver class.
""".
-spec doSpawnAs(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAs(Self, Name) ->
    do_spawn_with_selector(local, Self, #{}, Name, 'spawnAs:').

-doc """
FFI shim for `class spawnWith: initArgs as: name :: Symbol -> Result(Self, Error)`.

Atomically spawns an actor registered under `Name`. Returns a tagged
`{ok, ActorObject} | {error, #beamtalk_error{}}` tuple.
""".
-spec doSpawnWith(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWith(Self, InitArgs, Name) ->
    do_spawn_with_selector(local, Self, InitArgs, Name, 'spawnWith:as:').

-doc """
FFI shim for `class spawnAs: name :: Symbol scope: scope :: Symbol ->
Result(Self, Error)` (ADR 0126 §4, Phase 5, BT-3603).

`scope: #local` behaves identically to `doSpawnAs/2`; `scope: #global`
registers with OTP `global` instead (`'spawnAsGlobal'/3`, via
`do_spawn_with_selector/5`). Any other scope Symbol is a structured
`type_error` (ADR 0126 §4: "Any other scope symbol →
`#beamtalk_error{kind = type_error}`").
""".
-spec doSpawnAsScope(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAsScope(Self, Name, ScopeSym) ->
    case resolve_scope(ScopeSym, 'spawnAs:scope:') of
        {ok, Scope} ->
            do_spawn_with_selector(Scope, Self, #{}, Name, 'spawnAs:scope:');
        {error, #beamtalk_error{} = Err} ->
            {error, attach_class_if_known(Self, Err, 'spawnAs:scope:')}
    end.

-doc """
FFI shim for `class spawnWith: initArgs :: Object as: name :: Symbol scope:
scope :: Symbol -> Result(Self, Error)` (ADR 0126 §4, Phase 5, BT-3603).

Same contract as `doSpawnAsScope/3` plus the initialisation arguments — see
that function's doc for the scope validation.
""".
-spec doSpawnWithAsScope(#beamtalk_object{}, term(), term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWithAsScope(Self, InitArgs, Name, ScopeSym) ->
    case resolve_scope(ScopeSym, 'spawnWith:as:scope:') of
        {ok, Scope} ->
            do_spawn_with_selector(Scope, Self, InitArgs, Name, 'spawnWith:as:scope:');
        {error, #beamtalk_error{} = Err} ->
            {error, attach_class_if_known(Self, Err, 'spawnWith:as:scope:')}
    end.

-doc """
Resolve a `scope :: Symbol` FFI argument to `local` or `global` (ADR 0126
§4). `#local` is the explicit no-op alias for the unscoped selectors;
`#global` backs the name with OTP `global`. Any other Symbol (or non-Symbol)
is a structured `type_error` — the shared validation behind every
`...scope:` FFI shim (`doSpawnAsScope/3`, `doSpawnWithAsScope/4`,
`doNamedScope/3`), so the accepted vocabulary and its error shape can only
drift in one place.
""".
-spec resolve_scope(term(), atom()) -> {ok, local | global} | {error, #beamtalk_error{}}.
resolve_scope(local, _Selector) ->
    {ok, local};
resolve_scope(global, _Selector) ->
    {ok, global};
resolve_scope(Other, Selector) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', Selector),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "scope must be #local or #global, got ~tp", [Other]
                )
            )
        )}.

-doc """
Attach the receiver's class name (when resolvable) to a `#beamtalk_error{}`
raised before `class_self_to_name_and_module/1` would otherwise have done
so — `resolve_scope/2`'s type_error fires before that lookup runs, but the
error should still read as `Counter spawnAs:scope:`, not the generic
`'Actor'` placeholder, when the class is available. Falls back to leaving
the error's `'Actor'` class untouched when `Self` is not a resolvable class
receiver — this is a best-effort attribution, not itself the type check.
""".
-spec attach_class_if_known(term(), #beamtalk_error{}, atom()) -> #beamtalk_error{}.
attach_class_if_known(Self, Err, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, _Module} -> Err#beamtalk_error{class = ClassName, selector = Selector};
        {error, _} -> Err
    end.

%% Shared implementation for doSpawnAs/2, doSpawnWith/3, doSpawnAsScope/3,
%% and doSpawnWithAsScope/4. `Scope` selects `'spawnAs'/3` (local) or
%% `'spawnAsGlobal'/3` (global, ADR 0126 §4). The `Selector` parameter
%% controls which public-facing Beamtalk selector is threaded into returned
%% structured errors so error messages match the method the user called.
%%
%% `spawnAs:`/`spawnWith:as:` are `class` methods (ADR 0079 atomic naming),
%% so an EXTERNAL send (`Logger spawnAs: #foo`) reaches this function via the
%% normal class dispatch `gen_server:call` into the receiver class's own
%% singleton gen_server process (`dispatch_codegen.rs`: "a class send is a
%% gen_server:call into the singleton class process") — this function then
%% executes INSIDE that class process, which is therefore the one
%% `'spawnAs'/3`'s `gen_server:start_link` links to. `class_self_spawn_as`/
%% `do_class_self_named_spawn` (`beamtalk_class_instantiation.erl`) is a
%% SEPARATE deadlock-avoiding shortcut for the same selector reached only via
%% a same-class `self spawnAs:`/`self spawnWith:as:` send (`self`-dispatch
%% inside that class's own gen_server call handler would deadlock on a
%% second `gen_server:call` to itself) — it already unlinks, mirrored below.
%% This path did not, leaving the class's own gen_server process
%% permanently linked to every actor it spawns this way. BT-3596 made that
%% asymmetry newly dangerous: once the spawned actor traps exits (because it
%% overrides `terminate:`), an untrapped normal exit of its own class
%% process — previously inert, since a non-trapping linked partner ignores a
%% `normal` reason — now tears the actor down via `terminate/2` regardless
%% of reason, via the same built-in gen_server "parent EXIT" handling this
%% PR relies on for supervisor cascades. Sever it immediately.
%%
%% Guarded on `?BT_SUPERVISOR_SPAWN_CONTEXT_KEY` for consistency with the
%% self-send unlink site and `safe_spawn/2`, though it can never actually be
%% set here in practice: `beamtalk_supervisor:start_child_via_class_method/4`
%% (the only place that sets it) deliberately runs a `withClassMethod:`
%% factory via `call_class_method_direct`/`erlang:apply/3` — staying in the
%% real supervisor process precisely so a plain `self spawn`/`self spawnAs:`
%% inside it links to the supervisor, not the class gen_server — and never
%% through `class_send`'s `gen_server:call`, which is the only way this
%% function is ever reached. A future caller that did reach here from a
%% context where the key is visible would still get the correct behavior.
-spec do_spawn_with_selector(local | global, #beamtalk_object{}, term(), term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_spawn_with_selector(Scope, Self, InitArgs, Name, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            %% BT-3992: unlinked inside the start (unless in a
            %% withClassMethod: supervisor factory), not after it returned.
            case beamtalk_actor_registry:spawn_named_for_caller(Scope, Name, Module, InitArgs) of
                {ok, Pid} ->
                    %% ADR 0126 §4: the `local` scope returns the raw pid
                    %% (unchanged, `doSpawnAs/2`'s existing contract) — a
                    %% local `{local, Name}` registration is bidirectional
                    %% (`erlang:process_info(Pid, registered_name)` already
                    %% resolves it back), so a raw pid loses nothing.
                    %% `global` has no such reverse pid->name lookup, so the
                    %% raw pid alone would make `registeredName`/
                    %% `isRegistered` (and restart survival) silently stop
                    %% working — return the name-resolving `{global, Name}`
                    %% proxy instead, exactly what `named:scope:#global`
                    %% would hand back for this same actor a moment later.
                    IdentitySlot =
                        case Scope of
                            local -> Pid;
                            global -> proxy_ref_for_scope(global, Name)
                        end,
                    {ok, #beamtalk_object{
                        class = ClassName,
                        class_mod = Module,
                        pid = IdentitySlot
                    }};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{
                        class = ClassName,
                        selector = Selector
                    }};
                {error, Reason} ->
                    {error, generic_spawn_error(ClassName, Selector, Reason)}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
    end.

-doc """
FFI shim for `registerAs: name :: Symbol -> Result(Self, Error)`.

On success returns the receiver (the actor object) wrapped in `{ok, _}` so
the stdlib boundary materialises it as `Result ok: self` — matching ADR 0079's
fluent-chain contract.
""".
-spec registerAs(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
registerAs(Self, Name) when is_record(Self, beamtalk_object) ->
    ClassName = Self#beamtalk_object.class,
    case proxy_pid(Self, 'registerAs:') of
        {ok, Pid} ->
            case beamtalk_actor_registry:register_name(Name, Pid) of
                {ok, _} ->
                    {ok, Self};
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{
                        class = ClassName,
                        selector = 'registerAs:'
                    }}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{
                class = ClassName,
                selector = 'registerAs:'
            }}
    end;
registerAs(Self, _Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'registerAs:'),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "registerAs: expects an Actor receiver, got ~tp", [Self]
                )
            )
        )}.

-doc """
FFI shim for `unregister -> Symbol`.

Idempotent: returns `#ok` even if the receiver was not registered or was
already unregistered. Only raises on real failures (reserved-name, type
error) — and, for a node-qualified proxy pointing at a genuinely remote
node (ADR 0126 §3), `node_down`/`timeout` when that node cannot be reached
to find out, since the name may still be registered there.
""".
-spec unregister(#beamtalk_object{}) -> ok.
unregister(#beamtalk_object{pid = {registered, Name0, Node0}} = Self) when
    is_atom(Name0), is_atom(Node0), Node0 =/= node()
->
    %% ADR 0126 §3: a genuinely remote node-qualified proxy. `erlang:
    %% process_info/2` (`registered_name_for_pid/1`'s TOCTOU guard below,
    %% via unregister_resolved/2) only accepts *local* pids — badarg for a
    %% pid on another node — so the whole read-check-unregister sequence
    %% must run on `Node0` itself, via a plain two-tuple recursive call to
    %% this same function there, not here.
    %% Unlike the sibling remote ops (remote_spawn/4, remote_named/3,
    %% remote_all_registered/1), an unreachable node here is NOT coerced to
    %% a quiet `ok`: `unregister`'s "only raises on real failures" contract
    %% means idempotent success is reserved for "already unregistered",
    %% never for "couldn't find out" — the actor may still be registered on
    %% Node0. Raise the same node_down/timeout error the siblings surface.
    try
        erpc:call(
            Node0,
            ?BT_ACTOR_WIRE_MODULE,
            unregister,
            [Self#beamtalk_object{pid = {registered, Name0}}],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        ok -> ok
    catch
        error:{erpc, noconnection} ->
            beamtalk_error:raise(
                (beamtalk_actor:node_down_error_record(Node0, unregister))#beamtalk_error{
                    class = Self#beamtalk_object.class
                }
            );
        error:{erpc, timeout} ->
            beamtalk_error:raise(
                (beamtalk_actor_remote:remote_timeout_error_record(
                    Self#beamtalk_object.class, unregister, true
                ))
            );
        error:{erpc, ErpcReason} ->
            beamtalk_error:raise(
                generic_remote_error(Self#beamtalk_object.class, unregister, {erpc, ErpcReason})
            );
        error:{exception, Reason, _Stack} ->
            beamtalk_error:raise(remote_unregister_error(Self#beamtalk_object.class, Reason));
        exit:{exception, Reason} ->
            beamtalk_error:raise(remote_unregister_error(Self#beamtalk_object.class, Reason))
    end;
unregister(#beamtalk_object{pid = {global, Name0}}) when is_atom(Name0) ->
    %% ADR 0126 §4: `unregister` is the ADR 0079 *local* per-instance
    %% unregistration primitive (`registerAs:`'s counterpart) — it has no
    %% meaning for a cluster-unique `global` name, which is never touched by
    %% `registerAs:`/`unregister` in the first place (only `spawnAs:scope:`/
    %% `named:scope:` ever produce a `{global, Name}` identity slot). Raise a
    %% clean `type_error` rather than falling through to a `case_clause`
    %% crash in the local-scope clause below.
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', unregister),
            <<
                "unregister only applies to local (ADR 0079) registrations; "
                "a cluster-global (scope: #global) actor cannot be "
                "unregistered this way"
            >>
        )
    );
unregister(Self) when is_record(Self, beamtalk_object) ->
    %% ADR 0079: name-resolving proxies (`pid = {registered, N}`, including
    %% a node-qualified ref whose Node is *this* node) derive the pid via
    %% `whereis/1`. If the name has gone, treat as idempotent — there is
    %% nothing to unregister.
    Pid =
        case Self#beamtalk_object.pid of
            P when is_pid(P) ->
                P;
            {registered, Name0} when is_atom(Name0) ->
                case erlang:whereis(Name0) of
                    undefined -> dead;
                    Live when is_pid(Live) -> Live
                end;
            {registered, Name0, Node0} when is_atom(Name0), Node0 =:= node() ->
                case erlang:whereis(Name0) of
                    undefined -> dead;
                    Live when is_pid(Live) -> Live
                end
        end,
    case Pid of
        dead ->
            ok;
        _ ->
            unregister_resolved(Self, Pid)
    end;
unregister(Self) ->
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', unregister),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "unregister expects an Actor receiver, got ~tp", [Self]
                )
            )
        )
    ).

-spec unregister_resolved(#beamtalk_object{}, pid()) -> ok.
unregister_resolved(Self, Pid) ->
    case beamtalk_actor_registry:registered_name_for_pid(Pid) of
        nil ->
            ok;
        Name ->
            %% TOCTOU guard: between `registered_name_for_pid/1` above and
            %% `unregister_name/1` below, another process can claim the freed
            %% name. Only unregister when the registry still points at our
            %% pid; otherwise treat as idempotent (someone else beat us to it,
            %% possibly with a replacement actor of the same name).
            case erlang:whereis(Name) of
                Pid ->
                    case beamtalk_actor_registry:unregister_name(Name) of
                        ok ->
                            ok;
                        {error, #beamtalk_error{kind = name_registered}} ->
                            %% Raced with another unregister — still idempotent.
                            ok;
                        {error, #beamtalk_error{} = Err0} ->
                            Err1 = beamtalk_error:with_selector(Err0, unregister),
                            beamtalk_error:raise(
                                Err1#beamtalk_error{class = Self#beamtalk_object.class}
                            )
                    end;
                _Other ->
                    %% Name is gone or held by another pid — nothing to do.
                    ok
            end
    end.

-doc """
FFI shim for `registeredName -> Symbol | Nil`.

Returns the atom the actor is registered under, or `nil` when the actor has no
registered name (or has terminated).
""".
-spec registeredName(#beamtalk_object{}) -> atom() | nil.
registeredName(Self) when is_record(Self, beamtalk_object) ->
    case Self#beamtalk_object.pid of
        {registered, Name} when is_atom(Name) ->
            %% ADR 0079: name-resolving proxy answers from the
            %% identity slot directly — survives the registered actor
            %% being restarted under the same name.
            Name;
        {registered, Name, _Node} when is_atom(Name) ->
            %% ADR 0126 §3: node-qualified proxy — same, answers from the
            %% identity slot (the Symbol is node-independent).
            Name;
        {global, Name} when is_atom(Name) ->
            %% ADR 0126 §4: cluster-unique `scope: #global` proxy — same.
            Name;
        Pid when is_pid(Pid) ->
            beamtalk_actor_registry:registered_name_for_pid(Pid)
    end;
registeredName(_) ->
    nil.

-doc """
FFI shim for `isRegistered -> Boolean`.
""".
-spec isRegistered(#beamtalk_object{}) -> boolean().
isRegistered(Self) when is_record(Self, beamtalk_object) ->
    case Self#beamtalk_object.pid of
        {registered, _Name} ->
            %% ADR 0079: a name-resolving proxy is by
            %% construction registered. The proxy stays "registered"
            %% across restarts because the supervisor re-registers
            %% the name on each restart.
            true;
        {registered, _Name, _Node} ->
            %% ADR 0126 §3: node-qualified proxy — same reasoning.
            true;
        {global, _Name} ->
            %% ADR 0126 §4: cluster-unique `scope: #global` proxy — same.
            true;
        Pid when is_pid(Pid) ->
            beamtalk_actor_registry:registered_name_for_pid(Pid) =/= nil
    end;
isRegistered(_) ->
    false.

-doc """
FFI shim for `class named: name :: Symbol -> Result(Self, Error)`.

Looks up the pid registered under `Name`, validates the registered actor's
class is (or descends from) the receiver class via the `'$beamtalk_actor'`
process-dictionary marker, and returns a `#beamtalk_object{}` narrowed to the
receiver class. Returns structured errors for the `name_not_registered`,
`wrong_class`, and `type_error` cases per ADR 0079.
""".
-spec named(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
named(Self, Name) when is_atom(Name) ->
    case class_self_to_name_and_module(Self) of
        {ok, ReceiverClass, _ReceiverModule} ->
            named_lookup(ReceiverClass, Name, 'named:');
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'named:')}
    end;
named(_Self, Name) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'named:'),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "named: expects a Symbol, got ~tp", [Name]
                )
            )
        )}.

-doc """
FFI shim for `class named: name :: Symbol scope: scope :: Symbol ->
Result(Self, Error)` (ADR 0126 §4, Phase 5, BT-3603).

`scope: #local` behaves identically to `named/2`; `scope: #global` looks
`Name` up via OTP `global` instead — the returned proxy's identity slot is
`{global, Name}` (cluster-wide, resolved via `global:whereis_name/1` on
every subsequent send — see `named_lookup/4`'s doc). Any other scope Symbol
is a structured `type_error`, matching `doSpawnAsScope/3`'s validation
(`resolve_scope/2`).
""".
-spec doNamedScope(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doNamedScope(Self, Name, ScopeSym) when is_atom(Name) ->
    case resolve_scope(ScopeSym, 'named:scope:') of
        {ok, Scope} ->
            case class_self_to_name_and_module(Self) of
                {ok, ReceiverClass, _ReceiverModule} ->
                    named_lookup(Scope, ReceiverClass, Name, 'named:scope:');
                {error, #beamtalk_error{} = Err} ->
                    {error, beamtalk_error:with_selector(Err, 'named:scope:')}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, attach_class_if_known(Self, Err, 'named:scope:')}
    end;
doNamedScope(_Self, Name, _ScopeSym) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'named:scope:'),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "named:scope: expects a Symbol name, got ~tp", [Name]
                )
            )
        )}.

-doc """
Shared implementation behind `named/2` (local `named:`) and
`remote_named_target/2` (`named:on:`, ADR 0126 §3, BT-3599) — the actual
by-name lookup + class check, parameterised on `ReceiverClass` (rather than
a `#beamtalk_object{}` self to resolve it from) so the remote path can pass
the class name shipped over `erpc` directly, and on `Selector` so error
records name whichever public selector the caller actually used. Thin
`local`-scope wrapper over `named_lookup/4` — kept as its own arity since
`remote_named_target/2` (an `erpc` target, arity matters) and existing call
sites already spell it this way.
""".
-spec named_lookup(atom(), atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
named_lookup(ReceiverClass, Name, Selector) ->
    named_lookup(local, ReceiverClass, Name, Selector).

-doc """
Scope-parametrised sibling of `named_lookup/3` (ADR 0126 §4, Phase 5,
BT-3603): `local` resolves `Name` via `erlang:whereis/1` and returns a
`{registered, Name}` proxy exactly as before; `global` resolves it via
`global:whereis_name/1` (cluster-wide, no node targeting needed — the name
means the same actor from every connected node) and returns a `{global,
Name}` proxy instead. Both scopes share the same class-check logic
(`pid_class_name/1` + `class_matches/2`) once a candidate pid is found —
only how the name resolves to a pid, and how the returned proxy re-resolves
it on every later send, differ.
""".
-spec named_lookup(local | global, atom(), atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
named_lookup(Scope, ReceiverClass, Name, Selector) ->
    ResolvedPid =
        case Scope of
            local -> erlang:whereis(Name);
            global -> global:whereis_name(Name)
        end,
    case ResolvedPid of
        undefined ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(name_not_registered, ReceiverClass, Selector),
                    beamtalk_error:chars_to_binary(
                        io_lib:format(
                            "No actor is registered under name '~ts'",
                            [Name]
                        )
                    )
                )};
        Pid when is_pid(Pid) ->
            %% A `global` lookup's Pid can be on any connected node — the
            %% entire point of a cluster-unique name — so this must use
            %% the node-aware wrapper, not `pid_class_name/1` directly
            %% (which raises `badarg` for a remote pid). Always safe for
            %% `local` scope too, where Pid is always local.
            case
                resolve_named_class(
                    ReceiverClass, beamtalk_actor_registry:pid_class_candidates_remote_safe(Pid)
                )
            of
                none ->
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(wrong_class, ReceiverClass, Selector),
                            beamtalk_error:chars_to_binary(
                                io_lib:format(
                                    "Name '~ts' is registered to a non-Beamtalk "
                                    "process",
                                    [Name]
                                )
                            )
                        )};
                {shared, Candidates} ->
                    %% BT-4664: a `native:` actor whose backing module is shared
                    %% by several live classes — a Beamtalk actor, but the
                    %% runtime cannot tell which class it is.
                    {error,
                        beamtalk_error:with_hint(
                            beamtalk_error:new(wrong_class, ReceiverClass, Selector),
                            beamtalk_error:chars_to_binary(
                                io_lib:format(
                                    "Name '~ts' is registered to a native actor "
                                    "whose backing module is shared by classes ~ts; "
                                    "look it up through one of those classes",
                                    [Name, lists:join(", ", [atom_to_list(C) || C <- Candidates])]
                                )
                            )
                        )};
                {ok, ActualClass} ->
                    case class_matches(ActualClass, ReceiverClass) of
                        true ->
                            case class_mod_for(ActualClass) of
                                {ok, ActualModule} ->
                                    %% ADR 0079/§4: return a
                                    %% name-resolving proxy whose
                                    %% identity slot is `{registered,
                                    %% Name}` (local) or `{global, Name}`
                                    %% (global). The send-site re-
                                    %% resolves the name on every
                                    %% message, so the reference
                                    %% survives restarts. `remote_named/3`
                                    %% (ADR 0126 §3) node-qualifies the
                                    %% `local`-scope shape to `{registered,
                                    %% Name, Node}` for the `named:on:`
                                    %% caller; the `global`-scope shape is
                                    %% already node-independent and is
                                    %% never qualified (`beamtalk_pid:
                                    %% qualify_registered_ref/1`).
                                    _ = Pid,
                                    {ok, #beamtalk_object{
                                        class = ActualClass,
                                        class_mod = ActualModule,
                                        pid = proxy_ref_for_scope(Scope, Name)
                                    }};
                                not_found ->
                                    {error,
                                        beamtalk_error:with_hint(
                                            beamtalk_error:new(
                                                wrong_class,
                                                ReceiverClass,
                                                Selector
                                            ),
                                            beamtalk_error:chars_to_binary(
                                                io_lib:format(
                                                    "Registered actor class "
                                                    "'~ts' has no loaded module",
                                                    [ActualClass]
                                                )
                                            )
                                        )}
                            end;
                        false ->
                            {error,
                                beamtalk_error:with_hint(
                                    beamtalk_error:new(wrong_class, ReceiverClass, Selector),
                                    beamtalk_error:chars_to_binary(
                                        io_lib:format(
                                            "Name '~ts' is registered to a "
                                            "~ts, not a ~ts",
                                            [Name, ActualClass, ReceiverClass]
                                        )
                                    )
                                )}
                    end
            end
    end.

-doc """
FFI shim for `class allRegistered -> List(Actor)`.

Returns a list of `#beamtalk_object{}` proxies for every currently-registered
Beamtalk actor (those carrying the `'$beamtalk_actor'` marker, or a
`native:` actor's backing process, whose class `pid_class_name/1` derives from
its backing module, BT-4657). Kernel
processes and raw OTP-registered processes are excluded.
""".
-spec allRegistered(#beamtalk_object{}) -> [#beamtalk_object{}].
allRegistered(_Self) ->
    lists:filtermap(
        fun(Name) ->
            case erlang:whereis(Name) of
                undefined ->
                    false;
                Pid when is_pid(Pid) ->
                    case beamtalk_actor_registry:pid_class_name(Pid) of
                        nil ->
                            false;
                        ClassName ->
                            case class_mod_for(ClassName) of
                                {ok, Module} ->
                                    %% ADR 0079: enumerate as
                                    %% name-resolving proxies so subsequent
                                    %% sends survive restarts of the
                                    %% supervised actor under that name.
                                    _ = Pid,
                                    {true, #beamtalk_object{
                                        class = ClassName,
                                        class_mod = Module,
                                        pid = {registered, Name}
                                    }};
                                not_found ->
                                    false
                            end
                    end
            end
        end,
        beamtalk_actor_registry:all_registered()
    ).

-doc """
FFI shim for `class spawnOn: node :: Node -> Result(Self, Error)` (ADR 0126
§3, Phase 2, BT-3599).

Anonymous remote spawn: same contract as `spawn`, but the actor process
starts on `node` instead of here. **Not idempotent** — see `remote_spawn/4`'s
doc. **No links, no local tracking**: the spawned actor is never linked to
this caller (a link across a node boundary would turn a partition into a
crash here) and this workspace does not track it — `ActorSpawned` fires on
`node`, not here, so the actor outlives this session unless something else
stops it.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnOn: worker) unwrap
c node        // => Node(worker@localhost)
```
""".
-spec doSpawnOn(#beamtalk_object{}, term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnOn(Self, NodeArg) ->
    do_remote_spawn(Self, undefined, NodeArg, 'spawnOn:').

-doc """
FFI shim for `class spawnAs: name :: Symbol on: node :: Node ->
Result(Self, Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `spawnAs:`, plus the target `node`: the reserved-name check
and the atomic name registration both run on `node`, through the same
public `'spawnAs'/3` the local selector uses. **Not idempotent** — a timeout
after the far-side spawn succeeded leaves an unowned, named actor running;
retry with `named:on:` instead of calling `spawnAs:on:` again (see
`remote_spawn/4`'s doc). **No links, no local tracking** — see `spawnOn:`.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Counter spawnAs: #hits on: worker) unwrap
hits := (Counter named: #hits on: worker) unwrap
```
""".
-spec doSpawnAsOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnAsOn(Self, Name, NodeArg) when is_atom(Name) ->
    do_remote_spawn(Self, Name, NodeArg, 'spawnAs:on:');
doSpawnAsOn(Self, Name, _NodeArg) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, _Module} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, ClassName, 'spawnAs:on:'),
                    beamtalk_error:chars_to_binary(
                        io_lib:format("spawnAs:on: expects a Symbol name, got ~tp", [Name])
                    )
                )};
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'spawnAs:on:')}
    end.

%% Shared implementation for doSpawnOn/2 and doSpawnAsOn/3.
-spec do_remote_spawn(#beamtalk_object{}, atom() | undefined, term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_remote_spawn(Self, NameOrUndefined, NodeArg, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case beamtalk_actor_remote:node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    case
                        beamtalk_actor_remote:remote_spawn(
                            Node, ClassName, NameOrUndefined, Selector
                        )
                    of
                        {ok, Pid} ->
                            {ok, #beamtalk_object{class = ClassName, class_mod = Module, pid = Pid}};
                        {error, #beamtalk_error{} = Err} ->
                            {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
                    end;
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
    end.

-doc """
FFI shim for `class spawnWith: initArgs :: Object on: node :: Node ->
Result(Self, Error)` (ADR 0126 §5.1, Phase 3b, BT-3601).

Same contract as `spawnOn:`, plus the initialisation arguments passed to the
actor's `init/1` callback on `node` — `initArgs` is wire-encoded before it
crosses the node boundary (see `remote_spawn_with/5`'s doc). **Not
idempotent**, **no links, no local tracking** — see `spawnOn:`.

Error cases: as `spawnOn:`, plus `not_serialisable` (`initArgs` holds a
node-scoped handle).

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnWith: #{#count => 10} on: worker) unwrap
```
""".
-spec doSpawnWithOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWithOn(Self, InitArgs, NodeArg) ->
    do_remote_spawn_with(Self, InitArgs, undefined, NodeArg, 'spawnWith:on:').

-doc """
FFI shim for `class spawnWith: initArgs :: Object as: name :: Symbol on:
node :: Node -> Result(Self, Error)` (ADR 0126 §5.1, Phase 3b, BT-3601).

Same contract as `spawnAs:on:`, plus the initialisation arguments — see
`spawnWith:on:` for the wire-encoding note.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Counter spawnWith: #{#count => 10} as: #hits on: worker) unwrap
```
""".
-spec doSpawnWithAsOn(#beamtalk_object{}, term(), term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doSpawnWithAsOn(Self, InitArgs, Name, NodeArg) when is_atom(Name) ->
    do_remote_spawn_with(Self, InitArgs, Name, NodeArg, 'spawnWith:as:on:');
doSpawnWithAsOn(Self, _InitArgs, Name, _NodeArg) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, _Module} ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, ClassName, 'spawnWith:as:on:'),
                    beamtalk_error:chars_to_binary(
                        io_lib:format("spawnWith:as:on: expects a Symbol name, got ~tp", [Name])
                    )
                )};
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'spawnWith:as:on:')}
    end.

%% Shared implementation for doSpawnWithOn/3 and doSpawnWithAsOn/4.
-spec do_remote_spawn_with(#beamtalk_object{}, term(), atom() | undefined, term(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
do_remote_spawn_with(Self, InitArgs, NameOrUndefined, NodeArg, Selector) ->
    case class_self_to_name_and_module(Self) of
        {ok, ClassName, Module} ->
            case beamtalk_actor_remote:node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    case
                        beamtalk_actor_remote:remote_spawn_with(
                            Node, ClassName, NameOrUndefined, InitArgs, Selector
                        )
                    of
                        {ok, Pid} ->
                            {ok, #beamtalk_object{class = ClassName, class_mod = Module, pid = Pid}};
                        {error, #beamtalk_error{} = Err} ->
                            {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
                    end;
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, Selector)}
    end.

-doc """
FFI shim for `class named: name :: Symbol on: node :: Node ->
Result(Self, Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `named:`, run against `node`'s registry instead of this
node's. Returns a proxy whose identity slot is node-qualified
(`{registered, name, node}`) — see `remote_named/3`'s doc.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
hits := (Counter named: #hits on: worker) unwrap
hits increment   // => 1 — sent to worker, not resolved against this node
```
""".
-spec doNamedOn(#beamtalk_object{}, term(), term()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
doNamedOn(Self, Name, NodeArg) when is_atom(Name) ->
    case class_self_to_name_and_module(Self) of
        {ok, ReceiverClass, _Module} ->
            case beamtalk_actor_remote:node_arg_to_atom(NodeArg) of
                {ok, Node} ->
                    beamtalk_actor_remote:remote_named(Node, ReceiverClass, Name);
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ReceiverClass, selector = 'named:on:'}}
            end;
        {error, #beamtalk_error{} = Err} ->
            {error, beamtalk_error:with_selector(Err, 'named:on:')}
    end;
doNamedOn(_Self, Name, _NodeArg) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor', 'named:on:'),
            beamtalk_error:chars_to_binary(
                io_lib:format("named:on: expects a Symbol, got ~tp", [Name])
            )
        )}.

-doc """
FFI shim for `class allRegisteredOn: node :: Node -> Result(List(Actor),
Error)` (ADR 0126 §3, Phase 2, BT-3599).

Same contract as `allRegistered`, listing `node`'s registered actors instead
of this node's, wrapped in a `Result` (unlike the local, network-free
`allRegistered`, ADR 0060: reaching another node is an expected-failure
operation). Returns node-qualified proxies — see `remote_all_registered/1`'s
doc.

## Examples
```beamtalk
worker := (Node named: #'worker@localhost') unwrap
(Actor allRegisteredOn: worker) unwrap  // => #(an Actor(Counter), ...)
```
""".
-spec doAllRegisteredOn(#beamtalk_object{}, term()) ->
    {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
doAllRegisteredOn(_Self, NodeArg) ->
    case beamtalk_actor_remote:node_arg_to_atom(NodeArg) of
        {ok, Node} ->
            beamtalk_actor_remote:remote_all_registered(Node);
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{class = 'Actor', selector = 'allRegisteredOn:'}}
    end.

-spec class_self_to_name_and_module(term()) ->
    {ok, atom(), module()} | {error, #beamtalk_error{}}.
class_self_to_name_and_module(#beamtalk_object{pid = ClassPid}) when is_pid(ClassPid) ->
    try
        ClassName = beamtalk_object_class:class_name_safe(ClassPid),
        Module = beamtalk_object_class:module_name_safe(ClassPid),
        {ok, ClassName, Module}
    catch
        exit:_ ->
            {error,
                beamtalk_error:with_hint(
                    beamtalk_error:new(type_error, 'Actor'),
                    <<"Class object is not reachable">>
                )}
    end;
class_self_to_name_and_module(Other) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor'),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Expected a class receiver, got ~tp", [Other]
                )
            )
        )}.

-doc """
ADR 0079: derive a current pid from a `#beamtalk_object{}`
identity slot. Pid identities pass through unchanged; name-resolving
proxies look up the current pid via `whereis/1`. Returns a structured
`no_such_process` error when a proxy's name is not currently registered.
""".
-spec proxy_pid(#beamtalk_object{}, atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
%% The final clause is a defensive fallback for malformed identity slots
%% constructed outside the type system (e.g. from FFI). Dialyzer proves it
%% unreachable given the record spec, but keep it to raise a structured
%% error instead of `function_clause` when the invariant is violated.
-dialyzer({no_match, proxy_pid/2}).
proxy_pid(#beamtalk_object{pid = Pid}, _Selector) when is_pid(Pid) ->
    {ok, Pid};
proxy_pid(#beamtalk_object{pid = Ref}, Selector) when ?IS_PROXY_REF(Ref) ->
    %% BT-4676: the one proxy resolver lives in beamtalk_actor.
    beamtalk_actor:resolve_proxy_or_error(Ref, Selector);
proxy_pid(#beamtalk_object{class = ClassName, pid = Other}, Selector) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, ClassName, Selector),
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "proxy_pid/2 expects pid(), {registered, atom()}, or "
                    "{global, atom()} in #beamtalk_object.pid, got ~tp",
                    [Other]
                )
            )
        )}.

%% Pick the class a `named:` lookup resolves to from a pid's candidates
%% (BT-4664). An exact match on the receiver wins, then a single candidate
%% under it; several candidates under it leave the class undecidable, so
%% report them as `shared`. That includes a common-ancestor receiver
%% (`Actor named: #x`): rejecting it is intentional, as the proxy's class and
%% facade module would otherwise be arbitrary.
%%
%% Contract: classes sharing a backing module must each work against any
%% process of that module, so answering with the receiver's class (or its one
%% matching descendant) rather than the class the process was spawned through
%% is not a misclassification.
-spec resolve_named_class(atom(), [atom()]) -> none | {ok, atom()} | {shared, [atom()]}.
resolve_named_class(_ReceiverClass, []) ->
    none;
resolve_named_class(_ReceiverClass, [Class]) ->
    {ok, Class};
resolve_named_class(ReceiverClass, Candidates) ->
    case lists:member(ReceiverClass, Candidates) of
        true ->
            {ok, ReceiverClass};
        false ->
            case [C || C <- Candidates, class_matches(C, ReceiverClass)] of
                [Match] -> {ok, Match};
                _ -> {shared, Candidates}
            end
    end.

-spec class_matches(atom(), atom()) -> boolean().
class_matches(ActualClass, ReceiverClass) when ActualClass =:= ReceiverClass ->
    true;
class_matches(ActualClass, ReceiverClass) ->
    beamtalk_behaviour_intrinsics:walk_hierarchy(
        ActualClass,
        fun(CN, _CPid, _Acc) ->
            case CN of
                ReceiverClass -> {halt, true};
                _ -> {cont, false}
            end
        end,
        false
    ).

-spec class_mod_for(atom()) -> {ok, module()} | not_found.
class_mod_for(ClassName) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        {ok, Module} -> {ok, Module};
        not_found -> not_found
    end.

-doc "The `#beamtalk_object.pid` identity-slot shape a name-resolving lookup returns for `Scope` (ADR 0126 §4).".
-spec proxy_ref_for_scope(local | global, atom()) -> {registered, atom()} | {global, atom()}.
proxy_ref_for_scope(local, Name) -> {registered, Name};
proxy_ref_for_scope(global, Name) -> {global, Name}.

-spec generic_spawn_error(atom(), atom(), term()) -> #beamtalk_error{}.
generic_spawn_error(ClassName, Selector, Reason) ->
    beamtalk_error:with_hint(
        beamtalk_error:with_selector(
            beamtalk_error:new(instantiation_error, ClassName),
            Selector
        ),
        beamtalk_error:chars_to_binary(io_lib:format("spawn failed: ~tp", [Reason]))
    ).

-doc """
Catch-all for an unusual `erpc`/remote-side failure on a non-spawn remote op
(`named:on:`, `allRegisteredOn:`, remote `unregister`) — `generic_spawn_error/3`'s
`kind = instantiation_error`/"spawn failed" wording is specific to spawn and
would misdescribe a failed lookup or unregister, so those get this neutral
`runtime_error` instead (ADR 0126 §3).
""".
-spec generic_remote_error(atom(), atom(), term()) -> #beamtalk_error{}.
generic_remote_error(ClassName, Selector, Reason) ->
    beamtalk_error:with_hint(
        beamtalk_error:with_selector(
            beamtalk_error:new(runtime_error, ClassName),
            Selector
        ),
        beamtalk_error:chars_to_binary(io_lib:format("remote operation failed: ~tp", [Reason]))
    ).

-doc """
`erpc:call/5` re-surfaces a remote `beamtalk_error:raise/1` as
`error:{exception, Reason, Stacktrace}` at the origin, where `Reason` is
`raise/1`'s own wrap: `#{'$beamtalk_class' => _, error := #beamtalk_error{}}`
(`beamtalk_exception_handler:wrap/1`). When the remote `unregister/1` call
(via `unregister_resolved/2`) raised a genuine structured error this way —
`name_registered` on a lost TOCTOU race, a reserved-name/type error, etc. —
unwrap and re-raise *that* error, with this node's own view of the class
and `unregister` selector, so the caller sees the real `kind`/`hint` instead
of a generic wrapper. Any other exception shape falls back to
`generic_remote_error/3`.
""".
-spec remote_unregister_error(atom(), term()) -> #beamtalk_error{}.
remote_unregister_error(ClassName, #{error := #beamtalk_error{} = Err}) ->
    Err#beamtalk_error{class = ClassName, selector = unregister};
remote_unregister_error(ClassName, Reason) ->
    generic_remote_error(ClassName, unregister, Reason).
