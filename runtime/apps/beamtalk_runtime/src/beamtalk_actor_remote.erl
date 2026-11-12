%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_remote).

%%% **DDD Context:** Actor System Context

-moduledoc """
Remote actor spawn and lookup (ADR 0126 §3, Phase 2, BT-3599).

Split out of `beamtalk_actor` (BT-4701). Backs `spawnOn:`, `spawnAs:on:`,
`spawnWith:on:`, `spawnWith:as:on:`, `named:on:` and `allRegisteredOn:`.
Every entry point is a pair: an origin-side function (`remote_*`) that applies
`beamtalk_node:connect_policy/2`, makes the `erpc:call/5` and maps `erpc` /
connection failures to `#beamtalk_error{}`; and, for spawn/lookup, a
`*_target` function that runs ON the target node.

## Wire compatibility

The `*_target` functions are `erpc` targets, so their module name is part of
the wire contract between nodes. `beamtalk_actor` keeps one-line delegates
for `remote_spawn_target/2`, `remote_spawn_with_target/3` and
`remote_named_target/2`, and every `erpc:call/5` made from this module names
`beamtalk_actor` (`?BT_ACTOR_WIRE_MODULE`), so a node running this release
can drive a peer that predates the split and vice versa. The delegates stay
for at least one release.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    node_arg_to_atom/1,
    remote_all_registered/1,
    remote_named/3,
    remote_named_target/2,
    remote_spawn/4,
    remote_spawn_target/2,
    remote_spawn_with/5,
    remote_spawn_with_target/3,
    remote_timeout_error_record/3
]).

-doc """
Origin-side entry point for `spawnOn:`/`spawnAs:on:` (ADR 0126 §3).

`ClassName` is resolved **on the target node** by `remote_spawn_target/2` —
the caller's own compiled module is never shipped (ADR 0126 §3
"Implementation shape"), only the class name, so a class loaded under a
different module name (or a different version) on the target node still
resolves there. `NameOrUndefined` is `undefined` for an anonymous `spawnOn:`
or the requested name (already reserved-name-checked, on the target node,
by `'spawnAs'/3`) for `spawnAs:on:`.

**Not idempotent.** A `timeout` here does not mean the spawn failed — the
`erpc` call may have completed on the target node after the local timeout
fired. Retrying an anonymous `spawnOn:` risks a duplicate actor; a named
`spawnAs:on:` retried after `name_registered` can be resolved with
`named:on:` instead of retried blindly (ADR 0126 §3).
""".
-spec remote_spawn(node(), atom(), atom() | undefined, atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn(Node, ClassName, NameOrUndefined, Selector) ->
    remote_spawn_erpc(
        Node, ClassName, NameOrUndefined, Selector, remote_spawn_target, fun() -> {ok, []} end
    ).

-doc """
Runs ON the target node, in the `erpc` worker process `remote_spawn/4`'s
call spawns there (ADR 0126 §3).

Resolves `ClassName` to its module via `class_mod_for/1` — the *target*
node's own class metadata, matching "the class is resolved by name on the
target node" — answering `class_not_found` (an existing kind, now reachable
remotely) when it is not loaded there, exactly as the local instantiation
path already does before ever reaching a spawn primitive.

An anonymous spawn (`NameOrUndefined =:= undefined`) delegates to
`safe_spawn/2`, which (outside a supervisor context, never the case for an
`erpc` worker) uses `gen_server:start/3` — already unlinked, so no
unlink-after-start dance is needed there. A named spawn delegates to the
public `'spawnAs'/3` wrapper (reserved-name check + `safe_spawn_named/5`,
BT-3579's Phase 0.5 correction to this ADR — `safe_spawn_named/5` itself is
not `-export`ed) and **unlinks immediately** after a successful start: the
calling `erpc` worker process exits with a non-`normal` reason once this
function returns, and a still-linked actor would die right along with it
(mirrors `beamtalk_class_instantiation:do_class_self_named_spawn/6`'s own
unlink for the local class-method `self spawnAs:` path).
""".
-spec remote_spawn_target(atom(), atom() | undefined) ->
    {ok, pid()} | {error, term()}.
remote_spawn_target(ClassName, NameOrUndefined) ->
    case beamtalk_actor_ffi:class_mod_for(ClassName) of
        not_found ->
            {error, beamtalk_error:new(class_not_found, ClassName)};
        {ok, Module} ->
            case NameOrUndefined of
                undefined ->
                    beamtalk_actor:safe_spawn(Module, #{});
                Name when is_atom(Name) ->
                    %% BT-3992: unlinked inside the start.
                    beamtalk_actor_registry:spawn_named_unlinked(Name, Module, #{})
            end
    end.

-doc """
Origin-side entry point for `spawnWith:on:`/`spawnWith:as:on:` (ADR 0126
§5.1, Phase 3b, BT-3601) — `remote_spawn/4`'s sibling for the Value-carrying
spawn selectors deferred at Phase 2 (BT-3599) until the wire encoder landed.

`InitArgs` is wire-encoded before the `erpc` call — a remote spawn never
ships raw, unversioned args, exactly like every other cross-node send
(§5.1). An encode failure (e.g. `initArgs` holds a node-scoped `Ets`) is a
request-direction failure: it never reaches the network, and is returned
here in the caller, matching `wire_sync_call/5`'s send-side behaviour for
ordinary sends. Otherwise shares `remote_spawn/4`'s connect-policy check and
`erpc`/connection failure mapping verbatim, via `remote_spawn_erpc/6`.
""".
-spec remote_spawn_with(node(), atom(), atom() | undefined, term(), atom()) ->
    {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn_with(Node, ClassName, NameOrUndefined, InitArgs, Selector) ->
    remote_spawn_erpc(
        Node,
        ClassName,
        NameOrUndefined,
        Selector,
        remote_spawn_with_target,
        fun() ->
            case beamtalk_wire:encode(InitArgs) of
                {ok, WireInitArgs} -> {ok, [WireInitArgs]};
                {error, #beamtalk_error{}} = EncErr -> EncErr
            end
        end
    ).

-doc """
Runs ON the target node, in the `erpc` worker process `remote_spawn_with/5`'s
call spawns there (ADR 0126 §5.1) — decodes `WireInitArgs` (this node's own
class registry resolves any Value envelope or class ref it carries), then
otherwise mirrors `remote_spawn_target/2` exactly, including the named-spawn
unlink.
""".
-spec remote_spawn_with_target(atom(), atom() | undefined, term()) ->
    {ok, pid()} | {error, term()}.
remote_spawn_with_target(ClassName, NameOrUndefined, WireInitArgs) ->
    case beamtalk_wire:decode(WireInitArgs) of
        {ok, InitArgs} ->
            case beamtalk_actor_ffi:class_mod_for(ClassName) of
                not_found ->
                    {error, beamtalk_error:new(class_not_found, ClassName)};
                {ok, Module} ->
                    case NameOrUndefined of
                        undefined ->
                            beamtalk_actor:safe_spawn(Module, InitArgs);
                        Name when is_atom(Name) ->
                            %% BT-3992: unlinked inside the start.
                            beamtalk_actor_registry:spawn_named_unlinked(Name, Module, InitArgs)
                    end
            end;
        {error, #beamtalk_error{}} = Err ->
            Err
    end.

-doc """
Shared connect-policy-check + `erpc:call/5` + catch-mapping for
`remote_spawn/4` and `remote_spawn_with/5` (AGENTS.md "No duplicate
implementations") — the two differ only in which target function `erpc`
invokes on the remote node and what, if anything, is computed (and may
itself fail, e.g. `remote_spawn_with/5`'s wire-encode) as that target
function's extra arguments beyond `[ClassName, NameOrUndefined]`.

`BuildExtraArgs` is invoked only after `connect_policy/2` allows the
connection — never before, matching both callers' original ordering (a
refused connection is reported without ever running `remote_spawn_with/5`'s
wire-encode) — and its own `{error, #beamtalk_error{}}` is `class`/`selector`
tagged exactly like a `connect_policy/2` refusal or an `erpc` failure below.
""".
-spec remote_spawn_erpc(
    node(),
    atom(),
    atom() | undefined,
    atom(),
    atom(),
    fun(() -> {ok, [term()]} | {error, #beamtalk_error{}})
) -> {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn_erpc(Node, ClassName, NameOrUndefined, Selector, TargetFun, BuildExtraArgs) ->
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = ClassName, selector = Selector}};
        ok ->
            case BuildExtraArgs() of
                {ok, ExtraArgs} ->
                    remote_spawn_erpc_call(
                        Node, ClassName, NameOrUndefined, Selector, TargetFun, ExtraArgs
                    );
                {error, #beamtalk_error{} = Err} ->
                    {error, Err#beamtalk_error{class = ClassName, selector = Selector}}
            end
    end.

-spec remote_spawn_erpc_call(
    node(), atom(), atom() | undefined, atom(), atom(), [term()]
) -> {ok, pid()} | {error, #beamtalk_error{}}.
remote_spawn_erpc_call(Node, ClassName, NameOrUndefined, Selector, TargetFun, ExtraArgs) ->
    try
        erpc:call(
            Node,
            ?BT_ACTOR_WIRE_MODULE,
            TargetFun,
            [ClassName, NameOrUndefined | ExtraArgs],
            ?BT_REMOTE_CALL_TIMEOUT
        )
    of
        {ok, Pid} ->
            {ok, Pid};
        {error, #beamtalk_error{} = Err} ->
            {error, Err#beamtalk_error{class = ClassName, selector = Selector}};
        {error, Reason} ->
            {error, beamtalk_actor_ffi:generic_spawn_error(ClassName, Selector, Reason)}
    catch
        error:{erpc, noconnection} ->
            {error, beamtalk_actor:node_down_error_record(Node, Selector)};
        error:{erpc, timeout} ->
            {error, remote_timeout_error_record(ClassName, Selector, false)};
        error:{erpc, ErpcReason} ->
            {error,
                beamtalk_actor_ffi:generic_spawn_error(ClassName, Selector, {erpc, ErpcReason})};
        error:{exception, Reason, _Stack} ->
            {error, beamtalk_actor_ffi:generic_spawn_error(ClassName, Selector, Reason)};
        exit:{exception, Reason} ->
            {error, beamtalk_actor_ffi:generic_spawn_error(ClassName, Selector, Reason)}
    end.

-doc """
Origin-side entry point for `named:on:` (ADR 0126 §3).

Runs the actual lookup (`named_lookup/3`) on the target node via `erpc` —
the `'$beamtalk_actor'` class-marker read it needs is local-only
(`process_info/2`) — then node-qualifies the returned proxy's identity slot
to `{registered, Name, Node}` (`qualify_registered_ref/2`) so it keeps
resolving against `Node`, its origin, rather than whichever node next reads
it (ADR 0126 §3 "Registered refs are node-qualified on the wire").
""".
-spec remote_named(node(), atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
remote_named(Node, ReceiverClass, Name) ->
    Selector = 'named:on:',
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = ReceiverClass, selector = Selector}};
        ok ->
            try
                erpc:call(
                    Node,
                    ?BT_ACTOR_WIRE_MODULE,
                    remote_named_target,
                    [ReceiverClass, Name],
                    ?BT_REMOTE_CALL_TIMEOUT
                )
            of
                {ok, Obj} ->
                    {ok, qualify_registered_ref(Obj, Node)};
                {error, #beamtalk_error{}} = Err ->
                    Err
            catch
                error:{erpc, noconnection} ->
                    {error, beamtalk_actor:node_down_error_record(Node, Selector)};
                error:{erpc, timeout} ->
                    {error, remote_timeout_error_record(ReceiverClass, Selector, true)};
                error:{erpc, ErpcReason} ->
                    {error,
                        beamtalk_actor_ffi:generic_remote_error(
                            ReceiverClass, Selector, {erpc, ErpcReason}
                        )};
                error:{exception, Reason, _Stack} ->
                    {error,
                        beamtalk_actor_ffi:generic_remote_error(ReceiverClass, Selector, Reason)};
                exit:{exception, Reason} ->
                    {error,
                        beamtalk_actor_ffi:generic_remote_error(ReceiverClass, Selector, Reason)}
            end
    end.

-doc """
Runs ON the target node via `erpc` (`named:on:`, ADR 0126 §3) — thin wrapper
around `named_lookup/3` (shared with the local `named/2`) so the class check
(`'$beamtalk_actor'` marker via `process_info/2`) runs where the actor
actually lives.
""".
-spec remote_named_target(atom(), atom()) ->
    {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
remote_named_target(ReceiverClass, Name) ->
    beamtalk_actor_ffi:named_lookup(ReceiverClass, Name, 'named:on:').

-doc """
Origin-side entry point for `allRegisteredOn:` (ADR 0126 §3).

Reuses the public `allRegistered/1` FFI shim as the `erpc` target directly
(its `Self` argument is already ignored — `allRegistered(_Self) -> ...` —
so any placeholder value works), then node-qualifies every returned proxy
the same way `remote_named/3` does, for the same reason.
""".
-spec remote_all_registered(node()) -> {ok, [#beamtalk_object{}]} | {error, #beamtalk_error{}}.
remote_all_registered(Node) ->
    Selector = 'allRegisteredOn:',
    case beamtalk_node:connect_policy(Node, beamtalk_node:tls_distribution()) of
        {error, #beamtalk_error{} = Refused} ->
            {error, Refused#beamtalk_error{class = 'Actor', selector = Selector}};
        ok ->
            try
                erpc:call(
                    Node, ?BT_ACTOR_WIRE_MODULE, allRegistered, [undefined], ?BT_REMOTE_CALL_TIMEOUT
                )
            of
                List when is_list(List) ->
                    {ok, [qualify_registered_ref(Obj, Node) || Obj <- List]}
            catch
                error:{erpc, noconnection} ->
                    {error, beamtalk_actor:node_down_error_record(Node, Selector)};
                error:{erpc, timeout} ->
                    {error, remote_timeout_error_record('Actor', Selector, true)};
                error:{erpc, ErpcReason} ->
                    {error,
                        beamtalk_actor_ffi:generic_remote_error(
                            'Actor', Selector, {erpc, ErpcReason}
                        )};
                error:{exception, Reason, _Stack} ->
                    {error, beamtalk_actor_ffi:generic_remote_error('Actor', Selector, Reason)};
                exit:{exception, Reason} ->
                    {error, beamtalk_actor_ffi:generic_remote_error('Actor', Selector, Reason)}
            end
    end.

-doc """
Node-qualify a `#beamtalk_object{}` proxy returned by a remote lookup
(`remote_named/3`, `remote_all_registered/1`): rewrites a local
`{registered, Name}` identity slot to `{registered, Name, Node}` so it keeps
resolving against `Node` — its origin — wherever it is read next (ADR 0126
§3). A pid identity (never produced by `named/2`/`allRegistered/1` today,
but harmless if it ever were) passes through unchanged — a pid already
carries its node.
""".
-spec qualify_registered_ref(#beamtalk_object{}, node()) -> #beamtalk_object{}.
qualify_registered_ref(#beamtalk_object{pid = {registered, Name}} = Obj, Node) when is_atom(Name) ->
    Obj#beamtalk_object{pid = {registered, Name, Node}};
qualify_registered_ref(Obj, _Node) ->
    Obj.

-doc """
Construct a structured `timeout` error for a remote spawn/lookup/list/
unregister op (ADR 0126 §3). `Idempotent` picks the hint: `spawnOn:`/
`spawnAs:on:` are genuinely non-idempotent (a timeout may mean the far side
already spawned), while `named:on:`/`allRegisteredOn:` (read-only lookups)
and `unregister` (already idempotent locally) are always safe to retry —
the generic "not guaranteed to be safe" wording would be actively
misleading for those.
""".
-spec remote_timeout_error_record(atom(), atom(), boolean()) -> #beamtalk_error{}.
remote_timeout_error_record(ClassName, Selector, false) ->
    beamtalk_error:new(
        timeout,
        ClassName,
        Selector,
        <<
            "Remote operation did not complete within the timeout; the target node may or "
            "may not have applied it; a retry is not guaranteed to be safe (see the ADR "
            "0126 idempotency note)"
        >>
    );
remote_timeout_error_record(ClassName, Selector, true) ->
    beamtalk_error:new(
        timeout,
        ClassName,
        Selector,
        <<"Remote operation did not complete within the timeout; safe to retry">>
    ).

-doc "Resolve a `node :: Node` FFI argument to its underlying node atom.".
-spec node_arg_to_atom(term()) -> {ok, node()} | {error, #beamtalk_error{}}.
node_arg_to_atom(#{'$beamtalk_class' := 'Node', name := Name}) when is_atom(Name) ->
    {ok, Name};
node_arg_to_atom(Other) ->
    {error,
        beamtalk_error:with_hint(
            beamtalk_error:new(type_error, 'Actor'),
            beamtalk_error:chars_to_binary(io_lib:format("Expected a Node value, got ~tp", [Other]))
        )}.
