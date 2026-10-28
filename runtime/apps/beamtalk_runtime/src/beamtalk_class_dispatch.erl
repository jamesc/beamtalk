%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_dispatch).

%%% **DDD Context:** Object System Context

-moduledoc """
Class method dispatch for Beamtalk class objects.

Handles class-level message sending protocol, translating Beamtalk messages
to gen_server calls. Also provides helpers for class method execution
including test execution detection and result unwrapping.

ADR 0032 Phase 0: class chain fallthrough.
When a class-side message is not found in user-defined class methods,
dispatch falls through to 'Class' instance methods via beamtalk_dispatch:lookup/5.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    class_send/3,
    lookup_direct_call/3,
    lookup_direct_constructor/3,
    class_super_dispatch/4,
    class_self_send/3,
    class_self_direct_ok/4,
    class_self_dispatch_local/3,
    class_self_dispatch_local/4,
    metaclass_send/4,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6,
    handle_async_dispatch/5,
    class_understands_class_selector/2,
    find_class_method_from_class/2
]).

-type selector() :: atom().
-type class_name() :: atom().

-doc """
Send a message to a class object synchronously (ADR 0013 Phase 1).

Dispatches messages to the class gen_server, translating the Beamtalk
message protocol ({Selector, Args}) to the class process message format.
Unwraps {ok, Value} / {error, Error} results for seamless integration.
""".
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(undefined, Selector, _Args) ->
    Error = beamtalk_error:new(class_not_found, unknown, Selector),
    beamtalk_error:raise(Error);
%% ADR 0135 §1: `supervise` and the spawn family on a Supervisor or
%% DynamicSupervisor class start the tree in the caller, through
%% `beamtalk_supervisor:class_start/4`, never in the class gen_server (which
%% would queue every other send to the class behind the start and deadlock
%% any child that messages the class while it starts).
class_send(ClassPid, Selector, Args) when
    is_pid(ClassPid),
    (Selector =:= supervise orelse Selector =:= spawn orelse Selector =:= 'spawnWith:' orelse
        Selector =:= 'spawnAs:' orelse Selector =:= 'spawnWith:as:')
->
    case supervisor_start_target(ClassPid) of
        {ok, ClassName, Module} ->
            beamtalk_supervisor:class_start(ClassName, Module, Selector, Args);
        error ->
            class_send_plain(ClassPid, Selector, Args)
    end;
class_send(ClassPid, Selector, Args) ->
    class_send_plain(ClassPid, Selector, Args).

%% `{ok, ClassName, Module}` when `ClassPid` is a Supervisor or
%% DynamicSupervisor class (ETS reads only), else `error`.
-spec supervisor_start_target(pid()) -> {ok, class_name(), module()} | error.
supervisor_start_target(ClassPid) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case
                beamtalk_supervisor:is_supervisor(ClassName) andalso
                    beamtalk_class_metadata:lookup_module(ClassName)
            of
                {ok, Module} -> {ok, ClassName, Module};
                _ -> error
            end;
        not_found ->
            error
    end.

-spec class_send_plain(pid(), atom(), list()) -> term().
%% Self-call for instantiation selectors — bypass gen_server to avoid deadlock.
%% When a class method sends new/new:/spawn/spawnWith: to its own class (ClassPid == self()),
%% gen_server:call(self(), ...) would deadlock. Instead, perform instantiation directly
%% using class metadata from the process dictionary (set during init).
class_send_plain(ClassPid, 'new', Args) when ClassPid =:= self() ->
    handle_self_instantiation(new, 'new', Args);
class_send_plain(ClassPid, 'new:', Args) when ClassPid =:= self() ->
    handle_self_instantiation(new, 'new:', Args);
class_send_plain(ClassPid, spawn, Args) when ClassPid =:= self() ->
    handle_self_instantiation(spawn, spawn, Args);
class_send_plain(ClassPid, 'spawnWith:', Args) when ClassPid =:= self() ->
    handle_self_instantiation(spawn, 'spawnWith:', Args);
class_send_plain(ClassPid, 'new', []) ->
    %% an eligible Value builds in the caller via a direct
    %% `Module:new()` call — the same call `handle_new_compiled` would make,
    %% minus the gen_server hop (and its `IsConstructible` cache write, which
    %% a later slow-path call recomputes). Anything ineligible falls through
    %% to the gen_server path unchanged.
    case lookup_direct_constructor(ClassPid, 'new', []) of
        {ok, Module} ->
            erlang:apply(Module, new, []);
        error ->
            class_send_with_recovery(ClassPid, 'new', fun(P) ->
                unwrap_class_call(gen_server:call(P, {new, []}))
            end)
    end;
class_send_plain(ClassPid, 'new:', [Map]) when is_map(Map) ->
    %% same for `new:` with a map argument, via `Module:new/1`.
    %% A non-map argument keeps the unguarded clause below, whose gen_server
    %% path reproduces the legacy fallbacks (see `lookup_direct_constructor/3`).
    case lookup_direct_constructor(ClassPid, 'new:', [Map]) of
        {ok, Module} ->
            erlang:apply(Module, new, [Map]);
        error ->
            class_send_with_recovery(ClassPid, 'new:', fun(P) ->
                unwrap_class_call(gen_server:call(P, {new, [Map]}))
            end)
    end;
class_send_plain(ClassPid, 'new:', [Map]) ->
    class_send_with_recovery(ClassPid, 'new:', fun(P) ->
        unwrap_class_call(gen_server:call(P, {new, [Map]}))
    end);
class_send_plain(ClassPid, spawn, []) ->
    class_send_with_recovery(ClassPid, spawn, fun(P) ->
        unwrap_class_call(gen_server:call(P, {spawn, []}))
    end);
class_send_plain(ClassPid, 'spawnWith:', [Map]) ->
    class_send_with_recovery(ClassPid, 'spawnWith:', fun(P) ->
        unwrap_class_call(gen_server:call(P, {spawn, [Map]}))
    end);
%% ADR 0032 Phase 2: 8 of the originally planned 12 hardcoded selector
%% clauses were removed here (methods, superclass, class_name, module_name,
%% printString, class, subclasses, allSubclasses); they now dispatch through the
%% Class/Behaviour chain via the catch-all below.
%%
%% The remaining 4 instantiation selectors (new, new:, spawn, spawnWith:) retain
%% explicit clauses because they must route to the gen_server's {new, _} / {spawn, _}
%% handlers, not to class_method_call. Moving them to the Behaviour/Class chain
%% is future work (ADR 0032 Phase 4+). Real class-method bodies for all four
%% live on actor.bt (`class sealed new`/`new:`/`spawn`/`spawnWith:`), but this
%% routing deliberately does not use them — those bodies are the documented,
%% xref-visible definition of dynamic-dispatch behaviour, not the code path
%% an external `Counter spawn` actually runs today.
%% Every other selector aimed at the class's own gen_server from
%% inside that gen_server. `gen_server:call(self(), ...)` does not hang — it
%% exits with `{calling_self, {gen_server, call, [...]}}`, a raw tuple that
%% says nothing about what the caller did wrong. The commonest way to get here
%% is a block running inside a class method that messages its own class:
%%
%%     File open: p mode: #read do: [:h | File exists: q]
%%
%% Report it the way `handle_metaclass_self_call/2` already reports
%% the metaclass equivalent. No working path changes: the alternative was an
%% exit, not a successful call.
class_send_plain(ClassPid, Selector, Args) ->
    %% ADR 0129 Phase 0b: sealed, stateless classes' eligible class methods run
    %% in the caller, exactly like the static direct call. The eligibility set
    %% is generated metadata (Rust's `compute_direct_call_eligible`), never
    %% re-derived here. Checked before the self-call guard: a direct call has no
    %% gen_server to deadlock on.
    case lookup_direct_call(ClassPid, Selector, Args) of
        {ok, Module, SafeFn} ->
            erlang:apply(Module, SafeFn, [nil | Args]);
        error when ClassPid =:= self() ->
            handle_class_self_call(Selector);
        error ->
            class_send_with_recovery(ClassPid, Selector, fun(P) ->
                class_send_dispatch(P, Selector, Args)
            end)
    end.

-doc """
Resolve a dynamic class-side send to a direct module call, if eligible.

Eligible when the class's `direct_class_methods` lists the selector and the
compiled function exists at the right arity (`nil` plus the arguments);
an arity mismatch falls through to the gen_server path so the usual structured
error is raised.
""".
-spec lookup_direct_call(pid(), selector(), list()) -> {ok, module(), atom()} | error.
lookup_direct_call(ClassPid, Selector, Args) when is_pid(ClassPid), is_list(Args) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_metadata:lookup_direct_class_method(ClassName, Selector) of
                {ok, Module, SafeFn} = Found ->
                    case erlang:function_exported(Module, SafeFn, length(Args) + 1) of
                        true -> Found;
                        false -> error
                    end;
                error ->
                    error
            end;
        not_found ->
            error
    end;
lookup_direct_call(_ClassPid, _Selector, _Args) ->
    error.

-doc """
Resolve a dynamic `new`/`new:` send to a direct module call, if eligible.

Eligible when the class's baked `direct_constructors` lists the selector, the
class names a module and has no runtime-installed class-method funs (the
`lookup_direct_constructor/2` gates), and the module exports `new` at the
right arity (`new/0` for `new`, `new/1` for `new:` with a map argument). A
non-map `new:` argument is never eligible: the gen_server path reproduces
the legacy `IsConstructible = false` fallback and the `new: expects a
Dictionary argument` error there.

The verdict is generated metadata (Rust's `direct_constructors`), never
re-derived here. `new` needs no chain check because `new/0` is itself the
override slot (a custom `new` compiles to, or delegates from, it), so the
direct call and the gen_server path reach the same function either way.
`new:` shares the direct-call trust model: reloading a class re-syncs its own
verdicts, while an ancestor reload does not re-bake descendants. Like
`lookup_direct_call/3`, the export check only sees loaded code, so a class
whose module nobody has touched yet takes one gen_server fallback (which
loads it) before going direct.
""".
-spec lookup_direct_constructor(pid(), selector(), list()) -> {ok, module()} | error.
lookup_direct_constructor(ClassPid, new, []) when is_pid(ClassPid) ->
    lookup_direct_constructor_module(ClassPid, new, 0);
lookup_direct_constructor(ClassPid, 'new:', [Map]) when is_pid(ClassPid), is_map(Map) ->
    lookup_direct_constructor_module(ClassPid, 'new:', 1);
lookup_direct_constructor(_ClassPid, _Selector, _Args) ->
    error.

-spec lookup_direct_constructor_module(pid(), selector(), arity()) -> {ok, module()} | error.
lookup_direct_constructor_module(ClassPid, Selector, Arity) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_metadata:lookup_direct_constructor(ClassName, Selector) of
                {ok, Module} = Found ->
                    case erlang:function_exported(Module, new, Arity) of
                        true -> Found;
                        false -> error
                    end;
                error ->
                    error
            end;
        not_found ->
            error
    end.

-doc """
Report a class-method self-send that would deadlock.

The four instantiation selectors are short-circuited above; anything else
sent to a class from inside that class's own process has no safe route, so
raise a structured `dispatch_error` naming the deadlock instead of letting
`gen_server:call/2` exit with an opaque `calling_self` tuple.
""".
-spec handle_class_self_call(selector()) -> no_return().
handle_class_self_call(Selector) ->
    ClassName =
        case get(beamtalk_class_name) of
            undefined -> 'the class';
            CN -> CN
        end,
    Error0 = beamtalk_error:new(dispatch_error, ClassName, Selector),
    %% Built with atom_to_binary rather than io_lib:format on purpose. A badarg
    %% escaping from *inside* error construction is especially nasty here: the
    %% FFI boundary treats it as a genuine badarg, retries the whole call with
    %% charlist-coerced arguments, and the caller ends up seeing an unrelated
    %% type_error from whichever guard the coerced arguments happen to miss.
    Error1 = beamtalk_error:with_hint(
        Error0,
        iolist_to_binary([
            <<"Sending '">>,
            atom_to_binary(Selector, utf8),
            <<"' to '">>,
            atom_to_binary(ClassName, utf8),
            <<
                "' from inside one of its own class methods would deadlock "
                "(gen_server:call to self). This usually means a block passed to a "
                "class method is messaging that same class again; read what you "
                "need before the call, or hold the resource yourself instead of "
                "using the block form."
            >>
        ])
    ),
    beamtalk_error:raise(Error1).

-doc """
A class-side `super` send (BT-4001): `super Selector` in a class method (or
class-side extension, or ClassBuilder class-method fun) of `ClassName`.

The lookup starts at `ClassName`'s superclass and never consults
`ClassName` itself, so neither its compiled class methods nor its own
class-side extensions are reached: a `C class >> peek:` extension that sends
`super peek:` runs the superclass's method instead of recursing into itself.
At each ancestor, from the superclass up, a class-side extension registered
on that ancestor's metaclass tag is checked before the ancestor's own class
methods (the same extension-before-method order a send to that class uses),
so `super` finds a superclass method whether its body or an extension
defined it.

`ReceiverTag` is the live receiver's metaclass tag (`element(2, ClassSelf)`),
so the found method runs under the receiver's class-variable key. Returns the
plain result (ADR 0130 §3) and raises a structured `does_not_understand` when
no ancestor defines `Selector`.
""".
-spec class_super_dispatch(class_name(), selector(), list(), atom()) -> term() | no_return().
class_super_dispatch(ClassName, Selector, Args, ReceiverTag) when
    is_list(Args), is_atom(ReceiverTag)
->
    case find_class_super_method(Selector, ClassName) of
        {extension, AncestorName, AncestorTag} ->
            case
                check_class_self_extension(AncestorName, AncestorTag, Selector, Args, ReceiverTag)
            of
                {ok, Outcome} ->
                    Outcome;
                %% Unregistered between the probe and the call: the ancestor's
                %% own class methods (then its ancestors') answer instead.
                %% `find_class_method_from_class/2` walks class methods only,
                %% so a higher ancestor's class-side extension is not
                %% re-probed on this path. That is harmless: the answer can
                %% only differ when a higher ancestor also extends `Selector`
                %% and this send raced a concurrent unregister of the nearer
                %% one; the walk still starts at `AncestorName` (never
                %% `ClassName`, so no self-recursion), and the next send sees
                %% the settled registry through the full probe.
                not_found ->
                    class_self_dispatch_chain(
                        ClassName,
                        Selector,
                        Args,
                        find_class_method_from_class(Selector, AncestorName),
                        ReceiverTag
                    )
            end;
        {method, AncestorName, AncestorModule} ->
            class_self_dispatch_chain(
                ClassName, Selector, Args, {ok, AncestorName, AncestorModule}, ReceiverTag
            );
        not_found ->
            raise_class_self_dnu(ClassName, Selector)
    end.

-doc """
The ancestor a class-side `super Selector` from `ClassName` resolves to,
walked from `ClassName`'s superclass: `{extension, Ancestor, AncestorTag}`
when a class-side extension on that ancestor's metaclass tag matches first,
`{method, Ancestor, Module}` when its class-method table does, else
`not_found`.
""".
-spec find_class_super_method(selector(), class_name()) ->
    {extension, class_name(), atom()} | {method, class_name(), atom()} | not_found.
find_class_super_method(Selector, ClassName) ->
    StepFun = fun(AncestorName, _Depth) ->
        AncestorTag = receiver_tag(AncestorName),
        case beamtalk_dispatch:check_extension(AncestorTag, Selector) of
            {ok, _Fun} ->
                {found, {extension, AncestorName, AncestorTag}};
            not_found ->
                case beamtalk_class_metadata:lookup_methods(AncestorName) of
                    {ok, AncestorModule, Selectors} ->
                        case lists:member(Selector, Selectors) of
                            true -> {found, {method, AncestorName, AncestorModule}};
                            false -> {next, superclass_from_ets(AncestorName)}
                        end;
                    not_found ->
                        not_found
                end
        end
    end,
    case
        beamtalk_hierarchy:walk_ancestors(
            superclass_from_ets(ClassName), StepFun, ?MAX_HIERARCHY_DEPTH
        )
    of
        {found, Found} ->
            Found;
        not_found ->
            not_found;
        {max_depth_exceeded, CycleAncestor} ->
            ?LOG_WARNING(
                "find_class_super_method: max hierarchy depth ~p exceeded at ~p (starting from ~p) — possible cycle",
                [?MAX_HIERARCHY_DEPTH, CycleAncestor, ClassName],
                #{domain => [beamtalk, runtime]}
            ),
            not_found
    end.

-spec receiver_tag(class_name()) -> atom().
receiver_tag(ClassName) ->
    beamtalk_class_registry:class_object_tag(ClassName).

-doc """
Virtual class-side self-send (BT-3666): `self Selector` from inside a class
method, when the selector is not bound statically to a direct call.

Checks `ClassName`'s own class-side extensions first (keyed under its
metaclass tag; extensions are never inherited, so only that tag is checked),
mirroring `handle_class_method_call/6`'s extension-before-method order, then
walks the class-method hierarchy starting at `ClassName` itself and applies
the found `DefiningModule:class_<Selector>` (or runtime class-method fun) in
the caller's process via `apply_class_method_in_context/6`, with
`ClassSelf#beamtalk_object.class_mod` set to the defining module.

`ClassName` is the *receiving* class — the class named by `ClassSelf`, which
for an inherited class method is a subclass of the class whose code is
running. Starting at that class means an override of `Selector` (or a
subclass-only implementation of a template-method hook) is found before the
inherited definition, exactly as an instance-side self-send resolves.
A class-side `super`, builder or compiled, uses `class_super_dispatch/4`
instead, which never consults `ClassName` itself.

Returns the plain result (ADR 0130 §3) and raises a structured
`does_not_understand` when neither an extension nor any class in the chain
defines `Selector`.
""".
-spec class_self_send(class_name(), selector(), list()) -> term() | no_return().
class_self_send(ClassName, Selector, Args) ->
    %% The receiving class is `ClassName`: its tag is both the extension key and
    %% the receiver's tag, derived once per send (BT-3766).
    Tag = receiver_tag(ClassName),
    case check_class_self_extension(ClassName, Tag, Selector, Args, Tag) of
        {ok, Outcome} ->
            Outcome;
        not_found ->
            class_self_dispatch_chain(
                ClassName,
                Selector,
                Args,
                find_class_method_from_class(Selector, ClassName),
                Tag
            )
    end.

-doc """
Guard for the compiled class-side self-send fast path (BT-3666).

An open class's `self foo` (where the class itself defines `foo`) compiles to
`case class_self_direct_ok(element(2, ClassSelf), 'Cls class', 'Cls', foo) of
true -> class_foo(...) ; false -> class_self_send/3 walk end`. The direct call
is only equivalent to the walk when the receiving class is exactly the
compiling class (`ReceiverTag =:= ClassTag`) AND nothing would shadow the
compiled method that the walk honours: a class-side extension on the class
(`check_class_self_extension/5`) or a runtime-installed class-method fun
(ADR 0084, gated by the per-class `has_runtime_class_methods` flag). The
`TestCase` run-selector guard (`test_spawn`) is preserved by declining.

Costs one `persistent_term` read for the defining-class receiver (BT-3700: a
derived per-class flag in `beamtalk_class_shadow_flags` folds the extension flag,
the runtime-fun flag and readiness; BT-3669/BT-3676 had three reads); a
subclass receiver short-circuits on the first clause.
""".
-spec class_self_direct_ok(atom(), atom(), class_name(), selector()) -> boolean().
class_self_direct_ok(Tag, Tag, ClassName, Selector) ->
    case ClassName =:= 'TestCase' andalso is_test_execution_selector(Selector) of
        true ->
            false;
        false ->
            %% Extension registry not initialised yet (early bootstrap), or a
            %% shadow present, declines: the walk is always a correct answer.
            %% One persistent_term read of the derived flag (BT-3700).
            beamtalk_class_shadow_flags:direct_call_ok(Tag, ClassName)
    end;
class_self_direct_ok(_ReceiverTag, _ClassTag, _ClassName, _Selector) ->
    false.

-doc """
The superclass-chain walk shared by `class_self_dispatch_local/3`,
`class_self_send/3` and `class_super_dispatch/4`: each does its own single
extension check first, then falls through here. The `/4` form walks from
`ClassName`'s superclass; the `/5` form applies a lookup result the caller
already has.
""".
-spec class_self_dispatch_chain(class_name(), selector(), list(), atom()) ->
    term() | no_return().
class_self_dispatch_chain(ClassName, Selector, Args, ReceiverTag) ->
    class_self_dispatch_chain(
        ClassName,
        Selector,
        Args,
        find_class_method_in_chain(Selector, ClassName),
        ReceiverTag
    ).

-spec class_self_dispatch_chain(
    class_name(), selector(), list(), {ok, class_name(), atom()} | not_found, atom()
) -> term() | no_return().
class_self_dispatch_chain(ClassName, Selector, Args, Found, ReceiverTag) ->
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
                apply_class_method_in_context(
                    Selector, Args, ClassName, ReceiverTag, DefiningClass, DefiningModule
                )
            );
        not_found ->
            raise_class_self_dnu(ClassName, Selector)
    end.

-doc """
ADR 0084: Dispatch a class-method self-send that may resolve to the
class's OWN runtime-installed class method.

Class-method funs created by the programmatic `ClassBuilder` are
anonymous funs with no `class_<sel>` module export, so a self-send inside such a
fun cannot use the compiled direct-call path. It routes here instead.

Checks the extension registry first (same tag/priority rule as
`class_self_send/3` above — extension before local method), then the
class's own runtime class-method fun (the retrieval store), and only falls
back to `class_self_dispatch_chain/4` (super + inherited, walked from the
superclass) when the selector is neither.

Returns the raw result and raises a structured `does_not_understand`
when no definition is found. No `gen_server` hop: the local lookup is the same
ETS read the dispatch hot path already uses.
""".
-spec class_self_dispatch_local(class_name(), selector(), list()) -> term() | no_return().
class_self_dispatch_local(ClassName, Selector, Args) ->
    Tag = receiver_tag(ClassName),
    class_self_dispatch_local(ClassName, Tag, Selector, Args, Tag).

-doc """
`class_self_dispatch_local/3` with the live receiver's metaclass tag
(`element(2, ClassSelf)`), so a subclass receiver runs the found method under
its own class-variable key rather than under a `ClassSelf` rebuilt from
`ClassName`. Like `/3` it checks `ClassName`'s own class-side extensions
first, so it is not `super`: a class-side `super` uses
`class_super_dispatch/4`.
""".
-spec class_self_dispatch_local(class_name(), selector(), list(), atom()) ->
    term() | no_return().
class_self_dispatch_local(ClassName, Selector, Args, ReceiverTag) when
    is_list(Args), is_atom(ReceiverTag)
->
    %% `ReceiverTag` can differ from `ClassName`'s own tag (a subclass receiver).
    class_self_dispatch_local(ClassName, receiver_tag(ClassName), Selector, Args, ReceiverTag).

-spec class_self_dispatch_local(class_name(), atom(), selector(), list(), atom()) ->
    term() | no_return().
class_self_dispatch_local(ClassName, ClassTag, Selector, Args, ReceiverTag) ->
    case check_class_self_extension(ClassName, ClassTag, Selector, Args, ReceiverTag) of
        {ok, Outcome} ->
            Outcome;
        not_found ->
            case beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) of
                {ok, _Info} ->
                    %% Own runtime class method: dispatch with DefiningClass = ClassName
                    %% so apply_class_method_in_context resolves the fun from the store.
                    DefiningModule = self_dispatch_module(ClassName),
                    unwrap_self_dispatch_outcome(
                        ClassName,
                        Selector,
                        apply_class_method_in_context(
                            Selector, Args, ClassName, ReceiverTag, ClassName, DefiningModule
                        )
                    );
                error ->
                    %% Not a local runtime method — walk the chain (super + inherited),
                    %% which also resolves inherited runtime funs and compiled methods.
                    class_self_dispatch_chain(ClassName, Selector, Args, ReceiverTag)
            end
    end.

-doc """
Shared extension-registry probe for `class_self_send/3`,
`class_self_dispatch_local/3` and `class_super_dispatch/4` — checking whether
`ClassName`'s metaclass tag has a matching class-side extension
(`beamtalk_extensions`, ADR 0066) before the caller falls through to its
local/inherited-method lookups. Mirrors
`handle_class_method_call/6`'s priority order (extension before
local method table).

Reuses `apply_class_extension_fun/5` for the same calling convention and
crash-safety as the external-dispatch path (a bad extension body must not
take down the class's own long-lived gen_server, which this self-send also
runs inside), then adapts the outcome to the self-dispatch caller shape via
`unwrap_self_dispatch_outcome/3` — raw value on success, structured raises on
failure.

Returns `{ok, Outcome}` when an extension matched (`Outcome` already
unwrapped and ready to return to the codegen call site), or `not_found` so
the caller proceeds to its own next lookup.
""".
-spec check_class_self_extension(class_name(), atom(), selector(), list(), atom()) ->
    {ok, term()} | not_found | no_return().
check_class_self_extension(ClassName, ClassTag, Selector, Args, ReceiverTag) ->
    %% `ClassTag` (`ClassName`'s own metaclass tag, derived once by the caller)
    %% picks the extension; `ReceiverTag` is the live receiver's metaclass tag,
    %% so the extension body reads the receiver's class-variable key. The two
    %% are the same atom except for a subclass receiver
    %% (`class_self_dispatch_local/4`) or a `super` send that found the
    %% extension on an ancestor (`class_super_dispatch/4`), where the caller
    %% passes them separately.
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, Fun} ->
            Module = self_dispatch_module(ClassName),
            ClassSelf = #beamtalk_object{class = ReceiverTag, class_mod = Module, pid = self()},
            {ok,
                unwrap_self_dispatch_outcome(
                    ClassName,
                    Selector,
                    apply_class_extension_fun(Fun, ClassSelf, Args, ClassName, Selector)
                )};
        not_found ->
            not_found
    end.

-doc "The compiled module backing `ClassName`, for a `ClassSelf` built outside the gen_server state.".
-spec self_dispatch_module(class_name()) -> atom().
self_dispatch_module(ClassName) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        {ok, M} -> M;
        not_found -> ClassName
    end.

-doc """
Adapt a `class_method_outcome()` from `apply_class_method_in_context/6` to the
self-dispatch caller shape: raw value on success, structured raises on failure.
Shared by `class_self_send/3`, `class_self_dispatch_local/3` and
`class_super_dispatch/4`.
""".
-spec unwrap_self_dispatch_outcome(class_name(), selector(), class_method_outcome()) ->
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

-spec raise_test_spawn_self_dispatch_unsupported(class_name(), selector()) -> no_return().
raise_test_spawn_self_dispatch_unsupported(ClassName, Selector) ->
    %% Atom-safe formatting — see note on raise_class_self_dnu/2.
    Hint = iolist_to_binary(
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

-spec raise_class_self_dnu(class_name(), selector()) -> no_return().
raise_class_self_dnu(ClassName, Selector) ->
    %% ClassName and Selector are atoms; use ~ts (unicode string) with
    %% atom_to_list/1 rather than ~s, which expects an iolist and raises
    %% badarg on atoms — that would mask the intended does_not_understand.
    Hint = iolist_to_binary(
        io_lib:format(
            "Class '~ts' has no class method '~ts' and no ancestor defines it",
            [atom_to_list(ClassName), atom_to_list(Selector)]
        )
    ),
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector, Hint),
    beamtalk_error:raise(Error).

-doc "Dispatch a class method call (the main logic for the catch-all clause).".
-spec class_send_dispatch(pid(), selector(), list()) -> term().
class_send_dispatch(ClassPid, Selector, Args) ->
    %% Try user-defined class methods before raising does_not_understand.
    %% Test execution may take a long time; use longer timeout.
    %% Class methods can do arbitrary I/O (HTTP, file, database), so the default
    %% must accommodate network latency.  Test selectors get 5 minutes because a
    %% full suite can run hundreds of tests sequentially.
    Timeout =
        case is_test_execution_selector(Selector) of
            % 5 minutes for test suites
            true -> 300000;
            % 60 seconds — class methods may do network I/O
            false -> 60000
        end,
    %% ADR 0081: pass the caller's session context explicitly in the
    %% message instead of having the class gen_server copy our whole process
    %% dictionary via `process_info/2`. We read our own two keys cheaply with
    %% `get/1` here and the receiving clause seeds from the tuple.
    case
        gen_server:call(
            ClassPid, {class_method_call, Selector, Args, local_session_context()}, Timeout
        )
    of
        {ok, Result} ->
            %% ADR 0135 §1: no supervisor post-dispatch hook here any more.
            %% `supervise` and the spawn family start in the caller
            %% (`class_send/3` -> `beamtalk_supervisor:class_start/4`), which
            %% runs the instance-side `initialize` itself.
            Result;
        {error, not_found} ->
            %% BT-3820: resolve name/module from the metadata ETS rows, not by
            %% calling back into ClassPid. The `class_method_call` above already
            %% waited for the class process; a second and third 5s-timeout
            %% `gen_server:call` here re-queue behind whatever else the class
            %% process is serving and time out when it is busy.
            ClassName = beamtalk_object_class:class_name_safe(ClassPid),
            %% ADR 0032 Phase 0: Try Class chain before raising does_not_understand.
            %% Class objects are instances of 'Class'; fall through to Class instance methods.
            ModuleName = beamtalk_object_class:module_name_safe(ClassPid),
            ClassSelf = #beamtalk_object{
                class = beamtalk_class_registry:class_object_tag(ClassName),
                class_mod = ModuleName,
                pid = ClassPid
            },
            case try_class_chain_fallthrough(ClassSelf, Selector, Args) of
                {ok, Result} ->
                    Result;
                not_found ->
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Class does not understand this message">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        {error, {beamtalk_script_exit, _} = ScriptExit} ->
            %% ADR 0099 §3: a connected `Program exit: N` raised inside
            %% the dispatched class method surfaces here as the class gen_server's
            %% error reply. Re-raise it as the `{beamtalk_script_exit, N}` *throw*
            %% in the caller's (eval/dispatch worker's) process, so that worker
            %% reports the status and ends the session — instead of
            %% unwrap_class_call/1 wrapping it as an ordinary method failure.
            throw(ScriptExit);
        {error, Nlr} when ?IS_NLR(Nlr) ->
            %% A `^` inside a block that this class method ran on our
            %% behalf. The matching catch frame is in *our* process — the class
            %% gen_server had no frame holding that token, so its reply is the only
            %% way the signal can get back here. Re-throw so the enclosing method
            %% unwinds as it would have without the process hop; without this the
            %% raw `{'$bt_nlr', ...}` tuple surfaces to user code as an error value.
            throw(Nlr);
        Other ->
            unwrap_class_call(Other)
    end.

-doc """
Send a message to a metaclass object (ADR 0036).

Routes messages on metaclass objects (tagged `class='Metaclass'`) through:
  1. `{metaclass_method_call, Selector, Args, SessionCtx}` to the class
     gen_server via `gen_server:call/2` — resolves user-defined class methods
     (e.g. `withAll:`) via the superclass chain (ADR-0036 Phase 2). `SessionCtx`
     carries the caller's session context explicitly.
  2. Fallthrough to `beamtalk_dispatch:lookup/5` starting at 'Metaclass', which
     walks the Metaclass → Class → Behaviour → Object → ProtoObject chain for
     built-in messages (`new`, `class`, etc.).

The class pid is the same as the described class (virtual tag approach, ADR 0013).
Self carries `class='Metaclass'` so method dispatch receives the correct receiver.
""".
-spec metaclass_send(pid(), atom(), list(), #beamtalk_object{}) -> term().
%% `self class <msg>` inside a class method routes here with
%% Pid =:= self() (the class gen_server calling itself). A plain
%% gen_server:call on the instantiation selectors would deadlock; short-circuit
%% the spawn/new family via the process-dictionary-backed helpers used by
%% `self new` / `self spawnAs:`, and raise a clear error for everything else.
metaclass_send(Pid, Selector, Args, _Self) when Pid =:= self() ->
    handle_metaclass_self_call(Selector, Args);
metaclass_send(Pid, Selector, Args, Self) ->
    %% Wrap in crash recovery, same as class_send.
    %% Update Self's pid on recovery so downstream dispatch uses the new process.
    class_send_with_recovery(Pid, Selector, fun(P) ->
        metaclass_send_dispatch(P, Selector, Args, Self#beamtalk_object{pid = P})
    end).

-doc "Dispatch a metaclass method call (the main logic for metaclass_send).".
-spec metaclass_send_dispatch(pid(), atom(), list(), #beamtalk_object{}) -> term().
metaclass_send_dispatch(Pid, Selector, Args, Self) ->
    %% ADR-0036 Phase 2: Use metaclass_method_call so that
    %% `self species withAll: x` and similar dynamic class-side sends find
    %% user-defined class methods (e.g. `withAll:`) via the proper handler.
    %% Falls through to the Metaclass chain for built-in messages (`new`, `class`, etc.).
    %% Carry the caller's session context explicitly (see
    %% class_send_dispatch/3) so the class gen_server need not copy our dictionary.
    case
        gen_server:call(
            Pid, {metaclass_method_call, Selector, Args, local_session_context()}, 60000
        )
    of
        {ok, Result} ->
            Result;
        {error, not_found} ->
            case beamtalk_dispatch:lookup(Selector, Args, Self, #{}, 'Metaclass') of
                {reply, Result, _NewState} ->
                    Result;
                {error, #beamtalk_error{kind = does_not_understand}} ->
                    ClassName = beamtalk_object_class:class_name_safe(Pid),
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Metaclass does not understand this message">>
                    ),
                    beamtalk_error:raise(Error);
                {error, #beamtalk_error{kind = class_not_found}} ->
                    %% 'Metaclass' process not registered — graceful fallback.
                    Error = beamtalk_error:new(does_not_understand, 'Metaclass', Selector),
                    beamtalk_error:raise(Error);
                {error, Error} ->
                    %% A real error from a method in the chain. Wrap idempotently.
                    beamtalk_exception_handler:reraise(Error)
            end;
        {error, {beamtalk_script_exit, _} = ScriptExit} ->
            %% ADR 0099 §3: re-raise a connected `Program exit:` from a
            %% metaclass-dispatched class method as the script-exit throw (parity
            %% with class_send_dispatch/3), so the worker adopts the status.
            throw(ScriptExit);
        {error, Nlr} when ?IS_NLR(Nlr) ->
            %% Same relay as class_send_dispatch/3, and for the same
            %% reason — a metaclass-dispatched class method also runs in the class
            %% gen_server, so a `^` out of a caller-supplied block unwinds to here
            %% with no frame holding its token. Without this clause
            %% `unwrap_class_call/1` reraises it as an ordinary error and the raw
            %% `{'$bt_nlr', ...}` tuple reaches user code.
            %%
            %% Reached via a metaclass receiver — `SomeClass class class`, which
            %% `beamtalk_behaviour_intrinsics:classClass/1` tags `'Metaclass'` so
            %% `beamtalk_primitive:send/3` routes it here. A plain class literal or
            %% a single `class` send goes through `class_send/3` instead, which is
            %% why this needs its own clause rather than sharing that one.
            throw(Nlr);
        Other ->
            unwrap_class_call(Other)
    end.

-doc """
Read this process's session context (`beamtalk_session_pid` /
`beamtalk_session_id` / `beamtalk_session_meta` / `beamtalk_entry_group_leader`)
for explicit propagation into a class-method call.

ADR 0081: the eval worker seeds these keys
(`beamtalk_repl_shell:seed_session_context/3`). Class-method dispatch hops to
the class gen_server, so factory methods like `Session current` need the
caller's context there — including the origin metadata so `Session current kind`
reports the real client surface rather than `unknown`. We read our own keys
cheaply with `get/1` and pass them in the message tuple, avoiding a
`process_info(CallerPid, dictionary)` full-dictionary copy on the receiving
side.

The fourth element is the *entry group leader* — the IO sink a
connected `beamtalk run … --connect` dispatch wants its `Console` output to
reach. It is seeded only by `beamtalk_repl_eval:do_dispatch/5` (the `run-entry`
op) and is `undefined` everywhere else, including ordinary REPL `eval`, so no
other dispatch path changes where its output goes. Carrying it alongside the
session context makes it propagate transitively: a class method that calls
*another* class method re-emits the key it was seeded with, so nested output
keeps streaming to the same client.

Returns `{undefined, undefined, undefined, undefined}` when this process has no
seeded context (the common non-REPL case).
""".
-spec local_session_context() ->
    {pid() | undefined, binary() | undefined, map() | undefined, pid() | undefined}.
local_session_context() ->
    {
        get(beamtalk_session_pid),
        get(beamtalk_session_id),
        get(beamtalk_session_meta),
        get(beamtalk_entry_group_leader)
    }.

-doc """
Unwrap a class gen_server call result for use in class_send.

Translates {ok, Value} → Value, {error, Error} → re-raise as exception.
Handles both raw #beamtalk_error{} records and already-wrapped Exception
maps (from raise/1 inside handle_call). Uses reraise/1 (which wraps
idempotently).
""".
-spec unwrap_class_call(term()) -> term().
unwrap_class_call({ok, Value}) ->
    Value;
unwrap_class_call({error, Error}) ->
    beamtalk_exception_handler:reraise(Error).

-doc """
Handle a class method call from the gen_server.

User-defined class method dispatch.
Passes class variables to method and handles updates.
For test execution (runAll, run:), spawns in a separate process
to avoid gen_server deadlock.

ADR 0032 Phase 1: Receives local class_methods (not flattened table).
Walks the superclass chain if the method is not found locally.

Checks the extension registry (keyed under the metaclass tag,
`class_object_tag(ClassName)`) BEFORE the local method table / superclass
chain — mirroring `beamtalk_dispatch:lookup/5`'s own "extension checked
before local method table" order. A class-side extension
(`Target class >> sel => body`, ADR 0066) is read back by both
`class_method_call` (an ordinary `Target sel` send) and
`metaclass_method_call` (`Target class sel`), which both funnel through this
same function (`beamtalk_object_class:dispatch_class_method/5`).

Scope: this covers *external* sends (`Target sel` / `Target class sel`). A
`self someSelector` send from inside another class method of the same
class — `class_self_send/3` / `class_self_dispatch_local/3`, above —
goes through `check_class_self_extension/5` instead, which checks the same
registry under the same priority rule.

Returns {reply, Result, NewState} or test_spawn or {error, not_found}.
""".
-spec handle_class_method_call(
    selector(),
    list(),
    class_name(),
    atom(),
    #{selector() => term()},
    map()
) -> {reply, term(), map()} | test_spawn | {error, not_found}.
handle_class_method_call(Selector, Args, ClassName, Module, LocalClassMethods, ClassVars) ->
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, Fun} ->
            invoke_class_extension(Fun, Args, ClassName, ClassTag, Module, ClassVars, Selector);
        not_found ->
            case maps:is_key(Selector, LocalClassMethods) of
                true ->
                    %% Local class method found — invoke directly.
                    invoke_class_method(
                        Selector, Args, ClassName, Module, ClassName, Module, ClassVars
                    );
                false ->
                    %% Not found locally — walk the superclass chain for inherited class methods.
                    case find_class_method_in_chain(Selector, ClassName) of
                        {ok, DefiningClass, DefiningModule} ->
                            invoke_class_method(
                                Selector,
                                Args,
                                ClassName,
                                Module,
                                DefiningClass,
                                DefiningModule,
                                ClassVars
                            );
                        not_found ->
                            {error, not_found}
                    end
            end
    end.

-doc """
Invoke a class-side extension method found in the `beamtalk_extensions`
registry.

ADR 0130 §3: every class-side extension fun, whatever the target class's kind,
is `fun(Args, ClassSelf) -> Result`; class variables are read and written
through `beamtalk_class_vars`, not threaded. The entry follows the same
install / read-back / uninstall protocol as `invoke_class_method/7`
(`run_with_class_vars/3`). `ClassSelf` mirrors the receiver
`apply_class_method_in_context/6` builds for a local/inherited class method:
`class = ClassTag` (the metaclass tag), `class_mod = Module` (this call's own
compiled module; extensions are never inherited, so there is no separate
"defining class" indirection), `pid = self()` (the class gen_server this
handler is already running inside).

Like `beamtalk_dispatch:invoke_extension/6`, this catches and converts a
crashing extension body rather than letting it escape, via
`apply_class_extension_fun/5`'s own finer-grained classification
(`undef_in_body` vs. generic, plus NLR-relay / script-exit passthrough for
self-sends inside class methods), matching every other class-method dispatch
path: a bad extension body must not crash the class's own long-lived
gen_server.
""".
-spec invoke_class_extension(fun(), list(), class_name(), atom(), atom(), map(), selector()) ->
    {reply, term(), map()}.
invoke_class_extension(Fun, Args, ClassName, ClassTag, Module, ClassVars, Selector) ->
    ClassSelf = #beamtalk_object{class = ClassTag, class_mod = Module, pid = self()},
    run_with_class_vars(ClassTag, ClassVars, fun() ->
        apply_class_extension_fun(Fun, ClassSelf, Args, ClassName, Selector)
    end).

-doc """
Apply a class-side extension fun, classifying the outcome the same way
`apply_class_method_fun/5` does for a runtime-installed class method —
script-exit passthrough, NLR relay, `undef` classification, and
a generic catch-all — so a crashing extension body becomes a structured
`{error, ...}` reply instead of taking down the class gen_server.
""".
-spec apply_class_extension_fun(fun(), #beamtalk_object{}, list(), class_name(), selector()) ->
    {ok, term()}
    | {nlr_relay, term(), list()}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.
apply_class_extension_fun(Fun, ClassSelf, Args, ClassName, Selector) ->
    %% ADR 0130 §3: class-side extension funs are `fun(Args, ClassSelf)`.
    %% bt-catcher-audit: not-applicable-reraises - {error,_} is re-raised by the self-send path or
    %% reverted by run_with_class_vars
    try Fun(Args, ClassSelf) of
        Result ->
            {ok, Result}
    catch
        %% ADR 0099 §3: see apply_class_method_fun/5 — pass a
        %% connected `Program exit: N` through unlogged as a control-flow
        %% signal, not a method failure.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% See apply_class_method_fun/5 — a `^` unwinding through
        %% this extension belongs to a frame in the calling process.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            ?LOG_ERROR(
                "Class-side extension ~p:~p raised undef internally",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    stacktrace => ST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, undef_in_body};
        ErrClass:Error:ErrST ->
            %% Relayed to the caller, so debug-level: see
            %% apply_compiled_class_method/6 (BT-4115).
            ?LOG_DEBUG(
                "Class-side extension ~p:~p failed",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-doc """
Invoke a class method (local or inherited), handling test execution specially.

ADR 0130 §1: the class's variables live in this process's dictionary under
`beamtalk_class_vars:key(ClassName)` for the duration of the invocation. The
stored map is installed on entry and read back on exit; the compiled method
neither takes nor returns class variables. A normal return and a foreign `^`
(non-local return) keep the writes made before they left; an escaping error
replies with the pre-call map. `assert_absent/1` runs first, so a nested
invocation fails loudly with the outer map intact instead of overwriting it.
""".
-spec invoke_class_method(
    selector(),
    list(),
    class_name(),
    atom(),
    class_name(),
    atom(),
    map()
) ->
    {reply, term(), map()} | test_spawn.
invoke_class_method(Selector, Args, ClassName, _Module, DefiningClass, DefiningModule, ClassVars) ->
    %% The metaclass tag is derived once: it keys the class-variable home and
    %% is the `ClassSelf` tag the method receives.
    ClassTag = receiver_tag(ClassName),
    run_with_class_vars(ClassTag, ClassVars, fun() ->
        apply_class_method_in_context(
            Selector, Args, ClassName, ClassTag, DefiningClass, DefiningModule
        )
    end).

-doc """
Install `ClassVars` as `ClassName`'s home, run `Thunk` (which yields a
`class_method_outcome()`), and adapt the outcome to the gen_server reply shape
(ADR 0130 §1). Shared by `invoke_class_method/7` and `invoke_class_extension/7`.
""".
-spec run_with_class_vars(atom(), map(), fun(() -> class_method_outcome())) ->
    {reply, term(), map()} | test_spawn.
run_with_class_vars(ClassTag, ClassVars, Thunk) ->
    Key = beamtalk_class_vars:key_for_tag(ClassTag),
    beamtalk_class_vars:assert_absent(Key),
    beamtalk_class_vars:install(Key, ClassVars),
    try Thunk() of
        test_spawn ->
            test_spawn;
        {ok, Result} ->
            {reply, {ok, Result}, erlang:get(Key)};
        {nlr_relay, Nlr, _ST} ->
            %% ADR 0110/0130: a foreign `^` is control flow, not a failure;
            %% writes made before the unwind are kept. The reply shape is
            %% unchanged so the ?IS_NLR re-throw clauses in
            %% class_send_dispatch/3 and metaclass_send_dispatch/4 need no change.
            {reply, {error, Nlr}, erlang:get(Key)};
        {error, #beamtalk_error{} = Error} ->
            {reply, {error, Error}, ClassVars};
        {error, undef_in_body} ->
            {reply, {error, undef}, ClassVars};
        {error, {raised, _ErrClass, Error, _ST}} ->
            {reply, {error, Error}, ClassVars}
    after
        beamtalk_class_vars:uninstall(Key)
    end.

%% ADR 0110: `{nlr_relay, Nlr, ST}` is a foreign `^` (non-local
%% return) relaying out of the class method — the relayed NLR tuple plus its
%% stacktrace. It is kept distinct from `{error, {raised, ...}}` so
%% `run_with_class_vars/3` can keep the class-var writes made before the unwind
%% instead of reverting them the way it does for genuine errors.
-type class_method_outcome() ::
    test_spawn
    | {ok, term()}
    | {nlr_relay, term(), list()}
    | {error, #beamtalk_error{}}
    | {error, undef_in_body}
    | {error, {raised, atom(), term(), list()}}.

-doc """
Shared core of class-method dispatch.

Handles the TestCase `test_spawn` guard, constructs `ClassSelf` with
`class_mod = DefiningModule`, applies the method, and classifies any
errors into the structured variants in `class_method_outcome()`. Both
`invoke_class_method/7` (gen_server path) and `class_self_dispatch_chain/5`
(self-send path) adapt the outcome to their own caller shapes.

`{ok, Raw}` carries the method's bare result (ADR 0130 §3).
Stacktraces are preserved in the `{raised, ...}` variant so callers
that need to re-raise (e.g. self-dispatch) get the original trace.
""".
-spec apply_class_method_in_context(
    selector(), list(), class_name(), atom(), class_name(), atom()
) -> class_method_outcome().
apply_class_method_in_context(
    Selector, Args, ClassName, ReceiverTag, DefiningClass, DefiningModule
) ->
    %% For test execution (runAll, run:) inherited from TestCase,
    %% return a spawn request so the caller (gen_server) can handle noreply.
    %% The self-dispatch caller translates this to a structured error.
    case is_test_execution_selector(Selector) andalso DefiningClass =:= 'TestCase' of
        true ->
            test_spawn;
        false ->
            ClassSelf = #beamtalk_object{
                class = ReceiverTag,
                class_mod = DefiningModule,
                pid = self()
            },
            %% ADR 0084: a runtime-installed class-method fun shadows
            %% the compiled export. The retrieval store is keyed by the resolved
            %% DefiningClass, so this works identically for own and inherited
            %% methods, and is gated so compile-time-only classes pay no extra
            %% read on the dispatch hot path.
            case beamtalk_class_metadata:lookup_class_method_fun(DefiningClass, Selector) of
                {ok, #{block := Fun}} ->
                    apply_class_method_fun(Fun, ClassSelf, Args, ClassName, Selector);
                error ->
                    apply_compiled_class_method(
                        ClassSelf,
                        Args,
                        ClassName,
                        DefiningClass,
                        DefiningModule,
                        Selector
                    )
            end
    end.

-doc """
Apply a runtime-installed class-method fun (ADR 0084).

Same calling convention as the compiled path (`fun(ClassSelf, Args...)`,
ADR 0130 §3). A fun has no module export to mismatch, so an `undef` here is always raised
from inside the fun body (`undef_in_body`); any other error becomes `{raised,…}`.
""".
-spec apply_class_method_fun(fun(), #beamtalk_object{}, list(), class_name(), selector()) ->
    class_method_outcome().
apply_class_method_fun(Fun, ClassSelf, Args, ClassName, Selector) ->
    %% bt-catcher-audit: not-applicable-reraises - {error,_} is re-raised by the self-send path or
    %% reverted by run_with_class_vars
    try apply(Fun, [ClassSelf | Args]) of
        Raw ->
            {ok, Raw}
    catch
        %% ADR 0099 §3: a connected `Program exit: N` raised inside a
        %% class method is a control-flow signal, not a method failure. Pass it
        %% through unchanged (no error log) so class_send_dispatch/3 can re-raise
        %% it to the eval/dispatch worker that reports the status.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% A `^` non-local return that unwound through this class method
        %% belongs to a method frame in the *calling* process. Pass it through
        %% unlogged so class_send_dispatch/3 can re-throw it there.
        %% ADR 0110: tagged as its own outcome variant (not
        %% {error, {raised, ...}}) so the entry point can keep class-var
        %% writes made before the unwind instead of reverting them.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            ?LOG_ERROR(
                "Runtime class method ~p:~p raised undef internally",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    stacktrace => ST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, undef_in_body};
        ErrClass:Error:ErrST ->
            %% Relayed to the caller, so debug-level: see
            %% apply_compiled_class_method/6 (BT-4115).
            ?LOG_DEBUG(
                "Runtime class method ~p:~p failed",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-doc "Apply a compiled `class_<Selector>` export (the historical dispatch path).".
-spec apply_compiled_class_method(
    #beamtalk_object{}, list(), class_name(), class_name(), atom(), selector()
) -> class_method_outcome().
apply_compiled_class_method(
    ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector
) ->
    FunName = class_method_fun_name(Selector),
    %% ADR 0130 §3: `class_<sel>(ClassSelf, Args...)`; class variables live in
    %% the process dictionary home installed by the invocation entry point.
    %% bt-catcher-audit: not-applicable-reraises - {error,_} is re-raised by the self-send path or
    %% reverted by run_with_class_vars
    try erlang:apply(DefiningModule, FunName, [ClassSelf | Args]) of
        Raw ->
            {ok, Raw}
    catch
        %% ADR 0099 §3: pass a connected `Program exit: N` through as a
        %% control-flow signal (no "method failed" log); class_send_dispatch/3
        %% re-raises it to the eval/dispatch worker.
        throw:({beamtalk_script_exit, _} = ScriptExit):ScriptST ->
            {error, {raised, throw, ScriptExit, ScriptST}};
        %% See apply_class_method_fun/5 — a non-local return unwinding out
        %% of a class method is control flow for a frame in the calling process.
        %% ADR 0110: tagged {nlr_relay, ...} so class-var writes made
        %% before the unwind are kept rather than reverted.
        throw:Nlr:NlrST when ?IS_NLR(Nlr) ->
            {nlr_relay, Nlr, NlrST};
        error:undef:ST ->
            classify_undef(ClassName, DefiningClass, DefiningModule, Selector, FunName, ST);
        ErrClass:Error:ErrST ->
            %% BT-4115: debug, not error. The error is relayed as {raised, ...}
            %% and re-raised in the calling process, which owns reporting it
            %% (an uncaught one still reaches the caller's crash/REPL report).
            %% At error level every caught `Error signal:` built a formatted
            %% event here only for the domain filter to drop it; ?LOG_DEBUG's
            %% level check skips building it.
            ?LOG_DEBUG(
                "Class method ~p:~p failed",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-spec classify_undef(class_name(), class_name(), atom(), selector(), atom(), list()) ->
    {error, #beamtalk_error{}} | {error, undef_in_body}.
classify_undef(ClassName, DefiningClass, DefiningModule, Selector, FunName, ST) ->
    case is_dispatch_undef(DefiningModule, FunName, ST) of
        {true, module_not_loaded} ->
            Hint = module_not_loaded_hint(ClassName, DefiningClass, DefiningModule),
            {error, beamtalk_error:new(class_not_found, ClassName, Selector, Hint)};
        {true, method_not_found} ->
            Hint = method_not_found_hint(ClassName, DefiningClass, Selector),
            {error, beamtalk_error:new(does_not_understand, ClassName, Selector, Hint)};
        false ->
            %% undef was raised inside the method body, not at dispatch.
            ?LOG_ERROR(
                "Class method ~p:~p raised undef internally",
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    stacktrace => ST,
                    domain => [beamtalk, runtime]
                }
            ),
            {error, undef_in_body}
    end.

-spec module_not_loaded_hint(class_name(), class_name(), atom()) -> binary().
module_not_loaded_hint(ClassName, DefiningClass, DefiningModule) ->
    case ClassName =:= DefiningClass of
        true ->
            iolist_to_binary(
                io_lib:format(
                    "Class '~s' is not registered - the module '~s' may have failed to load",
                    [ClassName, DefiningModule]
                )
            );
        false ->
            iolist_to_binary(
                io_lib:format(
                    "Class '~s' inherits this method from '~s' - that class's module '~s' may have failed to load",
                    [ClassName, DefiningClass, DefiningModule]
                )
            )
    end.

-spec method_not_found_hint(class_name(), class_name(), selector()) -> binary().
method_not_found_hint(ClassName, DefiningClass, Selector) ->
    case ClassName =:= DefiningClass of
        true ->
            iolist_to_binary(
                io_lib:format(
                    "Class method '~s' not found on '~s' (arity mismatch or undefined selector)",
                    [Selector, ClassName]
                )
            );
        false ->
            iolist_to_binary(
                io_lib:format(
                    "Class method '~s' inherited from '~s' not found on '~s' (arity mismatch or undefined selector)",
                    [Selector, DefiningClass, ClassName]
                )
            )
    end.

-doc """
Walk the superclass chain to find an inherited class method.

Uses the ETS hierarchy table for O(1) superclass lookup per level and
the ETS class-methods table for the per-ancestor selector +
module lookup. The hot path issues no `gen_server:call`s — both tables
are populated atomically inside `beamtalk_object_class:init/1` before
the class becomes externally dispatch-visible, so a hierarchy entry
without a matching methods entry is not reachable from normal dispatch.

Safe to call from within a gen_server `handle_call` because we only walk
UP the hierarchy (no circular dependency possible).

The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; this function supplies only the
per-ancestor ETS probe.
""".
-spec find_class_method_in_chain(selector(), class_name()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_in_chain(Selector, ClassName) ->
    walk_class_method_chain(Selector, superclass_from_ets(ClassName), ClassName).

-doc """
Like `find_class_method_in_chain/2`, but the walk starts at `ClassName`
itself rather than its superclass (BT-3666): used by a class-side
self-send, whose receiving class may define the selector directly.
""".
-spec find_class_method_from_class(selector(), class_name()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_from_class(Selector, ClassName) ->
    walk_class_method_chain(Selector, ClassName, ClassName).

-spec walk_class_method_chain(selector(), class_name() | none, class_name()) ->
    {ok, class_name(), atom()} | not_found.
walk_class_method_chain(Selector, StartName, ClassName) ->
    StepFun = fun(AncestorName, _Depth) ->
        case beamtalk_class_metadata:lookup_methods(AncestorName) of
            {ok, AncestorModule, Selectors} ->
                case lists:member(Selector, Selectors) of
                    true -> {found, {AncestorName, AncestorModule}};
                    false -> {next, superclass_from_ets(AncestorName)}
                end;
            not_found ->
                not_found
        end
    end,
    case beamtalk_hierarchy:walk_ancestors(StartName, StepFun, ?MAX_HIERARCHY_DEPTH) of
        {found, {AncestorName, AncestorModule}} ->
            {ok, AncestorName, AncestorModule};
        not_found ->
            not_found;
        {max_depth_exceeded, CycleAncestor} ->
            ?LOG_WARNING(
                "find_class_method_in_ancestors: max hierarchy depth ~p exceeded at ~p (starting from ~p) — possible cycle",
                [?MAX_HIERARCHY_DEPTH, CycleAncestor, ClassName],
                #{domain => [beamtalk, runtime]}
            ),
            not_found
    end.

-doc "Read the superclass name from the hierarchy table (no gen_server call).".
-spec superclass_from_ets(class_name()) -> class_name() | none.
superclass_from_ets(ClassName) ->
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        {ok, Super} -> Super;
        not_found -> none
    end.

-doc """
Test whether `ClassName`'s own metaclass tag understands `Selector`
— a class-side extension (ADR 0066), a local or inherited class method, or a
local runtime-installed (ADR 0084) class method.

Backs `beamtalk_object_ops:responds_to_result/3`'s `respondsTo:` on a
class-object receiver. `SomeClass respondsTo: #sel` previously always
answered against the generic `'Class'` protocol only, because a
class's metaclass tag (`'SomeClass class'`) is virtual — never registered
as its own class in `beamtalk_class_registry`/`beamtalk_class_metadata` —
so the existing hierarchy-walk reflection helper
(`beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`) cannot be
pointed at it directly: there is no such class row to walk from. This
function instead re-implements "does dispatch find this selector" directly
against `ClassName`'s own tables, mirroring `handle_class_method_call/6`'s
priority order (extension, then local, then inherited chain) without
actually invoking anything.

Extensions are checked via the metaclass tag (matching how they are
registered, ADR 0066); local and inherited class methods are checked
against `ClassName` itself (the real registry entry) since class methods
belong to the class, not a separate metaclass registry row. Returns `false`
— not an error — for an unregistered `ClassName`, since every per-class
lookup this function makes already degrades to `not_found`/`error` for a
missing class; the generic-protocol fallback the caller applies next still
answers correctly (`Class`/`Behaviour`/`Object` messages are independent of
whether `ClassName` itself is currently registered).
""".
-spec class_understands_class_selector(class_name(), selector()) -> boolean().
class_understands_class_selector(ClassName, Selector) ->
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    case beamtalk_dispatch:check_extension(ClassTag, Selector) of
        {ok, _Fun} ->
            true;
        not_found ->
            has_local_class_method(ClassName, Selector) orelse
                (find_class_method_in_chain(Selector, ClassName) =/= not_found)
    end.

-doc """
Does `ClassName` itself (not an ancestor) define `Selector` as a
class method — either a compiled/static one (`lookup_methods/1`'s local
selector list) or a runtime-installed one (ADR 0084,
`lookup_class_method_fun/2`)? Mirrors the same two lookups
`class_self_dispatch_local/3` already checks for a self-send, so
`respondsTo:` agrees with what a `self sel` from inside another class
method would actually resolve.
""".
-spec has_local_class_method(class_name(), selector()) -> boolean().
has_local_class_method(ClassName, Selector) ->
    case beamtalk_class_metadata:lookup_class_method_fun(ClassName, Selector) of
        {ok, _Info} ->
            true;
        error ->
            case beamtalk_class_metadata:lookup_methods(ClassName) of
                {ok, _Module, Selectors} -> lists:member(Selector, Selectors);
                not_found -> false
            end
    end.

-doc """
Convert a class method selector to its module function name.
Class methods are generated with a 'class_' prefix, e.g.
`class defaultValue => 42` becomes `class_defaultValue/1`.
(Zero-arg selector: arity 0 + ClassSelf => arity 1.)

Uses list_to_atom rather than list_to_existing_atom because the class_
prefixed atom may not yet be in the atom table (e.g. when the module is
not loaded or the method is absent).  Atom exhaustion is not a concern:
selectors are bound atoms from user programs, so the class_ versions are
equally bounded.  If the resulting function does not exist in the module,
erlang:apply will raise undef, which invoke_class_method already handles.

A selector built at runtime (e.g. via `perform:` with a
dynamically-assembled selector) can exceed Erlang's 255-byte atom limit even
though no *compile-time* selector ever does — `list_to_atom` on the raw
`"class_" ++ Selector` string would then crash with `system_limit` instead of
producing a name. This mirrors the compile-time guard in the Rust codegen's
`safe_class_method_fn_name` (`crates/beamtalk-core/src/codegen/core_erlang/selector_mangler.rs`)
byte-for-byte — same 255-byte threshold on `"class_" ++ Selector`, same
FNV-1a 64-bit hash, same `"class_kw_<16-hex-digits>"` naming scheme — so a
selector that is long at compile time and the *same* selector text
constructed at runtime via `perform:` produce the identical atom.
""".
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    SelectorBin = atom_to_binary(Selector, utf8),
    Combined = <<"class_", SelectorBin/binary>>,
    case byte_size(Combined) =< 255 of
        true ->
            % elp:fixme W0023 intentional atom creation
            binary_to_atom(Combined, utf8);
        false ->
            Hash = fnv1a_64(SelectorBin),
            HashHex = io_lib:format("~16.16.0b", [Hash]),
            % elp:fixme W0023 intentional atom creation
            list_to_atom(lists:flatten(["class_kw_", HashHex]))
    end.

%% Deterministic FNV-1a 64-bit hash — bit-for-bit identical to the Rust
%% `fnv1a_64` in `selector_mangler.rs` (same offset basis, same prime,
%% same 64-bit wraparound). No external dependency; ~20 lines is not worth
%% pulling in a hashing library for.
-spec fnv1a_64(binary()) -> non_neg_integer().
fnv1a_64(Data) when is_binary(Data) ->
    fnv1a_64(Data, 16#cbf29ce484222325).

-spec fnv1a_64(binary(), non_neg_integer()) -> non_neg_integer().
fnv1a_64(<<>>, Hash) ->
    Hash;
fnv1a_64(<<Byte, Rest/binary>>, Hash) ->
    H1 = Hash bxor Byte,
    H2 = (H1 * 16#0100000001b3) band 16#ffffffffffffffff,
    fnv1a_64(Rest, H2).

-doc """
Check if a class method selector is a test execution command.
These selectors need special handling to avoid gen_server deadlock.
""".
-spec is_test_execution_selector(selector()) -> boolean().
is_test_execution_selector(runAll) -> true;
is_test_execution_selector('runAll:') -> true;
is_test_execution_selector('run:') -> true;
is_test_execution_selector('run:method:') -> true;
is_test_execution_selector(_) -> false.

-doc """
Handle async cast message dispatch for Future protocol.

Dispatches class query messages asynchronously, resolving or rejecting
the associated Future process.

ADR 0032 Phase 1: Receives local instance_methods (not flattened table).
The methods response returns local-only selectors; full hierarchy walk
will be done by Behaviour.methods in Phase 2.
""".
-spec handle_async_dispatch(
    term(),
    class_name(),
    #{selector() => term()},
    class_name() | none,
    atom()
) -> ok.
handle_async_dispatch({Selector, Args, FuturePid}, ClassName, InstanceMethods, Superclass, Module) ->
    case {Selector, Args} of
        {methods, []} ->
            FuturePid ! {resolve, maps:keys(InstanceMethods)};
        {superclass, []} ->
            Resolved =
                case Superclass of
                    none -> nil;
                    _ -> Superclass
                end,
            FuturePid ! {resolve, Resolved};
        {class_name, []} ->
            FuturePid ! {resolve, ClassName};
        {module_name, []} ->
            FuturePid ! {resolve, Module};
        {{method, _MethodSelector}, _} ->
            Error = beamtalk_error:new(type_error, ClassName),
            FuturePid ! {reject, Error};
        _ ->
            Error = beamtalk_error:new(does_not_understand, ClassName, Selector),
            FuturePid ! {reject, Error}
    end,
    ok;
handle_async_dispatch(_Msg, _ClassName, _InstanceMethods, _Superclass, _Module) ->
    ok.

-doc """
Classify an `undef` error as either a dispatch-level failure or one
raised from inside the method body.

When `erlang:apply(Module, Fun, Args)` is called and Module:Fun/Arity does
not exist, the top stacktrace frame is `{Module, Fun, ActualArgs, []}`.
If the undef originated from code inside the method, the top frame belongs
to some other module/function.

Returns:
  `{true, module_not_loaded}` — Module is not loaded; class failed to load.
  `{true, method_not_found}`  — Module is loaded but Fun/Arity is not exported.
  `false`                     — undef was raised from inside the method body.
""".
-spec is_dispatch_undef(atom(), atom(), list()) ->
    {true, module_not_loaded | method_not_found} | false.
is_dispatch_undef(Module, FunName, [{Module, FunName, _, _} | _]) ->
    case code:is_loaded(Module) of
        false -> {true, module_not_loaded};
        _ -> {true, method_not_found}
    end;
is_dispatch_undef(_Module, _FunName, _ST) ->
    false.

%%% ============================================================================
%%% Internal Functions — Class Process Crash Recovery
%%% ============================================================================

-doc """
Execute a class_send operation with automatic crash detection and recovery.

Wraps a gen_server call in a try/catch. If the target class process
has crashed (noproc), looks up the class name from the pid reverse index,
attempts auto-restart via `beamtalk_class_registry:restart_class/1`, and
retries the operation with the new pid. If restart fails, raises a clear error.

Only attempts recovery once to avoid infinite loops.
""".
-spec class_send_with_recovery(pid(), selector(), fun((pid()) -> term())) -> term().
class_send_with_recovery(ClassPid, Selector, Action) ->
    %% bt-catcher-audit: not-applicable-other-process - Action is a gen_server call to the class
    %% process
    try
        Action(ClassPid)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            handle_class_crash_recovery(ClassPid, Selector, Action);
        exit:{noproc, _} ->
            handle_class_crash_recovery(ClassPid, Selector, Action)
    end.

-doc "Attempt to recover from a crashed class process and retry the operation.".
-spec handle_class_crash_recovery(pid(), selector(), fun((pid()) -> term())) -> term().
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
bodies (`beamtalk_actor:doSpawn/1`, `doSpawnWith/2`), matching the treatment
given to `new`/`new:`. This function — and `class_send/3`'s explicit
`spawn`/`spawnWith:` clauses above, and `handle_metaclass_self_call/2` below
— deliberately keep routing through `class_self_spawn`/`class_self_new`
directly rather than falling through to generic inherited class-method
dispatch: doing so would mean a self-send here reaches the lifted body via
`gen_server:call(self(), ...)`, which deadlocks — the exact regression this
routing exists to avoid. Same "keep the shim, the lifted body is
the dynamic-dispatch definition, not the one every path actually calls"
choice made for `new`/`new:` — see `beamtalk_actor:doSpawn/1`'s doc.
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
-spec handle_metaclass_self_call(selector(), list()) -> term().
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
    ClassName =
        case get(beamtalk_class_name) of
            undefined -> class_name_from_pid(self());
            CN -> CN
        end,
    Error0 = beamtalk_error:new(dispatch_error, ClassName, Selector),
    Error1 = beamtalk_error:with_hint(
        Error0,
        iolist_to_binary(
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

Preserves the original selector and attempts to extract the class name from
the process's registered name for diagnostic purposes.
""".
-spec handle_self_instantiation_error(atom()) -> no_return().
handle_self_instantiation_error(Selector) ->
    ClassName = class_name_from_pid(self()),
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
Extract the Beamtalk class name from a class gen_server pid's registered name.

Class processes are registered as 'beamtalk_class_<ClassName>'. This strips
that prefix to recover the class name for use in error messages.
""".
-spec class_name_from_pid(pid()) -> atom().
class_name_from_pid(ClassPid) ->
    case erlang:process_info(ClassPid, registered_name) of
        {registered_name, RegName} ->
            RegStr = atom_to_list(RegName),
            Prefix = "beamtalk_class_",
            case lists:prefix(Prefix, RegStr) of
                true ->
                    Suffix = lists:nthtail(length(Prefix), RegStr),
                    try
                        list_to_existing_atom(Suffix)
                    catch
                        error:badarg -> unknown
                    end;
                false ->
                    unknown
            end;
        _ ->
            unknown
    end.

%%% ============================================================================
%%% Internal Functions — ADR 0032 Phase 0
%%% ============================================================================

-doc """
Try dispatching through the Class chain (ADR 0032 Phase 0).

When a class-side message is not found in user-defined class methods,
fall through to instance methods of 'Class'. This implements the
Smalltalk metaclass protocol: every class object is an instance of 'Class'.

ClassSelf has the metaclass tag (e.g., 'Counter class') so methods in
'Class' receive the correct self when invoked.

Returns {ok, Result} if the method is found in the Class chain.
Returns not_found if 'Class' does not understand the selector (does_not_understand).
Re-raises any other error (type_error, arity_mismatch, etc.) from Class methods
so callers see real errors rather than a misleading DNU on the original class.

NOTE: NewState from Class methods is intentionally dropped. Class has no
persistent instance state in Phase 0-1; if stateful Class methods are added
in Phase 2+, this must be revisited.
""".
-spec try_class_chain_fallthrough(#beamtalk_object{}, atom(), list()) ->
    {ok, term()} | not_found.
try_class_chain_fallthrough(ClassSelf, Selector, Args) ->
    %% Class objects have no mutable instance state; use an empty map.
    %% The Class methods receive ClassSelf (with its metaclass tag) as self.
    case beamtalk_dispatch:lookup(Selector, Args, ClassSelf, #{}, 'Class') of
        {reply, Result, _NewState} ->
            {ok, Result};
        {error, #beamtalk_error{kind = does_not_understand}} ->
            %% Method not found anywhere in the Class chain — tell caller to raise DNU.
            not_found;
        {error, #beamtalk_error{kind = class_not_found}} ->
            %% 'Class' process not registered (pre-Phase 2 or process died) — graceful fallback.
            not_found;
        {error, Error} ->
            %% A real error from a Class method (type_error, arity_mismatch, etc.).
            %% Error may be a raw #beamtalk_error{} or an already-wrapped Exception map
            %% (dispatch/4 catches and returns wrapped exceptions as {error, Wrapped, State}).
            %% Use reraise for idempotent wrapping before re-raising.
            beamtalk_exception_handler:reraise(Error)
    end.
