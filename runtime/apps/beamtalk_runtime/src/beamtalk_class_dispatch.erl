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

%% BT-5021: slack added to `?BT_INITIALIZE_TIMEOUT` so the class process's own
%% structured init-timeout reply reaches an instantiating caller first (ms).
-define(BT_INSTANTIATION_CALL_MARGIN, 2000).

-export([
    class_send/3,
    class_send/4,
    handle_class_self_call/1,
    lookup_direct_constructor/3,
    class_super_dispatch/4,
    inherited_class_new/1,
    class_self_send/3,
    class_self_direct_ok/4,
    class_self_dispatch_local/4,
    metaclass_send/4,
    unwrap_class_call/1,
    class_method_fun_name/1,
    is_test_execution_selector/1,
    handle_class_method_call/6,
    handle_async_dispatch/5,
    class_understands_class_selector/2,
    class_object_understands/2,
    find_class_method_from_class/2,
    in_caller_owner/2,
    class_method_body/2
]).

%% BT-5047: what `beamtalk_class_dispatch_self` and `_recovery` (split out of this
%% module) call back into. Exported for the split only; each is an existing
%% private helper of this module, internal to the dispatch family and not a
%% supported API for other runtime modules. (`find_class_method_from_class/2` is
%% already exported above.)
-export([
    apply_class_method_in_context/6,
    class_side_dispatch_found/5,
    find_class_side_method_from_class/2,
    handle_class_self_call/2,
    try_class_chain_fallthrough/3
]).

%% BT-5047: used by the specs of the modules split out of this one.
-export_type([
    selector/0, class_name/0, call_timeout/0, verdict/0, class_side_found/0, class_method_outcome/0
]).
-type selector() :: atom().
-type class_name() :: atom().
%% The bound on a class-process `class_method_call`: `default` picks the
%% per-selector default (`class_method_call_timeout/1`), anything else is
%% passed to `gen_server:call/3` unchanged (BT-4556).
-type call_timeout() :: timeout() | default.

%% ADR 0142 (BT-4300): the in-caller class-send path is one call into the
%% compiled method, plus the guard and memo reads; its helpers are inlined so
%% the hop costs no extra reductions.
-compile(
    {inline, [
        in_caller_owner/2,
        class_method_owner/2,
        run_in_caller/7,
        send_in_caller_or/4
    ]}
).
%% ADR 0142 §2 (BT-4300): a class method the caller may run itself: the
%% receiving class, its metaclass tag and module, the defining class and
%% module, and the compiled function name (`class_method_owner/2`).
-type owner() :: {owner, class_name(), atom(), module(), class_name(), module(), atom()}.
%% What the class-method memo records (`class_method_owner/2`).
-type verdict() :: owner() | decline.
-export_type([owner/0]).
%% What the class-side chain walk (`find_class_side_method/3`) finds first: a
%% class method in a class's table (compiled or ADR 0084 runtime fun).
-type class_side_found() :: {method, class_name(), module()} | not_found.

-doc """
Send a message to a class object synchronously (ADR 0013 Phase 1).

Dispatches messages to the class gen_server, translating the Beamtalk
message protocol ({Selector, Args}) to the class process message format.
Unwraps {ok, Value} / {error, Error} results for seamless integration.
""".
-spec class_send(pid() | atom(), atom(), list()) -> term().
class_send(ClassPid, Selector, Args) ->
    class_send(ClassPid, Selector, Args, default).

-doc """
`class_send/3` with an explicit bound on the wait for the class process
(BT-4556): when the send of a class method goes through the class gen_server
(ADR 0142 declined to run it in the caller), `Timeout` replaces the default
`class_method_call` timeout (`class_method_call_timeout/1`: 60 s, 5 min for
test execution); `default` keeps it. A send that runs in the caller has no
wait to bound, so `Timeout` does not apply to it; neither does it apply to the
instantiation and supervisor-start selectors (`new`, `new:`, `spawn`,
`spawnWith:`, `supervise`, ...), which keep their own routes.
""".
-spec class_send(pid() | atom(), atom(), list(), call_timeout()) -> term().
class_send(undefined, Selector, _Args, _Timeout) ->
    Error = beamtalk_error:new(class_not_found, unknown, Selector),
    beamtalk_error:raise(Error);
%% BT-4723: generated code passes the class's own name when its registry
%% lookup found no process, so the error names the class.
class_send(ClassName, Selector, _Args, _Timeout) when is_atom(ClassName) ->
    Error = beamtalk_error:new(class_not_found, ClassName, Selector),
    beamtalk_error:raise(Error);
%% ADR 0135 §1: `supervise` and the spawn family on a Supervisor or
%% DynamicSupervisor class start the tree in the caller, through
%% `beamtalk_supervisor:class_start/4`, never in the class gen_server (which
%% would queue every other send to the class behind the start and deadlock
%% any child that messages the class while it starts).
class_send(ClassPid, Selector, Args, Timeout) when
    is_pid(ClassPid),
    (Selector =:= supervise orelse Selector =:= spawn orelse Selector =:= 'spawnWith:' orelse
        Selector =:= 'spawnAs:' orelse Selector =:= 'spawnWith:as:')
->
    case supervisor_start_target(ClassPid) of
        {ok, ClassName, Module} ->
            beamtalk_supervisor:class_start(ClassName, Module, Selector, Args);
        error ->
            class_send_plain(ClassPid, Selector, Args, Timeout)
    end;
class_send(ClassPid, Selector, Args, Timeout) ->
    class_send_plain(ClassPid, Selector, Args, Timeout).

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

-spec class_send_plain(pid(), atom(), list(), call_timeout()) -> term().
%% Self-call for instantiation selectors — bypass gen_server to avoid deadlock.
%% When a class method sends new/new:/spawn/spawnWith: to its own class (ClassPid == self()),
%% gen_server:call(self(), ...) would deadlock. Instead, perform instantiation directly
%% using class metadata from the process dictionary (set during init).
class_send_plain(ClassPid, 'new', Args, _Timeout) when ClassPid =:= self() ->
    beamtalk_class_dispatch_recovery:handle_self_instantiation(new, 'new', Args);
class_send_plain(ClassPid, 'new:', Args, _Timeout) when ClassPid =:= self() ->
    beamtalk_class_dispatch_recovery:handle_self_instantiation(new, 'new:', Args);
class_send_plain(ClassPid, spawn, Args, _Timeout) when ClassPid =:= self() ->
    beamtalk_class_dispatch_recovery:handle_self_instantiation(spawn, spawn, Args);
class_send_plain(ClassPid, 'spawnWith:', Args, _Timeout) when ClassPid =:= self() ->
    beamtalk_class_dispatch_recovery:handle_self_instantiation(spawn, 'spawnWith:', Args);
class_send_plain(ClassPid, 'new', [], Timeout) ->
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
                unwrap_class_call(instantiation_call(P, {new, []}, 'new', Timeout))
            end)
    end;
class_send_plain(ClassPid, 'new:', [Arg], Timeout) ->
    %% same for `new:` with a map argument, via `Module:new/1`. A non-map
    %% argument never builds directly (see `lookup_direct_constructor/3`).
    %% BT-4475: either way, a class whose own explicit class-side `new:` needs
    %% nothing from its class process (codegen leaves `new:` out of its
    %% published process-bound selectors) runs that class method in the
    %% caller, exactly as the `{new, [Arg]}` handler would have called it
    %% there (`has_class_new_in_chain`), behind the same guard as every
    %% in-caller class method. Anything else keeps the gen_server path, which
    %% reproduces the legacy fallbacks.
    case lookup_direct_constructor(ClassPid, 'new:', [Arg]) of
        {ok, Module} ->
            erlang:apply(Module, new, [Arg]);
        error ->
            send_in_caller_or(ClassPid, 'new:', [Arg], fun() ->
                class_send_with_recovery(ClassPid, 'new:', fun(P) ->
                    unwrap_class_call(instantiation_call(P, {new, [Arg]}, 'new:', Timeout))
                end)
            end)
    end;
class_send_plain(ClassPid, spawn, [], Timeout) ->
    class_send_with_recovery(ClassPid, spawn, fun(P) ->
        unwrap_class_call(instantiation_call(P, {spawn, []}, spawn, Timeout))
    end);
class_send_plain(ClassPid, 'spawnWith:', [Map], Timeout) ->
    class_send_with_recovery(ClassPid, 'spawnWith:', fun(P) ->
        unwrap_class_call(instantiation_call(P, {spawn, [Map]}, 'spawnWith:', Timeout))
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
%% inside that gen_server: the class-object protocol answers in place when the
%% class itself does not understand the selector
%% (`class_send_from_own_process/2`, BT-4262). Otherwise
%% `gen_server:call(self(), ...)` would not hang — it
%% exits with `{calling_self, {gen_server, call, [...]}}`, a raw tuple that
%% says nothing about what the caller did wrong. The commonest way to get here
%% is a block running inside a class method that messages its own class:
%%
%%     File open: p mode: #read do: [:h | File exists: q]
%%
%% Report it the way `handle_metaclass_self_call/2` already reports
%% the metaclass equivalent. No working path changes: the alternative was an
%% exit, not a successful call.
class_send_plain(ClassPid, Selector, Args, Timeout) ->
    %% ADR 0142 §2 (BT-4300), ADR 0153 §5: a class method of a class that is
    %% not shadowed, sent a selector that is not process-bound, runs in the
    %% caller. Checked before the self-call guard: an in-caller call has no
    %% gen_server to deadlock on, so a block re-entering its class answers.
    %% BT-4771: liveness is probed once for the whole send and handed to both
    %% the in-caller check and the via-process path below.
    Alive = local_class_alive(ClassPid),
    case in_caller_owner(ClassPid, Selector, Alive) of
        {owner, ClassName, Tag, Module, DefiningClass, DefiningModule, FunName} ->
            ClassSelf = #beamtalk_object{class = Tag, class_mod = Module, pid = ClassPid},
            run_in_caller(
                ClassSelf, Selector, Args, ClassName, DefiningClass, DefiningModule, FunName
            );
        decline ->
            class_send_via_process(ClassPid, Selector, Args, Timeout, Alive)
    end.

%% Is `ClassPid` a live process on this node? The one liveness probe of a
%% class send (see `class_send_plain/4`).
-spec local_class_alive(pid()) -> boolean().
local_class_alive(ClassPid) ->
    node(ClassPid) =:= node() andalso is_process_alive(ClassPid).

%% Run `Selector` in the caller when `in_caller_owner/2` names its owner
%% (ADR 0142 §2-§3), else `Otherwise()`: the one in-caller step of both the
%% general class-method clause and the `new:` clause (BT-4475).
-spec send_in_caller_or(pid(), selector(), list(), fun(() -> term())) -> term().
send_in_caller_or(ClassPid, Selector, Args, Otherwise) ->
    case in_caller_owner(ClassPid, Selector) of
        {owner, ClassName, Tag, Module, DefiningClass, DefiningModule, FunName} ->
            ClassSelf = #beamtalk_object{class = Tag, class_mod = Module, pid = ClassPid},
            run_in_caller(
                ClassSelf, Selector, Args, ClassName, DefiningClass, DefiningModule, FunName
            );
        decline ->
            Otherwise()
    end.

%% Today's routing for a send the caller may not run itself (ADR 0142 §2).
-spec class_send_via_process(pid(), selector(), list(), call_timeout(), boolean()) -> term().
%% There is no unguarded direct call here (BT-4301): `in_caller_owner/2`
%% already ran every class method the caller may run, so anything left is a
%% shadowed method (an ADR 0084 runtime fun the class
%% process honours) or a process-bound selector. Only a class's send to itself
%% takes the first clause below.
class_send_via_process(ClassPid, Selector, Args, _Timeout, _Alive) when ClassPid =:= self() ->
    class_send_from_own_process(Selector, Args);
class_send_via_process(ClassPid, Selector, Args, Timeout, Alive) ->
    class_send_with_recovery(ClassPid, Selector, fun(P) ->
        %% The restarted pid of a crash-recovery retry is a different process:
        %% probe it afresh. The first attempt reuses the send's one probe.
        PAlive =
            case P =:= ClassPid of
                true -> Alive;
                false -> local_class_alive(P)
            end,
        class_send_dispatch(P, Selector, Args, Timeout, PAlive)
    end).

-doc """
ADR 0142 §1-§2 (BT-4300): may the caller run `Selector` sent to the class at
`ClassPid` itself? `{owner, ClassName, Tag, Module, DefiningClass,
DefiningModule, FunName}` when all of these hold, else `decline` (take
`class_send_via_process/5`):

- the class is local and alive;
- `class_method_owner/2` (memoised) resolves the defining class: the
  selector is not process-bound (the generated list, ADR 0153 §5) and the
  class-process lookup would answer it with a class method;
- `beamtalk_class_shadow_flags:direct_call_ok/2` holds for the receiving
  class (no ADR 0084 runtime fun, bootstrap finished). One `persistent_term`
  read of a raw-flag input its writers raise before the change becomes
  visible.

The memo stays consistent with class-metadata writes through the derived-fact
cache's generation counter (`beamtalk_class_derived_cache`): an answer derived
before a write is never served after the write's invalidation. Nothing here
relies on a derived flag having been erased. As for every metadata-driven path,
a hot reload's new code is current a moment before its class process publishes
the new metadata.

Exported for `beamtalk_class_shadow_flags:static_call_target/4` (BT-4316),
which installs a static call key only while this answer names the call
site's own class, module and function: one rule for both paths.
""".
-spec in_caller_owner(pid(), selector()) -> owner() | decline.
in_caller_owner(ClassPid, Selector) ->
    in_caller_owner(ClassPid, Selector, local_class_alive(ClassPid)).

%% `in_caller_owner/2` with the liveness probe (`local_class_alive/1`) already
%% done by the caller.
-spec in_caller_owner(pid(), selector(), boolean()) -> owner() | decline.
in_caller_owner(ClassPid, Selector, true) ->
    %% Alive before the memo is consulted: a send to a dead incarnation takes
    %% the crash recovery path and never derives a memo from a class mid-init.
    case class_method_owner(ClassPid, Selector) of
        {owner, ClassName, Tag, _Module, _DefiningClass, _DefiningModule, _FunName} = Owner ->
            case beamtalk_class_shadow_flags:direct_call_ok(Tag, ClassName) of
                true -> Owner;
                false -> decline
            end;
        decline ->
            decline
    end;
in_caller_owner(_ClassPid, _Selector, false) ->
    decline.

-doc """
The class-method owner memo, `{class_method_owner, ClassPid, Selector}` in
`beamtalk_class_derived_cache` (ADR 0142 §2, the BT-4109 `resolve_owner`
pattern of `beamtalk_dispatch`). Keyed by the class object's pid, which names
one class for its whole life, so a hit needs no pid-to-name lookup; a restarted
class has a new pid (and its `init/1` flushes the cache anyway).

Derived by walking the chain exactly as the class process does
(`handle_class_method_call/6`), through the one class-side walk
`find_class_side_method/3`: at the receiving class and each ancestor, a
the class's methods from its published selector list, which includes ADR 0084
runtime funs, so a runtime fun at an ancestor counts as that ancestor's method
(and is applied from the fun store by `apply_class_method_body/7`).

`decline` is memoised too (process-bound selector, no class method in the
chain or a fact unpublished): the next metadata
write flushes it. A pid with no registered class, or a class with no metadata
row, is never memoised. Every input is a class-metadata row field whose
writers invalidate the cache after the write lands: `insert/5`,
`merge_identity/5`, `set_runtime_class_methods/2` (selectors, superclass,
module) and `put_process_bound_selectors/2`.
""".
-spec class_method_owner(pid(), selector()) -> verdict().
class_method_owner(ClassPid, Selector) ->
    beamtalk_class_derived_cache:fetch(
        {class_method_owner, ClassPid, Selector},
        fun() -> compute_class_method_owner(ClassPid, Selector) end
    ).

-spec compute_class_method_owner(pid(), selector()) -> {cache | nocache, verdict()}.
compute_class_method_owner(ClassPid, Selector) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} -> compute_class_method_owner_of(ClassName, Selector);
        not_found -> {nocache, decline}
    end.

-spec compute_class_method_owner_of(class_name(), selector()) ->
    {cache | nocache, verdict()}.
compute_class_method_owner_of(ClassName, Selector) ->
    case beamtalk_class_metadata:lookup_process_bound_selectors(ClassName) of
        {ok, ProcessBound} ->
            {cache, derive_class_method_owner(ClassName, Selector, ProcessBound)};
        unknown ->
            %% No generated list: every selector is process-bound (ADR 0142
            %% §1 rule 3). Cached only while the class has a row, whose later
            %% publication of the list flushes it.
            case beamtalk_class_metadata:lookup_methods(ClassName) of
                {ok, _, _} -> {cache, decline};
                not_found -> {nocache, decline}
            end
    end.

-spec derive_class_method_owner(class_name(), selector(), [selector()]) ->
    verdict().
derive_class_method_owner(ClassName, Selector, ProcessBound) ->
    case lists:member(Selector, ProcessBound) of
        true ->
            decline;
        false ->
            case find_class_side_method_from_class(Selector, ClassName) of
                {method, DefiningClass, DefiningModule} ->
                    owner_of(ClassName, DefiningClass, DefiningModule, Selector);
                not_found ->
                    %% Only the `Class` chain answers: the BT-4073 skip in
                    %% `class_send_dispatch/3` already runs it in the caller.
                    decline
            end
    end.

-spec owner_of(class_name(), class_name(), module(), selector()) -> owner() | decline.
owner_of(ClassName, DefiningClass, DefiningModule, Selector) ->
    case beamtalk_class_metadata:lookup_module(ClassName) of
        {ok, Module} ->
            {owner, ClassName, receiver_tag(ClassName), Module, DefiningClass, DefiningModule,
                class_method_fun_name(Selector)};
        not_found ->
            decline
    end.

-doc """
Run a class method in the caller (ADR 0142 §3). `ClassSelf` is the real class
reference of the receiving class, the value its class literal evaluates to
(its metaclass tag, its own module and its registered pid), even for an
inherited method, as every class-side path now builds it
(the caller's own `ClassSelf`, `invoke_class_method/7`).
`beamtalk_object_class:local_call/3` used to apply its selector from
`class_mod` alone; it now walks the class-method chain from the receiving
class, so an inherited selector is found and a subclass override wins
(BT-4300). No class-variable
home is installed (ADR 0153: compiled code no longer reads one), and ADR 0130
§1's no-nesting rule concerns only homes. The class-process services (session
context seeding, the mirrored caller pid, the entry group leader, the call
timeouts) are not recreated: the caller is the context they stand in for.

The outcome is adapted exactly as the class-process path adapts the same
outcome (`run_with_class_vars/3`'s reply, then `class_method_call/4`), so
every error is raised with the value it has today.
""".
-spec run_in_caller(
    #beamtalk_object{}, selector(), list(), class_name(), class_name(), module(), atom()
) -> term().
run_in_caller(ClassSelf, Selector, Args, ClassName, DefiningClass, DefiningModule, FunName) ->
    Outcome =
        case DefiningClass =:= ClassName of
            %% `direct_call_ok/2` held, so the receiving class has no runtime
            %% class-method fun: its compiled export is the method.
            true ->
                apply_compiled_class_method(
                    ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector, FunName
                );
            false ->
                apply_class_method_body(
                    ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector, FunName
                )
        end,
    case Outcome of
        {ok, Result} ->
            Result;
        _ ->
            class_method_result(
                ClassSelf#beamtalk_object.pid, Selector, Args, outcome_reply(Outcome)
            )
    end.

-doc """
Resolve a dynamic `new`/`new:` send to a direct module call, if eligible.

Eligible when the class's baked `direct_constructors` lists the selector, the
class names a module and has no runtime-installed class-method funs (the
`beamtalk_class_metadata:lookup_direct_constructor/2` gates),
`beamtalk_class_shadow_flags:direct_call_ok/2` holds for the class (no ADR
0084 runtime fun; BT-4409), and the module exports `new` at the right
arity (`new/0` for `new`, `new/1` for `new:` with a map argument). For `new:`,
no ancestor may define a user class-side `new:`
(`beamtalk_class_metadata:chain_has_user_new1/1`, from the facts the
ancestors publish). A non-map `new:` argument is never eligible: the
gen_server path reproduces the legacy `IsConstructible = false` fallback and
the `new: expects a Dictionary argument` error there.

The class's own verdict is generated metadata (Rust's `direct_constructors`),
never re-derived here: the baked `new:` entry only says the class's own
synthesized `new:` is eligible (Rust also folds in the chain as it was at
compile time, which can only keep it off). The chain is decided at run time
(BT-4409), so an ancestor reloaded with a user `new:` is honoured by an
already-loaded descendant without recompiling it, and dropping it again
brings the fast path back. `new` needs no chain check because `new/0` is
itself the override slot (a custom `new` compiles to, or delegates from, it),
so the direct call and the gen_server path reach the same function either
way.

The class, its tag and module (or the decline) are memoised per class pid and
selector in `beamtalk_class_derived_cache` (`{direct_constructor, ClassPid,
Selector}`), flushed by every class-metadata write; `direct_call_ok/2` (one
`persistent_term` read) and the export check run on every send. The export
check only sees loaded code, so a class whose module nobody has touched yet
takes one gen_server fallback (which loads it) before going direct.

`beamtalk_class_shadow_flags:static_call_target/4` installs a static `new`
call key only while this answers `{ok, Module}` for the call site's module
(BT-4409): one rule for both paths.
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
    case direct_constructor_owner(ClassPid, Selector) of
        {ok, ClassName, Tag, Module} ->
            case
                beamtalk_class_shadow_flags:direct_call_ok(Tag, ClassName) andalso
                    erlang:function_exported(Module, new, Arity)
            of
                true -> {ok, Module};
                false -> error
            end;
        error ->
            error
    end.

%% The memoised part of `lookup_direct_constructor/3` (see its doc).
-spec direct_constructor_owner(pid(), selector()) ->
    {ok, class_name(), atom(), module()} | error.
direct_constructor_owner(ClassPid, Selector) ->
    beamtalk_class_derived_cache:fetch(
        {direct_constructor, ClassPid, Selector},
        fun() -> compute_direct_constructor_owner(ClassPid, Selector) end
    ).

-spec compute_direct_constructor_owner(pid(), selector()) ->
    {cache | nocache, {ok, class_name(), atom(), module()} | error}.
compute_direct_constructor_owner(ClassPid, Selector) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            {cache, direct_constructor_owner_of(ClassName, Selector)};
        not_found ->
            {nocache, error}
    end.

-spec direct_constructor_owner_of(class_name(), selector()) ->
    {ok, class_name(), atom(), module()} | error.
direct_constructor_owner_of(ClassName, Selector) ->
    case beamtalk_class_metadata:lookup_direct_constructor(ClassName, Selector) of
        {ok, Module} ->
            case
                (Selector =:= 'new:' andalso
                    beamtalk_class_metadata:chain_has_user_new1(ClassName)) orelse
                    (Selector =:= new andalso
                        beamtalk_class_metadata:inherits_user_class_new(ClassName))
            of
                true -> error;
                false -> {ok, ClassName, receiver_tag(ClassName), Module}
            end;
        error ->
            error
    end.

-doc """
A send to a class from inside that class's own process (BT-4262).

When only the `Class` chain can answer `Selector` (no
local, inherited or runtime-installed class method:
`class_chain_only_send/2`, the external
send's own check), the class process's own lookup would miss and fall
through to the class-object protocol (`Class -> Behaviour ->
...`). That protocol runs as instance methods of the class object, not as a
class method, so it answers here, in this process, through
`class_self_protocol_fallthrough/4`: the same path a class-side `self`
send takes (BT-4252). This is what lets a Beamtalk-bodied protocol method
that sends to its receiver (`Class>>printString => self name asString`)
answer when it runs in the class process.

The instance-field reflection sends (`hasField:`, `clearField:`,
`fieldAt:`, `fieldAt:put:`) are not part of that protocol (ADR 0153
decision 7, BT-5155): unless the class defines one as a class method, the
fallthrough refuses them and the send raises `does_not_understand`.

A selector the class itself understands would run one of its own class
methods re-entrantly, which has no safe route, so it still raises the
structured `dispatch_error` (`handle_class_self_call/1`). So does a protocol
primitive that changes the class (`removeFromSystem`, `renameTo:`, `doc:`,
...): `beamtalk_behaviour_intrinsics` refuses it up front, before any side
effect.
""".
-spec class_send_from_own_process(selector(), list()) -> term() | no_return().
class_send_from_own_process(Selector, Args) ->
    class_send_from_own_process_lookup(Selector, Args).

%% Any other send: the same decision
%% the external send makes before skipping the class process
%% (`class_chain_only_send/2`), so the two cannot disagree.
-spec class_send_from_own_process_lookup(selector(), list()) -> term() | no_return().
class_send_from_own_process_lookup(Selector, Args) ->
    case class_chain_only_send(self(), Selector) of
        {ok, ClassName, Module} ->
            %% This is the class's own process, so `self()` is its pid.
            ClassSelf = #beamtalk_object{
                class = receiver_tag(ClassName), class_mod = Module, pid = self()
            },
            beamtalk_class_dispatch_self:class_self_protocol_fallthrough(
                ClassName, Selector, Args, ClassSelf
            );
        error ->
            handle_class_self_call(Selector)
    end.

%% The name of the class whose process this is, from its process dictionary
%% (`'the class'` if unset), for an error raised in that process.
-spec own_class_name() -> atom().
own_class_name() ->
    case get(beamtalk_class_name) of
        undefined -> 'the class';
        ClassName -> ClassName
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
    handle_class_self_call(Selector, <<>>).

%% `handle_class_self_call/1` with `Note` appended to the hint (a binary; empty
%% for none).
-spec handle_class_self_call(selector(), binary()) -> no_return().
handle_class_self_call(Selector, Note) ->
    ClassName = own_class_name(),
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
            >>,
            Note
        ])
    ),
    beamtalk_error:raise(Error1).

-doc """
A class-side `super` send (BT-4001): `super Selector` in a class method (or
ClassBuilder class-method fun) of `ClassName`.

The lookup starts at `ClassName`'s superclass and never consults
`ClassName` itself, so its own class methods are not reached: a class method
that sends `super peek:` runs the superclass's method instead of recursing
into itself.

`ClassSelf` is the live receiver (the calling method's own `ClassSelf`), so
the found method runs under the receiver's class-variable key with the
receiver's real reference, and nothing is looked up to rebuild it (BT-4301).
Returns the
plain result (ADR 0130 §3). When no ancestor defines `Selector`, the
class-object protocol answers (`class_self_protocol_fallthrough/4`, BT-4252),
else a structured `does_not_understand` is raised.
""".
-spec class_super_dispatch(class_name(), selector(), list(), #beamtalk_object{}) ->
    term() | no_return().
class_super_dispatch(ClassName, Selector, Args, #beamtalk_object{} = ClassSelf) when
    is_list(Args)
->
    class_side_dispatch_found(
        ClassName, Selector, Args, find_class_super_method(Selector, ClassName), ClassSelf
    ).

-doc """
Run the nearest ancestor's class-side `new` for `ClassName` in this process
(BT-4738): what a class-side `self new` does when `ClassName` has no `new` of
its own and inherits a user `class new` (`beamtalk_class_metadata:inherits_user_class_new/1`).
The receiver (`self`) stays `ClassName`'s class object. `skip` when the chain
walk finds no `new` (a stale fact), so the caller falls back to the synthesized
constructor, like the external `{new, []}` handler.
""".
-spec inherited_class_new(class_name()) -> {ok, term()} | skip.
inherited_class_new(ClassName) ->
    case find_class_super_method(new, ClassName) of
        not_found ->
            skip;
        Found ->
            case beamtalk_class_registry:resolve_class_object(ClassName) of
                undefined ->
                    skip;
                ClassSelf ->
                    {ok, class_side_dispatch_found(ClassName, new, [], Found, ClassSelf)}
            end
    end.

-doc """
Apply what a class-side chain walk found (`find_class_side_method/3`) for a
self-send, a builder self-send or a `super` send: a class method through
`class_self_dispatch_chain/5`, and `not_found` falls through to the class-object
protocol there.
""".
-spec class_side_dispatch_found(
    class_name(), selector(), list(), class_side_found(), #beamtalk_object{}
) -> term() | no_return().
class_side_dispatch_found(ClassName, Selector, Args, {method, Owner, Module}, ClassSelf) ->
    beamtalk_class_dispatch_self:class_self_dispatch_chain(
        ClassName, Selector, Args, {ok, Owner, Module}, ClassSelf
    );
class_side_dispatch_found(ClassName, Selector, Args, not_found, ClassSelf) ->
    beamtalk_class_dispatch_self:class_self_dispatch_chain(
        ClassName, Selector, Args, not_found, ClassSelf
    ).

-doc """
The ancestor a class-side `super Selector` from `ClassName` resolves to: the
class-side chain walk (`find_class_side_method/3`) from `ClassName`'s
superclass.
""".
-spec find_class_super_method(selector(), class_name()) -> class_side_found().
find_class_super_method(Selector, ClassName) ->
    find_class_side_method(Selector, superclass_from_ets(ClassName), ClassName).

-doc """
The class-side chain walk from `ClassName` itself: what an external
send, a class-side self-send and `respondsTo:` of `Selector` to `ClassName`
resolve to (`find_class_side_method/3`).
""".
-spec find_class_side_method_from_class(selector(), class_name()) -> class_side_found().
find_class_side_method_from_class(Selector, ClassName) ->
    find_class_side_method(Selector, ClassName, ClassName).

-doc """
The one class-side method lookup, walked from `StartName` up the superclass
chain: `{method, Class, Module}` for the first class whose published selector
list (compiled class methods, or ADR 0084 runtime funs) has `Selector`, else
`not_found`.

Shared by every class-side lookup: the external send
(`handle_class_method_call/6`), the in-caller owner memo
(`derive_class_method_owner/3`), the class-side self-send
(`class_self_send/3`, `class_self_dispatch_local/4`), `super`
(`class_super_dispatch/4`, from the superclass), `local_call/3`
(`find_class_method_from_class/2`) and `respondsTo:`
(`class_understands_class_selector/2`). `ClassName` names the receiving class
in the cycle warning.
""".
-spec find_class_side_method(selector(), class_name() | none, class_name()) ->
    class_side_found().
find_class_side_method(Selector, StartName, ClassName) ->
    StepFun = fun(AncestorName, _Depth) ->
        case beamtalk_class_metadata:lookup_methods(AncestorName) of
            {ok, AncestorModule, Selectors} ->
                case lists:member(Selector, Selectors) of
                    true -> {found, {method, AncestorName, AncestorModule}};
                    false -> {next, superclass_from_ets(AncestorName)}
                end;
            not_found ->
                not_found
        end
    end,
    case
        beamtalk_hierarchy:walk_or_warn(
            StartName, StepFun, find_class_side_method, #{start => ClassName}
        )
    of
        {found, Found} -> Found;
        not_found -> not_found;
        {cycle, _} -> not_found
    end.

-spec receiver_tag(class_name()) -> atom().
receiver_tag(ClassName) ->
    beamtalk_class_registry:class_object_tag(ClassName).

-doc """
Virtual class-side self-send (BT-3666): `self Selector` from inside a class
method, when the selector is not bound statically to a direct call.

`ClassSelf` is the calling method's own `ClassSelf`: the receiving class's real
reference (its metaclass tag, its own module and its registered pid), passed
as is, so no registry read rebuilds it per send (BT-4301; BT-4300 had looked
it up with `whereis_class` + `lookup_module` to fix the caller-pid bug).

Walks the class side from the receiving class itself
(`find_class_side_method/3`) and applies the found
`DefiningModule:class_<Selector>` (or runtime class-method fun) in the
caller's process via `apply_class_method_in_context/6`.

The receiving class is the class named by `ClassSelf`, which for an inherited
class method is a subclass of the class whose code is running. Starting at
that class means an override of `Selector` (or a subclass-only implementation
of a template-method hook) is found before the inherited definition, exactly
as an instance-side self-send resolves. A class-side `super`, builder or
compiled, uses `class_super_dispatch/4` instead, which never consults the
receiving class itself.

Returns the plain result (ADR 0130 §3). When no class in the chain defines
`Selector`, the class-object protocol answers
(`self name`, `self superclass`: `class_self_protocol_fallthrough/4`,
BT-4252), else a structured `does_not_understand` is raised.
""".
-spec class_self_send(#beamtalk_object{}, selector(), list()) -> term() | no_return().
class_self_send(#beamtalk_object{class = Tag} = ClassSelf, Selector, Args) ->
    ClassName = beamtalk_primitive:class_name_from_tag(Tag),
    class_side_dispatch_found(
        ClassName, Selector, Args, find_class_side_method_from_class(Selector, ClassName), ClassSelf
    ).

-doc """
Guard for the compiled class-side self-send fast path (BT-3666).

A class-side `self foo` (where the class itself defines `foo`, sealed or not:
BT-4301 retired the unguarded sealed form) compiles to
`case class_self_direct_ok(element(2, ClassSelf), 'Cls class', 'Cls', foo) of
true -> class_foo(ClassSelf, ...) ; false -> class_self_send(ClassSelf, ...)
end`. The direct call is only equivalent to the walk when the receiving class
is exactly the compiling class (`ReceiverTag =:= ClassTag`) AND nothing would
shadow the compiled method that the walk honours: a runtime-installed
class-method fun (ADR 0084, gated by the per-class `has_runtime_class_methods`
flag). The
`TestCase` run-selector guard (`test_spawn`) is preserved by declining.

Costs one `persistent_term` read for the defining-class receiver (BT-3700: a
derived per-class flag in `beamtalk_class_shadow_flags` folds the runtime-fun flag
and readiness; BT-3669/BT-3676 had three reads); a
subclass receiver short-circuits on the first clause. It asks
`shadow_free/2`, which since ADR 0153 answers exactly as `direct_call_ok/2`:
a self-send runs wherever its calling method already runs.
""".
-spec class_self_direct_ok(atom(), atom(), class_name(), selector()) -> boolean().
class_self_direct_ok(Tag, Tag, ClassName, Selector) ->
    case ClassName =:= 'TestCase' andalso is_test_execution_selector(Selector) of
        true ->
            false;
        false ->
            %% Shadow flags not initialised yet (early bootstrap), or a
            %% shadow present, declines: the walk is always a correct answer.
            %% One persistent_term read of the derived flag (BT-3700).
            beamtalk_class_shadow_flags:shadow_free(Tag, ClassName)
    end;
class_self_direct_ok(_ReceiverTag, _ClassTag, _ClassName, _Selector) ->
    false.

-doc """
ADR 0084: Dispatch a class-method self-send that may resolve to the
class's OWN runtime-installed class method.

Class-method funs created by the programmatic `ClassBuilder` are
anonymous funs with no `class_<sel>` module export, so a self-send inside such a
fun cannot use the compiled direct-call path. It routes here instead.

Checks the class's own runtime class-method fun (the retrieval store)
first, and only then walks the class-method chain from `ClassName` itself (its own compiled method,
then inherited ones). Compiled code also reaches it as the fallback of an
explicit own-class reference's guarded direct call (`Base foo` in a `Base`
class method, BT-4301), which binds `foo` to `Base` whatever the receiver.

Returns the raw result; when no definition is found the class-object
protocol answers (`class_self_protocol_fallthrough/4`, BT-4252), else a
structured `does_not_understand` is raised. No `gen_server` hop: the local lookup is the same
ETS read the dispatch hot path already uses.

`ClassSelf` is the calling fun's own `ClassSelf`: the live receiver, which can
be a subclass of `ClassName`, so the found method runs under the receiver's
class-variable key with its real reference (BT-4301: passed in, not rebuilt
from a tag). Like `class_self_send/3` it starts at `ClassName` itself, so it
is not `super`: a class-side `super` uses `class_super_dispatch/4`.
""".
-spec class_self_dispatch_local(class_name(), selector(), list(), #beamtalk_object{}) ->
    term() | no_return().
class_self_dispatch_local(ClassName, Selector, Args, #beamtalk_object{} = ClassSelf) when
    is_list(Args)
->
    beamtalk_class_dispatch_self:class_self_dispatch_local(ClassName, Selector, Args, ClassSelf).

-doc """
Dispatch a class method call (the main logic for the catch-all clause).

A send that no class-side method can answer skips the class process (BT-4073):
when `class_chain_only_send/2` finds no local or
inherited class method and no runtime-installed class-method fun for the
selector, the `Class`/`Behaviour` method runs in the caller straight away,
exactly as it would after the gen_server replied `{error, not_found}`.
""".
-spec class_send_dispatch(pid(), selector(), list(), call_timeout(), boolean()) -> term().
class_send_dispatch(ClassPid, Selector, Args, Timeout, Alive) ->
    case class_chain_only_send(ClassPid, Selector, Alive) of
        {ok, ClassName, Module} ->
            class_chain_send(ClassPid, ClassName, Module, Selector, Args);
        error ->
            class_method_call(ClassPid, Selector, Args, Timeout)
    end.

-doc """
`{ok, ClassName, Module}` when a send of `Selector` to the class at `ClassPid`
can only be answered by the `Class` chain, so asking the class process first
would only learn `{error, not_found}`; else `error` (take the gen_server path).

Reads the same metadata the class process's own lookup
(`handle_class_method_call/6`) consults, through
`class_understands_class_selector/2`: the local class-method selectors (which `beamtalk_object_class` publishes from its
`class_methods` map, runtime-installed ADR 0084 funs included, on every init,
reload and `put_class_method`) and the inherited chain. It is general: no
selector is special-cased here.

A class with no metadata row yet (still registering), a pid with no registered
class, or a dead class process takes the gen_server path too, so registration
races and crash recovery (`class_send_with_recovery/3`) are unchanged.
""".
-spec class_chain_only_send(pid(), selector()) ->
    {ok, class_name(), module()} | error.
class_chain_only_send(ClassPid, Selector) ->
    class_chain_only_send(ClassPid, Selector, local_class_alive(ClassPid)).

%% `class_chain_only_send/2` with the liveness probe (`local_class_alive/1`,
%% which includes the local-node test) already done by the caller.
-spec class_chain_only_send(pid(), selector(), boolean()) ->
    {ok, class_name(), module()} | error.
class_chain_only_send(ClassPid, Selector, Alive) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_metadata:lookup_methods(ClassName) of
                {ok, Module, _Selectors} ->
                    case
                        Alive andalso
                            not class_understands_class_selector(ClassName, Selector)
                    of
                        true -> {ok, ClassName, Module};
                        false -> error
                    end;
                not_found ->
                    error
            end;
        not_found ->
            error
    end.

-doc """
Run a class-object send through the `Class` chain in the caller
(`try_class_chain_fallthrough/3`), raising `does_not_understand` on `ClassName`
when the chain does not define `Selector` either. Shared by the gen_server
path's `{error, not_found}` reply and the BT-4073 skip.
""".
-spec class_chain_send(pid(), class_name(), atom(), selector(), list()) -> term().
class_chain_send(ClassPid, ClassName, Module, Selector, Args) ->
    %% ADR 0032 Phase 0: Class objects are instances of 'Class'; fall through to
    %% Class instance methods before raising does_not_understand.
    ClassSelf = #beamtalk_object{
        class = beamtalk_class_registry:class_object_tag(ClassName),
        class_mod = Module,
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
    end.

-doc "Ask the class process to run a class method (`class_method_call`).".
-spec class_method_call(pid(), selector(), list(), call_timeout()) -> term().
class_method_call(ClassPid, Selector, Args, default) ->
    class_method_call(ClassPid, Selector, Args, class_method_call_timeout(Selector));
class_method_call(ClassPid, Selector, Args, Timeout) ->
    %% Try user-defined class methods before raising does_not_understand.
    %% ADR 0081: pass the caller's session context explicitly in the
    %% message instead of having the class gen_server copy our whole process
    %% dictionary via `process_info/2`. We read our own two keys cheaply with
    %% `get/1` here and the receiving clause seeds from the tuple.
    %% BT-4556: running out of `Timeout` waiting for this class process raises
    %% the structured `timeout` error an actor send raises, not the raw
    %% `gen_server:call` exit. Only this call's own timeout is translated: the
    %% `[ClassPid | _]` match leaves any other exit unchanged.
    Reply =
        try
            gen_server:call(
                ClassPid, {class_method_call, Selector, Args, local_session_context()}, Timeout
            )
        catch
            exit:{timeout, {gen_server, call, [ClassPid | _]}} ->
                raise_class_timeout(ClassPid, Selector, Timeout)
        end,
    case Reply of
        {error, Nlr} when ?IS_NLR(Nlr) ->
            %% A `^` out of a caller-supplied block the class process ran:
            %% re-thrown as `class_method_result/4` would, and told to the
            %% test-build commit check as thrown away from home, since the
            %% block ran in another process (BT-4536). `run_in_caller/7`'s
            %% `^` never hopped, so it takes `class_method_result/4`'s.
            beamtalk_actor_state:rethrow_from_away(Nlr);
        _ ->
            class_method_result(ClassPid, Selector, Args, Reply)
    end.

-doc """
BT-5021: the `gen_server:call` behind `new`, `new:`, `spawn` and `spawnWith:`.

The class process runs the instantiation, and an actor's `initialize` may
take up to `?BT_INITIALIZE_TIMEOUT`. The caller's timer starts before the
class process's, so under the default 5000 ms call timeout a slow
`initialize` reached the caller as a raw `gen_server:call` exit before the
class process could reply with its structured timeout error. With no
caller timeout the call therefore waits the init timeout plus a margin;
an explicit one is honoured. A call timeout is translated as
`class_method_call/4` does (`raise_class_timeout/3`).
""".
-spec instantiation_call(pid(), term(), selector(), call_timeout()) -> term().
instantiation_call(ClassPid, Request, Selector, Timeout0) ->
    Timeout = instantiation_call_timeout(Timeout0),
    try
        gen_server:call(ClassPid, Request, Timeout)
    catch
        exit:{timeout, {gen_server, call, [ClassPid | _]}} ->
            raise_class_timeout(ClassPid, Selector, Timeout)
    end.

-spec instantiation_call_timeout(call_timeout()) -> timeout().
instantiation_call_timeout(default) -> ?BT_INITIALIZE_TIMEOUT + ?BT_INSTANTIATION_CALL_MARGIN;
instantiation_call_timeout(Timeout) -> Timeout.

-doc """
Raise the structured `timeout` error for a class method the class process did
not answer within `Timeout` milliseconds (BT-4556).
""".
-spec raise_class_timeout(pid(), selector(), timeout()) -> no_return().
raise_class_timeout(ClassPid, Selector, Timeout) ->
    %% The registry's pid index only: `class_name_safe/1` falls back to a
    %% `gen_server:call` into the class process, which has just shown it is
    %% busy.
    ClassName =
        case beamtalk_class_registry:class_name_for_pid(ClassPid) of
            {ok, Name} -> Name;
            not_found -> unknown
        end,
    Error = beamtalk_error:new(
        timeout,
        ClassName,
        Selector,
        <<"The class method may legitimately take longer: send it with a longer timeout">>
    ),
    Message = beamtalk_error:chars_to_binary(
        io_lib:format("Class process did not answer within ~tp ms", [Timeout])
    ),
    Error1 = beamtalk_error:with_message(Error, Message),
    beamtalk_exception_handler:reraise(Error1).

-doc """
The default bound on a class-process `class_method_call` (BT-4556 exposes an
override through `class_send/4`). Class methods can do arbitrary I/O (HTTP,
file, database), so the default must accommodate network latency; test
selectors get 5 minutes because a full suite can run hundreds of tests
sequentially.
""".
-spec class_method_call_timeout(selector()) -> timeout().
class_method_call_timeout(Selector) ->
    case is_test_execution_selector(Selector) of
        true -> 300000;
        false -> 60000
    end.

-doc """
Interpret a class method's reply (`{ok, Result}`, `{error, _}`), from the
class process or from `run_in_caller/7` (ADR 0142), as the send's result.
""".
-spec class_method_result(pid(), selector(), list(), term()) -> term().
class_method_result(ClassPid, Selector, Args, Reply) ->
    case Reply of
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
            class_chain_send(
                ClassPid,
                beamtalk_object_class_read:class_name_safe(ClassPid),
                beamtalk_object_class:module_name_safe(ClassPid),
                Selector,
                Args
            );
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
    beamtalk_class_dispatch_recovery:handle_metaclass_self_call(Selector, Args);
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
                    ClassName = beamtalk_object_class_read:class_name_safe(Pid),
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
            %% The block was away from home in the class process (BT-4536).
            beamtalk_actor_state:rethrow_from_away(Nlr);
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

A class method is read back by both `class_method_call` (an ordinary
`Target sel` send) and `metaclass_method_call` (`Target class sel`), which
both funnel through this same function
(`beamtalk_object_class:dispatch_class_method/5`). Past the local table, the
superclass chain is walked by `find_class_side_method/3`.

Scope: this covers *external* sends (`Target sel` / `Target class sel`). A
`self someSelector` send from inside another class method of the same
class — `class_self_send/3` / `class_self_dispatch_local/4`, above —
walks the same chain through `find_class_side_method/3`.

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
    case maps:is_key(Selector, LocalClassMethods) of
        true ->
            %% Local class method found — invoke directly.
            invoke_class_method(Selector, Args, ClassName, Module, ClassName, Module, ClassVars);
        false ->
            %% Not found locally — walk the superclass chain.
            case find_class_side_method(Selector, superclass_from_ets(ClassName), ClassName) of
                {method, DefiningClass, DefiningModule} ->
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
invoke_class_method(Selector, Args, ClassName, Module, DefiningClass, DefiningModule, ClassVars) ->
    %% The metaclass tag is derived once: it keys the class-variable home and
    %% is the `ClassSelf` tag the method receives.
    ClassTag = receiver_tag(ClassName),
    %% BT-4300: `self()` is the receiving class's own gen_server here
    %% (`handle_class_method_call/6` runs only inside it), so this is the
    %% class's real reference; no path that may run elsewhere builds one from
    %% `self()`.
    ClassSelf = #beamtalk_object{class = ClassTag, class_mod = Module, pid = self()},
    run_with_class_vars(ClassTag, ClassVars, fun() ->
        apply_class_method_in_context(
            Selector, Args, ClassName, ClassSelf, DefiningClass, DefiningModule
        )
    end).

-doc """
Install `ClassVars` as `ClassName`'s home, run `Thunk` (which yields a
`class_method_outcome()`), and adapt the outcome to the gen_server reply shape
(ADR 0130 §1). Shared by `invoke_class_method/7`.
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
        {ok, _} = Outcome ->
            {reply, outcome_reply(Outcome), erlang:get(Key)};
        {nlr_relay, _, _} = Outcome ->
            %% ADR 0110/0130: a foreign `^` is control flow, not a failure;
            %% writes made before the unwind are kept.
            {reply, outcome_reply(Outcome), erlang:get(Key)};
        {error, _} = Outcome ->
            {reply, outcome_reply(Outcome), ClassVars}
    after
        beamtalk_class_vars:uninstall(Key)
    end.

-doc """
The reply a class method's outcome becomes (`{ok, Result}` or `{error, _}`),
whatever process ran it: the class process's `run_with_class_vars/3` and the
in-caller `run_in_caller/7` (ADR 0142) share it, and `class_method_result/4`
interprets it, so a method raises the same value on either path. A foreign `^`
keeps its `{error, Nlr}` shape so the `?IS_NLR` re-throw in
`class_method_result/4` and `metaclass_send_dispatch/4` sees it.
""".
-spec outcome_reply(
    {ok, term()}
    | {nlr_relay, term(), list()}
    | {error, #beamtalk_error{} | undef_in_body | {raised, atom(), term(), list()}}
) -> {ok, term()} | {error, term()}.
outcome_reply({ok, _} = Reply) ->
    Reply;
outcome_reply({nlr_relay, Nlr, _ST}) ->
    {error, Nlr};
outcome_reply({error, #beamtalk_error{}} = Reply) ->
    Reply;
outcome_reply({error, undef_in_body}) ->
    {error, undef};
%% An `error` carries the class process's own stacktrace across the relay inside
%% the wrapped exception (`raw_stacktrace`), so the caller's trace shows where
%% it happened (BT-4829); an exception that already has one keeps it. `exit` and
%% `throw` keep their raw reason, which the caller wraps as before.
outcome_reply({error, {raised, error, Error, ST}}) ->
    {error, beamtalk_exception_handler:ensure_wrapped(error, Error, ST)};
outcome_reply({error, {raised, _ErrClass, Error, _ST}}) ->
    {error, Error}.

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

Handles the TestCase `test_spawn` guard, applies the method to `ClassSelf`
(the receiving class's real reference, built by the caller: never from
`self()` unless the caller is the class's own gen_server), and classifies any
errors into the structured variants in `class_method_outcome()`. Both
`invoke_class_method/7` (gen_server path) and `class_self_dispatch_chain/5`
(self-send path) adapt the outcome to their own caller shapes.

`{ok, Raw}` carries the method's bare result (ADR 0130 §3).
Stacktraces are preserved in the `{raised, ...}` variant so callers
that need to re-raise (e.g. self-dispatch) get the original trace.
""".
-spec apply_class_method_in_context(
    selector(), list(), class_name(), #beamtalk_object{}, class_name(), atom()
) -> class_method_outcome().
apply_class_method_in_context(
    Selector, Args, ClassName, ClassSelf, DefiningClass, DefiningModule
) ->
    %% For test execution (runAll, run:) inherited from TestCase,
    %% return a spawn request so the caller (gen_server) can handle noreply.
    %% The self-dispatch caller translates this to a structured error.
    case is_test_execution_selector(Selector) andalso DefiningClass =:= 'TestCase' of
        true ->
            test_spawn;
        false ->
            apply_class_method_body(
                ClassSelf,
                Args,
                ClassName,
                DefiningClass,
                DefiningModule,
                Selector,
                class_method_fun_name(Selector)
            )
    end.

-doc """
Apply the method `DefiningClass` defines for `Selector` to `ClassSelf`: its
ADR 0084 runtime fun when one is installed, else the compiled `FunName`
export. Shared by `apply_class_method_in_context/6` and the in-caller path
(`run_in_caller/7`, ADR 0142).
""".
-spec apply_class_method_body(
    #beamtalk_object{}, list(), class_name(), class_name(), atom(), selector(), atom()
) -> class_method_outcome().
apply_class_method_body(
    ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector, FunName
) ->
    case class_method_body(DefiningClass, Selector) of
        {runtime_fun, Fun} ->
            apply_class_method_fun(Fun, ClassSelf, Args, ClassName, Selector);
        compiled ->
            apply_compiled_class_method(
                ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector, FunName
            )
    end.

-doc """
Which body a class-side `Selector` resolved to `DefiningClass` runs: its ADR
0084 runtime-installed fun, which shadows the compiled export, or the
compiled export. The retrieval store is keyed by the resolved
`DefiningClass`, so this works identically for own and inherited methods,
and is gated so compile-time-only classes pay no extra read on the dispatch
hot path. The one rule behind `apply_class_method_body/7` and
`beamtalk_class_shadow_flags`' static call keys (BT-4270), which name a
compiled function only while this answers `compiled`.
""".
-spec class_method_body(class_name(), selector()) -> {runtime_fun, fun()} | compiled.
class_method_body(DefiningClass, Selector) ->
    case beamtalk_class_metadata:lookup_class_method_fun(DefiningClass, Selector) of
        {ok, #{block := Fun}} -> {runtime_fun, Fun};
        error -> compiled
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
            log_relayed_raise(
                "Runtime class method ~p:~p failed", ClassName, Selector, ErrClass, Error, ErrST
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-doc """
Apply a compiled `class_<Selector>` export, `FunName`
(`class_method_fun_name/1` of `Selector`).
""".
-spec apply_compiled_class_method(
    #beamtalk_object{}, list(), class_name(), class_name(), atom(), selector(), atom()
) -> class_method_outcome().
apply_compiled_class_method(
    ClassSelf, Args, ClassName, DefiningClass, DefiningModule, Selector, FunName
) ->
    %% ADR 0130 §3: `class_<sel>(ClassSelf, Args...)`; class variables live in
    %% the process dictionary home installed by the invocation entry point.
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
            log_relayed_raise(
                "Class method ~p:~p failed", ClassName, Selector, ErrClass, Error, ErrST
            ),
            {error, {raised, ErrClass, Error, ErrST}}
    end.

-doc """
Log a class method's raise before it is relayed to the caller as
`{raised, ...}` (BT-4115).

A deliberate Beamtalk signal (`is_beamtalk_signal/2`) is logged at debug: the
calling process re-raises it and owns reporting it, and `?LOG_DEBUG`'s level
check skips building an event for every caught `Error signal:`. Anything else
(a runtime error such as does_not_understand or type_error, badarith,
badmatch, function_clause, a failed FFI call, a raw throw or exit) is a failure
inside the class method and is logged at error with its
stacktrace: the relay re-raises with the caller's stacktrace, so this event is
the only record of where in the class method it failed.
""".
-spec log_relayed_raise(string(), class_name(), selector(), atom(), term(), list()) -> ok.
log_relayed_raise(Format, ClassName, Selector, ErrClass, Error, ErrST) ->
    case is_beamtalk_signal(ErrClass, Error) of
        true ->
            ?LOG_DEBUG(
                Format,
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            );
        false ->
            ?LOG_ERROR(
                Format,
                [ClassName, Selector],
                #{
                    class => ClassName,
                    selector => Selector,
                    reason => beamtalk_error:format_reason(ErrClass, Error),
                    stacktrace => ErrST,
                    domain => [beamtalk, runtime]
                }
            )
    end,
    ok.

-doc """
Is this raise a deliberate Beamtalk signal? Only an `error` carrying the marker
set by `beamtalk_exception_handler:raise_signal/1` (see
`beamtalk_exception_handler:is_signal/1`) counts. A runtime error such as
does_not_understand or type_error has the same wrapped exception shape but no
marker, so it logs at error.
""".
-spec is_beamtalk_signal(atom(), term()) -> boolean().
is_beamtalk_signal(error, Error) -> beamtalk_exception_handler:is_signal(Error);
is_beamtalk_signal(_ErrClass, _Error) -> false.

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
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Class '~ts' is not registered - the module '~ts' may have failed to load",
                    [ClassName, DefiningModule]
                )
            );
        false ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Class '~ts' inherits this method from '~ts' - that class's module '~ts' may have failed to load",
                    [ClassName, DefiningClass, DefiningModule]
                )
            )
    end.

-spec method_not_found_hint(class_name(), class_name(), selector()) -> binary().
method_not_found_hint(ClassName, DefiningClass, Selector) ->
    case ClassName =:= DefiningClass of
        true ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Class method '~ts' not found on '~ts' (arity mismatch or undefined selector)",
                    [Selector, ClassName]
                )
            );
        false ->
            beamtalk_error:chars_to_binary(
                io_lib:format(
                    "Class method '~ts' inherited from '~ts' not found on '~ts' (arity mismatch or undefined selector)",
                    [Selector, DefiningClass, ClassName]
                )
            )
    end.

-doc """
Walk the class-method tables from `ClassName` itself up the superclass chain:
`{ok, DefiningClass, Module}` for the first
class whose published selector list (compiled methods and ADR 0084 runtime
funs) has `Selector`. Used by
`beamtalk_object_class:local_call/3`, which applies only compiled exports.
A thin adaptation of `find_class_side_method/3`, the one class-side lookup.

Uses the ETS hierarchy and class-methods tables only (no `gen_server:call`),
so it is safe from inside a class's own `handle_call`. The walk itself (depth
guard, advance-to-superclass) is `beamtalk_hierarchy:walk_or_warn/4`.
""".
-spec find_class_method_from_class(selector(), class_name()) ->
    {ok, class_name(), atom()} | not_found.
find_class_method_from_class(Selector, ClassName) ->
    case find_class_side_method(Selector, ClassName, ClassName) of
        {method, DefiningClass, Module} -> {ok, DefiningClass, Module};
        not_found -> not_found
    end.

-doc "Read the superclass name from the hierarchy table (no gen_server call).".
-spec superclass_from_ets(class_name()) -> class_name() | none.
superclass_from_ets(ClassName) ->
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        {ok, Super} -> Super;
        not_found -> none
    end.

-doc """
Is `Receiver` a class object whose own class side understands `Selector`
(`class_understands_class_selector/2`)? Answers `false` for any other value,
including a class object that only reaches `Selector` through the generic
`Class`/`Behaviour`/`Object` protocol.

BT-4075: guards the `error:` intrinsic. `Object>>error:` raises, but a class
object with a class-side `error:` (`Result class>>error:`, `Logger
class>>error:`) must receive the send instead, so codegen asks this before
raising when the receiver is not statically known to be an instance.
""".
-spec class_object_understands(term(), selector()) -> boolean().
class_object_understands(#beamtalk_object{class = Tag} = Receiver, Selector) ->
    beamtalk_class_registry:is_class_object(Receiver) andalso
        class_understands_class_selector(beamtalk_primitive:class_name_from_tag(Tag), Selector);
class_object_understands(_Receiver, _Selector) ->
    false.

-doc """
Test whether `ClassName`'s own metaclass tag understands `Selector`
— a local or inherited class method, or a local runtime-installed (ADR 0084) class
method.

Backs `beamtalk_object_ops:responds_to_result/3`'s `respondsTo:` on a
class-object receiver, and `class_chain_only_send/3`'s decision to skip the
class process (BT-4073), so a `false` here must mean the class process's own
lookup would answer `{error, not_found}`. `SomeClass respondsTo: #sel` previously always
answered against the generic `'Class'` protocol only, because a
class's metaclass tag (`'SomeClass class'`) is virtual — never registered
as its own class in `beamtalk_class_registry`/`beamtalk_class_metadata` —
so the existing hierarchy-walk reflection helper
(`beamtalk_behaviour_intrinsics:classCanUnderstandFromName/2`) cannot be
pointed at it directly: there is no such class row to walk from. This
function instead re-implements "does dispatch find this selector" directly
against `ClassName`'s own tables, mirroring `handle_class_method_call/6`'s
priority order (local, then the inherited chain walked by
`find_class_side_method/3`) without actually invoking anything.

Local and inherited class methods are checked against `ClassName` itself (the real registry entry) since class methods
belong to the class, not a separate metaclass registry row. Returns `false`
— not an error — for an unregistered `ClassName`, since every per-class
lookup this function makes already degrades to `not_found`/`error` for a
missing class; the generic-protocol fallback the caller applies next still
answers correctly (`Class`/`Behaviour`/`Object` messages are independent of
whether `ClassName` itself is currently registered).
""".
-spec class_understands_class_selector(class_name(), selector()) -> boolean().
class_understands_class_selector(ClassName, Selector) ->
    has_local_class_method(ClassName, Selector) orelse
        (find_class_side_method(Selector, superclass_from_ets(ClassName), ClassName) =/=
            not_found).

-doc """
Does `ClassName` itself (not an ancestor) define `Selector` as a
class method — either a compiled/static one (`lookup_methods/1`'s local
selector list) or a runtime-installed one (ADR 0084,
`lookup_class_method_fun/2`)? Mirrors the same two lookups
`class_self_dispatch_local/4` already checks for a self-send, so
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
`safe_class_method_fn_name` (`crates/beamtalk-codegen/src/core_erlang/selector_mangler.rs`)
byte-for-byte — same 255-byte threshold on `"class_" ++ Selector`, same
FNV-1a 64-bit hash, same `"class_kw_<16-hex-digits>"` naming scheme — so a
selector that is long at compile time and the *same* selector text
constructed at runtime via `perform:` produce the identical atom.
""".
-spec class_method_fun_name(selector()) -> atom().
class_method_fun_name(Selector) ->
    %% BT-4771: memoised per selector in `persistent_term` (the `class_` name is
    %% a pure function of the selector, so a stale or racing entry is never
    %% wrong). Saves an `atom_to_binary` + `binary_to_atom` round trip on every
    %% class send. Adding a key never triggers a global GC.
    Key = {?MODULE, class_method_fun_name, Selector},
    case persistent_term:get(Key, undefined) of
        undefined ->
            FunName = derive_class_method_fun_name(Selector),
            persistent_term:put(Key, FunName),
            FunName;
        FunName ->
            FunName
    end.

-spec derive_class_method_fun_name(selector()) -> atom().
derive_class_method_fun_name(Selector) ->
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
%% `fnv1a_64` in `crates/beamtalk-cerl-doc/src/hash.rs` (same offset basis,
%% same prime, same 64-bit wraparound). No external dependency; ~20 lines is
%% not worth pulling in a hashing library for.
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
    try
        Action(ClassPid)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            beamtalk_class_dispatch_recovery:handle_class_crash_recovery(
                ClassPid, Selector, Action
            );
        exit:{noproc, _} ->
            beamtalk_class_dispatch_recovery:handle_class_crash_recovery(ClassPid, Selector, Action)
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
    case beamtalk_object_class:is_class_var_reflection_selector(Selector) of
        true ->
            %% ADR 0153 decision 7 (BT-5155): a class object has no fields, so
            %% the instance-field reflection selectors are not part of its
            %% protocol, although `Object` (an ancestor of `Class`) defines
            %% them for instances. Not found here means does-not-understand on
            %% every route, matching `dispatch_class_method/5`.
            not_found;
        false ->
            class_chain_lookup(ClassSelf, Selector, Args)
    end.

-spec class_chain_lookup(#beamtalk_object{}, atom(), list()) -> {ok, term()} | not_found.
class_chain_lookup(ClassSelf, Selector, Args) ->
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
