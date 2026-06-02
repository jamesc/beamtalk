%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_class).
-behaviour(gen_server).

%%% **DDD Context:** Object System Context

-moduledoc """
Per-class gen_server for Beamtalk class objects.

Each class in Beamtalk is a first-class object backed by a gen_server
process. Follows Smalltalk philosophy where classes are messageable objects.

Class method dispatch is handled by beamtalk_class_dispatch.
Instance creation (new/spawn) by beamtalk_class_instantiation.
Method lookup walks the class chain directly via has_method/2 + superclass/1.

## Registration

Classes register via Erlang's built-in registry (beamtalk_class_Counter)
and join the `beamtalk_classes` pg group for enumeration.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start/2,
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    methods/1,
    superclass/1,
    method/2,
    has_method/2,
    put_method/3,
    put_method/4,
    put_class_method/3,
    put_class_method/4,
    instance_variables/1,
    is_sealed/1,
    is_abstract/1,
    is_internal/1,
    is_constructible/1,
    class_name/1,
    module_name/1,
    class_send/3,
    local_call/3,
    set_class_var/3,
    update_class/2,
    local_class_methods/1,
    local_class_methods_map/1,
    local_instance_methods/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type class_name() :: atom().
-type selector() :: atom().
%% ADR 0068: Runtime representation of types in method_info metadata.
%% Atoms for concrete types, tagged tuples for type parameters and generic types.
-type meta_type_repr() ::
    atom()
    | {type_param, Name :: atom(), Index :: integer()}
    | {generic, Base :: atom(), Params :: [meta_type_repr()]}.
-type method_info() :: #{
    arity => non_neg_integer(),
    block => fun(),
    is_sealed => boolean()
}.

-record(class_state, {
    name :: class_name(),
    module :: atom(),
    superclass :: class_name() | none,
    is_sealed = false :: boolean(),
    is_abstract = false :: boolean(),
    is_constructible = undefined :: boolean() | undefined,
    instance_methods = #{} :: #{selector() => method_info()},
    class_methods = #{} :: #{selector() => method_info()},
    fields = [] :: [atom()],
    %% BT-2275: Field name => default value map for module-less (builder-built)
    %% classes. Used by the generic instantiation path to construct an instance
    %% map when no compiled module exists. Compiled classes leave this empty and
    %% bake defaults into their generated `new/0`.
    field_defaults = #{} :: #{atom() => term()},
    class_state = #{} :: map(),
    method_source = #{} :: #{selector() => binary()},
    %% BT-2195: Class-side method source. Symmetric companion to
    %% method_source, used by SystemNavigation source-text scanners to walk
    %% class-side bodies (`sendersOf:`, `referencesTo:`, `methodsMatching:`).
    class_method_source = #{} :: #{selector() => binary()},
    %% BT-988: Method display signatures for :help command
    method_signatures = #{} :: #{selector() => binary()},
    %% BT-990: Class-side method display signatures for :help command
    class_method_signatures = #{} :: #{selector() => binary()},
    %% BT-1002 / ADR 0045: Machine-readable return-type map for chain resolution.
    %% ADR 0068: Values may be atoms (concrete types) or tagged tuples:
    %%   {type_param, Name :: atom(), Index :: integer()}
    %%   {generic, Base :: atom(), Params :: [meta_type_repr()]}
    method_return_types = #{} :: #{selector() => meta_type_repr()},
    class_method_return_types = #{} :: #{selector() => meta_type_repr()},
    %% ADR 0033: Runtime-embedded documentation
    doc = none :: binary() | none,
    method_docs = #{} :: #{selector() => binary()},
    %% BT-1634: Class method doc comments
    class_method_docs = #{} :: #{selector() => binary()},
    %% ADR 0071 Phase 5: Class visibility (internal vs public)
    is_internal = false :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

-doc "Start a class process (unlinked) for on_load registration.".
-spec start(class_name(), map()) -> {ok, pid()} | {error, term()}.
start(ClassName, ClassInfo) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    gen_server:start({local, RegName}, ?MODULE, {ClassName, ClassInfo}, []).

-doc "Start a class process with full options.".
-spec start_link(class_name(), map()) -> {ok, pid()} | {error, term()}.
start_link(ClassName, ClassInfo) ->
    RegName = beamtalk_class_registry:registry_name(ClassName),
    gen_server:start_link({local, RegName}, ?MODULE, {ClassName, ClassInfo}, []).

-doc "Start a class process with minimal info (for testing).".
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(ClassInfo) ->
    ClassName = maps:get(name, ClassInfo),
    start_link(ClassName, ClassInfo).

-doc "Update an existing class process with new metadata after redefinition.".
-spec update_class(class_name(), map()) -> {ok, [atom()]} | {error, term()}.
update_class(ClassName, ClassInfo) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            {error, {class_not_found, ClassName}};
        Pid ->
            gen_server:call(Pid, {update_class, ClassInfo})
    end.

-doc "Set a class variable on a class by name.".
-spec set_class_var(class_name(), atom(), term()) -> term().
set_class_var(ClassName, Name, Value) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Error0 = beamtalk_error:new(class_not_found, ClassName),
            error(Error0);
        Pid ->
            gen_server:call(Pid, {set_class_var, Name, Value})
    end.

-doc "Create a new instance of this class.".
-spec new(pid()) -> {ok, #beamtalk_object{}} | {error, term()}.
new(ClassPid) ->
    new(ClassPid, []).

-doc "Create a new instance with initialization arguments.".
-spec new(pid(), list()) -> {ok, #beamtalk_object{}} | {error, term()}.
new(ClassPid, Args) ->
    gen_server:call(ClassPid, {new, Args}).

-doc "Get all method selectors.".
-spec methods(pid()) -> [selector()].
methods(ClassPid) ->
    gen_server:call(ClassPid, methods).

-doc "Get local class-side method selectors (not inherited).".
-spec local_class_methods(pid()) -> [selector()].
local_class_methods(ClassPid) ->
    maps:keys(local_class_methods_map(ClassPid)).

-doc "Get the full local class-side methods map (selector => info).".
-spec local_class_methods_map(pid()) -> #{selector() => map()}.
local_class_methods_map(ClassPid) ->
    gen_server:call(ClassPid, get_local_class_methods).

-doc "Get local instance-side method selectors (not inherited).".
-spec local_instance_methods(pid()) -> [selector()].
local_instance_methods(ClassPid) ->
    {ok, InstanceMethods} = gen_server:call(ClassPid, get_instance_methods),
    maps:keys(InstanceMethods).

-doc "Get the superclass name.".
-spec superclass(pid()) -> class_name() | none.
superclass(ClassPid) ->
    gen_server:call(ClassPid, superclass).

-doc """
Get the class name.

BT-893: If called from within the class gen_server itself (self-call), read
from the process dictionary instead of gen_server:call to avoid calling_self.
""".
-spec class_name(pid()) -> class_name().
class_name(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_name);
class_name(ClassPid) ->
    gen_server:call(ClassPid, class_name).

-doc """
Get the module name.

BT-893: If called from within the class gen_server itself (self-call), read
from the process dictionary instead of gen_server:call to avoid calling_self.
""".
-spec module_name(pid()) -> atom().
module_name(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_module);
module_name(ClassPid) ->
    gen_server:call(ClassPid, module_name).

-doc """
Send a message to a class object synchronously (BT-246 / ADR 0013 Phase 1).

Delegates to beamtalk_class_dispatch (BT-704).
""".
-spec class_send(pid() | undefined, atom(), list()) -> term().
class_send(ClassPid, Selector, Args) ->
    beamtalk_class_dispatch:class_send(ClassPid, Selector, Args).

-doc """
Execute a class method in the caller's process (BT-1664).

Resolves the target module from the class object, then calls
Module:class_<Selector>(nil, #{}, Args) directly — bypassing the class
object's gen_server. The caller takes responsibility for knowing the
method does not mutate class state (nil is passed for ClassSelf).

Raises beamtalk_error if the receiver is not a class object, or if the
class does not define the requested method.
""".
-spec local_call(term(), atom(), list()) -> term().
local_call(Receiver = #beamtalk_object{class_mod = Module}, Selector, Args) when
    is_atom(Selector), is_list(Args)
->
    case beamtalk_class_registry:is_class_object(Receiver) of
        true ->
            FunName = beamtalk_class_dispatch:class_method_fun_name(Selector),
            code:ensure_loaded(Module),
            case erlang:function_exported(Module, FunName, length(Args) + 2) of
                true ->
                    case erlang:apply(Module, FunName, [nil, #{} | Args]) of
                        {class_var_result, Value, _NewClassVars} ->
                            %% Discard class var mutations — local_call does not
                            %% update the class object's state.
                            Value;
                        Result ->
                            Result
                    end;
                false ->
                    ClassName = Receiver#beamtalk_object.class,
                    Error = beamtalk_error:new(
                        does_not_understand,
                        ClassName,
                        Selector,
                        <<"Class does not understand this message">>
                    ),
                    beamtalk_error:raise(Error)
            end;
        false ->
            ClassName = Receiver#beamtalk_object.class,
            Error = beamtalk_error:new(
                type_error,
                ClassName,
                'performLocally:withArguments:',
                <<"Receiver is not a class object">>
            ),
            beamtalk_error:raise(Error)
    end;
local_call(Receiver, _Selector, _Args) ->
    ClassName =
        case Receiver of
            #beamtalk_object{class = C} -> C;
            _ -> erlang
        end,
    Error = beamtalk_error:new(
        type_error,
        ClassName,
        'performLocally:withArguments:',
        <<"Receiver is not a class object">>
    ),
    beamtalk_error:raise(Error).

-doc """
Get a compiled method object.
Delegates to beamtalk_method_resolver.
""".
-spec method(pid() | class_name() | tuple(), selector()) -> compiled_method() | 'nil'.
method(ClassRef, Selector) ->
    beamtalk_method_resolver:resolve(ClassRef, Selector).

-doc "Check if a class is internal (ADR 0071).".
-spec is_internal(pid()) -> boolean().
is_internal(ClassPid) ->
    gen_server:call(ClassPid, is_internal).

-doc """
Check if a class has a method (does not walk hierarchy).
First checks the gen_server state (instance_methods map for compiled methods),
then falls back to the module's has_method/1. The fallback is needed for
abstract/value-type classes (e.g., Behaviour) where @primitive methods appear
in the compiled module but may not be tracked in the gen_server instance_methods.
""".
-spec has_method(pid(), selector()) -> boolean().
has_method(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            case module_name(ClassPid) of
                undefined ->
                    false;
                ModuleName ->
                    case erlang:function_exported(ModuleName, has_method, 1) of
                        true ->
                            try
                                ModuleName:has_method(Selector)
                            catch
                                Kind:Reason:ST ->
                                    ?LOG_DEBUG(
                                        "has_method check failed for ~p:~p: ~p:~p",
                                        [ModuleName, Selector, Kind, Reason],
                                        #{stacktrace => ST, domain => [beamtalk, runtime]}
                                    ),
                                    false
                            end;
                        false ->
                            false
                    end
            end;
        _MethodInfo ->
            true
    end.

-doc "Replace a method with a new function (hot patching).".
-spec put_method(pid(), selector(), fun()) -> ok.
put_method(ClassPid, Selector, Fun) ->
    put_method(ClassPid, Selector, Fun, <<"">>).

-doc "Replace a method with source.".
-spec put_method(pid(), selector(), fun(), binary()) -> ok.
put_method(ClassPid, Selector, Fun, Source) ->
    gen_server:call(ClassPid, {put_method, Selector, Fun, Source}).

-doc """
Install or replace a class-side method with a runtime fun (ADR 0084, BT-2266).

Class-side mirror of `put_method/4`. The fun follows the compiled class-method
calling convention exactly: `fun(ClassSelf, ClassVars, A1..An) -> Result |
{class_var_result, Result, NewClassVars}`, arity `n + 2`. The fun is stored in
the class gen_server `class_methods` map (the source of truth) and mirrored into
the metadata retrieval store so subclasses can dispatch it without a gen_server
hop. A runtime fun shadows any compiled method of the same selector.
""".
-spec put_class_method(pid(), selector(), fun()) -> ok.
put_class_method(ClassPid, Selector, Fun) ->
    put_class_method(ClassPid, Selector, Fun, <<"">>).

-doc "Install or replace a class-side method with a runtime fun and source.".
-spec put_class_method(pid(), selector(), fun(), binary()) -> ok.
put_class_method(ClassPid, Selector, Fun, Source) ->
    gen_server:call(ClassPid, {put_class_method, Selector, Fun, Source}).

-doc "Get instance variable names.".
-spec instance_variables(pid()) -> [atom()].
instance_variables(ClassPid) ->
    gen_server:call(ClassPid, instance_variables).

-doc "Check if a class is sealed (cannot be subclassed).".
-spec is_sealed(pid()) -> boolean().
is_sealed(ClassPid) ->
    gen_server:call(ClassPid, is_sealed).

-doc "Check if a class is abstract (cannot be instantiated).".
-spec is_abstract(pid()) -> boolean().
is_abstract(ClassPid) ->
    gen_server:call(ClassPid, is_abstract).

-doc """
Check if a class can be instantiated via new/new:.
BT-474: Returns false for actors (use spawn) and sealed primitives.
""".
-spec is_constructible(pid()) -> boolean().
is_constructible(ClassPid) ->
    gen_server:call(ClassPid, is_constructible).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({ClassName, ClassInfo}) ->
    beamtalk_class_registry:ensure_pg_started(),
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_methods_table(),
    beamtalk_class_registry:ensure_pid_table(),
    ok = pg:join(beamtalk_classes, self()),

    Superclass = maps:get(superclass, ClassInfo, none),
    Module = maps:get(module, ClassInfo, ClassName),

    %% ADR 0050 Phase 5: Use meta from ClassInfo when available (set by the compiler
    %% in BuilderState.meta). During on_load, erlang:function_exported/3 returns false,
    %% making Module:'__beamtalk_meta'() unavailable via read_meta. The meta map literal
    %% in BuilderState provides the same data without requiring a function call.
    %% Falls back to read_meta for hot reload (apply_class_info) and dynamic classes.
    Meta =
        case maps:get(meta, ClassInfo, undefined) of
            undefined -> read_meta(Module);
            M when is_map(M) -> M
        end,
    IsAbstract = maps:get(is_abstract, Meta, maps:get(is_abstract, ClassInfo, false)),
    %% ADR 0071 Phase 5: Read visibility from meta (the codegen emits 'visibility' => public|internal).
    %% Also support 'is_internal' boolean for direct ClassInfo usage.
    IsInternal =
        case maps:get(visibility, Meta, maps:get(visibility, ClassInfo, public)) of
            internal -> true;
            _ -> maps:get(is_internal, Meta, maps:get(is_internal, ClassInfo, false))
        end,
    {InstanceMethods, MethodReturnTypes} = meta_to_methods(
        maps:get(method_info, Meta, undefined),
        maps:get(instance_methods, ClassInfo, #{})
    ),
    {ClassMethods, ClassMethodReturnTypes} = meta_to_methods(
        maps:get(class_method_info, Meta, undefined),
        maps:get(class_methods, ClassInfo, #{})
    ),

    %% BT-2222: One unified metadata row carries hierarchy + module + class-method
    %% selectors. The module name enables deadlock-free lookup during supervisor
    %% init (BT-1285); the selector set lets the inherited class-method chain walk
    %% in beamtalk_class_dispatch avoid gen_server hops (BT-2008).
    beamtalk_class_metadata:insert(ClassName, Module, maps:keys(ClassMethods), Superclass),

    %% ADR 0084 / BT-2266: Seed the runtime class-method fun retrieval store for
    %% any builder-supplied class methods that arrived as funs (#{block, arity}).
    %% Compile-time-only classes seed nothing and keep the gate flag false, so
    %% their dispatch never reads the funs table.
    seed_runtime_class_methods(ClassName, ClassMethods),

    %% BT-893: Store class metadata in process dictionary so class_send can
    %% bypass gen_server for self-calls (new/spawn from within class methods).
    put(beamtalk_class_name, ClassName),
    put(beamtalk_class_module, Module),
    put(beamtalk_class_is_abstract, IsAbstract),
    %% BT-2275: Cache field defaults in the process dictionary so the `self new`
    %% self-instantiation path (which runs inside this gen_server) can build a
    %% generic instance for a module-less class without a gen_server:call to self.
    FieldDefaults = maps:get(field_defaults, ClassInfo, #{}),
    put(beamtalk_class_field_defaults, FieldDefaults),
    %% BT-2277: Cache the local instance-method map and superclass so fun-backed
    %% instance dispatch that happens *inside* this gen_server (e.g. a class method
    %% does `Inst := self new` then `Inst someMethod`) can resolve the block fun
    %% without a deadlocking `gen_server:call(self(), ...)`. The cache is the local
    %% (non-inherited) methods only; inherited methods live in ancestor processes
    %% and are reachable without deadlock. Kept in sync by the {put_method, ...}
    %% handler so hot-patched methods stay visible to self-dispatch.
    put(beamtalk_class_instance_methods, InstanceMethods),
    put(beamtalk_class_superclass, Superclass),

    %% BT-1768: Record pid→classname mapping for crash recovery.
    %% This entry survives process death, allowing class_send to identify
    %% which class a dead pid belonged to and attempt auto-restart.
    beamtalk_class_registry:record_class_pid(self(), ClassName),

    %% ADR 0050 Phase 3: Notify compiler server of this class registration.
    %% Cast is fire-and-forget — silently dropped if the compiler server is not running.
    %% Use Meta map availability (not function_exported, which returns false during on_load).
    %% BT-1732: Wrap in try/catch so an undef crash (e.g., beamtalk_compiler_server
    %% not on code path) doesn't kill the class process during on_load.
    case Meta of
        CompilerMeta when is_map(CompilerMeta), map_size(CompilerMeta) > 0 ->
            try
                beamtalk_compiler_server:register_class(ClassName, CompilerMeta)
            catch
                error:undef ->
                    ?LOG_WARNING(#{
                        event => register_class_undef,
                        class => ClassName,
                        reason => "beamtalk_compiler_server:register_class/2 not available",
                        domain => [beamtalk, runtime]
                    }),
                    ok
            end;
        _ ->
            ok
    end,

    %% BT-877: is_constructible may be set by the compiler via ClassBuilder.
    %% If not set (undefined), it will be computed lazily on first new call.
    %% ADR 0032 Phase 1: No flattened method tables — dispatch walks the chain directly.
    State = #class_state{
        name = ClassName,
        module = Module,
        superclass = Superclass,
        is_sealed = maps:get(is_sealed, Meta, maps:get(is_sealed, ClassInfo, false)),
        is_abstract = IsAbstract,
        is_constructible = maps:get(is_constructible, ClassInfo, undefined),
        instance_methods = InstanceMethods,
        class_methods = ClassMethods,
        fields = maps:get(fields, Meta, maps:get(fields, ClassInfo, [])),
        field_defaults = FieldDefaults,
        class_state = maps:get(class_state, ClassInfo, #{}),
        method_source = maps:get(method_source, ClassInfo, #{}),
        class_method_source = maps:get(class_method_source, ClassInfo, #{}),
        method_signatures = maps:get(method_signatures, ClassInfo, #{}),
        class_method_signatures = maps:get(class_method_signatures, ClassInfo, #{}),
        %% Merge: ClassInfo explicit values (from old format) are lower priority than meta.
        method_return_types = maps:merge(
            maps:get(method_return_types, ClassInfo, #{}),
            MethodReturnTypes
        ),
        class_method_return_types = maps:merge(
            maps:get(class_method_return_types, ClassInfo, #{}),
            ClassMethodReturnTypes
        ),
        doc = maps:get(doc, ClassInfo, none),
        method_docs = maps:get(method_docs, ClassInfo, #{}),
        class_method_docs = maps:get(class_method_docs, ClassInfo, #{}),
        is_internal = IsInternal
    },

    %% ADR 0087 Phase 2 (BT-2298): Forward the per-method cross-reference index
    %% to beamtalk_xref synchronously, before init returns. Running inside init/1
    %% means the rows are registered before start/2 yields {ok, Pid}, so the
    %% class is never observable as "loaded" without its xref rows. The codegen
    %% bakes `method_xref` into the ClassInfo map (via BuilderState.methodXref);
    %% hand-coded stub classes that omit it default to [] for backward
    %% compatibility. A failure here propagates as a class-creation failure.
    MethodXref = maps:get(method_xref, ClassInfo, []),
    register_xref(ClassName, MethodXref),

    {ok, State}.

-doc """
Forward a class's per-method xref rows to `beamtalk_xref` (ADR 0087 Phase 2).

A no-op when `MethodXref` is empty (hand-coded stub classes, or classes
compiled before the codegen baked the field). The call is synchronous so the
index is populated before the class process finishes starting. `beamtalk_xref`
may legitimately be absent (e.g. minimal embedded runtimes, or during its own
supervisor restart), in which case we log at debug level and continue — the
miss-policy fallback (Phase 3) covers reads against an unpopulated index.
""".
-spec register_xref(class_name(), [map()]) -> ok.
register_xref(_ClassName, []) ->
    ok;
register_xref(ClassName, MethodXref) ->
    case erlang:whereis(beamtalk_xref) of
        undefined ->
            ?LOG_DEBUG(#{
                event => xref_not_running,
                class => ClassName,
                reason => "beamtalk_xref not registered; skipping index population",
                domain => [beamtalk, runtime]
            }),
            ok;
        _Pid ->
            ok = beamtalk_xref:register_class(ClassName, MethodXref)
    end.

-doc """
Refresh a class's xref rows on redefinition (ADR 0087 Phase 2, BT-2298).

Purges the class's existing rows before re-registering, because Phase 1's
`register_class/2` only inserts (the multi-generation sweep of stale rows lands
in Phase 4 / BT-2300). Purging first keeps the index consistent with the new
class definition — e.g. when a bootstrap stub's `unindexed_runtime_fun` rows are
replaced by the compiled class's `indexed` rows. A no-op when `beamtalk_xref`
is not running.
""".
-spec refresh_xref(class_name(), [map()]) -> ok.
refresh_xref(ClassName, MethodXref) ->
    case erlang:whereis(beamtalk_xref) of
        undefined ->
            ok;
        _Pid ->
            ok = beamtalk_xref:purge_class(ClassName),
            register_xref(ClassName, MethodXref)
    end.

handle_call(
    {spawn, Args},
    _From,
    #class_state{name = ClassName, module = Module, is_abstract = IsAbstract} = State
) ->
    {reply, beamtalk_class_instantiation:handle_spawn(Args, ClassName, Module, IsAbstract), State};
handle_call({new, Args}, _From, #class_state{name = ClassName, is_abstract = true} = State) ->
    Selector =
        case Args of
            [] -> 'new';
            _ -> 'new:'
        end,
    {reply, {error, beamtalk_class_instantiation:abstract_class_error(ClassName, Selector)}, State};
handle_call(
    {new, Args},
    _From,
    #class_state{
        name = ClassName,
        module = Module,
        is_constructible = IsConstructible0,
        class_methods = ClassMethods,
        class_state = ClassVars
    } = State
) ->
    %% When Args is non-empty and the class (or any ancestor) has a user-defined
    %% `class new:` method, route the `new:` call through dispatch instead of the
    %% generic field-initialisation constructor.
    %% This lets `List new: #(1,2,3)` call `List class new:` rather than the
    %% `Object.new:` dict-initialiser (which would error on a non-map argument).
    %%
    %% Note: the `classMethods` metadata includes auto-generated `new:` for all
    %% classes, so `is_map_key/2` alone cannot distinguish user-defined from
    %% auto-generated. We check `erlang:function_exported/3` on each module in the
    %% superclass chain, which is true only when the BEAM module contains an
    %% explicit `class_new:` function — correctly supporting inherited constructors.
    case Args =/= [] andalso has_class_new_in_chain(ClassName, Module) of
        true ->
            case
                beamtalk_class_dispatch:handle_class_method_call(
                    'new:', Args, ClassName, Module, ClassMethods, ClassVars
                )
            of
                {reply, Result, NewClassVars} ->
                    {reply, Result, State#class_state{class_state = NewClassVars}};
                {error, not_found} ->
                    Err = beamtalk_error:new(does_not_understand, ClassName, 'new:'),
                    {reply, {error, Err}, State}
            end;
        false ->
            case
                beamtalk_class_instantiation:handle_new(
                    Args,
                    ClassName,
                    Module,
                    IsConstructible0
                )
            of
                {ok, Result, IsConstructible} ->
                    {reply, {ok, Result}, State#class_state{is_constructible = IsConstructible}};
                {error, Error, IsConstructible} ->
                    {reply, {error, Error}, State#class_state{is_constructible = IsConstructible}}
            end
    end;
%% ADR 0032 Phase 1: Returns local (non-inherited) method selectors only.
%% Full hierarchy walk is done by Behaviour.methods in Phase 2.
handle_call(methods, _From, #class_state{instance_methods = Methods} = State) ->
    {reply, maps:keys(Methods), State};
handle_call(superclass, _From, #class_state{superclass = Super} = State) ->
    {reply, Super, State};
handle_call(class_name, _From, #class_state{name = Name} = State) ->
    {reply, Name, State};
handle_call(module_name, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};
handle_call(
    {method, Selector},
    _From,
    #class_state{
        instance_methods = Methods,
        method_source = Source,
        method_signatures = Signatures,
        method_docs = MethodDocs
    } = State
) ->
    Result =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(Selector, Methods) of
            {ok, MethodInfo} ->
                #{
                    '$beamtalk_class' => 'CompiledMethod',
                    '__selector__' => Selector,
                    '__source__' => maps:get(Selector, Source, <<"">>),
                    '__signature__' => maps:get(Selector, Signatures, nil),
                    '__method_info__' => MethodInfo,
                    '__doc__' => maps:get(Selector, MethodDocs, nil)
                };
            error ->
                nil
        end,
    {reply, Result, State};
%% BT-1002 / ADR 0045: Local return-type lookup (no chain walk — that is done in the registry).
handle_call(
    {get_method_return_type, Selector},
    _From,
    #class_state{method_return_types = MRT} = State
) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Selector, MRT) of
        {ok, Type} -> {reply, {ok, Type}, State};
        error -> {reply, not_found, State}
    end;
handle_call(
    {get_class_method_return_type, Selector},
    _From,
    #class_state{class_method_return_types = CMRT} = State
) ->
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(Selector, CMRT) of
        {ok, Type} -> {reply, {ok, Type}, State};
        error -> {reply, not_found, State}
    end;
%% BT-990: Return CompiledMethod-like map for class-side methods.
%% Walks superclass chain for inherited class methods (mirrors dispatch behaviour).
%% BT-2195: Populate __source__ from class_method_source so that
%% SystemNavigation source-text scanners (sendersOf:, referencesTo:,
%% methodsMatching:) can walk class-side method bodies. Empty binary when no
%% source has been registered (e.g. dynamic methods, primitives).
handle_call(
    {class_method, Selector},
    _From,
    #class_state{
        superclass = Superclass,
        class_methods = ClassMethods,
        class_method_source = ClassMethodSource,
        class_method_signatures = ClassMethodSigs,
        class_method_docs = ClassMethodDocs
    } = State
) ->
    Result =
        case maps:find(Selector, ClassMethods) of
            {ok, MethodInfo} ->
                #{
                    '$beamtalk_class' => 'CompiledMethod',
                    '__selector__' => Selector,
                    '__source__' => maps:get(Selector, ClassMethodSource, <<"">>),
                    '__signature__' => maps:get(Selector, ClassMethodSigs, nil),
                    '__method_info__' => MethodInfo,
                    '__doc__' => maps:get(Selector, ClassMethodDocs, nil)
                };
            error ->
                %% Not found locally — walk superclass chain
                find_inherited_class_method(Selector, Superclass)
        end,
    {reply, Result, State};
handle_call({put_method, Selector, Fun, Source}, _From, State) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    MethodInfo = #{block => Fun, arity => Arity},
    %% ADR 0032 Phase 1: No flattened table to rebuild or invalidate.
    %% Dispatch finds the new method via chain walk on the next call.
    %% BT-988: Remove stale display signature — dynamically-defined methods
    %% have no AST, so the fallback to selector atom is correct.
    %% BT-1002: Also clear stale return-type entry for the same reason.
    NewInstanceMethods = maps:put(Selector, MethodInfo, State#class_state.instance_methods),
    %% BT-2277: Keep the self-dispatch process-dictionary cache in sync with the
    %% authoritative instance_methods map so hot-patched fun-backed methods are
    %% visible to dispatch that happens inside this gen_server.
    put(beamtalk_class_instance_methods, NewInstanceMethods),
    NewState = State#class_state{
        instance_methods = NewInstanceMethods,
        method_source = maps:put(Selector, Source, State#class_state.method_source),
        method_signatures = maps:remove(Selector, State#class_state.method_signatures),
        method_return_types = maps:remove(Selector, State#class_state.method_return_types)
    },
    %% ADR 0050 Phase 3: Notify compiler server of hot-patch.
    %% __beamtalk_meta/0 is stale after hot-patching, so synthesize metadata
    %% from the live class_state record instead.  Return types are cleared for
    %% hot-patched methods — the compiler treats them as dynamic.
    notify_hot_patch(NewState),
    {reply, ok, NewState};
%% ADR 0084 / BT-2266: Install or replace a class-side method with a runtime fun.
%% Class-side mirror of {put_method, ...}. The fun is stored in the class_methods
%% map (source of truth) and mirrored into the metadata retrieval store so
%% inherited dispatch resolves it without a gen_server hop (BT-2008). Stale
%% class-side signature/return-type entries are cleared (no AST for dynamic funs).
handle_call(
    {put_class_method, Selector, Fun, Source},
    _From,
    #class_state{name = ClassName} = State
) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    MethodInfo = #{block => Fun, arity => Arity},
    NewClassMethods = maps:put(Selector, MethodInfo, State#class_state.class_methods),
    NewState = State#class_state{
        class_methods = NewClassMethods,
        class_method_source = maps:put(Selector, Source, State#class_state.class_method_source),
        class_method_signatures = maps:remove(
            Selector, State#class_state.class_method_signatures
        ),
        class_method_return_types = maps:remove(
            Selector, State#class_state.class_method_return_types
        )
    },
    %% Write the fun first, then publish the selector + gate flag atomically so a
    %% concurrent subclass dispatch never sees a discoverable selector whose fun
    %% is not yet present.
    beamtalk_class_metadata:put_class_method_fun(ClassName, Selector, MethodInfo),
    beamtalk_class_metadata:set_runtime_class_methods(ClassName, maps:keys(NewClassMethods)),
    notify_hot_patch(NewState),
    {reply, ok, NewState};
%% BT-572: Update class metadata after redefinition (hot reload).
%% BT-737/BT-738: Validation (shadowing + collision) delegated to beamtalk_class_registry.
%% ADR 0032 Phase 1: No flattened tables to rebuild or invalidate.
handle_call({update_class, ClassInfo}, _From, #class_state{name = ClassName} = State) ->
    OldModule = State#class_state.module,
    case beamtalk_class_registry:validate_class_update(ClassName, OldModule, ClassInfo) of
        {error, Error} ->
            {reply, {error, Error}, State};
        ok ->
            NewState = apply_class_info(State, ClassInfo),
            %% ADR 0050 Phase 3: Notify compiler server of class redefinition.
            %% The new module is already loaded, so __beamtalk_meta/0 is fresh.
            NewModule = NewState#class_state.module,
            case erlang:function_exported(NewModule, '__beamtalk_meta', 0) of
                true ->
                    try NewModule:'__beamtalk_meta'() of
                        Meta when is_map(Meta) ->
                            beamtalk_compiler_server:register_class(ClassName, Meta);
                        _ ->
                            ok
                    catch
                        Kind:Reason:ST ->
                            ?LOG_DEBUG(
                                "Failed to read __beamtalk_meta from ~p: ~p:~p",
                                [NewModule, Kind, Reason],
                                #{stacktrace => ST, domain => [beamtalk, runtime]}
                            ),
                            ok
                    end;
                false ->
                    ok
            end,
            %% ADR 0087 Phase 2 (BT-2298): Refresh the xref index on class
            %% redefinition. This path is hit both by interactive/hot redefinition
            %% (Phase 4 territory) and, more importantly for Phase 2, by the normal
            %% stdlib bootstrap: the metaclass-tower stubs (Class, Metaclass, …) are
            %% registered by beamtalk_bootstrap, then `update_class`'d when their
            %% compiled .bt modules load. Without this refresh those classes would
            %% keep only their bootstrap `unindexed_runtime_fun` rows and never gain
            %% the compiled `indexed` rows. We purge first because Phase 1's
            %% register_class/2 only inserts (the old-generation sweep is Phase 4),
            %% so a plain re-register would leave stale rows behind.
            refresh_xref(ClassName, maps:get(method_xref, ClassInfo, [])),
            {reply, {ok, NewState#class_state.fields}, NewState}
    end;
handle_call(instance_variables, _From, #class_state{fields = IVars} = State) ->
    {reply, IVars, State};
%% BT-2275: Expose field defaults so the generic instantiation path can read a
%% module-less superclass's defaults when building an inherited instance map.
handle_call(field_defaults, _From, #class_state{field_defaults = Defaults} = State) ->
    {reply, Defaults, State};
handle_call(is_sealed, _From, #class_state{is_sealed = Sealed} = State) ->
    {reply, Sealed, State};
handle_call(is_internal, _From, #class_state{is_internal = Internal} = State) ->
    {reply, Internal, State};
handle_call(is_abstract, _From, #class_state{is_abstract = Abstract} = State) ->
    {reply, Abstract, State};
handle_call(
    is_constructible,
    _From,
    #class_state{
        is_constructible = IsConstructible0,
        module = Module,
        is_abstract = IsAbstract
    } = State
) ->
    IsConstructible = beamtalk_class_instantiation:ensure_is_constructible(
        IsConstructible0, Module, IsAbstract
    ),
    {reply, IsConstructible, State#class_state{is_constructible = IsConstructible}};
%% ADR 0032 Phase 1: Returns local methods for chain walk queries.
handle_call(get_local_class_methods, _From, #class_state{class_methods = ClassMethods} = State) ->
    {reply, ClassMethods, State};
handle_call(get_instance_methods, _From, #class_state{instance_methods = InstanceMethods} = State) ->
    {reply, {ok, InstanceMethods}, State};
%% ADR 0033: Runtime-embedded documentation — class doc accessors.
handle_call(get_doc, _From, #class_state{doc = Doc} = State) ->
    {reply, Doc, State};
handle_call({set_doc, DocBinary}, _From, State) ->
    {reply, ok, State#class_state{doc = DocBinary}};
handle_call({set_method_doc, Selector, DocBinary}, _From, State) ->
    NewMethodDocs = maps:put(Selector, DocBinary, State#class_state.method_docs),
    {reply, ok, State#class_state{method_docs = NewMethodDocs}};
%% BT-411/BT-412/BT-440: Class method dispatch (BT-704).
%% ADR 0036 Phase 2 (BT-823): Also handles metaclass_method_call with identical logic.
%% ADR 0032 Phase 1: Passes local class_methods; dispatch walks superclass chain.
%%
%% BT-2379: the 4-tuple message carries the caller's session context explicitly
%% (`{Selector, Args, {SessionPid, SessionId}}`). We seed from the tuple, which
%% avoids the `process_info(CallerPid, dictionary)` full-dictionary copy the
%% legacy 3-tuple clause below must use.
handle_call(
    {MethodCallType, Selector, Args, SessionCtx},
    From,
    #class_state{} = State
) when MethodCallType =:= class_method_call; MethodCallType =:= metaclass_method_call ->
    %% No guard on SessionCtx: `seed_session_context_from/1` accepts any term and
    %% safely no-ops on unrecognised shapes, so a malformed/empty context
    %% degrades to "no context" rather than crashing the class gen_server with a
    %% function_clause (it would otherwise miss both this and the 3-tuple clause).
    Restore = seed_session_context_from(SessionCtx),
    dispatch_class_method(Selector, Args, From, State, Restore);
%% BT-2379: backward-compatible fallback for the legacy 3-tuple message shape
%% (in-flight messages across a hot-code reload). Mirrors the caller's session
%% context by reading its process dictionary via `process_info/2`.
handle_call(
    {MethodCallType, Selector, Args},
    From,
    #class_state{} = State
) when MethodCallType =:= class_method_call; MethodCallType =:= metaclass_method_call ->
    Restore = seed_caller_session_context(From),
    dispatch_class_method(Selector, Args, From, State, Restore);
handle_call({initialize, _Args}, _From, #class_state{} = State) ->
    {reply, {ok, nil}, State};
handle_call(get_module, _From, #class_state{module = Module} = State) ->
    {reply, Module, State};
handle_call({get_class_var, Name}, _From, #class_state{class_state = ClassVars} = State) ->
    {reply, maps:get(Name, ClassVars, nil), State};
handle_call({set_class_var, Name, Value}, _From, #class_state{class_state = ClassVars} = State) ->
    {reply, Value, State#class_state{class_state = ClassVars#{Name => Value}}}.

%% ADR 0032 Phase 1: Passes instance_methods (local only) instead of flattened table.
handle_cast(
    Msg,
    #class_state{
        instance_methods = InstanceMethods,
        name = ClassName,
        superclass = Superclass,
        module = Module
    } = State
) ->
    beamtalk_class_dispatch:handle_async_dispatch(
        Msg, ClassName, InstanceMethods, Superclass, Module
    ),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #class_state{name = ClassName}) ->
    %% BT-785: Clean up ETS hierarchy entry and pg group membership on shutdown.
    %% This runs when removeFromSystem stops the gen_server (gen_server:stop/1),
    %% ensuring the class is fully removed from the runtime registries.
    %% Wrapped in catch/try to be safe during node shutdown when ETS/pg may be gone.
    %% BT-2222: Single metadata row → single delete (was a three-table fan-out).
    _ = (catch beamtalk_class_metadata:delete(ClassName)),
    %% BT-1768: Clean up pid reverse index. Only cleaned on graceful shutdown —
    %% on crash, the entry intentionally survives for auto-restart recovery.
    _ = (catch ets:delete(beamtalk_class_pids, self())),
    _ = (catch pg:leave(beamtalk_classes, self())),
    ok.

%% ADR 0032 Phase 1: No flattened tables to rebuild after hot reload.
code_change(OldVsn, State, Extra) ->
    {ok, NewState} = beamtalk_hot_reload:code_change(OldVsn, State, Extra),
    {ok, NewState#class_state{is_constructible = undefined}}.

%%====================================================================
%% Internal functions
%%====================================================================

-doc """
Run a class-method (or metaclass-method) call against this class gen_server's
state, restoring the previously-seeded session context afterwards.

`Restore` is the zero-arity closure returned by `seed_session_context_from/1`
(explicit-context path, BT-2379) or `seed_caller_session_context/1` (legacy
3-tuple fallback). It is always run, even on a method-body crash, so a
concurrent call from another session never observes a stale mirrored context.
""".
-spec dispatch_class_method(
    atom(), list(), {pid(), term()} | term(), #class_state{}, fun(() -> ok)
) -> {reply, term(), #class_state{}} | {noreply, #class_state{}}.
dispatch_class_method(Selector, Args, From, State, Restore) ->
    #class_state{
        class_methods = ClassMethods,
        instance_methods = InstanceMethods,
        name = ClassName,
        module = Module,
        class_state = ClassVars
    } = State,
    try
        beamtalk_class_dispatch:handle_class_method_call(
            Selector, Args, ClassName, Module, ClassMethods, ClassVars
        )
    of
        {reply, Result, NewClassVars} ->
            {reply, Result, State#class_state{class_state = NewClassVars}};
        test_spawn ->
            beamtalk_test_case:spawn_test_execution(
                Selector, Args, ClassName, Module, InstanceMethods, From
            ),
            {noreply, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    after
        Restore()
    end.

-doc """
Seed this class gen_server with a session context passed explicitly in the
class-method-call message (BT-2379), returning a zero-arity restore closure.

ADR 0081: factory class methods like `Session current` /
`Workspace currentSession` read `beamtalk_session_pid` / `beamtalk_session_id`
from the process dictionary. Class-method dispatch hops from the eval worker to
this gen_server, so the worker now sends its two context keys in the message
tuple (`beamtalk_class_dispatch:local_session_context/0`) rather than having us
copy its entire dictionary via `process_info/2`. We `put/2` them locally and
restore the previous values (or erase) on the way out.
""".
-spec seed_session_context_from({pid() | undefined, binary() | undefined} | term()) ->
    fun(() -> ok).
seed_session_context_from({SessionPid, SessionId}) ->
    PrevPid = put(beamtalk_session_pid, SessionPid),
    PrevId = put(beamtalk_session_id, SessionId),
    fun() ->
        restore_dict_key(beamtalk_session_pid, PrevPid),
        restore_dict_key(beamtalk_session_id, PrevId),
        ok
    end;
seed_session_context_from(_Other) ->
    %% Unrecognised context shape: seed nothing rather than crash the call.
    fun() -> ok end.

-doc """
Mirror the calling process's session context into this class gen_server for the
duration of a class-method call, returning a zero-arity closure that restores
the prior state.

ADR 0081 (BT-2367): `seed_session_context/2` seeds the eval *worker*, but
class-method dispatch (`Session current`, `Workspace currentSession`) hops to
this gen_server, where those keys are absent. We read them from the caller via
`process_info/2` (no message-shape change) and `put/2` them locally, restoring
the previous values (or erasing) on the way out so concurrent calls from other
sessions never see a stale context.
""".
-spec seed_caller_session_context({pid(), term()} | term()) -> fun(() -> ok).
seed_caller_session_context({CallerPid, _Tag}) when is_pid(CallerPid) ->
    case process_info(CallerPid, dictionary) of
        {dictionary, Dict} ->
            CallerPidCtx = proplists:get_value(beamtalk_session_pid, Dict),
            CallerIdCtx = proplists:get_value(beamtalk_session_id, Dict),
            PrevPid = put(beamtalk_session_pid, CallerPidCtx),
            PrevId = put(beamtalk_session_id, CallerIdCtx),
            fun() ->
                restore_dict_key(beamtalk_session_pid, PrevPid),
                restore_dict_key(beamtalk_session_id, PrevId),
                ok
            end;
        undefined ->
            %% Caller already gone (dead before we ran): nothing to mirror.
            fun() -> ok end
    end;
seed_caller_session_context(_From) ->
    fun() -> ok end.

%% Restore a process-dictionary key to its previous value, or erase it if it had
%% none. `put/2` returns `undefined` for a key that was unset, which is exactly
%% the value we erase back to.
-spec restore_dict_key(atom(), term()) -> ok.
restore_dict_key(Key, undefined) ->
    erase(Key),
    ok;
restore_dict_key(Key, Prev) ->
    put(Key, Prev),
    ok.

-doc """
Walk superclass chain to find an inherited class method.
Returns a CompiledMethod-like map or nil if not found.
Each hop is a gen_server:call to the superclass process, which handles its
own {class_method, _} lookup.  The self() guard prevents deadlock if a
class lists itself as its own superclass; the 5 s timeout caps any deeper
cycle that might slip through.
""".
-spec find_inherited_class_method(atom(), atom() | none) -> map() | nil.
find_inherited_class_method(_Selector, none) ->
    nil;
find_inherited_class_method(Selector, SuperName) ->
    case beamtalk_class_registry:whereis_class(SuperName) of
        undefined ->
            nil;
        SuperPid when SuperPid =:= self() ->
            %% Guard against self-call which would deadlock
            nil;
        SuperPid ->
            try
                gen_server:call(SuperPid, {class_method, Selector}, 5000)
            catch
                exit:{timeout, _} -> nil;
                exit:{noproc, _} -> nil
            end
    end.

-doc """
Check whether `class_new:/3` is exported anywhere in the superclass chain.

Walks the ETS hierarchy table from `ClassName` upward.  Returns `true` as soon as
a module exporting `class_new:'/3` is found, `false` if none is found in the chain.
This correctly supports inherited `class new:` constructors: a subclass that does
not override `class new:` will still route through the parent's implementation.

We use `function_exported/3` rather than checking the `classMethods` metadata map
because the metadata includes auto-generated `new:` entries for all classes,
making it impossible to distinguish user-defined from auto-generated with a map
key check alone.
""".
-spec has_class_new_in_chain(class_name(), atom()) -> boolean().
has_class_new_in_chain(ClassName, Module) ->
    has_class_new_in_chain(ClassName, Module, 0).

-spec has_class_new_in_chain(class_name(), atom(), non_neg_integer()) -> boolean().
has_class_new_in_chain(_ClassName, _Module, Depth) when Depth > 50 ->
    false;
has_class_new_in_chain(ClassName, Module, Depth) ->
    case erlang:function_exported(Module, 'class_new:', 3) of
        true ->
            true;
        false ->
            SuperName =
                case beamtalk_class_metadata:lookup_superclass(ClassName) of
                    {ok, S} -> S;
                    not_found -> none
                end,
            case SuperName of
                none ->
                    false;
                _ ->
                    case beamtalk_class_registry:whereis_class(SuperName) of
                        undefined ->
                            false;
                        SuperPid when SuperPid =:= self() ->
                            false;
                        SuperPid ->
                            try
                                SuperModule = module_name(SuperPid),
                                has_class_new_in_chain(SuperName, SuperModule, Depth + 1)
                            catch
                                Kind:Reason:ST ->
                                    ?LOG_DEBUG(
                                        "has_class_new_in_chain failed for ~p: ~p:~p",
                                        [SuperName, Kind, Reason],
                                        #{stacktrace => ST, domain => [beamtalk, runtime]}
                                    ),
                                    false
                            end
                    end
            end
    end.

-doc """
Read __beamtalk_meta/0 from a compiled module.

Returns the meta map or #{} if the function is not exported or fails.
ADR 0050 Phase 5: Used by init/1 and apply_class_info/2 to read static metadata.
""".
-spec read_meta(atom()) -> map().
read_meta(Module) ->
    case erlang:function_exported(Module, '__beamtalk_meta', 0) of
        true ->
            try Module:'__beamtalk_meta'() of
                M when is_map(M) -> M;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.

-doc """
Convert a method_info map from __beamtalk_meta/0 to the internal format.

Returns {MethodMap, ReturnTypes} where:
  MethodMap: selector => #{arity => N, is_sealed => bool, ...}
  ReturnTypes: selector => atom (only for non-none return types)

Falls back to FallbackMethods when MetaMethodInfo is undefined (dynamic classes
without __beamtalk_meta/0 pass method closures via ClassInfo instead).

The full method info map is preserved (including is_sealed, param_types, etc.)
because runtime tooling (e.g. :help) reads these keys from __method_info__.
""".
-spec meta_to_methods(map() | undefined, map()) -> {map(), map()}.
meta_to_methods(undefined, FallbackMethods) ->
    %% No meta available: return fallback methods with empty return types.
    %% Callers that need to preserve explicit return_types must merge them separately.
    {FallbackMethods, #{}};
meta_to_methods(MetaMethodInfo, _FallbackMethods) when is_map(MetaMethodInfo) ->
    ReturnTypes = maps:filtermap(
        fun
            (_Sel, #{return_type := RT}) when RT =/= none -> {true, RT};
            (_, _) -> false
        end,
        MetaMethodInfo
    ),
    {MetaMethodInfo, ReturnTypes}.

-doc """
Seed the runtime class-method fun retrieval store from a class_methods map.

ADR 0084 / BT-2266: picks out the entries that carry a `block` (runtime/builder
funs, as opposed to compiled `#{arity => N}` references), writes each into the
metadata retrieval store, then sets the per-class gate flag. Classes with no fun
entries leave the gate flag false so their dispatch never reads the funs table.
""".
-spec seed_runtime_class_methods(class_name(), map()) -> ok.
seed_runtime_class_methods(ClassName, ClassMethods) ->
    Funs = maps:filter(
        fun(_Selector, Info) -> is_map(Info) andalso maps:is_key(block, Info) end,
        ClassMethods
    ),
    case maps:size(Funs) of
        0 ->
            ok;
        _ ->
            maps:foreach(
                fun(Selector, Info) ->
                    beamtalk_class_metadata:put_class_method_fun(ClassName, Selector, Info)
                end,
                Funs
            ),
            beamtalk_class_metadata:set_runtime_class_methods(ClassName, maps:keys(ClassMethods))
    end.

-doc """
Notify the compiler server of a hot-patch.

ADR 0050 Phase 3: `__beamtalk_meta/0` is stale after hot-patching (it reflects
compile-time metadata only).  This function synthesizes a minimal metadata map
from the current live `class_state` and casts it to the compiler server.
The compiler server replaces the class entry wholesale.
Fire-and-forget — silently dropped if the server is not running.
""".
-spec notify_hot_patch(#class_state{}) -> ok.
notify_hot_patch(#class_state{
    name = ClassName,
    module = Module,
    superclass = Superclass,
    is_sealed = IsSealed,
    is_abstract = IsAbstract,
    instance_methods = InstanceMethods,
    class_methods = ClassMethods,
    fields = Fields,
    method_return_types = MethodReturnTypes,
    class_method_return_types = ClassMethodReturnTypes
}) ->
    %% Build selector→arity maps from the live instance_methods and class_methods.
    %% param_types uses atom `none` (not empty list) per ADR 0050 format.
    MethodInfo = maps:map(
        fun(Sel, #{arity := Arity}) ->
            %% Return types are cleared for hot-patched selectors — the compiler
            %% will treat them as dynamic (none).
            #{
                arity => Arity,
                return_type => maps:get(Sel, MethodReturnTypes, none),
                param_types => none
            }
        end,
        InstanceMethods
    ),
    ClassMethodInfo = maps:map(
        fun(Sel, #{arity := Arity}) ->
            #{
                arity => Arity,
                return_type => maps:get(Sel, ClassMethodReturnTypes, none),
                param_types => none
            }
        end,
        ClassMethods
    ),
    %% Normalize superclass: class_state uses `none` for root classes, but
    %% __beamtalk_meta/0 uses `nil`.  Keep the cache consistent with the meta format.
    SuperclassOut =
        case Superclass of
            none -> nil;
            S -> S
        end,
    Meta = #{
        class => ClassName,
        superclass => SuperclassOut,
        module => Module,
        meta_version => 2,
        is_sealed => IsSealed,
        is_abstract => IsAbstract,
        fields => Fields,
        method_info => MethodInfo,
        class_method_info => ClassMethodInfo
    },
    %% Guard: the compiler app may not be present in all deployments.
    %% beamtalk_runtime does not formally depend on beamtalk_compiler.
    case erlang:function_exported(beamtalk_compiler_server, register_class, 2) of
        true -> beamtalk_compiler_server:register_class(ClassName, Meta);
        false -> ok
    end.

-doc """
Apply ClassInfo map to existing State, returning updated State.
ADR 0032 Phase 1: No flattened tables to rebuild.
ADR 0050 Phase 5: Static metadata read from __beamtalk_meta/0 on the new module.
""".
-spec apply_class_info(#class_state{}, map()) -> #class_state{}.
apply_class_info(State, ClassInfo) ->
    %% BT-893: Keep process dictionary in sync with state for self-instantiation.
    NewModule = maps:get(module, ClassInfo, State#class_state.module),
    put(beamtalk_class_module, NewModule),

    %% ADR 0050 Phase 5: Prefer meta from ClassInfo when available (same reason as init/1:
    %% erlang:function_exported/3 returns false during on_load, so read_meta/1 returns #{}).
    %% Falls back to read_meta for explicit hot-reload calls made outside on_load.
    Meta =
        case maps:get(meta, ClassInfo, undefined) of
            undefined -> read_meta(NewModule);
            M when is_map(M) -> M
        end,
    NewIsAbstract = maps:get(
        is_abstract,
        Meta,
        maps:get(is_abstract, ClassInfo, State#class_state.is_abstract)
    ),
    put(beamtalk_class_is_abstract, NewIsAbstract),

    %% BT-2275: Reconcile field defaults on reload (ADR 0082 "disk wins").
    %% A compiled reload supplies no field_defaults (defaults are baked into the
    %% module's new/0), so we clear them; a builder re-register supplies them
    %% and they are re-cached. Keep the process-dict cache in sync for self new.
    NewFieldDefaults = maps:get(field_defaults, ClassInfo, #{}),
    put(beamtalk_class_field_defaults, NewFieldDefaults),

    %% ADR 0071 Phase 5: Recompute is_internal on reload/redefinition.
    %% Same logic as init/1 — keeps visibility in sync after hot-reload.
    NewIsInternal =
        case maps:get(visibility, Meta, maps:get(visibility, ClassInfo, public)) of
            internal ->
                true;
            _ ->
                maps:get(
                    is_internal,
                    Meta,
                    maps:get(is_internal, ClassInfo, State#class_state.is_internal)
                )
        end,

    {NewInstanceMethods, NewMethodReturnTypes} = meta_to_methods(
        maps:get(method_info, Meta, undefined),
        maps:get(instance_methods, ClassInfo, State#class_state.instance_methods)
    ),
    {NewClassMethods, NewClassMethodReturnTypes} = meta_to_methods(
        maps:get(class_method_info, Meta, undefined),
        maps:get(class_methods, ClassInfo, State#class_state.class_methods)
    ),

    %% ADR 0057: Fix stale superclass left by bootstrap stubs. Bootstrap stubs register
    %% abstract stdlib classes with placeholder superclasses (e.g. 'Class' → 'Object').
    %% When the compiled stdlib module loads and calls update_class, Meta now carries the
    %% correct superclass from __beamtalk_meta/0. We update both the gen_server field and
    %% the ETS hierarchy table so all hierarchy traversal code sees the right value.
    %% Dynamic classes (no compiled module) have Meta = #{} so maps:find returns error
    %% and their existing superclass is preserved unchanged.
    NewSuperclass =
        case maps:find(superclass, Meta) of
            %% key absent — keep existing
            error -> State#class_state.superclass;
            %% root class (codegen emits 'nil')
            {ok, nil} -> none;
            %% corrected value from compiled module
            {ok, S} -> S
        end,
    %% BT-2277: Reconcile the self-dispatch caches on reload, mirroring the
    %% field-defaults handling above so fun-backed instance dispatch from inside
    %% the class process resolves against the current method set / hierarchy.
    put(beamtalk_class_instance_methods, NewInstanceMethods),
    put(beamtalk_class_superclass, NewSuperclass),
    %% BT-2222: Single metadata-row update keeps hierarchy + module + selectors in
    %% sync on hot reload (all three may change: new superclass, recompiled module,
    %% added class methods).
    %%
    %% ADR 0084 / BT-2266: reload is memory-vs-disk reconciliation (ADR 0082) —
    %% disk wins. Purge stale runtime class-method funs, then insert (which resets
    %% the gate flag to false), then re-seed from the incoming class methods. A
    %% pure compiled reload thus drops runtime funs; a builder re-register that
    %% supplies funs re-installs them.
    beamtalk_class_metadata:delete_class_method_funs(State#class_state.name),
    beamtalk_class_metadata:insert(
        State#class_state.name, NewModule, maps:keys(NewClassMethods), NewSuperclass
    ),
    seed_runtime_class_methods(State#class_state.name, NewClassMethods),

    State#class_state{
        module = NewModule,
        superclass = NewSuperclass,
        instance_methods = NewInstanceMethods,
        class_methods = NewClassMethods,
        fields = maps:get(
            fields,
            Meta,
            maps:get(fields, ClassInfo, State#class_state.fields)
        ),
        field_defaults = NewFieldDefaults,
        is_abstract = NewIsAbstract,
        is_sealed = maps:get(
            is_sealed,
            Meta,
            maps:get(is_sealed, ClassInfo, State#class_state.is_sealed)
        ),
        method_source = maps:get(method_source, ClassInfo, State#class_state.method_source),
        class_method_source = maps:get(
            class_method_source, ClassInfo, State#class_state.class_method_source
        ),
        method_signatures = maps:get(
            method_signatures, ClassInfo, State#class_state.method_signatures
        ),
        class_method_signatures = maps:get(
            class_method_signatures, ClassInfo, State#class_state.class_method_signatures
        ),
        method_return_types = maps:merge(
            maps:get(method_return_types, ClassInfo, State#class_state.method_return_types),
            NewMethodReturnTypes
        ),
        class_method_return_types = maps:merge(
            maps:get(
                class_method_return_types, ClassInfo, State#class_state.class_method_return_types
            ),
            NewClassMethodReturnTypes
        ),
        is_constructible = maps:get(is_constructible, ClassInfo, undefined),
        doc = maps:get(doc, ClassInfo, State#class_state.doc),
        method_docs = maps:get(method_docs, ClassInfo, State#class_state.method_docs),
        class_method_docs = maps:get(
            class_method_docs, ClassInfo, State#class_state.class_method_docs
        ),
        is_internal = NewIsInternal
    }.
