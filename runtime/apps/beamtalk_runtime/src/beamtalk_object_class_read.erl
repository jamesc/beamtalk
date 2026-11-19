%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_object_class_read).

%%% **DDD Context:** Runtime Context

-moduledoc """
ETS-only reflection readers for a class, split out of `beamtalk_object_class` (BT-5047).

The `*_safe` and `published_*` readers answer from the class registry and the
published class metadata tables, so they need no `gen_server:call` to the class
process and cannot queue behind a long class-side call (BT-3820). Each falls
back to the `beamtalk_object_class` call only on a miss.

Callers outside the class process call this module directly.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").
-export([
    class_name_or_undefined/1,
    class_name_safe/1,
    compiled_method_map/7,
    compiled_method_safe/3,
    instance_methods_safe/1,
    instance_variables_safe/1,
    local_class_method_selectors_safe/1,
    local_instance_method_infos_safe/1,
    published_compiled_method/3,
    read_published/3,
    reflection_class_name/1,
    superclass_safe/1
]).

-doc """
Get the class name, preferring a message-free metadata lookup.

`beamtalk_object_class:class_name/1`'s `gen_server:call(ClassPid, class_name)` uses the default 5s
timeout and queues behind whatever `ClassPid` is currently running, so it
fails with `{timeout, {gen_server, call, [ClassPid, class_name]}}` when the
class process is busy serving a long class-side call (BT-3820). This resolves
the name via `beamtalk_class_registry:class_name_for_pid/1` (a pure ETS reverse
lookup, no message send), falling back to `beamtalk_object_class:class_name/1` only on a miss. The
same caveat as `beamtalk_object_class:module_name_safe/1` applies to an untrappable `kill`.

Stale-name behaviour: the reverse index is kept for a class process that
crashed without running `terminate/2` (auto-restart recovery), so for such a
pid this returns the old name instead of exiting `noproc` — but only until the
restart, which deletes the dead pid's row (after which this exits `noproc`).
Callers acting by *name* in that window (`classRemoveFromSystem`,
`classRenameTo`, `classReload`, ...) may find no class registered under it.
Callers needing a liveness guarantee must check `is_process_alive/1`.
""".
-spec class_name_safe(pid()) -> beamtalk_object_class:class_name().
class_name_safe(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_name);
class_name_safe(ClassPid) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} -> ClassName;
        not_found -> beamtalk_object_class:class_name(ClassPid)
    end.

-doc """
Registered class name from the pure ETS reverse index only, or `undefined`.

Never sends a message, so unlike `class_name_safe/1` it cannot exit `noproc`
when the class stops mid-probe. Used as the class-name thunk of
`beamtalk_primitive:ensure_class_module_loaded/2`: `undefined` never equals a
module, so a miss falls back to the plain `code:ensure_loaded/1` probe
(BT-4627).
""".
-spec class_name_or_undefined(pid()) -> beamtalk_object_class:class_name() | undefined.
class_name_or_undefined(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_name);
class_name_or_undefined(ClassPid) ->
    case beamtalk_class_registry:class_name_for_pid(ClassPid) of
        {ok, ClassName} -> ClassName;
        not_found -> undefined
    end.

-doc """
Get the superclass, preferring a deadlock-safe metadata lookup.

Mirrors `beamtalk_object_class:module_name_safe/1`: resolves via
`beamtalk_class_registry:class_name_for_pid/1` (a pure ETS reverse lookup, no
message send) followed by `beamtalk_class_metadata:lookup_superclass/1` — the
same ETS row `beamtalk_class_dispatch` reads on the dispatch hot path, kept in
sync with the process dictionary by `sync_identity/6` at every `init/1` and
`beamtalk_object_class:apply_class_info/2`. Falls back to `beamtalk_object_class:superclass/1`'s `gen_server:call` only if
either lookup misses.

Without this, `beamtalk_behaviour_intrinsics` would read the superclass via
`gen_server:call(ClassPid, superclass)` — a second, independent source from
the one dispatch uses, which could disagree with it mid-reload (ADR 0057).
Prefer this function at any reflection/hierarchy-walk call site instead.
""".
-spec superclass_safe(pid()) -> beamtalk_object_class:class_name() | none.
superclass_safe(ClassPid) when ClassPid =:= self() ->
    get(beamtalk_class_superclass);
superclass_safe(ClassPid) ->
    read_published(
        ClassPid,
        fun beamtalk_class_metadata:lookup_superclass/1,
        fun() -> beamtalk_object_class:superclass(ClassPid) end
    ).

-doc """
Local (non-inherited) instance-method selectors, preferring a message-free read.

`beamtalk_object_class:methods/1`'s `gen_server:call(ClassPid, methods)` queues behind whatever
`ClassPid` is running, so calling it from inside another class's handler
(`Foo methods`, `allInstVarNames`, the protocol-conformance walk, ...) deadlocks
two busy classes against each other until the 5s call timeout (BT-3826; the
invariant is "a class process never makes a blocking call into another class
process while handling a call"). This reads the selectors the owning class
published with `beamtalk_class_publish:publish_reflection/1`.

Falls back to `beamtalk_object_class:methods/1` only when there is no row to read -- a remote pid
(the node-local ETS cannot name it; the cross-node call path is unchanged), or
a class that has not published yet.
""".
-spec instance_methods_safe(pid()) -> [beamtalk_object_class:selector()].
instance_methods_safe(ClassPid) ->
    read_published(
        ClassPid,
        fun beamtalk_class_metadata:lookup_instance_methods/1,
        fun() -> beamtalk_object_class:methods(ClassPid) end
    ).

-doc """
Own declared instance variables (not inherited), preferring a message-free read.

Same contract and fallback as `instance_methods_safe/1`, for
`gen_server:call(ClassPid, instance_variables)`.
""".
-spec instance_variables_safe(pid()) -> [atom()].
instance_variables_safe(ClassPid) ->
    read_published(
        ClassPid,
        fun beamtalk_class_metadata:lookup_instance_variables/1,
        fun() -> beamtalk_object_class:instance_variables(ClassPid) end
    ).

-doc """
Local (non-inherited) class-side method selectors, preferring a message-free read.

The selectors are the `selectors` field of the unified metadata row that
`sync_identity/6` keeps current (and `put_class_method` refreshes), the same
source `beamtalk_class_dispatch` reads -- so unlike `beamtalk_object_class:local_class_methods/1` this
never queues behind the owning class. Falls back to `beamtalk_object_class:local_class_methods/1`
under the same conditions as `instance_methods_safe/1`.
""".
-spec local_class_method_selectors_safe(pid()) -> [beamtalk_object_class:selector()].
local_class_method_selectors_safe(ClassPid) ->
    read_published(
        ClassPid,
        fun(ClassName) ->
            case beamtalk_class_metadata:lookup_methods(ClassName) of
                {ok, _Module, Selectors} -> {ok, Selectors};
                not_found -> not_found
            end
        end,
        fun() -> beamtalk_object_class:local_class_methods(ClassPid) end
    ).

-doc """
Local instance methods as `selector => MethodInfo`, preferring a message-free read.

Reads the per-method records the owning class published with
`beamtalk_class_publish:publish_reflection/1`; falls back to the `get_instance_methods` call under the
same conditions as `instance_methods_safe/1`.
""".
-spec local_instance_method_infos_safe(pid()) -> #{beamtalk_object_class:selector() => map()}.
local_instance_method_infos_safe(ClassPid) ->
    read_published(
        ClassPid,
        fun(ClassName) ->
            case beamtalk_class_metadata:lookup_instance_methods(ClassName) of
                {ok, _} ->
                    {ok,
                        maps:map(
                            fun(_Sel, #{info := Info}) -> Info end,
                            beamtalk_class_metadata:lookup_method_records(ClassName, instance)
                        )};
                not_found ->
                    not_found
            end
        end,
        fun() ->
            {ok, InstanceMethods} = gen_server:call(ClassPid, get_instance_methods),
            InstanceMethods
        end
    ).

-doc """
Resolve `Selector` to a `CompiledMethod` map on `ClassPid`'s own method table
(`Side = instance`) or, for `Side = class`, on its class-side table and then the
superclass chain's (the walk `{class_method, Sel}` has always done). Returns
`nil` when not found.

Message-free for a published local class: reads the per-method records
`beamtalk_class_publish:publish_reflection/1` keeps current, so it never queues behind a busy class
process (BT-3832 -- `beamtalk_method_resolver` runs inside class handlers via
`Behaviour >>`). Falls back to the `{method, Sel}` / `{class_method, Sel}` call
only for a remote pid or a class that has not published yet.
""".
-spec compiled_method_safe(pid(), instance | class, beamtalk_object_class:selector()) ->
    map() | 'nil'.
compiled_method_safe(ClassPid, Side, Selector) ->
    case published_class_name(ClassPid) of
        {ok, ClassName} ->
            published_compiled_method(ClassName, Side, Selector);
        not_found ->
            Msg =
                case Side of
                    instance -> {method, Selector};
                    class -> {class_method, Selector}
                end,
            gen_server:call(ClassPid, Msg)
    end.

%% The one shape shared by the `*_safe` accessors: resolve `ClassPid` to its class
%% name through the node-local reverse index (no message; `self()` reads the
%% process dictionary), then `Lookup(ClassName)` in the published metadata. A miss
%% at either step runs `Fallback()`, the blocking `gen_server:call` the accessor
%% exists to avoid, so a remote pid or an unpublished class behaves as before.
-spec read_published(
    pid(),
    fun((beamtalk_object_class:class_name()) -> {ok, Value} | not_found),
    fun(() -> Value)
) -> Value when
    Value :: term().
read_published(ClassPid, Lookup, Fallback) ->
    case reflection_class_name(ClassPid) of
        {ok, ClassName} ->
            case Lookup(ClassName) of
                {ok, Value} -> Value;
                not_found -> Fallback()
            end;
        not_found ->
            Fallback()
    end.

%% A class name for a pid whose reflection state is in ETS, else `not_found`.
-spec published_class_name(pid()) -> {ok, beamtalk_object_class:class_name()} | not_found.
published_class_name(ClassPid) ->
    case reflection_class_name(ClassPid) of
        {ok, ClassName} ->
            case beamtalk_class_metadata:lookup_instance_methods(ClassName) of
                {ok, _} -> {ok, ClassName};
                not_found -> not_found
            end;
        not_found ->
            not_found
    end.

-spec published_compiled_method(
    beamtalk_object_class:class_name(), instance | class, beamtalk_object_class:selector()
) ->
    map() | 'nil'.
published_compiled_method(ClassName, instance, Selector) ->
    local_compiled_method(ClassName, instance, Selector);
published_compiled_method(ClassName, class, Selector) ->
    StepFun = fun(Name, _Depth) ->
        case local_compiled_method(Name, class, Selector) of
            nil ->
                case beamtalk_class_metadata:lookup_superclass(Name) of
                    {ok, none} -> not_found;
                    {ok, Super} -> {next, Super};
                    not_found -> not_found
                end;
            Method ->
                {found, Method}
        end
    end,
    Ctx = #{start => ClassName, selector => Selector},
    case beamtalk_hierarchy:walk_or_warn(ClassName, StepFun, published_compiled_method, Ctx) of
        {found, Method} -> Method;
        not_found -> nil;
        {cycle, _} -> nil
    end.

%% `ClassName`'s own `CompiledMethod` for `Selector` (no chain walk), or `nil`.
-spec local_compiled_method(
    beamtalk_object_class:class_name(), instance | class, beamtalk_object_class:selector()
) ->
    map() | 'nil'.
local_compiled_method(ClassName, Side, Selector) ->
    case beamtalk_class_metadata:lookup_method_record(ClassName, Side, Selector) of
        {ok, #{info := Info, source := Source, signature := Sig, doc := Doc}} ->
            compiled_method_map(ClassName, Side, Selector, Info, Source, Sig, Doc);
        not_found ->
            nil
    end.

%% The one constructor for a `CompiledMethod` map, shared by the
%% `{method, _}` / `{class_method, _}` handlers and the ETS read path.
-spec compiled_method_map(
    beamtalk_object_class:class_name(),
    instance | class,
    beamtalk_object_class:selector(),
    map(),
    binary(),
    term(),
    term()
) -> map().
compiled_method_map(ClassName, Side, Selector, Info, Source, Sig, Doc) ->
    #{
        '$beamtalk_class' => 'CompiledMethod',
        '__selector__' => Selector,
        '__source__' => Source,
        '__signature__' => Sig,
        '__method_info__' => Info,
        '__doc__' => Doc,
        %% ADR 0127 §12: `CompiledMethod origin` is looked up lazily from these
        %% two keys by `beamtalk_compiled_method_ops` when reflection asks for
        %% it. It is never computed here: this constructor also serves method
        %% resolution, and an xref read per resolution cost ~20 us (BT-4071).
        '__defining_class__' => ClassName,
        '__side__' => Side
    }.

%% The class name for a pid, from the node-local reverse index (no message).
%% `self()` reads the process dictionary, like `class_name_safe/1`.
-spec reflection_class_name(pid()) -> {ok, beamtalk_object_class:class_name()} | not_found.
reflection_class_name(ClassPid) when ClassPid =:= self() ->
    case get(beamtalk_class_name) of
        undefined -> not_found;
        Name -> {ok, Name}
    end;
reflection_class_name(ClassPid) ->
    beamtalk_class_registry:class_name_for_pid(ClassPid).
