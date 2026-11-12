%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_publish).

%%% **DDD Context:** Object System Context

-moduledoc """
Outbound publication of a class's facts to the rest of the runtime.

A class gen_server (`beamtalk_object_class`) owns its state, but several other
contexts read facts *about* the class without messaging it. This module holds
the writers for those facts, none of which touch the `#class_state{}` record:

* the unified `beamtalk_class_metadata` ETS row and the process-dictionary
  caches that mirror it (`sync_identity/6`, `sync_direct_constructors/2`,
  `sync_constant_class_methods/2`, `sync_caller_safe_primitives/2`,
  `sync_class_process_facts/3`, `seed_runtime_class_methods/2`);
* the `beamtalk_xref` cross-reference index (`register_xref/2`,
  `register_state_var_xref/2`, `refresh_xref/2`, `put_method_xref/4`);
* the compiler-server registration (`notify_compiler_server_register/2`) and the
  `ClassLoaded`/`ClassRemoved` system announcements
  (`announce_class_lifecycle/2`).

`sync_identity/6` writes the *calling* process's dictionary, so it must be
called from the class gen_server itself, as `init/1` and the reload path do.

`publish_reflection/1` and `notify_hot_patch/1` take a `reflection_view()`, a
plain map of the class fields they read that `beamtalk_object_class` builds with
`reflection_view/1`, so the private `#class_state{}` record is never shared.
""".

-include_lib("kernel/include/logger.hrl").

-export([
    register_xref/2,
    register_state_var_xref/2,
    refresh_xref/2,
    put_method_xref/4,
    safe_xref/1,
    announce_class_lifecycle/2,
    notify_compiler_server_register/2,
    sync_identity/6,
    sync_direct_constructors/2,
    sync_constant_class_methods/2,
    sync_caller_safe_primitives/2,
    sync_class_process_facts/3,
    warn_stale_subclass_constructors/3,
    is_runtime_fun_info/1,
    seed_runtime_class_methods/2,
    publish_reflection/1,
    method_record/4,
    notify_hot_patch/1
]).

-type class_name() :: atom().
-type selector() :: atom().

-type reflection_view() :: #{
    name := class_name(),
    module := atom(),
    superclass := class_name() | none,
    is_sealed := boolean(),
    is_abstract := boolean(),
    is_typed := boolean(),
    is_internal := boolean(),
    shape_version := pos_integer(),
    doc := binary() | none,
    fields := [atom()],
    instance_methods := #{selector() => map()},
    class_methods := #{selector() => map()},
    method_return_types := #{selector() => term()},
    class_method_return_types := #{selector() => term()},
    method_source := #{selector() => binary()},
    method_signatures := #{selector() => binary()},
    method_docs := #{selector() => binary()},
    class_method_source := #{selector() => binary()},
    class_method_signatures := #{selector() => binary()},
    class_method_docs := #{selector() => binary()}
}.
-export_type([reflection_view/0]).

%%====================================================================
%% xref index and lifecycle announcements
%%====================================================================

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
Forward a class's per-instance-variable declaration-line rows to
`beamtalk_xref`, the state-var analogue of `register_xref/2`.

A no-op when `StateVarXref` is empty — a hand-coded stub class, a class
compiled before this feature landed, or a `ClassBuilder`-built class with no
compiler to derive lines from. Same `beamtalk_xref`-absent guard as
`register_xref/2`.
""".
-spec register_state_var_xref(class_name(), [map()]) -> ok.
register_state_var_xref(_ClassName, []) ->
    ok;
register_state_var_xref(ClassName, StateVarXref) ->
    case erlang:whereis(beamtalk_xref) of
        undefined ->
            ?LOG_DEBUG(#{
                event => xref_not_running,
                class => ClassName,
                reason => "beamtalk_xref not registered; skipping state-var index population",
                domain => [beamtalk, runtime]
            }),
            ok;
        _Pid ->
            ok = beamtalk_xref:register_state_vars(ClassName, StateVarXref)
    end.

-doc """
Refresh a class's xref rows on redefinition (ADR 0087 Phase 2).

Purges the class's existing rows before re-registering, because Phase 1's
`register_class/2` only inserts (the multi-generation sweep of stale rows lands
in Phase 4). Purging first keeps the index consistent with the new
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

-doc """
Announce a class-lifecycle system event (`'ClassLoaded'` | `'ClassRemoved'`) on
the `SystemAnnouncer` bus (ADR 0093 §2).

The announcements bus is a `beamtalk_runtime_sup` worker started *after*
`beamtalk_bootstrap`, so during early stdlib bootstrap (and on a minimal
embedded runtime) it may not be running yet. The whole call is therefore wrapped
in try/catch and a `whereis` guard: announcing is a best-effort observability
side effect and must never fail or delay class load/teardown. The
`beamtalk_announcements:system_announce/2` entry point builds the typed event
payload and dispatches it caller-side, fire-and-forget.
""".
-spec announce_class_lifecycle('ClassLoaded' | 'ClassRemoved', class_name()) -> ok.
announce_class_lifecycle(EventClass, ClassName) ->
    case erlang:whereis(beamtalk_announcements) of
        undefined ->
            ok;
        _Pid ->
            try
                beamtalk_announcements:system_announce(EventClass, #{className => ClassName})
            catch
                _:_ -> ok
            end
    end,
    ok.

-doc """
Re-index a single hot-patched method in `beamtalk_xref` (ADR 0087 Phase 4).

Called from the `{put_method, ...}` / `{put_class_method, ...}` handlers after
a live `>>` edit (ADR 0082). Re-parses the one method's `Source` via the
compiler AST FFI (`beamtalk_xref:build_method_entry/5`) and replaces just that
`{Class, ClassSide, Selector}` row set via `beamtalk_xref:put_method/4`;
sibling methods on the same class are untouched.

A no-op when `beamtalk_xref` is not running. Hot-patched methods are tagged
`source_status = indexed` (they carry source like any compiled method) with
`provenance = put_method`.
""".
-spec put_method_xref(class_name(), boolean(), selector(), binary()) -> ok.
put_method_xref(ClassName, ClassSide, Selector, Source) ->
    %% build_method_entry/5 is pure; only the put_method/4 gen_server call must
    %% be best-effort. A bare whereis/1 guard is racy: beamtalk_xref can die or
    %% restart between the check and the call, which would crash this class
    %% gen_server mid hot-patch. Degrade to a no-op on xref unavailability
    %% instead, matching the documented behaviour.
    Entry = beamtalk_xref:build_method_entry(
        ClassSide, Selector, Source, indexed, put_method
    ),
    safe_xref(fun() -> beamtalk_xref:put_method(ClassName, ClassSide, Selector, Entry) end).

-doc """
Run a `beamtalk_xref` gen_server call best-effort.

The xref index is advisory tooling state; a hot patch must not crash the class
gen_server if it is unavailable. Catches the `noproc`/`{noproc, _}` /
`{normal, _}` / `{{nodedown, _}, _}` exits raised by `gen_server:call/2`
against a missing or restarting `beamtalk_xref`, degrading to a no-op. Other
exits/errors propagate.
""".
-spec safe_xref(fun(() -> ok)) -> ok.
safe_xref(Fun) ->
    %% bt-catcher-audit: not-applicable-no-block - Fun is a gen_server call to beamtalk_xref
    try Fun() of
        ok -> ok
    catch
        exit:{noproc, _} -> ok;
        exit:noproc -> ok;
        exit:{normal, _} -> ok;
        exit:{{nodedown, _}, _} -> ok
    end.

%%====================================================================
%% Compiler server
%%====================================================================

-doc """
ADR 0050 Phase 3: notify the compiler server of a class (re)registration —
shared by the on_load registration path (`handle_call({start, …})`) and the
hot-reload/redefinition path (`handle_call({update_class, …})`), which both
call this with the class's fresh `__beamtalk_meta/0` map.

Cast is fire-and-forget — silently dropped if the compiler server is not
running. `beamtalk_compiler` is not started at all in release/escript mode
(ADR 0099 §4 / ADR 0125 §1.5's default), and the hot-reload path is hit by
ordinary stdlib bootstrap too (the metaclass-tower stubs are registered,
then `update_class`'d when their compiled `.bt` modules load) — not just
live hot-reload — so an `undef` here must never crash the class process.
""".
-spec notify_compiler_server_register(class_name(), map()) -> ok.
notify_compiler_server_register(ClassName, Meta) ->
    try
        beamtalk_compiler_server:register_class(ClassName, Meta)
    catch
        error:undef ->
            %% `include-compiler = false` is the release/escript default
            %% (ADR 0125 §1.2), so this fires for every class at every boot
            %% in the common case — expected steady state, not an
            %% operational concern, hence ?LOG_DEBUG rather than
            %% ?LOG_WARNING (matching the sibling `update_class` guard just
            %% above, which treats the same "compiler server may be absent"
            %% condition the same way).
            ?LOG_DEBUG(#{
                event => register_class_undef,
                class => ClassName,
                reason => "beamtalk_compiler_server:register_class/2 not available",
                domain => [beamtalk, runtime]
            }),
            ok
    end.

%%====================================================================
%% Metadata row and process-dictionary caches
%%====================================================================

-doc """
Write a class's identity facts — module, superclass, is_abstract, and local
class-method selectors — to the process-dictionary caches and the unified
`beamtalk_class_metadata` ETS row in one place.

`init/1` and `apply_class_info/2` used to each perform these writes via
several separate `put/2` calls plus a trailing
`beamtalk_class_metadata:insert/5`, spread across many lines with unrelated
code in between — a mid-function reader (dispatch, reflection, a hot-patch
racing the same class process) could observe the PD and ETS copies
disagreeing. Routing both call sites through this one function collapses that
window to this call's own internal ordering (PD first — the self-view used by
self-dispatch inside this gen_server — ETS last, published for other
processes), so a future edit cannot silently reintroduce "module written
here, superclass written 40 lines later" drift.

`Mode` selects the ETS write:

* `create` — `beamtalk_class_metadata:insert/5`, a full fresh row. Used by
  `init/1`; `has_runtime_class_methods` starts `false` (a brand-new row has
  no funs yet — the caller's own `seed_runtime_class_methods/2` call raises
  the gate afterward if needed).
* `merge` — `beamtalk_class_metadata:merge_identity/5`, a per-field update
  that leaves `has_runtime_class_methods` untouched. Used by
  `apply_class_info/2`'s reload path; a caller that needs to drop the gate
  (e.g. because it just purged the funs table) must call
  `beamtalk_class_metadata:reset_runtime_class_methods/1` explicitly rather
  than relying on this call's side effect.
""".
-spec sync_identity(
    class_name(), atom(), class_name() | none, boolean(), [selector()], create | merge
) -> ok.
sync_identity(ClassName, Module, Superclass, IsAbstract, Selectors, Mode) ->
    put(beamtalk_class_module, Module),
    put(beamtalk_class_is_abstract, IsAbstract),
    put(beamtalk_class_superclass, Superclass),
    case Mode of
        create ->
            beamtalk_class_metadata:insert(ClassName, Module, Selectors, Superclass, IsAbstract);
        merge ->
            beamtalk_class_metadata:merge_identity(
                ClassName, Module, Selectors, Superclass, IsAbstract
            )
    end.

-doc """
Publish the compiled `direct_constructors` eligibility set.

Copies `direct_constructors` from `Meta` into the unified metadata row, where
`beamtalk_class_dispatch:class_send/3` reads it without a gen_server hop. Meta
without the key (dynamic classes, older modules) clears the set, so a reload
that makes a class ineligible takes effect immediately. Only `new`/`new:`
atoms are kept; anything else in the list is dropped defensively.

Also publishes the class's own `defines_user_class_new0` (BT-4479) and
`defines_user_class_new1` facts (BT-4409),
which descendants' dynamic `new:` reads through
`beamtalk_class_metadata:chain_has_user_new1/1`; Meta without it stores
`undefined` (unknown), so the chain walk fails closed.
""".
-spec sync_direct_constructors(class_name(), map()) -> ok.
sync_direct_constructors(ClassName, Meta) ->
    Ctors =
        case maps:get(direct_constructors, Meta, []) of
            L when is_list(L) -> [S || S <- L, S =:= new orelse S =:= 'new:'];
            _ -> []
        end,
    DefinesNew1 =
        case maps:get(defines_user_class_new1, Meta, undefined) of
            B when is_boolean(B) -> B;
            _ -> undefined
        end,
    beamtalk_class_metadata:put_defines_user_class_new1(ClassName, DefinesNew1),
    %% BT-4479: the unary counterpart; absent reads as unknown (fail-open).
    DefinesNew0 =
        case maps:get(defines_user_class_new0, Meta, undefined) of
            B0 when is_boolean(B0) -> B0;
            _ -> undefined
        end,
    beamtalk_class_metadata:put_defines_user_class_new0(ClassName, DefinesNew0),
    beamtalk_class_metadata:put_direct_constructors(ClassName, Ctors).

-doc """
Publish the class's certified literal class-side bodies (BT-4363, BT-4504):
`constant_class_methods` from its `__beamtalk_meta/0`, keeping only
`Selector => constant_value()` entries
(`beamtalk_class_metadata:is_constant_value/1`); `#{}` when absent (a ClassBuilder class, an
older module). A constant-value static call key is certified against it.
""".
-spec sync_constant_class_methods(class_name(), map()) -> ok.
sync_constant_class_methods(ClassName, Meta) ->
    Constants =
        case maps:get(constant_class_methods, Meta, #{}) of
            M when is_map(M) ->
                maps:filter(
                    fun(K, V) ->
                        is_atom(K) andalso beamtalk_class_metadata:is_constant_value(V)
                    end,
                    M
                );
            _ ->
                #{}
        end,
    beamtalk_class_metadata:put_constant_class_methods(ClassName, Constants).

-doc """
Publish the class's caller-safe signal primitives (BT-4493):
`caller_safe_class_primitives` from its `__beamtalk_meta/0`, keeping only
`Selector => Primitive` atom entries; `#{}` when absent. A caller-safe raise
static call key is certified against it, not against the module's own meta,
which is the old code during a reload's `-on_load`.
""".
-spec sync_caller_safe_primitives(class_name(), map()) -> ok.
sync_caller_safe_primitives(ClassName, Meta) ->
    Primitives =
        case maps:get(caller_safe_class_primitives, Meta, #{}) of
            M when is_map(M) ->
                maps:filter(fun(K, V) -> is_atom(K) andalso is_atom(V) end, M);
            _ ->
                #{}
        end,
    beamtalk_class_metadata:put_caller_safe_primitives(ClassName, Primitives).

-doc """
Publish the class-variable fact and the process-bound selector list (ADR 0142
§1, BT-4299) into the unified metadata row.

Whether the class declares class variables is read from the compiled
`__beamtalk_meta/0`'s `class_field_kinds` (every declared `classState:`,
`late` ones included); a class without it (a ClassBuilder class, an older
module) falls back to the keys of its `classState` defaults. Never re-derived
from source. `beamtalk_class_metadata:put_class_variables/2` erases the
derived direct-call flag before a changed fact becomes visible.

`process_bound_class_selectors` is the generated list Rust owns; a module
without the key stores `undefined` (unknown), never a runtime copy.
""".
-spec sync_class_process_facts(class_name(), map(), map()) -> ok.
sync_class_process_facts(ClassName, Meta, ClassVarDefaults) ->
    HasClassVars =
        case maps:get(class_field_kinds, Meta, undefined) of
            Kinds when is_map(Kinds) -> map_size(Kinds) > 0;
            _ -> map_size(ClassVarDefaults) > 0
        end,
    beamtalk_class_metadata:put_class_variables(ClassName, HasClassVars),
    ProcessBound =
        case maps:get(process_bound_class_selectors, Meta, undefined) of
            L when is_list(L) -> [S || S <- L, is_atom(S)];
            _ -> undefined
        end,
    beamtalk_class_metadata:put_process_bound_selectors(ClassName, ProcessBound).

-doc """
Warn when `ClassName` has just gained a class-side `new:` that its subclasses'
baked `direct_constructors` verdicts do not know about (BT-4126).

A subclass's `new:` verdict is baked only when no ancestor defined a
class-side `new:` at its compile time (Rust's `chain_has_user_new_init`).
Since BT-4409 the chain is also decided at run time: a dynamic `new:` builds
in the caller only while `beamtalk_class_metadata:chain_has_user_new1/1`
finds no ancestor publishing a user `new:` (nor an ancestor's runtime-fun
`new:`), so if `ClassName` *gains* one (by a compiled reload or a runtime-fun
`put_class_method`), its subclasses route through it at once, without being
recompiled. This warning is therefore informational: it names the subclasses
whose baked verdict no longer matches their ancestors, which a recompile
brings back in line. Losing a `new:` needs no warning.

"Has a class-side `new:`" is read from the class-method map, which for a
compiled class is the module's `class_method_info` (user primary methods only,
no synthesized `new:`) and for a runtime patch includes the installed fun.
`class_new:/2` exports cannot be used because `erlang:function_exported/3`
answers `false` while a reloading module's `on_load` is still running, which
is when a reload arrives.
""".
-spec warn_stale_subclass_constructors(class_name(), map(), map()) -> ok.
warn_stale_subclass_constructors(ClassName, OldClassMethods, NewClassMethods) ->
    GainedNew =
        not maps:is_key('new:', OldClassMethods) andalso maps:is_key('new:', NewClassMethods),
    Stale =
        case GainedNew of
            true ->
                [
                    Sub
                 || Sub <- beamtalk_class_registry:all_subclasses(ClassName),
                    lists:member('new:', beamtalk_class_metadata:lookup_direct_constructors(Sub))
                ];
            false ->
                []
        end,
    case Stale of
        [] ->
            ok;
        _ ->
            ?LOG_WARNING(
                "Class ~p gained a class-side new:, but subclasses ~p "
                "were compiled without it and still build new: directly, "
                "bypassing the override. Recompile them.",
                [ClassName, Stale],
                #{
                    class => ClassName,
                    stale_direct_constructors => Stale,
                    domain => [beamtalk, runtime]
                }
            )
    end.

-spec is_runtime_fun_info(term()) -> boolean().
is_runtime_fun_info(Info) ->
    is_map(Info) andalso maps:is_key(block, Info).

-doc """
Seed the runtime class-method fun retrieval store from a class_methods map.

ADR 0084: picks out the entries that carry a `block` (runtime/builder
funs, as opposed to compiled `#{arity => N}` references), writes each into the
metadata retrieval store, then sets the per-class gate flag. Classes with no fun
entries leave the gate flag false so their dispatch never reads the funs table.
""".
-spec seed_runtime_class_methods(class_name(), map()) -> ok.
seed_runtime_class_methods(ClassName, ClassMethods) ->
    Funs = maps:filter(fun(_Selector, Info) -> is_runtime_fun_info(Info) end, ClassMethods),
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
Mirror this class's reflection state into its metadata row (BT-3826).

Publishes the local instance-method selectors, declared instance variables and
both return-type maps, so `instance_methods_safe/1`, `instance_variables_safe/1`,
`has_method/2` and `beamtalk_class_registry:get_method_return_type/2` can answer
for this class without a `gen_server:call` -- which, issued from inside another
class's handler, deadlocks two busy classes against each other. Called after
every state change to those four fields: `init/1`, `put_method`,
`put_class_method`, `update_class` and `rename_class`. Also publishes the class
doc (BT-4262; `set_doc` publishes it directly).
""".
-spec publish_reflection(reflection_view()) -> ok.
publish_reflection(
    #{
        name := ClassName,
        instance_methods := InstanceMethods,
        fields := Fields,
        method_return_types := MethodReturnTypes,
        class_method_return_types := ClassMethodReturnTypes,
        class_methods := ClassMethods,
        is_sealed := IsSealed,
        is_typed := IsTyped,
        is_internal := IsInternal,
        shape_version := ShapeVersion,
        doc := Doc
    } = View
) ->
    beamtalk_class_metadata:put_reflection(ClassName, #{
        instance_methods => maps:keys(InstanceMethods),
        instance_variables => Fields,
        method_return_types => MethodReturnTypes,
        class_method_return_types => ClassMethodReturnTypes
    }),
    %% BT-3832: flags, and the per-method records behind CompiledMethod lookups.
    beamtalk_class_metadata:put_flags(ClassName, #{
        is_sealed => IsSealed,
        is_typed => IsTyped,
        is_internal => IsInternal,
        shape_version => ShapeVersion
    }),
    beamtalk_class_metadata:put_method_records(
        ClassName, instance, method_records(instance, InstanceMethods, View)
    ),
    beamtalk_class_metadata:put_method_records(
        ClassName, class, method_records(class, ClassMethods, View)
    ),
    %% BT-4262: the class doc, read by `beamtalk_behaviour_intrinsics:classDoc/1`.
    beamtalk_class_metadata:put_class_doc(ClassName, Doc).

%% `selector => #{info, source, signature, doc}` for one side of a class.
-spec method_records(instance | class, #{selector() => map()}, reflection_view()) ->
    #{selector() => beamtalk_class_metadata:method_record()}.
method_records(Side, Methods, View) ->
    maps:map(fun(Sel, Info) -> method_record(Side, Sel, Info, View) end, Methods).

-spec method_record(instance | class, selector(), map(), reflection_view()) ->
    beamtalk_class_metadata:method_record().
method_record(instance, Sel, Info, #{
    method_source := Source, method_signatures := Sigs, method_docs := Docs
}) ->
    #{
        info => Info,
        source => maps:get(Sel, Source, <<"">>),
        signature => maps:get(Sel, Sigs, nil),
        doc => maps:get(Sel, Docs, nil)
    };
method_record(class, Sel, Info, #{
    class_method_source := Source,
    class_method_signatures := Sigs,
    class_method_docs := Docs
}) ->
    #{
        info => Info,
        source => maps:get(Sel, Source, <<"">>),
        signature => maps:get(Sel, Sigs, nil),
        doc => maps:get(Sel, Docs, nil)
    }.

-doc """
Notify the compiler server of a hot-patch.

ADR 0050 Phase 3: `__beamtalk_meta/0` is stale after hot-patching (it reflects
compile-time metadata only).  This function synthesizes a minimal metadata map
from the current live class state (a `reflection_view()`) and casts it to the compiler server.
The compiler server replaces the class entry wholesale.
Fire-and-forget — silently dropped if the server is not running.
""".
-spec notify_hot_patch(reflection_view()) -> ok.
notify_hot_patch(#{
    name := ClassName,
    module := Module,
    superclass := Superclass,
    is_sealed := IsSealed,
    is_abstract := IsAbstract,
    instance_methods := InstanceMethods,
    class_methods := ClassMethods,
    fields := Fields,
    method_return_types := MethodReturnTypes,
    class_method_return_types := ClassMethodReturnTypes
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
