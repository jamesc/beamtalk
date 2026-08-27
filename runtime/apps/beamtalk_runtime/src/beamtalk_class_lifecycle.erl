%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_lifecycle).

%%% **DDD Context:** Object System Context

-moduledoc """
Single teardown path for class removal (BT-3105).

`beamtalk_behaviour_intrinsics:classRemoveFromSystemByName/1` stops live
actors, stops the class gen_server (whose `terminate/2` removes the
`beamtalk_class_metadata` row, the ETS hierarchy entry, and the pg group),
and purges the BEAM module — but that leaves five *derived* registries
holding stale rows for the removed class:

- `beamtalk_xref` — per-method cross-reference rows (only `update_class` →
  `refresh_xref` purged these; class removal never did).
- `beamtalk_extensions` — extension methods registered *on* the class,
  across all three of its tables (main registry, sources, conflict
  history). Left live, these stay dispatchable, and resurrect if a class of
  the same name is later re-created (extensions are keyed by class *name*
  atom, not by the class's pid/module).
- `beamtalk_protocol_registry` — a protocol whose defining module is purged
  had no unregister path at all until this existed.
- `beamtalk_compiler_server` — the ambient class-metadata cache
  (`register_class/2`'s accumulator, BT-2916) never shrank, so the compiler
  kept type-checking against classes long gone from the runtime.
- `beamtalk_workspace_meta` — the `class_sources` map (only `set_class_source/2`
  existed; no delete), so a removed class's source text leaked, including
  into the persisted `metadata.json`.

`class_removed/2` is the single call site `classRemoveFromSystemByName/1`
invokes once its safety checks pass and the class process has been stopped;
each purge below targets a disjoint table/process, so there is no ordering
requirement between them.

`purge_class_registries/1` factors out the four *name-keyed* purges (xref,
extensions, compiler cache, workspace class source) as their own exported
step, reused by `classRenameTo` (ADR 0114, BT-3278) to retire a class's OLD
identity after a rename installs the new one. Rename deliberately does NOT
call `class_removed/2` itself for this: `purge_protocol/1` is keyed by
`Module`, not by class name, and an in-memory-only rename (no disk flush,
BT-3271) keeps the SAME BEAM module atom serving the class under its NEW
name — purging protocols for that still-live module would wrongly erase
registrations the renamed class still needs.

## Dependency direction

`beamtalk_runtime` (this module's app) sits *below* `beamtalk_compiler` and
`beamtalk_workspace` in the dependency graph — neither appears in
`beamtalk_runtime.app.src`'s `applications` list, and this module must never
gain a compile-time dependency on either (a plain `Module:function()` call
would count, even though Erlang has no linker to enforce it). The two
derived registries that live in those upper apps are therefore purged with a
direct `gen_server:cast/2` to the well-known *registered name* — the same
"registered-name trick" `beamtalk_behaviour_intrinsics:publish_class_removed/2`
already uses for `beamtalk_workspace_meta`'s `unregister_module` cast (and
`stop_class_actors/1` / `classReload/1` use elsewhere). `gen_server:cast/2`
never raises for an unregistered name — it degrades to a silent no-op — so
these two casts need no `noproc` guard, matching `publish_class_removed/2`'s
existing call.

The three registries that live in `beamtalk_runtime` itself (xref,
extensions, protocol registry) are purged via ordinary direct calls — no
dependency-direction concern, since they live in this app.
""".

-include_lib("kernel/include/logger.hrl").

-export([class_removed/2, purge_class_registries/1]).

-doc """
Purge every derived registry entry for a class removed via
`classRemoveFromSystemByName/1`.

`ClassName` is the removed class's name atom; `Module` is the BEAM module
that defined it. Called after the class gen_server has been stopped and the
BEAM module purged, before `nil` is returned to the caller. Always returns
`ok` — every individual purge already degrades silently when its target
registry/process is absent (early bootstrap, a minimal embedded runtime, a
non-REPL run), so class removal itself is never blocked by this cleanup.
""".
-spec class_removed(atom(), atom()) -> ok.
class_removed(ClassName, Module) ->
    purge_class_registries(ClassName),
    purge_protocol(Module),
    ok.

-doc """
Purge the four *name-keyed* derived registries for `ClassName` — xref,
extensions, compiler cache, and workspace class source — WITHOUT touching
`beamtalk_protocol_registry` (keyed by `Module`, not by class name; see this
module's doc for why `classRenameTo` must not call `class_removed/2` itself).

Used by `class_removed/2` for a full class removal, and directly by
`beamtalk_behaviour_intrinsics:classRenameTo/2` (ADR 0114, BT-3278) to retire
a class's OLD name after installing it under a new one.
""".
-spec purge_class_registries(atom()) -> ok.
purge_class_registries(ClassName) ->
    purge_xref(ClassName),
    purge_extensions(ClassName),
    purge_compiler_cache(ClassName),
    purge_workspace_class_source(ClassName),
    ok.

-doc """
Purge `ClassName`'s rows from `beamtalk_xref` — both instance- and
class-side method entries, and its sends/references. Run best-effort via
`beamtalk_extensions:safe_xref/1` (BT-2301's existing helper) so a dead or
restarting `beamtalk_xref` cannot raise out of this stage and skip the
other four purges below it.
""".
-spec purge_xref(atom()) -> ok.
purge_xref(ClassName) ->
    beamtalk_extensions:safe_xref(fun() -> beamtalk_xref:purge_class(ClassName) end).

-doc """
Purge every extension registered on `ClassName` — instance-side (keyed by
the class name atom) and class-side (keyed by the metaclass tag, e.g.
`'Counter class'`), which the extension registry stores as two distinct ETS
keys (mirrors `beamtalk_protocol_registry:class_has_class_method/2`'s
lookup convention for the same split).
""".
-spec purge_extensions(atom()) -> ok.
purge_extensions(ClassName) ->
    beamtalk_extensions:purge_class(ClassName),
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    beamtalk_extensions:purge_class(ClassTag),
    ok.

-doc """
Unregister every protocol defined by `Module` (the removed class's BEAM
module) from `beamtalk_protocol_registry`.
""".
-spec purge_protocol(atom()) -> ok.
purge_protocol(Module) ->
    beamtalk_protocol_registry:unregister_protocol(Module).

-doc """
Drop `ClassName` from `beamtalk_compiler_server`'s ambient class cache via
the registered-name cast (see this module's doc — no compile-time
dependency on `beamtalk_compiler`).
""".
-spec purge_compiler_cache(atom()) -> ok.
purge_compiler_cache(ClassName) ->
    gen_server:cast(beamtalk_compiler_server, {remove_class, ClassName}),
    ok.

-doc """
Drop `ClassName`'s stored source text from `beamtalk_workspace_meta`'s
`class_sources` map via the registered-name cast (see this module's doc —
no compile-time dependency on `beamtalk_workspace`). `class_sources` is
keyed by binary class name, so `ClassName` is converted before sending.
""".
-spec purge_workspace_class_source(atom()) -> ok.
purge_workspace_class_source(ClassName) ->
    ClassNameBin = atom_to_binary(ClassName, utf8),
    gen_server:cast(beamtalk_workspace_meta, {remove_class_source, ClassNameBin}),
    ok.
