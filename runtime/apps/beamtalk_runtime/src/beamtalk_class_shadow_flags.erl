%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_shadow_flags).

%%% **DDD Context:** Object System Context

-moduledoc """
Cheap per-class "has shadows" flags for the compiled class-side self-send
fast path (BT-3669).

A compiled class-side `self foo` may call the defining class's `class_foo`
directly only when nothing shadows it: a class-side extension registered on the
class (`beamtalk_extensions`) or a runtime-installed class-method fun
(`beamtalk_class_metadata`, ADR 0084). Checking those used to cost two ETS
reads per send. Each writer now mirrors its state into a `persistent_term`
flag, so the guard in `beamtalk_class_dispatch:class_self_direct_ok/4` is two
constant-time term reads with no ETS access.

Two independent kinds:
- `extension` keyed by the class-object tag (e.g. `'Counter class'`): set while
  any extension is registered under that tag.
- `runtime_fun` keyed by the class name: set while the class has runtime-
  installed class-method funs.

The flags are conservative: a stale `true` only costs the slow path (the
hierarchy walk is always correct); `runtime_fun` writers set the flag *before* making the
shadow visible and clear it only *after* the gate is closed; `extension`
transitions are serialized per tag under a lock in `beamtalk_extensions`
(an unlocked erase-then-recheck could leave the flag false beside a visible row).
Concurrency: the `runtime_fun` flag is unlocked and relies on a single writer
per class. All its writers (`insert`, `set_runtime_class_methods`,
`reset_runtime_class_methods`, `delete` in `beamtalk_class_metadata`) run inside
the class's own `beamtalk_object_class` gen_server, so they never interleave. A
future writer outside the class process would need a lock: e.g. a `set` racing
a `reset` could have the reset close the gate and then clear the flag just after
the set raised it (or vice versa), leaving the flag false while the gate is open.
(`extension` flags, written from arbitrary processes, are instead serialized per
tag in `beamtalk_extensions`.)

`persistent_term` writes that change a value trigger a global scan, so only
actual transitions write (set when unset, erase when set); both are rare
(registration / class (re)definition), never on the send path.
""".

-export([
    set/2,
    clear/2,
    is_set/2,
    is_shadowed/2,
    direct_call_ok/2,
    mark_ready/0,
    is_ready/0
]).

-define(READY_KEY, beamtalk_class_shadow_ready).

%% BT-3690: the send-path guard (`direct_call_ok/2`) is hot; inlining keeps it one
%% function with three `persistent_term` reads and no intermediate calls.
-compile({inline, [key/2, is_set/2, is_shadowed/2, is_ready/0]}).

-type kind() :: extension | runtime_fun.
-export_type([kind/0]).

-spec key(kind(), atom()) -> {beamtalk_class_shadow, kind(), atom()}.
key(Kind, Name) -> {beamtalk_class_shadow, Kind, Name}.

-doc "Mark `Name` as having a shadow of `Kind`. No-op when already set.".
-spec set(kind(), atom()) -> ok.
set(Kind, Name) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        true -> ok;
        false -> persistent_term:put(Key, true)
    end.

-doc "Clear the `Kind` flag for `Name`. No-op when not set.".
-spec clear(kind(), atom()) -> ok.
clear(Kind, Name) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        true ->
            _ = persistent_term:erase(Key),
            ok;
        false ->
            ok
    end.

-doc "Read one flag.".
-spec is_set(kind(), atom()) -> boolean().
is_set(Kind, Name) ->
    persistent_term:get(key(Kind, Name), false).

-doc """
True when a class-side extension under `ClassTag` or a runtime class-method fun
on `ClassName` may shadow a compiled class method.
""".
-spec is_shadowed(atom(), atom()) -> boolean().
is_shadowed(ClassTag, ClassName) ->
    is_set(extension, ClassTag) orelse is_set(runtime_fun, ClassName).

-doc """
BT-3690: the guard of the compiled class-side self-send fast path: the registry
is initialised (`is_ready/0`) and nothing shadows the compiled method
(`is_shadowed/2`). The shadow flags are read first: an unset flag is a missing
`persistent_term` key, which is cheaper to look up than the present readiness
flag, and the answer is the same whichever order the three reads run in.
""".
-spec direct_call_ok(atom(), atom()) -> boolean().
direct_call_ok(ClassTag, ClassName) ->
    not is_shadowed(ClassTag, ClassName) andalso is_ready().

-doc """
Mark the flags authoritative: the extension registry exists, so an unset
`extension` flag really means "no extension". Called once from
`beamtalk_extensions:init/0`. Until then (early bootstrap) the compiled
class-side self-send fast path declines to the always-correct hierarchy walk.

BT-3676: this is a `persistent_term` read on the send path, replacing an
`ets:whereis/1` of the extension table (measurably dearer: a registry lookup
with a lock per send). The flag is never cleared; a later loss of the table
cannot make it wrong, because no extension can be registered without the table.
""".
-spec mark_ready() -> ok.
mark_ready() ->
    case persistent_term:get(?READY_KEY, false) of
        true -> ok;
        false -> persistent_term:put(?READY_KEY, true)
    end.

-doc "True once `mark_ready/0` ran (the extension registry was initialised).".
-spec is_ready() -> boolean().
is_ready() ->
    persistent_term:get(?READY_KEY, false).
