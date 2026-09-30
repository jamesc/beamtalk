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
`persistent_term` writes that change a value trigger a global scan, so only
actual transitions write (set when unset, erase when set); both are rare
(registration / class (re)definition), never on the send path.
""".

-export([
    set/2,
    clear/2,
    is_set/2,
    is_shadowed/2
]).

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
