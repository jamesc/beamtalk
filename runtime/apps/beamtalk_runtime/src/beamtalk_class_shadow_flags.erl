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
flag, so the guard in `beamtalk_class_dispatch:class_self_direct_ok/4` is one
constant-time term read with no ETS access (BT-3700 folded the reads; see below).

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

BT-3700: the send-path guard reads ONE key, the derived "direct call is safe"
flag, keyed by the bare class-object tag (`'Counter class'`, a bare atom: hashing
an atom key is ~3x cheaper than a tuple key, and the tag contains a space so it
cannot collide with another module's `persistent_term` key). It is `true`
exactly while the class is ready, has no `extension` flag and no `runtime_fun`
flag; absent means "take the always-correct hierarchy walk".

Single writer per derived key, by construction, not by convention: every write
and erase of it runs under one per-tag `global` lock (`with_tag_lock/2`), and the
lock holder re-derives the answer from the raw flags above, never from the
reader's view.
- Invalidation (`set/2`, either kind): under the lock, raise the raw flag, then
  erase the derived key. Both happen before the caller makes the shadow visible
  (`beamtalk_extensions:register/5` raises the flag before its row insert;
  `beamtalk_class_metadata:set_runtime_class_methods/2` before the gate opens),
  so a send that can see a shadow cannot see a derived `true`.
- Install (`direct_call_ok/2` miss, lazy): outside the lock, cheap pre-check of
  the raw flags (a permanently shadowed class never takes the lock per send);
  then under the lock re-check ready and both raw flags and put the key. An
  install that ran before a `set/2` is erased by it; one that runs after sees the
  raw flag and declines. `clear/2` does not touch the derived key: a stale
  "unsafe" only costs the walk until the next send re-installs.
- The key is never rewritten while `true`, so a send never pays a global scan.

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
    is_direct_flag_set/1,
    mark_ready/0,
    is_ready/0,
    with_tag_lock/2
]).

-define(READY_KEY, beamtalk_class_shadow_ready).

%% BT-3700: the send-path guard (`direct_call_ok/2`) is hot; the slow-path
%% helpers are inlined so the miss branch is not a chain of calls.
-compile({inline, [key/2, is_set/2, is_shadowed/2, is_ready/0]}).

-type kind() :: extension | runtime_fun.
-export_type([kind/0]).

-spec key(kind(), atom()) -> {beamtalk_class_shadow, kind(), atom()}.
key(Kind, Name) -> {beamtalk_class_shadow, Kind, Name}.

-doc """
Mark `Name` as having a shadow of `Kind` and invalidate the class's derived
direct-call flag (BT-3700) before returning, so the caller can then make the
shadow visible. No-op when already set. Takes the per-tag lock itself; callers
may hold other locks (`beamtalk_extensions:with_shadow_lock/2`) but must never
call this from inside `with_tag_lock/2`.
""".
-spec set(kind(), atom()) -> ok.
set(Kind, Name) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            Tag = class_tag(Kind, Name),
            with_tag_lock(Tag, fun() ->
                persistent_term:put(Key, true),
                erase_direct_flag(Tag)
            end)
    end.

-spec class_tag(kind(), atom()) -> atom().
class_tag(extension, Tag) -> Tag;
class_tag(runtime_fun, Name) -> beamtalk_class_registry:class_object_tag(Name).

-spec erase_direct_flag(atom()) -> ok.
erase_direct_flag(Tag) ->
    case persistent_term:get(Tag, false) of
        true ->
            _ = persistent_term:erase(Tag),
            ok;
        _ ->
            ok
    end.

-doc """
Run `Fun` holding the per-tag lock that serializes every write of the derived
direct-call flag (BT-3700). Exported for tests that hold it deterministically.
""".
-spec with_tag_lock(atom(), fun(() -> T)) -> T.
with_tag_lock(Tag, Fun) ->
    global:trans({{?MODULE, direct_flag, Tag}, self()}, Fun, [node()], infinity).

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
BT-3700: the guard of the compiled class-side self-send fast path: one
`persistent_term` read of the derived flag keyed by `ClassTag`. A miss falls to
`install_direct_flag/2`, which derives the answer from the raw flags (ready, no
`extension`, no `runtime_fun`) and installs the flag when safe.
""".
-spec direct_call_ok(atom(), atom()) -> boolean().
direct_call_ok(ClassTag, ClassName) ->
    case persistent_term:get(ClassTag, false) of
        true -> true;
        _ -> install_direct_flag(ClassTag, ClassName)
    end.

-doc "True while the derived direct-call flag for `ClassTag` is installed (test hook).".
-spec is_direct_flag_set(atom()) -> boolean().
is_direct_flag_set(ClassTag) ->
    persistent_term:get(ClassTag, false) =:= true.

-spec raw_safe(atom(), atom()) -> boolean().
raw_safe(ClassTag, ClassName) ->
    is_ready() andalso not is_shadowed(ClassTag, ClassName).

-spec install_direct_flag(atom(), atom()) -> boolean().
install_direct_flag(ClassTag, ClassName) ->
    %% Cheap pre-check without the lock: a shadowed or not-yet-ready class must
    %% not take a global lock per send.
    case raw_safe(ClassTag, ClassName) of
        false ->
            false;
        true ->
            with_tag_lock(ClassTag, fun() ->
                case raw_safe(ClassTag, ClassName) of
                    true ->
                        persistent_term:put(ClassTag, true),
                        true;
                    false ->
                        false
                end
            end)
    end.

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
