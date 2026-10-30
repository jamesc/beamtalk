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

Three independent kinds:
- `extension` keyed by the class-object tag (e.g. `'Counter class'`): set while
  any extension is registered under that tag.
- `runtime_fun` keyed by the class name: set while the class has runtime-
  installed class-method funs.
- `class_vars` keyed by the class name (ADR 0142 §1 rule 1, BT-4299): set while
  the class declares class variables (`classState:`). Not a shadow: it mirrors
  the class-metadata fact `beamtalk_class_metadata:put_class_variables/2`
  publishes from the compiled `__beamtalk_meta/0` (`class_field_kinds`) or the
  builder's `classState`, never re-derived from source here. Its single writer
  is that function, run from the class's own gen_server (`init/1`, reload,
  rename), like `runtime_fun`. Absent means "declares none" (only a row that
  never went through a class process lacks the fact).

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
cannot collide with another module's `persistent_term` key). It is present
exactly while the class is ready, has no `extension` flag and no `runtime_fun`
flag; absent means "take the always-correct hierarchy walk". Its value folds
the `class_vars` input in (BT-4299), so both questions stay one read:
- `true`: no shadow and no class variables. `direct_call_ok/2` holds (ADR 0142
  §1 rule 1: the method may run outside the class process).
- `class_vars`: no shadow, but the class declares class variables, so only its
  class process may run the method. `direct_call_ok/2` is false;
  `shadow_free/2` (the open class-side self-send guard, which already runs
  inside the class process) is still true, so that path is unchanged.

Single writer per derived key, by construction, not by convention: every write
and erase of it runs under one per-tag `global` lock (`with_tag_lock/2`), and the
lock holder re-derives the answer from the raw flags above, never from the
reader's view.
- Invalidation (`set/2`, any kind): under the lock, raise the raw flag, then
  erase the derived key. Both happen before the caller makes the change visible
  (`beamtalk_extensions:register/5` raises the flag before its row insert;
  `beamtalk_class_metadata:set_runtime_class_methods/2` before the gate opens;
  `beamtalk_class_metadata:put_class_variables/2` before the row field), so a
  send that can see a shadow or a class variable cannot see a derived `true`.
- Erasure on every other class-metadata write (`invalidate/1`, ADR 0142 §2,
  BT-4299): under the lock, erase the derived key. Called on reload
  (`beamtalk_object_class:apply_class_info/2`), a class-side `removeSelector:`,
  a runtime `put_class_method`, and row removal
  (`beamtalk_class_metadata:delete/1`); a class-variable change erases through
  `set_class_vars/2`. This does **not** hold the fast path off until the write
  is visible: a concurrent `direct_call_ok/2` or `shadow_free/2` miss
  re-derives from the raw flags at once, and answers what they say. The derived
  key is a pure function of the raw flags, so erasing it changes no answer; it
  only forces the next send to re-derive. A fact that must bar the fast path
  during a write therefore has to be a raw input `derive/2` reads, raised before
  the write (as `class_vars` is). Other per-class answers (the in-caller
  class-method owner, BT-4300) live in `beamtalk_class_derived_cache`, whose
  generation counter, not this erasure, keeps them consistent with the write.
- Install (`direct_call_ok/2` or `shadow_free/2` miss, lazy): outside the lock,
  cheap pre-check of the raw flags (a permanently shadowed class never takes the
  lock per send); then under the lock re-derive from readiness and the raw flags
  and put the key (`true` or `class_vars`). An
  install that ran before a `set/2` is erased by it; one that runs after sees the
  raw flag and declines. `clear/2` does not touch the derived key: a stale
  "unsafe" only costs the walk until the next send re-installs.
- The key is never rewritten while present (either value answers both guards
  without an install), so a send never pays a global scan.

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
    shadow_free/2,
    is_direct_flag_set/1,
    direct_flag_value/1,
    invalidate/1,
    set_class_vars/2,
    mark_ready/0,
    is_ready/0,
    with_tag_lock/2
]).

-define(READY_KEY, beamtalk_class_shadow_ready).

%% BT-3700: the send-path guard (`direct_call_ok/2`) is hot; the slow-path
%% helpers are inlined so the miss branch is not a chain of calls.
-compile({inline, [key/2, is_set/2, is_shadowed/2, is_ready/0]}).

-type kind() :: extension | runtime_fun | class_vars.
%% The derived flag's installed value (BT-4299; see the moduledoc).
-type derived() :: true | class_vars.
-export_type([kind/0, derived/0]).

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
class_tag(runtime_fun, Name) -> beamtalk_class_registry:class_object_tag(Name);
class_tag(class_vars, Name) -> beamtalk_class_registry:class_object_tag(Name).

-spec erase_direct_flag(atom()) -> ok.
erase_direct_flag(Tag) ->
    case persistent_term:get(Tag, false) of
        false ->
            ok;
        _ ->
            _ = persistent_term:erase(Tag),
            ok
    end.

-doc """
Erase `ClassName`'s derived direct-call flag under the per-tag lock (ADR 0142
§2, BT-4299), so the next send re-derives it from the raw flags. Called on a
class-metadata write for the class: reload, a class-side `removeSelector:`, a
runtime `put_class_method`, row removal. It does not keep the fast path off
while the write lands: a concurrent miss re-derives from the raw flags at once
(see the moduledoc), so anything that must bar the fast path is a raw flag,
raised before the write. Skips the lock when no derived flag is installed, like
`set/2`: an install racing this is one that read the raw flags, which this does
not change. Must never be called from inside `with_tag_lock/2`.
""".
-spec invalidate(atom()) -> ok.
invalidate(ClassName) ->
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    case persistent_term:get(Tag, false) of
        false -> ok;
        _ -> with_tag_lock(Tag, fun() -> erase_direct_flag(Tag) end)
    end.

-doc """
Mirror the class-metadata fact "`ClassName` declares class variables" into the
`class_vars` raw flag (BT-4299). Every transition erases the derived flag under
the per-tag lock before returning: raising the fact makes `direct_call_ok/2`
false before the caller publishes it; dropping it lets the next send re-derive
`true`. No-op when unchanged. Called only by
`beamtalk_class_metadata:put_class_variables/2`.
""".
-spec set_class_vars(atom(), boolean()) -> ok.
set_class_vars(ClassName, true) ->
    set(class_vars, ClassName);
set_class_vars(ClassName, false) ->
    Key = key(class_vars, ClassName),
    case persistent_term:get(Key, false) of
        false ->
            ok;
        true ->
            Tag = class_tag(class_vars, ClassName),
            with_tag_lock(Tag, fun() ->
                _ = persistent_term:erase(Key),
                erase_direct_flag(Tag)
            end)
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
ADR 0142 §1 rule 1 (BT-4299): true exactly when the class is ready, nothing
shadows its compiled class methods (no `extension`, no `runtime_fun`) and it
declares no class variables. One `persistent_term` read of the derived flag
keyed by `ClassTag`; a miss falls to `install_direct_flag/2`, which derives the
value from the raw flags and installs it when the class is not shadowed.
""".
-spec direct_call_ok(atom(), atom()) -> boolean().
direct_call_ok(ClassTag, ClassName) ->
    case persistent_term:get(ClassTag, false) of
        true -> true;
        class_vars -> false;
        _ -> install_direct_flag(ClassTag, ClassName) =:= true
    end.

-doc """
BT-3700: the guard of the compiled class-side self-send fast path, which runs
inside the class process already: true while the class is ready and nothing
shadows its compiled class methods, whatever its class variables. The same
single read of the derived flag as `direct_call_ok/2`; either installed value
means "not shadowed".
""".
-spec shadow_free(atom(), atom()) -> boolean().
shadow_free(ClassTag, ClassName) ->
    case persistent_term:get(ClassTag, false) of
        false -> install_direct_flag(ClassTag, ClassName) =/= false;
        _ -> true
    end.

-doc "True while the derived flag for `ClassTag` is installed, with either value (test hook).".
-spec is_direct_flag_set(atom()) -> boolean().
is_direct_flag_set(ClassTag) ->
    persistent_term:get(ClassTag, false) =/= false.

-doc "The installed derived value for `ClassTag`, `false` when absent (test hook).".
-spec direct_flag_value(atom()) -> derived() | false.
direct_flag_value(ClassTag) ->
    persistent_term:get(ClassTag, false).

%% The derived value the raw flags imply; `false` while not ready or shadowed.
-spec derive(atom(), atom()) -> derived() | false.
derive(ClassTag, ClassName) ->
    case is_ready() andalso not is_shadowed(ClassTag, ClassName) of
        false ->
            false;
        true ->
            case is_set(class_vars, ClassName) of
                true -> class_vars;
                false -> true
            end
    end.

-spec install_direct_flag(atom(), atom()) -> derived() | false.
install_direct_flag(ClassTag, ClassName) ->
    %% Cheap pre-check without the lock: a shadowed or not-yet-ready class must
    %% not take a global lock per send.
    case derive(ClassTag, ClassName) of
        false ->
            false;
        _ ->
            with_tag_lock(ClassTag, fun() ->
                case derive(ClassTag, ClassName) of
                    false ->
                        false;
                    Value ->
                        persistent_term:put(ClassTag, Value),
                        Value
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
