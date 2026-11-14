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
BT-4808: the `runtime_fun` and `class_vars` flags hold their writer's pid (`set/3`)
rather than `true`, so the removal of a dead class erases them only while they
still name it (`clear_if_owner/3`); `extension` flags stay `true`.
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
the `class_vars` input in (BT-4299), taken over the whole chain (BT-4301), so
both questions stay one read:
- `true`: no shadow, and no class variables declared by the class or any
  ancestor. `direct_call_ok/2` holds (ADR 0142 §1 rules 1-2: the class's own
  methods may run outside the class process).
- `class_vars`: no shadow, but the class or an ancestor declares class
  variables, so only its class process may run the method (a subclass holds
  its ancestors' class variables in its own process, the rule
  `beamtalk_class_metadata:chain_has_class_variables/1` applies to dynamic
  sends and Rust's `ClassHierarchy::is_direct_call_eligible` to static ones).
  `direct_call_ok/2` is false; `shadow_free/2` (the class-side self-send
  guard, which runs wherever its calling method already runs) is still true,
  so that path is unchanged.

Single writer per derived key, by construction, not by convention: every write
and erase of it runs under one per-tag node-local FIFO lock (`beamtalk_fifo_lock`, `with_tag_lock/2`), and the
lock holder re-derives the answer from the raw flags above, never from the
reader's view.
- Invalidation (`set/2`, any kind): under the lock, raise the raw flag, then
  erase the derived key. Both happen before the caller makes the change visible
  (`beamtalk_extensions:register/5` raises the flag before its row insert;
  `beamtalk_class_metadata:set_runtime_class_methods/2` before the gate opens;
  `beamtalk_class_metadata:put_class_variables/2` before the row field), so a
  send that can see a shadow, or a class variable the class itself declares,
  cannot see the class's derived `true`.
- A class's derived key also reads its chain's published class-variable facts
  and superclass links, through `beamtalk_class_metadata:chain_has_class_variables/1`
  (the runtime's one answer, failing closed), so `invalidate/1` erases the
  class's descendants' keys too (each under its own tag lock, always taken,
  after the class's own). `beamtalk_class_metadata:put_class_variables/2` calls
  it once the row field is written, and a reload that re-parents a class calls
  it again once the new superclass is published
  (`beamtalk_object_class:apply_class_info/2`) (BT-4301). For a descendant this
  is erase-after-write, not raise-before-visible: between an ancestor's row
  write and the erase, a descendant's static send may still see its old
  `true` and run in the caller, the same window a reload has for its own new
  code. No stale value outlives the erase.
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

BT-4316: static call keys. A static class send that compiles to a guarded
direct call (ADR 0142 §2) is keyed by its call target, `{Module, FunName, Tag}`
(the call-site literal, passed in whole so the runtime never spells it; `Tag`
is the metaclass tag of the class the call site names). BT-4442: the targets
of one class share ONE `persistent_term` key, the atom `'<Tag> static calls'`
(`static_calls_key/1`), whose value maps each installed call-site literal to its class reference. The
call site reads that key once and matches its own literal in the map (a BEAM
map-match instruction, not a call), so the hit path is still one
`persistent_term` read. A target's value is the class reference the callee
receives as `ClassSelf`:
`#beamtalk_object{class = Tag, class_mod = ClassModule, pid = Pid}` (the class's
own module, which is `Module` unless the method is inherited), served from the
literal area without a copy. The tag is part of the key because the module is
not unique to a class over time: after `renameTo:` (same module, same process)
or when a removed class's module name is reused, a call site still naming the
old class must miss and ask for that class (`class_not_found`), not run with
the reference another class installed. While a target is in its class's map:
- the class's derived flag is `true` (`direct_call_ok/2` holds);
- `beamtalk_class_dispatch:in_caller_owner/2`, the dynamic path's answer,
  names this class as the receiver, `Module` as the defining module and
  `FunName` as the function, so a dynamic send would run exactly this
  compiled function in the caller. The definer is this class (a method the
  class defines) or, for BT-4270's caller-safe signal sends (`Error signal:
  m` running `Exception`'s compiled `class signal:`), an ancestor with no
  ADR 0084 runtime fun for the selector, whose writes erase this class's keys
  through `invalidate/1`'s descendant erase;
- `erlang:function_exported(Module, FunName, Arity)` holds;
- `Pid` is the class's live registered process.

BT-4270: a class-literal send of a caller-safe signal primitive (`Error
signal: m`) is keyed `{caller_safe_raise, Primitive, Module, FunName, Tag}`,
`Module:FunName` being the defining class's compiled method (`Exception`'s,
inherited). Its hit path raises in the caller through `Primitive`'s
`beamtalk_exception_handler` call with the class name, as that method's body
would, instead of calling it. Besides the four facts above, the key needs the
method's body to be exactly that primitive: the defining module's
`__beamtalk_meta/0` maps the selector to it under
`caller_safe_class_primitives` (generated by the compiler from the method
source). It shares the per-tag index and every eraser with the direct-call
keys (the same per-class map), so a `class signal:` override, a class-side
extension, a runtime fun or a reload of the class or an ancestor takes it away.

BT-4363: a class-literal send of a class-side method whose whole body is one
scalar literal (`Errors protocolError` with `class protocolError => 1`; since
BT-4504 any of integer, float, string, symbol, character, `nil`, `true`,
`false`) is keyed `{constant_value, Value, Module, FunName, Tag}`, `Value`
being the term the literal compiles to
(`beamtalk_class_metadata:constant_value()`). Its hit path answers
`Value` in place instead of calling `Module:FunName`. Besides the four facts
above, the key needs the body to be exactly that literal: the class's metadata
row maps the selector to `Value` (`constant_class_methods`, published from the
module's `__beamtalk_meta/0`, generated by the compiler from the method source;
`beamtalk_class_metadata:put_constant_class_methods/2`). The row, not the
module, is read because a reload erases keys inside the new module's
`-on_load`, while the old code is still current. It shares the per-class map
and every eraser with the direct-call keys, so a class-side extension, a
runtime fun (`put_class_method`), `removeSelector:` or a reload that changes the
body takes it away, and the call site's fallback answers as a dynamic send.

BT-4409: a class-literal `X new` on a Value whose synthesized `new/0` may build
in the caller (ADR 0142 rule 3, the BT-4099 constructor fast path) is keyed
`{Module, new, Tag}`, with arity 0: its hit path calls `Module:new()`, which
takes no `ClassSelf`, so the stored class reference is only the "installed"
witness. `new` is process-bound, so `in_caller_owner/2` never names it;
instead the key needs the dynamic `new` path's own answer,
`beamtalk_class_dispatch:lookup_direct_constructor(Pid, new, [])`, to be
`{ok, Module}` (the class's baked `direct_constructors` lists `new`, it has no
runtime fun, `direct_call_ok/2` holds and `Module` exports `new/0`), so the
static and dynamic paths ask one question. A change to the baked set erases the
class's keys (`beamtalk_class_metadata:put_direct_constructors/2`), as do the
erasers above; a later class-side extension or `put_class_method` of `new` is
then answered by the fallback, whose class process runs it
(`beamtalk_object_class`'s `{new, _}` handler).

BT-4756: a class literal (`x := Point`) is keyed `{class_ref, Tag}` in the same
per-class map; its value is the class object `{beamtalk_object, Tag, Module,
Pid}` the literal built before (the live registered pid and the module the
published row names). The call site reads the map once and, on a miss, calls
`class_ref/1`, which builds the object and installs it under the tag lock only
while the pid is registered and alive and the row still names `Module`. It
shares the per-tag index and every eraser above (a metadata write or reload, an
ancestor's write, the class monitor's `'DOWN'`, row removal), so a reload that
moves the class to another module is seen on the next literal. A restart is
seen only once the monitor's `'DOWN'` has erased the key: between the class
process dying and that `'DOWN'` the cached object still names the dead pid (the
same window as the direct-call keys above). An unregistered class installs
nothing: negative verdicts are not cached.

`static_call_target/4` (the call site's miss path) derives all four and, under
the class's tag lock, records the tag in the per-tag index
(`beamtalk_class_registry:record_static_call_tag/1`, read only by
`erase_all_static_call_keys/0`) and then puts the class's map with the target
added (a target already present is never rewritten). Every erase of the
class's derived key (`erase_direct_flag/1`: `set/2`, `set_class_vars/2`,
`invalidate/1`, and through `invalidate/1` an ancestor's write) also erases
the class's per-class static call key (as does an ancestor gaining or losing a
class-side extension, `invalidate_descendants_of_tag/1`, BT-4307), unconditionally (it may outlive a
derived key another path erased), so every target is gone wherever the
dynamic answer could change. Targets are never erased one at a time: any
write that could change one class's answer erases the whole map, and the next
send of each call site re-installs its own target. The class monitor's `'DOWN'` handler calls
`invalidate/1` too, so a dead pid is not served once the monitor has seen the
death. Between the death and that `'DOWN'`, a direct call may still receive the
dead pid in `ClassSelf`: the same window as a `whereis` that answers just
before the process dies, and harmless to a method that by construction needs
no class process.

Reload has no `undef` window. A compiled module's `-on_load` hook registers
the class synchronously (`register_class/0` -> `update_class/2` ->
`apply_class_info/2`), and the VM keeps the OLD code current until that hook
returns. Both of the reload's invalidations therefore erase the class's keys
while the old code still exports every function a key names, and any install
after the second one reads the new metadata, in which a dropped method is no
longer the owner, so it declines. By the time the new code is current no key
names a function it dropped. A method the reload adds is installed only once
the new code exports it. (Proved by the "reload that drops a method" test in
`beamtalk_class_shadow_flags_tests`.)

A call site that keeps declining (a shadowed class, a class-variable chain,
an unregistered or dead class, another owner) has no key, so it pays this
whole unlocked check on every send, and then `class_send` checks the same
facts again: dearer than the BT-4301 guard's decline, which was one
`persistent_term` read. A declining site already takes the gen_server
`class_send`, which dominates that cost, so a negative answer is not cached:
caching it would need an eraser for every way a site starts to qualify
(`clear/2` of a shadow, class registration and restart), and a missed one
would strand the site on the slow path.

Cost model (BT-4442). Each `persistent_term` update (a put or an erase of a
present key) swaps the global table and waits for thread progress on every
scheduler, about 0.1 to 0.3 ms on an idle node; OTP has no batch erase. So a
metadata write erases at most two keys per class it reaches (the derived flag
and the per-class map), however many targets were warm:
`invalidate/1` of a class with N descendants costs at most 2(N+1) updates,
not 1 + K per class (`benchmarks/vs-elixir/results/bench-bt4442.md`). Installing
a target is one update (the map is copied, but a class has few targets), the
same count as one key per target had, so re-warming after an erase pays one
update per target the next sends reach. All but a class's first install now
replace a present value, though, and replacing (like erasing) a value releases
its literal area, which has every process check its heap for the old term; a
put of a new key did not. That moves cost from the synchronous erase (paid by
the metadata writer, K per class) to the lazy re-warm (paid by the first
send of each site, up to K - 1 per class), and grows with the number and heap
size of the node's processes. Only metadata writes erase; never the send
path.

`persistent_term` writes that change a value trigger a global scan, so only
actual transitions write (set when unset, erase when set); both are rare
(registration / class (re)definition), never on the send path.
""".

-export([
    set/2,
    set/3,
    clear/2,
    clear_if_owner/3,
    is_set/2,
    is_shadowed/2,
    direct_call_ok/2,
    shadow_free/2,
    is_direct_flag_set/1,
    direct_flag_value/1,
    invalidate/1,
    invalidate_descendants_of_tag/1,
    erase_all_static_call_keys/0,
    set_class_vars/2,
    mark_ready/0,
    is_ready/0,
    with_tag_lock/2,
    static_call_target/4,
    class_ref/1,
    static_call_key_value/1,
    static_calls_key/1
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-define(READY_KEY, beamtalk_class_shadow_ready).
%% BT-4004: bound on waiting for a per-tag lock held by a hung critical section.
-define(TAG_LOCK_TIMEOUT_MS, 60000).

%% BT-3700: the send-path guard (`direct_call_ok/2`) is hot; the slow-path
%% helpers are inlined so the miss branch is not a chain of calls.
-compile({inline, [key/2, is_set/2, is_shadowed/2, is_ready/0]}).

-type kind() :: extension | runtime_fun | class_vars.
%% The derived flag's installed value (BT-4299; see the moduledoc).
-type derived() :: true | class_vars.
%% A static call key (BT-4316): `{Module, FunName, Tag}`, the call-site literal,
%% and since BT-4442 a key in its class's `static_calls_key/1` map.
%% `{Module, FunName, Tag}` keys a guarded direct call of `Module:FunName`
%% (BT-4316); `{caller_safe_raise, Primitive, Module, FunName, Tag}` keys a
%% class-literal signal send that raises in the caller through `Primitive`
%% while `Module:FunName`'s body is still that caller-safe signal primitive
%% (BT-4270).
%% `{constant_value, Value, Module, FunName, Tag}` keys a class-literal send
%% that answers `Value` in place while `Module:FunName`'s body is still exactly
%% that literal (BT-4363; any scalar literal kind since BT-4504).
-type static_call_key() ::
    {module(), atom(), atom()}
    | {caller_safe_raise, atom(), module(), atom(), atom()}
    | {constant_value, beamtalk_class_metadata:constant_value(), module(), atom(), atom()}
    | {class_ref, atom()}.
-export_type([kind/0, derived/0, static_call_key/0]).

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
    set(Kind, Name, true).

-doc """
`set/2` for a flag whose writer is a class process (BT-4808): the raw flag
holds the writer's pid instead of `true`, so a dead class's removal can erase
the flag only while it still names that dead owner (`clear_if_owner/3`) and
never a rebuilt class's. Every reader tests "set" as "not `false`", so the
read side costs the same one `persistent_term` read. No-op when the flag
already holds `Value`; a different value (a rebuilt class over a dead owner's)
is replaced under the same per-tag lock.
""".
-spec set(kind(), atom(), true | pid()) -> ok.
set(Kind, Name, Value) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        Value ->
            ok;
        _ ->
            Tag = class_tag(Kind, Name),
            with_tag_lock(Tag, fun() ->
                persistent_term:put(Key, Value),
                erase_direct_flag(Tag)
            end)
    end.

-spec class_tag(kind(), atom()) -> atom().
class_tag(extension, Tag) -> Tag;
class_tag(runtime_fun, Name) -> beamtalk_class_registry:class_object_tag(Name);
class_tag(class_vars, Name) -> beamtalk_class_registry:class_object_tag(Name).

%% Erase the class's derived key and, unconditionally, its static call key
%% (BT-4316): a static call key may outlive a derived key another path erased.
%% BT-4442: the class's static call targets all live in one per-class key, so
%% this is at most two `persistent_term` updates however many targets are warm.
%% Runs only under the tag lock.
-spec erase_direct_flag(atom()) -> ok.
erase_direct_flag(Tag) ->
    erase_if_present(Tag),
    erase_static_calls(Tag).

%% Erase the class's per-class static call key (BT-4442) and forget its tag in
%% the index. Runs only under the tag lock.
-spec erase_static_calls(atom()) -> ok.
erase_static_calls(Tag) ->
    %% No atom, no key: a class no call site ever warmed creates no atom here.
    try binary_to_existing_atom(static_calls_key_name(Tag)) of
        StaticCallsKey -> erase_if_present(StaticCallsKey)
    catch
        error:badarg -> ok
    end,
    beamtalk_class_registry:forget_static_call_tag(Tag).

%% Only an erase of a present key costs a `persistent_term` update; checking
%% first keeps an erase of an absent key free.
-spec erase_if_present(term()) -> ok.
erase_if_present(Key) ->
    case persistent_term:get(Key, false) of
        false ->
            ok;
        _ ->
            _ = persistent_term:erase(Key),
            ok
    end.

-doc """
The per-class static call key of the class tagged `Tag` (BT-4442): the atom
`'<Tag> static calls'` (`'Point class static calls'`), whose value maps each
installed call-site key literal to its class reference. An atom because
hashing an atom `persistent_term` key is much cheaper than hashing a tuple
(the hit path measured ~21 ns against ~36 ns for `{Name, Tag}` and ~26 ns for
BT-4316's per-target `{Module, FunName, Tag}`); the spaces keep it apart from
any module's key and from every class tag (a class name has no space, and a
tag ends in ` class`). The compiler spells it too, in the call site's read
(`util::static_calls_key` in beamtalk-codegen);
`GuardedStaticCallTest>>testCallSiteReadsTheRuntimesPerClassKey` fails if the
two spellings differ. Creates the atom (one per class, like the tag itself).
""".
-spec static_calls_key(atom()) -> atom().
static_calls_key(Tag) ->
    % elp:fixme W0023 intentional atom creation, one per class (as class_object_tag/1)
    binary_to_atom(static_calls_key_name(Tag)).

-spec static_calls_key_name(atom()) -> binary().
static_calls_key_name(Tag) ->
    <<(atom_to_binary(Tag))/binary, " static calls">>.

-doc """
Erase `ClassName`'s derived direct-call flag, and its descendants', each under
its per-tag lock (ADR 0142 §2, BT-4299, BT-4301), so the next send re-derives
it. Called on a class-metadata write for the class, after the write: reload
(twice, the second time once a re-parent is published), a change to its
class variables, a class-side `removeSelector:`, a runtime `put_class_method`,
row removal. It does not keep the fast path off while the write lands (see the
moduledoc).

The lock is taken even when no derived flag is installed (BT-4301): the
derivation reads published metadata (`chain_has_class_variables/1`), not only
raw flags raised under the same lock, so an install in flight may have derived
`true` from the row as it was before the write and not have put it yet. Taking
the lock waits for that install, and the erase that follows removes its
stale value; skipping the lock on "absent" would let it land after the erase
and stay. Runs only on metadata writes, never on the send path. Must never be
called from inside `with_tag_lock/2`.
""".
-spec invalidate(atom()) -> ok.
invalidate(ClassName) ->
    erase_class_flag(ClassName),
    erase_descendant_flags(ClassName).

-doc """
Erase the static call keys of every descendant of the class tagged `Tag`, each
under its own tag lock (BT-4307). Called by `beamtalk_extensions:register/5`
and `unregister/3` when a class-side extension row under `Tag` appears or goes
away, after the row is written and the derived-fact cache is flushed: an
ancestor's class-side extension counts as that ancestor's method for a send
to a descendant, so it can change the descendant's in-caller owner
(`beamtalk_class_dispatch:in_caller_owner/2`), and with it whether a static
call key whose owner is inherited (BT-4270's caller-safe raise) may stay
installed. A key for a method the descendant defines itself cannot change
(its own method is found before an ancestor's extension), but the whole
per-class map goes, as every eraser does. The descendant's derived flag stays:
it reads only the class's own shadows and its chain's class variables, never
an ancestor's extension. The class's own keys are erased by `set/2` already.

Erase-after-write, as `invalidate/1` is for descendants: between the row
write and the erase, a descendant's warmed static call may still take its old
key once. The lock is taken for each descendant even when it has no key, for
the reason `invalidate/1` gives (an install in flight may have validated
before the write and not have put yet). A tag that names no loaded class, or
a class with no descendants, erases nothing. Must never be called from inside
`with_tag_lock/2`.
""".
-spec invalidate_descendants_of_tag(atom()) -> ok.
invalidate_descendants_of_tag(Tag) ->
    case beamtalk_primitive:class_name_from_tag(Tag) of
        undefined ->
            ok;
        ClassName ->
            lists:foreach(
                fun(Descendant) ->
                    DescendantTag = beamtalk_class_registry:class_object_tag(Descendant),
                    with_tag_lock(DescendantTag, fun() -> erase_static_calls(DescendantTag) end)
                end,
                beamtalk_class_registry:all_subclasses(ClassName)
            )
    end.

-doc """
Erase every class's derived key and static call keys, each under its tag lock
(BT-4316). Called by a (re)starting `beamtalk_class_monitor`: a class killed
while the monitor was down sends it no `'DOWN'`, so the per-class
`invalidate/1` that would erase its static call keys never runs, and those
keys would keep serving the dead pid as `ClassSelf` after lazy recovery
restarts the class. A fresh monitor is rare, so erasing all keys is cheap; the
next send of each call site re-installs from the live class. Must never be
called from inside `with_tag_lock/2`.
""".
-spec erase_all_static_call_keys() -> ok.
erase_all_static_call_keys() ->
    lists:foreach(
        fun(Tag) -> with_tag_lock(Tag, fun() -> erase_direct_flag(Tag) end) end,
        beamtalk_class_registry:static_call_key_tags()
    ).

%% Erase one class's derived key under its tag lock, always taking the lock
%% (see `invalidate/1`: an in-flight install may still put a stale value).
-spec erase_class_flag(atom()) -> ok.
erase_class_flag(ClassName) ->
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    with_tag_lock(Tag, fun() -> erase_direct_flag(Tag) end).

%% BT-4301: a descendant's derived key folds in this class's published
%% class-variable fact and superclass link (`chain_class_vars/1`), so it is
%% erased with this class's, each under its own tag lock. Runs only on
%% metadata writes (never on the send path); the walk is ETS-only, so it is
%% safe from inside a class process.
-spec erase_descendant_flags(atom()) -> ok.
erase_descendant_flags(ClassName) ->
    lists:foreach(fun erase_class_flag/1, beamtalk_class_registry:all_subclasses(ClassName)).

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
    set(class_vars, ClassName, self());
set_class_vars(ClassName, false) ->
    Key = key(class_vars, ClassName),
    case persistent_term:get(Key, false) of
        false ->
            ok;
        _ ->
            Tag = class_tag(class_vars, ClassName),
            with_tag_lock(Tag, fun() ->
                _ = persistent_term:erase(Key),
                erase_direct_flag(Tag)
            end)
    end.

-doc """
Run `Fun` holding the per-tag lock that serializes every write of the derived
direct-call flag (BT-3700). Exported for tests that hold it deterministically.

A node-local FIFO lock (`beamtalk_fifo_lock`, BT-4004): waiters are woken on
release rather than after `global:trans/4`'s random back-off. The wait is
unbounded, like the `global:trans(..., infinity)` it replaced: every eraser
(`set/2`, `invalidate/1`, ...) is correct only if it takes the lock and so
waits out an in-flight install, which would otherwise `put` a stale derived
value after the erase. Only the send-path installers bound their wait (see
`with_install_lock/3`). Not re-entrant: a nested call on the same tag raises
`{recursive_lock, Key}` (no caller nests; see the "never called from inside"
notes above).
""".
-spec with_tag_lock(atom(), fun(() -> T)) -> T.
with_tag_lock(Tag, Fun) ->
    beamtalk_fifo_lock:with_lock_or_raise(
        tag_lock_key(Tag), infinity, {'ClassShadowFlags', with_tag_lock}, Fun
    ).

%% The send-path installers' lock (BT-4004): bounded, and on timeout (or a lost
%% lock table) it declines with `Declined` instead of raising into user code.
%% Declining is safe, installing nothing makes the send take the dynamic
%% `class_send` fallback, whereas an eraser that gave up could lose an
%% invalidation.
-spec with_install_lock(atom(), fun(() -> T), D) -> T | D.
with_install_lock(Tag, Fun, Declined) ->
    Timeout = beamtalk_fifo_lock:configured_timeout(direct_flag, ?TAG_LOCK_TIMEOUT_MS),
    case beamtalk_fifo_lock:with_lock(tag_lock_key(Tag), Timeout, Fun) of
        {ok, Result} ->
            Result;
        {error, Reason} ->
            ?LOG_WARNING("Class direct-call install declined: tag lock unavailable", #{
                tag => Tag, reason => Reason, timeout_ms => Timeout, domain => [beamtalk, runtime]
            }),
            Declined
    end.

-spec tag_lock_key(atom()) -> {module(), direct_flag, atom()}.
tag_lock_key(Tag) ->
    {?MODULE, direct_flag, Tag}.

-doc "Clear the `Kind` flag for `Name`. No-op when not set.".
-spec clear(kind(), atom()) -> ok.
clear(Kind, Name) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        false ->
            ok;
        _ ->
            _ = persistent_term:erase(Key),
            ok
    end.

-doc """
BT-4808: clear the `Kind` flag for `Name` only while it still names `Owner`,
the (dead) class process that raised it, and erase the class's derived
direct-call flag with it. A flag a rebuilt class raised under the same name
holds that class's pid and is left alone. The compare and the erase run under
the per-tag lock every `set/3` takes, so a rebuild's raise is either ordered
before the compare (and kept) or after the erase (and unaffected). No lock is
taken when the flag does not name `Owner`: only `Owner` raises a flag
carrying its pid, and it is dead.
""".
-spec clear_if_owner(kind(), atom(), pid()) -> ok.
clear_if_owner(Kind, Name, Owner) when is_pid(Owner) ->
    Key = key(Kind, Name),
    case persistent_term:get(Key, false) of
        Owner ->
            Tag = class_tag(Kind, Name),
            with_tag_lock(Tag, fun() ->
                case persistent_term:get(Key, false) of
                    Owner ->
                        _ = persistent_term:erase(Key),
                        erase_direct_flag(Tag);
                    _ ->
                        ok
                end
            end);
        _ ->
            ok
    end.

-doc "Read one flag.".
-spec is_set(kind(), atom()) -> boolean().
is_set(Kind, Name) ->
    persistent_term:get(key(Kind, Name), false) =/= false.

-doc """
True when a class-side extension under `ClassTag` or a runtime class-method fun
on `ClassName` may shadow a compiled class method.
""".
-spec is_shadowed(atom(), atom()) -> boolean().
is_shadowed(ClassTag, ClassName) ->
    is_set(extension, ClassTag) orelse is_set(runtime_fun, ClassName).

-doc """
ADR 0142 §1 rules 1-2 (BT-4299, BT-4301): true exactly when the class is ready,
nothing shadows its compiled class methods (no `extension`, no `runtime_fun`)
and neither it nor any ancestor declares class variables. The guard of every
static class-side direct call (ADR 0142 §2). One `persistent_term` read of the derived flag
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

%% The derived value the raw flags and the chain's published facts imply;
%% `false` (install nothing) while not ready or shadowed.
-spec derive(atom(), atom()) -> derived() | false.
derive(ClassTag, ClassName) ->
    case is_ready() andalso not is_shadowed(ClassTag, ClassName) of
        false -> false;
        true -> chain_class_vars(ClassName)
    end.

%% ADR 0142 §1 rules 1-2 over the whole chain (BT-4301): `class_vars` when the
%% class's own raw `class_vars` flag is up (raised before the fact is visible,
%% BT-4299) or when `beamtalk_class_metadata:chain_has_class_variables/1`, the
%% one runtime answer the dynamic in-caller path also reads, says the class or
%% an ancestor declares class variables or the chain cannot be followed (it
%% fails closed: an unpublished fact or superclass link, a missing row, a
%% cycle). Else `true`. An ancestor's change reaches a descendant's installed
%% value through `put_class_variables/2` and `apply_class_info/2`, which erase
%% the descendants' keys after the ancestor's row field is written
%% (`invalidate/1`).
-spec chain_class_vars(atom()) -> derived().
chain_class_vars(ClassName) ->
    case
        is_set(class_vars, ClassName) orelse
            beamtalk_class_metadata:chain_has_class_variables(ClassName)
    of
        true -> class_vars;
        false -> true
    end.

-spec install_direct_flag(atom(), atom()) -> derived() | false.
install_direct_flag(ClassTag, ClassName) ->
    %% Cheap pre-check without the lock: a shadowed or not-yet-ready class must
    %% not take a lock per send.
    case derive(ClassTag, ClassName) of
        false ->
            false;
        _ ->
            with_install_lock(
                ClassTag, fun() -> install_direct_flag_locked(ClassTag, ClassName) end, false
            )
    end.

%% The derived value under the tag lock: the installed one, or re-derived from
%% the raw flags and installed. Must run inside `with_tag_lock(ClassTag, _)`.
-spec install_direct_flag_locked(atom(), atom()) -> derived() | false.
install_direct_flag_locked(ClassTag, ClassName) ->
    case persistent_term:get(ClassTag, false) of
        false ->
            case derive(ClassTag, ClassName) of
                false ->
                    false;
                Value ->
                    persistent_term:put(ClassTag, Value),
                    Value
            end;
        Value ->
            Value
    end.

-doc """
The miss path of a static class send's guarded direct call (BT-4316, ADR 0142
§2): the class reference to pass as `ClassSelf` to `Module:FunName/Arity`, or
`false` (take the dynamic `class_send` fallback). `Key` is the call site's
`{Module, FunName, Tag}` literal, `Tag` the metaclass tag of `ClassName`.
When every condition in the moduledoc's "static call keys" holds, adds
`Key => Ref` to the class's per-class map (`static_calls_key/1`,
BT-4442) under the class's tag lock, so the call site's next read of that key
matches it.
Declines, installing nothing, when the class is shadowed, has class variables
in its chain, is not registered or not alive, when the dynamic path's owner
(`beamtalk_class_dispatch:in_caller_owner/2`) names another module or
function, when an inherited owner has an ADR 0084 runtime fun for the
selector (BT-4270), or when the function is not exported.

The checks run once without the lock, so a call site that keeps declining
never takes a lock per send, and again under it, after the class's
derived flag is installed there: every eraser holds the same lock, so an
install that lands before an erase is removed by it, and one after it sees
the write. Never called from inside `with_tag_lock/2`. Declines (`false`) too when the tag
lock stays held past the bounded wait (BT-4004).
""".
-spec static_call_target(static_call_key(), atom(), atom(), arity()) ->
    #beamtalk_object{} | false.
static_call_target(Key, ClassName, Selector, Arity) ->
    {_Module, _FunName, Tag} = key_target(Key),
    case static_call_ref(Key, ClassName, Selector, Arity) of
        false ->
            false;
        #beamtalk_object{} ->
            with_install_lock(
                Tag,
                fun() ->
                    case
                        install_direct_flag_locked(Tag, ClassName) =:= true andalso
                            static_call_ref(Key, ClassName, Selector, Arity)
                    of
                        false ->
                            false;
                        #beamtalk_object{} = Ref ->
                            install_static_call_locked(Tag, Key, Ref)
                    end
                end,
                false
            )
    end.

-doc """
The miss path of a class literal (BT-4756): the class object of the class named
`ClassName`, or `undefined` when no such class is registered (the call site
raises `class_not_found`). The hit path is the call site's own read of the
class's static call map for `{class_ref, Tag}`, so a
literal costs one `persistent_term` read once warm.

The answer is always what the literal compiled to before BT-4756: the live
registered pid from `whereis` and the module `module_name_safe/1` reads, in a
class object of the class's tag. When that answer is certified, it is also
installed in the class's map: under the class's tag lock, the pid must still be
the registered, alive one and the published metadata row must still name the
module. Every eraser of the map (a metadata write or reload, the class
monitor's `'DOWN'`, row removal) takes the same lock, so an install that read
before a reload is removed by its erase, and one after it reads the new row.
The module lookup runs before the lock (it may be a `gen_server:call` for an
unpublished class, which must never run under the lock), and an answer the
row does not certify is returned uncached. Nothing is installed for an
unregistered class: negative verdicts are never cached (ADR 0142 §2).
Never called from inside `with_tag_lock/2`.
""".
-spec class_ref(atom()) -> #beamtalk_object{} | undefined.
class_ref(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        Pid when is_pid(Pid) ->
            Tag = beamtalk_class_registry:class_object_tag(ClassName),
            Module = beamtalk_object_class:module_name_safe(Pid),
            Ref = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
            with_install_lock(
                Tag,
                fun() ->
                    case class_ref_certified(ClassName, Pid, Module) of
                        true -> _ = install_static_call_locked(Tag, {class_ref, Tag}, Ref);
                        false -> ok
                    end
                end,
                ok
            ),
            Ref;
        _ ->
            undefined
    end.

%% Whether the class object just built is still the one a fresh lookup would
%% build: `Pid` is the registered, live process and the published row names
%% `Module`. Reads ETS and the registry only (it runs under the tag lock).
-spec class_ref_certified(atom(), pid(), module()) -> boolean().
class_ref_certified(ClassName, Pid, Module) ->
    beamtalk_class_registry:whereis_class(ClassName) =:= Pid andalso
        is_process_alive(Pid) andalso
        beamtalk_class_metadata:lookup_module(ClassName) =:= {ok, Module}.

%% Add `Key => Ref` to the class's per-class static call map (BT-4442), under
%% the tag lock. A target already present with this very reference is never
%% rewritten (a rewrite would cost a `persistent_term` update for nothing); one
%% holding another reference (a pid the class monitor has not yet erased after
%% a restart) is replaced by the one just validated.
%% The per-tag index records the tag first, so a map is never installed
%% without the row that lets `erase_all_static_call_keys/0` find it.
-spec install_static_call_locked(atom(), static_call_key(), #beamtalk_object{}) ->
    #beamtalk_object{}.
install_static_call_locked(Tag, Key, Ref) ->
    MapKey = static_calls_key(Tag),
    Targets = persistent_term:get(MapKey, #{}),
    case Targets of
        #{Key := Ref} ->
            Ref;
        _ ->
            ok = beamtalk_class_registry:record_static_call_tag(Tag),
            persistent_term:put(MapKey, Targets#{Key => Ref}),
            Ref
    end.

%% The class reference a static call of `Module:FunName/Arity` may pass, or
%% `false`: the registered class process is alive and the dynamic path's owner
%% (`in_caller_owner/2`, which also requires `direct_call_ok/2`) runs
%% `Selector` sent to this class as exactly this exported function. The
%% reference is the `ClassSelf` that path builds (`class_send_plain/3`): this
%% class's tag, its own module and its pid. Under the tag lock it takes no
%% lock itself: the caller installed the derived flag first, so
%% `direct_call_ok/2` answers from it.
-spec static_call_ref(static_call_key(), atom(), atom(), arity()) ->
    #beamtalk_object{} | false.
static_call_ref({Module, new, Tag}, ClassName, new, 0) ->
    %% BT-4409: a constructor key. `new` is process-bound, so
    %% `in_caller_owner/2` never names it; the owner predicate is the dynamic
    %% `new` path's own (`lookup_direct_constructor/3`, which also requires
    %% `direct_call_ok/2` and the `new/0` export). The reference is only the
    %% "installed" witness: `new/0` takes no `ClassSelf`.
    Pid = beamtalk_class_registry:whereis_class(ClassName),
    case
        is_pid(Pid) andalso is_process_alive(Pid) andalso
            beamtalk_class_dispatch:lookup_direct_constructor(Pid, new, [])
    of
        {ok, Module} -> #beamtalk_object{class = Tag, class_mod = Module, pid = Pid};
        _ -> false
    end;
static_call_ref(Key, ClassName, Selector, Arity) ->
    {Module, FunName, Tag} = key_target(Key),
    Pid = beamtalk_class_registry:whereis_class(ClassName),
    case is_pid(Pid) andalso beamtalk_class_dispatch:in_caller_owner(Pid, Selector) of
        {owner, ClassName, Tag, ClassModule, DefiningClass, Module, FunName} ->
            case
                runs_compiled_function(ClassName, DefiningClass, Selector) andalso
                    erlang:function_exported(Module, FunName, Arity) andalso
                    key_body_holds(Key, ClassName, DefiningClass, Selector)
            of
                true -> #beamtalk_object{class = Tag, class_mod = ClassModule, pid = Pid};
                false -> false
            end;
        _ ->
            false
    end.

%% The function a key names, and the tag of the class its call site sends to.
-spec key_target(static_call_key()) -> {module(), atom(), atom()}.
key_target({caller_safe_raise, Primitive, Module, FunName, Tag}) when
    is_atom(Primitive), is_atom(Module), is_atom(FunName), is_atom(Tag)
->
    {Module, FunName, Tag};
key_target({constant_value, _Value, Module, FunName, Tag}) when
    is_atom(Module), is_atom(FunName), is_atom(Tag)
->
    {Module, FunName, Tag};
key_target({Module, FunName, Tag}) when is_atom(Module), is_atom(FunName), is_atom(Tag) ->
    {Module, FunName, Tag}.

%% BT-4270: a caller-safe raise key's hit path does not call `Module:FunName`;
%% it raises in the caller through `Primitive`, as that method's body would.
%% So the key also needs the body to be exactly that primitive, which the
%% defining class's published metadata row says (`caller_safe_class_primitives`,
%% decided by the compiler from the method source). BT-4493: read from the
%% row, not from `Module:'__beamtalk_meta'()`: a reload erases keys inside the
%% new module's `-on_load`, while the old code is still current, so the
%% module's meta would certify the old body. The row is written from the new
%% meta, and a change erases the keys (descendants' included).
-spec key_body_holds(static_call_key(), atom(), atom(), atom()) -> boolean().
key_body_holds({caller_safe_raise, Primitive, _, _, _}, _ClassName, DefiningClass, Selector) ->
    case
        maps:find(Selector, beamtalk_class_metadata:lookup_caller_safe_primitives(DefiningClass))
    of
        {ok, Primitive} -> true;
        _ -> false
    end;
key_body_holds({constant_value, Value, _, _, _}, ClassName, _DefiningClass, Selector) ->
    %% BT-4363: the hit path answers `Value` without calling the method, so
    %% the body must be exactly that literal (any scalar kind, BT-4504), as
    %% the class's published metadata row says (`constant_class_methods`, generated from the method
    %% source; the key's own class defines it, the owner check above holds).
    %% Read from the row, not from `Module:'__beamtalk_meta'()`: a reload
    %% erases this class's keys inside the new module's `-on_load`, while the
    %% old code is still current, so an install racing it would certify the
    %% old literal and outlive the reload. The reload publishes the row from
    %% the new meta and erases again on a change
    %% (`beamtalk_class_metadata:put_constant_class_methods/2`). `=:=`: `1`
    %% never certifies `1.0`, nor `$a` (97) a float `97.0`.
    case maps:find(Selector, beamtalk_class_metadata:lookup_constant_class_methods(ClassName)) of
        {ok, Certified} -> Certified =:= Value;
        error -> false
    end;
key_body_holds(_DirectCallKey, _ClassName, _Module, _Selector) ->
    true.

%% Whether the in-caller path runs the defining class's compiled function for
%% `Selector` (`beamtalk_class_dispatch:run_in_caller/7`): always for a method
%% the class defines (`direct_call_ok/2` held, so it has no runtime fun), and
%% for an inherited one (BT-4270) only while
%% `beamtalk_class_dispatch:class_method_body/2`, the choice
%% `apply_class_method_body/7` makes, picks the compiled function over an ADR
%% 0084 runtime fun. Installing such a fun is a metadata write on the defining
%% class, whose `invalidate/1` erases this class's keys as a descendant's.
-spec runs_compiled_function(atom(), atom(), atom()) -> boolean().
runs_compiled_function(ClassName, ClassName, _Selector) ->
    true;
runs_compiled_function(_ClassName, DefiningClass, Selector) ->
    beamtalk_class_dispatch:class_method_body(DefiningClass, Selector) =:= compiled.

-doc """
The class reference installed for the static call key `Key` in its class's
per-class map (BT-4442), `false` when absent (test and bench hook).
""".
-spec static_call_key_value(static_call_key()) -> #beamtalk_object{} | false.
static_call_key_value({class_ref, Tag} = Key) when is_atom(Tag) ->
    case persistent_term:get(static_calls_key(Tag), #{}) of
        #{Key := Ref} -> Ref;
        #{} -> false
    end;
static_call_key_value(Key) ->
    {_Module, _FunName, Tag} = key_target(Key),
    case persistent_term:get(static_calls_key(Tag), #{}) of
        #{Key := Ref} -> Ref;
        #{} -> false
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
