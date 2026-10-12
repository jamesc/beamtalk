%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_vars).
-moduledoc """
Single owner of the class-variable process-dictionary key shape and of every
class-variable access (ADR 0130 §1, §2, §4, §5).

## Storage

A class's variables live in the calling process's dictionary under
`key(ClassName)`: `{'$bt_class_vars', ClassTag}`, where `ClassTag` is the
class's metaclass tag (`'Name class'`, `element(2, ClassSelf)`). The key
shape is owned by Rust (`class_var_keys` in `beamtalk-codegen`) and shared
through the generated `beamtalk_class_vars_keys.hrl`, because the class-variable
reads and writes codegen inlines (`erlang:get/1` + `maps:*`, ADR 0130 Phase 0
gate 3) build the same key. It is keyed by tag so that an access recovers its
key from `ClassSelf` with one `element/2`, never a tag-to-name derivation. The
value is either

- the live `#{VarName => Value}` map (a class-method invocation that
  *installed* it), or
- the read-only marker `?BT_CLASS_VARS_RO(ClassTag)` installed by
  `with_snapshot/2` for runtime-owned out-of-process regions; `put/3` and
  `clear/2` raise `class_state_read_only` under it, and reads resolve the
  class's ETS mirror **by class name** through the registry (never by pid,
  so a class-process restart is transparent). A registered class process with
  no mirror row at all (a restart that has not yet recorded its first
  snapshot) is told apart from an empty map (`class_state_snapshot_lookup/1`)
  and raises `class_state_unreachable` instead of reading `#{}`.

`install/2` and `uninstall/1` are the only writers of the `?BT_CLASS_VARS_HOME`
entry, which records the key of the live invocation; they set and erase it
together with the class key. The class key itself is also written by `put/3`
and `clear/2` (the live map), `restore/1` (a snapshot's map) and
`with_snapshot/2` (the read-only marker, erased again afterwards).
`snapshot/0`, `restore/1` and `protect/1` act on the class key the home entry
names.

## Receivers

`key/1`, `assert_absent/1`, `install/2` and `uninstall/1` are the invocation
entry points' API and take a class *name* (or the key). The access helpers
(`get`, `get_late`, `put`, `clear`, `has`, `capture`, `with_snapshot`) take
the `ClassSelf` class object (`#beamtalk_object{}` whose class tag is
`'Name class'`) and derive the key from its tag; a `nil` or non-class-object
receiver is an internal error. On the success path the receiver check is only
the record match (`class_tag/1`): the class name, which costs a tag-to-name
derivation, is derived only when an error is raised, a read-only mirror is
read, or a read misses and checks the declared set (BT-3766). An instance tag
never has a key installed, so it reaches one of those paths and raises the
same internal error there; the one exception is a capture fallback form
(`get/3`, `get_late/3`, `has/3`) given a capture map, which answers from that
map without deriving the name. `capture/2` and `with_snapshot/2` validate the
tag fully (once per block-literal creation or region, not a hot path).

## Semantics

- **Key presence is the "at home" test.** A read with no key and no marker
  raises `class_state_unreachable`; the 3-arity forms fall back to the
  creation-time capture (`capture/2`), raising only when the capture is `none`.
- `has/2` is key presence in the map and never raises on the name (as
  `hasField:` is today: reads raise, `hasField:` does not); `clear/2` removes
  the key (as `clearField:` is today). `get/2` on a name absent from the map
  reads `nil` when the declared-kinds map is empty (no metadata, e.g.
  ClassBuilder classes, no live class, or a class that declares no class
  variables: the declared set cannot be told apart from "unknown") or the name
  is declared; it raises `undeclared_class_variable` only when the declared
  set is non-empty and does not contain the name.
- `get_late/2` raises the same `uninitialized_state_error` as the class
  gen_server's `get_class_var` for an unassigned (`nil` or absent) variable.

## Dependencies

This module is a leaf of the object system: it never calls
`beamtalk_object_class` or `beamtalk_behaviour_intrinsics`. Declared kinds
come from `beamtalk_class_metadata:class_var_kinds/1`, the error values from
`beamtalk_class_var_errors` (shared with the class gen_server), and the
`class_var_abi` load gate lives in `beamtalk_class_var_abi`.
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").
-include("beamtalk_class_vars_keys.hrl").

-export([
    key/1,
    key_for_tag/1,
    assert_absent/1,
    install/2,
    uninstall/1,
    get/2,
    get/3,
    get_late/2,
    get_late/3,
    put/3,
    clear/2,
    has/2,
    has/3,
    capture/2,
    snapshot/0,
    restore/1,
    protect/1,
    with_snapshot/2
]).

-export_type([key/0, snapshot/0, class_self/0]).

-define(HOME, ?BT_CLASS_VARS_HOME).

-type key() :: ?BT_CLASS_VARS_KEY(atom()).
-type snapshot() :: none | {key(), map()}.
-type class_self() :: #beamtalk_object{}.

%%====================================================================
%% Key shape and invocation entry points
%%====================================================================

-doc """
The process-dictionary key holding `ClassName`'s variables: the key shape
applied to the class's metaclass tag, the same key an access derives from
`ClassSelf`.

Not for hot paths: it derives the tag through
`beamtalk_class_registry:class_object_tag/1` (`list_to_atom`). Callers that
already hold the metaclass tag must use `key_for_tag/1`.
""".
-spec key(atom()) -> key().
key(nil) ->
    beamtalk_class_var_errors:raise_nil_receiver();
key(ClassName) when is_atom(ClassName) ->
    key_for_tag(beamtalk_class_registry:class_object_tag(ClassName)).

-doc """
`key/1` for a caller that already holds the class's metaclass tag (the
invocation entry points derive it once and reuse it for `ClassSelf`).
""".
-spec key_for_tag(atom()) -> key().
key_for_tag(ClassTag) when is_atom(ClassTag) ->
    ?BT_CLASS_VARS_KEY(ClassTag).

-doc """
Raise an internal error if `Key` is already present or any home entry is
present (a live invocation of any class in this process). Called by the
invocation entry points before `install/2`.
""".
-spec assert_absent(key()) -> ok.
assert_absent(?BT_CLASS_VARS_KEY(ClassTag) = Key) ->
    case {erlang:get(Key), erlang:get(?HOME)} of
        {undefined, undefined} ->
            ok;
        {KeyVal, HomeVal} ->
            Class = tag_to_name(ClassTag),
            Details = #{class => Class, key_present => KeyVal =/= undefined, home => HomeVal},
            ?LOG_ERROR(
                "class-variable home already present ~p",
                [Details],
                #{domain => [beamtalk, runtime]}
            ),
            Error0 = beamtalk_error:new(internal_error, Class, assert_absent),
            Error1 = beamtalk_error:with_message(
                Error0, <<"class-variable home is already installed in this process">>
            ),
            beamtalk_error:raise(beamtalk_error:with_details(Error1, Details))
    end.

-doc "Install the live map under `Key` and record it as the home entry.".
-spec install(key(), map()) -> ok.
install(?BT_CLASS_VARS_KEY(_) = Key, Map) when is_map(Map) ->
    erlang:put(Key, Map),
    erlang:put(?HOME, Key),
    ok.

-doc "Erase `Key` and the home entry together.".
-spec uninstall(key()) -> ok.
uninstall(?BT_CLASS_VARS_KEY(_) = Key) ->
    erlang:erase(Key),
    erlang:erase(?HOME),
    ok.

%%====================================================================
%% Access helpers (ClassSelf receivers)
%%====================================================================

-doc "Read a class variable (live map, or the read-only mirror).".
-spec get(class_self(), atom()) -> term().
get(ClassSelf, Name) ->
    Tag = class_tag(ClassSelf),
    read_value(Tag, Name, current_map(Tag, Name)).

-doc "Read a class variable; with no key present, read the creation-time `Captured` map.".
-spec get(class_self(), atom(), map() | none) -> term().
get(ClassSelf, Name, Captured) ->
    Tag = class_tag(ClassSelf),
    read_value(Tag, Name, map_or_captured(Tag, Name, Captured)).

-doc "Read a `late` class variable; raises `uninitialized_state_error` when unassigned.".
-spec get_late(class_self(), atom()) -> term().
get_late(ClassSelf, Name) ->
    Tag = class_tag(ClassSelf),
    late_value(Tag, Name, current_map(Tag, Name)).

-doc "`get_late/2` with a creation-time capture fallback.".
-spec get_late(class_self(), atom(), map() | none) -> term().
get_late(ClassSelf, Name, Captured) ->
    Tag = class_tag(ClassSelf),
    late_value(Tag, Name, map_or_captured(Tag, Name, Captured)).

-doc "Write a class variable. Returns the assigned value.".
-spec put(class_self(), atom(), term()) -> term().
put(ClassSelf, Name, Value) ->
    Tag = class_tag(ClassSelf),
    Key = ?BT_CLASS_VARS_KEY(Tag),
    case erlang:get(Key) of
        Map when is_map(Map) ->
            erlang:put(Key, Map#{Name => Value}),
            Value;
        ?BT_CLASS_VARS_RO(_) ->
            beamtalk_class_var_errors:raise_read_only(tag_to_name(Tag), Name);
        undefined ->
            beamtalk_class_var_errors:raise_unreachable(tag_to_name(Tag), Name, write)
    end.

-doc "Remove a class variable (`clearField:`); same errors as `put/3`. Returns `ClassSelf` (`clearField: -> Self`).".
-spec clear(class_self(), atom()) -> class_self().
clear(ClassSelf, Name) ->
    Tag = class_tag(ClassSelf),
    Key = ?BT_CLASS_VARS_KEY(Tag),
    case erlang:get(Key) of
        Map when is_map(Map) ->
            erlang:put(Key, maps:remove(Name, Map)),
            ClassSelf;
        ?BT_CLASS_VARS_RO(_) ->
            beamtalk_class_var_errors:raise_read_only(tag_to_name(Tag), Name);
        undefined ->
            beamtalk_class_var_errors:raise_unreachable(tag_to_name(Tag), Name, write)
    end.

-doc "Presence test (`hasField:`).".
-spec has(class_self(), atom()) -> boolean().
has(ClassSelf, Name) ->
    Tag = class_tag(ClassSelf),
    has_value(current_map(Tag, Name), Name).

-doc "`has/2` with a creation-time capture fallback.".
-spec has(class_self(), atom(), map() | none) -> boolean().
has(ClassSelf, Name, Captured) ->
    Tag = class_tag(ClassSelf),
    has_value(map_or_captured(Tag, Name, Captured), Name).

-doc """
Capture for a block literal: the live map when the key is present (the
resolved mirror under a read-only marker), else `Outer` (the enclosing
block's capture, or `none` at method level).
""".
-spec capture(class_self(), map() | none) -> map() | none.
capture(ClassSelf, Outer) ->
    Tag = class_tag(ClassSelf),
    %% Once per block-literal creation, not a hot path: keep the full receiver
    %% check (an instance tag raises here) as `with_snapshot/2` does.
    _ = tag_to_name(Tag),
    case erlang:get(?BT_CLASS_VARS_KEY(Tag)) of
        Map when is_map(Map) -> Map;
        ?BT_CLASS_VARS_RO(_) -> mirror(Tag, undefined);
        undefined -> Outer
    end.

%%====================================================================
%% Snapshot / restore / protect
%%====================================================================

-doc "Snapshot the home class variables: `none`, or `{Key, Map}`.".
-spec snapshot() -> snapshot().
snapshot() ->
    case erlang:get(?HOME) of
        undefined ->
            none;
        Key ->
            case erlang:get(Key) of
                Map when is_map(Map) -> {Key, Map};
                _ -> none
            end
    end.

-doc "Restore a snapshot. `none` is a pass-through that plants nothing.".
-spec restore(snapshot()) -> ok.
restore(none) ->
    ok;
restore({Key, Map}) when is_map(Map) ->
    erlang:put(Key, Map),
    ok.

-doc """
Run `Fun`; on any exception other than a `^` non-local-return throw (`?IS_NLR`)
restore the pre-call snapshot and re-raise.
""".
-spec protect(fun(() -> T)) -> T when T :: term().
protect(Fun) ->
    Snap = snapshot(),
    %% bt-catcher-audit: converted - this is protect/1 itself: restores the snapshot, then
    %% re-raises
    try
        Fun()
    catch
        throw:Nlr:Stack when ?IS_NLR(Nlr) ->
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            restore(Snap),
            erlang:raise(Class, Reason, Stack)
    end.

-doc """
Run `Fun` with the class's variables readable from the ETS mirror (resolved
by class name) when its key is absent: installs the key plus
`?BT_CLASS_VARS_RO(ClassTag)`, erases exactly that in an `after`. When the
key is already present (a live map or an outer snapshot) `Fun` just runs.
Never touches the home entry. Liveness is checked lazily, on the first mirror
read (`mirror/2`): a region that never reads a class variable succeeds even
while the class process is unregistered, and a read with no live class raises
`class_state_unreachable`.
""".
-spec with_snapshot(class_self(), fun(() -> T)) -> T when T :: term().
with_snapshot(ClassSelf, Fun) ->
    Tag = class_tag(ClassSelf),
    %% Unlike the access helpers, validate the tag fully before planting the
    %% marker (an instance tag raises here, not on the first mirror read): this
    %% is a once-per-region call, not a hot path.
    _ = tag_to_name(Tag),
    Key = ?BT_CLASS_VARS_KEY(Tag),
    case erlang:get(Key) of
        undefined ->
            erlang:put(Key, ?BT_CLASS_VARS_RO(Tag)),
            try
                Fun()
            after
                erlang:erase(Key)
            end;
        _ ->
            Fun()
    end.

%%====================================================================
%% Internal
%%====================================================================

%% The metaclass tag of a ClassSelf: the cheap receiver check every access
%% helper runs on its success path (BT-3766). A `nil` or non-object receiver
%% raises here. The class name is derived (`tag_to_name/1`) only where it is
%% needed: an error, a read-only mirror read, or the declared-set check of a
%% read that misses. An instance tag (no ` class` suffix) or an unknown base
%% atom therefore raises the same internal error at that point instead: no
%% access ever installs a key for such a tag, so it takes the "no key" path.
%% The one exception is `get/3`, `get_late/3` and `has/3` given a capture map,
%% which answer from that map without deriving the name.
-spec class_tag(term()) -> atom().
class_tag(#beamtalk_object{class = Tag}) when is_atom(Tag), Tag =/= nil ->
    Tag;
class_tag(_) ->
    beamtalk_class_var_errors:raise_nil_receiver().

%% The class name a metaclass tag (`'Name class'`) names; a non-class tag
%% raises the nil-receiver internal error. Off the success path only.
-spec tag_to_name(atom()) -> atom().
tag_to_name(Tag) ->
    TagBin = atom_to_binary(Tag, utf8),
    case beamtalk_class_registry:class_display_name(TagBin) of
        TagBin ->
            beamtalk_class_var_errors:raise_nil_receiver();
        Base ->
            try
                binary_to_existing_atom(Base, utf8)
            catch
                error:badarg -> beamtalk_class_var_errors:raise_nil_receiver()
            end
    end.

-spec current_map(atom(), atom()) -> map().
current_map(Tag, Name) ->
    case erlang:get(?BT_CLASS_VARS_KEY(Tag)) of
        Map when is_map(Map) -> Map;
        ?BT_CLASS_VARS_RO(_) -> mirror(Tag, Name);
        undefined -> beamtalk_class_var_errors:raise_unreachable(tag_to_name(Tag), Name, read)
    end.

-spec map_or_captured(atom(), atom(), map() | none) -> map().
map_or_captured(Tag, Name, Captured) ->
    case erlang:get(?BT_CLASS_VARS_KEY(Tag)) of
        Map when is_map(Map) -> Map;
        ?BT_CLASS_VARS_RO(_) -> mirror(Tag, Name);
        undefined when is_map(Captured) -> Captured;
        undefined -> beamtalk_class_var_errors:raise_unreachable(tag_to_name(Tag), Name, read)
    end.

%% Resolve the ETS mirror by class *name* on every read, so a restarted class
%% process is picked up transparently.
-spec mirror(atom(), atom() | undefined) -> map().
mirror(Tag, Name) ->
    Class = tag_to_name(Tag),
    case beamtalk_class_registry:class_state_snapshot_lookup(live_class_pid(Class)) of
        {ok, Map} -> Map;
        %% Registered but no snapshot row yet (a restarted class process that has
        %% not recorded its first snapshot): not an empty map.
        not_found -> beamtalk_class_var_errors:raise_no_snapshot(Class, Name)
    end.

%% No registered class process is a class-level condition, not a variable-level
%% one, so it uses the name-less message and hint (the block-oriented hint of the
%% named variant does not fit supervisor-init or `performLocally:` callers).
-spec live_class_pid(atom()) -> pid().
live_class_pid(Class) ->
    case beamtalk_class_registry:whereis_class(Class) of
        undefined -> beamtalk_class_var_errors:raise_unreachable(Class, undefined, read);
        Pid -> Pid
    end.

%% A name absent from the map is `nil` when declared (or when the declared
%% set is unknown), an error only when the declared set is known without it.
-spec read_value(atom(), atom(), map()) -> term().
read_value(Tag, Name, Map) ->
    case maps:find(Name, Map) of
        {ok, Value} ->
            Value;
        error ->
            ok = assert_declared(tag_to_name(Tag), Name),
            nil
    end.

-spec late_value(atom(), atom(), map()) -> term().
late_value(Tag, Name, Map) ->
    case maps:find(Name, Map) of
        {ok, nil} -> beamtalk_class_var_errors:raise_uninitialized(tag_to_name(Tag), Name);
        {ok, Value} -> Value;
        error -> beamtalk_class_var_errors:raise_uninitialized(tag_to_name(Tag), Name)
    end.

%% Never raises on the name: `hasField:` is a non-raising presence test.
-spec has_value(map(), atom()) -> boolean().
has_value(Map, Name) ->
    maps:is_key(Name, Map).

-spec assert_declared(atom(), atom()) -> ok.
assert_declared(Class, Name) ->
    case beamtalk_class_metadata:class_var_kinds(Class) of
        Kinds when map_size(Kinds) =:= 0 ->
            %% Empty declared-kinds map (no metadata, no live class, or no
            %% declared class variables): today's `get_class_var` answers nil,
            %% so do not claim "undeclared".
            ok;
        Kinds ->
            case maps:is_key(Name, Kinds) of
                true -> ok;
                false -> beamtalk_class_var_errors:raise_undeclared(Class, Name)
            end
    end.
