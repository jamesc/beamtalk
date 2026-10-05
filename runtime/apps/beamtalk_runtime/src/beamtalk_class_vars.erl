%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_vars).
-moduledoc """
Single owner of the class-variable process-dictionary key shape and of every
class-variable access (ADR 0130 §1, §2, §4, §5).

## Storage

A class's variables live in the calling process's dictionary under
`key(ClassName)` (`{'$bt_class_vars', ClassName}`). The value is either

- the live `#{VarName => Value}` map (a class-method invocation that
  *installed* it), or
- the read-only marker `{'$bt_class_vars_ro', ClassName}` installed by
  `with_snapshot/2` for runtime-owned out-of-process regions; `put/3` and
  `clear/2` raise `class_state_read_only` under it, and reads resolve the
  class's ETS mirror **by class name** through the registry (never by pid,
  so a class-process restart is transparent).

`install/2` and `uninstall/1` are the only writers of the class key together
with the `'$bt_class_vars_home'` entry, which records the key of the live
invocation. `snapshot/0`, `restore/1` and `protect/1` operate on the home
entry only.

## Receivers

`key/1`, `assert_absent/1`, `install/2` and `uninstall/1` are the invocation
entry points' API and take a class *name* (or the key). The access helpers
(`get`, `get_late`, `put`, `clear`, `has`, `capture`, `with_snapshot`) take
the `ClassSelf` class object (`#beamtalk_object{}` whose class tag is
`'Name class'`) and derive the key from its tag; a `nil` or non-class-object
receiver is an internal error.

## Semantics

- **Key presence is the "at home" test.** A read with no key and no marker
  raises `class_state_unreachable`; the 3-arity forms fall back to the
  creation-time capture (`capture/2`), raising only when the capture is `none`.
- `has/2` is key presence in the map (as `hasField:` is today); `clear/2`
  removes the key (as `clearField:` is today). `get/2` and `has/2` on a name
  that is neither in the map nor a declared class variable raise
  `undeclared_class_variable`; a declared name absent from the map reads `nil`.
- `get_late/2` raises the same `uninitialized_state_error` as the class
  gen_server's `get_class_var` for an unassigned (`nil` or absent) variable.
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").

-export([
    key/1,
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

-define(HOME, '$bt_class_vars_home').
-define(RO, '$bt_class_vars_ro').

-type key() :: {'$bt_class_vars', atom()}.
-type snapshot() :: none | {key(), map()}.
-type class_self() :: #beamtalk_object{}.

%%====================================================================
%% Key shape and invocation entry points
%%====================================================================

-doc "The process-dictionary key holding `ClassName`'s variables.".
-spec key(atom()) -> key().
key(nil) ->
    nil_receiver();
key(ClassName) when is_atom(ClassName) ->
    {'$bt_class_vars', ClassName}.

-doc """
Raise an internal error if `Key` is already present or any home entry is
present (a live invocation of any class in this process). Called by the
invocation entry points before `install/2`.
""".
-spec assert_absent(key()) -> ok.
assert_absent({'$bt_class_vars', Class} = Key) ->
    case {erlang:get(Key), erlang:get(?HOME)} of
        {undefined, undefined} ->
            ok;
        {KeyVal, HomeVal} ->
            Details = #{class => Class, key_present => KeyVal =/= undefined, home => HomeVal},
            ?LOG_ERROR(
                "class-variable home already present",
                Details#{domain => [beamtalk, runtime]}
            ),
            Error0 = beamtalk_error:new(internal_error, Class, assert_absent),
            Error1 = beamtalk_error:with_message(
                Error0, <<"class-variable home is already installed in this process">>
            ),
            beamtalk_error:raise(beamtalk_error:with_details(Error1, Details))
    end.

-doc "Install the live map under `Key` and record it as the home entry.".
-spec install(key(), map()) -> ok.
install({'$bt_class_vars', _} = Key, Map) when is_map(Map) ->
    erlang:put(Key, Map),
    erlang:put(?HOME, Key),
    ok.

-doc "Erase `Key` and the home entry together.".
-spec uninstall(key()) -> ok.
uninstall({'$bt_class_vars', _} = Key) ->
    erlang:erase(Key),
    erlang:erase(?HOME),
    ok.

%%====================================================================
%% Access helpers (ClassSelf receivers)
%%====================================================================

-doc "Read a class variable (live map, or the read-only mirror).".
-spec get(class_self(), atom()) -> term().
get(ClassSelf, Name) ->
    Class = class_name(ClassSelf),
    read_value(Class, Name, current_map(Class, Name)).

-doc "Read a class variable; with no key present, read the creation-time `Captured` map.".
-spec get(class_self(), atom(), map() | none) -> term().
get(ClassSelf, Name, Captured) ->
    Class = class_name(ClassSelf),
    read_value(Class, Name, map_or_captured(Class, Name, Captured)).

-doc "Read a `late` class variable; raises `uninitialized_state_error` when unassigned.".
-spec get_late(class_self(), atom()) -> term().
get_late(ClassSelf, Name) ->
    Class = class_name(ClassSelf),
    late_value(Class, Name, current_map(Class, Name)).

-doc "`get_late/2` with a creation-time capture fallback.".
-spec get_late(class_self(), atom(), map() | none) -> term().
get_late(ClassSelf, Name, Captured) ->
    Class = class_name(ClassSelf),
    late_value(Class, Name, map_or_captured(Class, Name, Captured)).

-doc "Write a class variable. Returns the assigned value.".
-spec put(class_self(), atom(), term()) -> term().
put(ClassSelf, Name, Value) ->
    Class = class_name(ClassSelf),
    Key = key(Class),
    case erlang:get(Key) of
        Map when is_map(Map) ->
            erlang:put(Key, Map#{Name => Value}),
            Value;
        {?RO, _} ->
            raise_read_only(Class, Name);
        undefined ->
            raise_unreachable(Class, Name, write)
    end.

-doc "Remove a class variable (`clearField:`); same errors as `put/3`. Returns `ClassSelf` (`clearField: -> Self`).".
-spec clear(class_self(), atom()) -> class_self().
clear(ClassSelf, Name) ->
    Class = class_name(ClassSelf),
    Key = key(Class),
    case erlang:get(Key) of
        Map when is_map(Map) ->
            erlang:put(Key, maps:remove(Name, Map)),
            ClassSelf;
        {?RO, _} ->
            raise_read_only(Class, Name);
        undefined ->
            raise_unreachable(Class, Name, write)
    end.

-doc "Presence test (`hasField:`).".
-spec has(class_self(), atom()) -> boolean().
has(ClassSelf, Name) ->
    Class = class_name(ClassSelf),
    has_value(Class, Name, current_map(Class, Name)).

-doc "`has/2` with a creation-time capture fallback.".
-spec has(class_self(), atom(), map() | none) -> boolean().
has(ClassSelf, Name, Captured) ->
    Class = class_name(ClassSelf),
    has_value(Class, Name, map_or_captured(Class, Name, Captured)).

-doc """
Capture for a block literal: the live map when the key is present (the
resolved mirror under a read-only marker), else `Outer` (the enclosing
block's capture, or `none` at method level).
""".
-spec capture(class_self(), map() | none) -> map() | none.
capture(ClassSelf, Outer) ->
    Class = class_name(ClassSelf),
    case erlang:get(key(Class)) of
        Map when is_map(Map) -> Map;
        {?RO, _} -> mirror(Class, undefined);
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
Run `Fun`; on any exception other than a `$bt_nlr` throw (either tuple shape)
restore the pre-call snapshot and re-raise.
""".
-spec protect(fun(() -> T)) -> T when T :: term().
protect(Fun) ->
    Snap = snapshot(),
    try
        Fun()
    catch
        throw:{'$bt_nlr', _, _} = Nlr:Stack ->
            erlang:raise(throw, Nlr, Stack);
        throw:{'$bt_nlr', _, _, _} = Nlr:Stack ->
            erlang:raise(throw, Nlr, Stack);
        Class:Reason:Stack ->
            restore(Snap),
            erlang:raise(Class, Reason, Stack)
    end.

-doc """
Run `Fun` with the class's variables readable from the ETS mirror (resolved
by class name) when its key is absent: installs the key plus
`{'$bt_class_vars_ro', ClassName}`, erases exactly that in an `after`. When the
key is already present (a live map or an outer snapshot) `Fun` just runs.
Never touches the home entry. A name with no live class raises
`class_state_unreachable`.
""".
-spec with_snapshot(class_self(), fun(() -> T)) -> T when T :: term().
with_snapshot(ClassSelf, Fun) ->
    Class = class_name(ClassSelf),
    Key = key(Class),
    case erlang:get(Key) of
        undefined ->
            _ = live_class_pid(Class, undefined),
            erlang:put(Key, {?RO, Class}),
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

%% Derive the class name from a ClassSelf (`'Name class'` tag). An instance
%% tag (no ` class` suffix) or an unknown base atom is a non-class receiver:
%% internal error.
-spec class_name(term()) -> atom().
class_name(#beamtalk_object{class = Tag}) when is_atom(Tag), Tag =/= nil ->
    TagBin = atom_to_binary(Tag, utf8),
    case beamtalk_class_registry:class_display_name(TagBin) of
        TagBin ->
            nil_receiver();
        Base ->
            try
                binary_to_existing_atom(Base, utf8)
            catch
                error:badarg -> nil_receiver()
            end
    end;
class_name(_) ->
    nil_receiver().

-spec current_map(atom(), atom()) -> map().
current_map(Class, Name) ->
    case erlang:get(key(Class)) of
        Map when is_map(Map) -> Map;
        {?RO, _} -> mirror(Class, Name);
        undefined -> raise_unreachable(Class, Name, read)
    end.

-spec map_or_captured(atom(), atom(), map() | none) -> map().
map_or_captured(Class, Name, Captured) ->
    case erlang:get(key(Class)) of
        Map when is_map(Map) -> Map;
        {?RO, _} -> mirror(Class, Name);
        undefined when is_map(Captured) -> Captured;
        undefined -> raise_unreachable(Class, Name, read)
    end.

%% Resolve the ETS mirror by class *name* on every read, so a restarted class
%% process is picked up transparently.
-spec mirror(atom(), atom() | undefined) -> map().
mirror(Class, Name) ->
    beamtalk_class_registry:class_state_snapshot(live_class_pid(Class, Name)).

-spec live_class_pid(atom(), atom() | undefined) -> pid().
live_class_pid(Class, Name) ->
    case beamtalk_class_registry:whereis_class(Class) of
        undefined -> raise_unreachable(Class, Name, read);
        Pid -> Pid
    end.

%% A name absent from the map is `nil` when declared, an error otherwise.
-spec read_value(atom(), atom(), map()) -> term().
read_value(Class, Name, Map) ->
    case maps:find(Name, Map) of
        {ok, Value} ->
            Value;
        error ->
            ok = assert_declared(Class, Name),
            nil
    end.

-spec late_value(atom(), atom(), map()) -> term().
late_value(Class, Name, Map) ->
    case maps:find(Name, Map) of
        {ok, nil} -> raise_uninitialized(Class, Name);
        {ok, Value} -> Value;
        error -> raise_uninitialized(Class, Name)
    end.

-spec has_value(atom(), atom(), map()) -> boolean().
has_value(Class, Name, Map) ->
    case maps:is_key(Name, Map) of
        true ->
            true;
        false ->
            ok = assert_declared(Class, Name),
            false
    end.

-spec assert_declared(atom(), atom()) -> ok.
assert_declared(Class, Name) ->
    Kinds = beamtalk_behaviour_intrinsics:classAllClassVarKindsByName(Class),
    case maps:is_key(Name, Kinds) of
        true -> ok;
        false -> raise_undeclared(Class, Name)
    end.

-spec nil_receiver() -> no_return().
nil_receiver() ->
    Error0 = beamtalk_error:new(internal_error, 'UndefinedObject'),
    Error = beamtalk_error:with_message(
        Error0, <<"class-variable access with a nil or non-class receiver">>
    ),
    beamtalk_error:raise(Error).

%% ADR 0130 §5: message, details and hint of `class_state_unreachable`.
-spec raise_unreachable(atom(), atom() | undefined, read | write) -> no_return().
raise_unreachable(Class, undefined, _Mode) ->
    %% Name-less variant (`capture/2`, `with_snapshot/2`): no live class process.
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format("~s's class state cannot be reached: no live ~s class process", [
                Class, Class
            ])
        )
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error1, <<"The class is not loaded or has been removed; load it and retry.">>
        )
    );
raise_unreachable(Class, Name, Mode) ->
    Verb =
        case Mode of
            read -> "read";
            write -> "written"
        end,
    Message = iolist_to_binary(
        io_lib:format("~s's class variable ~s cannot be ~s from this process", [Class, Name, Verb])
    ),
    Hint = iolist_to_binary(
        io_lib:format(
            "A block that writes ~s's class variables ran outside any ~s class "
            "method (it was passed to another class's class method or an actor, or "
            "stored or returned and run later outside ~s's own methods). A block "
            "can read ~s's class variables anywhere, as the values they had when the "
            "block was made, but can only write them from ~s's own method: return the "
            "value and assign it there.",
            [Class, Class, Class, Class, Class]
        )
    ),
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error2, Hint)).

-spec raise_read_only(atom(), atom()) -> no_return().
raise_read_only(Class, Name) ->
    Message = iolist_to_binary(
        io_lib:format("~s's class variable ~s is read-only here", [Class, Name])
    ),
    Hint = iolist_to_binary(
        io_lib:format(
            "This code runs against a read-only snapshot of ~s's class variables "
            "(supervisor definition, a class `initialize:` hook, or `performLocally:`). "
            "Assign the variable from one of ~s's own class methods, sent as a normal "
            "class-side message.",
            [Class, Class]
        )
    ),
    Error0 = beamtalk_error:new(class_state_read_only, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error2, Hint)).

-spec raise_undeclared(atom(), atom()) -> no_return().
raise_undeclared(Class, Name) ->
    Message = iolist_to_binary(
        io_lib:format("~s has no class variable named ~s", [Class, Name])
    ),
    Error0 = beamtalk_error:new(undeclared_class_variable, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error2, <<"Declare it with `classState:` on the class, or check the spelling.">>
        )
    ).

%% Same kind and hint as the class gen_server's `get_class_var` error (one
%% source for the text), but no selector: no `fieldAt:` was sent here.
-spec raise_uninitialized(atom(), atom()) -> no_return().
raise_uninitialized(Class, Name) ->
    #beamtalk_error{hint = Hint} =
        beamtalk_object_class:class_var_uninitialized_error(Class, Name),
    beamtalk_error:raise(
        beamtalk_error:with_hint(beamtalk_error:new(uninitialized_state_error, Class), Hint)
    ).
