%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_vars).
-moduledoc """
Single owner of the class-variable process-dictionary key shape and of every
class-variable access (ADR 0130 §1, §2, §4, §5).

## Storage

A class's variables live in the calling process's dictionary under
`key(Class)` (`{'$bt_class_vars', Class}`). The value is either

- the live `#{VarName => Value}` map (a class-method / actor invocation that
  *installed* the home), or
- the read-only marker `{'$bt_class_vars_ro', Tag}` installed by
  `with_snapshot/2`; reads then resolve the class's ETS mirror **by class
  name** through the registry (never by pid, so a class-process restart is
  transparent) and writes raise `class_state_read_only`.

`install/2` and `uninstall/1` are the only writers of the class key together
with the `'$bt_class_vars_home'` entry, which records which key is the
"home" (the class whose invocation this process is currently running).
`snapshot/0`, `restore/1` and `protect/1` operate on the home entry only.

## Semantics notes

- Every declared class variable is present in the live map (seeded with `nil`
  by the class process), so `get/2` and `has/2` on a name absent from the map
  raise `undeclared_class_variable`. `has/2` is true when the variable is
  assigned (value is not `nil`).
- Reads with neither a live key nor a marker raise `class_state_unreachable`
  (as writes do), except the 3-arity forms, which fall back to the captured map.
- A `nil` class receiver is an internal error on every access helper.
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

-export_type([key/0, snapshot/0]).

-define(HOME, '$bt_class_vars_home').
-define(RO, '$bt_class_vars_ro').

-type key() :: {'$bt_class_vars', atom()}.
-type snapshot() :: none | {key(), map()}.

%%====================================================================
%% Key shape
%%====================================================================

-doc "The process-dictionary key holding `Class`'s variables.".
-spec key(atom()) -> key().
key(nil) ->
    nil_receiver();
key(Class) when is_atom(Class) ->
    {'$bt_class_vars', Class}.

-doc """
Raise an internal error if the class key or any home entry is already present.
Called by invocation entry points before they `install/2`.
""".
-spec assert_absent(atom()) -> ok.
assert_absent(Class) ->
    Key = key(Class),
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

-doc "Install the live map and the home entry together.".
-spec install(atom(), map()) -> ok.
install(Class, Map) when is_map(Map) ->
    Key = key(Class),
    erlang:put(Key, Map),
    erlang:put(?HOME, Key),
    ok.

-doc "Erase the class key and the home entry together.".
-spec uninstall(atom()) -> ok.
uninstall(Class) ->
    erlang:erase(key(Class)),
    erlang:erase(?HOME),
    ok.

%%====================================================================
%% Access helpers
%%====================================================================

-doc "Read a class variable.".
-spec get(atom(), atom()) -> term().
get(Class, Name) ->
    read_value(Class, Name, current_map(Class, Name)).

-doc "Read a class variable, falling back to the captured map when no key is present.".
-spec get(atom(), atom(), map()) -> term().
get(Class, Name, Captured) ->
    read_value(Class, Name, map_or_captured(Class, Captured)).

-doc "Read a `late` class variable; raises `class_var_uninitialized` when unassigned.".
-spec get_late(atom(), atom()) -> term().
get_late(Class, Name) ->
    late_value(Class, Name, current_map(Class, Name)).

-doc "`get_late/2` with a captured-map fallback.".
-spec get_late(atom(), atom(), map()) -> term().
get_late(Class, Name, Captured) ->
    late_value(Class, Name, map_or_captured(Class, Captured)).

-doc "Write a class variable. Returns the assigned value.".
-spec put(atom(), atom(), term()) -> term().
put(Class, Name, Value) ->
    Key = key(Class),
    case erlang:get(Key) of
        Map when is_map(Map) ->
            erlang:put(Key, Map#{Name => Value}),
            Value;
        {?RO, _Tag} ->
            raise_read_only(Class, Name);
        undefined ->
            raise_unreachable(Class, Name)
    end.

-doc "Reset a class variable to `nil` (same errors as `put/3`).".
-spec clear(atom(), atom()) -> nil.
clear(Class, Name) ->
    put(Class, Name, nil),
    nil.

-doc "True when the class variable is assigned (not `nil`).".
-spec has(atom(), atom()) -> boolean().
has(Class, Name) ->
    has_value(Class, Name, current_map(Class, Name)).

-doc "`has/2` with a captured-map fallback.".
-spec has(atom(), atom(), map()) -> boolean().
has(Class, Name, Captured) ->
    has_value(Class, Name, map_or_captured(Class, Captured)).

-doc """
Capture the class variables for a block: the live map when the key is present
(the resolved mirror under a read-only marker), else `Outer`.
""".
-spec capture(atom(), map()) -> map().
capture(Class, Outer) ->
    map_or_captured(Class, Outer).

%%====================================================================
%% Snapshot / restore / protect
%%====================================================================

-doc "Snapshot the home class variables: `none` or `{Key, Map}`.".
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
Run `Fun` with read-only access to `Class`'s variables when no key is present.
Installs the key plus `{'$bt_class_vars_ro', Class}` and erases exactly that in
an `after`; an outer live key (or marker) and the home entry are left alone.
""".
-spec with_snapshot(atom(), fun(() -> T)) -> T when T :: term().
with_snapshot(Class, Fun) ->
    Key = key(Class),
    case erlang:get(Key) of
        undefined ->
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

-spec current_map(atom(), atom()) -> map().
current_map(Class, Name) ->
    case erlang:get(key(Class)) of
        Map when is_map(Map) -> Map;
        {?RO, Tag} -> mirror(Tag);
        undefined -> raise_unreachable(Class, Name)
    end.

-spec map_or_captured(atom(), map()) -> map().
map_or_captured(Class, Captured) ->
    case erlang:get(key(Class)) of
        Map when is_map(Map) -> Map;
        {?RO, Tag} -> mirror(Tag);
        undefined -> Captured
    end.

%% Resolve the ETS mirror by class *name* on every read, so a restarted class
%% process is picked up transparently.
-spec mirror(atom()) -> map().
mirror(ClassName) ->
    beamtalk_class_registry:class_state_snapshot(beamtalk_class_registry:whereis_class(ClassName)).

-spec read_value(atom(), atom(), map()) -> term().
read_value(Class, Name, Map) ->
    case maps:find(Name, Map) of
        {ok, Value} -> Value;
        error -> raise_undeclared(Class, Name)
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
    case maps:find(Name, Map) of
        {ok, Value} -> Value =/= nil;
        error -> raise_undeclared(Class, Name)
    end.

-spec nil_receiver() -> no_return().
nil_receiver() ->
    Error0 = beamtalk_error:new(internal_error, 'UndefinedObject'),
    Error = beamtalk_error:with_message(
        Error0, <<"class-variable access with a nil class receiver">>
    ),
    beamtalk_error:raise(Error).

-spec raise_unreachable(atom(), atom()) -> no_return().
raise_unreachable(Class, Name) ->
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format(
                "Cannot access class variable '~s' of ~s: no class state is reachable here",
                [Name, Class]
            )
        )
    ),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    Error = beamtalk_error:with_hint(
        Error2,
        <<
            "Class variables are only accessible while running a class method of the "
            "owning class (or a block created there); send a class-side message instead"
        >>
    ),
    beamtalk_error:raise(Error).

-spec raise_read_only(atom(), atom()) -> no_return().
raise_read_only(Class, Name) ->
    Error0 = beamtalk_error:new(class_state_read_only, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format(
                "Cannot assign class variable '~s' of ~s: class state is read-only here",
                [Name, Class]
            )
        )
    ),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    Error = beamtalk_error:with_hint(
        Error2,
        iolist_to_binary(
            io_lib:format(
                "Assign class variables from a class method of ~s (run through its class entry point)",
                [Class]
            )
        )
    ),
    beamtalk_error:raise(Error).

-spec raise_undeclared(atom(), atom()) -> no_return().
raise_undeclared(Class, Name) ->
    Error0 = beamtalk_error:new(undeclared_class_variable, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format("~s has no class variable named '~s'", [Class, Name])
        )
    ),
    beamtalk_error:raise(beamtalk_error:with_details(Error1, #{class_variable => Name})).

-spec raise_uninitialized(atom(), atom()) -> no_return().
raise_uninitialized(Class, Name) ->
    Hint = iolist_to_binary(
        io_lib:format(
            "~s class variable '~s' is declared `late` and has not been assigned yet",
            [Class, Name]
        )
    ),
    Error0 = beamtalk_error:new(uninitialized_state_error, Class, 'fieldAt:'),
    beamtalk_error:raise(beamtalk_error:with_hint(Error0, Hint)).
