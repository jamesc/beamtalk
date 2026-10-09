%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_vars_test_support).
-moduledoc """
EUnit helpers for tests that need a class-variable home in the test process
(ADR 0130 §1).

Tests never spell the `{'$bt_class_vars', Tag}` key or the home-entry atom:
they go through `beamtalk_class_vars:key/1`, `key_for_tag/1`, `install/2`,
`uninstall/1` and `snapshot/0`, or through the helpers here (which read the
home entry through the generated `?BT_CLASS_VARS_HOME` macro). The one
conformance test that pins the literal key shape is
`beamtalk_class_vars_tests:key_test/0`.
""".

-include("beamtalk_class_vars_keys.hrl").

-export([with_home/3, with_home_key/3, home_key/0, clean/1]).

-doc """
Run `Fun` with `ClassName`'s live class-variable map `Map` installed as this
process's home (`beamtalk_class_vars:install/2`), uninstalling it afterwards
whatever `Fun` does. `Fun` is `fun() -> _` or `fun(Key) -> _`, where `Key` is
the installed class key. Any stale key or home entry is cleared first.
""".
-spec with_home(atom(), map(), fun(() -> T) | fun((beamtalk_class_vars:key()) -> T)) -> T when
    T :: term().
with_home(ClassName, Map, Fun) when is_atom(ClassName) ->
    with_home_key(beamtalk_class_vars:key(ClassName), Map, Fun).

-doc """
`with_home/3` for a caller that already holds the class key (e.g. built with
`beamtalk_class_vars:key_for_tag/1` from a metaclass tag).
""".
-spec with_home_key(
    beamtalk_class_vars:key(), map(), fun(() -> T) | fun((beamtalk_class_vars:key()) -> T)
) -> T when
    T :: term().
with_home_key(Key, Map, Fun) ->
    clean(Key),
    ok = beamtalk_class_vars:install(Key, Map),
    try
        case erlang:fun_info(Fun, arity) of
            {arity, 0} -> Fun();
            {arity, 1} -> Fun(Key)
        end
    after
        clean(Key)
    end.

-doc """
The class key the home entry names in this process, or `undefined` when no
class-method invocation is installed. Reads the entry itself (not
`beamtalk_class_vars:snapshot/0`), so a home left behind without its class
key still shows up.
""".
-spec home_key() -> beamtalk_class_vars:key() | undefined.
home_key() ->
    erlang:get(?BT_CLASS_VARS_HOME).

-doc "Erase `Key` and the home entry (`beamtalk_class_vars:uninstall/1`).".
-spec clean(beamtalk_class_vars:key()) -> ok.
clean(Key) ->
    beamtalk_class_vars:uninstall(Key).
