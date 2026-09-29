%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_actor_registry).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture for the node-wide actor registry (BT-3633).

`beamtalk_actor_registry` is owned by `beamtalk_runtime_sup`, so a test that
needs a fresh, empty registry (or none at all) cannot start or stop it
without disturbing the supervised one. These helpers swap the registered name
for the duration of a fun and put the supervised process back afterwards.
""".

-export([with_registry/1, with_registered/2, without_registry/1, begin_isolated/0, end_isolated/0]).

-define(NAME, beamtalk_actor_registry).

-doc """
Run `Fun(RegistryPid)` against a fresh, empty `beamtalk_repl_actors` registered
as `beamtalk_actor_registry`. The supervised registry (if any) is restored
afterwards.
""".
-spec with_registry(fun((pid()) -> T)) -> T when T :: term().
with_registry(Fun) ->
    with_swapped(fun() ->
        {ok, Pid} = gen_server:start({local, ?NAME}, beamtalk_repl_actors, [], []),
        try
            Fun(Pid)
        after
            catch gen_server:stop(Pid)
        end
    end).

-doc "Run `Fun()` with `Pid` (a stub) registered as `beamtalk_actor_registry`.".
-spec with_registered(pid(), fun(() -> T)) -> T when T :: term().
with_registered(Pid, Fun) ->
    with_swapped(fun() ->
        true = register(?NAME, Pid),
        try
            Fun()
        after
            catch unregister(?NAME)
        end
    end).

-doc "Run `Fun()` with no process registered as `beamtalk_actor_registry`.".
-spec without_registry(fun(() -> T)) -> T when T :: term().
without_registry(Fun) ->
    with_swapped(Fun).

-doc """
Swap in a fresh, empty registry until `end_isolated/0`, for fixtures whose
setup and cleanup run in different steps (a `{setup, ...}` generator). Not
re-entrant: one isolation at a time.
""".
-spec begin_isolated() -> ok.
begin_isolated() ->
    Original = whereis(?NAME),
    case Original of
        undefined -> ok;
        _ -> unregister(?NAME)
    end,
    {ok, Fresh} = gen_server:start({local, ?NAME}, beamtalk_repl_actors, [], []),
    persistent_term:put({?MODULE, isolated}, {Original, Fresh}),
    ok.

-doc "Undo `begin_isolated/0`: stop the fresh registry and restore the supervised one.".
-spec end_isolated() -> ok.
end_isolated() ->
    case persistent_term:get({?MODULE, isolated}, none) of
        none ->
            ok;
        {Original, Fresh} ->
            _ = persistent_term:erase({?MODULE, isolated}),
            catch gen_server:stop(Fresh),
            case whereis(?NAME) of
                undefined -> ok;
                _ -> catch unregister(?NAME)
            end,
            case is_pid(Original) andalso is_process_alive(Original) of
                true -> catch register(?NAME, Original);
                false -> ok
            end,
            ok
    end.

%% Take the name away from the supervised registry, run Fun, give it back.
with_swapped(Fun) ->
    Original = whereis(?NAME),
    case Original of
        undefined -> ok;
        _ -> unregister(?NAME)
    end,
    try
        Fun()
    after
        case whereis(?NAME) of
            undefined -> ok;
            _ -> catch unregister(?NAME)
        end,
        case Original of
            undefined ->
                ok;
            _ ->
                case is_process_alive(Original) of
                    true -> catch register(?NAME, Original);
                    false -> ok
                end
        end
    end.
