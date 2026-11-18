%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_fake_native).
-behaviour(gen_server).

-moduledoc """
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
Fixture for the workspace actor-list tests (BT-4960): a fake `native:` backing
gen_server (ADR 0056: no `'$beamtalk_actor'` marker, found by its
`'$initial_call'` through the backing-module index) and a stand-in
`beamtalk_workspace_sup` whose child is such a process, i.e. a "system" actor
like `TranscriptStream`.

`with_native/2` registers the process under one or more classes that share
this module as their backing module. `with_system_native/2` does the same for
a process that is a child of the stand-in workspace supervisor.
""".

-export([with_native/2, with_system_native/2]).
%% gen_server / supervisor callbacks
-export([init/1, handle_call/3, handle_cast/2, start_link/0]).

%% Run `Fun(Pid)` with an unmarked native gen_server registered under `Classes`.
-spec with_native([atom()], fun((pid()) -> T)) -> T when T :: term().
with_native(Classes, Fun) ->
    {ok, Pid} = gen_server:start({local, native_name()}, ?MODULE, [], []),
    with_index(Pid, Classes, Fun).

%% Like `with_native/2`, but the process is a child of a stand-in
%% `beamtalk_workspace_sup` (the way `Transcript` is a child of the real one).
-spec with_system_native([atom()], fun((pid()) -> T)) -> T when T :: term().
with_system_native(Classes, Fun) ->
    undefined = whereis(beamtalk_workspace_sup),
    {ok, Sup} = supervisor:start_link({local, beamtalk_workspace_sup}, ?MODULE, sup),
    unlink(Sup),
    [{_, Pid, _, _}] = supervisor:which_children(Sup),
    try
        with_index(Pid, Classes, Fun)
    after
        MRef = erlang:monitor(process, Sup),
        exit(Sup, shutdown),
        ?awaitMessage({'DOWN', MRef, process, Sup, _})
    end.

with_index(Pid, Classes, Fun) ->
    beamtalk_class_registry:ensure_backing_module_index_table(),
    ClassProcs = [{C, start_class(C)} || C <- Classes],
    ets:insert(beamtalk_backing_module_index, [{?MODULE, C, P} || {C, P} <- ClassProcs]),
    try
        Fun(Pid)
    after
        ets:match_delete(beamtalk_backing_module_index, {?MODULE, '_', '_'}),
        [beamtalk_test_class:stop(P) || {_, P} <- ClassProcs],
        try
            gen_server:stop(Pid)
        catch
            exit:_ -> ok
        end
    end.

%% A real class process backed by this module, so `module_name_safe/1` answers.
start_class(Class) ->
    beamtalk_test_class:start(Class, #{
        name => Class,
        module => ?MODULE,
        superclass => none,
        instance_methods => #{},
        class_methods => #{}
    }).

native_name() ->
    beamtalk_test_unique:atom("bt4960_native").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(sup) ->
    {ok,
        {#{strategy => one_for_one, intensity => 0, period => 1}, [
            #{id => native, start => {?MODULE, start_link, []}, restart => temporary}
        ]}};
init([]) ->
    {ok, #{fake => native}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
