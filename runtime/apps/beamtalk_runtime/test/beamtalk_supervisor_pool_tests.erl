%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_pool_tests).

%%% **DDD Context:** Runtime Context

-moduledoc """
EUnit tests for `beamtalk_supervisor_pool` error paths that lacked coverage
per the nightly erlang-coverage-reports artifact (53% / 22 of 41 lines).

No Beamtalk runtime is started: `beamtalk_class_metadata:foldl_modules/2`
returns its empty accumulator when the ETS table is undefined, and
`beamtalk_class_derived_cache:fetch/2` computes directly when no generation
counter exists in `persistent_term`.  This keeps the tests fast and avoids
the application-start overhead for paths that require no runtime.
""".

-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%% OTP supervisor callback: used by the fixture supervisor started in pool tests.
-behaviour(supervisor).
-export([init/1, start_pool_worker/0]).

%%====================================================================
%% Tests: stale_handle_error/2
%%====================================================================

%% `stale_handle_error/2` returns `{error, #beamtalk_error{kind = stale_handle}}`
%% with the class and selector stamped from its arguments.  The `?LOG_WARNING`
%% inside it is intentional; the warning appearing in test output is expected.
stale_handle_error_returns_error_tuple_test() ->
    Result = beamtalk_supervisor_pool:stale_handle_error('MySup', stop),
    ?assertMatch(
        {error, #beamtalk_error{kind = stale_handle, class = 'MySup', selector = stop}},
        Result
    ).

stale_handle_error_records_class_and_selector_test() ->
    {error, Err} = beamtalk_supervisor_pool:stale_handle_error('AnotherSup', 'with:'),
    ?assertEqual(stale_handle, Err#beamtalk_error.kind),
    ?assertEqual('AnotherSup', Err#beamtalk_error.class),
    ?assertEqual('with:', Err#beamtalk_error.selector).

%%====================================================================
%% Tests: with_pool_child_class/3 — no child-class key, no running children
%%====================================================================

%% When a plain OTP supervisor has no `?CHILD_CLASS_KEY` in its process
%% dictionary and no running children, `with_pool_child_class/3` must return
%% `{error, #beamtalk_error{kind = child_class_unknown}}`.
%%
%% Path exercised:
%%   with_pool_child_class/3 (line 40 error branch)
%%     → pool_child_class/1 → sup_dictionary_value → undefined
%%     → child_class_from_children/2 → which_children=[], module_to_class_map={}
%%     → child_class_unknown error
with_pool_child_class_unknown_class_test() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    Self = {beamtalk_supervisor, 'BtPoolTestSup', ?MODULE, SupPid},
    Result = beamtalk_supervisor_pool:with_pool_child_class(
        Self, mySelector, fun(_, _) -> {ok, reached} end
    ),
    beamtalk_test_proc:stop(SupPid),
    ?assertMatch(
        {error, #beamtalk_error{
            kind = child_class_unknown, class = 'BtPoolTestSup', selector = mySelector
        }},
        Result
    ).

%% The error's selector is the one passed to `with_pool_child_class/3`, not the
%% `childClass` atom that `child_class_from_children/2` builds into its error.
with_pool_child_class_stamps_caller_selector_test() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    Self = {beamtalk_supervisor, 'BtPoolTestSup', ?MODULE, SupPid},
    Result = beamtalk_supervisor_pool:with_pool_child_class(
        Self, 'spawnWith:', fun(_, _) -> {ok, reached} end
    ),
    beamtalk_test_proc:stop(SupPid),
    {error, Err} = Result,
    ?assertEqual(child_class_unknown, Err#beamtalk_error.kind),
    ?assertEqual('BtPoolTestSup', Err#beamtalk_error.class),
    ?assertEqual('spawnWith:', Err#beamtalk_error.selector).

%%====================================================================
%% OTP supervisor callback
%%====================================================================

%% `start_pool_worker/0` is the child start fun listed in the spec template.
%% It is never actually called in these tests: no children are added.
start_pool_worker() ->
    ignore.

init([]) ->
    ChildSpec = #{
        id => pool_worker,
        start => {?MODULE, start_pool_worker, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    },
    {ok, {#{strategy => simple_one_for_one, intensity => 0, period => 1}, [ChildSpec]}}.
