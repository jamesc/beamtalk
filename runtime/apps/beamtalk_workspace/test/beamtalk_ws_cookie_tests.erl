%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_cookie_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit coverage for `beamtalk_ws_cookie:adopt_env_cookie/0` (BT-5126): the
cookie reaches the node through the environment, never argv.
""".

-include_lib("eunit/include/eunit.hrl").

-define(VAR, "BEAMTALK_ERL_COOKIE").

env_var_name_test() ->
    ?assertEqual(?VAR, beamtalk_ws_cookie:env_var()).

%% EUnit nodes are non-distributed: there is no cookie to set, but the
%% variable must still be scrubbed so child processes never inherit it.
non_distributed_node_scrubs_variable_test() ->
    true = os:putenv(?VAR, "s3cr3t"),
    ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie()),
    ?assertEqual(false, os:getenv(?VAR)).

unset_variable_is_none_test() ->
    true = os:unsetenv(?VAR),
    ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie()).

empty_variable_is_none_test() ->
    true = os:putenv(?VAR, ""),
    ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie()),
    ?assertEqual(false, os:getenv(?VAR)).
