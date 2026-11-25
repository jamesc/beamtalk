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

%% The variable must always be scrubbed so child processes never inherit it.
%% Whether the cookie is adopted depends on whether the (shared) EUnit node
%% happens to be distributed: another test may have started distribution, so
%% assert the result for whichever state this node is in, and restore the
%% node's cookie so no other test sees `s3cr3t`.
adopt_scrubs_variable_test() ->
    Node = node(),
    Orig = erlang:get_cookie(),
    true = os:putenv(?VAR, "s3cr3t"),
    try
        case Node of
            'nonode@nohost' ->
                ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie());
            _ ->
                ?assertEqual(adopted, beamtalk_ws_cookie:adopt_env_cookie()),
                ?assertEqual('s3cr3t', erlang:get_cookie())
        end,
        ?assertEqual(false, os:getenv(?VAR))
    after
        case Node of
            'nonode@nohost' -> ok;
            _ -> true = erlang:set_cookie(Node, Orig)
        end
    end.

unset_variable_is_none_test() ->
    true = os:unsetenv(?VAR),
    ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie()).

empty_variable_is_none_test() ->
    true = os:putenv(?VAR, ""),
    ?assertEqual(none, beamtalk_ws_cookie:adopt_env_cookie()),
    ?assertEqual(false, os:getenv(?VAR)).
