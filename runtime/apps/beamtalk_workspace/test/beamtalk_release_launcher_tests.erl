%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_launcher_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% Covers `beamtalk_release_launcher:rpc_entry/3` — the function
%%% `rpc_client_main/0` dispatches into over distribution (BT-3573). The
%%% halting entry points (`eval_main/0`, `rpc_client_main/0`,
%%% `ping_client_main/0`, `stop_client_main/0`) call `erlang:halt/1` and are
%%% exercised by the CLI integration tests
%%% (`crates/beamtalk-cli/tests/cli/cli_release.rs`), not here.

rpc_entry_unknown_class_returns_class_not_found_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(
        <<"ThisClassDoesNotExist">>, <<"run">>, []
    ),
    ?assertMatch({error, #beamtalk_error{kind = class_not_found}}, Result).

rpc_entry_invalid_selector_returns_invalid_argument_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(
        <<"Counter">>, <<"move:to:">>, []
    ),
    ?assertMatch({error, #beamtalk_error{kind = invalid_argument}}, Result).

rpc_entry_empty_class_returns_invalid_argument_error_test() ->
    Result = beamtalk_release_launcher:rpc_entry(<<>>, <<"run">>, []),
    ?assertMatch({error, #beamtalk_error{kind = invalid_argument}}, Result).
