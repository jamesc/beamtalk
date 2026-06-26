%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_program_tests).

-moduledoc """
Unit tests for the `Program` class runtime (`beamtalk_program`), focused on the
two-tier exit (ADR 0099 §3).

The node-owning branch of `exit:` calls `erlang:halt/1` and so cannot be
exercised under EUnit (it would kill the test VM); these tests pin the
**connected-mode** branch — `Program exit: Code` raising the tagged `script_exit`
signal the session evaluator catches (BT-2688) — and the value/type validation,
which runs before the node-owning gate.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Force the connected (non-node-owning) context so `exit:` raises the
%% `script_exit` signal rather than halting the EUnit VM. Saved/restored around
%% each test so a leaked `node_owning => true` cannot turn `Program exit:` into an
%% `erlang:halt/1` that kills the runner.
connected_setup() ->
    Prev = application:get_env(beamtalk_runtime, node_owning),
    application:set_env(beamtalk_runtime, node_owning, false),
    Prev.

connected_teardown(Prev) ->
    case Prev of
        {ok, V} -> application:set_env(beamtalk_runtime, node_owning, V);
        undefined -> application:unset_env(beamtalk_runtime, node_owning)
    end,
    ok.

%% BT-2688: in a connected/shared context, `Program exit: Code` raises the tagged
%% `{beamtalk_script_exit, Code}` signal (caught by the session evaluator) instead
%% of halting the node or raising the old `#unsupported` error.
connected_exit_with_code_raises_script_exit_test_() ->
    {setup, fun connected_setup/0, fun connected_teardown/1, fun(_) ->
        [
            ?_assertThrow({beamtalk_script_exit, 5}, beamtalk_program:'exit:'(5)),
            ?_assertThrow({beamtalk_script_exit, 0}, beamtalk_program:'exit:'(0)),
            ?_assertThrow({beamtalk_script_exit, 255}, beamtalk_program:'exit:'(255)),
            %% Zero-arg `exit`/`exit:` shim both route through `exit:(0)`.
            ?_assertThrow({beamtalk_script_exit, 0}, beamtalk_program:exit()),
            ?_assertThrow({beamtalk_script_exit, 7}, beamtalk_program:exit(7))
        ]
    end}.

%% Out-of-range is a wrong *value*, validated before the node-owning gate, so it
%% raises a structured error in any context (consistent with `System halt:`).
exit_out_of_range_raises_invalid_argument_test_() ->
    {setup, fun connected_setup/0, fun connected_teardown/1, fun(_) ->
        [
            ?_assertError(
                #{error := #beamtalk_error{kind = invalid_argument}},
                beamtalk_program:'exit:'(999)
            ),
            ?_assertError(
                #{error := #beamtalk_error{kind = invalid_argument}},
                beamtalk_program:'exit:'(-1)
            )
        ]
    end}.

%% Non-integer is a wrong *type*, also validated before the node-owning gate.
exit_non_integer_raises_type_error_test_() ->
    {setup, fun connected_setup/0, fun connected_teardown/1, fun(_) ->
        [
            ?_assertError(
                #{error := #beamtalk_error{kind = type_error}},
                beamtalk_program:'exit:'(<<"two">>)
            )
        ]
    end}.
