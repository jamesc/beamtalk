%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_noop_log_handler).

-moduledoc """
Minimal OTP logger handler for use in EUnit tests.

Provides a logger handler whose module is not `logger_std_h`, so tests for
`beamtalk_script_harness:flush_loggers/0` can exercise the catch-all arm that
skips non-`logger_std_h` handlers without touching the filesystem or calling
any real I/O.
""".

-export([log/2]).

-doc "No-op log callback — discards every log event.".
log(_LogEvent, _Config) ->
    ok.
