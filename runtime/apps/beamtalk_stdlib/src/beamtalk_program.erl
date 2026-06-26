%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_program).

%%% **DDD Context:** Object System Context

-moduledoc """
Program class implementation — the running program/invocation (ADR 0099 §2).

`Program` names *this running program*. In Phase 2 it exposes only `name`, a
convenience for usage/`--help` text; the two-tier exit (`exit`/`exit:`) lands in
Phase 3. All methods are class-side — `Program` has no instances.

## `name`

Returns the invoked program's name as a `String`:

* under a packaged escript it is the invoked filename (`escript:script_name/0`,
  wired by the escript bootstrap in Phase 4 via the `program_name` app env);
* under `beamtalk run` there is no `argv[0]`, so it is the literal `"beamtalk"`.

The value is read from the `beamtalk_runtime` `program_name` application
environment, seeded at boot by each entry harness (run-mode startup sets
`"beamtalk"`; the escript `main/1` sets the script name). This app-env signal is
process-independent and set synchronously before any user code dispatches — the
mechanism settled for the `program_name` Open Question (ADR 0099 §2). Absent
(e.g. the persistent/connected workspace path), it defaults to `"beamtalk"`.
""".

-export([name/0]).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Return the invoked program's name as a binary String.".
-spec name() -> binary().
name() ->
    case application:get_env(beamtalk_runtime, program_name) of
        {ok, Name} when is_binary(Name) -> Name;
        {ok, Name} when is_list(Name) -> list_to_binary(Name);
        _ -> <<"beamtalk">>
    end.
