%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_program).

%%% **DDD Context:** Object System Context

-moduledoc """
Program class implementation — the running program/invocation (ADR 0099 §2).

`Program` names *this running program* and ends it. It exposes `name` (a
convenience for usage/`--help` text) and the job-level half of the two-tier exit
(`exit`/`exit:`; `System halt:` is the node-level half). All methods are
class-side — `Program` has no instances.

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

%% `exit/1` shadows the auto-imported `erlang:exit/1` BIF; we never call the BIF
%% unqualified, and only ever call `erlang:halt/1` (also qualified).
-compile({no_auto_import, [exit/1]}).

-export([name/0, exit/0, 'exit:'/1, exit/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

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

-doc "End this program with status 0 (the job-level, safe exit). Does not return.".
-spec exit() -> no_return().
exit() ->
    'exit:'(0).

-doc """
End this program with status `Code` — the job-level, safe exit (ADR 0099 §3).

In a **node-owning** context (run-mode / escript — the program *is* the node)
this exits the node with `Code` via `erlang:halt/1` (which flushes the `io`
layer). In a **shared/connected** context it must end only *this session's* job
and leave the node up; that needs the `Session`-termination hook (BT-2688,
ADR 0099 §3 / Phase 5) which is not yet wired, so it refuses loudly rather than
halt a shared node.

**Process boundary.** The run-mode/escript path is an immediate `erlang:halt/1`,
so it is effective from *any* process — including a spawned/supervised Actor —
because the node is dedicated to this one program; there is no harness throw to
unwind, so the ADR 0099 §3 supervised-Actor-restart caveat does not apply here.
That caveat is about the *connected-mode* path (Phase 5), which ends the session
via a signal caught by the session evaluator and so is effective only from the
entry method's own synchronous call chain; a spawned Actor there must return a
status to the entry method and let *it* call `exit:`.
""".
-spec 'exit:'(integer()) -> no_return().
'exit:'(Code) when is_integer(Code), Code >= 0, Code =< 255 ->
    case node_owning() of
        true ->
            erlang:halt(Code);
        false ->
            raise_connected_exit_unsupported(Code)
    end;
'exit:'(Code) when is_integer(Code) ->
    %% Out of the POSIX range — a wrong *value*, not a wrong *type*. Validated
    %% here (not deferred to erlang:halt/1, which would silently truncate to the
    %% low byte) for consistency with System halt:.
    Error0 = beamtalk_error:new(invalid_argument, 'Program'),
    Error1 = beamtalk_error:with_selector(Error0, 'exit:'),
    Error2 = beamtalk_error:with_details(Error1, #{got => Code}),
    Error3 = beamtalk_error:with_hint(
        Error2, <<"Exit status must be in the POSIX range 0..255">>
    ),
    beamtalk_error:raise(Error3);
'exit:'(_Code) ->
    beamtalk_error:raise_type_error('Program', 'exit:', <<"Exit status must be an Integer">>).

-doc "FFI shim for `(Erlang beamtalk_program) exit:` (proxy strips the colon).".
-spec exit(integer()) -> no_return().
exit(Code) ->
    'exit:'(Code).

%%% ============================================================================
%%% Internal helpers
%%% ============================================================================

-doc "Whether this invocation owns the node (run-mode / escript boot sets the env).".
-spec node_owning() -> boolean().
node_owning() ->
    application:get_env(beamtalk_runtime, node_owning, false) =:= true.

-spec raise_connected_exit_unsupported(integer()) -> no_return().
raise_connected_exit_unsupported(Code) ->
    Error0 = beamtalk_error:new(unsupported, 'Program'),
    Error1 = beamtalk_error:with_selector(Error0, 'exit:'),
    Error2 = beamtalk_error:with_details(Error1, #{requested_code => Code}),
    Error3 = beamtalk_error:with_hint(
        Error2,
        <<
            "Program exit: in a shared/connected workspace needs the Session-termination "
            "hook (BT-2688), not yet available. Run the program with `beamtalk run` "
            "(run-mode) to use Program exit:, or halt the whole node with System halt:."
        >>
    ),
    beamtalk_error:raise(Error3).
