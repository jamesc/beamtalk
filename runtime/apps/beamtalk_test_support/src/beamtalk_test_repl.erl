%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_repl).

%%% **DDD Context:** REPL Context (test support)

-moduledoc """
Shared EUnit helpers for REPL/workspace suites (BT-4677): protocol message
construction and cross-reference table reset. Replaces the per-file
`make_msg/3` and `clear_xref/0` copies.
""".

-export([make_msg/3, clear_xref/0]).

-doc "Build a REPL `protocol_msg` with no params.".
-spec make_msg(binary(), binary(), binary() | undefined) -> tuple().
make_msg(Op, Id, Session) ->
    {protocol_msg, Op, Id, Session, #{}}.

-doc """
Empty the `beamtalk_xref` tables (methods, senders, references, class
generations) in place, if the server is running; a no-op otherwise. Best
effort: a server that dies mid-reset is ignored.
""".
-spec clear_xref() -> ok.
clear_xref() ->
    case whereis(beamtalk_xref) of
        undefined ->
            ok;
        _Pid ->
            try
                sys:replace_state(beamtalk_xref, fun(S) ->
                    ets:delete_all_objects(beamtalk_xref_methods),
                    ets:delete_all_objects(beamtalk_xref_senders),
                    ets:delete_all_objects(beamtalk_xref_references),
                    ets:delete_all_objects(xref_class_gen),
                    S
                end)
            catch
                _:_ -> ok
            end,
            ok
    end.
