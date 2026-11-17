%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_repl).

%%% **DDD Context:** REPL Context (test support)

-moduledoc """
Shared EUnit helpers for REPL/workspace suites (BT-4677): protocol message
construction and cross-reference table reset. Replaces the per-file
`make_msg/3` and `clear_xref/0` copies.
""".

-export([
    make_msg/3, clear_xref/0, ensure_xref/0, release_xref/0, assert_error_response/1, fake_self/2
]).

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

-doc """
Make sure a `beamtalk_xref` server is running (starting an unsupervised one if
the supervised one is absent) and empty its tables. The `setup` half of the
xref-backed suites; pair with `clear_xref/0` in cleanup.
""".
-spec ensure_xref() -> ok.
ensure_xref() ->
    case whereis(beamtalk_xref) of
        undefined -> {ok, _} = beamtalk_xref:start_link();
        _ -> ok
    end,
    clear_xref().

-doc """
The `cleanup` half of `ensure_xref/0`: stop the `beamtalk_xref` server if
`ensure_xref/0` started it bare (a supervised one is left alone).
""".
-spec release_xref() -> ok.
release_xref() ->
    beamtalk_test_proc:stop_if_bare(beamtalk_xref).

-doc """
Decode a JSON-encoded REPL response, assert it is a `done`/`error` response with
a binary `error` field, and return the decoded map.
""".
-spec assert_error_response(iodata()) -> map().
assert_error_response(Response) ->
    Decoded = json:decode(iolist_to_binary(Response)),
    #{<<"status">> := [<<"done">>, <<"error">>], <<"error">> := Error} = Decoded,
    true = is_binary(Error),
    Decoded.

-doc """
A `beamtalk_object` tuple standing in for the `self` of a stdlib singleton
(`workspace` or `beamtalk`) whose process is `Pid`.
""".
-spec fake_self(workspace | beamtalk, pid()) -> tuple().
fake_self(workspace, Pid) ->
    {beamtalk_object, 'Workspace', 'bt@stdlib@workspace', Pid};
fake_self(beamtalk, Pid) ->
    {beamtalk_object, 'Beamtalk class', 'bt@stdlib@beamtalk', Pid}.
