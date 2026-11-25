# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RevokedSessionsTest do
  use ExUnit.Case, async: false

  alias BtAttach.RevokedSessions

  test "a revoked id is reported revoked; others and non-binaries are not" do
    id = "bt_session:#{System.unique_integer([:positive])}"
    refute RevokedSessions.revoked?(id)
    RevokedSessions.revoke(id, 60)
    assert RevokedSessions.revoked?(id)
    refute RevokedSessions.revoked?("bt_session:other")
    refute RevokedSessions.revoked?(nil)
    assert RevokedSessions.revoke(nil, 60) == :ok
  end

  test "fails closed: with the table absent every session id reads as revoked" do
    :ok = Supervisor.terminate_child(BtAttach.Supervisor, RevokedSessions)
    on_exit(fn -> Supervisor.restart_child(BtAttach.Supervisor, RevokedSessions) end)

    assert :ets.whereis(RevokedSessions) == :undefined
    assert RevokedSessions.revoked?("bt_session:never-seen")
    # Non-binary ids stay "not revoked": there is no session to refuse.
    refute RevokedSessions.revoked?(nil)
  end

  test "an entry past its TTL is no longer revoked and is swept" do
    id = "bt_session:expired-#{System.unique_integer([:positive])}"
    :ets.insert(RevokedSessions, {id, System.system_time(:second) - 1})
    refute RevokedSessions.revoked?(id)
    RevokedSessions.sweep()
    assert :ets.lookup(RevokedSessions, id) == []
  end
end
