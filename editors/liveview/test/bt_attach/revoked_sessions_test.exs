# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RevokedSessionsTest do
  use ExUnit.Case, async: false

  alias BtAttach.RevokedSessions

  test "a revoked id is reported revoked; others are not" do
    id = "bt_session:#{System.unique_integer([:positive])}"
    refute RevokedSessions.revoked?(id)
    assert RevokedSessions.revoke(id, 60) == :ok
    assert RevokedSessions.revoked?(id)
    refute RevokedSessions.revoked?("bt_session:other")
  end

  # BT-5185: every login mints an id, so a session without one cannot be
  # checked against the list (a cookie from before revocation existed, say)
  # and must not pass as "not revoked".
  test "a session without an id reads as revoked" do
    assert RevokedSessions.revoked?(nil)
    assert RevokedSessions.revoked?(42)
    assert RevokedSessions.revoke(nil, 60) == :ok
  end

  test "fails closed: with the table absent every session id reads as revoked" do
    :ok = Supervisor.terminate_child(BtAttach.Supervisor, RevokedSessions)
    on_exit(fn -> Supervisor.restart_child(BtAttach.Supervisor, RevokedSessions) end)

    assert :ets.whereis(RevokedSessions) == :undefined
    assert RevokedSessions.revoked?("bt_session:never-seen")
    assert RevokedSessions.revoked?(nil)
  end

  # BT-5185: a revocation that cannot be recorded is reported, not dropped.
  test "revoke reports an error while the table is down" do
    :ok = Supervisor.terminate_child(BtAttach.Supervisor, RevokedSessions)
    on_exit(fn -> Supervisor.restart_child(BtAttach.Supervisor, RevokedSessions) end)

    assert RevokedSessions.revoke("bt_session:lost", 60) == {:error, :unavailable}
  end

  test "an entry past its TTL is no longer revoked and is swept" do
    id = "bt_session:expired-#{System.unique_integer([:positive])}"
    :ets.insert(RevokedSessions, {id, System.system_time(:second) - 1})
    refute RevokedSessions.revoked?(id)
    RevokedSessions.sweep()
    assert :ets.lookup(RevokedSessions, id) == []
  end
end
