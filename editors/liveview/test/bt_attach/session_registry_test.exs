# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.SessionRegistryTest do
  @moduledoc """
  Pure unit tests for the per-tab workspace-session registry (BT-2410 Wave 4).

  These run without a real workspace: the registry's only workspace coupling is
  `BtAttach.Workspace.close_session/1`, which RPCs to the (absent) workspace node
  and returns `{:badrpc, :nodedown}` — handled gracefully. So we drive the full
  resume / grace-reap / monitor lifecycle against ordinary local pids and assert
  the registry's bookkeeping (checkout/register/release/discard/down) directly.
  """
  use ExUnit.Case, async: true

  alias BtAttach.SessionRegistry

  # A fast reap window so the grace-period assertions don't slow the suite.
  @reap_ms 50

  setup do
    # A named-per-test registry so async tests don't share state.
    name = :"reg_#{System.unique_integer([:positive])}"
    start_supervised!({SessionRegistry, name: name, reap_after_ms: @reap_ms})
    {:ok, reg: name}
  end

  # A stand-in for a remote workspace session pid: just a live local process we
  # can register, monitor, and kill.
  defp fake_session do
    spawn(fn ->
      receive do
        :stop -> :ok
      end
    end)
  end

  test "checkout misses for an unknown or nil/non-binary token", %{reg: reg} do
    assert SessionRegistry.checkout(reg, "never-registered") == :miss
    assert SessionRegistry.checkout(reg, nil) == :miss
    assert SessionRegistry.checkout(reg, 123) == :miss
  end

  test "register then checkout resumes the same session for a token", %{reg: reg} do
    pid = fake_session()
    assert :ok = SessionRegistry.register(reg, "tab-1", "phoenix-1", pid)

    assert {:resumed, "phoenix-1", ^pid} = SessionRegistry.checkout(reg, "tab-1")
    # Resume is repeatable while the entry is live.
    assert {:resumed, "phoenix-1", ^pid} = SessionRegistry.checkout(reg, "tab-1")
  end

  test "two tabs map to two isolated entries (multi-tab isolation)", %{reg: reg} do
    pid1 = fake_session()
    pid2 = fake_session()
    SessionRegistry.register(reg, "tab-a", "phoenix-a", pid1)
    SessionRegistry.register(reg, "tab-b", "phoenix-b", pid2)

    assert {:resumed, "phoenix-a", ^pid1} = SessionRegistry.checkout(reg, "tab-a")
    assert {:resumed, "phoenix-b", ^pid2} = SessionRegistry.checkout(reg, "tab-b")
    refute pid1 == pid2
  end

  test "release schedules a reap that drops the entry after the grace window", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-2", "phoenix-2", pid)

    SessionRegistry.release(reg, "tab-2")
    # Still resumable immediately after release (grace window open)...
    assert {:resumed, "phoenix-2", ^pid} = SessionRegistry.checkout(reg, "tab-2")

    # ...but a release with NO intervening checkout reaps after the window.
    SessionRegistry.release(reg, "tab-2")
    Process.sleep(@reap_ms * 3)
    assert SessionRegistry.checkout(reg, "tab-2") == :miss
  end

  test "a checkout within the grace window cancels the reap (resume)", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-3", "phoenix-3", pid)

    SessionRegistry.release(reg, "tab-3")
    # Reconnect promptly — cancels the pending reap.
    assert {:resumed, "phoenix-3", ^pid} = SessionRegistry.checkout(reg, "tab-3")

    # Past the original window, the entry is still live (reap was cancelled).
    Process.sleep(@reap_ms * 3)
    assert {:resumed, "phoenix-3", ^pid} = SessionRegistry.checkout(reg, "tab-3")
  end

  test "discard forgets the entry immediately (no grace window)", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-4", "phoenix-4", pid)

    assert :ok = SessionRegistry.discard(reg, "tab-4")
    assert SessionRegistry.checkout(reg, "tab-4") == :miss
  end

  test "a dead remote session pid drops its entry via the monitor", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-5", "phoenix-5", pid)
    assert {:resumed, "phoenix-5", ^pid} = SessionRegistry.checkout(reg, "tab-5")

    # Kill the (stand-in) workspace session: the monitor should reap the entry.
    ref = Process.monitor(pid)
    send(pid, :stop)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}, 1_000

    # Give the registry a beat to process its own DOWN, then the entry is gone.
    eventually(fn -> SessionRegistry.checkout(reg, "tab-5") == :miss end)
  end

  test "re-registering a token closes/replaces the prior session", %{reg: reg} do
    pid1 = fake_session()
    pid2 = fake_session()
    SessionRegistry.register(reg, "tab-6", "phoenix-6a", pid1)
    SessionRegistry.register(reg, "tab-6", "phoenix-6b", pid2)

    # The latest registration wins; the token never holds two sessions.
    assert {:resumed, "phoenix-6b", ^pid2} = SessionRegistry.checkout(reg, "tab-6")
  end

  defp eventually(fun, retries \\ 50) do
    cond do
      fun.() ->
        :ok

      retries == 0 ->
        flunk("condition never became true")

      true ->
        Process.sleep(10)
        eventually(fun, retries - 1)
    end
  end
end
