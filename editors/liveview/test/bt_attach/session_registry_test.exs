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

  # A persistent stand-in for a LiveView process: it runs registry calls on demand
  # so each call's `from` pid is THIS process. Lets a test drive the ownership race
  # (register/checkout/release) with distinct caller identities.
  defp spawn_caller do
    spawn(fn -> caller_loop() end)
  end

  defp caller_loop do
    receive do
      {:call, fun, from} ->
        send(from, {:called, fun.()})
        caller_loop()

      :stop ->
        :ok
    end
  end

  defp via(caller, fun) do
    send(caller, {:call, fun, self()})

    receive do
      {:called, result} -> result
    after
      1000 -> flunk("caller process did not respond")
    end
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

  test "stash_windows persists a desk that window_stash reads once on resume", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-stash", "phoenix-stash", pid)

    # Nothing stashed yet.
    assert SessionRegistry.window_stash(reg, "tab-stash") == nil

    stash = %{windows: [%{label: "x", x: 120, y: 96}], mode: "float"}
    assert :ok = SessionRegistry.stash_windows(reg, "tab-stash", stash)

    # Read once on resume...
    assert SessionRegistry.window_stash(reg, "tab-stash") == stash
    # ...then cleared, so a later re-render can't replay a stale desk.
    assert SessionRegistry.window_stash(reg, "tab-stash") == nil
  end

  test "stash_doc persists the doc-block state that doc_stash reads once on resume (BT-2570)",
       %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-doc", "phoenix-doc", pid)

    # Nothing stashed yet — a first connect has no prior expand state.
    assert SessionRegistry.doc_stash(reg, "tab-doc") == nil

    # terminate/2 stashes the expanded flag before the grace window opens.
    assert :ok = SessionRegistry.stash_doc(reg, "tab-doc", true)

    # The resuming mount reads it once and re-applies it...
    assert SessionRegistry.doc_stash(reg, "tab-doc") == true
    # ...then it's cleared, so a later re-render can't replay a stale value.
    assert SessionRegistry.doc_stash(reg, "tab-doc") == nil
  end

  test "stash_doc round-trips a collapsed state too (BT-2570)", %{reg: reg} do
    pid = fake_session()
    SessionRegistry.register(reg, "tab-doc2", "phoenix-doc2", pid)

    # A user who collapsed an expanded block should keep it collapsed on resume:
    # `false` is a real preference, distinct from "nothing stashed" (`nil`).
    assert :ok = SessionRegistry.stash_doc(reg, "tab-doc2", false)
    assert SessionRegistry.doc_stash(reg, "tab-doc2") == false
    assert SessionRegistry.doc_stash(reg, "tab-doc2") == nil
  end

  test "a doc stash for an unknown/non-binary token is a harmless no-op (BT-2570)", %{reg: reg} do
    assert :ok = SessionRegistry.stash_doc(reg, "never", true)
    assert SessionRegistry.doc_stash(reg, "never") == nil

    assert :ok = SessionRegistry.stash_doc(reg, 123, true)
    assert SessionRegistry.doc_stash(reg, nil) == nil
  end

  test "a stash for an unknown/non-binary token is a harmless no-op", %{reg: reg} do
    # Unknown token: nothing to attach the stash to, and nothing to read back.
    assert :ok = SessionRegistry.stash_windows(reg, "never", %{windows: []})
    assert SessionRegistry.window_stash(reg, "never") == nil

    # Non-binary tokens never participate (resume disabled), matching checkout/3.
    assert :ok = SessionRegistry.stash_windows(reg, 123, %{windows: []})
    assert SessionRegistry.window_stash(reg, nil) == nil
  end

  test "a stale release from a superseded LiveView can't reap a resumed session", %{reg: reg} do
    # The reconnect race (BT-2527 review): `release` (old LiveView's terminate) and
    # `checkout` (new LiveView's mount) hit the registry from different processes.
    # If checkout is processed first it resumes the session; a then-late release
    # must NOT arm a reap, or it would tear the live, resumed session down after
    # the grace window. The ownership guard makes the stale release a no-op.
    session = fake_session()
    old = spawn_caller()
    new = spawn_caller()

    # Old LiveView starts and owns the session.
    assert :ok =
             via(old, fn -> SessionRegistry.register(reg, "tab-race", "phoenix-race", session) end)

    # Fast reconnect: the NEW LiveView resumes (checkout) BEFORE the old one's
    # terminate releases — checkout wins the race and takes ownership.
    assert {:resumed, "phoenix-race", ^session} =
             via(new, fn -> SessionRegistry.checkout(reg, "tab-race") end)

    # Now the OLD LiveView's terminate fires its (now-stale) release.
    assert :ok = via(old, fn -> SessionRegistry.release(reg, "tab-race") end)

    # Past the grace window the resumed session is STILL alive: the stale release
    # was ignored because the old LiveView no longer owns the session.
    Process.sleep(@reap_ms * 3)

    assert {:resumed, "phoenix-race", ^session} =
             via(new, fn -> SessionRegistry.checkout(reg, "tab-race") end)

    send(old, :stop)
    send(new, :stop)
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
