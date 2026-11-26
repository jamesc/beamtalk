# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.SessionTeardownTest do
  @moduledoc """
  Pins the tab-close -> session-stopped path without a workspace (BT-5105).

  The `e2e` job's "closing a tab tears down its session" test checks the same
  guarantee against a real workspace, but it can only observe the workspace-wide
  session count, which every other tab also moves. These tests mock the one
  workspace call the registry makes (the `close` operation dispatched through
  `beamtalk_repl_ops:request/3` with the session as its SessionPid, BT-5199 and
  BT-5201, via `:erpc`) and assert exactly WHICH session it is asked to stop and when.

  `async: false`: `:meck` replaces `:erpc` for the whole VM (see
  `workspace_rpc_test.exs` for the same arrangement and why it is safe).
  """
  use ExUnit.Case, async: false

  alias BtAttach.SessionRegistry

  @reap_ms 50

  setup do
    test = self()
    :meck.new(:erpc, [:unstick, :passthrough])

    :meck.expect(:erpc, :call, fn
      _node, :beamtalk_repl_ops, :request, ["close", _params, pid], _timeout ->
        send(test, {:stop_session, pid})
        {:status, :ok}

      node, mod, fun, args, timeout ->
        :meck.passthrough([node, mod, fun, args, timeout])
    end)

    on_exit(fn -> :meck.unload() end)

    name = :"teardown_reg_#{System.unique_integer([:positive])}"
    start_supervised!({SessionRegistry, name: name, reap_after_ms: @reap_ms})
    {:ok, reg: name}
  end

  defp session, do: spawn(fn -> Process.sleep(:infinity) end)

  # A stand-in LiveView: runs registry calls on its own pid, and exits on :stop
  # (after the call returned) like a LiveView whose `terminate/2` ran.
  defp liveview do
    spawn(fn -> liveview_loop() end)
  end

  defp liveview_loop do
    receive do
      {:call, fun, from} ->
        send(from, {:called, fun.()})
        liveview_loop()

      :stop ->
        :ok
    end
  end

  defp via(lv, fun) do
    send(lv, {:call, fun, self()})

    receive do
      {:called, result} -> result
    after
      1_000 -> flunk("liveview stand-in did not respond")
    end
  end

  test "a released tab's session is stopped after the grace window, and only that one",
       %{reg: reg} do
    closed_session = session()
    open_session = session()
    closed_lv = liveview()
    open_lv = liveview()

    :ok =
      via(closed_lv, fn -> SessionRegistry.register(reg, "closed", "s-closed", closed_session) end)

    :ok = via(open_lv, fn -> SessionRegistry.register(reg, "open", "s-open", open_session) end)

    # terminate/2 of the closing tab: release arms the grace reap.
    :ok = via(closed_lv, fn -> SessionRegistry.release(reg, "closed") end)
    send(closed_lv, :stop)

    assert_receive {:stop_session, ^closed_session}, 1_000
    refute_receive {:stop_session, ^open_session}, 4 * @reap_ms
    assert {:resumed, "s-open", ^open_session} = SessionRegistry.checkout(reg, "open")
  end

  test "a graceful registry stop closes every live session before exit (BT-5129)" do
    name = :"graceful_reg_#{System.unique_integer([:positive])}"
    start_supervised!({SessionRegistry, name: name, reap_after_ms: 60_000}, id: :graceful)
    a = session()
    b = session()
    lv = liveview()
    :ok = via(lv, fn -> SessionRegistry.register(name, "a", "s-a", a) end)
    :ok = via(lv, fn -> SessionRegistry.register(name, "b", "s-b", b) end)

    :ok = stop_supervised(:graceful)

    assert_receive {:stop_session, ^a}, 1_000
    assert_receive {:stop_session, ^b}, 1_000
  end

  test "a tab that reconnects within the grace window keeps its session", %{reg: reg} do
    sess = session()
    old_lv = liveview()
    new_lv = liveview()

    :ok = via(old_lv, fn -> SessionRegistry.register(reg, "tab", "s-tab", sess) end)
    :ok = via(old_lv, fn -> SessionRegistry.release(reg, "tab") end)

    assert {:resumed, "s-tab", ^sess} =
             via(new_lv, fn -> SessionRegistry.checkout(reg, "tab") end)

    refute_receive {:stop_session, _}, 4 * @reap_ms
  end

  test "a tab killed without terminate/2 still has its session stopped", %{reg: reg} do
    sess = session()
    lv = liveview()
    :ok = via(lv, fn -> SessionRegistry.register(reg, "killed", "s-killed", sess) end)

    Process.exit(lv, :kill)

    assert_receive {:stop_session, ^sess}, 1_000
  end
end
