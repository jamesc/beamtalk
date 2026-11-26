# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule :beamtalk_repl_ops do
  @moduledoc """
  Test-only stand-in for the workspace's `beamtalk_repl_ops` session API
  (BT-5202), loaded only in this app's test build.

  The resume lifecycle tests point `BT_WORKSPACE_NODE` at the test node itself,
  so `BtAttach.Workspace.session_alive?/1` can probe the fake session pids
  `BtAttachWeb.SessionProbe` hands out. Before BT-5202 that probe was
  `:erlang.is_process_alive/1`, which every node has; now it is
  `beamtalk_repl_ops:session_alive/1`, which only a workspace node has. This
  module answers it the same way for a local pid. The real function (a check of
  the session supervisor's children) is covered in the runtime tests
  (`beamtalk_repl_ops_session_api_tests`).
  """

  def session_alive(pid) when is_pid(pid), do: Process.alive?(pid)
end

defmodule :beamtalk_repl_subscriptions do
  @moduledoc """
  Test-only stand-in for the workspace's subscription facade (BT-5206), loaded
  only in this app's test build.

  A LiveView releases its push-stream subscriptions on every exit path through
  `beamtalk_repl_subscriptions:unsubscribe_all/1`. With `BT_WORKSPACE_NODE` at the
  test node itself, that call lands here. A test that wants to see it registers
  its own process as `:bt_subscriptions_probe` and gets
  `{:unsubscribe_all, pid}`; with no probe registered the call is a no-op, as it
  would be against a workspace with no such subscriber. The real function is
  covered in the runtime tests (`beamtalk_repl_subscriptions_tests`).
  """

  def unsubscribe_all(pid) when is_pid(pid) do
    if probe = Process.whereis(:bt_subscriptions_probe), do: send(probe, {:unsubscribe_all, pid})
    :ok
  end
end
