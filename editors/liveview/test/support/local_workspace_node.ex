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
