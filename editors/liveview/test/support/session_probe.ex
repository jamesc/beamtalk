# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.SessionProbe do
  @moduledoc """
  A per-test log of the workspace sessions a LiveView mount asked for (BT-3306).

  `BtAttachWeb.StubWorkspaceClient.start_session/2` answers with an
  *immediately-exiting* process, which is fine for the untokened mounts most
  tests drive but not for the resume lifecycle: a resumable (non-nil) token
  registers a monitor on the session pid, so a dead pid is dropped from
  `BtAttach.SessionRegistry` before a reconnect could ever reach the
  `Workspace.session_alive?/1` probe. This probe answers with a pid that stays
  alive for the whole test (it exits with the log Agent) and records what was
  asked for, so a test can assert *whether* a second session was started
  (resume vs. fresh) and what metadata a fresh one carried — and can read back
  the session id the LiveView is bound to, which the `BindingChanged` push
  stream is filtered on.

  Start it in `setup` with `start_link/0` and point the app env at
  `BtAttachWeb.SessionProbe.Client` (or at a test-local client that delegates
  its `start_session/2` to `record/2`).
  """

  @log __MODULE__.Log

  @doc "Start the per-test session log. Call in `setup`; it dies with the test."
  def start_link, do: Agent.start_link(fn -> [] end, name: @log)

  @doc """
  Record one `start_session/2` request and answer with a fake session pid that
  stays alive until the log Agent (and so the test) goes away.
  """
  def record(session_id, meta) do
    pid = spawn_session()
    Agent.update(@log, &(&1 ++ [%{session_id: session_id, meta: meta, pid: pid}]))
    pid
  end

  @doc "Every recorded session, oldest first."
  def sessions, do: Agent.get(@log, & &1)

  @doc "The session id of the first (usually only) recorded session, or nil."
  def session_id do
    case sessions() do
      [%{session_id: id} | _] -> id
      [] -> nil
    end
  end

  defp spawn_session do
    owner = Process.whereis(@log)

    spawn(fn ->
      ref = Process.monitor(owner)

      receive do
        {:DOWN, ^ref, :process, _pid, _reason} -> :ok
      after
        60_000 -> :ok
      end
    end)
  end

  defmodule Client do
    @moduledoc """
    The shared stub workspace client with `start_session/2` routed through
    `BtAttachWeb.SessionProbe` — the default client for tests that need a
    long-lived, observable session.
    """
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: BtAttachWeb.SessionProbe.record(session_id, meta)
  end
end
