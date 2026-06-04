# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Workspace do
  @moduledoc """
  Attach-topology client for a running Beamtalk workspace BEAM node.

  This is the entire surface the Phoenix side needs to talk to the workspace:
  it joins the workspace's Erlang distribution mesh with the shared cookie,
  then drives the workspace via `:rpc` and native cross-node message passing.

  Topology (BT-2394): **Attach** — Phoenix runs as its own BEAM node and
  connects to the workspace node over Erlang distribution, the same way
  Livebook attaches to a remote node. No new wire protocol: we call the exact
  same Erlang functions the WebSocket handler (`beamtalk_ws_handler`) calls.

  Configuration comes from the environment so this stays decoupled from the
  workspace's own config:

    * `BT_WORKSPACE_NODE`   — e.g. `beamtalk_workspace_spike@localhost`
    * `BT_WORKSPACE_COOKIE` — contents of `~/.beamtalk/workspaces/<id>/vm.args`
                              (`-setcookie <token>`), token only
  """

  require Logger

  @doc "The configured workspace node name as an atom."
  def node_name do
    System.get_env("BT_WORKSPACE_NODE", "beamtalk_workspace_spike@localhost")
    |> String.to_atom()
  end

  @doc """
  Ensure this Phoenix node is distributed, shares the workspace cookie, and is
  connected to the workspace node. Idempotent — safe to call on every mount.
  """
  def connect do
    ensure_distributed()
    set_cookie()

    case Node.connect(node_name()) do
      true -> :ok
      reason -> {:error, {:connect_failed, node_name(), reason}}
    end
  end

  @doc """
  Create a fresh, workspace-supervised REPL session. Each LiveView mount gets
  its own session, so two browser tabs have isolated bindings (proven in the
  BT-2394 spike: tab1 `x = 100`, tab2 `x = 999`).

  Returns the remote session pid.
  """
  def start_session(session_id) do
    case rpc(:beamtalk_session_sup, :start_session, [session_id]) do
      {:ok, pid} when is_pid(pid) -> pid
      other -> other
    end
  end

  @doc """
  Evaluate a Beamtalk expression in `session_pid`. Returns the workspace's own
  formatted result map (decoded from `beamtalk_repl_json:format_response/1`,
  the same formatter the browser protocol uses), so display is surface-consistent.
  """
  def eval(session_pid, expression) when is_binary(expression) do
    case rpc(:beamtalk_repl_shell, :eval, [session_pid, String.to_charlist(expression)]) do
      {:ok, value, output, warnings} ->
        {:ok, format(value), to_string(output), Enum.map(warnings, &to_string/1)}

      {:error, reason, output, warnings} ->
        {:error, format_error(reason), to_string(output), Enum.map(warnings, &to_string/1)}

      {:badrpc, reason} ->
        {:error, "workspace unreachable: #{inspect(reason)}", "", []}
    end
  end

  @doc """
  Subscribe the *calling* process (the LiveView process) to the workspace's
  Transcript push stream. The LiveView then receives `{:transcript_output, text}`
  messages directly over distribution — no polling, no extra protocol.
  """
  def subscribe_transcript do
    :gen_server.cast({:Transcript, node_name()}, {:subscribe, self()})
  end

  # ── internals ─────────────────────────────────────────────────────────────

  defp ensure_distributed do
    unless Node.alive?() do
      # A unique short name so multiple dev runs don't collide on epmd.
      name = :"bt_attach_#{System.unique_integer([:positive])}@localhost"
      {:ok, _} = :net_kernel.start([name, :shortnames])
    end
  end

  defp set_cookie do
    case System.get_env("BT_WORKSPACE_COOKIE") do
      nil -> Logger.warning("BT_WORKSPACE_COOKIE not set; relying on ~/.erlang.cookie")
      "" -> :ok
      token -> Node.set_cookie(node_name(), String.to_atom(token))
    end
  end

  defp rpc(mod, fun, args) do
    :rpc.call(node_name(), mod, fun, args)
  end

  # Reuse the workspace's own JSON formatter so the LiveView shows exactly what
  # the WebSocket REPL would show for the same value.
  defp format(value) do
    case rpc(:beamtalk_repl_json, :format_response, [value]) do
      json when is_binary(json) ->
        case Jason.decode(json) do
          {:ok, %{"value" => v}} -> stringify(v)
          _ -> inspect(value)
        end

      _ ->
        inspect(value)
    end
  end

  defp format_error(reason) do
    case rpc(:beamtalk_repl_json, :format_error, [reason]) do
      json when is_binary(json) ->
        case Jason.decode(json) do
          {:ok, %{"message" => m}} -> m
          _ -> inspect(reason)
        end

      _ ->
        inspect(reason)
    end
  end

  defp stringify(v) when is_binary(v), do: v
  defp stringify(v), do: inspect(v)
end
