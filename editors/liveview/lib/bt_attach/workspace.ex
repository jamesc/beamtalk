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
  Evaluate a Beamtalk expression in `session_pid`. Returns the **live Erlang
  term** the workspace produced — no JSON. Over distribution that means
  `Counter spawn` hands back a real, messageable remote pid wrapped in a
  `{:beamtalk_object, class, module, pid}` tuple, not a flattened display string.

  The term contract is the only coupling between Phoenix and the workspace:
    * success → `{:ok, term, output, warnings}`
    * error   → `{:error, reason_term, output, warnings}` (e.g. a `:beamtalk_error` tuple)
  """
  def eval(session_pid, expression) when is_binary(expression) do
    case rpc(:beamtalk_repl_shell, :eval, [session_pid, String.to_charlist(expression)]) do
      {:ok, value, output, warnings} ->
        {:ok, value, to_string(output), Enum.map(warnings, &to_string/1)}

      {:error, reason, output, warnings} ->
        {:error, reason, to_string(output), Enum.map(warnings, &to_string/1)}

      {:badrpc, reason} ->
        {:error, {:unreachable, reason}, "", []}

      # The workspace contract is {:ok,…}/{:error,…}; treat anything else as a
      # data error rather than letting a CaseClauseError crash the LiveView.
      other ->
        {:error, {:unexpected_reply, other}, "", []}
    end
  end

  @doc """
  Render a result *term* as a string for the page. Display is a separate
  concern from the data path: `eval/2` returns the live term, and this only
  stringifies it. A spawned actor renders as a live remote pid so it's obvious
  the object lives on the workspace node, not here.
  """
  def render(value) do
    case value do
      bin when is_binary(bin) ->
        bin

      {:beamtalk_object, class, _module, pid} when is_pid(pid) ->
        "##{class}⟨#{inspect(pid)} on #{node(pid)}⟩"

      # Unwrap a synchronous actor reply (`c value` -> {:ok, 3}).
      {:ok, inner} ->
        render(inner)

      other ->
        inspect(other)
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

      # Two connected mounts can both pass Node.alive?/0 and race here; the
      # loser sees {:error, {:already_started, _}}, which is fine.
      case :net_kernel.start([name, :shortnames]) do
        {:ok, _pid} -> :ok
        {:error, {:already_started, _pid}} -> :ok
        {:error, reason} -> raise "failed to start distributed node: #{inspect(reason)}"
      end
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
end
