# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLive do
  @moduledoc """
  Minimal LiveView exercising the BT-2394 Attach-topology critical path:

    * eval round-trip   — input field -> workspace `evaluate` -> rendered result
    * Transcript stream — live `Transcript show:` output pushed over distribution
    * isolation         — each mount (tab) gets its own workspace session

  All workspace interaction goes through `BtAttach.Workspace`, which talks to
  the workspace node via Erlang distribution + `:rpc`.
  """
  use BtAttachWeb, :live_view

  alias BtAttach.Workspace

  @impl true
  def mount(_params, _session, socket) do
    socket =
      case Workspace.connect() do
        :ok ->
          session_id = "phoenix-#{System.unique_integer([:positive])}"

          case Workspace.start_session(session_id) do
            pid when is_pid(pid) ->
              if connected?(socket), do: Workspace.subscribe_transcript()

              socket
              |> assign(:connected, true)
              |> assign(:node, Workspace.node_name())
              |> assign(:session_id, session_id)
              |> assign(:session_pid, pid)
              |> assign(:result, nil)
              |> assign(:error, nil)
              |> assign(:expr, "3 + 4")
              |> stream(:transcript, [])

            other ->
              assign(socket, connected: false, error: "session start failed: #{inspect(other)}")
          end

        {:error, reason} ->
          assign(socket, connected: false, error: "attach failed: #{inspect(reason)}")
      end

    {:ok, socket}
  end

  @impl true
  def handle_event("eval", %{"expr" => expr}, socket) do
    case Workspace.eval(socket.assigns.session_pid, expr) do
      {:ok, value, _output, _warnings} ->
        {:noreply, assign(socket, result: value, error: nil, expr: expr)}

      {:error, message, _output, _warnings} ->
        {:noreply, assign(socket, result: nil, error: message, expr: expr)}
    end
  end

  # Transcript push, delivered directly over distribution to this LiveView pid.
  @impl true
  def handle_info({:transcript_output, text}, socket) do
    line = %{id: System.unique_integer([:positive]), text: to_string(text)}
    {:noreply, stream_insert(socket, :transcript, line)}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  @impl true
  def render(assigns) do
    ~H"""
    <div style="font-family: ui-monospace, monospace; max-width: 760px; margin: 2rem auto;">
      <h1>Beamtalk Workspace — Attach spike (BT-2394)</h1>

      <%= if @connected do %>
        <p>Attached to <strong>{@node}</strong> · session <code>{@session_id}</code></p>

        <form phx-submit="eval" style="display:flex; gap:.5rem; margin:1rem 0;">
          <input
            name="expr"
            value={@expr}
            autocomplete="off"
            style="flex:1; padding:.5rem; font-family:inherit;"
          />
          <button type="submit" style="padding:.5rem 1rem;">Eval</button>
        </form>

        <%= if @result do %>
          <pre style="background:#f0fff0; padding:.75rem; border:1px solid #cec;"><%= @result %></pre>
        <% end %>
        <%= if @error do %>
          <pre style="background:#fff0f0; padding:.75rem; border:1px solid #ecc;"><%= @error %></pre>
        <% end %>

        <h2>Transcript (live)</h2>
        <div
          id="transcript"
          phx-update="stream"
          style="background:#111; color:#0f0; padding:.75rem; min-height:6rem;"
        >
          <div :for={{dom_id, line} <- @streams.transcript} id={dom_id}>{line.text}</div>
        </div>
        <p style="color:#666;">Try <code>Transcript show: "hello"</code> to see a live push.</p>
      <% else %>
        <pre style="background:#fff0f0; padding:1rem;">Not attached.

    <%= @error %>

    Start a workspace and export its node + cookie:
      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' ~/.beamtalk/workspaces/spike/vm.args)
    then restart this server.</pre>
      <% end %>
    </div>
    """
  end
end
