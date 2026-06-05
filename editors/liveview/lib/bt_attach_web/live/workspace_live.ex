# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLive do
  @moduledoc """
  Wave-1 LiveView IDE (BT-2407): the two-pane workspace core on the Attach
  topology (ADR 0017 Phase 3, validated by the BT-2394 spike).

    * Workspace pane   — input evaluates Beamtalk source against the attached
      workspace node via the BT-2399 term-returning op layer, rendering the
      result term (surface-consistent with the Phase-1 browser workspace).
    * Transcript pane  — subscribes via the BT-2399 subscription facade and
      renders live `Transcript show:` output pushed over distribution.
    * Session lifecycle — each LiveView mount gets its own workspace-supervised
      session; eval state (bindings, loaded classes) persists across evals
      within the session and is released when the LiveView process terminates.

  All workspace interaction goes through `BtAttach.Workspace`, which talks to
  the workspace node via Erlang distribution + `:rpc`. The data path carries
  live Erlang terms; JSON lives only at the browser WebSocket edge.
  """
  use BtAttachWeb, :live_view

  require Logger

  alias BtAttach.Workspace

  @impl true
  def mount(_params, _session, socket) do
    # Only attach over distribution on the *connected* (WebSocket) mount — the
    # initial disconnected HTTP render would otherwise create a second, orphaned
    # workspace session on every page load.
    if connected?(socket) do
      attach(socket)
    else
      {:ok, assign(socket, connected: false, error: nil)}
    end
  end

  defp attach(socket) do
    socket =
      case Workspace.connect() do
        :ok ->
          session_id = "phoenix-#{System.unique_integer([:positive])}"

          case Workspace.start_session(session_id) do
            pid when is_pid(pid) ->
              # Subscribe THIS LiveView pid (location-transparent over dist) to
              # the Transcript stream through the BT-2399 facade — no direct
              # gen_server cast. The facade's cast returns `:ok`; a `{:badrpc, _}`
              # (or any non-ok reply) means the transcript is NOT live, so we must
              # not render the pane as connected and claim a working stream.
              case Workspace.subscribe_transcript(self()) do
                :ok ->
                  socket
                  |> assign(:connected, true)
                  |> assign(:node, Workspace.node_name())
                  |> assign(:session_id, session_id)
                  |> assign(:session_pid, pid)
                  |> assign(:result, nil)
                  |> assign(:output, nil)
                  |> assign(:error, nil)
                  |> assign(:expr, "3 + 4")
                  |> stream(:transcript, [])

                other ->
                  # Transcript subscription failed: tear the half-started session
                  # back down so we don't leak it, then render a non-connected
                  # error page rather than a dead transcript pane.
                  Logger.error("transcript subscribe failed: #{inspect(other)}")
                  Workspace.close_session(pid)

                  assign(socket,
                    connected: false,
                    error: "transcript subscribe failed: #{inspect(other)}"
                  )
              end

            {:error, reason} ->
              assign(socket,
                connected: false,
                error: "session start failed: #{inspect(reason)}"
              )
          end

        {:error, reason} ->
          assign(socket, connected: false, error: "attach failed: #{inspect(reason)}")
      end

    {:ok, socket}
  end

  @impl true
  def handle_event("eval", %{"expr" => expr}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    case Workspace.eval(pid, expr) do
      {:ok, term, output, _warnings} ->
        # eval returns the live term; rendering is display-only and reuses the
        # workspace's own formatter for surface-consistency with the browser.
        {:noreply,
         assign(socket,
           result: Workspace.render_term(term),
           output: present(output),
           error: nil,
           expr: expr
         )}

      {:error, reason, output, _warnings} ->
        {:noreply,
         assign(socket,
           result: nil,
           output: present(output),
           error: Workspace.render_error(reason),
           expr: expr
         )}
    end
  end

  # No session (attach failed) — don't crash on a missing assign.
  def handle_event("eval", %{"expr" => expr}, socket) do
    {:noreply,
     assign(socket, result: nil, output: nil, error: "not attached to workspace", expr: expr)}
  end

  # Transcript push, delivered directly over distribution to this LiveView pid.
  @impl true
  def handle_info({:transcript_output, text}, socket) do
    line = %{id: System.unique_integer([:positive]), text: to_string(text)}
    {:noreply, stream_insert(socket, :transcript, line)}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  @impl true
  def terminate(_reason, socket) do
    # Best-effort: drop our Transcript subscription so the workspace doesn't keep
    # pushing to a dead pid. The event server also auto-removes dead subscribers
    # via its monitor, so this is belt-and-braces.
    #
    # Crucially also CLOSE the workspace-supervised session: it is owned by the
    # workspace's `beamtalk_session_sup`, not by this LiveView process, so it does
    # NOT go away when we exit. Without this we leak one orphaned session per
    # mount/reconnect (the session-lifecycle acceptance criterion).
    if socket.assigns[:connected] do
      Workspace.unsubscribe_transcript(self())

      case socket.assigns[:session_pid] do
        pid when is_pid(pid) -> Workspace.close_session(pid)
        _ -> :ok
      end
    end

    :ok
  end

  # Blank captured output is not worth rendering a pane for.
  defp present(""), do: nil
  defp present(nil), do: nil
  defp present(output) when is_binary(output), do: output

  @impl true
  def render(assigns) do
    ~H"""
    <div style="font-family: ui-monospace, monospace; max-width: 760px; margin: 2rem auto;">
      <h1>Beamtalk Workspace</h1>

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

        <%= if @output do %>
          <pre style="background:#f7f7f7; padding:.75rem; border:1px solid #ddd;"><%= @output %></pre>
        <% end %>
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
        <%= if @error do %>
          <pre style="background:#fff0f0; padding:1rem;">Not attached.

    <%= @error %>

    Start a workspace and export its node + cookie:
      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' ~/.beamtalk/workspaces/spike/vm.args)
    then restart this server.</pre>
        <% else %>
          <p>Connecting to workspace…</p>
        <% end %>
      <% end %>
    </div>
    """
  end
end
