# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLive do
  @moduledoc """
  LiveView IDE (BT-2407 Wave 1, BT-2408 Wave 2): the four-pane workspace core on
  the Attach topology (ADR 0017 Phase 3, validated by the BT-2394 spike).

    * Workspace pane   — input evaluates Beamtalk source against the attached
      workspace node via the BT-2399 term-returning op layer, rendering the
      result term (surface-consistent with the Phase-1 browser workspace).
    * Transcript pane  — subscribes via the BT-2399 subscription facade and
      renders live `Transcript show:` output pushed over distribution.
    * Bindings pane (Wave 2) — lists the session's current bindings via the
      read-surface (`Workspace.list_bindings/1`, ADR 0085) and refreshes live on
      the BT-2399 `bindings` push stream (a `{:bindings_changed, _}` signal),
      never by polling or a direct gen_server cast.
    * Inspector pane (Wave 2) — inspects a selected binding/value through the
      read-surface `inspect` op (ADR 0085), rendering the **live term's**
      structured fields. Object-valued slots are themselves live
      `{:beamtalk_object, …}` handles, so the Inspector can *follow references* —
      drill from a binding into a referenced object and into its object fields,
      the whole point of carrying terms (not JSON) to the LiveView.
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
              # the Transcript AND bindings streams through the BT-2399 facade —
              # no direct gen_server cast. The facade's cast returns `:ok`; any
              # non-ok reply (`{:badrpc, _}`) means a stream is NOT live, so we
              # must not render the pane as connected and claim a working stream.
              # Pattern-match both subscribe results (Wave-1 review rule) and tear
              # the half-started session back down on any failure so it can't leak.
              with :ok <- Workspace.subscribe_transcript(self()),
                   :ok <- Workspace.subscribe_bindings(self()) do
                socket
                |> assign(:connected, true)
                |> assign(:node, Workspace.node_name())
                |> assign(:session_id, session_id)
                |> assign(:session_pid, pid)
                |> assign(:result, nil)
                |> assign(:output, nil)
                |> assign(:error, nil)
                |> assign(:expr, "3 + 4")
                |> assign(:inspect_target, nil)
                |> assign(:inspect_rows, [])
                |> assign(:inspect_error, nil)
                |> assign_bindings(pid)
                |> stream(:transcript, [])
              else
                other ->
                  Logger.error("subscribe failed: #{inspect(other)}")
                  # Drop whichever subscription may have succeeded before closing
                  # the session, so we don't leave a dangling subscriber pushing
                  # to a pid we're about to abandon.
                  Workspace.unsubscribe_transcript(self())
                  Workspace.unsubscribe_bindings(self())
                  Workspace.close_session(pid)

                  assign(socket,
                    connected: false,
                    error: "subscribe failed: #{inspect(other)}"
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

  # Inspect a binding by name: look up its live term and drill into it via the
  # read-surface `inspect` op. Reference-following starts here.
  @impl true
  def handle_event("inspect", %{"name" => name}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    {:noreply, inspect_binding(socket, pid, name)}
  end

  # Drill into an object-valued field of the currently-inspected object: the
  # field term is itself a live `{:beamtalk_object, …}` handle, so following it
  # is the same read-surface inspect call one level deeper. The clicked field's
  # term is carried back to the server by index against the current rows, so we
  # never round-trip a flattened string.
  def handle_event("drill", %{"index" => index}, %{assigns: %{inspect_rows: rows}} = socket) do
    case Enum.at(rows, String.to_integer(index)) do
      %{term: term, name: name} -> {:noreply, inspect_term(socket, name, term)}
      _ -> {:noreply, socket}
    end
  end

  def handle_event("drill", _params, socket), do: {:noreply, socket}

  # Transcript push, delivered directly over distribution to this LiveView pid.
  @impl true
  def handle_info({:transcript_output, text}, socket) do
    line = %{id: System.unique_integer([:positive]), text: to_string(text)}
    {:noreply, stream_insert(socket, :transcript, line)}
  end

  # Bindings-changed push (BT-2399 `bindings` stream): a *signal*, not the data.
  # An eval on any session in the workspace may have changed binding values, so
  # re-read this session's bindings through the read-surface and re-render the
  # pane. This is the "updating live as bindings change" acceptance criterion,
  # driven by the facade subscription rather than polling.
  def handle_info({:bindings_changed, _session_id}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    {:noreply, assign_bindings(socket, pid)}
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
      Workspace.unsubscribe_bindings(self())

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

  # ── bindings + inspector helpers ────────────────────────────────────────────

  # Read the session's live bindings through the read-surface and assign display
  # rows. Each row keeps the live `term` so the Inspector can follow object
  # references without a string round-trip; `inspectable?` flags object-valued
  # bindings the user can drill into.
  defp assign_bindings(socket, pid) do
    case Workspace.list_bindings(pid) do
      {:error, reason} ->
        assign(socket, bindings: [], bindings_error: Workspace.render_error(reason))

      pairs when is_list(pairs) ->
        rows =
          Enum.map(pairs, fn {name, term} ->
            %{
              name: name,
              value: Workspace.render_term(term),
              inspectable: Workspace.inspectable?(term)
            }
          end)

        assign(socket, bindings: rows, bindings_error: nil)
    end
  end

  # Inspect a binding selected by name: resolve its live term from the current
  # binding list, then inspect that term. The term — not a string — drives the op.
  defp inspect_binding(socket, pid, name) do
    case Workspace.list_bindings(pid) do
      pairs when is_list(pairs) ->
        case List.keyfind(pairs, name, 0) do
          {^name, term} -> inspect_term(socket, name, term)
          nil -> assign(socket, inspect_error: "binding not found: #{name}")
        end

      {:error, reason} ->
        assign(socket, inspect_error: Workspace.render_error(reason))
    end
  end

  # Inspect a single live term via the read-surface `inspect` op and assign the
  # resulting structured-field rows. Object-valued fields are flagged drillable,
  # carrying their live term so the next drill follows the reference one level
  # deeper. Non-object terms are not inspectable, so we say so rather than guess.
  defp inspect_term(socket, label, term) do
    if Workspace.inspectable?(term) do
      case Workspace.inspect_value(term) do
        {:ok, fields} when is_map(fields) ->
          assign(socket,
            inspect_target: %{label: to_string(label), header: Workspace.render_term(term)},
            inspect_rows: field_rows(fields),
            inspect_error: nil
          )

        {:ok, scalar} ->
          assign(socket,
            inspect_target: %{label: to_string(label), header: Workspace.render_term(term)},
            inspect_rows: [
              %{
                name: "value",
                value: Workspace.format_value(scalar),
                term: scalar,
                drillable: false
              }
            ],
            inspect_error: nil
          )

        {:error, reason} ->
          assign(socket, inspect_error: Workspace.render_error(reason))
      end
    else
      assign(socket,
        inspect_target: %{label: to_string(label), header: Workspace.render_term(term)},
        inspect_rows: [],
        inspect_error: "#{label} is a #{scalar_kind(term)} — no fields to inspect"
      )
    end
  end

  # Turn an inspect fields map (live terms) into ordered display rows. Each row
  # keeps the live field `term` so a drill on an object-valued slot follows the
  # real reference; `drillable` marks the object-valued ones.
  defp field_rows(fields) do
    fields
    |> Enum.map(fn {key, term} ->
      %{
        name: to_string(key),
        value: Workspace.render_term(term),
        term: term,
        drillable: Workspace.inspectable?(term)
      }
    end)
    |> Enum.sort_by(& &1.name)
  end

  defp scalar_kind(term) when is_integer(term), do: "number"
  defp scalar_kind(term) when is_float(term), do: "number"
  defp scalar_kind(term) when is_binary(term), do: "string"
  defp scalar_kind(term) when is_boolean(term), do: "boolean"
  defp scalar_kind(term) when is_list(term), do: "collection"
  defp scalar_kind(term) when is_map(term), do: "map"
  defp scalar_kind(_term), do: "value"

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

        <h2>Bindings (live)</h2>
        <%= if @bindings_error do %>
          <pre style="background:#fff0f0; padding:.75rem; border:1px solid #ecc;"><%= @bindings_error %></pre>
        <% end %>
        <%= if @bindings == [] do %>
          <p style="color:#666;">No bindings yet. Try <code>x := 42</code>.</p>
        <% else %>
          <table style="width:100%; border-collapse:collapse;">
            <tbody>
              <tr :for={b <- @bindings} style="border-bottom:1px solid #eee;">
                <td style="padding:.25rem .5rem; font-weight:bold; vertical-align:top;">{b.name}</td>
                <td style="padding:.25rem .5rem; width:100%;">{b.value}</td>
                <td style="padding:.25rem .5rem; white-space:nowrap;">
                  <%= if b.inspectable do %>
                    <button
                      type="button"
                      phx-click="inspect"
                      phx-value-name={b.name}
                      style="font-family:inherit;"
                    >
                      Inspect →
                    </button>
                  <% end %>
                </td>
              </tr>
            </tbody>
          </table>
        <% end %>

        <h2>Inspector</h2>
        <%= if @inspect_target do %>
          <p>
            Inspecting <strong>{@inspect_target.label}</strong>
            · <code>{@inspect_target.header}</code>
          </p>
        <% end %>
        <%= if @inspect_error do %>
          <pre style="background:#fff8e8; padding:.75rem; border:1px solid #eda;"><%= @inspect_error %></pre>
        <% end %>
        <%= if @inspect_target && @inspect_rows != [] do %>
          <table style="width:100%; border-collapse:collapse; background:#fafaff; border:1px solid #dde;">
            <tbody>
              <tr
                :for={{row, i} <- Enum.with_index(@inspect_rows)}
                style="border-bottom:1px solid #eef;"
              >
                <td style="padding:.25rem .5rem; font-weight:bold; vertical-align:top;">
                  {row.name}
                </td>
                <td style="padding:.25rem .5rem; width:100%;">{row.value}</td>
                <td style="padding:.25rem .5rem; white-space:nowrap;">
                  <%= if row.drillable do %>
                    <button
                      type="button"
                      phx-click="drill"
                      phx-value-index={i}
                      style="font-family:inherit;"
                    >
                      Follow →
                    </button>
                  <% end %>
                </td>
              </tr>
            </tbody>
          </table>
        <% else %>
          <%= if @inspect_target == nil && @inspect_error == nil do %>
            <p style="color:#666;">
              Spawn an object (<code>Counter spawn</code>), bind it, then Inspect it to
              follow its live references.
            </p>
          <% end %>
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
