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

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace

  # All RBAC-relevant workspace ops go through the curated facade (ADR 0091
  # Decision 3) — never a raw Workspace/:rpc call from an event handler. Pure
  # transport/display/lifecycle helpers (connect, render_term, session start)
  # stay on Workspace: they are not browser-supplied ops. `ctx/1` carries the
  # request identity the facade audits / RBAC gates on (BT-2421).
  defp ctx(socket),
    do: %{user: socket.assigns[:current_user], role: socket.assigns[:role] || :owner}

  @impl true
  def mount(_params, _session, socket) do
    # LiveView `mount/3` runs TWICE: first on the disconnected HTTP render, then
    # on the connected WebSocket mount (and again on every reconnect). Only the
    # *connected* mount attaches over distribution — the disconnected render must
    # NOT create a workspace session, or every page load would leak an orphaned
    # one. The per-tab resume token is only present on the connected mount (it
    # rides the LiveSocket `params`), so `get_connect_params/1` is the right read.
    if connected?(socket) do
      token = connect_token(socket)
      attach(assign(socket, :token, token))
    else
      {:ok, assign(socket, connected: false, error: nil, token: nil)}
    end
  end

  # The per-tab token minted in `sessionStorage` (assets/js/app.js) and replayed
  # on every (re)connect. `get_connect_params/1` is only available on the
  # connected mount; a non-binary/absent value just disables resume (each connect
  # gets a fresh, non-resumable session) rather than crashing.
  defp connect_token(socket) do
    case get_connect_params(socket) do
      %{"workspace_token" => token} when is_binary(token) -> token
      _ -> nil
    end
  end

  defp attach(%{assigns: %{token: token}} = socket) do
    socket =
      case Workspace.connect() do
        :ok ->
          # Resume the tab's existing session if the registry still holds a live
          # one (reconnect within the grace window); otherwise start fresh.
          case resume_or_start(token) do
            {:ok, session_id, pid} ->
              bind_session(socket, session_id, pid)

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

  # Resume the tab's session if the registry has a *live* one for this token,
  # else start a brand-new workspace-supervised session and register it.
  #
  # A registry hit is only trusted if the remote session pid is still reachable:
  # the workspace could have died/restarted between the disconnect and this
  # reconnect, leaving a stale entry. We probe with a cheap `is_process_alive/1`
  # on the workspace node (`Workspace.session_alive?/1`) and fall back to a fresh
  # session on a dead pid, so resume never claims success on a session that is
  # already gone.
  defp resume_or_start(token) do
    case token && SessionRegistry.checkout(token) do
      {:resumed, session_id, pid} ->
        if Workspace.session_alive?(pid) do
          {:ok, session_id, pid}
        else
          # Stale entry (workspace restarted): discard it and start fresh.
          SessionRegistry.discard(token)
          start_fresh(token)
        end

      _miss ->
        start_fresh(token)
    end
  end

  defp start_fresh(token) do
    session_id = "phoenix-#{System.unique_integer([:positive])}"

    case Workspace.start_session(session_id) do
      pid when is_pid(pid) ->
        # Register before binding so a crash mid-bind can't leak: the registry
        # owns the close, keyed by the tab token (a nil token simply skips
        # registration — that session is non-resumable and closed in terminate/2).
        SessionRegistry.register(token, session_id, pid)
        {:ok, session_id, pid}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Subscribe THIS LiveView pid (location-transparent over dist) to the Transcript
  # AND bindings streams through the BT-2399 facade — no direct gen_server cast.
  # The facade's cast returns `:ok`; any non-ok reply (`{:badrpc, _}`) means a
  # stream is NOT live, so we must not render the pane as connected and claim a
  # working stream. Pattern-match both subscribe results (Wave-1 review rule) and,
  # on failure, release the session through the registry so it can't leak.
  #
  # Shared by the fresh-start and resume paths: on resume the same subscribe +
  # bindings re-read re-establishes the live streams for the new LiveView pid
  # (the old pid's subscriptions died with it), so the resumed tab gets its
  # Transcript and bindings flowing again with its accumulated state intact.
  defp bind_session(socket, session_id, pid) do
    with :ok <- Facade.dispatch(:subscribe_transcript, %{pid: self()}, ctx(socket)),
         :ok <- Facade.dispatch(:subscribe_bindings, %{pid: self()}, ctx(socket)) do
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
      # Method editor (Wave 3): the write-surface edit/save/flush pane.
      |> assign(:edit_class, "")
      |> assign(:edit_selector, "")
      |> assign(:edit_source, "")
      |> assign(:save_result, nil)
      |> assign(:save_error, nil)
      |> assign(:flush_result, nil)
      |> assign(:flush_error, nil)
      |> assign_bindings(pid)
      |> assign_changes()
      |> stream(:transcript, [])
    else
      other ->
        Logger.error("subscribe failed: #{inspect(other)}")
        # Drop whichever subscription may have succeeded, then release the
        # session through the registry (which closes it) so we don't leave a
        # dangling subscriber or an orphaned session behind.
        Workspace.unsubscribe_transcript(self())
        Workspace.unsubscribe_bindings(self())
        force_close(socket.assigns[:token], pid)

        assign(socket,
          connected: false,
          error: "subscribe failed: #{inspect(other)}"
        )
    end
  end

  # Close a session immediately (not via the grace timer): used when binding
  # fails, so a half-started session can't linger. A registered token is
  # discarded (the registry closes + forgets it now); an unregistered (nil-token)
  # session is closed directly.
  defp force_close(token, _pid) when is_binary(token) do
    SessionRegistry.discard(token)
    :ok
  end

  defp force_close(_token, pid) when is_pid(pid) do
    Workspace.close_session(pid)
    :ok
  end

  defp force_close(_token, _pid), do: :ok

  @impl true
  def handle_event("eval", %{"expr" => expr}, %{assigns: %{session_pid: pid}} = socket)
      when is_pid(pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: expr}, ctx(socket)) do
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

      # The facade (BT-2420/2421) can short-circuit BEFORE dispatching to the
      # workspace — an RBAC denial (`:unauthorized`, e.g. an Observer) or an
      # off-vocabulary op (`:forbidden_op`) — returning a 2-tuple the workspace
      # eval contract never produces. Render it as an actionable message rather
      # than crashing the LiveView on an unmatched case clause.
      {:error, reason} ->
        {:noreply,
         assign(socket, result: nil, output: nil, error: facade_error(reason), expr: expr)}
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
    # `index` is client-supplied; parse defensively so a malformed value can't
    # crash the LiveView (`String.to_integer/1` would raise on non-digits).
    with {i, ""} when i >= 0 <- Integer.parse(index),
         %{term: term, name: name} <- Enum.at(rows, i) do
      {:noreply, inspect_term(socket, name, term)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("drill", _params, socket), do: {:noreply, socket}

  # ── method editor (Wave 3, write-surface ADR 0082) ──────────────────────────

  # Save (durably patch) the edited method on the workspace via the write-surface.
  # On success the method is compiled + flushed into the live BEAM module and an
  # entry is recorded in the workspace ChangeLog, so a subsequent eval observes the
  # new behaviour and the change appears in the change-history pane. A failed
  # compile/save returns a structured #beamtalk_error{} we render as an actionable
  # message (not a flattened string).
  @impl true
  def handle_event(
        "save_method",
        %{"class" => class, "selector" => selector, "source" => source},
        socket
      )
      when is_binary(class) and is_binary(selector) and is_binary(source) do
    {:noreply, save_method(socket, class, selector, source)}
  end

  # Malformed payload (missing keys or non-binary values): never let a crafted
  # form event crash the LiveView — `save_method/4` calls `String.trim/1`, which
  # would raise on a non-binary. Surface a validation error instead.
  def handle_event("save_method", _params, socket) do
    {:noreply, assign(socket, save_result: nil, save_error: "Invalid method form payload.")}
  end

  # Flush all pending durable changes to disk ("Save All to Disk", ADR 0082
  # `Workspace flush`). The summary's conflicts/skipped lists carry recoverable
  # conditions; a hard runtime failure renders as a structured error.
  def handle_event("flush", _params, socket) do
    {:noreply, flush_changes(socket)}
  end

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
    # Best-effort: drop our Transcript + bindings subscriptions so the workspace
    # doesn't keep pushing to this (now-dead) pid. The event server also
    # auto-removes dead subscribers via its monitor, so this is belt-and-braces.
    #
    # Then hand the session to the registry's grace window rather than closing it
    # outright. A LiveView `terminate/2` fires on BOTH a real tab close AND a
    # transient socket drop (the latter immediately re-mounts and reconnects). If
    # we closed the session here, a reconnect would always find it gone and lose
    # the tab's accumulated state. So `release/1` schedules a short reap that a
    # reconnecting `checkout/1` cancels (resume); if no reconnect arrives, the
    # registry closes the workspace-supervised session — no orphaned sessions.
    #
    # The session is owned by the workspace's `beamtalk_session_sup` (not this
    # LiveView process), so it does NOT go away just because we exit — the
    # registry's reap is what actually reclaims it.
    if socket.assigns[:connected] do
      Workspace.unsubscribe_transcript(self())
      Workspace.unsubscribe_bindings(self())

      case socket.assigns[:token] do
        token when is_binary(token) ->
          # Resumable session: defer teardown to the grace window.
          SessionRegistry.release(token)

        _ ->
          # No token (resume disabled): nothing in the registry owns this
          # session, so close it directly here or it would leak.
          case socket.assigns[:session_pid] do
            pid when is_pid(pid) -> Workspace.close_session(pid)
            _ -> :ok
          end
      end
    end

    :ok
  end

  # Render a facade short-circuit (RBAC denial / off-vocabulary op) as a clear,
  # user-facing message. These are Phoenix-side decisions, so they don't go
  # through the workspace error formatter.
  defp facade_error(:unauthorized),
    do: "Not authorized: your role may not perform this operation."

  defp facade_error(:forbidden_op), do: "Operation not permitted."
  defp facade_error(reason), do: Workspace.render_error(reason)

  # Blank captured output is not worth rendering a pane for.
  defp present(""), do: nil
  defp present(nil), do: nil
  defp present(output) when is_binary(output), do: output

  # ── method editor helpers (Wave 3) ──────────────────────────────────────────

  # Validate the edit form, then drive the write-surface save. Empty class or
  # selector is a local validation error (rendered without a round-trip); a real
  # save threads the body value straight to the workspace install chokepoint.
  defp save_method(socket, class, selector, source) do
    class = String.trim(class)
    selector = String.trim(selector)

    socket =
      assign(socket,
        edit_class: class,
        edit_selector: selector,
        edit_source: source
      )

    cond do
      class == "" ->
        assign(socket, save_result: nil, save_error: "Enter a class name to save a method.")

      selector == "" ->
        assign(socket, save_result: nil, save_error: "Enter a selector to save a method.")

      true ->
        case Facade.dispatch(
               :save,
               %{class: class, selector: selector, source: source},
               ctx(socket)
             ) do
          {:ok, saved_class} ->
            # The patch is live + logged; refresh the change-history pane so the
            # new entry is visible (ChangeLog coherence).
            socket
            |> assign(
              save_result: "Saved #{selector} on #{saved_class}",
              save_error: nil,
              flush_result: nil,
              flush_error: nil
            )
            |> assign_changes()

          {:error, reason} ->
            # `reason` may be a facade RBAC denial (`:unauthorized`) for a crafted
            # event from a read-only role, or a workspace #beamtalk_error{}.
            assign(socket, save_result: nil, save_error: facade_error(reason))
        end
    end
  end

  # Drive the write-surface flush and render its summary, then refresh changes so
  # the (now-flushed) entries drop out of the active view.
  defp flush_changes(socket) do
    case Facade.dispatch(:flush, %{}, ctx(socket)) do
      {:ok, summary} ->
        socket
        |> assign(flush_result: Workspace.format_flush_summary(summary), flush_error: nil)
        |> assign_changes()

      {:error, reason} ->
        assign(socket, flush_result: nil, flush_error: facade_error(reason))
    end
  end

  # Read the active ChangeLog ("Workspace changes", ADR 0082) and assign display
  # rows. A workspace that is unreachable or returns an unexpected shape renders an
  # error rather than crashing the pane.
  defp assign_changes(socket) do
    case Facade.dispatch(:changes, %{}, ctx(socket)) do
      rows when is_list(rows) ->
        assign(socket, changes: rows, changes_error: nil)

      {:error, reason} ->
        assign(socket, changes: [], changes_error: Workspace.render_error(reason))
    end
  end

  # ── bindings + inspector helpers ────────────────────────────────────────────

  # Read the session's live bindings through the read-surface and assign display
  # rows. Each row keeps the live `term` so the Inspector can follow object
  # references without a string round-trip; `inspectable?` flags object-valued
  # bindings the user can drill into.
  defp assign_bindings(socket, pid) do
    case Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket)) do
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
    case Facade.dispatch(:bindings, %{session_pid: pid}, ctx(socket)) do
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
      case Facade.dispatch(:inspect, %{term: term}, ctx(socket)) do
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
        <p>
          Attached to <strong>{@node}</strong> · session <code>{@session_id}</code>
          <span :if={@role == :observer} style="color:#a60;">· read-only (Observer)</span>
        </p>

        <%= if @role == :owner do %>
          <form phx-submit="eval" style="display:flex; gap:.5rem; margin:1rem 0;">
            <input
              name="expr"
              value={@expr}
              autocomplete="off"
              style="flex:1; padding:.5rem; font-family:inherit;"
            />
            <button type="submit" style="padding:.5rem 1rem;">Eval</button>
          </form>
        <% else %>
          <p style="color:#666; margin:1rem 0;">
            Your role is read-only — evaluation and editing are disabled. You can
            still browse bindings, follow references in the Inspector, and watch
            the live Transcript.
          </p>
        <% end %>

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

        <%= if @role == :owner do %>
          <h2>Method Editor (write-surface)</h2>
          <p style="color:#666;">
            Edit a method and Save to compile + flush it onto the workspace
            (<code>Counter compile: #increment source: …</code>, ADR 0082). A later
            eval observes the new behaviour.
          </p>
          <form phx-submit="save_method" style="margin:1rem 0;">
            <div style="display:flex; gap:.5rem; margin-bottom:.5rem;">
              <input
                name="class"
                value={@edit_class}
                placeholder="Class (e.g. Counter)"
                autocomplete="off"
                style="flex:1; padding:.5rem; font-family:inherit;"
              />
              <input
                name="selector"
                value={@edit_selector}
                placeholder="selector (e.g. increment)"
                autocomplete="off"
                style="flex:1; padding:.5rem; font-family:inherit;"
              />
            </div>
            <textarea
              name="source"
              rows="4"
              placeholder="increment => self.value := self.value + 1"
              style="width:100%; padding:.5rem; font-family:inherit; box-sizing:border-box;"
            ><%= @edit_source %></textarea>
            <div style="display:flex; gap:.5rem; margin-top:.5rem;">
              <button type="submit" style="padding:.5rem 1rem;">Save Method</button>
              <button type="button" phx-click="flush" style="padding:.5rem 1rem;">
                Save All to Disk (flush)
              </button>
            </div>
          </form>
        <% end %>

        <%= if @save_result do %>
          <pre style="background:#f0fff0; padding:.75rem; border:1px solid #cec;"><%= @save_result %></pre>
        <% end %>
        <%= if @save_error do %>
          <pre style="background:#fff0f0; padding:.75rem; border:1px solid #ecc;"><%= @save_error %></pre>
        <% end %>
        <%= if @flush_result do %>
          <pre style="background:#eef6ff; padding:.75rem; border:1px solid #cce;"><%= @flush_result %></pre>
        <% end %>
        <%= if @flush_error do %>
          <pre style="background:#fff0f0; padding:.75rem; border:1px solid #ecc;"><%= @flush_error %></pre>
        <% end %>

        <h2>Changes (ChangeLog)</h2>
        <%= if @changes_error do %>
          <pre style="background:#fff0f0; padding:.75rem; border:1px solid #ecc;"><%= @changes_error %></pre>
        <% end %>
        <%= if @changes == [] do %>
          <p style="color:#666;">No pending changes. Save a method to record one.</p>
        <% else %>
          <table style="width:100%; border-collapse:collapse;">
            <thead>
              <tr style="border-bottom:1px solid #ccc; text-align:left;">
                <th style="padding:.25rem .5rem;">Class</th>
                <th style="padding:.25rem .5rem;">Selector</th>
                <th style="padding:.25rem .5rem;">Intent</th>
                <th style="padding:.25rem .5rem;">Flushable</th>
                <th style="padding:.25rem .5rem;">Author</th>
              </tr>
            </thead>
            <tbody>
              <tr :for={c <- @changes} style="border-bottom:1px solid #eee;">
                <td style="padding:.25rem .5rem; font-weight:bold;">{c.class}</td>
                <td style="padding:.25rem .5rem;">{c.selector}</td>
                <td style="padding:.25rem .5rem;">{c.intent}</td>
                <td style="padding:.25rem .5rem;">{if c.flushable, do: "yes", else: "no"}</td>
                <td style="padding:.25rem .5rem;">{c.author_kind}</td>
              </tr>
            </tbody>
          </table>
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
