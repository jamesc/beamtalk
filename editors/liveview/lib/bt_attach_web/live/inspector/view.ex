# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.Inspector.View do
  @moduledoc """
  Read-only view helpers for the Inspector: the head's target descriptor, the
  field and supervisor-child rows, the pid-stats chip accessors and the
  floating-window overlay component. Extracted from
  `BtAttachWeb.Live.Inspector` (BT-5112) with no behaviour change;
  `Inspector` delegates to it so every call site is unchanged.
  """

  use BtAttachWeb, :html

  alias BtAttach.Workspace

  # Build the Inspector head's target descriptor: the binding/field label,
  # the live printString header, and the class/pid type chips. For a live
  # actor the term is `{:beamtalk_object, class, _module, pid}` (over
  # distribution), so the class atom and pid render straight into the
  # spike's `proc-chips`. Non-object values carry no pid and report their
  # scalar kind as the class chip.
  @doc false
  def target_info(label, {:beamtalk_object, class, _module, pid} = term) when is_pid(pid) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: to_string(class),
      pid: inspect(pid)
    }
  end

  # Supervisor handles are pid-backed live refs too: chip the class + pid the
  # same way as objects so the head renders as a drillable target
  # (content/children is a follow-up — see `Workspace.inspect_value/1`).
  def target_info(label, {:beamtalk_supervisor, class, _module, pid} = term) when is_pid(pid) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: to_string(class),
      pid: inspect(pid)
    }
  end

  def target_info(label, term) do
    %{
      label: to_string(label),
      header: Workspace.render_term(term),
      class_name: scalar_kind(term),
      pid: nil
    }
  end

  # Turn an inspect fields map (live terms) into ordered display rows. Each
  # row keeps the live field `term` so a drill on an object-valued slot
  # follows the real reference; `drillable` marks the object-valued ones.
  @doc false
  def field_rows(fields) do
    fields
    |> Enum.map(fn {key, term} ->
      %{
        name: to_string(key),
        value: Workspace.render_term(term),
        term: term,
        drillable: Workspace.inspectable?(term),
        kind: term_kind(term)
      }
    end)
    |> Enum.sort_by(& &1.name)
  end

  # BT-2634: turn the runtime's supervisor-child maps (binary-keyed, from
  # `beamtalk_process_navigation:child_handles/1`) into Inspector display
  # rows. A child with a live `handle` (a Beamtalk actor / supervisor child)
  # is drillable — its `term` is the live `{:beamtalk_supervisor, …}` /
  # `{:beamtalk_object, …}` reference the "drill" event follows (ADR 0095). A
  # foreign / restarting child has `handle => :null`: it renders (so the
  # tree is complete) but is not drillable. The value column shows kind + pid
  # + child count so the supervision structure is legible at a glance.
  @doc false
  def supervisor_child_rows(child_rows) when is_list(child_rows) do
    Enum.map(child_rows, &supervisor_child_row/1)
  end

  defp supervisor_child_row(row) when is_map(row) do
    handle = Map.get(row, "handle", :null)
    drillable = handle != :null and handle != nil

    %{
      name: to_string(Map.get(row, "label", "child")),
      value: supervisor_child_value(row),
      term: if(drillable, do: handle, else: nil),
      drillable: drillable,
      kind: "ref"
    }
  end

  # The value-column text for a supervisor child row: "kind · pid · N
  # children" (child count only for supervisors), or "kind · restarting" for
  # a child caught mid-restart (no pid).
  defp supervisor_child_value(row) do
    kind = to_string(Map.get(row, "kind", "process"))
    pid = supervisor_child_pid_text(Map.get(row, "pid", :null))

    if Map.get(row, "isSupervisor", false) do
      count = Map.get(row, "childCount", 0)
      "#{kind} · #{pid} · #{count} #{pluralize_children(count)}"
    else
      "#{kind} · #{pid}"
    end
  end

  defp supervisor_child_pid_text(:null), do: "restarting"
  defp supervisor_child_pid_text(nil), do: "restarting"
  defp supervisor_child_pid_text(pid) when is_binary(pid), do: pid
  defp supervisor_child_pid_text(pid), do: to_string(pid)

  defp pluralize_children(1), do: "child"
  defp pluralize_children(_), do: "children"

  # Map a live term to the Inspector's "no fields to inspect" type word ("X
  # is a <scalar_kind>"). Derives from the single classifier
  # `Workspace.term_class/1` (BT-2635), so this is purely a presentation
  # mapping of its `{:scalar, kind}` output — it does not re-enumerate term
  # shapes. `{:ref, _}` terms never reach here from the non-inspectable
  # else-branch; they can reach it from `target_info/2`'s fallback, where
  # they fall through to "value". The `:value` / unmapped-scalar cases all
  # render "value".
  @doc false
  def scalar_kind(term) do
    case Workspace.term_class(term) do
      {:scalar, :integer} -> "number"
      {:scalar, :float} -> "number"
      {:scalar, :string} -> "string"
      {:scalar, :boolean} -> "boolean"
      {:scalar, :list} -> "collection"
      {:scalar, :map} -> "map"
      _ -> "value"
    end
  end

  # Map a live term to the spike Inspector's value-kind class (inspector.jsx
  # `valueClass`), driving the type-chip / value colour. Derives from the
  # single classifier `Workspace.term_class/1` (BT-2635): any live ref
  # (object, supervisor, future, pid) chips as the drillable `ref`; scalars
  # map to their CSS class. Booleans classify as `{:scalar, :boolean}`
  # (matched before the atom guard inside `term_class/1`), so they chip as
  # "bool", not "symbol". Lists/maps and any unrecognised term fall through
  # to "value", exactly as before centralisation. Public: also used by
  # `WorkspaceLive`'s Bindings-row kind chip (`apply_bindings/2`), which
  # predates this extraction and reuses the same classifier.
  def term_kind(term) do
    case Workspace.term_class(term) do
      {:ref, _kind} -> "ref"
      {:scalar, :boolean} -> "bool"
      {:scalar, :integer} -> "int"
      {:scalar, :float} -> "int"
      {:scalar, :string} -> "string"
      {:scalar, :atom} -> "symbol"
      _ -> "value"
    end
  end

  # ── pid-stats chip accessors (BT-2492) ──────────────────────────────────────
  #
  # The `pid_stats` op returns a binary-keyed map (`beamtalk_repl_ops_watch`):
  # `status`, `queue_depth`, `memory_bytes`, `reductions`, `current_function`,
  # plus `alive`. These read the head chips off that snapshot, returning nil
  # when the stat is absent (so the chip's `:if` hides it) — a dead pid
  # reports only `status: "dead"`, so the mailbox/reductions chips vanish
  # rather than show 0. Public: called from `WorkspaceLive`'s docked/window
  # panel template.

  # Process scheduling status (`running`/`waiting`/`runnable`/`dead`/…),
  # always shown when stats are present — it's the live "is it ticking" tell.
  def stat_status(stats) when is_map(stats), do: Map.get(stats, "status")
  def stat_status(_), do: nil

  # Mailbox (message queue) depth — only when the pid is alive (a dead pid
  # has no queue, and the map omits it).
  def stat_mailbox(stats) when is_map(stats), do: Map.get(stats, "queue_depth")
  def stat_mailbox(_), do: nil

  # Reduction count (a coarse "how much work has it done" gauge), thousands-
  # separated for readability like the spike. Absent on a dead pid.
  def stat_reductions(stats) when is_map(stats) do
    case Map.get(stats, "reductions") do
      n when is_integer(n) -> format_thousands(n)
      _ -> nil
    end
  end

  def stat_reductions(_), do: nil

  # Group an integer with thousands separators (the spike's
  # `toLocaleString()`), e.g. 1234567 → "1,234,567".
  defp format_thousands(n) when is_integer(n) and n < 0, do: "-" <> format_thousands(-n)

  defp format_thousands(n) when is_integer(n) do
    n
    |> Integer.to_string()
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1)
    |> String.reverse()
  end

  # True when `watch` is a pid-backed term watching `pid` — the shared
  # predicate every docked/window change-push router (`handle_info`) and
  # `pane_watched_elsewhere?/3` uses to decide whether a push/unsubscribe
  # applies to a given watcher.
  @doc false
  def watched_pid?({:beamtalk_object, _c, _m, watched}, pid)
      when is_pid(watched) and is_pid(pid),
      do: watched == pid

  def watched_pid?(_watch, _pid), do: false

  # Every distinct pid watched by an open floating window (BT-2493), deduped
  # so `WorkspaceLive.terminate/2` unsubscribes each watched actor exactly
  # once even when several windows share it. Public: called directly by
  # `WorkspaceLive.terminate/2`.
  def window_watched_terms(nil), do: []

  def window_watched_terms(windows) when is_list(windows) do
    windows
    |> Enum.map(& &1.watch)
    |> Enum.filter(&match?({:beamtalk_object, _c, _m, p} when is_pid(p), &1))
    |> Enum.uniq_by(fn {:beamtalk_object, _c, _m, pid} -> pid end)
  end

  # ── floating inspector windows overlay (BT-2493, epic BT-2482 Phase 3) ───────
  #
  # The overlay layer: one draggable, stackable inspector window per
  # `:windows` entry. The layer itself is pointer-inert so it never eats
  # clicks on the cockpit beneath; each window re-enables pointer events on
  # itself. Window position (`left`/`top`) and stacking (`z-index`) come
  # straight from the per-window state the WindowDrag hook reports on drop /
  # focus, so they survive an unrelated re-render. Each window's content
  # reuses the docked Inspector's markup (head + breadcrumb + chips + ivar
  # table + poke), parameterised by window id so its drill/crumb/close/
  # freeze/poke events carry the id back to the right window.

  attr :windows, :list, required: true
  attr :role, :atom, required: true

  def inspector_windows(assigns) do
    ~H"""
    <div class="insp-overlay" id="inspector-overlay">
      <%!-- Off-screen recovery (BT-2527 #4): re-cascade every window back onto
           the visible ladder. Shown only when a desk is open so it never
           floats over an empty cockpit. --%>
      <button
        :if={@windows != []}
        type="button"
        class="insp-tidy"
        phx-click="window_reset_positions"
        title="Bring all inspector windows back on-screen"
      >
        Tidy windows
      </button>
      <.inspector_window :for={w <- @windows} win={w} role={@role} />
    </div>
    """
  end

  attr :win, :map, required: true
  attr :role, :atom, required: true

  defp inspector_window(assigns) do
    ~H"""
    <section
      class="insp-window"
      id={"inspector-window-#{@win.id}"}
      style={"left:#{@win.x}px;top:#{@win.y}px;z-index:#{@win.z};"}
      phx-hook="WindowDrag"
      data-window-id={@win.id}
    >
      <%!-- Title bar: the drag handle (`.iw-title`, grabbed by the WindowDrag
           hook) plus the live freeze toggle and the close button. Dragging
           the bar moves the window client-side; the hook reports the final
           x/y on drop. --%>
      <header class="iw-bar" data-window-drag-handle>
        <span class="iw-title mono">
          {(@win.target && @win.target.label) || @win.label}
        </span>
        <span class="spacer"></span>
        <button
          :if={@win.target && @win.target.pid}
          type="button"
          class={["insp-freeze", (@win.frozen && "frozen") || "live"]}
          phx-click="window_freeze"
          phx-value-id={@win.id}
          title={
            if @win.frozen,
              do: "Frozen snapshot — click to track live",
              else: "Tracking live (subscribed to changes) — click to freeze a snapshot"
          }
        >
          <span class="iwf-dot"></span>{(@win.frozen && "frozen") || "live"}
        </button>
        <button
          type="button"
          class="iw-close"
          phx-click="window_close"
          phx-value-id={@win.id}
          title="Close window"
          aria-label="Close window"
        >
          ×
        </button>
      </header>
      <div class="iw-body">
        <%= if @win.target do %>
          <div class="insp-head">
            <div :if={length(@win.crumbs) > 1} class="insp-crumbs">
              <%= for {crumb, i} <- Enum.with_index(@win.crumbs) do %>
                <span :if={i > 0} class="sep">›</span>
                <span class="c" phx-click="window_crumb" phx-value-id={@win.id} phx-value-index={i}>
                  {crumb.label}
                </span>
              <% end %>
            </div>
            <div class="ps mono">
              Inspecting <strong>{@win.target.label}</strong>
              <span class="ps-header">{@win.target.header}</span>
            </div>
            <div class="proc-chips">
              <span class="chip">class <b>{@win.target.class_name}</b></span>
              <span :if={@win.target.pid} class="chip">
                pid <b>{@win.target.pid}</b>
              </span>
              <span :if={stat_status(@win.stats)} class="chip pid-stat">
                <span class="dot"></span>{stat_status(@win.stats)}
              </span>
              <span :if={not is_nil(stat_mailbox(@win.stats))} class="chip pid-stat">
                mailbox <b>{stat_mailbox(@win.stats)}</b>
              </span>
              <span :if={not is_nil(stat_reductions(@win.stats))} class="chip pid-stat">
                reductions <b>{stat_reductions(@win.stats)}</b>
              </span>
            </div>
          </div>
        <% end %>
        <div class="iw-content">
          <.notice
            :if={@win.error}
            variant={:warn}
            message={@win.error}
            dismiss_attrs={%{"phx-click" => "dismiss_window_error", "phx-value-id" => @win.id}}
          />
          <%= if @win.target && @win.rows != [] do %>
            <table
              id={"inspector-window-fields-#{@win.id}"}
              class="ivar-table"
              phx-hook="FieldFlash"
              data-flash-gen={@win.flash_gen}
            >
              <tbody>
                <tr
                  :for={{row, i} <- Enum.with_index(@win.rows)}
                  class={row.drillable && "drillable"}
                  phx-click={row.drillable && "window_drill"}
                  phx-value-id={row.drillable && @win.id}
                  phx-value-index={row.drillable && i}
                >
                  <td class="k">{row.name}</td>
                  <td class={["v", row.kind]} data-flash-key={row.name} data-flash-val={row.value}>
                    {row.value}
                  </td>
                  <td class="follow">
                    <span :if={row.drillable} class="follow-link">follow →</span>
                  </td>
                </tr>
              </tbody>
            </table>
          <% else %>
            <p :if={@win.target == nil && @win.error == nil} class="empty">
              Nothing to inspect.
            </p>
          <% end %>
          <%!-- Owner-only poke bar (BT-2492), per-window: pokeable only at a
               single named-binding crumb root (the same well-formedness gate
               the docked pane uses), and only for the owner role. --%>
          <div
            :if={@win.target && @win.target.pid && @role == :owner && window_pokeable?(@win)}
            class="poke"
          >
            <div class="poke-label">Send a message to {@win.target.label}</div>
            <form class="poke-row" phx-submit="window_poke" phx-value-id={@win.id}>
              <span class="poke-recv mono">‹recv›</span>
              <input
                class="field mono"
                name="message"
                autocomplete="off"
                placeholder="increment   ·   incrementBy: 10"
              />
              <button type="submit" class="btn">Send</button>
            </form>
            <div :if={@win.poke_result} class="poke-out ok mono">{@win.poke_result}</div>
            <div :if={@win.poke_error} class="poke-out warn mono">{@win.poke_error}</div>
          </div>
        </div>
      </div>
    </section>
    """
  end

  # Whether a floating window's head can be poked: a pid-backed object at the
  # inspection root with a valid-identifier binding name (the same contract
  # as the docked `pokeable?/1`, scoped to one window's crumbs).
  defp window_pokeable?(%{crumbs: [%{label: label}]}) when is_binary(label),
    do: valid_receiver?(label)

  defp window_pokeable?(_w), do: false

  # A poke receiver must be a plain lowercase Beamtalk identifier (a binding
  # name) — not the `→ result` synthetic label or anything with
  # spaces/punctuation that would make `<label> <message>` ill-formed
  # source. This is a well-formedness gate, not an injection guard: the poke
  # eval is RBAC-gated (owner-only) and the label comes from the crumb the
  # *server* set from the inspected binding name, never raw browser input. A
  # pseudo-keyword binding (`self`/`nil`/…) that slips through just produces
  # a normal eval that DNUs or no-ops — no escalation.
  def valid_receiver?(label), do: Regex.match?(~r/\A[a-z_][A-Za-z0-9_]*\z/, label)
end
