# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.MethodEditor.Refresh do
  @moduledoc """
  Re-reads open editor tabs after the live image changes (BT-2598, BT-2655):
  reverted `:def` buffers and the coalesced source refresh. Extracted from
  `BtAttachWeb.Live.MethodEditor` (BT-5112) with no behaviour change;
  `MethodEditor` delegates to it.

  """

  use BtAttachWeb, :html

  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.Live.SystemBrowser
  alias BtAttachWeb.WorkspaceLive

  # BT-2655: re-read the reverted `:def` tabs' *editable* definition buffer
  # so the visible editor reflects the reverted class header without a
  # close/reopen. This is the one piece the generic push refresh
  # (`refresh_source_tab/2`) intentionally skips: it re-reads only the
  # `:def` doc block, never the editable definition body, so a concurrent
  # edit during another session's flush is not clobbered. A revert is the
  # safe exception — blocked under pending edits for the reverted path
  # (`path_has_pending_edits?/2`, BT-2598 d2), so no in-progress edit can be
  # lost. A dirty tab is left untouched all the same (defence in depth);
  # method tabs and tabs outside `reloaded` pass through and are handled by
  # the refresh that follows. Public: `BtAttachWeb.Live.Dock`'s
  # `git_revert_event/2` (BT-3295) calls it directly.
  def reload_reverted_def_buffers(socket, reloaded) do
    tabs =
      Enum.map(socket.assigns.tabs, fn tab ->
        if match?(%{kind: :def, dirty: false}, tab) and
             MapSet.member?(reloaded, MethodEditor.tab_disk_key(tab)) do
          reread_reverted_def_buffer(socket, tab)
        else
          tab
        end
      end)

    socket
    |> assign(:tabs, tabs)
    |> resync_active_tab(tabs)
  end

  # Re-read one reverted `:def` tab's editable definition skeleton (header +
  # state) from the now-reverted live image. Only overwrite the editable
  # buffer when the re-fetch actually returned a skeleton —
  # `class_definition_info/2` yields `""` on a transient fetch failure, and
  # blanking a tab the user is looking at would be worse than leaving the
  # prior (about-to-be-correct) body until the next refresh.
  defp reread_reverted_def_buffer(socket, tab) do
    case MethodEditor.class_definition_info(socket, tab.class) do
      {"", _comment, _native_module, _class_modifiers, _is_protocol} ->
        tab

      {definition, _comment, _native_module, _class_modifiers, _is_protocol} ->
        # Only the editable definition buffer is touched here; the doc
        # block + badges are refreshed by `refresh_source_tab/2` in the
        # follow-up push refresh.
        %{tab | source: definition, base: definition, dirty: false, disk_differs: false}
    end
  end

  # BT-2598: the live image changed (a class was (re)loaded or removed).
  # Re-pull every source-dependent surface so open windows reflect the new
  # image without a manual refresh: the browser class list (cross-called back
  # on `BtAttachWeb.Live.SystemBrowser`, BT-3297) and the active ChangeLog
  # (cross-called back on `WorkspaceLive`, which still owns the Changes
  # pane), all open clean editor tabs (method +
  # definition, owned here), and the git panel when it is the active dock
  # tab (also cross-called back, `BtAttachWeb.Live.Dock` owns it). Folded
  # together here — rather than split across callers — so every caller gets
  # one coherent update in the same order the pre-extraction function did.
  # Public: `BtAttachWeb.Live.Dock`'s `git_revert_event/2` (BT-3295) and
  # `WorkspaceLive`'s coalesced `:do_source_refresh` handler both call it
  # directly.
  def refresh_after_source_change(socket) do
    socket
    |> SystemBrowser.assign_browser_classes()
    |> WorkspaceLive.assign_changes()
    |> refresh_open_source_tabs()
    |> WorkspaceLive.maybe_refresh_git()
  end

  # BT-2598: re-read every open *clean* editor tab from the live image so a
  # source change that landed out-of-band (a git revert's reload, another
  # session's flush, an MCP edit) is reflected in the visible buffer. A
  # `dirty` tab is left untouched — never clobber the user's in-progress
  # work — exactly as the re-activation re-read in `open_method_tab/4` /
  # `open_definition/2` does; this generalises that pull into a push so the
  # user need not re-focus the tab. An empty-source fallback (a
  # since-removed method/class, a transient facade error) keeps the
  # existing buffer rather than blanking a tab the user is looking at.
  defp refresh_open_source_tabs(socket) do
    tabs = Enum.map(socket.assigns.tabs, &refresh_source_tab(socket, &1))

    socket
    |> assign(:tabs, tabs)
    |> resync_active_tab(tabs)
  end

  # Re-read one tab's source from the image. Clean `:method`/`:def` tabs
  # refresh; dirty tabs and any other shape pass through unchanged.
  defp refresh_source_tab(socket, %{kind: :method, dirty: false} = tab) do
    case MethodEditor.method_source_info(socket, tab.class, tab.side, tab.selector) do
      %{source: ""} ->
        tab

      info ->
        %{
          tab
          | source: info.source,
            base: info.source,
            # Pick up *new* divergence from an out-of-band patch, but
            # never clear a divergence already set locally (mirrors the
            # re-activation invariant).
            disk_differs: tab.disk_differs or info.disk_differs,
            runtime_only: info.runtime_only,
            disk_source: MethodEditor.reactivation_disk_source(tab, info),
            doc: info.doc,
            signature: info.signature,
            native_delegate: info.native_delegate
        }
    end
  end

  defp refresh_source_tab(socket, %{kind: :def, dirty: false} = tab) do
    # Only the read-only doc block is re-read (the editable definition
    # buffer is left untouched), matching the `:def` re-activation re-read.
    # A failed re-fetch keeps the prior backing module rather than hiding
    # the "Erlang backend" badge. BT-2605: the reflected modifier badges
    # (sealed/abstract/native) are refreshed from the same fetch so a
    # recompile-into/out-of a modifier shows on push refresh; a transient
    # failure (native_module nil) keeps the prior native flag.
    {_definition, comment, native_module, class_modifiers, is_protocol} =
      MethodEditor.class_definition_info(socket, tab.class)

    resolved_native = native_module || tab.native_module

    %{
      tab
      | doc: comment,
        is_protocol: is_protocol,
        native_module: resolved_native,
        # `nil` modifiers signal a transient fetch failure — keep the
        # prior list rather than clearing the badges (BT-2605 review).
        class_modifiers: class_modifiers || tab.class_modifiers,
        class_native: is_binary(resolved_native) and resolved_native != ""
    }
  end

  defp refresh_source_tab(_socket, tab), do: tab

  # Keep the rendered active-tab editor in sync after a push refresh:
  # re-sync the active tab's fields so its breadcrumb/badges/doc block
  # re-render from the refreshed entry. A dirty active tab is untouched
  # above, so this never disturbs an edit.
  #
  # BT-2588: also decides whether to keep or clear the save/flush banners,
  # via TWO signals combined:
  #
  #   * `:save_echo_pending` — a one-shot flag `compile_clean/3` arms on a
  #     successful save. This function only READS it (never clears it —
  #     see why below); unset ⇒ this push is definitely not our own save's
  #     echo — clear unconditionally.
  #   * for a `:method` tab, whether the re-read body actually changed.
  #     Even when the flag IS set, BT-2600's coalescing can fold a
  #     genuinely external change into the SAME debounced push as our own
  #     save's echo (two `ClassLoaded` events landing within the same
  #     ~60ms window collapse to one `:do_source_refresh`) — the body
  #     comparison catches that case. Skipped for `:def`:
  #     `refresh_source_tab/2` never re-reads a `:def` tab's editable
  #     definition body (only its doc/modifiers), so the comparison would
  #     be vacuously "unchanged" on every push and could never legitimately
  #     clear a `:def` tab's banner — the flag alone decides there, same
  #     narrow coalescing blind spot as any other single-signal check,
  #     self-healing on the next tab switch or save.
  #
  # This function does NOT clear the flag (review-bot finding):
  # `git_revert_event/2` calls `resync_active_tab/2` TWICE in one
  # synchronous pipeline (`reload_reverted_def_buffers/2`, then
  # `refresh_after_source_change/1`) — a read-then-clear here would let the
  # first call spend the flag, leaving the second (which decides the
  # actually-rendered state) always seeing it unset and wiping an unrelated
  # tab's still-fresh banner on ANY git revert. The flag is a one-shot
  # signal for the ASYNC push cycle specifically, so only its real owner,
  # `WorkspaceLive`'s `handle_info(:do_source_refresh, ...)`, clears it —
  # once, after every synchronous caller (this function included) has had
  # its look.
  defp resync_active_tab(socket, tabs) do
    save_echo? = socket.assigns[:save_echo_pending]

    case Enum.find(tabs, &(&1.id == socket.assigns[:active_tab])) do
      %{dirty: false} = active ->
        # BT-2655: if the re-read changed the active tab's *body* (a git
        # revert, a push reconcile), bump `editor_rev` so the
        # `phx-update="ignore"` CodeMirror host is re-keyed and remounts
        # with the new source. A no-op re-read (same body) leaves the rev —
        # and thus the live editor instance — untouched, so a routine
        # refresh of an unchanged tab never disturbs the editor.
        socket = maybe_bump_editor_rev(socket, active.source)

        keep_banner? =
          save_echo? and
            (active.kind != :method or active.source == socket.assigns[:edit_source])

        if keep_banner? do
          MethodEditor.sync_active_fields(socket, active)
        else
          MethodEditor.sync_active(socket, active)
        end

      _ ->
        socket
    end
  end

  defp maybe_bump_editor_rev(socket, new_source) do
    if new_source == socket.assigns[:edit_source] do
      socket
    else
      assign(socket, :editor_rev, socket.assigns.editor_rev + 1)
    end
  end
end
