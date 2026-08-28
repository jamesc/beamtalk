# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.MethodEditor do
  @moduledoc """
  The tabbed Method Editor (BT-2494, epic BT-2482 Phase 2) — tab strip,
  compile (⌘S)/save routing, dirty tracking, and the class-definition/native
  editable-source panes — extracted out of `BtAttachWeb.WorkspaceLive`
  (BT-3296, epic BT-3290) so its `handle_event/3` clauses and the tab data
  model they drive are directly unit-testable instead of only reachable
  through a full-LiveView integration test. Follows the same extraction
  shape `BtAttachWeb.Live.Inspector` (BT-3291) and `BtAttachWeb.Live.Dock`
  (BT-3295) established.

  This module owns:

    * **Tab strip** — `"tab_select"` / `"tab_close"` / `"tab_close_active"`:
      pure view state over the open-tab list, no workspace round-trip.
    * **Compile (⌘S)** — `"save_method"`: reads the active tab's *kind* (not
      the payload shape) to route a `:method` tab through the write-surface
      `save` op (compile + flush, ADR 0082) or a `:def` tab through `eval`
      (compiling the whole class definition) — neither invents a new server
      op. The historical no-tab payload (`tab` absent) takes the method path
      unchanged.
    * **Class definitions** — `"open_definition"`: opens (or re-focuses) the
      active tab's class-definition tab.
    * **Dirty tracking** — `"edit_source"`: flips the active tab's `dirty`
      flag when its live source diverges from the last-compiled `base`.
    * **Doc-block disclosure** — `"toggle_doc"`.
    * **Selection tracking** — `"select_source"`: the method-editor CmEditor
      hook's selection report, kept separate from the Workspace dock's own
      `ws_selection` (`BtAttachWeb.Live.Dock`).
    * **Native (`.erl`) editable pane** — `"native_source"` (source-filter
      pick), `"native_save"` (compile + reload + write-back), and the two
      dismiss events for a native pane's inline error banner
      (`"dismiss_native_error"` for the class-definition tab's read-only
      pane, `"dismiss_native_module_error"` for a standalone editable
      `:native` module tab).

  It also owns the **tab data model** the AC calls out — the open-tab list
  (`:tabs`), the focused id (`:active_tab`), and the compile-routing/dirty
  helpers over it (`init_tabs/1`, `find_tab/2`, `active_tab/1`,
  `activate_tab/2`, `close_tab/2`, `sync_active/2`, `open_definition/1,2`,
  `open_method_tab/4`, `open_new_method/3`, `breadcrumb/1`,
  `modifier_badges/1`, `tab_disk_key/1`, `clear_disk_differs/2`,
  `reload_reverted_def_buffers/2`, `refresh_after_source_change/1`) — the
  same data several *other*, not-yet-extracted or intentionally-elsewhere
  features still reach into: the System Browser's method/definition
  navigation (`browser_select_method`, `omni_open`, `nav_open`,
  `open_test_method`, `open_implementor_site`, all BT-3297 territory),
  Remove/Rename (`remove_method`, `remove_class`, the Rename modal), New
  Class, and the standalone Native-module browser tab
  (`browser_open_native_module`). Those stay in `WorkspaceLive` (none of
  their *events* are in this extraction's scope) and cross-call the public
  functions here by name — exactly the temporary cross-call shape a
  sequential decomposition produces, mirrored from `Dock`'s own cross-calls
  back into `WorkspaceLive` for the System Browser/Tests code not yet
  extracted.

  Every workspace read/write goes through `BtAttach.Facade.dispatch/3` (ADR
  0091 Decision 3) with `BtAttachWeb.Live.RequestContext` — never a raw
  `BtAttach.Workspace`/`:rpc` call — so this module never reimplements the
  `save`/`eval`/`browse_*` ops or the RBAC gates they ride (CLAUDE.md
  no-duplicate-implementations).

  State (`:tabs`, `:active_tab`, `:editor_rev`, `:doc_expanded`,
  `:native_view`, `:edit_class`, `:edit_selector`, `:edit_source`,
  `:edit_selection`, `:save_result`, `:save_error`, `:save_echo_pending`)
  stays on the LiveView's own socket — initialised in
  `WorkspaceLive.bind_session/3` and mount, same as the Dock/Inspector
  assigns. `WorkspaceLive` still owns `handle_event/3` (`Phoenix.LiveView`
  callback contracts) and `render/1` (the `#method-editor` panel is woven
  together with the still-resident Remove/Rename/Senders/Implementors/
  protocol-action UI, so it does not split cleanly along this extraction's
  event boundary — see `Dock`'s and `Inspector`'s moduledocs for the same
  call), but delegates every method-editor event to the functions here by
  name — see the `@method_editor_events` guard clause in `WorkspaceLive`.
  """

  use BtAttachWeb, :html

  alias BtAttach.Facade
  alias BtAttach.SessionRegistry
  alias BtAttach.Workspace
  alias BtAttachWeb.Live.FacadeError
  alias BtAttachWeb.Live.RequestContext
  alias BtAttachWeb.WorkspaceLive

  defp ctx(socket), do: RequestContext.build(socket)
  defp facade_error(reason), do: FacadeError.render(reason)

  # ── handle_event dispatch ────────────────────────────────────────────────
  #
  # `WorkspaceLive.handle_event/3` forwards every event whose name is in
  # `@method_editor_events` here unchanged (same event name, params, socket),
  # so each clause below is exactly the body the LiveView used to run
  # directly.

  # Expand/collapse the method-editor doc block (BT-2558). The signature stays
  # visible either way; this only reveals/hides the rendered `///` doc body so
  # it doesn't permanently occupy the top of the editor. Sticky across tab
  # switches — one preference, not per-method — so a user who wants docs open
  # keeps them open, and sticky across reconnects too (BT-2570): `terminate/2`
  # stashes this flag in the registry and `restore_doc/3` re-applies it on the
  # resuming mount, so a socket drop / redeploy / laptop wake no longer
  # re-collapses an expanded block.
  def handle_event("toggle_doc", _params, socket) do
    {:noreply, assign(socket, doc_expanded: !socket.assigns.doc_expanded)}
  end

  # Save (durably patch) the edited method on the workspace via the
  # write-surface. On success the method is compiled + flushed into the live
  # BEAM module and an entry is recorded in the workspace ChangeLog, so a
  # subsequent eval observes the new behaviour and the change appears in the
  # change-history pane. A failed compile/save returns a structured
  # #beamtalk_error{} we render as an actionable message (not a flattened
  # string).
  def handle_event(
        "save_method",
        %{"class" => class, "selector" => selector, "source" => source} = params,
        socket
      )
      when is_binary(class) and is_binary(selector) and is_binary(source) do
    # The active tab's id rides the form as a hidden field (BT-2494) so a
    # successful compile clears *that* tab's dirty dot and refreshes its base.
    # The historical save_method payload (the BT-2409 e2e) carries no tab id —
    # `params["tab"]` is then nil and the save still works, just without a tab
    # to reconcile.
    {:noreply, save_method(socket, class, selector, source, params["tab"])}
  end

  # Malformed payload (missing keys or non-binary values): never let a
  # crafted form event crash the LiveView — `save_method/5` calls
  # `String.trim/1`, which would raise on a non-binary. Surface a validation
  # error instead.
  def handle_event("save_method", _params, socket) do
    {:noreply, assign(socket, save_result: nil, save_error: "Invalid method form payload.")}
  end

  # Switch the focused editor tab. Pure view state — no workspace round-trip;
  # an id that no longer maps to an open tab is ignored rather than blanking
  # the editor. Switching also re-syncs the visible class/selector/source
  # assigns (which the save_method form reads) to the newly-active tab.
  def handle_event("tab_select", %{"id" => id}, socket) do
    {:noreply, activate_tab(socket, id)}
  end

  def handle_event("tab_select", _params, socket), do: {:noreply, socket}

  # Close a tab (the spike's × affordance). The strip may empty completely —
  # the editor then shows its empty state (no default tab is re-seeded).
  # Closing the active tab moves focus to the previous remaining tab, or
  # clears focus when it was the last one open.
  def handle_event("tab_close", %{"id" => id}, socket) do
    {:noreply, close_tab(socket, id)}
  end

  def handle_event("tab_close", _params, socket), do: {:noreply, socket}

  # Keyboard chord (Esc in the browser, ⌘W in the desktop shell) closing the
  # focused editor tab — same path as clicking the tab's ✕, including the
  # silent discard of a dirty tab. No-op when nothing is open. While an
  # Escape-dismissable surface is open (New Class modal, Rename modal,
  # Senders/Implementors popover, Settings dropdown), Escape means "dismiss
  # it", never "close the tab". The PRIMARY defence is client-side
  # (`claimedByWindowKeydown` in `keyboard_shortcuts.js`): those surfaces
  # mount a `phx-window-keydown` element only while open, and LiveView's
  # window listener fires before the hook's, so the same keystroke's dismiss
  # event can reach the server first and clear these assigns before this
  # handler runs — the guard below is a best-effort backstop (and the
  # testable contract) for any push that does arrive while a surface is
  # still open.
  def handle_event("tab_close_active", _params, socket) do
    %{assigns: assigns} = socket

    escape_claimed? =
      assigns.new_class_open or assigns.rename_open or assigns.show_settings or
        assigns.nav_popover != nil

    case assigns.active_tab do
      _ when escape_claimed? -> {:noreply, socket}
      nil -> {:noreply, socket}
      id -> {:noreply, close_tab(socket, id)}
    end
  end

  # Open a fresh class-definition tab (the spike's "+ def" affordance): a tab
  # whose source is a *class definition* rather than a method body, so saving
  # it compiles the class. The class name comes from the active tab (so "+
  # def" opens the definition of the class you're editing); a definition tab
  # already open for that class is re-focused rather than duplicated.
  def handle_event("open_definition", _params, socket) do
    {:noreply, open_definition(socket)}
  end

  # BT-2656/BT-2661: narrow the Native browser by source origin (project /
  # deps / stdlib / all), mirroring the class tree's `browser_source`. Pure
  # view state over the already-loaded rows — no workspace round-trip; an
  # unknown value is ignored. A deliberate pick marks the filter "chosen" so
  # the BT-2661 Project default (applied once the modules load) can never
  # override it.
  def handle_event("native_source", %{"src" => src}, socket)
      when src in ~w(all project deps stdlib) do
    {:noreply, assign(socket, native_source: src, native_source_chosen: true)}
  end

  def handle_event("native_source", _params, socket), do: {:noreply, socket}

  # BT-2667: dismiss the error inside an open native-module tab. The error
  # lives on the tab's cached `native_view`, so clear it there (a no-op if
  # the active tab is not a native tab or carries no error).
  def handle_event("dismiss_native_module_error", _params, socket) do
    {:noreply,
     update_active_tab(socket, fn
       %{kind: :native, native_view: %{} = nv} = tab ->
         %{tab | native_view: Map.put(nv, :error, nil)}

       tab ->
         tab
     end)}
  end

  # BT-2670: save a project-owned native (`.erl`) module from its editable
  # tab — compile the edited buffer, hot-reload the module, and write the
  # source to disk. The active tab must be an editable `:native` tab; the
  # source rides the form's hidden `source` field (the CodeMirror-mirrored
  # textarea, exactly like the method editor). A crafted save against a
  # non-native / read-only tab is a graceful no-op. The ⌘S chord submits
  # this form, same as the method editor.
  def handle_event("native_save", %{"source" => source}, socket) when is_binary(source) do
    {:noreply, save_native_source(socket, source)}
  end

  def handle_event("native_save", _params, socket), do: {:noreply, socket}

  # Track edits to the active tab so its dirty dot reflects unsaved changes
  # (BT-2494). The save_method form's `phx-change` reports the live source on
  # each keystroke; we stash it on the active tab and recompute its dirty
  # flag (source != the last-compiled base). Client-supplied, so a
  # non-binary / absent source is ignored rather than crashing.
  #
  # A new-method tab no longer carries a separate selector input (BT-2606):
  # the author writes the full method (signature + body) in the CodeMirror
  # body, and the selector is parsed from that body on save — so there is
  # nothing extra to mirror here. The breadcrumb derives its label live from
  # the same tracked source (`breadcrumb/1`).
  def handle_event("edit_source", %{"source" => source}, socket)
      when is_binary(source) do
    {:noreply, track_edit(socket, source)}
  end

  def handle_event("edit_source", _params, socket), do: {:noreply, socket}

  # Dismiss the error inside the live native-source pane: `@native_view` is a
  # map whose `:error` field carries the banner. Clear only that field so the
  # rest of the pane (content/clauses/meta) is preserved; if the pane is
  # closed (`native_view: nil`) this is a no-op.
  def handle_event("dismiss_native_error", _params, socket) do
    case socket.assigns[:native_view] do
      %{} = nv -> {:noreply, assign(socket, native_view: Map.put(nv, :error, nil))}
      _ -> {:noreply, socket}
    end
  end

  # Selection tracking (BT-2485, BT-2539): the method-editor CmEditor hook
  # reports the editor's current selection (text + offsets). We hold it in
  # `edit_selection` so a later pane can evaluate the selected expression
  # rather than the whole buffer (the spike's "evaluates selection" vs
  # "evaluates buffer" distinction). The payload is client-supplied, so
  # accept only the well-formed shape and ignore anything else rather than
  # crash the LiveView.
  def handle_event("select_source", %{"text" => text, "tab_id" => tab_id} = params, socket)
      when is_binary(text) do
    # Stale/in-flight selection guard (BT-2549): a departing CmEditor can
    # dispatch one final `select_source` via `pushEvent` just before its
    # `destroyed()` callback runs; that event can land *after* `sync_active/2`
    # cleared `:edit_selection` on the tab switch, re-populating it with
    # coordinates from the closed tab. The editor instance stamps each push
    # with the tab it edits (data-tab-id), so ignore any stamp that no longer
    # matches the active tab.
    if tab_id == socket.assigns.active_tab do
      selection = %{
        text: text,
        start: WorkspaceLive.clamp_offset(params["start"]),
        end: WorkspaceLive.clamp_offset(params["end"])
      }

      {:noreply, assign(socket, edit_selection: selection)}
    else
      {:noreply, socket}
    end
  end

  # Malformed payload (missing text, non-binary, or missing the "tab_id"
  # key): ignore rather than crash the LiveView (the payload is
  # client-supplied). A present-but-mismatched stamp (incl. `tab_id: null`)
  # is handled by the guarded clause above, which drops it when it doesn't
  # match the active tab.
  def handle_event("select_source", _params, socket), do: {:noreply, socket}

  # ── method editor helpers (Wave 3) ──────────────────────────────────────

  # Compile (⌘S) the active tab's source. A class-definition tab evals its
  # whole definition (compiling the class); a method tab drives the
  # write-surface `save` install chokepoint (compile + flush). The tab kind
  # is read from the open-tab list by id — NOT inferred from the payload —
  # so the historical method save_method payload (no tab id, the BT-2409
  # e2e) keeps its exact behaviour. On success the matching tab's dirty dot
  # clears and its base source is updated to the compiled text.
  defp save_method(socket, class, selector, source, tab_id) do
    case tab_id && find_tab(socket, tab_id) do
      %{kind: :def} = tab -> save_definition(socket, tab, source)
      _ -> save_method_body(socket, class, selector, source, tab_id)
    end
  end

  # Validate the edit form, then drive the write-surface save. Empty class or
  # an un-derivable selector is a local validation error (rendered without a
  # round-trip); a real save threads the body value straight to the
  # workspace install chokepoint.
  #
  # On a *new-method* tab there is no selector input anymore (BT-2606): the
  # author writes the full method (signature + body) in the body, so the
  # selector is parsed from the source signature here and the form's
  # (empty) `selector` field is ignored. An existing-method tab keeps the
  # breadcrumb selector that rode the hidden field. The derived selector
  # still passes through the `:save` op, where the compiler re-parses it
  # from the body and rejects any mismatch — so a parse that disagrees with
  # the compiler fails loudly rather than installing under the wrong key.
  defp save_method_body(socket, class, selector, source, tab_id) do
    class = String.trim(class)

    selector =
      case tab_id && find_tab(socket, tab_id) do
        %{new: true} -> parse_method_signature_selector(source)
        _ -> String.trim(selector)
      end

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
        assign(socket,
          save_result: nil,
          save_error: "Could not parse a method signature from the source."
        )

      true ->
        case Facade.dispatch(
               :save,
               %{class: class, selector: selector, source: source},
               ctx(socket)
             ) do
          {:ok, saved_class} ->
            # The patch is live + logged; refresh the change-history pane so
            # the new entry is visible (ChangeLog coherence). A successful
            # compile also clears the active tab's dirty dot and re-bases it
            # on the compiled source.
            socket
            |> assign(
              save_result: "Saved #{selector} on #{saved_class}",
              save_error: nil,
              flush_result: nil,
              flush_error: nil
            )
            |> compile_clean(tab_id, source)
            |> promote_new_method_tab(tab_id, saved_class, selector)
            |> WorkspaceLive.assign_changes()

          {:error, reason} ->
            # `reason` may be a facade RBAC denial (`:unauthorized`) for a
            # crafted event from a read-only role, or a workspace
            # #beamtalk_error{}.
            assign(socket, save_result: nil, save_error: facade_error(reason))
        end
    end
  end

  # Compile a class-definition tab (BT-2494) by evaluating its definition
  # source against the workspace — exactly the path the e2e tests use to
  # define a class, so "saving a class definition compiles the class" needs
  # no new server op (ADR 0082; the `eval` facade op). An empty body is a
  # local validation error; a compile failure renders the structured
  # `#beamtalk_error{}`.
  defp save_definition(socket, tab, source) do
    socket = assign(socket, edit_source: source)
    pid = socket.assigns[:session_pid]

    cond do
      String.trim(source) == "" ->
        assign(socket, save_result: nil, save_error: "Enter a class definition to compile.")

      not is_pid(pid) ->
        assign(socket, save_result: nil, save_error: "not attached to workspace")

      true ->
        save_definition_eval(socket, tab, source, pid)
    end
  end

  defp save_definition_eval(socket, tab, source, pid) do
    case Facade.dispatch(:eval, %{session_pid: pid, code: source}, ctx(socket)) do
      {:ok, _term, _output, _warnings} ->
        socket
        |> assign(
          save_result: "Compiled #{tab.class}",
          save_error: nil,
          flush_result: nil,
          flush_error: nil
        )
        |> compile_clean(tab.id, source)
        |> WorkspaceLive.assign_changes()
        # BT-2586/BT-2590: refresh the git panel when it is open AND
        # autoflush is on — only then did the save write through to disk.
        # With autoflush off the save patches the live image only, so the
        # on-disk working tree is unchanged and the git shell-out is skipped
        # (no redundant refresh).
        |> WorkspaceLive.maybe_refresh_git_after_save()

      {:error, reason, _output, _warnings} ->
        assign(socket, save_result: nil, save_error: Workspace.render_error(reason))

      {:error, reason} ->
        assign(socket, save_result: nil, save_error: facade_error(reason))
    end
  end

  # BT-2670: save (edit → compile → reload → write-back) the active native
  # tab's `.erl`. The save is only honoured for an editable `:native` tab —
  # a crafted event against a read-only / non-native tab is a graceful
  # no-op. The op may return a clean success, structured compile errors
  # (rendered inline via the shared `@save_error` notice), or an
  # authorization/dispatch error.
  defp save_native_source(socket, source) do
    case active_tab(socket.assigns) do
      %{kind: :native, native_view: %{editable: true}, class: module} = tab ->
        socket = assign(socket, edit_source: source)

        case Facade.dispatch(
               :save_native_source,
               %{module: module, source: source},
               ctx(socket)
             ) do
          {:value, %{"ok" => true}} ->
            # Compiled, reloaded, and written to disk. Clear the dirty dot,
            # re-base the tab on the saved source, and refresh the cached
            # native_view content so a later read shows the new source.
            socket
            |> assign(
              save_result: "Saved #{module}.erl",
              save_error: nil,
              flush_result: nil,
              flush_error: nil
            )
            |> compile_clean(tab.id, source)
            |> update_native_view_content(tab.id, source)

          {:value, %{"errors" => [_ | _] = errors}} ->
            assign(socket, save_result: nil, save_error: native_compile_error(errors))

          {:error, reason} ->
            assign(socket, save_result: nil, save_error: facade_error(reason))

          _other ->
            assign(socket, save_result: nil, save_error: "Could not save native source.")
        end

      _ ->
        socket
    end
  end

  # BT-2670: refresh the editable native tab's cached `native_view.content`
  # to the just-saved source so a re-render (or a Callers query) reflects
  # what is now on disk and in the image, without a round-trip back to
  # `browse-native-source`.
  defp update_native_view_content(socket, tab_id, source) do
    update_active_tab_by_id(socket, tab_id, fn
      %{kind: :native, native_view: %{} = nv} = tab ->
        %{tab | native_view: Map.put(nv, :content, source)}

      tab ->
        tab
    end)
  end

  # BT-2670: flatten the structured native compile-error maps (the same
  # shape the load path produces — `path`/`kind`/`message`/optional `line`)
  # into a single inline message, mirroring how a `.bt` compile error reads.
  # The first error's message + line leads; a count tail signals there are
  # more.
  defp native_compile_error(errors) do
    count = length(errors)
    first = List.first(errors)
    msg = Map.get(first, "message", "Erlang compilation failed")

    located =
      case Map.get(first, "line") do
        line when is_integer(line) -> "line #{line}: #{msg}"
        _ -> msg
      end

    if count > 1, do: "#{located} (+#{count - 1} more)", else: located
  end

  # The method's image-accurate source (`browse-method-source`) plus the
  # divergence flags the editor breadcrumb badges: `disk_differs` (unflushed
  # live patch) and `runtime_only` (sourceless runtime method). `source` is
  # "" for a sourceless method / error, so opening it gives an empty
  # editable buffer rather than crashing.
  defp method_source_info(socket, class, side, selector) do
    case Facade.dispatch(
           :browse_method_source,
           %{class: class, side: side, selector: selector},
           ctx(socket)
         ) do
      {:value, result} when is_map(result) ->
        %{
          source: if(is_binary(result["source"]), do: result["source"], else: ""),
          disk_differs: result["disk_differs"] == true,
          runtime_only: WorkspaceLive.runtime_only?(result),
          # BT-2714: a compiler-derived method (value accessors /
          # `with<Field>:` setters / actor `new`/`spawn`) has no editable
          # source — the backend returns `source: null` but resolves the
          # real doc/signature. The tab renders read-only (no CodeMirror),
          # showing that doc block instead of a blank editable buffer.
          synthetic: result["source_status"] == "synthetic",
          # BT-2558: the method's `///` doc-comment and signature, carried
          # so the editor can show a read-only doc block alongside the
          # editable body. `nil` when the method has no doc / no resolvable
          # signature.
          doc: doc_text(result["doc"]),
          signature: doc_text(result["signature"]),
          # BT-2578: a `self delegate` method (ADR 0056) on a native: class —
          # its real implementation lives in the backing module's
          # `handle_call` clauses, reachable via the "→ Erlang
          # implementation" jump.
          native_delegate: result["native_delegate"] == true
        }

      # Facade returned a value but not the expected map (sourceless /
      # malformed payload): open an empty editable buffer with no badges.
      {:value, _non_map} ->
        empty_source_info()

      # Facade error (class/method missing, dispatch failure): same
      # empty-buffer fallback, kept as its own arm so the error origin stays
      # distinguishable when debugging.
      {:error, _reason} ->
        empty_source_info()

      _ ->
        empty_source_info()
    end
  end

  # Defaults for a method with no resolvable image source: an empty editable
  # buffer and no divergence badges (and no doc block — BT-2558).
  defp empty_source_info,
    do: %{
      source: "",
      disk_differs: false,
      runtime_only: false,
      synthetic: false,
      doc: nil,
      signature: nil,
      native_delegate: false
    }

  # The on-disk method body to diff a later compile against (BT-2550 item
  # 2). We can only know it when the image matched disk at open:
  # `disk_differs: false` and not runtime-only means the backend confirmed
  # the image body appears verbatim in the on-disk class source, so the
  # image `source` *is* the on-disk body. A runtime-only or already-diverged
  # method has no body we can pin to disk → `nil`, and `compile_clean/3`
  # falls back to its conservative flag.
  defp disk_body_snapshot(%{runtime_only: true}), do: nil

  defp disk_body_snapshot(%{disk_differs: false, source: src}) when is_binary(src) and src != "",
    do: src

  defp disk_body_snapshot(_), do: nil

  # The on-disk body to carry forward when a *clean* tab is re-activated
  # (BT-2565). `disk_body_snapshot/1` only yields a body while the image
  # matches disk (`disk_differs: false`), so a tab whose image diverged from
  # disk via an in-memory compile — `compile_clean/3` set `disk_differs:
  # true` while preserving the body captured at open — would otherwise
  # re-derive `nil` on re-activation, regressing a later exact-on-disk-body
  # re-compile back to the conservative `unflushed` flag. We split the two
  # ways `disk_body_snapshot/1` returns nil:
  #
  #   * now runtime-only (no on-disk body at all) → drop to nil, matching
  #     the conservative fallback for a method that genuinely lost its disk
  #     source. This guards against a naive `existing.disk_source || …`
  #     retaining a stale snapshot for a method that legitimately
  #     transitioned to runtime-only.
  #   * still disk-backed but image-diverged → keep the prior
  #     `existing.disk_source` so the on-disk body stays pinned across the
  #     round-trip.
  #
  # A fresh snapshot (image back in sync with disk) always wins over the
  # carried one.
  #
  # The carried `existing.disk_source` is only as fresh as tab-open time: if
  # the file is rewritten out-of-band (another session flushes, an external
  # editor) *while the image is diverged*, the carried body goes stale, and
  # a later compile of the *old* on-disk body would clear `unflushed`
  # against disk that has since moved on — a narrow false-negative
  # (concurrent out-of-band writes during divergence, BT-2567). As of
  # BT-2567 the backend's `disk_differs` is a *live* re-read of the on-disk
  # file (not a load-time snapshot), so this self-corrects on the next
  # re-activation: `info.disk_differs` comes back `true` and the
  # `existing.disk_differs or info.disk_differs` merge re-raises the badge.
  # The residual window is only the transient between an out-of-band write
  # and the next re-activation — and only when the user re-compiles the
  # *old* on-disk body in that gap. The conservative pre-BT-2565 path
  # avoided even that by re-flagging *every* re-activated diverged tab — the
  # false-positive BT-2565 fixes. The common-case win is worth the residual.
  @doc false
  def reactivation_disk_source(_existing, %{runtime_only: true}), do: nil

  def reactivation_disk_source(existing, info),
    do: disk_body_snapshot(info) || existing.disk_source

  # Normalise a browse-payload doc/signature field to a non-empty binary or
  # nil. The op already returns `null` (decoded to nil) for absent fields;
  # this also drops a stray empty string so the editor never shows a blank
  # doc block.
  defp doc_text(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      _ -> value
    end
  end

  defp doc_text(_), do: nil

  # Label for the collapsible doc block's toggle (BT-2558, BT-2604).
  # Deliberately a short generic label rather than the method signature: the
  # signature already shows in the breadcrumb and as the first line of the
  # editable source below, so repeating it on the toggle just triples it. A
  # class-definition tab's doc *is* the class comment, hence the distinct
  # wording. Public: `WorkspaceLive`'s render template calls it directly.
  def doc_summary_label(%{kind: :def}), do: "Class comment"
  def doc_summary_label(_), do: "Documentation"

  # The class' editable definition skeleton (`browse-class-definition` →
  # `definition`, the synthesized `Super subclass: Name` header + state
  # slots) paired with its doc-block comment, fetched in one browse op.
  # Returns `{definition, comment, native_module, class_modifiers,
  # is_protocol}` where `definition` is a binary (`""` for a file-less
  # ClassBuilder class with no skeleton, so the editor body is always a
  # string) and `comment` is the rendered doc text or `nil` (the same
  # comment `Beamtalk help:` renders, so the browser and `help:` agree on a
  # class' docs). `{"", nil, nil, nil, false}` if the browse fails — the tab
  # then opens empty rather than erroring.
  defp class_definition_info(socket, class) do
    case Facade.dispatch(:browse_class_definition, %{class: class}, ctx(socket)) do
      {:value, %{} = result} ->
        definition =
          case Map.get(result, "definition") do
            text when is_binary(text) -> text
            _ -> ""
          end

        # BT-2639: `is_protocol` is a runtime-reflection boolean on the
        # class-definition row (op 4) — NOT a header string-sniff — so the
        # def-tab can reliably render the protocol action row.
        {definition, doc_text(Map.get(result, "comment")), native_backing_module(result),
         class_modifiers_from(result), Map.get(result, "is_protocol") == true}

      _ ->
        # Failure sentinel: `nil` modifiers (distinct from `[]`, a plain
        # class with no modifiers) so a transient fetch failure can be told
        # apart from a real empty result and the caller can keep the prior
        # badges (BT-2605 review). `is_protocol` defaults to `false` on
        # failure (no protocol action row).
        {"", nil, nil, nil, false}
    end
  end

  # BT-2605: the reflected class-level modifiers for the editor-header
  # badges, in a stable display order. These are *runtime reflection*
  # booleans from `browse-class-definition` (op 4) — not parsed from the
  # `definition` skeleton, which carries no leading modifier keywords. A
  # missing/false flag contributes no badge. (BT-2629: `typed` is now
  # reflected too — the already-emitted is_typed meta flag is threaded
  # through the runtime, mirroring sealed/abstract.)
  defp class_modifiers_from(result) do
    Enum.filter([:sealed, :typed, :abstract], &(Map.get(result, Atom.to_string(&1)) == true))
  end

  # BT-2578: the backing Erlang module name of a native: class (ADR 0056),
  # or nil for an ordinary class. Drives the "Erlang backend" badge + the
  # read-only native pane on a class-definition tab.
  defp native_backing_module(%{"native" => true} = result) do
    case Map.get(result, "backing_module") do
      mod when is_binary(mod) and mod != "" -> mod
      _ -> nil
    end
  end

  defp native_backing_module(_result), do: nil

  # Origin + package for a class, looked up from the loaded class-tree rows
  # (BT-2643 carries both on every row). Snapshotted onto a tab at open so
  # the editor header can badge the package even when the tree pane is
  # collapsed. Falls back to `{nil, nil}` when the class isn't in the loaded
  # rows (graceful: the header badge then renders nothing).
  defp class_origin_package(socket, class) do
    case Enum.find(socket.assigns[:browser_classes] || [], &(Map.get(&1, "name") == class)) do
      %{} = row -> {Map.get(row, "source_origin"), Map.get(row, "package")}
      _ -> {nil, nil}
    end
  end

  # ── tabbed method editor data model (BT-2494) ───────────────────────────
  #
  # A tab is a plain map; the open-tab list lives in `:tabs` and the focused
  # tab's id in `:active_tab`. The visible class/selector/source assigns
  # (which the save_method form binds) always mirror the active tab, so the
  # existing write-surface handler reads them unchanged.
  #
  #   %{
  #     id: stable string id (method-key, "def:<Class>", or "new:<Class>"),
  #     kind: :method | :def,
  #     class: "Counter",
  #     side: "instance" | "class",     # methods only
  #     selector: "increment",          # methods only ("" for an unsaved new method)
  #     source: live edit buffer,
  #     base: last-compiled source (dirty = source != base),
  #     dirty: boolean,
  #     disk_differs: boolean,   # methods only — unflushed live `>>` patch; snapshot at open, set on compile
  #     runtime_only: boolean,   # methods only — sourceless runtime method at open
  #     disk_source: binary | nil, # methods only — on-disk body captured at open (BT-2550); nil when unknown
  #     doc: binary | nil,       # BT-2558 read-only doc block: method `///` doc / class comment
  #     signature: binary | nil, # BT-2558 method signature (nil for a class-definition tab)
  #     is_protocol: boolean,    # BT-2639 def tabs only — gates the protocol action row

  #     class_modifiers: [:sealed | :typed | :abstract] | nil, # BT-2605 reflected class modifiers; nil = transient fetch failure (no badges)
  #     class_native: boolean,   # BT-2605 native: class flag, for the Native badge (all tab kinds)
  #     source_origin: "stdlib" | "dependency" | "project" | nil, # BT-2642 owning class origin, for the header package badge
  #     package: binary | nil,   # BT-2642 owning class package name, for the header package badge
  #     new: boolean             # an unsaved "new method" tab (selector input shown, not the breadcrumb)
  #   }
  #
  # The cockpit opens with NO tabs — the editor shows an empty state until
  # the user opens something (a method or class definition from the System
  # Browser). `:active_tab` is `nil` while the strip is empty; the render
  # guards on it and shows the empty-state panel instead of the editor form.
  # Edit-backing assigns stay `""` (set just before this in the assign
  # chain) until a tab is focused. Public: `WorkspaceLive.bind_session/3`
  # calls it directly at mount.
  def init_tabs(socket) do
    socket
    |> assign(:tabs, [])
    |> assign(:active_tab, nil)
    # BT-2655: a monotonically-bumped revision folded into the
    # method-editor overlay's element id. The CmEditor (CodeMirror) host is
    # `phx-update="ignore"` and seeds its doc from the hidden textarea only
    # on mount, so the only way to push a NEW body into the focused tab's
    # editor is to re-key the element so LiveView replaces it and the hook
    # remounts. Switching tabs already re-keys on `@active_tab`; an
    # *in-place* body re-read of the already-active tab (a git revert, a
    # flush/push reconcile) keeps the same `@active_tab`, so we bump this to
    # force the remount and surface the reverted source without a
    # close/reopen.
    |> assign(:editor_rev, 0)
  end

  # Public: `WorkspaceLive`'s still-resident System Browser / Native module
  # navigation (`open_native_module_tab`, BT-3297) looks up a tab by id
  # before deciding whether to (re)focus or create one, mirroring every
  # find-or-create path in this module.
  def find_tab(socket, id), do: Enum.find(socket.assigns.tabs, &(&1.id == id))

  # The focused tab, or `nil` when the strip is empty (startup, or after the
  # last tab is closed) — callers and the render guard on the nil. Takes the
  # bare `assigns` (not the socket) so the template can call it for the
  # breadcrumb / dirty-state; falls back to the first tab if a non-nil
  # active id somehow no longer maps. Public: called throughout
  # `WorkspaceLive`'s render template and by its still-resident
  # Remove/Rename/System-Browser code.
  def active_tab(%{tabs: tabs, active_tab: id}) do
    (id && Enum.find(tabs, &(&1.id == id))) || List.first(tabs)
  end

  # Focus a tab by id and mirror its class/selector/source into the
  # form-backing assigns. Clears any stale save/flush result so switching
  # tabs starts clean. Public: `WorkspaceLive`'s still-resident
  # `open_native_module_tab` (BT-3297) cross-calls it.
  def activate_tab(socket, id) do
    case find_tab(socket, id) do
      nil -> socket
      tab -> sync_active(assign(socket, :active_tab, id), tab)
    end
  end

  # Push the active tab's fields into the form-backing assigns so the
  # (single) save_method form always reflects the focused tab, then clear
  # any stale save/flush banners — appropriate when the *user* is switching
  # focus (a new tab's fields should start clean), but NOT when a push
  # refresh re-syncs the tab that's still focused (see `sync_active_fields/2`,
  # and `focus_tab_keep_banner/3` for the same "mid-save" carve-out). Public:
  # `WorkspaceLive`'s still-resident `open_native_module_tab` (BT-3297)
  # cross-calls it.
  def sync_active(socket, tab) do
    socket
    |> sync_active_fields(tab)
    |> assign(save_result: nil, save_error: nil, flush_result: nil, flush_error: nil)
  end

  # Mirror `tab`'s class/selector/source into the form-backing assigns,
  # without touching the save/flush banners. BT-2588: `resync_active_tab/2`
  # (a push refresh) and `focus_tab_keep_banner/3` (post-save tab promotion)
  # both need this — re-syncing the STILL-focused active tab after the
  # user's OWN save must not wipe the "Saved …" banner that save just set,
  # unlike an explicit tab switch (`sync_active/2`), which should start the
  # new tab clean.
  defp sync_active_fields(socket, tab) do
    assign(socket,
      edit_class: tab.class,
      edit_selector: tab.selector || "",
      edit_source: tab.source,
      # Drop the previous tab's selection: switching tabs remounts the
      # editor, so the old `{text, start, end}` no longer points at
      # anything live. Without this, a future consumer of `:edit_selection`
      # could act on stale coords from the tab the user just left.
      edit_selection: nil
    )
  end

  # Close a tab. The strip may go empty (the editor then shows its empty
  # state) — closing the active tab moves focus to the previous remaining
  # tab, or clears focus (`active_tab: nil`, edit assigns reset) when it was
  # the last one. Public: `WorkspaceLive`'s still-resident Remove/Rename
  # code cross-calls it after a successful remove/rename makes the open tab
  # stale.
  def close_tab(socket, id) do
    tabs = socket.assigns.tabs

    if not Enum.any?(tabs, &(&1.id == id)) do
      socket
    else
      idx = Enum.find_index(tabs, &(&1.id == id))
      remaining = List.delete_at(tabs, idx)
      socket = assign(socket, :tabs, remaining)

      cond do
        socket.assigns.active_tab != id ->
          socket

        remaining == [] ->
          clear_active(socket)

        true ->
          next = Enum.at(remaining, max(idx - 1, 0))
          sync_active(assign(socket, :active_tab, next.id), next)
      end
    end
  end

  # Drop focus to the empty state: no active tab, edit-backing assigns reset
  # to the same blanks `init_tabs/1` starts from, and any stale save/flush
  # result cleared so the next opened tab starts clean.
  defp clear_active(socket) do
    assign(socket,
      active_tab: nil,
      edit_class: "",
      edit_selector: "",
      edit_source: "",
      edit_selection: nil,
      save_result: nil,
      save_error: nil,
      flush_result: nil,
      flush_error: nil
    )
  end

  # Open (or re-focus) a class-definition tab for the active tab's class. A
  # def tab evals its definition source on compile (saving compiles the
  # class). On first open it also reads the class' comment (BT-2558) so the
  # editor can show it as a read-only documentation block above the
  # editable definition body.
  def open_definition(socket) do
    # The "+ def" affordance opens the *active* tab's class — a no-op when
    # the strip is empty (nothing focused to take a class from). The button
    # is hidden in that state, so this guard is just belt-and-braces
    # against a stale click.
    case active_tab(socket.assigns) do
      # A native (.erl) tab's `class` is a module name, not a Beamtalk
      # class — there is no class definition to open, so "+ def" is a no-op
      # (the button is also hidden for native tabs; this guards a stale
      # click).
      %{kind: :native} -> socket
      %{class: class} -> open_definition(socket, class)
      nil -> socket
    end
  end

  # Open (or re-focus) a class-definition tab for a named class — the
  # System Browser's "class definition" entry opens the *selected* class's
  # definition, which need not be the active tab's class. Public:
  # `WorkspaceLive`'s still-resident System Browser / Rename / New Class /
  # go-to-definition code cross-calls it.
  def open_definition(socket, class) do
    id = "def:" <> class

    case find_tab(socket, id) do
      %{} = existing ->
        # Parity with the method-tab re-activation path: refresh the
        # read-only doc block from the live image so an out-of-band class
        # comment change (MCP `save_class`, a `>>` patch) shows on
        # re-focus instead of the snapshot taken at first open. Only
        # `doc:` is touched — the editable definition buffer and its dirty
        # flag are left untouched, so an in-progress edit survives a tab
        # switch. The skeleton `definition` the browse also returns is
        # intentionally discarded here.
        {_definition, comment, native_module, class_modifiers, is_protocol} =
          class_definition_info(socket, class)

        # Keep the prior backing module if the re-fetch fails transiently
        # (workspace unreachable → `{"", nil, nil, nil, false}`): a `nil`
        # here would hide the "Erlang backend" badge + pane toggle on an
        # already-open tab while `@native_view` still holds the fetched
        # source. A successful re-fetch always wins (the class of a `def:`
        # tab does not change between activations, so a non-nil result is
        # the same module).
        resolved_native = native_module || existing.native_module

        refreshed = %{
          existing
          | doc: comment,
            is_protocol: is_protocol,
            native_module: resolved_native,
            # BT-2605: refresh the reflected modifier badges off the same
            # fetch; a `nil` result is the transient-failure sentinel, so
            # keep the prior list rather than clearing the badges (BT-2605
            # review).
            class_modifiers: class_modifiers || existing.class_modifiers,
            class_native: is_binary(resolved_native) and resolved_native != ""
        }

        socket
        |> update_active_tab_by_id(id, fn _ -> refreshed end)
        |> activate_tab(id)

      nil ->
        # Fetch the class skeleton (header + state slots) and its comment
        # in one browse op: the skeleton seeds the editable definition
        # body, the comment the read-only doc block. Without the skeleton
        # the editor opened empty — the doc block rendered but the class
        # definition itself was missing (BT-2558 only wired the comment).
        {definition, comment, native_module, class_modifiers, is_protocol} =
          class_definition_info(socket, class)

        # BT-2642: snapshot the class's origin + package for the header
        # badge.
        {source_origin, package} = class_origin_package(socket, class)

        tab = %{
          id: id,
          kind: :def,
          class: class,
          side: nil,
          selector: nil,
          # BT-2639: is this class a Protocol? Gates the protocol action
          # row (Required methods / Conforming classes) on the def tab.
          is_protocol: is_protocol,
          # BT-2642: package/origin badge for the editor header.
          source_origin: source_origin,
          package: package,
          source: definition,
          base: definition,
          dirty: false,
          disk_differs: false,
          runtime_only: false,
          # A :def tab has no single on-disk method body to diff against.
          disk_source: nil,
          # The class comment as the doc block; no per-method signature on
          # a class-definition tab.
          doc: comment,
          signature: nil,
          # BT-2578: the backing Erlang module (native: classes only), nil
          # otherwise — gates the "Erlang backend" badge + read-only
          # native pane.
          native_module: native_module,
          # BT-2605: reflected class modifiers (sealed/abstract) + native
          # flag for the header modifier badges. `class_native` is kept
          # distinct from `native_module` so the badge can also show on
          # method tabs of the class.
          class_modifiers: class_modifiers,
          class_native: is_binary(native_module) and native_module != "",
          new: false
        }

        socket
        |> assign(:tabs, socket.assigns.tabs ++ [tab])
        |> assign(:active_tab, id)
        |> sync_active(tab)
    end
  end

  # Open (or re-focus) an *editable* method tab for class/side/selector — the
  # shared open path for an omni-search selector result and a
  # senders/implementors site. Mirrors `open_definition/1`'s
  # find-or-create-then-focus shape; the tab id is the same
  # `method:Class:side:selector` key the editor already uses, so opening
  # the same method twice de-dupes. The buffer is seeded with the method's
  # image-accurate source so editing starts from the live body. Public:
  # `WorkspaceLive`'s still-resident System Browser / omni-search /
  # senders-implementors / Tests-pane navigation cross-calls it.
  def open_method_tab(socket, class, side, selector) do
    id = "method:" <> class <> ":" <> side <> ":" <> selector

    case find_tab(socket, id) do
      %{dirty: true} ->
        # Unsaved edits live in the buffer — just refocus; never re-seed
        # source or clobber the user's in-progress work.
        activate_tab(socket, id)

      %{} = existing ->
        # Tab already open and clean: re-fetch the live image so the
        # breadcrumb badges reflect an out-of-band image patch (e.g. an
        # image compile / MCP `save_method`) that landed while the tab sat
        # open, instead of the snapshot taken at first open. On an
        # empty-source fallback — a transient facade error or a
        # since-deleted method — keep the existing buffer rather than
        # blanking a tab the user is looking at (the precondition is a
        # clean tab, so nothing typed is lost, but the visible source
        # should not silently vanish).
        case method_source_info(socket, class, side, selector) do
          %{source: ""} ->
            activate_tab(socket, id)

          info ->
            refreshed = %{
              existing
              | source: info.source,
                base: info.source,
                # Pick up *new* divergence (false → true) from an
                # out-of-band patch, but never clear a divergence already
                # set locally by an in-memory compile (`compile_clean/3`):
                # clearing on flush is BT-2545's path. Keeps the false →
                # true invariant the `nil ->` branch documents.
                disk_differs: existing.disk_differs or info.disk_differs,
                runtime_only: info.runtime_only,
                # BT-2714: re-derive derived-ness from the live image too,
                # so a tab that (out-of-band) became / stopped being
                # synthetic tracks it.
                synthetic: info.synthetic,
                # Carry the on-disk body forward across the re-activation.
                # A fresh snapshot wins when the image is back in sync
                # with disk; a method whose image diverged but is *still*
                # disk-backed keeps the prior snapshot instead of
                # regressing to nil (BT-2565); a now runtime-only method
                # drops to nil (BT-2550).
                disk_source: reactivation_disk_source(existing, info),
                # Re-read the doc block from the live image too (BT-2558),
                # so an out-of-band edit to the method's `///` doc /
                # signature is reflected when the clean tab is
                # re-activated.
                doc: info.doc,
                signature: info.signature,
                # Re-derive the native-delegate flag from the live image
                # too.
                native_delegate: info.native_delegate
            }

            socket
            |> update_active_tab_by_id(id, fn _ -> refreshed end)
            |> assign(:active_tab, id)
            |> sync_active(refreshed)
        end

      nil ->
        info = method_source_info(socket, class, side, selector)
        # BT-2605: fetch the owning class once so the editor can badge the
        # class-level modifiers (sealed/abstract/native) alongside the
        # method.
        {_def, _comment, class_native_module, class_modifiers, _is_protocol} =
          class_definition_info(socket, class)

        # BT-2642: snapshot the owning class's origin + package from the
        # loaded tree rows so the editor header can badge the package for
        # this tab.
        {source_origin, package} = class_origin_package(socket, class)

        tab = %{
          id: id,
          kind: :method,
          class: class,
          side: side,
          selector: selector,
          # BT-2642: package/origin badge for the editor header.
          source_origin: source_origin,
          package: package,
          source: info.source,
          base: info.source,
          dirty: false,
          # Image-divergence snapshot at browse time (the badges the old
          # read-only pane carried): `disk_differs` = an unflushed live
          # `>>` patch, `runtime_only` = no static source on disk.
          # `disk_differs` is later set to `true` by `compile_clean/3` on
          # an in-memory compile, and both flags are re-derived from the
          # live image when a clean tab is re-activated (see the `%{} =
          # existing` branch above).
          disk_differs: info.disk_differs,
          runtime_only: info.runtime_only,
          # BT-2714: a compiler-derived method — renders read-only (no
          # editable CodeMirror), showing the resolved doc block instead
          # of a blank buffer.
          synthetic: info.synthetic,
          # The on-disk body captured while the image matched disk, so a
          # later compile diffs against it instead of flagging every
          # re-save (BT-2550).
          disk_source: disk_body_snapshot(info),
          # The method's `///` doc-comment + signature for the read-only
          # doc block (BT-2558); nil when the method carries no doc /
          # signature.
          doc: info.doc,
          signature: info.signature,
          # BT-2578: native backing module is a class-level fact, never set
          # on a method tab; `native_delegate` marks a `self delegate`
          # method whose implementation lives in the backing module (the
          # jump affordance).
          native_module: nil,
          native_delegate: info.native_delegate,
          # BT-2605: class-level modifier badges for the header
          # (sealed/abstract/native), reflected off the owning class.
          class_modifiers: class_modifiers,
          class_native: is_binary(class_native_module) and class_native_module != "",
          new: false
        }

        socket
        |> assign(:tabs, socket.assigns.tabs ++ [tab])
        |> assign(:active_tab, id)
        |> sync_active(tab)
    end
  end

  # Open (or re-focus) a blank "new method" tab for a class on `side`
  # ("instance"/"class"): a `:method` tab whose selector is not yet chosen
  # (`selector: ""`, `new: true`). The author fills the selector (via the
  # new-method-only selector input) and types the body; saving drives the
  # same write-surface `save` as any method. One blank new-method tab per
  # (class, side) — re-clicking re-focuses rather than stacking empties.
  # Public: `WorkspaceLive`'s still-resident `new_method` handler
  # cross-calls it.
  def open_new_method(socket, class, side) do
    id = "new:" <> class <> ":" <> side

    case find_tab(socket, id) do
      %{} -> activate_tab(socket, id)
      nil -> add_new_method_tab(socket, id, class, side)
    end
  end

  defp add_new_method_tab(socket, id, class, side) do
    # BT-2605: the class already exists, so fetch it to badge the
    # class-level modifiers (sealed/abstract/native) on the new-method tab.
    {_def, _comment, native_module, class_modifiers, _is_protocol} =
      class_definition_info(socket, class)

    # BT-2642: snapshot the class's origin + package for the header badge.
    {source_origin, package} = class_origin_package(socket, class)

    tab = %{
      id: id,
      kind: :method,
      class: class,
      side: side,
      selector: "",
      # BT-2642: package/origin badge for the editor header.
      source_origin: source_origin,
      package: package,
      source: "",
      base: "",
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      # BT-2578: a blank new-method tab is never a native delegate (no
      # selector yet); the keys must still be present so the editor
      # render's dot-access holds for every :method tab. `native_module`
      # stays nil on method tabs (it gates the :def-only native pane); the
      # header's Native *badge* reads the separate `class_native` flag
      # below.
      native_module: nil,
      native_delegate: false,
      # BT-2605: class-level modifier badges (sealed/abstract/native) for
      # the header — class-level facts, so they show on method tabs of the
      # class too.
      class_modifiers: class_modifiers,
      class_native: is_binary(native_module) and native_module != "",
      # The new-method marker: drives the selector input + breadcrumb
      # label, and keeps the tab id stable (no selector to key on yet).
      new: true
    }

    socket
    |> assign(:tabs, socket.assigns.tabs ++ [tab])
    |> assign(:active_tab, id)
    |> sync_active(tab)
  end

  # Record a keystroke edit on the active tab and recompute its dirty flag
  # (source != last-compiled base). We deliberately do NOT re-assign
  # `:edit_source` here: it is only the *initial* value CodeMirror reads
  # when the CmEditor hook mounts (the hidden `source` textarea is
  # `phx-update="ignore"`, so the editor owns the live text and a server
  # echo can't reach it anyway). The tab's `source` is the truth the next
  # compile / tab-switch reads; `:edit_source` is re-synced when a tab is
  # (re)focused — the element is re-keyed on `@active_tab`, so a fresh
  # mount picks it up.
  defp track_edit(socket, source) do
    update_active_tab(socket, fn tab -> %{tab | source: source, dirty: source != tab.base} end)
  end

  # After a successful compile, clear the saved tab's dirty dot and re-base
  # it on the compiled source. `tab_id` is nil for the historical no-tab
  # save payload (or `""` from the empty-state hidden form) — neither
  # matches an open tab, so there's nothing to reconcile and the update is
  # a harmless no-op.
  #
  # A `:method` save is an in-memory live `>>` patch (logged to the
  # ChangeLog) that is *not* flushed to disk, so a successful compile *may*
  # diverge the live body from its on-disk counterpart: set `disk_differs`
  # so the `unflushed` breadcrumb badge appears. (Clearing it again on
  # flush needs a `flush_completed` subscription — tracked as BT-2545.) A
  # `:def` tab evaluates a whole class definition with no single on-disk
  # method body to diverge, so leave its snapshot untouched.
  #
  # We only badge a *real* divergence (BT-2550): `save_method` logs a
  # ChangeLog entry for every `>>`, so flipping `disk_differs`
  # unconditionally false-flags a byte-for-byte re-save of the on-disk body
  # (e.g. ⌘S with no edit). When we know the on-disk body (`disk_source`,
  # captured at open while the image matched disk), compare the compiled
  # source against it; only differ when they actually differ. With no
  # known disk body (`nil` — a runtime-only or already-diverged method) we
  # conservatively flag, matching the prior behaviour.
  defp compile_clean(socket, nil, _source), do: socket
  # `""` is the empty-state hidden form's `tab=""` sentinel (no tab to
  # re-base); short-circuit to a no-op rather than try to look up a tab by
  # an empty id.
  defp compile_clean(socket, "", _source), do: socket

  defp compile_clean(socket, tab_id, source) do
    socket
    |> update_active_tab_by_id(tab_id, fn tab ->
      %{
        tab
        | source: source,
          base: source,
          dirty: false,
          disk_differs: compiled_disk_differs(tab, source)
      }
    end)
    # BT-2588: this tab was just cleanly compiled — the class-reload
    # notification this save itself triggers will arrive back as a
    # coalesced `:do_source_refresh` push shortly after (BT-2600). Arm the
    # flag `resync_active_tab/2` consumes to recognise THAT SPECIFIC push as
    # its own echo (keep the banner) rather than a later, genuinely
    # external change (clear it) — see the flag's definition for why
    # comparing the tab's re-read body isn't a reliable enough signal on
    # its own (it's blind for `:def` tabs, whose editable body a push
    # refresh never re-reads at all).
    |> assign(:save_echo_pending, true)
  end

  # Whether a just-compiled `:method` body diverges from its on-disk
  # counterpart. `disk_source` is the on-disk body captured at open (see
  # `disk_body_snapshot/1`) — a precise body-to-body comparison, so an
  # identical re-save reads as "in sync". `nil` (no known disk body) falls
  # back to a conservative `true`. A `:def` tab has no single method body to
  # diverge, so its snapshot is left untouched.
  defp compiled_disk_differs(%{kind: :method, disk_source: disk}, source) when is_binary(disk),
    do: source != disk

  defp compiled_disk_differs(%{kind: :method}, _source), do: true
  defp compiled_disk_differs(%{disk_differs: existing}, _source), do: existing

  # After a successful save *from a new-method tab*, promote it to an
  # ordinary method tab: stamp the author-supplied `selector`, flip `new:
  # false`, and re-key the tab id to the canonical
  # `method:<class>:<side>:<selector>` so it matches what `open_method_tab`
  # would create. Without this the tab keeps `selector: ""`, so
  # `sync_active` re-seeds the selector input to "" on the post-save
  # re-render (wiping what the author typed) and a second ⌘S trips the
  # empty-selector guard; it would also leave a stale `new:<class>` tab
  # alongside a later real method tab for the same selector. A no-op for
  # ordinary method/def tabs (the guard only matches `new: true`). When a
  # canonical method tab for the selector is already open, the scratch tab
  # is dropped and that existing tab is focused (no duplicate, no stale
  # "Class ▸ (new method)" left behind). `focus_tab_keep_banner/3`
  # refreshes the edit assigns so the now-hidden selector reflects the
  # saved name.
  defp promote_new_method_tab(socket, tab_id, saved_class, selector) do
    case find_tab(socket, tab_id) do
      %{new: true, side: side, source: source} = tab ->
        # Key the id off `saved_class` — the class the Facade reports the
        # method was actually compiled onto — not the form-submitted
        # class, so the id always names the class behind it (a crafted
        # event with a mismatched `class` input can't desync the id from
        # the compiled class).
        new_id = "method:" <> saved_class <> ":" <> side <> ":" <> selector
        tabs = socket.assigns.tabs

        case find_tab(socket, new_id) do
          nil ->
            # Re-key the scratch tab in place into the canonical method
            # tab.
            promoted = %{tab | id: new_id, selector: selector, new: false}
            replaced = Enum.map(tabs, fn t -> if t.id == tab_id, do: promoted, else: t end)
            focus_tab_keep_banner(socket, replaced, promoted)

          existing ->
            # A canonical method tab for this selector is already open:
            # drop the redundant scratch tab and focus the existing one,
            # rather than leaving a stale "Class ▸ (new method)" tab
            # alongside it. Re-base that tab on the body just compiled
            # (same post-compile treatment `compile_clean/3` gives the
            # in-place path) so the editor shows the saved source, not its
            # browse snapshot.
            rebased = %{
              existing
              | source: source,
                base: source,
                dirty: false,
                disk_differs: compiled_disk_differs(existing, source)
            }

            replaced =
              tabs
              |> Enum.reject(&(&1.id == tab_id))
              |> Enum.map(fn t -> if t.id == new_id, do: rebased, else: t end)

            focus_tab_keep_banner(socket, replaced, rebased)
        end

      _ ->
        socket
    end
  end

  # Focus `tab` and mirror it into the edit assigns *without* clearing the
  # save/flush result banners (`sync_active_fields/2` — see BT-2588).
  # `sync_active/2` resets those (the "switching tabs starts clean" rule),
  # which is wrong mid-save: promotion runs as part of a successful save
  # and must keep the "Saved …" banner it just set.
  defp focus_tab_keep_banner(socket, tabs, tab) do
    socket
    |> assign(:tabs, tabs)
    |> assign(:active_tab, tab.id)
    |> sync_active_fields(tab)
  end

  defp update_active_tab(socket, fun),
    do: update_active_tab_by_id(socket, socket.assigns.active_tab, fun)

  defp update_active_tab_by_id(socket, id, fun) do
    tabs = Enum.map(socket.assigns.tabs, fn t -> if t.id == id, do: fun.(t), else: t end)
    assign(socket, :tabs, tabs)
  end

  # The Class › side › selector breadcrumb label parts for the active tab.
  # Public: `WorkspaceLive`'s render template calls it directly.
  def breadcrumb(%{kind: :def, class: class}), do: {class, nil, "class definition"}
  # A new-method tab has no stored selector yet (BT-2606): derive it live
  # from the body the author is typing so the breadcrumb names the method
  # as soon as a recognizable signature appears, falling back to the "(new
  # method)" placeholder until then (parenthesised so it can't be mistaken
  # for a real selector — BT-2613).
  def breadcrumb(%{new: true, class: class, side: side, source: source}) do
    case parse_method_signature_selector(source) do
      "" -> {class, side, "(new method)"}
      selector -> {class, side, selector}
    end
  end

  def breadcrumb(%{class: class, side: side, selector: selector}), do: {class, side, selector}

  # BT-2605: the colored modifier badges shown in the editor header for the
  # active tab. Each entry is `%{label, class, title}` — `label` is the
  # visible text, `class` the CSS modifier class (`.modifier-tag.<class>`),
  # `title` the accessible tooltip naming the modifier. Returns `[]` when
  # the tab carries no modifiers (an instance-side method on a plain class
  # shows nothing). Order is stable: side (Class) first, then class
  # modifiers (sealed/abstract), then native — mirroring the apidocs'
  # labels (Sealed/Abstract) for docs↔IDE parity. Public: `WorkspaceLive`'s
  # render template calls it directly.
  def modifier_badges(tab) do
    side_badge(tab) ++ class_modifier_badges(tab) ++ native_badge(tab)
  end

  # A class-side method gets a distinct "Class" badge; instance-side
  # methods and class-definition tabs get none here. `:def` tabs have
  # `side: nil`.
  defp side_badge(%{side: "class"}) do
    [%{label: "Class", class: "side", title: "Class-side method"}]
  end

  defp side_badge(_tab), do: []

  # The reflected class-level modifiers (`sealed`, `abstract`) cached on the
  # tab — present on every tab kind (BT-2605 threads it onto method tabs
  # too). Labels are capitalized to match the apidocs.
  defp class_modifier_badges(%{class_modifiers: mods}) when is_list(mods) do
    Enum.map(mods, &class_modifier_badge/1)
  end

  # No modifier list (key absent, or `nil` from a transient fetch failure)
  # → no class-modifier badges.
  defp class_modifier_badges(_tab), do: []

  # The canonical class-modifier badge labels/colors, keyed by the
  # reflected modifier atom. Kept exhaustive against `class_modifiers_from/1`'s
  # atom set so a future modifier is a compile-visible addition in both
  # places.
  defp class_modifier_badge(:sealed),
    do: %{label: "Sealed", class: "sealed", title: "Cannot be subclassed by user code"}

  defp class_modifier_badge(:typed),
    do: %{
      label: "Typed",
      class: "typed",
      title: "All fields and methods require type annotations"
    }

  defp class_modifier_badge(:abstract),
    do: %{
      label: "Abstract",
      class: "abstract",
      title: "Must be subclassed; not directly instantiable"
    }

  # A `native:` class (ADR 0056) gets a Native badge. The flag is
  # `class_native` (set on every tab kind) rather than `native_module` so
  # the badge shows on method tabs too, while `native_module` stays
  # reserved for the :def-only native pane.
  defp native_badge(%{class_native: true}) do
    [%{label: "Native", class: "native", title: "Backed by an Erlang module (native: class)"}]
  end

  defp native_badge(_tab), do: []

  # Parse the selector from a method's source signature (BT-2606), returning
  # `""` when no valid signature is present yet. The author writes the full
  # method (`selector ... => body`) in the editor body; this recovers the
  # selector the same way the compiler's method header grammar does, so the
  # new-method tab no longer needs a separate selector input.
  #
  # It is intentionally conservative: a string it can't confidently read as
  # a header returns `""` (treated as "no signature yet"). The authoritative
  # parse still runs server-side in the `:save` op, which rejects a selector
  # that disagrees with the compiled body — so this client-side read only
  # has to be right for the breadcrumb hint and the pre-flight validation,
  # never the install key of record.
  defp parse_method_signature_selector(source) when is_binary(source) do
    source
    |> strip_leading_comments()
    |> strip_method_modifiers()
    |> selector_from_header()
  end

  defp parse_method_signature_selector(_source), do: ""

  # Drop leading blank lines and whole-line `//` / `///` comments, returning
  # the first line that begins a real method header (mirrors the backend
  # `skip_leading_comments/1`).
  defp strip_leading_comments(source) do
    trimmed = String.trim_leading(source)

    if String.starts_with?(trimmed, "//") do
      case String.split(trimmed, "\n", parts: 2) do
        [_comment, rest] -> strip_leading_comments(rest)
        [_comment] -> ""
      end
    else
      trimmed
    end
  end

  # Drop leading method modifiers (`class`, `internal`, `sealed`) that may
  # precede the selector, matching the parser's `parse_method_definition`
  # modifier loop. A modifier word is only stripped when more header text
  # follows it — a bare `class =>` is a method *named* `class`, not the
  # `class` modifier.
  defp strip_method_modifiers(header) do
    case String.split(header, ~r/\s+/, parts: 2) do
      [word, rest] when word in ["class", "internal", "sealed"] ->
        # `class`/`internal`/`sealed` are modifiers only when a selector
        # follows.
        cond do
          # The next token opens the body (`=>`): the word itself is the
          # (unary) selector — a method *named* `class`/`internal`/`sealed`.
          String.starts_with?(rest, "=>") ->
            header

          # The next token is `->`. This is ambiguous without type context
          # (BT-2625): `sealed -> Type =>` is a unary method named `sealed`
          # with a return type, while `sealed -> arg =>` is a `sealed`
          # *binary* method whose selector is `->`. Disambiguate by the
          # token after `->`, matching the parser's grammar: a Capitalized
          # token reads as a return Type (the modifier word is the unary
          # selector, so keep the header), a lowercase token reads as a
          # binary-selector argument (strip the modifier so `->` becomes
          # the selector).
          String.starts_with?(rest, "->") ->
            if return_type_follows_arrow?(rest), do: header, else: strip_method_modifiers(rest)

          true ->
            strip_method_modifiers(rest)
        end

      _ ->
        header
    end
  end

  # Decide whether the text starting at a `->` is a *return-type* annotation
  # (`-> Type`) rather than a binary selector whose argument follows (`->
  # arg`). A return type is a Capitalized identifier (a Type name); a
  # binary-selector parameter is a lowercase identifier. Matches the
  # compiler's method-header grammar (BT-2625). A `->` with no following
  # identifier is treated as a binary selector (not a return type).
  defp return_type_follows_arrow?(rest) do
    rest
    |> String.replace_prefix("->", "")
    |> String.trim_leading()
    |> case do
      <<first::utf8, _::binary>> -> first in ?A..?Z
      _ -> false
    end
  end

  # Recover the selector token(s) from the start of a method header: a
  # keyword selector (`at:put:` from `at: i put: v => …`), a binary
  # selector (`+`, `->`, `>>`, … from `+ other => …`), or a unary selector
  # (`increment` from `increment => …`). Returns `""` when the head doesn't
  # read as a header (no `=>` before the first statement break, or an
  # empty/blank head).
  defp selector_from_header(""), do: ""

  defp selector_from_header(header) do
    # Only the header up to the body arrow can contribute a selector — bail
    # when there is no `=>` before the first newline.
    head =
      header
      |> String.split(~r/\r?\n/, parts: 2)
      |> List.first()

    cond do
      not String.contains?(head, "=>") ->
        ""

      true ->
        head
        |> String.split("=>", parts: 2)
        |> List.first()
        |> selector_from_signature_text()
    end
  end

  # Extract the selector from the text *before* the `=>` arrow. Keyword
  # selectors are the concatenation of their `keyword:` parts (parameter
  # names, type annotations and the return type are dropped); a binary
  # selector is its leading operator; a unary selector is its single
  # identifier.
  defp selector_from_signature_text(sig) do
    sig = String.trim(sig)

    # `:(?!:)` so a compact type annotation (`i::Integer`) doesn't capture
    # `i:` as a keyword part — only a real `keyword:` (single colon)
    # counts.
    keyword_parts = Regex.scan(~r/([A-Za-z_][A-Za-z0-9_]*):(?!:)/, sig, capture: :all_but_first)

    cond do
      sig == "" ->
        ""

      keyword_parts != [] ->
        keyword_parts |> Enum.map(fn [k] -> k <> ":" end) |> Enum.join()

      true ->
        case Regex.run(~r/^([A-Za-z_][A-Za-z0-9_]*)/, sig) do
          [_, ident] ->
            ident

          nil ->
            # Binary selector: the leading run of operator characters (e.g.
            # `+`, `->`, `>>`, `<=`). Anything else is not a recognizable
            # header.
            case Regex.run(~r/^([-+*\/~<>=&|@%^?]+)/, sig) do
              [_, op] -> op
              nil -> ""
            end
        end
    end
  end

  # BT-2588: the `id="method-editor-form"` hook wiring, shared verbatim
  # across both cond branches that can render that id (the active-tab
  # editor form and the no-tab empty-state form) — and by
  # `id="native-editor-form"`, which binds the identical chord. Splatted
  # via `{method_editor_shortcuts_attrs()}` so these renders cannot drift
  # apart again — the original bug was exactly that drift: the empty-state
  # form lacked `phx-hook`/`data-scope`, so opening a tab from that state
  # added `phx-hook` to an ALREADY-mounted DOM node (same id ⇒ LiveView's
  # morphdom patch treats it as an "updated" node, not an "added" one) and
  # `KeyboardShortcuts#mounted()` — which attaches the window keydown
  # listener — never ran.
  #
  # The empty-state form must still mount the hook (so a later transition
  # to the active-tab form is an "updated" patch on an already-mounted
  # hook, not another missed "added" one) but must NOT bind "mod+s" to
  # "submit" there — otherwise ⌘S anywhere on the page while no tab is
  # open (e.g. while typing in the Workspace eval pane) request-submits the
  # hidden empty form and surfaces a spurious "Enter a class name to save a
  # method." error. Callers pass `%{}` for that form; the active-tab forms
  # use the default. Public: `WorkspaceLive`'s render template calls it
  # directly.
  def method_editor_shortcuts_attrs(shortcuts \\ %{"mod+s" => "submit"}) do
    %{
      "phx-hook" => "KeyboardShortcuts",
      "data-scope" => "window",
      "data-shortcuts" => Jason.encode!(shortcuts)
    }
  end

  # BT-2714: whether the *active editor tab* was opened for a
  # compiler-derived method (its `browse-method-source` came back
  # `source_status: synthetic`). Such a tab has no editable body — it
  # renders read-only (the doc block + a "compiler-derived" note) instead
  # of a blank CodeMirror. Bracket-style pattern match so the many
  # non-method tab kinds (class definition, native module) that never set
  # the key read as `false` rather than raising a KeyError. Public:
  # `WorkspaceLive`'s render template calls it directly.
  def synthetic_tab?(%{synthetic: true}), do: true
  def synthetic_tab?(_), do: false

  # BT-2642: editor-header package/origin badge for the active tab. The tab
  # carries `source_origin` ("stdlib" | "dependency" | "project" | nil) and
  # `package`, snapshotted at open from the class's browse row. The header
  # badge reuses BT-2641's vocabulary (`WorkspaceLive.dependency_badge_label/1`,
  # shared with the System Browser tree) and extends it to project (which
  # the tree hides but the header shows): stdlib → "STDLIB", dependency →
  # "DEP · <pkg>" (or bare "DEP"), project → the bare project package name.
  # Returns "" when origin is unknown or a project tab carries no package,
  # so the badge simply does not render. Public: `WorkspaceLive`'s render
  # template calls it directly.
  def header_package_label(%{source_origin: "stdlib"}), do: "STDLIB"

  def header_package_label(%{source_origin: "dependency"} = tab),
    do: WorkspaceLive.dependency_badge_label(stringify_origin(tab))

  def header_package_label(%{source_origin: "project", package: pkg})
      when is_binary(pkg) and pkg != "",
      do: pkg

  def header_package_label(_tab), do: ""

  # The CSS modifier class keying the header badge color, per origin.
  # Mirrors `WorkspaceLive`'s `source_origin_class/1` but reads the tab's
  # atom-keyed `source_origin`. Public: `WorkspaceLive`'s render template
  # calls it directly.
  def header_origin_class(%{source_origin: "stdlib"}), do: "stdlib"
  def header_origin_class(%{source_origin: "dependency"}), do: "dependency"
  def header_origin_class(_tab), do: "project"

  # The tooltip spelling out the origin for the header badge. Project gets
  # a "Project: <pkg>" title (BT-2642 extends the bare "Project" the tree
  # title used). Public: `WorkspaceLive`'s render template calls it
  # directly.
  def header_origin_title(%{source_origin: "stdlib"}), do: "Standard library"

  def header_origin_title(%{source_origin: "dependency"} = tab),
    do: "Dependency: #{WorkspaceLive.package_name(stringify_origin(tab))}"

  def header_origin_title(%{source_origin: "project", package: pkg})
      when is_binary(pkg) and pkg != "",
      do: "Project: #{pkg}"

  def header_origin_title(_tab), do: "Project"

  # Bridge the tab's atom-keyed origin/package onto the string-keyed map the
  # BT-2641 browse-row helpers (`WorkspaceLive.dependency_badge_label/1`,
  # `WorkspaceLive.package_name/1`) expect, so the dependency badge text
  # stays in one place.
  defp stringify_origin(tab),
    do: %{"source_origin" => tab[:source_origin], "package" => tab[:package]}

  # The disk key a tab is cleared by: `{class, selector}` for a `:method`
  # tab, `{class, :def}` for a `:def` tab (the `:def` sentinel can't
  # collide with a real binary selector). Any other shape yields `nil`,
  # which is never a set member. Public: `BtAttachWeb.Live.Dock`'s
  # `reloaded_tab_keys/2` (BT-3295, the git revert path) calls it directly
  # rather than keeping its own copy — a tab shape change must not risk the
  # two silently diverging.
  def tab_disk_key(%{kind: :method, class: class, selector: selector}), do: {class, selector}
  def tab_disk_key(%{kind: :def, class: class}), do: {class, :def}
  def tab_disk_key(_tab), do: nil

  # Clear the `unflushed` (`disk_differs`) badge on every open tab whose
  # disk key is in `flushed`: a `:method` tab keys on `{class, selector}`
  # (the methods a flush reconciled to disk); a `:def` tab keys on `{class,
  # :def}` (a class header a revert reloaded — BT-2600). Other tabs are
  # returned unchanged: a still-pending conflict/skip (outside `flushed`)
  # or an untouched method/def. The flush path only ever passes `{class,
  # selector}` method keys, so `:def` tabs are untouched by a flush exactly
  # as before. Pure; unit-tested. Public: `BtAttachWeb.Live.Dock`'s flush
  # path (BT-3295) calls it directly.
  def clear_disk_differs(tabs, flushed) do
    Enum.map(tabs, fn tab ->
      if MapSet.member?(flushed, tab_disk_key(tab)) do
        %{tab | disk_differs: false}
      else
        tab
      end
    end)
  end

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
             MapSet.member?(reloaded, tab_disk_key(tab)) do
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
    case class_definition_info(socket, tab.class) do
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
  # Re-pull the source-dependent surfaces this module owns: all open clean
  # editor tabs (method + definition). `WorkspaceLive`'s own
  # `refresh_after_source_change/1` still refreshes the browser class list,
  # ChangeLog, and git panel — this is the method-editor-owned slice of
  # that same refresh, folded together here so callers get one coherent
  # update. Public: `BtAttachWeb.Live.Dock`'s `git_revert_event/2` (BT-3295)
  # and `WorkspaceLive`'s coalesced `:do_source_refresh` handler both call
  # it directly.
  def refresh_after_source_change(socket) do
    socket
    |> WorkspaceLive.assign_browser_classes()
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
    case method_source_info(socket, tab.class, tab.side, tab.selector) do
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
            disk_source: reactivation_disk_source(tab, info),
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
      class_definition_info(socket, tab.class)

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
          sync_active_fields(socket, active)
        else
          sync_active(socket, active)
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

  # BT-2570: restore the method-editor doc-block expand state on a genuine
  # session resume. The block's `:doc_expanded` is a socket assign that a
  # fresh mount — which every reconnect is — re-inits to its collapsed
  # default, so a user who expanded it would lose that on any transient
  # socket drop, redeploy, or laptop wake. `WorkspaceLive.terminate/2`
  # stashes the flag in the registry (Phoenix-node memory that outlives the
  # reconnect); this reads it back and re-applies it on resume. A fresh
  # session or a failed bind (not connected) leaves the collapsed default
  # untouched; a missing stash (nothing was expanded) likewise leaves the
  # default. Public: `WorkspaceLive`'s `attach/1` calls it directly on a
  # `:resumed` origin.
  def restore_doc(socket, _token, :fresh), do: socket

  def restore_doc(socket, token, :resumed) do
    with true <- socket.assigns[:connected],
         expanded when is_boolean(expanded) <- SessionRegistry.doc_stash(token) do
      assign(socket, :doc_expanded, expanded)
    else
      _ -> socket
    end
  end
end
