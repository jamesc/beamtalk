# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.MethodEditorTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.MethodEditor` (BT-3296), driving its
  `handle_event/3` clauses and the tab data model against a hand-built
  `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView mount, no
  real workspace node. Mirrors `BtAttachWeb.Live.DockTest` (BT-3295) and
  `BtAttachWeb.Live.InspectorTest` (BT-3291), the precedent this extraction
  follows.

  Covers the branches BT-3296's acceptance criteria calls out specifically:
  tab open/close/select as pure view state, `save_method/5`'s kind-based
  routing (method vs class-definition tab), the dirty-flag flip on source
  divergence + clear on successful compile, and the no-tab-field legacy
  payload path — previously reachable only through the `:workspace`-tagged
  full-stack `WorkspaceLiveTest`, which needs a live workspace node and is
  excluded from the default `mix test` lane.

  BT-3310 pushed coverage of `method_editor.ex` from 88.89% to 99.15%. Three
  lines are left deliberately uncovered — each checked against the source
  rather than assumed, per the epic's standing lesson that "looks RBAC-guarded"
  claims need verifying:

    * The `defmodule` line itself — every module in this app shows the same
      single always-"miss" line under `:cover` (see e.g.
      `BtAttachWeb.Live.Dock`'s own coverage report); it is a module-load-time
      instrumentation artifact, not a branch.
    * `update_native_view_content/3`'s anonymous fn's fallback `tab -> tab`
      clause: its one call site (`save_native_source/2`, on a successful
      compile) only reaches it after already matching `%{kind: :native,
      native_view: %{editable: true}} = tab`, so the fallback's precondition
      can never actually fail there.
    * `parse_method_signature_selector/1`'s `defp parse_method_signature_selector(_source), do: ""`
      clause: its only caller (`save_method_body/5`) receives `source` from
      `handle_event("save_method", %{"source" => source, ...})`, which already
      guards `is_binary(source)` — a non-binary can never reach it.
  """
  use ExUnit.Case, async: false

  alias BtAttach.SessionRegistry
  alias BtAttachWeb.Live.MethodEditor
  alias BtAttachWeb.StubWorkspaceClient

  # ── one-off workspace-client overrides (BT-3310) ────────────────────────
  #
  # `BtAttachWeb.StubClientOverrides` fills in every callback the module below
  # does not define with a delegate to `StubWorkspaceClient`, so each of these
  # models exactly one degraded/unusual server response instead of restating
  # the whole stub surface (CLAUDE.md no-duplicate-implementations) — the same
  # pattern `WorkspaceLivePanesTest` uses.

  # BT-2670: eval fails with the structured 4-tuple shape (`{:error, reason,
  # output, warnings}`), distinct from the facade-level 2-tuple `{:error,
  # reason}` an RBAC denial produces — `save_definition_eval/4` renders each
  # through a different helper (`Workspace.render_error/1` vs `facade_error/1`).
  defmodule Eval4TupleErrorClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def eval(_pid, _code), do: {:error, :compile_failed, "output", ["a warning"]}
  end

  # `save_native_source/2` answering a shape none of the three documented wire
  # shapes (`{:value, %{"ok" => true}}`, `{:value, %{"errors" => [...]}}`,
  # `{:error, reason}`) match — the defensive final catch-all.
  defmodule OddNativeSaveClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def save_native_source(_module, _source), do: {:value, %{"unexpected" => true}}
  end

  # `browse_method_source/3` answering each of the three non-`{:value, map}`
  # shapes `method_source_info/4` folds to the same empty-buffer fallback,
  # keyed by selector so one client drives all three.
  defmodule OddMethodSourceClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def browse_method_source(_class, _side, "nonmap-selector"), do: {:value, "not-a-map"}
    def browse_method_source(_class, _side, "erroring-selector"), do: {:error, :unreachable}
    def browse_method_source(_class, _side, "weird-selector"), do: :neither_value_nor_error
  end

  # `browse_method_source/3` that always fails — used to drive a re-fetch
  # failure against an already-open clean tab (both the re-activation pull in
  # `open_method_tab/4` and the push-refresh pull in `refresh_source_tab/2`).
  defmodule FailingMethodSourceClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def browse_method_source(_class, _side, _selector), do: {:error, :unreachable}
  end

  # `browse_class_definition/1` answering a class-definition row that is
  # simultaneously missing its `"definition"` text, carries a whitespace-only
  # `"comment"`, and is native-flagged with no `"backing_module"` — three
  # independent fallbacks (`class_definition_info/2`'s `_ -> ""`,
  # `doc_text/1`'s `"" -> nil`, `native_backing_module/1`'s `_ -> nil`) that
  # the shipped stub's canned rows never combine.
  defmodule OddClassDefinitionClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def browse_class_definition(class) do
      {:value,
       %{
         "class" => class,
         "definition" => nil,
         "comment" => "   ",
         "native" => true,
         "backing_module" => "",
         "sealed" => false,
         "typed" => false,
         "abstract" => false,
         "is_protocol" => false,
         "origin" => "both",
         "disk_differs" => false
       }}
    end
  end

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # A bare, disconnected socket carrying exactly the assigns MethodEditor's
  # functions read — the subset of `WorkspaceLive.bind_session/3`'s init
  # relevant to the tabbed method editor. `role: :owner` by default (most
  # tests aren't about RBAC); override per test.
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
        connected: true,
        tabs: [],
        active_tab: nil,
        editor_rev: 0,
        doc_expanded: false,
        native_view: nil,
        native_source: "all",
        native_source_chosen: false,
        edit_class: "",
        edit_selector: "",
        edit_source: "",
        edit_selection: nil,
        save_result: nil,
        save_error: nil,
        flush_result: nil,
        flush_error: nil,
        save_echo_pending: false,
        source_refresh_pending: false,
        browser_classes: [],
        autoflush: false,
        changes: [],
        changes_error: nil,
        expanded_changes: MapSet.new(),
        dock_tab: "workspace",
        git_status: nil,
        git_log: [],
        # Escape-claimed guard the `tab_close_active` handler reads.
        new_class_open: false,
        rename_open: false,
        show_settings: false,
        nav_popover: nil
      }
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{
      assigns: assigns,
      private: %{live_temp: %{}, lifecycle: %Phoenix.LiveView.Lifecycle{}}
    }
  end

  defp method_tab(id, class, selector, opts \\ []) do
    %{
      id: id,
      kind: :method,
      class: class,
      side: Keyword.get(opts, :side, "instance"),
      selector: selector,
      source: Keyword.get(opts, :source, "#{selector} => self"),
      base: Keyword.get(opts, :base, "#{selector} => self"),
      dirty: Keyword.get(opts, :dirty, false),
      disk_differs: Keyword.get(opts, :disk_differs, false),
      runtime_only: Keyword.get(opts, :runtime_only, false),
      synthetic: Keyword.get(opts, :synthetic, false),
      disk_source: Keyword.get(opts, :disk_source, "#{selector} => self"),
      doc: nil,
      signature: nil,
      native_module: nil,
      native_delegate: false,
      class_modifiers: [],
      class_native: false,
      source_origin: nil,
      package: nil,
      new: Keyword.get(opts, :new, false)
    }
  end

  defp def_tab(id, class, opts \\ []) do
    %{
      id: id,
      kind: :def,
      class: class,
      side: nil,
      selector: nil,
      source: Keyword.get(opts, :source, "Object subclass: #{class}"),
      base: Keyword.get(opts, :base, "Object subclass: #{class}"),
      dirty: Keyword.get(opts, :dirty, false),
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      is_protocol: false,
      native_module: nil,
      class_modifiers: [],
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }
  end

  # Mirrors `BtAttachWeb.Live.SystemBrowser`'s private `add_native_module_tab/3`
  # shape (BT-2667/BT-2670, BT-3297) — the 4th tab kind, constructed on the
  # `SystemBrowser` side but sharing this module's `:tabs` list, so
  # `compile_clean/3`'s update-syntax writes need every key present.
  defp native_tab(id, module, opts \\ []) do
    editable = Keyword.get(opts, :editable, true)
    content = Keyword.get(opts, :content, "-module(#{module}).")

    %{
      id: id,
      kind: :native,
      class: module,
      side: nil,
      selector: nil,
      native_view: %{editable: editable, content: content, error: nil},
      source: Keyword.get(opts, :source, content),
      base: Keyword.get(opts, :base, content),
      dirty: Keyword.get(opts, :dirty, false),
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      is_protocol: false,
      native_module: nil,
      native_delegate: false,
      class_modifiers: [],
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }
  end

  describe "tab strip: select/close/close_active (pure view state)" do
    test "tab_select focuses the tab and mirrors its fields into the edit assigns" do
      tabs = [
        method_tab("method:Counter:instance:foo", "Counter", "foo"),
        method_tab("method:Counter:instance:bar", "Counter", "bar", source: "bar => 1")
      ]

      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:foo"})

      {:noreply, socket} =
        MethodEditor.handle_event("tab_select", %{"id" => "method:Counter:instance:bar"}, socket)

      assert socket.assigns.active_tab == "method:Counter:instance:bar"
      assert socket.assigns.edit_selector == "bar"
      assert socket.assigns.edit_source == "bar => 1"
    end

    test "tab_select with an id that no longer maps to an open tab is a no-op" do
      tabs = [method_tab("method:Counter:instance:foo", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:foo"})

      {:noreply, result} = MethodEditor.handle_event("tab_select", %{"id" => "stale"}, socket)

      assert result.assigns.active_tab == "method:Counter:instance:foo"
    end

    test "tab_close removes the tab and moves focus to the previous remaining tab" do
      tabs = [
        method_tab("a", "Counter", "foo"),
        method_tab("b", "Counter", "bar"),
        method_tab("c", "Counter", "baz")
      ]

      socket = base_socket(%{tabs: tabs, active_tab: "b"})

      {:noreply, socket} = MethodEditor.handle_event("tab_close", %{"id" => "b"}, socket)

      assert Enum.map(socket.assigns.tabs, & &1.id) == ["a", "c"]
      assert socket.assigns.active_tab == "a"
    end

    test "tab_close on the last tab clears focus to the empty state" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, socket} = MethodEditor.handle_event("tab_close", %{"id" => "a"}, socket)

      assert socket.assigns.tabs == []
      assert socket.assigns.active_tab == nil
      assert socket.assigns.edit_class == ""
    end

    test "tab_close_active closes the focused tab" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, socket} = MethodEditor.handle_event("tab_close_active", %{}, socket)

      assert socket.assigns.tabs == []
      assert socket.assigns.active_tab == nil
    end

    test "tab_close_active no-ops when nothing is open" do
      socket = base_socket()

      {:noreply, result} = MethodEditor.handle_event("tab_close_active", %{}, socket)

      assert result.assigns.tabs == []
    end

    test "tab_close_active is claimed by an open Escape-dismissable surface (New Class modal)" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a", new_class_open: true})

      {:noreply, result} = MethodEditor.handle_event("tab_close_active", %{}, socket)

      # The tab must NOT have been closed — Escape was claimed by the modal.
      assert Enum.map(result.assigns.tabs, & &1.id) == ["a"]
      assert result.assigns.active_tab == "a"
    end
  end

  describe "save_method/5 kind-based routing" do
    test "a :method tab drives the write-surface save op (compile + flush)" do
      tabs = [method_tab("method:Counter:instance:foo", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:foo"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "foo",
            "source" => "foo => 42",
            "tab" => "method:Counter:instance:foo"
          },
          socket
        )

      assert socket.assigns.save_error == nil
      assert socket.assigns.save_result =~ "Saved foo on Counter"

      [tab] = socket.assigns.tabs
      assert tab.dirty == false
      assert tab.source == "foo => 42"
      assert tab.base == "foo => 42"
    end

    test "a :def tab evals its whole definition (compiling the class) instead of calling save" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "Object subclass: Counter",
            "tab" => "def:Counter"
          },
          socket
        )

      assert socket.assigns.save_error == nil
      assert socket.assigns.save_result == "Compiled Counter"

      [tab] = socket.assigns.tabs
      assert tab.dirty == false
    end

    test "the historical no-tab-field payload takes the method path unchanged" do
      # The BT-2409 e2e payload: no `tab` field at all. `save_method/5` must
      # still drive the method write-surface — it just has no tab to
      # reconcile against afterwards.
      socket = base_socket()

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "foo", "source" => "foo => 42"},
          socket
        )

      assert socket.assigns.save_error == nil
      assert socket.assigns.save_result =~ "Saved foo on Counter"
      # No tab existed to reconcile — the (empty) tab list is untouched.
      assert socket.assigns.tabs == []
    end

    test "an empty class name is a local validation error, no round-trip" do
      socket = base_socket()

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "", "selector" => "foo", "source" => "foo => 42"},
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error == "Enter a class name to save a method."
    end

    test "a malformed payload (missing keys) surfaces a validation error instead of crashing" do
      socket = base_socket()

      {:noreply, socket} = MethodEditor.handle_event("save_method", %{}, socket)

      assert socket.assigns.save_error == "Invalid method form payload."
    end

    test "a new-method tab parses its selector from the source signature" do
      tabs = [method_tab("new:Counter:instance", "Counter", "", new: true, source: "")]
      socket = base_socket(%{tabs: tabs, active_tab: "new:Counter:instance"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "bar => 1",
            "tab" => "new:Counter:instance"
          },
          socket
        )

      assert socket.assigns.save_error == nil
      # Promoted from the scratch `new:` tab to the canonical method tab id.
      [tab] = socket.assigns.tabs
      assert tab.id == "method:Counter:instance:bar"
      assert tab.new == false
      assert tab.selector == "bar"
    end
  end

  describe "edit_source: dirty-flag flip on source divergence" do
    test "flips dirty true when the live source diverges from the last-compiled base" do
      tabs = [method_tab("a", "Counter", "foo", base: "foo => 1", source: "foo => 1")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, socket} =
        MethodEditor.handle_event("edit_source", %{"source" => "foo => 2"}, socket)

      [tab] = socket.assigns.tabs
      assert tab.dirty == true
      assert tab.source == "foo => 2"
    end

    test "stays clean when the live source matches the last-compiled base" do
      tabs = [method_tab("a", "Counter", "foo", base: "foo => 1", source: "foo => 1")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, socket} =
        MethodEditor.handle_event("edit_source", %{"source" => "foo => 1"}, socket)

      [tab] = socket.assigns.tabs
      assert tab.dirty == false
    end

    test "a successful save_method compile clears the dirty dot and re-bases the tab" do
      tabs = [
        method_tab("a", "Counter", "foo", base: "foo => 1", source: "foo => 2", dirty: true)
      ]

      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "foo", "source" => "foo => 2", "tab" => "a"},
          socket
        )

      [tab] = socket.assigns.tabs
      assert tab.dirty == false
      assert tab.base == "foo => 2"
    end

    test "a malformed payload (non-binary source) is ignored rather than crashing" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, result} = MethodEditor.handle_event("edit_source", %{"source" => 123}, socket)

      assert result.assigns.tabs == tabs
    end

    test "a successful compile arms :save_echo_pending so the coalesced push refresh recognises its own echo" do
      # `WorkspaceLive.handle_info(:do_source_refresh, ...)` reads this flag
      # (via `MethodEditor.resync_active_tab/2`) to decide whether to keep or
      # clear the save/flush banner on the next push — it is the cross-module
      # half of `compile_clean/3`'s contract, otherwise only exercised by the
      # `:workspace`-tagged full-stack test.
      tabs = [method_tab("a", "Counter", "foo", base: "foo => 1", source: "foo => 2")]
      socket = base_socket(%{tabs: tabs, active_tab: "a", save_echo_pending: false})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "foo", "source" => "foo => 2", "tab" => "a"},
          socket
        )

      assert socket.assigns.save_echo_pending == true
    end
  end

  describe "open_method_tab/4 (System Browser / omni-search / senders-implementors call-through)" do
    test "opens a fresh method tab seeded from the image-accurate source" do
      socket = base_socket()

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")

      assert socket.assigns.active_tab == "method:Counter:instance:increment"
      assert [%{kind: :method, class: "Counter", selector: "increment"}] = socket.assigns.tabs
    end

    test "re-opening the same clean tab re-focuses rather than duplicating" do
      socket = base_socket()
      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")
      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "value")

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")

      assert socket.assigns.active_tab == "method:Counter:instance:increment"
      assert length(socket.assigns.tabs) == 2
    end

    test "re-opening a dirty tab just refocuses, never clobbering the in-progress edit" do
      dirty_tab =
        method_tab("method:Counter:instance:increment", "Counter", "increment",
          source: "increment => self.value := self.value + 99",
          dirty: true
        )

      socket = base_socket(%{tabs: [dirty_tab], active_tab: nil})

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")

      [tab] = socket.assigns.tabs
      assert tab.source == "increment => self.value := self.value + 99"
      assert tab.dirty == true
    end
  end

  describe "promote_new_method_tab/4 (via save_method on a new-method tab)" do
    test "drops the scratch tab and refocuses the already-open canonical tab for the same selector" do
      canonical =
        method_tab("method:Counter:instance:bar", "Counter", "bar", source: "bar => 0")

      scratch = method_tab("new:Counter:instance", "Counter", "", new: true, source: "")

      socket = base_socket(%{tabs: [canonical, scratch], active_tab: "new:Counter:instance"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "bar => 1",
            "tab" => "new:Counter:instance"
          },
          socket
        )

      assert socket.assigns.save_error == nil
      # The scratch tab is gone; the pre-existing canonical tab absorbed the
      # save and is now focused — no duplicate "Class ▸ (new method)" tab.
      assert [%{id: "method:Counter:instance:bar", source: "bar => 1", new: false}] =
               socket.assigns.tabs

      assert socket.assigns.active_tab == "method:Counter:instance:bar"
    end
  end

  describe "clear_disk_differs/2" do
    test "clears the badge only on flushed method tabs" do
      tabs = [
        method_tab("a", "Greeter", "greet", disk_differs: true),
        method_tab("b", "Counter", "increment", disk_differs: true)
      ]

      flushed = MapSet.new([{"Greeter", "greet"}])

      assert [
               %{class: "Greeter", disk_differs: false},
               %{class: "Counter", disk_differs: true}
             ] = MethodEditor.clear_disk_differs(tabs, flushed)
    end

    test "an empty flushed set leaves every tab untouched" do
      tabs = [method_tab("a", "Greeter", "greet", disk_differs: true)]

      assert MethodEditor.clear_disk_differs(tabs, MapSet.new()) == tabs
    end

    test "leaves a :def tab (no selector) untouched without raising" do
      tab = def_tab("def:Counter", "Counter") |> Map.put(:disk_differs, true)

      assert MethodEditor.clear_disk_differs([tab], MapSet.new([{"Counter", "increment"}])) == [
               tab
             ]
    end

    test "clears a :def tab when its {class, :def} key is in the flushed set (BT-2600)" do
      tab = def_tab("def:Counter", "Counter") |> Map.put(:disk_differs, true)
      other = def_tab("def:Greeter", "Greeter") |> Map.put(:disk_differs, true)

      assert [
               %{class: "Counter", disk_differs: false},
               %{class: "Greeter", disk_differs: true}
             ] = MethodEditor.clear_disk_differs([tab, other], MapSet.new([{"Counter", :def}]))
    end
  end

  describe "reactivation_disk_source/2 (BT-2565)" do
    defp source_info(opts) do
      %{
        runtime_only: Keyword.get(opts, :runtime_only, false),
        disk_differs: Keyword.get(opts, :disk_differs, false),
        source: Keyword.get(opts, :source, "")
      }
    end

    test "keeps the prior snapshot when the image diverged but stays disk-backed" do
      existing = %{disk_source: "increment => self.value := self.value + 1"}
      info = source_info(disk_differs: true, source: "increment => self.value := self.value + 2")

      assert MethodEditor.reactivation_disk_source(existing, info) ==
               "increment => self.value := self.value + 1"
    end

    test "takes a fresh snapshot when the image is back in sync with disk" do
      existing = %{disk_source: "stale body"}
      info = source_info(disk_differs: false, source: "fresh on-disk body")

      assert MethodEditor.reactivation_disk_source(existing, info) == "fresh on-disk body"
    end

    test "drops to nil when the method is now runtime-only" do
      existing = %{disk_source: "was on disk"}
      info = source_info(runtime_only: true, source: "runtime body")

      assert MethodEditor.reactivation_disk_source(existing, info) == nil
    end

    test "drops to nil when image-diverged with no prior snapshot to carry" do
      existing = %{disk_source: nil}
      info = source_info(disk_differs: true, source: "diverged body")

      assert MethodEditor.reactivation_disk_source(existing, info) == nil
    end
  end

  describe "toggle_doc" do
    test "flips doc_expanded" do
      socket = base_socket(%{doc_expanded: false})

      {:noreply, socket} = MethodEditor.handle_event("toggle_doc", %{}, socket)
      assert socket.assigns.doc_expanded == true

      {:noreply, socket} = MethodEditor.handle_event("toggle_doc", %{}, socket)
      assert socket.assigns.doc_expanded == false
    end
  end

  describe "select_source" do
    test "tracks a selection matching the active tab's stamp" do
      socket = base_socket(%{active_tab: "a"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "select_source",
          %{"text" => "foo", "tab_id" => "a", "start" => 0, "end" => 3},
          socket
        )

      assert socket.assigns.edit_selection == %{text: "foo", start: 0, end: 3}
    end

    test "drops a stale stamp from a departed CmEditor that no longer matches the active tab" do
      socket = base_socket(%{active_tab: "b", edit_selection: nil})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "select_source",
          %{"text" => "foo", "tab_id" => "a", "start" => 0, "end" => 3},
          socket
        )

      assert socket.assigns.edit_selection == nil
    end

    test "a malformed payload (missing tab_id) is ignored" do
      socket = base_socket()

      {:noreply, result} =
        MethodEditor.handle_event("select_source", %{"text" => "foo"}, socket)

      assert result.assigns.edit_selection == nil
    end
  end

  describe "native_source" do
    test "sets the native module source-origin filter and marks it chosen" do
      socket = base_socket()

      {:noreply, socket} = MethodEditor.handle_event("native_source", %{"src" => "deps"}, socket)

      assert socket.assigns.native_source == "deps"
      assert socket.assigns.native_source_chosen == true
    end

    test "an unrecognised value is ignored" do
      socket = base_socket()

      {:noreply, result} =
        MethodEditor.handle_event("native_source", %{"src" => "bogus"}, socket)

      assert result.assigns.native_source == "all"
      assert result.assigns.native_source_chosen == false
    end
  end

  describe "native_save" do
    test "compiles, reloads, and writes back an editable native tab's source" do
      tab = native_tab("native:mymod", "mymod")

      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "native_save",
          %{"source" => "-module(mymod).\n-export([f/0])."},
          socket
        )

      assert socket.assigns.save_error == nil
      assert socket.assigns.save_result == "Saved mymod.erl"

      [tab] = socket.assigns.tabs
      assert tab.dirty == false
      assert tab.native_view.content == "-module(mymod).\n-export([f/0])."
    end

    test "a save against a non-native active tab is a graceful no-op" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, result} =
        MethodEditor.handle_event("native_save", %{"source" => "whatever"}, socket)

      assert result.assigns.tabs == tabs
      assert result.assigns.save_error == nil
    end
  end

  describe "dismiss_native_error and dismiss_native_module_error" do
    test "dismiss_native_error clears only the :error field of the top-level native_view" do
      socket = base_socket(%{native_view: %{content: "…", error: "boom"}})

      {:noreply, socket} = MethodEditor.handle_event("dismiss_native_error", %{}, socket)

      assert socket.assigns.native_view == %{content: "…", error: nil}
    end

    test "dismiss_native_error is a no-op when the pane is closed" do
      socket = base_socket(%{native_view: nil})

      {:noreply, result} = MethodEditor.handle_event("dismiss_native_error", %{}, socket)

      assert result.assigns.native_view == nil
    end

    test "dismiss_native_module_error clears the active :native tab's cached error" do
      tab = %{
        id: "native:mymod",
        kind: :native,
        class: "mymod",
        native_view: %{content: "…", error: "compile failed"}
      }

      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      {:noreply, socket} =
        MethodEditor.handle_event("dismiss_native_module_error", %{}, socket)

      [updated] = socket.assigns.tabs
      assert updated.native_view.error == nil
    end

    test "dismiss_native_module_error no-ops against a non-native active tab" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, result} =
        MethodEditor.handle_event("dismiss_native_module_error", %{}, socket)

      assert result.assigns.tabs == tabs
    end
  end

  describe "open_definition/1,2" do
    test "open_definition/2 opens a fresh class-definition tab" do
      socket = base_socket()

      socket = MethodEditor.open_definition(socket, "Counter")

      assert socket.assigns.active_tab == "def:Counter"
      assert [%{kind: :def, class: "Counter"}] = socket.assigns.tabs
    end

    test "open_definition/2 re-focuses an already-open definition tab rather than duplicating" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: nil})

      socket = MethodEditor.open_definition(socket, "Counter")

      assert socket.assigns.active_tab == "def:Counter"
      assert length(socket.assigns.tabs) == 1
    end

    test "open_definition/1 opens the active tab's class definition" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      socket = MethodEditor.open_definition(socket)

      assert socket.assigns.active_tab == "def:Counter"
    end

    test "open_definition/1 is a no-op on an empty strip" do
      socket = base_socket()

      result = MethodEditor.open_definition(socket)

      assert result.assigns.tabs == []
    end
  end

  # ── malformed tab_select/tab_close/native_save fallback catch-alls ──────
  #
  # BT-3310: read against the source first (per the epic's stated lesson) —
  # none of these three fallback clauses check `socket.assigns.role`; they are
  # plain missing/malformed-params catch-alls, exactly like `save_method`'s
  # and `edit_source`'s already-tested "missing keys" clauses above, not RBAC
  # gates. Triggered by mismatched params, not a role.
  describe "malformed tab_select/tab_close/native_save payloads (plain catch-alls, not RBAC)" do
    test "tab_select with no \"id\" key is a no-op" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, result} = MethodEditor.handle_event("tab_select", %{}, socket)

      assert result.assigns.active_tab == "a"
      assert result.assigns.tabs == tabs
    end

    test "tab_close with no \"id\" key is a no-op" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      {:noreply, result} = MethodEditor.handle_event("tab_close", %{}, socket)

      assert result.assigns.tabs == tabs
    end

    test "native_save with no \"source\" key is a graceful no-op" do
      tab = native_tab("native:mymod", "mymod")
      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      {:noreply, result} = MethodEditor.handle_event("native_save", %{}, socket)

      assert result.assigns.tabs == [tab]
      assert result.assigns.save_error == nil
    end
  end

  # ── save_method / save_definition / save_native_source error branches ──
  #
  # BT-3310: `role: :observer` genuinely drives `BtAttach.Facade.dispatch/3`'s
  # own RBAC gate (`BtAttach.Rbac.authorize/2` denies `:execute`-class ops —
  # `:save`/`:eval`/`:save_native_source` — to the Observer role) — a real
  # unauthorized-role denial, not a params catch-all. `MethodEditor` itself
  # never inspects `socket.assigns.role`; it only renders whatever
  # `Facade.dispatch/3` returns through `facade_error/1`, the same helper any
  # other dispatch error (a down workspace, a malformed reply) renders through
  # too — so this is a real reachable *error* branch, but not evidence the
  # module does its own role check.
  describe "save_method / save_definition / save_native_source error branches" do
    test "save_method_body renders the facade error when :save is denied to a non-owner role" do
      tabs = [method_tab("a", "Counter", "foo")]
      socket = base_socket(%{tabs: tabs, active_tab: "a", role: :observer})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "foo", "source" => "foo => 42", "tab" => "a"},
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error =~ "Not authorized"
    end

    test "save_definition rejects a blank (whitespace-only) class-definition source" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "", "source" => "   ", "tab" => "def:Counter"},
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error == "Enter a class definition to compile."
    end

    test "save_definition reports 'not attached to workspace' when there is no session pid" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter", session_pid: nil})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "Object subclass: Counter",
            "tab" => "def:Counter"
          },
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error == "not attached to workspace"
    end

    test "save_definition_eval renders Workspace.render_error/1 on a structured 4-tuple eval error" do
      Application.put_env(:bt_attach, :workspace_client, Eval4TupleErrorClient)

      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "Object subclass: Counter",
            "tab" => "def:Counter"
          },
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error == StubWorkspaceClient.render_error(:compile_failed)
    end

    test "save_definition_eval renders the facade error when :eval is denied to a non-owner role" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter", role: :observer})

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{
            "class" => "Counter",
            "selector" => "",
            "source" => "Object subclass: Counter",
            "tab" => "def:Counter"
          },
          socket
        )

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error =~ "Not authorized"
    end

    test "save_native_source falls back to a generic message on an unrecognised result shape" do
      Application.put_env(:bt_attach, :workspace_client, OddNativeSaveClient)

      tab = native_tab("native:mymod", "mymod")
      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      {:noreply, socket} =
        MethodEditor.handle_event("native_save", %{"source" => "-module(mymod)."}, socket)

      assert socket.assigns.save_result == nil
      assert socket.assigns.save_error == "Could not save native source."
    end

    test "the empty-state hidden form's tab=\"\" sentinel is a no-op re-base" do
      socket = base_socket()

      {:noreply, socket} =
        MethodEditor.handle_event(
          "save_method",
          %{"class" => "Counter", "selector" => "foo", "source" => "foo => 42", "tab" => ""},
          socket
        )

      assert socket.assigns.save_error == nil
      assert socket.assigns.tabs == []
    end
  end

  describe "native_compile_error/1 (via native_save)" do
    test "a compile error with no integer \"line\" key uses the bare message" do
      StubWorkspaceClient.set_native_save(
        {:value, %{"errors" => [%{"message" => "syntax error"}]}}
      )

      tab = native_tab("native:mymod", "mymod")
      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      {:noreply, socket} =
        MethodEditor.handle_event("native_save", %{"source" => "-module(mymod)."}, socket)

      assert socket.assigns.save_error == "syntax error"
    end
  end

  describe "method_source_info/4 non-{:value, map} fallbacks (all fold to the empty buffer)" do
    test "a {:value, non_map} result opens an empty editable buffer" do
      Application.put_env(:bt_attach, :workspace_client, OddMethodSourceClient)
      socket = base_socket()

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "nonmap-selector")

      [tab] = socket.assigns.tabs
      assert tab.source == ""
      assert tab.doc == nil
    end

    test "an {:error, reason} result opens an empty editable buffer" do
      Application.put_env(:bt_attach, :workspace_client, OddMethodSourceClient)
      socket = base_socket()

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "erroring-selector")

      [tab] = socket.assigns.tabs
      assert tab.source == ""
    end

    test "any other unrecognised result opens an empty editable buffer" do
      Application.put_env(:bt_attach, :workspace_client, OddMethodSourceClient)
      socket = base_socket()

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "weird-selector")

      [tab] = socket.assigns.tabs
      assert tab.source == ""
      assert tab.runtime_only == false
    end
  end

  describe "disk_body_snapshot/1 (via open_method_tab on a runtime-only method)" do
    defmodule RuntimeOnlyMethodSourceClient do
      @moduledoc false
      use BtAttachWeb.StubClientOverrides

      def browse_method_source(_class, _side, _selector) do
        {:value,
         %{
           "source" => "runtime => body",
           "doc" => nil,
           "signature" => nil,
           "source_status" => "indexed",
           "origin" => "runtime",
           "disk_differs" => false,
           "native_delegate" => false
         }}
      end
    end

    test "a runtime-only method opens with no on-disk snapshot to diff future compiles against" do
      Application.put_env(:bt_attach, :workspace_client, RuntimeOnlyMethodSourceClient)
      socket = base_socket()

      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")

      [tab] = socket.assigns.tabs
      assert tab.runtime_only == true
      assert tab.disk_source == nil
    end
  end

  describe "class_definition_info/2 fallbacks (missing definition / blank comment / blank backing module)" do
    test "a definition row missing its text/comment/backing-module fields degrades gracefully" do
      Application.put_env(:bt_attach, :workspace_client, OddClassDefinitionClient)
      socket = base_socket()

      socket = MethodEditor.open_definition(socket, "Weird")

      [tab] = socket.assigns.tabs
      # `_ -> ""` in class_definition_info/2: a non-binary "definition" opens
      # an empty (rather than crashing) editable buffer.
      assert tab.source == ""
      # `doc_text/1`'s `"" -> nil`: a whitespace-only comment is treated as
      # "no doc", not rendered as a blank doc block.
      assert tab.doc == nil
      # `native_backing_module/1`'s `_ -> nil`: "native" => true but a blank
      # "backing_module" still yields no backing module (and no Native badge).
      assert tab.native_module == nil
      assert tab.class_native == false
    end
  end

  describe "close_tab/2 on a background (non-active) tab" do
    test "closing a tab that is not the active one leaves focus untouched" do
      tabs = [
        method_tab("a", "Counter", "foo"),
        method_tab("b", "Counter", "bar")
      ]

      socket = base_socket(%{tabs: tabs, active_tab: "a"})

      socket = MethodEditor.close_tab(socket, "b")

      assert Enum.map(socket.assigns.tabs, & &1.id) == ["a"]
      assert socket.assigns.active_tab == "a"
    end
  end

  describe "open_definition/1 on a :native active tab" do
    test "is a no-op: a native module has no Beamtalk class definition to open" do
      tab = native_tab("native:mymod", "mymod")
      socket = base_socket(%{tabs: [tab], active_tab: "native:mymod"})

      result = MethodEditor.open_definition(socket)

      assert result.assigns.tabs == [tab]
      assert result.assigns.active_tab == "native:mymod"
    end
  end

  describe "open_method_tab/4 re-activation when the re-fetch itself fails" do
    test "keeps the existing buffer rather than blanking a tab the user is looking at" do
      socket = base_socket()
      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")
      [original] = socket.assigns.tabs

      Application.put_env(:bt_attach, :workspace_client, FailingMethodSourceClient)
      socket = MethodEditor.open_method_tab(socket, "Counter", "instance", "increment")

      assert socket.assigns.active_tab == "method:Counter:instance:increment"
      [tab] = socket.assigns.tabs
      assert tab.source == original.source
    end
  end

  describe "open_new_method/3 re-focusing an already-open scratch tab" do
    test "re-clicking \"new method\" on the same class/side refocuses rather than duplicating" do
      socket = base_socket()
      socket = MethodEditor.open_new_method(socket, "Counter", "instance")

      {:noreply, socket} =
        MethodEditor.handle_event("edit_source", %{"source" => "partial => "}, socket)

      socket = MethodEditor.open_new_method(socket, "Counter", "instance")

      assert length(socket.assigns.tabs) == 1
      assert socket.assigns.active_tab == "new:Counter:instance"
      [tab] = socket.assigns.tabs
      assert tab.source == "partial => "
    end
  end

  describe "parse_method_signature_selector/1 via a new-method tab's source (BT-2606)" do
    defp save_new_method(socket, source) do
      MethodEditor.handle_event(
        "save_method",
        %{
          "class" => "Counter",
          "selector" => "",
          "source" => source,
          "tab" => "new:Counter:instance"
        },
        socket
      )
    end

    test "a leading whole-line comment followed by the real header is skipped recursively" do
      tabs = [method_tab("new:Counter:instance", "Counter", "", new: true, source: "")]
      socket = base_socket(%{tabs: tabs, active_tab: "new:Counter:instance"})

      {:noreply, socket} = save_new_method(socket, "// a leading comment\nbar => 1")

      assert socket.assigns.save_error == nil
      [tab] = socket.assigns.tabs
      assert tab.selector == "bar"
    end

    test "a comment-only source (no header line ever arrives) parses to no selector" do
      tabs = [method_tab("new:Counter:instance", "Counter", "", new: true, source: "")]
      socket = base_socket(%{tabs: tabs, active_tab: "new:Counter:instance"})

      {:noreply, socket} = save_new_method(socket, "// just a comment, nothing else")

      assert socket.assigns.save_error == "Could not parse a method signature from the source."
    end

    test "a bare modifier keyword immediately before a bodyless arrow parses to no selector" do
      tabs = [method_tab("new:Counter:instance", "Counter", "", new: true, source: "")]
      socket = base_socket(%{tabs: tabs, active_tab: "new:Counter:instance"})

      # "class ->" is neither `class => …` (the modifier word as a unary
      # selector) nor a real `-> Type =>`/`-> arg =>` header — it exercises
      # `return_type_follows_arrow?/1`'s empty-remainder fallback (`_ ->
      # false`) and `selector_from_header/1`'s no-`"=>"` fallback together.
      {:noreply, socket} = save_new_method(socket, "class ->")

      assert socket.assigns.save_error == "Could not parse a method signature from the source."
    end

    test "a header whose selector text is neither an identifier nor an operator parses to no selector" do
      tabs = [method_tab("new:Counter:instance", "Counter", "", new: true, source: "")]
      socket = base_socket(%{tabs: tabs, active_tab: "new:Counter:instance"})

      {:noreply, socket} = save_new_method(socket, "1 => nil")

      assert socket.assigns.save_error == "Could not parse a method signature from the source."
    end
  end

  describe "header_origin_title/1 and tab_disk_key/1 fallbacks (direct, pure)" do
    test "header_origin_title/1 falls back to \"Project\" for an unrecognised/absent origin" do
      assert MethodEditor.header_origin_title(%{source_origin: nil, package: nil}) == "Project"

      assert MethodEditor.header_origin_title(%{source_origin: "project", package: nil}) ==
               "Project"
    end

    test "tab_disk_key/1 has no disk key for a tab kind other than :method/:def" do
      assert MethodEditor.tab_disk_key(%{kind: :native, class: "mymod"}) == nil
    end

    test "modifier_badges/1 shows no class-modifier badges when the tab carries no modifier list" do
      assert MethodEditor.modifier_badges(%{class_modifiers: nil}) == []
    end
  end

  describe "reload_reverted_def_buffers/2 when the re-fetch fails" do
    test "keeps the tab's existing buffer rather than blanking it" do
      tab = def_tab("def:Counter", "Counter", source: "Object subclass: Counter // v1")
      socket = base_socket(%{tabs: [tab], active_tab: "def:Counter"})

      StubWorkspaceClient.fail_class_definition(true)
      on_exit(fn -> StubWorkspaceClient.fail_class_definition(false) end)

      socket = MethodEditor.reload_reverted_def_buffers(socket, MapSet.new([{"Counter", :def}]))

      [result] = socket.assigns.tabs
      assert result.source == "Object subclass: Counter // v1"
    end
  end

  describe "refresh_after_source_change/1: empty-source fallback and non-source tab kinds" do
    test "a clean method tab whose re-fetch now fails keeps its buffer; a :native tab passes through" do
      method =
        method_tab("a", "Counter", "foo", source: "foo => 1", base: "foo => 1", dirty: false)

      native = native_tab("native:mymod", "mymod")

      socket = base_socket(%{tabs: [method, native], active_tab: "a"})

      Application.put_env(:bt_attach, :workspace_client, FailingMethodSourceClient)
      socket = MethodEditor.refresh_after_source_change(socket)

      [refreshed_method, refreshed_native] = socket.assigns.tabs
      assert refreshed_method.source == "foo => 1"
      assert refreshed_native == native
    end
  end

  describe "restore_doc/3 on a genuine resume (BT-2570)" do
    test "re-applies a stashed expand state read back from the session registry" do
      token = "method-editor-test-doc-#{System.unique_integer([:positive])}"
      pid = spawn(fn -> Process.sleep(:infinity) end)
      on_exit(fn -> Process.exit(pid, :kill) end)

      SessionRegistry.register(token, "sess-doc-test", pid)
      :ok = SessionRegistry.stash_doc(token, true)

      socket = base_socket(%{doc_expanded: false, connected: true})

      result = MethodEditor.restore_doc(socket, token, :resumed)

      assert result.assigns.doc_expanded == true
    end
  end

  describe "@method_editor_events coverage" do
    test "every event WorkspaceLive delegates to MethodEditor resolves to an implemented clause" do
      params_by_event = %{
        "tab_select" => %{"id" => "a"},
        "tab_close" => %{"id" => "a"},
        "tab_close_active" => %{},
        "open_definition" => %{},
        "edit_source" => %{"source" => "foo => 1"},
        "save_method" => %{},
        "native_source" => %{"src" => "all"},
        "native_save" => %{"source" => "-module(foo)."},
        "dismiss_native_error" => %{},
        "dismiss_native_module_error" => %{},
        "toggle_doc" => %{},
        "select_source" => %{"text" => "x", "tab_id" => "a"}
      }

      # A hardcoded event-name list would itself be an unenforced "keep in
      # sync" copy of `@method_editor_events` — read it from `WorkspaceLive`
      # instead, so adding/removing a name on one side without the other
      # fails here.
      for event <- BtAttachWeb.WorkspaceLive.method_editor_events() do
        params = Map.fetch!(params_by_event, event)

        assert {:noreply, %Phoenix.LiveView.Socket{}} =
                 MethodEditor.handle_event(event, params, base_socket()),
               "MethodEditor.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end
  end
end
