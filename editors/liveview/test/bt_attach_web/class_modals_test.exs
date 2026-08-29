# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.ClassModalsTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.ClassModals` (BT-3298), driving its
  `handle_event/3` clauses and the create/rename/remove data model against a
  hand-built `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace
  client (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView
  mount, no real workspace node. Mirrors `BtAttachWeb.Live.MethodEditorTest`
  (BT-3296) and `BtAttachWeb.Live.SystemBrowserTest` (BT-3297), the
  precedent this extraction follows.

  Covers the branches BT-3298's acceptance criteria calls out specifically:
  new-class/new-method validation errors and rename submit validation.
  """
  use ExUnit.Case, async: false

  alias BtAttachWeb.Live.ClassModals
  alias BtAttachWeb.StubWorkspaceClient
  alias BtAttachWeb.WorkspaceLive

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # A bare, disconnected socket carrying exactly the assigns ClassModals'
  # functions read — the subset of `WorkspaceLive.bind_session/3`'s init
  # relevant to the New Class/Rename modals and the tabbed method editor's
  # Remove/Rename actions (mirroring `MethodEditorTest.base_socket/1`, since
  # this module cross-calls `BtAttachWeb.Live.MethodEditor` the same way).
  # `role: :owner` by default (most tests aren't about RBAC); override per
  # test.
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
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
        browser_source: "all",
        browser_source_chosen: true,
        browser_error: nil,
        selected_class: nil,
        selected_protocol: nil,
        autoflush: false,
        changes: [],
        changes_error: nil,
        expanded_changes: MapSet.new(),
        dock_tab: "workspace",
        git_status: nil,
        git_log: [],
        browser_side: "instance",
        new_class_open: false,
        new_class_error: nil,
        new_class_name: "",
        new_class_super: "Object",
        rename_open: false,
        rename_kind: nil,
        rename_class: nil,
        rename_side: nil,
        rename_old_selector: nil,
        rename_new_name: "",
        rename_error: nil,
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
      source: "#{selector} => self",
      base: "#{selector} => self",
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      synthetic: false,
      disk_source: "#{selector} => self",
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

  defp def_tab(id, class) do
    %{
      id: id,
      kind: :def,
      class: class,
      side: nil,
      selector: nil,
      source: "Object subclass: #{class}",
      base: "Object subclass: #{class}",
      dirty: false,
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

  describe "@class_modals_events coverage (BT-3301 pattern)" do
    test "WorkspaceLive's @class_modals_events IS ClassModals's canonical list, not a copy" do
      assert WorkspaceLive.class_modals_events() == ClassModals.__class_modals_events__()
    end

    test "every event WorkspaceLive delegates to ClassModals resolves to an implemented clause" do
      tabs = [
        method_tab("method:Counter:instance:increment", "Counter", "increment"),
        def_tab("def:Counter", "Counter")
      ]

      socket =
        base_socket(%{
          tabs: tabs,
          active_tab: "method:Counter:instance:increment",
          rename_open: true,
          rename_kind: :class,
          rename_class: "Counter"
        })

      params_by_event = %{
        "remove_method" => %{},
        "remove_class" => %{},
        "open_rename" => %{},
        "close_rename" => %{},
        "rename_submit" => %{"new_name" => "Accumulator"},
        "new_method" => %{"class" => "Counter"},
        "toggle_new_class" => %{},
        "close_new_class" => %{},
        "new_class" => %{"name" => "Greeter", "superclass" => "Object"}
      }

      for event <- ClassModals.__class_modals_events__() do
        params = Map.fetch!(params_by_event, event)
        result = ClassModals.handle_event(event, params, socket)

        assert match?({:noreply, %Phoenix.LiveView.Socket{}}, result),
               "ClassModals.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end

    test "no handle_event/3 clause head names an event missing from the canonical list" do
      source =
        Path.expand("../../lib/bt_attach_web/live/class_modals.ex", __DIR__) |> File.read!()

      clause_names =
        ~r/def handle_event\("([a-z0-9_]+)"/
        |> Regex.scan(source)
        |> Enum.map(fn [_, name] -> name end)
        |> MapSet.new()

      assert clause_names == MapSet.new(ClassModals.__class_modals_events__())
    end
  end

  describe "New Class modal validation (BT-2293, BT-2645)" do
    test "toggle_new_class opens the modal with clean defaults" do
      socket = base_socket(%{new_class_error: "stale"})

      {:noreply, socket} = ClassModals.handle_event("toggle_new_class", %{}, socket)

      assert socket.assigns.new_class_open == true
      assert socket.assigns.new_class_error == nil
      assert socket.assigns.new_class_name == ""
      assert socket.assigns.new_class_super == "Object"
    end

    test "toggle_new_class closes an already-open modal" do
      socket = base_socket(%{new_class_open: true})

      {:noreply, socket} = ClassModals.handle_event("toggle_new_class", %{}, socket)

      assert socket.assigns.new_class_open == false
    end

    test "close_new_class discards the in-flight fields" do
      socket = base_socket(%{new_class_open: true, new_class_error: "boom"})

      {:noreply, socket} = ClassModals.handle_event("close_new_class", %{}, socket)

      assert socket.assigns.new_class_open == false
      assert socket.assigns.new_class_error == nil
    end

    test "an empty class name is rejected with an in-modal error" do
      {:noreply, socket} =
        ClassModals.handle_event("new_class", %{"name" => ""}, base_socket())

      assert socket.assigns.new_class_open == true
      assert socket.assigns.new_class_error == "Enter a class name to create a class."
    end

    test "a non-PascalCase class name is rejected" do
      {:noreply, socket} =
        ClassModals.handle_event("new_class", %{"name" => "greeter"}, base_socket())

      assert socket.assigns.new_class_error =~ "PascalCase"
      # The in-flight value is preserved so the re-rendered modal shows it.
      assert socket.assigns.new_class_name == "greeter"
    end

    test "a duplicate class name is rejected" do
      socket = base_socket(%{browser_classes: [%{"name" => "Greeter"}]})

      {:noreply, socket} =
        ClassModals.handle_event("new_class", %{"name" => "Greeter"}, socket)

      assert socket.assigns.new_class_error == "A class named Greeter already exists."
    end

    test "an invalid superclass is rejected even when the class name is valid" do
      {:noreply, socket} =
        ClassModals.handle_event(
          "new_class",
          %{"name" => "Greeter", "superclass" => "not valid!"},
          base_socket()
        )

      assert socket.assigns.new_class_error =~ "Superclass must be a class name"
    end

    test "a valid submission creates the class, opens its definition tab, and closes the modal" do
      socket = base_socket(%{new_class_open: true})

      {:noreply, socket} =
        ClassModals.handle_event("new_class", %{"name" => "Greeter"}, socket)

      assert socket.assigns.new_class_open == false
      assert socket.assigns.new_class_error == nil
      assert socket.assigns.active_tab == "def:Greeter"
      assert socket.assigns.selected_class == "Greeter"
      assert socket.assigns.save_result =~ "Created new class"
    end

    test "a blank superclass falls back to Object" do
      socket = base_socket()

      {:noreply, socket} =
        ClassModals.handle_event(
          "new_class",
          %{"name" => "Greeter", "superclass" => "  "},
          socket
        )

      assert socket.assigns.new_class_error == nil
      assert socket.assigns.active_tab == "def:Greeter"
    end

    test "a malformed payload (missing name) surfaces a validation error rather than crashing" do
      {:noreply, socket} = ClassModals.handle_event("new_class", %{}, base_socket())

      assert socket.assigns.new_class_error == "Invalid new-class form payload."
    end
  end

  describe "new_method (\"Add a method…\")" do
    test "opens a blank method tab for the given class on the current browser side" do
      socket = base_socket(%{browser_side: "instance"})

      {:noreply, socket} =
        ClassModals.handle_event("new_method", %{"class" => "Counter"}, socket)

      assert [%{kind: :method, class: "Counter", side: "instance", new: true}] =
               socket.assigns.tabs
    end

    test "a non-owner (Observer) click is a no-op" do
      socket = base_socket(%{role: :observer})

      assert {:noreply, ^socket} =
               ClassModals.handle_event("new_method", %{"class" => "Counter"}, socket)
    end

    test "a malformed payload (empty class) is a no-op" do
      socket = base_socket()

      assert {:noreply, ^socket} =
               ClassModals.handle_event("new_method", %{"class" => ""}, socket)
    end
  end

  describe "Remove Method (ADR 0112 Phase 4, BT-3189)" do
    test "removes the active method's selector and closes its tab" do
      tabs = [method_tab("method:Counter:instance:increment", "Counter", "increment")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:increment"})

      {:noreply, socket} = ClassModals.handle_event("remove_method", %{}, socket)

      assert socket.assigns.tabs == []
      assert socket.assigns.save_result =~ "Removed increment from Counter"
    end

    test "a brand-new (unsaved) method tab has nothing to remove" do
      tabs = [method_tab("method:new:1", "Counter", nil, new: true)]
      socket = base_socket(%{tabs: tabs, active_tab: "method:new:1"})

      {:noreply, socket} = ClassModals.handle_event("remove_method", %{}, socket)

      assert socket.assigns.save_error =~ "hasn't been saved yet"
    end

    test "a crafted event against a non-method tab is a graceful no-op" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} = ClassModals.handle_event("remove_method", %{}, socket)

      assert socket.assigns.save_error == "Open an existing method to remove it."
    end

    test "a non-owner (Observer) click is a no-op" do
      socket = base_socket(%{role: :observer})

      assert {:noreply, ^socket} = ClassModals.handle_event("remove_method", %{}, socket)
    end
  end

  describe "Remove Class (ADR 0113 Phase 4, BT-3210)" do
    test "removes the active class-definition tab's class and closes it" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} = ClassModals.handle_event("remove_class", %{}, socket)

      assert socket.assigns.tabs == []
      assert socket.assigns.save_result =~ "Removed Counter from memory"
    end

    test "a crafted event against a non-def tab is a graceful no-op" do
      tabs = [method_tab("method:Counter:instance:increment", "Counter", "increment")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:increment"})

      {:noreply, socket} = ClassModals.handle_event("remove_class", %{}, socket)

      assert socket.assigns.save_error == "Open a class definition to remove it."
    end

    test "a non-owner (Observer) click is a no-op" do
      socket = base_socket(%{role: :observer})

      assert {:noreply, ^socket} = ClassModals.handle_event("remove_class", %{}, socket)
    end
  end

  describe "Rename modal open/close (ADR 0114 Phase 5, BT-3277)" do
    test "opening against a :def tab targets a class rename, pre-filled with the current name" do
      tabs = [def_tab("def:Counter", "Counter")]
      socket = base_socket(%{tabs: tabs, active_tab: "def:Counter"})

      {:noreply, socket} = ClassModals.handle_event("open_rename", %{}, socket)

      assert socket.assigns.rename_open == true
      assert socket.assigns.rename_kind == :class
      assert socket.assigns.rename_class == "Counter"
      assert socket.assigns.rename_new_name == "Counter"
    end

    test "opening against an existing :method tab targets a selector rename" do
      tabs = [method_tab("method:Counter:instance:increment", "Counter", "increment")]
      socket = base_socket(%{tabs: tabs, active_tab: "method:Counter:instance:increment"})

      {:noreply, socket} = ClassModals.handle_event("open_rename", %{}, socket)

      assert socket.assigns.rename_kind == :method
      assert socket.assigns.rename_class == "Counter"
      assert socket.assigns.rename_old_selector == "increment"
      assert socket.assigns.rename_new_name == "increment"
    end

    test "opening against a brand-new (unsaved) method tab reports nothing to rename" do
      tabs = [method_tab("method:new:1", "Counter", nil, new: true)]
      socket = base_socket(%{tabs: tabs, active_tab: "method:new:1"})

      {:noreply, socket} = ClassModals.handle_event("open_rename", %{}, socket)

      assert socket.assigns.rename_open == false
      assert socket.assigns.save_error =~ "hasn't been saved yet"
    end

    test "opening with an empty tab strip reports nothing to rename" do
      {:noreply, socket} = ClassModals.handle_event("open_rename", %{}, base_socket())

      assert socket.assigns.save_error =~ "Open a class definition or an existing method"
    end

    test "a non-owner (Observer) open is a no-op" do
      socket = base_socket(%{role: :observer})

      assert {:noreply, ^socket} = ClassModals.handle_event("open_rename", %{}, socket)
    end

    test "close_rename dismisses without renaming anything" do
      socket = base_socket(%{rename_open: true, rename_error: "stale"})

      {:noreply, socket} = ClassModals.handle_event("close_rename", %{}, socket)

      assert socket.assigns.rename_open == false
      assert socket.assigns.rename_error == nil
    end
  end

  describe "rename_submit validation (ADR 0114 Phase 5, BT-3277)" do
    test "an empty new class name is rejected, keeping the modal open" do
      socket =
        base_socket(%{rename_open: true, rename_kind: :class, rename_class: "Counter"})

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => ""}, socket)

      assert socket.assigns.rename_open == true
      assert socket.assigns.rename_error == "Enter a new class name."
    end

    test "a non-PascalCase new class name is rejected" do
      socket =
        base_socket(%{rename_open: true, rename_kind: :class, rename_class: "Counter"})

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => "accumulator"}, socket)

      assert socket.assigns.rename_error =~ "PascalCase"
    end

    test "a duplicate new class name is rejected" do
      socket =
        base_socket(%{
          rename_open: true,
          rename_kind: :class,
          rename_class: "Counter",
          browser_classes: [%{"name" => "Accumulator"}]
        })

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => "Accumulator"}, socket)

      assert socket.assigns.rename_error == "A class named Accumulator already exists."
    end

    test "a valid class rename closes the modal and reports success" do
      tabs = [def_tab("def:Counter", "Counter")]

      socket =
        base_socket(%{
          tabs: tabs,
          active_tab: "def:Counter",
          rename_open: true,
          rename_kind: :class,
          rename_class: "Counter",
          selected_class: "Counter"
        })

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => "Accumulator"}, socket)

      assert socket.assigns.rename_open == false
      assert socket.assigns.rename_error == nil
      assert socket.assigns.save_result =~ "Renamed Counter to Accumulator"
      # The stale-named tab was closed and the renamed class's definition
      # tab opened in its place.
      assert socket.assigns.active_tab == "def:Accumulator"
      assert socket.assigns.selected_class == "Accumulator"
    end

    test "an empty new selector is rejected for a method rename" do
      socket =
        base_socket(%{
          rename_open: true,
          rename_kind: :method,
          rename_class: "Counter",
          rename_side: "instance",
          rename_old_selector: "increment"
        })

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => ""}, socket)

      assert socket.assigns.rename_error == "Enter a new selector."
    end

    test "a new selector containing an injection-shaped character is rejected" do
      socket =
        base_socket(%{
          rename_open: true,
          rename_kind: :method,
          rename_class: "Counter",
          rename_side: "instance",
          rename_old_selector: "increment"
        })

      {:noreply, socket} =
        ClassModals.handle_event(
          "rename_submit",
          %{"new_name" => "increment to: #evil"},
          socket
        )

      assert socket.assigns.rename_error =~ "cannot contain spaces"
    end

    test "a valid method rename closes the modal, closes the stale tab, and reports success" do
      tabs = [method_tab("method:Counter:instance:increment", "Counter", "increment")]

      socket =
        base_socket(%{
          tabs: tabs,
          active_tab: "method:Counter:instance:increment",
          rename_open: true,
          rename_kind: :method,
          rename_class: "Counter",
          rename_side: "instance",
          rename_old_selector: "increment"
        })

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => "incrementBy"}, socket)

      assert socket.assigns.rename_open == false
      assert socket.assigns.save_result =~ "Renamed Counter increment to incrementBy"
      assert socket.assigns.tabs == []
    end

    test "no session (attach failed) reports 'not attached' rather than crashing" do
      socket =
        base_socket(%{
          session_pid: nil,
          rename_open: true,
          rename_kind: :class,
          rename_class: "Counter"
        })

      {:noreply, socket} =
        ClassModals.handle_event("rename_submit", %{"new_name" => "Accumulator"}, socket)

      assert socket.assigns.rename_error == "not attached to workspace"
    end

    test "a non-owner (Observer) submit is a no-op" do
      socket = base_socket(%{role: :observer, rename_open: true})

      assert {:noreply, ^socket} =
               ClassModals.handle_event("rename_submit", %{"new_name" => "X"}, socket)
    end

    test "a malformed payload (missing new_name) is a no-op" do
      socket = base_socket(%{rename_open: true})

      assert {:noreply, ^socket} = ClassModals.handle_event("rename_submit", %{}, socket)
    end
  end

  describe "valid_class_name?/1 (shared with BtAttachWeb.Live.Dock's flush_destructive/3)" do
    test "accepts a bare PascalCase identifier" do
      assert ClassModals.valid_class_name?("Greeter")
      assert ClassModals.valid_class_name?("HTTPServer")
    end

    test "rejects lowercase, empty, and injection-shaped input" do
      refute ClassModals.valid_class_name?("greeter")
      refute ClassModals.valid_class_name?("")
      refute ClassModals.valid_class_name?("Foo. Session current clear")
    end
  end
end
