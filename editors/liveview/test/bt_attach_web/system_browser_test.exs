# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.SystemBrowserTest do
  @moduledoc """
  Direct unit tests for `BtAttachWeb.Live.SystemBrowser` (BT-3297), driving
  its `handle_event/3` clauses and the browse/navigation data model against a
  hand-built `%Phoenix.LiveView.Socket{}` and the fully-stubbed workspace
  client (`BtAttachWeb.StubWorkspaceClient`, BT-2554) — no full LiveView
  mount, no real workspace node. Mirrors `BtAttachWeb.Live.MethodEditorTest`
  (BT-3296), `BtAttachWeb.Live.DockTest` (BT-3295), and
  `BtAttachWeb.Live.InspectorTest` (BT-3291), the precedent this extraction
  follows.

  Covers the branches BT-3297's acceptance criteria calls out specifically:
  hierarchy vs category view (`hierarchy_rows/1` — already covered by
  `BtAttachWeb.ClassTreeTest`, reused directly here — and `category_groups/1`),
  instance/class side toggle refetch, the protocol filter
  (`filtered_methods/2`), runtime-badge origin marking, and the
  source-navigation events (hover, goto-definition, senders, implementors).
  """
  use ExUnit.Case, async: false

  alias BtAttachWeb.Live.SystemBrowser
  alias BtAttachWeb.StubWorkspaceClient
  alias BtAttachWeb.WorkspaceLive

  @system_browser_source Path.expand("../../lib/bt_attach_web/live/system_browser.ex", __DIR__)

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)
    {:ok, _} = StubWorkspaceClient.start_state()

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # A bare, disconnected socket carrying exactly the assigns SystemBrowser's
  # functions read — the subset of `WorkspaceLive.bind_session/3`'s init
  # relevant to the System Browser pane, plus the handful of tab-related keys
  # `BtAttachWeb.Live.MethodEditor` owns that this module cross-calls
  # (`:tabs`/`:active_tab`). `role: :owner` by default (most tests aren't
  # about RBAC); override per test.
  defp base_socket(overrides \\ %{}) do
    assigns =
      %{
        __changed__: %{},
        flash: %{},
        current_user: nil,
        role: :owner,
        session_id: "sess-1",
        session_pid: self(),
        connected: true,
        browser_view: "hierarchy",
        browser_source: "all",
        browser_source_chosen: true,
        browser_side: "instance",
        selected_class: nil,
        selected_protocol: nil,
        browser_protocols: [],
        browser_classes: [],
        browser_error: nil,
        browser_categories: %{"has_dividers" => false, "categories" => []},
        browser_group_mode: "protocol",
        editing_section: nil,
        section_form_error: nil,
        native_view: nil,
        browser_mode: :classes,
        browser_native_modules: [],
        browser_type_aliases: [],
        native_source: "all",
        native_source_chosen: true,
        nav_popover: nil,
        show_browser: true,
        tabs: [],
        active_tab: nil
      }
      |> Map.merge(overrides)

    %Phoenix.LiveView.Socket{assigns: assigns}
  end

  defp def_tab(id, class, opts \\ []) do
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
      is_protocol: Keyword.get(opts, :is_protocol, false),
      native_module: nil,
      class_modifiers: [],
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
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
      disk_source: nil,
      doc: nil,
      signature: nil,
      native_module: nil,
      native_delegate: false,
      class_modifiers: [],
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }
  end

  defp native_tab(id, module, opts \\ []) do
    %{
      id: id,
      kind: :native,
      class: module,
      side: nil,
      selector: nil,
      native_view: %{editable: Keyword.get(opts, :editable, false), content: "", error: nil},
      source: "",
      base: "",
      dirty: false,
      disk_differs: false,
      runtime_only: false,
      disk_source: nil,
      doc: nil,
      signature: nil,
      is_protocol: false,
      native_module: nil,
      native_delegate: false,
      class_modifiers: nil,
      class_native: false,
      source_origin: nil,
      package: nil,
      new: false
    }
  end

  describe "@system_browser_events coverage (BT-3301 pattern)" do
    test "WorkspaceLive's @system_browser_events IS SystemBrowser's canonical list, not a copy" do
      assert WorkspaceLive.system_browser_events() == SystemBrowser.__system_browser_events__()
    end

    test "every event WorkspaceLive delegates to SystemBrowser resolves to an implemented clause" do
      tabs = [
        method_tab("method:Counter:instance:increment", "Counter", "increment"),
        def_tab("def:Printable", "Printable", is_protocol: true),
        native_tab("native:beamtalk_subprocess", "beamtalk_subprocess")
      ]

      socket =
        base_socket(%{
          tabs: tabs,
          active_tab: "method:Counter:instance:increment",
          browser_classes: [%{"name" => "Counter"}]
        })

      params_by_event = %{
        "complete" => %{"code" => "Int"},
        "hover" => %{"code" => "Counter"},
        "diagnostics" => %{"code" => "3 + 4"},
        "close_browser" => %{},
        "toggle_browser" => %{},
        "browser_open_definition" => %{"class" => "Counter"},
        "browser_open_native" => %{"class" => "Subprocess"},
        "browser_mode" => %{"mode" => "classes"},
        "browser_open_native_module" => %{"module" => "beamtalk_http_client"},
        "browser_jump_native" => %{"class" => "Subprocess", "selector" => "readLine"},
        "browser_open_alias" => %{"name" => "RestartStrategy", "package" => "my_app"},
        "dismiss_alias_error" => %{},
        "browser_view" => %{"view" => "hierarchy"},
        "browser_source" => %{"src" => "all"},
        "browser_side" => %{"side" => "instance"},
        "browser_select_class" => %{"class" => "Counter"},
        "browser_select_protocol" => %{"protocol" => ""},
        "browser_select_method" => %{
          "class" => "Counter",
          "side" => "instance",
          "selector" => "increment"
        },
        "browser_group_mode" => %{"mode" => "protocol"},
        "browser_edit_section" => %{"name" => ""},
        "browser_cancel_section" => %{},
        "browser_rename_section" => %{"old_name" => "Old", "new_name" => "New"},
        "browser_add_section" => %{"new_name" => "New", "before_selector" => "increment"},
        "senders" => %{},
        "implementors" => %{},
        "native_callers" => %{},
        "required_methods" => %{},
        "conforming_classes" => %{},
        "nav_open_class" => %{"class" => "Counter"},
        "nav_required_open" => %{"selector" => "printOn:"},
        "nav_open" => %{"class" => "Counter", "side" => "instance", "selector" => "increment"},
        "nav_close" => %{},
        "goto_definition" => %{"token" => "Counter"},
        "dismiss_nav_error" => %{}
      }

      # `selected_class` must be set for `browser_rename_section`/
      # `browser_add_section` (both require it in the socket, not the params).
      socket = %{
        socket
        | assigns: Map.merge(socket.assigns, %{selected_class: "Counter"})
      }

      # A hardcoded event-name list here would itself be an unenforced
      # "keep in sync" copy of `@system_browser_events` — read it from
      # `SystemBrowser` instead, so adding/renaming/removing a name fails
      # here rather than only at runtime in the browser.
      for event <- SystemBrowser.__system_browser_events__() do
        params = Map.fetch!(params_by_event, event)
        result = SystemBrowser.handle_event(event, params, socket)

        assert match?({:noreply, %Phoenix.LiveView.Socket{}}, result) or
                 match?({:reply, %{}, %Phoenix.LiveView.Socket{}}, result),
               "SystemBrowser.handle_event/3 has no clause for #{inspect(event)} (or it crashed)"
      end
    end

    test "no handle_event/3 clause head names an event missing from the canonical list" do
      # The test above only catches a name in the canonical list with no
      # matching clause (a rename/removal that leaves the list stale). It
      # can't catch the OTHER direction: a brand-new
      # `SystemBrowser.handle_event("some_new_event", ...)` clause added
      # without adding "some_new_event" to `__system_browser_events__/0` is
      # unreachable dead code (`WorkspaceLive`'s `when event in
      # @system_browser_events` guard never lets it through) rather than a
      # crash, so nothing above would fail. Elixir doesn't expose clause-head
      # literals through module reflection, so this scans the module's own
      # source text for `handle_event("...", ...)` clause heads instead and
      # asserts the literal name set matches the canonical list exactly.
      source = File.read!(@system_browser_source)

      clause_names =
        ~r/def handle_event\("([a-z0-9_]+)"/
        |> Regex.scan(source)
        |> Enum.map(fn [_, name] -> name end)
        |> MapSet.new()

      assert clause_names == MapSet.new(SystemBrowser.__system_browser_events__())
    end
  end

  describe "class tree view helpers" do
    test "category_groups/1 buckets by category, with dedicated Tests/Protocols buckets" do
      classes = [
        %{"name" => "Counter", "category" => "Numbers"},
        %{"name" => "Ledger", "category" => "Numbers"},
        %{"name" => "CounterTest", "is_test" => true},
        %{"name" => "Printable", "is_protocol" => true},
        %{"name" => "Orphan"}
      ]

      groups = SystemBrowser.category_groups(classes)
      bucket_names = Enum.map(groups, fn {category, _rows} -> category end)

      assert Enum.sort(bucket_names) == ["(uncategorized)", "Numbers", "Protocols", "Tests"]

      assert {_, [%{"name" => "Orphan"}]} = Enum.find(groups, &(elem(&1, 0) == "(uncategorized)"))
      assert {_, [%{"name" => "Printable"}]} = Enum.find(groups, &(elem(&1, 0) == "Protocols"))
      assert {_, [%{"name" => "CounterTest"}]} = Enum.find(groups, &(elem(&1, 0) == "Tests"))

      assert {_, [%{"name" => "Counter"}, %{"name" => "Ledger"}]} =
               Enum.find(groups, &(elem(&1, 0) == "Numbers"))
    end

    test "filter_by_source/2 narrows by source_origin, \"all\" and unknown filters pass through" do
      classes = [
        %{"name" => "Counter", "source_origin" => "project"},
        %{"name" => "Object", "source_origin" => "stdlib"},
        %{"name" => "HttpClient", "source_origin" => "dependency"}
      ]

      assert SystemBrowser.filter_by_source(classes, "all") == classes

      assert SystemBrowser.filter_by_source(classes, "project") == [
               %{"name" => "Counter", "source_origin" => "project"}
             ]

      assert SystemBrowser.filter_by_source(classes, "deps") == [
               %{"name" => "HttpClient", "source_origin" => "dependency"}
             ]

      assert SystemBrowser.filter_by_source(classes, "bogus") == classes
    end
  end

  describe "protocol filter + method list" do
    @protocols [
      %{"name" => "all", "selectors" => [%{"selector" => "increment"}]},
      %{
        "name" => "printing",
        "selectors" => [%{"selector" => "printOn:"}, %{"selector" => "displayString"}]
      }
    ]

    test "filtered_methods/2 with a nil filter flattens every protocol's selectors" do
      selectors =
        SystemBrowser.filtered_methods(@protocols, nil) |> Enum.map(&Map.get(&1, "selector"))

      assert Enum.sort(selectors) == ["displayString", "increment", "printOn:"]
    end

    test "filtered_methods/2 with a protocol name narrows to just that protocol" do
      selectors =
        SystemBrowser.filtered_methods(@protocols, "printing")
        |> Enum.map(&Map.get(&1, "selector"))

      assert Enum.sort(selectors) == ["displayString", "printOn:"]
    end

    test "protocol_method_count/1 totals selectors across every protocol" do
      assert SystemBrowser.protocol_method_count(@protocols) == 3
    end
  end

  describe "runtime-badge origin marking" do
    test "runtime_only?/1 is true only for a runtime-origin row" do
      assert SystemBrowser.runtime_only?(%{"origin" => "runtime"})
      refute SystemBrowser.runtime_only?(%{"origin" => "both"})
      refute SystemBrowser.runtime_only?(%{})
    end

    test "synthetic?/1 is true only for a synthetic-source-status row" do
      assert SystemBrowser.synthetic?(%{"source_status" => "synthetic"})
      refute SystemBrowser.synthetic?(%{"source_status" => "indexed"})
    end

    test "source_origin_label/1 badges a dependency row with its package, stdlib bare" do
      assert SystemBrowser.source_origin_label(%{
               "source_origin" => "dependency",
               "package" => "http"
             }) == "DEP · http"

      assert SystemBrowser.source_origin_label(%{"source_origin" => "stdlib"}) == "stdlib"
      assert SystemBrowser.source_origin_label(%{"source_origin" => "project"}) == ""
    end
  end

  describe "browser_select_class / browser_side (instance/class toggle refetch)" do
    test "selecting a class fetches its protocols for the current side" do
      socket = base_socket()

      {:noreply, socket} =
        SystemBrowser.handle_event("browser_select_class", %{"class" => "Counter"}, socket)

      assert socket.assigns.selected_class == "Counter"
      assert [%{"selectors" => selectors}] = socket.assigns.browser_protocols
      assert Enum.any?(selectors, &(&1["selector"] == "increment"))
    end

    test "flipping the instance/class side re-fetches protocols for the new side" do
      socket = base_socket(%{selected_class: "Counter", browser_side: "instance"})

      {:noreply, socket} =
        SystemBrowser.handle_event("browser_side", %{"side" => "class"}, socket)

      assert socket.assigns.browser_side == "class"
      # `Counter` has no class-side stub methods, so the class-side fetch
      # comes back empty — proving the side toggle actually re-fetched
      # (rather than keeping the instance-side result).
      assert [%{"selectors" => []}] = socket.assigns.browser_protocols
    end

    test "an unrecognised side is ignored (pure no-op)" do
      socket = base_socket(%{browser_side: "instance"})

      {:noreply, unchanged} =
        SystemBrowser.handle_event("browser_side", %{"side" => "bogus"}, socket)

      assert unchanged == socket
    end
  end

  describe "source-navigation: hover / diagnostics / complete" do
    test "hover replies with the live docs for a known class" do
      socket = base_socket()

      assert {:reply, %{"hover" => hover}, ^socket} =
               SystemBrowser.handle_event("hover", %{"code" => "Counter"}, socket)

      assert hover =~ "Counter"
    end

    test "hover with no session_pid replies with an empty string" do
      socket = base_socket(%{session_pid: nil})

      assert {:reply, %{"hover" => ""}, ^socket} =
               SystemBrowser.handle_event("hover", %{"code" => "Counter"}, socket)
    end

    test "complete replies with the stub's completion list" do
      socket = base_socket()

      assert {:reply, %{"completions" => []}, ^socket} =
               SystemBrowser.handle_event("complete", %{"code" => "Int"}, socket)
    end

    test "diagnostics replies with the stub's diagnostic list, no session_pid needed" do
      socket = base_socket(%{session_pid: nil})

      assert {:reply, %{"diagnostics" => []}, ^socket} =
               SystemBrowser.handle_event("diagnostics", %{"code" => "3 + 4"}, socket)
    end
  end

  describe "source-navigation: senders / implementors" do
    test "implementors opens the popover with the active method tab's selector" do
      StubWorkspaceClient.set_implementors("increment", [
        %{"class" => "Counter", "class_side" => false, "method" => "increment"}
      ])

      socket =
        base_socket(%{
          tabs: [method_tab("method:Counter:instance:increment", "Counter", "increment")],
          active_tab: "method:Counter:instance:increment"
        })

      {:noreply, socket} = SystemBrowser.handle_event("implementors", %{}, socket)

      assert %{kind: :implementors, selector: "increment", sites: [_site]} =
               socket.assigns.nav_popover
    end

    test "senders/implementors on a class-definition tab (no selector) is a graceful no-op" do
      socket =
        base_socket(%{
          tabs: [def_tab("def:Counter", "Counter")],
          active_tab: "def:Counter"
        })

      {:noreply, socket} = SystemBrowser.handle_event("senders", %{}, socket)
      assert socket.assigns.nav_popover == nil
    end

    test "nav_open opens the method tab and points the browser at its class/side" do
      socket =
        base_socket(%{nav_popover: %{kind: :implementors, selector: "increment", sites: []}})

      {:noreply, socket} =
        SystemBrowser.handle_event(
          "nav_open",
          %{"class" => "Counter", "side" => "instance", "selector" => "increment"},
          socket
        )

      assert socket.assigns.nav_popover == nil
      assert socket.assigns.selected_class == "Counter"
      assert socket.assigns.active_tab == "method:Counter:instance:increment"
    end
  end

  describe "source-navigation: goto_definition (BT-2666)" do
    test "a known class name opens its definition tab and navigates the browser" do
      socket = base_socket(%{browser_classes: [%{"name" => "Counter"}]})

      {:noreply, socket} =
        SystemBrowser.handle_event("goto_definition", %{"token" => "Counter"}, socket)

      assert socket.assigns.active_tab == "def:Counter"
      assert socket.assigns.selected_class == "Counter"
    end

    test "an unknown selector with no implementors flashes \"No definition found\"" do
      socket = base_socket(%{browser_classes: []})

      {:noreply, socket} =
        SystemBrowser.handle_event("goto_definition", %{"token" => "nonesuch"}, socket)

      assert Phoenix.Flash.get(socket.assigns.flash, :info) == "No definition found."
    end

    test "a single implementor opens that method tab directly" do
      StubWorkspaceClient.set_implementors("increment", [
        %{"class" => "Counter", "class_side" => false, "method" => "increment"}
      ])

      socket = base_socket(%{browser_classes: []})

      {:noreply, socket} =
        SystemBrowser.handle_event(
          "goto_definition",
          %{"token" => "increment", "code" => "self increment"},
          socket
        )

      assert socket.assigns.active_tab == "method:Counter:instance:increment"
      assert socket.assigns.nav_popover == nil
    end

    test "several implementors open the shared Senders/Implementors popover" do
      StubWorkspaceClient.set_implementors("increment", [
        %{"class" => "Counter", "class_side" => false, "method" => "increment"},
        %{"class" => "Ledger", "class_side" => false, "method" => "increment"}
      ])

      socket = base_socket(%{browser_classes: []})

      {:noreply, socket} =
        SystemBrowser.handle_event(
          "goto_definition",
          %{"token" => "increment", "code" => "self increment"},
          socket
        )

      assert %{kind: :implementors, sites: [_, _]} = socket.assigns.nav_popover
    end
  end

  describe "panel visibility" do
    test "toggle_browser flips show_browser, close_browser always closes it" do
      socket = base_socket(%{show_browser: true})

      {:noreply, socket} = SystemBrowser.handle_event("toggle_browser", %{}, socket)
      assert socket.assigns.show_browser == false

      {:noreply, socket} = SystemBrowser.handle_event("toggle_browser", %{}, socket)
      assert socket.assigns.show_browser == true

      {:noreply, socket} = SystemBrowser.handle_event("close_browser", %{}, socket)
      assert socket.assigns.show_browser == false
    end
  end
end
