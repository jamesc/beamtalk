# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceNativeModulesTest do
  @moduledoc """
  Integration test for the System Browser "Native modules" section (BT-2648): a
  loaded package's hand-written native Erlang modules are discoverable in the IDE
  even when no `native:` class backs them (the reported beamtalk-http case — a
  dependency loaded without instantiating its classes). The section lists the
  modules, tags them by package/origin (DEP · <pkg> / STDLIB), and opens a
  module's `.erl` read-only in the native source-view pane. A `.beam`-only module
  (no shipped source) degrades to the existing "source not available" note.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_native_modules` /
  `browse_native_module_source` mirror the new ops. No `:workspace` tag, so this
  runs in the bare `mix test` lane.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  setup do
    Application.put_env(:bt_attach, :workspace_client, BtAttachWeb.StubWorkspaceClient)

    Application.put_env(:bt_attach, :oidc, %{
      issuer: "https://idp",
      client_id: "id",
      redirect_uri: "https://ide/callback",
      groups_claim: "groups",
      client_secret: "x",
      roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
    })

    Application.put_env(:bt_attach, :session_ttl_secs, 3600)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
      BtAttachWeb.StubWorkspaceClient.stop_state(2_000)
    end)

    {:ok, _} = BtAttachWeb.StubWorkspaceClient.start_state()

    :ok
  end

  defp owner_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  defp observer_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "bob", "groups" => ["beamtalk-observers"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  describe "Native modules section (BT-2648)" do
    test "the section header renders with the module count", %{conn: conn} do
      {:ok, _view, html} = live(owner_conn(conn), "/")

      # The collapsible section is present (the stub returns three native modules),
      # collapsed by default — the module rows are not yet in the DOM.
      assert html =~ "Native modules"
      assert html =~ ~s(phx-click="toggle_native_modules")
      refute html =~ ~s(phx-value-module="beamtalk_http_client")
    end

    test "expanding lists native modules with package/origin badges", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "toggle_native_modules", %{})

      # A dependency's native module (no class to back it) is listed and tagged
      # DEP · <pkg> — discoverable WITHOUT a native: class (the acceptance case).
      assert html =~ ~s(phx-value-module="beamtalk_http_client")
      assert html =~ "DEP · http"
      # The stdlib native module is tagged STDLIB.
      assert html =~ ~s(phx-value-module="beamtalk_subprocess")
      assert html =~ "stdlib"
    end

    test "clicking a native module opens its .erl read-only as an editor tab (BT-2667)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "toggle_native_modules", %{})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      # BT-2667: the module opens as a first-class editor TAB (in the tab strip),
      # not the retired single-slot overlay. The tab is labelled `<module>.erl`.
      assert html =~ ~s(role="tab")
      assert html =~ "beamtalk_http_client.erl"
      # The tab body renders the source read-only.
      assert html =~ "Erlang module"
      assert html =~ ~s(class="native-pre")
      assert html =~ "handle_call({get"
      assert html =~ "read-only"
      # BT-2668: the displayed path is the clean, project-relative form — never the
      # absolute `/home/...` host path the op returned.
      assert html =~ "deps/beamtalk_http/native/beamtalk_http_client.erl"
      refute html =~ "/home/agent"

      # BT-2667: re-clicking the same module FOCUSES the existing tab rather than
      # closing it (the old overlay toggled off) or stacking a duplicate — only one
      # `.erl` tab exists.
      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      assert html =~ ~s(class="native-pre")
      # Exactly one tab in the strip for this module (re-open focused, didn't stack
      # a duplicate): the tab id appears twice — the tab button + its close × —
      # and a duplicated tab would double that to four.
      assert count_occurrences(html, ~s(phx-value-id="native:beamtalk_http_client")) == 2

      # Closing the tab removes it; the editor returns to its empty state.
      html = render_click(view, "tab_close", %{"id" => "native:beamtalk_http_client"})
      refute html =~ ~s(class="native-pre")
      refute html =~ "beamtalk_http_client.erl"
    end

    test "a native module tab coexists with a class tab; it does not replace the class (BT-2667)",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open a Beamtalk class definition tab first.
      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})
      assert html =~ "Counter ▸ def"

      # Open a native module — it must NOT overlay/replace the class tab.
      render_click(view, "toggle_native_modules", %{})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      # Both tabs are present in the strip; the native tab is now focused.
      assert html =~ "Counter ▸ def"
      assert html =~ "beamtalk_http_client.erl"
      assert html =~ ~s(class="native-pre")

      # Switching back to the class tab restores the class editor (the native tab
      # did not clobber it).
      html = render_click(view, "tab_select", %{"id" => "def:Counter"})
      assert html =~ "Counter ▸ def"
      refute html =~ ~s(class="native-pre")
    end

    test "a module with no shipped source shows the graceful empty state", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "toggle_native_modules", %{})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_stripped"})

      # No source content → the honest "not available" note, not an error or an
      # empty pane.
      assert html =~ "Erlang source not available"
      refute html =~ ~s(class="native-pre")
    end

    test "an observer can browse native modules (the op is :read)", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      render_click(view, "toggle_native_modules", %{})

      html =
        render_click(view, "browser_open_native_module", %{"module" => "beamtalk_http_client"})

      assert html =~ "Erlang module"
      assert html =~ ~s(class="native-pre")
      assert html =~ "handle_call({get"
    end
  end

  # BT-2668: the path-cleaning that keeps the absolute host path off-screen. Unit
  # test of the relativiser directly (it runs on whatever path the runtime op
  # returns), independent of the rendered viewer above.
  describe "clean_native_path/1 (BT-2668)" do
    alias BtAttachWeb.WorkspaceLive

    test "relativises an absolute build path to a recognisable source root" do
      assert WorkspaceLive.clean_native_path(
               "/home/james/source/proj/_build/default/deps/http/native/x.erl"
             ) == "deps/http/native/x.erl"

      assert WorkspaceLive.clean_native_path("/opt/app/apps/beamtalk_stdlib/src/y.erl") ==
               "apps/beamtalk_stdlib/src/y.erl"
    end

    test "leaves an already-relative path untouched" do
      assert WorkspaceLive.clean_native_path("deps/http/native/x.erl") ==
               "deps/http/native/x.erl"
    end

    test "falls back to the basename for an absolute path with no known source root" do
      assert WorkspaceLive.clean_native_path("/home/james/scratch/foo.erl") == "foo.erl"
    end

    test "returns nil for the absent-path sentinels (no path leak)" do
      assert WorkspaceLive.clean_native_path(:null) == nil
      assert WorkspaceLive.clean_native_path(nil) == nil
      assert WorkspaceLive.clean_native_path("") == nil
    end
  end

  # Count non-overlapping occurrences of `needle` in `haystack` — proves a tab is
  # focused, not duplicated, when its module is re-opened.
  defp count_occurrences(haystack, needle) do
    haystack |> String.split(needle) |> length() |> Kernel.-(1)
  end
end
