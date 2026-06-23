# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceOriginFilterTest do
  @moduledoc """
  Integration tests for the System Browser's source-origin filter (BT-2657) and
  its initial default (BT-2661).

  BT-2657: selecting Project / Deps / Stdlib must narrow the class tree to the
  matching origin (and All shows everything). The narrowing is measured by the
  *interactive* class rows — those carrying `phx-value-class` — since under the
  Hierarchy view a filtered-out superclass ancestor can still render as a dimmed,
  non-interactive context row (BT-2649). The stub's class rows carry no
  `superclass`, so each is its own root and the visible set equals the matching
  set with no context spine.

  BT-2661: the filter defaults to Project on first open (scoping the tree to the
  project's own classes), falls back to All when the workspace has no
  project-origin classes, and a deliberate user pick wins over the default on a
  later refresh.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`). No `:workspace` tag, so this runs in the
  bare `mix test` lane.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.StubWorkspaceClient

  setup do
    Application.put_env(:bt_attach, :workspace_client, StubWorkspaceClient)

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
      StubWorkspaceClient.stop_state(2_000)
    end)

    {:ok, _} = StubWorkspaceClient.start_state()
    :ok
  end

  defp owner_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "alice", "groups" => ["beamtalk-owners"]},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  # The interactive (clickable) class names in the rendered tree: only matching
  # rows carry `phx-value-class` (context ancestor rows do not — BT-2649).
  defp interactive_classes(html) do
    html
    |> Floki.parse_fragment!()
    |> Floki.find("#system-browser-tree div[phx-value-class]")
    |> Floki.attribute("phx-value-class")
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.sort()
  end

  # Switch the origin filter via the source-filter <select> form (`phx-change`).
  defp choose_source(view, src) do
    view
    |> element("form.src-filter")
    |> render_change(%{"src" => src})
  end

  # The stub's canned origins: project = Counter/Subprocess/Ledger,
  # dependency = HttpClient/Orphan, stdlib = Object.
  @project ~w(Counter Ledger Subprocess)
  @deps ~w(HttpClient Orphan)
  @stdlib ~w(Object)
  @all Enum.sort(@project ++ @deps ++ @stdlib)

  describe "origin filter narrows the class tree (BT-2657)" do
    test "Project shows only project-origin classes", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      html = choose_source(view, "project")
      assert interactive_classes(html) == Enum.sort(@project)
    end

    test "Deps shows only dependency-origin classes", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      html = choose_source(view, "deps")
      assert interactive_classes(html) == Enum.sort(@deps)
    end

    test "Stdlib shows only stdlib-origin classes", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      html = choose_source(view, "stdlib")
      assert interactive_classes(html) == Enum.sort(@stdlib)
    end

    test "All shows every class regardless of origin", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      html = choose_source(view, "all")
      assert interactive_classes(html) == @all
    end

    test "the same narrowing holds in the Category view", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # Switch to the Category view, then filter to Deps.
      view |> element(~s(button[phx-value-view="category"])) |> render_click()
      html = choose_source(view, "deps")

      assert interactive_classes(html) == Enum.sort(@deps)
      # The project/stdlib classes are gone, not merely dimmed.
      refute html =~ ~s(phx-value-class="Counter")
      refute html =~ ~s(phx-value-class="Object")
    end
  end

  describe "initial origin-filter default (BT-2661)" do
    test "opens Project-scoped once the class tree loads", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      # BT-2591: the tree loads off the mount; await the async fold, which is also
      # when the BT-2661 default is applied (`apply_browser_classes` →
      # `apply_default_browser_source`).
      html = render_async(view, 5_000)

      # The Proj option is selected in the filter <select>...
      select =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(select.src-select option[value="project"]))

      assert Floki.attribute(select, "selected") == ["selected"]

      # ...and the tree is actually scoped to the project's own classes.
      assert interactive_classes(html) == Enum.sort(@project)
    end

    test "falls back to All when there are no project-origin classes", %{conn: conn} do
      # A bare / stdlib-only workspace: no project rows, so Project would blank the
      # tree — the default must fall back to All.
      StubWorkspaceClient.set_browse_classes([
        %{"name" => "Object", "source_origin" => "stdlib", "package" => "beamtalk"},
        %{"name" => "Integer", "source_origin" => "stdlib", "package" => "beamtalk"}
      ])

      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = render_async(view, 5_000)

      select =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(select.src-select option[value="all"]))

      assert Floki.attribute(select, "selected") == ["selected"]
      assert interactive_classes(html) == ~w(Integer Object)
    end

    test "a deliberate pick wins over the default on a later refresh", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # The user switches to Stdlib...
      html = choose_source(view, "stdlib")
      assert interactive_classes(html) == Enum.sort(@stdlib)

      # ...and a later class-tree refresh (the coalesced source refresh a
      # `ClassLoaded`/`ClassRemoved` burst fires, which re-runs
      # `apply_browser_classes`) must NOT snap the filter back to the Project
      # default — the chosen Stdlib filter is preserved.
      send(view.pid, :do_source_refresh)
      # Drain the async :do_source_refresh deterministically before asserting:
      # :sys.get_state is a synchronous call to the view process (FIFO after the
      # send), so the re-render + diff push have completed by the time render/1
      # reads the proxy. Without it the negative ("filter not reset") assertion
      # could pass vacuously on the pre-refresh HTML under unlucky scheduling.
      :sys.get_state(view.pid)
      html = render(view)
      assert interactive_classes(html) == Enum.sort(@stdlib)
    end
  end
end
