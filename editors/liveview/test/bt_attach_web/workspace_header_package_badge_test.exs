# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceHeaderPackageBadgeTest do
  @moduledoc """
  Integration test for the editor-header package/origin badge (BT-2642): the
  breadcrumb header (`editor-meta`) surfaces a `.source-origin-tag.header` for the
  active tab on every tab kind (method, class-definition) and for every origin,
  including project. It reuses BT-2641's vocabulary (STDLIB / DEP · <pkg>) and adds
  the bare project package name for project tabs (which the class tree still hides).

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_classes` reports `Counter` as
  project (package `exdura`), `HttpClient` as a dependency (package `HTTP`), and
  `Object` as stdlib. No `:workspace` tag, so this runs in the bare `mix test` lane.
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

  # The header package badge element (`.source-origin-tag.header`) inside the
  # editor-meta breadcrumb line, as parsed Floki nodes.
  defp header_tag(html) do
    html
    |> Floki.parse_fragment!()
    |> Floki.find(".editor-meta .source-origin-tag.header")
  end

  describe "class-definition tab package badge (BT-2642)" do
    test "a project class shows the bare project package name", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})

      tag = header_tag(html)
      assert tag != []
      assert Floki.attribute(tag, "class") |> List.first() =~ "project"
      # Project shows the bare package name (the new BT-2642 behaviour), not "DEP".
      assert Floki.text(tag) |> String.trim() == "exdura"
      assert Floki.attribute(tag, "title") |> List.first() == "Project: exdura"
    end

    test "a dependency class shows a DEP · <pkg> badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # BT-2661: the tree defaults to the Project filter, so switch to Deps to make
      # the dependency row interactive before opening it from the tree.
      view |> element("form.src-filter") |> render_change(%{"src" => "deps"})
      view |> element(~s(div[phx-value-class="HttpClient"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "HttpClient"})

      tag = header_tag(html)
      assert tag != []
      assert Floki.attribute(tag, "class") |> List.first() =~ "dependency"
      assert Floki.text(tag) |> String.trim() == "DEP · HTTP"
      assert Floki.attribute(tag, "title") |> List.first() == "Dependency: HTTP"
    end

    test "a stdlib class shows a STDLIB badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # BT-2661: default is Project — switch to Stdlib to open the stdlib row.
      view |> element("form.src-filter") |> render_change(%{"src" => "stdlib"})
      view |> element(~s(div[phx-value-class="Object"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Object"})

      tag = header_tag(html)
      assert tag != []
      assert Floki.attribute(tag, "class") |> List.first() =~ "stdlib"
      assert Floki.text(tag) |> String.trim() == "STDLIB"
      assert Floki.attribute(tag, "title") |> List.first() == "Standard library"
    end
  end

  describe "method tab package badge (BT-2642)" do
    test "a project method tab shows the project package name", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        render_click(view, "browser_select_method", %{
          "class" => "Counter",
          "side" => "instance",
          "selector" => "increment"
        })

      tag = header_tag(html)
      assert tag != []
      assert Floki.attribute(tag, "class") |> List.first() =~ "project"
      assert Floki.text(tag) |> String.trim() == "exdura"
    end

    test "a dependency method tab shows DEP · <pkg>", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # BT-2661: default is Project — switch to Deps to open the dependency row.
      view |> element("form.src-filter") |> render_change(%{"src" => "deps"})
      view |> element(~s(div[phx-value-class="HttpClient"])) |> render_click()

      html =
        render_click(view, "browser_select_method", %{
          "class" => "HttpClient",
          "side" => "instance",
          "selector" => "increment"
        })

      tag = header_tag(html)
      assert tag != []
      assert Floki.text(tag) |> String.trim() == "DEP · HTTP"
    end
  end

  describe "class tree unaffected (BT-2642)" do
    test "a project class still shows no badge in the class tree", %{conn: conn} do
      {:ok, _view, html} = live(owner_conn(conn), "/")

      tree_tag =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(div[phx-value-class="Counter"] .source-origin-tag))

      assert tree_tag == []
    end
  end
end
