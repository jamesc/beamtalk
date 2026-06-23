# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceOriginBadgeTest do
  @moduledoc """
  Integration test for the System Browser's source-origin badges (BT-2641): the
  class-tree rows surface a `.source-origin-tag` for non-project classes. A
  dependency class reads as a recognizable "DEP" marker (parity with the generic
  "STDLIB" marker), suffixed with the package name when known ("DEP · HTTP") and
  bare ("DEP") when the package is absent. The tooltip keeps the longhand
  "Dependency: <pkg>".

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_classes` reports `HttpClient`
  as a dependency in package `HTTP` and `Orphan` as a dependency with no package.
  No `:workspace` tag, so this runs in the bare `mix test` lane.
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

  describe "class-tree source-origin badges (BT-2641)" do
    test "a dependency class with a known package shows a DEP · <pkg> badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      # BT-2591: the class tree loads off the mount (`start_async(:mount_load, …)`),
      # so await the async fold before parsing the populated tree.
      render_async(view, 5_000)

      # BT-2661: the tree now defaults to the Project origin filter, so switch to
      # the Deps filter to render the dependency rows this badge test asserts on.
      html =
        view
        |> element("form.src-filter")
        |> render_change(%{"src" => "deps"})

      tag =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(div[phx-value-class="HttpClient"] .source-origin-tag))

      assert tag != []
      assert Floki.attribute(tag, "class") |> List.first() =~ "dependency"
      # The dependency badge reads as "DEP · HTTP" (parity with STDLIB), not the
      # bare package name.
      assert Floki.text(tag) |> String.trim() == "DEP · HTTP"
      # The tooltip keeps the longhand "Dependency: <pkg>".
      assert Floki.attribute(tag, "title") |> List.first() == "Dependency: HTTP"
    end

    test "a dependency class with no package falls back to a plain DEP badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      render_async(view, 5_000)

      # BT-2661: default is now Project — switch to Deps to render the dep rows.
      html =
        view
        |> element("form.src-filter")
        |> render_change(%{"src" => "deps"})

      tag =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(div[phx-value-class="Orphan"] .source-origin-tag))

      assert tag != []
      assert Floki.text(tag) |> String.trim() == "DEP"
      # The package degrades to "unknown" in the tooltip so it never renders blank.
      assert Floki.attribute(tag, "title") |> List.first() == "Dependency: unknown"
    end

    test "a project class shows no source-origin badge", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      html = render_async(view, 5_000)

      # The row IS present in the tree (the async load has folded the classes in),
      # but a project class carries no source-origin badge.
      assert html =~ ~s(phx-value-class="Counter")

      tag =
        html
        |> Floki.parse_fragment!()
        |> Floki.find(~s(div[phx-value-class="Counter"] .source-origin-tag))

      assert tag == []
    end
  end
end
