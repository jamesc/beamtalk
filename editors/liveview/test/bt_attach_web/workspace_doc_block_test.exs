# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceDocBlockTest do
  @moduledoc """
  Integration test for the System Browser doc block (BT-2558): selecting a method
  surfaces its signature + rendered `///` doc-comment, and opening a class
  definition surfaces the class comment — both as a read-only documentation block
  distinct from the editable source body.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `browse_method_source` /
  `browse_class_definition` carry the doc/signature/comment fields the real ops
  added. No `:workspace` tag, so this runs in the bare `mix test` lane.
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

  describe "method doc block (BT-2558)" do
    test "selecting a method shows its signature and rendered doc-comment", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Select Counter (always present in the stub), then open the `increment`
      # method — the stub carries a signature and a Markdown `///` doc for it.
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        view
        |> element(~s(div[phx-value-selector="increment"]))
        |> render_click()

      # The doc block is present, distinct from the editable source.
      assert html =~ ~s(class="doc-block")
      # Signature (HEEx-escaped `->`).
      assert html =~ "increment -&gt; Counter"
      # The doc body, rendered from Markdown: prose, a heading, and fenced code.
      assert html =~ "Increment the counter by one."
      assert html =~ ~s(<h4 class="doc-h">Examples</h4>)
      assert html =~ "<code>c increment</code>"
    end

    test "a method with no doc-comment shows only its signature", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        view
        |> element(~s(div[phx-value-selector="value"]))
        |> render_click()

      assert html =~ ~s(class="doc-block")
      assert html =~ "value -&gt; Integer"
      # No `///` doc → no rendered doc body.
      refute html =~ ~s(class="doc-body")
    end
  end

  describe "class-definition doc block (BT-2558)" do
    test "opening a class definition renders the class comment as docs", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # "+ def" opens the active class' (Counter) definition tab, which reads the
      # class comment for its doc block.
      html =
        view
        |> element(~s(button[phx-click="open_definition"]))
        |> render_click()

      assert html =~ ~s(class="doc-block")
      assert html =~ "The Counter class."
      assert html =~ ~s(<h4 class="doc-h">Overview</h4>)
    end

    test "opening a class definition seeds the editor with the class skeleton",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html =
        view
        |> element(~s(button[phx-click="open_definition"]))
        |> render_click()

      # The editable body (distinct from the doc block) carries the class
      # skeleton the browse op returns — not an empty editor.
      assert html =~ "Object subclass: Counter"
    end
  end
end
