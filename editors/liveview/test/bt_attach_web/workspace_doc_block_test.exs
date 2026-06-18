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

  defp observer_conn(conn) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "bob", "groups" => ["beamtalk-observers"]},
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
      # Signature (HEEx-escaped `->`) — always visible as the collapse toggle.
      assert html =~ "increment -&gt; Counter"
      # Collapsed by default (BT-2558): the rendered body (which is also present
      # verbatim in the editable source) is hidden until expanded, so the docs
      # aren't shown twice and don't crowd the editor.
      refute html =~ ~s(class="doc-body")

      # Expanding the block reveals the rendered Markdown: prose, a heading, fenced
      # code. The signature line doubles as the toggle (phx-click="toggle_doc").
      html = view |> element(~s(button[phx-click="toggle_doc"])) |> render_click()

      assert html =~ ~s(class="doc-body")
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

    test "the expanded state is sticky across tab switches", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      # Expand the doc'd method's block.
      html = view |> element(~s(button[phx-click="toggle_doc"])) |> render_click()
      assert html =~ ~s(class="doc-body")

      # Switching to a method with no doc collapses to a plain signature (nothing
      # to expand) but must not reset the preference…
      html = view |> element(~s(div[phx-value-selector="value"])) |> render_click()
      refute html =~ ~s(class="doc-body")

      # …so returning to the doc'd method shows the body again *without* re-toggling.
      html = view |> element(~s(div[phx-value-selector="increment"])) |> render_click()
      assert html =~ ~s(class="doc-body")
      assert html =~ "Increment the counter by one."
    end
  end

  describe "doc block visibility by role (BT-2558)" do
    test "an observer sees the read-only doc block", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      # The doc block rides the `:read`-capability browse ops, so it renders for
      # an observer even though the editable source form below does not.
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        view
        |> element(~s(div[phx-value-selector="increment"]))
        |> render_click()

      assert html =~ ~s(class="doc-block")
      assert html =~ "increment -&gt; Counter"
      # The toggle is present and expands the body for an observer too.
      html = view |> element(~s(button[phx-click="toggle_doc"])) |> render_click()
      assert html =~ ~s(class="doc-body")
      assert html =~ "Increment the counter by one."
    end
  end

  describe "class-definition doc block (BT-2558)" do
    test "opening a class definition renders the class comment as docs", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Select Counter, then open its definition via the System Browser's "class
      # definition" entry — it reads the class comment for the doc block. (The
      # editor now opens with no tab, so there is no "+ def" until one is open.)
      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        view
        |> element(~s([phx-click="browser_open_definition"]))
        |> render_click()

      assert html =~ ~s(class="doc-block")
      # A class-definition tab has no signature, so the toggle reads "Class comment".
      assert html =~ "Class comment"

      # Expand to reveal the rendered class comment (collapsed by default, BT-2558).
      html = view |> element(~s(button[phx-click="toggle_doc"])) |> render_click()

      assert html =~ "The Counter class."
      assert html =~ ~s(<h4 class="doc-h">Overview</h4>)
    end

    test "opening a class definition seeds the editor with the class skeleton",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        view
        |> element(~s([phx-click="browser_open_definition"]))
        |> render_click()

      # The editable body (distinct from the doc block) carries the class
      # skeleton the browse op returns — not an empty editor.
      assert html =~ "Object subclass: Counter"
    end
  end
end
