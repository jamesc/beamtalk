# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceGroupedMethodsTest do
  @moduledoc """
  Integration test for the System Browser's divider-grouped method view
  (BT-3238): grouping a class's methods by `// === Name ===` section
  divider (`browse-categories`), the group-mode toggle, and the file-level
  section-authoring affordance (`save-section`, add/rename).

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`); `Counter`'s divider categories are
  seeded per test via `seed_categories/2`. No `:workspace` tag, so this runs
  in the bare `mix test` lane.
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

  describe "no dividers (BT-2601 unaffected case)" do
    test "a divider-free class shows no group-mode toggle and renders the protocol view",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_select_class", %{"class" => "Counter"})

      refute html =~ ~s(phx-click="browser_group_mode")
      # The pre-existing protocol filter row still renders unchanged.
      assert html =~ ~s(phx-click="browser_select_protocol")
      assert html =~ "increment"
    end
  end

  describe "grouped method view (BT-3238)" do
    setup do
      BtAttachWeb.StubWorkspaceClient.seed_categories("Counter", %{
        "has_dividers" => true,
        "categories" => [
          %{
            "name" => nil,
            "methods" => [%{"selector" => "value", "side" => "instance"}]
          },
          %{
            "name" => "Arithmetic",
            "methods" => [%{"selector" => "increment", "side" => "instance"}]
          }
        ]
      })

      :ok
    end

    test "the group-mode toggle appears once the class has dividers", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_select_class", %{"class" => "Counter"})

      assert html =~ ~s(phx-click="browser_group_mode")
      assert html =~ "Sections"
    end

    test "switching to section mode groups methods under their divider name", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      html = render_click(view, "browser_group_mode", %{"mode" => "section"})

      assert html =~ "Arithmetic"
      assert html =~ "increment"
      # The implicit leading (unnamed) group's method still renders, with no
      # section header of its own.
      assert html =~ "value"
    end

    test "an owner sees a rename affordance on a named section", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      html = render_click(view, "browser_group_mode", %{"mode" => "section"})

      assert html =~ ~s(phx-value-name="Arithmetic")
    end

    test "renaming a section dispatches save-section with old_name/new_name", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      render_click(view, "browser_group_mode", %{"mode" => "section"})
      html = render_click(view, "browser_edit_section", %{"name" => "Arithmetic"})

      assert html =~ ~s(phx-submit="browser_rename_section")

      html =
        render_submit(view, "browser_rename_section", %{
          "old_name" => "Arithmetic",
          "new_name" => "Math"
        })

      assert [{:save_section, "Counter", "Math", opts} | _] =
               BtAttachWeb.StubWorkspaceClient.calls()

      assert Keyword.get(opts, :old_name) == "Arithmetic"
      # The form closes on a successful save.
      refute html =~ ~s(phx-submit="browser_rename_section")
    end

    test "a successful rename keeps the view in section mode", %{conn: conn} do
      # Review finding: the post-save refresh used to call the same
      # class-change reset path as `browser_select_class`, which
      # unconditionally reset `browser_group_mode` back to "protocol" —
      # kicking the viewer out of the Sections view they were just using.
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      render_click(view, "browser_group_mode", %{"mode" => "section"})
      render_click(view, "browser_edit_section", %{"name" => "Arithmetic"})

      html =
        render_submit(view, "browser_rename_section", %{
          "old_name" => "Arithmetic",
          "new_name" => "Math"
        })

      # Still rendering the section-mode method list (`sb-sections`), not
      # kicked back to the protocol filter row (`sb-protocols`).
      assert html =~ "sb-sections"
      refute html =~ "sb-protocols"
    end

    test "adding a new section dispatches save-section with before_selector", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      html = render_click(view, "browser_edit_section", %{"name" => ""})

      assert html =~ ~s(phx-submit="browser_add_section")

      render_submit(view, "browser_add_section", %{
        "new_name" => "New Section",
        "before_selector" => "increment",
        "before_side" => "instance"
      })

      assert [{:save_section, "Counter", "New Section", opts} | _] =
               BtAttachWeb.StubWorkspaceClient.calls()

      assert Keyword.get(opts, :before_selector) == "increment"
    end

    test "the add-section dropdown excludes a category's own first method", %{conn: conn} do
      # `increment` is `Arithmetic`'s only (hence first) method — inserting a
      # new divider directly above it would write two dividers back-to-back
      # and silently orphan `Arithmetic`'s own. `value` is in the unnamed
      # leading group, so it stays a valid insertion point.
      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      html = render_click(view, "browser_edit_section", %{"name" => ""})

      refute html =~ ~s(value="increment")
      assert html =~ ~s(value="value")
    end

    test "a failed save surfaces an inline error and keeps the form open", %{conn: conn} do
      BtAttachWeb.StubWorkspaceClient.set_section_save({:error, :unreachable})

      {:ok, view, _html} = live(owner_conn(conn), "/")

      render_click(view, "browser_select_class", %{"class" => "Counter"})
      render_click(view, "browser_group_mode", %{"mode" => "section"})
      render_click(view, "browser_edit_section", %{"name" => "Arithmetic"})

      html =
        render_submit(view, "browser_rename_section", %{
          "old_name" => "Arithmetic",
          "new_name" => "Math"
        })

      # The form stays open with an inline error, rather than silently
      # discarding the failed rename.
      assert html =~ ~s(phx-submit="browser_rename_section")
      assert html =~ "role=\"alert\""
    end
  end
end
