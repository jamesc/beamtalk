# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceNewClassTest do
  @moduledoc """
  Integration test for the System Browser's New Class modal (BT-2645): the ＋
  affordance opens a modal/wizard with two explicit fields — a plain PascalCase
  class name and a superclass (default `Object`, a datalist typeahead over
  existing classes). The `<Superclass> subclass: <Name>` definition + the `.bt`
  path are synthesized server-side, so the owner never types `subclass:` or a file
  path. A valid create opens + selects the NEW class (not the superclass);
  invalid input (empty / lowercase / duplicate) renders an in-modal error with no
  round-trip and no method-editor banner.

  Drives the LiveView against the fully-stubbed workspace client
  (`BtAttachWeb.StubWorkspaceClient`), whose `new_class/2` returns the created
  path and whose `browse_class_definition/1` answers for any class name. No
  `:workspace` tag, so this runs in the bare `mix test` lane.
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

  defp open_modal(view) do
    view |> element(~s(button[phx-click="toggle_new_class"])) |> render_click()
  end

  describe "New Class modal affordance (BT-2645)" do
    test "the ＋ toggle is hidden for an Observer", %{conn: conn} do
      {:ok, _view, html} = live(observer_conn(conn), "/")
      refute html =~ ~s(phx-click="toggle_new_class")
      refute html =~ ~s(id="new-class-modal")
    end

    test "the ＋ toggle opens a modal with name + superclass fields", %{conn: conn} do
      {:ok, view, html} = live(owner_conn(conn), "/")

      # Closed by default — only the toggle shows.
      assert html =~ ~s(phx-click="toggle_new_class")
      refute html =~ ~s(id="new-class-modal")

      opened = open_modal(view)
      assert opened =~ ~s(id="new-class-modal")
      assert opened =~ ~s(role="dialog")
      assert opened =~ ~s(phx-submit="new_class")
      assert opened =~ ~s(name="name")
      assert opened =~ ~s(name="superclass")
      # Superclass defaults to `Object`.
      assert opened =~ ~s(value="Object")
      # A datalist typeahead over existing classes (no bare-definition textarea).
      assert opened =~ ~s(id="new-class-super-options")
      # The modal form itself has no `source` definition field — the def is
      # synthesized server-side. (`name="source"` still exists elsewhere as the
      # method editor's hidden field, so we scope the check to the modal form.)
      refute opened =~ ~s(id="new-class-source")
    end
  end

  describe "in-modal validation (BT-2645)" do
    test "an empty name is rejected inside the modal with no round-trip", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "", "superclass" => "Object"})

      assert html =~ "Enter a class name"
      assert html =~ ~s(id="new-class-modal")
      refute html =~ "Created new class"
    end

    test "a lowercase (non-PascalCase) name is rejected inside the modal", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "foo", "superclass" => "Object"})

      assert html =~ "PascalCase"
      assert html =~ ~s(id="new-class-modal")
      refute html =~ "Created new class"
    end

    test "a duplicate class name is rejected inside the modal", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      # `Counter` is always present in the stub's browse list.
      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "Counter", "superclass" => "Object"})

      assert html =~ "already exists"
      assert html =~ ~s(id="new-class-modal")
      refute html =~ "Created new class"
    end

    test "a validation error never reaches the method-editor save banner", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "foo", "superclass" => "Object"})

      # The error renders inside the modal (its `role="alert"` block), not in the
      # method editor's save-error area.
      assert html =~ ~s(id="new-class-error")
    end
  end

  describe "create + open the new class (BT-2645)" do
    test "default Object superclass creates the class and opens + selects it", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "Foo", "superclass" => "Object"})

      # `Foo` → `Object subclass: Foo` → `src/Foo.bt` (PascalCase basename; the
      # snake_case convention is deferred to BT-2646).
      assert html =~ "Created new class — src/Foo.bt"
      # The modal closes after a successful create.
      refute html =~ ~s(id="new-class-modal")
      # The created class is opened (a `def:Foo` tab) and selected in the tree.
      assert html =~ "def:Foo"
    end

    test "an explicit superclass opens the NEW class, not the superclass", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      # Superclass `Actor` → `Actor subclass: Bar`; the success path must open the
      # new class (`Bar`), never the superclass (`Actor`).
      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "Bar", "superclass" => "Actor"})

      assert html =~ "Created new class — src/Bar.bt"
      assert html =~ "def:Bar"
      refute html =~ "def:Actor"
    end

    test "a blank superclass falls back to Object", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")
      open_modal(view)

      # A cleared superclass field must not synthesize a headerless `subclass: Baz`;
      # it falls back to `Object` and still creates.
      html =
        view
        |> form("#new-class-form")
        |> render_submit(%{"name" => "Baz", "superclass" => "  "})

      assert html =~ "Created new class — src/Baz.bt"
      assert html =~ "def:Baz"
    end
  end
end
