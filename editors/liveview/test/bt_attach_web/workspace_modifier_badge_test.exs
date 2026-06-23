# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceModifierBadgeTest do
  @moduledoc """
  Integration test for the editor-header class/method modifier badges (BT-2605):
  the breadcrumb header surfaces a class-side method's `Class` badge plus the
  class-level modifiers (`Sealed`, `Typed`, `Abstract`, `Native`) as colored
  pills, matching the API-docs' badge labels.

  The modifiers are *reflected* booleans from `browse-class-definition` (op 4) —
  the IDE does not parse them from the synthesized `definition` skeleton (which
  carries no leading modifier keywords). Drives the LiveView against the
  fully-stubbed workspace client (`BtAttachWeb.StubWorkspaceClient`), whose
  `browse_class_definition` reports `Ledger` as `sealed`, `Vector` as `typed`,
  `Shape` as `abstract`, and `Subprocess` as `native:`; `Counter` is a plain
  class. No `:workspace` tag,
  so this runs in the bare `mix test` lane.
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

  describe "class-level modifier badges (BT-2605)" do
    test "a class-definition tab badges a sealed class", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Ledger"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Ledger"})

      assert html =~ ~s(class="modifier-tag sealed")
      assert html =~ "Sealed"
      # The badge names the modifier for assistive tech.
      assert html =~ "Cannot be subclassed by user code"
      # Sealed-only: no abstract/native badge.
      refute html =~ ~s(class="modifier-tag abstract")
      refute html =~ ~s(class="modifier-tag native")
    end

    test "an abstract class badges Abstract", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_open_definition", %{"class" => "Shape"})

      assert html =~ ~s(class="modifier-tag abstract")
      assert html =~ "Abstract"
      assert html =~ "Must be subclassed"
      refute html =~ ~s(class="modifier-tag sealed")
    end

    test "a typed class badges Typed", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      html = render_click(view, "browser_open_definition", %{"class" => "Vector"})

      assert html =~ ~s(class="modifier-tag typed")
      assert html =~ "Typed"
      assert html =~ "All fields and methods require type annotations"
      refute html =~ ~s(class="modifier-tag sealed")
      refute html =~ ~s(class="modifier-tag abstract")
    end

    test "a plain class shows no class-modifier badges", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Counter"})

      # Counter is reported with neither sealed, typed nor abstract.
      refute html =~ ~s(class="modifier-tag sealed")
      refute html =~ ~s(class="modifier-tag typed")
      refute html =~ ~s(class="modifier-tag abstract")
      refute html =~ ~s(class="modifier-tag native")
    end

    test "a native: class badges Native on its definition tab", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Subprocess"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Subprocess"})

      assert html =~ ~s(class="modifier-tag native")
      assert html =~ "Native"
      assert html =~ "Backed by an Erlang module"
    end

    test "a transient class-definition fetch failure keeps the prior badges", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Open the sealed Ledger definition — the Sealed badge shows.
      html = render_click(view, "browser_open_definition", %{"class" => "Ledger"})
      assert html =~ ~s(class="modifier-tag sealed")

      # Re-opening re-fetches the class header; simulate the workspace going
      # unreachable for that re-fetch. The badge must NOT vanish — a `nil` modifier
      # result is the failure sentinel, so the editor keeps the prior reflected
      # list rather than clearing to "no modifiers" (BT-2605 review).
      BtAttachWeb.StubWorkspaceClient.fail_class_definition(true)
      html = render_click(view, "browser_open_definition", %{"class" => "Ledger"})
      assert html =~ ~s(class="modifier-tag sealed")
    end
  end

  describe "method-side + class-modifier badges (BT-2605)" do
    test "a class-side method badges Class plus the owning class's modifiers", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Ledger"])) |> render_click()

      # Open a class-side method on the sealed Ledger class.
      html =
        render_click(view, "browser_select_method", %{
          "class" => "Ledger",
          "side" => "class",
          "selector" => "increment"
        })

      # The class-side badge…
      assert html =~ ~s(class="modifier-tag side")
      assert html =~ "Class-side method"
      # …rides alongside the inherited class modifier (Sealed) — class modifiers
      # show on method tabs too, not just class-definition tabs.
      assert html =~ ~s(class="modifier-tag sealed")
    end

    test "an instance-side method on a plain class shows no badges", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()

      html =
        render_click(view, "browser_select_method", %{
          "class" => "Counter",
          "side" => "instance",
          "selector" => "increment"
        })

      refute html =~ ~s(class="modifier-tag side")
      refute html =~ ~s(class="modifier-tag sealed")
      refute html =~ ~s(class="modifier-tag abstract")
      refute html =~ ~s(class="modifier-tag native")
    end
  end

  describe "badge visibility by role (BT-2605)" do
    test "an observer sees the modifier badges too", %{conn: conn} do
      {:ok, view, _html} = live(observer_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Ledger"])) |> render_click()
      html = render_click(view, "browser_open_definition", %{"class" => "Ledger"})

      assert html =~ ~s(class="modifier-tag sealed")
    end
  end
end
