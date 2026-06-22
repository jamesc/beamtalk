# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceDismissNoticeTest do
  @moduledoc """
  Tests for user-dismissable `io-block` status notices (BT-2612).

  Every colored status banner in the cockpit (err / ok / warn / plain) now
  carries a keyboard-accessible `×` dismiss control. Dismissing fires a
  `dismiss_notice` (or a targeted nested event) that clears the backing assign so
  the banner does not reappear on the next render.

  These exercise at least one notice per category (err / ok / warn), assert the
  notice clears, and lock in the security boundary: a crafted dismiss key is a
  no-op (the handler maps known strings through a whitelist and NEVER calls
  `String.to_atom/1` on user input). Driven through the full LiveView stack with
  the fully-stubbed workspace client — no `:workspace` tag, so it runs in the
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

  describe "every io-block notice renders a dismiss control" do
    test "an error notice (err) renders a keyboard-accessible × with an aria-label", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # An empty class name on save is a local validation error → `save_error`
      # (the `io-block err` banner under the editor).
      html =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "",
          "selector" => "increment",
          "source" => "increment => self",
          "tab" => "method:Counter:instance:increment"
        })

      assert html =~ "Enter a class name"
      # The dismiss control is a button keyed to clear `save_error`, with an
      # aria-label for keyboard/AT users.
      assert html =~ ~s(aria-label="Dismiss notification")
      assert html =~ ~s(phx-value-key="save_error")
    end
  end

  describe "dismissing clears the backing assign (err / ok / warn)" do
    test "err: dismissing save_error clears it", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      shown =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "",
          "selector" => "increment",
          "source" => "increment => self",
          "tab" => "method:Counter:instance:increment"
        })

      assert shown =~ "Enter a class name"

      # Click the × keyed to save_error.
      after_dismiss =
        view
        |> element(~s(button[phx-click="dismiss_notice"][phx-value-key="save_error"]))
        |> render_click()

      refute after_dismiss =~ "Enter a class name"
    end

    test "ok: dismissing save_result clears it", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      view |> element(~s(div[phx-value-class="Counter"])) |> render_click()
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      saved =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "Counter",
          "selector" => "increment",
          "source" => "increment => self.value := self.value + 1",
          "tab" => "method:Counter:instance:increment"
        })

      assert saved =~ "Saved increment on Counter"

      after_dismiss =
        view
        |> element(~s(button[phx-click="dismiss_notice"][phx-value-key="save_result"]))
        |> render_click()

      refute after_dismiss =~ "Saved increment on Counter"
    end

    test "warn: dismissing flush_result clears it", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      # Create a flushable class, then save a divergent body so a flush has work
      # to report (the warn `flush_result` banner: "Flushed …").
      suffix = System.unique_integer([:positive])
      class = "DismissWarn#{suffix}"

      class_src = """
      Actor subclass: #{class}
        state: value = 0

        increment => self.value := self.value + 1

        value => self.value
      """

      view |> form("#eval-form") |> render_submit(%{expr: class_src})

      {:ok, view, _html} = live(owner_conn(conn), "/")
      view |> element(~s(div[phx-value-class="#{class}"])) |> render_click()
      view |> element(~s(div[phx-value-selector="increment"])) |> render_click()

      view
      |> form("form[phx-submit='save_method']")
      |> render_submit(%{
        "class" => class,
        "selector" => "increment",
        "source" => "increment => self.value := self.value + 2",
        "tab" => "method:#{class}:instance:increment"
      })

      flushed =
        view
        |> element("button[phx-click='flush']")
        |> render_click()

      assert flushed =~ "Flushed"

      after_dismiss =
        view
        |> element(~s(button[phx-click="dismiss_notice"][phx-value-key="flush_result"]))
        |> render_click()

      refute after_dismiss =~ "Flushed"
    end
  end

  describe "security: dismiss keys are whitelisted, never atomized" do
    test "an unknown dismiss key is a no-op and creates no atom", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      shown =
        view
        |> form("form[phx-submit='save_method']")
        |> render_submit(%{
          "class" => "",
          "selector" => "increment",
          "source" => "increment => self",
          "tab" => "method:Counter:instance:increment"
        })

      assert shown =~ "Enter a class name"

      # A crafted key that is not on the whitelist must NOT clear anything and
      # must NOT crash the LiveView (no `String.to_atom/1` on user input).
      crafted = "bt_2612_definitely_not_an_existing_atom_#{System.unique_integer([:positive])}"
      after_evil = render_click(view, "dismiss_notice", %{"key" => crafted})

      # The real banner is untouched, and the crafted string never became an atom.
      assert after_evil =~ "Enter a class name"

      assert_raise ArgumentError, fn -> String.to_existing_atom(crafted) end

      # The legitimate dismiss still works afterwards.
      cleared =
        view
        |> element(~s(button[phx-click="dismiss_notice"][phx-value-key="save_error"]))
        |> render_click()

      refute cleared =~ "Enter a class name"
    end
  end
end
