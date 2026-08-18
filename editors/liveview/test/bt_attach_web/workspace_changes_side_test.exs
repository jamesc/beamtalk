# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceChangesSideTest do
  @moduledoc """
  Changes-pane Kind/Side column + row-toggle independence (BT-3195).

  Follow-up from BT-3187 (ADR 0112 Phase 3, `removeSelector:` ChangeLog
  entries + `side` field): the `(class, selector, side)` shadow-key fix
  (`beamtalk_workspace_changelog:shadow_key/1`) newly allows an instance-side
  patch and a class-side patch/removal of the *same selector name* to both be
  active pending ChangeLog rows simultaneously. Before that fix, only one
  entry per `(class, selector)` could ever be active, so two rows never
  shared a key — this scenario, and the ambiguity it creates, are new.

  Driven through the full LiveView stack against the stubbed workspace
  client (`StubWorkspaceClient.seed_change_row/1`, BT-3195) — a same-selector
  instance-side/class-side pair can't be expressed via `save_method/3`'s
  `{class, selector}`-keyed write path, which structurally holds only one
  entry per selector, regardless of side.
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

  # A row's Kind cell sits right after its Class/Selector cells in the same
  # `<tr>` (workspace_live.ex's Changes-pane table); matching non-greedily
  # from the class name to the Kind cell ties the assertion to the *correct*
  # row rather than any row sharing the same selector.
  defp kind_cell_for(html, class, selector, kind) do
    html =~
      ~r/<td class="k">#{Regex.escape(class)}<\/td>\s*<td>#{Regex.escape(selector)}<\/td>\s*<td>#{Regex.escape(kind)}<\/td>/
  end

  describe "Kind/Side column (BT-3195)" do
    test "an instance-side and class-side row for the same selector show distinct Kind cells and toggle independently",
         %{conn: conn} do
      class = "Bt3195Sides#{System.unique_integer([:positive])}"

      instance_diff = " foo => 1\n-  self.value\n+  NINETY_NINE_MARKER"
      class_diff = " class foo => 2\n-  self.value\n+  ONE_HUNDRED_MARKER"

      # Two ChangeLog rows sharing `(class, "foo")`, differing only by side —
      # exactly the scenario BT-3187's `(class, selector, side)` shadow-key fix
      # newly allows to coexist (previously impossible: only one entry per
      # `(class, selector)` could ever be active).
      StubWorkspaceClient.seed_change_row(%{
        class: class,
        selector: "foo",
        kind: "instance",
        side: "instance",
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "human",
        diff: instance_diff
      })

      StubWorkspaceClient.seed_change_row(%{
        class: class,
        selector: "foo",
        kind: "class",
        side: "class",
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "human",
        diff: class_diff
      })

      {:ok, view, _html} = live(owner_conn(conn), "/")
      changes_html = render_click(view, "dock_tab", %{"tab" => "changes"})

      # (1) Visually distinguishable: the Kind column renders "instance" for
      # one row and "class" for the other, even though Class/Selector are
      # identical on both.
      assert kind_cell_for(changes_html, class, "foo", "instance")
      assert kind_cell_for(changes_html, class, "foo", "class")

      instance_caret =
        ~s(button[phx-click="toggle_change_diff"][phx-value-class="#{class}"][phx-value-selector="foo"][phx-value-entry-side="instance"])

      class_caret =
        ~s(button[phx-click="toggle_change_diff"][phx-value-class="#{class}"][phx-value-selector="foo"][phx-value-entry-side="class"])

      # Two distinct carets exist (not one shared control) — both collapsed
      # by default.
      assert has_element?(view, instance_caret <> ~s([aria-expanded="false"]))
      assert has_element?(view, class_caret <> ~s([aria-expanded="false"]))

      # (2) Independently expandable: expanding the instance-side row's diff
      # must NOT flip the class-side row's toggle. Before BT-3195, both rows
      # keyed on `{class, selector}` alone, so this click would have expanded
      # both.
      after_instance_expand = view |> element(instance_caret) |> render_click()

      assert has_element?(view, instance_caret <> ~s([aria-expanded="true"]))
      assert has_element?(view, class_caret <> ~s([aria-expanded="false"]))
      assert after_instance_expand =~ "NINETY_NINE_MARKER"
      refute after_instance_expand =~ "ONE_HUNDRED_MARKER"

      # Expanding the class-side row too: both now independently expanded,
      # each showing its own diff.
      after_class_expand = view |> element(class_caret) |> render_click()
      assert has_element?(view, instance_caret <> ~s([aria-expanded="true"]))
      assert has_element?(view, class_caret <> ~s([aria-expanded="true"]))
      assert after_class_expand =~ "ONE_HUNDRED_MARKER"

      # (3) Collapsing the instance-side row again must leave the class-side
      # row's (still-expanded) toggle untouched — the definitive proof the two
      # rows no longer share one MapSet key.
      after_instance_collapse = view |> element(instance_caret) |> render_click()
      assert has_element?(view, instance_caret <> ~s([aria-expanded="false"]))
      assert has_element?(view, class_caret <> ~s([aria-expanded="true"]))
      refute after_instance_collapse =~ "NINETY_NINE_MARKER"
      assert after_instance_collapse =~ "ONE_HUNDRED_MARKER"
    end

    test "a remove-method row's Kind cell names its side; a new-class row's names neither",
         %{conn: conn} do
      class = "Bt3195Kinds#{System.unique_integer([:positive])}"

      # `remove-method` never carries a diff (`method_delta/1`'s catch-all,
      # `beamtalk_workspace_changelog.erl`) — its Kind cell is the only visual
      # cue distinguishing it from an instance/class-side patch of the same
      # selector, so this asserts that label directly rather than through the
      # (absent) diff-toggle caret.
      StubWorkspaceClient.seed_change_row(%{
        class: class,
        selector: "bar",
        kind: "remove-method",
        side: "class",
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "human",
        diff: nil
      })

      # A new-class row carries no selector and no side (ADR 0112: side is
      # meaningful only for a method-table target).
      StubWorkspaceClient.seed_change_row(%{
        class: class,
        selector: "(class)",
        kind: "new-class",
        side: nil,
        intent: "durable",
        flushable: true,
        flushed: false,
        author_kind: "human",
        diff: nil
      })

      {:ok, view, _html} = live(owner_conn(conn), "/")
      changes_html = render_click(view, "dock_tab", %{"tab" => "changes"})

      assert kind_cell_for(changes_html, class, "bar", "remove (class)")
      assert kind_cell_for(changes_html, class, "(class)", "new class")

      # Neither row carries a diff, so neither gets a disclosure caret.
      refute has_element?(
               view,
               ~s(button[phx-click="toggle_change_diff"][phx-value-class="#{class}"][phx-value-selector="bar"])
             )

      refute has_element?(
               view,
               "button[phx-click=\"toggle_change_diff\"][phx-value-class=\"#{class}\"][phx-value-selector=\"(class)\"]"
             )
    end
  end
end
