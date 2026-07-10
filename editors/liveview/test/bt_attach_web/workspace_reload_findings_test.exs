# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceReloadFindingsTest do
  @moduledoc """
  Integration tests for BT-2779 (ADR 0105 Phase 1): the workspace/cockpit UI's
  reload-induced findings panel (Changes tab, "Reload Checks" section). Driven
  through the full LiveView stack against the fully-stubbed workspace client
  (`StubWorkspaceClient`). No `:workspace` tag, so this runs in the bare
  `mix test` lane.

  Covers:

    * the cockpit subscribes to the `reload_check` push stream on connect,
      mirroring `subscribe_classes` (AC: surface parity plumbing).
    * the initial mount reads the current `reload_findings` snapshot.
    * a `ReloadCheckCompleted` push applies clearing-by-replacement: an
      owner listed in `checkedOwners` has its findings replaced wholesale —
      including replaced with nothing, which is how a stale finding clears
      without anyone editing the caller (reload-fixes-reload).
    * a malformed/unexpected push does not crash the LiveView.
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

  defp eventually(fun), do: eventually(fun, 20)
  defp eventually(fun, 0), do: fun.()

  defp eventually(fun, retries) do
    if fun.() do
      true
    else
      Process.sleep(50)
      eventually(fun, retries - 1)
    end
  end

  defp finding(owner, opts \\ []) do
    %{
      owner: owner,
      changed_class: Keyword.get(opts, :changed_class, "Counter"),
      selector: Keyword.get(opts, :selector, "getCount"),
      classification: Keyword.get(opts, :classification, :signature_change),
      severity: Keyword.get(opts, :severity, "warning"),
      category: Keyword.get(opts, :category, "Dnu"),
      message: Keyword.get(opts, :message, "String does not understand '+'"),
      note: Keyword.get(opts, :note),
      sites: Keyword.get(opts, :sites, [%{method: "refresh", line: 14}])
    }
  end

  defp reload_check_event(opts) do
    %{
      changedClass: Keyword.get(opts, :changed_class, "Counter"),
      changedSelector: Keyword.get(opts, :selector, "getCount"),
      classification: Keyword.get(opts, :classification, :signature_change),
      checked: Keyword.get(opts, :checked, 1),
      notChecked: Keyword.get(opts, :not_checked, 0),
      capNote: Keyword.get(opts, :cap_note),
      checkedOwners: Keyword.fetch!(opts, :checked_owners),
      findings: Keyword.get(opts, :findings, [])
    }
  end

  describe "subscribe_reload_check wired at mount" do
    test "the cockpit subscribes to the reload-check push stream on connect", %{conn: conn} do
      {:ok, _view, _html} = live(owner_conn(conn), "/")

      assert eventually(fn ->
               Enum.any?(StubWorkspaceClient.calls(), &(elem(&1, 0) == :subscribe_reload_check))
             end)
    end
  end

  describe "initial mount snapshot" do
    test "the panel renders findings seeded before mount", %{conn: conn} do
      StubWorkspaceClient.put_reload_findings([
        %{
          owner: "Dashboard",
          changed_class: "Counter",
          selector: "getCount",
          classification: "signature_change",
          severity: "warning",
          category: "Dnu",
          message: "String does not understand '+'",
          note: nil,
          sites: [%{method: "refresh", line: 14}]
        }
      ])

      {:ok, _view, html} = live(owner_conn(conn), "/")

      assert html =~ "Dashboard"
      assert html =~ "String does not understand"
    end

    test "the panel shows the empty state when there are no findings", %{conn: conn} do
      {:ok, _view, html} = live(owner_conn(conn), "/")

      assert html =~ "No reload-induced findings"
    end
  end

  describe "clearing-by-replacement (ADR 0105 Mechanism step 4)" do
    test "a ReloadCheckCompleted push adds a new stale finding", %{conn: conn} do
      {:ok, view, html} = live(owner_conn(conn), "/")
      refute html =~ "Dashboard"

      event =
        reload_check_event(
          checked_owners: ["Dashboard"],
          findings: [finding("Dashboard")]
        )

      send(view.pid, {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, event})

      assert eventually(fn -> render(view) =~ "Dashboard" end)
      assert render(view) =~ "String does not understand"
    end

    test "a clean re-check clears a previously-stale finding for the same owner (reload-fixes-reload)",
         %{conn: conn} do
      StubWorkspaceClient.put_reload_findings([
        %{
          owner: "Dashboard",
          changed_class: "Counter",
          selector: "getCount",
          classification: "signature_change",
          severity: "warning",
          category: "Dnu",
          message: "String does not understand '+'",
          note: nil,
          sites: [%{method: "refresh", line: 14}]
        }
      ])

      {:ok, view, html} = live(owner_conn(conn), "/")
      assert html =~ "Dashboard"

      # Reload B re-checks Dashboard against Counter's restored signature and
      # finds it clean: `checkedOwners` still lists Dashboard, but `findings`
      # no longer contains an entry for it — the finding must disappear
      # without anyone editing Dashboard.
      event = reload_check_event(checked_owners: ["Dashboard"], findings: [])
      send(view.pid, {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, event})

      assert eventually(fn -> not (render(view) =~ "Dashboard") end)
      assert render(view) =~ "No reload-induced findings"
    end

    test "supersession replaces generation-A findings with generation-B rather than accumulating",
         %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      event_a =
        reload_check_event(
          checked_owners: ["Dashboard"],
          findings: [finding("Dashboard", message: "generation A message")]
        )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, event_a}
      )

      assert eventually(fn -> render(view) =~ "generation A message" end)

      event_b =
        reload_check_event(
          checked_owners: ["Dashboard"],
          findings: [finding("Dashboard", message: "generation B message")]
        )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, event_b}
      )

      assert eventually(fn -> render(view) =~ "generation B message" end)
      refute render(view) =~ "generation A message"
    end

    test "a caller broken by two independently-reloading classes keeps both findings until each is fixed on its own schedule",
         %{conn: conn} do
      # Dashboard calls both Counter>>getCount and Widget>>size; both go
      # stale in SEPARATE reloads. Clearing-by-replacement must be scoped
      # per (owner, changed_class) — not per owner alone — or fixing the
      # Counter-caused finding would silently discard the still-valid
      # Widget-caused one (the CRITICAL data-loss case).
      {:ok, view, _html} = live(owner_conn(conn), "/")

      counter_finding =
        finding("Dashboard", changed_class: "Counter", message: "counter finding")

      widget_finding =
        finding("Dashboard", changed_class: "Widget", selector: "size", message: "widget finding")

      counter_event =
        reload_check_event(
          changed_class: "Counter",
          checked_owners: ["Dashboard"],
          findings: [counter_finding]
        )

      widget_event =
        reload_check_event(
          changed_class: "Widget",
          selector: "size",
          checked_owners: ["Dashboard"],
          findings: [widget_finding]
        )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, counter_event}
      )

      assert eventually(fn -> render(view) =~ "counter finding" end)

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, widget_event}
      )

      assert eventually(fn -> render(view) =~ "widget finding" end)
      # Both must be present at once — Widget's reload must NOT have
      # discarded Counter's still-valid finding.
      assert render(view) =~ "counter finding"

      # Now Counter is fixed (a clean re-check): only the Counter-origin
      # finding clears; Widget's must survive.
      counter_clean_event =
        reload_check_event(changed_class: "Counter", checked_owners: ["Dashboard"], findings: [])

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, counter_clean_event}
      )

      assert eventually(fn -> not (render(view) =~ "counter finding") end)
      assert render(view) =~ "widget finding"
    end

    test "an untouched owner's findings survive an unrelated reload's push", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      event =
        reload_check_event(
          checked_owners: ["Dashboard"],
          findings: [finding("Dashboard")]
        )

      send(view.pid, {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, event})
      assert eventually(fn -> render(view) =~ "Dashboard" end)

      # A second, unrelated reload only touches StatsView — Dashboard's
      # finding must remain untouched.
      other_event =
        reload_check_event(
          changed_class: "Widget",
          selector: "size",
          checked_owners: ["StatsView"],
          findings: []
        )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, other_event}
      )

      Process.sleep(50)
      assert render(view) =~ "Dashboard"
    end

    test "a caller's own reload fully wipes ALL of its findings, regardless of origin",
         %{conn: conn} do
      # Dashboard itself reloads (e.g. a plain hand-edit, or a revert). Its
      # own source just changed, so every recorded finding about it — no
      # matter which changed class originally produced it — is stale and
      # must clear in full, unlike the scoped per-origin clear a
      # *dependent* re-check applies. This is the `owner == changed_class`
      # branch of the clearing rule.
      {:ok, view, _html} = live(owner_conn(conn), "/")

      counter_finding =
        finding("Dashboard", changed_class: "Counter", message: "counter finding")

      widget_finding =
        finding("Dashboard", changed_class: "Widget", selector: "size", message: "widget finding")

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler,
         reload_check_event(
           changed_class: "Counter",
           checked_owners: ["Dashboard"],
           findings: [counter_finding]
         )}
      )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler,
         reload_check_event(
           changed_class: "Widget",
           selector: "size",
           checked_owners: ["Dashboard"],
           findings: [widget_finding]
         )}
      )

      assert eventually(fn -> render(view) =~ "widget finding" end)
      assert render(view) =~ "counter finding"

      # Dashboard itself reloads — `changedClass` now equals the owner —
      # with nothing further to report (a plain edit, no dependents).
      own_reload_event =
        reload_check_event(
          changed_class: "Dashboard",
          selector: "refresh",
          classification: :self_edit,
          checked_owners: ["Dashboard"],
          findings: []
        )

      send(
        view.pid,
        {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, own_reload_event}
      )

      assert eventually(fn -> not (render(view) =~ "counter finding") end)
      refute render(view) =~ "widget finding"
    end
  end

  describe "resilience" do
    test "an unexpected push shape does not crash the LiveView", %{conn: conn} do
      {:ok, view, _html} = live(owner_conn(conn), "/")

      send(view.pid, {:beamtalk_announcement, make_ref(), :ReloadCheckCompleted, :handler, %{}})

      assert eventually(fn -> Process.alive?(view.pid) end)
      assert render(view) =~ "No reload-induced findings"
    end
  end
end
