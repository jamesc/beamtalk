# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveLifecycleTest do
  @moduledoc """
  The cockpit's session attach/resume lifecycle (BT-3306), driven through the
  full LiveView stack against the stubbed workspace client — no `:workspace`
  tag, so this runs in the bare `mix test` lane.

  `BtAttachWeb.WorkspaceLive.mount/3`'s connected path is a small state machine
  with several failure exits that nothing else in the suite reaches:

    * the per-tab `workspace_token` connect param (minted in
      `assets/js/app.js`) is read off `get_connect_params/1` and drives resume;
    * a registry hit is only *trusted* when the remote session pid is still
      alive (`Workspace.session_alive?/1`) — a stale entry left by a restarted
      workspace is discarded and a fresh session started instead;
    * `connect/0` failing, `start_session/2` failing, and either subscribe
      failing each leave the socket `connected: false` with its own error
      (and, for the subscribe case, must not leak the half-started session);
    * the session metadata a fresh session carries picks the authenticated
      user out of three claim shapes (a bare binary, `%{username:}`, `%{id:}`).

  `Workspace.session_alive?/1` runs `is_process_alive/1` **on the workspace
  node** (`:rpc.call(node_name(), …)`), so the alive/dead axis is driven by
  pointing `BT_WORKSPACE_NODE` at either this node (the call runs locally and
  answers truthfully) or an unreachable node (`{:badrpc, :nodedown}` → false,
  exactly the restarted-workspace case) — no live workspace required either
  way.
  """
  # Mutates global app env / env vars and the (global) session registry.
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttach.SessionRegistry
  alias BtAttachWeb.SessionProbe
  alias BtAttachWeb.StubWorkspaceClient

  # ── test workspace clients ──────────────────────────────────────────────────
  #
  # Each overrides exactly the callback whose failure it drives; everything else
  # delegates to the shared `StubWorkspaceClient` (see
  # `BtAttachWeb.StubClientOverrides`).

  defmodule UnreachableClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def connect, do: {:error, {:connect_failed, :beamtalk_workspace_gone@localhost, false}}
  end

  defmodule NoSessionClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(_session_id, _meta), do: {:error, {:unreachable, :nodedown}}
  end

  defmodule NoTranscriptClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    def subscribe_transcript(_pid), do: {:badrpc, :nodedown}
  end

  defmodule NoBindingsStreamClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    def subscribe_bindings(_pid), do: {:badrpc, :nodedown}
  end

  defmodule DegradedPushClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    # Both best-effort push streams and the initial reload-findings snapshot are
    # unavailable (an older workspace, or a transient dist hiccup).
    def subscribe_classes(_pid), do: {:badrpc, :nodedown}
    def subscribe_reload_check(_pid), do: {:badrpc, :nodedown}
    def reload_findings, do: {:error, :nodedown}
  end

  setup do
    {:ok, _} = SessionProbe.start_link()
    {:ok, _} = StubWorkspaceClient.start_state()
    Application.put_env(:bt_attach, :workspace_client, SessionProbe.Client)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  defp sessions, do: SessionProbe.sessions()

  defp unique_token, do: "tok-#{System.unique_integer([:positive])}"

  defp put_workspace_node(name) do
    previous = System.get_env("BT_WORKSPACE_NODE")
    System.put_env("BT_WORKSPACE_NODE", name)

    on_exit(fn ->
      if previous,
        do: System.put_env("BT_WORKSPACE_NODE", previous),
        else: System.delete_env("BT_WORKSPACE_NODE")
    end)
  end

  defp connect_with(conn, token),
    do: put_connect_params(conn, %{"workspace_token" => token})

  # `bt_user` is assigned verbatim as `:current_user` when no auth gate is
  # configured (`BtAttachWeb.Auth.on_mount/4`'s `not auth_required?()` arm), so
  # a test can hand the LiveView any of the claim shapes `session_meta/1`
  # pattern-matches on.
  defp user_conn(conn, user) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => user,
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  # ── token extraction + resume ───────────────────────────────────────────────

  describe "per-tab resume token" do
    test "a `workspace_token` connect param registers a resumable session", %{conn: conn} do
      token = unique_token()

      {:ok, _view, html} = conn |> connect_with(token) |> live("/")

      assert html =~ "Beamtalk Workspace"
      assert [%{session_id: session_id, pid: pid}] = sessions()

      # The token was read off the connect params (a nil token skips
      # registration entirely), so the registry now owns this tab's session.
      assert {:resumed, ^session_id, ^pid} = SessionRegistry.checkout(token)
    end

    test "a connect param that is not a `workspace_token` binary disables resume", %{conn: conn} do
      params = %{"workspace_token" => 42}

      {:ok, _first, _html} = conn |> put_connect_params(params) |> live("/")
      {:ok, _second, _html} = build_conn() |> put_connect_params(params) |> live("/")

      # A non-binary token collapses to nil rather than crashing the mount, and
      # a nil token registers nothing — so each connect gets its own,
      # non-resumable session.
      assert [%{session_id: first}, %{session_id: second}] = sessions()
      assert first != second
    end

    test "a reconnect resumes the live session instead of starting a second one", %{conn: conn} do
      # `session_alive?/1` runs on the workspace node — point it at this node so
      # the probe answers truthfully for the still-running fake session pid.
      put_workspace_node(to_string(node()))
      token = unique_token()

      {:ok, _first, _html} = conn |> connect_with(token) |> live("/")
      assert [%{session_id: session_id}] = sessions()

      {:ok, _second, html} = build_conn() |> connect_with(token) |> live("/")

      assert html =~ "Beamtalk Workspace"
      # Resumed: the reconnect re-bound the SAME session rather than starting a
      # fresh one, so `start_session/2` was never called a second time.
      assert [%{session_id: ^session_id}] = sessions()
    end

    test "a stale token whose remote session is gone is discarded and restarted", %{conn: conn} do
      # The workspace restarted between the disconnect and this reconnect: the
      # registry entry survives (the local fake pid is alive) but the *remote*
      # liveness probe cannot reach it, so resume must not claim success.
      put_workspace_node("beamtalk_workspace_gone@localhost")
      token = unique_token()

      {:ok, _first, _html} = conn |> connect_with(token) |> live("/")
      assert [%{session_id: first_id}] = sessions()

      {:ok, _second, html} = build_conn() |> connect_with(token) |> live("/")

      assert html =~ "Beamtalk Workspace"
      assert [%{session_id: ^first_id}, %{session_id: second_id}] = sessions()
      assert second_id != first_id
      # The stale entry was discarded and replaced by the fresh session.
      assert {:resumed, ^second_id, _pid} = SessionRegistry.checkout(token)
    end

    test "closing a tokened tab stashes its desk and defers teardown", %{conn: conn} do
      token = unique_token()

      {:ok, view, _html} = conn |> connect_with(token) |> live("/")
      assert [%{session_id: session_id, pid: session_pid}] = sessions()

      # A LiveView `terminate/2` fires on a real tab close AND a transient
      # socket drop, so it must hand the session to the registry's grace
      # window (stashing the desk) rather than closing it outright.
      Process.flag(:trap_exit, true)
      ref = Process.monitor(view.pid)
      GenServer.stop(view.pid, :shutdown)
      assert_receive {:DOWN, ^ref, :process, _pid, _reason}, 2_000

      assert {:resumed, ^session_id, ^session_pid} = SessionRegistry.checkout(token)
    end
  end

  # ── attach failures ─────────────────────────────────────────────────────────

  describe "attach failures leave a diagnosable, disconnected socket" do
    test "a workspace that will not accept a connection renders the attach error", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, UnreachableClient)

      {:ok, _view, html} = live(conn, "/")

      assert html =~ "Not attached."
      assert html =~ "attach failed:"
      assert html =~ "connect_failed"
      # No session was ever requested — the failure is upstream of session start.
      assert sessions() == []
    end

    test "a workspace that cannot start a session renders the session-start error", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, NoSessionClient)

      {:ok, _view, html} = live(conn, "/")

      assert html =~ "Not attached."
      assert html =~ "session start failed:"
      assert html =~ "unreachable"
    end

    test "a failed Transcript subscribe tears the session down (untokened tab)", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, NoTranscriptClient)

      {:ok, _view, html} = live(conn, "/")

      assert html =~ "Not attached."
      assert html =~ "subscribe failed:"
      # A session WAS started before the subscribe failed, and an untokened one
      # is not registry-owned — so the failure path closes it directly rather
      # than leaving an orphan behind (the close itself is a workspace-side
      # `stop_session` RPC, so only the started-then-abandoned session is
      # observable from here).
      assert [%{}] = sessions()
    end

    test "a failed bindings subscribe discards the tokened session", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, NoBindingsStreamClient)
      token = unique_token()

      {:ok, _view, html} = conn |> connect_with(token) |> live("/")

      assert html =~ "Not attached."
      assert html =~ "subscribe failed:"
      # A tokened session IS registry-owned, so the failure path discards the
      # entry (which closes it) rather than leaving it to the grace timer.
      assert :miss = SessionRegistry.checkout(token)
    end

    test "the best-effort push streams and the findings snapshot degrade, not fail", %{conn: conn} do
      Application.put_env(:bt_attach, :workspace_client, DegradedPushClient)

      {:ok, view, html} = live(conn, "/")

      # Neither the class-lifecycle nor the reload-check subscribe is load
      # bearing, and an unreadable findings snapshot degrades to an empty panel:
      # the cockpit still mounts connected.
      refute html =~ "Not attached."
      assert html =~ ~s(id="system-browser")
      assert render(view) =~ "No reload-induced findings."
    end
  end

  # ── session metadata ────────────────────────────────────────────────────────

  describe "session metadata carries the authenticated user" do
    test "a bare binary claim is used verbatim", %{conn: conn} do
      {:ok, _view, _html} = conn |> user_conn("alice") |> live("/")

      assert [%{meta: meta}] = sessions()
      assert meta.user == "alice"
      assert meta.kind == "liveview"
      assert meta.node == node()
    end

    test "a claims map with a username uses it", %{conn: conn} do
      {:ok, _view, _html} = conn |> user_conn(%{username: "bob"}) |> live("/")

      assert [%{meta: %{user: "bob"}}] = sessions()
    end

    test "a claims map with only an id stringifies it", %{conn: conn} do
      {:ok, _view, _html} = conn |> user_conn(%{id: 42}) |> live("/")

      assert [%{meta: %{user: "42"}}] = sessions()
    end

    test "an unauthenticated mount carries no user", %{conn: conn} do
      {:ok, _view, _html} = live(conn, "/")

      assert [%{meta: meta}] = sessions()
      refute Map.has_key?(meta, :user)
    end
  end
end
