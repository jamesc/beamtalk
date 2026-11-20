# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceSessionLifecycleTest do
  @moduledoc """
  BT-5081: the LiveView attaches its session to its own pid on the workspace
  side (so the workspace's idle reaping covers a front that dies without
  cleanup), and notices when the session it is bound to goes away (a workspace
  restart, or a distribution loss) instead of keeping a dead `session_pid`.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.SessionProbe
  alias BtAttachWeb.StubWorkspaceClient

  defmodule AttachClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    def attach_session(session_pid, client_pid) do
      send(
        Application.fetch_env!(:bt_attach, :lifecycle_test_pid),
        {:attached, session_pid, client_pid}
      )

      :ok
    end
  end

  setup do
    {:ok, _} = SessionProbe.start_link()
    {:ok, _} = StubWorkspaceClient.start_state()
    Application.put_env(:bt_attach, :workspace_client, AttachClient)
    Application.put_env(:bt_attach, :lifecycle_test_pid, self())

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :lifecycle_test_pid)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  # Killing a process is asynchronous: the LiveView's monitor `DOWN` arrives some
  # moments after `Process.exit/2` returns, so poll the rendered view for the
  # banner rather than assuming it is already there.
  defp wait_for_banner(view, retries \\ 100) do
    html = render(view)

    cond do
      html =~ ~s(id="workspace-lost") ->
        html

      retries == 0 ->
        flunk("the workspace-lost banner never appeared")

      true ->
        # sleep: waiting for the monitor DOWN to be delivered to the LiveView
        Process.sleep(10)
        wait_for_banner(view, retries - 1)
    end
  end

  defp mount_cockpit(conn) do
    {:ok, view, _html} = live(conn, "/")
    render_async(view, 5_000)
    view
  end

  test "the LiveView attaches its own pid to the session on the workspace", %{conn: conn} do
    view = mount_cockpit(conn)

    [%{pid: session_pid}] = SessionProbe.sessions()
    assert_receive {:attached, ^session_pid, client_pid}, 1_000
    assert client_pid == view.pid
  end

  test "a session that goes away shows a workspace-lost banner and the tab stays usable",
       %{conn: conn} do
    view = mount_cockpit(conn)
    refute render(view) =~ ~s(id="workspace-lost")

    [%{pid: session_pid}] = SessionProbe.sessions()
    Process.exit(session_pid, :kill)

    html = wait_for_banner(view)
    assert html =~ "Reconnect"
    # The cockpit is still rendered and answering events.
    assert html =~ "bt-cockpit"
    assert render_click(view, "toggle_browser", %{}) =~ "bt-cockpit"
  end

  test "Reconnect starts a fresh session, re-attaches and clears the banner", %{conn: conn} do
    view = mount_cockpit(conn)
    [%{pid: first}] = SessionProbe.sessions()
    assert_receive {:attached, ^first, _}, 1_000

    Process.exit(first, :kill)
    wait_for_banner(view)

    html = view |> element("#workspace-reconnect") |> render_click()
    refute html =~ ~s(id="workspace-lost")

    assert [_, %{pid: second}] = SessionProbe.sessions()
    assert second != first
    assert_receive {:attached, ^second, client_pid}, 1_000
    assert client_pid == view.pid

    # And the new session is monitored too: losing it shows the banner again.
    Process.exit(second, :kill)
    wait_for_banner(view)
  end
end
