# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceEvalAsyncTest do
  @moduledoc """
  BT-5081: a Workspace / REPL eval runs off the LiveView process, so a long or
  hung eval no longer freezes the tab, a Stop control interrupts it, and a
  timed-out call shows an error and leaves the tab usable.

  The client's `eval/2` blocks until the test releases it, which is exactly
  what a hung workspace call looked like when the LiveView awaited it inline:
  any `render`/`render_click` on the view would then block behind the eval.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttachWeb.SessionProbe
  alias BtAttachWeb.StubWorkspaceClient

  defmodule SlowClient do
    @moduledoc false
    use BtAttachWeb.StubClientOverrides

    def start_session(session_id, meta), do: SessionProbe.record(session_id, meta)

    # Blocks until the test says what the eval returns (`:release` → ok, or the
    # exact reply term), or `interrupt/1` is called.
    def eval(_pid, _code) do
      test = Application.fetch_env!(:bt_attach, :eval_async_test_pid)
      send(test, {:eval_started, self()})

      receive do
        {:finish, reply} -> reply
        :interrupted -> {:error, :interrupted, "", []}
      end
    end

    def list_bindings(_pid) do
      send(Application.fetch_env!(:bt_attach, :eval_async_test_pid), :bindings_read)
      []
    end

    def interrupt(_pid) do
      test = Application.fetch_env!(:bt_attach, :eval_async_test_pid)
      send(test, :interrupt_called)

      case Application.get_env(:bt_attach, :eval_async_eval_pid) do
        pid when is_pid(pid) -> send(pid, :interrupted)
        _ -> :ok
      end

      :ok
    end
  end

  setup do
    {:ok, _} = SessionProbe.start_link()
    {:ok, _} = StubWorkspaceClient.start_state()
    Application.put_env(:bt_attach, :workspace_client, SlowClient)
    Application.put_env(:bt_attach, :eval_async_test_pid, self())

    on_exit(fn ->
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :eval_async_test_pid)
      Application.delete_env(:bt_attach, :eval_async_eval_pid)
      StubWorkspaceClient.stop_state(2_000)
    end)

    :ok
  end

  defp mount_cockpit(conn) do
    {:ok, view, _html} = live(conn, "/")
    render_async(view, 5_000)
    view
  end

  test "a hung eval does not block the LiveView: the tab still answers and shows Stop", %{
    conn: conn
  } do
    view = mount_cockpit(conn)

    view |> form("#eval-form", %{"expr" => "[true] whileTrue"}) |> render_submit()
    assert_receive {:eval_started, eval_task}, 1_000

    # If the eval ran inside the LiveView process, this render would never return.
    html = render(view)
    assert html =~ ~s(id="eval-stop")
    assert html =~ "evaluating"

    # Other interactions are served while the eval is still running.
    assert render_click(view, "toggle_browser", %{}) =~ "bt-cockpit"

    # Finishing the eval folds its result and clears the running state.
    send(eval_task, {:finish, {:ok, 42, "", []}})
    html = render_async(view, 5_000)
    refute html =~ ~s(id="eval-stop")
    assert html =~ "42"
  end

  test "Stop interrupts the running eval and the tab shows the interrupted error", %{conn: conn} do
    view = mount_cockpit(conn)

    view |> form("#eval-form", %{"expr" => "[true] whileTrue"}) |> render_submit()
    assert_receive {:eval_started, eval_task}, 1_000
    Application.put_env(:bt_attach, :eval_async_eval_pid, eval_task)

    view |> element("#eval-stop") |> render_click()
    assert_receive :interrupt_called, 1_000

    html = render_async(view, 5_000)
    refute html =~ ~s(id="eval-stop")
    assert html =~ "ws-result err"
  end

  test "a second eval while one runs is queued and runs after it, in order", %{conn: conn} do
    view = mount_cockpit(conn)

    view |> form("#eval-form", %{"expr" => "1"}) |> render_submit()
    assert_receive {:eval_started, first}, 1_000

    view |> form("#eval-form", %{"expr" => "2"}) |> render_submit()
    refute_receive {:eval_started, _}, 100

    send(first, {:finish, {:ok, 1, "", []}})
    assert_receive {:eval_started, second}, 1_000
    send(second, {:finish, {:ok, 2, "", []}})
    assert render_async(view, 5_000) =~ "2"
  end

  test "the eval queue is capped: a submit past the cap is refused with a message", %{conn: conn} do
    view = mount_cockpit(conn)
    cap = BtAttachWeb.Live.Dock.max_eval_queue()

    view |> form("#eval-form", %{"expr" => "0"}) |> render_submit()
    assert_receive {:eval_started, first}, 1_000

    for n <- 1..cap, do: view |> form("#eval-form", %{"expr" => "#{n}"}) |> render_submit()
    html = view |> form("#eval-form", %{"expr" => "overflow"}) |> render_submit()
    assert html =~ "Too many evaluations are waiting"

    # Drain: the running eval plus exactly `cap` queued ones run; the refused one never does.
    Application.put_env(:bt_attach, :eval_async_eval_pid, first)
    send(first, {:finish, {:ok, 0, "", []}})

    for _ <- 1..cap do
      assert_receive {:eval_started, next}, 2_000
      send(next, {:finish, {:ok, 1, "", []}})
    end

    render_async(view, 5_000)
    refute_receive {:eval_started, _}, 200
  end

  test "Stop also cancels the evals queued behind the running one", %{conn: conn} do
    view = mount_cockpit(conn)

    view |> form("#eval-form", %{"expr" => "1"}) |> render_submit()
    assert_receive {:eval_started, first}, 1_000
    Application.put_env(:bt_attach, :eval_async_eval_pid, first)
    view |> form("#eval-form", %{"expr" => "2"}) |> render_submit()

    view |> element("#eval-stop") |> render_click()
    render_async(view, 5_000)
    refute_receive {:eval_started, _}, 200
  end

  test "an eval that timed out shows an error, interrupts the runaway, and leaves the tab usable",
       %{conn: conn} do
    view = mount_cockpit(conn)

    view |> form("#eval-form", %{"expr" => "[true] whileTrue"}) |> render_submit()
    assert_receive {:eval_started, eval_task}, 1_000

    send(eval_task, {:finish, {:error, {:unreachable, :timeout}, "", []}})
    html = render_async(view, 5_000)

    assert html =~ "did not respond in time"
    # The call timed out, so the session is interrupted rather than left running.
    assert_receive :interrupt_called, 1_000
    refute html =~ ~s(id="eval-stop")

    # Usable afterwards: a fresh eval runs.
    view |> form("#eval-form", %{"expr" => "1 + 1"}) |> render_submit()
    assert_receive {:eval_started, next}, 1_000
    send(next, {:finish, {:ok, 2, "", []}})
    assert render_async(view, 5_000) =~ "2"
  end

  test "a REPL eval clears the composer at submit, not when it finishes", %{conn: conn} do
    view = mount_cockpit(conn)

    render_hook(view, "repl_eval", %{"expr" => "1 + 1"})
    assert_receive {:eval_started, eval_task}, 1_000

    # Already cleared while the eval is still running, so text typed into the
    # composer meanwhile is not wiped when the result lands.
    assert_push_event(view, "repl_set_input", %{text: ""})

    send(eval_task, {:finish, {:ok, 2, "", []}})
    render_async(view, 5_000)
    refute_push_event(view, "repl_set_input", %{text: ""}, 50)
  end

  test "bindings are re-read once the eval finishes (a mid-eval push reads stale state)", %{
    conn: conn
  } do
    view = mount_cockpit(conn)
    flush_bindings_reads()

    view |> form("#eval-form", %{"expr" => "x := 1"}) |> render_submit()
    assert_receive {:eval_started, eval_task}, 1_000
    refute_receive :bindings_read, 50

    send(eval_task, {:finish, {:ok, 1, "", []}})
    render_async(view, 5_000)
    assert_received :bindings_read
  end

  defp flush_bindings_reads do
    receive do
      :bindings_read -> flush_bindings_reads()
    after
      0 -> :ok
    end
  end
end
