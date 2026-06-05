# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveTest do
  @moduledoc """
  End-to-end proof of the Wave-1 LiveView IDE (BT-2407) through the full Phoenix
  LiveView stack against a *real* running Beamtalk workspace node, on the
  BT-2394 Attach topology. Exercises the BT-2399 term-returning op layer (eval)
  and subscription facade (Transcript), plus session-bound eval state.

  Requires a workspace and its cookie in the environment:

      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' \\
        ~/.beamtalk/workspaces/spike/vm.args)

  Tag-gated: excluded from the default `mix test` run (see test_helper.exs) and
  run only in the workspace-gated CI job.
  """
  use BtAttachWeb.ConnCase
  import Phoenix.LiveViewTest

  # Excluded by default in test_helper.exs unless BT_WORKSPACE_COOKIE is set.
  @moduletag :workspace

  test "eval round-trip renders the workspace result term", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("form") |> render_submit(%{expr: "3 + 4"})
    assert html =~ "7"
  end

  test "eval state persists across evals within a session", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")

    # Bind a variable, then read it back in a later eval on the same session.
    view |> form("form") |> render_submit(%{expr: "x := 21 * 2"})
    html = view |> form("form") |> render_submit(%{expr: "x"})
    assert html =~ "42"
  end

  test "Transcript output streams live into the LiveView", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    marker = "hello-#{System.unique_integer([:positive])}"
    view |> form("form") |> render_submit(%{expr: ~s|Transcript show: "#{marker}"|})

    # The push is delivered asynchronously over distribution; poll the render.
    assert eventually(fn -> render(view) =~ marker end)
  end

  # ~6s total — generous for cross-node async transcript delivery under CI load.
  defp eventually(fun, retries \\ 120) do
    cond do
      fun.() ->
        true

      retries == 0 ->
        false

      true ->
        Process.sleep(50)
        eventually(fun, retries - 1)
    end
  end
end
