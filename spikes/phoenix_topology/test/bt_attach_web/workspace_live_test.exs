# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveTest do
  @moduledoc """
  End-to-end proof of the BT-2394 Attach topology through the full Phoenix
  LiveView stack against a *real* running Beamtalk workspace node.

  Requires a workspace and its cookie in the environment:

      beamtalk workspace create spike --background --persistent
      export BT_WORKSPACE_NODE=beamtalk_workspace_spike@localhost
      export BT_WORKSPACE_COOKIE=$(sed 's/-setcookie //;s/ //g' \\
        ~/.beamtalk/workspaces/spike/vm.args)

  Skipped automatically if no workspace cookie is configured.
  """
  use BtAttachWeb.ConnCase
  import Phoenix.LiveViewTest

  # Excluded by default in test_helper.exs unless BT_WORKSPACE_COOKIE is set.
  @moduletag :workspace

  test "eval round-trip renders the workspace result", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/")
    html = view |> form("form") |> render_submit(%{expr: "3 + 4"})
    assert html =~ "7"
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
