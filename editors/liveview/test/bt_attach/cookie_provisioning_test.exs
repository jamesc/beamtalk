# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.CookieProvisioningTest do
  @moduledoc """
  BT-2422: the dist cookie is an infrastructure secret provisioned to Phoenix via
  env — it must never reach a browser-facing surface.
  """
  use BtAttachWeb.ConnCase, async: false

  setup do
    saved = {System.get_env("BT_WORKSPACE_NODE"), System.get_env("BT_WORKSPACE_COOKIE")}

    on_exit(fn ->
      {node, cookie} = saved
      put_or_delete("BT_WORKSPACE_NODE", node)
      put_or_delete("BT_WORKSPACE_COOKIE", cookie)
    end)

    :ok
  end

  defp put_or_delete(key, nil), do: System.delete_env(key)
  defp put_or_delete(key, val), do: System.put_env(key, val)

  test "the workspace node name is provisioned from BT_WORKSPACE_NODE" do
    System.put_env("BT_WORKSPACE_NODE", "beamtalk_workspace_demo@localhost")
    assert BtAttach.Workspace.node_name() == :beamtalk_workspace_demo@localhost
  end

  test "the provisioned cookie never appears in a browser-facing surface", %{conn: conn} do
    secret = "super-secret-cookie-#{System.unique_integer([:positive])}"
    System.put_env("BT_WORKSPACE_NODE", "beamtalk_workspace_demo@localhost")
    System.put_env("BT_WORKSPACE_COOKIE", secret)

    # The IDE page (disconnected HTTP render; auth disabled in test) must not
    # carry the cookie. The cookie flows only through BtAttach.Workspace's env →
    # Node.set_cookie for the dist link, never into an assign, page, or URL.
    conn = get(conn, ~p"/")
    body = html_response(conn, 200)

    refute body =~ secret
  end
end
