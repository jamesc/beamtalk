# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.RbacWebTest do
  use BtAttachWeb.ConnCase, async: false

  @config %{
    issuer: "https://idp",
    client_id: "id",
    redirect_uri: "https://ide/callback",
    groups_claim: "groups",
    client_secret: "x",
    roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
  }

  setup do
    Application.put_env(:bt_attach, :oidc, @config)
    Application.put_env(:bt_attach, :session_ttl_secs, 3600)

    on_exit(fn ->
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :session_ttl_secs)
    end)

    :ok
  end

  defp session(conn, groups) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "u", "groups" => groups},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  test "an Observer may view the IDE (HTTP render)", %{conn: conn} do
    conn = conn |> session(["beamtalk-observers"]) |> get(~p"/")
    assert html_response(conn, 200) =~ "Beamtalk Workspace"
    assert conn.assigns.role == :observer
  end

  test "an Owner may view the IDE (HTTP render)", %{conn: conn} do
    conn = conn |> session(["beamtalk-owners"]) |> get(~p"/")
    assert html_response(conn, 200) =~ "Beamtalk Workspace"
    assert conn.assigns.role == :owner
  end

  test "an authenticated user matching no role is refused (403), fail closed", %{conn: conn} do
    conn = conn |> session(["some-other-group"]) |> get(~p"/")
    assert response(conn, 403) =~ "maps to no IDE role"
  end
end
