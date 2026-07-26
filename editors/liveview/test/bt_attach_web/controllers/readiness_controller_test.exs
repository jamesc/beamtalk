# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ReadinessControllerTest do
  @moduledoc """
  `GET /readiness` request/response coverage (ADR 0097 Implementation §1c) —
  the desktop-attach broker's pre-window health probe.

  Drives the endpoint against small fake workspace clients (the same
  `Application.put_env(:bt_attach, :workspace_client, ...)` injection point
  `workspace_live_test.exs` uses via `BtAttachWeb.StubWorkspaceClient`) rather
  than a real workspace, so each of the four outcomes — reachable, and the
  three failure-taxonomy reasons — is deterministic and doesn't need real
  distribution/epmd/a live workspace.
  """
  use BtAttachWeb.ConnCase, async: false

  defmodule ReachableClient do
    @moduledoc false
    def readiness do
      {:ok,
       %{
         runtime_version: "1.2.3",
         protocol_version: "2.0",
         otp_release: "26",
         erts_version: "14.0"
       }}
    end
  end

  defmodule EpmdAbsentClient do
    @moduledoc false
    def readiness, do: {:error, :epmd_absent}
  end

  defmodule BadCookieClient do
    @moduledoc false
    def readiness, do: {:error, :bad_cookie}
  end

  defmodule DeadWorkspaceClient do
    @moduledoc false
    def readiness, do: {:error, :dead_workspace}
  end

  setup do
    on_exit(fn -> Application.delete_env(:bt_attach, :workspace_client) end)
    :ok
  end

  test "200 with the version report when the workspace is reachable", %{conn: conn} do
    Application.put_env(:bt_attach, :workspace_client, ReachableClient)

    conn = get(conn, ~p"/readiness")

    assert json_response(conn, 200) == %{
             "status" => "ok",
             "version" => %{
               "runtime_version" => "1.2.3",
               "protocol_version" => "2.0",
               "otp_release" => "26",
               "erts_version" => "14.0"
             }
           }
  end

  test "503 with reason epmd_absent when the front can't publish to epmd", %{conn: conn} do
    Application.put_env(:bt_attach, :workspace_client, EpmdAbsentClient)

    conn = get(conn, ~p"/readiness")

    assert json_response(conn, 503) == %{"status" => "error", "reason" => "epmd_absent"}
  end

  test "503 with reason bad_cookie when the dist handshake is rejected", %{conn: conn} do
    Application.put_env(:bt_attach, :workspace_client, BadCookieClient)

    conn = get(conn, ~p"/readiness")

    assert json_response(conn, 503) == %{"status" => "error", "reason" => "bad_cookie"}
  end

  test "503 with reason dead_workspace when epmd has no record of the target node", %{
    conn: conn
  } do
    Application.put_env(:bt_attach, :workspace_client, DeadWorkspaceClient)

    conn = get(conn, ~p"/readiness")

    assert json_response(conn, 503) == %{"status" => "error", "reason" => "dead_workspace"}
  end

  # This front's release is shared with OIDC-authenticated remote deployments
  # (ADR 0091) as well as the (always-unauthenticated) desktop-attach broker.
  # An unauthenticated `/readiness` would let any internet client on a remote
  # deployment force a dist connect + RPC and read the version report — so
  # when OIDC *is* configured, the endpoint must sit behind the same
  # `require_auth` gate as the IDE route, not be unconditionally public.
  test "redirects to the OIDC login route instead of leaking the version report when OIDC is configured",
       %{conn: conn} do
    Application.put_env(:bt_attach, :workspace_client, ReachableClient)

    Application.put_env(:bt_attach, :oidc, %{
      issuer: "https://idp",
      client_id: "id",
      redirect_uri: "https://ide/callback",
      groups_claim: "groups",
      client_secret: "x",
      roles: %{"owner" => ["beamtalk-owners"]}
    })

    on_exit(fn -> Application.delete_env(:bt_attach, :oidc) end)

    conn = get(conn, ~p"/readiness")

    assert redirected_to(conn) == ~p"/oidc/auth"
  end
end
