# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ReadinessController do
  @moduledoc """
  `GET /readiness` — the desktop-attach broker's attach-health probe (ADR 0097
  Implementation §1c).

  An external (non-BEAM) broker can't trigger this front's lazy
  `connect/0` from outside the VM, so it spawns the front, polls the HTTP port
  to confirm Phoenix is up, then hits this endpoint to force `connect/0` plus
  one cheap RPC and get a real answer *before* opening a window — a bad
  cookie or a dead workspace surfaces here rather than on the user's first
  eval.

  Responses:

    * `200` — the workspace is reachable. Body includes the version report
      (the `info` operation's `version`, BT-2991, BT-5204) so the broker can warn/refuse on a
      runtime/protocol mismatch.
    * `503` — not reachable, with a machine-readable `reason` distinguishing
      *epmd absent*, *bad cookie*, *workspace too old* and *dead workspace*
      (`BtAttach.Workspace.readiness/0`'s taxonomy) so the broker can show a
      precise error instead of a generic "connection failed". A too-old workspace
      also carries a `message` saying what to do (BT-5206).
  """
  use BtAttachWeb, :controller

  @doc "Force a connect + cheap RPC and report workspace reachability."
  def show(conn, _params) do
    case workspace_client().readiness() do
      {:ok, version_report} ->
        conn
        |> put_status(:ok)
        |> json(%{status: "ok", version: version_report})

      {:error, :workspace_too_old} ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{
          status: "error",
          reason: "workspace_too_old",
          message:
            "The workspace is older than this app and does not serve the operations it " <>
              "needs. Stop it (`beamtalk workspace stop <id>`) and start it again from a " <>
              "matching Beamtalk install."
        })

      {:error, reason} ->
        conn
        |> put_status(:service_unavailable)
        |> json(%{status: "error", reason: to_string(reason)})
    end
  end

  defp workspace_client,
    do: Application.get_env(:bt_attach, :workspace_client, BtAttach.Workspace)
end
