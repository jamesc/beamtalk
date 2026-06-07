# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.AuthBoundaryTest do
  @moduledoc """
  End-to-end auth-boundary test (BT-2424, ADR 0091 Phase 4).

  Exercises the whole Phoenix-side path together — OIDC session → claim→role
  mapping (BT-2421) → curated facade (BT-2420) → (would-be) dist — asserting the
  boundary holds at each layer. The workspace dist call is stood in for by an
  injectable recording client (`:bt_attach, :workspace_client`), so this runs in
  CI without a live workspace while still proving *whether a dist call happens*.
  The full path against a real node is the tag-gated `WorkspaceLiveTest`.
  """
  use BtAttachWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias BtAttach.Facade
  alias BtAttach.Rbac

  @config %{
    issuer: "https://idp",
    client_id: "id",
    redirect_uri: "https://ide/callback",
    groups_claim: "groups",
    client_secret: "x",
    roles: %{"owner" => ["beamtalk-owners"], "observer" => ["beamtalk-observers"]}
  }

  # Records every workspace call so we can assert a denied op makes NO dist call.
  defmodule RecordingClient do
    def calls, do: Process.get(:ab_calls, [])
    defp rec(e), do: Process.put(:ab_calls, [e | Process.get(:ab_calls, [])])
    def eval(p, c), do: rec({:eval, p, c}) && {:ok, "live-term-7", "", []}
    def list_bindings(p), do: rec({:bindings, p}) && [{"x", 1}]
    def inspect_value(t), do: rec({:inspect, t}) && {:ok, %{count: 7}}
    def subscribe_transcript(p), do: rec({:sub_t, p}) && :ok
    def subscribe_bindings(p), do: rec({:sub_b, p}) && :ok
  end

  setup do
    Application.put_env(:bt_attach, :oidc, @config)
    Application.put_env(:bt_attach, :workspace_client, RecordingClient)
    Application.put_env(:bt_attach, :session_ttl_secs, 3600)
    Process.put(:ab_calls, [])

    on_exit(fn ->
      Application.delete_env(:bt_attach, :oidc)
      Application.delete_env(:bt_attach, :workspace_client)
      Application.delete_env(:bt_attach, :session_ttl_secs)
    end)

    :ok
  end

  defp owner_ctx, do: %{user: %{"sub" => "alice"}, role: :owner}
  defp observer_ctx, do: %{user: %{"sub" => "bob"}, role: :observer}

  defp session(conn, groups) do
    Plug.Test.init_test_session(conn, %{
      "bt_user" => %{"sub" => "u", "groups" => groups},
      "bt_logged_in_at" => System.system_time(:second)
    })
  end

  test "an unauthenticated request is rejected (redirect) with no eval", %{conn: conn} do
    conn = get(conn, ~p"/")
    assert redirected_to(conn) == ~p"/oidc/auth"
    # Nothing reached the workspace.
    assert RecordingClient.calls() == []
  end

  test "claim → role mapping resolves the two roles", %{conn: _conn} do
    assert Rbac.role_for(%{"groups" => ["beamtalk-owners"]}, @config) == {:ok, :owner}
    assert Rbac.role_for(%{"groups" => ["beamtalk-observers"]}, @config) == {:ok, :observer}
    assert Rbac.role_for(%{"groups" => ["nobody"]}, @config) == {:error, :denied}
  end

  test "Owner can eval and gets a live term result", %{conn: _conn} do
    assert Facade.dispatch(:eval, %{session_pid: self(), code: "3 + 4"}, owner_ctx()) ==
             {:ok, "live-term-7", "", []}

    assert [{:eval, _, "3 + 4"}] = RecordingClient.calls()
  end

  test "Observer is denied eval/load-source with NO dist call", %{conn: _conn} do
    assert Facade.dispatch(:eval, %{session_pid: self(), code: "3 + 4"}, observer_ctx()) ==
             {:error, :unauthorized}

    assert Facade.dispatch(:load_source, %{}, observer_ctx()) == {:error, :unauthorized}
    assert RecordingClient.calls() == []
  end

  test "Observer CAN see the Transcript and inspect/browse bindings", %{conn: _conn} do
    assert Facade.dispatch(:subscribe_transcript, %{pid: self()}, observer_ctx()) == :ok
    assert Facade.dispatch(:bindings, %{session_pid: self()}, observer_ctx()) == [{"x", 1}]

    assert Facade.dispatch(:inspect, %{term: {:beamtalk_object, :C, :c, self()}}, observer_ctx()) ==
             {:ok, %{count: 7}}

    ops = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()
    assert ops == MapSet.new([:sub_t, :bindings, :inspect])
  end

  test "an off-facade op is rejected with no dist call", %{conn: _conn} do
    assert Facade.dispatch(:os_cmd, %{module: :os, function: :cmd, args: ["id"]}, owner_ctx()) ==
             {:error, :forbidden_op}

    assert RecordingClient.calls() == []
  end

  test "an authenticated user mapping to no role is refused at the HTTP edge (403)", %{conn: conn} do
    conn = conn |> session(["random-group"]) |> get(~p"/")
    assert response(conn, 403) =~ "maps to no IDE role"
    assert RecordingClient.calls() == []
  end

  test "an expired/revoked session tears down an active socket", %{conn: conn} do
    conn = session(conn, ["beamtalk-owners"])
    {:ok, view, _html} = live_isolated(conn, BtAttachWeb.ProbeLive)

    # Session passes its TTL; the armed re-validation tick must tear the socket
    # down (redirect to re-auth), the lifecycle half of the auth boundary.
    Application.put_env(:bt_attach, :session_ttl_secs, 0)
    send(view.pid, :bt_session_revalidate)
    assert_redirect(view, "/oidc/auth")
  end
end
