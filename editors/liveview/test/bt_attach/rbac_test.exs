# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.RbacTest do
  use ExUnit.Case, async: false

  alias BtAttach.Facade
  alias BtAttach.Rbac

  @config %{
    issuer: "https://idp",
    client_id: "id",
    redirect_uri: "https://ide/callback",
    groups_claim: "groups",
    client_secret: "x",
    roles: %{
      "owner" => ["beamtalk-owners", "platform-admins"],
      "observer" => ["beamtalk-observers"]
    }
  }

  # Execute + admin ops Observer must never reach; read ops Observer may.
  # `processes` (default-scope supervision tree, ADR 0092) is a read op like
  # `actors`; `processes_system` is the privileged whole-node view (execute).
  @execute_admin_ops ~w(eval load_source save flush reload kill rotate_cookie processes_system)a
  @read_ops ~w(info inspect bindings actors processes sessions complete subscribe_transcript)a

  describe "role_for/2 — claim → role (fail closed)" do
    test "owner group → :owner (owner takes precedence)" do
      assert Rbac.role_for(%{"groups" => ["platform-admins", "beamtalk-observers"]}, @config) ==
               {:ok, :owner}
    end

    test "observer group → :observer" do
      assert Rbac.role_for(%{"groups" => ["beamtalk-observers"]}, @config) == {:ok, :observer}
    end

    test "a single-string groups claim is accepted" do
      assert Rbac.role_for(%{"groups" => "beamtalk-owners"}, @config) == {:ok, :owner}
    end

    test "no matching group → denied (no privileged fallback)" do
      assert Rbac.role_for(%{"groups" => ["randoms"]}, @config) == {:error, :denied}
    end

    test "claims missing the groups claim → denied" do
      assert Rbac.role_for(%{"sub" => "alice"}, @config) == {:error, :denied}
    end
  end

  describe "authorize/2 — per-op (execute vs read boundary)" do
    test "Owner is allowed every facade op" do
      for op <- Facade.ops() do
        assert Rbac.authorize(:owner, op) == :ok, "owner should be allowed #{op}"
      end
    end

    test "Observer is allowed read ops" do
      for op <- @read_ops do
        assert Rbac.authorize(:observer, op) == :ok, "observer should be allowed #{op}"
      end
    end

    test "Observer is denied every execute/admin op" do
      for op <- @execute_admin_ops do
        assert Rbac.authorize(:observer, op) == {:error, :unauthorized},
               "observer must be denied #{op}"
      end
    end

    test "an unknown op is never authorized, even for Owner" do
      assert Rbac.authorize(:owner, :os_cmd) == {:error, :unauthorized}
    end

    # ADR 0092 §6: the supervision-tree introspection asymmetry.
    test "Observer may reach default `processes` but is denied `system`" do
      assert Rbac.authorize(:observer, :processes) == :ok
      assert Rbac.authorize(:observer, :processes_system) == {:error, :unauthorized}
    end

    test "Owner may reach both `processes` and the privileged `system` view" do
      assert Rbac.authorize(:owner, :processes) == :ok
      assert Rbac.authorize(:owner, :processes_system) == :ok
    end
  end

  describe "validate!/1 — fail closed at boot" do
    test "an empty role map raises" do
      assert_raise RuntimeError, ~r/no role map/, fn ->
        Rbac.validate!(%{@config | roles: %{}})
      end
    end

    test "a map with at least one role is OK" do
      assert Rbac.validate!(%{@config | roles: %{"owner" => ["g"]}}) == :ok
    end
  end

  describe "enforcement through the facade (before any dist call)" do
    defmodule RecordingClient do
      def calls, do: Process.get(:rbac_calls, [])
      defp rec(e), do: Process.put(:rbac_calls, [e | Process.get(:rbac_calls, [])])
      def eval(pid, code), do: rec({:eval, pid, code}) && {:ok, :t, "", []}
      def list_bindings(pid), do: rec({:bindings, pid}) && [{"x", 1}]
    end

    setup do
      Application.put_env(:bt_attach, :workspace_client, RecordingClient)
      Process.put(:rbac_calls, [])
      on_exit(fn -> Application.delete_env(:bt_attach, :workspace_client) end)
      :ok
    end

    test "Observer's eval is denied with NO dist call" do
      assert Facade.dispatch(:eval, %{session_pid: self(), code: "3+4"}, %{role: :observer}) ==
               {:error, :unauthorized}

      assert RecordingClient.calls() == []
    end

    test "Observer's read op (bindings) is allowed and reaches the client" do
      assert Facade.dispatch(:bindings, %{session_pid: self()}, %{role: :observer}) == [{"x", 1}]
      assert [{:bindings, _}] = RecordingClient.calls()
    end

    test "Owner's eval is allowed and reaches the client" do
      assert Facade.dispatch(:eval, %{session_pid: self(), code: "3+4"}, %{role: :owner}) ==
               {:ok, :t, "", []}

      assert [{:eval, _, "3+4"}] = RecordingClient.calls()
    end
  end
end
