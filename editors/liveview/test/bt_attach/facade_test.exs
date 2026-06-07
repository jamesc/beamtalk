# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.FacadeTest do
  # Swaps the global :workspace_client, so not async.
  use ExUnit.Case, async: false

  alias BtAttach.Facade

  # A fake workspace client that records every call (and makes no dist call), so
  # we can assert both routing and the "off-list op makes no dist call" property.
  defmodule RecordingClient do
    def calls, do: Process.get(:facade_calls, [])
    defp record(entry), do: Process.put(:facade_calls, [entry | Process.get(:facade_calls, [])])

    def eval(pid, code), do: record({:eval, pid, code}) && {:ok, :term, "", []}
    def inspect_value(term), do: record({:inspect, term}) && {:ok, %{a: 1}}
    def list_bindings(pid), do: record({:bindings, pid}) && [{"x", 1}]
    def change_history, do: record({:changes}) && []
    def flush, do: record({:flush}) && {:ok, %{}}
    def save_method(c, s, src), do: record({:save, c, s, src}) && {:ok, c}
    def subscribe_transcript(pid), do: record({:sub_t, pid}) && :ok
    def subscribe_bindings(pid), do: record({:sub_b, pid}) && :ok
  end

  setup do
    Application.put_env(:bt_attach, :workspace_client, RecordingClient)
    Process.put(:facade_calls, [])
    on_exit(fn -> Application.delete_env(:bt_attach, :workspace_client) end)
    :ok
  end

  describe "vocabulary + capability classes (RBAC substrate, BT-2421)" do
    test "catalog covers the ADR 0091 op surface with correct capabilities" do
      assert Facade.capability(:eval) == :execute
      assert Facade.capability(:load_source) == :execute
      assert Facade.capability(:save) == :execute
      assert Facade.capability(:flush) == :execute
      assert Facade.capability(:reload) == :execute

      for read <- ~w(info inspect bindings actors processes sessions complete
                     subscribe_transcript subscribe_bindings subscribe_actors
                     subscribe_classes)a do
        assert Facade.capability(read) == :read, "#{read} should be :read"
      end

      # ADR 0092: the privileged `system`-scope supervision view is execute-gated.
      assert Facade.capability(:processes_system) == :execute

      assert Facade.capability(:kill) == :admin
      assert Facade.capability(:rotate_cookie) == :admin
    end

    test "known?/1 and an unknown op" do
      assert Facade.known?(:eval)
      refute Facade.known?(:os_cmd)
      assert Facade.capability(:os_cmd) == nil
    end
  end

  describe "dispatch" do
    test "an off-vocabulary op is refused with no dist call" do
      assert Facade.dispatch(:os_cmd, %{module: :os, function: :cmd, args: ["rm -rf /"]}) ==
               {:error, :forbidden_op}

      # The crux: the workspace client was NEVER invoked.
      assert RecordingClient.calls() == []
    end

    test "in-vocabulary ops route to the client and return its live term verbatim" do
      assert Facade.dispatch(:eval, %{session_pid: self(), code: "3 + 4"}) == {:ok, :term, "", []}

      assert Facade.dispatch(:inspect, %{term: {:beamtalk_object, :C, :c, self()}}) ==
               {:ok, %{a: 1}}

      assert Facade.dispatch(:bindings, %{session_pid: self()}) == [{"x", 1}]
      assert Facade.dispatch(:changes, %{}) == []
      assert Facade.dispatch(:flush, %{}) == {:ok, %{}}
      assert Facade.dispatch(:save, %{class: "C", selector: "m", source: "m => 1"}) == {:ok, "C"}
      assert Facade.dispatch(:subscribe_transcript, %{pid: self()}) == :ok
      assert Facade.dispatch(:subscribe_bindings, %{pid: self()}) == :ok

      recorded = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()

      assert recorded ==
               MapSet.new([:eval, :inspect, :bindings, :changes, :flush, :save, :sub_t, :sub_b])
    end

    test "a catalog op with no UI binding is unsupported, not a dist call" do
      # `kill` is in the vocabulary (so RBAC can reason about it) but has no route
      # yet — it must not silently no-op or hit the client.
      assert Facade.dispatch(:kill, %{}) == {:error, {:unsupported_op, :kill}}
      assert RecordingClient.calls() == []
    end
  end
end
