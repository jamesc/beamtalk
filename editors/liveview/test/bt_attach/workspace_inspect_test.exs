# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceInspectTest do
  @moduledoc """
  Unit tests for the pure (RPC-free) reference-following predicates the Wave-2
  Inspector relies on (BT-2408): which live terms are inspectable objects, and
  the short-circuit that rejects non-objects before any RPC. The RPC-backed
  read-surface paths (`list_bindings/1`, `inspect_value/1` against a real object)
  are exercised by the `@tag :workspace` LiveView e2e tests.
  """
  use ExUnit.Case, async: true

  alias BtAttach.Workspace

  describe "inspectable?/1 — the reference-following gate" do
    test "a pid-backed object handle is inspectable" do
      # Over distribution `#beamtalk_object{}` arrives as this 4-tuple (beamtalk.hrl):
      # {:beamtalk_object, Class, Module, Pid}. A real pid is messageable, so it
      # can be followed by reference.
      obj = {:beamtalk_object, :Counter, :counter, self()}
      assert Workspace.inspectable?(obj)
    end

    test "a name-resolving proxy (no pid, ADR 0079) is not inspectable" do
      # ADR 0079 proxies carry {:registered, Name} in the identity slot — there is
      # no pid to read live state from, so they are not drillable.
      proxy = {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      refute Workspace.inspectable?(proxy)
    end

    test "scalar and container terms are not inspectable" do
      refute Workspace.inspectable?(42)
      refute Workspace.inspectable?(3.14)
      refute Workspace.inspectable?("hello")
      refute Workspace.inspectable?(true)
      refute Workspace.inspectable?([1, 2, 3])
      refute Workspace.inspectable?(%{a: 1})
    end
  end

  describe "inspect_value/1 — non-object short-circuit (no RPC)" do
    test "non-object terms are rejected with :not_inspectable" do
      assert {:error, :not_inspectable} = Workspace.inspect_value(42)
      assert {:error, :not_inspectable} = Workspace.inspect_value("hello")
      assert {:error, :not_inspectable} = Workspace.inspect_value([1, 2, 3])
    end

    test "a registered proxy (no pid) is rejected with :not_inspectable" do
      proxy = {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      assert {:error, :not_inspectable} = Workspace.inspect_value(proxy)
    end
  end

  describe "per-object tracking — non-object short-circuit (no RPC, ADR 0095 §5 / BT-2489)" do
    # The live-Inspector tracking calls watch/read a *pid-backed* actor handle.
    # A non-pid term (scalar, container, or a name-resolving proxy with no pid)
    # is rejected with :not_inspectable before any RPC — the same contract
    # `inspect_value/1` uses, so the caller never has to guess the term shape.
    test "subscribe_object_changes rejects non-pid-backed terms" do
      proxy = {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      assert {:error, :not_inspectable} = Workspace.subscribe_object_changes(42, self())
      assert {:error, :not_inspectable} = Workspace.subscribe_object_changes("x", self())
      assert {:error, :not_inspectable} = Workspace.subscribe_object_changes(proxy, self())
    end

    test "unsubscribe_object_changes rejects non-pid-backed terms" do
      proxy = {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      assert {:error, :not_inspectable} = Workspace.unsubscribe_object_changes(42, self())
      assert {:error, :not_inspectable} = Workspace.unsubscribe_object_changes(proxy, self())
    end

    test "pid_stats rejects non-pid-backed terms" do
      proxy = {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      assert {:error, :not_inspectable} = Workspace.pid_stats(42)
      assert {:error, :not_inspectable} = Workspace.pid_stats("x")
      assert {:error, :not_inspectable} = Workspace.pid_stats(proxy)
    end
  end
end
