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

  describe "term_class/1 — the single source of truth (BT-2635)" do
    # One table asserting the centralized classifier for every known tag, so a
    # future regression (a new live-handle tag silently defaulting to :value, or
    # a scalar mis-mapped) fails here at the one place that enumerates tags.
    test "classifies every known tag" do
      pid = self()

      cases = [
        # {label, term, expected term_class/1}
        {"object handle", {:beamtalk_object, :Counter, :counter, pid}, {:ref, :object}},
        {"supervisor handle", {:beamtalk_supervisor, :AppSup, :app_sup, pid},
         {:ref, :supervisor}},
        {"future handle", {:beamtalk_future, pid}, {:ref, :future}},
        {"bare pid", pid, {:ref, :pid}},
        {"integer", 42, {:scalar, :integer}},
        {"float", 3.14, {:scalar, :float}},
        {"string", "hello", {:scalar, :string}},
        {"true", true, {:scalar, :boolean}},
        {"false", false, {:scalar, :boolean}},
        {"list", [1, 2, 3], {:scalar, :list}},
        {"map", %{a: 1}, {:scalar, :map}},
        {"atom/symbol", :foo, {:scalar, :atom}},
        # A name-resolving proxy carries no pid (ADR 0079) — NOT a ref, NOT a
        # known scalar: it falls through to :value (non-drillable, preserved).
        {"registered proxy", {:beamtalk_object, :Counter, :counter, {:registered, :counter}},
         :value},
        # An unmodelled tuple is :value, not a silent ref.
        {"unknown tuple", {:something, :else}, :value}
      ]

      for {label, term, expected} <- cases do
        assert Workspace.term_class(term) == expected, "term_class/1 misclassified #{label}"
      end
    end

    test "the four derived functions all agree with term_class/1" do
      # inspectable?/1 derives from term_class/1: every {:ref, _} is inspectable,
      # nothing else is — futures and bare pids included (no longer "value").
      pid = self()

      refs = [
        {:beamtalk_object, :Counter, :counter, pid},
        {:beamtalk_supervisor, :AppSup, :app_sup, pid},
        {:beamtalk_future, pid},
        pid
      ]

      non_refs = [
        42,
        3.14,
        "hello",
        true,
        [1, 2, 3],
        %{a: 1},
        :foo,
        {:beamtalk_object, :Counter, :counter, {:registered, :counter}}
      ]

      for term <- refs,
          do: assert(Workspace.inspectable?(term), "#{inspect(term)} should be a ref")

      for term <- non_refs,
          do: refute(Workspace.inspectable?(term), "#{inspect(term)} should not be a ref")
    end
  end

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

    test "a pid-backed supervisor handle is inspectable (BT-2633)" do
      # A supervisor handle arrives as {:beamtalk_supervisor, Class, Module, Pid}
      # (beamtalk_supervisor.erl) — distinct from #beamtalk_object{}. A pid-backed
      # supervisor is a live, drillable ref, so it is recognized as inspectable
      # (content is a follow-up).
      sup = {:beamtalk_supervisor, :AppSup, :bt@my_app@app_sup, self()}
      assert Workspace.inspectable?(sup)
    end

    test "a pid-backed dynamic supervisor handle is inspectable (BT-2633)" do
      # DynamicSupervisor carries the same tuple tag/shape, so it is recognized
      # identically to a static supervisor.
      sup = {:beamtalk_supervisor, :WorkerSup, :bt@my_app@worker_sup, self()}
      assert Workspace.inspectable?(sup)
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

    test "a pid-backed supervisor routes to the supervision-tree path (BT-2634)" do
      # BT-2634: a supervisor's content is its CHILDREN / supervision tree, not
      # actor instance vars (replacing BT-2633's empty {:ok, %{}} placeholder). A
      # supervisor pid is NOT an actor backed by sys:get_state/2, so it must not
      # route through the actor inspect RPC; instead it calls the supervision-tree
      # helper (beamtalk_process_navigation:child_handles/1) over distribution.
      # Without a live workspace node, that RPC degrades to {:error, {:unreachable,
      # _}} — never a crash, and crucially never the actor sys:get_state path. The
      # children-content + drill-through is exercised against the stub in the
      # LiveView tests.
      sup = {:beamtalk_supervisor, :AppSup, :bt@my_app@app_sup, self()}
      assert {:error, {:unreachable, _reason}} = Workspace.inspect_value(sup)
    end

    test "a future degrades to a graceful minimal result, never :not_inspectable (BT-2635)" do
      # A tagged future {:beamtalk_future, pid} is a live ref, so it must NOT be
      # rejected as :not_inspectable. Deep future content (await/resolve) is a
      # follow-up, so it degrades to a graceful minimal process-info snapshot —
      # and without a live workspace node the RPC degrades to an empty field set
      # ({:ok, %{}}), never a crash and never :not_inspectable.
      future = {:beamtalk_future, self()}
      assert {:ok, fields} = Workspace.inspect_value(future)
      assert is_map(fields)
    end

    test "a bare pid degrades to a graceful minimal result, never :not_inspectable (BT-2635)" do
      # A bare pid is a live ref too. Like futures, its deep content is a
      # follow-up, so it degrades to a minimal snapshot ({:ok, %{}} without a
      # live workspace) rather than :not_inspectable or a crash.
      assert {:ok, fields} = Workspace.inspect_value(self())
      assert is_map(fields)
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
