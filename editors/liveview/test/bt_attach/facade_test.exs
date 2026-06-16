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

    # ADR 0082 Phase 5 (BT-2293): the New File + ChangeLog revert affordances.
    def new_class(src, path), do: record({:new_class, src, path}) && {:ok, path}
    def revert(c, s), do: record({:revert, c, s}) && {:ok, c}

    def subscribe_transcript(pid), do: record({:sub_t, pid}) && :ok
    def subscribe_bindings(pid), do: record({:sub_b, pid}) && :ok

    # ADR 0095 §5 (BT-2489): the live-Inspector per-object tracking ops.
    def subscribe_object_changes(term, pid), do: record({:sub_obj, term, pid}) && :ok
    def unsubscribe_object_changes(term, pid), do: record({:unsub_obj, term, pid}) && :ok
    def pid_stats(term), do: record({:pid_stats, term}) && {:ok, %{"alive" => true}}

    # ADR 0095 (BT-2488): the four System Browser browse ops.
    def browse_classes, do: record({:browse_classes}) && {:value, [%{"name" => "Counter"}]}

    def browse_protocols(class, side),
      do: record({:browse_protocols, class, side}) && {:value, %{"class" => class}}

    def browse_method_source(class, side, selector),
      do: record({:browse_method_source, class, side, selector}) && {:value, %{"source" => "x"}}

    def browse_class_definition(class),
      do: record({:browse_class_definition, class}) && {:value, %{"class" => class}}

    # BT-2495: the navigation aids — senders/implementors (nav-query) + the
    # omni-search symbol index (nav-symbols).
    def senders_of(selector),
      do: record({:senders_of, selector}) && {:value, %{"sites" => []}}

    def implementors_of(selector),
      do: record({:implementors_of, selector}) && {:value, %{"sites" => []}}

    def symbol_index(scope),
      do: record({:symbol_index, scope}) && {:value, %{"classes" => []}}

    # BT-2544: backend-driven autocomplete for the CodeMirror editors.
    def complete(pid, code),
      do: record({:complete, pid, code}) && {:ok, ["size", "sizeWith:"]}

    # BT-2555: live-image hover docs for the CodeMirror editors.
    def hover(pid, code),
      do: record({:hover, pid, code}) && {:ok, "== Integer < Number =="}

    # BT-2556: parse-only diagnostics for the CodeMirror editors.
    def diagnostics(code),
      do:
        record({:diagnostics, code}) &&
          {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}

    # BT-2557: test-runner pane — discovery + run.
    def list_tests,
      do: record({:list_tests}) && {:ok, [%{"class" => "FooTest", "selectors" => ["testOne"]}]}

    def run_tests(class),
      do:
        record({:run_tests, class}) &&
          {:ok,
           %{
             "total" => 1,
             "passed" => 1,
             "failed" => 0,
             "skipped" => 0,
             "duration" => 0.1,
             "tests" => [
               %{"name" => "testOne", "class" => "FooTest", "status" => "pass", "detail" => ""}
             ]
           }}
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
      # ADR 0082 Phase 5 (BT-2293): New File + revert install/run code → :execute.
      assert Facade.capability(:new_class) == :execute
      assert Facade.capability(:revert) == :execute
      assert Facade.capability(:flush) == :execute
      assert Facade.capability(:reload) == :execute
      # BT-2557: running tests evaluates code → :execute; discovery is :read.
      assert Facade.capability(:run_tests) == :execute

      for read <- ~w(info inspect bindings actors processes sessions complete hover
                     diagnostics
                     subscribe_transcript subscribe_bindings subscribe_actors
                     subscribe_classes
                     subscribe_object unsubscribe_object pid_stats
                     browse_classes browse_protocols browse_method_source
                     browse_class_definition list_tests
                     senders implementors symbols)a do
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

  describe "live-Inspector per-object tracking ops (ADR 0095 §5 / BT-2489)" do
    test "subscribe/unsubscribe/pid_stats route to the client and return its term" do
      term = {:beamtalk_object, :Counter, :counter, self()}

      assert Facade.dispatch(:subscribe_object, %{term: term, pid: self()}) == :ok
      assert Facade.dispatch(:unsubscribe_object, %{term: term, pid: self()}) == :ok
      assert Facade.dispatch(:pid_stats, %{term: term}) == {:ok, %{"alive" => true}}

      recorded = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()
      assert recorded == MapSet.new([:sub_obj, :unsub_obj, :pid_stats])
    end

    test "the observer role may track objects (read capability)" do
      observer = %{role: :observer}
      term = {:beamtalk_object, :Counter, :counter, self()}

      assert Facade.dispatch(:subscribe_object, %{term: term, pid: self()}, observer) == :ok
      assert Facade.dispatch(:pid_stats, %{term: term}, observer) == {:ok, %{"alive" => true}}
    end
  end

  describe "browse ops (System Browser data source, ADR 0095 / BT-2488)" do
    test "the four browse ops route to the client and return the live term verbatim" do
      assert Facade.dispatch(:browse_classes, %{}) == {:value, [%{"name" => "Counter"}]}

      assert Facade.dispatch(:browse_protocols, %{class: "Counter", side: "instance"}) ==
               {:value, %{"class" => "Counter"}}

      assert Facade.dispatch(:browse_method_source, %{
               class: "Counter",
               side: "instance",
               selector: "increment"
             }) == {:value, %{"source" => "x"}}

      assert Facade.dispatch(:browse_class_definition, %{class: "Counter"}) ==
               {:value, %{"class" => "Counter"}}

      recorded = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()

      assert recorded ==
               MapSet.new([
                 :browse_classes,
                 :browse_protocols,
                 :browse_method_source,
                 :browse_class_definition
               ])
    end

    test "browse_protocols side defaults to instance when omitted" do
      assert Facade.dispatch(:browse_protocols, %{class: "Counter"}) ==
               {:value, %{"class" => "Counter"}}

      assert {:browse_protocols, "Counter", "instance"} in RecordingClient.calls()
    end

    test "the observer role may browse (read capability) but not eval" do
      observer = %{role: :observer}

      assert Facade.dispatch(:browse_classes, %{}, observer) == {:value, [%{"name" => "Counter"}]}

      assert Facade.dispatch(:browse_class_definition, %{class: "Counter"}, observer) ==
               {:value, %{"class" => "Counter"}}

      # The same role is denied an execute op, with no dist call.
      assert Facade.dispatch(:eval, %{session_pid: self(), code: "1"}, observer) ==
               {:error, :unauthorized}

      refute Enum.any?(RecordingClient.calls(), &(elem(&1, 0) == :eval))
    end
  end

  describe "navigation aids (Senders/Implementors + omni search, BT-2495)" do
    test "senders/implementors/symbols route to the client and return the live term" do
      assert Facade.dispatch(:senders, %{selector: "increment"}) == {:value, %{"sites" => []}}

      assert Facade.dispatch(:implementors, %{selector: "increment"}) ==
               {:value, %{"sites" => []}}

      assert Facade.dispatch(:symbols, %{scope: "all"}) == {:value, %{"classes" => []}}

      recorded = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()
      assert recorded == MapSet.new([:senders_of, :implementors_of, :symbol_index])
    end

    test "senders/implementors pass the selector and symbols passes the scope through" do
      Facade.dispatch(:senders, %{selector: "at:put:"})
      Facade.dispatch(:implementors, %{selector: "printOn:"})
      Facade.dispatch(:symbols, %{scope: "user"})

      assert {:senders_of, "at:put:"} in RecordingClient.calls()
      assert {:implementors_of, "printOn:"} in RecordingClient.calls()
      assert {:symbol_index, "user"} in RecordingClient.calls()
    end

    test "symbols defaults to the all scope when omitted" do
      assert Facade.dispatch(:symbols, %{}) == {:value, %{"classes" => []}}
      assert {:symbol_index, "all"} in RecordingClient.calls()
    end

    test "a non-binary selector is invalid params, with no dist call" do
      assert Facade.dispatch(:senders, %{selector: :increment}) == {:error, :invalid_params}
      assert Facade.dispatch(:implementors, %{selector: 42}) == {:error, :invalid_params}
      assert RecordingClient.calls() == []
    end

    test "the observer role may use the navigation aids (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:senders, %{selector: "increment"}, observer) ==
               {:value, %{"sites" => []}}

      assert Facade.dispatch(:implementors, %{selector: "increment"}, observer) ==
               {:value, %{"sites" => []}}

      assert Facade.dispatch(:symbols, %{scope: "all"}, observer) == {:value, %{"classes" => []}}
    end
  end

  describe "backend-driven autocomplete (BT-2544)" do
    test "complete routes to the client and passes the session pid + line through" do
      assert Facade.dispatch(:complete, %{session_pid: self(), code: "Integer fa"}) ==
               {:ok, ["size", "sizeWith:"]}

      assert {:complete, self(), "Integer fa"} in RecordingClient.calls()
    end

    test "a bad shape is invalid params, with no dist call" do
      # Non-pid session, non-binary code → no completion round-trip.
      assert Facade.dispatch(:complete, %{session_pid: :nope, code: "x"}) ==
               {:error, :invalid_params}

      assert Facade.dispatch(:complete, %{session_pid: self(), code: 42}) ==
               {:error, :invalid_params}

      assert RecordingClient.calls() == []
    end

    test "the observer role may complete (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:complete, %{session_pid: self(), code: "s"}, observer) ==
               {:ok, ["size", "sizeWith:"]}
    end
  end

  describe "live-image hover (BT-2555)" do
    test "hover routes to the client and passes the session pid + line through" do
      assert Facade.dispatch(:hover, %{session_pid: self(), code: "Integer"}) ==
               {:ok, "== Integer < Number =="}

      assert {:hover, self(), "Integer"} in RecordingClient.calls()
    end

    test "a bad shape is invalid params, with no dist call" do
      assert Facade.dispatch(:hover, %{session_pid: :nope, code: "x"}) ==
               {:error, :invalid_params}

      assert Facade.dispatch(:hover, %{session_pid: self(), code: 42}) ==
               {:error, :invalid_params}

      assert RecordingClient.calls() == []
    end

    test "the observer role may hover (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:hover, %{session_pid: self(), code: "Integer"}, observer) ==
               {:ok, "== Integer < Number =="}
    end
  end

  describe "parse-only diagnostics (BT-2556)" do
    test "diagnostics routes to the client and passes the buffer through" do
      assert Facade.dispatch(:diagnostics, %{code: "1 +"}) ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}

      assert {:diagnostics, "1 +"} in RecordingClient.calls()
    end

    test "a bad shape is invalid params, with no dist call" do
      # Non-binary code → no diagnostics round-trip. Diagnostics need no session,
      # so the only shape check is on `code`.
      assert Facade.dispatch(:diagnostics, %{code: 42}) == {:error, :invalid_params}

      assert RecordingClient.calls() == []
    end

    test "the observer role may see diagnostics (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:diagnostics, %{code: "x"}, observer) ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}
    end
  end

  describe "test-runner pane (BT-2557)" do
    test "list_tests routes to the client (discovery, no args)" do
      assert Facade.dispatch(:list_tests, %{}) ==
               {:ok, [%{"class" => "FooTest", "selectors" => ["testOne"]}]}

      assert {:list_tests} in RecordingClient.calls()
    end

    test "the observer role may list tests (read capability)" do
      observer = %{role: :observer}

      assert {:ok, [_ | _]} = Facade.dispatch(:list_tests, %{}, observer)
    end

    test "run_tests with a class runs that class" do
      assert {:ok, %{"passed" => 1}} = Facade.dispatch(:run_tests, %{class: "FooTest"})
      assert {:run_tests, "FooTest"} in RecordingClient.calls()
    end

    test "run_tests with a nil class runs all" do
      assert {:ok, %{"total" => 1}} = Facade.dispatch(:run_tests, %{class: nil})
      assert {:run_tests, nil} in RecordingClient.calls()
    end

    test "a bad class shape is invalid params, with no dist call" do
      assert Facade.dispatch(:run_tests, %{class: 42}) == {:error, :invalid_params}
      assert RecordingClient.calls() == []
    end

    test "the observer role may NOT run tests (execute capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:run_tests, %{class: nil}, observer) == {:error, :unauthorized}
      assert RecordingClient.calls() == []
    end
  end

  describe "New File + ChangeLog revert (ADR 0082 Phase 5, BT-2293)" do
    test "new_class/revert route to the client and pass their args through" do
      assert Facade.dispatch(:new_class, %{source: "Object subclass: G", path: "src/g.bt"}) ==
               {:ok, "src/g.bt"}

      assert Facade.dispatch(:revert, %{class: "Counter", selector: "value"}) == {:ok, "Counter"}

      assert {:new_class, "Object subclass: G", "src/g.bt"} in RecordingClient.calls()
      assert {:revert, "Counter", "value"} in RecordingClient.calls()
    end

    test "non-binary args are invalid params, with no dist call" do
      assert Facade.dispatch(:new_class, %{source: "x", path: 42}) == {:error, :invalid_params}

      assert Facade.dispatch(:revert, %{class: "Counter", selector: :value}) ==
               {:error, :invalid_params}

      assert RecordingClient.calls() == []
    end

    test "the observer role is denied new_class/revert (execute capability), with no dist call" do
      observer = %{role: :observer}

      assert Facade.dispatch(:new_class, %{source: "x", path: "src/g.bt"}, observer) ==
               {:error, :unauthorized}

      assert Facade.dispatch(:revert, %{class: "Counter", selector: "value"}, observer) ==
               {:error, :unauthorized}

      assert RecordingClient.calls() == []
    end
  end
end
