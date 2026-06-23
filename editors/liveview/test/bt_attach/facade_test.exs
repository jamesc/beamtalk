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

    # BT-2648: native-modules enumeration + a module-keyed native source view.
    def browse_native_modules,
      do: record({:browse_native_modules}) && {:value, [%{"module" => "beamtalk_http_client"}]}

    def browse_native_module_source(module),
      do:
        record({:browse_native_module_source, module}) && {:value, %{"backing_module" => module}}

    # BT-2495: the navigation aids — senders/implementors (nav-query) + the
    # omni-search symbol index (nav-symbols).
    def senders_of(selector),
      do: record({:senders_of, selector}) && {:value, %{"sites" => []}}

    def implementors_of(selector),
      do: record({:implementors_of, selector}) && {:value, %{"sites" => []}}

    # BT-2639: the protocol-definition action row — required methods / conforming
    # classes (nav-query protocol kinds).
    def required_methods_of(protocol),
      do: record({:required_methods_of, protocol}) && {:value, %{"sites" => []}}

    def conforming_classes_of(protocol),
      do: record({:conforming_classes_of, protocol}) && {:value, %{"sites" => []}}

    # BT-2669: native-module callers (reverse FFI nav-query kind).
    def callers_of_native_module(module),
      do: record({:callers_of_native_module, module}) && {:value, %{"sites" => []}}

    def symbol_index(scope),
      do: record({:symbol_index, scope}) && {:value, %{"classes" => []}}

    # BT-2544: backend-driven autocomplete for the CodeMirror editors.
    def complete(pid, code),
      do: record({:complete, pid, code}) && {:ok, ["size", "sizeWith:"]}

    # BT-2555: live-image hover docs for the CodeMirror editors.
    def hover(pid, code),
      do: record({:hover, pid, code}) && {:ok, "== Integer < Number =="}

    # BT-2556: parse-only diagnostics for the CodeMirror editors. BT-2569: `mode`
    # selects the parse grammar ("expression" | "method").
    def diagnostics(code, mode),
      do:
        record({:diagnostics, code, mode}) &&
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

    # BT-2557: load the project's test/ files into the live image.
    def load_tests,
      do:
        record({:load_tests}) &&
          {:ok, %{"classes" => ["FooTest"], "errors" => [], "summary" => "Reloaded 1 of 1 files"}}

    # BT-2586: the cockpit git panel — read ops (status/diff/log) + mutating ops
    # (stage/unstage/commit/revert).
    def git_status,
      do: record({:git_status}) && {:ok, %{branch: "main", files: []}}

    def git_diff(path),
      do: record({:git_diff, path}) && {:ok, %{worktree: "", staged: ""}}

    def git_log(count),
      do: record({:git_log, count}) && {:ok, []}

    def git_stage(path), do: record({:git_stage, path}) && {:ok, nil}
    def git_unstage(path), do: record({:git_unstage, path}) && {:ok, nil}
    def git_commit(message), do: record({:git_commit, message}) && {:ok, nil}
    def git_revert_file(path), do: record({:git_revert_file, path}) && {:ok, nil}

    # BT-2590: the autoflush flag read returns a bare boolean (not a tuple).
    def autoflush, do: record({:autoflush}) && true
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
      # BT-2557: loading test files compiles + loads user code → :execute.
      assert Facade.capability(:load_tests) == :execute

      for read <- ~w(info inspect bindings actors processes sessions complete hover
                     diagnostics
                     subscribe_transcript subscribe_bindings subscribe_actors
                     subscribe_classes
                     subscribe_object unsubscribe_object pid_stats
                     browse_classes browse_protocols browse_method_source
                     browse_class_definition browse_native_source
                     browse_native_modules browse_native_module_source list_tests
                     senders implementors required_methods conforming_classes
                     callers_of_native_module symbols
                     git_status git_diff git_log
                     autoflush)a do
        assert Facade.capability(read) == :read, "#{read} should be :read"
      end

      # BT-2586: the mutating git ops alter the index/working tree/history → :execute.
      for execute <- ~w(git_stage git_unstage git_commit git_revert_file)a do
        assert Facade.capability(execute) == :execute, "#{execute} should be :execute"
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

    # BT-2648: the native-modules enumeration + module-keyed native source view.
    test "browse_native_modules routes with no params and returns the live term" do
      assert Facade.dispatch(:browse_native_modules, %{}) ==
               {:value, [%{"module" => "beamtalk_http_client"}]}

      assert {:browse_native_modules} in RecordingClient.calls()
    end

    test "browse_native_module_source passes the module through" do
      assert Facade.dispatch(:browse_native_module_source, %{module: "beamtalk_http_client"}) ==
               {:value, %{"backing_module" => "beamtalk_http_client"}}

      assert {:browse_native_module_source, "beamtalk_http_client"} in RecordingClient.calls()
    end

    test "browse_native_module_source with a non-binary module is invalid params" do
      assert Facade.dispatch(:browse_native_module_source, %{module: 123}) ==
               {:error, :invalid_params}
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

  describe "protocol actions (Required methods / Conforming classes, BT-2639)" do
    test "required_methods/conforming_classes route to the client and return the live term" do
      assert Facade.dispatch(:required_methods, %{protocol: "Printable"}) ==
               {:value, %{"sites" => []}}

      assert Facade.dispatch(:conforming_classes, %{protocol: "Printable"}) ==
               {:value, %{"sites" => []}}

      recorded = RecordingClient.calls() |> Enum.map(&elem(&1, 0)) |> MapSet.new()
      assert recorded == MapSet.new([:required_methods_of, :conforming_classes_of])
    end

    test "the protocol name is passed through" do
      Facade.dispatch(:required_methods, %{protocol: "Awaitable"})
      Facade.dispatch(:conforming_classes, %{protocol: "TimeoutToken"})

      assert {:required_methods_of, "Awaitable"} in RecordingClient.calls()
      assert {:conforming_classes_of, "TimeoutToken"} in RecordingClient.calls()
    end

    test "a non-binary protocol is invalid params, with no dist call" do
      assert Facade.dispatch(:required_methods, %{protocol: :Printable}) ==
               {:error, :invalid_params}

      assert Facade.dispatch(:conforming_classes, %{protocol: 42}) == {:error, :invalid_params}
      assert RecordingClient.calls() == []
    end

    test "the observer role may use the protocol actions (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:required_methods, %{protocol: "Printable"}, observer) ==
               {:value, %{"sites" => []}}

      assert Facade.dispatch(:conforming_classes, %{protocol: "Printable"}, observer) ==
               {:value, %{"sites" => []}}
    end
  end

  describe "native-module callers (reverse FFI nav, BT-2669)" do
    test "callers_of_native_module routes to the client and returns the live term" do
      assert Facade.dispatch(:callers_of_native_module, %{module: "lists"}) ==
               {:value, %{"sites" => []}}

      assert {:callers_of_native_module, "lists"} in RecordingClient.calls()
    end

    test "a non-binary module is invalid params, with no dist call" do
      assert Facade.dispatch(:callers_of_native_module, %{module: :lists}) ==
               {:error, :invalid_params}

      assert RecordingClient.calls() == []
    end

    test "the observer role may use the native-module callers view (read capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:callers_of_native_module, %{module: "lists"}, observer) ==
               {:value, %{"sites" => []}}
    end
  end

  describe "git panel ops (cockpit VCS surface, ADR 0082 / BT-2586)" do
    test "read ops route to the client and return its term verbatim" do
      assert Facade.dispatch(:git_status, %{}) == {:ok, %{branch: "main", files: []}}

      assert Facade.dispatch(:git_diff, %{path: "src/foo.bt"}) ==
               {:ok, %{worktree: "", staged: ""}}

      assert Facade.dispatch(:git_log, %{count: 5}) == {:ok, []}

      assert {:git_diff, "src/foo.bt"} in RecordingClient.calls()
      assert {:git_log, 5} in RecordingClient.calls()
    end

    test "git_log defaults to 20 commits when count is omitted" do
      assert Facade.dispatch(:git_log, %{}) == {:ok, []}
      assert {:git_log, 20} in RecordingClient.calls()
    end

    test "autoflush routes to the client and returns the bare boolean (BT-2590)" do
      assert Facade.dispatch(:autoflush, %{}) == true
      assert {:autoflush} in RecordingClient.calls()
    end

    test "the observer role may read autoflush (read capability, BT-2590)" do
      assert Facade.dispatch(:autoflush, %{}, %{role: :observer}) == true
      assert {:autoflush} in RecordingClient.calls()
    end

    test "mutating ops route to the client and pass their arg through" do
      assert Facade.dispatch(:git_stage, %{path: "a.bt"}) == {:ok, nil}
      assert Facade.dispatch(:git_unstage, %{path: "a.bt"}) == {:ok, nil}
      assert Facade.dispatch(:git_commit, %{message: "wip"}) == {:ok, nil}
      assert Facade.dispatch(:git_revert_file, %{path: "a.bt"}) == {:ok, nil}

      assert {:git_stage, "a.bt"} in RecordingClient.calls()
      assert {:git_unstage, "a.bt"} in RecordingClient.calls()
      assert {:git_commit, "wip"} in RecordingClient.calls()
      assert {:git_revert_file, "a.bt"} in RecordingClient.calls()
    end

    test "a bad shape is invalid params, with no dist call" do
      assert Facade.dispatch(:git_diff, %{path: 42}) == {:error, :invalid_params}
      assert Facade.dispatch(:git_log, %{count: 0}) == {:error, :invalid_params}
      assert Facade.dispatch(:git_commit, %{message: :nope}) == {:error, :invalid_params}
      assert Facade.dispatch(:git_stage, %{path: nil}) == {:error, :invalid_params}
      assert RecordingClient.calls() == []
    end

    test "the observer role may inspect git but not mutate it" do
      observer = %{role: :observer}

      # Read ops are visible to the Observer.
      assert Facade.dispatch(:git_status, %{}, observer) == {:ok, %{branch: "main", files: []}}
      assert Facade.dispatch(:git_log, %{count: 5}, observer) == {:ok, []}

      assert Facade.dispatch(:git_diff, %{path: "src/foo.bt"}, observer) ==
               {:ok, %{worktree: "", staged: ""}}

      # Mutating ops are denied, with no dist call.
      assert Facade.dispatch(:git_stage, %{path: "a.bt"}, observer) == {:error, :unauthorized}
      assert Facade.dispatch(:git_unstage, %{path: "a.bt"}, observer) == {:error, :unauthorized}
      assert Facade.dispatch(:git_commit, %{message: "wip"}, observer) == {:error, :unauthorized}

      assert Facade.dispatch(:git_revert_file, %{path: "a.bt"}, observer) ==
               {:error, :unauthorized}

      refute Enum.any?(
               RecordingClient.calls(),
               &(elem(&1, 0) in [:git_stage, :git_unstage, :git_commit, :git_revert_file])
             )
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

      # No `mode` given → the default "expression" grammar (the Workspace/REPL
      # editors).
      assert {:diagnostics, "1 +", "expression"} in RecordingClient.calls()
    end

    test "diagnostics forwards the parse mode for the method editor (BT-2569)" do
      assert Facade.dispatch(:diagnostics, %{code: "decrement => self.value", mode: "method"}) ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}

      assert {:diagnostics, "decrement => self.value", "method"} in RecordingClient.calls()
    end

    test "an unknown or non-binary mode normalises to expression at the facade (BT-2569)" do
      # The facade is the shared boundary: anything but "method" degrades to the
      # safe default, so a non-binary mode never reaches the `is_binary/1`-guarded
      # client call. Both an unknown string and a non-binary normalise to "expression".
      assert Facade.dispatch(:diagnostics, %{code: "x", mode: "bogus"}) ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}

      assert Facade.dispatch(:diagnostics, %{code: "x", mode: 42}) ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "boom"}]}

      calls = RecordingClient.calls()
      assert {:diagnostics, "x", "expression"} in calls
      refute {:diagnostics, "x", "bogus"} in calls
      refute {:diagnostics, "x", 42} in calls
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

    test "load_tests routes to the client (BT-2557)" do
      assert {:ok, %{"classes" => ["FooTest"]}} = Facade.dispatch(:load_tests, %{})
      assert {:load_tests} in RecordingClient.calls()
    end

    test "the observer role may NOT load tests (execute capability)" do
      observer = %{role: :observer}

      assert Facade.dispatch(:load_tests, %{}, observer) == {:error, :unauthorized}
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
