# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceRpcTest do
  @moduledoc """
  Direct unit tests for `BtAttach.Workspace`'s RPC-backed public functions
  (BT-3292), covering the branches `workspace_test.exs`'s "against an
  unreachable workspace" pattern cannot reach: a real disconnected node only
  ever produces `{:badrpc, :nodedown}`, so the success / non-badrpc-error /
  unexpected-reply arms of every `case rpc(...)` in the module go untested
  without a way to script what a "workspace" replies.

  `BtAttach.Workspace.rpc/3` is a thin wrapper over `:rpc.call/4`, so these
  tests stub that one call with `:meck` (already used for exactly this kind of
  branch-shaping in `workspace_classify_unreachable_hostname_test.exs`) rather
  than standing up a real distributed peer node — booting `:net_kernel`
  dynamically is not reliable across CI containers (see that file's moduledoc).
  `mock_rpc/1` maps `{module, function}` to a canned reply (or a 1-arity
  function of the call's `args`, for a handler that must branch on its input),
  so each test scripts exactly the reply sequence the function under test
  makes and nothing else — an unscripted call fails loudly via `flunk/1`
  rather than silently falling through to a real (and here, meaningless)
  network round-trip. Every scripted `{:badrpc, _}` reply below uses the
  `:mock_scripted` reason, deliberately distinct from the `:nodedown` a real
  disconnected `:rpc.call/4` produces — a test asserting on `:mock_scripted`
  can only pass if `mock_rpc/1` actually intercepted the call, so it can't
  silently degrade into re-testing `workspace_test.exs`'s unreachable-node
  path if the mock is ever deleted or mis-keyed.

  `async: false`: `:meck` globally replaces the `:rpc` module for the whole
  VM. ExUnit runs every `async: true` module (including `workspace_test.exs`'s
  real-disconnected-node tests, which also call through `:rpc.call/4`) to
  completion before any `async: false` module starts, so there is no
  interleaving with this file's global patch — the same assumption
  `workspace_classify_unreachable_hostname_test.exs` documents for `:net_adm`.

  One residual hazard the module boundary doesn't cover: `BtAttach.SessionRegistry`
  is a long-lived singleton (part of the app's supervision tree, not restarted
  per test) that arms a `session_reap_after_ms` (300ms in `config/test.exs`)
  timer per registered session and, on expiry, `spawn`s a **detached** process
  that calls the real `Workspace.close_session/1` — bypassing the injectable
  `:workspace_client` other suites stub. If an earlier `async: true` LiveView
  test's reap timer is still pending when this file's global mock is active,
  that detached process's `:rpc.call/4` lands here too. `@safe_defaults` below
  scripts a harmless reply for that one call so a stray reap can't `flunk/1`
  from a foreign, untracked process (which wouldn't fail the test that
  triggered it anyway — `flunk/1` only fails the test *process* it runs in —
  but would otherwise crash-log the unrelated detached process).
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  # See the moduledoc's "residual hazard" note: a stray SessionRegistry reap
  # firing while this file's global `:rpc` mock is active must not encounter
  # an unscripted call. Merged under every `mock_rpc/1` call's own handlers so
  # a test can override it, though none need to.
  @safe_defaults %{{:beamtalk_session_sup, :stop_session} => :ok}

  setup do
    :meck.new(:rpc, [:unstick, :passthrough])
    on_exit(fn -> :meck.unload() end)
    :ok
  end

  # Script `:rpc.call/4`'s reply for each `{module, function}` pair the
  # function under test is expected to hit. A value is returned verbatim on
  # EVERY call to that pair (there is no sequencing — two different calls to
  # the same pair get the same scripted reply); a 1-arity function is invoked
  # with the call's `args` list instead, so a handler can branch on the
  # request payload (e.g. `beamtalk_repl_ops:dispatch/4`'s first arg is the op
  # name) when a test needs two distinct replies from the same pair. A call to
  # a pair with no handler (beyond `@safe_defaults`) fails the test
  # immediately instead of falling through to a real (here meaningless)
  # network round-trip.
  defp mock_rpc(handlers) when is_map(handlers) do
    handlers = Map.merge(@safe_defaults, handlers)

    :meck.expect(:rpc, :call, fn _node, mod, fun, args ->
      case Map.fetch(handlers, {mod, fun}) do
        {:ok, response} when is_function(response, 1) ->
          response.(args)

        {:ok, response} ->
          response

        :error ->
          flunk("unscripted rpc call: #{mod}:#{fun}/#{length(args)} args=#{inspect(args)}")
      end
    end)
  end

  # The `decode` + `get_params` prelude every dispatch_* helper shares, always
  # succeeding — the tests below script only the op-specific outcome that
  # follows it via the `dispatch_key` handler.
  defp decode_ok(dispatch_key, dispatch_reply) do
    %{
      {:beamtalk_repl_protocol, :decode} => {:ok, :fake_msg},
      {:beamtalk_repl_protocol, :get_params} => %{},
      dispatch_key => dispatch_reply
    }
  end

  # Like `decode_ok/2`, but for a wrapper whose behavior IS the request it
  # builds (which op name, which params) — `assertion` runs against the
  # decoded JSON request body, so a wrapper that sent the wrong op or dropped
  # a param fails here rather than only being provable by reading the source.
  defp decode_asserting(assertion, dispatch_reply) when is_function(assertion, 1) do
    %{
      {:beamtalk_repl_protocol, :decode} => fn [json] ->
        assertion.(:json.decode(json))
        {:ok, :fake_msg}
      end,
      {:beamtalk_repl_protocol, :get_params} => %{},
      {:beamtalk_repl_ops, :dispatch} => dispatch_reply
    }
  end

  describe "start_session/1,2 — session creation (all rpc branches)" do
    test "start_session/2 returns the remote session pid on success" do
      pid = self()
      mock_rpc(%{{:beamtalk_session_sup, :start_session} => {:ok, pid}})
      assert Workspace.start_session("sess-1", %{kind: "liveview"}) == pid
    end

    test "start_session/1 delegates to start_session/2 with a liveview meta" do
      pid = self()

      mock_rpc(%{
        {:beamtalk_session_sup, :start_session} => fn ["sess-1", %{kind: "liveview"}] ->
          {:ok, pid}
        end
      })

      assert Workspace.start_session("sess-1") == pid
    end

    test "start_session/2 surfaces a badrpc as an unreachable error" do
      mock_rpc(%{{:beamtalk_session_sup, :start_session} => {:badrpc, :mock_scripted}})

      assert Workspace.start_session("sess-1", %{}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "start_session/2 wraps any other reply as-is" do
      mock_rpc(%{{:beamtalk_session_sup, :start_session} => {:error, :quota_exceeded}})
      assert Workspace.start_session("sess-1", %{}) == {:error, {:error, :quota_exceeded}}
    end
  end

  describe "session_alive?/1 and session_count/0" do
    test "session_alive?/1 returns the boolean the workspace reports" do
      mock_rpc(%{{:erlang, :is_process_alive} => true})
      assert Workspace.session_alive?(self()) == true

      mock_rpc(%{{:erlang, :is_process_alive} => false})
      assert Workspace.session_alive?(self()) == false
    end

    test "session_alive?/1 treats a badrpc as not-alive" do
      mock_rpc(%{{:erlang, :is_process_alive} => {:badrpc, :mock_scripted}})
      assert Workspace.session_alive?(self()) == false
    end

    test "session_count/0 reads the active child count" do
      mock_rpc(%{
        {:supervisor, :count_children} => [specs: 1, active: 3, supervisors: 0, workers: 1]
      })

      assert Workspace.session_count() == 3
    end

    test "session_count/0 surfaces a badrpc as an unreachable error" do
      mock_rpc(%{{:supervisor, :count_children} => {:badrpc, :mock_scripted}})
      assert Workspace.session_count() == {:error, {:unreachable, :mock_scripted}}
    end
  end

  describe "eval/2 — term-returning eval seam (BT-2399)" do
    test "success returns the live term with stringified output/warnings" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:ok, 42, ~c"output", [:warn1]}))

      assert Workspace.eval(self(), "1 + 1") == {:ok, 42, "output", ["warn1"]}
    end

    test "an error result carries stringified output/warnings alongside the reason" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :dispatch},
          {:error, {:beamtalk_error, :boom}, ~c"partial", []}
        )
      )

      assert Workspace.eval(self(), "boom") ==
               {:error, {:beamtalk_error, :boom}, "partial", []}
    end

    test "a bare 2-tuple error (decode failure) degrades to empty output/warnings" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:error, :bad_json}})
      assert Workspace.eval(self(), "bad") == {:error, :bad_json, "", []}
    end

    test "a badrpc is reported as unreachable" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}})
      assert Workspace.eval(self(), "x") == {:error, {:unreachable, :mock_scripted}, "", []}
    end

    test "an unrecognised reply degrades to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :something_weird))

      assert Workspace.eval(self(), "x") ==
               {:error, {:unexpected_reply, :something_weird}, "", []}
    end
  end

  describe "supervision_tree/2" do
    test "success returns the ProcessNavigation tree value" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:ok, [%{name: "root"}], ~c"", []}))
      assert Workspace.supervision_tree(self(), "default") == {:ok, [%{name: "root"}]}
    end

    test "an eval error propagates as the tree error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :boom, ~c"", []}))
      assert Workspace.supervision_tree(self(), "system") == {:error, :boom}
    end
  end

  describe "browse-surface dispatch_browse branch matrix (via browse_classes)" do
    test "a {:value, _} dispatch reply is returned verbatim" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:value, [%{"name" => "Counter"}]}))
      assert Workspace.browse_classes() == {:value, [%{"name" => "Counter"}]}
    end

    test "a {:error, _} dispatch reply is returned verbatim" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :unknown_class}))
      assert Workspace.browse_classes() == {:error, :unknown_class}
    end

    test "an unrecognised dispatch reply degrades to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.browse_classes() == {:error, {:unexpected_reply, :odd}}
    end

    test "an unrecognised decode reply (not ok/badrpc) degrades to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => :odd})
      assert Workspace.browse_classes() == {:error, {:unexpected_reply, :odd}}
    end

    test "a {:badrpc, _} dispatch reply (decode succeeded, the op dispatch itself badrpc'd) is unreachable" do
      # Distinct from the decode-step badrpc test above (a real disconnected
      # node's FIRST rpc call already covers that one): this scripts decode +
      # get_params succeeding and the inner beamtalk_repl_ops:dispatch call
      # itself returning {:badrpc, _}, which only a workspace that decodes the
      # request but dies before dispatching it would produce.
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.browse_classes() == {:error, {:unreachable, :mock_scripted}}
    end
  end

  describe "browse-surface one-off wrappers (BT-2488/BT-2578/BT-2648/BT-2903/BT-3238)" do
    test "browse_categories dispatches browse-categories with the class param" do
      mock_rpc(
        decode_asserting(
          fn decoded -> assert %{"op" => "browse-categories", "class" => "Counter"} = decoded end,
          {:value, %{"categories" => []}}
        )
      )

      assert Workspace.browse_categories("Counter") == {:value, %{"categories" => []}}
    end

    test "browse_native_source/1 defaults selector to nil (the param is omitted)" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{"op" => "browse-native-source", "class" => "Counter"} = decoded
            refute Map.has_key?(decoded, "selector")
          end,
          {:value, %{"class" => "Counter"}}
        )
      )

      assert Workspace.browse_native_source("Counter") == {:value, %{"class" => "Counter"}}
    end

    test "browse_native_source/2 passes an explicit selector" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{
                     "op" => "browse-native-source",
                     "class" => "Counter",
                     "selector" => "foo"
                   } = decoded
          end,
          {:value, %{"selector" => "foo"}}
        )
      )

      assert Workspace.browse_native_source("Counter", "foo") == {:value, %{"selector" => "foo"}}
    end

    test "browse_native_module_source dispatches with a module param (no class)" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{"op" => "browse-native-source", "module" => "lists"} = decoded
            refute Map.has_key?(decoded, "class")
          end,
          {:value, %{"module" => "lists"}}
        )
      )

      assert Workspace.browse_native_module_source("lists") == {:value, %{"module" => "lists"}}
    end

    test "browse_native_modules dispatches browse-native-modules" do
      mock_rpc(
        decode_asserting(
          fn decoded -> assert %{"op" => "browse-native-modules"} = decoded end,
          {:value, []}
        )
      )

      assert Workspace.browse_native_modules() == {:value, []}
    end

    test "browse_type_aliases dispatches browse-type-aliases" do
      mock_rpc(
        decode_asserting(
          fn decoded -> assert %{"op" => "browse-type-aliases"} = decoded end,
          {:value, []}
        )
      )

      assert Workspace.browse_type_aliases() == {:value, []}
    end

    test "browse_alias_source/1 dispatches browse-alias-source with no package param" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{"op" => "browse-alias-source", "name" => "RestartStrategy"} = decoded
            refute Map.has_key?(decoded, "package")
          end,
          {:value, %{"name" => "RestartStrategy"}}
        )
      )

      assert Workspace.browse_alias_source("RestartStrategy") ==
               {:value, %{"name" => "RestartStrategy"}}
    end

    test "browse_alias_source/2 dispatches browse-alias-source with a package param" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{
                     "op" => "browse-alias-source",
                     "name" => "RestartStrategy",
                     "package" => "my_app"
                   } = decoded
          end,
          {:value, %{"name" => "RestartStrategy", "package" => "my_app"}}
        )
      )

      assert Workspace.browse_alias_source("RestartStrategy", "my_app") ==
               {:value, %{"name" => "RestartStrategy", "package" => "my_app"}}
    end

    test "save_native_source dispatches save-native-source with module + source" do
      mock_rpc(
        decode_asserting(
          fn decoded ->
            assert %{
                     "op" => "save-native-source",
                     "module" => "mymod",
                     "source" => "-module(mymod)."
                   } = decoded
          end,
          {:value, %{"ok" => true}}
        )
      )

      assert Workspace.save_native_source("mymod", "-module(mymod).") == {:value, %{"ok" => true}}
    end

    test "save_section with no opts omits old_name/before_selector/before_side" do
      mock_rpc(%{
        {:beamtalk_repl_protocol, :decode} => fn [json] ->
          decoded = :json.decode(json)
          refute Map.has_key?(decoded, "old_name")
          refute Map.has_key?(decoded, "before_selector")
          refute Map.has_key?(decoded, "before_side")
          {:ok, :fake_msg}
        end,
        {:beamtalk_repl_protocol, :get_params} => %{},
        {:beamtalk_repl_ops, :dispatch} => {:value, %{"ok" => true}}
      })

      assert Workspace.save_section("Counter", "Accessing") == {:value, %{"ok" => true}}
    end

    test "save_section with old_name renames an existing category" do
      mock_rpc(%{
        {:beamtalk_repl_protocol, :decode} => fn [json] ->
          assert %{"old_name" => "Old"} = :json.decode(json)
          {:ok, :fake_msg}
        end,
        {:beamtalk_repl_protocol, :get_params} => %{},
        {:beamtalk_repl_ops, :dispatch} => {:value, %{"ok" => true}}
      })

      assert Workspace.save_section("Counter", "New", old_name: "Old") ==
               {:value, %{"ok" => true}}
    end

    test "save_section with before_selector + before_side inserts a new divider" do
      mock_rpc(%{
        {:beamtalk_repl_protocol, :decode} => fn [json] ->
          assert %{"before_selector" => "increment", "before_side" => "class"} =
                   :json.decode(json)

          {:ok, :fake_msg}
        end,
        {:beamtalk_repl_protocol, :get_params} => %{},
        {:beamtalk_repl_ops, :dispatch} => {:value, %{"ok" => true}}
      })

      assert Workspace.save_section("Counter", "New",
               before_selector: "increment",
               before_side: "class"
             ) == {:value, %{"ok" => true}}
    end
  end

  describe "completion-surface: complete/2, hover/2, diagnostics/1,2 (all branches)" do
    test "complete/2 returns stringified completion candidates" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:completions, [:foo, :bar]}))
      assert Workspace.complete(self(), "Coun") == {:ok, ["foo", "bar"]}
    end

    test "complete/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :bad_receiver}))
      assert Workspace.complete(self(), "x") == {:error, :bad_receiver}
    end

    test "complete/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.complete(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "complete/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.complete(self(), "x") == {:error, {:unexpected_reply, :odd}}
    end

    test "complete/2 surfaces a badrpc from the decode step itself (dispatch_complete's own catch-all)" do
      # Distinct from the dispatch-step badrpc above: here decode/1 itself
      # badrpc's, hitting dispatch_complete/2's own `other -> other` catch-all
      # (there is no dedicated {:badrpc, _} clause at the decode step, unlike
      # dispatch_browse/2 / dispatch_simple/2) rather than the {:ok, msg} branch.
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}})
      assert Workspace.complete(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "hover/2 returns the formatted docs string" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:docs, "Counter class docs"}))
      assert Workspace.hover(self(), "Counter") == {:ok, "Counter class docs"}
    end

    test "hover/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :unresolved}))
      assert Workspace.hover(self(), "x") == {:error, :unresolved}
    end

    test "hover/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.hover(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "hover/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.hover(self(), "x") == {:error, {:unexpected_reply, :odd}}
    end

    test "hover/2 surfaces a badrpc from the decode step itself (dispatch_hover's own catch-all)" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}})
      assert Workspace.hover(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "diagnostics/1 defaults mode to expression and normalizes each diagnostic" do
      diag = %{start: 0, end: 3, severity: :error, message: "bad token"}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:diagnostics, [diag]}))

      assert Workspace.diagnostics("1 +") ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "bad token"}]}
    end

    test "diagnostics/2 accepts an explicit mode and a non-map entry degrades gracefully" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:diagnostics, [:not_a_map]}))

      assert Workspace.diagnostics("body", "method") ==
               {:ok, [%{"from" => 0, "to" => 0, "severity" => "error", "message" => ""}]}
    end

    test "diagnostics/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :parse_failed}))
      assert Workspace.diagnostics("x", "expression") == {:error, :parse_failed}
    end

    test "diagnostics/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.diagnostics("x", "expression") == {:error, {:unreachable, :mock_scripted}}
    end

    test "diagnostics/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.diagnostics("x", "expression") == {:error, {:unexpected_reply, :odd}}
    end

    test "diagnostics/2 surfaces a badrpc from the decode step itself (dispatch_diagnostics's own catch-all)" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}})
      assert Workspace.diagnostics("x", "expression") == {:error, {:unreachable, :mock_scripted}}
    end
  end

  describe "test-runner surface: list_tests/0, run_tests/1, load_tests/0 (BT-2557)" do
    test "list_tests/0 returns the discovered classes" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :dispatch},
          {:value, %{"classes" => [%{"class" => "CounterTest"}]}}
        )
      )

      assert Workspace.list_tests() == {:ok, [%{"class" => "CounterTest"}]}
    end

    test "list_tests/0 degrades an unexpected value shape to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:value, %{"nope" => true}}))
      assert Workspace.list_tests() == {:error, {:unexpected_reply, %{"nope" => true}}}
    end

    test "list_tests/0 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :discovery_failed}))
      assert Workspace.list_tests() == {:error, :discovery_failed}
    end

    test "list_tests/0 surfaces a badrpc at the decode step as unreachable" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}})
      assert Workspace.list_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "list_tests/0 degrades an unrecognised decode reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_protocol, :decode} => :odd})
      assert Workspace.list_tests() == {:error, {:unexpected_reply, :odd}}
    end

    test "list_tests/0 surfaces a badrpc from the dispatch step (not the decode step) as unreachable" do
      # Distinct from the decode-step badrpc above: here decode + get_params
      # succeed and it's the op dispatch call itself that badrpcs, which
      # dispatch_simple/2 returns verbatim (no unreachable-wrapping) for
      # list_tests/0's own {:badrpc, _} clause to catch.
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.list_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "list_tests/0 degrades a wholly unrecognised dispatch reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.list_tests() == {:error, {:unexpected_reply, :odd}}
    end

    test "run_tests(nil) runs test-all and normalizes the result, including a non-map entry" do
      result = %{
        total: 2,
        passed: 1,
        failed: 1,
        skipped: 0,
        duration: 12.5,
        tests: [
          %{name: :test_increment, class: :CounterTest, status: :pass},
          %{
            name: :test_decrement,
            class: :CounterTest,
            status: :fail,
            error: "expected 0, got 1"
          },
          :not_a_map
        ]
      }

      mock_rpc(%{
        {:beamtalk_repl_protocol, :decode} => fn [json] ->
          assert %{"op" => "test-all"} = :json.decode(json)
          {:ok, :fake_msg}
        end,
        {:beamtalk_repl_protocol, :get_params} => %{},
        {:beamtalk_repl_ops, :dispatch} => {:test_results, result}
      })

      assert {:ok, normalized} = Workspace.run_tests(nil)
      assert normalized["total"] == 2
      assert normalized["passed"] == 1
      assert normalized["failed"] == 1
      assert normalized["duration"] == 12.5

      assert Enum.map(normalized["tests"], & &1["name"]) == [
               "test_increment",
               "test_decrement",
               inspect(:not_a_map)
             ]

      assert Enum.at(normalized["tests"], 1)["detail"] == "expected 0, got 1"
      assert Enum.at(normalized["tests"], 2)["status"] == "fail"
    end

    test "run_tests/1 with a class name runs a single-class test and stringifies a non-binary detail" do
      result = %{
        total: 1,
        passed: 0,
        failed: 0,
        skipped: 1,
        duration: 0.1,
        tests: [%{name: :test_x, class: :CounterTest, status: :skip, reason: {:disabled, :flaky}}]
      }

      mock_rpc(%{
        {:beamtalk_repl_protocol, :decode} => fn [json] ->
          assert %{"op" => "test", "class" => "CounterTest"} = :json.decode(json)
          {:ok, :fake_msg}
        end,
        {:beamtalk_repl_protocol, :get_params} => %{},
        {:beamtalk_repl_ops, :dispatch} => {:test_results, result}
      })

      assert {:ok, normalized} = Workspace.run_tests("CounterTest")
      [entry] = normalized["tests"]
      assert entry["detail"] == inspect({:disabled, :flaky})
    end

    test "run_tests/1 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :execute_denied}))
      assert Workspace.run_tests(nil) == {:error, :execute_denied}
    end

    test "run_tests/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.run_tests(nil) == {:error, {:unreachable, :mock_scripted}}
    end

    test "run_tests/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.run_tests(nil) == {:error, {:unexpected_reply, :odd}}
    end

    test "load_tests/0 returns the loaded classes on success" do
      mock_rpc(
        decode_ok({:beamtalk_repl_ops, :dispatch}, {:value, %{"classes" => ["CounterTest"]}})
      )

      assert Workspace.load_tests() == {:ok, %{"classes" => ["CounterTest"]}}
    end

    test "load_tests/0 degrades an unexpected value shape to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:value, %{"nope" => true}}))
      assert Workspace.load_tests() == {:error, {:unexpected_reply, %{"nope" => true}}}
    end

    test "load_tests/0 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :compile_failed}))
      assert Workspace.load_tests() == {:error, :compile_failed}
    end

    test "load_tests/0 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, {:badrpc, :mock_scripted}))
      assert Workspace.load_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "load_tests/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :dispatch}, :odd))
      assert Workspace.load_tests() == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "write-surface: save_method/3, new_class/2, revert/3, reload_file/1, flush/0" do
    test "save_method/3 returns the installed class name on success" do
      mock_rpc(%{{:beamtalk_repl_eval, :compile_method} => {:ok, "Counter"}})
      assert Workspace.save_method("Counter", "increment", "^self") == {:ok, "Counter"}
    end

    test "save_method/3 structures a raw error via ensure_structured_error" do
      mock_rpc(%{
        {:beamtalk_repl_eval, :compile_method} => {:error, :parse_error},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :parse_error}
      })

      assert Workspace.save_method("Counter", "increment", "bad(") ==
               {:error, {:beamtalk_error, :parse_error}}
    end

    test "save_method/3 falls back to the raw reason when structuring itself is unreachable" do
      mock_rpc(%{
        {:beamtalk_repl_eval, :compile_method} => {:error, :parse_error},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:badrpc, :mock_scripted}
      })

      assert Workspace.save_method("Counter", "increment", "bad(") == {:error, :parse_error}
    end

    test "save_method/3 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_repl_eval, :compile_method} => {:badrpc, :mock_scripted}})

      assert Workspace.save_method("Counter", "increment", "^self") ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "save_method/3 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_eval, :compile_method} => :odd})

      assert Workspace.save_method("Counter", "increment", "^self") ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "new_class/2 returns the target path on success" do
      mock_rpc(%{{:beamtalk_repl_eval, :new_class} => {:ok, [:some_class_object]}})

      assert Workspace.new_class("Object subclass: Greeter", "src/greeter.bt") ==
               {:ok, "src/greeter.bt"}
    end

    test "new_class/2 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_repl_eval, :new_class} => {:error, :name_clash},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :name_clash}
      })

      assert Workspace.new_class("Object subclass: Greeter", "src/greeter.bt") ==
               {:error, {:beamtalk_error, :name_clash}}
    end

    test "new_class/2 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_repl_eval, :new_class} => {:badrpc, :mock_scripted}})

      assert Workspace.new_class("Object subclass: G", "src/g.bt") ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "new_class/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_eval, :new_class} => :odd})

      assert Workspace.new_class("Object subclass: G", "src/g.bt") ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "revert/3 returns the reverted class name on success" do
      mock_rpc(%{
        {:beamtalk_workspace_interface_primitives, :revert_method} => {:ok, [:class_obj]}
      })

      assert Workspace.revert("Counter", "increment") == {:ok, "Counter"}
    end

    test "revert/3 structures a raw error and threads the side through" do
      mock_rpc(%{
        {:beamtalk_workspace_interface_primitives, :revert_method} => fn [
                                                                           "Counter",
                                                                           "increment",
                                                                           "class"
                                                                         ] ->
          {:error, :not_revertable}
        end,
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :not_revertable}
      })

      assert Workspace.revert("Counter", "increment", "class") ==
               {:error, {:beamtalk_error, :not_revertable}}
    end

    test "revert/3 surfaces a badrpc as unreachable" do
      mock_rpc(%{
        {:beamtalk_workspace_interface_primitives, :revert_method} => {:badrpc, :mock_scripted}
      })

      assert Workspace.revert("Counter", "increment") == {:error, {:unreachable, :mock_scripted}}
    end

    test "revert/3 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_workspace_interface_primitives, :revert_method} => :odd})
      assert Workspace.revert("Counter", "increment") == {:error, {:unexpected_reply, :odd}}
    end

    test "reload_file/1 returns the reloaded class names on success" do
      mock_rpc(%{{:beamtalk_repl_eval, :reload_file} => {:ok, [~c"Counter", ~c"Greeter"]}})
      assert Workspace.reload_file("src/counter.bt") == {:ok, ["Counter", "Greeter"]}
    end

    test "reload_file/1 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_repl_eval, :reload_file} => {:error, :enoent},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :enoent}
      })

      assert Workspace.reload_file("src/missing.bt") == {:error, {:beamtalk_error, :enoent}}
    end

    test "reload_file/1 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_repl_eval, :reload_file} => {:badrpc, :mock_scripted}})
      assert Workspace.reload_file("src/counter.bt") == {:error, {:unreachable, :mock_scripted}}
    end

    test "reload_file/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_eval, :reload_file} => :odd})
      assert Workspace.reload_file("src/counter.bt") == {:error, {:unexpected_reply, :odd}}
    end

    test "flush/0 returns the FlushResult summary on success" do
      mock_rpc(%{{:beamtalk_workspace_flush, :flush} => {:ok, %{flushed: 1}}})
      assert Workspace.flush() == {:ok, %{flushed: 1}}
    end

    test "flush/0 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_workspace_flush, :flush} => {:error, :disk_full},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :disk_full}
      })

      assert Workspace.flush() == {:error, {:beamtalk_error, :disk_full}}
    end

    test "flush/0 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_workspace_flush, :flush} => {:badrpc, :mock_scripted}})
      assert Workspace.flush() == {:error, {:unreachable, :mock_scripted}}
    end

    test "flush/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_workspace_flush, :flush} => :odd})
      assert Workspace.flush() == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "change_history/0 and reload_findings/0" do
    test "change_history/0 reduces raw entries to pending rows" do
      entries = [
        %{
          className: :Counter,
          selector: :increment,
          kind: :instance,
          intent: :durable,
          flushable: true,
          flushed: false,
          authorKind: :human,
          active: true,
          shadowed: false,
          clean: false,
          diff: nil,
          side: :instance
        }
      ]

      mock_rpc(%{{:beamtalk_workspace_changelog, :change_entries} => entries})
      assert [%{selector: "increment"}] = Workspace.change_history()
    end

    test "change_history/0 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_workspace_changelog, :change_entries} => {:badrpc, :mock_scripted}})
      assert Workspace.change_history() == {:error, {:unreachable, :mock_scripted}}
    end

    test "change_history/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_workspace_changelog, :change_entries} => :odd})
      assert Workspace.change_history() == {:error, {:unexpected_reply, :odd}}
    end

    test "reload_findings/0 normalizes each finding + its sites" do
      finding = %{
        owner: :Greeter,
        changed_class: :Counter,
        selector: :greet,
        classification: :breaking,
        severity: :error,
        category: :undefined,
        message: "Counter no longer responds to #foo",
        note: "see migration guide",
        sites: [%{method: :greet, line: 12}]
      }

      mock_rpc(%{{:beamtalk_workspace_findings_store, :all} => [finding]})

      assert {:ok, [normalized]} = Workspace.reload_findings()
      assert normalized.owner == "Greeter"
      assert normalized.category == nil
      assert normalized.note == "see migration guide"
      assert normalized.sites == [%{method: "greet", line: 12}]
    end

    test "reload_findings/0 surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_workspace_findings_store, :all} => {:badrpc, :mock_scripted}})
      assert Workspace.reload_findings() == {:error, {:unreachable, :mock_scripted}}
    end

    test "reload_findings/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_workspace_findings_store, :all} => :odd})
      assert Workspace.reload_findings() == {:error, {:unexpected_reply, :odd}}
    end

    test "normalize_reload_event/1 projects a ReloadCheckCompleted announcement" do
      event = %{
        changedClass: :Counter,
        checkedOwners: [:Greeter, :Widget],
        findings: [
          %{
            owner: :Greeter,
            changed_class: :Counter,
            selector: :greet,
            classification: :breaking,
            severity: :error,
            category: nil,
            message: "boom",
            note: nil,
            sites: []
          }
        ]
      }

      assert {"Counter", ["Greeter", "Widget"], [%{owner: "Greeter"}]} =
               Workspace.normalize_reload_event(event)
    end
  end

  describe "git-surface success paths (ADR 0082, BT-2586)" do
    test "git_status/0 returns the status map on success" do
      mock_rpc(%{{:beamtalk_git, :git_status} => {:ok, %{branch: "main"}}})
      assert Workspace.git_status() == {:ok, %{branch: "main"}}
    end

    test "git_status/0 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_git, :git_status} => {:error, :not_a_repo},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :not_a_repo}
      })

      assert Workspace.git_status() == {:error, {:beamtalk_error, :not_a_repo}}
    end

    test "git_status/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_git, :git_status} => :odd})
      assert Workspace.git_status() == {:error, {:unexpected_reply, :odd}}
    end

    test "git_diff/1 returns the diff map on success" do
      mock_rpc(%{{:beamtalk_git, :git_diff} => {:ok, %{worktree: "", staged: ""}}})
      assert Workspace.git_diff("src/foo.bt") == {:ok, %{worktree: "", staged: ""}}
    end

    # git_diff/1 has its own copy of the {:ok,_}/{:error,_}/{:badrpc,_}/other
    # case (it does not share git_mutate/2's helper), so the success test above
    # does not exercise its error/other branches — each needs its own test.
    test "git_diff/1 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_git, :git_diff} => {:error, :not_a_repo},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :not_a_repo}
      })

      assert Workspace.git_diff("src/foo.bt") == {:error, {:beamtalk_error, :not_a_repo}}
    end

    test "git_diff/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_git, :git_diff} => :odd})
      assert Workspace.git_diff("src/foo.bt") == {:error, {:unexpected_reply, :odd}}
    end

    test "git_log/1 returns the commit list on success" do
      mock_rpc(%{{:beamtalk_git, :git_log} => {:ok, [%{sha: "abc123"}]}})
      assert Workspace.git_log(5) == {:ok, [%{sha: "abc123"}]}
    end

    # Likewise git_log/1 has its own copy of the case, distinct from git_diff/1
    # and git_status/0's.
    test "git_log/1 structures a raw error" do
      mock_rpc(%{
        {:beamtalk_git, :git_log} => {:error, :not_a_repo},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :not_a_repo}
      })

      assert Workspace.git_log(5) == {:error, {:beamtalk_error, :not_a_repo}}
    end

    test "git_log/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_git, :git_log} => :odd})
      assert Workspace.git_log(5) == {:error, {:unexpected_reply, :odd}}
    end

    # Each wrapper is scripted ALONE (not alongside its three siblings): if
    # `git_unstage/1` ever called through to `:git_stage` (a copy/paste
    # routing mistake), only `:git_stage` would be scripted here and the
    # misrouted call would `flunk/1` as unscripted rather than quietly
    # succeeding off a neighboring stub.
    test "git_stage/1 normalizes an :ok reply to {:ok, nil}" do
      mock_rpc(%{{:beamtalk_git, :git_stage} => {:ok, :ignored}})
      assert Workspace.git_stage("a.bt") == {:ok, nil}
    end

    test "git_unstage/1 normalizes an :ok reply to {:ok, nil}" do
      mock_rpc(%{{:beamtalk_git, :git_unstage} => {:ok, :ignored}})
      assert Workspace.git_unstage("a.bt") == {:ok, nil}
    end

    test "git_commit/1 normalizes an :ok reply to {:ok, nil}" do
      mock_rpc(%{{:beamtalk_git, :git_commit} => {:ok, :ignored}})
      assert Workspace.git_commit("wip") == {:ok, nil}
    end

    test "git_revert_file/1 normalizes an :ok reply to {:ok, nil}" do
      mock_rpc(%{{:beamtalk_git, :git_revert_file} => {:ok, :ignored}})
      assert Workspace.git_revert_file("a.bt") == {:ok, nil}
    end

    test "git_commit/1 structures a raw mutation error" do
      mock_rpc(%{
        {:beamtalk_git, :git_commit} => {:error, :nothing_staged},
        {:beamtalk_repl_errors, :ensure_structured_error} => {:beamtalk_error, :nothing_staged}
      })

      assert Workspace.git_commit("wip") == {:error, {:beamtalk_error, :nothing_staged}}
    end

    test "git_stage/1 degrades an unrecognised mutation reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_git, :git_stage} => :odd})
      assert Workspace.git_stage("a.bt") == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "autoflush/0 (BT-2590, ADR 0082 Phase 4)" do
    test "reads the workspace's boolean autoflush setting" do
      mock_rpc(%{{:beamtalk_workspace_meta, :get_setting} => true})
      assert Workspace.autoflush() == true

      mock_rpc(%{{:beamtalk_workspace_meta, :get_setting} => false})
      assert Workspace.autoflush() == false
    end

    test "a non-boolean reply defaults to false rather than crashing" do
      mock_rpc(%{{:beamtalk_workspace_meta, :get_setting} => :not_a_boolean})
      assert Workspace.autoflush() == false
    end
  end

  describe "render_term/1 and render_error/1 (surface-consistent display)" do
    test "render_term/1 formats the term_to_json value via format_value/1" do
      mock_rpc(%{{:beamtalk_repl_json, :term_to_json} => %{"items" => [1, 2]}})
      assert Workspace.render_term(%{items: [1, 2]}) == "{items: #(1, 2)}"
    end

    test "render_term/1 falls back to inspect/1 when the formatter is unreachable" do
      mock_rpc(%{{:beamtalk_repl_json, :term_to_json} => {:badrpc, :mock_scripted}})
      assert Workspace.render_term(:some_value) == inspect(:some_value)
    end

    test "render_error/1 returns the formatted message on success" do
      mock_rpc(%{
        {:beamtalk_repl_json, :format_error_message} => "Counter does not understand #foo"
      })

      assert Workspace.render_error({:beamtalk_error, :dnu}) == "Counter does not understand #foo"
    end

    test "render_error/1 falls back to inspect/1 for a non-binary / unreachable reply" do
      mock_rpc(%{{:beamtalk_repl_json, :format_error_message} => {:badrpc, :mock_scripted}})
      assert Workspace.render_error(:some_reason) == inspect(:some_reason)
    end
  end

  describe "list_bindings/1" do
    test "returns name-sorted {name, term} pairs on success" do
      mock_rpc(%{{:beamtalk_repl_shell, :get_bindings} => {:ok, %{y: 2, x: 1}}})
      assert Workspace.list_bindings(self()) == [{"x", 1}, {"y", 2}]
    end

    test "surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_repl_shell, :get_bindings} => {:badrpc, :mock_scripted}})
      assert Workspace.list_bindings(self()) == {:error, {:unreachable, :mock_scripted}}
    end

    test "degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_shell, :get_bindings} => :odd})
      assert Workspace.list_bindings(self()) == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "inspect_value/1 + pid_stats/1 — inspector term dispatch (BT-2634/BT-2635/BT-2489)" do
    test "an object handle inspects its instance fields" do
      pid = self()

      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, {:inspect, %{"count" => 1}})
        )
      )

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, pid}) ==
               {:ok, %{"count" => 1}}
    end

    test "an object handle wraps a non-map inspect payload" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, {:inspect, "raw state"})
        )
      )

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:ok, "raw state"}
    end

    test "an object handle propagates an inspect error" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :boom})
        )
      )

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, :boom}
    end

    test "an object handle whose pid_to_list itself badrpcs is unreachable" do
      mock_rpc(%{{:erlang, :pid_to_list} => {:badrpc, :mock_scripted}})

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "an object handle whose decode step itself badrpcs is unreachable" do
      # Distinct from the pid_to_list badrpc above: pid_to_list succeeds here,
      # and it's dispatch_inspect/1's own decode step that badrpc's, hitting
      # both dispatch_inspect/1's `other -> other` catch-all AND
      # dispatch_inspect_result/1's {:badrpc, _} clause.
      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}
      })

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "an object handle whose inspect dispatch reply is wholly unrecognised" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, :odd)
        )
      )

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "a supervisor handle lists its children" do
      pid = self()
      row = %{"label" => "worker", "className" => "Widget", "handle" => nil}

      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_process_navigation, :child_handles} => {:ok, [row]}
      })

      assert Workspace.inspect_value({:beamtalk_supervisor, "Sup", Sup, pid}) ==
               {:ok, {:supervisor_children, [row]}}
    end

    test "a supervisor handle propagates a children error" do
      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_process_navigation, :child_handles} => {:error, :dead}
      })

      assert Workspace.inspect_value({:beamtalk_supervisor, "Sup", Sup, self()}) ==
               {:error, :dead}
    end

    test "supervisor_children/1 surfaces a badrpc from child_handles as unreachable" do
      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_process_navigation, :child_handles} => {:badrpc, :mock_scripted}
      })

      assert Workspace.supervisor_children(self()) == {:error, {:unreachable, :mock_scripted}}
    end

    test "supervisor_children/1 degrades an unrecognised child_handles reply" do
      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_process_navigation, :child_handles} => :odd
      })

      assert Workspace.supervisor_children(self()) == {:error, {:unexpected_reply, :odd}}
    end

    test "a future handle degrades to a minimal process-info snapshot" do
      mock_rpc(%{
        {:erlang, :process_info} => [
          status: :waiting,
          message_queue_len: 0,
          memory: 100,
          reductions: 5
        ]
      })

      assert Workspace.inspect_value({:beamtalk_future, self()}) ==
               {:ok,
                %{
                  "status" => :waiting,
                  "message_queue_len" => 0,
                  "memory" => 100,
                  "reductions" => 5
                }}
    end

    test "a bare pid handle degrades to an empty snapshot for a dead process" do
      mock_rpc(%{{:erlang, :process_info} => :undefined})
      assert Workspace.inspect_value(self()) == {:ok, %{}}
    end

    test "a non-ref term is not inspectable" do
      assert Workspace.inspect_value("just a string") == {:error, :not_inspectable}
      assert Workspace.inspect_value(42) == {:error, :not_inspectable}
    end

    test "pid_stats/1 returns the metrics map for an object handle" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, {:value, %{"memory" => 1024}})
        )
      )

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:ok, %{"memory" => 1024}}
    end

    test "pid_stats/1 propagates an error from the pid-stats op" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, {:error, :dead})
        )
      )

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, :dead}
    end

    test "pid_stats/1 surfaces a badrpc pid_to_list as unreachable" do
      mock_rpc(%{{:erlang, :pid_to_list} => {:badrpc, :mock_scripted}})

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "pid_stats/1 surfaces a badrpc from the decode step itself" do
      # Mirrors the inspect_value/1 case above: pid_to_list succeeds, and it's
      # dispatch_pid_stats/1's own decode step that badrpc's, hitting both its
      # own `other -> other` catch-all AND dispatch_pid_stats_result/1's
      # {:badrpc, _} clause.
      mock_rpc(%{
        {:erlang, :pid_to_list} => ~c"<0.99.0>",
        {:beamtalk_repl_protocol, :decode} => {:badrpc, :mock_scripted}
      })

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "pid_stats/1 degrades a wholly unrecognised dispatch reply to unexpected_reply" do
      mock_rpc(
        Map.merge(
          %{{:erlang, :pid_to_list} => ~c"<0.99.0>"},
          decode_ok({:beamtalk_repl_ops, :dispatch}, :odd)
        )
      )

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "pid_stats/1 rejects a non-pid-backed term" do
      assert Workspace.pid_stats("not a handle") == {:error, :not_inspectable}
    end
  end
end
