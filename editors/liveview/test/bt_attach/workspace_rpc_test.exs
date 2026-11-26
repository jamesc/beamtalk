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

  `BtAttach.Workspace.rpc/4` is a thin wrapper over `:erpc.call/5`, so these
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
  disconnected `:erpc.call/5` produces — a test asserting on `:mock_scripted`
  can only pass if `mock_rpc/1` actually intercepted the call, so it can't
  silently degrade into re-testing `workspace_test.exs`'s unreachable-node
  path if the mock is ever deleted or mis-keyed.

  `async: false`: `:meck` globally replaces the `:erpc` module for the whole
  VM. ExUnit runs every `async: true` module (including `workspace_test.exs`'s
  real-disconnected-node tests, which also call through `:erpc.call/5`) to
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
  that detached process's `:erpc.call/5` lands here too. `@safe_defaults` below
  scripts a harmless reply for that one call so a stray reap can't `flunk/1`
  from a foreign, untracked process (which wouldn't fail the test that
  triggered it anyway — `flunk/1` only fails the test *process* it runs in —
  but would otherwise crash-log the unrelated detached process).
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  # See the moduledoc's "residual hazard" note: a stray SessionRegistry reap
  # firing while this file's global `:erpc` mock is active must not encounter
  # an unscripted call. Merged under every `mock_rpc/1` call's own handlers so
  # a test can override it, though none need to.
  # The reap's close is the dispatched `close` operation (BT-5199), one
  # `beamtalk_repl_ops:request/3` call (BT-5201).
  @safe_defaults %{
    {:beamtalk_repl_ops, :request} => {:status, :ok}
  }

  setup do
    :meck.new(:erpc, [:unstick, :passthrough])
    on_exit(fn -> :meck.unload() end)
    :ok
  end

  # Script `:erpc.call/5`'s reply for each `{module, function}` pair the
  # function under test is expected to hit. A value is returned verbatim on
  # EVERY call to that pair (there is no sequencing — two different calls to
  # the same pair get the same scripted reply); a 1-arity function is invoked
  # with the call's `args` list instead, so a handler can branch on the
  # request payload (e.g. `beamtalk_repl_ops:request/3`'s first arg is the op
  # name) when a test needs two distinct replies from the same pair. A call to
  # a pair with no handler (beyond `@safe_defaults`) fails the test
  # immediately instead of falling through to a real (here meaningless)
  # network round-trip.
  defp mock_rpc(handlers) when is_map(handlers) do
    handlers = Map.merge(@safe_defaults, handlers)

    :meck.expect(:erpc, :call, fn _node, mod, fun, args, _timeout ->
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

  # Every dispatch_* helper is ONE `beamtalk_repl_ops:request/3` call (BT-5201),
  # so a test scripts just that call's reply (the first argument is the call
  # site's spelling of "the op-layer call").
  defp decode_ok({:beamtalk_repl_ops, :request}, reply),
    do: %{{:beamtalk_repl_ops, :request} => reply}

  # Like `decode_ok/2`, but for a wrapper whose behavior IS the request it
  # builds (which op name, which params) — `assertion` runs against the
  # request the workspace node receives (`"op"` merged into the flat params),
  # so a wrapper that sent the wrong op or dropped a param fails here rather
  # than only being provable by reading the source.
  defp decode_asserting(assertion, dispatch_reply) when is_function(assertion, 1) do
    %{
      {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
        assertion.(Map.put(params, "op", op))
        dispatch_reply
      end
    }
  end

  describe "start_session/1,2 — session creation (all rpc branches)" do
    test "start_session/2 returns the remote session pid on success" do
      pid = self()
      mock_rpc(%{{:beamtalk_repl_ops, :start_session} => {:ok, pid}})
      assert Workspace.start_session("sess-1", %{kind: "liveview"}) == pid
    end

    test "start_session/1 delegates to start_session/2 with a liveview meta" do
      pid = self()

      mock_rpc(%{
        {:beamtalk_repl_ops, :start_session} => fn ["sess-1", %{kind: "liveview"}] ->
          {:ok, pid}
        end
      })

      assert Workspace.start_session("sess-1") == pid
    end

    test "start_session/2 surfaces a badrpc as an unreachable error" do
      mock_rpc(%{{:beamtalk_repl_ops, :start_session} => {:badrpc, :mock_scripted}})

      assert Workspace.start_session("sess-1", %{}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "start_session/2 wraps any other reply as-is" do
      mock_rpc(%{{:beamtalk_repl_ops, :start_session} => {:error, :quota_exceeded}})
      assert Workspace.start_session("sess-1", %{}) == {:error, {:error, :quota_exceeded}}
    end
  end

  describe "attach_session/2 — through the shared session API (BT-5202)" do
    test "calls beamtalk_repl_ops:attach_session and keeps ok / gone / timeout apart" do
      pid = self()

      for {reply, expected} <- [ok: :ok, gone: {:error, :gone}, timeout: {:error, :timeout}] do
        mock_rpc(%{{:beamtalk_repl_ops, :attach_session} => fn [^pid, ^pid] -> reply end})
        assert Workspace.attach_session(pid, pid) == expected
      end
    end

    test "a badrpc is unreachable; any other reply is unexpected" do
      mock_rpc(%{{:beamtalk_repl_ops, :attach_session} => {:badrpc, :mock_scripted}})
      assert Workspace.attach_session(self(), self()) == {:error, {:unreachable, :mock_scripted}}

      mock_rpc(%{{:beamtalk_repl_ops, :attach_session} => :odd})
      assert Workspace.attach_session(self(), self()) == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "interrupt/1 — through the interrupt operation (BT-5202)" do
    test "dispatches `interrupt` with the session pid and answers :ok" do
      pid = self()

      mock_rpc(%{
        {:beamtalk_repl_ops, :request} => fn ["interrupt", %{}, ^pid] -> {:status, :ok} end
      })

      assert Workspace.interrupt(pid) == :ok
    end

    test "surfaces a structured error, a badrpc and an odd reply" do
      mock_rpc(%{{:beamtalk_repl_ops, :request} => {:error, :boom}})
      assert Workspace.interrupt(self()) == {:error, :boom}

      mock_rpc(%{{:beamtalk_repl_ops, :request} => {:badrpc, :mock_scripted}})
      assert Workspace.interrupt(self()) == {:error, {:unreachable, :mock_scripted}}

      mock_rpc(%{{:beamtalk_repl_ops, :request} => :odd})
      assert Workspace.interrupt(self()) == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "session_alive?/1 and session_count/0 (the session API, BT-5202)" do
    test "session_alive?/1 returns the boolean the workspace reports for that pid" do
      pid = self()
      mock_rpc(%{{:beamtalk_repl_ops, :session_alive} => fn [^pid] -> true end})
      assert Workspace.session_alive?(pid) == true

      mock_rpc(%{{:beamtalk_repl_ops, :session_alive} => false})
      assert Workspace.session_alive?(pid) == false
    end

    test "session_alive?/1 treats a badrpc as not-alive" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_alive} => {:badrpc, :mock_scripted}})
      assert Workspace.session_alive?(self()) == false
    end

    test "session_count/0 reads the active session count" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_count} => 3})
      assert Workspace.session_count() == 3
    end

    test "session_count/0 surfaces a badrpc as an unreachable error" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_count} => {:badrpc, :mock_scripted}})
      assert Workspace.session_count() == {:error, {:unreachable, :mock_scripted}}
    end
  end

  describe "eval/2 — term-returning eval seam (BT-2399)" do
    test "success returns the live term with stringified output/warnings" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:ok, 42, ~c"output", [:warn1]}))

      assert Workspace.eval(self(), "1 + 1") ==
               {:ok, 42, "output", [%{message: "warn1", hint: nil}]}
    end

    test "a compiler warning keeps its hint as its own field (BT-4449)" do
      warnings = [
        %{
          message: "Udp class does not conform to Transport class",
          hint: "missing: class defaultPort"
        },
        %{message: "no hint here"}
      ]

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:ok, 1, ~c"", warnings}))

      assert Workspace.eval(self(), "x") ==
               {:ok, 1, "",
                [
                  %{
                    message: "Udp class does not conform to Transport class",
                    hint: "missing: class defaultPort"
                  },
                  %{message: "no hint here", hint: nil}
                ]}
    end

    test "an error result carries stringified output/warnings alongside the reason" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :request},
          {:error, {:beamtalk_error, :boom}, ~c"partial", []}
        )
      )

      assert Workspace.eval(self(), "boom") ==
               {:error, {:beamtalk_error, :boom}, "partial", []}
    end

    test "a bare 2-tuple error (decode failure) degrades to empty output/warnings" do
      mock_rpc(%{{:beamtalk_repl_ops, :request} => {:error, :bad_json}})
      assert Workspace.eval(self(), "bad") == {:error, :bad_json, "", []}
    end

    test "a badrpc is reported as unreachable" do
      mock_rpc(%{{:beamtalk_repl_ops, :request} => {:badrpc, :mock_scripted}})
      assert Workspace.eval(self(), "x") == {:error, {:unreachable, :mock_scripted}, "", []}
    end

    test "an unrecognised reply degrades to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :something_weird))

      assert Workspace.eval(self(), "x") ==
               {:error, {:unexpected_reply, :something_weird}, "", []}
    end
  end

  describe "supervision_tree/2" do
    test "success returns the ProcessNavigation tree value" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:ok, [%{name: "root"}], ~c"", []}))
      assert Workspace.supervision_tree(self(), "default") == {:ok, [%{name: "root"}]}
    end

    test "an eval error propagates as the tree error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :boom, ~c"", []}))
      assert Workspace.supervision_tree(self(), "system") == {:error, :boom}
    end
  end

  describe "browse-surface dispatch_browse branch matrix (via browse_classes)" do
    test "a {:value, _} dispatch reply is returned verbatim" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, [%{"name" => "Counter"}]}))
      assert Workspace.browse_classes() == {:value, [%{"name" => "Counter"}]}
    end

    test "a {:error, _} dispatch reply is returned verbatim" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :unknown_class}))
      assert Workspace.browse_classes() == {:error, :unknown_class}
    end

    test "an unrecognised dispatch reply degrades to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.browse_classes() == {:error, {:unexpected_reply, :odd}}
    end

    test "a {:badrpc, _} reply from the request call is unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
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
        {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
          request = Map.put(params, "op", op)
          refute Map.has_key?(request, "old_name")
          refute Map.has_key?(request, "before_selector")
          refute Map.has_key?(request, "before_side")

          {:value, %{"ok" => true}}
        end
      })

      assert Workspace.save_section("Counter", "Accessing") == {:value, %{"ok" => true}}
    end

    test "save_section with old_name renames an existing category" do
      mock_rpc(%{
        {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
          request = Map.put(params, "op", op)
          assert %{"old_name" => "Old"} = request

          {:value, %{"ok" => true}}
        end
      })

      assert Workspace.save_section("Counter", "New", old_name: "Old") ==
               {:value, %{"ok" => true}}
    end

    test "save_section with before_selector + before_side inserts a new divider" do
      mock_rpc(%{
        {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
          request = Map.put(params, "op", op)

          assert %{"before_selector" => "increment", "before_side" => "class"} =
                   request

          {:value, %{"ok" => true}}
        end
      })

      assert Workspace.save_section("Counter", "New",
               before_selector: "increment",
               before_side: "class"
             ) == {:value, %{"ok" => true}}
    end
  end

  describe "completion-surface: complete/2, hover/2, diagnostics/1,2 (all branches)" do
    test "complete/2 returns stringified completion candidates" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:completions, [:foo, :bar]}))
      assert Workspace.complete(self(), "Coun") == {:ok, ["foo", "bar"]}
    end

    test "complete/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :bad_receiver}))
      assert Workspace.complete(self(), "x") == {:error, :bad_receiver}
    end

    test "complete/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.complete(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "complete/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.complete(self(), "x") == {:error, {:unexpected_reply, :odd}}
    end

    test "hover/2 returns the formatted docs string" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:docs, "Counter class docs"}))
      assert Workspace.hover(self(), "Counter") == {:ok, "Counter class docs"}
    end

    test "keyword_selector_at/2 returns the live nav-query value (BT-4455)" do
      mock_rpc(
        decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"selector" => "fetch:options:"}})
      )

      assert Workspace.keyword_selector_at("Fetcher fetch:", %{suffix: " u options: o"}) ==
               {:value, %{"selector" => "fetch:options:"}}
    end

    test "keyword_selector_at/2 surfaces a badrpc as unreachable (BT-4455)" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.keyword_selector_at("X a:") == {:error, {:unreachable, :mock_scripted}}
    end

    test "hover/3 forwards the editor context and returns the docs (BT-4343)" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:docs, "fetch:options: docs"}))

      assert Workspace.hover(self(), "  options:", %{before: "Fetcher fetch: u\n", suffix: " o"}) ==
               {:ok, "fetch:options: docs"}
    end

    test "hover/3 sends the edited class and side only when both are given (BT-4463)" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request["class"] == "Counter"
            assert request["side"] == "class"
          end,
          {:docs, "docs"}
        )
      )

      assert Workspace.hover(self(), "self size", %{class: "Counter", side: "class"}) ==
               {:ok, "docs"}

      mock_rpc(
        decode_asserting(
          fn request ->
            refute Map.has_key?(request, "class")
            refute Map.has_key?(request, "side")
          end,
          {:docs, "docs"}
        )
      )

      assert Workspace.hover(self(), "self size", %{class: "Counter"}) == {:ok, "docs"}
    end

    test "hover/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :unresolved}))
      assert Workspace.hover(self(), "x") == {:error, :unresolved}
    end

    test "hover/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.hover(self(), "x") == {:error, {:unreachable, :mock_scripted}}
    end

    test "hover/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.hover(self(), "x") == {:error, {:unexpected_reply, :odd}}
    end

    test "diagnostics/1 defaults mode to expression and normalizes each diagnostic" do
      diag = %{start: 0, end: 3, severity: :error, message: "bad token"}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:diagnostics, [diag]}))

      assert Workspace.diagnostics("1 +") ==
               {:ok, [%{"from" => 0, "to" => 3, "severity" => "error", "message" => "bad token"}]}
    end

    test "diagnostics/2 accepts an explicit mode and a non-map entry degrades gracefully" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:diagnostics, [:not_a_map]}))

      assert Workspace.diagnostics("body", "method") ==
               {:ok, [%{"from" => 0, "to" => 0, "severity" => "error", "message" => ""}]}
    end

    test "diagnostics/2 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :parse_failed}))
      assert Workspace.diagnostics("x", "expression") == {:error, :parse_failed}
    end

    test "diagnostics/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.diagnostics("x", "expression") == {:error, {:unreachable, :mock_scripted}}
    end

    test "diagnostics/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.diagnostics("x", "expression") == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "close_session/1, kill_actor/1, actors/0, sessions/0 — wire ops (BT-5199)" do
    test "close_session/1 dispatches `close` with the session as its SessionPid" do
      session = spawn(fn -> :ok end)
      test = self()

      mock_rpc(%{
        {:beamtalk_repl_ops, :request} => fn ["close", params, pid] ->
          assert params == %{}
          send(test, {:closed, pid})
          {:status, :ok}
        end
      })

      assert Workspace.close_session(session) == :ok
      assert_received {:closed, ^session}
    end

    test "close_session/1 surfaces a dispatch badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.close_session(self()) == {:error, {:unreachable, :mock_scripted}}
    end

    test "kill_actor/1 dispatches `kill` with the live pid, formatting nothing client-side" do
      pid = self()

      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "kill", "actor" => pid} end,
          {:status, :ok}
        )
      )

      assert Workspace.kill_actor({:beamtalk_object, :Counter, :counter, pid}) == :ok
    end

    test "kill_actor/1 passes a refusal through and rejects a non-actor term" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :system_actor}))

      assert Workspace.kill_actor({:beamtalk_object, :T, :t, self()}) == {:error, :system_actor}
      assert Workspace.kill_actor(42) == {:error, :not_inspectable}
    end

    test "kill_actor/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.kill_actor({:beamtalk_object, :T, :t, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "actors/0 and sessions/0 unwrap their op terms; the pid text comes from the op" do
      actor = %{pid: self(), class: :Counter, pid_text: "<0.77.0>"}

      mock_rpc(
        decode_ok({:beamtalk_repl_ops, :request}, fn
          ["actors", _, _] -> {:actors, [actor]}
          ["sessions", _, _] -> {:sessions, [%{id: "s1"}]}
        end)
      )

      assert Workspace.actors() == {:ok, [actor]}
      assert Workspace.sessions() == {:ok, [%{id: "s1"}]}
    end

    test "actors/0 makes one operation call and no pid formatting call" do
      test_pid = self()

      :meck.expect(:erpc, :call, fn _node, mod, fun, args, _timeout ->
        send(test_pid, {:called, mod, fun, List.first(args)})
        {:actors, [%{pid: self(), pid_text: "<0.1.0>"}]}
      end)

      assert {:ok, [_]} = Workspace.actors()
      assert_received {:called, :beamtalk_repl_ops, :request, "actors"}
      refute_received {:called, _, _, _}
    end

    test "subscribe_actors/1 and unsubscribe_actors/1 use the `actors` stream (BT-5205)" do
      pid = self()

      mock_rpc(%{
        {:beamtalk_repl_subscriptions, :subscribe} => fn [stream, ^pid] ->
          send(pid, {:subscribed, stream})
          :ok
        end,
        {:beamtalk_repl_subscriptions, :unsubscribe} => fn [stream, ^pid] ->
          send(pid, {:unsubscribed, stream})
          :ok
        end
      })

      assert Workspace.subscribe_actors(pid) == :ok
      assert_received {:subscribed, :actors}
      assert Workspace.unsubscribe_actors(pid) == :ok
      assert_received {:unsubscribed, :actors}
    end
  end

  describe "test-runner surface: list_tests/0, run_tests/1, load_tests/0 (BT-2557)" do
    test "list_tests/0 returns the discovered classes" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :request},
          {:value, %{"classes" => [%{"class" => "CounterTest"}]}}
        )
      )

      assert Workspace.list_tests() == {:ok, [%{"class" => "CounterTest"}]}
    end

    test "list_tests/0 degrades an unexpected value shape to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"nope" => true}}))
      assert Workspace.list_tests() == {:error, {:unexpected_reply, %{"nope" => true}}}
    end

    test "list_tests/0 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :discovery_failed}))
      assert Workspace.list_tests() == {:error, :discovery_failed}
    end

    test "list_tests/0 surfaces a badrpc at the decode step as unreachable" do
      mock_rpc(%{{:beamtalk_repl_ops, :request} => {:badrpc, :mock_scripted}})
      assert Workspace.list_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "list_tests/0 surfaces a badrpc from the request call as unreachable" do
      # dispatch_simple/2 returns the badrpc verbatim (no unreachable-wrapping)
      # for list_tests/0's own {:badrpc, _} clause to catch.
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.list_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "list_tests/0 degrades a wholly unrecognised dispatch reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
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
        {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
          request = Map.put(params, "op", op)
          assert %{"op" => "test-all"} = request

          {:test_results, result}
        end
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
        {:beamtalk_repl_ops, :request} => fn [op, params, _session_pid] ->
          request = Map.put(params, "op", op)
          assert %{"op" => "test", "class" => "CounterTest"} = request

          {:test_results, result}
        end
      })

      assert {:ok, normalized} = Workspace.run_tests("CounterTest")
      [entry] = normalized["tests"]
      assert entry["detail"] == inspect({:disabled, :flaky})
    end

    test "run_tests/1 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :execute_denied}))
      assert Workspace.run_tests(nil) == {:error, :execute_denied}
    end

    test "run_tests/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.run_tests(nil) == {:error, {:unreachable, :mock_scripted}}
    end

    test "run_tests/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.run_tests(nil) == {:error, {:unexpected_reply, :odd}}
    end

    test "load_tests/0 returns the loaded classes on success" do
      mock_rpc(
        decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"classes" => ["CounterTest"]}})
      )

      assert Workspace.load_tests() == {:ok, %{"classes" => ["CounterTest"]}}
    end

    test "load_tests/0 degrades an unexpected value shape to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"nope" => true}}))
      assert Workspace.load_tests() == {:error, {:unexpected_reply, %{"nope" => true}}}
    end

    test "load_tests/0 propagates a dispatch error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :compile_failed}))
      assert Workspace.load_tests() == {:error, :compile_failed}
    end

    test "load_tests/0 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.load_tests() == {:error, {:unreachable, :mock_scripted}}
    end

    test "load_tests/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.load_tests() == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "write-surface: save_method/3, new_class/2, revert/3, reload_file/1, flush/0" do
    test "save_method/3 dispatches save-method with structured params and returns the class" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request == %{
                     "op" => "save-method",
                     "class" => "Counter",
                     "selector" => "at:put:",
                     "source" => "\"quoted\" {braces} \\ ^self",
                     "author" => "liveview",
                     "author_kind" => "human"
                   }
          end,
          {:value, %{"class" => "Counter", "selector" => "at:put:"}}
        )
      )

      assert Workspace.save_method("Counter", "at:put:", "\"quoted\" {braces} \\ ^self") ==
               {:ok, "Counter"}
    end

    test "save_method/3 returns the operation's structured error as is" do
      err = {:beamtalk_error, :compile_failed}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.save_method("Counter", "increment", "bad(") == {:error, err}
    end

    test "save_method/3 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.save_method("Counter", "increment", "^self") ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "save_method/3 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))

      assert Workspace.save_method("Counter", "increment", "^self") ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "new_class/2 dispatches save-class and returns the target path" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request == %{
                     "op" => "save-class",
                     "source" => "Object subclass: Greeter",
                     "path" => "src/greeter.bt"
                   }
          end,
          {:value, %{"path" => "src/greeter.bt"}}
        )
      )

      assert Workspace.new_class("Object subclass: Greeter", "src/greeter.bt") ==
               {:ok, "src/greeter.bt"}
    end

    test "new_class/2 returns the ChangeLog warnings the operation reports" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :request},
          {:value, %{"path" => "src/g.bt", "warnings" => ["ChangeLog append failed"]}}
        )
      )

      assert Workspace.new_class("Object subclass: G", "src/g.bt") ==
               {:ok, "src/g.bt", ["ChangeLog append failed"]}
    end

    test "new_class/2 returns the operation's structured error as is" do
      err = {:beamtalk_error, :name_clash}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.new_class("Object subclass: Greeter", "src/greeter.bt") == {:error, err}
    end

    test "new_class/2 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.new_class("Object subclass: G", "src/g.bt") ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "new_class/2 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))

      assert Workspace.new_class("Object subclass: G", "src/g.bt") ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "revert/3 dispatches revert-method, omitting an unknown side" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request == %{
                     "op" => "revert-method",
                     "class" => "Counter",
                     "selector" => "increment"
                   }
          end,
          {:value, %{"class" => "Counter", "selector" => "increment"}}
        )
      )

      assert Workspace.revert("Counter", "increment") == {:ok, "Counter"}
    end

    test "revert/3 threads the side through and returns the structured error" do
      err = {:beamtalk_error, :not_revertable}

      mock_rpc(
        decode_asserting(
          fn request -> assert request["side"] == "class" end,
          {:error, err}
        )
      )

      assert Workspace.revert("Counter", "increment", "class") == {:error, err}
    end

    test "revert/3 sends an atom side as its wire string" do
      mock_rpc(
        decode_asserting(
          fn request -> assert request["side"] == "instance" end,
          {:value, %{"class" => "Counter", "selector" => "increment"}}
        )
      )

      assert Workspace.revert("Counter", "increment", :instance) == {:ok, "Counter"}
    end

    test "revert/3 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.revert("Counter", "increment") == {:error, {:unreachable, :mock_scripted}}
    end

    test "revert/3 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.revert("Counter", "increment") == {:error, {:unexpected_reply, :odd}}
    end

    test "reload_file/1 dispatches reload-file and returns the reloaded class names" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request == %{"op" => "reload-file", "path" => "src/counter.bt"}
          end,
          {:value, %{"classes" => ["Counter", "Greeter"]}}
        )
      )

      assert Workspace.reload_file("src/counter.bt") == {:ok, ["Counter", "Greeter"]}
    end

    test "reload_file/1 returns the operation's structured error as is" do
      err = {:beamtalk_error, :file_not_found}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.reload_file("src/missing.bt") == {:error, err}
    end

    test "reload_file/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.reload_file("src/counter.bt") == {:error, {:unreachable, :mock_scripted}}
    end

    test "reload_file/1 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.reload_file("src/counter.bt") == {:error, {:unexpected_reply, :odd}}
    end

    test "load_source/2 dispatches load-source in the caller's session" do
      pid = self()

      mock_rpc(%{
        {:beamtalk_repl_ops, :request} => fn [
                                               "load-source",
                                               %{"source" => "Object subclass: Foo"},
                                               ^pid
                                             ] ->
          {:loaded, [%{"name" => "Foo"}], [%{message: "unused", hint: nil}, "plain"]}
        end
      })

      assert Workspace.load_source(pid, "Object subclass: Foo") ==
               {:ok, [%{"name" => "Foo"}],
                [%{message: "unused", hint: nil}, %{message: "plain", hint: nil}]}
    end

    test "load_source/2 returns the structured error, unreachable and unexpected replies" do
      pid = self()
      err = {:beamtalk_error, :compile_error}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.load_source(pid, "bad") == {:error, err}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.load_source(pid, "x") == {:error, {:unreachable, :mock_scripted}}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.load_source(pid, "x") == {:error, {:unexpected_reply, :odd}}
    end

    test "flush/0 dispatches `flush` with no params and returns the summary" do
      summary = %{"flushed" => 1, "files" => ["src/a.bt"], "skipped" => [], "conflicts" => []}

      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "flush"} end,
          {:value, summary}
        )
      )

      assert Workspace.flush() == {:ok, summary}
    end

    test "flush/1 sends the scope and the confirmation as the operation's own params" do
      mock_rpc(
        decode_asserting(
          fn request ->
            assert request == %{
                     "op" => "flush",
                     "class" => "Counter",
                     "confirm_destructive" => true
                   }
          end,
          {:value, %{"flushed" => 0}}
        )
      )

      assert {:ok, _} = Workspace.flush(class: "Counter", confirm_destructive: true)

      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "flush", "kind" => "new-class"} end,
          {:value, %{}}
        )
      )

      assert {:ok, _} = Workspace.flush(kind: "new-class")

      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "flush", "file" => "src/a.bt"} end,
          {:value, %{}}
        )
      )

      assert {:ok, _} = Workspace.flush(file: "src/a.bt")
    end

    test "flush/0 passes a structured error through" do
      err = {:beamtalk_error, :disk_full}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))

      assert Workspace.flush() == {:error, err}
    end

    test "flush/0 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.flush() == {:error, {:unreachable, :mock_scripted}}
    end

    test "flush/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.flush() == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "change_history/0 and reload_findings/0" do
    test "change_history/0 dispatches `changes` and reduces the entries to pending rows" do
      entries = [
        %{
          "seq" => 1,
          "className" => "Counter",
          "selector" => "increment",
          "kind" => "instance",
          "intent" => "durable",
          "flushable" => true,
          "flushed" => false,
          "authorKind" => "human",
          "active" => true,
          "shadowed" => false,
          "clean" => false,
          "diff" => :null,
          "side" => "instance",
          "oldClass" => :null
        },
        # Flushed (inactive), shadowed and clean entries stay out of the pane.
        %{"className" => "Counter", "selector" => "gone", "active" => false},
        %{"className" => "Counter", "selector" => "old", "active" => true, "shadowed" => true},
        %{"className" => "Counter", "selector" => "same", "active" => true, "clean" => true}
      ]

      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "changes"} end,
          {:value, %{"entries" => entries}}
        )
      )

      assert [row] = Workspace.change_history()

      assert %{
               class: "Counter",
               selector: "increment",
               kind: "instance",
               side: "instance",
               diff: nil,
               flushable: true,
               author_kind: "human"
             } = row
    end

    test "change_history/0 reads a null selector (a new-class entry) as the class placeholder" do
      entry = %{
        "className" => "Greeter",
        "selector" => :null,
        "kind" => "new-class",
        "side" => :null,
        "active" => true
      }

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"entries" => [entry]}}))

      assert [%{selector: "(class)", side: nil}] = Workspace.change_history()
    end

    test "change_history/0 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.change_history() == {:error, {:unreachable, :mock_scripted}}
    end

    test "change_history/0 passes an operation error through" do
      err = {:beamtalk_error, :no_changelog}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.change_history() == {:error, err}
    end

    test "change_history/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.change_history() == {:error, {:unexpected_reply, :odd}}
    end

    test "reload_findings/0 dispatches `reload-findings` and normalizes each finding + its sites" do
      finding = %{
        "owner" => "Greeter",
        "changedClass" => "Counter",
        "selector" => "greet",
        "classification" => "breaking",
        "severity" => "error",
        "category" => :null,
        "message" => "Counter no longer responds to #foo",
        "note" => "see migration guide",
        "sites" => [%{"method" => "greet", "line" => 12}],
        "start" => 0,
        "end" => 4
      }

      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "reload-findings"} end,
          {:value, %{"findings" => [finding]}}
        )
      )

      assert {:ok, [normalized]} = Workspace.reload_findings()
      assert normalized.owner == "Greeter"
      assert normalized.changed_class == "Counter"
      assert normalized.category == nil
      assert normalized.note == "see migration guide"
      assert normalized.sites == [%{method: "greet", line: 12}]
    end

    test "reload_findings/0 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.reload_findings() == {:error, {:unreachable, :mock_scripted}}
    end

    test "reload_findings/0 degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
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

  describe "git operations (ADR 0082, BT-2586, BT-5205)" do
    @git_file %{"path" => "src/a.bt", "index" => "unmodified", "worktree" => "modified"}

    test "git_status/0 dispatches `git-status` and reads the files into atom-keyed maps" do
      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "git-status"} end,
          {:value,
           %{
             "branch" => "main",
             "upstream" => :null,
             "ahead" => 1,
             "behind" => 0,
             "truncated" => false,
             "files" => [@git_file, %{@git_file | "path" => "x.bt", "worktree" => "weird_new"}]
           }}
        )
      )

      assert Workspace.git_status() ==
               {:ok,
                %{
                  branch: "main",
                  upstream: nil,
                  ahead: 1,
                  behind: 0,
                  truncated: false,
                  files: [
                    %{path: "src/a.bt", index: :unmodified, worktree: :modified},
                    # A status word this node does not know stays text: no atom is made.
                    %{path: "x.bt", index: :unmodified, worktree: "weird_new"}
                  ]
                }}
    end

    test "git_status/0 passes an operation error through" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :not_a_repo}))
      assert Workspace.git_status() == {:error, :not_a_repo}
    end

    test "git_status/0 surfaces a badrpc as unreachable and a bad reply as unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.git_status() == {:error, {:unreachable, :mock_scripted}}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.git_status() == {:error, {:unexpected_reply, :odd}}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"nope" => 1}}))
      assert Workspace.git_status() == {:error, {:unexpected_reply, {:value, %{"nope" => 1}}}}
    end

    test "git_diff/1 dispatches `git-diff` with the path as a structured param" do
      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "git-diff", "path" => "src/foo.bt"} end,
          {:value, %{"path" => "src/foo.bt", "worktree" => "w", "staged" => "s"}}
        )
      )

      assert Workspace.git_diff("src/foo.bt") == {:ok, %{worktree: "w", staged: "s"}}
    end

    test "git_diff/1 passes a refused path through, and degrades odd replies" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :invalid_argument}))
      assert Workspace.git_diff("../x") == {:error, :invalid_argument}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.git_diff("a.bt") == {:error, {:unexpected_reply, :odd}}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.git_diff("a.bt") == {:error, {:unreachable, :mock_scripted}}
    end

    test "git_log/1 dispatches `git-log` with the count and reads commits into atom keys" do
      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "git-log", "count" => 5} end,
          {:value,
           %{
             "limit" => 5,
             "commits" => [
               %{
                 "sha" => "abc123",
                 "short_sha" => "abc",
                 "subject" => "s",
                 "author" => "a",
                 "ts" => "t",
                 "extra" => 1
               }
             ]
           }}
        )
      )

      assert Workspace.git_log(5) ==
               {:ok, [%{sha: "abc123", short_sha: "abc", subject: "s", author: "a", ts: "t"}]}
    end

    test "git_log/1 passes errors through and degrades odd replies" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :not_a_repo}))
      assert Workspace.git_log(5) == {:error, :not_a_repo}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.git_log(5) == {:error, {:unexpected_reply, :odd}}
    end

    # Each wrapper sends ITS OWN op and params: a copy/paste routing mistake
    # (unstage calling `git-stage`) fails the request assertion.
    for {fun, op, arg, param} <- [
          {:git_stage, "git-stage", "a.bt", "path"},
          {:git_unstage, "git-unstage", "a.bt", "path"},
          {:git_revert_file, "git-revert-file", "a.bt", "path"},
          {:git_commit, "git-commit", "wip; $(rm -rf /)", "message"}
        ] do
      test "#{fun}/1 sends `#{op}` with `#{param}` and answers {:ok, nil}" do
        arg = unquote(arg)
        op = unquote(op)
        param = unquote(param)

        mock_rpc(
          decode_asserting(
            fn req -> assert req == %{"op" => op, param => arg} end,
            {:value, %{"path" => arg}}
          )
        )

        assert apply(Workspace, unquote(fun), [arg]) == {:ok, nil}
      end

      test "#{fun}/1 passes an error through and degrades an unrecognised reply" do
        mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :nothing_staged}))
        assert apply(Workspace, unquote(fun), ["a.bt"]) == {:error, :nothing_staged}

        mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
        assert apply(Workspace, unquote(fun), ["a.bt"]) == {:error, {:unexpected_reply, :odd}}

        mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

        assert apply(Workspace, unquote(fun), ["a.bt"]) ==
                 {:error, {:unreachable, :mock_scripted}}
      end
    end
  end

  describe "info/0 and autoflush/0 (BT-2590, BT-5204)" do
    @info %{
      "version" => %{
        "runtime_version" => "1.2.3",
        "protocol_version" => "2.0",
        "otp_release" => "27",
        "erts_version" => "15",
        "future_field" => "kept"
      },
      "node" => "workspace@host",
      "workspace_id" => "ws-1",
      "settings" => %{"autoflush" => true}
    }

    test "info/0 dispatches `info` and keeps the version report's atom keys" do
      mock_rpc(
        decode_asserting(
          fn request -> assert request == %{"op" => "info"} end,
          {:value, @info}
        )
      )

      assert {:ok, info} = Workspace.info()

      assert info == %{
               version: %{
                 # A key a newer workspace adds is kept, never turned into an atom.
                 "future_field" => "kept",
                 runtime_version: "1.2.3",
                 protocol_version: "2.0",
                 otp_release: "27",
                 erts_version: "15"
               },
               node: "workspace@host",
               workspace_id: "ws-1",
               settings: %{autoflush: true}
             }
    end

    test "info/0 surfaces a badrpc, an operation error and an odd reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.info() == {:error, {:unreachable, :mock_scripted}}

      err = {:beamtalk_error, :unknown_op}
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, err}))
      assert Workspace.info() == {:error, err}

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.info() == {:error, {:unexpected_reply, :odd}}
    end

    test "autoflush/0 reads the boolean setting from `info`" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, @info}))
      assert Workspace.autoflush() == true

      off = put_in(@info, ["settings", "autoflush"], false)
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, off}))
      assert Workspace.autoflush() == false
    end

    test "a non-boolean setting, an error or an unreachable workspace defaults autoflush to false" do
      odd = put_in(@info, ["settings", "autoflush"], "yes")
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, odd}))
      assert Workspace.autoflush() == false

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.autoflush() == false
    end
  end

  describe "render_term/1 and render_error/1 (surface-consistent display)" do
    test "render_term/1 formats the term_to_json value via format_value/1" do
      mock_rpc(%{{:beamtalk_repl_ops, :render} => %{"items" => [1, 2]}})
      assert Workspace.render_term(%{items: [1, 2]}) == "{items: #(1, 2)}"
    end

    test "render_term/1 falls back to inspect/1 when the formatter is unreachable" do
      mock_rpc(%{{:beamtalk_repl_ops, :render} => {:badrpc, :mock_scripted}})
      assert Workspace.render_term(:some_value) == inspect(:some_value)
    end

    test "render_error/1 returns the formatted message on success" do
      mock_rpc(%{
        {:beamtalk_repl_ops, :render} =>
          {{:beamtalk_error, :dnu}, "Counter does not understand #foo"}
      })

      assert Workspace.render_error({:beamtalk_error, :dnu}) == "Counter does not understand #foo"
    end

    test "render_error/1 falls back to inspect/1 for a non-binary / unreachable reply" do
      mock_rpc(%{{:beamtalk_repl_ops, :render} => {:badrpc, :mock_scripted}})
      assert Workspace.render_error(:some_reason) == inspect(:some_reason)
    end
  end

  describe "list_bindings/1" do
    test "returns name-sorted {name, term} pairs on success" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_bindings} => {:ok, %{y: 2, x: 1}}})
      assert Workspace.list_bindings(self()) == [{"x", 1}, {"y", 2}]
    end

    test "surfaces a badrpc as unreachable" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_bindings} => {:badrpc, :mock_scripted}})
      assert Workspace.list_bindings(self()) == {:error, {:unreachable, :mock_scripted}}
    end

    test "surfaces a structured error (a session that has ended)" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_bindings} => {:error, :session_gone}})
      assert Workspace.list_bindings(self()) == {:error, :session_gone}
    end

    test "degrades an unrecognised reply to unexpected_reply" do
      mock_rpc(%{{:beamtalk_repl_ops, :session_bindings} => :odd})
      assert Workspace.list_bindings(self()) == {:error, {:unexpected_reply, :odd}}
    end
  end

  describe "inspect_value/1 + pid_stats/1 — inspector term dispatch (BT-2634/BT-2635/BT-2489)" do
    test "an object handle inspects its instance fields" do
      pid = self()

      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:inspect, %{"count" => 1}}))

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, pid}) ==
               {:ok, %{"count" => 1}}
    end

    test "an object handle wraps a non-map inspect payload" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:inspect, "raw state"}))

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:ok, "raw state"}
    end

    test "an object handle propagates an inspect error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :boom}))

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, :boom}
    end

    test "an object handle sends the live pid to `inspect`, formatting nothing client-side" do
      pid = self()

      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "inspect", "actor" => pid} end,
          {:inspect, %{"count" => 1}}
        )
      )

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, pid}) ==
               {:ok, %{"count" => 1}}
    end

    test "an object handle whose inspect dispatch badrpcs is unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "an object handle whose inspect dispatch reply is wholly unrecognised" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))

      assert Workspace.inspect_value({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "a supervisor handle lists its children through the `children` op" do
      pid = self()
      row = %{"label" => "worker", "className" => "Widget", "handle" => nil}

      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "children", "supervisor" => pid} end,
          {:value, %{"children" => [row], "truncated" => false}}
        )
      )

      assert Workspace.inspect_value({:beamtalk_supervisor, "Sup", Sup, pid}) ==
               {:ok, {:supervisor_children, [row]}}
    end

    test "a supervisor handle propagates a children error" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :dead}))

      assert Workspace.inspect_value({:beamtalk_supervisor, "Sup", Sup, self()}) ==
               {:error, :dead}
    end

    test "supervisor_children/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.supervisor_children(self()) == {:error, {:unreachable, :mock_scripted}}
    end

    test "supervisor_children/1 degrades an unrecognised reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))
      assert Workspace.supervisor_children(self()) == {:error, {:unexpected_reply, :odd}}
    end

    test "a future handle degrades to a minimal process-info snapshot (`process-info` op)" do
      pid = self()

      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "process-info", "pid" => pid} end,
          {:value,
           %{
             "pid" => "<0.1.0>",
             "alive" => true,
             "status" => "waiting",
             "message_queue_len" => 0,
             "memory" => 100,
             "reductions" => 5
           }}
        )
      )

      assert Workspace.inspect_value({:beamtalk_future, pid}) ==
               {:ok,
                %{
                  "status" => "waiting",
                  "message_queue_len" => 0,
                  "memory" => 100,
                  "reductions" => 5
                }}
    end

    test "a bare pid handle degrades to an empty snapshot for a dead process" do
      mock_rpc(
        decode_ok(
          {:beamtalk_repl_ops, :request},
          {:value, %{"pid" => "<0.1.0>", "alive" => false}}
        )
      )

      assert Workspace.inspect_value(self()) == {:ok, %{}}
    end

    test "a bare pid handle degrades to an empty snapshot when the workspace is unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))
      assert Workspace.inspect_value(self()) == {:ok, %{}}
    end

    test "a non-ref term is not inspectable" do
      assert Workspace.inspect_value("just a string") == {:error, :not_inspectable}
      assert Workspace.inspect_value(42) == {:error, :not_inspectable}
    end

    test "pid_stats/1 returns the metrics map for an object handle" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:value, %{"memory" => 1024}}))

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:ok, %{"memory" => 1024}}
    end

    test "pid_stats/1 propagates an error from the pid-stats op" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:error, :dead}))

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, :dead}
    end

    test "pid_stats/1 sends the live pid to `pid-stats`" do
      pid = self()

      mock_rpc(
        decode_asserting(
          fn req -> assert req == %{"op" => "pid-stats", "actor" => pid} end,
          {:value, %{"memory" => 1}}
        )
      )

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, pid}) ==
               {:ok, %{"memory" => 1}}
    end

    test "pid_stats/1 surfaces a badrpc as unreachable" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, {:badrpc, :mock_scripted}))

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unreachable, :mock_scripted}}
    end

    test "pid_stats/1 degrades a wholly unrecognised dispatch reply to unexpected_reply" do
      mock_rpc(decode_ok({:beamtalk_repl_ops, :request}, :odd))

      assert Workspace.pid_stats({:beamtalk_object, "Counter", Counter, self()}) ==
               {:error, {:unexpected_reply, :odd}}
    end

    test "pid_stats/1 rejects a non-pid-backed term" do
      assert Workspace.pid_stats("not a handle") == {:error, :not_inspectable}
    end
  end

  describe "bounded rpc (BT-5081)" do
    test "a timed-out workspace call surfaces as :unreachable, never a hang or crash" do
      :meck.expect(:erpc, :call, fn _node, _mod, _fun, _args, _timeout ->
        :erlang.error({:erpc, :timeout})
      end)

      assert Workspace.session_count() == {:error, {:unreachable, :timeout}}
      assert Workspace.eval(self(), "1 + 1") == {:error, {:unreachable, :timeout}, "", []}
      assert Workspace.complete(self(), "Int") == {:error, {:unreachable, :timeout}}
    end

    test "every call carries a finite timeout, and eval gets the longest one" do
      test_pid = self()

      :meck.expect(:erpc, :call, fn _node, mod, fun, args, timeout ->
        send(test_pid, {:rpc_timeout, mod, fun, List.first(args), timeout})

        case {mod, fun} do
          {:beamtalk_repl_ops, :request} -> {:ok, 2, "", []}
        end
      end)

      assert {:ok, 2, "", []} = Workspace.eval(self(), "1 + 1")
      assert {:ok, 2, "", []} = Workspace.eval(self(), "1 + 1")

      # eval is ONE op-layer call (BT-5201) carrying the long eval bound.
      assert_received {:rpc_timeout, :beamtalk_repl_ops, :request, "eval", eval_timeout}
      assert eval_timeout == Workspace.rpc_eval_timeout()
      assert eval_timeout > 10_000
      refute_received {:rpc_timeout, :beamtalk_repl_protocol, _, _, _}
    end

    test "close_session/1 bounds its one call by the short interactive timeout (BT-5199)" do
      test_pid = self()

      :meck.expect(:erpc, :call, fn _node, mod, fun, _args, timeout ->
        send(test_pid, {:rpc_timeout, mod, fun, timeout})
        {:status, :ok}
      end)

      assert Workspace.close_session(self()) == :ok

      # One call now: the decode/get_params prelude is gone (BT-5201).
      assert_received {:rpc_timeout, :beamtalk_repl_ops, :request, timeout}
      refute_received {:rpc_timeout, _, _, _}
      assert timeout <= 5_000, "close waited up to #{inspect(timeout)} ms"
    end

    test "render_terms/1 renders every term with ONE round-trip, in order" do
      test_pid = self()

      :meck.expect(:erpc, :call, fn _node,
                                    :beamtalk_repl_ops,
                                    :render,
                                    [:values, terms],
                                    _timeout ->
        send(test_pid, {:round_trip, length(terms)})
        Enum.map(terms, &Integer.to_string/1)
      end)

      assert Workspace.render_terms([1, 2, 3]) == ["1", "2", "3"]
      assert_received {:round_trip, 3}
      refute_received {:round_trip, _}
    end

    test "render_terms/1 degrades per term when the batch fails" do
      :meck.expect(:erpc, :call, fn
        _node, :beamtalk_repl_ops, :render, [:values, _terms], _timeout ->
          :erlang.error({:erpc, :timeout})

        _node, :beamtalk_repl_ops, :render, [:value, 2], _timeout ->
          "two"

        _node, :beamtalk_repl_ops, :render, [:value, _term], _timeout ->
          :erlang.error({:erpc, :timeout})
      end)

      assert Workspace.render_terms([1, 2]) == ["1", "two"]
    end

    test "a runtime-side eval timeout (a remote exit) is reported as :timeout" do
      :meck.expect(:erpc, :call, fn
        _node, :beamtalk_repl_ops, :request, _args, _t ->
          :erlang.exit({:exception, {:timeout, {:gen_server, :call, []}}})
      end)

      assert Workspace.eval(self(), "[true] whileTrue") ==
               {:error, {:unreachable, :timeout}, "", []}
    end

    test "a remote exception keeps the {:badrpc, {:EXIT, _}} shape" do
      :meck.expect(:erpc, :call, fn _node, _m, _f, _a, _t ->
        :erlang.error({:exception, :boom, []})
      end)

      assert {:error, {:unreachable, {:EXIT, {:boom, []}}}} = Workspace.session_count()
    end
  end
end
