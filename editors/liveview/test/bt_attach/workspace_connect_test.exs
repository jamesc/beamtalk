# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceConnectTest do
  @moduledoc """
  Direct unit tests for `BtAttach.Workspace.connect/0`, `readiness/0`, and the
  private `ensure_distributed/0` + `set_cookie/0` helpers behind them (BT-3309).

  None of these three functions go through `BtAttach.Workspace.rpc/3` — they
  drive real distribution primitives directly (`Node.alive?/0`,
  `Node.connect/1`, `Node.set_cookie/2`, `:net_kernel.start/1`,
  `:gen_tcp.connect/4`), so `workspace_rpc_test.exs`'s `:rpc`-only meck cannot
  shape their branches. This sandbox also has no real epmd on the standard
  port and this BEAM isn't distributed (confirmed empirically — see the "no
  local epmd" describe block below), so `:workspace`-tagged
  `readiness/0` in `workspace_test.exs` is the only place these functions were
  previously exercised at all.

  `Node` is meck'd for the whole file (every test needs deterministic control
  over `alive?/0` / `connect/1` / `set_cookie/2` rather than depending on this
  sandbox's incidental non-distributed state) with `:passthrough`, so a test
  that doesn't override a given function gets its real (here: not-alive,
  disconnected) behaviour. `:net_kernel` and `:gen_tcp` are meck'd only in the
  describe block that needs to fake a reachable local epmd, to keep their
  blast radius as small as possible.

  `async: false`, like `workspace_rpc_test.exs` and
  `workspace_classify_unreachable_hostname_test.exs`: meck globally replaces
  each mocked module for the whole VM, and ExUnit runs every `async: true`
  module to completion before any `async: false` module starts, so there is no
  interleaving with other suites' real distribution/RPC calls.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  setup do
    :meck.new(Node, [:unstick, :passthrough])
    # :meck.unload/0 (no args), not :meck.unload(Node): meck ties a mock's
    # lifecycle to its owning process and auto-unloads when that process
    # exits, which has already happened by the time on_exit runs (a separate
    # runner process) — see workspace_classify_unreachable_hostname_test.exs's
    # moduledoc. The no-arg form tolerates an already-gone mock; the 1-arg
    # form raises {not_mocked, _} on it.
    on_exit(fn -> :meck.unload() end)
    :ok
  end

  describe "connect/0 — ensure_distributed/0 already :ok (Node.alive? true)" do
    test "Node.connect succeeds -> :ok (set_cookie's unset-env branch runs)" do
      System.delete_env("BT_WORKSPACE_COOKIE")
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> true end)

      assert Workspace.connect() == :ok
    end

    test "Node.connect returns false -> connect_failed/false (set_cookie's empty-token branch runs)" do
      System.put_env("BT_WORKSPACE_COOKIE", "")
      on_exit(fn -> System.delete_env("BT_WORKSPACE_COOKIE") end)
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> false end)

      assert Workspace.connect() ==
               {:error, {:connect_failed, Workspace.node_name(), false}}
    end

    test "Node.connect returns :ignored -> connect_failed/:ignored (set_cookie sets the real cookie)" do
      System.put_env("BT_WORKSPACE_COOKIE", "sometoken")
      on_exit(fn -> System.delete_env("BT_WORKSPACE_COOKIE") end)
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> :ignored end)
      :meck.expect(Node, :set_cookie, fn _node, :sometoken -> true end)

      assert Workspace.connect() ==
               {:error, {:connect_failed, Workspace.node_name(), :ignored}}
    end
  end

  describe "connect/0 and readiness/0 — no local epmd" do
    # No mocking here beyond the file-wide Node passthrough: this sandbox
    # genuinely has neither a live epmd on the standard port nor a
    # distributed BEAM (confirmed with `Node.alive?/0` and
    # `Workspace.epmd_reachable?/0` directly), so `ensure_distributed/0` takes
    # its real not-alive + epmd-unreachable path with no mocking required —
    # the same "against an unreachable workspace" philosophy
    # `workspace_test.exs` uses for the RPC wrappers.
    test "connect/0 reports epmd_absent" do
      assert Workspace.connect() == {:error, :epmd_absent}
    end

    test "readiness/0 reports epmd_absent" do
      assert Workspace.readiness() == {:error, :epmd_absent}
    end
  end

  describe "readiness/0 — connect_failed branches" do
    test "connect_failed/false is classified via classify_unreachable/1" do
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> false end)

      # classify_unreachable/1's own default arg queries the real
      # :net_adm.names(:localhost) — which fails in this sandbox's no-epmd
      # environment the same way connect/0's own epmd probe does, so the
      # taxonomy folds this to :epmd_absent (classify_unreachable/2's branch
      # matrix against a scripted epmd_names list is unit-tested directly in
      # workspace_test.exs).
      assert Workspace.readiness() == {:error, :epmd_absent}
    end

    test "connect_failed/:ignored folds into :dead_workspace" do
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> :ignored end)

      assert Workspace.readiness() == {:error, :dead_workspace}
    end
  end

  describe "readiness/0 — connect :ok, readiness_rpc/0 branches" do
    setup do
      :meck.new(:rpc, [:unstick, :passthrough])
      on_exit(fn -> :meck.unload() end)
      :meck.expect(Node, :alive?, fn -> true end)
      :meck.expect(Node, :connect, fn _node -> true end)
      :ok
    end

    # Each expect below passes through any call that isn't the expected
    # beamtalk_version:get/0 rather than a bare match failure, so a stray call
    # from elsewhere (e.g. a leftover SessionRegistry reap timer — the same
    # residual hazard workspace_rpc_test.exs's moduledoc documents) degrades
    # to a harmless real badrpc instead of crashing an unrelated process.
    test "a badrpc version report is :dead_workspace" do
      :meck.expect(:rpc, :call, fn
        _node, :beamtalk_version, :get, [] -> {:badrpc, :mock_scripted}
        node, mod, fun, args -> :meck.passthrough([node, mod, fun, args])
      end)

      assert Workspace.readiness() == {:error, :dead_workspace}
    end

    test "a version-report map succeeds with :ok" do
      report = %{
        runtime_version: "1.2.3",
        protocol_version: 4,
        otp_release: "27",
        erts_version: "15"
      }

      :meck.expect(:rpc, :call, fn
        _node, :beamtalk_version, :get, [] -> report
        node, mod, fun, args -> :meck.passthrough([node, mod, fun, args])
      end)

      assert Workspace.readiness() == {:ok, report}
    end

    test "a wholly unrecognised version-report reply degrades to :dead_workspace" do
      :meck.expect(:rpc, :call, fn
        _node, :beamtalk_version, :get, [] -> :odd
        node, mod, fun, args -> :meck.passthrough([node, mod, fun, args])
      end)

      assert Workspace.readiness() == {:error, :dead_workspace}
    end
  end

  describe "ensure_distributed/0 (via connect/0) — epmd reachable, :net_kernel.start/1 branches" do
    # Node.alive?/0 is left on its real (not-alive) passthrough here — the
    # branch under test is the *other* side of `ensure_distributed/0`'s outer
    # `if`, reached only when this node isn't already distributed.
    setup do
      :meck.new(:gen_tcp, [:unstick, :passthrough])
      :meck.new(:net_kernel, [:unstick, :passthrough])

      on_exit(fn -> :meck.unload() end)

      # Fake a reachable local epmd on the real port only — every other
      # host/port (e.g. workspace_test.exs's own throwaway-listener tests, had
      # they run concurrently) passes through to the real :gen_tcp.
      :meck.expect(:gen_tcp, :connect, fn
        _host, 4369, _opts, _timeout -> {:ok, :fake_epmd_socket}
        host, port, opts, timeout -> :meck.passthrough([host, port, opts, timeout])
      end)

      :meck.expect(:gen_tcp, :close, fn
        :fake_epmd_socket -> :ok
        socket -> :meck.passthrough([socket])
      end)

      :ok
    end

    # net_kernel.start/1 succeeding does not make this BEAM *really*
    # distributed (only the mock reported success), so the subsequent real
    # Node.connect/1 call still reports :ignored — that outcome is incidental
    # to this test (real distribution state, not scripted); what's under test
    # is that ensure_distributed/0 reached :ok and let connect/0 proceed
    # past it to the Node.connect/1 call at all.
    test "net_kernel.start/1 succeeding lets ensure_distributed/0 proceed" do
      :meck.expect(:net_kernel, :start, fn _args -> {:ok, self()} end)

      assert {:error, {:connect_failed, _node, :ignored}} = Workspace.connect()
    end

    test "net_kernel.start/1 racing to :already_started is treated as success" do
      :meck.expect(:net_kernel, :start, fn _args -> {:error, {:already_started, self()}} end)

      assert {:error, {:connect_failed, _node, :ignored}} = Workspace.connect()
    end

    test "net_kernel.start/1 failing for any other reason raises" do
      :meck.expect(:net_kernel, :start, fn _args -> {:error, :some_reason} end)

      assert_raise RuntimeError, ~r/failed to start distributed node/, fn ->
        Workspace.connect()
      end
    end
  end
end
