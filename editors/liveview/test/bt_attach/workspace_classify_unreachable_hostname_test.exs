# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceClassifyUnreachableHostnameTest do
  @moduledoc """
  BT-3003 regression: `classify_unreachable/2`'s default `epmd_names`
  argument must query epmd via the explicit `:localhost` host
  (`:net_adm.names(:localhost)`), not the zero-arg `:net_adm.names/0` form.

  The zero-arg form resolves via `inet:gethostname/0` (the machine's real
  hostname) rather than `:localhost` — on a host where that hostname doesn't
  cleanly self-resolve for an epmd TCP query (confirmed live on a WSL2
  sandbox, plausible on other containers/CI runners/some macOS mDNS setups),
  `:net_adm.names()` fails outright even though epmd is reachable and
  `:net_adm.names(:localhost)` works fine against the exact same epmd. Since
  `classify_unreachable/2`'s catch-all is `{:error, _} -> :epmd_absent`, that
  collapsed both `:bad_cookie` and `:dead_workspace` into `:epmd_absent`.

  This is exercised by stubbing `:net_adm.names/0` and `:net_adm.names/1`
  independently with `:meck` — giving them different results is exactly what
  proves *which* arity `classify_unreachable/1`'s default argument calls —
  rather than starting real distribution to reproduce the failure against a
  live epmd: dynamically bringing up `:net_kernel` isn't reliable across CI
  containers (some can't start distribution post-boot at all), so a
  live-epmd version of this test would be flaky by construction on the one
  thing it most needs to be deterministic about.

  `async: false`: `:meck` globally replaces the `:net_adm` module for the
  whole VM, so this can't run concurrently with anything else touching it.
  Nothing else in this suite does.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  setup do
    :meck.new(:net_adm, [:unstick, :passthrough])

    # Common to every test below: the bare zero-arg query fails (as it does
    # when the machine's own hostname doesn't self-resolve) — reproducing the
    # WSL2 bug precisely. Each test overrides only the `:localhost` arity-1
    # response, which is what actually varies per scenario.
    :meck.expect(:net_adm, :names, fn -> {:error, :address} end)

    # `:meck.unload/0` (no args), not `:meck.unload(:net_adm)`: meck ties a
    # mock's lifecycle to its owning process and auto-unloads when that
    # process exits, which — for `on_exit` callbacks — has already happened
    # by the time this runs (they execute in a separate runner process after
    # the test process itself has terminated). The no-arg form silently
    # tolerates a mock that's already gone; the 1-arg form raises
    # `{not_mocked, _}` on it.
    on_exit(fn -> :meck.unload() end)
    :ok
  end

  describe "classify_unreachable/1 default arg — hostname self-resolution failure (BT-3003)" do
    test "bad_cookie: epmd knows the target node, reachable only via the explicit :localhost host" do
      # The explicit-host query the fixed default now uses succeeds against
      # the same epmd, with the target node's short name present.
      :meck.expect(:net_adm, :names, fn :localhost ->
        {:ok, [{~c"beamtalk_workspace_spike", 45678}]}
      end)

      # The regression: with the old `:net_adm.names()` default, this would
      # hit the stubbed zero-arg failure from setup/0 and misreport
      # `:epmd_absent`.
      assert Workspace.classify_unreachable(:beamtalk_workspace_spike@localhost) == :bad_cookie
    end

    test "dead_workspace: epmd is reachable via :localhost but has no record of the target node" do
      :meck.expect(:net_adm, :names, fn :localhost -> {:ok, []} end)

      assert Workspace.classify_unreachable(:beamtalk_workspace_spike@localhost) ==
               :dead_workspace
    end

    test "the default argument calls names/1 with :localhost, not names/0" do
      # Belt-and-braces: assert on the call itself, not just the classification
      # outcome, so a regression to the bare form fails here even if some
      # future classification change happened to produce the same atom either way.
      :meck.expect(:net_adm, :names, fn :localhost -> {:ok, []} end)

      Workspace.classify_unreachable(:beamtalk_workspace_spike@localhost)

      assert :meck.called(:net_adm, :names, [:localhost])
      refute :meck.called(:net_adm, :names, [])
    end
  end
end
