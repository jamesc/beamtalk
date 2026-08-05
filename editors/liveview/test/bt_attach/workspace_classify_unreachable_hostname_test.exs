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

  `async: false`: this reproduces the bug by mutating global VM resolver
  state (`:inet_db` lookup order + a fabricated hosts entry) and briefly
  starting distribution, all restored in `on_exit`. ExUnit runs `async: false`
  modules after all `async: true` ones (never interleaved), so this can't
  race concurrent tests doing real hostname/network work.
  """
  use ExUnit.Case, async: false

  alias BtAttach.Workspace

  # RFC 5737 TEST-NET-1: guaranteed non-routable, so a connect attempt to it
  # fails fast and deterministically instead of hanging or (worse) actually
  # reaching some host on the network.
  @unroutable_ip {192, 0, 2, 1}

  setup do
    {:ok, my_host} = :inet.gethostname()
    old_lookup = :inet_db.res_option(:lookup)

    # Register a node with the real local epmd so we have a name we know is
    # present, without depending on whatever else happens to be registered
    # on this (possibly shared) machine. Skip start/stop if this VM is
    # already distributed for some other reason — reuse its identity instead.
    already_alive? = Node.alive?()

    registered_node =
      if already_alive? do
        node()
      else
        short_name = :"bt3003hostbug#{System.unique_integer([:positive])}"
        {:ok, _pid} = :net_kernel.start([short_name, :shortnames])
        node()
      end

    on_exit(fn ->
      :inet_db.del_host(@unroutable_ip)
      :inet_db.set_lookup(old_lookup)
      unless already_alive?, do: :net_kernel.stop()
    end)

    %{my_host: my_host, registered_node: registered_node}
  end

  describe "classify_unreachable/1 default arg — hostname self-resolution failure (BT-3003)" do
    test "epmd still distinguishes bad_cookie/dead_workspace when the machine's own hostname doesn't self-resolve",
         %{my_host: my_host, registered_node: registered_node} do
      # Sanity check: epmd genuinely knows about our just-started node via the
      # explicit-host form, before we break anything.
      assert {:ok, names} = :net_adm.names(:localhost)
      short_name = registered_node |> Atom.to_string() |> String.split("@") |> List.first()
      assert List.keymember?(names, String.to_charlist(short_name), 0)

      # Reproduce the WSL2 bug: make the machine's own hostname resolve to an
      # unroutable address, so an epmd query built from `inet:gethostname/0`
      # (the zero-arg `:net_adm.names/0` path) fails, while the explicit
      # `:localhost` path is untouched and keeps working against the same epmd.
      :inet_db.set_lookup([:file])
      :ok = :inet_db.add_host(@unroutable_ip, [my_host])

      # Confirm the forced failure actually reproduces the reported symptom
      # (the WSL2 report saw `{:error, :address}` specifically) rather than
      # accidentally still succeeding. Match on the shape, not the exact
      # reason atom — that's platform/OTP-version-specific and irrelevant to
      # classify_unreachable/2's own catch-all, which is `{:error, _}`.
      assert {:error, _reason} = :net_adm.names()
      assert {:ok, _} = :net_adm.names(:localhost)

      # The regression: with the old `:net_adm.names()` default, both calls
      # below would misreport `:epmd_absent` right here, because the default
      # arg's own lookup would hit the same forced failure.
      assert Workspace.classify_unreachable(registered_node) == :bad_cookie

      unregistered_node =
        :"bt3003_no_such_workspace_#{System.unique_integer([:positive])}@localhost"

      assert Workspace.classify_unreachable(unregistered_node) == :dead_workspace
    end
  end
end
