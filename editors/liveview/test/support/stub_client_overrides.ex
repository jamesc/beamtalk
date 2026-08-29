# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.StubClientOverrides do
  @moduledoc """
  Build a one-off workspace client that behaves exactly like
  `BtAttachWeb.StubWorkspaceClient` apart from the handful of callbacks a test
  overrides (BT-3306).

      defmodule UnreachableClient do
        use BtAttachWeb.StubClientOverrides

        def connect, do: {:error, {:connect_failed, :ws@host, false}}
      end

      Application.put_env(:bt_attach, :workspace_client, UnreachableClient)

  The workspace client is a plain module read out of app env
  (`:bt_attach, :workspace_client` — see `BtAttach.Facade`'s `client/0` and
  `BtAttachWeb.WorkspaceLive`'s `ws_client/0`), so driving a single degraded
  callback (a failed `connect`, a `subscribe_transcript` that answers
  `{:badrpc, _}`, a `browse_classes` that raises) otherwise means restating the
  stub's whole ~100-function surface. `@before_compile` fills in a `defdelegate`
  to `BtAttachWeb.StubWorkspaceClient` for every exported name/arity the using
  module did NOT define itself, so the overrides ARE the whole test double —
  and the shared stub stays the single implementation of the happy path
  (CLAUDE.md no-duplicate-implementations) rather than each failure test
  growing its own copy.

  The using module keeps the stub's per-test `Agent` state, so the usual
  `StubWorkspaceClient.start_state/0` + seeding helpers apply unchanged.
  """

  defmacro __using__(_opts) do
    quote do
      @before_compile BtAttachWeb.StubClientOverrides
    end
  end

  defmacro __before_compile__(env) do
    overridden = Module.definitions_in(env.module, :def)

    for {fun, arity} <- BtAttachWeb.StubWorkspaceClient.__info__(:functions),
        {fun, arity} not in overridden do
      args = Macro.generate_arguments(arity, env.module)

      quote do
        defdelegate unquote(fun)(unquote_splicing(args)), to: BtAttachWeb.StubWorkspaceClient
      end
    end
  end
end
