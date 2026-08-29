# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.ForcedReplyClient do
  @moduledoc """
  Shared plumbing for a per-test-configurable veneer over
  `BtAttachWeb.StubWorkspaceClient` (BT-3316).

  `BtAttachWeb.Live.InspectorFakeClient` (BT-3305) and
  `BtAttachWeb.Live.DockFakeClient` (BT-3308) each wrap the shared stub so a
  test can force a reply for one op — a degrade-gracefully branch the stub's
  success-shaped replies never reach — while every other op still delegates
  straight through. Both wanted the identical mechanics: a `forced/1` helper
  reading a module-specific `Application.get_env(:bt_attach, key, %{})`, and
  a `case forced(op) do {:ok, reply} -> reply; :error -> Stub.op(args) end`
  body per wrapped function. This module factors that mechanism out so each
  fake client only states its own op list and *why* those particular ops need
  a forced-reply knob (module-specific context that belongs in that module's
  own `@moduledoc`, not here).

      defmodule SomeFakeClient do
        use BtAttachWeb.ForcedReplyClient, key: :some_fake

        forceable(some_op(arg1, arg2))

        defdelegate other_op(arg), to: StubWorkspaceClient
      end

      Application.put_env(:bt_attach, :some_fake, %{some_op: forced_reply})

  `use BtAttachWeb.ForcedReplyClient, key: :some_fake` generates the
  `forced/1` helper bound to `:some_fake` and aliases `StubWorkspaceClient`.
  `forceable(name(args...))` then generates:

      def name(args...) do
        case forced(:name) do
          {:ok, reply} -> reply
          :error -> apply(StubWorkspaceClient, :name, [args...])
        end
      end

  Ops that never need forcing stay plain `defdelegate` lines in the using
  module — this macro only touches the ops that can be forced.
  """

  defmacro __using__(opts) do
    key = Keyword.fetch!(opts, :key)

    quote do
      alias BtAttachWeb.StubWorkspaceClient
      import BtAttachWeb.ForcedReplyClient, only: [forceable: 1]

      # The reply this test forced for `op`, or `:error` when it forced none
      # (in which case the caller falls through to the real stub).
      defp forced(op), do: Map.fetch(Application.get_env(:bt_attach, unquote(key), %{}), op)
    end
  end

  @doc """
  Generates a function named/shaped after `call` (e.g. `some_op(a, b)`) that
  returns the test-forced reply for that op, if any, falling back to
  `StubWorkspaceClient.some_op(a, b)` otherwise. See the moduledoc.
  """
  defmacro forceable({name, _meta, args}) do
    args = args || []

    quote do
      def unquote(name)(unquote_splicing(args)) do
        case forced(unquote(name)) do
          {:ok, reply} -> reply
          :error -> apply(BtAttachWeb.StubWorkspaceClient, unquote(name), unquote(args))
        end
      end
    end
  end
end
