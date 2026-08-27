# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.RequestContext do
  @moduledoc """
  Builds the RBAC-relevant request identity (`user`, `role`) that
  `BtAttach.Facade.dispatch/3` audits/gates on (ADR 0091 Decision 3,
  BT-2421).

  Extracted as a shared leaf module (BT-3291) so `BtAttachWeb.WorkspaceLive`
  and its extracted panes (e.g. `BtAttachWeb.Live.Inspector`) build the same
  context from a socket's assigns rather than each keeping its own copy
  (CLAUDE.md's no-duplicate-implementations rule).
  """

  @type t :: %{user: term(), role: atom()}

  @doc "The facade context for `socket`: its authenticated user + RBAC role."
  @spec build(Phoenix.LiveView.Socket.t()) :: t()
  def build(socket),
    do: %{user: socket.assigns[:current_user], role: socket.assigns[:role] || :owner}
end
