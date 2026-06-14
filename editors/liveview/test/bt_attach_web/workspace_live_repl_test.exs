# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveReplTest do
  @moduledoc """
  Fast, workspace-free unit tests for the REPL tab's crafted-event safety
  (BT-2543). These call `WorkspaceLive.handle_event/3` directly on a socket with
  NO REPL assigns — the exact `mount/3` attach-failure state, where
  `bind_session` never initialised `:repl_seq` / `:repl_history` / the `:repl`
  stream. A crafted `repl_eval` / `repl_history_prev` / `repl_history_next`
  WebSocket message during that window must be a no-op, not a KeyError that
  crashes the LiveView (regression for the review BLOCK on PR #2586).

  Unlike `WorkspaceLiveTest`, these need no running workspace node, so they run
  in the default `mix test`.
  """
  use ExUnit.Case, async: true

  alias BtAttachWeb.WorkspaceLive
  alias Phoenix.LiveView.Socket

  # A socket in the attach-failure state: connected mount ran but `bind_session`
  # did not, so none of the REPL assigns (or the `:repl` stream) exist.
  defp unbound_socket, do: %Socket{assigns: %{__changed__: %{}}}

  test "a crafted repl_eval is a no-op when REPL assigns are absent" do
    socket = unbound_socket()

    assert {:noreply, ^socket} =
             WorkspaceLive.handle_event("repl_eval", %{"expr" => "3 + 4"}, socket)
  end

  test "a blank crafted repl_eval is a no-op when REPL assigns are absent" do
    socket = unbound_socket()

    assert {:noreply, ^socket} =
             WorkspaceLive.handle_event("repl_eval", %{"expr" => "   "}, socket)
  end

  test "crafted ↑/↓ history recall is a no-op when the ring is absent" do
    socket = unbound_socket()

    assert {:noreply, ^socket} =
             WorkspaceLive.handle_event("repl_history_prev", %{}, socket)

    assert {:noreply, ^socket} =
             WorkspaceLive.handle_event("repl_history_next", %{}, socket)
  end

  test "a crafted repl_inspect is a no-op when the term map is absent" do
    socket = unbound_socket()

    assert {:noreply, ^socket} =
             WorkspaceLive.handle_event("repl_inspect", %{"id" => "repl-entry-1"}, socket)
  end
end
