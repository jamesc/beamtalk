# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceTranscriptCoalesceTest do
  @moduledoc """
  Fast, workspace-free unit tests for the Transcript pane's burst coalescing
  (BT-2609). A high-output expression (e.g. `Hanoi solve: 8`) fans out one
  `{:transcript_output, _}` message per line from `beamtalk_transcript_stream`.
  Inserting each individually is one render + diff push per line — a render
  storm that floods the LiveView mailbox and stalls the socket. The handler
  must instead drain the queued burst and `stream_insert` it in a single pass,
  with a per-insert depth cap so the DOM can't grow unbounded.

  These call `WorkspaceLive.handle_info/2` directly on a socket with an
  initialised `:transcript` stream, so they need no running workspace node and
  run in the default `mix test`.
  """
  use ExUnit.Case, async: true

  import Phoenix.LiveView, only: [stream: 3]

  alias BtAttachWeb.WorkspaceLive
  alias Phoenix.LiveView.Socket

  # The depth cap the handler applies to every transcript insert. Kept in step
  # with the producer's 1000-entry ring buffer (`beamtalk_transcript_stream`).
  @transcript_limit 1000

  # A socket with the `:transcript` stream initialised, as `bind_session` leaves
  # it after a successful connected mount. `stream/3` attaches an after-render
  # lifecycle hook, so the socket's private map must carry a Lifecycle struct —
  # the bare `%Socket{}` default omits it.
  defp transcript_socket do
    %Socket{
      assigns: %{__changed__: %{}},
      private: %{live_temp: %{}, lifecycle: %Phoenix.LiveView.Lifecycle{}}
    }
    |> stream(:transcript, [])
  end

  # The pending inserts the handler queued onto the stream this render pass.
  # Stored newest-first as `{dom_id, at, item, limit}` tuples; reverse to read
  # them in arrival order.
  defp pending_inserts(socket) do
    socket.assigns.streams.transcript.inserts |> Enum.reverse()
  end

  test "a single transcript message inserts one line with the depth cap" do
    socket = transcript_socket()

    {:noreply, socket} =
      WorkspaceLive.handle_info({:transcript_output, "one move"}, socket)

    assert [{_id, _at, %{text: "one move"}, -@transcript_limit}] = pending_inserts(socket)
  end

  test "a burst already queued in the mailbox is coalesced into one render pass" do
    socket = transcript_socket()

    # Seed the mailbox as the producer would: the triggering message is handled
    # by handle_info, the rest are drained from the mailbox in the same pass.
    for n <- 2..50, do: send(self(), {:transcript_output, "move #{n}"})

    {:noreply, socket} =
      WorkspaceLive.handle_info({:transcript_output, "move 1"}, socket)

    inserts = pending_inserts(socket)

    # All 50 lines landed in a SINGLE handle_info — no per-line render.
    assert length(inserts) == 50
    texts = Enum.map(inserts, fn {_id, _at, %{text: t}, _limit} -> t end)
    assert texts == Enum.map(1..50, &"move #{&1}")

    # Every insert carries the depth cap so the DOM stays bounded.
    assert Enum.all?(inserts, fn {_id, _at, _item, limit} -> limit == -@transcript_limit end)

    # The mailbox is fully drained — nothing left to trigger another render.
    refute_received {:transcript_output, _}
  end

  test "ids are unique across a coalesced burst so stream entries don't collide" do
    socket = transcript_socket()

    for _ <- 1..20, do: send(self(), {:transcript_output, "dup"})

    {:noreply, socket} =
      WorkspaceLive.handle_info({:transcript_output, "dup"}, socket)

    ids = pending_inserts(socket) |> Enum.map(fn {id, _at, _item, _limit} -> id end)
    assert length(ids) == 21
    assert length(Enum.uniq(ids)) == 21
  end

  test "non-binary transcript text is coerced to a string" do
    socket = transcript_socket()

    {:noreply, socket} =
      WorkspaceLive.handle_info({:transcript_output, ~c"charlist line"}, socket)

    assert [{_id, _at, %{text: "charlist line"}, _limit}] = pending_inserts(socket)
  end

  test "the drain is capped per pass so a single handle_info can't run unbounded" do
    socket = transcript_socket()

    # More messages than the depth cap queued at once (e.g. concurrent
    # high-output evals, or a client that's fallen behind on renders). One
    # handle_info must drain at most @transcript_limit of them — the remainder
    # stay in the mailbox to be picked up by the next pass.
    overflow = 50

    for n <- 2..(@transcript_limit + overflow),
        do: send(self(), {:transcript_output, "line #{n}"})

    {:noreply, socket} =
      WorkspaceLive.handle_info({:transcript_output, "line 1"}, socket)

    assert length(pending_inserts(socket)) == @transcript_limit

    # The overflow is not lost — it's still queued for the next handle_info pass.
    remaining =
      Stream.repeatedly(fn ->
        receive do
          {:transcript_output, _} -> :ok
        after
          0 -> :empty
        end
      end)
      |> Enum.take_while(&(&1 == :ok))
      |> length()

    assert remaining == overflow
  end
end
