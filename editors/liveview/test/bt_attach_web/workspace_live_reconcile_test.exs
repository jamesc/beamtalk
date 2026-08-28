# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveReconcileTest do
  # Pure unit tests for the BT-2545 flush badge-clear reconcile.
  #
  # A full compile → flush → badge-clear integration test is covered by
  # `workspace_flush_badge_test.exs` using a fully-stubbed workspace client
  # (BT-2554). We test the reconcile functions directly with plain data — the
  # same way `Workspace.format_flush_summary/1` is unit-tested without a node.
  # No `:workspace` tag, so these run in the bare `mix test` lane.
  #
  # `clear_disk_differs/2` and `reactivation_disk_source/2` moved to
  # `BtAttachWeb.Live.MethodEditor` (BT-3296) along with the rest of the tab
  # data model — their direct tests moved to `method_editor_test.exs`.
  # `pending_method_keys/1`/`flushed_method_keys/3` stayed on `WorkspaceLive`
  # (the Changes-pane ChangeLog reconcile, shared with `BtAttachWeb.Live.Dock`).
  use ExUnit.Case, async: true

  alias BtAttachWeb.WorkspaceLive

  # A ChangeLog row as `entry_to_row/1` shapes it (atom-keyed, string fields).
  defp change_row(class, selector), do: %{class: class, selector: selector}

  describe "pending_method_keys/1" do
    test "collects the (class, selector) of every method row" do
      rows = [change_row("Counter", "increment"), change_row("Greeter", "greet")]

      assert WorkspaceLive.pending_method_keys(rows) ==
               MapSet.new([{"Counter", "increment"}, {"Greeter", "greet"}])
    end

    test "an empty or non-list ChangeLog yields the empty set" do
      assert WorkspaceLive.pending_method_keys([]) == MapSet.new()
      assert WorkspaceLive.pending_method_keys(nil) == MapSet.new()
    end
  end

  describe "flushed_method_keys/3" do
    test "the flushed keys are those pending before but gone after" do
      was_pending = MapSet.new([{"Counter", "increment"}, {"Greeter", "greet"}])
      # `greet` was written (dropped from changes); `increment` conflicted and stays.
      still_pending = [change_row("Counter", "increment")]

      assert WorkspaceLive.flushed_method_keys(was_pending, still_pending, nil) ==
               MapSet.new([{"Greeter", "greet"}])
    end

    test "nothing pending after a clean flush flushes the whole before-set" do
      was_pending = MapSet.new([{"Counter", "increment"}])

      assert WorkspaceLive.flushed_method_keys(was_pending, [], nil) ==
               MapSet.new([{"Counter", "increment"}])
    end

    test "a failed post-flush refresh clears nothing (changes_error guard)" do
      was_pending = MapSet.new([{"Counter", "increment"}])

      # `changes: []` *with* an error must NOT collapse to the full before-set —
      # otherwise an unreachable ChangeLog would false-clear every pending badge.
      assert WorkspaceLive.flushed_method_keys(was_pending, [], "changes unreachable") ==
               MapSet.new()
    end
  end
end
