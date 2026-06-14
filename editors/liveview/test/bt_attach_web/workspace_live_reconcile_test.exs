# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceLiveReconcileTest do
  # Pure unit tests for the BT-2545 flush badge-clear reconcile.
  #
  # A full compile → flush → badge-clear integration test would need a real
  # workspace node that can flush a *method* to disk, which the `:workspace` e2e lane
  # can't provide: the only way to get an in-project class into that workspace is the
  # New File flow, and a New-File class's in-memory method patch is non-flushable
  # (the loader can't resolve an in-project on-disk span for it), so a real flush
  # leaves the patch pending and the badge legitimately never clears. We therefore
  # test the reconcile functions directly with plain data — the same way
  # `Workspace.format_flush_summary/1` is unit-tested without a node. No `:workspace`
  # tag, so these run in the bare `mix test` lane.
  use ExUnit.Case, async: true

  alias BtAttachWeb.WorkspaceLive

  # A ChangeLog row as `entry_to_row/1` shapes it (atom-keyed, string fields).
  defp change_row(class, selector), do: %{class: class, selector: selector}

  defp method_tab(class, selector, disk_differs) do
    %{
      id: "method:#{class}:instance:#{selector}",
      kind: :method,
      class: class,
      side: "instance",
      selector: selector,
      disk_differs: disk_differs,
      runtime_only: false
    }
  end

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

  describe "clear_disk_differs/2" do
    test "clears the badge only on flushed method tabs" do
      tabs = [
        method_tab("Greeter", "greet", true),
        method_tab("Counter", "increment", true)
      ]

      flushed = MapSet.new([{"Greeter", "greet"}])

      assert [
               %{class: "Greeter", disk_differs: false},
               %{class: "Counter", disk_differs: true}
             ] = WorkspaceLive.clear_disk_differs(tabs, flushed)
    end

    test "an empty flushed set leaves every tab untouched" do
      tabs = [method_tab("Greeter", "greet", true)]

      assert WorkspaceLive.clear_disk_differs(tabs, MapSet.new()) == tabs
    end

    test "leaves a :def tab (no selector) untouched without raising" do
      def_tab = %{id: "def:Counter", kind: :def, class: "Counter", disk_differs: true}

      # Would raise on `tab.selector` if the `:method` guard didn't short-circuit.
      assert WorkspaceLive.clear_disk_differs([def_tab], MapSet.new([{"Counter", "increment"}])) ==
               [def_tab]
    end
  end
end
