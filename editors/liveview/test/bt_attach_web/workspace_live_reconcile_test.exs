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

      # A flushed *method* key keys on `{class, selector}`; a `:def` tab keys on
      # `{class, :def}`, so a method key never clears a def tab (and never raises
      # on the def tab's absent `selector`).
      assert WorkspaceLive.clear_disk_differs([def_tab], MapSet.new([{"Counter", "increment"}])) ==
               [def_tab]
    end

    test "clears a :def tab when its {class, :def} key is in the flushed set (BT-2600)" do
      # A revert that reloaded the class header keys the def tab as `{class, :def}`,
      # so the open def tab's `unflushed` badge clears without a re-open.
      def_tab = %{id: "def:Counter", kind: :def, class: "Counter", disk_differs: true}
      other_def = %{id: "def:Greeter", kind: :def, class: "Greeter", disk_differs: true}

      assert [
               %{class: "Counter", disk_differs: false},
               %{class: "Greeter", disk_differs: true}
             ] =
               WorkspaceLive.clear_disk_differs(
                 [def_tab, other_def],
                 MapSet.new([{"Counter", :def}])
               )
    end
  end

  describe "reactivation_disk_source/2 (BT-2565)" do
    # `info` as `method_source_info/4` shapes it for the re-activation branch.
    defp source_info(opts) do
      %{
        runtime_only: Keyword.get(opts, :runtime_only, false),
        disk_differs: Keyword.get(opts, :disk_differs, false),
        source: Keyword.get(opts, :source, "")
      }
    end

    test "keeps the prior snapshot when the image diverged but stays disk-backed" do
      # The bug: a tab compiled to a new body (disk_differs flipped true, disk_source
      # preserved) is navigated away from and re-activated. The backend now reports
      # `disk_differs: true`, so a fresh snapshot is unavailable — the carried body
      # must survive instead of regressing to nil.
      existing = %{disk_source: "increment => self.value := self.value + 1"}
      info = source_info(disk_differs: true, source: "increment => self.value := self.value + 2")

      assert WorkspaceLive.reactivation_disk_source(existing, info) ==
               "increment => self.value := self.value + 1"
    end

    test "takes a fresh snapshot when the image is back in sync with disk" do
      # Image matches disk again (disk_differs false): the live body *is* the on-disk
      # body, so re-snapshot it — it supersedes any carried value.
      existing = %{disk_source: "stale body"}
      info = source_info(disk_differs: false, source: "fresh on-disk body")

      assert WorkspaceLive.reactivation_disk_source(existing, info) == "fresh on-disk body"
    end

    test "drops to nil when the method is now runtime-only" do
      # A method that legitimately lost its on-disk body must not retain a stale
      # snapshot — fall back to nil so `compiled_disk_differs/2` stays conservative.
      existing = %{disk_source: "was on disk"}
      info = source_info(runtime_only: true, source: "runtime body")

      assert WorkspaceLive.reactivation_disk_source(existing, info) == nil
    end

    test "drops to nil when image-diverged with no prior snapshot to carry" do
      existing = %{disk_source: nil}
      info = source_info(disk_differs: true, source: "diverged body")

      assert WorkspaceLive.reactivation_disk_source(existing, info) == nil
    end
  end
end
