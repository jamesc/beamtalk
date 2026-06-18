# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.WorkspaceTest do
  @moduledoc """
  Unit tests for the pure, non-RPC parts of the Attach client. These run in the
  default `mix test` (no `:workspace` tag) because they need no live workspace.

  `format_value/1` must stay surface-consistent with the canonical Rust
  formatter `beamtalk_repl_protocol::format::format_value` (Plain mode), which
  the CLI / MCP / browser share — these assertions mirror its snapshot tests so
  drift is caught here.
  """
  use ExUnit.Case, async: true

  alias BtAttach.Workspace

  describe "format_value/1 — surface-consistent with the Rust formatter (Plain mode)" do
    test "plain string renders verbatim" do
      assert Workspace.format_value("hello") == "hello"
    end

    test "actor pid string renders verbatim" do
      assert Workspace.format_value("#Actor<0.123.0>") == "#Actor<0.123.0>"
    end

    test "block string renders verbatim" do
      assert Workspace.format_value("a Block/2") == "a Block/2"
    end

    test "float arrives as a string (BT-1336) and renders verbatim" do
      assert Workspace.format_value("6.0") == "6.0"
    end

    test "raw float value renders via Float.to_string/1" do
      # The is_float/1 guard handles a term that arrives as a native float
      # (rather than the BT-1336 string form), e.g. from a fallback inspect path.
      assert Workspace.format_value(6.0) == "6.0"
      assert Workspace.format_value(3.14159) == "3.14159"
    end

    test "integer renders as its digits" do
      assert Workspace.format_value(42) == "42"
    end

    test "booleans render as true/false" do
      assert Workspace.format_value(true) == "true"
      assert Workspace.format_value(false) == "false"
    end

    test "null renders as nil" do
      assert Workspace.format_value(nil) == "nil"
    end

    test "list renders as a Beamtalk list literal" do
      assert Workspace.format_value([1, 2, 3]) == "#(1, 2, 3)"
    end

    test "empty list renders as #()" do
      assert Workspace.format_value([]) == "#()"
    end

    test "nested list renders recursively" do
      assert Workspace.format_value([[1, 2], [3, 4]]) == "#(#(1, 2), #(3, 4))"
    end

    test "map renders as {k: v, ...}" do
      # Single key keeps the assertion order-independent (maps are unordered).
      assert Workspace.format_value(%{"x" => 1}) == "{x: 1}"
    end

    test "map values are recursively formatted" do
      assert Workspace.format_value(%{"items" => [1, 2]}) == "{items: #(1, 2)}"
    end
  end

  describe "format_flush_summary/1 — write-surface flush rendering (BT-2409, ADR 0082)" do
    test "a clean flush reports the written count and file count" do
      # The workspace returns a FlushResult-shaped atom-keyed map over distribution.
      summary = %{
        :"$beamtalk_class" => :FlushResult,
        flushed: 2,
        files: ["src/counter.bt", "src/greeter.bt"],
        newClasses: 0,
        skipped: [],
        conflicts: []
      }

      assert Workspace.format_flush_summary(summary) ==
               "Flushed 2 change(s) across 2 file(s)"
    end

    test "nothing-to-flush renders zero counts without conflict/skip suffixes" do
      summary = %{flushed: 0, files: [], skipped: [], conflicts: []}
      assert Workspace.format_flush_summary(summary) == "Flushed 0 change(s) across 0 file(s)"
    end

    test "conflicts and skipped entries are surfaced as recoverable suffixes" do
      summary = %{
        flushed: 1,
        files: ["src/counter.bt"],
        skipped: [%{seq: 7, reason: "stdlib"}],
        conflicts: [%{file: "src/other.bt", reason: "external_edit", seqs: [9]}]
      }

      result = Workspace.format_flush_summary(summary)
      assert result =~ "Flushed 1 change(s) across 1 file(s)"
      assert result =~ "1 conflict(s)"
      assert result =~ "1 skipped"
    end

    test "string-keyed summary (e.g. JSON-decoded) reads the same fields" do
      summary = %{"flushed" => 1, "files" => ["a.bt"], "skipped" => [], "conflicts" => []}
      assert Workspace.format_flush_summary(summary) == "Flushed 1 change(s) across 1 file(s)"
    end

    test "an unexpected non-map shape degrades to inspect/1 rather than crashing" do
      assert Workspace.format_flush_summary({:error, :boom}) == "{:error, :boom}"
    end
  end

  describe "pending_rows/1 — collapse the Changes pane to one row per method (BT-2574)" do
    # A change-entry value map as it arrives over distribution from
    # `beamtalk_workspace_changelog:change_entries/0` (atom keys; className /
    # selector / kind as atoms; derived `active` + `shadowed` booleans).
    defp entry(seq, selector, opts) do
      %{
        seq: seq,
        className: :Counter,
        selector: selector,
        kind: :instance,
        intent: :durable,
        flushable: true,
        flushed: false,
        authorKind: :human,
        active: Keyword.get(opts, :active, true),
        shadowed: Keyword.get(opts, :shadowed, false),
        clean: Keyword.get(opts, :clean, false),
        diff: Keyword.get(opts, :diff, nil)
      }
    end

    test "drops shadowed entries, keeps active survivors, newest-first" do
      # Counter>>increment patched then reverted (the older entry is shadowed),
      # plus an unrelated Counter>>decrement patch.
      entries = [
        entry(0, :increment, shadowed: true),
        entry(1, :increment, shadowed: false),
        entry(2, :decrement, shadowed: false)
      ]

      rows = Workspace.pending_rows(entries)

      assert length(rows) == 2
      # change_entries/0 is oldest-first; pending_rows renders newest-first.
      assert Enum.map(rows, & &1.selector) == ["decrement", "increment"]
    end

    test "drops inactive entries entirely" do
      assert Workspace.pending_rows([entry(0, :render, active: false)]) == []
    end

    test "a missing shadowed key (older workspace build) is treated as not shadowed" do
      raw = %{
        seq: 0,
        className: :Counter,
        selector: :increment,
        kind: :instance,
        intent: :durable,
        flushable: true,
        flushed: false,
        authorKind: :human,
        active: true
      }

      assert [%{selector: "increment"}] = Workspace.pending_rows([raw])
    end

    test "drops clean entries (reverted back to disk — disappear when clean)" do
      assert Workspace.pending_rows([entry(0, :increment, clean: true)]) == []
    end

    test "carries the net-vs-disk diff onto the row" do
      [row] = Workspace.pending_rows([entry(0, :increment, diff: "- a\n+ b\n")])
      assert row.diff == "- a\n+ b\n"
    end

    test "a row with no computed diff carries nil" do
      [row] = Workspace.pending_rows([entry(0, :increment, [])])
      assert row.diff == nil
    end
  end

  describe "browse-surface wrappers (System Browser data source, ADR 0095 / BT-2488)" do
    # No live workspace here, so the RPC targets an unconnected node. The
    # contract under test: the wrappers route through `dispatch_browse/2` and
    # surface a `{:badrpc, :nodedown}` as `{:error, {:unreachable, _}}` rather
    # than crashing — the same graceful-degradation other Attach reads use.
    test "browse_classes against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.browse_classes()
    end

    test "browse_protocols against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.browse_protocols("Counter", "instance")
    end

    test "browse_method_source against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} =
               Workspace.browse_method_source("Counter", "instance", "increment")
    end

    test "browse_class_definition against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.browse_class_definition("Counter")
    end

    test "the browse wrappers reject non-binary arguments (guard contract)" do
      # apply/3 keeps the deliberately mistyped arguments opaque to the
      # compiler's type checker, which would otherwise warn on these calls.
      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :browse_protocols, [:Counter, "instance"])
      end

      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :browse_class_definition, [:Counter])
      end

      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :browse_method_source, ["Counter", :instance, "increment"])
      end
    end
  end

  describe "navigation-surface wrappers (Senders/Implementors + omni search, BT-2495)" do
    # No live workspace: the RPC targets an unconnected node, so the wrappers must
    # surface `{:badrpc, :nodedown}` as `{:error, {:unreachable, _}}` through
    # `dispatch_browse/2` (the same graceful degradation the browse wrappers use),
    # not crash.
    test "senders_of against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.senders_of("increment")
    end

    test "implementors_of against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.implementors_of("increment")
    end

    test "symbol_index against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.symbol_index()
      assert {:error, {:unreachable, _}} = Workspace.symbol_index("user")
    end

    test "the navigation wrappers reject non-binary arguments (guard contract)" do
      # Dispatch via apply/3 so the runtime guard is exercised without Elixir's
      # compile-time type checker statically flagging the deliberately-bad literal
      # against the binary() guard (a --warnings-as-errors failure on a full build).
      assert_raise FunctionClauseError, fn -> apply(Workspace, :senders_of, [:increment]) end
      assert_raise FunctionClauseError, fn -> apply(Workspace, :implementors_of, [42]) end
      assert_raise FunctionClauseError, fn -> apply(Workspace, :symbol_index, [:all]) end
    end
  end

  describe "save/flush write-surface wrappers (New File + revert, ADR 0082 Phase 5, BT-2293)" do
    # No live workspace: the RPC targets an unconnected node, so the wrappers must
    # surface `{:badrpc, :nodedown}` as `{:error, {:unreachable, _}}` (the same
    # graceful degradation the read wrappers use), not crash.
    test "new_class against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} =
               Workspace.new_class("Object subclass: Greeter", "src/greeter.bt")
    end

    test "revert against an unreachable workspace returns an unreachable error" do
      assert {:error, {:unreachable, _}} = Workspace.revert("Counter", "increment")
    end

    test "the write wrappers reject non-binary arguments (guard contract)" do
      # apply/3 keeps the deliberately mistyped arguments opaque to the compiler's
      # type checker, which would otherwise warn against the binary() guards.
      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :new_class, ["Object subclass: G", :path])
      end

      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :new_class, [42, "src/g.bt"])
      end

      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :revert, ["Counter", :increment])
      end

      assert_raise FunctionClauseError, fn ->
        apply(Workspace, :revert, [:Counter, "increment"])
      end
    end
  end
end
