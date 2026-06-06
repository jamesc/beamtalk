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
end
