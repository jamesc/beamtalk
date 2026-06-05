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
end
