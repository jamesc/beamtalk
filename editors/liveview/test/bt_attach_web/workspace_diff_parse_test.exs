# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.WorkspaceDiffParseTest do
  @moduledoc """
  Unit tests for `WorkspaceLive.parse_diff/1` (BT-2636) — the presentation-only
  classifier that turns a verbatim unified-diff string into structured lines for
  the `unified_diff/1` component. Pure: no LiveView, no workspace.

  Covers the acceptance criteria: classifies add/remove/context/hunk/meta;
  handles empty/binary; preserves content (including leading whitespace) with the
  +/-/space marker lifted into the gutter.
  """
  use ExUnit.Case, async: true

  alias BtAttachWeb.WorkspaceLive

  test "empty / nil / binary diffs yield no lines" do
    assert WorkspaceLive.parse_diff("") == []
    assert WorkspaceLive.parse_diff(nil) == []
    assert WorkspaceLive.parse_diff(nil) == []
    # A non-binary value (defensive) degrades to [].
    assert WorkspaceLive.parse_diff(123) == []
  end

  test "classifies add / remove / context lines and strips the marker into the gutter" do
    diff = " unchanged\n+added line\n-removed line"

    assert [
             %{kind: :context, marker: " ", content: "unchanged"},
             %{kind: :add, marker: "+", content: "added line"},
             %{kind: :remove, marker: "-", content: "removed line"}
           ] = WorkspaceLive.parse_diff(diff)
  end

  test "classifies hunk headers and file-header meta lines" do
    diff =
      "diff --git a/x.bt b/x.bt\n" <>
        "index 111..222 100644\n" <>
        "--- a/x.bt\n" <>
        "+++ b/x.bt\n" <>
        "@@ -1,3 +1,4 @@ Foo class >> bar"

    parsed = WorkspaceLive.parse_diff(diff)

    kinds = Enum.map(parsed, & &1.kind)
    assert kinds == [:meta, :meta, :meta, :meta, :hunk]

    # The `--- ` / `+++ ` headers must NOT be mis-read as remove/add lines.
    assert Enum.at(parsed, 2) == %{kind: :meta, marker: "", content: "--- a/x.bt"}
    assert Enum.at(parsed, 3) == %{kind: :meta, marker: "", content: "+++ b/x.bt"}

    hunk = List.last(parsed)
    assert hunk.kind == :hunk
    assert hunk.content == "@@ -1,3 +1,4 @@ Foo class >> bar"
  end

  test "preserves leading whitespace in content (only the marker is stripped)" do
    # A real method diff line: marker, then the source's own indentation.
    diff = "+    parseEtfBinary: bin offset: off size: sz acc: acc =>"

    assert [
             %{
               kind: :add,
               marker: "+",
               content: "    parseEtfBinary: bin offset: off size: sz acc: acc =>"
             }
           ] =
             WorkspaceLive.parse_diff(diff)
  end

  test "an empty added/removed line keeps an empty content (just the marker)" do
    assert [
             %{kind: :add, marker: "+", content: ""},
             %{kind: :remove, marker: "-", content: ""}
           ] = WorkspaceLive.parse_diff("+\n-")
  end

  test "no-newline marker and mode lines fall back to neutral meta" do
    diff =
      "old mode 100644\n" <>
        "new mode 100755\n" <>
        "\\ No newline at end of file"

    assert Enum.all?(WorkspaceLive.parse_diff(diff), &(&1.kind == :meta))
  end
end
