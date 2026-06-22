# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.DocFormatTest do
  @moduledoc """
  Unit tests for the System Browser doc-comment renderer (BT-2558). Runs in the
  bare `mix test` lane — pure functions, no workspace node.
  """
  use ExUnit.Case, async: true

  alias BtAttach.DocFormat

  # Render to a plain string for assertions.
  defp html(doc) do
    case DocFormat.to_html(doc) do
      nil -> nil
      safe -> Phoenix.HTML.safe_to_string(safe)
    end
  end

  describe "empty input" do
    test "nil renders nothing" do
      assert DocFormat.to_html(nil) == nil
    end

    test "blank / whitespace-only renders nothing" do
      assert DocFormat.to_html("") == nil
      assert DocFormat.to_html("   \n  \n") == nil
    end
  end

  describe "block structure" do
    test "a paragraph becomes a doc-p" do
      assert html("A simple sentence.") =~ ~s(<p class="doc-p">A simple sentence.</p>)
    end

    test "ATX headings render as capped doc headings" do
      out = html("## Examples")
      assert out =~ ~s(<h4 class="doc-h">Examples</h4>)
    end

    test "fenced code blocks render verbatim in a doc-code pre" do
      out = html("Intro text.\n\n```beamtalk\nc increment\n```")
      assert out =~ ~s(<p class="doc-p">Intro text.</p>)
      assert out =~ ~s(<pre class="doc-code"><code>c increment</code></pre>)
    end

    test "bare CR and CRLF line endings are normalised" do
      # Classic-Mac (\r) and Windows (\r\n) newlines split into clean blocks —
      # no trailing carriage returns left to misparse a heading or blank line.
      out = html("## Heading\r\rBody text.")
      assert out =~ ~s(<h4 class="doc-h">Heading</h4>)
      assert out =~ ~s(<p class="doc-p">Body text.</p>)

      out_crlf = html("## Heading\r\n\r\nBody text.")
      assert out_crlf =~ ~s(<h4 class="doc-h">Heading</h4>)
      assert out_crlf =~ ~s(<p class="doc-p">Body text.</p>)
    end

    test "unordered list items group into a single doc-list" do
      out = html("- first\n- second")
      assert out =~ ~s(<ul class="doc-list">)
      assert out =~ "<li>first</li>"
      assert out =~ "<li>second</li>"
      # One list, not one per item.
      assert out |> String.split("<ul") |> length() == 2
    end
  end

  describe "indented code blocks (BT-2650)" do
    test "a 4-space-indented block renders as a doc-code pre" do
      out = html("Examples\n\n    c increment\n    c value")
      assert out =~ ~s(<p class="doc-p">Examples</p>)
      assert out =~ ~s(<pre class="doc-code"><code>c increment\nc value</code></pre>)
    end

    test "a tab-indented block renders as a doc-code pre" do
      out = html("Examples\n\n\tc increment")
      assert out =~ ~s(<pre class="doc-code"><code>c increment</code></pre>)
    end

    test "the common 4-space indent is stripped but inner indentation is preserved" do
      out = html("    line one\n        nested")
      assert out =~ ~s(<pre class="doc-code"><code>line one\n    nested</code></pre>)
    end

    test "an indented block mixed with paragraphs splits correctly" do
      out = html("Intro paragraph.\n\n    code here\n\nTrailing paragraph.")
      assert out =~ ~s(<p class="doc-p">Intro paragraph.</p>)
      assert out =~ ~s(<pre class="doc-code"><code>code here</code></pre>)
      assert out =~ ~s(<p class="doc-p">Trailing paragraph.</p>)
      # The trailing prose is a paragraph, not folded into the code block.
      refute out =~ ~s(Trailing paragraph.</code>)
    end

    test "a blank line interior to an indented block is preserved" do
      out = html("    first\n\n    second")
      assert out =~ ~s(<pre class="doc-code"><code>first\n\nsecond</code></pre>)
    end

    test "indented code escapes its contents" do
      out = html("    <b>not bold</b>")
      refute out =~ "<b>not bold</b>"
      assert out =~ ~s(<pre class="doc-code"><code>&lt;b&gt;not bold&lt;/b&gt;</code></pre>)
    end

    test "fenced code still works alongside the indented-code path" do
      out = html("```\nfenced code\n```")
      assert out =~ ~s(<pre class="doc-code"><code>fenced code</code></pre>)
    end

    test "a list item's indented text is not misread as code" do
      out = html("- item one\n- item two")
      assert out =~ ~s(<ul class="doc-list">)
      refute out =~ ~s(<pre class="doc-code">)
    end
  end

  describe "inline spans" do
    test "inline code, bold and italic" do
      assert html("use `foo`") =~ "use <code>foo</code>"
      assert html("**bold** here") =~ "<strong>bold</strong> here"
      assert html("an *italic* word") =~ "an <em>italic</em> word"
      assert html("an _italic_ word") =~ "an <em>italic</em> word"
    end

    test "emphasis markers inside a code span stay literal" do
      out = html("`a * b * c`")
      assert out =~ "<code>a * b * c</code>"
      refute out =~ "<em>"
    end
  end

  describe "HTML escaping (no injection)" do
    test "author angle brackets are escaped, never emitted as tags" do
      out = html("Danger <script>alert(1)</script> here")
      refute out =~ "<script>"
      assert out =~ "&lt;script&gt;"
    end

    test "code blocks escape their contents too" do
      out = html("```\n<b>not bold</b>\n```")
      refute out =~ "<b>not bold</b>"
      assert out =~ "&lt;b&gt;not bold&lt;/b&gt;"
    end

    test "ampersands are escaped" do
      assert html("Tom & Jerry") =~ "Tom &amp; Jerry"
    end
  end
end
