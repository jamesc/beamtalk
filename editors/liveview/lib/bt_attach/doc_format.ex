# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.DocFormat do
  @moduledoc """
  Render a Beamtalk doc-comment as a small, safe HTML documentation block for the
  System Browser (BT-2558).

  The System Browser is the IDE's help destination: `browse_class_definition`
  carries a class' comment and `browse_method_source` carries a method's `///`
  doc-comment (the same text `Beamtalk help:` / `help:selector:` reads). Both are
  authored as a small Markdown subset; this module turns that text into a
  read-only HTML block so the browser reads like docs instead of a raw blob.

  Supported Markdown subset (what Beamtalk doc-comments actually use):

    * ATX headings — `#` .. `######`
    * fenced code blocks — ` ```lang ` … ` ``` `
    * indented code blocks — a block-start run of lines indented ≥4 spaces / a tab
    * unordered lists — lines starting with `- ` or `* `
    * blank-line-separated paragraphs
    * inline `` `code` ``, `**bold**`, and `*italic*` / `_italic_` spans

  Every piece of author text is HTML-escaped *before* any markup is added, so a
  doc-comment can never inject HTML — the only tags emitted are the ones this
  module produces. The result is a `Phoenix.HTML`-safe tuple, ready to render
  directly in a HEEx template.
  """

  @typedoc "A `Phoenix.HTML`-safe value, or `nil` when there is nothing to render."
  @type safe :: {:safe, iodata()} | nil

  @doc """
  Render a doc-comment string to a safe HTML block.

  Returns `nil` for `nil` / blank input (the caller renders no doc block), or a
  `{:safe, iodata}` tuple otherwise.
  """
  @spec to_html(binary() | nil) :: safe()
  def to_html(nil), do: nil

  def to_html(doc) when is_binary(doc) do
    case String.trim(doc) do
      "" ->
        nil

      _ ->
        html =
          doc
          |> normalize_newlines()
          |> String.split("\n")
          |> blocks([])
          |> Enum.reverse()

        {:safe, html}
    end
  end

  # ── Block parsing ──────────────────────────────────────────────────────────

  # Walk the lines, accumulating rendered blocks (newest-first; the caller
  # reverses). Each clause consumes one block's worth of lines and recurses on
  # the rest.
  defp blocks([], acc), do: acc

  # Fenced code block: collect verbatim lines until the closing fence (or EOF),
  # so code is never interpreted as markup.
  defp blocks([line | rest], acc) do
    cond do
      fence?(line) ->
        {code_lines, after_fence} = take_until_fence(rest, [])
        blocks(after_fence, [code_block(code_lines) | acc])

      heading_level(line) > 0 ->
        blocks(rest, [heading_block(line) | acc])

      list_item?(line) ->
        {items, after_list} = take_list_items([line | rest], [])
        blocks(after_list, [list_block(items) | acc])

      indented_code?(line) ->
        # An indented code block at block-start (the fence?/heading/list clauses
        # above already ruled those out, so a `>=4`-space / tab line here is a
        # top-level code block — a list item's indented continuation never
        # reaches this clause because `list_item?` claimed it first).
        {code_lines, after_code} = take_indented_code([line | rest], [])
        blocks(after_code, [code_block(strip_indent(code_lines)) | acc])

      blank?(line) ->
        # A paragraph separator on its own — nothing to emit.
        blocks(rest, acc)

      true ->
        {para_lines, after_para} = take_paragraph([line | rest], [])
        blocks(after_para, [paragraph_block(para_lines) | acc])
    end
  end

  # Collect lines up to (and consuming) the closing ``` fence. On EOF the open
  # block still renders — an unterminated fence shows its content rather than
  # swallowing the rest of the doc.
  defp take_until_fence([], acc), do: {Enum.reverse(acc), []}

  defp take_until_fence([line | rest], acc) do
    if fence?(line) do
      {Enum.reverse(acc), rest}
    else
      take_until_fence(rest, [line | acc])
    end
  end

  # Consecutive `- ` / `* ` lines form one list.
  defp take_list_items([line | rest], acc) do
    if list_item?(line) do
      take_list_items(rest, [strip_list_marker(line) | acc])
    else
      {Enum.reverse(acc), [line | rest]}
    end
  end

  defp take_list_items([], acc), do: {Enum.reverse(acc), []}

  # A paragraph runs until a blank line or the start of another block kind. An
  # indented-code line does *not* end a paragraph mid-stream: indented code is
  # only recognised at block-start (after a blank line / doc start), so a wrapped
  # paragraph line that happens to be indented stays part of the paragraph.
  defp take_paragraph([line | rest], acc) do
    if blank?(line) or fence?(line) or heading_level(line) > 0 or list_item?(line) do
      {Enum.reverse(acc), [line | rest]}
    else
      take_paragraph(rest, [line | acc])
    end
  end

  defp take_paragraph([], acc), do: {Enum.reverse(acc), []}

  # Collect a run of indented-code lines. Blank lines *within* the block are kept
  # (a blank line between two indented lines is part of the code), but a blank
  # line that is the last line before a non-indented line ends the block and is
  # not consumed — trailing blanks are trimmed so the `<pre>` has no empty tail.
  defp take_indented_code([line | rest], acc) do
    cond do
      indented_code?(line) -> take_indented_code(rest, [line | acc])
      blank?(line) and continues_indented_code?(rest) -> take_indented_code(rest, [line | acc])
      true -> {Enum.reverse(acc), [line | rest]}
    end
  end

  defp take_indented_code([], acc), do: {Enum.reverse(acc), []}

  # Is the NEXT non-blank line still indented code? Used to decide whether a blank
  # line is interior to the code block or ends it. We look only at the first
  # non-blank line: if it is indented the blank is interior; otherwise (prose, or
  # end of input) the blank ends the block. (`Enum.find_value` can't be used here
  # — `false` is falsy, so a `false` arm would not stop the scan and a later
  # indented block could pull an intervening blank into this one.)
  defp continues_indented_code?(lines) do
    case Enum.find(lines, &(not blank?(&1))) do
      nil -> false
      line -> indented_code?(line)
    end
  end

  # Strip the common indent (a leading tab, else four spaces) from each code
  # line, leaving the code's own relative indentation intact. Blank lines stay
  # blank.
  defp strip_indent(lines) do
    Enum.map(lines, fn line ->
      cond do
        blank?(line) -> ""
        String.starts_with?(line, "\t") -> String.slice(line, 1..-1//1)
        String.starts_with?(line, "    ") -> String.slice(line, 4..-1//1)
        true -> line
      end
    end)
  end

  # ── Block renderers ──────────────────────────────────────────────────────────

  defp code_block(lines) do
    ["<pre class=\"doc-code\"><code>", escape(Enum.join(lines, "\n")), "</code></pre>"]
  end

  defp heading_block(line) do
    level = heading_level(line)
    text = line |> String.replace(~r/^\#{1,6}\s*/, "") |> String.trim()
    tag = "h#{min(level + 2, 6)}"
    ["<", tag, " class=\"doc-h\">", inline(text), "</", tag, ">"]
  end

  defp list_block(items) do
    lis = Enum.map(items, fn item -> ["<li>", inline(item), "</li>"] end)
    ["<ul class=\"doc-list\">", lis, "</ul>"]
  end

  defp paragraph_block(lines) do
    text = lines |> Enum.map(&String.trim/1) |> Enum.join(" ")
    ["<p class=\"doc-p\">", inline(text), "</p>"]
  end

  # ── Inline spans ─────────────────────────────────────────────────────────────

  # Split out `` `code` `` spans first so emphasis markers inside code are left
  # literal, then escape every segment and add markup to the text segments only.
  defp inline(text) do
    ~r/`[^`]*`/
    |> Regex.split(text, include_captures: true, trim: true)
    |> Enum.map(fn segment ->
      case code_span(segment) do
        {:ok, code} -> ["<code>", escape(code), "</code>"]
        :no -> segment |> escape() |> emphasis()
      end
    end)
  end

  defp code_span(<<"`", _::binary>> = seg) do
    if String.ends_with?(seg, "`") and byte_size(seg) >= 2 do
      {:ok, String.slice(seg, 1..-2//1)}
    else
      :no
    end
  end

  defp code_span(_), do: :no

  # Bold before italic so `**x**` is not mis-split by the single-`*` rule. Runs on
  # already-escaped text, so the captured groups carry no raw HTML. No dotall flag
  # is needed: `inline/1` only ever sees single-line text (paragraphs are joined
  # with spaces, list items and headings are one line), so the spans never cross a
  # newline.
  defp emphasis(escaped) do
    escaped
    |> String.replace(~r/\*\*(.+?)\*\*/, "<strong>\\1</strong>")
    |> String.replace(~r/(?<![\*\w])\*(?!\s)([^*]+?)\*(?![\*\w])/, "<em>\\1</em>")
    |> String.replace(~r/(?<![_\w])_(?!\s)([^_]+?)_(?![_\w])/, "<em>\\1</em>")
  end

  # ── Line classifiers ─────────────────────────────────────────────────────────

  defp normalize_newlines(text) do
    text
    |> String.replace("\r\n", "\n")
    |> String.replace("\r", "\n")
  end

  defp blank?(line), do: String.trim(line) == ""

  defp fence?(line), do: String.starts_with?(String.trim_leading(line), "```")

  defp heading_level(line) do
    case Regex.run(~r/^(\#{1,6})\s/, line) do
      [_, hashes] -> String.length(hashes)
      _ -> 0
    end
  end

  defp list_item?(line) do
    Regex.match?(~r/^\s*[-*]\s+\S/, line)
  end

  # An indented code line: a non-blank line that starts with a tab or at least
  # four spaces. Callers only test this at block-start, after the fence/heading/
  # list classifiers have run, so it never claims a list marker or a fence.
  defp indented_code?(line) do
    not blank?(line) and (String.starts_with?(line, "\t") or String.starts_with?(line, "    "))
  end

  defp strip_list_marker(line) do
    line |> String.replace(~r/^\s*[-*]\s+/, "") |> String.trim()
  end

  defp escape(text) do
    text
    |> Phoenix.HTML.html_escape()
    |> Phoenix.HTML.safe_to_string()
  end
end
