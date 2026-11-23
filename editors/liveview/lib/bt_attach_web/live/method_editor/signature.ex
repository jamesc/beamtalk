# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttachWeb.Live.MethodEditor.Signature do
  @moduledoc """
  Reads a method's selector out of its source signature (BT-2606), for the
  new-method tab's breadcrumb and pre-flight validation. Pure; extracted from
  `BtAttachWeb.Live.MethodEditor` (BT-5112) with no behaviour change.

  """

  # Parse the selector from a method's source signature (BT-2606), returning
  # `""` when no valid signature is present yet. The author writes the full
  # method (`selector ... => body`) in the editor body; this recovers the
  # selector the same way the compiler's method header grammar does, so the
  # new-method tab no longer needs a separate selector input.
  #
  # It is intentionally conservative: a string it can't confidently read as
  # a header returns `""` (treated as "no signature yet"). The authoritative
  # parse still runs server-side in the `:save` op, which rejects a selector
  # that disagrees with the compiled body — so this client-side read only
  # has to be right for the breadcrumb hint and the pre-flight validation,
  # never the install key of record.
  @doc """
  The selector of the method whose source starts with `source`, or `""`.
  """
  def selector(source) when is_binary(source) do
    source
    |> strip_leading_comments()
    |> strip_method_modifiers()
    |> selector_from_header()
  end

  def selector(_source), do: ""

  # Drop leading blank lines and whole-line `//` / `///` comments, returning
  # the first line that begins a real method header (mirrors the backend
  # `skip_leading_comments/1`).
  defp strip_leading_comments(source) do
    trimmed = String.trim_leading(source)

    if String.starts_with?(trimmed, "//") do
      case String.split(trimmed, "\n", parts: 2) do
        [_comment, rest] -> strip_leading_comments(rest)
        [_comment] -> ""
      end
    else
      trimmed
    end
  end

  # Drop leading method modifiers (`class`, `internal`, `sealed`) that may
  # precede the selector, matching the parser's `parse_method_definition`
  # modifier loop. A modifier word is only stripped when more header text
  # follows it — a bare `class =>` is a method *named* `class`, not the
  # `class` modifier.
  defp strip_method_modifiers(header) do
    case String.split(header, ~r/\s+/, parts: 2) do
      [word, rest] when word in ["class", "internal", "sealed"] ->
        # `class`/`internal`/`sealed` are modifiers only when a selector
        # follows.
        cond do
          # The next token opens the body (`=>`): the word itself is the
          # (unary) selector — a method *named* `class`/`internal`/`sealed`.
          String.starts_with?(rest, "=>") ->
            header

          # The next token is `->`. This is ambiguous without type context
          # (BT-2625): `sealed -> Type =>` is a unary method named `sealed`
          # with a return type, while `sealed -> arg =>` is a `sealed`
          # *binary* method whose selector is `->`. Disambiguate by the
          # token after `->`, matching the parser's grammar: a Capitalized
          # token reads as a return Type (the modifier word is the unary
          # selector, so keep the header), a lowercase token reads as a
          # binary-selector argument (strip the modifier so `->` becomes
          # the selector).
          String.starts_with?(rest, "->") ->
            if return_type_follows_arrow?(rest), do: header, else: strip_method_modifiers(rest)

          true ->
            strip_method_modifiers(rest)
        end

      _ ->
        header
    end
  end

  # Decide whether the text starting at a `->` is a *return-type* annotation
  # (`-> Type`) rather than a binary selector whose argument follows (`->
  # arg`). A return type is a Capitalized identifier (a Type name); a
  # binary-selector parameter is a lowercase identifier. Matches the
  # compiler's method-header grammar (BT-2625). A `->` with no following
  # identifier is treated as a binary selector (not a return type).
  defp return_type_follows_arrow?(rest) do
    rest
    |> String.replace_prefix("->", "")
    |> String.trim_leading()
    |> case do
      <<first::utf8, _::binary>> -> first in ?A..?Z
      _ -> false
    end
  end

  # Recover the selector token(s) from the start of a method header: a
  # keyword selector (`at:put:` from `at: i put: v => …`), a binary
  # selector (`+`, `->`, `>>`, … from `+ other => …`), or a unary selector
  # (`increment` from `increment => …`). Returns `""` when the head doesn't
  # read as a header (no `=>` before the first statement break, or an
  # empty/blank head).
  defp selector_from_header(""), do: ""

  defp selector_from_header(header) do
    # Only the header up to the body arrow can contribute a selector — bail
    # when there is no `=>` before the first newline.
    head =
      header
      |> String.split(~r/\r?\n/, parts: 2)
      |> List.first()

    cond do
      not String.contains?(head, "=>") ->
        ""

      true ->
        head
        |> String.split("=>", parts: 2)
        |> List.first()
        |> selector_from_signature_text()
    end
  end

  # Extract the selector from the text *before* the `=>` arrow. Keyword
  # selectors are the concatenation of their `keyword:` parts (parameter
  # names, type annotations and the return type are dropped); a binary
  # selector is its leading operator; a unary selector is its single
  # identifier.
  defp selector_from_signature_text(sig) do
    sig = String.trim(sig)

    # `:(?!:)` so a compact type annotation (`i::Integer`) doesn't capture
    # `i:` as a keyword part — only a real `keyword:` (single colon)
    # counts.
    keyword_parts = Regex.scan(~r/([A-Za-z_][A-Za-z0-9_]*):(?!:)/, sig, capture: :all_but_first)

    cond do
      sig == "" ->
        ""

      keyword_parts != [] ->
        keyword_parts |> Enum.map(fn [k] -> k <> ":" end) |> Enum.join()

      true ->
        case Regex.run(~r/^([A-Za-z_][A-Za-z0-9_]*)/, sig) do
          [_, ident] ->
            ident

          nil ->
            # Binary selector: the leading run of operator characters (e.g.
            # `+`, `->`, `>>`, `<=`). Anything else is not a recognizable
            # header.
            case Regex.run(~r/^([-+*\/~<>=&|@%^?]+)/, sig) do
              [_, op] -> op
              nil -> ""
            end
        end
    end
  end
end
