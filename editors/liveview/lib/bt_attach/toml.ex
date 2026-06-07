# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule BtAttach.Toml do
  @moduledoc """
  A deliberately tiny TOML reader for `~/.beamtalk/ide.toml` (ADR 0091 Decision 1).

  This parses only the small, fixed subset the IDE config file uses — string
  values and arrays-of-strings under `[oidc]` / `[oidc.roles]` tables — rather
  than pulling in a general-purpose TOML dependency. The config file gates an
  RCE-bearing tool, and ADR 0091 flags "a compromised Elixir dep" as an explicit
  threat vector, so a small in-tree, audited reader is preferred over a broad
  parser for this one file (the issue calls for "a small TOML reader").

  ## Supported subset

    * blank lines and `#` comments (whole-line and trailing)
    * table headers: `[oidc]`, `[oidc.roles]` (dotted, nested)
    * `key = "string"` with `\\"` and `\\\\` escapes
    * `key = ["a", "b"]` — single-line array of strings

  Anything outside this subset (integers, floats, booleans, inline tables,
  multiline strings, dotted keys) is **not** supported and yields
  `{:error, {:unsupported, line_number, line}}` rather than a silent
  mis-parse — fail loud on a security config file.

  Returns a nested map keyed by binary strings, e.g.

      %{"oidc" => %{"issuer" => "https://idp", "roles" => %{"owner" => ["g1"]}}}
  """

  @type t :: %{optional(String.t()) => String.t() | [String.t()] | t()}

  @doc """
  Parse TOML `source` into a nested map, or return a structured error.

  Errors are `{:error, {:unsupported | :malformed, line_number, raw_line}}` so
  the caller can point an operator at the exact offending line.
  """
  @spec parse(String.t()) :: {:ok, t()} | {:error, term()}
  def parse(source) when is_binary(source) do
    source
    |> String.split(["\r\n", "\n"])
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, %{}, []}, fn {raw, lineno}, {:ok, acc, path} ->
      case parse_line(strip_comment(raw)) do
        :blank -> {:cont, {:ok, acc, path}}
        {:table, new_path} -> {:cont, {:ok, ensure_path(acc, new_path), new_path}}
        {:pair, key, value} -> {:cont, {:ok, put_in_path(acc, path ++ [key], value), path}}
        {:error, kind} -> {:halt, {:error, {kind, lineno, raw}}}
      end
    end)
    |> case do
      {:ok, map, _path} -> {:ok, map}
      {:error, _} = err -> err
    end
  end

  # Drop a trailing `# comment`, but not a `#` that sits inside a quoted string.
  # We scan character by character, tracking whether we're inside double quotes
  # (honouring `\"`), and cut at the first unquoted `#`.
  defp strip_comment(line), do: strip_comment(line, <<>>, false, false)

  defp strip_comment(<<>>, acc, _in_str, _esc), do: acc

  defp strip_comment(<<?\\, rest::binary>>, acc, true, false),
    do: strip_comment(rest, acc <> "\\", true, true)

  defp strip_comment(<<?\", rest::binary>>, acc, in_str, true),
    do: strip_comment(rest, acc <> "\"", in_str, false)

  defp strip_comment(<<?\", rest::binary>>, acc, in_str, false),
    do: strip_comment(rest, acc <> "\"", not in_str, false)

  defp strip_comment(<<?#, _rest::binary>>, acc, false, _esc), do: acc

  defp strip_comment(<<c::utf8, rest::binary>>, acc, in_str, _esc),
    do: strip_comment(rest, acc <> <<c::utf8>>, in_str, false)

  defp parse_line(line) do
    case String.trim(line) do
      "" ->
        :blank

      "[" <> _ = header ->
        parse_table(header)

      pair ->
        parse_pair(pair)
    end
  end

  defp parse_table(header) do
    trimmed = String.trim(header)

    if String.ends_with?(trimmed, "]") do
      inner = trimmed |> String.slice(1..-2//1) |> String.trim()

      segments =
        inner
        |> String.split(".")
        |> Enum.map(&String.trim/1)

      if inner != "" and Enum.all?(segments, &(&1 != "")) do
        {:table, segments}
      else
        {:error, :malformed}
      end
    else
      {:error, :malformed}
    end
  end

  defp parse_pair(pair) do
    case String.split(pair, "=", parts: 2) do
      [key, rawval] ->
        key = String.trim(key)
        val = String.trim(rawval)

        cond do
          key == "" -> {:error, :malformed}
          true -> parse_value(key, val)
        end

      _ ->
        {:error, :malformed}
    end
  end

  defp parse_value(key, "[" <> _ = arr), do: parse_array(key, arr)

  defp parse_value(key, "\"" <> _ = str) do
    case unquote_string(str) do
      {:ok, value} -> {:pair, key, value}
      :error -> {:error, :malformed}
    end
  end

  # Bare values (ints, bools, floats, dates) are intentionally unsupported.
  defp parse_value(_key, _val), do: {:error, :unsupported}

  defp parse_array(key, arr) do
    trimmed = String.trim(arr)

    if String.ends_with?(trimmed, "]") do
      inner = trimmed |> String.slice(1..-2//1) |> String.trim()

      if inner == "" do
        {:pair, key, []}
      else
        inner
        |> split_array_elements()
        |> collect_strings()
        |> case do
          {:ok, values} -> {:pair, key, values}
          :error -> {:error, :malformed}
        end
      end
    else
      {:error, :malformed}
    end
  end

  # Split top-level array elements on commas that sit outside quoted strings.
  defp split_array_elements(inner), do: split_array_elements(inner, <<>>, [], false, false)

  defp split_array_elements(<<>>, cur, acc, _in_str, _esc),
    do: Enum.reverse([String.trim(cur) | acc])

  defp split_array_elements(<<?\\, rest::binary>>, cur, acc, true, false),
    do: split_array_elements(rest, cur <> "\\", acc, true, true)

  defp split_array_elements(<<?\", rest::binary>>, cur, acc, in_str, true),
    do: split_array_elements(rest, cur <> "\"", acc, in_str, false)

  defp split_array_elements(<<?\", rest::binary>>, cur, acc, in_str, false),
    do: split_array_elements(rest, cur <> "\"", acc, not in_str, false)

  defp split_array_elements(<<?,, rest::binary>>, cur, acc, false, _esc),
    do: split_array_elements(rest, <<>>, [String.trim(cur) | acc], false, false)

  defp split_array_elements(<<c::utf8, rest::binary>>, cur, acc, in_str, _esc),
    do: split_array_elements(rest, cur <> <<c::utf8>>, acc, in_str, false)

  defp collect_strings(elements) do
    Enum.reduce_while(elements, {:ok, []}, fn el, {:ok, acc} ->
      case unquote_string(String.trim(el)) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      :error -> :error
    end
  end

  # Unquote a `"..."` basic string, honouring `\"` and `\\`. Rejects anything
  # that is not a single, fully-closed double-quoted string.
  defp unquote_string("\"" <> rest), do: unquote_chars(rest, <<>>)
  defp unquote_string(_), do: :error

  defp unquote_chars(<<?\\, ?\", rest::binary>>, acc), do: unquote_chars(rest, acc <> "\"")
  defp unquote_chars(<<?\\, ?\\, rest::binary>>, acc), do: unquote_chars(rest, acc <> "\\")
  defp unquote_chars(<<?\\, ?n, rest::binary>>, acc), do: unquote_chars(rest, acc <> "\n")
  defp unquote_chars(<<?\\, ?t, rest::binary>>, acc), do: unquote_chars(rest, acc <> "\t")
  # A closing quote must be the final character.
  defp unquote_chars(<<?\">>, acc), do: {:ok, acc}
  defp unquote_chars(<<?\", _more::binary>>, _acc), do: :error
  defp unquote_chars(<<>>, _acc), do: :error
  defp unquote_chars(<<c::utf8, rest::binary>>, acc), do: unquote_chars(rest, acc <> <<c::utf8>>)

  # Ensure a (possibly nested) table path exists as empty maps.
  defp ensure_path(map, []), do: map

  defp ensure_path(map, [head | tail]) do
    child = Map.get(map, head, %{})
    Map.put(map, head, ensure_path(child, tail))
  end

  # Put a value at the dotted path (the current table path ++ key).
  defp put_in_path(map, [key], value), do: Map.put(map, key, value)

  defp put_in_path(map, [head | tail], value) do
    child = Map.get(map, head, %{})
    Map.put(map, head, put_in_path(child, tail, value))
  end
end
