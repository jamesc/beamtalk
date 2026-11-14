# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PROTOTYPE (BT-4852, ADR 0149): what Elixir sees. Usage: elixir -pa <ebin built with noload> e8_iex.exs
import IEx.Helpers
for m <- [:"bt@demo@counter", :"bt@demo@tally"] do
  case Code.fetch_docs(m) do
    {:docs_v1, _, lang, fmt, mdoc, _meta, docs} -> IO.puts("Code.fetch_docs(#{m}): language=#{lang} format=#{fmt} moduledoc?=#{is_map(mdoc)} entries=#{length(docs)}")
    other -> IO.inspect(other, label: "Code.fetch_docs(#{m})")
  end
end
IO.puts("--- h :bt@demo@counter"); h(:"bt@demo@counter")
IO.puts("--- h :bt@demo@counter.increment"); h(:"bt@demo@counter".increment)
IO.puts("--- h :bt@demo@counter.increment/1"); h(:"bt@demo@counter".increment/1)
IO.puts("--- h :bt@demo@counter.\"add:\"/2"); h(:"bt@demo@counter"."add:"/2)
IO.puts("--- h :bt@demo@tally.total/1 (not an exported function)"); h(:"bt@demo@tally".total/1)
IO.puts("--- exports :bt@demo@counter"); exports(:"bt@demo@counter")
