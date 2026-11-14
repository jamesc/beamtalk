# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PROTOTYPE (BT-4852, ADR 0149). Does Elixir read `doc/chunks/<Mod>.chunk` side
# files when the .beam has no Docs chunk? Erlang's code:get_doc/1 does.
# Usage: elixir -pa <stdlib ebin with ../doc/chunks written by E6> e11_elixir_side_file.exs
m = :"bt@stdlib@integer"
{:ok, _, cs} = :beam_lib.all_chunks(:code.which(m))
IO.puts("Docs chunk inside the .beam: #{List.keymember?(cs, ~c"Docs", 0)}")
IO.inspect(:code.get_doc(m) |> elem(0), label: ":code.get_doc/1")
IO.inspect(Code.fetch_docs(m) |> then(fn t -> if elem(t, 0) == :docs_v1, do: {:docs_v1, elem(t, 2)}, else: t end), label: "Code.fetch_docs/1")
