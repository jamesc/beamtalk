# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PROTOTYPE (BT-4852, ADR 0149). Usage: mix run run_exdoc.exs <ebin dir> <output dir>
[ebin, out] = System.argv()
{:ok, _} = Application.ensure_all_started(:ex_doc)
Code.prepend_path(ebin)
res = ExDoc.generate("demo", "0.1.0", [ebin], output: out, formatters: ["html"], proglang: :erlang)
IO.inspect(res, label: "ExDoc.generate")
