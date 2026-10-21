# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0

defmodule VeBench.MixProject do
  use Mix.Project

  # The Elixir half of benchmarks/vs-elixir. No dependencies and no
  # application: the harness loads the compiled modules (and the
  # consolidated protocols) into the same node as the Beamtalk side.
  def project do
    [
      app: :ve_bench,
      version: "0.1.0",
      elixir: "~> 1.18",
      consolidate_protocols: true,
      deps: []
    ]
  end

  def application, do: []
end
