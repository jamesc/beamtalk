# Copyright 2026 James Casey
# SPDX-License-Identifier: Apache-2.0
#
# PROTOTYPE (BT-4852, ADR 0149): scratch project used only to run ExDoc over prototype .beam files
defmodule Eep48Spike.MixProject do
  use Mix.Project
  def project, do: [app: :eep48_spike, version: "0.0.0", deps: [{:ex_doc, "~> 0.38", only: :dev, runtime: false}]]
end
