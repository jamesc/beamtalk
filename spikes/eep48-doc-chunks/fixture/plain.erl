%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE fixture (BT-4852, ADR 0149): the baseline that does get a Docs chunk.
-module(plain).
-moduledoc "Module doc from -moduledoc.".
-export([inc/1]).
-doc "Adds one.".
-doc #{since => "1.0"}.
inc(X) -> X + 1.
