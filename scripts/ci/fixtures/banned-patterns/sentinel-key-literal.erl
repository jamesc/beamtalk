%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%% Fixture for the sentinel-key-literal lint: the code line below must be flagged;
%% the comment and the doc-string lines must not be (see self_test).
-module(sentinel_key_literal_fixture).
-doc """
A doc string may name the `'$beamtalk_class'` key.
""".
tag(#{'$beamtalk_class' := C}) -> C.
