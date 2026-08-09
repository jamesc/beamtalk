%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_text).

%%% **DDD Context:** Object System Context / Shared Kernel

-moduledoc """
Single authority for normalising a name-like term (atom, binary, string, or
arbitrary term) to a UTF-8 binary (BT-3090).

Before this module, `to_binary/1` was re-typed five times: the complete
atom/binary/list/other version in `beamtalk_repl_protocol` and (as
`format_name/1`) `beamtalk_repl_errors`, and two *incomplete* atom/binary-only
copies in `beamtalk_interface` and `beamtalk_compiler_port` that would raise
`function_clause` if ever handed a list. All but `beamtalk_compiler_port`'s
now delegate here — `beamtalk_compiler` is a peer of `beamtalk_runtime`, not a
dependent (ADR 0022: "the compiler has no dependency on the runtime"), so its
copy cannot reach this module without breaking that boundary. Its copy is
narrower by construction (every call site guards `is_atom/1 orelse
is_binary/1` before calling it, so it can never hit the list/other case) and
is pinned by a golden test instead — see `beamtalk_compiler_port_tests.erl`.
""".

-export([to_binary/1]).

-doc "Normalise an atom, binary, list, or arbitrary term to a UTF-8 binary.".
-spec to_binary(term()) -> binary().
to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
to_binary(Name) when is_binary(Name) ->
    Name;
to_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
to_binary(Name) ->
    list_to_binary(io_lib:format("~p", [Name])).
