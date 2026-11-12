%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_text).

%%% **DDD Context:** Object System Context / Shared Kernel

-moduledoc """
Single authority for normalising a name-like term (atom, binary, string, or
arbitrary term) to a UTF-8 binary.

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

-export([to_binary/1, unwrap/1, unwrap_or_raise/4]).

-doc "Normalise an atom, binary, String, list, or arbitrary term to a UTF-8 binary.".
-spec to_binary(term()) -> binary().
to_binary({beamtalk_string, Bin}) when is_binary(Bin) ->
    %% BT-4170: a tagged `String` unwraps to its bytes (valid UTF-8 by
    %% construction). A bare binary below is bytes, passed through as-is.
    Bin;
to_binary(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
to_binary(Name) when is_binary(Name) ->
    Name;
to_binary(Name) when is_list(Name) ->
    list_to_binary(Name);
to_binary(Name) ->
    list_to_binary(io_lib:format("~p", [Name])).

-doc """
Unwrap a text value — a tagged `String` or a bare binary — to its bytes.
Answers `error` for anything else (including a tagged value whose payload is
not a binary).
""".
-spec unwrap(term()) -> {ok, binary()} | error.
unwrap({beamtalk_string, Bin}) when is_binary(Bin) -> {ok, Bin};
unwrap(Bin) when is_binary(Bin) -> {ok, Bin};
unwrap(_) -> error.

-doc """
Unwrap a text value (tagged `String` or bare binary) to its bytes, or raise a
`type_error` for `Class`/`Selector` carrying `Hint`.

Pass `undefined` as `Hint` for a type error with no hint. Shared by the
stdlib primitives that accept either spelling of text (`File`, `String`,
`Subprocess`, ...) so none re-types the unwrap-or-raise clauses.
""".
-spec unwrap_or_raise(term(), atom(), atom(), term()) -> binary() | no_return().
unwrap_or_raise(Value, Class, Selector, Hint) ->
    case unwrap(Value) of
        {ok, Bin} -> Bin;
        error -> beamtalk_error:raise_type_error(Class, Selector, Hint)
    end.
