%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_module_name).

%%% **DDD Context:** Object System Context / Shared Kernel

-moduledoc """
Single Erlang-side authority for the `ClassName ⇄ bt@[pkg@]snake_case`
naming convention (ADR 0016, BT-3081).

Before this module, the CamelCase→snake_case conversion was re-typed four
times: `beamtalk_primitive:camel_to_snake/1`, a byte-identical copy in
`beamtalk_repl_ops_dev`, another in `beamtalk_test_case:class_name_to_snake/1`,
and a *drifted* one in `beamtalk_repl_loader:to_snake_case/1` — the last of
which force-lowercased the first character unconditionally instead of
tracking whether it actually started lowercase, and used Unicode
`string:to_lower/1` where the other three used ASCII-only `$A..$Z`
arithmetic. All four now delegate here.

## Forward conversion

`camel_to_snake/1` mirrors the Rust authority
(`crates/beamtalk-core/src/ast/mod.rs` `to_module_name`) character for
character: insert `_` before an uppercase letter only when the *previous*
character was lowercase, then lowercase the uppercase letter; every other
character (including an already-lowercase, digit, or non-alphabetic leading
character) passes through unchanged. It is Unicode-aware — like Rust's
`char::is_uppercase`/`to_lowercase` — not ASCII-only, so it treats accented
and other non-ASCII letters the same way both sides of the compiler agree on.

## Assembly

`to_module_atom/1`, `to_stdlib_module_atom/1`, and `to_package_module_atom/2`
build the three `bt@…` module-atom shapes the compiler and runtime use:
unqualified static (`bt@{snake}`), stdlib (`bt@stdlib@{snake}`), and
package-qualified (`bt@{Package}@{snake}`).

## Inverse (lossy — fallback only)

`snake_to_class/1` is the naive inverse: split on `_`, capitalize each
segment. It is **provably lossy** for acronym-cased classes — `BEAMError`
(module `bt@stdlib@beamerror`) round-trips to `'Beamerror'`, not
`'BEAMError'` — because the original casing inside a run of letters is
gone once folded to snake_case. Never treat this as the source of truth for
module→class resolution; `beamtalk_stack_frame:module_to_class/1` consults
the live class registry first (which knows the real casing) and falls back
to this heuristic only when a module isn't a registered class.
""".

-export([
    camel_to_snake/1,
    to_module_atom/1,
    to_stdlib_module_atom/1,
    to_package_module_atom/2,
    is_stdlib_module/1,
    snake_to_class/1
]).

%%====================================================================
%% Forward: CamelCase -> snake_case
%%====================================================================

-doc """
Convert a CamelCase string to snake_case, matching Rust `to_module_name`
(`crates/beamtalk-core/src/ast/mod.rs`) exactly — see the moduledoc.

## Examples

```
1> beamtalk_module_name:camel_to_snake("Counter").
"counter"
2> beamtalk_module_name:camel_to_snake("MyCounterActor").
"my_counter_actor"
3> beamtalk_module_name:camel_to_snake("HTTPRouter").
"httprouter"
4> beamtalk_module_name:camel_to_snake("myClass").
"my_class"
```
""".
-spec camel_to_snake(string()) -> string().
camel_to_snake(Str) when is_list(Str) ->
    camel_to_snake(Str, false, []).

%% Acc is built up in reverse (standard Erlang accumulator idiom) and
%% reversed once at the end, so every step is O(1) amortized rather than
%% the O(n) list-append per character an `Acc ++ [...]` pattern would cost.
-spec camel_to_snake(string(), boolean(), string()) -> string().
camel_to_snake([], _PrevWasLower, Acc) ->
    lists:reverse(Acc);
camel_to_snake([Ch | Rest], PrevWasLower, Acc) ->
    case is_upper_char(Ch) of
        true ->
            %% to_lower_chars/1 is almost always one character, but a
            %% handful of codepoints lower to more than one — reverse that
            %% short run before prepending so the final reverse restores it
            %% in the right order.
            RevLowerChars = lists:reverse(to_lower_chars(Ch)),
            NewAcc =
                case PrevWasLower of
                    true -> RevLowerChars ++ [$_ | Acc];
                    false -> RevLowerChars ++ Acc
                end,
            camel_to_snake(Rest, false, NewAcc);
        false ->
            camel_to_snake(Rest, is_lower_char(Ch), [Ch | Acc])
    end.

%% Unicode-aware uppercase test for a single codepoint: true when the char
%% has distinct case and is already in its upper form (mirrors Rust's
%% `char::is_uppercase`). Case-less codepoints (digits, CJK, …) are neither
%% upper nor lower, same as Rust.
-spec is_upper_char(char()) -> boolean().
is_upper_char(Ch) ->
    Upper = string:to_upper([Ch]),
    Lower = string:to_lower([Ch]),
    Upper =/= Lower andalso Upper =:= [Ch].

%% Unicode-aware lowercase test for a single codepoint — see is_upper_char/1.
-spec is_lower_char(char()) -> boolean().
is_lower_char(Ch) ->
    Upper = string:to_upper([Ch]),
    Lower = string:to_lower([Ch]),
    Upper =/= Lower andalso Lower =:= [Ch].

%% A handful of codepoints lowercase to more than one character (mirrors
%% Rust's `char::to_lowercase()`, which is also multi-char for those); this
%% returns every resulting character, not just the first.
-spec to_lower_chars(char()) -> string().
to_lower_chars(Ch) ->
    string:to_lower([Ch]).

%%====================================================================
%% Assembly: ClassName atom -> bt@... module atom
%%====================================================================

-doc "Unqualified static module atom: `bt@{snake_case}` (no `stdlib` segment).".
-spec to_module_atom(atom()) -> atom().
to_module_atom(ClassName) when is_atom(ClassName) ->
    to_atom("bt@" ++ camel_to_snake(atom_to_list(ClassName))).

-doc "Stdlib module atom: `bt@stdlib@{snake_case}`.".
-spec to_stdlib_module_atom(atom()) -> atom().
to_stdlib_module_atom(ClassName) when is_atom(ClassName) ->
    to_atom("bt@stdlib@" ++ camel_to_snake(atom_to_list(ClassName))).

-doc """
Package-qualified module atom: `bt@{Package}@{snake_case}`.

`Package` is inserted verbatim (it is already a valid module-name segment,
e.g. a `beamtalk.toml` package name) — only the class name is snake_cased.
""".
-spec to_package_module_atom(atom(), atom() | binary() | string()) -> atom().
to_package_module_atom(ClassName, Package) when is_atom(ClassName), is_binary(Package) ->
    to_package_module_atom(ClassName, unicode:characters_to_list(Package, utf8));
to_package_module_atom(ClassName, Package) when is_atom(ClassName), is_atom(Package) ->
    to_package_module_atom(ClassName, atom_to_list(Package));
to_package_module_atom(ClassName, Package) when is_atom(ClassName), is_list(Package) ->
    to_atom("bt@" ++ Package ++ "@" ++ camel_to_snake(atom_to_list(ClassName))).

-spec to_atom(string()) -> atom().
to_atom(Str) ->
    try
        list_to_existing_atom(Str)
    catch
        error:badarg ->
            % elp:fixme W0023 intentional atom creation
            list_to_atom(Str)
    end.

%%====================================================================
%% is_stdlib_module/1
%%
%% BT-3081: was duplicated byte-for-byte in beamtalk_class_registry (BT-738)
%% and beamtalk_behaviour_intrinsics (BT-785); both now delegate here.
%%====================================================================

-doc "Whether a module atom belongs to the Beamtalk stdlib (`bt@stdlib@` prefix).".
-spec is_stdlib_module(atom()) -> boolean().
is_stdlib_module(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@stdlib@", _/binary>> -> true;
        _ -> false
    end;
is_stdlib_module(_) ->
    false.

%%====================================================================
%% Inverse (lossy) — snake_case -> CamelCase, fallback only
%%====================================================================

-doc """
Best-effort snake_case → CamelCase class name guess: split on `_`, capitalize
each segment, and turn the result into an atom via `list_to_existing_atom`
(returns `nil` if that atom was never created — e.g. no such class exists).

**Lossy** — see the moduledoc. Prefer resolving a module to its class via the
live class registry (`beamtalk_class_registry:class_name_for_module/1`) and
use this only as a last-resort fallback.
""".
-spec snake_to_class(string()) -> atom() | 'nil'.
snake_to_class(Snake) ->
    Words = string:split(Snake, "_", all),
    Capitalized = [capitalize(W) || W <- Words],
    try
        list_to_existing_atom(lists:flatten(Capitalized))
    catch
        error:badarg -> nil
    end.

-spec capitalize(string()) -> string().
capitalize([]) ->
    [];
capitalize([H | T]) ->
    to_upper_chars(H) ++ T.

-spec to_upper_chars(char()) -> string().
to_upper_chars(Ch) ->
    string:to_upper([Ch]).
