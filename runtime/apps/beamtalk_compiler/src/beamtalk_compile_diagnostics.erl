%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compile_diagnostics).

-moduledoc """
Formats `compile:forms/2' error terms produced while compiling
Beamtalk-generated Core Erlang to BEAM bytecode (BT-3115).

Every error `compile:forms/2' can return at this pipeline stage is an
internal-compiler-error by definition: the input is compiler-generated
Core Erlang, not user source, so a well-formedness failure here — most
commonly `core_lint''s `unbound_var'/`duplicate_var' (see
`docs/development/debugging.md' § Codegen Debugging) — means codegen
produced invalid output, not that the user wrote bad Beamtalk.

`core_lint' already runs *unconditionally* as part of OTP's `from_core'
compile pipeline: `compile:forms(Forms, [from_core | Opts])' always
includes `core_lint_module' in `core_passes(non_verified_core)'
(`compiler-*/src/compile.erl'), completely independent of the
`clint'/`clint0'/`no_lint' options — those only gate lint re-runs on the
`verified_core' branch taken when compiling from Erlang *source*, which
this from-Core-Erlang pipeline never does. So detection was never the
gap here; readability was. `compile:forms' with `return_errors' hands
back a raw `[{File,[{Loc,Mod,ErrDesc}]}]' term with no formatting,
unlike `compile:file'/`compile:forms' with `report_errors', which prints
through `sys_messages:format_messages/4' — the exact same OTP-internal
function this module calls, so the text this module produces is
byte-for-byte what `report_errors' would have printed (verified: both
render as `"<file>: unbound variable 'State' in foo/1"' for the same
input).

Used by both `beamtalk_build_worker' and `beamtalk_compiler_server' (the
in-memory Port backend, ADR 0022 Phase 3) so the two never drift. The
escript fallback backend (`compile.escript') cannot depend on this
module — escripts run standalone with no project code path, only OTP's
own applications — so it carries its own small `print_messages/2' that
calls the same `sys_messages:format_messages/4' directly; see that
function's doc for why `report_errors'/`report_warnings' (the more
obvious option) turned out to be actively wrong here, not just an
unshared duplicate of this formatting: they print through the compiling
process's default group leader, which lands on stdout, not stderr — the
Rust CLI's stdout parser only recognises the `beamtalk-compile-*'
protocol markers and silently drops every other line, so the message was
being lost outright rather than merely unformatted.

BT-3126 extended this module with `format_warnings/1' for the same
reason on the *warning* path, fixed in both compile-from-Core-Erlang
callers:

  * `beamtalk_build_worker' (the `beamtalk build' batch CLI worker)
    passed `report_warnings' to `compile:forms/2', which hits the same
    stdout-not-stderr sink and the same silent-drop as the pre-BT-3115
    error path above.
  * `beamtalk_compiler_server' (the in-memory Port backend, ADR 0022
    Phase 3, backing the REPL/LSP/live-compile path) passed neither
    `report_warnings' nor `return_warnings' at all, so `compile:forms'
    computed warnings and then discarded them unconditionally — not
    printed anywhere, not even to stdout.

Either way, `compile:forms' warnings (e.g. unused-variable warnings in
generated Core Erlang) were never surfaced to the developer.
""".

-export([format_errors/1, format_warnings/1, print_warnings/1]).

-doc """
Turn a `compile:forms/2' `{error, Errors, Warnings}' error list into a
single human-readable binary, one line per underlying error, each
carrying whatever identifying detail the originating lint pass reports
(e.g. the unbound variable's name and the enclosing function/arity for
`core_lint').
""".
-spec format_errors([{file:filename() | string() | binary(), [tuple()]}]) -> binary().
format_errors(Errors) when is_list(Errors) ->
    format_messages(Errors, "").

-doc """
Turn a `compile:forms/2' `Warnings' list (from `return_warnings') into a
single human-readable binary, one line per underlying warning, prefixed
`"Warning: "' — matching the wording `report_warnings' would have
printed, and what `compile.escript''s `print_messages/2' already prints
for the escript backend (BT-3115).
""".
-spec format_warnings([{file:filename() | string() | binary(), [tuple()]}]) -> binary().
format_warnings(Warnings) when is_list(Warnings) ->
    format_messages(Warnings, "Warning: ").

format_messages(Messages, Prefix) ->
    Lines = [
        Text
     || {File, ErrorInfos} <- Messages,
        {_Loc, Text} <- sys_messages:format_messages(to_filename(File), Prefix, ErrorInfos, [])
    ],
    unicode:characters_to_binary(Lines).

-doc """
Print a `compile:forms/2' `Warnings' list (from `return_warnings') to
`standard_error' via [`format_warnings/1`](`format_warnings/1`), or do
nothing for an empty list.

Shared sink-selection wrapper for `beamtalk_build_worker' and
`beamtalk_compiler_server' — both call this directly after `compile:forms'
returns the `{ok, ModuleName, Binary, Warnings}' or `{error, Errors,
Warnings}' shape, so the two never drift on *where* warnings get printed,
matching `format_warnings/1' already doing so for *how* they get
formatted.
""".
-spec print_warnings([{file:filename() | string() | binary(), [tuple()]}]) -> ok.
print_warnings([]) ->
    ok;
print_warnings(Warnings) ->
    io:put_chars(standard_error, format_warnings(Warnings)).

%% sys_messages:format_messages/4 expects a string() file identifier;
%% compile:forms/2's error tuples may carry it as a binary depending on
%% how the module name/filename was set upstream.
to_filename(File) when is_binary(File) -> unicode:characters_to_list(File);
to_filename(File) -> File.
