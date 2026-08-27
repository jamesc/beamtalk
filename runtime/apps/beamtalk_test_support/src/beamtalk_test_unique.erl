%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_unique).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: a genuinely cross-invocation-unique identifier for
on-disk test paths (BT-3281).

`erlang:unique_integer/1` alone is NOT enough entropy for a workspace id /
temp `HOME` directory string that a test resolves an **on-disk** path from:
its counter resets on every fresh `rebar3 eunit` invocation (each is a
genuinely fresh BEAM VM), and a given call site is reached after a
deterministic, fixed number of prior `unique_integer` calls within a test
run — so two SEPARATE invocations can compute the IDENTICAL id/path. When
that path is one a fixture's own code later reads back (e.g.
`beamtalk_workspace_changelog`'s `load_from_disk`, restoring
`changes.jsonl`), a prior run's leftover on-disk state leaks into the new
run and causes intermittent, non-reproducing test failures — not from a real
code defect, but from accidental cross-run state leakage via a reused path.
Confirmed directly: an accumulated `changes.jsonl` at one such resolved path
held entries from 4+ separate `rebar3 eunit` invocations spanning ~47
minutes of a debugging session (~30-45% intermittent failure rate before the
fix below, 8/8 clean runs after).

`os:getpid()` — the OS process id, genuinely distinct per separate VM
invocation, unlike the in-VM counter — closes the gap when mixed into the
same string. The two halves are joined with a single lowercase-letter
separator (`z`), not `-`: BT-3281's own audit found a fixture
(`beamtalk_workspace_revert_tests.erl`'s `case_setup/0`) that folds `Unique`
into a Beamtalk *class name*, where a bare `-` parses as subtraction and
breaks compilation. A separator is still required, though: `os:getpid()`
and the unique-integer counter are both pure decimal-digit strings, so
concatenating them directly is ambiguous at the digit boundary (e.g. pid
`"123"` + counter `"45"` and pid `"12"` + counter `"345"` both yield
`"12345"`) — a lower-probability reintroduction of the exact
cross-invocation collision this module exists to close. A single ASCII
letter removes that ambiguity while staying safe both as a path segment
and as an identifier suffix.

This only matters for **on-disk** path uniqueness. A `Unique` value that
only ever backs an in-memory identifier (an ETS key, a process-local atom, a
map key never persisted to disk) does not need this: nothing outlives that
one VM invocation to collide with.

Lives in `beamtalk_test_support` rather than `beamtalk_workspace/test/`
(where the bug was first found and fixed) for the same reason
`beamtalk_test_corpus` does — see its `moduledoc` — `beamtalk_runtime` and
`beamtalk_compiler` EUnit suites use HOME-rooted fixture directories too and
would otherwise reach into a peer app's test tree to reuse this.
""".

-export([id/0]).

-doc """
A short string, distinct from every other call in this VM invocation AND
from any call made by a separate `rebar3 eunit` invocation (past or
future) — safe to fold into an on-disk path (workspace id, temp `HOME`
directory, project fixture directory, ...) that a test's own code might
later read back, AND safe to fold into an identifier (a Beamtalk class name,
a module atom) since it contains only ASCII digits and a single lowercase
letter separator — no `-`, `_`, or other punctuation that could change how
the result parses.

Not safe as a cryptographic nonce or for concurrent-invocation isolation
(two `rebar3 eunit` processes racing at the same instant, same PID
namespace, are not distinguished by this alone) — only for the sequential
cross-invocation case this module exists for.
""".
-spec id() -> string().
id() ->
    os:getpid() ++ "z" ++ integer_to_list(erlang:unique_integer([positive])).
