%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_corpus).

-moduledoc """
Shared EUnit test helper: locate the project root and load a JSON
conformance-corpus fixture from it (BT-3099).

`beamtalk_compiler`, `beamtalk_runtime`, and `beamtalk_workspace` each have
EUnit suites that pin an Erlang-side implementation to a JSON corpus shared
with the Rust side (`crates/beamtalk-core`) — walking up from the test CWD to
the directory holding the workspace `Cargo.toml`, then reading the fixture.
That pattern was copied byte-for-byte across six test files before BT-3099
extracted it here.

**Why a standalone app, not a shared `test/` dir on one of the peer apps:**
`beamtalk_compiler` is a peer of `beamtalk_runtime`, not a dependent (ADR
0022 — "the compiler has no dependency on the runtime"). Putting this helper
under `beamtalk_runtime/test/` would make `beamtalk_compiler`'s test suite
reach into a peer app's test tree, quietly recreating the dependency ADR
0022 rejected. `beamtalk_test_support` is a fourth, independent umbrella app
(picked up automatically via `{project_app_dirs, ["apps/*"]}` in the
top-level `rebar.config`) that only test suites use — none of
`beamtalk_compiler.app.src` / `beamtalk_runtime.app.src` /
`beamtalk_workspace.app.src` list it in `applications`, so it is never part
of any production boot sequence or release.
""".

-export([project_root/0, load_json_fixture/1]).

-doc """
Walk up from the current working directory to the project root — the
directory containing the workspace `Cargo.toml` — and return its absolute
path. Raises `error({project_root_not_found})` if the filesystem root is
reached first (should not happen when run from inside the repo).
""".
-spec project_root() -> file:filename().
project_root() ->
    find_project_root(filename:absname("")).

find_project_root("/") ->
    error(project_root_not_found);
find_project_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
        true -> Dir;
        false -> find_project_root(filename:dirname(Dir))
    end.

-doc """
Read and JSON-decode a conformance-corpus fixture addressed by
`PathSegments`, a list of path components relative to the project root
(e.g. `["runtime", "apps", "beamtalk_runtime", "test", "fixtures",
"class_method_fun_name_corpus.json"]`). Raises
`error({corpus_file_unreadable, Path, Reason})` if the file cannot be read.
""".
-spec load_json_fixture([file:filename_all()]) -> json:decode_value().
load_json_fixture(PathSegments) when is_list(PathSegments) ->
    Path = filename:join([project_root() | PathSegments]),
    Bin =
        case file:read_file(Path) of
            {ok, B} -> B;
            {error, Reason} -> error({corpus_file_unreadable, Path, Reason})
        end,
    json:decode(Bin).
