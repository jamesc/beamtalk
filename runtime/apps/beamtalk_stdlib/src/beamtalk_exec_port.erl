%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_exec_port).

%%% **DDD Context:** runtime

-moduledoc """
OTP Port interface to the beamtalk-exec Rust binary (ADR 0051, Phase 1).

Spawns `beamtalk-exec` as an OTP port with `{packet, 4}` framing and
ETF-encoded commands. The binary manages OS subprocesses and sends
events back as ETF tuples: `{stdout, ChildId, Data}`,
`{stderr, ChildId, Data}`, `{exit, ChildId, ExitCode}`.

The caller is responsible for receiving port messages and dispatching
events. This module only handles port open/close/command mechanics.

== Protocol ==

Commands (BEAM → binary) — ETF maps:
```
#{command => spawn,       child_id => N, executable => Bin, args => [Bin],
                          env => #{Bin => Bin}, dir => Bin}
#{command => kill,        child_id => N}
#{command => write_stdin, child_id => N, data => Bin}
#{command => close_stdin, child_id => N}
```

Events (binary → BEAM) — ETF tuples on the port:
```
{Port, {data, Bin}}  where binary_to_term(Bin) is one of:
  {stdout, ChildId, Data}
  {stderr, ChildId, Data}
  {exit,   ChildId, ExitCode}
```
""".

-export([
    open/0,
    open/1,
    spawn_child/4,
    spawn_child/5,
    kill_child/2,
    write_stdin/3,
    close_stdin/2,
    close/1
]).

-ifdef(TEST).
-export([
    find_exec_binary/0,
    find_project_root/0,
    find_in_project/1
]).
-endif.

-include_lib("kernel/include/logger.hrl").

-doc "Open a port to the exec binary, finding it automatically.".
-spec open() -> port().
open() ->
    open(find_exec_binary()).

-doc "Open a port to the exec binary at the given path.".
-spec open(file:filename_all()) -> port().
open(BinaryPath) ->
    ?LOG_INFO("Opening exec port", #{binary => BinaryPath, domain => [beamtalk, stdlib]}),
    open_port({spawn_executable, BinaryPath}, [
        {packet, 4},
        binary,
        exit_status,
        use_stdio
    ]).

-doc """
Spawn a subprocess through the exec port (no custom env or dir).

`ChildId` is a caller-chosen non-negative integer used to correlate events.
`Executable` is an absolute path or a name resolvable via PATH.
`Args` is a list of binary strings.
""".
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()]) -> true.
spawn_child(Port, ChildId, Executable, Args) ->
    spawn_child(Port, ChildId, Executable, Args, #{}).

-doc """
Spawn a subprocess through the exec port with optional env/dir.

`Options` may contain:
  `env => #{binary() => binary()}` — extra environment variables
  `dir => binary()`                — working directory
""".
-spec spawn_child(port(), non_neg_integer(), binary(), [binary()], map()) -> true.
spawn_child(Port, ChildId, Executable, Args, Options) ->
    Env = maps:get(env, Options, #{}),
    Base = #{
        command => spawn,
        child_id => ChildId,
        executable => Executable,
        args => Args,
        env => Env
    },
    %% Only include dir when explicitly provided — empty string causes
    %% Command::current_dir("") to fail with ENOENT on the Rust side.
    Cmd =
        % elp:fixme W0032 maps:find with complex branch logic
        case maps:find(dir, Options) of
            {ok, Dir} when byte_size(Dir) > 0 -> Base#{dir => Dir};
            _ -> Base
        end,
    port_command(Port, term_to_binary(Cmd)).

-doc "Send a graceful kill to a subprocess (SIGTERM, then SIGKILL after 2s).".
-spec kill_child(port(), non_neg_integer()) -> true.
kill_child(Port, ChildId) ->
    Cmd = #{command => kill, child_id => ChildId},
    port_command(Port, term_to_binary(Cmd)).

-doc "Write data to a subprocess's stdin.".
-spec write_stdin(port(), non_neg_integer(), binary()) -> true.
write_stdin(Port, ChildId, Data) ->
    Cmd = #{command => write_stdin, child_id => ChildId, data => Data},
    port_command(Port, term_to_binary(Cmd)).

-doc "Close a subprocess's stdin (sends EOF).".
-spec close_stdin(port(), non_neg_integer()) -> true.
close_stdin(Port, ChildId) ->
    Cmd = #{command => close_stdin, child_id => ChildId},
    port_command(Port, term_to_binary(Cmd)).

-doc "Close the exec port (terminates all managed subprocesses).".
-spec close(port()) -> true.
close(Port) ->
    port_close(Port).

%%% Internal helpers

-doc """
Find the exec binary.
Checks BEAMTALK_EXEC_PORT_BIN env var first, then dev/release build dirs,
then PATH.
""".
find_exec_binary() ->
    case os:getenv("BEAMTALK_EXEC_PORT_BIN") of
        false ->
            find_exec_binary_dev();
        "" ->
            find_exec_binary_dev();
        EnvPath ->
            case filelib:is_regular(EnvPath) of
                true -> EnvPath;
                false -> find_exec_binary_dev()
            end
    end.

find_exec_binary_dev() ->
    ExeName =
        case os:type() of
            {win32, _} -> "beamtalk-exec.exe";
            _ -> "beamtalk-exec"
        end,
    case find_in_project(ExeName) of
        {ok, Path} ->
            Path;
        error ->
            %% Use ExeName (includes ".exe" on Windows) when searching PATH.
            case os:find_executable(ExeName) of
                false ->
                    error({exec_not_found, "beamtalk-exec binary not found"});
                Path ->
                    Path
            end
    end.

-doc """
Try to find the exec binary relative to the Beamtalk project root.

Returns `{ok, Path}` if a dev or release build is found under the project
root, or `error` if no Cargo.toml ancestor exists or neither build exists.
Catching the project-root error lets `find_exec_binary_dev/0` fall through
to a PATH lookup, enabling Beamtalk programs to run arbitrary OS executables
outside the dev tree (BT-1221).
""".
-spec find_in_project(string()) -> {ok, string()} | error.
find_in_project(ExeName) ->
    try find_project_root() of
        ProjectRoot ->
            DevPath = filename:join([ProjectRoot, "target", "debug", ExeName]),
            case filelib:is_regular(DevPath) of
                true ->
                    {ok, DevPath};
                false ->
                    ReleasePath = filename:join([ProjectRoot, "target", "release", ExeName]),
                    case filelib:is_regular(ReleasePath) of
                        true -> {ok, ReleasePath};
                        false -> error
                    end
            end
    catch
        error:{exec_not_found, _} -> error
    end.

-doc "Find the project root by walking up until Cargo.toml is found.".
find_project_root() ->
    Cwd = filename:absname(""),
    find_project_root(Cwd).

find_project_root(Dir) ->
    case filename:dirname(Dir) of
        Dir ->
            %% Reached filesystem root without finding Cargo.toml.
            error({exec_not_found, "could not locate project root (Cargo.toml not found)"});
        Parent ->
            case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
                true -> Dir;
                false -> find_project_root(Parent)
            end
    end.
