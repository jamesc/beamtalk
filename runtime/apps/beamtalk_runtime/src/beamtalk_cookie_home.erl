%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_cookie_home).

%%% **DDD Context:** Runtime Context

-moduledoc """
Undo the private boot home the CLI and the release launcher start a node
with (BT-5185).

A node must never be reachable with a cookie that appears in its command
line, and `-setcookie`, `ERL_AFLAGS` and `-args_file` all land there, where
any local user can read them with `ps`. So the launcher writes the real
cookie to `.erlang.cookie` in a private (0700) directory and starts `erl`
with that directory as its home: `HOME` on Unix, `HOMEDRIVE` + `HOMEPATH` on
Windows, the only variables `erlexec` derives `-home` from. OTP's `auth`
reads the cookie from there as the kernel boots, before distribution
accepts its first connection, so there is no window with any other cookie.

The launcher records what it changed:

* `BEAMTALK_COOKIE_HOME`: the private directory;
* `BEAMTALK_ORIG_<VAR>`: `set:<value>` or `unset`, for each home variable it
  replaced.

`restore/0` puts those variables back, removes the cookie file and the
(then empty) directory, and clears its own variables, so nothing the node
starts later sees the private home. It is the first `-eval` of every
CLI-started node and the first step of `beamtalk_runtime_app:start/2` (the
earliest Beamtalk code in a release); a second call is a no-op.
""".

-export([restore/0, home_env/0, orig_prefix/0, home_vars/0]).

-define(HOME_ENV, "BEAMTALK_COOKIE_HOME").
-define(ORIG_PREFIX, "BEAMTALK_ORIG_").

-doc "The variable naming the private home (the Rust side checks the name).".
-spec home_env() -> string().
home_env() -> ?HOME_ENV.

-doc "Prefix of the variables recording each replaced home variable.".
-spec orig_prefix() -> string().
orig_prefix() -> ?ORIG_PREFIX.

-doc "Every variable `erlexec` derives `-home` from, on any platform.".
-spec home_vars() -> [string()].
home_vars() -> ["HOME", "HOMEDRIVE", "HOMEPATH"].

-doc """
Restore the home variables and remove the private cookie home. `restored`
when there was one, else `none`.
""".
-spec restore() -> restored | none.
restore() ->
    case os:getenv(?HOME_ENV) of
        Dir when is_list(Dir), Dir =/= "" ->
            lists:foreach(fun restore_var/1, home_vars()),
            %% Only the one file we wrote, and a non-recursive rmdir: nothing
            %% else can be removed even if the variable was tampered with.
            _ = file:delete(filename:join(Dir, ".erlang.cookie")),
            _ = file:del_dir(Dir),
            true = os:unsetenv(?HOME_ENV),
            restored;
        _ ->
            none
    end.

restore_var(Var) ->
    Key = ?ORIG_PREFIX ++ Var,
    case os:getenv(Key) of
        false ->
            ok;
        "set:" ++ Value ->
            true = os:putenv(Var, Value),
            true = os:unsetenv(Key);
        _Unset ->
            true = os:unsetenv(Var),
            true = os:unsetenv(Key)
    end.
