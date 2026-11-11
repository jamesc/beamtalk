#!/usr/bin/env escript
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% Build a minimal, unsigned-in-spirit hex repository on disk for offline
%% rebar3 tests (BT-4582). It reuses the `r3_hex_*` modules bundled in the
%% rebar3 escript, so no network and no extra dependency is needed.
%%
%%   make_hex_repo.escript <rebar3-escript> <repo-dir> <name@version>...
%%
%% Every release is a rebar3 app `name` whose `version/0` returns the version
%% string. The whole repo (names, versions, package indexes, tarballs) is
%% regenerated on each call, so "publishing" a newer version is a re-run with
%% one more release. Prints one `name version inner_hex outer_hex` line per
%% release.
-mode(compile).
-include_lib("stdlib/include/zip.hrl").

main([Rebar3, RepoDir | Releases]) ->
    load_hex_core(Rebar3),
    Key = public_key:generate_key({rsa, 2048, 65537}),
    Parsed = [parse_release(R) || R <- Releases],
    ok = filelib:ensure_dir(filename:join([RepoDir, "tarballs", "x"])),
    ok = filelib:ensure_dir(filename:join([RepoDir, "packages", "x"])),
    Built = [build_tarball(RepoDir, P) || P <- Parsed],
    Names = lists:usort([N || {N, _} <- Parsed]),
    write(RepoDir, "names",
          signed(r3_hex_registry:encode_names(
                   #{repository => <<"hexpm">>,
                     packages => [#{name => N} || N <- Names]}), Key)),
    write(RepoDir, "versions",
          signed(r3_hex_registry:encode_versions(
                   #{repository => <<"hexpm">>,
                     packages => [#{name => N,
                                    versions => [V || {N2, V, _, _} <- Built, N2 =:= N],
                                    retired => []}
                                  || N <- Names]}), Key)),
    [write(RepoDir, filename:join("packages", binary_to_list(N)),
           signed(r3_hex_registry:encode_package(
                    #{repository => <<"hexpm">>, name => N,
                      releases => [#{version => V, inner_checksum => Inner,
                                     outer_checksum => Outer, dependencies => []}
                                   || {N2, V, Inner, Outer} <- Built, N2 =:= N]}), Key))
     || N <- Names],
    [io:format("~s ~s ~s ~s~n", [N, V, hex(I), hex(O)]) || {N, V, I, O} <- Built].

load_hex_core(Rebar3) ->
    {ok, Bin} = file:read_file(Rebar3),
    %% The escript has a shebang header; find the zip archive after it.
    {ok, Files} = zip:unzip(binary:part(Bin, zip_start(Bin), byte_size(Bin) - zip_start(Bin)),
                            [memory, {file_filter, fun(#zip_file{name = N}) ->
                                string:find(N, "r3_hex_") =/= nomatch andalso
                                    filename:extension(N) =:= ".beam" end}]),
    [begin
         Mod = list_to_atom(filename:basename(N, ".beam")),
         {module, Mod} = code:load_binary(Mod, N, B)
     end || {N, B} <- Files],
    ok.

zip_start(Bin) ->
    case binary:match(Bin, <<"PK\x03\x04">>) of
        {Pos, _} -> Pos;
        nomatch -> error(no_zip_archive)
    end.

parse_release(R) ->
    [N, V] = string:split(R, "@"),
    {list_to_binary(N), list_to_binary(V)}.

build_tarball(RepoDir, {Name, Vsn}) ->
    N = binary_to_list(Name),
    Files = [{"src/" ++ N ++ ".erl",
              iolist_to_binary(io_lib:format(
                "-module(~s).~n-export([version/0]).~nversion() -> \"~s\".~n", [N, Vsn]))},
             {"src/" ++ N ++ ".app.src",
              iolist_to_binary(io_lib:format(
                "{application, ~s, [{description, \"fixture\"}, {vsn, \"~s\"}, {applications, [kernel, stdlib]}]}.~n",
                [N, Vsn]))},
             {"rebar.config", <<"{deps, []}.\n">>}],
    Meta = #{<<"name">> => Name, <<"version">> => Vsn,
             <<"app">> => Name, <<"build_tools">> => [<<"rebar3">>],
             <<"requirements">> => [], <<"licenses">> => [<<"Apache-2.0">>],
             <<"description">> => <<"BT-4582 fixture">>},
    {ok, #{tarball := Tar, inner_checksum := Inner, outer_checksum := Outer}} =
        r3_hex_tarball:create(Meta, Files),
    ok = file:write_file(filename:join([RepoDir, "tarballs", N ++ "-" ++ binary_to_list(Vsn) ++ ".tar"]), Tar),
    {Name, Vsn, Inner, Outer}.

signed(Protobuf, Key) -> zlib:gzip(r3_hex_registry:sign_protobuf(Protobuf, Key)).

write(RepoDir, Rel, Data) -> ok = file:write_file(filename:join(RepoDir, Rel), Data).

hex(Bin) -> string:lowercase(binary:encode_hex(Bin)).
