%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_tls).

%%% **DDD Context:** Runtime Context

-moduledoc """
Whether Erlang distribution runs over TLS *with peer verification* (ADR 0126
§9 item 2 and its amendment, decision D2; BT-5233).

`-proto_dist inet_tls` alone encrypts the link but authenticates nothing: with
OTP's defaults a node accepts any client and trusts any server certificate.
Distribution counts as TLS here only when every protocol in the first
`-proto_dist` is a TLS one (`net_kernel:proto_dist_argument/0` uses the first
and only warns about the rest, on every OTP major this project supports;
the release launcher also refuses a second one), and:

* the server side sets `{verify, verify_peer}`;
* the server side sets `{fail_if_no_peer_cert, true}`, so a client with no
  certificate is refused rather than let in unverified;
* the client side sets `{verify, verify_peer}`;
* neither side sets `verify_fun` or `partial_chain`: either can accept a peer
  the CA did not sign, so a node that wants them is outside what this check
  can vouch for.

The settings come from where OTP's `inet_tls_dist` reads them: the
`-ssl_dist_optfile` file (`[{server, Opts}, {client, Opts}]`) and the
`-ssl_dist_opt server_<opt> <value> client_<opt> <value>` flags. OTP uses both
sources, so each required setting must appear in at least one of them, and
every place that sets it must set the safe value; a conflicting value anywhere
fails closed.

`enabled/0` is the check `beamtalk_node:tls_distribution/0` makes before an
off-host connect. `check_optfile/2` is the release launcher's refusal to start
with `RELEASE_DISTRIBUTION=tls` (`beamtalk_release_launcher:tls_preflight/2`).
Both use `peer_verification/2`.
""".

-export([
    enabled/0,
    verified/3,
    peer_verification/2,
    read_optfile/1,
    check_optfile/2,
    format_error/1
]).

-type side() :: server | client.
-type problem() ::
    server_verify_peer
    | server_fail_if_no_peer_cert
    | client_verify_peer
    | {forbidden, side(), verify_fun | partial_chain}.
-type reason() ::
    {optfile_missing, file:filename_all()}
    | {optfile_unreadable, file:filename_all(), term()}
    | {optfile_malformed, file:filename_all(), term()}
    | {peer_verification, [problem(), ...]}
    | ssl_unavailable.
-export_type([reason/0]).

%% Each required setting: its name, the side, the ssl option and the value.
-define(REQUIRED, [
    {server_verify_peer, server, verify, verify_peer},
    {server_fail_if_no_peer_cert, server, fail_if_no_peer_cert, true},
    {client_verify_peer, client, verify, verify_peer}
]).

%% Options that can accept a peer the CA did not sign.
-define(FORBIDDEN, [verify_fun, partial_chain]).

-doc """
Whether this node runs TLS distribution with peer verification (see the
module doc). The optfile's options are read from the table OTP loaded them
into at boot (`ssl_dist_opts`) when distribution is up, else from the file
itself; a file that cannot be read counts as setting nothing.
""".
-spec enabled() -> boolean().
enabled() ->
    Protos =
        case init:get_argument(proto_dist) of
            {ok, [First | _]} -> First;
            _ -> []
        end,
    verified(Protos, arg_words(ssl_dist_opt), optfile_opts()).

-doc """
The decision `enabled/0` makes, from explicit inputs: the protocols of the
`-proto_dist` OTP uses, the `-ssl_dist_opt` words and the optfile's options.
""".
-spec verified([string()], [string()], [{side(), list()}]) -> boolean().
verified(ProtoDists, DistOptWords, FileOpts) ->
    ProtoDists =/= [] andalso
        lists:all(fun(P) -> lists:suffix("_tls", P) end, ProtoDists) andalso
        peer_verification(DistOptWords, FileOpts) =:= ok.

-doc """
`ok` if the `-ssl_dist_opt` words and the optfile's options together set all
three peer-verification settings and neither side sets an option that can
override them; else every problem found.
""".
-spec peer_verification([string()], [{side(), list()}]) ->
    ok | {error, {peer_verification, [problem(), ...]}}.
peer_verification(DistOptWords, FileOpts) ->
    Pairs = word_pairs(DistOptWords),
    Missing = [
        Name
     || {Name, Side, Key, Want} <- ?REQUIRED, not settled(Side, Key, Want, Pairs, FileOpts)
    ],
    Forbidden = [
        {forbidden, Side, Key}
     || Side <- [server, client], Key <- ?FORBIDDEN, sets(Side, Key, Pairs, FileOpts)
    ],
    case Missing ++ Forbidden of
        [] -> ok;
        Problems -> {error, {peer_verification, Problems}}
    end.

-doc """
Read an `-ssl_dist_optfile` exactly as the node will: with OTP's own
`ssl_dist_sup:consult/1` (one Erlang expression, normally `[{server, Opts},
{client, Opts}].`, with no free variables), which must evaluate to a list of
`{server | client, Options}` entries, each side at most once. OTP stores the
entries in a set keyed by side, so a repeated side silently keeps only the
last list; it would let a complete list pass this check while the node uses a
weaker one, so it is refused.
""".
-spec read_optfile(file:filename_all()) -> {ok, [{side(), list()}]} | {error, reason()}.
read_optfile(Path) ->
    case {file:read_file_info(Path), code:ensure_loaded(ssl_dist_sup)} of
        {{ok, _}, {error, _}} ->
            {error, ssl_unavailable};
        {{ok, _}, {module, _}} ->
            try ssl_dist_sup:consult(Path) of
                Opts when is_list(Opts) ->
                    case sides_problem(Opts) of
                        none -> {ok, Opts};
                        Problem -> {error, {optfile_malformed, Path, Problem}}
                    end;
                Other ->
                    {error, {optfile_malformed, Path, {not_a_list, Other}}}
            catch
                error:{bad_ssl_dist_optfile, Detail} ->
                    {error, {optfile_malformed, Path, Detail}};
                Class:Detail ->
                    {error, {optfile_malformed, Path, {Class, Detail}}}
            end;
        {{error, enoent}, _} ->
            {error, {optfile_missing, Path}};
        {{error, Posix}, _} ->
            {error, {optfile_unreadable, Path, Posix}}
    end.

-doc """
`ok` if the optfile at `Path` exists, parses, and passes
`peer_verification/2` both by itself and together with the `-ssl_dist_opt`
words the node will also get. The words can only fail it, never complete it:
the launcher's client nodes (`stop`, `ping`, `rpc`, `remote_console`) start
without `vm.args`, so the file alone must make them verify their peer.
""".
-spec check_optfile(file:filename_all(), [string()]) -> ok | {error, reason()}.
check_optfile(Path, DistOptWords) ->
    case read_optfile(Path) of
        {ok, Opts} ->
            case peer_verification([], Opts) of
                ok -> peer_verification(DistOptWords, Opts);
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

-doc "An operator-facing explanation of a `reason()`, with the fix.".
-spec format_error(reason()) -> unicode:chardata().
format_error({optfile_missing, Path}) ->
    io_lib:format(
        "the TLS distribution option file '~ts' does not exist; "
        "set RELEASE_SSL_DIST_OPTFILE to the file that names this node's certificate, "
        "key and CA (see docs/development/deploying.md, \"Clustering across hosts\")",
        [Path]
    );
format_error({optfile_unreadable, Path, Posix}) ->
    io_lib:format("cannot read the TLS distribution option file '~ts': ~ts", [
        Path, file:format_error(Posix)
    ]);
format_error({optfile_malformed, Path, Detail}) ->
    io_lib:format(
        "the TLS distribution option file '~ts' is not one Erlang term "
        "[{server, [...]}, {client, [...]}]. (~0tp)",
        [Path, Detail]
    );
format_error(ssl_unavailable) ->
    "this release does not include the ssl application, which TLS distribution needs; "
    "add ssl to the release (for example as a dependency of the project's application)";
format_error({peer_verification, Problems}) ->
    [
        "the TLS distribution options (the option file and any -ssl_dist_opt flags) "
        "do not make both sides verify their peer: ",
        lists:join("; ", [problem_fix(P) || P <- Problems])
    ].

%%% ============================================================================
%%% Internal
%%% ============================================================================

%% `none`, or the first problem: an entry that is not `{server | client,
%% list()}`, a repeated side, a side whose options are an improper list
%% (`{improper_options, Side}`), or the tail of an improper outer list.
sides_problem(Opts) ->
    sides_problem(Opts, []).

sides_problem([], _Seen) ->
    none;
sides_problem([{Side, List} | Rest], Seen) when
    (Side =:= server orelse Side =:= client) andalso is_list(List)
->
    case {lists:member(Side, Seen), is_proper_list(List)} of
        {true, _} -> {duplicate_side, Side};
        {false, false} -> {improper_options, Side};
        {false, true} -> sides_problem(Rest, [Side | Seen])
    end;
sides_problem([Other | _], _Seen) ->
    {not_a_side, Other};
sides_problem(Tail, _Seen) ->
    %% The tail of an improper list.
    {not_a_list, Tail}.

is_proper_list([_ | Tail]) -> is_proper_list(Tail);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

problem_fix(server_verify_peer) ->
    "add {verify, verify_peer} to the server list";
problem_fix(server_fail_if_no_peer_cert) ->
    "add {fail_if_no_peer_cert, true} to the server list";
problem_fix(client_verify_peer) ->
    "add {verify, verify_peer} to the client list";
problem_fix({forbidden, Side, Key}) ->
    io_lib:format("remove ~ts from the ~ts list (it can accept an unverified peer)", [
        Key, Side
    ]).

%% Every place that sets `Key` on `Side` must set `Want`, and at least one must.
settled(Side, Key, Want, Pairs, FileOpts) ->
    FromWords = [V =:= atom_to_list(Want) || V <- word_values(Side, Key, Pairs)],
    FromFile = [V =:= Want || V <- file_values(Side, Key, FileOpts)],
    case FromWords ++ FromFile of
        [] -> false;
        Seen -> lists:all(fun(Ok) -> Ok end, Seen)
    end.

sets(Side, Key, Pairs, FileOpts) ->
    word_values(Side, Key, Pairs) =/= [] orelse file_values(Side, Key, FileOpts) =/= [].

word_values(Side, Key, Pairs) ->
    Flag = atom_to_list(Side) ++ "_" ++ atom_to_list(Key),
    [V || {F, V} <- Pairs, F =:= Flag].

file_values(Side, Key, FileOpts) ->
    [V || {S, Opts} <- FileOpts, S =:= Side, is_list(Opts), {K, V} <- Opts, K =:= Key].

word_pairs([Flag, Value | Rest]) -> [{Flag, Value} | word_pairs(Rest)];
word_pairs(_) -> [].

arg_words(Flag) ->
    case init:get_argument(Flag) of
        {ok, Values} -> lists:append(Values);
        error -> []
    end.

optfile_opts() ->
    try
        ets:tab2list(ssl_dist_opts)
    catch
        error:badarg ->
            case init:get_argument(ssl_dist_optfile) of
                {ok, [[File]]} ->
                    case read_optfile(File) of
                        {ok, Opts} -> Opts;
                        {error, _} -> []
                    end;
                _ ->
                    []
            end
    end.
