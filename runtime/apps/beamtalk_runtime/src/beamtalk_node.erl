%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_node).

%%% **DDD Context:** Runtime Context

-moduledoc """
`Node` class implementation — the `native:` backing module for
`stdlib/src/node.bt` (ADR 0126 §2, Phase 1).

A `Node` is a **value, not a proxy**: a tagged map wrapping the node atom,

```
#{'\$beamtalk_class' => 'Node', name => 'worker@localhost'}
```

`name` must stay the map's only payload key — two `Node`s are equal iff their
names are equal, and the inherited `Value` `=`/`hash` (raw term comparison)
give exactly that. Creating one never touches the network; every effectful
operation (`connect`, `disconnect`, `ping`) routes to `net_kernel`/`erlang`
from the calling process, so there is no per-node process to crash or leak.
Membership changes are observed through `beamtalk_node_monitor`'s
`NodeUp`/`NodeDown` announcements, not through a method here (ADR 0126 §8).

## Connect policy (ADR 0126 §9 item 2)

`connect` (and `ping`, which auto-connects) to a host that is not this host
succeeds only when this node runs TLS distribution (`-proto_dist inet_tls`
or `inet6_tls`); otherwise it answers
`#beamtalk_error{kind = insecure_distribution}`. "This host" means the host
part names `localhost` or this machine's own hostname, or every address it
resolves to is a loopback address or one of this machine's interface
addresses — i.e. the connection cannot leave the box. The guard is advisory
for the language surface, not an enforcement boundary (§9 item 4): raw
`net_kernel` via FFI, and Erlang's auto-connect on a send to a remote pid,
bypass it. `connect_policy/2` is exported so the later remote spawn/lookup
selectors (ADR 0126 Phase 2) apply the same rule rather than re-deriving it.
""".

-include("beamtalk.hrl").

%% Class methods (`self delegate` calls the first keyword, colon stripped)
-export([current/0, named/1, connected/0]).

%% Instance methods
-export([
    connect/1,
    disconnect/1,
    isConnected/1,
    isCurrent/1,
    ping/1,
    printString/1
]).

%% Runtime helpers (Pid>>node, beamtalk_node_monitor, §9 policy)
-export([from_atom/1, ofPid/1, name/1, connect_policy/2, tls_distribution/0]).

-type t() :: #{'$beamtalk_class' := 'Node', name := node()}.
-export_type([t/0]).

-define(INVALID_NAME_HINT, <<"expected name@host">>).
-define(INSECURE_HINT, <<"configure TLS distribution (ADR 0091) or use a tunnel">>).

%%% ============================================================================
%%% Class Methods
%%% ============================================================================

-doc "The node this code is running on (`nonode@nohost` when not distributed).".
-spec current() -> t().
current() ->
    from_atom(node()).

-doc """
A `Node` value for `Name`. Pure: validates the `name@host` shape only (a
non-empty alphanumeric/`_`/`-` name part, exactly one `@`, and a non-empty
host part with no whitespace) and never touches the network. A malformed
name answers `Result error:` with `kind = invalid_node_name`.
""".
-spec named(atom()) -> beamtalk_result:t().
named(Name) when is_atom(Name), Name =/= nil, not is_boolean(Name) ->
    case valid_node_name(Name) of
        true ->
            beamtalk_result:from_tagged_tuple({ok, from_atom(Name)});
        false ->
            Error0 = beamtalk_error:new(invalid_node_name, 'Node', 'named:', ?INVALID_NAME_HINT),
            Error1 = beamtalk_error:with_message(
                Error0,
                iolist_to_binary([<<"Invalid node name: ">>, atom_to_binary(Name, utf8)])
            ),
            Error2 = beamtalk_error:with_details(Error1, #{name => Name}),
            beamtalk_result:from_tagged_tuple({error, Error2})
    end;
named(_) ->
    beamtalk_error:raise_type_error('Node', 'named:', <<"Argument must be a Symbol">>).

-doc """
Visible nodes this node is connected to. Hidden nodes (tooling, ADR 0126 §9
item 5) are excluded — `erlang:nodes/0` already omits them.
""".
-spec connected() -> [t()].
connected() ->
    [from_atom(N) || N <- lists:sort(nodes())].

%%% ============================================================================
%%% Instance Methods
%%% ============================================================================

-doc """
Establish a connection to this node, applying the §9 host policy first
(see the moduledoc). Answers `Result ok: self` on success. A refused
connection answers `Result error:` with `kind = insecure_distribution`; an
unreachable node (or a local node that is not itself distributed) answers
`kind = node_down`. Connecting to the current node is a no-op success.
""".
-spec connect(t()) -> beamtalk_result:t().
connect(#{'$beamtalk_class' := 'Node', name := Name} = Self) ->
    Result =
        case Name =:= node() of
            true ->
                {ok, Self};
            false ->
                case connect_policy(Name, tls_distribution()) of
                    ok -> do_connect(Self, Name);
                    {error, _} = Refused -> Refused
                end
        end,
    beamtalk_result:from_tagged_tuple(Result).

-doc """
Close the connection to this node (`erlang:disconnect_node/1`). Answers
`true` if a connection was closed, `false` otherwise (including when this
node is not distributed, or for the current node).
""".
-spec disconnect(t()) -> boolean().
disconnect(#{'$beamtalk_class' := 'Node', name := Name}) ->
    case Name =:= node() of
        true -> false;
        false -> erlang:disconnect_node(Name) =:= true
    end.

-doc """
Whether this node currently has a connection to `self` (visible or hidden).
The current node is trivially connected to itself.
""".
-spec isConnected(t()) -> boolean().
isConnected(#{'$beamtalk_class' := 'Node', name := Name}) ->
    Name =:= node() orelse lists:member(Name, nodes(connected)).

-doc "Whether `self` is the node this code is running on.".
-spec isCurrent(t()) -> boolean().
isCurrent(#{'$beamtalk_class' := 'Node', name := Name}) ->
    Name =:= node().

-doc """
`net_adm:ping/1` — `true` iff the node answered (and is now connected). The
current node always answers. Because a ping auto-connects, it applies the
same §9 host policy as `connect`: an off-host node without TLS distribution
answers `false` without touching the network.
""".
-spec ping(t()) -> boolean().
ping(#{'$beamtalk_class' := 'Node', name := Name}) ->
    case Name =:= node() of
        true ->
            true;
        false ->
            case connect_policy(Name, tls_distribution()) of
                ok -> net_adm:ping(Name) =:= pong;
                {error, _} -> false
            end
    end.

-doc "Human-readable representation: `Node(name@host)` (ADR 0126 §2).".
-spec printString(t()) -> binary().
printString(#{'$beamtalk_class' := 'Node', name := Name}) ->
    iolist_to_binary([<<"Node(">>, atom_to_binary(Name, utf8), <<")">>]).

%%% ============================================================================
%%% Runtime helpers
%%% ============================================================================

-doc "Build a `Node` value for a node atom (no validation — the atom came from the VM).".
-spec from_atom(node()) -> t().
from_atom(Name) when is_atom(Name) ->
    #{'$beamtalk_class' => 'Node', name => Name}.

-doc "The `Node` a pid lives on (`erlang:node/1`) — backs `Pid>>node`.".
-spec ofPid(pid()) -> t().
ofPid(Pid) when is_pid(Pid) ->
    from_atom(node(Pid)).

-doc "The node atom a `Node` value wraps.".
-spec name(t()) -> node().
name(#{'$beamtalk_class' := 'Node', name := Name}) ->
    Name.

-doc """
The ADR 0126 §9 item 2 connect guard, as a pure-ish decision: `ok` if a
connection to `Node` may be attempted, or `{error, #beamtalk_error{kind =
insecure_distribution}}` if `Node`'s host is not this host and `TlsEnabled`
is false. Host resolution uses `inet:getaddrs/2`; an IP-literal host
resolves without DNS. A host that does not resolve at all cannot be proven
local, so it falls under the policy too.
""".
-spec connect_policy(node(), boolean()) -> ok | {error, #beamtalk_error{}}.
connect_policy(_Node, true) ->
    ok;
connect_policy(Node, false) ->
    Host = host_part(Node),
    case is_this_host(Host) of
        true ->
            ok;
        false ->
            Error0 = beamtalk_error:new(insecure_distribution, 'Node', connect, ?INSECURE_HINT),
            Error1 = beamtalk_error:with_message(
                Error0,
                iolist_to_binary([
                    <<"Refusing to connect to off-host node ">>,
                    atom_to_binary(Node, utf8),
                    <<" without TLS distribution">>
                ])
            ),
            {error, beamtalk_error:with_details(Error1, #{node => Node, host => Host})}
    end.

-doc """
Whether this node runs TLS distribution: `-proto_dist inet_tls` (or
`inet6_tls`) on the command line / in `vm.args`.
""".
-spec tls_distribution() -> boolean().
tls_distribution() ->
    case init:get_argument(proto_dist) of
        {ok, Values} -> lists:any(fun(V) -> lists:suffix("_tls", V) end, lists:append(Values));
        error -> false
    end.

%%% ============================================================================
%%% Internal
%%% ============================================================================

-spec do_connect(t(), node()) -> {ok, t()} | {error, #beamtalk_error{}}.
do_connect(Self, Name) ->
    case net_kernel:connect_node(Name) of
        true ->
            {ok, Self};
        false ->
            {error,
                node_down_error(
                    Name,
                    <<"The node did not answer; check that it is running and shares this cookie">>
                )};
        ignored ->
            {error,
                node_down_error(
                    Name,
                    <<"This node is not distributed; start it with a node name (--sname/--name)">>
                )}
    end.

-spec node_down_error(node(), binary()) -> #beamtalk_error{}.
node_down_error(Name, Hint) ->
    Error0 = beamtalk_error:new(node_down, 'Node', connect, Hint),
    Error1 = beamtalk_error:with_message(
        Error0, iolist_to_binary([<<"Could not connect to node ">>, atom_to_binary(Name, utf8)])
    ),
    beamtalk_error:with_details(Error1, #{node => Name}).

-spec valid_node_name(atom()) -> boolean().
valid_node_name(Name) ->
    case string:split(atom_to_list(Name), "@", all) of
        [Local, Host] when Local =/= [], Host =/= [] ->
            lists:all(fun is_name_char/1, Local) andalso
                not lists:any(fun is_space/1, Host);
        _ ->
            false
    end.

is_name_char(C) when C >= $a, C =< $z -> true;
is_name_char(C) when C >= $A, C =< $Z -> true;
is_name_char(C) when C >= $0, C =< $9 -> true;
is_name_char($_) -> true;
is_name_char($-) -> true;
is_name_char(_) -> false.

is_space(C) -> C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r.

-spec host_part(node()) -> string().
host_part(Node) ->
    case string:split(atom_to_list(Node), "@") of
        [_Local, Host] -> Host;
        _ -> ""
    end.

-spec is_this_host(string()) -> boolean().
is_this_host("") ->
    false;
is_this_host(Host) ->
    Lower = string:lowercase(Host),
    lists:member(Lower, own_host_names()) orelse resolves_locally(Host).

-spec own_host_names() -> [string()].
own_host_names() ->
    Hostname =
        case inet:gethostname() of
            {ok, H} -> [string:lowercase(H)];
            _ -> []
        end,
    %% `nonode@nohost`'s "nohost" is not a real host name — only count this
    %% node's own host part once distribution is actually up.
    OwnNodeHost =
        case is_alive() of
            true -> [string:lowercase(host_part(node()))];
            false -> []
        end,
    ["localhost" | Hostname ++ OwnNodeHost].

-spec resolves_locally(string()) -> boolean().
resolves_locally(Host) ->
    Addrs = resolve(Host, inet) ++ resolve(Host, inet6),
    Addrs =/= [] andalso lists:all(fun is_local_addr/1, Addrs).

-spec resolve(string(), inet | inet6) -> [inet:ip_address()].
resolve(Host, Family) ->
    case inet:getaddrs(Host, Family) of
        {ok, Addrs} -> Addrs;
        {error, _} -> []
    end.

-spec is_local_addr(inet:ip_address()) -> boolean().
is_local_addr({127, _, _, _}) -> true;
is_local_addr({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_local_addr(Addr) -> lists:member(Addr, interface_addrs()).

-spec interface_addrs() -> [inet:ip_address()].
interface_addrs() ->
    case inet:getifaddrs() of
        {ok, Ifs} -> [A || {_Name, Opts} <- Ifs, {addr, A} <- Opts];
        {error, _} -> []
    end.
