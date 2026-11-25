%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_handler).

%%% **DDD Context:** REPL Session Context

-moduledoc """
WebSocket handler for Beamtalk REPL protocol (ADR 0020).

Handles WebSocket connections from CLI, browser, and MCP clients.
Implements cookie-based authentication on first message before
accepting any protocol operations.
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include("beamtalk_ws_state.hrl").

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-export([check_upgrade_headers/3]).
%% BT-5188: the pre-auth identity proof, shared with the Rust clients.
-export([identity_proof/2]).

-ifdef(TEST).
-export([actor_snapshot_frames/0]).
-endif.

%% BT-5066: an unauthenticated connection is dropped after this long, and may
%% only send a frame this big (an `auth` message is a few hundred bytes).
-define(AUTH_DEADLINE_MS, 10000).
-define(PRE_AUTH_MAX_FRAME, 4096).
-define(POST_AUTH_MAX_FRAME, 1048576).

-doc """
HTTP upgrade to WebSocket.

Rejects the upgrade with 403 when the `Origin` or `Host` header is not
loopback (ADR 0020 DNS-rebinding mitigation); see `check_upgrade_headers/3`.
""".
init(Req, Opts) ->
    BindAddr =
        case Opts of
            #{bind_addr := B} -> B;
            _ -> {127, 0, 0, 1}
        end,
    Origin = cowboy_req:header(<<"origin">>, Req, undefined),
    Host = cowboy_req:header(<<"host">>, Req, undefined),
    Peer = cowboy_req:peer(Req),
    case check_upgrade_headers(Origin, Host, BindAddr) of
        ok ->
            upgrade(Req, Peer);
        {error, Reason} ->
            ?LOG_WARNING("WebSocket upgrade rejected", #{
                reason => Reason, peer => Peer, domain => [beamtalk, runtime]
            }),
            {ok, cowboy_req:reply(403, #{}, <<"Forbidden">>, Req), []}
    end.

upgrade(Req, Peer) ->
    {cowboy_websocket, Req,
        #ws_state{
            authenticated = false,
            peer = Peer,
            pending_eval = undefined,
            io_capture_pid = undefined,
            stdin_ref = undefined,
            log_subscribed = false
        },
        #{idle_timeout => ?AUTH_DEADLINE_MS, max_frame_size => ?PRE_AUTH_MAX_FRAME}}.

-doc """
Validate the `Origin` and `Host` upgrade headers (ADR 0020).

* `Origin`, when present, must name a loopback host. Non-browser clients (CLI,
  MCP, LSP, VS Code's Node client) send none and are unaffected; a web page
  in the user's browser always sends one.
* `Host` must be a loopback name, the address the listener is bound to, or —
  for a wildcard bind — any IP literal. A DNS-rebinding page reaches the port
  under an attacker-controlled *name*, which this refuses.
* Operators who reach the port by name (a reverse proxy forwarding `Host`, a
  Tailscale MagicDNS name) list those names in `BEAMTALK_WS_ALLOWED_HOSTS`
  (comma-separated hostnames, no ports) or the `beamtalk_workspace` app env
  `ws_allowed_hosts`. A listed name is accepted for both `Host` and `Origin`
  (BT-5127). Cookie auth still applies; the list only widens which *names*
  pass the rebinding check.
""".
-spec check_upgrade_headers(binary() | undefined, binary() | undefined, inet:ip_address()) ->
    ok | {error, bad_origin | bad_host}.
check_upgrade_headers(Origin, Host, BindAddr) ->
    case origin_ok(Origin) of
        false ->
            {error, bad_origin};
        true ->
            case host_ok(Host, BindAddr) of
                true -> ok;
                false -> {error, bad_host}
            end
    end.

origin_ok(undefined) ->
    true;
origin_ok(Origin) ->
    case uri_string:parse(Origin) of
        #{host := H} -> is_loopback_host(H) orelse is_allowed_name(H);
        _ -> false
    end.

host_ok(undefined, _BindAddr) ->
    %% HTTP/1.0-style upgrade without Host: nothing to rebind.
    true;
host_ok(HostHeader, BindAddr) ->
    case uri_string:parse(<<"//", HostHeader/binary>>) of
        #{host := H} ->
            is_loopback_host(H) orelse is_bound_host(H, BindAddr) orelse is_allowed_name(H);
        _ ->
            false
    end.

is_loopback_host(H0) ->
    H = string:lowercase(H0),
    case H of
        <<"localhost">> ->
            true;
        _ ->
            case inet:parse_address(binary_to_list(H)) of
                {ok, {127, _, _, _}} -> true;
                {ok, {0, 0, 0, 0, 0, 0, 0, 1}} -> true;
                _ -> false
            end
    end.

%% BT-5127: operator-listed hostnames (see `check_upgrade_headers/3`).
is_allowed_name(H0) ->
    H = string:lowercase(H0),
    lists:member(H, allowed_names()).

allowed_names() ->
    FromEnv =
        case os:getenv("BEAMTALK_WS_ALLOWED_HOSTS") of
            false -> [];
            Str -> string:tokens(Str, ", ")
        end,
    FromApp = application:get_env(beamtalk_workspace, ws_allowed_hosts, []),
    [Name || Raw <- FromEnv ++ FromApp, {ok, Name} <- [allowed_name(Raw)]].

%% BT-5185: one name as a lowercase UTF-8 binary, or `skip`. An entry that is
%% not valid Unicode (or not a string at all) is ignored, not a crash on every
%% upgrade: `list_to_binary/1` raised on any character above 255. A non-ASCII
%% name can never equal a `Host` header (those are ASCII/IDNA), so skipping it
%% loses nothing.
allowed_name(Raw) when is_list(Raw); is_binary(Raw) ->
    try unicode:characters_to_binary(Raw) of
        Bin when is_binary(Bin), Bin =/= <<>> -> {ok, string:lowercase(Bin)};
        _ -> skip
    catch
        error:badarg -> skip
    end;
allowed_name(_) ->
    skip.

is_bound_host(H, BindAddr) ->
    case inet:parse_address(binary_to_list(H)) of
        {ok, Addr} ->
            Addr =:= BindAddr orelse BindAddr =:= {0, 0, 0, 0} orelse
                BindAddr =:= {0, 0, 0, 0, 0, 0, 0, 0};
        _ ->
            false
    end.

-doc """
WebSocket connection initialized — send auth challenge.
Session is NOT created yet; deferred until after successful auth (security).
""".
websocket_init(State) ->
    %% BT-5127: count this connection (ranch stops counting it at upgrade). This
    %% must run here, not in `init/2`: `init/2` runs in cowboy's short-lived
    %% request process, while this and `terminate/3` run in the process that
    %% owns the socket. Over the cap: close with 1013 (try again later).
    case beamtalk_ws_conn_cap:acquire() of
        ok ->
            websocket_init_accepted(State#ws_state{
                cap_owner_mon = beamtalk_ws_conn_cap:monitor_owner()
            });
        {error, at_capacity} ->
            ?LOG_WARNING("WebSocket refused: connection cap reached", #{
                peer => State#ws_state.peer,
                limit => beamtalk_ws_conn_cap:limit(),
                domain => [beamtalk, runtime]
            }),
            {[{close, 1013, <<"Too many connections">>}], State}
    end.

websocket_init_accepted(State) ->
    %% Send a minimal welcome so clients know the connection is alive.
    %% Session details are sent after successful authentication.
    Welcome = iolist_to_binary(json:encode(#{<<"op">> => <<"auth-required">>})),
    %% Hard deadline (BT-5066): `idle_timeout` is an inactivity timer that any
    %% received frame (ping included) resets, so it cannot bound a stalling
    %% client on its own.
    erlang:send_after(?AUTH_DEADLINE_MS, self(), auth_deadline),
    {[{text, Welcome}], State}.

-doc """
Handle incoming WebSocket text frame.
First message must be auth; subsequent messages are protocol ops.
""".
websocket_handle({text, Data}, State = #ws_state{authenticated = false}) ->
    case handle_auth(Data, State) of
        {Cmds, NewState = #ws_state{authenticated = true}} ->
            %% BT-5185: move from the pre-auth budget to the authenticated one.
            case beamtalk_ws_conn_cap:promote() of
                ok ->
                    %% Lift the pre-auth deadline and frame cap (BT-5066).
                    Lift =
                        {set_options, #{
                            idle_timeout => infinity, max_frame_size => ?POST_AUTH_MAX_FRAME
                        }},
                    {[Lift | Cmds], NewState};
                {error, Why} ->
                    ?LOG_WARNING("WebSocket refused after auth: connection cap", #{
                        peer => State#ws_state.peer,
                        reason => Why,
                        limit => beamtalk_ws_conn_cap:limit(),
                        domain => [beamtalk, runtime]
                    }),
                    {[{close, 1013, <<"Too many connections">>}], State}
            end;
        Other ->
            Other
    end;
websocket_handle({text, Data}, State = #ws_state{authenticated = true, session_pid = SessionPid}) ->
    handle_protocol(Data, SessionPid, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

-doc """
Handle Erlang messages sent to the handler process.
Streaming stdout chunk from eval
""".
websocket_info({beamtalk_ws_conn_cap, evicted}, State = #ws_state{authenticated = false}) ->
    %% BT-5185: the pre-auth budget was full and this was the oldest
    %% unauthenticated socket; a newer one has taken its slot.
    {[{close, 1013, <<"Too many unauthenticated connections">>}], State};
websocket_info(
    {'DOWN', Ref, process, _Owner, _Reason}, State = #ws_state{cap_owner_mon = Ref}
) when Ref =/= undefined ->
    %% BT-5185: the REPL server (and its connection-cap table) restarted; this
    %% connection would live on uncounted, so close it (1012: service restart).
    {[{close, 1012, <<"Server restarting">>}], State#ws_state{cap_owner_mon = undefined}};
websocket_info(auth_deadline, State = #ws_state{authenticated = false}) ->
    ?LOG_WARNING("WebSocket closed: auth deadline", #{
        peer => State#ws_state.peer, domain => [beamtalk, runtime]
    }),
    {[{close, 1008, <<"Authentication timeout">>}], State};
websocket_info(auth_deadline, State) ->
    {ok, State};
websocket_info(
    {eval_out, Chunk},
    State = #ws_state{
        authenticated = true,
        pending_eval = Msg
    }
) when
    Msg =/= undefined
->
    Encoded = beamtalk_repl_protocol:encode_out(Chunk, Msg, <<"out">>),
    case Encoded of
        %% Legacy client — skip streaming
        <<>> -> {ok, State};
        _ -> {[{text, Encoded}], State}
    end;
%% Eval code requests stdin input
websocket_info(
    {need_input, CapturePid, Ref, Prompt},
    State = #ws_state{authenticated = true, pending_eval = Msg}
) when
    Msg =/= undefined
->
    Encoded = beamtalk_repl_protocol:encode_need_input(Prompt, Msg),
    {[{text, Encoded}], State#ws_state{
        io_capture_pid = CapturePid,
        stdin_ref = Ref
    }};
%% Late need_input after eval completed — drop silently
websocket_info({need_input, _, _, _}, State = #ws_state{pending_eval = undefined}) ->
    {ok, State};
%% Eval completed successfully
websocket_info(
    {eval_done, Value, Output, Warnings},
    State = #ws_state{authenticated = true, pending_eval = Msg}
) when
    Msg =/= undefined
->
    Response = beamtalk_repl_protocol:encode_result(
        Value,
        Msg,
        fun beamtalk_repl_json:term_to_json/1,
        Output,
        Warnings,
        fun beamtalk_repl_json:value_type/1
    ),
    finish_eval(Response, State);
%% BindingChanged system announcement (SystemAnnouncer bus), re-encoded
%% to the byte-identical `bindings`/`changed` push frame the legacy
%% `beamtalk_bindings_events` broadcast produced. The frame carries only the
%% session id (a refresh trigger); the typed event also carries name/value, which
%% browser consumers ignore.
websocket_info(
    {beamtalk_announcement, _SubRef, 'BindingChanged', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    {
        [
            {text,
                push_frame(<<"bindings">>, <<"changed">>, #{
                    <<"session">> => nullable(maps:get(sessionId, Event, nil))
                })}
        ],
        State
    };
%% Eval completed with error
websocket_info(
    {eval_error, Reason, Output, Warnings},
    State = #ws_state{authenticated = true, pending_eval = Msg}
) when
    Msg =/= undefined
->
    WrappedReason = beamtalk_repl_errors:ensure_structured_error(Reason),
    Location = extract_compile_error_location(Reason),
    %% Frames come from the raw reason: `ensure_structured_error/1` drops them.
    Metadata = maps:merge(Location, beamtalk_repl_protocol:frames_metadata(Reason)),
    Response = beamtalk_repl_json:encode_error(
        WrappedReason,
        Msg,
        Output,
        Warnings,
        Metadata
    ),
    finish_eval(Response, State);
%% Connected-session `Program exit:` under a streaming eval. Surface the
%% exit-status reply (parity with the synchronous op path) so the subscriber gets
%% a terminal message rather than waiting forever; the session shell has already
%% stopped. Rare in practice — `Program exit:` is documented for `main:` / bare
%% expressions, not streaming contexts.
websocket_info(
    {eval_script_exit, Code, Output, Warnings},
    State = #ws_state{authenticated = true, pending_eval = Msg}
) when
    Msg =/= undefined
->
    Response = beamtalk_repl_protocol:encode_script_exit(Code, Msg, Output, Warnings),
    finish_eval(Response, State);
%% Late streaming messages after eval completed — drop silently
websocket_info({eval_out, _Chunk}, State = #ws_state{pending_eval = undefined}) ->
    {ok, State};
websocket_info({eval_done, _, _, _}, State = #ws_state{pending_eval = undefined}) ->
    {ok, State};
websocket_info({eval_error, _, _, _}, State = #ws_state{pending_eval = undefined}) ->
    {ok, State};
websocket_info({eval_script_exit, _, _, _}, State = #ws_state{pending_eval = undefined}) ->
    {ok, State};
%% Transcript push — forwarded from beamtalk_transcript_stream subscriber (ADR 0017)
%% `Text` can carry invalid UTF-8 (`show:` of an `asStringUnchecked` string), on
%% which `json:encode/1` raises and the connection dies: BT-5018.
websocket_info({transcript_output, Text}, State = #ws_state{authenticated = true}) ->
    Push = iolist_to_binary(
        json:encode(#{
            <<"push">> => <<"transcript">>,
            <<"text">> => beamtalk_transcript_stream:ensure_utf8(Text)
        })
    ),
    {[{text, Push}], State};
%% Actor / class / flush system announcements (SystemAnnouncer bus),
%% re-encoded to the existing JSON push frames. The legacy bespoke channels
%% (the actor-list lifecycle channel, `beamtalk_class_events`,
%% `beamtalk_flush_events`) were retired; consumers now discriminate on the
%% announced class atom.
%%
%% ADR 0017 Phase 2: actor lifecycle. The live `spawned` frame carries
%% `{class, pid}` only — `spawned_at` is no longer on the live event (it is still
%% present on the connect snapshot, read from the actor scan). `stopped` carries the
%% normalized reason symbol (`#normal`/`#shutdown`/`#crashed`).
websocket_info(
    {beamtalk_announcement, _SubRef, 'ActorSpawned', _Handler, Event},
    State = #ws_state{authenticated = true, actor_feed = Feed}
) ->
    case beamtalk_workspace_actors:feed_event(Event, Feed) of
        {drop, Feed1} ->
            {[], State#ws_state{actor_feed = Feed1}};
        {emit, Feed1} ->
            Frame = push_frame(<<"actors">>, <<"spawned">>, encode_actor_spawned_event(Event)),
            {[{text, Frame}], State#ws_state{actor_feed = Feed1}}
    end;
websocket_info(
    {beamtalk_announcement, _SubRef, 'ActorStopped', _Handler, Event},
    State = #ws_state{authenticated = true, actor_feed = Feed}
) ->
    case beamtalk_workspace_actors:feed_event(Event, Feed) of
        {drop, Feed1} ->
            {[], State#ws_state{actor_feed = Feed1}};
        {emit, Feed1} ->
            Frame = push_frame(<<"actors">>, <<"stopped">>, encode_actor_stopped_event(Event)),
            {[{text, Frame}], State#ws_state{actor_feed = Feed1}}
    end;
%% Class-loaded push — broadcast when any session loads or reloads a class.
websocket_info(
    {beamtalk_announcement, _SubRef, 'ClassLoaded', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    {[{text, push_frame(<<"classes">>, <<"loaded">>, encode_class_event(Event))}], State};
%% Class-removed push — newly visible to push consumers now that the
%% `classes` stream rides the bus (ADR 0093). Symmetric with the `loaded` frame.
websocket_info(
    {beamtalk_announcement, _SubRef, 'ClassRemoved', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    {[{text, push_frame(<<"classes">>, <<"removed">>, encode_class_event(Event))}], State};
%% ADR 0082 Phase 3; ADR 0113 LSP follow-up; ADR 0114 LSP
%% follow-up: Flush-completion push — broadcast after a `Workspace
%% flush` writes one or more `.bt` source files. LSP clients use this to emit
%% `workspace/applyEdit` so open editor buffers refresh against the new
%% on-disk state. `files` is the list of absolute paths touched (written or
%% removed); `fileKinds` is the per-file companion — one
%% `{file, kind}` object per touched file (plus an optional `oldFile`),
%% `kind` being `beamtalk_workspace_changelog:entry_kind/1`'s own
%% wire value (`"new-class"`, `"remove-class"`, `"instance"`, `"class"`,
%% `"remove-method"`, `"rename-class"`, `"rename-method"`) — so a consumer
%% can classify `CreateFile` vs. `DeleteFile` vs. `RenameFile` (the
%% `"rename-class"` file that also carries `oldFile`) vs. a per-site
%% `TextDocumentEdit` (`"rename-method"`) vs. an ordinary patch before
%% touching the filesystem, rather than inferring it from post-flush
%% existence. Absent/empty on an older producer (defensive
%% default `[]`), so an older runtime still degrades to the
%% existence-check fallback client-side.
websocket_info(
    {beamtalk_announcement, _SubRef, 'FlushCompleted', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    Data = #{
        <<"files">> => normalise_files_for_push(maps:get(files, Event, [])),
        <<"fileKinds">> => normalise_file_kinds_for_push(maps:get(fileKinds, Event, []))
    },
    {[{text, push_frame(<<"workspace">>, <<"flush_completed">>, Data)}], State};
%% ADR 0105 Phase 1: reload-induced re-check outcome — broadcast
%% whenever `beamtalk_repl_loader_recheck:maybe_trigger_recheck/4` has something for a
%% live surface to act on (a dependent re-check ran, or a caller's stale
%% findings were cleared because its own source just changed). `checkedOwners`
%% is the clearing-by-replacement signal: every surface subscriber should
%% replace what it is showing for each listed owner with `findings` filtered
%% to that owner (possibly none — a clean re-check is exactly how a stale
%% finding disappears without anyone editing the caller).
websocket_info(
    {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    {
        [{text, push_frame(<<"reload_check">>, <<"completed">>, encode_reload_check_event(Event))}],
        State
    };
%% ADR 0123 §4 (BT-3538): shape-reload findings (dropped-without-bump,
%% version bumped/decreased, migration outcome) — a **distinct** event
%% (`shape_findings`, not `completed`) on the *same* `reload_check` channel
%% (`beamtalk_repl_subscriptions:announcement_classes(reload_check)`), so it
%% reaches every existing `reload_check` subscriber without a new
%% subscription, but does not match the CLI/LSP's existing
%% `(reload_check, completed)` pattern — see
%% `beamtalk_repl_loader_recheck:publish_reload_findings/2`'s doc for why this is a
%% new event rather than folded into `'ReloadCheckCompleted'`'s existing
%% payload shape (a different finding schema: `class`/`kind`/`fields`/
%% `migrated`/`suspended`/`pids`, not `owner`/`selector`/`sites`).
websocket_info(
    {beamtalk_announcement, _SubRef, 'ShapeReloadFindingsCompleted', _Handler, Event},
    State = #ws_state{authenticated = true}
) ->
    {
        [
            {text,
                push_frame(
                    <<"reload_check">>,
                    <<"shape_findings">>,
                    encode_shape_reload_findings_event(Event)
                )}
        ],
        State
    };
%% Log event push from beamtalk_ws_log_handler
websocket_info(
    {log_event, EventData}, State = #ws_state{authenticated = true, log_subscribed = true}
) ->
    {[{text, push_frame(<<"logs">>, <<"entry">>, encode_log_event(EventData))}], State};
websocket_info({log_event, _EventData}, State) ->
    %% Not subscribed or not authenticated — drop silently
    {ok, State};
%% Session process ended — clean up ETS entry and close WebSocket. A `normal`
%% exit is a deliberate stop (the client's `close` op, idle shutdown): close
%% with 1000. Anything else is an unexpected session death: 1011.
websocket_info(
    {'DOWN', MonRef, process, _Pid, Reason},
    State = #ws_state{session_mon = MonRef, session_id = SessionId}
) ->
    ?LOG_INFO("Session process terminated, closing WebSocket", #{
        session => SessionId, domain => [beamtalk, runtime]
    }),
    beamtalk_session_table:delete(SessionId),
    {[{close, session_close_code(Reason), <<"Session terminated">>}], State#ws_state{
        authenticated = false,
        session_id = undefined,
        session_pid = undefined,
        session_mon = undefined
    }};
websocket_info(_Info, State) ->
    {ok, State}.

-doc """
WebSocket close code for a session process exit: 1000 if deliberate, else 1011.
`shutdown` is what the supervisor sends for the `close` op (BT-5084), so it is
deliberate too.
""".
-spec session_close_code(term()) -> 1000 | 1011.
session_close_code(normal) -> 1000;
session_close_code(shutdown) -> 1000;
session_close_code(_Unexpected) -> 1011.

-doc "Connection terminated — clean up or keep session for resume.".
terminate(_Reason, _Req, #ws_state{session_id = undefined}) ->
    beamtalk_ws_conn_cap:release();
terminate(_Reason, _Req, #ws_state{session_id = SessionId, session_pid = SessionPid, peer = Peer}) ->
    %% BT-5127: give the connection-cap slot back.
    beamtalk_ws_conn_cap:release(),
    ?LOG_INFO("WebSocket client disconnected", #{
        session => SessionId,
        session_pid => SessionPid,
        peer => Peer,
        domain => [beamtalk, runtime]
    }),
    %% Unsubscribe from all live push streams (Transcript, actors,
    %% classes, bindings, flush) via the stable facade.
    beamtalk_repl_subscriptions:unsubscribe_all(),
    %% Unsubscribe from log streaming — a separate diagnostic stream,
    %% not part of the workspace push-stream facade.
    beamtalk_ws_log_handler:unsubscribe(),
    %% Keep the session alive so the client can resume it, but don't stop it or
    %% delete its table row here. The shell monitors this connection process
    %% (`beamtalk_repl_shell:attach/2`), so it notices the disconnect itself: it
    %% stays resumable for `beamtalk_repl_shell:session_idle_seconds/0`, then
    %% stops and removes its own row (BT-5012). The idle monitor ignores such
    %% detached sessions when deciding whether the workspace is idle.
    ok.

%%% Internal — Authentication

-doc """
Validate the first WebSocket message as a cookie auth handshake.
Expected: {"type":"auth","cookie":"<workspace cookie>"}
""".
handle_auth(Data, State) ->
    case decode_auth_frame(Data) of
        {ok, Decoded} -> handle_auth_message(Decoded, State);
        error -> auth_malformed(State)
    end.

-doc """
Decode the first, unauthenticated frame.

Any decode failure counts as malformed. OTP's `json:decode/1` raises
`unexpected_end`, `{invalid_byte, _}`, `{invalid_unicode, _}` and, for an
unpaired surrogate escape such as `\\ud800\\u0041`, `{unexpected_sequence, _}`;
enumerating them let the next new reason crash the handler for an
unauthenticated peer. Nothing about the failure reaches the peer.
""".
-spec decode_auth_frame(binary()) -> {ok, term()} | error.
decode_auth_frame(Data) ->
    try
        {ok, json:decode(Data)}
    catch
        error:_ -> error
    end.

-doc """
The identity proof for `Challenge` (BT-5188): lowercase hex SHA-256 of
`beamtalk-workspace-identity`, the port-file `Nonce` and `Challenge`,
newline-separated. Must match `beamtalk_repl_protocol::handshake::identity_proof`
in Rust; both are pinned to the `identity_reply` vector in
`ws_auth_handshake_wire_corpus.json`.
""".
-spec identity_proof(binary(), binary()) -> binary().
identity_proof(Nonce, Challenge) ->
    Digest = crypto:hash(
        sha256, <<"beamtalk-workspace-identity\n", Nonce/binary, "\n", Challenge/binary>>
    ),
    binary:encode_hex(Digest, lowercase).

%% The nonce this node wrote to its port file, or `error` while the REPL
%% server is not running (the identify request then fails like a malformed
%% one, and the client does not send its cookie).
-spec current_nonce() -> {ok, binary()} | error.
current_nonce() ->
    try beamtalk_repl_server:get_nonce() of
        {ok, Nonce} -> {ok, Nonce}
    catch
        exit:_ -> error
    end.

-doc "Reject an auth frame that did not parse: one generic reply, then close.".
-spec auth_malformed(#ws_state{}) -> {list(), #ws_state{}}.
auth_malformed(State) ->
    ?LOG_WARNING("WebSocket auth failed: malformed JSON", #{
        peer => State#ws_state.peer, domain => [beamtalk, runtime]
    }),
    Reply = iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"auth_error">>,
            <<"message">> => <<"Malformed auth message">>
        })
    ),
    {[{text, Reply}, {close, 1008, <<"Authentication required">>}], State}.

-spec handle_auth_message(term(), #ws_state{}) -> {list(), #ws_state{}}.
handle_auth_message(
    #{<<"type">> := <<"identify">>, <<"challenge">> := Challenge}, State
) when
    is_binary(Challenge), byte_size(Challenge) >= 16, byte_size(Challenge) =< 256
->
    %% BT-5188: prove, before any cookie is sent, that this node is the
    %% workspace whose port file names this port. Only the node that wrote
    %% the port file knows its nonce; the client's fresh challenge makes an
    %% observed proof useless for a later connection. The connection stays
    %% unauthenticated (the pre-auth deadline and frame cap still apply).
    case current_nonce() of
        {ok, Nonce} ->
            Reply = iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"identity">>,
                    <<"proof">> => identity_proof(Nonce, Challenge)
                })
            ),
            {[{text, Reply}], State};
        error ->
            ?LOG_WARNING("WebSocket identify failed: REPL server nonce unavailable", #{
                peer => State#ws_state.peer, domain => [beamtalk, runtime]
            }),
            Reply = iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"auth_error">>,
                    <<"message">> => <<"Workspace identity unavailable">>
                })
            ),
            {[{text, Reply}, {close, 1011, <<"Identity unavailable">>}], State}
    end;
handle_auth_message(Decoded, State) ->
    case Decoded of
        #{<<"type">> := <<"auth">>, <<"cookie">> := ProvidedCookie} = AuthMsg when
            is_binary(ProvidedCookie)
        ->
            %% Timing-safe comparison; never accepts `nocookie` (BT-5066).
            CookieValid = beamtalk_ws_cookie:valid(ProvidedCookie),
            case CookieValid of
                true ->
                    %% Version lock (BT-4327): the client must have been built
                    %% from the same checkout as this node — same
                    %% `BEAMTALK_VERSION` (exact, including any `-dev+<sha>`
                    %% suffix) and same `?PROTOCOL_VERSION`. A stale binary
                    %% attaching to a newer node (or vice versa) previously
                    %% failed in confusing ways far from the cause; reject it
                    %% here, before any session exists, with a message naming
                    %% both sides. OTP is deliberately NOT part of this gate:
                    %% mixed OTP releases are governed by the support window
                    %% (`beamtalk_cli::otp_support`), not an exact match.
                    %%
                    %% The `shutdown` admin surface (`workspace stop`) is
                    %% exempt: stopping a stale node must work precisely when
                    %% its versions do not match. Shutdown loads no code, and
                    %% the resulting connection is restricted to the
                    %% `shutdown` op by `handle_protocol/3`, so the
                    %% exemption grants no compilation or eval capability.
                    %% (`handle_shutdown/2` additionally re-validates the
                    %% cookie from the op body.)
                    %%
                    %% The `vscode` surface (BT-4373) is checked on
                    %% `protocol_version` only: the extension compiles no
                    %% code (its `eval` is compiled here, by this node's own
                    %% compiler), and it ships separately on the marketplace,
                    %% so an exact toolchain match would break it on every
                    %% rebuild. See `check_client_versions/2`.
                    ClientSurface = maps:get(<<"client">>, AuthMsg, undefined),
                    ShutdownOnly = ClientSurface =:= <<"shutdown">>,
                    VersionCheck = check_client_versions(ClientSurface, AuthMsg),
                    case VersionCheck of
                        ok ->
                            %% Check for session resume request
                            ResumeId = maps:get(<<"resume">>, AuthMsg, undefined),
                            %% Origin/debug metadata for a freshly-created session: the
                            %% client surface it declared (`repl`/`mcp`/`lsp`/`shutdown`/…,
                            %% normalised downstream) plus the connecting peer. Surfaced by
                            %% `Workspace sessions` / `Session info`.
                            Meta = #{
                                kind => ClientSurface,
                                peer => format_peer(State#ws_state.peer),
                                connected_at => erlang:system_time(microsecond)
                            },
                            start_or_resume_session(
                                ResumeId, Meta, State#ws_state{shutdown_only = ShutdownOnly}
                            );
                        {error, VersionMessage} ->
                            ?LOG_WARNING("WebSocket auth failed: version mismatch", #{
                                peer => State#ws_state.peer,
                                domain => [beamtalk, runtime]
                            }),
                            VersionReply = iolist_to_binary(
                                json:encode(#{
                                    <<"type">> => <<"auth_error">>,
                                    <<"message">> => VersionMessage
                                })
                            ),
                            {[{text, VersionReply}, {close, 1008, <<"Version mismatch">>}], State}
                    end;
                false ->
                    ?LOG_WARNING("WebSocket auth failed: invalid cookie", #{
                        peer => State#ws_state.peer,
                        domain => [beamtalk, runtime]
                    }),
                    Reply = iolist_to_binary(
                        json:encode(#{
                            <<"type">> => <<"auth_error">>,
                            <<"message">> => <<"Invalid cookie">>
                        })
                    ),
                    {[{text, Reply}, {close, 1008, <<"Authentication failed">>}], State}
            end;
        _ ->
            ?LOG_WARNING("WebSocket auth failed: invalid auth message", #{
                peer => State#ws_state.peer,
                domain => [beamtalk, runtime]
            }),
            Reply = iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"auth_error">>,
                    <<"message">> => <<"First message must be auth">>
                })
            ),
            {[{text, Reply}, {close, 1008, <<"Authentication required">>}], State}
    end.

-doc """
Apply the version gate for the connecting client's declared surface.

- `shutdown` (`workspace stop`, BT-4327): exempt — it must be able to stop
  a stale node, and its connection is restricted to the `shutdown` op.
- `vscode` (the VS Code extension's workspace client, BT-4373): only
  `protocol_version` must match. The extension never compiles code — every
  `eval` it sends is compiled inside this node by this node's compiler — so
  the toolchain-identity half of the lock guards nothing for it, while an
  exact `-dev+<sha>` match would break a marketplace-installed extension on
  every rebuild.
- every other surface (CLI REPL, MCP, LSP, …): full toolchain lock via
  `check_client_versions/1`.
""".
-spec check_client_versions(term(), map()) -> ok | {error, binary()}.
check_client_versions(<<"shutdown">>, _AuthMsg) ->
    ok;
check_client_versions(<<"vscode">>, AuthMsg) ->
    check_client_protocol(AuthMsg);
check_client_versions(_ClientSurface, AuthMsg) ->
    check_client_versions(AuthMsg).

-doc """
Protocol-only gate (the `vscode` surface): `protocol_version` must equal
`?PROTOCOL_VERSION`. A missing field counts as a mismatch.
""".
-spec check_client_protocol(map()) -> ok | {error, binary()}.
check_client_protocol(AuthMsg) ->
    ClientProtocol = maps:get(<<"protocol_version">>, AuthMsg, undefined),
    case ClientProtocol =:= ?PROTOCOL_VERSION of
        true ->
            ok;
        false ->
            {error,
                beamtalk_error:chars_to_binary(
                    io_lib:format(
                        "Protocol mismatch: the VS Code extension speaks workspace "
                        "protocol ~ts, this workspace node (beamtalk ~ts) speaks "
                        "protocol ~ts. Update the Beamtalk extension or the "
                        "beamtalk toolchain so they agree, then restart the "
                        "workspace.",
                        [
                            display_vsn(ClientProtocol),
                            display_vsn(beamtalk_version:app_vsn(beamtalk_workspace)),
                            display_vsn(?PROTOCOL_VERSION)
                        ]
                    )
                )}
    end.

-doc """
Compare the connecting client's toolchain versions against this node's.

Both `beamtalk_version` (this node's `beamtalk_workspace` app `vsn`, from
`scripts/version.escript`) and `protocol_version` (`?PROTOCOL_VERSION`)
must match exactly. A missing field counts as a mismatch — it means the
client predates the version handshake (BT-4327) and must be rebuilt.
""".
-spec check_client_versions(map()) -> ok | {error, binary()}.
check_client_versions(AuthMsg) ->
    ServerBeamtalk = beamtalk_version:app_vsn(beamtalk_workspace),
    ClientBeamtalk = maps:get(<<"beamtalk_version">>, AuthMsg, undefined),
    ClientProtocol = maps:get(<<"protocol_version">>, AuthMsg, undefined),
    case ClientBeamtalk =:= ServerBeamtalk andalso ClientProtocol =:= ?PROTOCOL_VERSION of
        true ->
            ok;
        false ->
            {error,
                beamtalk_error:chars_to_binary(
                    io_lib:format(
                        "Version mismatch: client is beamtalk ~ts protocol ~ts, "
                        "this workspace node is beamtalk ~ts protocol ~ts. "
                        "Stop this node with `beamtalk workspace stop ~ts`, "
                        "then ~ts and restart the workspace.",
                        [
                            display_vsn(ClientBeamtalk),
                            display_vsn(ClientProtocol),
                            display_vsn(ServerBeamtalk),
                            display_vsn(?PROTOCOL_VERSION),
                            this_workspace_id(),
                            version_remedy(ClientBeamtalk, ServerBeamtalk)
                        ]
                    )
                )}
    end.

-doc """
How to get matching toolchains (BT-4382): `just build` is a source-checkout
remedy, suggested only when either side is a `-dev` build; between two
releases the fix is installing the same release on both sides.
""".
-spec version_remedy(term(), binary()) -> string().
version_remedy(ClientBeamtalk, ServerBeamtalk) ->
    case is_dev_vsn(ClientBeamtalk) orelse is_dev_vsn(ServerBeamtalk) of
        true -> "rebuild both from the same checkout (`just build`)";
        false -> "install the same beamtalk release for both"
    end.

-spec is_dev_vsn(term()) -> boolean().
is_dev_vsn(Vsn) when is_binary(Vsn) -> binary:match(Vsn, <<"-dev">>) =/= nomatch;
is_dev_vsn(_) -> false.

-doc """
This node's workspace id for the `workspace stop` hint, or `<id>` when the
node has no workspace metadata (`beamtalk workspace list` shows it). Best
effort: a slow or crashed metadata server only degrades the hint, never the
auth rejection itself.
""".
-spec this_workspace_id() -> binary().
this_workspace_id() ->
    try beamtalk_workspace_meta:get_metadata() of
        {ok, #{workspace_id := Id}} when is_binary(Id), Id =/= <<>> -> Id;
        _ -> <<"<id>">>
    catch
        exit:_ -> <<"<id>">>
    end.

-doc """
Render a version field for the mismatch message (`undefined` → `<missing>`,
any non-binary → `<invalid>` — `io_lib:format`'s `~s` would raise on those).
""".
-spec display_vsn(term()) -> binary().
display_vsn(undefined) -> <<"<missing>">>;
display_vsn(Vsn) when is_binary(Vsn) -> Vsn;
display_vsn(_) -> <<"<invalid>">>.

%%% Internal — Protocol Dispatch

-doc """
Dispatch a protocol message after authentication.

Connections on the version-exempt `shutdown` admin surface (BT-4327) may
only issue the `shutdown` op — anything else is rejected and the
connection closed, so the exemption grants no compilation or eval
capability to a version-mismatched client.
""".
handle_protocol(Data, SessionPid, State) ->
    case beamtalk_repl_protocol:decode(Data) of
        {ok, Msg} ->
            Op = beamtalk_repl_protocol:get_op(Msg),
            case State#ws_state.shutdown_only andalso Op =/= <<"shutdown">> of
                true ->
                    ?LOG_WARNING("Rejected op on shutdown-only session", #{
                        op => Op,
                        peer => State#ws_state.peer,
                        domain => [beamtalk, runtime]
                    }),
                    Restricted = iolist_to_binary(
                        json:encode(#{
                            <<"type">> => <<"auth_error">>,
                            <<"message">> =>
                                <<
                                    "Session is restricted to the shutdown operation "
                                    "(version-exempt admin surface)"
                                >>
                        })
                    ),
                    {[{text, Restricted}, {close, 1008, <<"Restricted session">>}], State};
                false ->
                    dispatch_protocol_op(Op, Msg, SessionPid, State)
            end;
        {error, DecodeError} ->
            ErrorJson = beamtalk_repl_json:format_error(DecodeError),
            {[{text, ErrorJson}], State}
    end.

dispatch_protocol_op(<<"shutdown">>, Msg, _SessionPid, State) ->
    handle_shutdown(Msg, State);
dispatch_protocol_op(<<"eval">>, Msg, SessionPid, State) ->
    %% Use async eval for streaming output. The async path
    %% bypasses beamtalk_repl_ops:dispatch/4, so it consults
    %% the capability check itself (ADR 0125 §1.5).
    with_capability(<<"eval">>, Msg, State, fun() ->
        handle_eval_async(Msg, SessionPid, State)
    end);
dispatch_protocol_op(<<"run-entry">>, Msg, SessionPid, State) ->
    %% Connected-mode `beamtalk run` — dispatch a class
    %% entry method with argv, streaming output like async
    %% eval. Compile-free, so available in every mode
    %% (ADR 0125 §1.5); still routed through the check so the
    %% classification stays the single source of truth.
    with_capability(<<"run-entry">>, Msg, State, fun() ->
        handle_run_entry_async(Msg, SessionPid, State)
    end);
dispatch_protocol_op(<<"stdin">>, Msg, _SessionPid, State) ->
    %% Route stdin input to IO capture process
    handle_stdin(Msg, State);
dispatch_protocol_op(<<"subscribe-logs">>, Msg, _SessionPid, State) ->
    %% Subscribe to live log streaming
    handle_subscribe_logs(Msg, State);
dispatch_protocol_op(<<"unsubscribe-logs">>, Msg, _SessionPid, State) ->
    %% Unsubscribe from log streaming
    handle_unsubscribe_logs(Msg, State);
dispatch_protocol_op(_Op, Msg, SessionPid, State) ->
    Response = beamtalk_repl_server:handle_protocol_request(Msg, SessionPid),
    {[{text, Response}], State}.

-doc """
Run `Handle` only if this node may perform `Op`
(`beamtalk_capability:check/1`, ADR 0125 §1.5); otherwise reply with the
structured refusal.
""".
with_capability(Op, Msg, State, Handle) ->
    case beamtalk_capability:check(Op) of
        ok ->
            Handle();
        {error, Err} ->
            {[{text, beamtalk_repl_json:encode_error(Err, Msg)}], State}
    end.

-doc "Start a new session or resume an existing one if session ID is provided.".
start_or_resume_session(undefined, Meta, State) ->
    %% No resume — create a new session
    SessionId = beamtalk_repl_server:generate_session_id(),
    create_session(SessionId, Meta, State);
start_or_resume_session(ResumeId, Meta, State) when is_binary(ResumeId) ->
    %% Try to resume existing session
    case beamtalk_session_table:lookup(ResumeId) of
        {ok, Pid} ->
            %% attach/2 is a call (BT-5084): `gone` means the shell stopped (idle
            %% timeout) between the lookup and now, so fall back to a new session.
            case is_process_alive(Pid) andalso beamtalk_repl_shell:attach(Pid, self()) =:= ok of
                true ->
                    MonRef = erlang:monitor(process, Pid),
                    ?LOG_INFO("WebSocket session resumed", #{
                        session => ResumeId,
                        session_pid => Pid,
                        peer => State#ws_state.peer,
                        domain => [beamtalk, runtime]
                    }),
                    beamtalk_workspace_meta:update_activity(),
                    %% Subscribe to all live push streams (Transcript,
                    %% actors, classes, bindings, flush) via the stable facade.
                    %% ADR 0082 Phase 3: the flush stream lets LSP
                    %% clients emit `workspace/applyEdit` per touched file.
                    maybe_subscribe_all(State),
                    {InitialActors, ActorFeed} = actor_snapshot_frames(),
                    AuthOk = iolist_to_binary(json:encode(#{<<"type">> => <<"auth_ok">>})),
                    SessionMsg = iolist_to_binary(
                        json:encode(#{
                            <<"op">> => <<"session-started">>,
                            <<"session">> => ResumeId
                        })
                    ),
                    {[{text, AuthOk}, {text, SessionMsg}] ++ InitialActors, State#ws_state{
                        authenticated = true,
                        session_id = ResumeId,
                        session_pid = Pid,
                        session_mon = MonRef,
                        actor_feed = ActorFeed
                    }};
                false ->
                    %% Session process died — clean up and create new
                    beamtalk_session_table:delete(ResumeId),
                    ?LOG_INFO("Resumed session expired, creating new", #{
                        old_session => ResumeId,
                        peer => State#ws_state.peer,
                        domain => [beamtalk, runtime]
                    }),
                    create_session(beamtalk_repl_server:generate_session_id(), Meta, State)
            end;
        error ->
            %% Session not found — create new
            ?LOG_INFO("Resume session not found, creating new", #{
                requested_session => ResumeId,
                peer => State#ws_state.peer,
                domain => [beamtalk, runtime]
            }),
            create_session(beamtalk_repl_server:generate_session_id(), Meta, State)
    end;
start_or_resume_session(_Invalid, Meta, State) ->
    %% Non-binary resume value (e.g. number, list) — ignore and create new
    SessionId = beamtalk_repl_server:generate_session_id(),
    create_session(SessionId, Meta, State).

-doc """
Subscribe to the push streams unless this is a shutdown-only connection
(`workspace stop`), which needs none of them (BT-5066).
""".
maybe_subscribe_all(#ws_state{shutdown_only = true}) -> ok;
maybe_subscribe_all(_State) -> beamtalk_repl_subscriptions:subscribe_all().

-doc "Create a fresh session and send auth_ok + session-started messages.".
create_session(SessionId, Meta, State) ->
    case beamtalk_session_sup:start_session(SessionId, Meta) of
        {ok, SessionPid} ->
            MonRef = erlang:monitor(process, SessionPid),
            beamtalk_session_table:insert(SessionId, SessionPid),
            ?LOG_INFO("WebSocket auth succeeded, session created", #{
                session => SessionId,
                session_pid => SessionPid,
                peer => State#ws_state.peer,
                domain => [beamtalk, runtime]
            }),
            _ = beamtalk_repl_shell:attach(SessionPid, self()),
            beamtalk_workspace_meta:update_activity(),
            %% Subscribe to all live push streams (Transcript, actors,
            %% classes, bindings, flush) via the stable facade. ADR 0082 Phase 3:
            %% the flush stream lets LSP clients emit
            %% `workspace/applyEdit` on flush.
            maybe_subscribe_all(State),
            {InitialActors, ActorFeed} = actor_snapshot_frames(),
            AuthOk = iolist_to_binary(json:encode(#{<<"type">> => <<"auth_ok">>})),
            SessionMsg = iolist_to_binary(
                json:encode(#{
                    <<"op">> => <<"session-started">>,
                    <<"session">> => SessionId
                })
            ),
            {[{text, AuthOk}, {text, SessionMsg}] ++ InitialActors, State#ws_state{
                authenticated = true,
                session_id = SessionId,
                session_pid = SessionPid,
                session_mon = MonRef,
                actor_feed = ActorFeed
            }};
        {error, Reason} ->
            ?LOG_ERROR("Session creation failed after auth", #{
                reason => Reason,
                peer => State#ws_state.peer,
                domain => [beamtalk, runtime]
            }),
            ErrorJson = iolist_to_binary(
                json:encode(#{
                    <<"type">> => <<"auth_error">>,
                    <<"message">> => <<"Session creation failed">>
                })
            ),
            {[{text, ErrorJson}, {close, 1011, <<"Session creation failed">>}], State}
    end.

-doc """
Format a Cowboy peer (`{inet:ip_address(), inet:port_number()}`) as a
`"host:port"` binary for session origin metadata, or `undefined` when the peer
is unavailable.
""".
-spec format_peer({inet:ip_address(), inet:port_number()} | undefined) -> binary() | undefined.
format_peer({IpAddr, Port}) when is_integer(Port) ->
    case inet:ntoa(IpAddr) of
        {error, _} -> undefined;
        Host -> iolist_to_binary([Host, $:, integer_to_binary(Port)])
    end;
format_peer(_) ->
    undefined.

-doc "Start async eval with this handler as the streaming subscriber.".
handle_eval_async(Msg, SessionPid, State = #ws_state{pending_eval = undefined}) ->
    Params = beamtalk_repl_protocol:get_params(Msg),
    case maps:get(<<"code">>, Params, <<>>) of
        CodeBin when is_binary(CodeBin) ->
            Code = binary_to_list(CodeBin),
            case Code of
                [] ->
                    Err = beamtalk_error:new(empty_expression, 'REPL'),
                    Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
                    Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to evaluate.">>),
                    Response = beamtalk_repl_json:encode_error(
                        Err2, Msg
                    ),
                    {[{text, Response}], State};
                _ ->
                    case is_process_alive(SessionPid) of
                        true ->
                            beamtalk_repl_shell:eval_async(SessionPid, Code, self()),
                            {ok, State#ws_state{pending_eval = Msg}};
                        false ->
                            Response = session_down_reply(
                                <<"Reconnect and retry the evaluation.">>, Msg
                            ),
                            {[{text, Response}], State}
                    end
            end;
        _ ->
            Err = beamtalk_error:new(invalid_code, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Invalid code type">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Code must be a string.">>),
            Response = beamtalk_repl_json:encode_error(
                Err2, Msg
            ),
            {[{text, Response}], State}
    end;
handle_eval_async(Msg, _SessionPid, State) ->
    %% Already have a pending eval — reject to prevent mis-correlation
    Err = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    Response = beamtalk_repl_json:encode_error(
        Err2, Msg
    ),
    {[{text, Response}], State}.

-doc """
Handle the `run-entry` op — dispatch a class entry method (`class` /
`selector` / `args`) with streaming output.

The connected-mode `beamtalk run` consumer. `selector` carries its trailing `:`
for the arity-1 keyword (`main:`) form; `args` is the program arguments as a JSON
array of strings, delivered to the entry as a `List(String)`. Streams and
terminates exactly like `handle_eval_async/3` (same `pending_eval` correlation and
`{eval_*}` terminal messages), so a connected `Program exit: N` ends this session
and the connecting client adopts `N`.
""".
handle_run_entry_async(Msg, SessionPid, State = #ws_state{pending_eval = undefined}) ->
    Params = beamtalk_repl_protocol:get_params(Msg),
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    SelectorBin = maps:get(<<"selector">>, Params, <<>>),
    RawArgs = maps:get(<<"args">>, Params, []),
    case beamtalk_repl_eval:validate_run_entry(ClassBin, SelectorBin, RawArgs) of
        {ok, Argv} ->
            case is_process_alive(SessionPid) of
                true ->
                    beamtalk_repl_shell:dispatch_async(
                        SessionPid, ClassBin, SelectorBin, Argv, self()
                    ),
                    {ok, State#ws_state{pending_eval = Msg}};
                false ->
                    {[{text, session_down_reply(<<"Reconnect and retry.">>, Msg)}], State}
            end;
        {error, Err} ->
            {[{text, beamtalk_repl_json:encode_error(Err, Msg)}], State}
    end;
handle_run_entry_async(Msg, _SessionPid, State) ->
    %% Already have a pending eval — reject to prevent mis-correlation
    Err = beamtalk_error:new(eval_busy, 'REPL'),
    Err1 = beamtalk_error:with_message(Err, <<"An evaluation is already in progress">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"Use Ctrl-C to interrupt the current evaluation.">>),
    {[{text, beamtalk_repl_json:encode_error(Err2, Msg)}], State}.

-doc """
Handle stdin op — route input to IO capture process with ref correlation.
""".
handle_stdin(
    Msg,
    State = #ws_state{
        io_capture_pid = CapturePid,
        stdin_ref = Ref
    }
) when
    is_pid(CapturePid), is_reference(Ref)
->
    Params = beamtalk_repl_protocol:get_params(Msg),
    Value = maps:get(<<"value">>, Params, <<>>),
    case Value of
        <<"eof">> ->
            CapturePid ! {stdin_input, Ref, eof},
            {ok, State#ws_state{io_capture_pid = undefined, stdin_ref = undefined}};
        _ when is_binary(Value) ->
            CapturePid ! {stdin_input, Ref, Value},
            {ok, State#ws_state{io_capture_pid = undefined, stdin_ref = undefined}};
        _ ->
            %% Missing or non-binary value — return error, keep io_capture_pid
            %% so client can retry with a valid value
            Err = beamtalk_error:new(invalid_stdin, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"stdin value must be a string or \"eof\"">>),
            Err2 = beamtalk_error:with_hint(
                Err1, <<"Send {\"op\": \"stdin\", \"value\": \"your input\\n\"}">>
            ),
            Response = beamtalk_repl_json:encode_error(
                Err2, Msg
            ),
            {[{text, Response}], State}
    end;
handle_stdin(Msg, State) ->
    %% No pending IO capture — stdin not expected
    Err = beamtalk_error:new(no_pending_input, 'REPL'),
    Err1 = beamtalk_error:with_message(Err, <<"No pending input request">>),
    Err2 = beamtalk_error:with_hint(Err1, <<"stdin op is only valid after a need-input status.">>),
    Response = beamtalk_repl_json:encode_error(
        Err2, Msg
    ),
    {[{text, Response}], State}.

-doc """
Handle shutdown op — re-validates cookie as an extra security measure.
Even though WebSocket is already authenticated, shutdown is a privileged
operation that requires explicit cookie authorization in the message.
""".
handle_shutdown(Msg, State) ->
    Params = beamtalk_repl_protocol:get_params(Msg),
    case maps:get(<<"cookie">>, Params, undefined) of
        undefined ->
            ?LOG_WARNING("Shutdown denied: missing cookie in message", #{
                domain => [beamtalk, runtime]
            }),
            ErrorJson = beamtalk_repl_json:format_error(<<"Shutdown requires cookie">>),
            {[{text, ErrorJson}], State};
        ProvidedCookie when is_binary(ProvidedCookie) ->
            CookieValid = beamtalk_ws_cookie:valid(ProvidedCookie),
            case CookieValid of
                true ->
                    ?LOG_INFO("Shutdown requested via WebSocket protocol", #{
                        domain => [beamtalk, runtime]
                    }),
                    %% Send to beamtalk_repl_server (a persistent gen_server)
                    %% instead of self() — the WS handler process dies when
                    %% the client closes the connection, racing the 100ms timer.
                    erlang:send_after(100, beamtalk_repl_server, shutdown_requested),
                    Reply = beamtalk_repl_protocol:encode_status(
                        ok,
                        Msg,
                        fun beamtalk_repl_json:term_to_json/1
                    ),
                    {[{text, Reply}], State};
                false ->
                    ?LOG_WARNING("Shutdown denied: invalid cookie", #{domain => [beamtalk, runtime]}),
                    ErrorJson = beamtalk_repl_json:format_error(<<"Invalid shutdown cookie">>),
                    {[{text, ErrorJson}], State}
            end;
        _ ->
            ?LOG_WARNING("Shutdown denied: invalid cookie type", #{domain => [beamtalk, runtime]}),
            ErrorJson = beamtalk_repl_json:format_error(<<"Invalid shutdown cookie">>),
            {[{text, ErrorJson}], State}
    end.

%%% Internal — Actor lifecycle encoding

-doc """
Build WebSocket text frames for all currently live actors, plus the feed that
filters later announcements against them. One actor scan (ADR 0150). Used to
send an initial snapshot on connect/resume so the browser sees actors that were
already running before it subscribed. Callers subscribe first, then call this:
an actor that starts in between is in the snapshot and announced, and the
feed drops that second sighting.
""".
actor_snapshot_frames() ->
    try beamtalk_workspace_actors:list() of
        Actors ->
            Frames = [
                {text, push_frame(<<"actors">>, <<"spawned">>, encode_actor_metadata(M))}
             || M <- Actors
            ],
            {Frames, beamtalk_workspace_actors:feed_new(Actors)}
    catch
        Class:Reason ->
            ?LOG_WARNING("Failed to snapshot actors", #{
                class => Class,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            {[], beamtalk_workspace_actors:feed_new([])}
    end.

-doc "Reply to the pending eval and clear its per-eval state (capture pid, stdin ref).".
-spec finish_eval(binary(), #ws_state{}) -> {[{text, binary()}], #ws_state{}}.
finish_eval(Response, State) ->
    {[{text, Response}], State#ws_state{
        pending_eval = undefined,
        io_capture_pid = undefined,
        stdin_ref = undefined
    }}.

-doc "One push frame (`#{type => push, channel, event, data}`) as JSON text.".
-spec push_frame(binary(), binary(), term()) -> binary().
push_frame(Channel, Event, Data) ->
    iolist_to_binary(
        json:encode(#{
            <<"type">> => <<"push">>,
            <<"channel">> => Channel,
            <<"event">> => Event,
            <<"data">> => Data
        })
    ).

-doc "The `session_down` error reply for a request whose session process has died.".
-spec session_down_reply(binary(), term()) -> binary().
session_down_reply(Hint, Msg) ->
    Err = beamtalk_error:new(session_down, 'REPL'),
    Err1 = beamtalk_error:with_message(Err, <<"REPL session is not running">>),
    Err2 = beamtalk_error:with_hint(Err1, Hint),
    beamtalk_repl_json:encode_error(Err2, Msg).

-doc "Encode actor metadata for push message JSON.".
encode_actor_metadata(#{pid := Pid, class := Class} = Meta) ->
    Base = #{
        <<"class">> => atom_to_binary(Class, utf8),
        <<"pid">> => list_to_binary(pid_to_list(Pid))
    },
    % elp:fixme W0032 maps:find with complex branch logic
    WithTime =
        case maps:find(spawned_at, Meta) of
            {ok, T} -> Base#{<<"spawned_at">> => T};
            error -> Base
        end,
    %% BT-4960: present only for a runtime-started actor, like the `actors` op.
    case Meta of
        #{system := true} -> WithTime#{<<"system">> => true};
        _ -> WithTime
    end.

-doc """
ADR 0082 Phase 3: Normalise the file list shipped on a
`flush_completed` WS push frame. The runtime stores `ChangeEntry.sourceFile`
as a binary (`<<"src/foo.bt">>` or absolute), and `beamtalk_workspace_flush`
forwards that shape unchanged. The push consumer (LSP) expects each element
to be a JSON string; binaries and lists are accepted, anything else is
dropped with a warning so a single malformed entry doesn't break the frame.
""".
-spec normalise_files_for_push([term()]) -> [binary()].
normalise_files_for_push(Files) when is_list(Files) ->
    lists:filtermap(
        fun
            (F) when is_binary(F) ->
                {true, F};
            (F) when is_list(F) ->
                %% `unicode:characters_to_binary/1` returns a binary on
                %% success but `{error, _, _}` / `{incomplete, _, _}` tuples
                %% on bad data — it does NOT throw. Filter failure tuples
                %% out so a malformed entry never reaches the JSON encoder.
                %% `try` also catches the `badarg` case (improper list,
                %% bitstring, etc.) so a single bad entry can't crash the
                %% whole push frame.
                try unicode:characters_to_binary(F) of
                    Bin when is_binary(Bin) ->
                        {true, Bin};
                    {error, _Encoded, _Rest} ->
                        ?LOG_WARNING(
                            "flush_completed push: dropping invalid unicode path entry",
                            #{entry => F, domain => [beamtalk, runtime]}
                        ),
                        false;
                    {incomplete, _Encoded, _Rest} ->
                        ?LOG_WARNING(
                            "flush_completed push: dropping incomplete unicode path entry",
                            #{entry => F, domain => [beamtalk, runtime]}
                        ),
                        false
                catch
                    _:_ ->
                        ?LOG_WARNING(
                            "flush_completed push: dropping non-path file entry",
                            #{entry => F, domain => [beamtalk, runtime]}
                        ),
                        false
                end;
            (Other) ->
                ?LOG_WARNING(
                    "flush_completed push: dropping non-path file entry",
                    #{entry => Other, domain => [beamtalk, runtime]}
                ),
                false
        end,
        Files
    ).

-doc """
ADR 0113 LSP follow-up: normalise the per-file `{file, kind}` list
shipped on a `flush_completed` WS push frame. `beamtalk_workspace_flush`
forwards `[#{file => Path, kind => KindAtom}]`, where `Path` is a binary
(`ChangeEntry.sourceFile`'s own shape) and `KindAtom` is
`beamtalk_workspace_changelog:entry_kind/1`'s own enum value verbatim — not a
parallel taxonomy invented here. Each valid entry becomes a JSON object
`{"file": ..., "kind": ...}`; a malformed entry (not a map, missing/non-binary
`file`, non-atom `kind`) is dropped with a warning so one bad entry cannot
break the whole push frame — mirrors `normalise_files_for_push/1`'s
defensiveness. Defaults to `[]` for a producer that predates this field (the
`Event` map simply has no `fileKinds` key), so an older runtime still degrades
gracefully to an empty list here (the LSP's own existence-check fallback then
takes over).

ADR 0114 LSP follow-up: an entry may also carry `oldFile` — present
only for the `'rename-class'`-kind file that is the actual moved declaration
(`beamtalk_workspace_flush:file_kind_map/1`'s `op = move` case) — forwarded
as `"oldFile"` when present so the LSP can distinguish that file (needs a
`RenameFile`) from an ordinary same-batch `'rename-class'` reference rewrite
(no `oldFile`, needs an ordinary patch). Non-binary `oldFile` is treated the
same as absent rather than dropping the whole entry — the `file`/`kind` pair
is still valid and useful degraded to the ordinary-patch shape.
""".
-spec normalise_file_kinds_for_push([term()]) -> [map()].
normalise_file_kinds_for_push(FileKinds) when is_list(FileKinds) ->
    lists:filtermap(fun normalise_file_kind_entry/1, FileKinds);
normalise_file_kinds_for_push(_Other) ->
    [].

-spec normalise_file_kind_entry(term()) -> {true, map()} | false.
normalise_file_kind_entry(#{file := File, kind := Kind} = Entry) when
    is_binary(File), is_atom(Kind)
->
    Base = #{<<"file">> => File, <<"kind">> => atom_to_binary(Kind, utf8)},
    Out =
        case maps:get(oldFile, Entry, undefined) of
            OldFile when is_binary(OldFile) -> Base#{<<"oldFile">> => OldFile};
            _ -> Base
        end,
    {true, Out};
normalise_file_kind_entry(Other) ->
    ?LOG_WARNING(
        "flush_completed push: dropping malformed fileKinds entry",
        #{entry => Other, domain => [beamtalk, runtime]}
    ),
    false.

-doc """
Encode an `ActorSpawned` announcement payload for the `actors`/`spawned`
push frame. The typed event carries `actorClass` (a Symbol) and `pid`; the live
frame is `{class, pid}` — `spawned_at` is no longer present on the live event
(the connect snapshot still carries it, read from the actor scan).
""".
-spec encode_actor_spawned_event(map()) -> map().
encode_actor_spawned_event(Event) ->
    #{
        <<"class">> => atom_to_binary(maps:get(actorClass, Event, unknown), utf8),
        <<"pid">> => encode_event_pid(maps:get(pid, Event, nil))
    }.

-doc """
Encode an `ActorStopped` announcement payload for the `actors`/`stopped`
push frame. `reason` is the typed, normalized stop symbol (`#normal` / `#shutdown`
/ `#crashed`), encoded as its atom string — replacing the legacy raw `~P`-format.
""".
-spec encode_actor_stopped_event(map()) -> map().
encode_actor_stopped_event(Event) ->
    #{
        <<"class">> => atom_to_binary(maps:get(actorClass, Event, unknown), utf8),
        <<"pid">> => encode_event_pid(maps:get(pid, Event, nil)),
        %% `reason` is always one of the atoms `normal | shutdown | crashed`:
        %% `beamtalk_actor:normalize_stop_reason/1` maps every OTP stop reason onto
        %% that closed set before the `ActorStopped` event is built, so
        %% `atom_to_binary/2` here cannot raise `badarg` (no defensive guard needed).
        <<"reason">> => atom_to_binary(maps:get(reason, Event, normal), utf8)
    }.

-doc """
Encode a `ClassLoaded` / `ClassRemoved` announcement payload — both
carry `className` (a Symbol) — for the `classes` push frame `{class}`.
""".
-spec encode_class_event(map()) -> map().
encode_class_event(Event) ->
    #{<<"class">> => atom_to_binary(maps:get(className, Event, unknown), utf8)}.

-doc """
ADR 0105 Phase 1: encode a `'ReloadCheckCompleted'` announcement
payload (built by `beamtalk_repl_loader_recheck:publish_recheck_outcome/5`) for the
`reload_check`/`completed` push frame.
""".
-spec encode_reload_check_event(map()) -> map().
encode_reload_check_event(Event) ->
    #{
        <<"changedClass">> => maps:get(changedClass, Event, <<>>),
        <<"changedSelector">> => maps:get(changedSelector, Event, <<>>),
        <<"classification">> => atom_to_binary(maps:get(classification, Event, self_edit), utf8),
        <<"checked">> => maps:get(checked, Event, 0),
        <<"notChecked">> => maps:get(notChecked, Event, 0),
        <<"capNote">> => beamtalk_repl_protocol:undefined_to_null(
            maps:get(capNote, Event, undefined)
        ),
        <<"checkedOwners">> => maps:get(checkedOwners, Event, []),
        %% Shared with the `reload-findings` op response
        %% (`beamtalk_repl_ops_dev:handle_term/4`) so the push and
        %% request/response wire shapes can never drift apart.
        <<"findings">> => [
            beamtalk_repl_protocol:encode_reload_finding(F)
         || F <- maps:get(findings, Event, [])
        ]
    }.

-doc """
ADR 0123 §4 (BT-3538): encode a `'ShapeReloadFindingsCompleted'` announcement
payload (built by `beamtalk_repl_loader_recheck:publish_reload_findings/2`) for the
`reload_check`/`shape_findings` push frame — the class-body reload/migration
findings table (dropped-without-bump, added-only, bumped-without-migration,
version-decreased, instances-suspended), distinct from
`encode_reload_check_event/1`'s xref-dependent-caller findings.
""".
-spec encode_shape_reload_findings_event(map()) -> map().
encode_shape_reload_findings_event(Event) ->
    #{
        <<"class">> => maps:get(class, Event, <<>>),
        <<"findings">> => [
            encode_shape_reload_finding(F)
         || F <- maps:get(findings, Event, [])
        ]
    }.

-doc "Encode one `beamtalk_shape_diff:reload_finding()` for JSON.".
-spec encode_shape_reload_finding(map()) -> map().
encode_shape_reload_finding(Finding) ->
    #{
        <<"class">> => maps:get(class, Finding, <<>>),
        <<"kind">> => atom_to_binary(maps:get(kind, Finding, undefined), utf8),
        <<"severity">> => atom_to_binary(maps:get(severity, Finding, hint), utf8),
        <<"fields">> => maps:get(fields, Finding, []),
        <<"fromVersion">> => beamtalk_repl_protocol:undefined_to_null(
            maps:get(from_version, Finding, undefined)
        ),
        <<"toVersion">> => beamtalk_repl_protocol:undefined_to_null(
            maps:get(to_version, Finding, undefined)
        ),
        <<"migrated">> => beamtalk_repl_protocol:undefined_to_null(
            maps:get(migrated, Finding, undefined)
        ),
        <<"suspended">> => beamtalk_repl_protocol:undefined_to_null(
            maps:get(suspended, Finding, undefined)
        ),
        <<"pids">> => [encode_event_pid(P) || P <- maps:get(pids, Finding, [])],
        <<"message">> => maps:get(message, Finding, <<>>)
    }.

-doc "Encode an announcement event pid for JSON, mapping a `nil` pid to `null`.".
-spec encode_event_pid(pid() | nil) -> binary() | null.
encode_event_pid(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
encode_event_pid(_) -> null.

-doc """
Map a `nil` value to JSON `null`, passing any other value through. Distinct
from `beamtalk_repl_protocol:undefined_to_null/1`, which maps the
Erlang "missing value" idiom `undefined` instead — `json:encode/1` has no
default clause for an arbitrary atom, so encoding either unconverted would
raise.
""".
-spec nullable(term()) -> term().
nullable(nil) -> null;
nullable(Value) -> Value.

-doc """
Extract line/hint metadata from a compile error for inclusion in JSON response.
Returns a map with `<<"line">>' and optionally `<<"hint">>' keys, or an empty map.
""".
-spec extract_compile_error_location(term()) -> map().
extract_compile_error_location({compile_error, [DiagMap | _]}) when is_map(DiagMap) ->
    case maps:find(line, DiagMap) of
        {ok, Line} when is_integer(Line) ->
            Base = #{<<"line">> => Line},
            % elp:fixme W0032 maps:find with complex branch logic
            case maps:find(hint, DiagMap) of
                {ok, Hint} -> Base#{<<"hint">> => Hint};
                error -> Base
            end;
        _ ->
            #{}
    end;
extract_compile_error_location(_) ->
    #{}.

%%% Internal — Log streaming

-doc "Subscribe to live log events. Accepts an optional level parameter.".
handle_subscribe_logs(Msg, State) ->
    %% Already subscribed: `subscribe/1` just updates the level of the existing
    %% subscription (one monitor process throughout).
    do_subscribe_logs(Msg, State).

do_subscribe_logs(Msg, State) ->
    Params = beamtalk_repl_protocol:get_params(Msg),
    case parse_log_level(maps:get(<<"level">>, Params, undefined)) of
        {ok, Level} ->
            beamtalk_ws_log_handler:subscribe(Level),
            Response = beamtalk_repl_protocol:encode_status(
                ok, Msg, fun beamtalk_repl_json:term_to_json/1
            ),
            {[{text, Response}], State#ws_state{log_subscribed = true}};
        {error, Err} ->
            %% Unknown level: refuse rather than guess, and leave any existing
            %% subscription as it was (see `handle_subscribe_logs/2`).
            {[{text, beamtalk_repl_json:encode_error(Err, Msg)}], State}
    end.

handle_unsubscribe_logs(Msg, State) ->
    beamtalk_ws_log_handler:unsubscribe(),
    Response = beamtalk_repl_protocol:encode_status(
        ok, Msg, fun beamtalk_repl_json:term_to_json/1
    ),
    {[{text, Response}], State#ws_state{log_subscribed = false}}.

-doc """
Parse the `level` of a `subscribe-logs` request. An absent level means `debug`
(everything); a level that is not one of the OTP levels is an
`invalid_argument` error rather than silently the most verbose stream.
""".
-spec parse_log_level(term()) -> {ok, logger:level()} | {error, #beamtalk_error{}}.
parse_log_level(undefined) ->
    {ok, debug};
parse_log_level(Level) when is_binary(Level) ->
    case
        lists:member(Level, [
            <<"emergency">>,
            <<"alert">>,
            <<"critical">>,
            <<"error">>,
            <<"warning">>,
            <<"notice">>,
            <<"info">>,
            <<"debug">>
        ])
    of
        true -> {ok, binary_to_existing_atom(Level, utf8)};
        false -> {error, invalid_log_level()}
    end;
parse_log_level(_Other) ->
    {error, invalid_log_level()}.

-spec invalid_log_level() -> #beamtalk_error{}.
invalid_log_level() ->
    beamtalk_repl_errors:make(
        invalid_argument,
        'REPL',
        <<"Unknown log level">>,
        <<
            "Use one of: emergency, alert, critical, error, warning, notice, info, debug"
            " (omit `level` for debug)."
        >>
    ).

-doc """
Encode a log event map for JSON push. Converts atom keys to binary keys
and strips undefined values.
""".
-spec encode_log_event(map()) -> map().
encode_log_event(EventData) ->
    maps:fold(
        fun
            (_K, undefined, Acc) -> Acc;
            (K, V, Acc) -> Acc#{atom_to_binary(K, utf8) => V}
        end,
        #{},
        EventData
    ).
