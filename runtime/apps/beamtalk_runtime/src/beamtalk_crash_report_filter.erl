%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_crash_report_filter).
-moduledoc """
Show Beamtalk names in OTP crash reports (ADR 0139, amendment 2026-11-14,
BT-4820).

A crashing actor is reported by OTP: a `gen_server` terminate report, a
`proc_lib` crash report, and a supervisor `child_terminated` report. They
print compiler-made names (`bt@pkg@counter:dispatch/4`). This logger
**primary filter** makes the text of those reports say `Counter>>increment`
instead, using the one frame renderer, `beamtalk_stack_frame`.

## Scope (the decision recorded in ADR 0139)

| Report | Translated | Left as terms |
|---|---|---|
| `gen_server` terminate | stacktrace frames in `reason` | `state`, `last_message`, `log`, `client_info` |
| `proc_lib` crash | `initial_call`; stacktrace in `error_info` | `messages`, `dictionary`, linked-process info |
| supervisor `child_terminated` / `start_error` | `offender` `mfargs`/`mfa`; stacktrace in `reason` | `pid`, `name`, `restart_type`, ... |

Anything not recognised passes through unchanged.

## Mechanism

The filter does not touch the report. It replaces the event's `report_cb`
with a wrapper that formats the *translated* report with the original
callback. The report term stays raw, so structured consumers
(`beamtalk_json_formatter`, `ws_log_handler`) keep the compiler names (the
raw MFA), and a formatter that does not use `report_cb` is unaffected. The
module→class map is built once per report, in the filter (not per frame,
not at format time).

## Failure contract

The filter never raises (logger removes a filter that crashes, silently
switching translation off). Any error is caught, the event is passed on
untranslated and one warning is logged. The wrapper `report_cb` falls
back to the original callback on error too.
""".

-include_lib("kernel/include/logger.hrl").

-export([install/0, filter/2]).

-define(FILTER_ID, beamtalk_crash_report_filter).
%% Bounds on the walk of a `reason` term looking for stacktraces.
-define(MAX_DEPTH, 4).
-define(MAX_LIST, 1000).

-doc """
Install the primary filter. Called once at runtime start; installing it
again is a no-op. Never removed (actors outlive the runtime application).
""".
-spec install() -> ok.
install() ->
    case logger:add_primary_filter(?FILTER_ID, {fun ?MODULE:filter/2, []}) of
        ok -> ok;
        {error, {already_exist, _}} -> ok
    end.

-doc """
Logger primary filter. Rewrites `report_cb` for recognised crash reports
that name at least one Beamtalk MFA; everything else is `ignore`d. Never raises.
""".
-spec filter(logger:log_event(), term()) -> logger:filter_return().
filter(Event, _Extra) ->
    try
        translate(Event)
    catch
        Class:Reason ->
            ?LOG_WARNING(
                #{
                    label => {beamtalk, crash_report_translation_failed},
                    class => Class,
                    reason => Reason
                },
                #{domain => [beamtalk, runtime]}
            ),
            ignore
    end.

%%% Internal

-spec translate(logger:log_event()) -> logger:filter_return().
translate(#{msg := {report, #{label := Label} = Report}, meta := Meta} = Event) ->
    case recognised(Label) of
        false ->
            ignore;
        true ->
            case maps:get(report_cb, Meta, undefined) of
                undefined ->
                    ignore;
                OrigCb ->
                    Mfas = lists:usort(report_mfas(Label, Report)),
                    Rendered = beamtalk_stack_frame:render_mfas(Mfas),
                    Changed = maps:filter(
                        fun(Mfa, Text) -> Text =/= beamtalk_stack_frame:raw_mfa(Mfa) end,
                        Rendered
                    ),
                    case map_size(Changed) of
                        0 -> ignore;
                        _ -> Event#{meta := Meta#{report_cb => wrap_cb(OrigCb, Label, Changed)}}
                    end
            end
    end;
translate(_Event) ->
    ignore.

-spec recognised(term()) -> boolean().
recognised({gen_server, terminate}) -> true;
recognised({proc_lib, crash}) -> true;
recognised({supervisor, child_terminated}) -> true;
recognised({supervisor, start_error}) -> true;
recognised(_) -> false.

%% The MFAs named by the translated fields of a report.
-spec report_mfas(term(), map()) -> [{atom(), atom(), arity()}].
report_mfas({gen_server, terminate}, Report) ->
    frames_in(maps:get(reason, Report, undefined), ?MAX_DEPTH);
report_mfas({proc_lib, crash}, #{report := [Info | _]}) when is_list(Info) ->
    case lists:keyfind(initial_call, 1, Info) of
        {initial_call, {M, F, A}} -> mfa_of(M, F, A);
        _ -> []
    end ++
        case lists:keyfind(error_info, 1, Info) of
            {error_info, ErrorInfo} -> frames_in(ErrorInfo, ?MAX_DEPTH);
            _ -> []
        end;
report_mfas(_Supervisor, #{report := Report}) when is_list(Report) ->
    Reason =
        case lists:keyfind(reason, 1, Report) of
            {reason, R} -> frames_in(R, ?MAX_DEPTH);
            _ -> []
        end,
    case lists:keyfind(offender, 1, Report) of
        {offender, Offender} when is_list(Offender) -> Reason ++ offender_mfas(Offender);
        _ -> Reason
    end;
report_mfas(_, _) ->
    [].

-spec offender_mfas(list()) -> [{atom(), atom(), arity()}].
offender_mfas(Offender) ->
    lists:flatmap(
        fun
            ({Key, {M, F, A}}) when Key =:= mfargs; Key =:= mfa -> mfa_of(M, F, A);
            (_) -> []
        end,
        Offender
    ).

-spec mfa_of(term(), term(), term()) -> [{atom(), atom(), arity()}].
mfa_of(M, F, A) when is_atom(M), is_atom(F), is_list(A) -> [{M, F, length(A)}];
mfa_of(M, F, A) when is_atom(M), is_atom(F), is_integer(A), A >= 0 -> [{M, F, A}];
mfa_of(_, _, _) -> [].

%% Stacktrace frames anywhere in the first few levels of a term.
-spec frames_in(term(), non_neg_integer()) -> [{atom(), atom(), arity()}].
frames_in(_Term, 0) ->
    [];
frames_in({M, F, A, Loc}, _Depth) when is_atom(M), is_atom(F), is_list(Loc) ->
    mfa_of(M, F, A);
frames_in(Term, Depth) when is_tuple(Term) ->
    lists:flatmap(fun(E) -> frames_in(E, Depth - 1) end, tuple_to_list(Term));
frames_in(Term, Depth) when is_list(Term) ->
    try
        lists:flatmap(fun(E) -> frames_in(E, Depth - 1) end, lists:sublist(Term, ?MAX_LIST))
    catch
        error:_ -> []
    end;
frames_in(_Term, _Depth) ->
    [].

%%% report_cb wrapper

-spec wrap_cb(function(), term(), #{{atom(), atom(), arity()} => binary()}) -> function().
wrap_cb(Orig, Label, Rendered) when is_function(Orig, 1) ->
    fun(Report) ->
        {"~ts", [
            format(
                fun(R) ->
                    {F, A} = Orig(R),
                    io_lib:format(F, A)
                end,
                Label,
                Rendered,
                Report
            )
        ]}
    end;
wrap_cb(Orig, Label, Rendered) when is_function(Orig, 2) ->
    fun(Report, Opts) ->
        format(fun(R) -> Orig(R, Opts) end, Label, Rendered, Report)
    end.

%% A `/1` callback's result is formatted here, so logger's `depth` option no
%% longer applies to it (the OTP reports in scope use `/2` callbacks).
%% Format `Report` with the original callback after naming the initial call
%% and offender, then rename the stacktrace frames in the text. On any error
%% the untranslated original output is used.
-spec format(fun((term()) -> unicode:chardata()), term(), map(), term()) ->
    unicode:chardata().
format(Call, Label, Rendered, Report) ->
    try
        Text = unicode:characters_to_binary(Call(rename_terms(Label, Report, Rendered))),
        true = is_binary(Text),
        rename_frames(Text, Rendered)
    catch
        _:_ -> Call(Report)
    end.

%% A supervisor prints the offender's `mfargs` as a term, so it is replaced in
%% the report with its rendered name before formatting. (`proc_lib` prints its
%% `initial_call` as `M:F/A`, renamed in the text with the stacktrace.)
-spec rename_terms(term(), term(), map()) -> term().
rename_terms({supervisor, _}, #{report := Items} = Report, Rendered) when is_list(Items) ->
    Report#{report := [rename_offender(Item, Rendered) || Item <- Items]};
rename_terms(_Label, Report, _Rendered) ->
    Report.

rename_offender({offender, Offender}, Rendered) when is_list(Offender) ->
    {offender, [
        case Prop of
            {Key, {M, F, A}} when Key =:= mfargs; Key =:= mfa ->
                rename_mfa_item(Key, M, F, A, Prop, Rendered);
            _ ->
                Prop
        end
     || Prop <- Offender
    ]};
rename_offender(Item, _Rendered) ->
    Item.

rename_mfa_item(Key, M, F, A, Item, Rendered) ->
    case mfa_of(M, F, A) of
        [Mfa] ->
            case Rendered of
                #{Mfa := Text} -> {Key, Text};
                _ -> Item
            end;
        [] ->
            Item
    end.

%% A frame prints as `M:F/A` (erl_error), `M:F(Args)` (erl_error with
%% arguments) or `{M,F,A,Location}` (a reason printed as a term, which is what
%% `gen_server` does for a reason that is not a `{Class, Reason, Stack}`).
%% Known limit: this works on the text of the whole report, so a frame-shaped
%% string inside `state` or `last_message` would be renamed too.
-spec rename_frames(binary(), map()) -> binary().
rename_frames(Text, Rendered) ->
    maps:fold(
        fun({M, F, A} = Mfa, Name, Acc) ->
            %% erl_error writes atoms with `~tw` (quoted when needed, as for
            %% every keyword or operator selector: `'at:put:'/3`); the raw
            %% spelling is unquoted. Replace both.
            Spellings = lists:usort([
                fmt("~ts:~ts/~B", [M, F, A]),
                fmt("~tw:~tw/~B", [M, F, A]),
                beamtalk_stack_frame:raw_mfa(Mfa)
            ]),
            Acc1 = lists:foldl(
                fun(Spelling, T) -> replace_literal(T, Spelling, Name, <<"(?![0-9])">>) end,
                Acc,
                Spellings
            ),
            Acc2 = lists:foldl(
                fun(Prefix, T) -> replace_literal(T, Prefix, <<Name/binary, "(">>, <<>>) end,
                Acc1,
                lists:usort([fmt("~ts:~ts(", [M, F]), fmt("~tw:~tw(", [M, F])])
            ),
            rename_tuple_frame(Acc2, M, F, A, Name)
        end,
        Text,
        Rendered
    ).

-spec fmt(string(), list()) -> binary().
fmt(Format, Args) ->
    unicode:characters_to_binary(io_lib:format(Format, Args)).

%% Replace every occurrence of `Literal` (not a regex) followed by `Lookahead`.
-spec replace_literal(binary(), binary(), binary(), binary()) -> binary().
replace_literal(Text, Literal, Replacement, Lookahead) ->
    re:replace(
        Text,
        [re:replace(Literal, "[^A-Za-z0-9_]", "\\\\&", [global, {return, binary}]), Lookahead],
        re:replace(Replacement, "[\\\\&]", "\\\\&", [global, {return, binary}]),
        [global, {return, binary}, unicode]
    ).

-spec rename_tuple_frame(binary(), atom(), atom(), arity(), binary()) -> binary().
rename_tuple_frame(Text, M, F, A, Name) ->
    Pattern = iolist_to_binary([
        "\\{",
        re:replace(io_lib:write_atom(M), "[^A-Za-z0-9_]", "\\\\&", [global, {return, binary}]),
        ",\\s*",
        re:replace(io_lib:write_atom(F), "[^A-Za-z0-9_]", "\\\\&", [global, {return, binary}]),
        ",\\s*(?:",
        integer_to_binary(A),
        "|\\[[^\\]]*\\]),"
    ]),
    re:replace(
        Text,
        Pattern,
        ["{", re:replace(Name, "[\\\\&]", "\\\\&", [global, {return, binary}]), ","],
        [
            global, {return, binary}, unicode
        ]
    ).
