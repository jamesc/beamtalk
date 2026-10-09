#!/usr/bin/env escript
%% -*- erlang -*-
%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% Lint (BT-3769, ADR 0130 §4): every runtime/stdlib `catch` that runs a
%% Beamtalk block either restores class variables or says why it need not.
%%
%% ADR 0130 makes a protected region a transaction for class variables: when an
%% error crosses the region's catch, class-variable writes made inside it are
%% discarded. Erlang code that catches an error around a block and continues
%% must wrap the call in `beamtalk_class_vars:protect/1` (erlang-guidelines.md
%% § Class variables). This lint replaces the hand-kept site list that BT-3728
%% recorded in docs/development/class-var-catcher-audit.md.
%%
%% ── What it flags ───────────────────────────────────────────────────────────
%% It parses `runtime/apps/*/src/*.erl` with `epp_dodger`/`erl_syntax` and looks
%% at every catching region: a `try` with at least one `catch` clause (its body,
%% not its `of`/`after` clauses) and every old-style `catch Expr`. A region is a
%% *site* when its body applies a value it cannot see statically:
%%
%%     Block(...)                      a variable in operator position
%%     apply(F, Args), erlang:apply/2  any apply/2
%%     apply(M, F, Args), erlang:apply/3   unless M and F are both literal atoms
%%     Mod:Fun(...)                    a remote call whose module or function is
%%                                     not a literal atom
%%
%% Nested funs are searched too (a fun built in the region may be run by it).
%% Anything inside the argument of `beamtalk_class_vars:protect/1` is skipped:
%% that is the conversion this lint exists to keep. A region whose body calls
%% protect/1 is a site too once it carries a marker, so a `converted` marker on
%% it is checked rather than stale.
%%
%% ── How to clear a failure ──────────────────────────────────────────────────
%% Either wrap the call in `beamtalk_class_vars:protect/1`, or put an audit
%% marker comment on its own line inside the enclosing function (or in the
%% comment block directly above it), at or above the `try`/`catch`:
%%
%%     %% bt-catcher-audit: <disposition> - <reason>
%%
%% A marker governs only the first catching region below it in the same
%% function, so a region added later needs its own marker; mark each region,
%% even when several share one reason. The dispositions (docs/development/class-var-catcher-audit.md):
%%
%%     converted                     restores via protect/1 or snapshot+restore
%%                                   (the function must call protect/1 or
%%                                   restore/1, or take a snapshot/0 that the
%%                                   module restores, so it cannot rot)
%%     not-applicable-no-block       the applied value is never a Beamtalk block
%%     not-applicable-reraises       every catch clause re-raises
%%     not-applicable-other-process  the block runs in another process
%%     deliberately-left             swallows, but restoring would be wrong
%%
%% `needs-conversion` is not a valid marker: convert the site instead. A marker
%% that governs no site is stale and fails too, so markers cannot outlive code.
%%
%% Usage:
%%   escript scripts/ci/lint-class-var-catchers.escript              lint the runtime
%%   escript scripts/ci/lint-class-var-catchers.escript FILE...      lint these files
%%                                                                   (paths from repo root)
%%   escript scripts/ci/lint-class-var-catchers.escript --self-test  check the fixtures
%%   escript scripts/ci/lint-class-var-catchers.escript --list       print every site

-mode(compile).

-define(FIXTURE_DIR, "scripts/ci/fixtures/lint-class-var-catchers").

-define(DISPOSITIONS, [
    "converted",
    "not-applicable-no-block",
    "not-applicable-reraises",
    "not-applicable-other-process",
    "deliberately-left"
]).

main(Args) ->
    io:setopts(standard_io, [{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    ok = goto_repo_root(),
    case Args of
        ["--self-test"] -> self_test();
        ["--list"] -> list_sites(runtime_sources());
        [] -> lint(runtime_sources());
        Files -> lint(Files)
    end.

%% ── Lint mode ───────────────────────────────────────────────────────────────

lint([]) ->
    io:format(standard_error, "❌ lint-class-var-catchers: no Erlang sources found.~n", []),
    halt(1);
lint(Files) ->
    Results = [{F, analyze(F)} || F <- Files],
    Errors = lists:append([Es || {_, {_Sites, Es}} <- Results]),
    %% With no errors, every reported site carries a marker.
    Marked = length(lists:append([Ss || {_, {Ss, _}} <- Results])),
    case Errors of
        [] ->
            io:format(
                "✅ Class-variable catcher audit: ~b catching site(s) carry a bt-catcher-audit "
                "marker.~n",
                [Marked]
            ),
            halt(0);
        _ ->
            io:format(standard_error, "~n❌ Class-variable catcher audit (BT-3769) failed:~n~n", []),
            lists:foreach(fun report_error/1, lists:sort(Errors)),
            io:format(
                standard_error,
                "~nA catch around a Beamtalk block must restore class variables (ADR 0130 §4).~n"
                "Fix: wrap the call in beamtalk_class_vars:protect/1, or add~n"
                "     %% bt-catcher-audit: <disposition> - <reason>~n"
                "     at or above the try/catch. Dispositions and their meaning:~n"
                "     docs/development/class-var-catcher-audit.md~n",
                []
            ),
            halt(1)
    end.

report_error({File, Line, Kind, Detail}) ->
    io:format(standard_error, "  ~ts:~b: ~ts~n", [File, Line, describe_error(Kind, Detail)]).

describe_error(unmarked, Calls) ->
    io_lib:format("catch runs ~ts without protect/1 or a bt-catcher-audit marker", [Calls]);
describe_error(bad_disposition, D) ->
    io_lib:format("unknown bt-catcher-audit disposition '~ts' (valid: ~ts)", [
        D, lists:join(", ", ?DISPOSITIONS)
    ]);
describe_error(missing_reason, D) ->
    io_lib:format("bt-catcher-audit: ~ts has no reason; write '~ts - <why>'", [D, D]);
describe_error(stale_marker, D) ->
    io_lib:format("bt-catcher-audit: ~ts governs no catching site that runs a dynamic call", [D]);
describe_error(converted_without_restore, _) ->
    "bt-catcher-audit: converted, but the function neither calls "
    "beamtalk_class_vars:protect/1 or restore/1 nor takes a snapshot/0 the module restores";
describe_error(parse_error, Why) ->
    io_lib:format("could not parse: ~tp", [Why]).

%% ── List mode ───────────────────────────────────────────────────────────────

list_sites(Files) ->
    lists:foreach(
        fun(F) ->
            {Sites0, _} = analyze(F),
            Sites = lists:sort(fun(#{line := A}, #{line := B}) -> A =< B end, Sites0),
            lists:foreach(
                fun(#{line := L, function := Fn, calls := Calls, marker := M}) ->
                    Disp =
                        case M of
                            none -> "UNMARKED";
                            {_, D, _} -> D
                        end,
                    io:format("~ts:~b\t~ts\t~ts\t~ts~n", [F, L, Fn, Disp, Calls])
                end,
                Sites
            )
        end,
        Files
    ),
    halt(0).

%% ── Self-test mode ──────────────────────────────────────────────────────────
%%
%% Each fixture declares the errors it must produce, one per line, as
%%     %% expect: <kind> <line>
%% and a fixture with no `expect:` lines must lint clean. The self-test fails
%% if any fixture's actual errors differ from its expectations, so the lint is
%% proved to fail on an unmarked `Block()` under a `catch`.

self_test() ->
    Fixtures = lists:sort(filelib:wildcard(?FIXTURE_DIR ++ "/*.erl")),
    Fixtures =:= [] andalso
        begin
            io:format(standard_error, "❌ No fixtures under ~ts~n", [?FIXTURE_DIR]),
            halt(1)
        end,
    Failures = lists:filtermap(fun check_fixture/1, Fixtures),
    case Failures of
        [] ->
            io:format("✅ lint-class-var-catchers self-test: ~b fixture(s) behave as expected.~n", [
                length(Fixtures)
            ]),
            halt(0);
        _ ->
            io:format(standard_error, "❌ lint-class-var-catchers self-test failed:~n", []),
            [
                io:format(standard_error, "  ~ts~n    expected ~tp~n    got      ~tp~n", [F, E, G])
             || {F, E, G} <- Failures
            ],
            halt(1)
    end.

check_fixture(File) ->
    {ok, Bin} = file:read_file(File),
    Expected = lists:sort([
        {list_to_atom(K), list_to_integer(L)}
     || Line <- string:split(unicode:characters_to_list(Bin), "\n", all),
        {match, [K, L]} <- [
            re:run(Line, "^\\s*%+\\s*expect:\\s*([a-z_]+)\\s+([0-9]+)\\s*$", [
                unicode, {capture, all_but_first, list}
            ])
        ]
    ]),
    {_Sites, Errors} = analyze(File),
    Got = lists:sort([{K, L} || {_, L, K, _} <- Errors]),
    case Got =:= Expected of
        true -> false;
        false -> {true, {File, Expected, Got}}
    end.

%% ── Analysis ────────────────────────────────────────────────────────────────
%%
%% analyze(File) -> {Sites, Errors}
%%   Site  = #{line, function, calls, marker}
%%   Error = {File, Line, Kind, Detail}

analyze(File) ->
    case epp_dodger:parse_file(File, [{no_fail, true}]) of
        {ok, Forms} ->
            Markers = scan_markers(File),
            analyze_forms(File, Forms, Markers);
        {error, Why} ->
            {[], [{File, 1, parse_error, Why}]}
    end.

analyze_forms(File, Forms, Markers) ->
    Funs = function_regions(Forms),
    %% A form epp_dodger could not parse may hide a site: fail rather than skip.
    %% With `no_fail` such a form comes back as a `text` node (raw source).
    ParseErrors = [parse_error(File, F) || F <- Forms, is_unparsed(F)],
    %% Per function: catching regions, and whether it restores class variables.
    Self = module_name(Forms),
    ModuleRestores = lists:member(restore, class_var_calls(Self, Forms)),
    PerFun = [
        {Region, Name, catching_regions(Form),
            restores(class_var_calls(Self, [Form]), ModuleRestores)}
     || {Region, Name, Form} <- Funs
    ],
    MarkerErrors = ParseErrors ++ lists:append([validate_marker(File, M) || M <- Markers]),
    {Sites, Governed, SiteErrors} = lists:foldl(
        fun({{Start, _End}, Name, Regions, Restores}, Acc0) ->
            %% Walk the function's regions top-down; `Prev` is the line of the
            %% region before, which bounds the markers this one can claim.
            {Acc, _} = lists:foldl(
                fun({Line, _, _} = Region, {Acc1, Prev}) ->
                    Marker = governing_marker(Markers, Prev, Line),
                    {check_region(File, Name, Region, Marker, Restores, Acc1), Line}
                end,
                {Acc0, Start - 1},
                lists:keysort(1, Regions)
            ),
            Acc
        end,
        {[], [], []},
        PerFun
    ),
    Stale = [
        {File, L, stale_marker, D}
     || {L, D, _} <- Markers, lists:member(D, ?DISPOSITIONS), not lists:member(L, Governed)
    ],
    {lists:reverse(Sites), lists:usort(SiteErrors ++ MarkerErrors ++ Stale)}.

is_unparsed(Form) ->
    lists:member(erl_syntax:type(Form), [error_marker, text]).

parse_error(File, Form) ->
    Detail =
        case erl_syntax:type(Form) of
            error_marker -> erl_syntax:error_marker_info(Form);
            text -> string:slice(string:trim(erl_syntax:text_string(Form)), 0, 60)
        end,
    {File, line(Form), parse_error, Detail}.

%% Record one catching region as a site (and its errors) when it makes a
%% dynamic call, or calls protect/1 under a marker; otherwise ignore it.
check_region(File, Name, {Line, Calls, Protected}, Marker, Restores, {Sites, Governed, Errors}) ->
    case Calls =/= [] orelse (Protected andalso Marker =/= none) of
        false ->
            {Sites, Governed, Errors};
        true ->
            CallText = lists:join(", ", lists:usort(Calls)),
            Site = #{line => Line, function => Name, calls => CallText, marker => Marker},
            Governed1 =
                case Marker of
                    none -> Governed;
                    {ML, _, _} -> [ML | Governed]
                end,
            {[Site | Sites], Governed1,
                site_errors(File, Line, CallText, Marker, Restores) ++ Errors}
    end.

site_errors(File, Line, CallText, none, _Restores) ->
    [{File, Line, unmarked, CallText}];
site_errors(File, _Line, _CallText, {ML, "converted", _}, false) ->
    [{File, ML, converted_without_restore, ""}];
site_errors(_File, _Line, _CallText, _Marker, _Restores) ->
    [].

validate_marker(File, {L, D, Reason}) ->
    case lists:member(D, ?DISPOSITIONS) of
        false ->
            [{File, L, bad_disposition, D}];
        true when Reason =:= "" ->
            [{File, L, missing_reason, D}];
        true ->
            []
    end.

%% A marker governs only the first catching region after it: the nearest
%% marker above `Line` and below the previous region of the same function
%% (`Prev`, or the line before the function's region for its first region).
%% A region added later under an existing marker therefore needs its own.
governing_marker(Markers, Prev, Line) ->
    case [M || {L, _, _} = M <- Markers, L > Prev, L =< Line] of
        [] -> none;
        In -> lists:last(In)
    end.

%% `{{RegionStart, EndLine}, "name/arity", Form}` for each function. A region
%% starts on the line after the previous function ends, so the comment block
%% (and `-doc`/`-spec`) directly above a function belongs to it.
function_regions(Forms) ->
    {Regions, _} = lists:mapfoldl(
        fun(Form, PrevEnd) ->
            case erl_syntax:type(Form) of
                function ->
                    End = max_line(Form),
                    Name = function_name(Form),
                    {[{{PrevEnd + 1, End}, Name, Form}], End};
                _ ->
                    {[], PrevEnd}
            end
        end,
        0,
        Forms
    ),
    lists:append(Regions).

function_name(Form) ->
    Name =
        case erl_syntax:type(erl_syntax:function_name(Form)) of
            atom -> erl_syntax:atom_literal(erl_syntax:function_name(Form));
            _ -> "?"
        end,
    io_lib:format("~ts/~b", [Name, erl_syntax:function_arity(Form)]).

max_line(Tree) ->
    erl_syntax_lib:fold(fun(N, Acc) -> max(line(N), Acc) end, line(Tree), Tree).

line(Node) ->
    case erl_syntax:get_pos(Node) of
        L when is_integer(L) -> L;
        Anno ->
            try erl_anno:line(Anno) of
                L when is_integer(L) -> L
            catch
                _:_ -> 0
            end
    end.

%% `[{Line, Calls, Protected}]`: every `try ... catch` (body only) and every
%% `catch Expr` in the form, with the dynamic calls its protected body makes
%% outside protect/1, and whether it makes any inside protect/1.
catching_regions(Form) ->
    lists:reverse(
        erl_syntax_lib:fold(
            fun(N, Acc) ->
                case catching_body(N) of
                    none ->
                        Acc;
                    Body ->
                        {Calls, Protected} = dynamic_calls(Body),
                        [{line(N), Calls, Protected} | Acc]
                end
            end,
            [],
            Form
        )
    ).

catching_body(N) ->
    case erl_syntax:type(N) of
        try_expr ->
            case erl_syntax:try_expr_handlers(N) of
                [] -> none;
                _ -> erl_syntax:try_expr_body(N)
            end;
        catch_expr ->
            [erl_syntax:catch_expr_body(N)];
        _ ->
            none
    end.

dynamic_calls(Body) ->
    lists:foldl(fun(T, Acc) -> calls(T, Acc) end, {[], false}, Body).

calls(Node, {Acc, Prot} = State) ->
    case erl_syntax:type(Node) of
        application ->
            Op = erl_syntax:application_operator(Node),
            Args = erl_syntax:application_arguments(Node),
            case is_protect(Op, Args) of
                true ->
                    %% Under protect/1 the call is converted: note it, do not flag.
                    {Acc, true};
                false ->
                    Acc1 =
                        case classify(Op, Args) of
                            none -> Acc;
                            Text -> [Text | Acc]
                        end,
                    descend(Node, {Acc1, Prot})
            end;
        _ ->
            descend(Node, State)
    end.

descend(Node, State) ->
    lists:foldl(
        fun(Group, S) -> lists:foldl(fun calls/2, S, Group) end,
        State,
        erl_syntax:subtrees(Node)
    ).

is_protect(Op, [_]) ->
    remote_name(Op) =:= {beamtalk_class_vars, protect};
is_protect(_, _) ->
    false.

%% A function counts as restoring class variables when it calls protect/1 or
%% restore/1 itself, or takes a snapshot/0 that the module restores elsewhere
%% (beamtalk_erlang_proxy hands its snapshot to the retry helper).
restores(Used, ModuleRestores) ->
    lists:member(protect, Used) orelse lists:member(restore, Used) orelse
        (lists:member(snapshot, Used) andalso ModuleRestores).

%% Which of beamtalk_class_vars:protect/restore/snapshot the trees call. Inside
%% beamtalk_class_vars itself (protect/1 is a catcher too) local calls count.
class_var_calls(Self, Trees) ->
    lists:usort(
        lists:append([
            erl_syntax_lib:fold(
                fun(N, Acc) ->
                    case erl_syntax:type(N) of
                        application ->
                            case call_name(Self, erl_syntax:application_operator(N)) of
                                {beamtalk_class_vars, F} when
                                    F =:= protect; F =:= restore; F =:= snapshot
                                ->
                                    [F | Acc];
                                _ ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
                end,
                [],
                T
            )
         || T <- Trees
        ])
    ).

module_name(Forms) ->
    case
        [
            erl_syntax:atom_value(M)
         || F <- Forms,
            erl_syntax:type(F) =:= attribute,
            erl_syntax:type(erl_syntax:attribute_name(F)) =:= atom,
            erl_syntax:atom_value(erl_syntax:attribute_name(F)) =:= module,
            [M | _] <- [erl_syntax:attribute_arguments(F)],
            erl_syntax:type(M) =:= atom
        ]
    of
        [Name | _] -> Name;
        [] -> undefined
    end.

%% `{Mod, Fun}` for a literal call operator, resolving a local call in `Self`.
call_name(Self, Op) ->
    case erl_syntax:type(Op) of
        atom -> {Self, erl_syntax:atom_value(Op)};
        _ -> remote_name(Op)
    end.

%% `{Mod, Fun}` for a literal remote call operator, else `none`.
remote_name(Op) ->
    case erl_syntax:type(Op) of
        module_qualifier ->
            M = erl_syntax:module_qualifier_argument(Op),
            F = erl_syntax:module_qualifier_body(Op),
            case {erl_syntax:type(M), erl_syntax:type(F)} of
                {atom, atom} -> {erl_syntax:atom_value(M), erl_syntax:atom_value(F)};
                _ -> none
            end;
        _ ->
            none
    end.

%% A human-readable description of a dynamic call, or `none` for a static one.
classify(Op, Args) ->
    case {erl_syntax:type(Op), length(Args)} of
        {variable, N} ->
            io_lib:format("~ts/~b", [erl_syntax:variable_literal(Op), N]);
        {atom, N} when N =:= 2; N =:= 3 ->
            case erl_syntax:atom_value(Op) of
                apply -> classify_apply("apply", Args);
                _ -> none
            end;
        {module_qualifier, N} ->
            case remote_name(Op) of
                {erlang, apply} when N =:= 2; N =:= 3 ->
                    classify_apply("erlang:apply", Args);
                {_, _} ->
                    none;
                none ->
                    io_lib:format("~ts/~b", [erl_prettypr:format(Op), N])
            end;
        _ ->
            none
    end.

classify_apply(Name, [F, _]) ->
    io_lib:format("~ts(~ts, _)", [Name, erl_prettypr:format(F)]);
classify_apply(Name, [M, F, _]) ->
    case {erl_syntax:type(M), erl_syntax:type(F)} of
        {atom, atom} ->
            none;
        _ ->
            io_lib:format("~ts(~ts, ~ts, _)", [
                Name, erl_prettypr:format(M), erl_prettypr:format(F)
            ])
    end.

%% ── Markers ─────────────────────────────────────────────────────────────────
%%
%% `[{Line, Disposition, Reason}]` for each full-line comment
%%     %% bt-catcher-audit: <disposition> - <reason>

scan_markers(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
    {Markers, _} = lists:mapfoldl(
        fun(Text, N) ->
            M =
                case
                    re:run(Text, "^\\s*%+\\s*bt-catcher-audit:\\s*(\\S*)\\s*(.*?)\\s*$", [
                        unicode, {capture, all_but_first, list}
                    ])
                of
                    {match, [D, Rest]} -> [{N, D, strip_reason(Rest)}];
                    nomatch -> []
                end,
            {M, N + 1}
        end,
        1,
        Lines
    ),
    lists:append(Markers).

%% The reason follows a `-` (or em dash) separator.
strip_reason(Rest) ->
    case re:run(Rest, "^(?:-|—)\\s*(.*)$", [unicode, {capture, all_but_first, list}]) of
        {match, [R]} -> R;
        nomatch -> ""
    end.

%% ── File discovery ──────────────────────────────────────────────────────────

goto_repo_root() ->
    case git(["rev-parse", "--show-toplevel"]) of
        [Root] ->
            file:set_cwd(Root);
        _ ->
            io:format(standard_error, "❌ Not a git repository - cannot enumerate sources.~n", []),
            halt(1)
    end.

%% Tracked files plus untracked-but-not-ignored ones, so a new site is caught
%% before it is committed.
runtime_sources() ->
    Pat = "runtime/apps/*/src/*.erl",
    Tracked = git(["ls-files", Pat]),
    Untracked = git(["ls-files", "--others", "--exclude-standard", Pat]),
    [F || F <- lists:usort(Tracked ++ Untracked), filelib:is_regular(F)].

git(Args) ->
    Cmd = "git " ++ lists:join(" ", [quote(A) || A <- Args]) ++ " 2>/dev/null",
    [L || L <- string:lexemes(os:cmd(Cmd), "\n"), L =/= ""].

quote(A) -> "'" ++ A ++ "'".
