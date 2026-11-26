%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_paths).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The one path policy for ops that take a path inside the project (BT-5205).

A path an op receives from a client names a file under the project root. It
must be relative, must not contain a `..` segment (on `/` or `\`), and must
not be empty or hold a NUL byte. The ops that read or change the working
tree (`git-*`, `reload-file`) all check it here, so no op carries its own
copy of the rule.

This is a check on the text, not on the file system: it refuses a path that
could name anything outside the root, and leaves it to the caller (git, the
loader) to resolve what is inside it.
""".

-export([check_relative/1]).

-define(IS_LETTER(L), ((L >= $a andalso L =< $z) orelse (L >= $A andalso L =< $Z))).

-export_type([refusal/0]).

-type refusal() :: not_a_string | empty | nul | absolute | parent_segment | whole_tree.

-doc """
`ok` for a project-relative path, else why it is refused: `not_a_string`,
`empty`, `nul` (a NUL byte), `absolute` (a root, a UNC path or a drive such as `C:\\`;
a name like `a:b.bt` is an ordinary relative file name) or `parent_segment` (a `..` segment) or `whole_tree` (a path that names the root itself,
such as `.` or `./`, which a git write would read as "every file").
""".
-spec check_relative(term()) -> ok | {error, refusal()}.
check_relative(Path) when not is_binary(Path) ->
    {error, not_a_string};
check_relative(<<>>) ->
    {error, empty};
check_relative(Path) ->
    case binary:match(Path, <<0>>) of
        nomatch -> check_segments(Path);
        _ -> {error, nul}
    end.

-spec check_segments(binary()) -> ok | {error, absolute | parent_segment | whole_tree}.
check_segments(Path) ->
    Segments = binary:split(Path, [<<"/">>, <<"\\">>], [global]),
    case lists:member(<<"..">>, Segments) of
        true ->
            {error, parent_segment};
        false ->
            case is_absolute(Path) of
                true ->
                    {error, absolute};
                false ->
                    case [S || S <- Segments, S =/= <<>>, S =/= <<".">>] of
                        [] -> {error, whole_tree};
                        _ -> ok
                    end
            end
    end.

%% Absolute on any platform: a leading separator (POSIX root, UNC) or a drive
%% letter, whatever the node's own OS says, since the client's text is judged
%% the same everywhere.
-spec is_absolute(binary()) -> boolean().
is_absolute(<<C, _/binary>>) when C =:= $/; C =:= $\\ -> true;
is_absolute(<<L, $:>>) when ?IS_LETTER(L) -> true;
is_absolute(<<L, $:, Sep, _/binary>>) when ?IS_LETTER(L), Sep =:= $/ orelse Sep =:= $\\ -> true;
is_absolute(Path) -> filename:pathtype(binary_to_list(Path)) =/= relative.
