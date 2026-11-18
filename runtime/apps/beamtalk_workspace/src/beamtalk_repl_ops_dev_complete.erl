%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_dev_complete).

%%% **DDD Context:** Workspace Context

-moduledoc """
Completion and hover engines behind the developer read-surface ops (ADR 0085): keyword and class-name completion, type-annotation completion, receiver classification, method collection with internal/hidden filtering, and the live-registry hover resolver. Pure functions over already-decoded arguments; the request-param decoding stays in `beamtalk_repl_ops_dev`. Moved verbatim out of it (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Methods inherited from Object that are internal implementation protocol and
%% should not appear in user-facing completions. Long-term, reflection methods
%% (fieldNames, fieldAt:, fieldAt:put:) should move to Mirror classes.
-define(COMPLETION_HIDDEN_METHODS, [
    %% Abstract-class stubs — only meaningful inside a class body
    subclassResponsibility,
    notImplemented,
    %% Message-not-understood handler — internal dispatch protocol
    'doesNotUnderstand:args:',
    %% Reflection intrinsics — long-term these should move to a Mirror class
    fieldNames,
    'fieldAt:',
    'fieldAt:put:',
    %% Object constructors — valid on class objects but confusing on instances
    %% ("foo" new dispatches via basicNew which most classes don't support directly)
    new,
    'new:',
    %% Dynamic dispatch — advanced/meta protocol, rarely called directly
    'perform:',
    'perform:withArguments:'
]).

%% The class a method editor edits and which side (`self`'s receiver kind).
-type self_context() :: undefined | {atom(), instance | class}.
-export_type([self_context/0]).

-export([
    get_completions/1,
    get_context_completions/1, get_context_completions/2, get_context_completions/3,
    hover_docs/5
]).

%% White-box tests of the completion keyword vocabulary, the receiver
%% classifier, the hover resolver and the local-overrides-wins dedup fix.
-ifdef(TEST).
-export([
    builtin_keywords/0,
    collect_all_methods/2,
    classify_receiver/2,
    classify_receiver/3,
    hover_docs/4,
    hover_markdown/2,
    resolve_chain_type/2
]).
-endif.

-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) ->
    [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    class_name_completions(Prefix) ++
        [
            Kw
         || Kw <- builtin_keywords(),
            binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
            PrefixStr =/= ""
        ].

-doc """
Live class/protocol name candidates matching `Prefix` (a raw prefix match,
`<<>>` matches every class — unlike `get_completions/1`, which special-cases
`<<>>` to `[]` for the no-receiver top-level case).

Factored out of `get_completions/1` so `type_annotation_completions/2`
can offer the same class/protocol candidates it already offered
before aliases were considered — filtering out cross-package `internal`
classes (ADR 0071 Phase 5) exactly as `get_completions/1` does.
""".
-spec class_name_completions(binary()) -> [binary()].
class_name_completions(Prefix) ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    %% ADR 0071 Phase 5: Filter internal classes from cross-package completions.
    %% The REPL runs in an implicit package (nil), so internal classes from any
    %% named package (e.g. stdlib) are excluded. Internal classes with no package
    %% (user-loaded) are kept since they share the REPL's nil package.
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_runtime_api:class_name(Pid),
                case is_cross_package_internal(Pid) of
                    true -> false;
                    false -> {true, atom_to_binary(Name, utf8)}
                end
            catch
                _:_ -> false
            end
        end,
        ClassPids
    ),
    [Name || Name <- ClassNames, prefix_match(Name, Prefix)].

-doc "Prefix match that treats `<<>>` as matching everything (unlike binary:match/2, which rejects an empty pattern).".
-spec prefix_match(binary(), binary()) -> boolean().
prefix_match(_Name, <<>>) ->
    true;
prefix_match(Name, Prefix) ->
    binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}.

-doc """
Context-aware completion: parses the line to find a receiver and returns
matching method selectors.  Falls back to get_completions/1 when
no receiver is detected.  Wrapper with no binding context or live alias names.
""".
-spec get_context_completions(binary()) -> [binary()].
get_context_completions(Line) ->
    get_context_completions(Line, #{}, []).

-doc "Context-aware completion with session bindings for variable lookup. Wrapper with no live alias names.".
-spec get_context_completions(binary(), map()) -> [binary()].
get_context_completions(Line, Bindings) ->
    get_context_completions(Line, Bindings, []).

-doc """
Context-aware completion with session bindings and this session's live type
alias names.

In type-annotation position (immediately after `::`) this offers class/
protocol names plus `AliasNames` — the live-REPL counterpart to the static
LSP path's `add_alias_name_completions` (`completion_provider.rs`).
`AliasNames` is the session's carried-over + current-turn `type Name = ...`
declarations (ADR 0108 Phase 8), threaded in by the
`complete` op handler via `get_session_alias_names/1`. Everywhere else this
behaves exactly like `get_context_completions/2`.
""".
-spec get_context_completions(binary(), map(), [binary()]) -> [binary()].
get_context_completions(<<>>, _Bindings, _AliasNames) ->
    [];
get_context_completions(Line, Bindings, AliasNames) when is_binary(Line) ->
    Parsed = beamtalk_repl_editor_context:parse_receiver_and_prefix(Line),
    case type_annotation_prefix(Parsed) of
        {true, TypePrefix} ->
            type_annotation_completions(TypePrefix, AliasNames);
        false ->
            case Parsed of
                {undefined, Prefix} ->
                    %% No receiver — use standard prefix completion
                    get_completions(Prefix);
                {expression, ReceiverExpr, Prefix} ->
                    %% Multi-token receiver expression — resolve via chain type inference
                    case resolve_chain_type(ReceiverExpr, Bindings) of
                        {ok, ClassName, class} -> complete_class_methods(ClassName, Prefix);
                        {ok, ClassName, instance} -> complete_instance_methods(ClassName, Prefix);
                        undefined -> []
                    end;
                {Receiver, <<>>} ->
                    %% Empty prefix with receiver (e.g., "Integer ") — return all methods
                    MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
                    lists:usort([atom_to_binary(S, utf8) || S <- MethodSelectors]);
                {Receiver, Prefix} ->
                    %% Receiver with prefix — look up methods filtered by prefix
                    MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
                    lists:usort([
                        atom_to_binary(S, utf8)
                     || S <- MethodSelectors,
                        binary:match(atom_to_binary(S, utf8), Prefix) =:= {0, byte_size(Prefix)}
                    ])
            end
    end.

-doc """
Detect type-annotation position (immediately after `::`) from a
`parse_receiver_and_prefix/1` result and extract the partial type name typed
so far.

`::` is built from identifier chars (`beamtalk_repl_editor_context:is_identifier_char/1` treats `:` as one,
so keyword selectors like `ifTrue:` complete as a unit) so it surfaces in a
few shapes depending on whether a space follows `::` and whether the receiver
is a single token or a multi-token expression:
  `"policy :: R"`                   -> `{expression, <<"policy ::">>, <<"R">>}` (space, single-token receiver)
  `"policy ::R"`                    -> `{<<"policy">>, <<"::R">>}` (no space, single-token receiver)
  `"deposit: amount :: I"`          -> `{expression, <<"deposit: amount ::">>, <<"I">>}` (space, keyword receiver)
  `"deposit: amount ::I"`           -> `{expression, <<"deposit: amount">>, <<"::I">>}` (no space, keyword receiver —
                                        the attached `::` lands in Prefix, not the receiver expression, since
                                        parse_receiver_and_prefix/1 always splits on the last whitespace run)
All four are annotation position; checked by looking at *both* halves — a
literal `::`-suffixed receiver expression, or a `::`-prefixed completion
token — so the no-space attached form isn't missed just because the receiver
happens to be a keyword-send expression rather than a bare token. A
single-colon keyword-selector prefix (e.g. `"foo:"`) is not annotation
position, since it lacks the second colon.
""".
-spec type_annotation_prefix({binary() | undefined, binary()} | {expression, binary(), binary()}) ->
    {true, binary()} | false.
type_annotation_prefix({expression, ReceiverExpr, Prefix}) ->
    case strip_double_colon_prefix(Prefix) of
        {true, _} = Result ->
            Result;
        false ->
            case ends_with_double_colon(ReceiverExpr) of
                true -> {true, Prefix};
                false -> false
            end
    end;
type_annotation_prefix({Receiver, Prefix}) when is_binary(Receiver) ->
    strip_double_colon_prefix(Prefix);
type_annotation_prefix(_) ->
    %% Covers {undefined, Prefix} — a bare `::` with no preceding token
    %% (nothing before it on the line) is not valid Beamtalk syntax, so
    %% falling through to ordinary prefix completion (which finds nothing
    %% for a "::"-containing prefix) is correct, not just a missed case.
    false.

-spec strip_double_colon_prefix(binary()) -> {true, binary()} | false.
strip_double_colon_prefix(<<"::", Rest/binary>>) ->
    {true, Rest};
strip_double_colon_prefix(_) ->
    false.

-spec ends_with_double_colon(binary()) -> boolean().
ends_with_double_colon(Bin) ->
    byte_size(Bin) >= 2 andalso binary:part(Bin, byte_size(Bin) - 2, 2) =:= <<"::">>.

-doc """
Type-annotation-position completions: class/protocol names (the
same live registry `class_name_completions/1` already offers) plus this
session's live type alias names, both filtered by the partial type name
typed so far. Deduplicated and sorted via `lists:usort/1` — a class and an
alias could share a name transiently across a live redefinition.
""".
-spec type_annotation_completions(binary(), [binary()]) -> [binary()].
type_annotation_completions(Prefix, AliasNames) ->
    AliasCompletions = [Name || Name <- AliasNames, prefix_match(Name, Prefix)],
    lists:usort(class_name_completions(Prefix) ++ AliasCompletions).

-doc """
Resolve hover documentation for the hovered token.

`Code` is the editor line up to and including the hovered token. When that
token is a keyword part (`fetch:` / `options:`), `Before` (the editor lines
above, ending in a newline) and `Suffix` (the rest of the buffer after the
token) let `keyword_send_at/3` recover the whole keyword send — even one split
across lines — so any keyword resolves the full selector (`fetch:options:`) on
its single-token receiver (BT-4343). Every other token takes the unary /
class-name path below, which reads only `Code`: we reuse
`parse_receiver_and_prefix/1` (the completion parser) so the trailing token is
the hovered word and the token before it (if any) is its receiver:

  * bare class name (`Counter`)            → that class's live docs
  * `Receiver selector` (`42 factorial`)   → that method's live docs
  * multi-token receiver expression        → class-name hover only (full
                                             receiver inference is future work)

Returns the formatted markdown binary, or `<<>>` when nothing resolves (the
client shows no tooltip): a missing class/selector is a `{error, _}` return that
degrades to `<<>>`, since hover is advisory. This does not *catch* exceptions —
an unexpected throw in `beamtalk_repl_docs` propagates and surfaces to the
`rpc`-attached client as `{badrpc, _}`, which the Elixir layer maps to an empty
hover, so the system still degrades cleanly.

`SelfCtx` (BT-4463) is `{ClassName, instance | class}` when the hover comes
from a method editor, so a `self` / `super` receiver resolves against that
class (see `classify_receiver/3`); `undefined` elsewhere.
""".
-spec hover_docs(binary(), binary(), binary(), map(), self_context()) -> binary().
hover_docs(Before, Code, Suffix, Bindings, SelfCtx) ->
    case beamtalk_repl_editor_context:keyword_send_at(Before, Code, Suffix) of
        {ok, Receiver, Selector} -> hover_method(Receiver, Selector, Bindings, SelfCtx);
        none -> <<>>;
        not_keyword -> hover_token(Code, Bindings, SelfCtx)
    end.
-ifdef(TEST).
-doc false.
-spec hover_docs(binary(), binary(), binary(), map()) -> binary().
hover_docs(Before, Code, Suffix, Bindings) ->
    hover_docs(Before, Code, Suffix, Bindings, undefined).
-endif.

-spec hover_token(binary(), map(), self_context()) -> binary().
hover_token(Code, Bindings, SelfCtx) ->
    case beamtalk_repl_editor_context:parse_receiver_and_prefix(Code) of
        {expression, _ReceiverExpr, Word} ->
            %% Multi-token receiver: only offer hover when the hovered token is
            %% itself a class name. Selector hover on an inferred expression
            %% receiver is deferred (full receiver inference).
            hover_class(Word, Bindings);
        {_Receiver, <<>>} ->
            %% Empty trailing token (line ends in whitespace). Defensive: the JS
            %% `wordAt` always ends `code` at the last char of an identifier, so
            %% this is unreachable from the client path — nothing to hover.
            <<>>;
        {undefined, Word} ->
            hover_class(Word, Bindings);
        {Receiver, Word} ->
            hover_method(Receiver, Word, Bindings, SelfCtx)
    end.

-doc "Hover docs for a bare token that is a class name; `<<>>` otherwise.".
-spec hover_class(binary(), map()) -> binary().
hover_class(<<>>, _Bindings) ->
    <<>>;
hover_class(Word, Bindings) ->
    case classify_receiver(Word, Bindings) of
        {class, ClassName} ->
            hover_format(class, fun() -> beamtalk_repl_docs:format_class_docs(ClassName) end);
        %% A bare lowercase token (a binding/instance) is intentionally NOT
        %% hovered in v1: hover targets class names + explicit receiver→selector.
        _ ->
            <<>>
    end.

-doc """
Hover docs for a `Receiver selector` pair, picking the instance-side or
class-side doc lookup from how the receiver classifies (mirroring how
`complete` offers class-side vs instance-side selectors for a receiver).
""".
-spec hover_method(binary(), binary(), map(), self_context()) -> binary().
hover_method(Receiver, Selector, Bindings, SelfCtx) ->
    case classify_receiver(Receiver, Bindings, SelfCtx) of
        {class, ClassName} ->
            hover_format(method, fun() ->
                beamtalk_repl_docs:format_method_doc_class_side(ClassName, Selector)
            end);
        {instance, ClassName} ->
            hover_format(method, fun() ->
                beamtalk_repl_docs:format_method_doc(ClassName, Selector)
            end);
        undefined ->
            <<>>
    end.

-doc """
Run a `{ok, Bin} | {error, _}` doc lookup, mapping anything but ok to `<<>>`,
and shape the `:help` text into the hover's Markdown (`hover_markdown/2`).
""".
-spec hover_format(class | method, fun(() -> {ok, binary()} | {error, term()})) -> binary().
hover_format(Kind, Lookup) ->
    case Lookup() of
        {ok, Docs} when is_binary(Docs) -> hover_markdown(Kind, Docs);
        _ -> <<>>
    end.

-doc """
Turn `:help` text into the Markdown the cockpit hover tooltip renders (BT-4344).

The hover reuses `beamtalk_repl_docs` — the text `:help` prints — but the
tooltip renders it as Markdown (`BtAttach.DocFormat`), so a method's `///`
doc-comment shows its `## Options` heading and bullets rendered rather than as
raw source. Only the doc-comment is author Markdown; the `:help` layout around
it is plain, line-oriented text that Markdown would reflow into one paragraph.
So that layout is emitted as an indented code block (every non-blank line
indented four spaces), which renders preformatted exactly as before:

  * `method` — `format_method_output/4` lays out a header block (`== C >> sel ==`,
    modifier/inherited lines, the signature) with no blank line inside it, then
    `\n\n` and the doc-comment. The header becomes a code block; the doc-comment
    after the first blank line passes through as Markdown.
  * `class` — the class header, class comment and method listing are
    interleaved line-by-line, so the whole text stays a code block (unchanged
    from the pre-BT-4344 plain-text tooltip).
""".
-spec hover_markdown(class | method, binary()) -> binary().
hover_markdown(_Kind, <<>>) ->
    <<>>;
hover_markdown(class, Docs) ->
    hover_preformatted(Docs);
hover_markdown(method, Docs) ->
    case binary:split(Docs, <<"\n\n">>) of
        [Head, Doc] -> <<(hover_preformatted(Head))/binary, "\n\n", Doc/binary>>;
        [Head] -> hover_preformatted(Head)
    end.

%% Indent every non-blank line four spaces: a Markdown indented code block.
-spec hover_preformatted(binary()) -> binary().
hover_preformatted(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    iolist_to_binary(lists:join(<<"\n">>, [hover_indent(L) || L <- Lines])).

-spec hover_indent(binary()) -> binary().
hover_indent(<<>>) -> <<>>;
hover_indent(Line) -> <<"    ", Line/binary>>.

%%% Chain resolution

-doc """
Resolve the type at the end of a send chain using static return-type metadata.

Tokenises the expression, classifies the receiver, then walks the chain by
looking up each send's return type in `method_return_types` on the class registry.

When the unary tokenizer fails, tries the binary/mixed tokenizer.
When both tokenizers fail (e.g. parenthesised subexpressions, keyword sends
mid-chain), falls back to compiler-based type resolution (ADR 0045 Option C).
""".
-spec resolve_chain_type(binary(), map()) -> {ok, atom(), instance | class} | undefined.
resolve_chain_type(Expr, Bindings) ->
    case beamtalk_repl_editor_context:tokenise_send_chain(Expr) of
        {ok, ReceiverToken, Selectors} ->
            case classify_receiver(ReceiverToken, Bindings) of
                {instance, ClassName} ->
                    beamtalk_repl_editor_context:walk_chain(ClassName, Selectors);
                {class, ClassName} ->
                    beamtalk_repl_editor_context:walk_chain_class(ClassName, Selectors);
                undefined ->
                    undefined
            end;
        error ->
            %% Try binary/mixed chain tokenizer before the compiler port.
            case beamtalk_repl_editor_context:tokenise_binary_chain(Expr) of
                {ok, ReceiverToken, Hops} ->
                    case classify_receiver(ReceiverToken, Bindings) of
                        {instance, ClassName} ->
                            beamtalk_repl_editor_context:walk_mixed_chain(ClassName, Hops);
                        {class, ClassName} ->
                            beamtalk_repl_editor_context:walk_mixed_chain_class(ClassName, Hops);
                        undefined ->
                            %% Receiver parsed but not classifiable — fall back to compiler.
                            resolve_type_via_compiler(Expr)
                    end;
                error ->
                    %% Tokeniser can't parse the expression — try the compiler port.
                    resolve_type_via_compiler(Expr)
            end
    end.

-doc """
Compiler-based type resolution fallback for complex expressions.

Sends the expression to the Rust compiler via the port. The compiler parses
it fully, runs type inference, and returns the type of the last expression.
Falls back to `undefined' if the compiler is unavailable or the type is unknown.

On a release node without a compiler (ADR 0125 §1.5) this step is skipped
outright: `complete` itself stays available, with only its tokeniser-driven
completions, rather than failing the whole op.
""".
-spec resolve_type_via_compiler(binary()) -> {ok, atom(), instance | class} | undefined.
resolve_type_via_compiler(Expr) ->
    case beamtalk_capability:available(completion_type_inference) of
        false ->
            undefined;
        true ->
            try beamtalk_compiler:resolve_completion_type(Expr) of
                {ok, ClassName} -> {ok, ClassName, instance};
                {error, type_unknown} -> undefined
            catch
                _:_ -> undefined
            end
    end.

-doc """
Remove methods that are internal Object protocol and should not appear
in user-facing completions. See ?COMPLETION_HIDDEN_METHODS.
""".
-spec filter_hidden_methods([atom()]) -> [atom()].
filter_hidden_methods(Selectors) ->
    Hidden = sets:from_list(?COMPLETION_HIDDEN_METHODS, [{version, 2}]),
    [S || S <- Selectors, not sets:is_element(S, Hidden)].

-doc """
Return all instance methods of a class filtered by the given prefix.
ADR 0071 Phase 5: Also filters internal methods from cross-package classes.
""".
-spec complete_instance_methods(atom(), binary()) -> [binary()].
complete_instance_methods(ClassName, Prefix) ->
    MethodSelectors = filter_hidden_methods(collect_all_methods(ClassName, 0)),
    Filtered = filter_internal_methods(MethodSelectors, ClassName),
    filter_by_prefix(Filtered, Prefix).

-doc """
Return all class-side methods of a class filtered by the given prefix.

Includes class methods plus ProtoObject instance methods (e.g. `class`,
`respondsTo:`) since class objects are also objects.
ADR 0071 Phase 5: Also filters internal methods from cross-package classes.
""".
-spec complete_class_methods(atom(), binary()) -> [binary()].
complete_class_methods(ClassName, Prefix) ->
    ProtoObjMethods = filter_hidden_methods(collect_all_methods('ProtoObject', 0)),
    ClassMethods = collect_all_class_methods(ClassName, 0),
    %% Filter internal class methods from the target class
    FilteredClassMethods = filter_internal_methods(ClassMethods, ClassName, class),
    filter_by_prefix(FilteredClassMethods ++ ProtoObjMethods, Prefix).

-doc "Filter method selectors by a binary prefix, returning sorted binaries.".
-spec filter_by_prefix([atom()], binary()) -> [binary()].
filter_by_prefix(MethodSelectors, Prefix) ->
    All = [atom_to_binary(S, utf8) || S <- MethodSelectors],
    case Prefix of
        <<>> ->
            lists:usort(All);
        _ ->
            lists:usort([
                M
             || M <- All,
                binary:match(M, Prefix) =:= {0, byte_size(Prefix)}
            ])
    end.

-doc """
Get method selectors for a given receiver token.
For class-name receivers (uppercase), returns only class-side methods (via hierarchy
walk). Instance methods are excluded — they cannot be called on the class object.
For instance receivers (literals, bindings), returns instance methods.
""".
-spec get_methods_for_receiver(binary(), map()) -> [atom()].
get_methods_for_receiver(Receiver, Bindings) when is_binary(Receiver) ->
    case classify_receiver(Receiver, Bindings) of
        undefined ->
            [];
        {class, ClassName} ->
            %% Class-object receiver: class-side methods plus the ProtoObject instance
            %% methods that are genuinely callable on any object (including class objects).
            %% Most instance methods are excluded — they can't be called on the class object
            %% and would cause misleading completions that always error.
            %% ProtoObject methods (e.g. `class`) are included because class objects ARE
            %% objects and `ClassName class` is meaningful (returns the metaclass).
            ProtoObjMethods = filter_hidden_methods(collect_all_methods('ProtoObject', 0)),
            %% ADR 0071 Phase 5: Filter internal class methods from cross-package classes
            FilteredClassMethods = filter_internal_methods(
                collect_all_class_methods(ClassName, 0), ClassName, class
            ),
            FilteredClassMethods ++ ProtoObjMethods;
        {instance, ClassName} ->
            %% ADR 0071 Phase 5: Filter internal instance methods from cross-package classes
            Selectors = filter_hidden_methods(collect_all_methods(ClassName, 0)),
            filter_internal_methods(Selectors, ClassName)
    end.

-doc """
Classify a receiver token as a class object or instance, with optional binding lookup.
Returns {class, ClassName} for class-object receivers (uppercase class names),
{instance, ClassName} for instance receivers (literals, bindings), or undefined.
""".
-spec classify_receiver(binary(), map()) -> {class, atom()} | {instance, atom()} | undefined.
classify_receiver(Receiver, Bindings) ->
    classify_receiver(Receiver, Bindings, undefined).

-doc """
`classify_receiver/2` that also resolves `self` / `super` against the class a
method editor edits (BT-4463). `SelfCtx` is `{ClassName, instance | class}`:
`self` is that class's instance (or the class object itself on the class
side); `super` is the same on its superclass, `undefined` for a root class.
Without a context (`undefined`) `self` / `super` are ordinary names.
""".
-spec classify_receiver(binary(), map(), self_context()) ->
    {class, atom()} | {instance, atom()} | undefined.
classify_receiver(<<"self">>, _Bindings, {ClassName, Side}) ->
    side_receiver(ClassName, Side);
classify_receiver(<<"super">>, _Bindings, {ClassName, Side}) ->
    case superclass_name(ClassName) of
        {ok, Super} -> side_receiver(Super, Side);
        none -> undefined
    end;
classify_receiver(Receiver, Bindings, _SelfCtx) ->
    classify_receiver_token(Receiver, Bindings).

-spec side_receiver(atom(), instance | class) -> {class, atom()} | {instance, atom()}.
side_receiver(ClassName, instance) -> {instance, ClassName};
side_receiver(ClassName, class) -> {class, ClassName}.

-spec superclass_name(atom()) -> {ok, atom()} | none.
superclass_name(ClassName) ->
    try beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            none;
        Pid ->
            case beamtalk_runtime_api:superclass(Pid) of
                none -> none;
                Super when is_atom(Super) -> {ok, Super}
            end
    catch
        _:_ -> none
    end.

-spec literal_instance('Integer' | 'String' | undefined) -> {instance, atom()} | undefined.
literal_instance(undefined) ->
    undefined;
literal_instance(Class) ->
    case maybe_class(Class) of
        undefined -> undefined;
        ClassName -> {instance, ClassName}
    end.

-spec classify_receiver_token(binary(), map()) ->
    {class, atom()} | {instance, atom()} | undefined.
classify_receiver_token(<<>>, _Bindings) ->
    undefined;
classify_receiver_token(<<H, _/binary>> = Receiver, Bindings) when H >= $A, H =< $Z ->
    %% Starts with uppercase — first try class registry, then fall back to bindings.
    %% The fallback handles uppercase global bindings like `Transcript` that are
    %% workspace actors, not class names.
    case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
        {ok, ClassName} ->
            case
                try
                    beamtalk_runtime_api:whereis_class(ClassName)
                catch
                    _:_ -> undefined
                end
            of
                undefined ->
                    %% Not a class — check if it's a named binding (e.g. Transcript)
                    classify_by_binding(ClassName, Bindings);
                _Pid ->
                    {class, ClassName}
            end;
        {error, _} ->
            undefined
    end;
classify_receiver_token(<<H, _/binary>> = Receiver, _Bindings) when H >= $0, H =< $9 ->
    literal_instance(beamtalk_repl_editor_context:receiver_literal_class(Receiver));
classify_receiver_token(<<$-, D, _/binary>> = Receiver, _Bindings) when D >= $0, D =< $9 ->
    %% Negative integer literal (`-5`): the lexer folds the sign into the token.
    literal_instance(beamtalk_repl_editor_context:receiver_literal_class(Receiver));
classify_receiver_token(<<$", _/binary>> = Receiver, _Bindings) ->
    %% String literal — complete String instance methods
    literal_instance(beamtalk_repl_editor_context:receiver_literal_class(Receiver));
classify_receiver_token(Receiver, Bindings) ->
    %% Check for package-qualified class name (e.g. "json@Parser")
    case binary:match(Receiver, <<"@">>) of
        nomatch ->
            %% Lowercase identifier — look up in bindings to find the class
            case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
                {ok, VarAtom} -> classify_by_binding(VarAtom, Bindings);
                {error, _} -> undefined
            end;
        _ ->
            case beamtalk_repl_errors:resolve_qualified_class_name(Receiver) of
                {ok, ClassName} ->
                    case
                        try
                            beamtalk_runtime_api:whereis_class(ClassName)
                        catch
                            _:_ -> undefined
                        end
                    of
                        undefined -> undefined;
                        _Pid -> {class, ClassName}
                    end;
                {error, _} ->
                    undefined
            end
    end.

-doc """
Classify a receiver by looking it up in the bindings map.
Uses beamtalk_runtime_api:primitive_class_of/1 as the canonical type classifier,
which handles actors, tagged maps, primitives, symbols, blocks, nil, etc.
""".
-spec classify_by_binding(atom(), map()) -> {instance, atom()} | undefined.
classify_by_binding(VarAtom, Bindings) ->
    case maps:find(VarAtom, Bindings) of
        {ok, Value} ->
            ClassName = beamtalk_runtime_api:primitive_class_of(Value),
            case maybe_class(ClassName) of
                undefined -> undefined;
                _ -> {instance, ClassName}
            end;
        error ->
            undefined
    end.

-spec maybe_class(atom()) -> atom() | undefined.
maybe_class(ClassName) ->
    case
        try
            beamtalk_runtime_api:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined -> undefined;
        _Pid -> ClassName
    end.

-doc """
Collect all class-side method selectors for a class by walking the superclass chain.
Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
""".
-spec collect_all_class_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_class_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:local_class_methods/1).

-doc """
Collect all instance method selectors for a class by walking the superclass chain.
Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
""".
-spec collect_all_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:class_methods/1).

-doc """
Walk the superclass chain collecting methods via a caller-supplied getter fun.

Local methods shadow inherited ones (Smalltalk-style override): a selector
redefined by a subclass appears exactly once in the result, attributed to
the subclass. Previously `LocalMethods ++ InheritedMethods` with no
dedup at all, so an overridden selector appeared twice — a real user-visible
bug in method-listing completions.

Built on `beamtalk_hierarchy:walk_ancestors/3` — one depth guard,
one `?LOG_WARNING`-on-exhaustion policy, matching every other consolidated
hierarchy walker (`beamtalk_behaviour_intrinsics:walk_hierarchy/3`,
`beamtalk_hierarchy_docs`). `Depth` is accepted for API compatibility with
existing callers (always 0) but the walk itself always starts fresh at 0 —
`walk_ancestors/3` owns depth-counting internally.

On depth exhaustion (`?MAX_HIERARCHY_DEPTH`, a hierarchy cycle) returns the
partial set of selectors collected from the ancestors actually visited
before the guard tripped — not `[]` — and logs a `?LOG_WARNING`
naming the ancestor where the cycle was detected.
""".
-spec collect_methods_with_fun(atom(), non_neg_integer(), fun((pid()) -> [atom()])) -> [atom()].
collect_methods_with_fun(ClassName, _Depth, Fun) ->
    StepFun = fun({CurrentClassName, AccMap}, _WalkDepth) ->
        case
            try
                beamtalk_runtime_api:whereis_class(CurrentClassName)
            catch
                _:_ -> undefined
            end
        of
            undefined ->
                {found, AccMap};
            ClassPid ->
                LocalMethods =
                    %% bt-catcher-audit: not-applicable-no-block - Fun is an Erlang reflection fun
                    %% over ClassPid
                    try
                        Fun(ClassPid)
                    catch
                        _:_ -> []
                    end,
                LocalMap = maps:from_keys(LocalMethods, true),
                %% AccMap already reflects every closer (subclass-side)
                %% selector winning over farther ancestors; keep that
                %% invariant as this (farther) level's LocalMap is folded in.
                NewAcc = maps:merge(LocalMap, AccMap),
                Superclass =
                    try
                        beamtalk_runtime_api:superclass(ClassPid)
                    catch
                        _:_ -> none
                    end,
                case Superclass of
                    none -> {found, NewAcc};
                    Super -> {next, {Super, NewAcc}}
                end
        end
    end,
    Ctx = #{start => ClassName, node_name => fun({Name, _Acc}) -> Name end},
    case
        beamtalk_hierarchy:walk_or_warn({ClassName, #{}}, StepFun, collect_methods_with_fun, Ctx)
    of
        {found, ResultMap} ->
            maps:keys(ResultMap);
        {cycle, {_CycleClassName, PartialMap}} ->
            maps:keys(PartialMap);
        not_found ->
            %% Unreachable: StepFun above always resolves to {found, _} — an
            %% unregistered class or a `none` superclass is translated to a
            %% terminal {found, _}, never a bare `none` node.
            erlang:error({unreachable, not_found, ClassName})
    end.

-doc """
ADR 0071 Phase 5: Check if a class is internal and belongs to a different
package than the REPL's implicit nil package. Returns true for internal classes
from named packages (e.g. stdlib). Returns false for public classes and for
internal classes with no package (user-loaded in REPL, same nil package).
""".
-spec is_cross_package_internal(pid()) -> boolean().
is_cross_package_internal(Pid) ->
    try
        case beamtalk_runtime_api:is_internal(Pid) of
            false ->
                false;
            true ->
                %% Internal class — check if it belongs to a named package
                %% using the ETS module table for fast lookup (avoids gen_server
                %% calls that can timeout on mock class processes in tests).
                ClassName = beamtalk_runtime_api:class_name(Pid),
                case beamtalk_class_metadata:lookup_module(ClassName) of
                    not_found ->
                        false;
                    {ok, Mod} ->
                        case beamtalk_module_name:package_of(Mod) of
                            undefined -> false;
                            _ -> true
                        end
                end
        end
    catch
        _:_ -> false
    end.

-doc """
ADR 0071 Phase 5: Filter internal methods from a list of method selectors.
Reads method visibility from __beamtalk_meta/0 and removes internal methods
when the class belongs to a different package than the REPL.

Walks the class hierarchy to also filter inherited internal methods from
ancestor classes. Checks method_info for instance side and class_method_info
for class side.

Uses the class hierarchy table (ETS) for module lookup to avoid gen_server
calls that can timeout on mock class processes in tests.
""".
-spec filter_internal_methods([atom()], atom()) -> [atom()].
filter_internal_methods(Selectors, ClassName) ->
    filter_internal_methods(Selectors, ClassName, instance).

-spec filter_internal_methods([atom()], atom(), instance | class) -> [atom()].
filter_internal_methods(Selectors, ClassName, Side) ->
    %% Collect all internal selectors from the class and its ancestors.
    InternalSet = collect_internal_selectors(ClassName, Side, 0),
    case maps:size(InternalSet) of
        0 -> Selectors;
        _ -> [S || S <- Selectors, not maps:is_key(S, InternalSet)]
    end.

-doc "Walk the hierarchy collecting internal selectors from cross-package classes.".
-spec collect_internal_selectors(atom(), instance | class, non_neg_integer()) -> map().
collect_internal_selectors(_ClassName, _Side, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    #{};
collect_internal_selectors(ClassName, Side, Depth) ->
    Local = collect_internal_selectors_for_class(ClassName, Side),
    Super =
        case
            try
                beamtalk_class_metadata:lookup_superclass(ClassName)
            catch
                _:_ -> not_found
            end
        of
            not_found -> #{};
            {ok, none} -> #{};
            {ok, SuperName} -> collect_internal_selectors(SuperName, Side, Depth + 1)
        end,
    maps:merge(Super, Local).

-doc "Get internal selectors from a single class's meta (if cross-package).".
-spec collect_internal_selectors_for_class(atom(), instance | class) -> map().
collect_internal_selectors_for_class(ClassName, Side) ->
    case
        try
            beamtalk_class_metadata:lookup_module(ClassName)
        catch
            _:_ -> not_found
        end
    of
        not_found ->
            #{};
        {ok, Mod} ->
            case beamtalk_module_name:package_of(Mod) of
                undefined ->
                    #{};
                _ ->
                    Meta = read_class_meta(Mod),
                    InfoKey =
                        case Side of
                            instance -> method_info;
                            class -> class_method_info
                        end,
                    MethodInfo = maps:get(InfoKey, Meta, #{}),
                    maps:filter(
                        fun
                            (_Sel, Info) when is_map(Info) ->
                                maps:get(visibility, Info, public) =:= internal;
                            (_Sel, _Info) ->
                                false
                        end,
                        MethodInfo
                    )
            end
    end.

-doc """
Read __beamtalk_meta/0 from a compiled module.
Returns the meta map or #{} if unavailable.
""".
-spec read_class_meta(atom()) -> map().
read_class_meta(Module) ->
    case beamtalk_class_metadata:read_meta(Module) of
        {ok, M} -> M;
        not_available -> #{}
    end.

%% This list and the LSP's static `add_keyword_completions`
%% (`crates/beamtalk-core/src/queries/completion_provider.rs`) are two
%% engines that cannot literally share code (Erlang vs. Rust) but should
%% offer the same control-flow vocabulary — a keyword missing from one and
%% not the other is a completion gap on whichever surface is missing it.
%% `match:`/`matchExhaustive:`/`if:then:else:` were LSP-only before this.
%% This list's core control-flow keywords are pinned against
%% `add_keyword_completions` by a shared conformance fixture
%% (`runtime/apps/beamtalk_workspace/test/fixtures/completion_keyword_vocabulary_corpus.json`),
%% asserted by `builtin_keywords_covers_shared_vocabulary_corpus_test` in
%% `beamtalk_repl_ops_dev_tests.erl` and
%% `keyword_completions_cover_shared_vocabulary_corpus` on the Rust side —
%% keep both lists a superset of the corpus.
-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [
        <<"self">>,
        <<"super">>,
        <<"true">>,
        <<"false">>,
        <<"nil">>,
        <<"ifTrue:">>,
        <<"ifFalse:">>,
        <<"ifTrue:ifFalse:">>,
        <<"whileTrue:">>,
        <<"timesRepeat:">>,
        <<"subclass:">>,
        <<"spawn">>,
        <<"new">>,
        <<"match:">>,
        <<"matchExhaustive:">>,
        <<"if:then:else:">>
    ].
