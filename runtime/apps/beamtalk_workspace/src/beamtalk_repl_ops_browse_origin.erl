%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_origin).

%%% **DDD Context:** Workspace Context

-moduledoc """
Origin, package and protocol-bucket classification for the system-browser ops (ADR 0096): where a class or selector comes from (`stdlib` / `project` / dependency / `native`), which package owns it, and which protocol bucket a selector falls into. Pure classification; moved verbatim out of `beamtalk_repl_ops_browse` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    origin_for_provenance/2,
    origin_module/2,
    origin_of/1,
    package_of/2,
    package_origin/2,
    path_origin/1,
    protocol_for_selector/5,
    source_origin_of/2
]).

%% Origin of a package's native modules: `stdlib` for the stdlib app, else the
%% project/dependency split from the package name vs the workspace's own package.
-spec package_origin(atom(), binary()) -> binary().
package_origin(beamtalk_stdlib, _PkgName) ->
    <<"stdlib">>;
package_origin(_App, PkgName) ->
    case project_package_name() of
        ProjectPkg when is_binary(ProjectPkg), ProjectPkg =:= PkgName ->
            <<"project">>;
        _ ->
            <<"dependency">>
    end.

%%% ====================================================================
%%% Divergence — origin / disk_differs
%%% ====================================================================

%% origin :: both | static | runtime (ADR 0096). A class/row with a backing disk
%% source that is loaded in the image is `both`; a file-less / ClassBuilder class
%% (no disk source) is `runtime`. The `static` case (on disk, not loaded) is the
%% cold-mode answer and is produced by the static (Rust) layer, not here — a
%% class enumerated by `all_classes/0` is loaded by definition.
-spec origin_of(binary() | null) -> binary().
origin_of(null) -> <<"runtime">>;
origin_of(SourceFile) when is_binary(SourceFile) -> <<"both">>.

%% Class source origin: the *classification* of where a class comes from —
%% `project`, `dependency`, or `stdlib`. Used for IDE badges. This is
%% orthogonal to the *package name* (see `package_of/2`): the package is
%% carried in a separate `package` row field, never packed into this value.
-spec source_origin_of(atom(), binary() | null) -> binary().
source_origin_of(ModName, SourceFile) when is_atom(ModName) ->
    %% Stdlib always wins first.
    case beamtalk_class_registry:is_stdlib_module(ModName) of
        true ->
            <<"stdlib">>;
        false ->
            classify_origin(
                beamtalk_module_name:package_of(ModName), project_package_name(), SourceFile
            )
    end.

%% Classification rule: the module's package segment is the *primary*
%% signal, not the filesystem path. `bt@{pkg}@{class}` encodes the owning package;
%% comparing it to the project's own package name correctly labels dependency
%% classes whose sources happen to resolve under the project tree, and avoids
%% dumping everything into `project` when `project_path` metadata is missing.
%%
%%   - package present, ≠ project package         -> dependency
%%   - package present, == project package        -> project
%%   - package present but project package unknown -> path fallback (then project)
%%   - no package segment                          -> path fallback (null -> project)
-spec classify_origin(binary() | undefined, binary() | null, binary() | null) -> binary().
classify_origin(Pkg, ProjectPkg, _SourceFile) when
    is_binary(Pkg), is_binary(ProjectPkg)
->
    case Pkg =:= ProjectPkg of
        true -> <<"project">>;
        false -> <<"dependency">>
    end;
classify_origin(Pkg, _ProjectPkg, SourceFile) when is_binary(Pkg) ->
    %% Package known but project package unresolvable: defer to the path check.
    path_origin(SourceFile);
classify_origin(undefined, _ProjectPkg, SourceFile) ->
    %% No package segment in the module atom: fall back to the path-prefix check.
    path_origin(SourceFile).

%% Secondary, path-prefix fallback used only when the package can't decide.
-spec path_origin(binary() | null) -> binary().
path_origin(null) ->
    <<"project">>;
path_origin(SourceFile) when is_binary(SourceFile) ->
    case classify_source_origin(SourceFile) of
        project -> <<"project">>;
        dependency -> <<"dependency">>
    end.

%% Package name a class lives in, for ALL origins: `stdlib` for stdlib
%% classes, the dependency package for dependencies, and the project's own
%% package name for project classes. Orthogonal to `source_origin_of/2`'s
%% classification. The package comes from the module atom (`bt@{pkg}@{class}`)
%% when encoded there; for project classes whose module carries no package
%% segment, fall back to the workspace's configured package name. `null` when no
%% package can be determined (e.g. unknown dependency, no workspace metadata).
-spec package_of(atom(), binary()) -> binary() | null.
package_of(ModName, <<"stdlib">>) when is_atom(ModName) ->
    <<"stdlib">>;
package_of(ModName, <<"dependency">>) when is_atom(ModName) ->
    case beamtalk_module_name:package_of(ModName) of
        undefined -> null;
        Pkg -> Pkg
    end;
package_of(ModName, <<"project">>) when is_atom(ModName) ->
    case beamtalk_module_name:package_of(ModName) of
        undefined -> project_package_name();
        Pkg -> Pkg
    end.

%% The workspace's configured package name (project's own package), or `null`
%% when no workspace metadata is available (startup, no project).
-spec project_package_name() -> binary() | null.
project_package_name() ->
    case beamtalk_workspace_meta:get_package_name() of
        Name when is_binary(Name) -> Name;
        _ -> null
    end.

%% Secondary fallback: determine if a source file belongs to the
%% project or a dependency by filesystem path. Used only by `classify_origin/3`
%% when the module's package segment can't decide. Falls back to `project` when
%% metadata is unavailable (startup, no workspace) — a wrong "project" badge is
%% less confusing than a wrong "dependency" badge.
-spec classify_source_origin(binary()) -> project | dependency.
classify_source_origin(SourceFile) ->
    SourceStr = binary_to_list(SourceFile),
    AbsPath = filename:absname(SourceStr),
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
            ProjectRoot = filename:absname(binary_to_list(ProjectPath)),
            case beamtalk_workspace_file:path_inside(ProjectRoot, AbsPath) of
                true -> project;
                false -> dependency
            end;
        _ ->
            project
    end.

%% Per-selector origin from xref provenance: a runtime-installed selector
%% (put_method, class_builder) is runtime-only; a class_body / extension selector
%% in a file-backed class is `both`.
-spec origin_for_provenance(beamtalk_xref:provenance(), binary() | null) -> binary().
origin_for_provenance(put_method, _SourceFile) -> <<"runtime">>;
origin_for_provenance(class_builder, _SourceFile) -> <<"runtime">>;
origin_for_provenance(_Provenance, null) -> <<"runtime">>;
origin_for_provenance(_Provenance, _SourceFile) -> <<"both">>.

%% The module whose package + on-disk source determine a class's origin badges
%% (category, origin, source_origin). For an ordinary class this is its own
%% dispatch module. A protocol class object (ADR 0068) is dispatched by the shared
%% `beamtalk_protocol_object` module, which carries no package and no
%% `beamtalk_source` — so origin would wrongly read "runtime"/"project" for every
%% protocol, stdlib ones included. Resolve the protocol's *defining*
%% module from the registry (recorded at registration via the codegen `module`
%% key) so a stdlib protocol like `Printable` (`bt@stdlib@printable`) is badged
%% stdlib and a project protocol is badged project. Falls back to the dispatch
%% module for non-protocols and for protocols compiled before the `module` key
%% existed.
-spec origin_module(atom(), atom()) -> atom().
origin_module(ClassName, ModName) ->
    case beamtalk_runtime_api:protocol_info(ClassName) of
        #{module := M} when is_atom(M) -> M;
        _ -> ModName
    end.

%% Decide a selector's browser protocol bucket (ADR 0096 §"Protocol of a
%% selector": *the xref row's provenance plus the method's declared protocol
%% category*). The decision is layered, most-authoritative source first — a
%% declared category is a fact, a provenance is a fact, a name match is a guess:
%%
%%   1. **Pragma** — the method's *declared* protocol category. This is the
%%      ground truth a class author assigns (the spike's hand-written categories:
%%      "operations", "accessing", "instance creation", …). Beamtalk method
%%      metadata does not yet carry a category field — `beamtalk_xref:method_info/3`
%%      returns only line / source_status / provenance (no category) — so
%%      `declared_protocol/1` reads the (currently absent) `category` key off the
%%      xref `method_info` map: it is the single wiring point for when xref adds
%%      one (a `<category: ...>`-style pragma or an xref `category` field). Until
%%      then the key is absent, the lookup is `undefined`, and the decision falls
%%      through to the source-fact / name tiers.
%%   2. **xref-extension source** — `provenance = extension` (ADR 0066 package
%%      extension) is a *source* fact: the selector was added to this class from
%%      another package, so it belongs in the "extensions" bucket regardless of
%%      its name. This outranks the name heuristic (a fact beats a guess).
%%   3. **Heuristic** — a Pharo-convention bucket derived from the *selector name*
%%      (accessors → "accessing", `is`/`has` tests → "testing", `print*` →
%%      "printing", …). This is what gives ordinary `class_body` methods a
%%      meaningful protocol instead of collapsing every non-extension selector
%%      into one bucket. Selectors that match no convention fall into Pharo's
%%      "as yet unclassified".
-spec protocol_for_selector(
    atom(),
    beamtalk_xref:method_info() | undefined,
    beamtalk_xref:provenance(),
    beamtalk_xref:source_status(),
    boolean()
) -> binary().
protocol_for_selector(Selector, Info, protocol, SourceStatus, ClassSide) ->
    %% ADR 0127 §12 — a method flattened from a `uses:`d protocol is filed under
    %% that protocol, "<Protocol> › <category>" when the provided method carries
    %% its own category, else the protocol name alone. A class-body override or
    %% an `excluding:`-ed selector has a different provenance and never gets here.
    case protocol_origin(Info) of
        Origin when is_binary(Origin) ->
            case declared_protocol(Info) of
                Category when is_binary(Category) ->
                    <<Origin/binary, " › "/utf8, Category/binary>>;
                undefined ->
                    Origin
            end;
        undefined ->
            %% Malformed row (protocol provenance without an origin): fall back,
            %% still honouring the Tier-1 declared category like every other row.
            case declared_protocol(Info) of
                Declared when is_binary(Declared) -> Declared;
                undefined -> protocol_from_source(Selector, protocol, SourceStatus, ClassSide)
            end
    end;
protocol_for_selector(Selector, Info, Provenance, SourceStatus, ClassSide) ->
    case declared_protocol(Info) of
        Category when is_binary(Category) ->
            %% Tier 1 — declared category (author ground truth).
            Category;
        undefined ->
            protocol_from_source(Selector, Provenance, SourceStatus, ClassSide)
    end.

%% The protocol name a flattened method came from (xref `origin`), as a binary,
%% or `undefined` when the row carries none.
-spec protocol_origin(beamtalk_xref:method_info() | undefined) -> binary() | undefined.
protocol_origin(Info) when is_map(Info) ->
    case maps:get(origin, Info, undefined) of
        Origin when is_atom(Origin), Origin =/= undefined, Origin =/= nil ->
            atom_to_binary(Origin, utf8);
        _ ->
            undefined
    end;
protocol_origin(_Info) ->
    undefined.

%% Tier 1 hook: the method's declared protocol category, or `undefined` when none
%% is declared. Beamtalk's `beamtalk_xref:method_info/3` does not carry a category
%% field today (see `protocol_for_selector/5`), so the `category` key is absent
%% and this returns `undefined`. It is the one place to wire a real category once
%% xref (or a method pragma) surfaces one — reading it here keeps the decision
%% precedence (declared > provenance > name) in a single function with no other
%% call-site change. ADR 0096 §"Protocol of a selector".
-spec declared_protocol(beamtalk_xref:method_info() | undefined) -> binary() | undefined.
declared_protocol(Info) when is_map(Info) ->
    case maps:get(category, Info, undefined) of
        Cat when is_binary(Cat), byte_size(Cat) > 0 -> Cat;
        _ -> undefined
    end;
declared_protocol(_Info) ->
    undefined.

%% Tiers 2–3: no declared category, so decide from the source facts we do have.
%% A compiler-generated accessor (source_status = synthetic) is "accessing" by
%% construction. An extension-provenance selector is "extensions" (source fact).
%% Otherwise fall to the selector-name heuristic.
-spec protocol_from_source(
    atom(), beamtalk_xref:provenance(), beamtalk_xref:source_status(), boolean()
) -> binary().
protocol_from_source(_Selector, _Provenance, synthetic, ClassSide) ->
    %% Synthetic methods are compiler-generated. Today that means value-type
    %% field accessors (ADR 0087), always instance-side, bucketed "accessing"
    %% by construction. (The actor constructors `new`/`new:`/`spawn`/`spawn:`
    %% are real, source-backed — hence `indexed`, not `synthetic` — class
    %% methods on `Actor`, not synthetic entries.) The `class_side => true`
    %% branch is kept for any future
    %% class-side synthetic entry point (and is exercised directly by the
    %% EUnit tests below with fabricated xref rows) — classify by
    %% `class_side`, not the selector name, so an instance-side synthetic slot
    %% that happens to be named `new`/`spawn` (e.g. `state: new :: Integer =
    %% 0`) is not misclassified as "instance creation".
    case ClassSide of
        true -> <<"instance creation">>;
        false -> <<"accessing">>
    end;
protocol_from_source(_Selector, extension, _SourceStatus, _ClassSide) ->
    %% Tier 2 — package-extension provenance (ADR 0066) is a source fact that
    %% outranks the name heuristic.
    <<"extensions">>;
protocol_from_source(Selector, _Provenance, _SourceStatus, _ClassSide) ->
    %% Tier 3 — Pharo-convention name heuristic.
    protocol_from_name(Selector).

%% Pharo-convention protocol buckets derived from the selector name. Mirrors the
%% category vocabulary the spike (`spikes/cockpit-ux-spike/image.js`) and Pharo's
%% default protocols use. A guess, not a fact — only reached when no declared
%% category and no source-fact bucket applies. Unrecognised selectors land in
%% "as yet unclassified" (Pharo's name for the default bucket).
-spec protocol_from_name(atom()) -> binary().
protocol_from_name(Selector) ->
    Name = atom_to_binary(Selector, utf8),
    classify_name(Name).

-spec classify_name(binary()) -> binary().
classify_name(Name) ->
    Keyword = binary:match(Name, <<":">>) =/= nomatch,
    case Name of
        %% Object protocol / printing.
        _ when
            Name =:= <<"printString">>;
            Name =:= <<"displayString">>;
            Name =:= <<"printOn:">>;
            Name =:= <<"displayOn:">>
        ->
            <<"printing">>;
        %% Equality / comparison.
        _ when
            Name =:= <<"=">>;
            Name =:= <<"==">>;
            Name =:= <<"~=">>;
            Name =:= <<"hash">>;
            Name =:= <<"<">>;
            Name =:= <<">">>;
            Name =:= <<"<=">>;
            Name =:= <<">=">>;
            Name =:= <<"<=>">>
        ->
            <<"comparing">>;
        _ ->
            classify_by_prefix(Name, Keyword)
    end.

%% Prefix / shape conventions (checked after the exact-name special cases).
%%
%% Prefix matches respect camelCase *word boundaries* via `has_word_prefix/2`:
%% the prefix must be followed by an uppercase letter (`asString`, `toArray`) or
%% end the selector (`do:`, `add:`). This avoids the classic false positives a
%% naive `binary:match` prefix check produces — `toggle` is not a conversion,
%% `address` is not adding, `island` / `issue` are not tests, `domain` is not
%% enumeration. A guess remains a guess, but a disciplined one.
-spec classify_by_prefix(binary(), boolean()) -> binary().
classify_by_prefix(Name, Keyword) ->
    Prefixes = [
        %% {prefix, protocol} — first boundary-respecting match wins.
        {<<"is">>, <<"testing">>},
        {<<"has">>, <<"testing">>},
        {<<"includes">>, <<"testing">>},
        {<<"respondsTo">>, <<"testing">>},
        {<<"print">>, <<"printing">>},
        {<<"display">>, <<"printing">>},
        {<<"initialize">>, <<"initialization">>},
        {<<"init">>, <<"initialization">>},
        {<<"as">>, <<"converting">>},
        {<<"to">>, <<"converting">>},
        {<<"do">>, <<"enumerating">>},
        {<<"collect">>, <<"enumerating">>},
        {<<"select">>, <<"enumerating">>},
        {<<"reject">>, <<"enumerating">>},
        {<<"detect">>, <<"enumerating">>},
        {<<"inject">>, <<"enumerating">>},
        {<<"add">>, <<"adding">>},
        {<<"remove">>, <<"removing">>}
    ],
    case match_prefix(Name, Prefixes) of
        {ok, Protocol} -> Protocol;
        nomatch -> classify_shape(Name, Keyword)
    end.

%% First prefix in the list that matches `Name` at a camelCase word boundary.
-spec match_prefix(binary(), [{binary(), binary()}]) -> {ok, binary()} | nomatch.
match_prefix(_Name, []) ->
    nomatch;
match_prefix(Name, [{Prefix, Protocol} | Rest]) ->
    case has_word_prefix(Name, Prefix) of
        true -> {ok, Protocol};
        false -> match_prefix(Name, Rest)
    end.

%% True when `Name` starts with `Prefix` AND the prefix is a whole camelCase
%% word: the next character is an uppercase ASCII letter (a new word starts) or a
%% `:` (a keyword selector like `do:`), or the prefix consumes the whole name
%% (`add`, `remove` as bare unary words). A lowercase continuation (`toggle`,
%% `address`) is NOT a boundary and does not match.
-spec has_word_prefix(binary(), binary()) -> boolean().
has_word_prefix(Name, Prefix) ->
    PLen = byte_size(Prefix),
    case Name of
        <<Prefix:PLen/binary>> ->
            true;
        <<Prefix:PLen/binary, Next, _/binary>> ->
            (Next >= $A andalso Next =< $Z) orelse Next =:= $:;
        _ ->
            false
    end.

%% Final shape rule: a keyword selector (has a `:`) that matched no more specific
%% convention reads as an "operations" message — the spike's default protocol for
%% state-changing keyword sends (`setTo:`, `deposit:`). Everything else (a bare
%% unary word, a binary operator) is left "as yet unclassified": guessing
%% getter-vs-operation from a bare name like `increment` / `reset` / `value` is
%% unreliable, so we do not (genuine field accessors are caught earlier via
%% `source_status = synthetic`, which is a fact, not a guess). Pharo's default
%% bucket name is "as yet unclassified".
-spec classify_shape(binary(), boolean()) -> binary().
classify_shape(_Name, true) ->
    <<"operations">>;
classify_shape(_Name, false) ->
    <<"as yet unclassified">>.
