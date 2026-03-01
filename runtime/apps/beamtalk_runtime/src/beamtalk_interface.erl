%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc BeamtalkInterface actor - Beamtalk system introspection singleton.
%%%
%%% **DDD Context:** Runtime
%%%
%%% This gen_server implements the BeamtalkInterface actor that backs the
%%% `Beamtalk` global object. It provides system-wide introspection:
%%% - Class registry access (allClasses, classNamed:)
%%% - Workspace globals (globals) — immutable snapshot of the class registry
%%% - Version information (version)
%%%
%%% This is Phase 1 of ADR 0010: Global Objects and Singleton Dispatch.
%%% The actor coexists with the current beamtalk_stdlib.erl dispatch.
%%% Migration to use this actor will happen in later phases.
%%%
%%% ## Message Protocol
%%%
%%% BeamtalkInterface responds to these selectors:
%%% - `allClasses` - Returns list of all class names
%%% - `classNamed:` - Returns class info or error for given name
%%% - `globals` - Returns class registry snapshot as a Dictionary
%%% - `help:` - Returns formatted class documentation
%%% - `help:selector:` - Returns formatted method documentation
%%% - `version` - Returns Beamtalk version string
%%%
%%% All methods are synchronous (gen_server:call) as they return values
%%% that callers need immediately.
%%%
%%% ## Example Usage
%%%
%%% ```erlang
%%% %% Start the BeamtalkInterface actor
%%% {ok, Pid} = beamtalk_interface:start_link(),
%%%
%%% %% Query all classes
%%% Classes = gen_server:call(Pid, {allClasses, []}),
%%%
%%% %% Look up a specific class
%%% ClassPid = gen_server:call(Pid, {'classNamed:', [<<"Counter">>]}),
%%%
%%% %% Get workspace globals
%%% Globals = gen_server:call(Pid, {globals, []}),
%%%
%%% %% Get version
%%% Version = gen_server:call(Pid, {version, []}).
%%% ```
-module(beamtalk_interface).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    start_link/2,
    has_method/1,
    class_info/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type selector() :: atom().

-record(beamtalk_interface_state, {
    version :: binary(),
    % Workspace name for future phases
    workspace :: atom() | undefined
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the BeamtalkInterface actor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the BeamtalkInterface actor with options.
%%
%% Options:
%%   {workspace, atom()} - Workspace name (for future phases)
%%   {version, binary()} - Version string (defaults to OTP app vsn, or "unknown")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a named BeamtalkInterface for workspace use.
%% Registers with the given name via gen_server name registration.
-spec start_link({local, atom()}, proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(ServerName, Opts) ->
    gen_server:start_link(ServerName, ?MODULE, Opts, []).

%% @doc Check if BeamtalkInterface supports a given method selector.
%%
%% Used by dispatch compatibility layer. Returns true if the selector
%% corresponds to one of the supported methods.
-spec has_method(selector()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method('help:') -> true;
has_method('help:selector:') -> true;
has_method(version) -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for BeamtalkInterface.
%%
%% Used by beamtalk_stdlib to register this singleton's class.
%% Single source of truth for class name, superclass, and method table.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'BeamtalkInterface',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            allClasses => #{arity => 0},
            'classNamed:' => #{arity => 1},
            globals => #{arity => 0},
            'help:' => #{arity => 1, is_sealed => true},
            'help:selector:' => #{arity => 2, is_sealed => true},
            version => #{arity => 0}
        },
        class_methods => #{},
        fields => []
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the BeamtalkInterface actor state.
-spec init(proplists:proplist()) -> {ok, #beamtalk_interface_state{}}.
init(Opts) ->
    VersionDefault =
        case application:get_key(beamtalk_runtime, vsn) of
            {ok, Vsn} -> list_to_binary(Vsn);
            _ -> <<"unknown">>
        end,
    Version = proplists:get_value(version, Opts, VersionDefault),
    Workspace = proplists:get_value(workspace, Opts, undefined),

    State = #beamtalk_interface_state{
        version = Version,
        workspace = Workspace
    },
    {ok, State}.

%% @doc Handle synchronous method calls.
%%
%% Dispatches to the appropriate method implementation based on selector.
-spec handle_call(term(), {pid(), term()}, #beamtalk_interface_state{}) ->
    {reply, term(), #beamtalk_interface_state{}}.

%% allClasses - Returns list of all class names
handle_call({allClasses, []}, _From, State) ->
    Classes = handle_all_classes(),
    {reply, Classes, State};
%% classNamed: - Returns class pid or error for given class name
handle_call({'classNamed:', [ClassName]}, _From, State) ->
    Result = handle_class_named(ClassName),
    {reply, Result, State};
%% globals - Returns class registry snapshot as a Dictionary
handle_call({globals, []}, _From, State) ->
    Globals = handle_globals(),
    {reply, Globals, State};
%% help: - Returns formatted class documentation
handle_call({'help:', [ClassArg]}, _From, State) ->
    case handle_help(ClassArg) of
        {error, Err} -> {reply, {error, Err}, State};
        Result -> {reply, Result, State}
    end;
%% help:selector: - Returns formatted method documentation
handle_call({'help:selector:', [ClassArg, SelectorArg]}, _From, State) ->
    case handle_help_selector(ClassArg, SelectorArg) of
        {error, Err} -> {reply, {error, Err}, State};
        Result -> {reply, Result, State}
    end;
%% version - Returns Beamtalk version string
handle_call({version, []}, _From, State) ->
    Version = State#beamtalk_interface_state.version,
    {reply, Version, State};
%% Unknown selector - try hierarchy walk for inherited methods (e.g. printString, class, respondsTo:)
handle_call({Selector, Args}, From, State) when is_atom(Selector), is_list(Args) ->
    GenServerPid = self(),
    spawn(fun() ->
        Self = {beamtalk_object, 'BeamtalkInterface', ?MODULE, GenServerPid},
        try
            beamtalk_dispatch:lookup(
                Selector,
                Args,
                Self,
                #{'$beamtalk_class' => 'BeamtalkInterface'},
                'BeamtalkInterface'
            )
        of
            {reply, Value, _NewState} ->
                gen_server:reply(From, Value);
            {error, Error} ->
                gen_server:reply(From, {error, Error})
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("BeamtalkInterface dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'BeamtalkInterface'),
                gen_server:reply(
                    From,
                    {error,
                        beamtalk_error:with_selector(
                            beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                        )}
                )
        end
    end),
    {noreply, State};
%% Unknown call format
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_hint(Error0, <<"Expected {Selector, Args} format">>),
    Error2 = beamtalk_error:with_details(Error1, #{request => Request}),
    {reply, {error, Error2}, State}.

%% @doc Handle asynchronous messages.
%%
%% BT-374: Workspace binding dispatch uses beamtalk_actor:async_send/4 which
%% sends {Selector, Args, FuturePid} as a cast. We dispatch to the same
%% method implementations as handle_call and resolve the Future with the result.
-spec handle_cast(term(), #beamtalk_interface_state{}) -> {noreply, #beamtalk_interface_state{}}.
%% Actor protocol: {Selector, Args, FuturePid} from beamtalk_actor:async_send/4
handle_cast({allClasses, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_all_classes(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({'classNamed:', [ClassName], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_class_named(ClassName) of
        {error, Err} ->
            beamtalk_future:reject(FuturePid, Err);
        Result ->
            beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({globals, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = handle_globals(),
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({'help:', [ClassArg], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_help(ClassArg) of
        {error, Err} -> beamtalk_future:reject(FuturePid, Err);
        Result -> beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({'help:selector:', [ClassArg, SelectorArg], FuturePid}, State) when is_pid(FuturePid) ->
    case handle_help_selector(ClassArg, SelectorArg) of
        {error, Err} -> beamtalk_future:reject(FuturePid, Err);
        Result -> beamtalk_future:resolve(FuturePid, Result)
    end,
    {noreply, State};
handle_cast({version, [], FuturePid}, State) when is_pid(FuturePid) ->
    Result = State#beamtalk_interface_state.version,
    beamtalk_future:resolve(FuturePid, Result),
    {noreply, State};
handle_cast({Selector, Args, FuturePid}, State) when
    is_pid(FuturePid), is_atom(Selector), is_list(Args)
->
    %% Unknown selector - try hierarchy walk for inherited methods
    GenServerPid = self(),
    spawn(fun() ->
        Self = {beamtalk_object, 'BeamtalkInterface', ?MODULE, GenServerPid},
        try
            beamtalk_dispatch:lookup(
                Selector,
                Args,
                Self,
                #{'$beamtalk_class' => 'BeamtalkInterface'},
                'BeamtalkInterface'
            )
        of
            {reply, Value, _NewState} ->
                beamtalk_future:resolve(FuturePid, Value);
            {error, Error} ->
                beamtalk_future:reject(FuturePid, Error)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("BeamtalkInterface dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'BeamtalkInterface'),
                beamtalk_future:reject(
                    FuturePid,
                    beamtalk_error:with_selector(
                        beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                    )
                )
        end
    end),
    {noreply, State};
handle_cast(Msg, State) ->
    ?LOG_WARNING("BeamtalkInterface received unexpected cast", #{message => Msg}),
    {noreply, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #beamtalk_interface_state{}) -> {noreply, #beamtalk_interface_state{}}.
handle_info(Info, State) ->
    ?LOG_DEBUG("BeamtalkInterface received info", #{info => Info}),
    {noreply, State}.

%% @doc Handle process termination.
-spec terminate(term(), #beamtalk_interface_state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change during hot reload.
-spec code_change(term(), #beamtalk_interface_state{}, term()) ->
    {ok, #beamtalk_interface_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal method implementations
%%====================================================================

%% @doc Get all registered class names.
%%
%% Delegates to live_class_entries/0 to get live class pid entries,
%% then extracts just the class names.
-spec handle_all_classes() -> [atom()].
handle_all_classes() ->
    [Name || {Name, _Mod, _Pid} <- live_class_entries()].

%% @doc Look up a class by name.
%%
%% Delegates to beamtalk_class_registry:whereis_class/1.
%% Returns the class as a beamtalk_object record (for dispatch), or nil if not found.
%% BT-246: Wraps result in {beamtalk_object, ClassTag, ModuleName, Pid} tuple
%% where ClassTag is 'ClassName class' (e.g., 'Point class') for is_class_object detection.
-spec handle_class_named(binary() | atom()) -> tuple() | 'nil' | {error, #beamtalk_error{}}.
handle_class_named(ClassName) when is_binary(ClassName) ->
    %% Use binary_to_existing_atom to avoid creating atoms at runtime
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_class_named(ClassAtom)
    catch
        error:badarg ->
            nil
    end;
handle_class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            nil;
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            %% BT-246: Use class object tag for is_class_object detection
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end;
handle_class_named(_ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, 'classNamed:'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"classNamed: expects an atom or binary class name">>
    ),
    {error, Error2}.

%% @doc Get workspace global bindings as an immutable snapshot of the class registry.
%%
%% Returns a map from binary class names to class object references.
%% This provides read-only access to the class namespace at the moment of the call.
-spec handle_globals() -> map().
handle_globals() ->
    lists:foldl(
        fun({Name, ModuleName, Pid}, Acc) ->
            ClassTag = beamtalk_class_registry:class_object_tag(Name),
            ClassObj = {beamtalk_object, ClassTag, ModuleName, Pid},
            maps:put(Name, ClassObj, Acc)
        end,
        #{},
        live_class_entries()
    ).

%% @doc Format class documentation for help:.
%%
%% Accepts a class object ({beamtalk_object, ...}) or an atom class name.
%% Returns formatted documentation text or {error, #beamtalk_error{}}.
-spec handle_help(term()) -> binary() | {error, #beamtalk_error{}}.
handle_help(ClassArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    try
                        format_class_help(ClassName, ClassPid)
                    catch
                        exit:{noproc, _} -> {error, make_class_not_found_error(ClassName)};
                        exit:{timeout, _} -> {error, make_class_not_found_error(ClassName)}
                    end
            end
    end.

%% @doc Format method documentation for help:selector:.
%%
%% Accepts a class object or atom for the class, and a symbol for the selector.
-spec handle_help_selector(term(), atom()) -> binary() | {error, #beamtalk_error{}}.
handle_help_selector(ClassArg, SelectorArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    case ensure_atom(SelectorArg) of
                        {error, Err} ->
                            {error, Err};
                        SelectorAtom ->
                            try
                                case beamtalk_method_resolver:resolve(ClassPid, SelectorAtom) of
                                    nil ->
                                        {error,
                                            make_method_not_found_error(ClassName, SelectorAtom)};
                                    MethodObj when is_map(MethodObj) ->
                                        DefiningClass = find_defining_class(ClassPid, SelectorAtom),
                                        format_method_help(
                                            ClassName, SelectorAtom, DefiningClass, MethodObj
                                        )
                                end
                            catch
                                exit:{noproc, _} -> {error, make_class_not_found_error(ClassName)};
                                exit:{timeout, _} -> {error, make_class_not_found_error(ClassName)}
                            end
                    end
            end
    end.

%% @private Resolve a class argument to an atom class name.
%% Handles class objects, atoms, and binaries.
-spec resolve_class_name(term()) -> {ok, atom()} | {error, #beamtalk_error{}}.
resolve_class_name({beamtalk_object, _ClassTag, _Mod, ClassPid}) when is_pid(ClassPid) ->
    try
        Name = beamtalk_object_class:class_name(ClassPid),
        {ok, Name}
    catch
        exit:{noproc, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            Error1 = beamtalk_error:with_message(Error0, <<"Class process no longer alive">>),
            {error, Error1};
        exit:{timeout, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            Error1 = beamtalk_error:with_message(Error0, <<"Class process not responding">>),
            {error, Error1}
    end;
resolve_class_name(Name) when is_atom(Name) ->
    {ok, Name};
resolve_class_name(Name) when is_binary(Name) ->
    try
        {ok, binary_to_existing_atom(Name, utf8)}
    catch
        error:badarg ->
            {error, make_class_not_found_error(Name)}
    end;
resolve_class_name(_Other) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_message(Error0, <<"Expected a class or symbol argument">>),
    {error, Error1}.

%% @private Ensure a selector argument is an existing atom.
%% Uses binary_to_existing_atom to prevent atom table exhaustion from user input.
-spec ensure_atom(atom() | binary()) -> atom() | {error, #beamtalk_error{}}.
ensure_atom(A) when is_atom(A) -> A;
ensure_atom(B) when is_binary(B) ->
    try
        binary_to_existing_atom(B, utf8)
    catch
        error:badarg ->
            Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'help:selector:'),
            Err2 = beamtalk_error:with_message(
                Err1,
                iolist_to_binary([<<"Unknown selector: ">>, B])
            ),
            {error, Err2}
    end.

%% @private Format class-level help output.
-spec format_class_help(atom(), pid()) -> binary().
format_class_help(ClassName, ClassPid) ->
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    IsSealed = gen_server:call(ClassPid, is_sealed, 5000),
    IsAbstract = gen_server:call(ClassPid, is_abstract, 5000),
    ModuleDoc =
        case gen_server:call(ClassPid, get_doc, 5000) of
            none -> none;
            Doc when is_binary(Doc) -> Doc
        end,

    %% Collect flattened methods (local shadow inherited)
    Flattened = collect_flattened_methods(ClassName, ClassPid),

    %% Split into own vs inherited
    {Own, Inherited} = maps:fold(
        fun(Selector, {DefClass, MethodInfo}, {OwnAcc, InhAcc}) ->
            case DefClass of
                ClassName ->
                    MethodSealed = maps:get(is_sealed, MethodInfo, false),
                    {[{Selector, MethodSealed} | OwnAcc], InhAcc};
                _ ->
                    {OwnAcc, [{Selector, DefClass} | InhAcc]}
            end
        end,
        {[], []},
        Flattened
    ),

    %% Get signatures for own methods
    OwnSelectors = lists:sort([S || {S, _} <- Own]),
    SealedMap = maps:from_list(Own),
    OwnDocs = lists:map(
        fun(Sel) ->
            {Sig, _Doc} = get_method_sig(ClassPid, Sel),
            IsSealedMethod = maps:get(Sel, SealedMap, false),
            {Sel, Sig, IsSealedMethod}
        end,
        OwnSelectors
    ),

    %% Group inherited by defining class
    InheritedGrouped = group_by_class(lists:sort(Inherited)),

    %% Build output
    NameBin = atom_to_binary(ClassName, utf8),
    Header =
        case Superclass of
            none ->
                iolist_to_binary([<<"== ">>, NameBin, <<" ==">>]);
            Super ->
                iolist_to_binary([
                    <<"== ">>, NameBin, <<" < ">>, atom_to_binary(Super, utf8), <<" ==">>
                ])
        end,

    ModifierPart =
        case {IsSealed, IsAbstract} of
            {true, true} -> <<"\n[sealed] [abstract]">>;
            {true, false} -> <<"\n[sealed]">>;
            {false, true} -> <<"\n[abstract]">>;
            {false, false} -> <<>>
        end,

    DocPart =
        case ModuleDoc of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n">>, Text])
        end,

    OwnMethodsPart =
        case OwnDocs of
            [] ->
                <<>>;
            _ ->
                Lines = lists:map(
                    fun
                        ({_Sel, Sig, true}) ->
                            iolist_to_binary([<<"  ">>, Sig, <<" [sealed]">>]);
                        ({_Sel, Sig, false}) ->
                            iolist_to_binary([<<"  ">>, Sig])
                    end,
                    OwnDocs
                ),
                iolist_to_binary([<<"\nInstance methods:\n">>, lists:join(<<"\n">>, Lines)])
        end,

    InheritedParts = lists:map(
        fun({FromClass, Selectors}) ->
            Count = length(Selectors),
            Summary =
                case Count =< 5 of
                    true ->
                        lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- Selectors]);
                    false ->
                        {First3, _} = lists:split(3, Selectors),
                        Remaining = Count - 3,
                        iolist_to_binary([
                            lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- First3]),
                            <<", ... (">>,
                            integer_to_binary(Remaining),
                            <<" more)">>
                        ])
                end,
            iolist_to_binary([
                <<"\nInherited from ">>,
                atom_to_binary(FromClass, utf8),
                <<" (">>,
                integer_to_binary(Count),
                <<" methods): ">>,
                Summary
            ])
        end,
        InheritedGrouped
    ),

    HintPart = <<"\nUse Beamtalk help: ClassName selector: #method for method details.">>,

    AllParts = [Header, ModifierPart, DocPart, OwnMethodsPart | InheritedParts] ++ [HintPart],
    iolist_to_binary(
        lists:filter(
            fun
                (<<>>) -> false;
                (_) -> true
            end,
            lists:flatten(AllParts)
        )
    ).

%% @private Format method-level help output.
-spec format_method_help(atom(), atom(), atom(), map()) -> binary().
format_method_help(ClassName, SelectorAtom, DefiningClass, MethodObj) ->
    SelectorBin = atom_to_binary(SelectorAtom, utf8),
    NameBin = atom_to_binary(ClassName, utf8),

    Header = iolist_to_binary([<<"== ">>, NameBin, <<" >> ">>, SelectorBin, <<" ==">>]),

    IsSealed =
        case maps:get('__method_info__', MethodObj, #{}) of
            MethodInfo when is_map(MethodInfo) ->
                maps:get(is_sealed, MethodInfo, false);
            _ ->
                false
        end,

    SealedLine =
        case IsSealed of
            true -> <<"\n[sealed]">>;
            false -> <<>>
        end,

    InheritedPart =
        case DefiningClass of
            ClassName ->
                <<>>;
            _ ->
                iolist_to_binary([
                    <<"\n(inherited from ">>, atom_to_binary(DefiningClass, utf8), <<")">>
                ])
        end,

    Signature =
        case maps:get('__signature__', MethodObj, nil) of
            nil -> SelectorBin;
            SigBin when is_binary(SigBin) -> SigBin
        end,

    SignatureLine = iolist_to_binary([<<"\n  ">>, Signature]),

    DocPart =
        case maps:get('__doc__', MethodObj, nil) of
            nil -> <<>>;
            DocBin when is_binary(DocBin) -> iolist_to_binary([<<"\n\n">>, DocBin])
        end,

    iolist_to_binary([Header, SealedLine, InheritedPart, SignatureLine, DocPart]).

%% @private Get method signature from a class pid.
-spec get_method_sig(pid(), atom()) -> {binary(), binary() | none}.
get_method_sig(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            {atom_to_binary(Selector, utf8), none};
        MethodObj when is_map(MethodObj) ->
            Doc =
                case maps:get('__doc__', MethodObj, nil) of
                    nil -> none;
                    D when is_binary(D) -> D
                end,
            {atom_to_binary(Selector, utf8), Doc}
    end.

%% @private Walk the class hierarchy to collect flattened method map.
%% Returns #{Selector => {DefiningClass, MethodInfo}}.
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    collect_flattened_methods(ClassName, ClassPid, 0).

-spec collect_flattened_methods(atom(), pid(), non_neg_integer()) -> map().
collect_flattened_methods(_ClassName, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    #{};
collect_flattened_methods(ClassName, ClassPid, Depth) ->
    {ok, LocalMethods} = gen_server:call(ClassPid, get_instance_methods, 5000),
    LocalFlat = maps:map(fun(_Sel, Info) -> {ClassName, Info} end, LocalMethods),
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    SuperFlat = collect_chain_methods(Superclass, Depth + 1),
    maps:merge(SuperFlat, LocalFlat).

-spec collect_chain_methods(atom() | none, non_neg_integer()) -> map().
collect_chain_methods(none, _Depth) ->
    #{};
collect_chain_methods(SuperName, Depth) ->
    case beamtalk_class_registry:whereis_class(SuperName) of
        undefined -> #{};
        SuperPid -> collect_flattened_methods(SuperName, SuperPid, Depth)
    end.

%% @private Find which class in the hierarchy defines a selector.
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining_class(ClassPid, Selector, 0).

-spec find_defining_class(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class(ClassPid, _Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    gen_server:call(ClassPid, class_name, 5000);
find_defining_class(ClassPid, Selector, Depth) ->
    ClassName = gen_server:call(ClassPid, class_name, 5000),
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            case gen_server:call(ClassPid, superclass, 5000) of
                none ->
                    ClassName;
                Super ->
                    case beamtalk_class_registry:whereis_class(Super) of
                        undefined -> ClassName;
                        SuperPid -> find_defining_class(SuperPid, Selector, Depth + 1)
                    end
            end;
        _MethodInfo ->
            ClassName
    end.

%% @private Group inherited methods by defining class.
-spec group_by_class([{atom(), atom()}]) -> [{atom(), [atom()]}].
group_by_class(Methods) ->
    Grouped = lists:foldl(
        fun({Selector, DefClass}, Acc) ->
            Existing = maps:get(DefClass, Acc, []),
            maps:put(DefClass, [Selector | Existing], Acc)
        end,
        #{},
        Methods
    ),
    lists:sort(
        maps:fold(
            fun(Class, Selectors, Acc) ->
                [{Class, lists:sort(Selectors)} | Acc]
            end,
            [],
            Grouped
        )
    ).

%% @private Build a structured error for a class not found.
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin =
        case ClassName of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Err0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Class '">>, NameBin, <<"' not found.">>])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Use Beamtalk allClasses for available classes.">>
    ).

%% @private Build a structured error for a method not found.
-spec make_method_not_found_error(atom(), atom()) -> #beamtalk_error{}.
make_method_not_found_error(ClassName, Selector) ->
    NameBin = atom_to_binary(ClassName, utf8),
    SelBin = atom_to_binary(Selector, utf8),
    Err0 = beamtalk_error:new(does_not_understand, ClassName),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([NameBin, <<" does not understand ">>, SelBin])
    ),
    beamtalk_error:with_hint(
        Err2,
        iolist_to_binary([<<"Use Beamtalk help: ">>, NameBin, <<" to see available methods.">>])
    ).

%% @private Fetch all live class entries from the registry.
%%
%% Returns a list of {Name, ModuleName, Pid} tuples for all registered class
%% processes that are still alive. Dead processes (noproc, timeout) are silently
%% filtered out. Returns [] if the pg process group is not running.
-type class_entry() :: {atom(), module(), pid()}.
-spec live_class_entries() -> [class_entry()].
live_class_entries() ->
    try
        ClassPids = beamtalk_class_registry:all_classes(),
        lists:filtermap(
            fun(Pid) ->
                try
                    Name = beamtalk_object_class:class_name(Pid),
                    Mod = beamtalk_object_class:module_name(Pid),
                    {true, {Name, Mod, Pid}}
                catch
                    exit:{noproc, _} -> false;
                    exit:{timeout, _} -> false
                end
            end,
            ClassPids
        )
    catch
        exit:{noproc, _} ->
            ?LOG_WARNING("pg not started when fetching class entries", #{module => ?MODULE}),
            []
    end.
