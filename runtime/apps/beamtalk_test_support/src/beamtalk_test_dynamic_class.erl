%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_dynamic_class).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Shared EUnit fixture: register a freestanding dynamic (`ClassBuilder`)
class and wrap it as a `#beamtalk_object{}` class object, for EUnit suites
that need a real live class with no `.bt` source file to exercise a
dynamic-class code path (e.g. `beamtalk_behaviour_intrinsics:classRenameTo/2`'s
dynamic-class branch).

Extracted (BT-3443) from `beamtalk_behaviour_intrinsics_rename_to_tests`'s
own `register_dynamic_class/1`, once `beamtalk_repl_compiler_rename_freshness_tests`
needed the identical fixture — same "copied once, extracted on the second
use" precedent `beamtalk_test_corpus`'s own moduledoc documents (CLAUDE.md's
no-duplicate-implementations rule).
""".

-export([register/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-doc """
Registers `ClassName` as a freestanding dynamic subclass of `Object` (no
fields, no methods) and returns `{ClassObj, Pid}` — `ClassObj` the
`#beamtalk_object{}` class object a primitive like `classRenameTo/2` takes
as `Self`, `Pid` the class gen_server's pid directly, for callers that also
want to assert on/stop it.
""".
-spec register(atom()) -> {#beamtalk_object{}, pid()}.
register(ClassName) ->
    State = #{
        className => ClassName,
        superclassRef => 'Object',
        fieldSpecs => #{},
        methodSpecs => #{}
    },
    {ok, Pid} = beamtalk_class_builder:register(State),
    Tag = beamtalk_class_registry:class_object_tag(ClassName),
    Module = beamtalk_object_class:module_name(Pid),
    ClassObj = #beamtalk_object{class = Tag, class_mod = Module, pid = Pid},
    {ClassObj, Pid}.
