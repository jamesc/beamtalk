# beamtalk-repl

REPL-specific Core Erlang code generation for Beamtalk: workspace binding
threading, trace-mode wrapping, and test-module assembly. Delegates
expression compilation and state threading to `beamtalk-core`'s
`CoreErlangGenerator`, keeping the core compiler domain-agnostic about the
REPL/workspace bounded context.
