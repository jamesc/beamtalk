// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Miscellaneous primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for smaller primitive classes:
//! `File`, `Exception`, `Symbol`, `Tuple`, `Object`, `Association`, `Set`,
//! `CompiledMethod`, `TestCase`, `TestRunner`, `TestResult`, `Stream`,
//! `StackFrame`, `JSON`.

use super::super::document::Document;
use super::{binary_bif, ops_dispatch};
use crate::docvec;

/// File primitive implementations (BT-336, BT-513).
///
/// File class methods delegate directly to `beamtalk_file` runtime module.
/// These are class-level methods (no Self parameter needed).
pub(crate) fn generate_file_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "exists:" => Some(docvec![
            "call 'beamtalk_file':'exists:'(",
            p0.to_string(),
            ")"
        ]),
        "readAll:" => Some(docvec![
            "call 'beamtalk_file':'readAll:'(",
            p0.to_string(),
            ")"
        ]),
        "writeAll:contents:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_file':'writeAll:contents:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "lines:" => Some(docvec![
            "call 'beamtalk_file':'lines:'(",
            p0.to_string(),
            ")"
        ]),
        "open:do:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_file':'open:do:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        _ => None,
    }
}

/// Exception primitive implementations (BT-338).
///
/// Exception field access delegates to `beamtalk_exception_handler` runtime module.
/// This avoids naming conflict: compiled Exception.bt produces `beamtalk_exception`,
/// while the handler module provides the actual implementation.
pub(crate) fn generate_exception_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "message" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('message', [], Self)",
        )),
        "hint" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('hint', [], Self)",
        )),
        "kind" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('kind', [], Self)",
        )),
        "selector" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('selector', [], Self)",
        )),
        "errorClass" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('errorClass', [], Self)",
        )),
        "printString" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('printString', [], Self)",
        )),
        "signal" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('signal', [], Self)",
        )),
        "signal:" => {
            let p0 = params.first().map_or("_Msg", String::as_str);
            Some(docvec![
                "call 'beamtalk_exception_handler':'dispatch'('signal:', [",
                p0.to_string(),
                "], Self)",
            ])
        }
        "stackTrace" => Some(Document::Str(
            "call 'beamtalk_exception_handler':'dispatch'('stackTrace', [], Self)",
        )),
        _ => None,
    }
}

/// `StackFrame` primitive implementations (BT-107).
///
/// `StackFrame` field access delegates to `beamtalk_stack_frame` runtime module.
pub(crate) fn generate_stack_frame_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "method" | "receiverClass" | "arguments" | "sourceLocation" | "moduleName" | "line"
        | "file" | "printString" => Some(docvec![
            "call 'beamtalk_stack_frame':'dispatch'('",
            Document::String(selector.to_owned()),
            "', [], Self)",
        ]),
        _ => None,
    }
}

/// Symbol primitive implementations (BT-273).
///
/// Symbols are Erlang atoms — interned, immutable identifiers.
pub(crate) fn generate_symbol_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Comparison (ADR 0002: Erlang operators)
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "=/=" => binary_bif("=/=", params),
        // Conversion
        "asString" => Some(Document::Str(
            "call 'erlang':'atom_to_binary'(Self, 'utf8')",
        )),
        "asAtom" => {
            // Identity — symbols are already atoms
            Some(Document::Str("Self"))
        }
        // Display — delegate to runtime primitive for consistent formatting
        "printString" => Some(Document::Str(
            "call 'beamtalk_primitive':'print_string'(Self)",
        )),
        // Identity
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// Tuple primitive implementations (BT-417).
///
/// Tuples are Erlang tuples — immutable fixed-size collections, particularly
/// useful for Erlang interop with {ok, Value} and {error, Reason} patterns.
pub(crate) fn generate_tuple_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "size" => Some(Document::Str("call 'erlang':'tuple_size'(Self)")),
        "at:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'at'(Self, ",
                p0.clone(),
                ")"
            ])
        }
        "isOk" => Some(Document::Str(
            "case Self of <{'ok', _Value}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "isError" => Some(Document::Str(
            "case Self of <{'error', _Reason}> when 'true' -> 'true' <_> when 'true' -> 'false' end",
        )),
        "unwrap" => Some(Document::Str("call 'beamtalk_tuple_ops':'unwrap'(Self)")),
        "unwrapOr:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'unwrap_or'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "unwrapOrElse:" => {
            let p0 = params.first()?;
            Some(docvec![
                "call 'beamtalk_tuple_ops':'unwrap_or_else'(Self, ",
                p0.clone(),
                ")",
            ])
        }
        "asString" => Some(Document::Str("call 'beamtalk_tuple_ops':'as_string'(Self)")),
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_tuple_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "atRandom" => Some(Document::Str("call 'beamtalk_random':'atRandom'(Self)")),
        _ => None,
    }
}

/// Object primitive implementations (BT-335).
///
/// Object is the root class — methods here are inherited by all objects.
pub(crate) fn generate_object_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        // Association creation: `self -> value` creates an Association tagged map
        "->" => {
            let p0 = params.first()?;
            Some(docvec![
                "~{'$beamtalk_class' => 'Association', 'key' => Self, 'value' => ",
                p0.clone(),
                "}~",
            ])
        }
        _ => None,
    }
}

/// Association primitive implementations (BT-335).
///
/// Associations are key-value pairs represented as tagged maps.
pub(crate) fn generate_association_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "key" => Some(Document::Str("call 'maps':'get'('key', Self)")),
        "value" => Some(Document::Str("call 'maps':'get'('value', Self)")),
        "asString" => Some(Document::Str(
            "call 'beamtalk_association':'format_string'(Self)",
        )),
        _ => None,
    }
}

/// Set primitive implementations (BT-73).
///
/// Sets are represented as tagged maps: `#{'$beamtalk_class' => 'Set', elements => OrdsetData}`.
/// Operations delegate to `beamtalk_set_ops` helper module which wraps Erlang `ordsets`.
pub(crate) fn generate_set_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "fromList:" => {
            let p0 = params.first().map_or("_List", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'from_list'(",
                p0.to_string(),
                ")"
            ])
        }
        "size" => Some(Document::Str("call 'beamtalk_set_ops':'size'(Self)")),
        "isEmpty" => Some(Document::Str("call 'beamtalk_set_ops':'is_empty'(Self)")),
        "includes:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'includes'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "add:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'add'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "remove:" => {
            let p0 = params.first().map_or("_Element", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'remove'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "union:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'union'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "intersection:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'intersection'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "difference:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'difference'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "isSubsetOf:" => {
            let p0 = params.first().map_or("_Other", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'is_subset_of'(Self, ",
                p0.to_string(),
                ")",
            ])
        }
        "asList" => Some(Document::Str("call 'beamtalk_set_ops':'as_list'(Self)")),
        "do:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            Some(docvec![
                "call 'beamtalk_set_ops':'do'(Self, ",
                p0.to_string(),
                ")"
            ])
        }
        "printString" => {
            // BT-477: Delegate to beamtalk_primitive:print_string/1 which
            // formats Sets as "Set(element1, element2, ...)"
            Some(Document::Str(
                "call 'beamtalk_primitive':'print_string'(Self)",
            ))
        }
        // Streaming (BT-514)
        "stream" => Some(Document::Str("call 'beamtalk_stream':'on'(Self)")),
        _ => None,
    }
}

/// `CompiledMethod` primitive implementations.
///
/// All selectors delegate to `beamtalk_compiled_method_ops:dispatch/3`
/// (the hand-written Erlang runtime helper) to avoid recursion through
/// the compiled stdlib module's own dispatch/3.
pub(crate) fn generate_compiled_method_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "selector" | "source" | "argumentCount" | "printString" | "asString" => Some(ops_dispatch(
            "beamtalk_compiled_method_ops",
            selector,
            params,
        )),
        _ => None,
    }
}

/// `TestCase` primitive implementations for `BUnit` test framework (ADR 0014).
pub(crate) fn generate_test_case_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "assert:" => {
            let p0 = params.first().map_or("_Condition", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'assert'(",
                p0.to_string(),
                ")"
            ])
        }
        "assert:equals:" => {
            let p0 = params.first().map_or("_Actual", String::as_str);
            let p1 = params.get(1).map_or("_Expected", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'assert_equals'(",
                p1.to_string(),
                ", ",
                p0.to_string(),
                ")",
            ])
        }
        "deny:" => {
            let p0 = params.first().map_or("_Condition", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'deny'(",
                p0.to_string(),
                ")"
            ])
        }
        "should:raise:" => {
            let p0 = params.first().map_or("_Block", String::as_str);
            let p1 = params.get(1).map_or("_ErrorKind", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'should_raise'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "fail:" => {
            let p0 = params.first().map_or("_Message", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_case':'fail'(",
                p0.to_string(),
                ")"
            ])
        }
        // BT-440: Class-side methods for REPL test execution
        // These are always called from class method context where self = ClassSelf
        // Pass class name by extracting from ClassSelf tag ('ClassName class')
        "runAll" => Some(Document::Str(
            "let <_RunTag> = call 'erlang':'element'(2, ClassSelf) in \
                 let <_RunTagStr> = call 'erlang':'atom_to_list'(_RunTag) in \
                 let <_RunLen> = call 'erlang':'length'(_RunTagStr) in \
                 let <_RunNameLen> = call 'erlang':'-'(_RunLen, 6) in \
                 let <_RunNameStr> = call 'lists':'sublist'(_RunTagStr, _RunNameLen) in \
                 let <_RunClassName> = call 'erlang':'list_to_atom'(_RunNameStr) in \
                 call 'beamtalk_test_case':'run_all'(_RunClassName)",
        )),
        "run:" => {
            let p0 = params.first().map_or("_TestName", String::as_str);
            Some(docvec![
                "let <_RunTag2> = call 'erlang':'element'(2, ClassSelf) in \
                 let <_RunTagStr2> = call 'erlang':'atom_to_list'(_RunTag2) in \
                 let <_RunLen2> = call 'erlang':'length'(_RunTagStr2) in \
                 let <_RunNameLen2> = call 'erlang':'-'(_RunLen2, 6) in \
                 let <_RunNameStr2> = call 'lists':'sublist'(_RunTagStr2, _RunNameLen2) in \
                 let <_RunClassName2> = call 'erlang':'list_to_atom'(_RunNameStr2) in \
                 call 'beamtalk_test_case':'run_single'(_RunClassName2, ",
                p0.to_string(),
                ")",
            ])
        }
        _ => None,
    }
}

/// `TestRunner` primitive implementations for programmatic test execution (BT-762).
///
/// `TestRunner` class-side methods discover and run tests, returning `TestResult`
/// objects. Unlike `TestCase` primitives which need class name extraction from
/// `ClassSelf`, `TestRunner`'s `runAll` takes no args and `run:`/`run:method:`
/// take a class reference arg.
pub(crate) fn generate_test_runner_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        // Class-side: discover all test classes and run everything
        "runAll" => Some(Document::Str("call 'beamtalk_test_runner':'run_all'()")),
        // Class-side: run all tests in a single class
        "run:" => {
            let p0 = params.first().map_or("_TestClass", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_runner':'run_class'(",
                p0.to_string(),
                ")",
            ])
        }
        // Class-side: run a single test method in a class
        "run:method:" => {
            let p0 = params.first().map_or("_TestClass", String::as_str);
            let p1 = params.get(1).map_or("_TestName", String::as_str);
            Some(docvec![
                "call 'beamtalk_test_runner':'run_method'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        _ => None,
    }
}

/// `TestResult` primitive implementations for test result introspection (BT-762).
///
/// `TestResult` is a tagged map with `$beamtalk_class => 'TestResult'`.
/// Instance methods extract fields from the map.
pub(crate) fn generate_test_result_bif(
    selector: &str,
    _params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "passed" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_passed'(Self)",
        )),
        "failed" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_failed'(Self)",
        )),
        "total" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_total'(Self)",
        )),
        "duration" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_duration'(Self)",
        )),
        "failures" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_failures'(Self)",
        )),
        "hasPassed" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_has_passed'(Self)",
        )),
        "summary" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_summary'(Self)",
        )),
        "printString" => Some(Document::Str(
            "call 'beamtalk_test_runner':'result_print_string'(Self)",
        )),
        _ => None,
    }
}

/// Pid primitive implementations (BT-681).
///
/// Pids are opaque BEAM process identifiers. Most methods use direct BIFs;
/// `asString` delegates to `beamtalk_opaque_ops` runtime module.
pub(crate) fn generate_pid_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'pid_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        "isAlive" => Some(Document::Str("call 'erlang':'is_process_alive'(Self)")),
        _ => None,
    }
}

/// Port primitive implementations (BT-681).
///
/// Ports are opaque BEAM port identifiers.
pub(crate) fn generate_port_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'port_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// Reference primitive implementations (BT-681).
///
/// References are opaque BEAM unique identifiers.
pub(crate) fn generate_reference_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    match selector {
        "=:=" => binary_bif("=:=", params),
        "/=" => binary_bif("/=", params),
        "asString" => Some(Document::Str(
            "call 'beamtalk_opaque_ops':'ref_to_string'(Self)",
        )),
        "hash" => Some(Document::Str("call 'erlang':'phash2'(Self)")),
        _ => None,
    }
}

/// Stream primitive implementations (BT-511).
///
/// Class-side constructors delegate to `beamtalk_stream` module.
/// Instance methods delegate to `beamtalk_stream` module with Self as first arg.
pub(crate) fn generate_stream_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Class-side constructors
        "from:" => Some(docvec![
            "call 'beamtalk_stream':'from'(",
            p0.to_string(),
            ")"
        ]),
        "from:by:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_stream':'from_by'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "on:" => Some(docvec!["call 'beamtalk_stream':'on'(", p0.to_string(), ")"]),
        // Lazy operations
        "select:" => Some(docvec![
            "call 'beamtalk_stream':'select'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "collect:" => Some(docvec![
            "call 'beamtalk_stream':'collect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "reject:" => Some(docvec![
            "call 'beamtalk_stream':'reject'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "drop:" => Some(docvec![
            "call 'beamtalk_stream':'drop'(Self, ",
            p0.to_string(),
            ")"
        ]),
        // Terminal operations
        "take:" => Some(docvec![
            "call 'beamtalk_stream':'take'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "do:" => Some(docvec![
            "call 'beamtalk_stream':'do'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "inject:into:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_stream':'inject_into'(Self, ",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "detect:" => Some(docvec![
            "call 'beamtalk_stream':'detect'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "asList" => Some(Document::Str("call 'beamtalk_stream':'as_list'(Self)")),
        "anySatisfy:" => Some(docvec![
            "call 'beamtalk_stream':'any_satisfy'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "allSatisfy:" => Some(docvec![
            "call 'beamtalk_stream':'all_satisfy'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(Document::Str("call 'beamtalk_stream':'print_string'(Self)")),
        _ => None,
    }
}

/// System primitive implementations (BT-713).
///
/// System class methods delegate directly to `beamtalk_system` runtime module.
/// These are class-level methods (no Self parameter needed).
pub(crate) fn generate_system_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    let p1 = params.get(1).map_or("_Arg1", String::as_str);
    match selector {
        "getEnv:" => Some(docvec![
            "call 'beamtalk_system':'getEnv:'(",
            p0.to_string(),
            ")"
        ]),
        "getEnv:default:" => Some(docvec![
            "call 'beamtalk_system':'getEnv:default:'(",
            p0.to_string(),
            ", ",
            p1.to_string(),
            ")"
        ]),
        "osPlatform" => Some(docvec!["call 'beamtalk_system':'osPlatform'()"]),
        "osFamily" => Some(docvec!["call 'beamtalk_system':'osFamily'()"]),
        "architecture" => Some(docvec!["call 'beamtalk_system':'architecture'()"]),
        "hostname" => Some(docvec!["call 'beamtalk_system':'hostname'()"]),
        "erlangVersion" => Some(docvec!["call 'beamtalk_system':'erlangVersion'()"]),
        "pid" => Some(docvec!["call 'beamtalk_system':'pid'()"]),
        _ => None,
    }
}

/// Random primitive implementations (BT-723).
///
/// Class-side methods use process dictionary seed via `rand:uniform`.
/// Instance methods use explicit state via `rand:uniform_s`.
/// Instance state is a tagged map with `$beamtalk_class => 'Random'`.
pub(crate) fn generate_random_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Class-side (process dictionary seed)
        "next" => Some(Document::Str("call 'beamtalk_random':'next'()")),
        "nextInteger:" => Some(docvec![
            "call 'beamtalk_random':'nextInteger:'(",
            p0.to_string(),
            ")"
        ]),
        "new" => Some(Document::Str("call 'beamtalk_random':'new'()")),
        "seed:" => Some(docvec![
            "call 'beamtalk_random':'seed:'(",
            p0.to_string(),
            ")"
        ]),
        // Instance-side (explicit state)
        "instanceNext" => Some(Document::Str("call 'beamtalk_random':'instanceNext'(Self)")),
        "instanceNextInteger:" => Some(docvec![
            "call 'beamtalk_random':'instanceNextInteger:'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "printString" => Some(Document::Str("call 'beamtalk_random':'printString'(Self)")),
        _ => None,
    }
}

/// JSON primitive implementations (BT-711).
///
/// JSON class methods delegate directly to `beamtalk_json` runtime module.
/// These are class-level methods (no Self parameter needed).
pub(crate) fn generate_json_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "parse:" => Some(docvec![
            "call 'beamtalk_json':'parse:'(",
            p0.to_string(),
            ")"
        ]),
        "generate:" => Some(docvec![
            "call 'beamtalk_json':'generate:'(",
            p0.to_string(),
            ")"
        ]),
        "prettyPrint:" => Some(docvec![
            "call 'beamtalk_json':'prettyPrint:'(",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}

/// Regex primitive implementations (BT-709).
///
/// Regex class methods delegate to `beamtalk_regex` runtime module.
/// Instance methods (source, printString, describe) operate on Self.
pub(crate) fn generate_regex_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        "from:" => Some(docvec![
            "call 'beamtalk_regex':'from:'(",
            p0.to_string(),
            ")"
        ]),
        "from:options:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            Some(docvec![
                "call 'beamtalk_regex':'from:options:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")",
            ])
        }
        "source" => Some(Document::Str("call 'beamtalk_regex':'source'(Self)")),
        "printString" => Some(Document::Str("call 'beamtalk_regex':'printString'(Self)")),
        "describe" => Some(Document::Str("call 'beamtalk_regex':'describe'(Self)")),
        _ => None,
    }
}

/// `DateTime` primitive implementations (BT-710).
///
/// Class methods delegate to `beamtalk_datetime` runtime module.
/// Instance methods operate on Self (tagged map).
/// Comparison operators delegate to runtime rather than inlining Erlang BIFs
/// because datetime values compare by timestamp, not by map structure.
pub(crate) fn generate_datetime_bif(
    selector: &str,
    params: &[String],
) -> Option<Document<'static>> {
    let p0 = params.first().map_or("_Arg0", String::as_str);
    match selector {
        // Class methods
        "now"
        | "monotonicNow"
        | "year:month:day:"
        | "year:month:day:hour:minute:second:"
        | "fromTimestamp:"
        | "fromString:" => generate_datetime_class_bif(selector, params, p0),
        // Instance methods
        _ => generate_datetime_instance_bif(selector, p0),
    }
}

fn generate_datetime_class_bif(
    selector: &str,
    params: &[String],
    p0: &str,
) -> Option<Document<'static>> {
    match selector {
        "now" => Some(Document::Str("call 'beamtalk_datetime':'now'()")),
        "monotonicNow" => Some(Document::Str("call 'beamtalk_datetime':'monotonicNow'()")),
        "year:month:day:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            let p2 = params.get(2).map_or("_Arg2", String::as_str);
            Some(docvec![
                "call 'beamtalk_datetime':'year:month:day:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", ",
                p2.to_string(),
                ")",
            ])
        }
        "year:month:day:hour:minute:second:" => {
            let p1 = params.get(1).map_or("_Arg1", String::as_str);
            let p2 = params.get(2).map_or("_Arg2", String::as_str);
            let p3 = params.get(3).map_or("_Arg3", String::as_str);
            let p4 = params.get(4).map_or("_Arg4", String::as_str);
            let p5 = params.get(5).map_or("_Arg5", String::as_str);
            Some(docvec![
                "call 'beamtalk_datetime':'year:month:day:hour:minute:second:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ", ",
                p2.to_string(),
                ", ",
                p3.to_string(),
                ", ",
                p4.to_string(),
                ", ",
                p5.to_string(),
                ")",
            ])
        }
        "fromTimestamp:" => Some(docvec![
            "call 'beamtalk_datetime':'fromTimestamp:'(",
            p0.to_string(),
            ")"
        ]),
        "fromString:" => Some(docvec![
            "call 'beamtalk_datetime':'fromString:'(",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}

fn generate_datetime_instance_bif(selector: &str, p0: &str) -> Option<Document<'static>> {
    match selector {
        // Accessors
        "year" => Some(Document::Str("call 'beamtalk_datetime':'year'(Self)")),
        "month" => Some(Document::Str("call 'beamtalk_datetime':'month'(Self)")),
        "day" => Some(Document::Str("call 'beamtalk_datetime':'day'(Self)")),
        "hour" => Some(Document::Str("call 'beamtalk_datetime':'hour'(Self)")),
        "minute" => Some(Document::Str("call 'beamtalk_datetime':'minute'(Self)")),
        "second" => Some(Document::Str("call 'beamtalk_datetime':'second'(Self)")),
        // Conversion
        "asTimestamp" => Some(Document::Str(
            "call 'beamtalk_datetime':'asTimestamp'(Self)",
        )),
        "asString" => Some(Document::Str("call 'beamtalk_datetime':'asString'(Self)")),
        "printString" => Some(Document::Str(
            "call 'beamtalk_datetime':'printString'(Self)",
        )),
        "describe" => Some(Document::Str("call 'beamtalk_datetime':'describe'(Self)")),
        // Arithmetic
        "addSeconds:" => Some(docvec![
            "call 'beamtalk_datetime':'addSeconds:'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "addDays:" => Some(docvec![
            "call 'beamtalk_datetime':'addDays:'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "diffSeconds:" => Some(docvec![
            "call 'beamtalk_datetime':'diffSeconds:'(Self, ",
            p0.to_string(),
            ")"
        ]),
        // Comparison
        "<" => Some(docvec![
            "call 'beamtalk_datetime':'<'(Self, ",
            p0.to_string(),
            ")"
        ]),
        ">" => Some(docvec![
            "call 'beamtalk_datetime':'>'(Self, ",
            p0.to_string(),
            ")"
        ]),
        "<=" => Some(docvec![
            "call 'beamtalk_datetime':'=<'(Self, ",
            p0.to_string(),
            ")"
        ]),
        ">=" => Some(docvec![
            "call 'beamtalk_datetime':'>='(Self, ",
            p0.to_string(),
            ")"
        ]),
        "=:=" => Some(docvec![
            "call 'beamtalk_datetime':'=:='(Self, ",
            p0.to_string(),
            ")"
        ]),
        "/=" => Some(docvec![
            "call 'beamtalk_datetime':'/='(Self, ",
            p0.to_string(),
            ")"
        ]),
        _ => None,
    }
}
