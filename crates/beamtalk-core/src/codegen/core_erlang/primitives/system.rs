// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! System-level primitive implementations.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Contains BIF generators for system-related classes: `File`, `Stream`, `System`,
//! `Random`, `DateTime`.

use super::super::document::Document;
use super::param;
use crate::docvec;

/// File primitive implementations (BT-336, BT-513).
///
/// File class methods delegate directly to `beamtalk_file` runtime module.
/// These are class-level methods (no Self parameter needed).
pub(crate) fn generate_file_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
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
            let p1 = param(params, 1, "_Arg1");
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
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_file':'open:do:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "isDirectory:" => Some(docvec![
            "call 'beamtalk_file':'isDirectory:'(",
            p0.to_string(),
            ")"
        ]),
        "isFile:" => Some(docvec![
            "call 'beamtalk_file':'isFile:'(",
            p0.to_string(),
            ")"
        ]),
        "mkdir:" => Some(docvec![
            "call 'beamtalk_file':'mkdir:'(",
            p0.to_string(),
            ")"
        ]),
        "mkdirAll:" => Some(docvec![
            "call 'beamtalk_file':'mkdirAll:'(",
            p0.to_string(),
            ")"
        ]),
        "listDirectory:" => Some(docvec![
            "call 'beamtalk_file':'listDirectory:'(",
            p0.to_string(),
            ")"
        ]),
        "delete:" => Some(docvec![
            "call 'beamtalk_file':'delete:'(",
            p0.to_string(),
            ")"
        ]),
        "deleteAll:" => Some(docvec![
            "call 'beamtalk_file':'deleteAll:'(",
            p0.to_string(),
            ")"
        ]),
        "rename:to:" => {
            let p1 = param(params, 1, "_Arg1");
            Some(docvec![
                "call 'beamtalk_file':'rename:to:'(",
                p0.to_string(),
                ", ",
                p1.to_string(),
                ")"
            ])
        }
        "absolutePath:" => Some(docvec![
            "call 'beamtalk_file':'absolutePath:'(",
            p0.to_string(),
            ")"
        ]),
        "tempDirectory" => Some(Document::Str("call 'beamtalk_file':'tempDirectory'()")),
        _ => None,
    }
}

/// Stream primitive implementations (BT-511).
///
/// Class-side constructors delegate to `beamtalk_stream` module.
/// Instance methods delegate to `beamtalk_stream` module with Self as first arg.
pub(crate) fn generate_stream_bif(selector: &str, params: &[String]) -> Option<Document<'static>> {
    let p0 = param(params, 0, "_Arg0");
    match selector {
        // Class-side constructors
        "from:" => Some(docvec![
            "call 'beamtalk_stream':'from'(",
            p0.to_string(),
            ")"
        ]),
        "from:by:" => {
            let p1 = param(params, 1, "_Arg1");
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
            let p1 = param(params, 1, "_Arg1");
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
    let p0 = param(params, 0, "_Arg0");
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
            let p1 = param(params, 1, "_Arg1");
            let p2 = param(params, 2, "_Arg2");
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
            let p1 = param(params, 1, "_Arg1");
            let p2 = param(params, 2, "_Arg2");
            let p3 = param(params, 3, "_Arg3");
            let p4 = param(params, 4, "_Arg4");
            let p5 = param(params, 5, "_Arg5");
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
