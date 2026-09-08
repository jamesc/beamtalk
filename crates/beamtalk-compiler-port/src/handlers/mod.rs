// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Request handlers, one module per request family.
//!
//! [`crate::handle_request`] dispatches each wire command to the handler
//! named here; each module owns its family's helper functions (response
//! shaping specific to that family, error-response builders, ...) that
//! aren't shared widely enough to live in [`crate::respond`].

mod categorize;
mod class_span;
mod compile;
mod compile_method;
pub(crate) mod completion;
mod diagnostics_and_version;
mod expression;
pub(crate) mod inline_definitions;
mod method_span;
mod source_queries;

pub(crate) use categorize::handle_categorize_methods;
pub(crate) use class_span::{
    handle_build_class_module_index_in_source, handle_class_state_field_defaults,
    handle_resolve_class_span,
};
pub(crate) use compile::handle_compile;
pub(crate) use compile_method::handle_compile_method;
pub(crate) use completion::handle_resolve_completion_type;
pub(crate) use diagnostics_and_version::{handle_diagnostics, handle_version};
pub(crate) use expression::{handle_compile_expression, handle_compile_expression_trace};
pub(crate) use method_span::{
    handle_find_definition_selector_spans, handle_find_selector_send_spans,
    handle_reindent_method_source, handle_resolve_method_span,
};
pub(crate) use source_queries::{
    handle_find_all_sends_in_source, handle_find_announce_sites_in_source,
    handle_find_ffi_sites_in_source, handle_find_field_readers_in_source,
    handle_find_field_writers_in_source, handle_find_references_to_in_source,
    handle_find_senders_in_source,
};
