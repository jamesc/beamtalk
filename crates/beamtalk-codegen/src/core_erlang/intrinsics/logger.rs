// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Logger intrinsics.
//!
//! Lowers `Logger debug:`/`info:`/… call sites to the
//! `beamtalk_logging_config:log/3` call with injected domain, class and
//! selector metadata.
//!
//! **DDD Context:** Compilation — Code Generation

use super::{CoreErlangGenerator, Document, Expression, MessageSelector, Result, docvec, leaf};

impl CoreErlangGenerator {
    /// Emits the `beamtalk_logging_config:log/3` call shared by the call-site
    /// Logger intrinsic ([`Self::try_generate_logger_intrinsic`]) and the
    /// `@intrinsic logger*` method bodies
    /// (`IntrinsicBody::Logger` in `primitives::intrinsic_bodies`). The call
    /// answers `'nil'`.
    ///
    /// The message is passed through unchanged: a Beamtalk `String` is the
    /// tagged `{beamtalk_string, Bin}` term, and the runtime unwraps it (and
    /// any String keys/values in the metadata) before it reaches OTP's
    /// formatter (BT-4321).
    ///
    /// # Generated Code
    ///
    /// For `Logger debug: Msg` in `Counter>>increment`:
    /// ```erlang
    /// call 'beamtalk_logging_config':'log'('debug', Msg, ~{
    ///     'domain' => ['beamtalk' | ['user']],
    ///     'beamtalk_class' => 'Counter',
    ///     'beamtalk_selector' => 'increment'
    /// }~)
    /// ```
    ///
    /// With `metadata: UserMeta`, the injected map is merged over the user's
    /// (`call 'maps':'merge'(UserMeta, ~{...}~)`), so the domain/class/selector
    /// keys cannot be overridden.
    pub(in crate::core_erlang) fn logger_log_call_doc(
        &self,
        level: &str,
        msg_doc: Document<'static>,
        user_meta_doc: Option<Document<'static>>,
    ) -> Document<'static> {
        let ctx_class = self.class_name();
        let ctx_selector = self
            .current_method_selector
            .clone()
            .unwrap_or_else(|| "unknown".to_string());

        let metadata_map_doc = docvec![
            "~{",
            "'domain' => ['beamtalk' | ['user']], ",
            "'beamtalk_class' => ",
            leaf::atom(ctx_class),
            ", ",
            "'beamtalk_selector' => ",
            leaf::atom(ctx_selector),
            "}~",
        ];

        let meta_doc = match user_meta_doc {
            Some(user_meta) => docvec![
                "call 'maps':'merge'(",
                user_meta,
                ", ",
                metadata_map_doc,
                ")"
            ],
            None => metadata_map_doc,
        };

        docvec![
            "call 'beamtalk_logging_config':'log'(",
            leaf::atom(level.to_string()),
            ", ",
            msg_doc,
            ", ",
            meta_doc,
            ")"
        ]
    }

    /// Tries to generate inline Logger calls for Logger class sends.
    ///
    /// Recognizes `Logger debug:`, `Logger info:`, `Logger warn:`, `Logger error:`
    /// (and their `*:metadata:` variants) and generates direct
    /// `beamtalk_logging_config:log/3` calls with domain metadata injected at
    /// the call site.
    ///
    /// Non-logging selectors fall through to normal class dispatch.
    ///
    /// - Returns `Ok(Some(doc))` if the message was a Logger intrinsic
    /// - Returns `Ok(None)` if not a Logger message (caller should continue)
    /// - Returns `Err(...)` on error
    ///
    /// # Generated Code
    ///
    /// See [`Self::logger_log_call_doc`] for the emitted call.
    pub(in crate::core_erlang) fn try_generate_logger_intrinsic(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) -> Result<Option<Document<'static>>> {
        // Only intercept ClassReference receivers named "Logger"
        let class_name = match receiver {
            Expression::ClassReference { name, .. } if name.name == "Logger" => &name.name,
            _ => return Ok(None),
        };

        // Map Beamtalk selectors to OTP logger levels
        let (level, has_metadata) = match selector {
            MessageSelector::Keyword(parts) => {
                let selector_name: String = parts.iter().map(|p| p.keyword.as_str()).collect();
                match selector_name.as_str() {
                    "debug:" if arguments.len() == 1 => ("debug", false),
                    "info:" if arguments.len() == 1 => ("info", false),
                    "warn:" if arguments.len() == 1 => ("warning", false),
                    "error:" if arguments.len() == 1 => ("error", false),
                    "debug:metadata:" if arguments.len() == 2 => ("debug", true),
                    "info:metadata:" if arguments.len() == 2 => ("info", true),
                    "warn:metadata:" if arguments.len() == 2 => ("warning", true),
                    "error:metadata:" if arguments.len() == 2 => ("error", true),
                    // any other selector — fall through to normal dispatch
                    _ => return Ok(None),
                }
            }
            _ => return Ok(None),
        };

        // Hoist message arg (and optional metadata arg) so class method
        // self-sends in sub-expression position (e.g. `Logger info: (self tick)`)
        // thread their class var mutations through to the enclosing scope.
        let arg_exprs: Vec<&Expression> = arguments.iter().collect();
        let mut seq = self.sequence_call(&arg_exprs, "LogArg")?;
        let msg_doc = seq.next();
        let user_meta_doc = if has_metadata { Some(seq.next()) } else { None };

        let _ = class_name; // Logger class name itself is not needed in the metadata

        let call_doc = self.logger_log_call_doc(level, msg_doc, user_meta_doc);

        Ok(Some(seq.close(self, call_doc, "LogRes")))
    }
}
