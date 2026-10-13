// WIP-3793-EXPERIMENT: compact IR printer, removed before commit.
use super::super::threaded_ir::{BindOp, ThreadedStmt};

pub(crate) fn wip_summary(stmts: &[ThreadedStmt], indent: usize) -> String {
    let pad = "  ".repeat(indent);
    let mut out = String::new();
    for s in stmts {
        match s {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                span,
            } => {
                let kind = match op {
                    BindOp::Put { field, .. } => format!("Put({field})"),
                    BindOp::Unpack { field } => format!("Unpack({field})"),
                    BindOp::Direct(_) => "Direct".to_string(),
                };
                out.push_str(&format!(
                    "{pad}Bind {:?}@{} <- {:?}@{} [{kind}] {}..{}\n",
                    target.prefix,
                    target.version,
                    source.prefix,
                    source.version,
                    span.start(),
                    span.end()
                ));
            }
            ThreadedStmt::Threaded {
                mode,
                frame,
                threads,
                body,
                produces,
                ..
            } => {
                out.push_str(&format!(
                    "{pad}Threaded {mode:?} {frame:?} threads={threads:?} produces={:?}\n",
                    produces
                        .iter()
                        .map(|v| format!("{:?}@{}", v.prefix, v.version))
                        .collect::<Vec<_>>()
                ));
                out.push_str(&wip_summary(body, indent + 1));
            }
            ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                threads,
                body,
                produces,
                ..
            } => {
                out.push_str(&format!(
                    "{pad}ConditionalLoop {mode:?} {frame:?} threads={threads:?} produces={:?}\n",
                    produces
                        .iter()
                        .map(|v| format!("{:?}@{}", v.prefix, v.version))
                        .collect::<Vec<_>>()
                ));
                out.push_str(&wip_summary(body, indent + 1));
            }
            ThreadedStmt::ConstructTuple {
                carrier, threads, ..
            } => {
                out.push_str(&format!("{pad}ConstructTuple {carrier} threads={threads:?}\n"));
            }
            ThreadedStmt::LocalRebind {
                local,
                carrier,
                frame,
                value_var,
                lowering,
                state_first,
                ..
            } => {
                out.push_str(&format!(
                    "{pad}LocalRebind {local} <- {carrier} {frame:?} v={value_var} {lowering:?} state_first={state_first:?}\n"
                ));
            }
            ThreadedStmt::Statement(_, span) => {
                out.push_str(&format!("{pad}Statement {span:?}\n"));
            }
            other => out.push_str(&format!(
                "{pad}{}\n",
                format!("{other:?}").chars().take(80).collect::<String>()
            )),
        }
    }
    out
}
