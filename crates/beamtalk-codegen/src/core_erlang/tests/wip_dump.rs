// WIP-3793-EXPERIMENT: scratch core dumper, removed before commit.
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

#[test]
#[ignore = "scratch"]
fn wip_dump() {
    let path = std::env::var("BT_DUMP_SRC").expect("BT_DUMP_SRC");
    let src = std::fs::read_to_string(path).expect("read");
    let (module, diags) = parse(lex_with_eof(&src));
    assert!(diags.is_empty(), "{diags:?}");
    let generated = generate_module_with_warnings(&module, CodegenOptions::new("wipdump"));
    match generated {
        Ok(g) => {
            let out = std::env::var("BT_DUMP_OUT").expect("BT_DUMP_OUT");
            std::fs::write(out, g.code).expect("write");
        }
        Err(e) => {
            let out = std::env::var("BT_DUMP_OUT").expect("BT_DUMP_OUT");
            std::fs::write(out, format!("ERROR: {e:?}")).expect("write");
        }
    }
}
