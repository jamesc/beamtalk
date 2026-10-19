use super::codegen;

fn try_compile(name: &str, src: &str) {
    let r = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)));
    match r {
        Err(e) => {
            let m = e
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| e.downcast_ref::<&str>().map(|s| s.to_string()))
                .unwrap_or_default();
            println!("{name}: PANIC {}", &m[..m.len().min(300)]);
        }
        Ok(code) => {
            let dir = tempfile::tempdir().unwrap();
            let f = dir.path().join("test.core");
            std::fs::write(&f, &code).unwrap();
            let out = std::process::Command::new("erlc")
                .arg("+from_core")
                .arg("-o")
                .arg(dir.path())
                .arg(&f)
                .output()
                .unwrap();
            println!(
                "{name}: ERLC {} {}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
                    .lines()
                    .filter(|l| l.contains("unbound") || l.contains("rror"))
                    .collect::<Vec<_>>()
                    .join(" | ")
            );
        }
    }
}

const BUILDER: &str = "Object classBuilder name: #Zed; superclass: Object; classMethods: #{#m => [:self | c := 0. #(1, 2) do: [:e | c := c + e]. c]}; register";

#[test]
fn scratch_builder_in_loop() {
    for (name, ctx) in [
        (
            "class",
            "Object subclass: Repro\n  classState: n = 0\n  class f =>\n",
        ),
        ("actor", "Actor subclass: Repro\n  state: n = 0\n  f =>\n"),
        ("value", "Value subclass: Repro\n  f =>\n"),
    ] {
        for (shape, body) in [
            ("do", format!("    #(1, 2) do: [:i | {BUILDER}]\n    0\n")),
            (
                "while",
                format!("    i := 0\n    [i < 2] whileTrue: [i := i + 1. {BUILDER}]\n    0\n"),
            ),
            (
                "arm",
                format!("    t := 0\n    t < 1 ifTrue: [t := t + 1. {BUILDER}]\n    t\n"),
            ),
            (
                "protected",
                format!("    t := 0\n    [t := t + 1. {BUILDER}] on: Error do: [:e | 0]\n    t\n"),
            ),
            (
                "cond",
                format!("    i := 0\n    [{BUILDER}. i < 2] whileTrue: [i := i + 1]\n    0\n"),
            ),
        ] {
            try_compile(&format!("{name}/{shape}"), &format!("{ctx}{body}"));
        }
    }
}
