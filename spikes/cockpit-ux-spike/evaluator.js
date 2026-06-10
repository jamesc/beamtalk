/* evaluator.js — a small, believable Beamtalk evaluator for the Workspace.
   window.beamEval(source) -> { ok, kind, value, printString, className, fields }
   Understands the seeded demo expressions plus arithmetic with real PEMDAS
   precedence (Beamtalk diverges from Smalltalk's left-to-right here). Not a real
   VM — just enough to feel alive and return inspectable objects. */
(function () {
  const env = {};

  const int = (n) => ({ ok: true, kind: "int", value: n, printString: String(n), className: "Integer" });
  const str = (s) => ({ ok: true, kind: "string", value: s, printString: '"' + s + '"', className: "String" });
  const bool = (b) => ({ ok: true, kind: "bool", value: b, printString: b ? "true" : "false", className: "Boolean" });
  const sym = (s) => ({ ok: true, kind: "symbol", value: s, printString: s, className: "Symbol" });
  const obj = (printString, className, fields) => ({ ok: true, kind: "object", className, printString, fields });
  const err = (msg) => ({ ok: false, error: msg });

  // strip // line comments and trailing blank lines, keep code lines
  function clean(src) {
    return (src || "")
      .split("\n")
      .map((l) => l.replace(/\/\/.*$/, "").trimEnd())
      .filter((l) => l.trim().length)
      .join("\n");
  }

  // PEMDAS arithmetic for things like "2 + 3 * 4" -> 14
  function tryArithmetic(expr) {
    const t = expr.trim();
    if (!/^[\d\s+\-*/().]+$/.test(t) || !/[+\-*/]/.test(t)) return null;
    try {
      // guard: only digits and operators — safe to Function-eval
      const v = Function('"use strict"; return (' + t + ")")();
      if (typeof v === "number" && isFinite(v)) return int(Number.isInteger(v) ? v : +v.toFixed(4));
    } catch (e) { /* fall through */ }
    return null;
  }

  window.beamEval = function (source) {
    const raw = (source || "").trim();
    const src = clean(source);
    if (!src) return err("Nothing to evaluate");
    const last = src.split("\n").pop().trim();

    // Greeter new greet: "Ada"   (string interpolation)
    let mm = src.match(/Greeter\s+(?:new\s+)?greet:\s*"([^"]*)"/);
    if (mm) return str("Hello, " + mm[1] + "! Welcome to Beamtalk.");

    // Counter demo:  c := Counter spawn / startingAt: N ; increment ; incrementBy: ; value
    if (/Counter\s+(spawn|startingAt:)/.test(src)) {
      let v = 0;
      const seed = src.match(/startingAt:\s*(\d+)/);
      if (seed) v = parseInt(seed[1], 10);
      v += (src.match(/\bincrement\b/g) || []).length;
      v -= (src.match(/\bdecrement\b/g) || []).length;
      (src.match(/incrementBy:\s*(\d+)/g) || []).forEach((b) => { v += parseInt(b.match(/\d+/)[0], 10); });
      env.counter = v;
      if (/\bvalue\b$/.test(last) || /^\d/.test(last) === false && /value/.test(last)) return int(v);
      return obj("a Counter(" + v + ")", "Counter", {
        value: { kind: "int", value: v }, step: { kind: "int", value: 1 }, pid: { kind: "pid", value: "<0.214.0>" },
      });
    }

    // Account demo:  acct := Account for: "Ada" ; deposit: ; withdraw: ; balance
    if (/Account\s+for:/.test(src)) {
      let bal = 0;
      (src.match(/deposit:\s*(\d+)/g) || []).forEach((d) => bal += parseInt(d.match(/\d+/)[0], 10));
      (src.match(/withdraw:\s*(\d+)/g) || []).forEach((d) => bal -= parseInt(d.match(/\d+/)[0], 10));
      const owner = (src.match(/for:\s*"([^"]*)"/) || [])[1] || "—";
      if (/balance\s*$/.test(last)) return int(bal);
      return obj("an Account(" + owner + " · " + bal + ")", "Account", {
        owner: { kind: "string", value: owner }, balance: { kind: "int", value: bal }, pid: { kind: "pid", value: "<0.217.0>" },
      });
    }

    // Room demo:  Room named: #lobby
    if (/Room\s+named:/.test(src)) {
      const nm = (src.match(/named:\s*#?'?([A-Za-z]+)'?/) || [])[1] || "room";
      return obj("a Room(#" + nm + " · 0 members)", "Room", {
        name: { kind: "symbol", value: "#" + nm }, members: { kind: "ref", value: "a Set(0)" },
        history: { kind: "ref", value: "a List(0)" }, pid: { kind: "pid", value: "<0.219.0>" },
      });
    }

    // Transcript show: "..."  -> displays, answers the receiver
    mm = last.match(/Transcript\s+show:\s*"([^"]*)"/);
    if (mm) return { ok: true, kind: "transcript", value: mm[1], printString: "a TranscriptStream", className: "TranscriptStream" };

    // String concat with ++ :  "a" ++ "b"
    mm = last.match(/^"([^"]*)"\s*\+\+\s*"([^"]*)"$/);
    if (mm) return str(mm[1] + mm[2]);

    // factorial:  10 factorial
    mm = last.match(/^(\d+)\s+factorial$/);
    if (mm) { let f = 1; for (let k = 2; k <= +mm[1]; k++) f *= k; return int(f); }

    // (1 to: n) sum
    mm = last.match(/\(1 to:\s*(\d+)\)\s+sum/);
    if (mm) { const n = +mm[1]; return int((n * (n + 1)) / 2); }

    // structural equality demo
    if (/==/.test(last)) {
      const a = tryArithmetic(last.split("==")[0]);
      const b = tryArithmetic(last.split("==")[1]);
      if (a && b) return bool(a.value === b.value);
    }

    // arithmetic with real precedence
    const a = tryArithmetic(last);
    if (a) return a;

    // bare literals
    if (/^\d+$/.test(last)) return int(parseInt(last, 10));
    mm = last.match(/^"([^"]*)"$/);
    if (mm) return str(mm[1]);
    if (/^#[A-Za-z_]\w*$/.test(last)) return sym(last);
    if (/^(true|false)$/.test(last)) return bool(last === "true");
    if (/^nil$/.test(last)) return { ok: true, kind: "nil", value: null, printString: "nil", className: "UndefinedObject" };

    // fallback — looks evaluated, not errored
    return obj("a Result", "Object", {
      source: { kind: "string", value: last.length > 44 ? last.slice(0, 44) + "…" : last },
      note: { kind: "string", value: "evaluated · no display value" },
    });
  };
})();
