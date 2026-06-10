/* components.jsx — System Browser (class tree + method list), code editor with live
   re-highlighting, and the senders/implementors popover. Exported to window. */

const HL = (s) => ({ __html: (window.highlightBeamtalk || ((x) => x))(s || "") });

/* ---------- A re-highlighting code editor (textarea over a highlighted <pre>) ---------- */
function CodeEditor({ value, onChange, onSelect, taRef, wrapClass, preClass, taClass, onKeyDown, spellCheck }) {
  const innerTa = React.useRef(null);
  const ref = taRef || innerTa;
  const preR = React.useRef(null);
  const sync = () => {
    if (preR.current && ref.current) {
      preR.current.scrollTop = ref.current.scrollTop;
      preR.current.scrollLeft = ref.current.scrollLeft;
    }
  };
  return (
    <div className={wrapClass}>
      <pre ref={preR} className={preClass} aria-hidden="true">
        <code dangerouslySetInnerHTML={HL(value + (value.endsWith("\n") ? " " : ""))} />
      </pre>
      <textarea
        ref={ref}
        className={taClass}
        value={value}
        spellCheck={spellCheck === undefined ? false : spellCheck}
        onChange={(e) => onChange && onChange(e.target.value)}
        onScroll={sync}
        onKeyDown={onKeyDown}
        onSelect={(e) => {
          if (!onSelect) return;
          const el = e.target;
          onSelect(el.value.slice(el.selectionStart, el.selectionEnd), el.selectionStart, el.selectionEnd);
        }}
      />
    </div>
  );
}

/* ---------- Class tree (hierarchy or category view) + instance/class side toggle ---------- */
function ClassBrowser({ image, view, setView, selectedClass, onSelectClass, side, setSide }) {
  const classes = image.classes;

  // build category -> classes
  const cats = {};
  classes.forEach((c) => { (cats[c.category] = cats[c.category] || []).push(c); });

  // hierarchy depth for indentation
  const depthOf = (c) => {
    let d = 0, cur = c;
    while (cur && cur.superclass) {
      cur = classes.find((x) => x.name === cur.superclass);
      d += 1;
    }
    return d;
  };

  const renderClassRow = (c, indent) => {
    const sub = c.superclass ? "subclass" : "";
    const cls = "row " + (indent === 2 ? "subclass2" : indent === 1 ? "subclass" : "") + (selectedClass === c.name ? " sel" : "");
    return (
      <div key={c.name} className={cls} onClick={() => onSelectClass(c.name)} title={c.name}>
        <span className="twig">{c.superclass ? "→" : "●"}</span>
        <span className="cls">{c.name}</span>
        {selectedClass === c.name && side === "class" ? <span className="pill">class</span> : null}
      </div>
    );
  };

  return (
    <div className="panel" style={{ flex: 1 }}>
      <div className="panel-head">
        System Browser
        <span className="spacer" />
        <div className="seg" role="tablist">
          <button className={view === "hierarchy" ? "on" : ""} onClick={() => setView("hierarchy")}>Hier</button>
          <button className={view === "category" ? "on" : ""} onClick={() => setView("category")}>Cats</button>
        </div>
      </div>
      <div className="panel-body">
        <div className="tree">
          {view === "category"
            ? Object.keys(cats).map((cat) => (
                <div className="cat-group" key={cat}>
                  <div className="cat-row">{cat}</div>
                  {cats[cat].map((c) => renderClassRow(c, c.superclass ? 1 : 0))}
                </div>
              ))
            : renderHierarchy()}
        </div>
      </div>
      <div className="actionbar" style={{ borderTop: "1px solid var(--line)", borderBottom: 0 }}>
        <div className="seg" style={{ flex: 1, display: "flex" }}>
          <button style={{ flex: 1 }} className={side === "instance" ? "on" : ""} onClick={() => setSide("instance")}>instance</button>
          <button style={{ flex: 1 }} className={side === "class" ? "on" : ""} onClick={() => setSide("class")}>class</button>
        </div>
      </div>
    </div>
  );

  // hierarchy render: walk roots then children
  function renderHierarchy() {
    const byParent = {};
    classes.forEach((c) => { (byParent[c.superclass || "__root"] = byParent[c.superclass || "__root"] || []).push(c); });
    const rows = [];
    const walk = (parent, indent) => {
      (byParent[parent] || []).forEach((c) => {
        rows.push(renderClassRow(c, indent));
        walk(c.name, Math.min(indent + 1, 2));
      });
    };
    walk("__root", 0);
    return rows;
  }
}

/* ---------- Protocols + method list for the selected class/side ---------- */
function MethodList({ klass, side, selectedProtocol, setProtocol, selectedMethod, onSelectMethod, image }) {
  if (!klass) {
    return (
      <div className="panel" style={{ flex: 1 }}>
        <div className="panel-head">Protocols & Methods</div>
        <div className="empty">Select a class to browse its methods.</div>
      </div>
    );
  }
  const methods = klass.methods.filter((m) => m.side === side);
  const protocols = [];
  methods.forEach((m) => { if (!protocols.includes(m.category)) protocols.push(m.category); });

  const shown = methods.filter((m) => !selectedProtocol || m.category === selectedProtocol);

  // a method is "overridden" if an ancestor also implements the selector
  const isOverride = (m) => {
    let cur = image.classes.find((c) => c.name === klass.superclass);
    while (cur) {
      if (cur.methods.some((x) => x.side === m.side && x.selector === m.selector)) return true;
      cur = image.classes.find((c) => c.name === cur.superclass);
    }
    return false;
  };

  return (
    <div className="panel" style={{ flex: 1 }}>
      <div className="panel-head">
        {side === "class" ? klass.name + " class" : klass.name}
        <span className="spacer" />
        <span className="count">{methods.length} method{methods.length === 1 ? "" : "s"}</span>
      </div>
      <div className="panel-body" style={{ display: "flex", flexDirection: "column" }}>
        <div className="tree" style={{ borderBottom: "1px solid var(--line-soft)", paddingBottom: 2 }}>
          <div className={"row" + (!selectedProtocol ? " sel" : "")} onClick={() => setProtocol(null)}>
            <span className="twig">∗</span><span>all</span>
            <span className="meta">{methods.length}</span>
          </div>
          {protocols.map((p) => (
            <div key={p} className={"row" + (selectedProtocol === p ? " sel" : "")} onClick={() => setProtocol(p)}>
              <span className="twig">·</span><span>{p}</span>
              <span className="meta">{methods.filter((m) => m.category === p).length}</span>
            </div>
          ))}
        </div>
        <div className="tree">
          {shown.length === 0 ? <div className="empty">No methods on the {side} side.</div> : null}
          {shown.map((m) => {
            const k = m.klass + "|" + m.side + "|" + m.selector;
            return (
              <div
                key={k}
                className={"row method-row" + (isOverride(m) ? " overridden" : "") + (selectedMethod === k ? " sel" : "")}
                onClick={() => onSelectMethod(m)}
              >
                <span className="twig" style={{ color: "var(--accent)" }}>ƒ</span>
                <span className="mname mono">{m.selector}</span>
                {m.runtime ? <span className="runtime-tag">runtime</span> : null}
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
}

/* ---------- Senders / Implementors popover ---------- */
function RefsPopover({ data, onClose, onNavigate }) {
  if (!data) return null;
  const { x, y, kind, selector, results } = data;
  return (
    <>
      <div style={{ position: "fixed", inset: 0, zIndex: 39 }} onClick={onClose} />
      <div className="refs-pop" style={{ left: x, top: y }}>
        <div className="panel-head">
          {kind} of <span className="mono" style={{ marginLeft: 6, color: "var(--accent-2)", textTransform: "none", letterSpacing: 0 }}>#{selector}</span>
          <span className="spacer" />
          <span className="count">{results.length}</span>
        </div>
        <div className="panel-body" style={{ maxHeight: 280 }}>
          {results.length === 0 ? <div className="empty">None found in image.</div> : null}
          {results.map((r, i) => (
            <div key={i} className="row" onClick={() => { onNavigate(r); onClose(); }}>
              <span className="twig" style={{ color: "var(--accent)" }}>{kind === "senders" ? "↗" : "ƒ"}</span>
              <span className="mono">{r.klass}{r.side === "class" ? " class" : ""} » {r.selector}</span>
            </div>
          ))}
        </div>
      </div>
    </>
  );
}

Object.assign(window, { CodeEditor, ClassBrowser, MethodList, RefsPopover });
