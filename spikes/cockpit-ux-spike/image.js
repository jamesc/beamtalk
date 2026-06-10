/* image.js — the Beamtalk live image, as seen through the Attach topology
   (ADR 0017): Phoenix LiveView attaches to a running workspace BEAM node over
   Erlang distribution. Classes are browsed statically (ADR 0024 Tier 1/2);
   bindings + objects are live (Tier 3).

   Syntax is real Beamtalk (Smalltalk-INSPIRED, not -compatible — see
   docs/beamtalk-syntax-rationale.md): `//` comments, `=>` method bodies,
   `self.field` access, `:=` assignment, implicit return of the last expression
   (`^` only for early return), `!` suffix for async (gen_server:cast) sends,
   `"strings with {interpolation}"`, and `#symbols`. */

window.BEAM_IMAGE = (function () {
  // workspace attach metadata (shown in the title bar)
  const workspace = {
    node: "beamtalk_workspace_demo@localhost",
    session: "phoenix-4821",
    cookie: "··· hidden ···",
    otp: "27",
  };

  // ---- method source bodies (Beamtalk) -------------------------------------
  const S = {
    // Object — the root
    "Object>>printString": `printString =>
  // Default rendering used by the Transcript and Inspector.
  "{self class article} {self class name}"`,
    "Object>>inspect": `inspect =>
  // Open this object in the live Inspector, following references lazily.
  Inspector on: self`,
    "Object>>respondsTo:": `respondsTo: aSelector =>
  // Tier-3 truth: ask the live method table, so extension methods and
  // doesNotUnderstand: handlers count too.
  self class allSelectors includes: aSelector`,
    "Object>>changed:": `changed: anAspect =>
  // Announce that an aspect changed. Observers are themselves processes,
  // so each is notified asynchronously — note the trailing !.
  self observers do: [:each | each onChanged: anAspect!]`,
    "Object>>->": `-> aValue =>
  // Pair the receiver with a value as an Association.
  Association key: self value: aValue`,

    // Actor — an Object that runs as its own BEAM process
    "Actor class>>spawn": `spawn =>
  // Start a fresh instance as a supervised BEAM process; answer the actor
  // handle (its pid travels inside). Sends to it become gen_server calls.
  | actor |
  actor := self new
  actor startProcess
  actor`,
    "Actor>>pid": `pid =>
  self.pid`,
    "Actor>>isAlive": `isAlive =>
  Process isAlive: self.pid`,
    "Actor>>stop": `stop =>
  // Ask the process to shut down cleanly (synchronous).
  Process stop: self.pid`,
    "Actor>>onChanged:": `onChanged: anAspect =>
  // Default observer hook — subclasses override to react to peers.
  self`,

    // Counter
    "Counter class>>startingAt:": `startingAt: start =>
  // Spawn a counter process and seed its value.
  | c |
  c := self spawn
  c setTo: start
  c`,
    "Counter>>increment": `increment =>
  self.value := self.value + self.step
  self changed: #value
  self.value`,
    "Counter>>incrementBy:": `incrementBy: delta =>
  self.value := self.value + delta
  self changed: #value
  self.value`,
    "Counter>>decrement": `decrement =>
  self.value := self.value - self.step
  self changed: #value
  self.value`,
    "Counter>>value": `value =>
  // Field reads need no method, but exposing one lets peers query us.
  self.value`,
    "Counter>>setTo:": `setTo: n =>
  self.value := n`,
    "Counter>>step:": `step: n =>
  self.step := n`,
    "Counter>>reset": `reset =>
  self.value := 0
  self changed: #value`,
    "Counter>>printString": `printString =>
  "a Counter({self.value})"`,

    // Account
    "Account class>>for:": `for: aName =>
  | acct |
  acct := self spawn
  acct setOwner: aName
  acct`,
    "Account>>balance": `balance =>
  self.balance`,
    "Account>>deposit:": `deposit: amount =>
  amount <= 0 ifTrue: [^Error signal: "Amount must be positive"]
  self.balance := self.balance + amount
  self.log := self.log add: (Txn credit: amount)
  self changed: #balance
  self.balance`,
    "Account>>withdraw:": `withdraw: amount =>
  amount > self.balance ifTrue: [^Error signal: "Insufficient funds"]
  self.balance := self.balance - amount
  self.log := self.log add: (Txn debit: amount)
  self changed: #balance
  self.balance`,
    "Account>>transferTo:amount:": `transferTo: peer amount: amount =>
  // Withdraw locally (sync), then post to the peer's mailbox (async, note !).
  self withdraw: amount
  peer deposit: amount!
  amount`,

    // Room — fan-out demo
    "Room class>>named:": `named: aName =>
  | room |
  room := self spawn
  room setName: aName
  room`,
    "Room>>join:": `join: anActor =>
  self.members := self.members add: anActor
  self broadcast: "{anActor name} joined"`,
    "Room>>broadcast:": `broadcast: message =>
  // Fan message out to every member concurrently — each ! is a cast.
  self.members do: [:each | each receive: message!]
  self.history := self.history add: message
  self changed: #history
  message`,
    "Room>>leave:": `leave: anActor =>
  self.members := self.members remove: anActor
  self broadcast: "{anActor name} left"`,

    // Stack — a plain Object (no process needed)
    "Stack>>push:": `push: anObject =>
  self.items := self.items addLast: anObject
  anObject`,
    "Stack>>pop": `pop =>
  self isEmpty ifTrue: [^nil]
  self.items removeLast`,
    "Stack>>isEmpty": `isEmpty =>
  self.items isEmpty`,
    "Stack>>do:": `do: aBlock =>
  self.items reverseDo: aBlock`,

    // Greeter — string interpolation in one line
    "Greeter>>greet:": `greet: aName =>
  "Hello, {aName}! Welcome to Beamtalk."`,
  };

  function m(klass, side, selector, category, key, runtime) {
    return { klass, side, selector, category, source: S[key], key, runtime: !!runtime };
  }

  // ---- classes --------------------------------------------------------------
  const classes = [
    {
      name: "Object", superclass: null, category: "Kernel-Objects",
      state: [], comment: "The root of the class hierarchy. Every Beamtalk value is an Object; unknown messages land in doesNotUnderstand:.",
      definition: `// Object is the primitive root of the hierarchy.
// Browsed statically (ADR 0024 Tier 1); its method table is
// augmented live from the attached workspace (Tier 3).
Object
  // no state — the base of everything`,
      methods: [
        m("Object", "instance", "printString", "printing", "Object>>printString"),
        m("Object", "instance", "inspect", "inspecting", "Object>>inspect"),
        m("Object", "instance", "respondsTo:", "reflection", "Object>>respondsTo:", true),
        m("Object", "instance", "changed:", "announcing", "Object>>changed:"),
        m("Object", "instance", "->", "associating", "Object>>->"),
      ],
    },
    {
      name: "Actor", superclass: "Object", category: "Kernel-Processes",
      state: ["pid"], comment: "An Object that runs as its own BEAM process. A sync send compiles to gen_server:call; a send with a trailing ! compiles to gen_server:cast. State is touched by one message at a time, so actors are race-free by construction.",
      definition: `Actor subclass: Actor
  state: pid = nil   // the BEAM process backing this actor`,
      methods: [
        m("Actor", "class", "spawn", "instance creation", "Actor class>>spawn"),
        m("Actor", "instance", "pid", "accessing", "Actor>>pid"),
        m("Actor", "instance", "isAlive", "lifecycle", "Actor>>isAlive"),
        m("Actor", "instance", "stop", "lifecycle", "Actor>>stop"),
        m("Actor", "instance", "onChanged:", "announcing", "Actor>>onChanged:"),
      ],
    },
    {
      name: "Counter", superclass: "Actor", category: "Demo-Counting",
      state: ["value = 0", "step = 1"],
      comment: "The canonical live object. Spawn one, poke it from the Inspector, and watch its value change while it runs.",
      definition: `Actor subclass: Counter
  state: value = 0
  state: step = 1`,
      methods: [
        m("Counter", "class", "startingAt:", "instance creation", "Counter class>>startingAt:"),
        m("Counter", "instance", "increment", "operations", "Counter>>increment"),
        m("Counter", "instance", "incrementBy:", "operations", "Counter>>incrementBy:"),
        m("Counter", "instance", "decrement", "operations", "Counter>>decrement"),
        m("Counter", "instance", "value", "accessing", "Counter>>value"),
        m("Counter", "instance", "setTo:", "accessing", "Counter>>setTo:"),
        m("Counter", "instance", "step:", "accessing", "Counter>>step:"),
        m("Counter", "instance", "reset", "operations", "Counter>>reset"),
        m("Counter", "instance", "printString", "printing", "Counter>>printString"),
      ],
    },
    {
      name: "Account", superclass: "Actor", category: "Demo-Banking",
      state: ["owner", "balance = 0", "log = List new"],
      comment: "A bank account as an actor. Every deposit and withdrawal is a message handled in the account's own process, so balances stay consistent without locks.",
      definition: `Actor subclass: Account
  state: owner
  state: balance = 0
  state: log = List new`,
      methods: [
        m("Account", "class", "for:", "instance creation", "Account class>>for:"),
        m("Account", "instance", "balance", "accessing", "Account>>balance"),
        m("Account", "instance", "deposit:", "operations", "Account>>deposit:"),
        m("Account", "instance", "withdraw:", "operations", "Account>>withdraw:"),
        m("Account", "instance", "transferTo:amount:", "operations", "Account>>transferTo:amount:"),
      ],
    },
    {
      name: "Room", superclass: "Actor", category: "Demo-Chat",
      state: ["name", "members = Set new", "history = List new"],
      comment: "A chat room that fans messages out to member actors concurrently. A thousand rooms with a thousand members each is unremarkable on BEAM.",
      definition: `Actor subclass: Room
  state: name
  state: members = Set new
  state: history = List new`,
      methods: [
        m("Room", "class", "named:", "instance creation", "Room class>>named:"),
        m("Room", "instance", "join:", "membership", "Room>>join:"),
        m("Room", "instance", "leave:", "membership", "Room>>leave:"),
        m("Room", "instance", "broadcast:", "messaging", "Room>>broadcast:"),
      ],
    },
    {
      name: "Stack", superclass: "Object", category: "Collections",
      state: ["items = List new"],
      comment: "A last-in, first-out collection. A plain object — no process — to show that not everything is an actor.",
      definition: `Object subclass: Stack
  state: items = List new`,
      methods: [
        m("Stack", "instance", "push:", "adding", "Stack>>push:"),
        m("Stack", "instance", "pop", "removing", "Stack>>pop"),
        m("Stack", "instance", "isEmpty", "testing", "Stack>>isEmpty"),
        m("Stack", "instance", "do:", "enumerating", "Stack>>do:"),
      ],
    },
    {
      name: "Greeter", superclass: "Object", category: "Demo-Counting",
      state: [],
      comment: "The smallest possible class. In the Workspace, type `Greeter new greet: \"Ada\"` and Print it.",
      definition: `Object subclass: Greeter
  // stateless`,
      methods: [
        m("Greeter", "instance", "greet:", "greeting", "Greeter>>greet:"),
      ],
    },
  ];

  // ---- live session bindings (Tier 3) --------------------------------------
  // Each binding name maps to a live value. Object-valued ones carry a pid and
  // are "inspectable" — the Inspector follows their references.
  const objects = [
    {
      id: "c", binding: "c", printString: "a Counter(7)", className: "Counter",
      pid: "<0.142.0>", status: "running", mailbox: 0, reductions: 18440, inspectable: true,
      fields: { value: { kind: "int", value: 7 }, step: { kind: "int", value: 1 }, pid: { kind: "pid", value: "<0.142.0>" } },
    },
    {
      id: "acct", binding: "acct", printString: "an Account(Ada · $1,240.00)", className: "Account",
      pid: "<0.151.0>", status: "running", mailbox: 0, reductions: 9210, inspectable: true,
      fields: {
        owner: { kind: "string", value: "Ada Lovelace" },
        balance: { kind: "int", value: 124000 },
        log: { kind: "ref", value: "a List(3)", ref: "acctLog" },
        pid: { kind: "pid", value: "<0.151.0>" },
      },
    },
    {
      id: "lobby", binding: "lobby", printString: "a Room(#lobby · 3 members)", className: "Room",
      pid: "<0.160.0>", status: "running", mailbox: 2, reductions: 51200, inspectable: true,
      fields: {
        name: { kind: "symbol", value: "#lobby" },
        members: { kind: "ref", value: "a Set(3)", ref: "lobbyMembers" },
        history: { kind: "ref", value: "a List(12)", ref: "lobbyHistory" },
        pid: { kind: "pid", value: "<0.160.0>" },
      },
    },
    {
      id: "ticker", binding: "ticker", printString: "a Counter(1284)", className: "Counter",
      pid: "<0.171.0>", status: "running", mailbox: 0, reductions: 204880, ticking: true, inspectable: true,
      fields: { value: { kind: "int", value: 1284 }, step: { kind: "int", value: 1 }, pid: { kind: "pid", value: "<0.171.0>" } },
    },
    // scalar bindings — shown in the pane, but not inspectable (no references)
    { id: "greeting", binding: "greeting", printString: '"Hello, Ada! Welcome to Beamtalk."', className: "String", scalar: true, inspectable: false },
    { id: "tau", binding: "tau", printString: "6.283185", className: "Float", scalar: true, inspectable: false },
  ];

  // drill-in reference targets
  const refs = {
    acctLog: {
      printString: "a List(3)", className: "List",
      fields: {
        "1": { kind: "string", value: "credit $1,000.00 · 09:14" },
        "2": { kind: "string", value: "credit $300.00 · 11:02" },
        "3": { kind: "string", value: "debit $60.00 · 14:35" },
      },
    },
    lobbyMembers: {
      printString: "a Set(3)", className: "Set",
      fields: {
        "1": { kind: "ref", value: "an Actor(@ada)", ref: "memberAda" },
        "2": { kind: "string", value: "an Actor(@grace)" },
        "3": { kind: "string", value: "an Actor(@alan)" },
      },
    },
    lobbyHistory: {
      printString: "a List(12)", className: "List",
      fields: {
        "10": { kind: "string", value: '"@ada: ship it"' },
        "11": { kind: "string", value: '"@grace: spawning 1k workers"' },
        "12": { kind: "string", value: '"@alan: mailbox looks clean"' },
      },
    },
    memberAda: {
      printString: "an Actor(@ada)", className: "Member", pid: "<0.181.0>",
      fields: {
        handle: { kind: "string", value: "@ada" },
        room: { kind: "string", value: "a Room(#lobby)" },
        pid: { kind: "pid", value: "<0.181.0>" },
      },
    },
  };

  return { workspace, classes, objects, refs };
})();
