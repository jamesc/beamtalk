# Do I Sound Crazy?

*The first post on Dead Text — on live systems, agent runtimes, and why dead text was always the problem.*

---

Every AI coding agent in the world is drowning in dead text.

Reading files. Guessing at changes. Writing files back. Running a build. Waiting. Parsing error output — unstructured strings designed for human eyes. Guessing again. The entire development loop is a conversation with corpses: source files that can't answer questions, error messages that can't explain themselves, a runtime that doesn't exist until you compile and pray.

We tolerate this because we're good at it. Humans maintain mental models that persist across compile cycles. We remember what that function does. We know why that test fails. We carry context in our heads that the tools never had to provide.

AI agents can't do that. They rebuild their understanding from scratch every time. And watching them drown in dead text — reading files, guessing, failing, reading error strings, guessing again — is what made me realize: **the dead text problem isn't new. We've just been compensating for it so long we forgot it was there.**

I'm building a programming language to fix it. I know how that sounds. Bear with me.

## I've Always Known This Was the Problem

My first real job was at Harlequin, the Cambridge company that built Dylan — a language that tried to be Lisp with a more accessible syntax. I was twenty-something, surrounded by people building a programming language from scratch, and I thought: *this is the most interesting thing you can do with a computer.*

I didn't get to build the language. I was too junior. But the itch was planted. I started collecting programming language books the way other people collect vinyl — C, C++, Java, Dylan, Lisp, Haskell, ML, Scala, Erlang, Ruby, Smalltalk. They're still on my shelf. Every few years the collection got weirder — category theory, type theory, process algebras. My wife stopped asking.

What I really wanted was a Symbolics machine — the Lisp workstations from the '80s where the entire system was live, introspectable, modifiable while running. Development wasn't "edit a file and recompile." It wasn't dead text. It was a conversation with a living system. You could inspect any object, redefine any function, and the system just absorbed the change and kept going.

I couldn't afford a Symbolics. I got a SPARCstation instead and became a sysadmin.

And then, without quite realizing it, I spent the next twenty years fighting dead text in different forms.

At CERN, building tooling for operators of the Large Hadron Collider grid — because the operational runbooks were dead text and the operators needed live systems that could answer questions about what was actually happening.

At Chef — because infrastructure documentation was dead text, and we turned it into living code that continuously converged machines toward the desired state. "Infrastructure as code" was, at its core, a war on dead text: replace the wiki page that says "install nginx version 1.14" with a recipe that *does* it and *knows* whether it's done.

At Microsoft — my team owned the Azure CLI, Azure PowerShell, the Terraform and Ansible integrations, and every management SDK. If you've ever talked to Azure programmatically, you used something we built. Making the dead-text loop faster, better, smoother. But never questioning whether the loop itself was the problem.

I didn't have the phrase "dead text" then. But every job was the same fight: take something static and make it live. Take something you read and turn it into something you talk to.

## The Lightning Bolt

I got made redundant last year. After two decades of building tools for other people's platforms, I was staring at a blank page. Everyone asked if I'd do a startup. I was blank on an idea.

And then one morning, watching Claude struggle to modify a Python codebase — drowning in dead text, exactly the way I'd watched operators drown in runbooks at CERN, the way I'd watched sysadmins drown in config files before Chef — something clicked.

I'd seen this problem solved. In 1980. It was called Smalltalk.

Smalltalk environments were live. Objects existed in a running image. You sent messages to them. You redefined methods on running instances. The entire system was introspectable — you could ask any object what it understood, what class it was, what methods it had. There was no dead text. Development was a conversation with a living system.

That's exactly what AI agents need. And there's a production-grade runtime that handles the hard parts — concurrency, fault tolerance, distribution, hot code reload — better than anything else: the BEAM.

The BEAM virtual machine, Erlang's runtime, runs WhatsApp, Discord, and telecom infrastructure that can't go down. It's built around lightweight processes with isolated state that communicate by sending messages — a remarkably similar architecture to what Alan Kay envisioned when he coined "object-oriented programming," even though the lineages are entirely different.

The lightning bolt: **what if a syntactically simple, live-running language on the BEAM is a better runtime for agents to write code?**

Not "what if I build a nice language." What if the Smalltalk development model — the one I'd wanted since Harlequin, since I couldn't afford a Symbolics — is the answer to a problem that didn't fully exist until 2024?

I'd never felt anything like it. I went from blank page to twelve-hour days overnight.

## Twelve Hours a Day for Two Months

I've been writing code — with Copilot and Claude as development partners — for twelve hours a day since that morning. Look at my GitHub contribution graph. It's a solid wall of green. The language is being built in the very development loop it's trying to replace, which has been educational in ways I didn't expect.

The language is called Beamtalk. It compiles to BEAM bytecode through Core Erlang. The compiler is written in Rust. In two months, working with AI agents as collaborators, it has a working compiler, REPL, LSP, VS Code extension, and an MCP server that exposes the entire runtime to AI agents.

Here's what it looks like — and why the dead text disappears:

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  getValue => self.value
```

That's a complete, supervised BEAM process. And here's how an agent interacts with it — not through files, but through a living system:

```beamtalk
// Agent evaluates via MCP — immediate result, not "compile and wait"
>> counter := Counter spawn
=> Counter<0.156.0>

>> counter increment
=> 1

// Agent redefines the method on the running actor using >> syntax
>> Counter >> increment => self.value := self.value + 10

>> counter increment
=> 11    // State preserved, method changed. No restart.
```

No dead text anywhere in this loop. The agent talks to the system. The system answers. The agent changes a method and the running actor picks up the change without losing state. When the agent makes a mistake and crashes an actor, the supervisor restarts it automatically. The agent gets a structured error — `kind`, `class`, `selector`, `hint` — not a stack trace to grep through. It adjusts and tries again.

This is the development loop I wanted when I was staring at Symbolics brochures. Except now it runs on one of the most battle-tested concurrent runtimes in existence, it's open source, and the developer sitting next to me is an AI.

## The Text Came Alive

Here's the thing I didn't expect: the features that kill dead text for agents are the same features that made Smalltalk beloved by the programmers who used it. Liveness. Introspection. Minimal syntax. Hot code reload. Structured feedback. Objects you interact with, not files you compile.

Smalltalk developers spent decades saying their way of working was better. The industry mostly ignored them, because the ecosystem was closed, proprietary, and didn't integrate with anything.

The BEAM is none of those things. It's open source. It's battle-tested. It runs in production at massive scale. And it was designed, from the ground up, for exactly the kind of isolated-process, message-passing, supervisor-recovered architecture that makes live development safe.

AI agents didn't create the need for live development environments. They revealed it. The dead-text loop — edit, compile, wait, read errors, guess, repeat — was always the problem. We just compensated so well we stopped noticing.

## The Honest Version

Could you kill some of the dead text by adding an MCP server to Elixir's IEx? Yes. Elixir already has LiveBook, hot reload, fast compilation. Could you do it by exposing GemStone/S through MCP? Probably. The live-object model already exists there.

Beamtalk's bet is that designing the language from scratch for this model — where the class-kind system, the error structure, the syntax surface area, and the introspection capabilities are all built knowing that agents are first-class developers — produces a qualitatively better result than retrofitting live interactions onto a language designed around dead text.

Maybe I'm wrong. Maybe MCP-for-Elixir is the right answer and this is an elaborate yak-shave driven by a thirty-year-old itch. But two months in, the language works, the agents use it, and I'm writing more code than I have since I was twenty-five.

There are worse ways to spend a redundancy.

---

*Beamtalk is open source at [beamtalk.dev](https://www.beamtalk.dev). The technical deep dive — class kinds, supervision syntax, the MCP tool suite, and why the BEAM is the right runtime — is in the companion post, [Beamtalk: A Language Designed for Agentic Development](why-beamtalk-technical.md).*

*This is the first post on Dead Text. There's a lot more to say about live systems, agent runtimes, and why the decades-old ideas keep turning out to be right. [Subscribe](https://deadtext.substack.com) if you want to follow along.*

*If you want to argue about whether I'm crazy, I'm on [LinkedIn](https://linkedin.com/in/jamesc000).*
