# Chat Room Demo — Multi-Actor Showcase

This example demonstrates Beamtalk's core features through an interactive chat room with multiple classes, actors, value types, and inheritance.

## What You'll Learn

| Concept | Where |
|---------|-------|
| **Actors** | `ChatRoom`, `ChatMember`, `Moderator` — independent processes with isolated state |
| **Value types** | `Message` — lightweight Object subclass, no process overhead |
| **Inheritance** | `Moderator` extends `ChatMember` with `super` calls |
| **Collections** | `Set` for member tracking, `List` for message history |
| **Blocks** | `do:` iteration for broadcasting messages |
| **Message passing** | Async actor-to-actor communication |

## Files

| File | Class | Type | Demonstrates |
|------|-------|------|-------------|
| `message.bt` | `Message` | Value type | Immutable data, `new:` initialization, `describe` |
| `chat_room.bt` | `ChatRoom` | Actor | Set ops, broadcasting via `do:`, message history |
| `chat_member.bt` | `ChatMember` | Actor | Actor references, inbox, room field |
| `moderator.bt` | `Moderator` | Actor | Inheritance, `super` calls, mute/kick |

## Starting the Workspace

```bash
cd examples/chat-room
beamtalk repl
```

### Step 1: Load all classes

```beamtalk
> :load src/message.bt
> :load src/chat_room.bt
> :load src/chat_member.bt
> :load src/moderator.bt
```

### Step 2: Create the room and members

```beamtalk
> room := ChatRoom spawn
> alice := ChatMember spawn
> bob := ChatMember spawn
> alice setName: "Alice"
> bob setName: "Bob"
```

### Step 3: Connect and join

```beamtalk
> alice setRoom: room
> bob setRoom: room
> room join: alice
> room join: bob
> room online
```

### Step 4: Chat!

```beamtalk
> room say: "Hello everyone!" from: "Alice"
> room say: "Hey Alice!" from: "Bob"
> room getHistory
```

### Step 5: Check inboxes

```beamtalk
> alice getInbox
> bob getInbox
```

### Step 6: Members can leave

```beamtalk
> room leave: alice
> room say: "Anyone there?" from: "Bob"
> alice getInbox    // Alice didn't receive it
> bob getInbox      // Bob did
```

## Value Types: Message

The `Message` class is an **Object subclass** (value type), not an Actor. It's an immutable Erlang map with no process overhead:

```beamtalk
> msgArgs := #{#text => "Hello!", #sender => "Alice"}
> msg := Message new: msgArgs
> msg getText       // => Hello!
> msg getSender     // => Alice
> msg describe      // => Alice: Hello!
> msg class         // => Message
```

**Key difference from Actors:**
- Created with `new` / `new:`, not `spawn`
- No process — lives in the caller's memory
- Immutable — fields can't be changed after creation

## Moderator: Inheritance in Action

`Moderator` is a **subclass of ChatMember**, demonstrating Actor inheritance:

```beamtalk
> mod := Moderator spawn
> mod setName: "Admin"      // Inherited from ChatMember
> mod setRoom: room         // Inherited from ChatMember
> room join: mod
> mod mute: "Bob"           // New method
> mod isMuted: "Bob"        // => true
> mod unmute: "Bob"
```

The `say:` method overrides ChatMember's version using `super`:

```beamtalk
// Moderator's say: adds "[MOD] " prefix, then calls ChatMember's say:
say: message =>
    super say: "[MOD] " ++ message
```

## Architecture

```
ChatMember (Actor)
  ├── state: name, inbox, room
  ├── setName:, setRoom:, say:, receiveMessage:, getInbox, getName
  └── Moderator (Actor, subclass of ChatMember)
        ├── state: muted (Set)
        ├── say: (overrides, uses super)
        └── kick:, mute:, unmute:, isMuted:, getMuted

ChatRoom (Actor)
  ├── state: members (Set), history (List)
  └── join:, leave:, say:from:, online, getHistory

Message (Object subclass — value type)
  ├── state: text, sender
  └── getText, getSender, describe
```

## Key Patterns

### Broadcasting with `do:`
```beamtalk
self.members do: [:m | m receiveMessage: formatted]
```
Iterates over the Set of actor references, sending each member the message.

### Immutable state updates
```beamtalk
self.members := self.members add: member
self.history := self.history ++ #(formatted)
```
Creates **new** collections — the old ones are unchanged (functional style).

### `new:` initialization for value types
```beamtalk
args := #{#text => "Hello", #sender => "Alice"}
msg := Message new: args
```
Creates a new instance by merging provided fields with defaults.

## What's Next?

Try these experiments:
1. **Add more members** and watch Set handle them
2. **Use `mod say:`** to see `[MOD]` prefix via super
3. **Hot reload**: Edit a `.bt` file and `:reload` it — existing actors get new methods!
4. **Multiple rooms**: Create several ChatRoom instances and move members between them
