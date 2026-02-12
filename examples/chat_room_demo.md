# Chat Room Demo Walkthrough

This walkthrough demonstrates Beamtalk's multi-actor capabilities through an interactive chat room example. You'll see actors, message passing, collections (Set and List), and blocks in action.

## What You'll Learn

- **Actors**: Independent processes with isolated state
- **Message Passing**: Async communication between actors
- **Collections**: Set for member tracking, List for message history
- **Blocks**: Iteration with `do:` for broadcasting messages
- **Live Development**: Interactive coding in the REPL

## Prerequisites

Make sure you have Beamtalk installed and the runtime built:

```bash
cargo build --release
cd runtime && rebar3 compile && cd ..
```

## Step 1: Start the REPL

```bash
$ beamtalk repl
Beamtalk REPL v0.1.0
Type :help for available commands
>
```

## Step 2: Load the Chat Room

```beamtalk
> :load examples/chat_room.bt
Loaded: ChatRoom, ChatMember
```

This loads two actor classes:
- `ChatRoom`: Manages members (Set) and message history (List)
- `ChatMember`: Represents a chat participant with name and inbox

## Step 3: Create a Chat Room

```beamtalk
> room := ChatRoom spawn
<pid>
```

The `spawn` message creates a new actor process. This is an **actor reference** (Erlang PID) that you'll use to send messages to the room.

## Step 4: Create Chat Members

```beamtalk
> alice := ChatMember spawn
<pid>

> bob := ChatMember spawn
<pid>
```

Now we have three actors running:
- 1 ChatRoom
- 2 ChatMembers

Each actor runs in its own process with isolated state.

## Step 5: Set Up Members

Give each member a name and connect them to the room:

```beamtalk
> alice setName: "Alice"
<future>

> bob setName: "Bob"
<future>
```

Actor messages return **futures** immediately. The message is sent asynchronously, and the future represents the eventual result. For state changes, we don't need to await the result.

Now connect members to the room:

```beamtalk
> alice setRoom: room
<future>

> bob setRoom: room
<future>
```

## Step 6: Members Join the Room

```beamtalk
> room join: alice
<future>

> room join: bob
<future>
```

Behind the scenes:
1. `room` receives the `join:` message with alice's PID
2. ChatRoom adds alice to its `members` Set
3. The same happens for bob

The `members` field now contains a **Set of actor references** (PIDs).

## Step 7: Check Who's Online

```beamtalk
> (room online) await
<Set with 2 members>

> ((room online) await) size
2
```

The `await` method blocks until the future resolves. Here we're asking the room for its member Set and getting the size.

## Step 8: Send Messages

Now for the fun part! Let's send a message:

```beamtalk
> room say: "Hello everyone!" from: alice
<future>
```

What just happened?
1. Room receives `say:from:` message
2. Room appends "Hello everyone!" to its `history` List
3. Room calls `do:` on the `members` Set with a block: `[:m | m receiveMessage: "Hello everyone!"]`
4. The block is called for each member (alice and bob)
5. Each member receives the message in their inbox

This demonstrates:
- **List concatenation** (`,` operator)
- **Set iteration** (`do:` method)
- **Blocks as closures** (captured `message` variable)
- **Actor method calls inside blocks** (calling `receiveMessage:` on each member)

## Step 9: Check Message History

```beamtalk
> (room getHistory) await
["Hello everyone!"]
```

The room maintains a **List** of all messages sent.

## Step 10: Check Member Inboxes

```beamtalk
> (alice getInbox) await
["Hello everyone!"]

> (bob getInbox) await
["Hello everyone!"]
```

Both members received the broadcast message!

## Step 11: More Conversation

```beamtalk
> room say: "Hey Alice!" from: bob
<future>

> (room getHistory) await
["Hello everyone!", "Hey Alice!"]

> (alice getInbox) await
["Hello everyone!", "Hey Alice!"]

> (bob getInbox) await
["Hello everyone!", "Hey Alice!"]
```

Note that members receive their own messages too (just like in real chat apps).

## Step 12: Member Leaves

```beamtalk
> room leave: alice
<future>

> ((room online) await) size
1
```

Alice is removed from the members Set.

## Step 13: Messages After Leaving

```beamtalk
> room say: "Anyone there?" from: bob
<future>

> (alice getInbox) await
["Hello everyone!", "Hey Alice!"]

> (bob getInbox) await
["Hello everyone!", "Hey Alice!", "Anyone there?"]
```

Alice didn't receive the new message because she's no longer in the room!

## Key Takeaways

### Actors Are Processes
Each `spawn` creates a new Erlang process with isolated state. Messages are sent asynchronously, and actors process them one at a time (no race conditions!).

### Collections Are Immutable
When we do `self.members := self.members add: member`, we're creating a **new Set** with the member added. The old Set is unchanged. This is Smalltalk/functional style.

### Blocks Are Powerful
The `do:` iteration uses a **block**: `[:m | m receiveMessage: message]`. The block:
- Takes one parameter (`m` = each member)
- Captures variables from surrounding scope (`message`)
- Executes for each element in the Set

### Futures Enable Async
All actor messages return futures immediately. You can:
- Ignore the future for "fire and forget" messages
- `await` to get the result synchronously
- Chain with `whenResolved:whenRejected:` (not shown here)

## What's Next?

Try these experiments:

1. **Add more members**: Spawn more ChatMembers and see how Set handles them
2. **Format messages**: Modify `say:from:` to include sender name: `sender getName ++ ": " ++ message`
3. **Private messages**: Add a `whisper:to:from:` method that sends to one member
4. **Emoji reactions**: Add a `react:to:from:` method (great hot reload demo!)
5. **Multiple rooms**: Create several ChatRoom instances and move members between them

## Hot Reload Demo (Advanced)

One of Beamtalk's strengths is **live coding**. While the REPL is running with active actors, you can:

1. Edit `examples/chat_room.bt` to add a new method
2. `:reload examples/chat_room.bt` in the REPL
3. The new method is immediately available to **existing actors**
4. No restart, no lost state!

Example: Add emoji reactions without restarting:

```beamtalk
// In chat_room.bt, add to ChatRoom:
react: emoji to: messageIndex from: sender =>
  // Store reactions in a new Dictionary field
  self.reactions := self.reactions ...
```

Then `:reload` and your running room gets the new feature!

## Architecture Notes

### BEAM Mapping

- `Actor spawn` → `gen_server:start_link/4`
- Async message send → `gen_server:cast/2` + future
- `await` → `receive` block waiting for future response
- Actor state → gen_server state record

### Why Sets Work With Actors

Actor references are Erlang PIDs, which are orderable (`<`, `>`, `==`). The Set implementation uses `ordsets` (ordered lists), so PIDs work perfectly.

### Broadcasting Pattern

The `do:` iteration over a Set of actor references is a common pattern in actor systems:

```beamtalk
self.members do: [:m | m someMessage: value]
```

Each iteration sends an async message. All members receive messages concurrently.

## Conclusion

You've seen:
- ✅ Multiple actors with independent state
- ✅ Async message passing with futures
- ✅ Set operations with actor references
- ✅ List operations for history
- ✅ Blocks for iteration
- ✅ Live REPL interaction

The chat room is a simple example, but the patterns scale to complex actor systems. Beamtalk runs on the BEAM VM, giving you OTP's battle-tested concurrency and fault tolerance!

Happy coding! 🚀
