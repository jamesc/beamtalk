# Beamtalk BEAM Interop Specification

Beamtalk is designed for seamless integration with the BEAM ecosystem. This document specifies how Beamtalk code interoperates with Erlang, Elixir, and other BEAM languages.

**Core principle:** Interop is not optional — it's essential for adoption. The BEAM ecosystem has decades of battle-tested libraries. Beamtalk must use them with minimal friction.

---

## Table of Contents

- [1. Calling Erlang from Beamtalk](#1-calling-erlang-from-beamtalk)
- [2. Calling Elixir from Beamtalk](#2-calling-elixir-from-beamtalk)
- [3. Calling Beamtalk from Erlang/Elixir](#3-calling-beamtalk-from-erlangelixir)
- [4. Data Type Mapping](#4-data-type-mapping)
- [5. OTP Integration](#5-otp-integration)
- [6. Using Hex.pm Packages](#6-using-hexpm-packages)
- [7. Import and Aliasing](#7-import-and-aliasing)
- [8. Foreign Function Interface (FFI)](#8-foreign-function-interface-ffi)
- [9. Build System Integration](#9-build-system-integration)
- [10. Example: Using Phoenix from Beamtalk](#10-example-using-phoenix-from-beamtalk)
- [References](#references)

---

## 1. Calling Erlang from Beamtalk

### Basic Syntax

Use the `Erlang` namespace to call Erlang modules:

```
// Call Erlang's :crypto module
hash := Erlang.crypto hash: #sha256 data: "hello"

// Call Erlang's :ets module
table := Erlang.ets new: #myTable options: [#set, #public]
Erlang.ets insert: table entry: {#key, "value"}

// Call Erlang's :timer module
Erlang.timer sleep: 1000
```

### Module and Function Mapping

| Beamtalk | Erlang |
|----------|--------|
| `Erlang.crypto hash: alg data: d` | `:crypto.hash(alg, d)` |
| `Erlang.lists reverse: list` | `:lists.reverse(list)` |
| `Erlang.erlang self` | `:erlang.self()` |
| `Erlang.gen_server call: pid msg: m` | `:gen_server.call(pid, m)` |

### Erlang Functions with Multiple Arities

Erlang functions can have multiple arities. Use positional keywords or explicit arity:

```
// :lists.nth/2
element := Erlang.lists nth: 3 list: myList

// :string.split/2 vs :string.split/3
parts := Erlang.string split: str pattern: ","
parts := Erlang.string split: str pattern: "," where: #all
```

### Handling Erlang Atoms

Beamtalk symbols map directly to Erlang atoms:

```
// #ok becomes :ok
// #error becomes :error
// #undefined becomes :undefined

result := Erlang.file read_file: "test.txt"
result match: [
  {#ok, content} -> self process: content
  {#error, reason} -> self handleError: reason
]
```

---

## 2. Calling Elixir from Beamtalk

### Basic Syntax

Use the `Elixir` namespace for Elixir modules:

```
// Call Jason (JSON library)
json := Elixir.Jason decode!: '{"name": "Alice"}'
encoded := Elixir.Jason encode!: myMap

// Call Enum module
doubled := Elixir.Enum map: list fn: [:x | x * 2]
sum := Elixir.Enum reduce: list acc: 0 fn: [:acc :x | acc + x]

// Call String module
upper := Elixir.String upcase: "hello"
```

### Elixir Module Naming

Elixir modules use dot notation internally (e.g., `Phoenix.PubSub`). In Beamtalk:

```
// Phoenix.PubSub.broadcast/3
Elixir.Phoenix.PubSub broadcast: pubsub topic: "events" message: event

// Ecto.Repo functions
users := Elixir.MyApp.Repo all: Elixir.MyApp.User

// Plug.Conn
conn := Elixir.Plug.Conn put_resp_header: conn key: "content-type" value: "application/json"
```

### Elixir Structs

Elixir structs are maps with a `__struct__` key:

```
// Create an Elixir struct
user := Elixir.struct: Elixir.MyApp.User fields: {name: "Alice", email: "alice@example.com"}

// Pattern match on structs
processUser: user: %Elixir.MyApp.User{name: name} =>
  self greet: name
```

---

## 3. Calling Beamtalk from Erlang/Elixir

### Generated Module Structure

Each Beamtalk actor compiles to a standard `gen_server` module:

```
// Beamtalk source: counter.bt
Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  decrement => self.value -= 1
  getValue => ^self.value
  incrementBy: delta => self.value += delta
```

Generates an Erlang module `beamtalk_counter` with:

```erlang
-module(beamtalk_counter).
-behaviour(gen_server).

-export([start_link/1, increment/1, decrement/1, get_value/1, increment_by/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

increment(Pid) ->
    gen_server:cast(Pid, {increment}).

get_value(Pid) ->
    gen_server:call(Pid, {get_value}).

increment_by(Pid, Delta) ->
    gen_server:cast(Pid, {increment_by, Delta}).

%% gen_server callbacks...
```

### Calling from Erlang

```erlang
%% Start a Beamtalk actor
{ok, Counter} = beamtalk_counter:start_link([]),

%% Call methods
beamtalk_counter:increment(Counter),
beamtalk_counter:increment(Counter),
Value = beamtalk_counter:get_value(Counter),  %% => 2

%% Or use gen_server directly
gen_server:cast(Counter, {increment}),
gen_server:call(Counter, {get_value}).
```

### Calling from Elixir

```elixir
# Start a Beamtalk actor
{:ok, counter} = :beamtalk_counter.start_link([])

# Call methods
:beamtalk_counter.increment(counter)
:beamtalk_counter.increment(counter)
value = :beamtalk_counter.get_value(counter)  # => 2

# Or use GenServer
GenServer.cast(counter, {:increment})
GenServer.call(counter, {:get_value})
```

### Module Naming Convention

| Beamtalk Class | Generated Module |
|----------------|------------------|
| `Counter` | `beamtalk_counter` |
| `MyApp.UserAgent` | `beamtalk_my_app_user_agent` |
| `HTTP.Client` | `beamtalk_http_client` |

The `beamtalk_` prefix avoids collisions with existing modules.

---

## 4. Data Type Mapping

### Primitive Types

| Beamtalk | Erlang | Elixir |
|----------|--------|--------|
| `42` (Integer) | `42` | `42` |
| `3.14` (Float) | `3.14` | `3.14` |
| `#symbol` | `'symbol'` (atom) | `:symbol` |
| `true`, `false` | `true`, `false` | `true`, `false` |
| `nil` | `undefined` | `nil` |

### Strings and Binaries

Beamtalk strings are **UTF-8 binaries** by default, matching Elixir's approach:

```
// Single-quoted strings are UTF-8 binaries (recommended)
name := 'Alice'           // => <<"Alice">> in Erlang
multilang := '你好世界'    // => UTF-8 encoded binary
emoji := '🎉'             // => <<240,159,142,137>>

// Double-quoted strings support interpolation (also UTF-8)
greeting := "Hello, {name}!"  // => <<"Hello, Alice!">>
localized := "欢迎 {name}"     // => UTF-8 binary

// Character literals are Unicode codepoints
$a                        // => 97
$世                       // => 19990

// Erlang charlists (legacy, rarely needed)
charlist := 'hello' toCharlist  // => [104,101,108,108,111]
```

**Default:** Beamtalk strings compile to Erlang UTF-8 binaries, **not charlists**. This matches modern BEAM convention.

#### String Encoding Details

| Beamtalk | Erlang/Elixir | Bytes (example) |
|----------|---------------|----------------|
| `'hello'` | `<<"hello">>` | `[104,101,108,108,111]` (ASCII) |
| `'世界'` | `<<228,184,150,231,149,140>>` | UTF-8 encoded |
| `"Hi, {x}"` | `<<"Hi, ", X/binary>>` | Interpolated UTF-8 |
| Charlist | `[104,101,108,108,111]` | List of integers |

#### When to Use Charlists

Most Erlang modules accept binaries now. Use charlists only when:
- Required by old Erlang APIs (`:io.format/2`, some `:file` functions)
- Explicitly documented as charlist-only

```
// Modern Erlang - accepts binaries
Erlang.file read_file: 'data.txt'  // Binary path works

// Legacy Erlang - needs charlist
path := 'data.txt' toCharlist
Erlang.io format: '~s~n' toCharlist arguments: [name toCharlist]
```

### Collections

| Beamtalk | Erlang | Notes |
|----------|--------|-------|
| `[1, 2, 3]` | `[1, 2, 3]` | Linked list |
| `{1, 2, 3}` | `{1, 2, 3}` | Tuple |
| `{key: "value"}` | `#{key => <<"value">>}` | Map |
| `#[1, 2, 3]` | (array) | ETS-backed, O(1) access |

### Blocks and Functions

Beamtalk blocks compile to Erlang funs:

```
// Beamtalk block
doubled := list collect: [:x | x * 2]

// Compiles to Erlang
Doubled = lists:map(fun(X) -> X * 2 end, List)
```

### Records (Erlang)

Erlang records can be accessed but are discouraged (use maps instead):

```
// Access Erlang record field
name := Erlang.record get: userRecord field: #name record_type: #user

// Prefer converting to map
userMap := Erlang.record toMap: userRecord type: #user
```

---

## 5. OTP Integration

### Supervision Trees

Beamtalk actors can be supervised by Erlang/Elixir supervisors:

**In Elixir:**
```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Beamtalk actor
      {BeamtalkCounter, [initial: 0]},
      # Elixir worker
      {MyApp.Worker, []},
      # Beamtalk supervisor managing mixed children
      {BeamtalkResearchTeam, []}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

**In Beamtalk:**
```
// Beamtalk supervisor with Erlang/Elixir children
Supervisor subclass: MixedTeam
  children: [
    Counter spawn,                                    // Beamtalk
    Erlang.my_erlang_worker start_link: [],          // Erlang
    Elixir.MyApp.Worker start_link: []               // Elixir
  ]
  strategy: #oneForOne
```

### OTP Behaviors

Beamtalk actors implement `gen_server` by default. Other behaviors:

```
// Implement gen_statem for state machines
Actor subclass: Connection
  behavior: #gen_statem

  // States
  state: #disconnected
  state: #connecting
  state: #connected

  // State callbacks
  disconnected: #connect =>
    self startConnection
    ^{#next_state, #connecting}
```

### Application Integration

Beamtalk projects can define OTP applications:

```
// application.bt
Application subclass: MyApp
  modules: [Counter, Agent, Supervisor]

  start: _type args: _args =>
    MyApp.Supervisor startLink
```

Generates standard `myapp.app` file for OTP.

---

## 6. Using Hex.pm Packages

### Declaring Dependencies

In `beamtalk.toml` (project configuration):

```toml
[package]
name = "my_agent"
version = "0.1.0"

[dependencies]
jason = "~> 1.4"
req = "~> 0.4"
phoenix_pubsub = "~> 2.1"

[dev_dependencies]
stream_data = "~> 0.6"
```

### Fetching and Building

```bash
# Fetch dependencies from Hex.pm
beamtalk deps.get

# Compile everything (deps + Beamtalk code)
beamtalk compile

# Or use Mix integration
mix deps.get && mix compile
```

### Using Hex Packages

```
// Use Req for HTTP requests
response := Elixir.Req get!: "https://api.example.com/data"
body := response at: #body

// Use Jason for JSON
data := Elixir.Jason decode!: body

// Use Phoenix.PubSub for messaging
Elixir.Phoenix.PubSub subscribe: pubsub topic: "events"
Elixir.Phoenix.PubSub broadcast: pubsub topic: "events" message: {#new_data, data}
```

---

## 7. Import and Aliasing

### Importing Modules

Reduce verbosity with imports:

```
// Import specific module
import Elixir.Jason
import Elixir.Enum

// Now use without prefix
data := Jason decode!: jsonString
doubled := Enum map: list fn: [:x | x * 2]
```

### Aliasing

```
// Alias long module names
alias Elixir.Phoenix.PubSub as: PubSub
alias Elixir.MyApp.Repo as: Repo

// Use the alias
PubSub broadcast: pubsub topic: "events" message: msg
users := Repo all: User
```

### Import All from Namespace

```
// Import all Phoenix modules with Phoenix. prefix
import Elixir.Phoenix.*

// Now use as:
PubSub broadcast: ...
LiveView socket: ...
```

---

## 8. Foreign Function Interface (FFI)

For performance-critical code or complex interop, use explicit FFI declarations:

```
// Declare foreign function
foreign Erlang.crypto.hash(algorithm: Atom, data: Binary) -> Binary

// Declare with type conversion
foreign Elixir.Jason.decode!(json: String) -> Map
  converts: [json: #toBinary]

// Use normally after declaration
hash := crypto hash: #sha256 data: "hello"
data := Jason decode!: '{"key": "value"}'
```

### FFI for Callbacks

When Erlang/Elixir needs to call Beamtalk code:

```
// Export function for foreign callers
export handleEvent: event for: [Erlang, Elixir]

handleEvent: event =>
  // Process event from Phoenix channel, Erlang gen_event, etc.
  self processEvent: event
```

---

## 9. Build System Integration

### Standalone (beamtalk CLI)

```bash
# Create project
beamtalk new my_project
cd my_project

# Project structure
my_project/
├── beamtalk.toml       # Project config
├── src/
│   └── my_project.bt   # Main source
├── test/
│   └── my_project_test.bt
└── lib/                # Additional .bt files

# Build commands
beamtalk deps.get       # Fetch Hex dependencies
beamtalk compile        # Compile to .beam
beamtalk test           # Run tests
beamtalk repl           # Start REPL
```

### Mix Integration (Recommended for Elixir Projects)

Add to `mix.exs`:

```elixir
defmodule MyApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :my_app,
      version: "0.1.0",
      compilers: [:beamtalk] ++ Mix.compilers(),
      deps: deps()
    ]
  end

  defp deps do
    [
      {:beamtalk, "~> 0.1"},
      {:phoenix, "~> 1.7"},
      {:jason, "~> 1.4"}
    ]
  end
end
```

Beamtalk source goes in `lib/` alongside Elixir:

```
my_app/
├── lib/
│   ├── my_app.ex           # Elixir
│   ├── my_app/
│   │   ├── application.ex  # Elixir
│   │   └── worker.ex       # Elixir
│   └── agents/
│       ├── researcher.bt   # Beamtalk
│       └── critic.bt       # Beamtalk
└── mix.exs
```

### Rebar3 Integration (For Erlang Projects)

Add to `rebar.config`:

```erlang
{plugins, [rebar3_beamtalk]}.

{provider_hooks, [
    {pre, [{compile, {beamtalk, compile}}]}
]}.

{beamtalk_src_dirs, ["src/beamtalk"]}.
```

---

## 10. Example: Using Phoenix from Beamtalk

### Phoenix Channel Handler

```
// agents/chat_agent.bt
Actor subclass: ChatAgent
  state:
    model = #claude_sonnet,
    socket = nil,
    history = []

  // Called from Phoenix channel
  handleMessage: message socket: socket =>
    self.socket := socket
    self.history := self.history append: {#user, message}

    // Call LLM
    response := self generateResponse: message
    self.history := self.history append: {#assistant, response}

    // Push to Phoenix channel
    Elixir.Phoenix.Channel push: socket event: "response" payload: {text: response}
    ^#ok

  generateResponse: message =>
    // Call Anthropic API via Req
    response := Elixir.Req post!: "https://api.anthropic.com/v1/messages"
      headers: [{"Authorization", "Bearer {self apiKey}"}]
      json: {
        model: "claude-3-sonnet",
        messages: self.history
      }
    response at: #body at: "content" at: 0 at: "text"
```

### Phoenix Channel (Elixir side)

```elixir
defmodule MyAppWeb.ChatChannel do
  use Phoenix.Channel

  def join("chat:" <> room_id, _params, socket) do
    # Start Beamtalk agent for this chat
    {:ok, agent} = :beamtalk_chat_agent.start_link([])
    {:ok, assign(socket, :agent, agent)}
  end

  def handle_in("message", %{"text" => text}, socket) do
    # Delegate to Beamtalk agent
    :beamtalk_chat_agent.handle_message(socket.assigns.agent, text, socket)
    {:noreply, socket}
  end
end
```

### LiveView Integration

```
// agents/dashboard_agent.bt
Actor subclass: DashboardAgent
  state: metrics = {}

  // Called from LiveView
  getMetrics =>
    ^self.metrics

  updateMetric: name value: value =>
    self.metrics := self.metrics put: name value: value
    // Broadcast update via PubSub
    Elixir.Phoenix.PubSub broadcast:
      Elixir.MyApp.PubSub
      topic: "dashboard:updates"
      message: {#metric_updated, name, value}
```

```elixir
defmodule MyAppWeb.DashboardLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    Phoenix.PubSub.subscribe(MyApp.PubSub, "dashboard:updates")
    {:ok, agent} = :beamtalk_dashboard_agent.start_link([])
    metrics = :beamtalk_dashboard_agent.get_metrics(agent)
    {:ok, assign(socket, agent: agent, metrics: metrics)}
  end

  def handle_info({:metric_updated, name, value}, socket) do
    {:noreply, update(socket, :metrics, &Map.put(&1, name, value))}
  end
end
```

---

## References

- [Erlang/Elixir Interop](https://elixir-lang.org/getting-started/erlang-libraries.html)
- [Gleam FFI](https://gleam.run/book/tour/external-functions.html)
- [LFE Interop](https://lfe.io/books/reference/interop/)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
