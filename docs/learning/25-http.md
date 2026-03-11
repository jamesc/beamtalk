## HTTP Client

Beamtalk's `HTTPClient` lets you make HTTP requests — both as simple class-level
calls and through a configurable actor for repeated requests to the same server.

All request methods return a `Result` (see chapter 12). Use `unwrap` to extract
the response or `isOk` / `isError` to check the outcome.

## Quick requests with class methods

The simplest way to make a request — no setup needed:

```beamtalk
// GET request
resp := (HTTPClient get: "https://api.example.com/users") unwrap
resp status     // HTTP status code (e.g. 200)
resp ok         // true if status is 200–299
resp body       // response body as String
resp headers    // response headers as a list
```

POST, PUT, and DELETE work the same way:

```beamtalk
// POST with a body
resp := (HTTPClient post: "https://api.example.com/users" body: "data") unwrap

// PUT with a body
resp := (HTTPClient put: "https://api.example.com/users/1" body: "update") unwrap

// DELETE
resp := (HTTPClient delete: "https://api.example.com/users/1") unwrap
```

## The HTTPResponse object

Every successful request returns an `HTTPResponse`:

```beamtalk
resp := (HTTPClient get: url) unwrap
resp status      // → 200 (Integer)
resp ok          // → true (Boolean, true if 200–299)
resp body        // → "..." (String)
resp headers     // → list of header tuples
resp bodyAsJson  // → Dictionary (parsed JSON)
```

`bodyAsJson` parses the body as JSON, returning a `Dictionary`:

```beamtalk
resp := (HTTPClient get: "https://api.example.com/users/1") unwrap
data := resp bodyAsJson
data at: #name    // → "Alice"
data at: #email   // → "alice@example.com"
```

## Configurable client actor

For repeated requests to the same server, spawn an `HTTPClient` actor with
a base URL. Paths are appended automatically:

```beamtalk
client := HTTPClient spawnWith: #{
  #baseUrl => "https://api.example.com",
  #headers => #(),
  #timeout => 5000
}

// Requests use relative paths
resp := (client get: "/users") unwrap
resp := (client post: "/users" body: payload) unwrap

// Clean up when done
client stop
```

The actor maintains the base URL and default headers, reducing repetition
when making many requests to the same API.

## Custom request options

`request:url:options:` gives full control over headers, body, and timeout:

```beamtalk
// Set custom headers
resp := (HTTPClient request: #get url: url options: #{
  #headers => #(#("Accept", "application/json"),
                #("Authorization", "Bearer token123"))
}) unwrap

// Set a custom timeout (in milliseconds)
resp := (HTTPClient request: #post url: url options: #{
  #body => payload,
  #timeout => 30000
}) unwrap
```

## Error handling

All methods return `Result`. Use `isOk` / `isError` for safe checking:

```beamtalk
result := HTTPClient get: "https://unreachable.example.com"
result isOk     // → false (connection failed)
result isError  // → true
```

Invalid arguments raise type errors:

```beamtalk
// URL must be a String
HTTPClient get: 42           // raises #type_error

// Must use spawn, not new
HTTPClient new               // raises #instantiation_error
```

## Working with JSON APIs

A common pattern: serialize a Dictionary to JSON, POST it, and parse the response:

```beamtalk
payload := Json generate: #{#name => "Alice", #age => 30}

resp := (HTTPClient request: #post url: "https://api.example.com/users" options: #{
  #body => payload,
  #headers => #(#("Content-Type", "application/json"))
}) unwrap

user := resp bodyAsJson
user at: #id    // → server-assigned ID
```

## Summary

**Class methods (quick requests):**

```text
HTTPClient get: url                    → Result<HTTPResponse>
HTTPClient post: url body: string      → Result<HTTPResponse>
HTTPClient put: url body: string       → Result<HTTPResponse>
HTTPClient delete: url                 → Result<HTTPResponse>
HTTPClient request: method url: url options: dict  → Result<HTTPResponse>
```

**Actor methods (configured client):**

```text
client := HTTPClient spawnWith: #{#baseUrl => url, #headers => ..., #timeout => ms}
client get: path              → Result<HTTPResponse>
client post: path body: str   → Result<HTTPResponse>
client put: path body: str    → Result<HTTPResponse>
client delete: path           → Result<HTTPResponse>
client stop                   → #ok
```

**HTTPResponse accessors:**

```text
resp status      → Integer (HTTP status code)
resp ok          → Boolean (status 200–299)
resp body        → String
resp headers     → List of header tuples
resp bodyAsJson  → Dictionary (parsed JSON)
```

**Options dictionary keys:**

```text
#body     → String (request body)
#headers  → List of #(name, value) tuples
#timeout  → Integer (milliseconds)
```

## Exercises

**1. Check a response.** Describe how you would make a GET request to a URL and
check whether it succeeded (status 200–299). What method on `HTTPResponse`
tells you this?

<details>
<summary>Hint</summary>

```text
result := HTTPClient get: "https://api.example.com/health"
result isOk ifTrue: [
  resp := result unwrap
  resp ok          // => true if status is 200-299
  resp status      // => the actual status code (e.g. 200)
]
```

`resp ok` returns `true` for 2xx status codes. `result isOk` tells you the
request completed (no network error), while `resp ok` tells you the server
returned a success status.
</details>

**2. Reusable client.** How would you set up an `HTTPClient` actor for repeated
requests to `https://api.example.com` with a 10-second timeout?

<details>
<summary>Hint</summary>

```text
client := HTTPClient spawnWith: #{
  #baseUrl => "https://api.example.com",
  #headers => #(),
  #timeout => 10000
}
// All requests use relative paths:
resp := (client get: "/users") unwrap
client stop    // clean up when done
```
</details>

**3. POST JSON.** How would you POST a JSON payload with the correct
Content-Type header using `request:url:options:`?

<details>
<summary>Hint</summary>

```text
payload := Json generate: #{"name" => "Alice", "age" => 30}
resp := (HTTPClient request: #post url: url options: #{
  #body => payload,
  #headers => #(#("Content-Type", "application/json"))
}) unwrap
```

Use `Json generate:` to serialize the dictionary, then set the Content-Type
header so the server knows to parse it as JSON.
</details>

Next: this completes the learning guide!
See `docs/beamtalk-language-features.md` for the full language reference.
