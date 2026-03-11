## HTTP Client

Beamtalk's `HTTPClient` lets you make HTTP requests — both as simple class-level
calls and through a configurable actor for repeated requests to the same server.

All request methods return a `Result` (see chapter 12). Use `unwrap` to extract
the response or `isOk` / `isError` to check the outcome.

## Quick requests with class methods

The simplest way to make a request — no setup needed:

```beamtalk
TestCase subclass: Ch25QuickRequests
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testGet =>
    @expect type
    resp := (HTTPClient get: self.baseUrl ++ "/get") unwrap
    self assert: resp ok
    self assert: resp status equals: 200

  testPost =>
    @expect type
    resp := (HTTPClient post: self.baseUrl ++ "/post" body: "hello") unwrap
    self assert: resp ok
```

Available class methods:

```beamtalk
TestCase subclass: Ch25ClassMethods
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testPut =>
    @expect type
    resp := (HTTPClient put: self.baseUrl ++ "/put" body: "data") unwrap
    self assert: resp ok

  testDelete =>
    @expect type
    resp := (HTTPClient delete: self.baseUrl ++ "/delete") unwrap
    self assert: resp ok
```

## The HTTPResponse object

Every successful request returns an `HTTPResponse` with these accessors:

```beamtalk
TestCase subclass: Ch25Response
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testResponseAccessors =>
    @expect type
    resp := (HTTPClient get: self.baseUrl ++ "/get") unwrap
    self assert: resp status equals: 200
    self assert: resp ok
    self assert: (resp body isKindOf: String)
    self assert: (resp headers isKindOf: Array)
```

### Parsing JSON responses

`bodyAsJson` parses the response body as JSON, returning a `Dictionary`:

```beamtalk
TestCase subclass: Ch25JsonResponse
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testBodyAsJson =>
    @expect type
    resp := (HTTPClient get: self.baseUrl ++ "/json") unwrap
    @expect type
    data := resp bodyAsJson
    self assert: (data isKindOf: Dictionary)
```

## Configurable client actor

For repeated requests to the same server, spawn an `HTTPClient` actor with
a base URL. Paths are appended automatically:

```beamtalk
TestCase subclass: Ch25ActorClient
  state: baseUrl = ""
  state: client = nil
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self.client := HTTPClient spawnWith: #{
      #baseUrl => self.baseUrl,
      #headers => #(),
      #timeout => 5000
    }
    self

  tearDown =>
    self.client stop
    (Erlang beamtalk_http_test_server) stop
    self

  testActorGet =>
    @expect type
    resp := (self.client get: "/get") unwrap
    self assert: resp ok
    self assert: resp status equals: 200

  testActorPost =>
    @expect type
    resp := (self.client post: "/post" body: "payload") unwrap
    self assert: resp ok
```

## Custom request options

`request:url:options:` gives full control over headers, body, and timeout:

```beamtalk
TestCase subclass: Ch25CustomRequest
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testWithHeaders =>
    @expect type
    resp := (HTTPClient request: #get url: self.baseUrl ++ "/get" options: #{
      #headers => #(#("Accept", "application/json"))
    }) unwrap
    self assert: resp ok

  testWithTimeout =>
    @expect type
    resp := (HTTPClient request: #get url: self.baseUrl ++ "/get" options: #{
      #timeout => 10000
    }) unwrap
    self assert: resp ok
```

## Error handling

Invalid inputs raise type errors. Connection failures return error Results:

```beamtalk
TestCase subclass: Ch25Errors
  testTypeErrors =>
    self should: [HTTPClient get: 42] raise: #type_error
    self should: [HTTPClient post: 42 body: "x"] raise: #type_error

  testInstantiationError =>
    // Must use spawn, not new
    self should: [HTTPClient new] raise: #instantiation_error
```

## Working with JSON APIs

A common pattern is to POST JSON and parse the response:

```beamtalk
TestCase subclass: Ch25JsonApi
  state: baseUrl = ""
  setUp =>
    port := (Erlang beamtalk_http_test_server) start
    self.baseUrl := "http://localhost:" ++ port asString
    self

  tearDown =>
    (Erlang beamtalk_http_test_server) stop
    self

  testPostJson =>
    payload := Json generate: #{#name => "Alice", #age => 30}
    @expect type
    resp := (HTTPClient request: #post url: self.baseUrl ++ "/json" options: #{
      #body => payload,
      #headers => #(#("Content-Type", "application/json"))
    }) unwrap
    self assert: resp ok
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
#headers  → Array of #(name, value) tuples
#timeout  → Integer (milliseconds)
```

Next: this completes the learning guide!
See `docs/beamtalk-language-features.md` for the full language reference.
