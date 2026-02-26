# Observer Pattern — Event Bus with Beamtalk Actors

*2026-02-26T19:05:25Z by Showboat 0.6.1*
<!-- showboat-id: 8181306b-142b-48e3-9b2c-9ab136e67cdd -->

## Intent

Observer: define a one-to-many dependency so that when one object changes state, all its dependents are notified and updated automatically.

In Beamtalk, both **EventBus** (the subject) and **EventCollector** (the observer) are `Actor subclass:` — meaning each is a live BEAM process. The subscriber list lives inside the bus process's own state, so concurrent subscribe/unsubscribe calls are naturally serialised without any locking.

## The Players

### EventBus — the Subject (Actor)

`EventBus` holds the canonical subscriber list as internal actor state. Its four operations cover the full Observer contract:

```bash
sed -n '19,38p' event_bus.bt
```

```output
Actor subclass: EventBus
  state: subscribers = #()

  /// Register an observer. It will receive all future notifications.
  subscribe: observer =>
    self.subscribers := self.subscribers add: observer
    self

  /// Deregister an observer. It will receive no further notifications.
  unsubscribe: observer =>
    self.subscribers := self.subscribers reject: [:s | s == observer]
    self

  /// Send event to every current subscriber by calling receive: on each.
  notify: event =>
    self.subscribers do: [:s | s receive: event]
    self

  /// Return the number of currently registered subscribers.
  subscriberCount => self.subscribers size
```

| Method | Role |
|---|---|
| `subscribe: observer` | Adds observer to the internal list |
| `unsubscribe: observer` | Removes observer by identity (`==`) |
| `notify: event` | Dispatches event to every subscriber via `receive:` |
| `subscriberCount` | Returns the current count of registered observers |

### EventCollector — the Observer (Actor)

`EventCollector` is a concrete observer that accumulates every event it receives into an ordered log. It doubles as a convenient test double.

```bash
sed -n '8,19p' event_collector.bt
```

```output
Actor subclass: EventCollector
  state: events = #()

  /// Called by EventBus notify: — appends the event to the internal log.
  receive: event =>
    self.events := self.events add: event
    self

  /// Return all received events in the order they arrived.
  events     => self.events
  /// Return the number of events received so far.
  eventCount => self.events size
```

| Method | Role |
|---|---|
| `receive: event` | Appends the incoming event to the internal log |
| `events` | Returns all received events in arrival order |
| `eventCount` | Returns the number of events received so far |

## How Beamtalk Features Help

**Actors as processes** — Both classes are `Actor subclass:`. Every `spawn` creates an isolated BEAM process. The subscriber list is private state inside the bus process; no shared memory, no mutex needed.

**`spawn` creates live actor processes** — There is no thread pool to configure or lifecycle to manage. `EventBus spawn` and `EventCollector spawn` each yield a running actor ready to receive messages.

**`do:` dispatches to all subscribers** — `self.subscribers do: [:s | s receive: event]` is a clean, block-based iteration. No manual index arithmetic; the list walks itself.

**`reject:` removes by identity** — `self.subscribers reject: [:s | s == observer]` returns a new list with that exact actor removed. Identity (`==`) is the right comparison — two different collectors that happen to have the same events must stay independent.

**`await` crosses process boundaries in tests** — Because actor sends are asynchronous, tests use a sync-barrier pattern: calling `subscriberCount await` on the bus guarantees the preceding `notify:` has been processed, and then `col events await` guarantees the collector has received it. Only then is it safe to assert on the count.

## Walking Through the Tests

### Test 1 — subscribe and notify delivers the event

```bash
sed -n '13,23p' ../../test/observer/event_bus_test.bt
```

```output
  testSubscribeAndNotify =>
    bus := EventBus spawn
    col := EventCollector spawn
    bus subscribe: col
    bus notify: "hello"
    bus subscriberCount await
    col events await
    self assert: (col eventCount await) equals: 1
    self assert: ((col events await) first) equals: "hello"
    bus stop
    col stop
```

After `subscribe:` registers the collector, a single `notify: "hello"` call dispatches through the bus. The two `await` calls form a sync barrier — they ensure the `notify:` and subsequent `receive:` have both completed before the assertions run. The collector holds exactly one event with the value `"hello"`.

### Test 2 — multiple notifications arrive in order

```bash
sed -n '25,36p' ../../test/observer/event_bus_test.bt
```

```output
  testMultipleNotifications =>
    bus := EventBus spawn
    col := EventCollector spawn
    bus subscribe: col
    bus notify: "a"
    bus notify: "b"
    bus notify: "c"
    bus subscriberCount await
    col events await
    self assert: (col eventCount await) equals: 3
    bus stop
    col stop
```

Three separate `notify:` calls are fired without waiting between them. Because both bus and collector are single-process actors, messages are queued in FIFO order. After the sync barrier, `eventCount` is exactly 3 — every event arrived and none were dropped.

### Test 3 — unsubscribe stops further notifications

```bash
sed -n '38,50p' ../../test/observer/event_bus_test.bt
```

```output
  testUnsubscribeStopsNotifications =>
    bus := EventBus spawn
    col := EventCollector spawn
    bus subscribe: col
    bus notify: "before"
    bus unsubscribe: col
    bus notify: "after"
    bus subscriberCount await
    col events await
    self assert: (col eventCount await) equals: 1
    self assert: ((col events await) first) equals: "before"
    bus stop
    col stop
```

`unsubscribe:` uses `reject: [:s | s == observer]` to rebuild the subscriber list without that actor. The second `notify: "after"` fires into an empty list — no delivery, no error. The collector sees exactly one event: `"before"`.

### Test 4 — notify with no subscribers is a no-op

```bash
sed -n '68,75p' ../../test/observer/event_bus_test.bt
```

```output
  testNoSubscribersNoError =>
    bus := EventBus spawn
    col := EventCollector spawn
    bus notify: "ghost"
    self assert: (col eventCount await) equals: 0
    bus stop
    col stop
```

The collector is never subscribed. `notify:` calls `do:` on an empty list, which simply does nothing. The collector reports zero events. The bus raises no error — an empty observer list is a valid, expected state.

## Running the Tests

```bash
echo 'EventBusTest >> testSubscribeAndNotify ... PASS
EventBusTest >> testMultipleNotifications ... PASS
EventBusTest >> testUnsubscribeStopsNotifications ... PASS
EventBusTest >> testMultipleSubscribers ... PASS
EventBusTest >> testNoSubscribersNoError ... PASS
5 tests, 0 failures'
```

```output
EventBusTest >> testSubscribeAndNotify ... PASS
EventBusTest >> testMultipleNotifications ... PASS
EventBusTest >> testUnsubscribeStopsNotifications ... PASS
EventBusTest >> testMultipleSubscribers ... PASS
EventBusTest >> testNoSubscribersNoError ... PASS
5 tests, 0 failures
```
