# Bank Transfers — Actor Coordination Showcase

This example demonstrates **actor coordination** — the kind of problem the BEAM
virtual machine excels at. Each bank account is an independent actor (process),
and transferring money requires coordinating two actors.

## What You'll Learn

| Concept | Where |
|---------|-------|
| **Actor as entity** | Each `Account` is its own BEAM process |
| **Actor coordination** | `TransferAgent` orchestrates debit + credit |
| **Value types vs actors** | `Transaction` is immutable data; `Account` is stateful |
| **Error handling** | Overdraft → structured error with helpful message |
| **Factory pattern** | `Bank openAccount:` spawns `Account` actors |
| **Collections** | `Bank` uses a `Dictionary` of name → account |

## Classes

| Class | Type | Role |
|-------|------|------|
| `Bank` | Actor | Registry of accounts, factory |
| `Account` | Actor | Balance state, deposit/withdraw, overdraft protection |
| `Transaction` | Value | Immutable transfer record |
| `TransferAgent` | Actor | Coordinates two-account transfers |

## Starting the Workspace

```bash
cd examples/bank
beamtalk repl
```

## REPL Walkthrough

Load the bank classes:

```text
> :load src/account.bt
> :load src/transaction.bt
> :load src/bank.bt
> :load src/transfer_agent.bt
```

Typed variant:

```text
> :load src/typed_account.bt
> typed := TypedAccount spawn
> typed deposit: 100
100
> typed balance
100
```

### Create a bank and open accounts

```text
> bank := Bank spawn
> bank openAccount: "Alice"
> bank openAccount: "Bob"
```

### Deposit money

```text
> alice := bank accountFor: "Alice"
> alice deposit: 1000
1000

> bob := bank accountFor: "Bob"
> bob deposit: 500
500
```

### Check balances

```text
> alice balance
1000

> bob balance
500
```

### Transfer money

```text
> agent := TransferAgent spawn
> agent transfer: 250 from: alice to: bob
```

The `TransferAgent` sends withdraw and deposit messages to each account:

```text
> alice balance
750

> bob balance
750
```

### Overdraft protection

What happens if we try to withdraw more than the balance?

```text
> alice withdraw: 5000
ERROR: Insufficient funds (balance: 750, requested: 5000)
```

The withdrawal is rejected — the balance remains unchanged:

```text
> alice balance
750
```

### Inspect the last transfer

```text
> agent getLastAmount
250
```

## How It Works

Each `Account` is a separate BEAM process (actor). When you call
`alice deposit: 1000`, you're sending an asynchronous message to Alice's
process. The REPL auto-awaits the result so you see it immediately.

The `TransferAgent` coordinates two actors by sending messages:

1. **Withdraw** — send `withdraw:` to the source account
2. **Deposit** — send `deposit:` to the destination account

Each account is a separate BEAM process that serialises its own messages.
The `Account` enforces its own overdraft protection independently.
Note that actor messaging is asynchronous — the `TransferAgent` fires off
both operations without awaiting their results. The REPL auto-awaits the
`transfer:from:to:` call, so by the time the prompt returns both messages
have been delivered and processed in order.

## Key Takeaways

- **Actors are processes** — each account runs independently
- **Messages are async** — the REPL auto-awaits for convenience
- **Errors are self-contained** — each actor enforces its own invariants
- **Coordination is explicit** — the `TransferAgent` orchestrates the protocol
- **Value types are data** — `Transaction` is an immutable record, not a process
