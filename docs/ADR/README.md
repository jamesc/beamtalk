# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records (ADRs) for Beamtalk.

ADRs document significant architectural and design decisions, including:
- Language design choices
- Implementation strategies
- Trade-offs and their rationale

## Format

Each ADR follows this structure:
- **Title:** Descriptive title with sequential number (ADR 0001, ADR 0002, etc.)
- **Status:** Proposed, Accepted, Deprecated, Superseded
- **Context:** Background and problem statement
- **Decision:** The decision made
- **Consequences:** Positive, negative, and neutral impacts
- **References:** Related issues, documentation, discussions

## ADR List

| Number | Title | Status | Date |
|--------|-------|--------|------|
| [0001](0001-no-compound-assignment.md) | No Compound Assignment in Beamtalk | Proposed | 2026-02-03 |
| [0002](0002-use-erlang-comparison-operators.md) | Use Erlang Comparison Operators Directly | Proposed | 2026-02-03 |

## Creating New ADRs

1. **Number sequentially:** Use the next available number (0002, 0003, etc.)
2. **Use descriptive titles:** Make it clear what decision is being made
3. **Document thoroughly:** Include context, rationale, and consequences
4. **Link to issues:** Reference Linear issues (e.g., BT-XXX) where relevant
5. **Update this index:** Add your ADR to the table above

## References

- [ADR template and best practices](https://github.com/joelparkerhenderson/architecture-decision-record)
- [Why write ADRs](https://cognitect.com/blog/2011/11/15/documenting-architecture-decisions)
