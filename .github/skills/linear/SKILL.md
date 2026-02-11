---
name: linear
description: Manage Linear issues. Use when asked about tasks, tickets, bugs, or Linear.
---

# Linear CLI

Command-line interface for Linear issue management.

## Search Issues

```bash
# Your active issues (default)
streamlinear-cli search

# Text search
streamlinear-cli search "auth bug"

# Filter by state
streamlinear-cli search --state "In Progress"

# Filter by assignee
streamlinear-cli search --assignee me
streamlinear-cli search --assignee user@example.com

# Filter by team
streamlinear-cli search --team BT

# Combine filters
streamlinear-cli search --state "In Progress" --assignee me --team BT
```

## Get Issue Details

```bash
# By short ID
streamlinear-cli get BT-123

# By URL
streamlinear-cli get "https://linear.app/team/issue/BT-123"
```

## Update Issue

```bash
# Change state
streamlinear-cli update BT-123 --state Done
streamlinear-cli update BT-123 --state "In Progress"

# Change priority (1=Urgent, 2=High, 3=Medium, 4=Low)
streamlinear-cli update BT-123 --priority 1

# Assign to me
streamlinear-cli update BT-123 --assignee me

# Assign to someone else
streamlinear-cli update BT-123 --assignee user@example.com

# Unassign
streamlinear-cli update BT-123 --assignee null

# Multiple updates
streamlinear-cli update BT-123 --state Done --priority 3
```

## Comment on Issue

```bash
streamlinear-cli comment BT-123 "Fixed in commit abc123"
streamlinear-cli comment BT-123 "Blocked on dependency update"
```

## Create Issue

```bash
# Basic
streamlinear-cli create --team BT --title "Bug: Login fails"

# With description
streamlinear-cli create --team BT --title "Bug: Login fails" --body "Users see error on submit"

# With priority
streamlinear-cli create --team BT --title "Urgent fix" --priority 1
```

## List Teams

```bash
streamlinear-cli teams
```

## Raw GraphQL

```bash
streamlinear-cli graphql "query { viewer { name email } }"
streamlinear-cli graphql "query { projects { nodes { id name } } }"
```

## Priority Reference

| Value | Label |
|-------|-------|
| 0 | No priority |
| 1 | Urgent |
| 2 | High |
| 3 | Medium |
| 4 | Low |

## Authentication

Uses `LINEAR_API_TOKEN` environment variable (already configured).
