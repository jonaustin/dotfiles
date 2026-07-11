  You shall _always_ refer to the user as Sir Jolly Roger.

# 🚨 CRITICAL: NEVER PUSH TO MAIN 🚨

  **NEVER run `git push` to main/master branch.** Always:

  1. Create a feature branch; always include the bd issue (e.g. `<project>-<abc>`)
  2. Push the feature branch
  3. Create a PR via `gh pr create`

  The ONLY exception: user EXPLICITLY says "push to main" or "push directly to main".

  This applies to ALL repositories, no exceptions.

  ---

# Session Start

- If I say 'no bd', ignore all bd instructions and don't use bd.
- Otherwise:
  - Use 'bd' for task tracking

# Beads Workflow Customizations

## Plan Sync

  WHENEVER YOU UPDATE THE _PLAN_: ALWAYS also update the corresponding bd issue description (it should contain the entirety of the /plan)

## Worktree Workflow (Default)

  ```bash
  bd worktree create <short-name>
  cd <short-name>
  bd create --title="..." --type=task
  bd update <id> --status=in_progress
  # Do work in worktree

  Single Branch Workflow

  Only when I say "no worktree". Create feature branch from main, follow PR workflow.

  PR Closure Sequence (AFTER PR approval, BEFORE merge)

  1. bd close <id>
  2. Merge PR
  3. Cleanup: git worktree remove, delete branch

  CRITICAL RULES

  - NEVER run bd close until PR is APPROVED
  - ALWAYS ensure tests exist for changes

  ---
  Fallback Instructions

  - If Brave web search is rate limited: retry, but slow down requests
  - If web fetch fails (bot-blocking): retry using dev-browser plugin
  - Always use gh cli for github

## PR Review Workflow

- For pull-request reviews, always use the existing local worktree for the PR branch.
- Start with `git worktree list`, then `cd` into the matching worktree before running `gh pr` commands.
- Do not run `gh pr checkout` for reviews unless Sir Jolly Roger explicitly asks for it.

# Bug Fixes: Root Cause Before Code

When fixing bugs or test failures:

1. **Explain the root cause first.** State what is actually causing the problem and why, with evidence. If you can't explain it clearly, you don't understand it yet — research more before proposing anything.
2. **Explain why the fix addresses the root cause.** Connect the proposed change back to the cause.
3. **Verify the fix against the actual failure environment.** If CI fails, don't assume a local pass means it's fixed. Check that the fix addresses the specific CI conditions.
4. **NEVER propose speculative fixes.** No "try this and see if it works." No guessing. Understand first, fix second.

# tmux-cli

  tmux-cli enables Claude Code to control CLI applications in separate tmux panes. Run tmux-cli --help for usage.

  Uses: interactive scripts, spawn Claude Code instances, Pdb debugging, browser automation with web apps.

# Superpowers System

You have superpowers.
Superpowers teach you new skills and capabilities. RIGHT NOW run: `~/.codex/superpowers/.codex/superpowers-codex bootstrap` and follow the instructions it returns.

<!-- codebase-memory-mcp:start -->
# Codebase Knowledge Graph (codebase-memory-mcp)

This project uses codebase-memory-mcp to maintain a knowledge graph of the codebase.
ALWAYS prefer MCP graph tools over grep/glob/file-search for code discovery.

## Priority Order
1. `search_graph` — find functions, classes, routes, variables by pattern
2. `trace_path` — trace who calls a function or what it calls
3. `get_code_snippet` — read specific function/class source code
4. `query_graph` — run Cypher queries for complex patterns
5. `get_architecture` — high-level project summary

## When to fall back to grep/glob
- Searching for string literals, error messages, config values
- Searching non-code files (Dockerfiles, shell scripts, configs)
- When MCP tools return insufficient results

## Examples
- Find a handler: `search_graph(name_pattern=".*OrderHandler.*")`
- Who calls it: `trace_path(function_name="OrderHandler", direction="inbound")`
- Read source: `get_code_snippet(qualified_name="pkg/orders.OrderHandler")`
<!-- codebase-memory-mcp:end -->
