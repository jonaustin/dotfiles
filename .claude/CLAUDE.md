  You shall _always_ refer to the user as Sir Jolly Roger.

# 🚨 CRITICAL: NEVER PUSH TO MAIN 🚨

  **NEVER run `git push` to main/master branch.** Always:

  1. Create a feature branch
  2. Push the feature branch
  3. Create a PR via `gh pr create`

  The ONLY exception: user EXPLICITLY says "push to main" or "push directly to main".

  This applies to ALL repositories, no exceptions.

  ---

# Development Workflow

## Worktree Workflow (Default)

  Use a git worktree for feature work by default.

  Single Branch Workflow

  Only when I say "no worktree". Create feature branch from main, follow PR workflow.

  PR Closure Sequence (AFTER PR approval)

  1. Merge PR
  2. Cleanup: git worktree remove, delete branch

  CRITICAL RULES

  - NEVER merge the PR until it is APPROVED
  - ALWAYS ensure tests exist for changes

  ---
  Fallback Instructions

  - If Exa web search is rate limited: retry, but slow down requests
  - If web fetch fails (bot-blocking): retry using browser-use skill
  - Always use gh cli for github

# 🚨 NEVER FABRICATE 🚨

Do not invent specifics about external products, app UIs, web UIs, APIs, features, settings paths, button labels, or capabilities. Training data goes stale; UIs change. Confident bullshit wastes time and destroys trust.

**The rule:** If I haven't verified it in *this session*, I either:
1. Actually verify it (WebFetch, web search, docs, ask the user to check), OR
2. Say "I don't know" plainly and offer to research.

No hedging weasel-words, no embellishment, no pattern-matching from training data dressed up as fact. This applies *especially* to UI specifics (tab names, filter options, menu paths) which I am prone to inventing.

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

## PR Review Workflow

- For pull-request reviews, always use the existing local worktree for the PR branch.
- Start with `git worktree list`, then `cd` into the matching worktree before running `gh pr` commands.
- Do not run `gh pr checkout` for reviews unless Sir Jolly Roger explicitly asks for it.

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

# context7 MCP (Library Documentation)

Use context7 MCP to fetch current documentation for ANY library, framework, SDK, or API.
This includes well-known tools (React, Django, pygame, numpy, etc.) — training data may be stale.

## When to use
- API syntax, configuration, version migration
- Library-specific debugging or setup
- CLI tool usage documentation
- Any time you'd otherwise guess at library behavior

## When NOT to use
- General programming concepts, refactoring, code review
- Writing scripts from scratch, debugging business logic

## Usage
1. `resolve-library-id` — find the library's context7 ID
2. `query-docs` — fetch relevant documentation sections
