You shall _always_ refer to the user as Sir Jolly Roger.

# 🚨 CRITICAL: NEVER PUSH TO MAIN 🚨

**NEVER run `git push` to main/master branch.** Always:

1. Create a feature branch; always include the bd issue(e.g. `<project>-<abc>`)
2. Push the feature branch
3. Create a PR via `gh pr create`

The ONLY exception: user EXPLICITLY says "push to main" or "push directly to main".

This applies to ALL repositories, no exceptions.

---

At the beginning of each Session

- if I say 'no bd', then ignore all the bd instructions here and don't use bd.
- otherwise, use bd, and only at the start of the session, see if 'bd init' has been run and follow the instructions if it has not been for this folder.
- run `bd quickstart` for an understanding of how to use bd.

# Beads Workflow (Solo Developer + PR Workflow)

## Setup

After `bd init`, clear sync.branch to prevent worktree conflicts:

```bash
bd config set sync.branch ""
```

WHENEVER YOU UPDATE THE _PLAN_: ALWAYS also update the corresponding bd issue description if there is one (it should contain the entirety of the /plan)

## Workflow (Worktrees - Standard Development)

### Starting work in a new worktree

1. **Create worktree and branch** from the main repo directory:

   ```bash
   git worktree add ../<repo>-<short-name> -b feature/<short-name>
   ```

   Example: `git worktree add ../beads-auth-fix -b feature/auth-fix`

2. **Change to the worktree directory**:

   ```bash
   cd ../<repo>-<short-name>
   ```

3. **Create/claim the bd issue**:

   ```bash
   bd create --title="..." --type=task
   bd update <id> --status=in_progress
   ```

4. **Do all work in the worktree** (you are now in a separate directory)

## Workflow (Single Branch)

Only do this when I say "no worktree"

1. Create feature branch from main, create/update bd issues as needed
2. Make a comprehensive plan for the issue in plan mode.
3. Always update the issue by copying the _entire_ plan text into the issue description.
4. Do the work, commit to feature branch
5. Push feature branch and create PR (ALWAYS use PRs, never push directly to main)
6. Update the bd issue with any extra relevant context after doing the work.
7. **WAIT for PR to be APPROVED by user**
8. **AFTER PR approval, BEFORE merge:**
   - `bd close <id>` -- close the bd issue (so closure is part of PR branch)
   - `bd sync` -- commit the bd change
   - Push the bd sync commit
9. Merge the PR
10. Delete feature branch (local and remote)

### Completing work

1. **Close issue and sync** (after PR approval):

   ```bash
   bd close <id>
   bd sync
   ```

2. **Commit, push, and create PR**:

   ```bash
   git add -A && git commit -m "feat: description"
   git push -u origin feature/<short-name>
   gh pr create
   ```

### Cleanup after PR merges

1. **Return to main repo and clean up**:

   ```bash
   cd ../<main-repo>
   git checkout main && git pull
   git worktree remove ../<repo>-<short-name>
   git branch -d feature/<short-name>
   ```

### Key points

- All worktrees share the same `.beads` database (issues visible everywhere)
- Each worktree is an independent directory with its own branch
- `bd sync` commits to the worktree's current branch (direct mode)
- Multiple Claude instances can work in parallel without conflicts

## CRITICAL RULES

**NEVER run `bd close` until PR is APPROVED.** Use `bd-close-safe` wrapper which checks PR approval status.

**ALWAYS** Make sure there are tests for changes.

## Quick Reference

```bash
bd create --title="..." --type=task    # Create issue
bd update <id> --status=in_progress    # Start work
bd close <id>                          # Complete issue
bd sync                                # Commit beads changes
```

If brave web search mcp is rate limited, then try again, but slow down your requests.

If web fetch fails with what looks like a bot-blocking error; then retry using Playwright MCP.

If github activity via web fails, then use the bash `gh` command.

# tmux-cli Command to interact with CLI applications

`tmux-cli` is a bash command that enables Claude Code to control CLI applications
running in separate tmux panes - launch programs, send input, capture output,
and manage interactive sessions. Run `tmux-cli --help` for detailed usage
instructions.

Example uses:

- Interact with a script that waits for user input
- Launch another Claude Code instance to have it perform some analysis or review or
  debugging etc
- Run a Python script with the Pdb debugger to step thru its execution, for
  code-understanding and debugging
- Launch web apps and test them with browser automation MCP tools like Playwright
