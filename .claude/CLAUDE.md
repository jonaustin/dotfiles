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
  - make sure a remote git repo already exists; if not create a new private repo with the same name as the code directory.
  - check if `bd init` has been run; if not then `bd init --branch beads-sync`
    - This creates a .beads/ directory and configures beads to commit to beads-sync instead of main.
  - Important: After initialization, you'll see some untracked files that should be committed to your protected branch; run:
    - `git status`
    - git add .beads/.gitignore .gitattributes
    - git commit -m "Initialize beads issue tracker"
    - git push origin main  # Or create a PR if required
- run `db doctor --fix` if needed
- Run `bd quickstart` if you need help with bd.

# Beads Workflow Customizations

## Plan Sync

  WHENEVER YOU UPDATE THE _PLAN_: ALWAYS also update the corresponding bd issue description (it should contain the entirety of the /plan)

## Worktree Workflow (Default)

  ```bash
  git worktree add ../<repo>-<short-name> -b feature/<short-name>
  cd ../<repo>-<short-name>
  bd create --title="..." --type=task
  bd update <id> --status=in_progress
  # Do work in worktree

  Single Branch Workflow

  Only when I say "no worktree". Create feature branch from main, follow PR workflow.

  PR Closure Sequence (AFTER PR approval, BEFORE merge)

  1. bd close <id>
  2. bd sync
  3. Push the bd sync commit
  4. Merge PR
  5. Cleanup: git worktree remove, delete branch

  CRITICAL RULES

  - NEVER run bd close until PR is APPROVED
  - ALWAYS ensure tests exist for changes

  ---
  Fallback Instructions

  - If Brave web search is rate limited: retry, but slow down requests
  - If web fetch fails (bot-blocking): retry using dev-browser plugin
  - Always use gh cli for github

# tmux-cli

  tmux-cli enables Claude Code to control CLI applications in separate tmux panes. Run tmux-cli --help for usage.

  Uses: interactive scripts, spawn Claude Code instances, Pdb debugging, browser automation with web apps.

# Superpowers System

You have superpowers.
Superpowers teach you new skills and capabilities. RIGHT NOW run: `~/.codex/superpowers/.codex/superpowers-codex bootstrap` and follow the instructions it returns.
