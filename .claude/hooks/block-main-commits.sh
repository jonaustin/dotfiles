#!/bin/bash
# block-main-commits.sh - Prevent git commits on main/master branch

# Read the hook input from stdin
input=$(cat)

# Extract tool name and command
tool_name=$(echo "$input" | jq -r '.tool_name // empty')
command=$(echo "$input" | jq -r '.tool_input.command // empty')

# Only check Bash commands
if [ "$tool_name" != "Bash" ]; then
  exit 0
fi

# Check if this is a git commit command
if [[ "$command" =~ git[[:space:]]+commit ]]; then
  # Extract target directory if command starts with "cd /path &&"
  # This handles worktrees where the cd target differs from cwd
  target_dir=""
  if [[ "$command" =~ ^cd[[:space:]]+([^[:space:]&]+) ]]; then
    target_dir="${BASH_REMATCH[1]}"
  fi

  # Get current branch (in target directory if specified, otherwise cwd)
  if [ -n "$target_dir" ] && [ -d "$target_dir" ]; then
    current_branch=$(git -C "$target_dir" rev-parse --abbrev-ref HEAD 2>/dev/null)
  else
    current_branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  fi

  if [ "$current_branch" = "main" ] || [ "$current_branch" = "master" ]; then
    echo "BLOCKED: Cannot commit directly to '$current_branch' branch. Create a feature branch first." >&2
    exit 2
  fi
fi

# Allow other commands
exit 0
