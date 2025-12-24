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

# Check if this is a git commit command or bd sync (which commits internally)
if [[ "$command" =~ git[[:space:]]+commit ]] || [[ "$command" =~ ^bd[[:space:]]+sync ]]; then
  # Get current branch
  current_branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)

  if [ "$current_branch" = "main" ] || [ "$current_branch" = "master" ]; then
    echo "BLOCKED: Cannot commit directly to '$current_branch' branch. Create a feature branch first." >&2
    exit 2
  fi
fi

# Allow other commands
exit 0
