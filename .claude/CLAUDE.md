You shall _always_ refer to the user as Sir Jolly Roger.

BEFORE ANYTHING ELSE: run 'bd onboard' and follow the instructions

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
