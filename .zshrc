### ZSH {{{
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="bira"
#export ZSH_THEME="wedisagree"
export ZSH_THEME="jon"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rails gem ruby rake node npm)

source $ZSH/oh-my-zsh.sh
source ~/.zsh/rake.zsh

unsetopt correctall

## source files
source ~/.zsh/functions.sh
source ~/.zsh/aliases.sh
# }}}

# Shell init {{{
unsetopt beep
set -o vi
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search
ulimit -S -c 0 # Don't want any coredumps
stty -ixon # disable ^S/^Q flow control 
# }}}

# Exports {{{ 
export EDITOR='/usr/bin/vim'
export SHELL='/bin/zsh'
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr -R' # --LINE-NUMBERS --quit-if-one-screen' # -R for less coloring with source-highlight (external app)
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
#export HISTCONTROL=ignoredups # don't put duplicate lines in the history. See bash(1) for more options
export TERM=screen-256color # unfortunately required so tmux doesn't color bleed - http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=618809

export SAVEHIST=100000
export HISTSIZE=10000
export INPUTRC=~/.inputrc
#export HTTP_PROXY=http://127.0.0.1:8118
#export http_proxy=http://127.0.0.1:8118
# }}} 

# Colourful manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'


source ~/.zsh/initializers.sh
source ~/.zsh/zshrc_local
