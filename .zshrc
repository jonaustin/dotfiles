### ZSH {{{
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git rails gem ruby rake node npm)

source $ZSH/oh-my-zsh.sh
source ~/.zsh/rake.zsh

unsetopt correctall
setopt interactivecomments

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

source ~/.zsh/initializers.sh
source ~/.zsh/initializers_private.sh
. ${HOME}/.zsh/zshrc.local.work

# Fix git sloooow autocompletion
# https://superuser.com/questions/458906/zsh-tab-completion-of-git-commands-is-very-slow-how-can-i-turn-it-off
#setopt no_complete_aliases
#__git_files () {
#    _wanted files expl 'local files' _files
#}
# egh, just disable intelligent completion: http://www.zsh.org/mla/workers/2011/msg00502.html
__git_files(){}
__git_complete_index_file(){}
# or disable git completion entirely
#compdef -d git

#unalias run-help
autoload run-help
#HELPDIR=/usr/local/share/zsh/help
