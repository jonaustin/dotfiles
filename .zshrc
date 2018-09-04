# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#zmodload zsh/zprof

### ZSH {{{
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
export SYSTEM_TYPE=`uname`

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="bira"
#export ZSH_THEME="wedisagree"
#export ZSH_THEME="jon"
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export ZSH_THEME='refined'
fi

# safe-paste fixes up/down history breakage
# https://github.com/robbyrussell/oh-my-zsh/issues/1720
# `gbr create/publish/delete/track/rename branch_name origin_server`
# jsontools: pp_json, is_json, urlencode_json, urldecode_json
# node-docs [section]
# systemd: add `sc-[command]` aliases to all systemctl cmds
# urlencode / urldecode
# git author accounts: https://github.com/walle/gas
if [ $SYSTEM_TYPE = "Darwin" ]; then
  plugins=(zsh-completions) # git_remote_branch httpie jira jsontools ng npm)# pip python aws redis-cli rand-quote taskwarrior urltools web-search gas)
else
  plugins=(zsh-completions safe-paste zsh-syntax-highlighting bd) # zsh-autosuggestions) # gitfast rails gem rake node  docker encode64 git_remote_branch httpie jira jsontools ng npm pip python aws redis-cli rand-quote systemd taskwarrior urltools web-search gas)
fi

source $ZSH/oh-my-zsh.sh
if [ $SYSTEM_TYPE != "Darwin" ]; then
  autoload -U promptinit; promptinit
  prompt pure
fi

source ~/.zsh/rake.zsh

# zsh
unsetopt correctall
setopt interactivecomments
# have command completion ignore case - e.g. `git co foo` will complete to `git co FOO`
# Fair warning; may have side effects: https://unix.stackexchange.com/questions/197700/zsh-case-insensitive-mid-word-completion
# FIXME: if a completion fails it takes the 1st character of the path you tried to complete and doubles it.
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
#  '+l:|?=** r:|?=**'

## source files
source ~/.zsh/functions.sh
source ~/.zsh/aliases.sh
# }}}

# Shell init {{{
unsetopt beep
# zsh
set -o vi
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

bindkey '^?' backward-delete-char # backspace on chars before start of insert mode (after leaving cmd mode) - https://www.zsh.org/mla/users/2009/msg00812.html
bindkey '^h' backward-delete-char # ctrl-h also deletes chars
bindkey '^r' history-incremental-search-backward # ctrl-r starts searching history backward

export KEYTIMEOUT=1 # reduce lag between hitting esc and entering normal mode - https://dougblack.io/words/zsh-vi-mode.html, https://superuser.com/a/648046

ulimit -S -c 0 # Don't want any coredumps
stty -ixon # disable ^S/^Q flow control
# }}}

# Exports {{{
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr -R --quit-if-one-screen' # --LINE-NUMBERS ' # -R for coloring with source-highlight (external app)
export LESSOPEN="| src-hilite-lesspipe.sh %s"
#export HISTCONTROL=ignoredups # don't put duplicate lines in the history. See bash(1) for more options
export TERM=xterm-256color # https://github.com/mhinz/vim-galore#true-colors

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

export PATH=/usr/local/bin:/usr/local/sbin:~/bin:~/opt/bin:$PATH

if [ $SYSTEM_TYPE = "Darwin" ]; then
  export PATH="$PATH:/sbin:/usr/sbin"
  . ${HOME}/.zsh/zshrc.local.osx
elif [ `uname -o` = "GNU/Linux" ]; then
  . ${HOME}/.zsh/zshrc.local.linux
fi

source ${HOME}/.zsh/initializers.sh
source ${HOME}/.zsh/initializers_private.sh
source ${HOME}/.zsh/zshrc.local.work

# Fix git sloooow autocompletion
# https://superuser.com/questions/458906/zsh-tab-completion-of-git-commands-is-very-slow-how-can-i-turn-it-off
#setopt no_complete_aliases
#__git_files () {
#    _wanted files expl 'local files' _files
#}
# egh, just disable intelligent completion: http://www.zsh.org/mla/workers/2011/msg00502.html
#__git_files(){}
#__git_complete_index_file(){}
# or disable git completion entirely
#compdef -d git

#unalias run-help
autoload run-help
autoload -U compinit && compinit -d # -d cache completion info
autoload bashcompinit && bashcompinit # support bash completions
if [ $SYSTEM_TYPE = "GNU/Linux" ]; then
  source $HOME/bin/i3_completion.sh
fi
HELPDIR=/usr/local/share/zsh/help

PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

# nodejs
export PATH=$PATH:./node_modules/.bin

# go
export GOPATH=$HOME/code/_sandbox/_go
export PATH=$HOME/code/_sandbox/_go/bin:$PATH
typeset -U PATH # remove duplicate paths

# oh-my-zsh aws doesn't work for some reason (can't find autoload?! even though SHELL==zsh), so source directly
source ~/.pyenv/versions/2.7.13/bin/aws_zsh_completer.sh
