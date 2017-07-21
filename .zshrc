### ZSH {{{
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="bira"
#export ZSH_THEME="wedisagree"
#export ZSH_THEME="jon"
export ZSH_THEME='pure' # use upstream pure - https://github.com/sindresorhus/pure

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(git rails gem ruby rake node npm)
# safe-paste fixes up/down history breakage
# https://github.com/robbyrussell/oh-my-zsh/issues/1720
plugins=(gitfast rails gem ruby rake node safe-paste autojump) # npm

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

# Exports {{{
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr -R' # --LINE-NUMBERS --quit-if-one-screen' # -R for less coloring with source-highlight (external app)
export LESSOPEN="| src-hilite-lesspipe.sh %s"
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

export PATH=/usr/local/bin:/usr/local/sbin:~/bin:~/opt/bin:$PATH

if [ `uname` = "Darwin" ]; then
  export PATH="/usr/local/share/npm/bin:$PATH"
  export PATH="$HOME/pear/bin:$PATH"
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
__git_files(){}
__git_complete_index_file(){}
# or disable git completion entirely
#compdef -d git

#unalias run-help
autoload run-help
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
