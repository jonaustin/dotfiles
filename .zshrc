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
export PATH="$PATH:/usr/lib/perl5/vendor_perl/bin/:/sbin:/usr/sbin:/home/jon/bin:/usr/local/bin:/usr/lib/surfraw:/opt/android-sdk/tools:/usr/local/bin:/home/jon/bin/ruby:/home/jon/bin/bash:/home/jon/bin/mpd:/home/jon/bin/subtle:/home/jon/bin/bin:/home/jon/node_modules/.bin"
export NODE_PATH=/home/jon/bin/lib/node_modules

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

export EDITOR='vim'
export SHELL='zsh'
export BROWSER='firefox'
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr -R' # --LINE-NUMBERS --quit-if-one-screen' # -R for less coloring with source-highlight (external app)
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export DISPLAY=:0
export HISTCONTROL=ignoredups # don't put duplicate lines in the history. See bash(1) for more options
export TERM=screen-256color # unfortunately required so tmux doesn't color bleed - http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=618809
export MOZ_DISABLE_PANGO=1 # improve rendering (may also fix font issues for all mozilla apps -- https://wiki.archlinux.org/index.php/Firefox_Tips_and_Tweaks#Network_settings )
#export WINEDEBUG=-all

export SAVEHIST=100000
export HISTSIZE=10000
export OOO_FORCE_DESKTOP=gnome
export INPUTRC=~/.inputrc
#export HTTP_PROXY=http://127.0.0.1:8118
#export http_proxy=http://127.0.0.1:8118
export OPCODEDIR64=/lib/csound/plugins64  # for csound5
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
