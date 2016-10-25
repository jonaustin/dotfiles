#### PATH ####
export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH

if [ `uname` = "Darwin" ]; then
  export PATH="/usr/local/share/npm/bin:$PATH"
  export PATH="$HOME/pear/bin:$PATH"
  export PATH="$PATH:/sbin:/usr/sbin"
  . ${HOME}/.zsh/zshrc.local.osx
elif [ `uname -o` = "GNU/Linux" ]; then
  . ${HOME}/.zsh/zshrc.local.linux
fi


# nodejs
export PATH=$PATH:./node_modules/.bin

# go
export GOPATH=$HOME/code/_sandbox/_go
export PATH=$HOME/code/_sandbox/_go/bin:$PATH

# Haskell Cabal
export PATH=$PATH:$HOME/.cabal/bin

# rvm
#[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.
#PATH=$HOME/.rvm/bin:$PATH # Add RVM to PATH for scripting


#### EXPORTS ####
#### ZSH {{{
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="bira"
#export ZSH_THEME="wedisagree"
#export ZSH_THEME="jon"
export ZSH_THEME='pure'
# }}}

export EDITOR='/usr/local/bin/vim'
export SHELL='/usr/local/bin/zsh'
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

# Colourful manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

