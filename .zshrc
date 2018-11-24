# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#for n in `seq 0 10`; do time zsh -i -c exit; done
#zmodload zsh/zprof
export ZPLUG_HOME=$HOME/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/fasd", from:oh-my-zsh
zplug "plugins/golang", from:oh-my-zsh

#https://github.com/unixorn/awesome-zsh-plugins#plugins
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
#zplug "zsh-users/zaw" # Ctrl-x
zplug "mafredri/zsh-async"
zplug "fcambus/ansiweather"
zplug "wting/autojump"
#zplug Tarrasch/zsh-bd
zplug "zdharma/zsh-diff-so-fancy" # git dsf
#zplug h3poteto/zsh-ec2ssh

#zplug MichaelAquilina/zsh-you-should-use # alias reminders; meh, no whitelists
#zplug djui/alias-tips # alias reminders; ugh adds 300ms to load time
zplug "peterhurford/git-it-on.zsh" # gitit -- open your current folder, on your current branch, in GitHub or GitLab
#zplug Tarrasch/zsh-bd # meh, autojump already does it
#zplug StackExchange/blackbox # gpg encrypt secrets in git repos
zplug "supercrabtree/k" # pretty directory listings

zplug "sindresorhus/pure", use:pure.zsh, as:theme
export RPROMPT='%F{blue}`date +"%F %T"`' # put date on right side

# plugin helpers
[[ -s /home/jon/.autojump/etc/profile.d/autojump.sh ]] && source /home/jon/.autojump/etc/profile.d/autojump.sh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load #--verbose

export SYSTEM_TYPE=`uname`

source ~/.zsh/rake.zsh

# zsh
unsetopt correctall
setopt interactivecomments
# have command completion ignore case - e.g. `git co foo` will complete to `git co FOO`
# Fair warning; may have side effects: https://unix.stackexchange.com/questions/197700/zsh-case-insensitive-mid-word-completion
# FIXME: if a completion fails it takes the 1st character of the path you tried to complete and doubles it.
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
#  '+l:|?=** r:|?=**'

# case insensitive completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

## source files
source ~/.zsh/initializers.sh
source ~/.zsh/functions.sh
source ~/.zsh/aliases.sh
# }}}

# Shell init {{{
unsetopt beep
bindkey -v # vim mode

key=(
    BackSpace  "${terminfo[kbs]}"
    Home       "${terminfo[khome]}"
    End        "${terminfo[kend]}"
    Insert     "${terminfo[kich1]}"
    Delete     "${terminfo[kdch1]}"
    Up         "${terminfo[kcuu1]}"
    Down       "${terminfo[kcud1]}"
    Left       "${terminfo[kcub1]}"
    Right      "${terminfo[kcuf1]}"
    PageUp     "${terminfo[kpp]}"
    PageDown   "${terminfo[knp]}"
)

# history search
autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search
bindkey "$terminfo[kcuu1]" up-line-or-beginning-search # Up
bindkey '^[[A' up-line-or-search # terminfo above doesn't work in termite
bindkey "^P" history-beginning-search-backward

autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "$terminfo[kcud1]" down-line-or-beginning-search # down
bindkey '^[[B' down-line-or-search # terminfo above doesn't work in termite
bindkey "^N" history-beginning-search-forward

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
export TERM=xterm-256color # https://github.com/mhinz/vim-galore#true-colors

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

#source ${HOME}/.zsh/initializers.sh
source ${HOME}/.zsh/initializers_private.sh
source ${HOME}/.zsh/zshrc.local.work
source ${HOME}/.zsh/zshrc.local.private

#unalias run-help
autoload run-help

# On slow systems, checking the cached .zcompdump file to see if it must be
# regenerated adds a noticable delay to zsh startup.  This little hack restricts
# it to once a day.  It should be pasted into your own completion file.
#
# The globbing is a little complicated here:
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+24' matches files (or directories or whatever) that are older than 24 hours.
#autoload -Uz compinit
#if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
#  compinit;
#else
#  compinit -C;
#fi;
### MAC OS X VERSION+autoload -Uz compinit
#if [ $(date +'%j') != $(stat -f '%Sm' -t '%j' ~/.zcompdump) ]; then
#  compinit
#else
#  compinit -C
#fi

autoload -U compinit && compinit -du # -d cache completion info
autoload bashcompinit && bashcompinit # support bash completions
if [ $SYSTEM_TYPE = "GNU/Linux" ]; then
  source $HOME/bin/i3_completion.sh
fi

HELPDIR=/usr/local/share/zsh/help

if [ -z $USE_HOME ] && ([ `cat /tmp/ip` = `cat $HOME/work/ipw` ] || [ `cat /tmp/ip` = `cat $HOME/work/ipe` ]); then
  HISTFILE=$HOME/.zsh_historyw
else
  HISTFILE=$HOME/.zsh_history
fi
HISTSIZE=100000
SAVEHIST=100000
echo $HISTFILE

# nodejs
export PATH=$PATH:./node_modules/.bin

# go
export GOPATH=$HOME/code/_sandbox/_go
export PATH=$HOME/code/_sandbox/_go/bin:$PATH

typeset -U PATH # remove duplicate paths

# oh-my-zsh aws doesn't work for some reason (can't find autoload?! even though SHELL==zsh), so source directly
source ~/.pyenv/versions/2.7.13/bin/aws_zsh_completer.sh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/jon/work/merchant-monitoring-api-deploy/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/jon/work/merchant-monitoring-api-deploy/node_modules/tabtab/.completions/serverless.zsh

# load avn
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"
