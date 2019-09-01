# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#zmodload zsh/zprof
#for n in `seq 0 10`; do time zsh -i -c exit; done

export SYSTEM_TYPE=`uname`

if [ $SYSTEM_TYPE = "Darwin" ]; then
  export ZPLUG_HOME=/usr/local/opt/zplug
  source $ZPLUG_HOME/init.zsh
else
  export ZPLUG_HOME=$HOME/opt/zplug
  source $ZPLUG_HOME/init.zsh
fi;

zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/fasd", from:oh-my-zsh
#zplug "plugins/golang", from:oh-my-zsh

zplug "mkokho/kubemrr" # kubectl completions (sourced below)

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

source ~/.zsh/rake.zsh

# zsh

## source files
source ~/.zsh/initializers.sh
source ~/.zsh/functions.sh
source ~/.zsh/aliases.sh
# }}}

# Shell init {{{
# setopts
setopt auto_cd              # type bare dir name and cd to it e.g. `$ /`
setopt complete_in_word     # don't move cursor to end of line on completion
setopt interactive_comments # allow comments even in interactive shells.
unsetopt beep               # don't bloody beep
unsetopt bg_nice            # don't re-nice bg procs to lower priority
unsetopt correct            # don't autocorrect spelling for args
unsetopt correct_all        # don't autocorrect spelling for args
unsetopt flow_control       # disable ^S/^Q flow control
unsetopt hup                # don't send the HUP signal to running jobs when the shell exits.
unsetopt list_beep          # don't beep on ambiguous completions
unsetopt local_options      # allow funcs to have their own setopts (i.e. don't change globally)
unsetopt local_traps        # allow funcs to have their own signal trap opts (i.e. don't change globally)


# history
setopt append_history       # appends history file instead of replacing it
setopt extended_history     # add timestamps to history
setopt hist_ignore_all_dups # don't record dupes in history
setopt hist_ignore_space    # remove command line from history list when first character on the line is a space
setopt hist_reduce_blanks   # remove superflous blanks
setopt hist_verify          # don't execute, just expand history
setopt inc_append_history share_history  # adds history incrementally and share it across sessions

# zstyles
# case insensitive completion
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# Make new commands immediately visible to zsh
zstyle ':completion:*' rehash true
# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR

# bindkeys
bindkey -v # vim mode

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


# FIXME: why did i add this?
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

export KEYTIMEOUT=1 # reduce lag between hitting esc and entering normal mode - https://dougblack.io/words/zsh-vi-mode.html, https://superuser.com/a/648046

ulimit -S -c 0 # Don't want any coredumps
# }}}

# Exports {{{
export PAGER='less'
# --RAW-CONTROL-CHARS for coloring with external app
# --squeeze-blank-lines  -- no more than one blank line in a row
# --quit-on-intr -- quit on interrupt, e.g. C-c
# --quit-if-one-screen -- quit if content fills less than the screen
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --quit-on-intr --quit-if-one-screen --clear-screen'
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
  . /usr/share/fzf/completion.zsh
  . /usr/share/fzf/key-bindings.zsh
fi

# fzf
# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f'
# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

#source ${HOME}/.zsh/initializers.sh
source ${HOME}/.zsh/initializers_private.sh
source ${HOME}/.zsh/zshrc.local.work
source ${HOME}/.zsh/zshrc.local.private

# FIXME: why did i put this here?
autoload -Uz run-help
unalias run-help
alias help=run-help

if [ $SYSTEM_TYPE = "GNU/Linux" ]; then
  source $HOME/bin/i3_completion.sh

  # https://gist.github.com/ctechols/ca1035271ad134841284
  setopt EXTENDEDGLOB
  for dump in $HOME/.zcompdump(#qN.m1); do
    compinit
    if [[ -s "$dump" && (! -s "$dump.zwc" || "$dump" -nt "$dump.zwc") ]]; then
      zcompile "$dump"
    fi
  done
  unsetopt EXTENDEDGLOB
  compinit -C
else
  # FIXME: Test if above works on os x
  autoload -Uz compinit && compinit -du # -U suppress alias expansion, -z use zsh native (instead of ksh i guess); -d cache completion info
  autoload -U bashcompinit && bashcompinit # support bash completions
fi

#if [ -z $USE_HOME ] && ([ `cat /tmp/ip` = `cat $HOME/work/ipw` ] || [ `cat /tmp/ip` = `cat $HOME/work/ipe` ]); then
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export HISTFILE=$HOME/.zsh_historyw
else
  export HISTFILE=$HOME/.zsh_history
fi
export HISTSIZE=100000
export SAVEHIST=100000

# nodejs
export PATH=$PATH:./node_modules/.bin

# go
export GOPATH=$HOME/code/_sandbox/_go
export PATH=$HOME/code/_sandbox/_go/bin:$PATH

typeset -U PATH # remove duplicate paths

# load avn
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

### ZSH Completions ###
source $ZPLUG_REPOS/mkokho/kubemrr/kubectl_zsh_completions

# oh-my-zsh aws doesn't work for some reason (can't find autoload?! even though SHELL==zsh), so source directly
source ~/.pyenv/versions/2.7.13/bin/aws_zsh_completer.sh
