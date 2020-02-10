# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#zmodload zsh/zprof
#for n in `seq 0 10`; do time zsh -i -c exit; done

export SYSTEM_TYPE=`uname`
export DOTFILES=$HOME/.config

source ~/.zplugin/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

autoload -Uz compinit && compinit -du # -U suppress alias expansion, -z use zsh native (instead of ksh i guess); -d cache completion info
autoload -U bashcompinit && bashcompinit # support bash completions

if [ $SYSTEM_TYPE = "Linux" ]; then
  source $HOME/bin/i3/i3-completion/i3_completion.sh # must come after bashcompinit
  fpath=(/usr/local/share/zsh-completions $fpath)

  # init XDG dirs
  export XDG_CONFIG_HOME=$HOME/.config
  export XDG_CACHE_HOME=$HOME/.cache
  export XDG_DATA_HOME=$HOME/.local/share
  export XDG_CONFIG_DIRS=/etc/xdg
fi

#zplugin OMZ::plugins/command-not-found/command-not-found.zsh
zplugin snippet OMZ::plugins/fasd/fasd.plugin.zsh # v <fuzzy path> (vim); j <fuzzy path> (cd)
# built-ins - until they get muscle memory
#alias a='fasd -a'        # any
#alias s='fasd -si'       # show / search / select
#alias d='fasd -d'        # directory
#alias f='fasd -f'        # file
#alias sd='fasd -sid'     # interactive directory selection
#alias sf='fasd -sif'     # interactive file selection
#alias z='fasd_cd -d'     # cd, same functionality as j in autojump
#alias zz='fasd_cd -d -i' # cd with interactive selection

zplugin snippet OMZ::plugins/golang/golang.plugin.zsh # completions/aliases

#zplug "mkokho/kubemrr" # kubectl completions (sourced below)
#zplugin light "stevemcilwain/nonotes" # nmap zsh funcs

# node
#export NVM_LAZY_LOAD=true
#zplug "lukechilds/zsh-nvm" # even with lazy loading adds ~0.1 to zsh startup

# asdf: multi-lang version mgr
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export asdf_dir=$(brew --prefix asdf)
else
  export asdf_dir=/opt/asdf-vm/
fi;
if [[ -d $asdf_dir ]]; then
  source $asdf_dir/asdf.sh
  source $asdf_dir/completions/asdf.bash
fi

# colors
## pretty colors (using grc) for various commands; diff,mtr,netstat,ps,etc
zplugin light unixorn/warhol.plugin.zsh
### I like these for ls
#export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43" # https://geoff.greer.fm/lscolors/
zplugin light zdharma/fast-syntax-highlighting
# Base16 Shell
#zplugin light "chriskempson/base16-shell" # base16<tab> # color themes
# trapd00r
#zplugin ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
#    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
#    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
#zplugin light trapd00r/LS_COLORS

# pywal; automatic colors from current wallpaper
if [ ! $SYSTEM_TYPE = "Darwin" ]; then
  (cat ~/.cache/wal/sequences &) # async
fi;

# navi
#zplugin light denisidoro/navi/navi.plugin.zsh # ^g
#zplugin light "denisidoro/navi", as:command, use:"navi"

# https://github.com/unixorn/awesome-zsh-plugins#plugins
# zaw
zplugin light zsh-users/zaw
  zplugin light termoshtt/zaw-systemd
  zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
  zstyle ':filter-select' hist-find-no-dups yes # ignore duplicates in history source
zplugin light zsh-users/zsh-autosuggestions # fish-like autosuggestions for zsh
zplugin light zsh-users/zsh-completions # just various completions
# zplugin light mafredri/zsh-async # cool, but haven't needed it: https://github.com/mafredri/zsh-async#example-code
zplugin light fcambus/ansiweather # $ weather <zip>
zplugin light "zdharma/zsh-diff-so-fancy" # $ git dsf
#zplugin light h3poteto/zsh-ec2ssh

# and see bindkey below
#zplugin light "aperezdc/zsh-notes"
#zstyle :notes home  $HOME/notes/

#zplugin light MichaelAquilina/zsh-you-should-use # alias reminders; meh, no whitelists
#zplugin light djui/alias-tips # alias reminders; ugh adds 300ms to load time
zplugin light "peterhurford/git-it-on.zsh" # gitit -- open your current folder, on your current branch, in GitHub or GitLab
#zplugin light StackExchange/blackbox # gpg encrypt secrets in git repos
zplugin light "supercrabtree/k" # pretty directory listings
zplugin light "wfxr/forgit" # fzf for git -- ga; glo; gi; gd; grh; gcf; gss; gclean
zplugin light "hlohm/mfunc" # dynamically define and use shell functions
#zplugin light "amstrad/oh-my-matrix"

# Prompt
zplugin light "romkatv/powerlevel10k"
# To customize prompt, run `p10k configure` or edit .p10k.zsh.
[[ -f ~/.zsh/p10k.zsh ]] && source ~/.zsh/p10k.zsh

zplugin ice wait'1' lucid
zplugin light laggardkernel/zsh-thefuck


# zsh

## source files
source ~/.zsh/functions.sh
source ~/.zsh/aliases.sh
[[ -f ~/configs_private/zshrc.local.private ]] && source ~/configs_private/zshrc.local.private
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
bindkey -v # vi mode
## open current command in vim; esc to visual mode and hit 'v'
export VISUAL=$EDITOR
autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# history search (fzf overrides this, but may as well keep it for fallbac)
autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search

if [ ! $SYSTEM_TYPE = "Darwin" ]; then
  bindkey "$terminfo[kcuu1]" up-line-or-beginning-search # Up
  bindkey "$terminfo[kcud1]" down-line-or-beginning-search # down
fi;
bindkey '^[[A' up-line-or-search # terminfo above doesn't work in termite
bindkey "^P" history-beginning-search-backward

autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey '^[[B' down-line-or-search # terminfo above doesn't work in termite
bindkey "^N" history-beginning-search-forward

bindkey '^?' backward-delete-char # backspace on chars before start of insert mode (after leaving cmd mode) - https://www.zsh.org/mla/users/2009/msg00812.html
bindkey '^h' backward-delete-char # ctrl-h also deletes chars
bindkey '^r' history-incremental-search-backward # ctrl-r starts searching history backward (note: doesn't work in vi mode)

# zsh-notes
bindkey '^N' notes-edit-widget

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

ulimit -S -c 0 # Don't want any coredumps from segfaults
# }}}

# Exports {{{
export PAGER='less'
# --RAW-CONTROL-CHARS for coloring with external app
# --squeeze-blank-lines  -- no more than one blank line in a row
# --quit-on-intr -- quit on interrupt, e.g. C-c
# --quit-if-one-screen -- quit if content fills less than the screen
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --quit-on-intr --clear-screen'
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
  export PATH="$PATH:/sbin:/usr/sbin:$HOME/.local/bin"
  . ${HOME}/.zsh/zshrc.local.osx
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
elif [ $SYSTEM_TYPE = "Linux" ]; then
  export PATH=/home/jon/Android/Sdk:$PATH
  . ${HOME}/.zsh/zshrc.local.linux
  . /usr/share/fzf/completion.zsh
  . /usr/share/fzf/key-bindings.zsh

  # oh-my-zsh aws doesn't work for some reason in linux (can't find autoload?! even though SHELL==zsh), so source directly
  source ~/.pyenv/versions/2.7.13/bin/aws_zsh_completer.sh
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
#autoload -Uz run-help
#unalias run-help
#alias help=run-help

fpath+=~/.zfunc # for poetry (python)


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

typeset -U PATH # remove duplicate paths

# load avn
#[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

### ZSH Completions ###
#source $ZPLUG_REPOS/mkokho/kubemrr/kubectl_zsh_completions

# oh-my-zsh aws doesn't work for some reason (can't find autoload?! even though SHELL==zsh), so source directly
#source ~/.pyenv/versions/2.7.13/bin/aws_zsh_completer.sh

# go
export GOPATH=$HOME/code/_sandbox/_go
export PATH=$HOME/code/_sandbox/_go/bin:$PATH

# python
## pyenv
#export PATH="$PYENV_ROOT/shims:$PATH" # due to path ordering (touch PATH later in zshrc) had to add this at bottom of zshrc
#export PYTHON_CONFIGURE_OPTS="--enable-shared" # for youcompleteme
#eval "$(pyenv init -)"
#export PYENV_VERSION=3.6.1 #2.7.13 # use pyenv global
#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/shims:$PATH"
#if [ $SYSTEM_TYPE = "Darwin" ]; then
#  . /usr/local/share/zsh/site-functions/pyenv.zsh
#else
#  . $(pyenv root)/completions/pyenv.zsh
#fi

## poetry
#export PATH="$HOME/.poetry/bin:$PATH"

# load avn
#[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

eval "$(direnv hook zsh)"
. $HOME/opt/dntw/dntw.sh # dedicated neovim tmux window; start tmux with `dntw`

if [ $SYSTEM_TYPE = "Darwin" ]; then
  export BROWSER='open'
  export EDITOR='/usr/local/bin/nvim' # homebrew
  export SHELL='/usr/local/bin/zsh'
else
  export EDITOR='/usr/bin/nvim'
  export SYSTEMD_EDITOR=$EDITOR
  export BROWSER='/usr/bin/firefox'
  export SHELL='/usr/bin/zsh'
  export PATH=${PATH}:$HOME/.local/bin
  # gtk3 hidpi
  export GDK_SCALE=2
  export GDK_DPI_SCALE=0.5
  export XCURSOR_SIZE=48
  export TERMINAL=termite
  export DISABLE_AUTO_TITLE=true
fi

# multi-lang version mgr
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export asdf_dir=$(brew --prefix asdf)
else
  export asdf_dir=/opt/asdf-vm/
fi;
zplugin light kiurchv/asdf.plugin.zsh

source /home/jon/.config/broot/launcher/bash/br

# bash-my-aws
export PATH="$PATH:$HOME/.bash-my-aws/bin"
source ~/.bash-my-aws/aliases
source ~/.bash-my-aws/bash_completion.sh
