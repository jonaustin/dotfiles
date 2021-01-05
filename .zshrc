# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#zmodload zsh/zprof
#for n in `seq 0 10`; do time zsh -i -c exit; done
#hyperfine --warmup 3 --min-runs 10 "zsh -i -c exit"

export SYSTEM_TYPE=`uname`
export DOTFILES=$HOME/.config

source ~/.zinit/bin/zinit.zsh

if [ $SYSTEM_TYPE = "Linux" ]; then
  fpath=($HOME/.zsh/completion $fpath)

  # init XDG dirs
  export XDG_CONFIG_HOME=$HOME/.config
  export XDG_CACHE_HOME=$HOME/.cache
  export XDG_DATA_HOME=$HOME/.local/share
  export XDG_CONFIG_DIRS=/etc/xdg
fi

# completions
autoload -Uz compinit && compinit -du # -U suppress alias expansion, -z use zsh native (instead of ksh i guess); -d cache completion info
autoload -U bashcompinit && bashcompinit # support bash completions
if [ $SYSTEM_TYPE = "Linux" ]; then
  source $HOME/bin/i3/i3-completion/i3_completion.sh # must come after bashcompinit
fi

zplugin light kazhala/dotbare
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

#zplugin light "stevemcilwain/nonotes" # nmap zsh funcs

zplugin light "macunha1/zsh-terraform"
#zplugin light hanjunlee/terragrunt-oh-my-zsh-plugin

# node
#export NVM_LAZY_LOAD=true
#zplug "lukechilds/zsh-nvm" # even with lazy loading adds ~0.1 to zsh startup

# asdf: multi-lang version mgr
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export asdf_dir=/usr/local/opt/asdf
else
  export asdf_dir=/opt/asdf-vm/
fi;
if [[ -d $asdf_dir ]]; then
  source $asdf_dir/asdf.sh
  fpath=(${ASDF_DIR}/completions $fpath)
fi

# colors
## pretty colors (using grc) for various commands; diff,mtr,netstat,ps,etc
zplugin light unixorn/warhol.plugin.zsh
#export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43" # https://geoff.greer.fm/lscolors/
zplugin light zdharma/fast-syntax-highlighting # better than zsh-users/zsh-syntax-highlighting
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
zplugin light "wfxr/forgit" # fzf for git -- ga; glo; gi; gd; grh; gcf; gss; gclean
zplugin light "hlohm/mfunc" # dynamically define and use shell functions
zplugin light "b4b4r07/emoji-cli"
#zplugin light "amstrad/oh-my-matrix"

# Prompt
zplugin light "romkatv/powerlevel10k"
# To customize prompt, run `p10k configure` or edit .p10k.zsh.
# Show prompt segment "kubecontext" only when the command you are typing
# invokes kubectl, helm, kubens, kubectx, oc, istioctl, kogito, k9s or helmfile.
typeset -g POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND='kubectl|helm|kubens|kubectx|oc|istioctl|kogito|k9s|helmfile'
[[ -f ~/.zsh/p10k.zsh ]] && source ~/.zsh/p10k.zsh

zplugin ice wait'1' lucid
zplugin light laggardkernel/zsh-thefuck

zplugin ice wait'1' lucid
zplugin light "buonomo/yarn-completion"

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
unsetopt list_beep          # don't beep on ambiguous completions
unsetopt bg_nice            # don't re-nice bg procs to lower priority
unsetopt correct            # don't autocorrect spelling for args
unsetopt correct_all        # don't autocorrect spelling for args
unsetopt flow_control       # disable ^S/^Q flow control
unsetopt hup                # don't send the HUP signal to running jobs when the shell exits.
unsetopt local_options      # allow funcs to have their own setopts (i.e. don't change globally)
unsetopt local_traps        # allow funcs to have their own signal trap opts (i.e. don't change globally)


# history
#setopt append_history        # appends history file instead of replacing it (not needed if share_history is enabled)
setopt extended_history       # add timestamps to history
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_all_dups   # don't record dupes in history
setopt hist_ignore_space      # remove command line from history list when first character on the line is a space
setopt hist_reduce_blanks     # remove superflous blanks
setopt hist_verify            # when interpolating history into commands (e.g. `$ echo !!`; require another press of enter to actually execute after expanding the last command with `!!`)
setopt share_history          # adds history incrementally and share it across sessions

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

# history search (fzf overrides this, but may as well keep it for fallback)
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

# https://wiki.archlinux.org/index.php/Zsh#Key_bindings
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

# built in mass rename
# -n -- dry run
autoload -U zmv # zmv '* *' '$f:gs/ /_' Replace all spaces in filenames with underscores.

export KEYTIMEOUT=1 # reduce lag between hitting esc and entering normal mode - https://dougblack.io/words/zsh-vi-mode.html, https://superuser.com/a/648046

ulimit -S -c 0 # Don't want any coredumps from segfaults
# }}}

# Exports {{{
export PAGER='less'
# --RAW-CONTROL-CHARS:   translate raw escape sequences to colors
# --squeeze-blank-lines: no more than one blank line in a row
# --quit-on-intr:        quit on interrupt, e.g. C-c
# --quit-if-one-screen:  quit if content fills less than the screen
# --no-init:             don't clear screen on exit
# --mouse:               support mouse - only >=551 (brew install less on mac)
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --quit-on-intr --quit-if-one-screen --no-init --mouse'
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export TERM=xterm-256color # https://github.com/mhinz/vim-galore#true-colors
# }}}

export PATH=/usr/local/bin:/usr/local/sbin:~/bin:~/opt/bin:$PATH

# OS configs and fzf
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export PATH="$PATH:/sbin:/usr/sbin:$HOME/.local/bin"
  . ${HOME}/.zsh/zshrc.local.osx
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
  bindkey "ç" fzf-cd-widget # fix alt-c for `cd` fzf for osx
elif [ $SYSTEM_TYPE = "Linux" ]; then
  . ${HOME}/.zsh/zshrc.local.linux
  #. /usr/share/fzf/completion.zsh
  . /usr/share/zsh/site-functions/_fzf
  #. /usr/share/fzf/key-bindings.zsh
  . /etc/profile.d/fzf.zsh
fi
# note for some unknown reason --follow (symlinks) causes it not to find anything under ~/.zsh (which is not a symlink dir regardless). fd bug?
FD_OPTIONS="--no-ignore --hidden --exclude .git --exclude node_modules --exclude .cache --exclude .asdf"
# fixme: alias pbcopy for linux
# notes:
#   -1: automatically select the only match
#   open in vim: https://github.com/junegunn/fzf/issues/1593
export FZF_DEFAULT_OPTS="--no-mouse --height=50% -1 --reverse --multi --info=inline --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2> /dev/null | head -300' --preview-window='right:hidden:wrap' --bind='f3:execute(bat --style=numbers {} || less -f {}),f2:toggle-preview,ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy),ctrl-e:execute(nvim -p {+} < /dev/tty > /dev/tty 2>&1),ctrl-x:execute(rm -i {+})+abort'"
export FZF_DEFAULT_COMMAND="fd --type f --type l $FD_OPTIONS"
export FZF_CTRL_T_COMMAND="fd --type f --type l $FD_OPTIONS"
export FZF_ALT_C_COMMAND="fd --type d $FD_OPTIONS"

# Private configs
#source ${HOME}/.zsh/initializers.sh
source ${HOME}/configs_private/initializers_private.sh
source ${HOME}/configs_private/zshrc.local.work
source ${HOME}/configs_private/zshrc.local.private

# type in any command, then hit alt-? to bring up man page (eg $ lso ALT-shift-/)
unalias run-help
autoload -Uz run-help
bindkey '\e?' run-help

#if [ -z $USE_HOME ] && ([ `cat /tmp/ip` = `cat $HOME/work/ipw` ] || [ `cat /tmp/ip` = `cat $HOME/work/ipe` ]); then
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export HISTFILE=$HOME/.zsh_historyw
else
  export HISTFILE=$HOME/configs_private/zsh_history
fi
export HISTSIZE=100000
export SAVEHIST=100000

# nodejs
export PATH=$PATH:./node_modules/.bin

typeset -U PATH # remove duplicate paths

# load avn
#[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

# go
export GOPATH=$HOME/opt/_go
export GOBIN=$GOPATH/bin
export PATH=$HOME/opt/_go/bin:$PATH

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

  # mac paths
  export PATH="/usr/local/opt/qt@5.5/bin:$PATH"
  export PATH="/usr/local/opt/mysql-client/bin:$PATH"
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
  export TERMINAL=alacritty
  export DISABLE_AUTO_TITLE=true
fi
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# aws
complete -C $(which aws_completer) aws2
# bash-my-aws
export PATH="$PATH:$HOME/.bash-my-aws/bin"
source ~/.bash-my-aws/aliases
source ~/.bash-my-aws/bash_completion.sh

if [ $SYSTEM_TYPE = "Linux" ]; then
  source $HOME/.config/broot/launcher/bash/br
fi;

### ZSH Completions ###
#zplugin light marlonrichert/zsh-autocomplete # must come after fzf
#zplugin light Aloxaf/fzf-tab # make sure its after zsh-completions (see end of README)
# FIXME: lazy load these
#source ~/.zsh/completion/_kubectl # adds ~70ms to zsh startup
#source ~/.zsh/completion/_eksctl

# ruby
if [ $SYSTEM_TYPE = "Linux" ]; then # being lazy...not linux-specific, just mac/work is on 2.5.3 still
  export RUBYOPT='-W:no-deprecated -W:no-experimental'
fi;

zinit light "vifon/deer"
zle -N deer
bindkey '\ek' deer

# pretty colors for df, etc
source /etc/grc.zsh

# kubernetes
#zplugin light superbrothers/zsh-kubectl-prompt
#zplugin snippet OMZ::plugins/kubectl/kubectl.plugin.zsh
#zplugin light "bonnefoa/kubectl-fzf"

# ripgrep
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
