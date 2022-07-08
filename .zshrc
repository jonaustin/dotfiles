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
  source $HOME/bin/i3/i3-completion/i3_completion.sh # i3-msg completions; must come after bashcompinit
  source $HOME/.config/doctl/doctl.zsh; compdef _doctl doctl # $ doctl completion zsh
else # osx
  source $HOME/opt/completions/docker.zsh-completion;         compdef _docker         docker
  source $HOME/opt/completions/docker-compose.zsh-completion; compdef _docker-compose docker-compose
fi

zinit light kazhala/dotbare
_dotbare_completion_cmd

#zinit OMZ::plugins/command-not-found/command-not-found.zsh
zinit snippet OMZ::plugins/fasd/fasd.plugin.zsh # v <fuzzy path> (vim); j <fuzzy path> (cd)
# built-ins - until they get muscle memory
#alias a='fasd -a'        # any
#alias s='fasd -si'       # show / search / select
#alias d='fasd -d'        # directory
#alias f='fasd -f'        # file
#alias sd='fasd -sid'     # interactive directory selection
#alias sf='fasd -sif'     # interactive file selection
#alias z='fasd_cd -d'     # cd, same functionality as j in autojump
#alias zz='fasd_cd -d -i' # cd with interactive selection

#zinit light "stevemcilwain/nonotes" # nmap zsh funcs

#zinit light "macunha1/zsh-terraform"
#zinit light hanjunlee/terragrunt-oh-my-zsh-plugin

# node
#export NVM_LAZY_LOAD=true
#zplug "lukechilds/zsh-nvm" # even with lazy loading adds ~0.1 to zsh startup

# asdf: multi-lang version mgr
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export ASDF_DIR="$(brew --prefix asdf)/libexec"
  . "$ASDF_DIR/asdf.sh"
else
  . /opt/asdf-vm/asdf.sh
fi

# colors
## pretty colors (using grc) for various commands; diff,mtr,netstat,ps,etc
zinit light unixorn/warhol.plugin.zsh
#export LS_COLORS="di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43" # https://geoff.greer.fm/lscolors/

# Base16 Shell
#zinit light "chriskempson/base16-shell" # base16<tab> # color themes
# trapd00r
#zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
#    atpull'%atclone' pick"clrs.zsh" nocompile'!' \
#    atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
#zinit light trapd00r/LS_COLORS

# pywal; automatic colors from current wallpaper
#if [ ! $SYSTEM_TYPE = "Darwin" ]; then
#  (cat ~/.cache/wal/sequences &) # async
#fi;



##################################################

# navi
#zinit light denisidoro/navi/navi.plugin.zsh # ^g
#zinit light "denisidoro/navi", as:command, use:"navi"

# https://github.com/unixorn/awesome-zsh-plugins#plugins
# zaw
zinit light zsh-users/zaw
  zinit light termoshtt/zaw-systemd
# zinit light mafredri/zsh-async # cool, but haven't needed it: https://github.com/mafredri/zsh-async#example-code
zinit light fcambus/ansiweather # $ weather <zip>
zinit light "z-shell/zsh-diff-so-fancy" # $ git dsf
#zinit light h3poteto/zsh-ec2ssh

# and see bindkey below
#zinit light "aperezdc/zsh-notes"
#zstyle :notes home  $HOME/notes/

#zinit light MichaelAquilina/zsh-you-should-use # alias reminders; meh, no whitelists
#zinit light djui/alias-tips # alias reminders; ugh adds 300ms to load time
zinit light "peterhurford/git-it-on.zsh" # gitit -- open your current folder, on your current branch, in GitHub or GitLab
#zinit light StackExchange/blackbox # gpg encrypt secrets in git repos

# argh; i'll just make my own mappings; latest update overrode `ps` for fuck's sake
#zinit light "wfxr/forgit" # fzf for git -- ga; glo; gi; gd; grh; gcf; gss; gclean
#  forgit_log=glo
#  forgit_diff=gd
#  forgit_add=ga
#  forgit_reset_head=grh
#  forgit_ignore=gi
#  forgit_checkout_file=gcf
#  forgit_checkout_branch=gcb
#  forgit_branch_delet=gbd
#  forgit_checkout_tag=gct
#  forgit_checkout_commit=gco
# unalias grc # grc breaks...grc
#  forgit_clean=gclean
#  forgit_stash_show=gss
#  forgit_cherry_pick=gcp
#  forgit_rebase=grb
#  forgit_fixup=gfu
zinit light "b4b4r07/emoji-cli" # ctrl-s
#zinit light "amstrad/oh-my-matrix"

# Prompt
zinit light "jonaustin/powerlevel10k"
# To customize prompt, run `p10k configure` or edit .p10k.zsh.
# Show prompt segment "kubecontext" only when the command you are typing
# invokes kubectl, helm, kubens, kubectx, oc, istioctl, kogito, k9s or helmfile.
typeset -g POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND='kubectl|helm|kubens|kubectx|oc|istioctl|kogito|k9s|helmfile'
[[ -f ~/.zsh/p10k.zsh ]] && source ~/.zsh/p10k.zsh

zinit ice wait'1' lucid
zinit light laggardkernel/zsh-thefuck

#zinit ice wait'1' lucid
#zinit light "buonomo/yarn-completion"

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
setopt extendedglob         # extended globbing. Allows using regular expressions with *
setopt nocaseglob           # case insensitive globbing
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
# todo: maybe grab some from https://github.com/arp242/dotfiles/blob/master/zsh/zshrc#L145


# history
#setopt append_history        # appends history file instead of replacing it (not needed if share_history is enabled)
setopt extended_history       # add timestamps to history
setopt hist_ignore_all_dups   # don't record dupes in history
setopt hist_ignore_space      # remove command line from history list when first character on the line is a space
setopt hist_reduce_blanks     # remove superflous blanks
setopt hist_verify            # when interpolating history into commands (e.g. `$ echo !!`; require another press of enter to actually execute after expanding the last command with `!!`)
setopt share_history          # adds history incrementally and share it across sessions

# ehhh, this results in no history getting added...
# zshaddhistory() { whence ${${(z)1}[1]} >| /dev/null || return 1 } # do not add failed commands to history - https://superuser.com/a/902508

# zstyles
# todo: organize completion section; steal from https://github.com/arp242/dotfiles/blob/master/zsh/zshrc#L201
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' hist-find-no-dups yes # ignore duplicates in history source
## case insensitive path completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|?=**' 

## Make new commands immediately visible to zsh
zstyle ':completion:*' rehash true

## Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR

# bindkeys
bindkey -v # vi mode

#export ZVM_VI_ESCAPE_BINDKEY='jk'
#zinit ice depth=1
#zinit light jeffreytse/zsh-vi-mode
# eh...wish i could just disable in zsh-vi-mode whatever's breaking fzf
#zvm_after_init_commands+=('[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh')

## open current command in vim; esc to visual mode and hit 'v'
export VISUAL=$EDITOR
autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# history search (fzf overrides this, but may as well keep it for fallback)
# beginning-of-search: e.g. if you type ls and press up it will only find history entries that start with ls
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
bindkey '^r' history-incremental-search-backward # ctrl-r starts searching history backward (note: fzf fuzzy search makes this waaay better)

# zsh-notes
#bindkey '^N' notes-edit-widget

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

export KEYTIMEOUT=1 # vi-mode: reduce lag between hitting esc and entering normal mode - https://dougblack.io/words/zsh-vi-mode.html, https://superuser.com/a/648046

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
# remove: let the terminal set the TERM var; pint xterm-256color
# export TERM=xterm-256color # https://github.com/mhinz/vim-galore#true-colors
# }}}

export PATH=/usr/local/bin:/usr/local/sbin:~/bin:~/opt/bin:$PATH

# doom emacs
export PATH=$PATH:$HOME/.emacs.d/bin/

# OS configs and fzf
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export PATH="$PATH:/sbin:/usr/sbin:$HOME/.local/bin"
  . ${HOME}/.zsh/zshrc.local.osx
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
  bindkey "ç" fzf-cd-widget # fix alt-c for `cd` fzf for osx
elif [ $SYSTEM_TYPE = "Linux" ]; then
  . ${HOME}/.zsh/zshrc.local.linux
  . /usr/share/fzf/completion.zsh
  . /usr/share/fzf/key-bindings.zsh
fi
# note for some unknown reason --follow (symlinks) causes it not to find anything under ~/.zsh (which is not a symlink dir regardless). fd bug?
FD_OPTIONS="--no-ignore --hidden --exclude .git --exclude node_modules --exclude .cache --exclude .asdf"
# notes:
#   -1: automatically select the only match
#   open in vim: https://github.com/junegunn/fzf/issues/1593
#   --no-mouse: otherwise can't copy from terminal with mouse (gets locked to fzf)
#   --reverse: put prompt at top
#   --height=50%: only take up 50% of the terminal screen
#   --multi: enables multi-select with shift-tab
#   --info=inline: save space my showing on same line
#   --preview: preview files with e.g. bat (syntax highlighted); toggle with F2
#   --preview-window: attributes for preview window; add ':hidden:' to hide by default
export FZF_DEFAULT_OPTS="--no-mouse --height=50% -1 --reverse --multi --info=inline --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2> /dev/null | head -300' --preview-window='right:wrap' --bind='f3:execute(bat --style=numbers {} || less -f {}),f2:toggle-preview,ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy),ctrl-e:execute(nvim -p {+} < /dev/tty > /dev/tty 2>&1),ctrl-x:execute(rm -i {+})+abort'"
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

# rust
export PATH=$PATH:$HOME/.cargo/bin

typeset -U PATH # remove duplicate paths

# load avn
#[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh"

# go
export GOPATH=$HOME/opt/_go
export GOBIN=$GOPATH/bin
export PATH=$HOME/opt/_go/bin:$PATH
export GO111MODULE=on # this can go away with 1.16 - https://tip.golang.org/doc/go1.16
zinit snippet OMZ::plugins/golang/golang.plugin.zsh # completions/aliases

# shell enhancements
# cd - <enter> -- recent dirs
# cd .. <enter>
# cd <enter> fzf search dirs
#zinit light "b4b4r07/enhancd"
#export ENHANCD_FILTER=fzf-tmux

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

#eval "$(direnv hook zsh)"
#. $HOME/opt/dntw/dntw.sh # dedicated neovim tmux window; start tmux with `dntw`

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
  export GDK_SCALE=1
  #export GDK_DPI_SCALE=0.5
  export GDK_DPI_SCALE=1
  export QT_SCALE_FACTOR=0.7
  export QT_AUTO_SCREEN_SCALE_FACTOR=1.0
  export XCURSOR_SIZE=32
  export TERMINAL=alacritty
  export DISABLE_AUTO_TITLE=true
fi
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# aws
complete -C $(which aws_completer) aws
# bash-my-aws
export PATH="$PATH:$HOME/.bash-my-aws/bin"
source ~/.bash-my-aws/aliases
source ~/.bash-my-aws/bash_completion.sh

if [ $SYSTEM_TYPE = "Linux" ]; then
  source $HOME/.config/broot/launcher/bash/br
fi;

### ZSH Completions ###
# this won't work with zinit due to a bug: https://github.com/marlonrichert/zsh-autocomplete/issues/335
#zinit light marlonrichert/zsh-autocomplete # must come after fzf

# ruby
# eh, add this back when i'm not on 2.5.3 at work
# if [ ASDF_RUBY_VERSION = "2.5.3" ]; then # will cause super weird error on 2.5.3
#   export RUBYOPT='-W:no-deprecated -W:no-experimental'
# fi;

#zinit light "vifon/deer"
#zle -N deer
#bindkey '\ek' deer # alt-k

# pretty colors for df, etc
source /etc/grc.zsh

# kubernetes
#zinit light superbrothers/zsh-kubectl-prompt
#zinit snippet OMZ::plugins/kubectl/kubectl.plugin.zsh
#zinit light "bonnefoa/kubectl-fzf"

# ripgrep
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# cod -- automatic command completions
# just run <cmd> --help and it'll add it
# then `<cmd> -<tab>`
# i.e. it _only_ works if you put a `-` before tab'ing!
# go get -u github.com/dim-an/cod
zinit wait lucid for dim-an/cod

# fzf-tab
# needs to be loaded after compinit, but before plugins which will wrap widgets, such as zsh-autosuggestions or fast-syntax-highlighting
zinit light Aloxaf/fzf-tab # make sure its after zsh-completions (see end of README)
zstyle ':fzf-tab:*' fzf-bindings 'space:accept' # hit space (instead of enter) to accept completion

# syntax coloring
# Unfortunately has a super weird bug where it hangs reliably with: git checkout $(git branch -a | grep -v remotes | fzf)
# zinit light zdharma/fast-syntax-highlighting

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions # fish-like autosuggestions for zsh
zinit light zsh-users/zsh-completions # just various completions

# calibre
export PATH=$PATH:$HOME/opt/calibre/ # bin/ doesn't work for some reason

if [ $SYSTEM_TYPE = "Linux" ]; then
  # perl
  PATH="/home/jon/perl5/bin${PATH:+:${PATH}}"; export PATH;
  PERL5LIB="/home/jon/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
  PERL_LOCAL_LIB_ROOT="/home/jon/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
  PERL_MB_OPT="--install_base \"/home/jon/perl5\""; export PERL_MB_OPT;
  PERL_MM_OPT="INSTALL_BASE=/home/jon/perl5"; export PERL_MM_OPT;
fi

# Things I always forget 
# FOO="${VARIABLE:-default}"
