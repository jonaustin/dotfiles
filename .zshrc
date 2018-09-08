# zsh profiling
#setopt prompt_subst; zmodload zsh/datetime; PS4='+[$EPOCHREALTIME]%N:%i> '; set -x
#for n in `seq 0 10`; do time zsh -i -c exit; done
#zmodload zsh/zprof
export ZPLUG_HOME=$HOME/opt/zplug
source $ZPLUG_HOME/init.zsh

# antigen - zsh plugin manager
#if [ $SYSTEM_TYPE = "Darwin" ]; then
#  source /usr/local/share/antigen/antigen.zsh
#else
  #source /usr/share/zsh/share/antigen.zsh
#fi

#zplug "plugins/git", from:oh-my-zsh
#https://github.com/unixorn/awesome-zsh-plugins#plugins
zplug zsh-users/zsh-syntax-highlighting
zplug zsh-users/zsh-autosuggestions
zplug zsh-users/zsh-completions
#zplug zsh-users/zaw # Ctrl-x
zplug mafredri/zsh-async
zplug fcambus/ansiweather
zplug wting/autojump
#zplug Tarrasch/zsh-bd
zplug zdharma/zsh-diff-so-fancy # git dsf
#zplug h3poteto/zsh-ec2ssh

#zplug MichaelAquilina/zsh-you-should-use # alias reminders; meh, no whitelists
zplug djui/alias-tips # alias reminders; ugh adds 300ms to load time
zplug peterhurford/git-it-on.zsh
#zplug Tarrasch/zsh-bd # meh, autojump already does it
#zplug StackExchange/blackbox # gpg encrypt secrets in git repos
zplug supercrabtree/k # pretty directory listings
zplug b4b4r07/enhancd

zplug sindresorhus/pure, use:pure.zsh, as:theme
#autoload -U promptinit; promptinit
#prompt pure
#export RPROMPT='%F{blue}`date +"%F %T"`'

# plugin helpers
[[ -s /home/jon/.autojump/etc/profile.d/autojump.sh ]] && source /home/jon/.autojump/etc/profile.d/autojump.sh
source $HOME/.antigen/bundles/b4b4r07/enhancd/init.sh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load #--verbose

### ZSH {{{
# Path to your oh-my-zsh configuration.
#export ZSH=$HOME/.oh-my-zsh
export SYSTEM_TYPE=`uname`

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="bira"
#export ZSH_THEME="wedisagree"
#export ZSH_THEME="jon"
#if [ $SYSTEM_TYPE = "Darwin" ]; then
#  export ZSH_THEME='refined'
#fi

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
  plugins=(zsh-completions safe-paste zsh-syntax-highlighting bd autojump) # zsh-autosuggestions) # gitfast rails gem rake node  docker encode64 git_remote_branch httpie jira jsontools ng npm pip python aws redis-cli rand-quote systemd taskwarrior urltools web-search gas)
fi

#source $ZSH/oh-my-zsh.sh
#if [ $SYSTEM_TYPE != "Darwin" ]; then
#  autoload -U promptinit; promptinit
#  prompt pure
#fi

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

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "$terminfo[kcuu1]" up-line-or-beginning-search # Up
bindkey "$terminfo[kcud1]" down-line-or-beginning-search # down
bindkey "^P" history-beginning-search-backward
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



autoload -U compinit && compinit -du # -d cache completion info
autoload bashcompinit && bashcompinit # support bash completions
if [ $SYSTEM_TYPE = "GNU/Linux" ]; then
  source $HOME/bin/i3_completion.sh
fi
HELPDIR=/usr/local/share/zsh/help
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

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
