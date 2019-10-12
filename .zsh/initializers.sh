# Core
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export BROWSER='open'
  export EDITOR='/usr/local/bin/nvim' # homebrew
  export SHELL='/usr/local/bin/zsh'
else
  export EDITOR='/usr/bin/nvim'
  export SYSTEMD_EDITOR=$EDITOR
  export BROWSER='/home/bin/firefox'
  export SHELL='/usr/bin/zsh'
  export PATH=${PATH}:$HOME/.local/bin
  # gtk3 hidpi
  export GDK_SCALE=2
  export GDK_DPI_SCALE=0.5
  export XCURSOR_SIZE=48
  export TERMINAL=termite
fi

## tmuxinator
#[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator

# disable autosetting terminal title. (so that tmux `automatic-rename off` actually works)
export DISABLE_AUTO_TITLE=true

# edit command line in vi 
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line # C-x, then e
bindkey '^v' edit-command-line # C-x, then e

fpath=(/usr/local/share/zsh-completions $fpath)

# direnv
eval "$(direnv hook zsh)"

## Ruby
# rbenv
#export RBENV_VERSION=2.5.3
#export PATH="$HOME/.rbenv/bin:$PATH"
#eval "$(command rbenv init -)"

## Python
# pyenv
#export PYTHON_CONFIGURE_OPTS="--enable-shared" # for youcompleteme
#eval "$(pyenv init -)"
##export PYENV_VERSION=3.6.1 #2.7.13 # use pyenv global
#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/shims:$PATH"
#if [ $SYSTEM_TYPE = "Darwin" ]; then
#  . /usr/local/share/zsh/site-functions/pyenv.zsh
#else
#  . $(pyenv root)/completions/pyenv.zsh
#fi

## Node
# nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion -- ugh, this adds >300ms to zsh startup
export NVM_DIR="$HOME/.nvm"
## https://github.com/creationix/nvm/issues/1261
## https://github.com/creationix/nvm/pull/1737
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # OMG nvm startup is slow
nvm-init() {
  # lazy load as loading it by default adds ~300ms to load time
  if [ -s "$NVM_DIR/nvm.sh" ]; then
      # load nvm but don't use it yet: we need to do some other hacks first.
      # see https://github.com/creationix/nvm/issues/1261#issuecomment-366879288
      . "$NVM_DIR/nvm.sh" --no-use
      # i don't need this check, and it's slow (loads npm).
      # do not use the npm `prefix` config; do not report related bugs to nvm ;)
      nvm_die_on_prefix() {
          return 0
      }
      # this also loads npm; let's just skip it.
      nvm_print_npm_version() {
          return 0
      }
      nvm_ensure_version_installed() {
          return 0
      }
      # nvm_resolve_local_alias can also be slow; cache it.
      if [ -s "$NVM_DIR/_default_version" ]; then
          nvm_auto_load_version=$(cat "$NVM_DIR/_default_version")
      else
          nvm_auto_load_version=$(nvm_resolve_local_alias default)
          echo "$nvm_auto_load_version" > "$NVM_DIR/_default_version"
      fi
      nvm use --silent "$nvm_auto_load_version"
  fi
}
#nvm() {
#  nvm-init
#  nvm "$@"
#}

## Golang
export GOPATH=$HOME/opt/golang
export PATH=$HOME/opt/golang/bin:$PATH

# vmux
export VMUX_EDITOR=nvim
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export VMUX_REALEDITOR_VIM=/usr/local/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/local/bin/nvim
else
  export VMUX_REALEDITOR_VIM=/usr/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/bin/nvim
fi

## Other

# fzf fuzzy finder - ctrl-r (history), ctrl-t (files)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# transfer.sh
transfer() {
    wget -t 1 -qO - --method=PUT --body-file="$1" --header="Content-Type: $(file -b --mime-type $1)" https://transfer.sh/$(basename $1);
}
