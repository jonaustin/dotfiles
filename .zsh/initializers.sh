# Core
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export BROWSER='open'
  export EDITOR='/usr/local/bin/nvim' # homebrew
  export SHELL='/usr/local/bin/zsh'
else
  export EDITOR='/usr/bin/nvim'
  export SYSTEMD_EDITOR=$EDITOR
  export BROWSER='/usr/bin/chromium'
  export SHELL='/usr/bin/zsh'
  export PATH=${PATH}:$HOME/.local/bin
  # gtk3 hidpi
  export GDK_SCALE=2
  export GDK_DPI_SCALE=0.5
  export XCURSOR_SIZE=48
  export TERMINAL=/usr/bin/hypert
fi

## tmuxinator
#[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator

# autocomplete
compctl -g '~/.teamocil/*(:t:r)' teamocil

# disable autosetting terminal title. (so that tmux `automatic-rename off` actually works)
export DISABLE_AUTO_TITLE=true

# edit command line in vi
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line

fpath=(/usr/local/share/zsh-completions $fpath)

# direnv
eval "$(direnv hook zsh)"

## Ruby
# rbenv
eval "$(rbenv init -)"
export RBENV_VERSION=2.3.4
export PATH="$HOME/.rbenv/bin:$PATH"

## Python
#source /usr/bin/activate.sh # https://github.com/kennethreitz/autoenv/

# virtualenvwrapper
#export WORKON_HOME=$HOME/.virtualenvs
#source /usr/bin/virtualenvwrapper.sh

# pyenv
export PYTHON_CONFIGURE_OPTS="--enable-shared" # for youcompleteme
eval "$(pyenv init -)"
#export PYENV_VERSION=3.6.1 #2.7.13 # use pyenv global
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
if [ $SYSTEM_TYPE = "Darwin" ]; then
  . /usr/local/share/zsh/site-functions/pyenv.zsh
else
  . $(pyenv root)/completions/pyenv.zsh
fi

# shims aren't being copied from VERSION/bin to .pyenv/shims for some reason - use rehash
#export PATH="$PYENV_ROOT/versions/2.7.13/bin:$PATH"
#export PATH="$PYENV_ROOT/versions/3.6.1/bin:$PATH"

## Node
# nvm
export NVM_DIR="$HOME/.nvm"
# https://github.com/creationix/nvm/issues/1261
# https://github.com/creationix/nvm/pull/1737
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # OMG nvm startup is slow

if [ -s "$NVM_DIR/nvm.sh" ]; then
    # Load nvm but don't use it yet: we need to do some other hacks first.
    # See https://github.com/creationix/nvm/issues/1261#issuecomment-366879288
    . "$NVM_DIR/nvm.sh" --no-use
    # I don't need this check, and it's slow (loads npm).
    # Do not use the npm `prefix` config; do not report related bugs to nvm ;)
    nvm_die_on_prefix() {
        return 0
    }
    # This also loads npm; let's just skip it.
    nvm_print_npm_version() {
        return 0
    }
    nvm_ensure_version_installed() {
        return 0
    }
    # nvm_resolve_local_alias can also be slow; cache it.
    if [ -s "$NVM_DIR/_default_version" ]; then
        NVM_AUTO_LOAD_VERSION=$(cat "$NVM_DIR/_default_version")
    else
        NVM_AUTO_LOAD_VERSION=$(nvm_resolve_local_alias default)
        echo "$NVM_AUTO_LOAD_VERSION" > "$NVM_DIR/_default_version"
    fi
    nvm use --silent "$NVM_AUTO_LOAD_VERSION"
fi

# vmux
export VMUX_EDITOR=nvim
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export VMUX_REALEDITOR_VIM=/usr/local/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/local/bin/nvim
else
  export VMUX_REALEDITOR_VIM=/usr/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/bin/nvim
fi

# antigen - zsh plugin manager
if [ $SYSTEM_TYPE = "Darwin" ]; then
  source /usr/local/share/antigen/antigen.zsh
else
  source /usr/share/zsh/share/antigen.zsh
fi

## Other

# tabtab source for serverless package (npm i -g tabtab)
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh

# fzf fuzzy finder - ctrl-r (history), ctrl-t (files)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
