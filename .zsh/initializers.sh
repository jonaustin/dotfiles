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
export PYENV_VERSION=3.6.1 #2.7.13
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
if [ $SYSTEM_TYPE = "Darwin" ]; then
  . /usr/local/share/zsh/site-functions/pyenv.zsh
else
  . $(pyenv root)/completions/pyenv.zsh
fi

# shims aren't being copied from VERSION/bin to .pyenv/shims for some reason
export PATH="$PYENV_ROOT/versions/2.7.13/bin:$PATH"
export PATH="$PYENV_ROOT/versions/3.6.1/bin:$PATH"

## Node
# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

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
# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
#antigen bundle command-not-found

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
#antigen theme pure #FIXME: use my custom version

# Tell Antigen that you're done.
antigen apply
###

## Other

# tabtab source for serverless package (npm i -g tabtab)
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh

# fzf fuzzy finder - ctrl-r (history), ctrl-t (files)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
