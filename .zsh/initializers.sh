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
export PATH="$PYENV_ROOT/bin:$PATH"
#source $(pyenv root)/completions/pyenv.zsh

## Node
# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# vmux
export VMUX_EDITOR=nvim
if [ $SYSTEM_TYPE = "Darwin" ]; then
  export VMUX_REALEDITOR_VIM=/usr/local/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/local/bin/nvim
else
  export VMUX_REALEDITOR_VIM=/usr/bin/vim
  export VMUX_REALEDITOR_NVIM=/usr/bin/nvim
fi

# fzf
source ~/.fzf.zsh

## Other

# tabtab source for serverless package (npm i -g tabtab)
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /home/jon/.nvm/versions/node/v6.8.1/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
