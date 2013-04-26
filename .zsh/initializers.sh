## rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.
## tmuxinator
[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator

# SSH
# ssh-agent
SSH_ENV="$HOME/.ssh/environment"
start_agent() {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo 'succeeded'
  chmod 600 "${SSH_ENV}"
  . "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add;
}
# Source SSH settings, if applicable
if [ -f "${SSH_ENV}" ]; then
  . "${SSH_ENV}" > /dev/null
  #ps ${SSH_AGENT_PID}
  ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null ||  start_agent;
else
  start_agent;
fi

# fasd
eval "$(fasd --init zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install)"
alias f='fasd -f'        # file
#alias c='fasd_cd -d'
alias v='f -e vim' # quick opening files with vim
alias o='a -e xdg-open' # quick opening files with xdg-open

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator

# node version manager
#. ~/code/nodejs/nvm/nvm.sh

# autocomplete
compctl -g '~/.teamocil/*(:t:r)' teamocil

# disable autosetting terminal title. (so that tmux `automatic-rename off` actually works)
export DISABLE_AUTO_TITLE="true"
