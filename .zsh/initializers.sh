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
#eval "$(--init zsh-hook zsh-ccomp zsh-ccomp-install zsh-wcomp zsh-wcomp-install)"
#alias f='fasd -f'        # file
#alias c='fasd_cd -d'
alias v='f -e vim' # quick opening files with vim
alias o='a -e xdg-open' # quick opening files with xdg-open

# node version manager
#. ~/code/nodejs/nvm/nvm.sh

# autocomplete
compctl -g '~/.teamocil/*(:t:r)' teamocil

# disable autosetting terminal title. (so that tmux `automatic-rename off` actually works)
export DISABLE_AUTO_TITLE="true"

# fasd
#eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-wcomp)"

# Ruby GC Optimizations
# http://fredwu.me/post/60441991350/protip-ruby-devs-please-tweak-your-gc-settings-for
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_FREE_MIN=200000

fpath=(/usr/local/share/zsh-completions $fpath)

# npm/nodejs
export NODE_PATH="/usr/local/share/npm/lib/node_modules"

# z
. `brew --prefix`/etc/profile.d/z.sh

# tmuxinator
export DISABLE_AUTO_TITLE=true
source ~/bin/tmuxinator.zsh

# Homebrew python path (for Mopidy)
export PYTHONPATH=$(brew --prefix)/lib/python2.7/site-packages:$PYTHONPATH

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
