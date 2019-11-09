## SHORTCUTS

# overrides
#alias cat=bat
alias ll=k; alias lsl='ls -l'

# open with suffix aliases
alias -s md=vim
alias -s markdown=vim
alias -s txt=vim
#alias -s py=vim
#alias -s rb=vim
#alias -s sh=vim

#sane defaults
alias sudo='sudo ' # so aliases work
alias fd='fd -HI' # search all files
alias vi='vim'
alias spacemacs='emacs'
alias mkdir='mkdir -p'
alias grep="grep --color"
alias gi="|grep -i"
alias Grep="grep" # when hitting shift for pipe
alias wget='wget -c' #auto continue files
alias df='df -Th'
#alias rm='rm -i' #ask before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -f' # don't ask
alias rmrf='rm -rf'
alias tree='tree -I "node_modules"' # add more with ignorethis|andthis
alias ip='ip -br -c a' # ip -brief -color address

#ls
#alias l='ls -CF'
#alias ll='ls -l'
alias lll='ls -l|less'
#alias la='ls -lA'
alias la='k -a'
#alias lt='ls -lt'
alias lt='k -lt'
alias lth='ls -lt|head'
alias lthn='ls -lt|head -n'
#alias ltr='ls -ltr'
alias ltr='k -cr'
alias ltrn='ls -ltr| tail -n'
alias lh='ls -lhS' # sort by filesize
alias lsd="ls -l $1 | grep '^d'" # only dirs
alias lsd-names="ls -F $1 | grep \/ | sed -e 's/\/$//g'" # bare dir names for looping and such

# other
alias ..='cd ..'
alias -- -='cd -' # -- treats next chars as operand

## App shortcuts (GUI/Curses)
alias ff='firefox'

## Apps shortcuts (CLI)
alias t='top'
alias ht='htop'
alias h='cd ~/'
alias lp='lesspipe.sh '
alias loc='locate'
alias nd="ncdu ."
alias vnode='NODE_NO_READLINE=1 rlwrap node' # vim node repl

# config files
alias zc='source $HOME/.zshrc'
alias vzc='$EDITOR $HOME/.zshrc'
alias vzcl='$EDITOR $HOME/.zsh/zshrc_local'
alias vlx='$EDITOR $HOME/.zsh/zshrc.local.linux'
alias vrc='$EDITOR $HOME/.bashrc'
alias vlrc='$EDITOR $HOME/.bashrc_local'
alias vac='$EDITOR $HOME/.zsh/aliases.sh'
alias vfc='$EDITOR $HOME/.zsh/functions.sh'

# other editing shortcuts
alias vnlog='$EDITOR ~/.config/configstore/nlog.json'

## sys
alias u='uptime'
have() { type "$1" &> /dev/null; }

## shortcut-TRICKS
alias findwindow="python ~/bin/i3/i3-wm-scripts/nextmatch.py"
alias igrep='grep -i'
alias g="grep -ir"
alias hist='cat ~/.zsh_history* | grep -i'
alias gh='cat ~/.zsh_history* | grep -i'
pg() { ps aux | grep -v grep | grep -i ${1}; }
howlong() { ps -p ${1} -o etime=; }
sport() { lsof -P -iTCP:${1}; }
alias cdb='cd ..; cd -' # cd back (sometimes needed to reload dotfiles)

# http://stackoverflow.com/a/15012788
rename-spaces-to-underscores() { 
  find . -type f -exec rename 's/ /_/' '{}' \; 
}

## Music
alias rmm3u='find . -iname "*m3u" -print0 | xargs -0 rm'
alias rmmac='find . -iname "__MACOSX" -print0 | xargs -0 rm -rf'
alias retag='find . -type f -print0|xargs -0 id3tag '

# Vim
alias vim='dntw_edit'
#alias vim='nvim'
alias vimdiff='$EDITOR -d'
alias svi='sudo $EDITOR'
alias svdi='sudo vimdiff'
alias vvc='$EDITOR ~/.vimrc'
alias vvcl='$EDITOR ~/.vimrc.local'
alias vvh='$EDITOR ~/.hyper.js'
alias vvj='$EDITOR ~/.hyper.js'


## Other
alias aunpackall='for n in *{rar,zip}; do aunpack $n; done'
alias aunpackallr='for n in *rar; do aunpack $n; done'
alias aunpackallz='for n in *zip; do aunpack $n; done'
alias c='clear'
alias lsg='ls *|grep -i '
alias mine='sudo chown -R $USER:$USER *; sudo chmod -R 775 *;'
alias lsfuncbody='declare -f'
alias lsfunc='declare -F'
alias reset='reset; vr'
alias fdays='find . -mtime '

## network
alias pgoo='ping -c2 google.com'
alias wgetnc='wget --no-check-certificate'
alias syn='ssh root@10.0.1.8'

## Monitoring
alias it='iotop'

## Git
alias gs='git status --ignore-submodules'
alias gsl='git status --ignore-submodules | less'
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gcv='git commit --no-verify'
alias gg='gitg --all&'
alias gx='gitx --all'
alias gd='git diff '
alias gdl='git diff|less'
alias gsd='git ds'
alias gsdl='git ds|less'
alias got='git '
alias get='git '
alias gpo='git push origin'
  alias gpom='gpo'
alias gp='git pull'
alias gpp='git-up'
alias gclone='git clone '
alias git-diff=git_diff
alias glpo='git log --oneline --decorate'
alias gdd='git diff develop | less'
function git_diff() {
  git diff --no-ext-diff -w "$@" #| vim -R -
}
function ggh() {
  # git grep all history
  echo $1
  git grep $1 $(git rev-list --all)
}
function git-blame-all() {
  git ls-files -z | xargs -0n1 git blame -w --show-email | perl -n -e '/^.*?\((.*?)\s+[\d]{4}/; print $1,"\n"' | sort -f | uniq -c | sort -n
}
vimgd() { vim -p `git status --short | awk '{print $2}'`; }
vimgdm() { vim -p `git diff master --numstat | awk '{print $3}'`; }
vimgdbr() { vim -p `git diff $1 --numstat | awk '{print $3}'`; }
gitbr() { git for-each-ref --sort=-committerdate refs/heads/ | head -n 10 | awk '{print $3}' | sed 's@refs/heads/@@' } # git branches sorted by date desc

## ruby
alias be='bundle exec'
alias gems='gem search -r '
alias gspec='gem spec -r '
alias gdep='gem dep -r '
function cdgem() { cd ${GEMO_HOME}/gems/$1*; }

# list largest files
alias bigfiles='find . -type f -ls | sort -nrk7 | head -10'

# media conversion
alias wma2ogg='for i in *.wma; do ffmpeg -i $i -acodec vorbis -aq 100 ${i}.ogg; done'
alias ogv2avi='for n in `ls *`; do mencoder $n -ovc lavc -oac mp3lame -o $(echo $n | cut -d "." -f 1).avi; done'
alias ogv2mp4="mencoder out.ogg -of lavf -lavfopts format=mp4 -oac mp3lame -lameopts cbr:br=128 -ovc x264 -x264encopts bitrate=1000 -o final.mp4"
alias ogv23gp='for n in `ls *.ogv`; do sudo ffmpeg -i $n -r 15 -b 64kb -ac 1 -s 176x132 -padtop 6 -padbottom 6 -ar 16000 -ab 32kb -acodec libfaac -vcodec h263 $(echo $n | cut -d "." -f 1).3gp; done'
alias mp423gp='for n in `ls *.mp4`; do mencoder $n -vf scale=176:144 -oac mp3lame -ovc lavc -o $(echo $n | cut -d "." -f 1).3gp; done'

# terminal window
alias vr='for n in `seq 0 99`; do echo; done'

## Coding

# tmux
alias tma='tmux attach'
alias tmat='tmux attach -t'

# network
alias digsimple='dig +nocmd +nocomments '
alias rdns='dig +noall +answer -x ' # reverse dns lookup -- or a simpler way is to just use `host <ip>`
alias getip='wget http://checkip.dyndns.org/ -O - -o /dev/null | cut -d: -f 2 | cut -d\< -f 1'

# simple http server
alias serve='ruby -run -e httpd . -p 5000'

# node/npm
npmd() {
  npm view $1 description
}
npm-list() { #'npm list -g --depth=0' # list globally installed modules
  npm -g ls --parseable --depth=0 | awk '{gsub(/\/.*\//,"",$1); print}'| sort -u
}

# get remote ip
alias myip="curl https://canhazip.com/" #"curl ifconfig.me"

# https://github.com/nvbn/thefuck
alias fuck='$(thefuck $(fc -ln -1))'

# Python
alias pip-upgrade-all=' pip freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U' # no, really: https://github.com/pypa/pip/issues/59

### Apps
alias mpsyt="PYENV_VERSION=3.6.1 mpsyt" # youtube cli player

### Database
alias mys='mycli -uroot '

# Youtube-dl
alias yta='youtube-dl -x -f bestaudio --prefer-free-formats -i --output "%(title)s.%(ext)s" '
alias ytalbum='youtube-dl -x -o "%(autonumber)s - %(title)s.%(ext)s" --autonumber-size 2 --audio-format=opus '
alias ytv='youtube-dl -f bestvideo+bestaudio --prefer-free-formats -i --output "%(title)s.%(ext)s"'
alias ytcast='youtube-dl -o - https://youtu.be/BaW_jenozKc | castnow --quiet -' # cast to chromecast

### Systemd
alias restart='sudo systemctl restart '
alias start='sudo systemctl start '
alias stop='sudo systemctl stop '
alias status='sudo systemctl status '
alias reload='sudo systemctl daemon-reload'
alias restartu='systemctl --user restart '
alias startu='systemctl --user start '
alias stopu='systemctl --user stop '
alias statusu='systemctl --user status '
#alias reloadu='systemctl --user daemon-reload'

# i3
alias cur-monitor="xrandr | grep -C 3 '*' | grep DP | awk '{print \$1}'"

# work
alias wvpn='nmcli con down "US Seattle"; nmcli con up ls'
alias hvpn='nmcli con down ls; nmcli con up "US Seattle"'

# s (web-cli search)
alias ss='/usr/bin/s' # override s aliased to yay
alias sa='/usr/bin/s -p amazon'
alias sw='/usr/bin/s -p wikipedia'

# fasd
# these are largely the same as the aliases defined in /usr/bin/fasd script
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias j='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection
