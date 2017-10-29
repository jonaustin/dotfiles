## SHORTCUTS

# open with suffix aliases
alias -s rb=vim
alias -s txt=vim
alias -s md=vim
alias -s markdown=vim
alias -s py=vim
alias -s sh=vim

#sane defaults
alias sudo='sudo ' # so aliases work
alias vi='vim'
alias mkdir='mkdir -p'
alias grep="grep --color"
alias gp="|grep -i"
alias Grep="grep" # when hitting shift for pipe
alias wget='wget -c' #auto continue files
alias df='df -Th'
#alias rm='rm -i' #ask before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -f' # don't ask
alias rmrf='rm -rf'

#ls
#alias l='ls -CF'
alias ll='ls -l'
alias lll='ls -l|less'
alias la='ls -lA'
alias lt='ls -lt'
alias lth='ls -lt|head'
alias lthn='ls -lt|head -n'
alias lh='ls -lhS' # sort by filesize
alias lsd="ls -l $1 | grep '^d'"
alias lsd2="ls -F $1 | grep \/ | sed -e 's/\/$//g'"

## App shortcuts (GUI/Curses)
#alias sup="rvm use 1.9.2-p180; sup"
alias ff='firefox'

## Apps shortcuts (CLI)
alias t='top'
alias ht='htop'
alias h='cd ~/'
alias lp='lesspipe.sh '
alias loc='locate'
alias nd="ncdu ."

# config files
alias vzc='vim $HOME/.zshrc'
alias vzcl='vim $HOME/.zsh/zshrc_local'
alias zc='source $HOME/.zshrc'
alias vlx='vim $HOME/.zsh/zshrc.local.linux'

## sys
alias u='uptime'
have() { type "$1" &> /dev/null; }

## shortcut-TRICKS
alias igrep='grep -i'
alias g="grep -ir"
alias hist='history | grep -i '
pg() { ps aux | grep -v grep | grep -i ${1}; }
howlong() { ps -p ${1} -o etime=; }
sport() { lsof -P -iTCP:${1}; }
alias cdb='cd ..; cd -' # cd back
rename-spaces-to-underscores() { find . -type f -exec rename 's/ /_/' '{}' \; } # http://stackoverflow.com/a/15012788

## Music
alias rmm3u='find . -iname "*m3u" -print0 | xargs -0 rm'
alias rmmac='find . -iname "__MACOSX" -print0 | xargs -0 rm -rf'
alias retag='find . -type f -print0|xargs -0 id3tag '
alias toptracksall='for n in *; do cd $n; toptracks; cd -; done'

# Vim
alias sv='sudo vim'
alias vrc='vim ~/.bashrc'
alias vlrc='vim ~/.bashrc_local'
alias vvc='vim ~/.vimrc'
alias vvcl='vim ~/.vimrc.local'

## Other
alias aunpackall='for n in *{rar,zip}; do aunpack $n; done'
alias aunpackallr='for n in *rar; do aunpack $n; done'
alias aunpackallz='for n in *zip; do aunpack $n; done'
alias rmspace="prename 's/ /_/g' *"
alias cm='chmod'
alias c='clear'
alias e='exit; clear'
alias svi='sudo vim'
alias svdi='sudo vimdiff'
alias S='sudo '
alias lsg='ls *|grep -i '
alias mine='sudo chown -R jon.users *; sudo chmod -R 775 *;'
alias lsfuncbody='declare -f'
alias lsfunc='declare -F'
alias reset='reset; vr'
alias fdays='find . -mtime '
alias loci='locate -i'

## network
alias pgoo='ping -c2 google.com'
alias wgetnc='wget --no-check-certificate'

# Window Manager

## Monitoring
alias it='iotop'
alias dt='dmesg|tail'
alias tm='tail -f /var/log/messages.log'
alias lm='less /var/log/messages.log'
alias cl='cd /var/log/'

## Git
alias gs='git status --ignore-submodules'
alias gsl='git status --ignore-submodules | less'
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gg='gitg --all&'
alias gx='gitx --all'
alias gd='git diff '
alias got='git '
alias get='git '
alias gpo='git push origin'
  alias gpom='gpo'
alias gp='git pull'
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
vimgd() { vim -p `git status --short | awk '{print $2}'`; }
vimgdm() { vim -p `git diff master --numstat | awk '{print $3}'`; }
gitbr() { git for-each-ref --sort=-committerdate refs/heads/ | head -n 10 | awk '{print $3}' | sed 's@refs/heads/@@' } # git branches sorted by date desc

## ruby
alias cdstdlib="cd $MY_RUBY_HOME/lib/ruby/1.9.1"
alias be='bundle exec'
alias bec='bundle exec cucumber'
alias wr='which ruby'
alias gems='gem search -r '
alias gspec='gem spec -r '
alias gdep='gem dep -r '
alias cdgems="cd $GEM_HOME"
function cdgem() { cd ${GEMO_HOME}/gems/$1*; }
alias qlg='gem contents '
alias glq='gem contents '
alias rtags='rvm use 1.8.7-head; rtags --vi -R -f tmp/tags; rvm use default; sed -i -e "s@\./@../@" tmp/tags' # vi compatible rtags (default is emacs) -- and fails on ruby 1.9.2
alias rctag='ctags -R --exclude=.git --exclude=log *'
alias gwhois='gem whois '
gswhois() { for n in `gems $1|cut -f1 -d' '`; do gem whois $n; done; }
rshowoff() { rvm use 1.8.7; showoff $* ; rvm use default; }
alias yardserver="yard server -g -r -d -p8809"
rgrep() { ruby -ne 'puts $_ if $_ =~ /\$1/' $2; }
alias jekyllr='jekyll --pygments --safe --rdiscount'
function install_global_gems() {
  for n in `cat ~/configs/global.gems`
    do rvm @global do gem install $n
  done
}

# list largest files
alias bigfiles='find . -type f -ls | sort -nrk7 | head -10'

# sharing files
share() { scp $1 jon@ssh.lastfmspot.com:/home/jon/apps/lastfmspot/current/public/ }
alias lf='ssh jon@ssh.lastfmspot.com'

### ruboto
ruboto_gen_app() { ruboto gen app --package com.${1} --name ${2} --target android-8 --activity ${3:-Main} --path `pwd`/${2} ; }
alias ruboto_rake_build="CLASSPATH=$JAVA_HOME/lib/tools.jar rake" # jruby rake having issues with java_home... https://github.com/ruboto/ruboto-core/issues#issue/5

# media conversion
alias wma2ogg='for i in *.wma; do ffmpeg -i $i -acodec vorbis -aq 100 ${i}.ogg; done'
alias ogv2avi='for n in `ls *`; do mencoder $n -ovc lavc -oac mp3lame -o $(echo $n | cut -d "." -f 1).avi; done'
alias ogv2mp4="mencoder out.ogg -of lavf -lavfopts format=mp4 -oac mp3lame -lameopts cbr:br=128 -ovc x264 -x264encopts bitrate=1000 -o final.mp4"
alias ogv23gp='for n in `ls *.ogv`; do sudo ffmpeg -i $n -r 15 -b 64kb -ac 1 -s 176x132 -padtop 6 -padbottom 6 -ar 16000 -ab 32kb -acodec libfaac -vcodec h263 $(echo $n | cut -d "." -f 1).3gp; done'
alias mp423gp='for n in `ls *.mp4`; do mencoder $n -vf scale=176:144 -oac mp3lame -ovc lavc -o $(echo $n | cut -d "." -f 1).3gp; done'

# terminal window
alias vr='n=99; while [ $n -gt 0 ]; do echo; n=`echo $n-1|bc`; done'
vn() { n=$1; while [ $n -gt 0 ]; do echo; n=`echo $n-1|bc`; done; }

## screen
alias screen='TERM=xterm-256color screen -T $TERM'
alias scr='screen -Sx $1'
alias scl='screen -list'
alias sc='screen -Sx '
alias sd='screen -Sd'
#alias scbg='cd ~; screen -c $HOME/.screenrcs/screenrc_bg -S bg'
#alias scqt='cd ~; screen -c $HOME/.screenrcs/screenrc_qt -S qt'
#alias scc='cd ~; screen -c $HOME/.screenrcs/screenrc_coding -S code'

## Coding

## incantations
alias vless='vim -u /usr/share/vim/vim73/macros/less.vim'

# tmux
alias scs='tmux attach -t core'
alias sccs='cd ~; tmux new -s core'
alias tma='tmux attach'
alias tmat='tmux attach -t'

# network
alias digsimple='dig +nocmd +nocomments '
alias rdns='dig +noall +answer -x ' # reverse dns lookup -- or a simpler way is to just use `host <ip>`
alias getip='wget http://checkip.dyndns.org/ -O - -o /dev/null | cut -d: -f 2 | cut -d\< -f 1'

# ruby/rails
alias szs='sleep 30; zeus s'

# iphone simulator
alias iphone_simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/Applications/iPhone\ Simulator.app'
alias ipad_simulator='open /Applications/Xcode.app/Contents/Developer/Platforms/iPadSimulator.platform/Developer/Applications/iPad\ Simulator.app'

# simple http server
alias serve='ruby -run -e httpd . -p 5000'

# node/npm
npmd() { npm view $1 description }
#alias npm='node --max-old-space-size=4000 /usr/local/bin/npm' # temporary: https://github.com/npm/npm/issues/12619#issuecomment-224547637

# get remote ip
alias myip="curl ifconfig.me"

# run various updates
alias maint='sh ~/bin/maintenance.sh'

# https://github.com/nvbn/thefuck
alias fuck='$(thefuck $(fc -ln -1))'

# Rails/ruby
alias rnof='bundle exec rspec --exclude-pattern "spec/features/**/*_spec.rb"'

# Python
alias py3='PYENV_VERSION=3.6.1 '
alias pip3='PYENV_VERSION=3.6.1 pip3'

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
