# init {{{
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="flazz"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git github vi-mode rails gem bundler ruby)

source $ZSH/oh-my-zsh.sh
# }}}
# Options {{{
unsetopt beep
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# }}}
# Exports {{{ 
export PATH="$PATH:/usr/lib/perl5/vendor_perl/bin/:/sbin:/usr/sbin:/home/jon/bin:/usr/local/bin:/usr/lib/surfraw:/opt/android-sdk/tools:/usr/local/bin:/home/jon/bin/ruby:/home/jon/bin/bash:/home/jon/bin/mpd:/home/jon/bin/subtle"

export EDITOR='vim'
export BROWSER='elinks'
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr -R' # --LINE-NUMBERS --quit-if-one-screen' # -R for less coloring with source-highlight (external app)
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export DISPLAY=:0
export HISTCONTROL=ignoredups # don't put duplicate lines in the history. See bash(1) for more options
export TERM=xterm-256color    # no idea why I didn't add this before
export MOZ_DISABLE_PANGO=1 # improve rendering (may also fix font issues for all mozilla apps -- https://wiki.archlinux.org/index.php/Firefox_Tips_and_Tweaks#Network_settings )
#export WINEDEBUG=-all

export SAVEHIST=100000
export HISTSIZE=10000
export OOO_FORCE_DESKTOP=gnome
export INPUTRC=~/.inputrc
#export HTTP_PROXY=http://127.0.0.1:8118
#export http_proxy=http://127.0.0.1:8118
export OPCODEDIR64=/lib/csound/plugins64  # for csound5
# }}} 

ulimit -S -c 0        # Don't want any coredumps

# disable ^S/^Q flow control 
stty -ixon

# Colourful manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

## SHORTCUTS
#sane defaults
alias vi='vim'
alias mkdir='mkdir -p'
alias grep="grep --color"
alias nautilus="nautilus --no-desktop --browser"
alias wget='wget -c' #auto continue files
alias df='df -Th'
alias free='free -m'
alias info='pinfo'
alias fortune='echo && fortune tao && echo'

# non pkg'd app-updates
alias upcalibre="sudo python2 -c \"import urllib2; exec urllib2.urlopen('http://status.calibre-ebook.com/linux_installer').read(); main()\""

#alias rm='rm -i' #ask before overwriting
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -f' # don't ask


#ls
#alias l='ls -CF'
alias ll='ls -l'
alias lll='ls -l|less'
alias la='ls -lA'
alias lt='ls -lt'
alias lth='ls -lt|head'
alias lthn='ls -lt|head -n'
alias lh='ls -lhS' # sort by filesize
alias lsd="ls -l $1 | grep -r ^d"
alias lsd2="ls -F $1 | grep \/ | sed -e 's/\/$//g'"
# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'
fi

# convenience #

## App shortcuts (GUI/Curses)
alias sup="rvm use 1.9.2-p180; sup"
alias ff='firefox'

## Apps shortcuts (CLI)
alias t='top'
alias ht='htop'
alias h='head ' 
alias lp='lesspipe.sh '
alias loc='locate'
alias nd="ncdu ."
alias rex='rexima' #vi style vol control
alias am='alsamixer'

# zsh
alias vzc='vim $HOME/.zshrc'
alias vzcl='vim $HOME/.zshrc_local'
alias zc='source $HOME/.zshrc'

## sys
alias u='uptime'
alias udb='sudo updatedb'
alias killspkr='sudo modprobe -r pcspkr'
alias psm="echo '%CPU %MEM   PID COMMAND' && ps hgaxo %cpu,%mem,pid,comm | sort -nrk1 | head -n 5 | sed -e 's/-bin//'" #get top cpu eating processess 
have() { type "$1" &> /dev/null; }

## shortcut-TRICKS
alias igrep='grep -i'
alias g="grep -ir"
alias hist='history | grep -i '
pg() { ps aux | grep -v grep | grep -i ${1}; }

## Music
alias rmm3u='find . -iname "*m3u" -print0 | xargs -0 rm'
alias rmmac='find . -iname "__MACOSX" -print0 | xargs -0 rm -rf'
alias retag='find . -type f -print0|xargs -0 id3tag '
alias toptracksall='for n in *; do cd $n; toptracks; cd -; done'

### MPD
alias m='ncmpcpp -p 6600' # mpd alsa
alias toptracks='toptracks.rb'

## Other
alias aunpackall='for n in *; do aunpack $n; done'
alias rmspace="prename 's/ /_/g' *"
alias cm='chmod'
alias sv='sudo vim'
alias vrc='vim ~/.bashrc'
alias vlrc='vim ~/.bashrc_local'
alias v='vr'
alias vvc='vim ~/.vimrc'
alias vsc='vim ~/.config/subtle/subtle.rb'
alias c='clear'
alias e='exit; clear'
alias svi='sudo vim'
alias svdi='sudo vimdiff'
alias S='sudo '
alias lsg='ls *|grep -i '
alias mine='sudo chown -R jon.users *; sudo chmod -R 775 *;'
alias lsfuncbody='declare -f'
alias lsfunc='declare -F' 
alias xm='/home/jon/.xmodmap'
alias reset='reset; v'
alias fdays='find . -mtime '
alias loci='locate -i'
alias acka='ack -air '

## network
alias pgoo='ping -c2 google.com' 
alias wgetnc='wget --no-check-certificate'
### ssh
alias pb='ssh jonaustin@mrfantastic.dreamhost.com'
alias home_proxy='ssh -D 8080 -f -C -q -N jon@xs.homeunix.net'
alias work_proxy='ssh -D 8080 -f -C -q -N jon@barracuda-ext.cmdpdx.com'
alias work_rdc='ssh jon@barracuda-ext.cmdpdx.com -L 10000:jaustin.cmdpdx.com:3389' # tunnel rdc connection to localhost:10000
### synergy
alias syn='synergyc 192.168.0.9'
alias ssyn='ssh -f -N -L12345:10.10.10.155:24800 barracuda-ext.cmdpdx.com; synergyc localhost:12345'
alias ksyn="killall synergyc"
### non-frak
alias sxs='ssh jon@frak'
alias fxs='sftp jon@xs.homeunix.net'
alias home_proxy='ssh -D 8080 -f -C -q -N jon@xs.homeunix.net'
alias sxxs='ssh jon@xs.homeunix.net'
alias sfs='ssh jon@frak'
alias sss='ssh jon@sam'
alias xsfs='sshfs -o reconnect jon@192.168.0.99:/ /media/xs'
alias xxsfs='sshfs -o reconnect jon@xs.homeunix.net:/ /media/xs'
alias fsfs='sshfs -o reconnect -o allow_other jon@frak:/ /media/frakssh'
alias ssfs='sshfs -o reconnect -o allow_other jon@sam:/ /media/sam'
# queries
alias rdns='dig +noall +answer -x ' # reverse dns lookup -- or a simpler way is to just use `host <ip>`
alias getip='wget http://checkip.dyndns.org/ -O - -o /dev/null | cut -d: -f 2 | cut -d\< -f 1'

# Window Manager
## subtle
alias sl='subtler'
alias fbg="`cat ~/.fehbg`"

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
alias gd='git diff'
alias go='git checkout '
alias gg='gitg --all&'
alias gx='gitx --all'
alias got='git '
alias get='git '
alias gpo='git push origin'
  alias gpom='gpo'
alias gp='git pull'
alias gclone='git clone '
function git_diff() {
  git diff --no-ext-diff -w "$@" #| vim -R -
}
alias git-diff=git_diff
alias glpo='git log --oneline --decorate'

## ruby
alias be='bundle exec'
alias bec='bundle exec cucumber'
alias wr='which ruby'
alias gems='gem search -r '
alias gspec='gem spec -r '
alias gdep='gem dep -r '
alias cdgems='cd /home/jon/.rvm/gems/ruby-1.9.2-p290/gems/'
function cdgem() { cd /home/jon/.rvm/gems/ruby-1.9.2-p290/gems/$1*; }
alias qlg='gem contents '
alias glq='gem contents '
alias rtags='rvm use 1.8.7; rtags --vi -R -f tmp/tags; rvm use default' # vi compatible rtags (default is emacs) -- and fails on ruby 1.9.2
alias gwhois='gem whois '
gswhois() { for n in `gems $1|cut -f1 -d' '`; do gem whois $n; done; }
rshowoff() { rvm use 1.8.7; showoff $* ; rvm use default; }
alias yardserver="yard server -g -r -d -p8809"
rgrep() { ruby -ne 'puts $_ if $_ =~ /\$1/' $2; }
# rspec
specb() { spec ${1} | grep -v "#"; } # remove backtrace since there doesn't seem to be an option to do so .. wtf..
alias specs="spec --color --format specdoc " # show spec results with english summaries
#sass - compass watch
alias compassw='compass watch --app rails --sass-dir public/stylesheets/sass --css-dir public/stylesheets'
alias jekyllr='jekyll --pygments --safe --rdiscount'

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
alias scs='screen -Sx screen'
alias sccs='cd ~; screen -c /home/jon/.screenrcs/screenrc_screen -S screen'
alias scbg='cd ~; screen -c /home/jon/.screenrcs/screenrc_bg -S bg'
alias scqt='cd ~; screen -c /home/jon/.screenrcs/screenrc_qt -S qt'
alias scc='cd ~; screen -c /home/jon/.screenrcs/screenrc_coding -S code'

# dir shortcuts
alias configs='cd /home/jon/configs'
alias cd..='cd ..'
alias ..='cd ..'
msx=~/.mpd/music/
mmsx=~/.mpd_local/music/
fmsx=/media/frakssh/media/musix/
fmmsx=/media/frakssh/media/musix/musix/
alias msx="cd $msx"
alias mmsx="cd $mmsx"
alias fmsx="cd $fmsx"
alias fmmsx="cd $fmmsx"
kindle=/media/Kindle/DK_Documents//
# network dirs
alias newz='cd /media/media/filez/usenet/newzbin/'

## Coding

### Arch ###
#export PACMAN=pacman-color
export PACMAN=pacman
alias pi="sudo powerpill -S "
alias pag="sudo powerpill -S "
alias ag="sudo pacman -S "
alias s="pacman -Ss "
alias i="pacman -Si "
alias Q='pacman -Q | grep -i '
alias ql="pacman -Ql "
alias pq="pacman -Q|grep -i "
alias r="sudo pacman -R "
alias agu='sudo pacman -Syu; yaourt -Syu'
alias pagu="sudo powerpill -Syu"
alias psizes="LANG=C pacman -Qi | sed -n '/^Name[^:]*: \(.*\)/{s//\1 /;x};/^Installed[^:]*: \(.*\)/{s//\1/;H;x;s/\n//;p}' | sort -nk2"
alias redownload_all='for n in `pacman -Q`; do sudo pacman -Sw $n; done'

#packer
alias y='packer $1'
alias show='packer -Si $1'
alias yg="packer -G" #just fetch PKGBUILD
alias yu='packer -Syu --aur'
alias yac='packer -C' # autoclean
alias yacc='packer -Cc' # clean and remove all archived packageiiis
alias yaqt='packer -Qdt' # search for orphaned packages

# urxvt
alias fsize="smallprompt; printf '\33]50;%s%d\007' 'xft:Terminus:pixelsize='"

## incantations
alias vless='vim -u /usr/share/vim/vim73/macros/less.vim'


# ls
alias stark='sshfs -o reconnect -o allow_other jon@stark.legitscript.com:/home/jon/ /media/stark'



# Functions
#Usage: ii
RED='\e[1;31m'
BLUE='\e[1;34m'
CYAN='\e[1;36m'
NC='\e[0m'

function ii(){
    clear
    echo -e "\nYou are logged on ${RED}$HOSTNAME"
    echo -e "\nAdditional information:$NC " ; uname -a
    echo -e "\n${RED}Users logged on:$NC " ; w -h
    echo -e "\n${RED}Current date :$NC " ; date
    echo -e "\n${RED}Machine stats :$NC " ; uptime
    echo -e "\n${RED}Memory stats :$NC " ; free -m
    echo -e "\n${RED}Disk usage :$NC " ; df -lh
    echo -e "\n${RED}Local IP Address :$NC" ; /sbin/ifconfig eth0 | awk '/inet/
{ print $2 } ' | sed -e s/addr://
    echo -e "----------------------------------------------------------------------\n"
}

# roll - archive wrapper
# usage: roll <foo.tar.gz> ./foo ./bar
roll ()
{
  FILE=$1
  case $FILE in
    *.tar.bz2) shift && tar cjf $FILE $* ;;
    *.tar.gz) shift && tar czf $FILE $* ;;
    *.tgz) shift && tar czf $FILE $* ;;
    *.zip) shift && zip $FILE $* ;;
    *.rar) shift && rar $FILE $* ;;
  esac
}

function randlines () {
    cat ${1} | while read i; do echo $RANDOM "$i"; done | sort -n | sed 's/^[0-9]* //'
}

function f () { # find file/dir *1*
    find . -iname "*${1}*"
}

function fd () { # find directory *1*
    find . -type d -iname "*${1}*"
}

function fr () { # find random file *1*
    find . -iname "*${1}*" | randline
}

###   Locate and Grep! Let's call it lg 
lg () {
    locate $1 | grep $1
}

###   Calculator
calc () { 
    echo "$*" | bc -l; 
}

###   mkdir and cd directly
mkcd () { 
    mkdir $1 && cd $1 
}

# deal with rc.d status easily
start()
{
  for arg in $*; do
    sudo /etc/rc.d/$arg start
  done
}
restart()
{
  for arg in $*; do
    sudo /etc/rc.d/$arg restart
  done
}
stop()
{
  for arg in $*; do
    sudo /etc/rc.d/$arg stop
  done
} 

# nohup - run command detached from terminal and without output
# usage: nh <command>
nh()
{
  nohup "$@" &>/dev/null &
}

# define - fetch word defnition from google
# usage: define <word>
define ()
{
  lynx -dump "http://www.google.com/search?hl=en&q=define%3A+${1}&btnG=Google+Search" | grep -m 5 -w "*"  | sed 's/;/ -/g' | cut -d- -f5 > /tmp/templookup.txt
  if [[ -s  /tmp/templookup.txt ]] ;then    
    until ! read response
      do
      echo "${response}"
      done < /tmp/templookup.txt
    else
      echo "Sorry $USER, I can't find the term \"${1} \""                
  fi    
  rm -f /tmp/templookup.txt
}

# absbuild - quickly build and upgrade a pkg from ABS
# usage: absbuild <pkgname>
absbuild ()
{
  ABSPATH=`find /var/abs -type d -name $1`
  mkdir -p ~/.abs/$1
  cp -R $ABSPATH/* ~/.abs/$1
  cd ~/.abs/$1
  $EDITOR PKGBUILD
  makepkg -frs
  sudo pacman -U $1*i686.pkg.tar.gz
  cd -
}

# search the vim reference manual for a keyword
# usage: :h <keyword>
#:h() {  vim --cmd ":silent help $@" --cmd "only"; }
:h() { vim -c "help $1" -c "only"; } # seems to work a lot better than above (i.e. ':h dbext' doesn't work with the function, but does with alias..)

# mkmine - recursively change ownership to $USER:$USER
# usage:  mkmine, or
#         mkmine <filename | dirname>
function mkmine() { sudo chown -R ${USER}:users ${1:-.}; }

# sanitize - set file/directory owner and permissions to normal values (644/755)
# usage: sanitize <file>
sanitize()
{
  chmod -R u=rwX,go=rX "$@"
  chown -R ${USER}:users "$@"
}

# remind me, its important!
# usage: remindme <time> <text>
# e.g.: remindme 10m "omg, the pizza"
function remindme()
{
    sleep $1 && zenity --info --text "$2" &
}

# weather() -- Check weather
function weather () 
{ 
    lynx -dump "http://google.com/search?q=weather+${1:-97212}" | grep -A 5 -m 1 '^ *Weather for' | grep -v 'Add to'
}


# extract
extract () {
    if [ -f $1 ] ; then
       case $1 in
           *.tar.bz2) tar xjf $1    ;;
           *.tar.gz)  tar xzf $1    ;;
           *.bz2)     bunzip2 $1    ;;
           *.rar)     unrar x $1    ;;
           *.gz)      gunzip $1    ;;
           *.tar)     tar xf $1    ;;
           *.tbz2)    tar xjf $1    ;;
           *.tgz)     tar xzf $1    ;;
           *.zip)     unzip $1    ;;
           *.ZIP)     unzip $1    ;;
           *.Z)       uncompress $1;;
           *.7z)      7za e $1;;
           *)         echo "'$1' cannot be extracted via extract()" ;;
       esac
    else
        echo "'$1' is not a valid file"
    fi
}

# roll - archive wrapper
# usage: roll <foo.tar.gz> ./foo ./bar
roll ()
{
  FILE=$1
  case $FILE in
    *.tar.bz2) shift && tar cjf $FILE $* ;;
    *.tar.gz) shift && tar czf $FILE $* ;;
    *.tgz) shift && tar czf $FILE $* ;;
    *.zip) shift && zip $FILE $* ;;
    *.rar) shift && rar $FILE $* ;;
  esac
}



# initializers
## rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # This loads RVM into a shell session.

source ~/.zshrc_local
