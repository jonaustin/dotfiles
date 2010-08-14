# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.

# note: this is usually autodetected -- but IS needed for rsync, and probably some other utils (scp,rcp...?)
#if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
#	return
#fi




## ENV
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/scripts:/usr/local/bin:/usr/X11R6/bin:/home/jon/bin:/usr/local/:/usr/lib/firefox/:/opt/kde/bin:/usr/lib/surfraw:/opt/java/jre/bin/:/opt/android-sdk/tools:/home/jon/bin/ruby"
export EDITOR='vim'
export BROWSER='elinks'
export PAGER='less'
export LESS='--RAW-CONTROL-CHARS --squeeze-blank-lines --ignore-case --quit-on-intr' # --LINE-NUMBERS --quit-if-one-screen'
export DISPLAY=:0
export HISTCONTROL=ignoredups # don't put duplicate lines in the history. See bash(1) for more options
export TERM=xterm-256color    # no idea why I didn't add this before

export HISTFILESIZE=10000
export HISTSIZE=10000
export OOO_FORCE_DESKTOP=gnome
export INPUTRC=~/.inputrc
#export HTTP_PROXY=http://127.0.0.1:8118
#export http_proxy=http://127.0.0.1:8118
export OPCODEDIR64=/lib/csound/plugins64  # for csound5

export SURFRAW_graphical_browser="vimprobable"
export SURFRAW_browser="vimprobable"

### woohoo! use for paths -- i.e. cd ${msx}/Mew -- even works with Tab completion!
export msx='/media/MORGOTH/musix/musix/'


#export RAILS_GEM_VERSION=2.3.5

# umask orig = 022
umask 002 #create with groups privs by default (ie chmod 775) ...yes, be wary...

ulimit -S -c 0        # Don't want any coredumps

set -o vi
set -o notify         # Tell me about background jobs right away
#set -x # enable shell script tracing

# disable ^S/^Q flow control 
stty -ixon


#bash completion. 
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
    #other bash completion
    complete -cf sudo
    complete -cf man
fi

#####

# shell options
shopt -s cdspell        # Correct cd typos
shopt -s checkwinsize   # Update $LINES and $COLUMNS after resizes
shopt -s histappend     # Enable history appending instead of overwriting.  #139609
#shopt -s expand aliases
#shopt -s checkhash
#shopt -s sourcepath
#shopt -s no_empty_command_completion
#shopt -s extglob    # necessary for programmable completion
#shopt -s progcomp    # Programmable completion is FUN
shopt -u mailwarn
unset MAILCHECK        # I dont want my shell to warn me of incoming mail
export MAIL=/var/spool/mail/jon


#silence the beep
setterm -blength 0
#xset -b

# BASH COLOR CODES (for reference)
#Black       0;30     Dark Gray     1;30
#Blue        0;34     Light Blue    1;34
#Green       0;32     Light Green   1;32
#Cyan        0;36     Light Cyan    1;36
#Red         0;31     Light Red     1;31
#Purple      0;35     Light Purple  1;35
#Brown       0;33     Yellow        1;33
#Light Gray  0;37     White         1;37
BLACK='\e[0;30m'
BLUE='\e[0;34m'
GREEN='\e[0;32m'
CYAN='\e[0;36m'
RED='\e[0;31m'
PURPLE='\e[0;35m'
BROWN='\e[0;33m'
LIGHTGRAY='\e[0;37m'
DARKGRAY='\e[1;30m'
LIGHTBLUE='\e[1;34m'
LIGHTGREEN='\e[1;32m'
LIGHTCYAN='\e[1;36m'
LIGHTRED='\e[1;31m'
LIGHTPURPLE='\e[1;35m'
YELLOW='\e[1;33m'
WHITE='\e[1;37m'
NC='\e[0m'
#
#PS1=$'%{\e[1;32m%}%n%{\e[0m%} %{\e[1;37m%}%~ %% '
#PS1='\u[\W]$'
#PS1='[\u@\h \W]\$ '
#PS1='\u@\h:\w\$ '
#PS1='\[\e[0;32m\]\u\[\e[m\] \[\e[1;34m\]\w\[\e[m\] \[\e[m\] \[\e[1;32m\]\$ \[\e[m\]\[\e[1;37m\] '
#PS1='[\[\e[37m\]\u\[\e[31m\]@\[\e[37m\]\h\[\e[0m\]:\[\e[33m\]\w\[\e[0m\]] \$ '
if [ `/usr/bin/whoami` = 'root' ]; then
  PS1='\[\e[0;32m\]\u \[\e[1;34m\]\w \[\e[1;32m\]\$ \[\e[1;37m\] '
else
  #PS1='\A/$? \[\033[01;32m\][\u@\h] \[\033[01;34m\]\w \$ \[\033[00m\]'
  export PS1='\h \A [\W]$ '
fi

####
#Set the screen title
# This is the escape sequence ESC k \w ESC \
#Use path as titel
#SCREENTITLE='\[\ek\w\e\\\]'
#Use program name as titel
SCREENTITLEPROGRAM='\[\ek\e\\\]'
PS1="${SCREENTITLEPROGRAM}${SCREENTITLE}${PS1}"
 
# Colourful manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'                           
export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'


## LOGIN
# echo fortune from all (offensive and not) fortunes, show cookie file, consider all fortunes equal regardless of size -- only short ones
#echo && fortune -aces && echo
#echo && fortune -ac && echo

echo && fortune -e tao && echo # taow



## SHORTCUTS
#sane defaults
alias vi='vim'
alias mkdir='mkdir -p'
alias grep="grep --color"
alias nautilus="nautilus --no-desktop --browser"
alias screen='TERM=xterm-256color screen -T $TERM' 
alias wget='wget -c' #auto continue files
alias df='df -Th'
alias free='free -m'
alias info='pinfo'

#ask before overwriting
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'


#ls
alias l='ls -CF'
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


#convenience
alias mr='sudo /etc/rc.d/mpd restart'
alias mrm='sudo /etc/rc.d/mpd restart && ncmpcpp'
alias mra='sudo /etc/rc.d/mpd_all restart'
alias m='ncmpcpp'
alias mnoo='ncmpcpp -p 6601'
alias pnoo='pms -p 6601'
alias ma='ncmpcpp -p 6602'
alias pall='pms -p 6602'
alias mva='ncmpcpp -p 6603'
alias pva='pms -p 6603'
alias mc='cmus /media/musix/'
alias vol="alsamixer"
alias am="alsamixer"
alias sv='sudo vim'
alias vrc='vim ~/.bashrc'
alias v='vr'
alias vvc='vim ~/.vimrc'
alias c='clear'
alias e='exit; clear'
alias svi='sudo vim'
alias t='todo.sh'
alias enc='h264enc -2p -p hq'
alias h2='h264enc -2p -p hq'
alias S='sudo '
alias t='top'
alias ht='htop'
alias tw='twidge update '
alias tu='twidge update '
#alias irb='irb --readline -r irb/completion'
alias fortune='echo && fortune tao && echo'
#alias hv='cd ~/ && vi $1 && cd -'   #this doesnt work
alias mtr='mtr --curses '
alias sabn='sudo /etc/rc.d/sabnzbd start'
alias droidbackup='rsync -avz /media/usb/ /media/MORGOTH/documents/backup/droid/'
alias droidmount='sudo mount /dev/disk/by-uuid/1054-C987 /media/usb; cd /media/usb'
alias sfdroid='sshfs root@192.168.0.6:/ /media/droid'
alias stdroid='ssh root@localhost -p2222'
alias ri='ri -T' # don't use a pager
alias xevgrep="xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'"
alias x2='xinit ~/.xinitrc2 -- :1'
alias acroread='acroread -openInNewWindow'
alias pgoo='ping -c5 google.com' 
alias sabn='su - sabnzbd -c python /opt/sabnzbd/SABnzbd.py -f /opt/sabnzbd/sabnzbd.ini -s 192.168.0.166:8081 -d -s /bin/sh'
alias h='head ' 
fs() { find $1 -iname '*'$2'*'; }
alias jack='cd ~/Jack/'
alias sand='cd ~/Jack/sandbox'
alias nyxd='cd ~/Jack/sandbox/nyx'
alias lsg='ls *|grep -i '
alias vbl='vim -c BlogList'
alias games='cd /media/ext3/Warez/Games/'
export games='/media/ext3/Warez/Games/'
alias lsgames='echo Civ4; echo Quake4; echo Portal; echo Warhammer; echo Freedom Force;'

### ruby
alias fri='fri -L' # ruby fastri -- always bind to local as net stuff doesn't work
alias create_ri_rdoc_for_gems='gem rdoc --all --overwrite'
alias ris='ri --system '
alias rri='/usr/bin/ri -T'  # god this is fucking retarded... rvm's 1.8.7 `gem rdoc ...' only install *.ri versions of ri docs, instead of the *.yaml versions that are needed (AFAIK) because 1.8.7's `ri` version is 1.0.1 whereas 1.9.1's version is 2.3.5 ... fucking jesus fucking christ.
#alias ri='/usr/bin/ri -T'  # fuck it - the 1.8.x ri`s are just useless
alias wr='which ruby'
alias gems='gem search -r '
alias gspec='gem spec -r '
alias gdep='gem dep -r '
alias cdgem='cd /home/jon/.rvm/gems/ruby-1.8.6-p399/gems/'
alias redmine='cd /media/MORGOTH/documents/code/ruby/rails/redmine/ && sh server.sh'
alias qlg='gem contents '
alias glq='gem contents '
alias rtags='rtags -vi' # vi compatible rtags (default is emacs)
alias gwhois='gem whois '
gswhois() { for n in `gems $1|cut -f1 -d' '`; do gem whois $n; done; }

### awesome
alias vac='vim ~/.config/awesome/rc.lua'

# media conversion
alias wma2ogg='for i in *.wma; do ffmpeg -i $i -acodec vorbis -aq 100 ${i}.ogg; done'
alias ogv2avi='for n in `ls *`; do mencoder $n -ovc lavc -oac mp3lame -o $(echo $n | cut -d "." -f 1).avi; done'
alias ogv2mp4="mencoder out.ogg -of lavf -lavfopts format=mp4 -oac mp3lame -lameopts cbr:br=128 -ovc x264 -x264encopts bitrate=1000 -o final.mp4"
alias ogv23gp='for n in `ls *.ogv`; do sudo ffmpeg -i $n -r 15 -b 64kb -ac 1 -s 176x132 -padtop 6 -padbottom 6 -ar 16000 -ab 32kb -acodec libfaac -vcodec h263 $(echo $n | cut -d "." -f 1).3gp; done'

# terminal window
alias vr='for n in 1 2 3 4  5 6 7 8 9 0; do for n in 1 2 3 4 5 6 7 8 9; do echo; done; done'

### screen
alias sc='screen -dr'
alias scs='screen -Sx screen'
alias sccs='screen -c /home/jon/.screenrcs/screen -S screen'
alias scsr='screen -c /home/jon/.screenrcs/rivo -S rivo'
alias sccr='screen -Sx rivo'
alias scc='screen -c /home/jon/.screenrcs/coding -S code'
alias scx='screen -Sx '

# dir shortcuts
alias tv='cd /media/ext3/tv/'
alias mov='cd /media/ext/Movies'
alias books='cd /media/ext/books/'
alias noo='cd /media/ext/nootropic/'
alias tor='cd /media/filez/torrents/'
alias docs='cd /media/MORGOTH/documents'
alias code='cd /media/MORGOTH/documents/code'
alias codeo='cd /media/MORGOTH/documents/code_others'
alias rdir='cd /media/MORGOTH/documents/code/ruby'
alias d='cd ~/documents/'
alias simpsons="cd /media/extbak/tv/Simpsons/"
alias home='cd /home/jon'
alias nyx='cd ~/Jack/sandbox/nyx'
alias fixtape='cd ~/documents/code/ruby/rails/musix'
#alias frm='find . -iname "*$1*" -print0 |xargs -0 rm' #egh put in function, otherwise with no arg deletes everything! 
alias cd..='cd ..'
alias ..='cd ..'
alias newz='cd /media/filez/usenet/newzbin'
alias msx='cd /media/MORGOTH/musix'
alias mmsx='cd /media/MORGOTH/musix/musix'
h='/home/jon/'

# syncing
alias async='sudo rsync -avz /media/ext/books_epub/ /media/usb/eBooks/;  sudo rsync -avz /media/ext/books_epub/ /media/usb/Books/; ' 

#temporary


#function sx () {
#  startx /usr/bin/${1} -- :1
#}

function smallprompt () {
  export PS1='\A [\W]$ '
}

function randlines () {
    cat ${1} | while read i; do echo $RANDOM "$i"; done | sort -n | sed 's/^[0-9]* //'
}

function f () { # find file *1*
    find . -iname "*${1}*"
}

function fr () { # find random file *1*
    find . -iname "*${1}*" | randline
}

## CODING (FIXME: move all coding stuff under here)
# code dirs #
alias rivo='cd /media/MORGOTH/documents/code/ruby/rails/tv_updater/railer'
# shortcuts #
alias rvd='vim --servername VIM $1'; # ruby vim debug -- then :Rdebugger, then connect to xxx:3000, then go to it

##configs
alias rc='source ~/.bashrc'

##network
#ssh
alias smb="ssh jon@192.168.0.123"
alias smini="ssh 192.168.0.199"
alias b="ssh jon@barracuda-ext.cmdpdx.com"
alias pb='ssh jonaustin@mrfantastic.dreamhost.com'
alias n8='ssh root@192.168.0.232'
alias work_proxy='ssh -D 8080 -f -C -q -N jon@barracuda-ext.cmdpdx.com'
alias work_rdc='ssh jon@barracuda-ext.cmdpdx.com -L 10000:jaustin.cmdpdx.com:3389' # tunnel rdc connection to localhost:10000

alias syn='synergyc 192.168.0.9'
alias ssyn='ssh -f -N -L12345:10.10.10.155:24800 barracuda-ext.cmdpdx.com; synergyc localhost:12345'
alias ksyn="killall synergyc"

##apps
alias nd="ncdu ."
alias rex='rexima' #vi style vol control

##sys
alias u='uptime'
alias udb='sudo updatedb'
alias killspkr='sudo modprobe -r pcspkr'

### shortcut-TRICKS
alias igrep='grep -i'
alias pg='ps aux | grep -v grep | grep -i '
alias g="grep -ir"
alias hist='history | grep -i '

#NETWORK TOOLS

                            


### INCANTATIONS ###
alias burn='growisofs -Z /dev/sr0 -v -l -R -J -joliet-long' 
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""' # get xprop CLASS and NAME
alias fixres="xrandr --size 1920x1200"    # reset resolution
alias getip='wget http://checkip.dyndns.org/ -O - -o /dev/null | cut -d: -f 2 | cut -d\< -f 1'
alias psm="echo '%CPU %MEM   PID COMMAND' && ps hgaxo %cpu,%mem,pid,comm | sort -nrk1 | head -n 5 | sed -e 's/-bin//'" #get top cpu eating processess 

alias dvd="sudo mount /dev/sr0 /media/dvd; cd /media/dvd"
alias dvd2="sudo mount /dev/sr1 /media/dvd; cd /media/dvd"
alias ejectdvd="sudo eject /media/dvd"
alias udvd="sudo eject /media/dvd"
alias movie="vlc -f $1"
#alias v='vlc -f $1'

alias tx='tar zxvf'
alias tj='tar jxvf'

alias eterm='Eterm -t trans&'
alias eterm="Eterm -t smoked_glass -f green"
alias aterm="aterm -name aterm -title 'term' -sl 3000 -tr +sb -sr -si -sk -bg black -shading 60 -fade 90 -tn xterm -fn -misc-fixed-medium-r-normal-*-*-120-*-*-c-*-iso8859-2 -g 130x50+20-0 -fg \#cecece"
# alias aterm='aterm -tr -trsb -cr red +sb -fg gray -fn fixed -fb fixed'
alias sudo='A=`alias` sudo '

alias cdbusy='fuser -muv /cdrom/' #sweet! shows which process is keeping the damn cdrom busy - also see lsof - list open files

#svn
alias svnupdry='svn merge --dry-run -r BASE:HEAD .'
alias ssv='svn status|grep -v .svn|grep -v cache|grep -v tmp'
alias ssvm='svn status|grep -v .svn|grep -v cache|grep -v tmp | grep "^M"'
alias ssva='svn status|grep -v .svn|grep -v cache|grep -v tmp| grep "^A"'

#size
alias dirsizes='du -sh $(find $1 -xdev -maxdepth 1 -type d) | sort -g'
alias dhog='du -cks * |sort -rn |head -11' # disk hog

alias backup-config='sh ~/bin/backup-config.sh'
alias backup-config-usb='rm -rf /tmp/backup_config && mkdir /tmp/backup_config && cp /home/jon/.screenrc /tmp/backup_config && cp /home/jon/.fonts.conf /tmp/backup_config && cp -rf /home/jon/.elinks /tmp/backup_config && cp /etc/rc.conf /tmp/backup_config && cp /etc/pacman.conf /tmp/backup_config && cp /home/jon/.bashrc /tmp/backup_config/ && cp /etc/X11/xorg.conf /tmp/backup_config && cp -rf /home/jon/.vim* /tmp/backup_config/ && cp /home/jon/.pinerc /tmp/backup_config/ && cp -rf ~/.config /tmp/backup_config/ && cp -rf /home/jon/.mozilla /tmp/backup_config && cp -rf /home/jon/.cmus /tmp/backup_config && cp -rf /home/jon/.dvdrip /tmp/backup_config && cp -rf /home/jon/.fluxbox /tmp/backup_config && cp -rf /home/jon/.gkrellm2 /tmp/backup_config && cp -rf /home/jon/.irssi /tmp/backup_config && cp -rf /var/lib/mpd /tmp/backup_config && cp -rf /home/jon/.mpd /tmp/backup_config && cp -rf /home/jon/.naimlog /tmp/backup_config && cp -rf /home/jon/.ncmpcpp /tmp/backup_config && cp -rf /home/jon/.sabnzbd /tmp/backup_config && cp -rf /home/jon/.ssh /tmp/backup_config && cp -rf /home/jon/.thunderbird /tmp/backup_config && cp -rf /home/jon/.vifm /tmp/backup_config && cp -rf /home/jon/.weechat /tmp/backup_config && cp -rf /home/jon/.xchat2 /tmp/backup_config && cp -rf /home/jon/bin /tmp/backup_config && cp -rf /home/jon/.inputrc /tmp/backup_config && cp -rfu /home/jon/.X* /tmp/backup_config && cp -rfu /home/jon/.x* /tmp/backup_config && rsync --delete --exclude "Cache" -avzu /tmp/backup_config/ /media/automount/usb3/linux/backup/backup_config && rm -rf /tmp/backup_config'

alias org='cal -3mm && when'
alias dup='echo `date` -------- $1 >> /home/jon/documents/d_cal.txt'
alias dupc='echo >> /home/jon/documents/d_cal.txt'
alias dsee='cat /home/jon/documents/d_cal.txt'
alias de='vi /home/jon/documents/d_cal.txt'

alias rup='echo >> /home/jon/documents/resume_new.txt'
alias re='vim /home/jon/documents/resume_new.txt'
alias rsee='cat /home/jon/documents/resume_new.txt'

### GTD ###
alias gcal=' gcalcli; ' 
alias gcala='gcalcli agenda; ' 
### weird, had to add 'function' keyword or got syntax error
function gcadd(){ gcalcli quick; }  # gcadd "10/31 7 pm Halloween Party"
function gcq() { rvm use system; gcalcli quick $1; rvm use default; }  # gcadd "10/31 7 pm Halloween Party"
function gcla() { rvm use system; google calendar add $1; rvm use default; }  #$ google calendar add "Social Media Day SF Party at 7pm"{ 
function gcll() { rvm use system; google calendar list $1; rvm use default; } 
set_task() { echo "  * " $1 "  " > /home/jon/.cur_task; }
add_task() { echo "  * " $1 "  " >> /home/jon/.cur_task; }
alias vct='vim ~/.cur_task'
  

### HACKS ###
LC_CTYPE=en_US.UTF-8;      export LC_CTYPE
LANG=en_US.UTF-8;          export LANG
LC_ALL=en_US.UTF-8;        export LC_ALL




### Some one-liners
# for testing if we have a command - i.e. `have less`
have() { type "$1" &> /dev/null; }
###
rgrep() { ruby -ne 'puts $_ if $_ =~ /\$1/' $2; }


### FUNCTIONS ###
cmdfu(){ curl "http://www.commandlinefu.com/commands/matching/$@/$(echo -n $@ | openssl base64)/plaintext"; }

create_index(){ 
  rm index.html;
  for n in *; 
  do 
    echo $n;
    n="<a href=${n}>${n}</a>\n<br/>"; 
    echo $n >> index.html; 
  done
}


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

# mktar - tarball wrapper
# usage: mktar <filename | dirname>
function mktar() { tar czf "${1%%/}.tar.gz" "${1%%/}/"; }


###   Easy file/dir chown
grab () {
        sudo chown -R ${USER} ${1:-.}
}

###   Locate and Grep! Let's call it lg 
lg () {
    locate $1 | grep $1
}

###   Calculator
calc () { 
    echo "$*" | bc -l; 
}

###   FuseISO mount
iso () {
    echo ":::   FuseISO mounting $1 in $HOME/$1"
    fuseiso -p $1 ~/$1 -f
    echo ":::   Done"
}

###   Burn ISO to CD/DVD
iso2cd () { 
    wodim -v -eject speed=16 driveropts=burnfree dev=/dev/cdrom0 $1
}
iso2dvd () {
    growisofs -dvd-compat -speed=4 -Z /dev/cdrom0=$1
} 

###   Play DVD
playdvd () {
    mplayer -dvd-device $1 dvd://$2
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

# simple notes
note ()
{
        #if file doesn't exist, create it
        [ -f $HOME/.notes ] || touch $HOME/.notes
 
        #no arguments, print file
        if [ $# = 0 ]
        then
                cat $HOME/.notes
        #clear file
        elif [ $1 = -c ]
        then
                > $HOME/.notes
        #add all arguments to file
        else
                echo "$@" >> $HOME/.notes
        fi
}

# s - gnu screen function
# usage: lists screen sessions, otherwise
#        s <name> reattaches to <name>, otherwise
#        s <name> creates a new session <name>.
scr ()
{
  if [[ $1 ]]; then
    screen -Sx $1
  else
    screen -ls
  fi
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
alias :h='vim -c help ' # seems to work a lot better than above (i.e. ':h dbext' doesn't work with the function, but does with alias..)

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

function smartprompt
{
#   How many characters of the $PWD should be kept
    local pwdmaxlen=30
#   Indicator that there has been directory truncation:
    local trunc_symbol=".."
    newPWD=$PWD
    #newPWD=${newPWD/$HOME/\~}

    if [ ${#newPWD} -gt $pwdmaxlen ]
    then
        local pwdoffset=$(( ${#PWD} - $pwdmaxlen ))
        newPWD="${trunc_symbol}${newPWD:$pwdoffset:$pwdmaxlen}"
    fi
}

### VARIOUS CONFIG ###







### CODING
alias def="grep -ir def $1"
alias fun="grep -i function $1"

function ctag(){
    cd $1
    ctags -V -f ~/.vim/tags/$2 \
    -h ".php" -R \
    --exclude="\.svn" \
    --totals=yes \
    --tag-relative=yes \
    --PHP-kinds=+cf \
    --regex-PHP='/abstract class ([^ ]*)/\1/c/' \
    --regex-PHP='/interface ([^ ]*)/\1/c/' \
    --regex-PHP='/(public |static |abstract |protected |private )+function ([^ (]*)/\2/f/'
}


                            


### Debian ###
#apt
alias ac='apt-cache $1'
alias ag='sudo apt-get install $1'
alias agu='sudo apt-get update; sudo apt-get upgrade'
alias show='apt-cache show $1'
alias a='aptitude'
alias s='apt-cache search'
alias ac='apt-cache'
alias apt-top='grep-status -FStatus -sInstalled-Size,Package -n             "install ok installed" | paste -sd "  \n" | sort -n'

### Arch ###
#pacman
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

#yaourt
alias y='yaourt $1'
alias show='yaourt -Si $1'
alias yg="yaourt -G" #just fetch PKGBUILD
alias yu='yaourt -Syu --aur'
alias yac='yaourt -C' # autoclean
alias yacc='yaourt -Cc' # clean and remove all archived packageiiis
alias yaqt='yaourt -Qdt' # search for orphaned packages

# urxvt
alias fsize="smallprompt; printf '\33]50;%s%d\007' 'xft:Terminus:pixelsize='"

### OSX ###
#alias updatedb='sudo /usr/libexec/locate.updatedb'



alias sxs='ssh -p666 jon@xs.homeunix.net'
alias fxs='sftp -oPort=666 jon@xs.homeunix.net'
alias sfxs='sftp -oPort=666 jon@xs.homeunix.net'

# not sure if this works, but should:
# allow specific commands to be run at X startup
rl=$(runlevel | grep -o [0-9])
case $rl in
    4) TERM=screen; exec /usr/bin/screen;;
esac

### Wine Games ###
alias civ="cd /home/jon/.wine/drive_c/Program\ Files/2K\ Games/Firaxis\ Games/Sid\ Meier\'s\ Civilization\ 4\ Complete/; wine Civilization4.exe";
alias heroes5='cd "/home/jon/.PlayOnLinux/wineprefix/HOMM5/drive_c/Program Files/Ubisoft/Heroes of Might and Magic V/bin"; WINEPREFIX=/home/jon/.PlayOnLinux/wineprefix/HOMM5 wine H5_Game.exe'
alias cdheroes5='cd "/home/jon/.PlayOnLinux/wineprefix/HOMM5/drive_c/Program Files/Ubisoft/Heroes of Might and Magic V/bin";'
alias fallout='cd "/home/jon/.wine/drive_c/Program Files/Interplay/Fallout 1.3.4"; wine Falloutw.exe'
alias fallout2='cd "/home/jon/.wine/drive_c/Program Files/Interplay/Fallout 2 1.02.25"; wine FALLOUT2.EXE'
alias fallouttactics='cd "/home/jon/.wine/drive_c/Program Files/Interplay/Fallout Tactics"; wine BOS.EXE'
alias ra3='cd "/home/jon/.PlayOnLinux/wineprefix/RA3U/drive_c/Program Files/Electronic Arts/Red Alert 3"; WINEPREFIX=/home/jon/.PlayOnLinux/wineprefix/RA3U/ wine RA3.exe'

