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
alias fortune='echo && fortune taom && echo'
alias xephr='Xephyr -ac -br -noreset -screen 1152x720 :1 &'
alias test_awesome='DISPLAY=:1.0 awesome -c ~/.config/awesome/rc.lua'

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
#alias sup="rvm use 1.9.2-p180; sup"
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
alias mvis='ncmpcpp -c ~/.ncmpcpp/config_vis'
alias fixmpd="sudo chown -R jon.users /var/run/mpd*; sudo chown -R jon.users /var/log/mpd*; start mpd{,_test,_all}"

## Other
alias aunpackall='for n in *{rar,zip}; do aunpack $n; done'
alias aunpackallr='for n in *rar; do aunpack $n; done'
alias aunpackallz='for n in *zip; do aunpack $n; done'
alias rmspace="prename 's/ /_/g' *"
alias cm='chmod'
alias sv='sudo vim'
alias vrc='vim ~/.bashrc'
alias vlrc='vim ~/.bashrc_local'
alias vvc='vim ~/.vimrc'
alias vsc='vim ~/.config/subtle/subtle.rb'
alias vac='vim ~/.config/awesome/rc.lua'
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
alias reset='reset; vr'
alias fdays='find . -mtime '
alias loci='locate -i'
alias acka='ack -air '

## network
alias pgoo='ping -c2 google.com' 
alias wgetnc='wget --no-check-certificate'
### ssh
alias pb='ssh jonaustin@mrfantastic.dreamhost.com'
alias home_proxy='ssh -D 8089 -f -C -q -N jon@xs.homeunix.net'
alias work_proxy='ssh -D 8080 -f -C -q -N jon@barracuda-ext.cmdpdx.com'
alias work_rdc='ssh jon@barracuda-ext.cmdpdx.com -L 10000:jaustin.cmdpdx.com:3389' # tunnel rdc connection to localhost:10000
### synergy
alias syn='synergyc 192.168.0.9'
alias ssyn='ssh -f -N -L12345:10.10.10.155:24800 barracuda-ext.cmdpdx.com; synergyc localhost:12345'
alias ksyn="killall synergyc"
### non-frak
alias sxs='ssh jon@frak'
alias fxs='sftp jon@xs.homeunix.net'
alias sxxs='ssh jon@xs.homeunix.net'
alias sfs='ssh jon@frak'
alias sss='ssh jon@sam'
alias xsfs='sshfs -o reconnect jon@192.168.0.99:/ /media/xs'
alias xxsfs='sshfs -o reconnect jon@xs.homeunix.net:/ /media/xs'
alias fsfs='sudo umount /media/frakssh; sshfs -o reconnect -o allow_other jon@frak:/ /media/frakssh'
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
alias scs='tmux attach -t core'
alias sccs='cd ~; tmux new -s core'
#alias scbg='cd ~; screen -c /home/jon/.screenrcs/screenrc_bg -S bg'
#alias scqt='cd ~; screen -c /home/jon/.screenrcs/screenrc_qt -S qt'
#alias scc='cd ~; screen -c /home/jon/.screenrcs/screenrc_coding -S code'

# dir shortcuts
alias configs='cd /home/jon/configs'
alias cd..='cd ..'
alias ..='cd ..'
msx=~/.mpd/music/
mmsx=~/.mpd_local/music/
fmsx=/media/frakssh/media/MORGOTH/musix/
fmmsx=/media/frakssh/media/MORGOTH/musix/musix/
alias msx="cd $msx"
alias mmsx="cd $mmsx"
alias fmsx="cd $fmsx"
alias fmmsx="cd $fmmsx"
kindle=/media/Kindle/DK_Documents//
# network dirs
alias newz='cd /media/media/ext/filez/usenet/newzbin/'

## Coding

### Arch ###
#export PACMAN=pacman-color
export PACMAN=pacman
alias ag="sudo pacman -S "
alias as="pacman -Ss "
alias i="pacman -Si "
alias Q='pacman -Q | grep -i '
alias ql="pacman -Ql "
alias pq="pacman -Q|grep -i "
alias r="sudo pacman -R "
alias pac='pacman -c' # autoclean
alias agu='sudo pacman -Syu'
alias porphans='pacman -Qdt' # search for orphaned packages
alias psizes="LANG=C pacman -Qi | sed -n '/^Name[^:]*: \(.*\)/{s//\1 /;x};/^Installed[^:]*: \(.*\)/{s//\1/;H;x;s/\n//;p}' | sort -nk2"
alias redownload_all='for n in `pacman -Q`; do sudo pacman -Sw $n; done'

#packer
alias y='packer $1'
alias show='packer -Si $1'
alias yg="packer -G" #just fetch PKGBUILD
alias yu='packer -Syu --aur'

# urxvt
alias fsize="smallprompt; printf '\33]50;%s%d\007' 'xft:Terminus:pixelsize='"

## incantations
alias vless='vim -u /usr/share/vim/vim73/macros/less.vim'


# ls
alias stark='sshfs -o reconnect -o allow_other jon@stark.legitscript.com:/home/jon/ /media/stark'

# tmux
alias tma='tmux attach'
alias tmat='tmux attach -t'

# unorganized
alias bgbg='cat ~/.fehbg >> ~/fehbgs'
alias mem='sudo ps_mem'

alias teaosd='tosd 2.5m "============================\nYour Tea is Ready\n============================"'

# mounting
alias sdb1='sudo mount /dev/sdb1 /mnt'
alias sdc1='sudo mount /dev/sdc1 /mnt'
alias sdd1='sudo mount /dev/sdd1 /mnt'
alias umnt='sudo umount /mnt'
