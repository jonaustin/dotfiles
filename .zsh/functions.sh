#Usage: ii
RED='\e[1;31m'
BLUE='\e[1;34m'
CYAN='\e[1;36m'
NC='\e[0m'

ii(){
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
roll()
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
zipc() {'zip -r ${1}.zip $1'}

randlines() {
  cat ${1} | while read i; do echo $RANDOM "$i"; done | sort -n | sed 's/^[0-9]* //'
}

fn () { # find file/dir *1*
    find . -iname "*${1}*"
}

fd() { # find directory *1*
    find . -type d -iname "*${1}*"
}

fr() { # find random file *1*
    find . -iname "*${1}*" | randline
}

fsz() { # find files larger than <size>mb
    find . -size +${1}
}

### Locate and Grep! Let's call it lg 
lg() {
    locate $1 | grep $1
}

### Calculator
calc() { 
    echo "$*" | bc -l; 
}

### mkdir and cd directly
mkcd() { 
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
define()
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
absbuild()
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
mkmine() { sudo chown -R ${USER}:users ${1:-.}; }

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
remindme()
{
    sleep $1 && zenity --info --text "$2" &
}

# weather() -- Check weather
weather() 
{ 
    lynx -dump "http://google.com/search?q=weather+${1:-97212}" | grep -A 5 -m 1 '^ *Weather for' | grep -v 'Add to'
}


# extract
extract() {
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
roll()
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

# simple centered notification
osd_cat2() {
  echo -e "$1" | osd_cat -f '-*-terminus-*-*-*-*-24-*-*-*-*-*-*-*' -p middle -A center;
}

# timed (minute) notification
tosd() {
  sleep $1; osd_cat2 $2;
}
