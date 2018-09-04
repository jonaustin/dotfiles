#Usage: ii
RED='\e[1;31m'
BLUE='\e[1;34m'
CYAN='\e[1;36m'
NC='\e[0m'

ii()
{
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
zipc() { zip -r "${1}.zip" "$1"; }

randlines() {
  cat ${1} | while read i; do echo $RANDOM "$i"; done | sort -n | sed 's/^[0-9]* //'
}

fn () { # find file/dir *1*
  find . -iname "*${1}*"
}

#fd() { # find directory *1*
#  find . -type d -iname "*${1}*"
#}

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

# nohup - run command detached from terminal and without output
# usage: nh <command>
# note: or use `disown` after launching and bg'ing
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

# remind me, it's important!
# usage: remindme <time> <text>
# e.g.: remindme 10m "omg, the pizza"
remindme()
{
  minutes=$(echo "$1 * 60" | bc -l)
  sleep $minutes && zenity --info --text "$2" &
}

# weather() -- Check weather
weather()
{
  curl http://en.wttr.in/${1:-97203}
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

# fix excel-formatted csvs (utf-16 (*.txt)) to be utf-8 (*.csv)
fixcsv() {
  iconv -f utf-16 -t utf-8 $1 > $1.utf8.csv
  sed -i "s/_RETURN_/\n/g" $1.utf8.csv
}

scpc() {
  # extract first element (relative path or '.')
  rel_path=$1
  # remove relative path element
  shift
  # loop through filenames (escaping those with spaces) and send to remote
  for n in "${@}"; do
    scp_wild "$n" root@10.0.1.26:/Removable/MicroSD/Comics/${rel_path} 1984 2222
  done;
}

# sum CSV column
sum_col() {
  awk -F ',' '{ x = x + $4 } END { print x }' $1
}

# sum mint csv
sum_mint_csv() {
  awk -F ',' '{str=$4; gsub(/"/, "", str); x=x+str} END { print x }' $1
}

# view markdown files as formatted man pages
markdown-view() {
  pandoc -s -f markdown -t man $1 | groff -T utf8 -man | less
}

# toggle iTerm Dock icon
# add this to your .bash_profile or .zshrc
function toggleiTerm()
{
  pb='/usr/libexec/PlistBuddy'
  iTerm='/opt/homebrew-cask/Caskroom/iterm2/2.1.3/iTerm.app/Contents/Info.plist'

  echo "Do you wish to hide iTerm in Dock?"
  select ync in "Hide" "Show" "Cancel"; do
    case $ync in
      'Hide' )
        $pb -c "Add :LSUIElement bool true" $iTerm
        echo "relaunch iTerm to take effect"
        break
        ;;
      'Show' )
        $pb -c "Delete :LSUIElement" $iTerm
        echo "run killall 'iTerm' to exit, and then relaunch it"
        break
        ;;
      'Cancel' )
        break
        ;;
    esac
  done
}

function timer() {
  for n in `seq 0 $1`
  do
    echo "($n-$1)*-1" | bc -l
    sleep 1
  done
  espeak "$2"
  notify-send "$2"
}

# Codi - awesome vim repl
# Usage: codi [filetype] [filename]
codi() {
  local syntax="${1:-python}"
  shift
  vim -c \
    "let g:startify_disable_at_vimenter = 1 |\
    set bt=nofile ls=0 noru nonu nornu |\
    hi ColorColumn ctermbg=NONE |\
    hi VertSplit ctermbg=NONE |\
    hi NonText ctermfg=0 |\
    Codi $syntax" "$@"
}

# daily goals
#nlog() {
#  DIR=$HOME/notes/drugs/nootropics/log
#  FILE=$DIR/$(date -I).md
#  mkdir -p $DIR
#  if [ -e $FILE ]; then
#    $EDITOR $FILE
#  else
#    cp $DIR/template.md $FILE
#    $EDITOR $FILE
#  fi;
#}

today() {
  DIR=$HOME/notes/daily_goals
  FILE=$DIR/$(date -I).md
  mkdir -p $DIR
  if [ -e $FILE ]; then
    $EDITOR $FILE
  else
    cp $DIR/template.md $FILE
    $EDITOR $FILE
  fi;
}


## rsync
rsynconly() {
  rsync -amP --include="*/" --include="*.$1" --exclude="*" $2 $3
}

## watch files
watch-reload() {
  find . -iname $1 | entr $2
}

# notes
enote() { $EDITOR -p `fd "$1*" $HOME/notes` }
note() { less `fd "$1*" $HOME/notes` }
