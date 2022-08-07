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
calc() { echo "$*" | bc -l; }

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
gdefine()
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
  COL=$2
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
vf() {
  $EDITOR -p `fd ".*$1.*" ${2:-.}`
}
enote() {
  $EDITOR -p `fd "$1*" $HOME/notes`
}
note() {
  $PAGER `fd "$1*" $HOME/notes`
}

# bluetooth
btstr() {
  cat << EOD
  [bluetooth]# power on
  [bluetooth]# agent on
  [bluetooth]# default-agent
  [bluetooth]# scan on
  [bluetooth]# connect  28:11:A5:48:19:A5
EOD
}

# diffstr "9f770789df3b7803105e5fbc19212889674cd503" $(sha1sum strap.sh|awk '{print $1}')
diffstr() {
  diff <(echo $1) <(echo $2)
}

# proxy through jump host
setup-proxy() {
  local_port=$1
  target_host=$2
  remote_port=$3
  jump_host=$4
  # e.g. ssh -v -N -L 54321:puppetdb-jon.cq4yztl3y2cz.us-east-1.rds.amazonaws.com:5432 jon@10.11.1.97
  # setup-proxy 54321 puppetdb-jon.cq4yztl3y2cz.us-east-1.rds.amazonaws.com 5432 jon@10.11.1.97
  ssh -v -N -L ${local_port}:${target_host}:${remote_port} $jump_host
}

walbg-info() {
  export curbg=$(cat ~/.fehbg|tail -n1|cut -d"'" -f2)
  file $curbg
  identify $curbg
  mediainfo $curbg
}

zsh-available-completions() {
  for command completion in ${(kv)_comps:#-*(-|-,*)}
  do
    printf "%-32s %s\n" $command $completion
  done | sort
}

# incantations
sed-recurse() {
  grep -rl $1 . | parallel sed -i "s/$1/$2/g" {}
}

# fzf
# checkout git branch
# https://junegunn.kr/2015/03/fzf-tmux/
gcob() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf-tmux -d 15 +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

fcd() {
  local dir;   
  while true; do
    # exit with ^D
    dir="$(/bin/ls -a1p | grep '/$' | grep -v '^./$' | fzf --height 40% --reverse --no-multi --preview 'pwd' --preview-window=up,1,border-none --no-info)"
    if [[ -z "${dir}" ]]; then
      break
    else
      cd "${dir}"
    fi
  done
}

sysz() {
  # or just use fzf's built in completion e.g. systemctl status <tab>
  FZF_DEFAULT_OPTS_OLD=$FZF_DEFAULT_OPTS
  export FZF_DEFAULT_OPTS=$(echo $FZF_DEFAULT_OPTS | sed 's/right:hidden/right/')
  /usr/bin/sysz
  export FZF_DEFAULT_OPTS=$(echo $FZF_DEFAULT_OPTS | sed 's/right:wrap/right:hidden:wrap/')
}

cpfd() {
  # cp "`fd sleep |fzf`" ../sync/ereader/health
  cp "$(fd $1 | fzf )" $2
}

rga-fzf() {
  RG_PREFIX="~/.cargo/bin/rga --files-with-matches"
  local file
  file="$(FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
    fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
    --phony -q "$1" \
    --bind "change:reload:$RG_PREFIX {q}" \
    --preview-window="70%:wrap"
    )" &&
    echo "opening $file" &&
    xdg-open "$file"
}

# Note: Arch version 0.9.6 has a bug where poppler breaks: https://github.com/phiresky/ripgrep-all/issues/113
# Whereas `master` version was refactored i guess and no longer includes Page Numbers
#  so hack it to use master for search, but 0.9.6 for display: https://github.com/phiresky/ripgrep-all/issues/113#issuecomment-1167861526
rga-fzf-pdf() {
  RG_PREFIX="~/.cargo/bin/rga --files-with-matches --rga-adapters=poppler"
  local file
  file="$(FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
    fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
    --phony -q "$1" \
    --bind "change:reload:$RG_PREFIX {q}" \
    --preview-window="70%:wrap"
    )" &&
    echo "opening $file" &&
    xdg-open "$file"
}

# git
git-diff-dir-names() {
  git diff --name-only $1 | awk -F "/*[^/]*/*$" '{ print ($1 == "" ? "." : $1); }' | sort | uniq
}

# copy changed within x time
# cpch 30m ~/dl/ `pwd`/books/
cpch() {
  # handle spaces
  OIFS="$IFS"
  IFS=$'\n'

  within=${1:-15m}
  wherefrom=${2:-$HOME/downloads/}
  whereto=${3:-$HOME/sync/ereader/}

  for file in `fd -t f --changed-within $within . "$wherefrom"`; do
    if [[ -z "${DRY}" ]]; then
      rsync -aP "$file" "${whereto}/"
    else
      echo "$file" "${whereto}/"
    fi
  done

  IFS="$OIFS"
}

curljson() {
  curl $1 \
    --header "Content-Type: application/json" \
    --request "${2:-GET}"
}

bluetooth_init() {
  bluetoothctl power on
  bluetoothctl agent on
  bluetoothctl default-agent
  bluetoothctl scan on
  #bluetoothctl pair $1
  #bluetoothctl trust $1
  bluetoothctl connect $1
}

# Git
## find all branches that have a particular file (note: this may only find branches that _change_ a particular, haven't tested)
git-find-file-in-branches() {
  git log --all --source -- "**/$1"|grep commit|awk '{print $3}'|sort -u
}

# conversion
html2pdf() {
  pandoc --pdf-engine=prince $1 -o $2
}

## dns toys
dy() { 
  dig +noall +answer +additional "$1" @dns.toys; 
}
