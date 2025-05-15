# things I forget
# \ls -- ignore ls alias override

## SHORTCUTS

# overrides
alias emacs='TERM=xterm-24bit emacs -nw'
alias db=dotbare
alias dbs='dotbare status'
alias cat=bat
alias catnp='bat --paging never'
alias ls=eza
alias ll='eza -l'
alias lsl='ls -l'
#alias calibre='asdf shell python system && calibre'
alias fd='fd -I' # don't ignore things in gitignore
alias locate='plocate'
alias tg='terragrunt'
alias b2='/usr/local/bin/b2' # backblaze
alias free='free -m'
alias ranger=lf

# typos
alias cd..='cd ..'

# general convenience
alias td='echo $(date +%Y-%m-%d)'
alias now='echo $(date +%Y-%m-%d\ %T)'
alias ignore-blank='grep -Ev "^#|^$"'
alias bash-completions='for n in /usr/share/bash-completion/completions/*; do . $n &> /dev/null; done' # ugh
alias img='wezterm imgcat'
epoch-to-date() {
  date --date="@${1}" +%F
}

# remembering to reshim asdf is a pain
#alias pip="pip $@ ; asdf reshim"

# open with suffix aliases
alias -s md=vim
alias -s markdown=vim
alias -s txt=vim
#alias -s py=vim
#alias -s rb=vim
#alias -s sh=vim

#sane defaults
alias sudo='sudo ' # so aliases work
#alias fd='fd -HI' # search all files
alias vi='vim'
alias spacemacs='emacs'
alias mkdir='mkdir -p'
alias grep="grep --color"
#alias gi="|grep -i"
alias Grep="grep"    # when hitting shift for pipe
alias wget='wget -c' #auto continue files
alias df='df -Th'
#alias rm='rm -i' #ask before overwriting
alias cp='cp -i'
alias mv='mv -i'
# alias rm='rm -f' # don't ask
alias rmrf='rm -rf'
alias tree='tree -I "node_modules"' # add more with ignorethis|andthis
alias ipb='ip -br -c a'             # ip -brief -color address

#ls
#alias l='ls -CF'
#alias ll='ls -l'
alias lll='ls -l|less'
#alias la='ls -lA'
alias la='ls -a'
#alias lt='ls -lt'
alias lt='ls -lt'
alias lth='ls -lt|head'
alias lthn='ls -lt|head -n'
#alias ltr='ls -ltr'
alias ltr='eza -l -s newest'
alias ltrn='ltr | tail -n'
alias lh='ls -lhS'                                       # sort by filesize
alias lsd="ls -l $1 | grep '^d'"                         # only dirs
alias lsd-names="ls -F $1 | grep \/ | sed -e 's/\/$//g'" # bare dir names for looping and such

# other
alias ..='/usr/bin/cd ..'
alias -- -='/usr/bin/cd -' # -- treats next chars as operand

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
alias kc=kubectl

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
have() { type "$1" &>/dev/null; }

## shortcut-TRICKS
alias findwindow="python ~/bin/i3/i3-wm-scripts/nextmatch.py"
alias igrep='grep -i'
alias g="googler "
alias hist='cat ~/.zsh_history* | grep -i'
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
#alias vim='dntw_edit'
alias vim='nvim'
alias vimdiff='nvim -d' # neovim changes this for some reason
alias svi='sudo -E $EDITOR'
alias svdi='sudo -E vim -d'
alias vvc='$EDITOR ~/.vimrc'
alias vvl='$EDITOR ~/.config/nvim/init.lua'
alias vvp='$EDITOR ~/.config/nvim/lua/plugins/all.lua'
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
alias fdays='find . -mtime'
alias mimetype='file --mime-type -b'
alias screenshot='ffmpeg -f x11grab  -s 3840x2160 -i :0.0 -r 25 -vcodec libx264  output.mp4'

## network
alias pgoo='ping -c2 google.com'
alias wgetnc='wget --no-check-certificate'
alias syn='ssh root@10.0.1.8'
alias listeners="sudo lsof -i -P -n | grep LISTEN"
alias listener="sudo lsof -i -P -n | grep LISTEN | grep "

## Monitoring
alias it='iotop'

## Git
git-diff-make-patch() {
  git diff -p --no-color $1 >patch
}
git-diff-uniq-dirs() {
  git diff --name-only HEAD~1 | awk -F "/*[^/]*/*$" '{ print ($1 == "" ? "." : $1); }' | sort | uniq
}
git-lines-by-author() {
  git ls-tree -r -z --name-only HEAD -- */* | sed 's/^/.\//' | xargs -0 -n1 git blame --line-porcelain HEAD | grep -ae "^author " | sort | uniq -c | sort -nr
}
alias ga='git add'
alias gau'git add -u'
alias gs='git status --ignore-submodules'
alias gsl='git status --ignore-submodules | less'
alias gb='git branch '
alias gc='git commit'
alias gcv='git commit --no-verify'
alias gg='gitg --all&'
alias gx='gitx --all'
alias gd='git diff --color | sed "s/^\([^-+ ]*\)[-+ ]/\\1/" | less -r' # remove +/- from git diff output
alias gdl='git diff|less'
alias gds='git diff --stat'
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
vimgd() { vim -p $(git status --short | awk '{print $2}'); }
vimgdm() { vim -p $(git diff master --numstat | awk '{print $3}'); }
vimgdbr() { vim -p $(git diff $1 --numstat | awk '{print $3}'); }
gitbr() { git for-each-ref --sort=-committerdate refs/heads/ | head -n 10 | awk '{print $3}' | sed 's@refs/heads/@@'; } # git branches sorted by date desc

## ruby
alias be='bundle exec'
alias gems='gem search -r '
alias gspec='gem spec -r '
alias gdep='gem dep -r '
function cdgem() { cd ${GEM_HOME}/gems/$1*; }

# list largest files
alias bigfiles='find . -type f -ls | sort -nrk7 | head -10'

# media conversion
alias wma2ogg='for i in *.wma; do ffmpeg -i $i -acodec vorbis -aq 100 ${i}.ogg; done'
alias ogv2avi='for n in `ls *`; do mencoder $n -ovc lavc -oac mp3lame -o $(echo $n | cut -d "." -f 1).avi; done'
alias ogv2mp4="mencoder out.ogg -of lavf -lavfopts format=mp4 -oac mp3lame -lameopts cbr:br=128 -ovc x264 -x264encopts bitrate=1000 -o final.mp4"
alias ogv23gp='for n in `ls *.ogv`; do sudo ffmpeg -i $n -r 15 -b 64kb -ac 1 -s 176x132 -padtop 6 -padbottom 6 -ar 16000 -ab 32kb -acodec libfaac -vcodec h263 $(echo $n | cut -d "." -f 1).3gp; done'
alias mp423gp='for n in `ls *.mp4`; do mencoder $n -vf scale=176:144 -oac mp3lame -ovc lavc -o $(echo $n | cut -d "." -f 1).3gp; done'
alias ddjvu2pdf='ddjvu --format=pdf input.djvu output.pdf'
ocrpdf() {
  TESSDATA_PREFIX=/usr/share/tessdata/ ocrmypdf ${1} ocr-${1}
}

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
  npm -g ls --parseable --depth=0 | awk '{gsub(/\/.*\//,"",$1); print}' | sort -u
}

# get remote ip
alias myip="curl https://canhazip.com/" #"curl ifconfig.me"

# https://github.com/nvbn/thefuck
alias fuck='$(thefuck $(fc -ln -1))'

# Python
alias pip-upgrade-all='pip freeze --local | grep -v "^\-e" | cut -d = -f 1  | xargs -n1 pip install -U' # no, really: https://github.com/pypa/pip/issues/59

### Apps
alias mpsyt="PYENV_VERSION=3.6.1 mpsyt" # youtube cli player

### Database
alias mys='mycli -uroot '

# yt-dlp
alias ytmp3='yt-dlp -x -f bestaudio --audio-quality 0 --audio-format mp3'
alias yta='yt-dlp -x -f bestaudio --prefer-free-formats -i --output "%(title)s.%(ext)s" '
alias ytv='yt-dlp -f bestvideo+bestaudio --prefer-free-formats -i --output "%(title)s.%(ext)s"'
alias ytcast='yt-dlp -o - https://youtu.be/BaW_jenozKc | castnow --quiet -' # cast to chromecast

### Systemd
alias update-pacman-keys='sudo pacman -Sy archlinux-keyring && sudo pacman -Su'
alias restart='sudo systemctl restart '
alias start='sudo systemctl start '
alias stop='sudo systemctl stop '
alias status='systemctl status '
alias reload='sudo systemctl daemon-reload'
alias restartu='systemctl --user restart '
alias startu='systemctl --user start '
alias stopu='systemctl --user stop '
alias statusu='systemctl --user status '
#alias reloadu='systemctl --user daemon-reload'
alias hibernate='echo "Hibernate" && sudo systemctl hibernate'
alias suspend='echo "Suspend then Hibernate" && sudo systemctl suspend-then-hibernate'

# i3
alias cur-monitor="xrandr | grep -C 3 '*' | grep DP | awk '{print \$1}'"

# work
alias novpn='nmcli con down "mullvad_us_sea"'
alias hvpn='nmcli con up "mullvad_us_sea"'

# s (web-cli search)
alias sa='/usr/bin/s -p amazon'
alias sw='/usr/bin/s -p wikipedia'

# fasd
# these are largely the same as the aliases defined in /usr/bin/fasd script
#alias a='fasd -a'        # any
#alias s='fasd -si'       # show / search / select
#alias d='fasd -d'        # directory
#alias f='fasd -f'        # file
#alias sd='fasd -sid'     # interactive directory selection
#alias sf='fasd -sif'     # interactive file selection
#alias z='fasd_cd -d'     # cd, same functionality as j in autojump
#alias j='fasd_cd -d'     # cd, same functionality as j in autojump
#alias zz='fasd_cd -d -i' # cd with interactive selection

# blackarch
alias blarchls="sudo pacman -Sgg | grep blackarch | cut -d' ' -f2 | sort -u"

# bluetooth
alias trekz="bluetoothctl connect 20:74:CF:ED:64:E5"
alias q2="bluetoothctl connect 28:11:A5:48:19:A5"
alias hifi="bluetoothctl connect CC:39:8C:01:DB:2D"
alias sony="bluetoothctl connect F8:4E:17:53:98:92"
alias shokz='bluetoothctl connect 20:74:CF:8E:0E:66'
alias bt-codec='pw-cli info all | grep codec'

# battery (thinkpad)
#alias battery="upower -i /org/freedesktop/UPower/devices/battery_BAT0" # "inxi -Bxxx"
# battery (framework)
alias battery="upower -i /org/freedesktop/UPower/devices/battery_BAT1" # "inxi -Bxxx"

# cheatsheets
cheat() {
  curl http://cht.sh/$1
}
cht() {
  cheat $1
}

# noteair
alias na=' . ~/bin/noteair.sh'
alias vvim="vim +'source ~/vimrcnoteair'"
alias temp="cpu-x -D 2>/dev/null|grep 'Temp\.'"

# aws
alias assume="~/bin/assume"

# kasina
alias kasina-sync='rsync -aPz ~/avs/kasina/ /run/media/jon/6923-A037/ --include="*/" --include "*.mp3" --include "*.kbs" --exclude "*" --delete; find /run/media/jon/6923-A037/ -empty -delete'

# golang
alias gos=go-search

# conversion
md2pdf() {
  # or https://www.mscharhag.com/software-development/pandoc-markdown-to-pdf
  pandoc --pdf-engine=xelatex ${1} -o ${1}.pdf
}

# kill
# weird, this is stalling out shell startup all of a sudden
#alias kill-windows="IFS=$'\n'; for n in `ps aux|grep windows`; do kill $(echo $n | awk '{print $2}'); done"

# keyboard
xm() {
  xmodmap ~/.xmodmap # laptop keyboard
  sudo systemctl restart keyd
  # pkill xcape
  # xcape -e "Control_L=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0;Super_R=Shift_R|bracketright;Super_L=Shift_L|bracketleft;Print=Shift_R|bracketright;Alt_L=Shift_L|bracketleft" -t 200
}
xk() {
  xmodmap ~/configs_misc/linux/xmodmap_kinesis # kinesis keyboard
  sudo systemctl restart keyd
  # pkill xcape
  # xcape -e "Control_L=Escape;Shift_L=Shift_L|9;Shift_R=Shift_R|0;Super_R=Shift_R|bracketright;Super_L=Shift_L|bracketleft;Print=Shift_R|bracketright;Alt_L=Shift_L|bracketleft" -t 200
}
alias xm-reset='setxkbmap -layout us'

alias performance='echo "performance" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'
alias powersave='echo "powersave" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'

# fzf
alias pf="fzf --preview='less {}' --bind shift-up:preview-page-up,shift-down:preview-page-down"

# archlinux
alias ql='pacman -Ql '
alias q='pacman -Q|grep '

# docker containers
alias unifi='cd ~/opt/docker-unifi-controller && docker compose up' # https://0.0.0.0:8443
alias calibre-web='cd ~/opt/calibre-web && docker compose up'

# docker
alias docker_clean_images='docker rmi $(docker images -a --filter=dangling=true -q)'
alias docker_clean_ps='docker rm $(docker ps --filter=status=exited --filter=status=created -q)'

# hue
alias hue-br-off="openhue set room Bedroom --off"
alias hue-br-on="openhue set room Bedroom --on"
alias hue-bath-off="openhue set room Bathroom --off"
alias hue-bath-on="openhue set room Bathroom --on"
alias hue-office-off="openhue set room Office --off"
alias hue-office-on="openhue set room Office --on"

# aws
find-lambda() {
  aws lambda list-functions| grep $1
}
open-lambda() {
  open "https://us-west-2.console.aws.amazon.com/lambda/home?region=us-west-2#/functions/$(aws lambda get-function --function-name $1 --query 'Configuration.FunctionName' --output text)"
}
sam-tail-logs() {
  # sam logs -n HelloWorldFunction --stack-name mystack --tail
  sam logs -n $1 --stack-name $2 --tail #--include-traces
}

# ai
alias llm="llm -o unlimited 1"
alias llml="llm -m Llama-3.3-70B-Instruct-4bit "
alias llmc="llm -m claude-3.7-sonnet "
alias ld7="llm -m deepseek-r1:7b"
alias ld8="llm -m deepseek-r1:8b"
alias gdllm="git diff | llm -s 'Describe these changes'"
llmcnt() {
  llm "cliffnotes for $1"
}
aidero() {
  aider --model "ollama_chat/${1}"
}
aiderl() {
  aider --model "lm_studio/${1}"
}
alias aiderords="aider --architect --model openrouter/deepseek/deepseek-r1 --editor-model openrouter/anthropic/claude-3.5-sonnet"
alias aiderqwen="aider --notifications --watch-files --model ollama_chat/qwen2.5-coder:32b"
alias aiderqwqqwen="aider --notifications --watch-files --architect --model ollama_chat/qwq:32b --editor-model ollama_chat/qwen2.5-coder:32b"
alias aiderqwqqwenmlx="aider --notifications --watch-files --no-show-model-warnings --architect --model lm_studio/qwq-32b@8bit --editor-model lm_studio/qwen2.5-coder-32b-instruct-mlx@8bit"
alias aiderqwqmistral="aider --notifications --watch-files --architect --model ollama_chat/qwq:32b --editor-model ollama_chat/hf.co/bartowski/mistralai_Mistral-Small-3.1-24B-Instruct-2503-GGUF:Q4_K_M"
# note: k6_K overheats m4 max too much (225* F)
alias aidermistral8="aider --notifications --watch-files --model ollama_chat/hf.co/bartowski/mistralai_Mistral-Small-3.1-24B-Instruct-2503-GGUF:Q6_K"
alias aidermistral="aider --notifications --watch-files --model ollama_chat/hf.co/bartowski/mistralai_Mistral-Small-3.1-24B-Instruct-2503-GGUF:Q4_K_M"
alias aiderds="aider --architect --model r1 --editor-model sonnet"
alias ghe="gh copilot explain"
alias ghs="gh copilot suggest"
alias fabric='fabric-ai'

## ollama 
ollamastopall() { for n in $(ollama ps | grep -v NAME | awk '{print $1}'); do ollama stop $n; done }
ollamapull() { for n in $(grep -v '^#' ~/exp/ai/models.txt); do echo $n; ollama pull $n; done }
alias ol='ollama'
llmtest() { 
  llm -m $1 "delete a model from ollama" 
}
ollamatemps() { 
  for MODEL in $(ollama ls | grep -v NAME | awk '{print $1}'); do echo $MODEL; ollama show --parameters $MODEL | grep temperature; done
}

# mac
stupidmacallow() { 
  xattr -d 'com.apple.quarantine' $1 
}

alias ddocker="~/.docker/bin/docker"
yt-summarize() {
  pattern=${2:-"summarize"}
  fabric-ai --transcript-with-timestamps -y $1 --stream --pattern $pattern
}

# Mac
if [ $SYSTEM_TYPE = "Darwin" ]; then
  alias agu='brew doctor; brew update && brew upgrade'
  alias r="brew uninstall"
  alias bs="brew services"
fi;
