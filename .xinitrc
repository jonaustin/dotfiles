#!/bin/bash
set -e

xrdb -load ~/.Xresources

if [[ -d "/etc/X11/xinit/xinitrc.d" ]]; then
  for n in /etc/X11/xinit/xinitrc.d/*.sh; do
    source "${n}"
  done
fi

xset -dpms; xset s off; xset s noblank

#/usr/bin/xmodmap /home/jon/.xmodmap

#xrandr --newmode "1616x1080_60.00" 145.97 1616 1720 1896 2176 1080 1081 1084 1118 -HSync +Vsync
#xrandr --newmode "3000x2000_60.00" 513.44 3000 3240 3568 4136 2000 2001 2004 2069 -HSync +Vsync
#xrandr --addmode eDP1 "1616x1080_60.00"
#xrandr --addmode eDP1 "3000x2000_60.00"

exec i3
