xrdb -load ~/.Xresources
/usr/bin/xmodmap /home/jon/.xmodmap


xrandr --newmode "1616x1080_60.00" 145.97 1616 1720 1896 2176 1080 1081 1084 1118 -HSync +Vsync
xrandr --addmode eDP1 "1616x1080_60.00" 

exec i3
