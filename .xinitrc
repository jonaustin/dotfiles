#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

#/etc/X11/Xsession
sh /home/jon/.xmodmap
# D-bus
if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
       eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

# disable internal screen
eval `xrandr --output VGA-0 --mode 1920x1200; xrandr --output LVDS --off`
# background
#eval `cat ~/.fehbg`
#xsetroot -solid black

# network
#eval `sshfs -o reconnect -o allow_other -p666 jon@frak:/ /media/frakssh`

#ivman&
#exec ck-launch-session awesome  # consolekit -- for udev/devmon automounting policy allowance
#exec ck-launch-session subtle
exec awesome

#exec xterm


# exec gnome-session
# exec startkde
# exec startxfce4
# exec icewm
# exec openbox
# exec blackbox
# exec fluxbox
# exec ratpoison
# exec dwm
# ... or any other WM of your choosing ...
