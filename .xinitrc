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

#ivman&
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
