#!/bin/sh

STATUS=`/usr/share/awesome/bashets/mpd.awk`
TITLE=`mpc -p 6602 | head -n 1`
MTIME=`mpc -p 6602 | head -n 2 | tail -n 1 | awk '{print $3}'`

echo -n "$STATUS|$TITLE|$MTIME"
